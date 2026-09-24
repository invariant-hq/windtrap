(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Capture: fd-level redirection round-trips (including Unix.write
   and subprocess output that bypass OCaml channels), the log-dir layout,
   incremental consumption windows, bounded tails with drop counts, Disabled
   ([--stream]) behavior, and per-attempt truncation.
   These sessions nest inside the runner's own capture: [with_capture]
   saves and restores the (already redirected) descriptors, so the outer
   capture is unaffected.

   Discipline: no assertion may run inside [with_capture] — a printed
   report would land in the inner file. Bodies collect values into refs;
   checks run after. *)

open Windtrap
module Capture = Windtrap.Private.Capture
module Failure = Windtrap.Private.Failure
module Loc = Windtrap.Private.Loc
module Os = Windtrap.Private.Os

(* Helpers *)

let contains text pattern =
  Windtrap.Private.Text.contains_substring ~pattern text

let read_file path = In_channel.with_open_bin path In_channel.input_all

let fd_id fd =
  let st = Unix.fstat fd in
  (st.Unix.st_dev, st.Unix.st_ino)

let concat_all = List.fold_left Filename.concat

(* Round trip: fd-level capture into the per-test file *)

let test_fd_round_trip () =
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"suite" () in
  let out_before = fd_id Unix.stdout and err_before = fd_id Unix.stderr in
  let out_inside = ref out_before in
  let sub_status = ref (-1) in
  let value =
    Capture.with_capture cap ~groups:[ "outer"; "inner" ] ~test_name:"my test"
      (fun () ->
        print_string "chan-out.";
        Printf.eprintf "chan-err.";
        Format.printf "fmt-out.";
        let raw = Bytes.of_string "raw-out." in
        ignore (Unix.write Unix.stdout raw 0 (Bytes.length raw));
        (match Unix.system "echo sub-out." with
        | Unix.WEXITED n -> sub_status := n
        | _ -> ());
        out_inside := fd_id Unix.stdout;
        42)
  in
  equal ~msg:"with_capture returns the body's value" int 42 value;
  is_true ~msg:"descriptor 1 is redirected during the body"
    (!out_inside <> out_before);
  is_true ~msg:"descriptor 1 is restored" (fd_id Unix.stdout = out_before);
  is_true ~msg:"descriptor 2 is restored" (fd_id Unix.stderr = err_before);
  equal ~msg:"subprocess exit status" int 0 !sub_status;
  let suite_dir = Filename.concat root "suite" in
  (* The layout is read off the filesystem, not rebuilt: naming the file
     with [sanitize_component] would agree with any mapping whatsoever, so
     it would assert nothing. That Capture routes components through the
     sanitizer at all is [test_sanitized_layout]'s job; that the mapping
     keeps distinct tests on distinct files is [test_name_collisions]'. *)
  let group_dir = concat_all suite_dir [ "outer"; "inner" ] in
  let entries =
    if Sys.is_directory group_dir then Array.to_list (Sys.readdir group_dir)
    else []
  in
  let log_file =
    match entries with
    | [ name ] when Filename.check_suffix name ".output" -> Some name
    | _ -> None
  in
  is_true ~msg:"one .output file at <log_dir>/<suite>/<groups...>"
    (log_file <> None);
  match log_file with
  | Some name ->
      let path = Filename.concat group_dir name in
      let content = read_file path in
      List.iter
        (fun fragment ->
          is_true ~msg:("file captures " ^ fragment) (contains content fragment))
        [ "chan-out."; "chan-err."; "fmt-out."; "raw-out."; "sub-out." ];
      (* Restored streams no longer feed the file. *)
      let size_after = (Unix.stat path).Unix.st_size in
      print_string "\n";
      flush stdout;
      equal ~msg:"post-capture writes do not reach the file" int size_after
        (Unix.stat path).Unix.st_size
  | None -> ()

(* Incremental consumption *)

let test_incremental_consumption () =
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  equal ~msg:"output before any capture is empty" string "" (Capture.output cap);
  is_true ~msg:"output_tail before any capture is None"
    (Capture.output_tail cap = None);
  let o0 = ref "?" and o1 = ref "?" and o2 = ref "?" in
  let o3 = ref "?" and o4 = ref "?" in
  Capture.with_capture cap ~groups:[] ~test_name:"t" (fun () ->
      o0 := Capture.output cap;
      print_string "alpha";
      o1 := Capture.output cap;
      print_string "beta";
      Format.printf "gamma";
      (* buffered in the formatter: output must drain it *)
      o2 := Capture.output cap;
      o3 := Capture.output cap;
      let raw = Bytes.of_string "delta" in
      ignore (Unix.write Unix.stdout raw 0 (Bytes.length raw));
      o4 := Capture.output cap);
  equal ~msg:"window at attempt start is empty" string "" !o0;
  equal ~msg:"first window" string "alpha" !o1;
  equal ~msg:"second window drains channel and formatter" string "betagamma" !o2;
  equal ~msg:"consumed bytes are not returned again" string "" !o3;
  equal ~msg:"fd-level bytes appear in the window" string "delta" !o4;
  equal ~msg:"output after the attempt sees nothing new" string ""
    (Capture.output cap);
  match Capture.output_tail cap with
  | None -> is_true ~msg:"output_tail present after the attempt" false
  | Some tail ->
      equal ~msg:"tail covers the whole file despite consumption" string
        "alphabetagammadelta" tail.Failure.text;
      equal ~msg:"nothing omitted" int 0 tail.Failure.omitted_bytes;
      is_true ~msg:"tail names the log file" (tail.Failure.log_path <> None)

(* Exception safety *)

exception Boom

let test_exception_restores () =
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  let out_before = fd_id Unix.stdout in
  let propagated =
    match
      Capture.with_capture cap ~groups:[] ~test_name:"t" (fun () ->
          print_string "pre-raise";
          raise Boom)
    with
    | () -> false
    | exception Boom -> true
    | exception _ -> false
  in
  is_true ~msg:"the body's exception propagates unchanged" propagated;
  is_true ~msg:"descriptor 1 is restored after a raise"
    (fd_id Unix.stdout = out_before);
  match Capture.output_tail cap with
  | None -> is_true ~msg:"output_tail present after a raise" false
  | Some tail ->
      equal ~msg:"buffered output is drained to the file on the raise path"
        string "pre-raise" tail.Failure.text

let test_setup_failure_isolation () =
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  Capture.with_capture cap ~groups:[] ~test_name:"prev" (fun () ->
      print_string "prev-output");
  (* A regular file where the next attempt needs a directory: the log file
     cannot be created and setup raises. *)
  Out_channel.with_open_bin (concat_all root [ "s"; "g" ]) (fun _ -> ());
  let out_before = fd_id Unix.stdout and err_before = fd_id Unix.stderr in
  let raised =
    match
      Capture.with_capture cap ~groups:[ "g" ] ~test_name:"t" (fun () ->
          print_string "never-runs")
    with
    | _ -> false
    | exception (Unix.Unix_error _ | Sys_error _) -> true
  in
  is_true ~msg:"a failed setup raises" raised;
  is_true ~msg:"descriptor 1 untouched by a failed setup"
    (fd_id Unix.stdout = out_before);
  is_true ~msg:"descriptor 2 untouched by a failed setup"
    (fd_id Unix.stderr = err_before);
  is_true ~msg:"a failed setup does not expose the previous attempt's tail"
    (Capture.output_tail cap = None);
  equal ~msg:"output after a failed setup is empty" string ""
    (Capture.output cap)

let test_drain_failure_restores () =
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  let out_before = fd_id Unix.stdout and err_before = fd_id Unix.stderr in
  let outcome =
    match
      Capture.with_capture cap ~groups:[] ~test_name:"t" (fun () ->
          (* Buffer a byte in the stderr channel, then close descriptor 2:
             the cleanup drain's flush fails after the body returns. *)
          Printf.eprintf " ";
          Unix.close Unix.stderr)
    with
    | () -> `Returned
    | exception Fun.Finally_raised _ -> `Finally_raised
    | exception _ -> `Other
  in
  is_true ~msg:"the failed cleanup drain propagates as Finally_raised"
    (outcome = `Finally_raised);
  is_true ~msg:"descriptor 1 is restored despite the failed drain"
    (fd_id Unix.stdout = out_before);
  is_true ~msg:"descriptor 2 is restored despite the failed drain"
    (fd_id Unix.stderr = err_before);
  (* The failed flush left the byte buffered; descriptor 2 is valid again, so
     drain it to the real stderr and leave later tests a clean channel. *)
  try flush stderr with Sys_error _ -> ()

(* A drain that fails before the attempt: descriptor 2 is read-only with a
   byte buffered for it, so [drain] and the first drain of [with_capture]
   both fail. (A closed descriptor 2 would be reused by the log file the
   attempt opens.) The real descriptor 2 is put back afterwards. *)
let test_first_drain_failure () =
  if Sys.win32 then skip ~reason:"POSIX only" ();
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  let out_before = fd_id Unix.stdout in
  let saved = Unix.dup Unix.stderr in
  let ran = ref false in
  let drain_raised, outcome =
    Fun.protect
      ~finally:(fun () ->
        Unix.dup2 saved Unix.stderr;
        Unix.close saved;
        try flush stderr with Sys_error _ -> ())
      (fun () ->
        Printf.eprintf " ";
        let read_only = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
        Unix.dup2 read_only Unix.stderr;
        Unix.close read_only;
        let drain_raised =
          match Capture.drain () with
          | () -> false
          | exception Sys_error _ -> true
        in
        (* A failed flush drops what it held: buffer a byte again. *)
        Printf.eprintf " ";
        let outcome =
          match
            Capture.with_capture cap ~groups:[] ~test_name:"t" (fun () ->
                ran := true)
          with
          | () -> `Returned
          | exception Sys_error _ -> `Sys_error
          | exception _ -> `Other
        in
        (drain_raised, outcome))
  in
  is_true ~msg:"drain raises Sys_error on a descriptor it cannot write"
    drain_raised;
  is_true ~msg:"with_capture raises the first drain's Sys_error"
    (outcome = `Sys_error);
  is_false ~msg:"the function did not run" !ran;
  is_true ~msg:"descriptor 1 is as it was" (fd_id Unix.stdout = out_before);
  is_true ~msg:"the state has no current log" (Capture.output_tail cap = None)

let test_last_drain_replaces_fatal () =
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  let outcome =
    match
      Capture.with_capture cap ~groups:[] ~test_name:"t" (fun () ->
          Printf.eprintf " ";
          Unix.close Unix.stderr;
          raise Stack_overflow)
    with
    | () -> `Returned
    | exception Fun.Finally_raised (Sys_error _) -> `Finally_raised
    | exception Stack_overflow -> `Fatal
    | exception _ -> `Other
  in
  (try flush stderr with Sys_error _ -> ());
  is_true ~msg:"the failed last drain replaces even a fatal exception"
    (outcome = `Finally_raised)

let test_one_text_in_arrival_order () =
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  Capture.with_capture cap ~groups:[] ~test_name:"t" (fun () ->
      print_string "1";
      flush stdout;
      prerr_string "2";
      flush stderr;
      print_string "3";
      flush stdout;
      prerr_string "4";
      flush stderr);
  equal ~msg:"both descriptors in the order the bytes arrived" string "1234"
    (read_file (concat_all root [ "s"; "t.output" ]))

let test_create_creates_nothing () =
  let root = temp_dir () in
  let log_dir = Filename.concat root "logs" in
  let cap = Capture.create ~log_dir ~suite:"s" () in
  is_false ~msg:"no directory before an attempt" (Sys.file_exists log_dir);
  Capture.with_capture cap ~groups:[] ~test_name:"t" ignore;
  is_true ~msg:"the attempt makes it" (Sys.file_exists log_dir)

let test_abandon_ignores_drain_failure () =
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  let out_before = fd_id Unix.stdout and err_before = fd_id Unix.stderr in
  let abandoned = ref false and after = ref None in
  Capture.with_capture cap ~groups:[] ~test_name:"t" (fun () ->
      Printf.eprintf " ";
      Unix.close Unix.stderr;
      (match Capture.abandon cap with
      | () -> abandoned := true
      | exception Sys_error _ -> ());
      after := Some (fd_id Unix.stdout, fd_id Unix.stderr));
  is_true ~msg:"abandon ignores the Sys_error of its drain" !abandoned;
  is_true ~msg:"and the real descriptors are back"
    (!after = Some (out_before, err_before))

let test_unopenable_log () =
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  let log = concat_all root [ "s"; "t.output" ] in
  let first = ref "?" and away = ref "?" in
  let tail_away = ref None and back = ref "?" in
  Capture.with_capture cap ~groups:[] ~test_name:"t" (fun () ->
      print_string "abc";
      first := Capture.output cap;
      print_string "def";
      Sys.rename log (log ^ ".away");
      away := Capture.output cap;
      tail_away := Capture.output_tail cap;
      Sys.rename (log ^ ".away") log;
      back := Capture.output cap);
  equal ~msg:"the first window" string "abc" !first;
  equal ~msg:"output is empty while the log cannot be opened" string "" !away;
  is_true ~msg:"output_tail is None then" (!tail_away = None);
  equal ~msg:"the cursor stayed, so the unread bytes come back" string "def"
    !back

(* Disabled ([--stream]) *)

(* Abandon: what a signal handler does to an attempt it will not return
   to. The body below does return, which also shows [with_capture]'s own
   exit is harmless after it. *)
let test_abandon () =
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"suite" () in
  let out_before = fd_id Unix.stdout and err_before = fd_id Unix.stderr in
  let after_abandon = ref None in
  Capture.abandon cap;
  is_true ~msg:"nothing redirected: a no-op" (fd_id Unix.stdout = out_before);
  Capture.abandon Capture.disabled;
  Capture.with_capture cap ~groups:[] ~test_name:"stopped" (fun () ->
      print_string "buffered when the signal came.";
      Capture.abandon cap;
      after_abandon := Some (fd_id Unix.stdout, fd_id Unix.stderr));
  is_true ~msg:"the real descriptors are back at once"
    (!after_abandon = Some (out_before, err_before));
  is_true ~msg:"and still are after with_capture's own exit"
    (fd_id Unix.stdout = out_before && fd_id Unix.stderr = err_before);
  equal ~msg:"what the test had buffered was drained into its log" string
    "buffered when the signal came."
    (read_file (concat_all root [ "suite"; "stopped.output" ]))

let stream_error = "this test requires capture; rerun without --stream"

let test_disabled () =
  let cap = Capture.disabled in
  is_true ~msg:"output_tail is None under Disabled"
    (Capture.output_tail cap = None);
  let value =
    Capture.with_capture cap ~groups:[ "g" ] ~test_name:"t" (fun () -> 7)
  in
  equal ~msg:"with_capture runs the body directly" int 7 value;
  (match Capture.output ~__POS__:("test_capture.ml", 42, 3, 9) cap with
  | _ -> is_true ~msg:"output under Disabled raises Check_failure" false
  | exception Failure.Check_failure f -> (
      (match f.Failure.kind with
      | Failure.Message m ->
          equal ~msg:"the typed requires-capture message" string stream_error m
      | _ -> is_true ~msg:"failure kind is Message" false);
      match f.Failure.loc with
      | Some l ->
          equal ~msg:"failure location file comes from ?__POS__" string
            "test_capture.ml" l.Loc.file;
          equal ~msg:"failure location line comes from ?__POS__" int 42
            l.Loc.line
      | None -> is_true ~msg:"failure carries the ?__POS__ location" false)
  | exception _ ->
      is_true ~msg:"output under Disabled raises Check_failure" false);
  (* The realistic path: output () called inside a streamed test body. *)
  let saw = ref false in
  ignore
    (Capture.with_capture cap ~groups:[] ~test_name:"t" (fun () ->
         (match Capture.output cap with
         | _ -> ()
         | exception Failure.Check_failure _ -> saw := true);
         0));
  is_true ~msg:"output raises at the call site inside a streamed body" !saw

(* Bounded tails.

   The bound is [Failure.tail_bytes] — no per-state knob to dial down — so
   every case below has to overrun 8 KiB for real, and the expectations are
   computed from the bound rather than written out. Where the cut lands
   inside a UTF-8 sequence is a property of the payload's character width
   against that fixed bound: 8192 is a multiple of 2 and 4 but not of 3, so
   a run of three-byte scalars is what puts the cut mid-sequence. *)

let bound = Failure.tail_bytes

(* The tail of the last attempt, or a failed check and a stand-in. *)
let tail_of name cap =
  match Capture.output_tail cap with
  | Some tail -> tail
  | None ->
      is_true ~msg:("tail present (" ^ name ^ ")") false;
      Failure.tail ""

let capture_string cap ~test_name payload =
  Capture.with_capture cap ~groups:[] ~test_name (fun () ->
      print_string payload);
  tail_of test_name cap

let test_bounded_tail () =
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  let letters n =
    String.init n (fun i -> Char.chr (Char.code 'a' + (i mod 26)))
  in
  let payload = letters (bound + 3_000) in
  let tail = capture_string cap ~test_name:"big" payload in
  equal ~msg:"tail is exactly the final tail_bytes" string
    (String.sub payload 3_000 bound)
    tail.Failure.text;
  equal ~msg:"omitted_bytes counts everything before the tail" int 3_000
    tail.Failure.omitted_bytes;
  equal ~msg:"omitted + retained accounts for every byte" int
    (String.length payload)
    (tail.Failure.omitted_bytes + String.length tail.Failure.text);
  (match tail.Failure.log_path with
  | Some p ->
      equal ~msg:"the log file holds the complete output" string payload
        (read_file p)
  | None -> is_true ~msg:"log_path present" false);
  (* Output exactly at the bound is complete: no cut, no skip. *)
  let exact = letters bound in
  let tail = capture_string cap ~test_name:"exact" exact in
  equal ~msg:"output at exactly tail_bytes is retained whole" string exact
    tail.Failure.text;
  equal ~msg:"output at exactly tail_bytes omits nothing" int 0
    tail.Failure.omitted_bytes;
  (* Output below the bound is complete. *)
  let tail = capture_string cap ~test_name:"small" "tiny" in
  equal ~msg:"small output is retained whole" string "tiny" tail.Failure.text;
  equal ~msg:"small output omits nothing" int 0 tail.Failure.omitted_bytes

let repeat n s = String.concat "" (List.init n (fun _ -> s))

let test_utf8_boundary () =
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  (* Three-byte scalars: the bound is not a multiple of 3, so the suffix
     read starts one byte past a lead and two continuation bytes go. *)
  let euro = "\xE2\x82\xAC" in
  let chars = (bound / 3) + 100 in
  let payload = repeat chars euro in
  let cut = String.length payload - bound in
  equal ~msg:"the payload puts the cut one byte past a lead" int 1 (cut mod 3);
  let tail = capture_string cap ~test_name:"t" payload in
  equal ~msg:"the mid-sequence bytes are skipped" int (bound - 2)
    (String.length tail.Failure.text);
  is_true ~msg:"the tail starts on a UTF-8 boundary"
    (String.length tail.Failure.text > 0
    && Char.code tail.Failure.text.[0] land 0xC0 <> 0x80);
  equal ~msg:"the skipped bytes count as omitted" int (cut + 2)
    tail.Failure.omitted_bytes;
  equal ~msg:"the tail is whole characters" string
    (repeat ((bound - 2) / 3) euro)
    tail.Failure.text

let test_utf8_max_skip () =
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  (* Four-byte scalars would align with the bound exactly; the trailing
     one-byte 'z' shifts the run so the suffix starts one byte after a lead
     and three continuation bytes must be skipped — the maximum. *)
  let pile = "\xF0\x9F\x92\xA9" in
  let payload = repeat ((bound / 4) + 10) pile ^ "z" in
  let cut = String.length payload - bound in
  equal ~msg:"the payload puts the cut one byte past a lead" int 1 (cut mod 4);
  let tail = capture_string cap ~test_name:"t" payload in
  equal ~msg:"three continuation bytes are skipped" string
    (String.sub payload (cut + 3) (bound - 3))
    tail.Failure.text;
  is_true ~msg:"the tail starts on a UTF-8 boundary"
    (Char.code tail.Failure.text.[0] land 0xC0 <> 0x80);
  equal ~msg:"the three skipped bytes count as omitted" int (cut + 3)
    tail.Failure.omitted_bytes

let test_invalid_utf8_verbatim () =
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  (* Continuation-byte flood: no lead within reach, so nothing is skipped and
     the suffix is kept verbatim. *)
  let payload = String.make (bound + 56) '\x80' in
  let tail = capture_string cap ~test_name:"t" payload in
  equal ~msg:"invalid UTF-8 is kept verbatim" string (String.make bound '\x80')
    tail.Failure.text;
  equal ~msg:"no extra bytes counted omitted" int 56 tail.Failure.omitted_bytes

(* Per-attempt reset *)

let test_per_attempt_reset () =
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  let w1 = ref "?" and w2 = ref "?" in
  Capture.with_capture cap ~groups:[ "g" ] ~test_name:"t" (fun () ->
      print_string "first-attempt";
      w1 := Capture.output cap);
  Capture.with_capture cap ~groups:[ "g" ] ~test_name:"t" (fun () ->
      print_string "second";
      w2 := Capture.output cap);
  equal ~msg:"attempt 1 window" string "first-attempt" !w1;
  equal ~msg:"attempt 2 starts from a truncated file and reset cursor" string
    "second" !w2;
  match Capture.output_tail cap with
  | None -> is_true ~msg:"tail present after retries" false
  | Some tail ->
      equal ~msg:"only the final attempt's output remains" string "second"
        tail.Failure.text;
      equal ~msg:"the final attempt omits nothing" int 0
        tail.Failure.omitted_bytes

(* Sanitized layout *)

let test_sanitized_layout () =
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"my suite" () in
  Capture.with_capture cap ~groups:[ "a/b" ] ~test_name:"x:y" (fun () ->
      print_string "content");
  let suite_dir = Filename.concat root (Os.sanitize_component "my suite") in
  is_true ~msg:"the suite name is sanitized into one component"
    (Sys.is_directory suite_dir);
  let path =
    Filename.concat
      (Filename.concat suite_dir (Os.sanitize_component "a/b"))
      (Os.sanitize_component "x:y" ^ ".output")
  in
  is_true ~msg:"group and test components are sanitized" (Sys.file_exists path);
  is_true ~msg:"a slash in a group makes one component, not two"
    (not (Sys.file_exists (Filename.concat suite_dir "a")))

(* Distinct names, distinct files *)

let test_name_collisions () =
  (* Two names that differ only in punctuation map to the same readable
     form ([parse__empty]): the sanitizer keeps them apart only because it
     appends a digest of the original. Without it both attempts open one
     path — and [with_capture] opens it O_TRUNC — so the second test erases
     the first test's output while the first failure's tail still points at
     the file. Asserted on the files themselves, since reconstructing the
     names with the sanitizer would hold for any mapping at all. *)
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  let log_of name =
    Capture.with_capture cap ~groups:[] ~test_name:name (fun () ->
        print_string ("output of " ^ name));
    match Capture.output_tail cap with
    | Some { Failure.log_path = Some path; _ } -> path
    | _ -> ""
  in
  let colon = log_of "parse: empty" in
  let comma = log_of "parse, empty" in
  is_true ~msg:"each attempt names its log file" (colon <> "" && comma <> "");
  is_true ~msg:"names differing only in punctuation get distinct log files"
    (colon <> comma);
  equal ~msg:"the first name's log still holds its own output" string
    "output of parse: empty" (read_file colon);
  equal ~msg:"the second name's log holds its own output" string
    "output of parse, empty" (read_file comma)

(* Stable paths *)

let test_stable_paths () =
  (* The log path is a function of the test's identity, so a second run of
     the same suite writes the same file — which is what makes a path
     printed in a failure report worth typing into an editor. *)
  let root = temp_dir () in
  let log_of cap =
    Capture.with_capture cap ~groups:[ "g" ] ~test_name:"t" (fun () ->
        print_string "x");
    match Capture.output_tail cap with
    | Some { Failure.log_path = Some path; _ } -> path
    | _ -> ""
  in
  let first = log_of (Capture.create ~log_dir:root ~suite:"s" ()) in
  let second = log_of (Capture.create ~log_dir:root ~suite:"s" ()) in
  equal ~msg:"a rerun writes the same path" string first second;
  equal ~msg:"and the path is the test's identity under the suite" string
    (concat_all root [ "s"; "g"; "t.output" ])
    first

(* Saved descriptors are close-on-exec *)

(* The re-exec'd child: captures, spawns a minute-long sleeper whose own
   stdio is /dev/null, prints the sleeper's pid on its restored stdout, and
   exits. Under capture the child's real stdout is the parent's pipe; only
   the saved dups could leak it to the sleeper. *)
let child_spawn_holder log_dir =
  let cap = Capture.create ~log_dir ~suite:"cloexec" () in
  let sleeper = ref 0 in
  Capture.with_capture cap ~groups:[] ~test_name:"spawn" (fun () ->
      let null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
      sleeper :=
        Unix.create_process "/bin/sleep" [| "sleep"; "60" |] null null null;
      Unix.close null);
  print_int !sleeper;
  print_newline ();
  exit 0

let test_saved_descriptors_are_cloexec () =
  (* Capture's saved dups of the real stdout/stderr must be
     close-on-exec — an exec'd child that outlives the run must not hold
     the runner's stdout open, or a piped reader (`suite.exe | cat`, dune
     runtest) waits on the child after the suite finished. EOF on the
     child's pipe must arrive when the child exits, while its sleeper
     still lives, and no clock decides it. *)
  if Sys.win32 then skip ~reason:"no /bin/sleep on Windows" ();
  let root = temp_dir () in
  let out_read, out_write = Unix.pipe () in
  (* Keep our own pipe ends out of the child and its sleeper: only the
     child's stdout may hold the write end. *)
  Unix.set_close_on_exec out_read;
  Unix.set_close_on_exec out_write;
  let pid =
    Unix.create_process Sys.executable_name
      [| Sys.executable_name; "--capture-cloexec-child"; root |]
      Unix.stdin out_write Unix.stderr
  in
  Unix.close out_write;
  let output = Buffer.create 16 in
  let chunk = Bytes.create 4096 in
  let rec drain () =
    let n = Unix.read out_read chunk 0 (Bytes.length chunk) in
    if n > 0 then begin
      Buffer.add_subbytes output chunk 0 n;
      drain ()
    end
  in
  drain ();
  Unix.close out_read;
  (match Unix.waitpid [] pid with
  | _, Unix.WEXITED 0 -> ()
  | _ -> fail "cloexec child did not exit cleanly");
  let sleeper =
    require_some ~msg:"the child printed its sleeper's pid"
      (int_of_string_opt (String.trim (Buffer.contents output)))
  in
  (* A process that died but is not yet reaped still answers [kill 0], and
     the sleeper's EOF and its death are one event: ask [ps] for its state,
     where a zombie reads [Z]. *)
  let alive () =
    let ps =
      Unix.open_process_args_in "/bin/ps"
        [| "ps"; "-o"; "stat="; "-p"; string_of_int sleeper |]
    in
    let state = String.trim (In_channel.input_all ps) in
    ignore (Unix.close_process_in ps);
    state <> "" && state.[0] <> 'Z'
  in
  Fun.protect
    ~finally:(fun () ->
      try Unix.kill sleeper Sys.sigkill with Unix.Unix_error _ -> ())
    (fun () ->
      is_true
        ~msg:"the pipe closes when the suite exits, not when its sleeper dies"
        (alive ()))

(* Re-exec dispatch for the child above; this suite's own toplevel calls
   it before its run. Never returns for a child invocation. *)
let dispatch_child () =
  match Array.to_list Sys.argv with
  | [ _; "--capture-cloexec-child"; log_dir ] -> child_spawn_holder log_dir
  | _ -> ()

(* The log fd itself is close-on-exec *)

let test_log_fd_is_cloexec () =
  (* The companion of the test above: the .output fd must be as close-on-exec as
     the saved dups. An exec'd child writes through the redirected fds 1-2
     and must not also inherit the raw log fd. The child probes which of
     its fds 3-9 alias its own stdout — the log file during capture — so
     unrelated descriptors open in this process cannot trip it. *)
  if Sys.win32 then skip ~reason:"no /bin/sh on Windows" ();
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"cloexec" () in
  let status = ref (-1) in
  Capture.with_capture cap ~groups:[] ~test_name:"probe" (fun () ->
      status :=
        Sys.command
          "for fd in 3 4 5 6 7 8 9; do if [ /dev/fd/$fd -ef /dev/fd/1 ]; then \
           echo \"LEAK:$fd\"; fi; done; echo probed");
  equal ~msg:"probe shell exits 0" int 0 !status;
  let content = read_file (concat_all root [ "cloexec"; "probe.output" ]) in
  is_true ~msg:"the child still writes through the redirected fd 1"
    (contains content "probed");
  is_true ~msg:"no fd aliasing the log file leaks to the child"
    (not (contains content "LEAK:"))

let tests =
  [
    test "fd-level round trip into the per-test file" test_fd_round_trip;
    test "incremental consumption windows" test_incremental_consumption;
    test "an exception restores the descriptors" test_exception_restores;
    test "a failed setup leaves the descriptors untouched"
      test_setup_failure_isolation;
    test "a failed cleanup drain still restores" test_drain_failure_restores;
    test "a failed first drain runs nothing" test_first_drain_failure;
    test "a failed last drain replaces a fatal exception"
      test_last_drain_replaces_fatal;
    test "one text in arrival order" test_one_text_in_arrival_order;
    test "create creates nothing" test_create_creates_nothing;
    test "abandon ignores a failed drain" test_abandon_ignores_drain_failure;
    test "a log that cannot be opened reads as nothing" test_unopenable_log;
    test "abandon restores the descriptors from inside an attempt" test_abandon;
    test "Disabled (--stream) behavior" test_disabled;
    test "bounded tails with drop counts" test_bounded_tail;
    test "tail cut lands on a UTF-8 boundary" test_utf8_boundary;
    test "tail cut skips up to three continuation bytes" test_utf8_max_skip;
    test "invalid UTF-8 is kept verbatim" test_invalid_utf8_verbatim;
    test "per-attempt reset truncates the file" test_per_attempt_reset;
    test "sanitized layout" test_sanitized_layout;
    test "punctuation variants get distinct log files" test_name_collisions;
    test "log paths are stable across runs" test_stable_paths;
    test "saved descriptors are close-on-exec"
      test_saved_descriptors_are_cloexec;
    test "the log fd is close-on-exec" test_log_fd_is_cloexec;
  ]

let () = dispatch_child ()
let () = exit @@ Windtrap.run "capture" tests
