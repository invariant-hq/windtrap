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
module Path_ops = Windtrap.Private.Path_ops

let check name cond = is_true ~msg:name cond
let check_string name ~expected ~actual = equal ~msg:name string expected actual
let check_int name ~expected ~actual = equal ~msg:name int expected actual

(* Helpers *)

let contains text pattern =
  Windtrap.Private.Text.contains_substring ~pattern text

let read_file path = In_channel.with_open_bin path In_channel.input_all

let fd_id fd =
  let st = Unix.fstat fd in
  (st.Unix.st_dev, st.Unix.st_ino)

(* The runner removes each test's scratch directory with the attempt. *)
let with_temp_root f = f (temp_dir ())
let concat_all = List.fold_left Filename.concat

(* Round trip: fd-level capture into the per-test file *)

let test_fd_round_trip () =
  with_temp_root @@ fun root ->
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
  check_int "with_capture returns the body's value" ~expected:42 ~actual:value;
  check "descriptor 1 is redirected during the body" (!out_inside <> out_before);
  check "descriptor 1 is restored" (fd_id Unix.stdout = out_before);
  check "descriptor 2 is restored" (fd_id Unix.stderr = err_before);
  check_int "subprocess exit status" ~expected:0 ~actual:!sub_status;
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
  check "one .output file at <log_dir>/<suite>/<groups...>" (log_file <> None);
  match log_file with
  | Some name ->
      let path = Filename.concat group_dir name in
      let content = read_file path in
      List.iter
        (fun fragment ->
          check ("file captures " ^ fragment) (contains content fragment))
        [ "chan-out."; "chan-err."; "fmt-out."; "raw-out."; "sub-out." ];
      (* Restored streams no longer feed the file. *)
      let size_after = (Unix.stat path).Unix.st_size in
      print_string "\n";
      flush stdout;
      check_int "post-capture writes do not reach the file" ~expected:size_after
        ~actual:(Unix.stat path).Unix.st_size
  | None -> ()

(* Incremental consumption *)

let test_incremental_consumption () =
  with_temp_root @@ fun root ->
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  check_string "output before any capture is empty" ~expected:""
    ~actual:(Capture.output cap);
  check "output_tail before any capture is None" (Capture.output_tail cap = None);
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
  check_string "window at attempt start is empty" ~expected:"" ~actual:!o0;
  check_string "first window" ~expected:"alpha" ~actual:!o1;
  check_string "second window drains channel and formatter"
    ~expected:"betagamma" ~actual:!o2;
  check_string "consumed bytes are not returned again" ~expected:"" ~actual:!o3;
  check_string "fd-level bytes appear in the window" ~expected:"delta"
    ~actual:!o4;
  check_string "output after the attempt sees nothing new" ~expected:""
    ~actual:(Capture.output cap);
  match Capture.output_tail cap with
  | None -> check "output_tail present after the attempt" false
  | Some tail ->
      check_string "tail covers the whole file despite consumption"
        ~expected:"alphabetagammadelta" ~actual:tail.Failure.text;
      check_int "nothing omitted" ~expected:0 ~actual:tail.Failure.omitted_bytes;
      check "tail names the log file" (tail.Failure.log_path <> None)

(* Exception safety *)

exception Boom

let test_exception_restores () =
  with_temp_root @@ fun root ->
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
  check "the body's exception propagates unchanged" propagated;
  check "descriptor 1 is restored after a raise" (fd_id Unix.stdout = out_before);
  match Capture.output_tail cap with
  | None -> check "output_tail present after a raise" false
  | Some tail ->
      check_string "buffered output is drained to the file on the raise path"
        ~expected:"pre-raise" ~actual:tail.Failure.text

let test_setup_failure_isolation () =
  with_temp_root @@ fun root ->
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
  check "a failed setup raises" raised;
  check "descriptor 1 untouched by a failed setup"
    (fd_id Unix.stdout = out_before);
  check "descriptor 2 untouched by a failed setup"
    (fd_id Unix.stderr = err_before);
  check "a failed setup does not expose the previous attempt's tail"
    (Capture.output_tail cap = None);
  check_string "output after a failed setup is empty" ~expected:""
    ~actual:(Capture.output cap)

let test_drain_failure_restores () =
  with_temp_root @@ fun root ->
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
  check "the failed cleanup drain propagates as Finally_raised"
    (outcome = `Finally_raised);
  check "descriptor 1 is restored despite the failed drain"
    (fd_id Unix.stdout = out_before);
  check "descriptor 2 is restored despite the failed drain"
    (fd_id Unix.stderr = err_before);
  (* The failed flush left the byte buffered; descriptor 2 is valid again, so
     drain it to the real stderr and leave later tests a clean channel. *)
  try flush stderr with Sys_error _ -> ()

(* Disabled ([--stream]) *)

let stream_error = "this test requires capture; rerun without --stream"

let test_disabled () =
  let cap = Capture.disabled in
  check "output_tail is None under Disabled" (Capture.output_tail cap = None);
  let value =
    Capture.with_capture cap ~groups:[ "g" ] ~test_name:"t" (fun () -> 7)
  in
  check_int "with_capture runs the body directly" ~expected:7 ~actual:value;
  (match Capture.output ~pos:("test_capture.ml", 42, 3, 9) cap with
  | _ -> check "output under Disabled raises Check_failure" false
  | exception Failure.Check_failure f -> (
      (match f.Failure.kind with
      | Failure.Message m ->
          check_string "the typed requires-capture message"
            ~expected:stream_error ~actual:m
      | _ -> check "failure kind is Message" false);
      match f.Failure.loc with
      | Some l ->
          check_string "failure location file comes from ?pos"
            ~expected:"test_capture.ml" ~actual:l.Loc.file;
          check_int "failure location line comes from ?pos" ~expected:42
            ~actual:l.Loc.line
      | None -> check "failure carries the ?pos location" false)
  | exception _ -> check "output under Disabled raises Check_failure" false);
  (* The realistic path: output () called inside a streamed test body. *)
  let saw = ref false in
  ignore
    (Capture.with_capture cap ~groups:[] ~test_name:"t" (fun () ->
         (match Capture.output cap with
         | _ -> ()
         | exception Failure.Check_failure _ -> saw := true);
         0));
  check "output raises at the call site inside a streamed body" !saw

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
      check ("tail present (" ^ name ^ ")") false;
      Failure.tail ""

let capture_string cap ~test_name payload =
  Capture.with_capture cap ~groups:[] ~test_name (fun () ->
      print_string payload);
  tail_of test_name cap

let test_bounded_tail () =
  with_temp_root @@ fun root ->
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  let letters n =
    String.init n (fun i -> Char.chr (Char.code 'a' + (i mod 26)))
  in
  let payload = letters (bound + 3_000) in
  let tail = capture_string cap ~test_name:"big" payload in
  check_string "tail is exactly the final tail_bytes"
    ~expected:(String.sub payload 3_000 bound)
    ~actual:tail.Failure.text;
  check_int "omitted_bytes counts everything before the tail" ~expected:3_000
    ~actual:tail.Failure.omitted_bytes;
  check_int "omitted + retained accounts for every byte"
    ~expected:(String.length payload)
    ~actual:(tail.Failure.omitted_bytes + String.length tail.Failure.text);
  (match tail.Failure.log_path with
  | Some p ->
      check_string "the log file holds the complete output" ~expected:payload
        ~actual:(read_file p)
  | None -> check "log_path present" false);
  (* Output exactly at the bound is complete: no cut, no skip. *)
  let exact = letters bound in
  let tail = capture_string cap ~test_name:"exact" exact in
  check_string "output at exactly tail_bytes is retained whole" ~expected:exact
    ~actual:tail.Failure.text;
  check_int "output at exactly tail_bytes omits nothing" ~expected:0
    ~actual:tail.Failure.omitted_bytes;
  (* Output below the bound is complete. *)
  let tail = capture_string cap ~test_name:"small" "tiny" in
  check_string "small output is retained whole" ~expected:"tiny"
    ~actual:tail.Failure.text;
  check_int "small output omits nothing" ~expected:0
    ~actual:tail.Failure.omitted_bytes

let repeat n s = String.concat "" (List.init n (fun _ -> s))

let test_utf8_boundary () =
  with_temp_root @@ fun root ->
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  (* Three-byte scalars: the bound is not a multiple of 3, so the suffix
     read starts one byte past a lead and two continuation bytes go. *)
  let euro = "\xE2\x82\xAC" in
  let chars = (bound / 3) + 100 in
  let payload = repeat chars euro in
  let cut = String.length payload - bound in
  check_int "the payload puts the cut one byte past a lead" ~expected:1
    ~actual:(cut mod 3);
  let tail = capture_string cap ~test_name:"t" payload in
  check_int "the mid-sequence bytes are skipped" ~expected:(bound - 2)
    ~actual:(String.length tail.Failure.text);
  check "the tail starts on a UTF-8 boundary"
    (String.length tail.Failure.text > 0
    && Char.code tail.Failure.text.[0] land 0xC0 <> 0x80);
  check_int "the skipped bytes count as omitted" ~expected:(cut + 2)
    ~actual:tail.Failure.omitted_bytes;
  check_string "the tail is whole characters"
    ~expected:(repeat ((bound - 2) / 3) euro)
    ~actual:tail.Failure.text

let test_utf8_max_skip () =
  with_temp_root @@ fun root ->
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  (* Four-byte scalars would align with the bound exactly; the trailing
     one-byte 'z' shifts the run so the suffix starts one byte after a lead
     and three continuation bytes must be skipped — the maximum. *)
  let pile = "\xF0\x9F\x92\xA9" in
  let payload = repeat ((bound / 4) + 10) pile ^ "z" in
  let cut = String.length payload - bound in
  check_int "the payload puts the cut one byte past a lead" ~expected:1
    ~actual:(cut mod 4);
  let tail = capture_string cap ~test_name:"t" payload in
  check_string "three continuation bytes are skipped"
    ~expected:(String.sub payload (cut + 3) (bound - 3))
    ~actual:tail.Failure.text;
  check "the tail starts on a UTF-8 boundary"
    (Char.code tail.Failure.text.[0] land 0xC0 <> 0x80);
  check_int "the three skipped bytes count as omitted" ~expected:(cut + 3)
    ~actual:tail.Failure.omitted_bytes

let test_invalid_utf8_verbatim () =
  with_temp_root @@ fun root ->
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  (* Continuation-byte flood: no lead within reach, so nothing is skipped and
     the suffix is kept verbatim. *)
  let payload = String.make (bound + 56) '\x80' in
  let tail = capture_string cap ~test_name:"t" payload in
  check_string "invalid UTF-8 is kept verbatim"
    ~expected:(String.make bound '\x80') ~actual:tail.Failure.text;
  check_int "no extra bytes counted omitted" ~expected:56
    ~actual:tail.Failure.omitted_bytes

(* Per-attempt reset *)

let test_per_attempt_reset () =
  with_temp_root @@ fun root ->
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  let w1 = ref "?" and w2 = ref "?" in
  Capture.with_capture cap ~groups:[ "g" ] ~test_name:"t" (fun () ->
      print_string "first-attempt";
      w1 := Capture.output cap);
  Capture.with_capture cap ~groups:[ "g" ] ~test_name:"t" (fun () ->
      print_string "second";
      w2 := Capture.output cap);
  check_string "attempt 1 window" ~expected:"first-attempt" ~actual:!w1;
  check_string "attempt 2 starts from a truncated file and reset cursor"
    ~expected:"second" ~actual:!w2;
  match Capture.output_tail cap with
  | None -> check "tail present after retries" false
  | Some tail ->
      check_string "only the final attempt's output remains" ~expected:"second"
        ~actual:tail.Failure.text;
      check_int "the final attempt omits nothing" ~expected:0
        ~actual:tail.Failure.omitted_bytes

(* Sanitized layout *)

let test_sanitized_layout () =
  with_temp_root @@ fun root ->
  let cap = Capture.create ~log_dir:root ~suite:"my suite" () in
  Capture.with_capture cap ~groups:[ "a/b" ] ~test_name:"x:y" (fun () ->
      print_string "content");
  let suite_dir =
    Filename.concat root (Path_ops.sanitize_component "my suite")
  in
  check "the suite name is sanitized into one component"
    (Sys.is_directory suite_dir);
  let path =
    Filename.concat
      (Filename.concat suite_dir (Path_ops.sanitize_component "a/b"))
      (Path_ops.sanitize_component "x:y" ^ ".output")
  in
  check "group and test components are sanitized" (Sys.file_exists path);
  check "a slash in a group makes one component, not two"
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
  with_temp_root @@ fun root ->
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
  check "each attempt names its log file" (colon <> "" && comma <> "");
  check "names differing only in punctuation get distinct log files"
    (colon <> comma);
  check_string "the first name's log still holds its own output"
    ~expected:"output of parse: empty" ~actual:(read_file colon);
  check_string "the second name's log holds its own output"
    ~expected:"output of parse, empty" ~actual:(read_file comma)

(* Stable paths *)

let test_stable_paths () =
  (* The log path is a function of the test's identity, so a second run of
     the same suite writes the same file — which is what makes a path
     printed in a failure report worth typing into an editor. *)
  with_temp_root @@ fun root ->
  let log_of cap =
    Capture.with_capture cap ~groups:[ "g" ] ~test_name:"t" (fun () ->
        print_string "x");
    match Capture.output_tail cap with
    | Some { Failure.log_path = Some path; _ } -> path
    | _ -> ""
  in
  let first = log_of (Capture.create ~log_dir:root ~suite:"s" ()) in
  let second = log_of (Capture.create ~log_dir:root ~suite:"s" ()) in
  check_string "a rerun writes the same path" ~expected:first ~actual:second;
  check_string "and the path is the test's identity under the suite"
    ~expected:(concat_all root [ "s"; "g"; "t.output" ])
    ~actual:first

(* Saved descriptors are close-on-exec (cli/F-5) *)

(* The re-exec'd child: captures, spawns a 10-second sleeper whose own
   stdio is /dev/null, and exits. Under capture the child's real stdout is
   the parent's pipe; only the saved dups could leak it to the sleeper. *)
let child_spawn_holder log_dir =
  let cap = Capture.create ~log_dir ~suite:"cloexec" () in
  Capture.with_capture cap ~groups:[] ~test_name:"spawn" (fun () ->
      let null = Unix.openfile "/dev/null" [ Unix.O_RDWR ] 0 in
      let pid =
        Unix.create_process "/bin/sleep" [| "sleep"; "10" |] null null null
      in
      Unix.close null;
      ignore pid);
  exit 0

let test_saved_descriptors_are_cloexec () =
  (* cli/F-5: capture's saved dups of the real stdout/stderr must be
     close-on-exec — an exec'd child that outlives the run must not hold
     the runner's stdout open, or a piped reader (`suite.exe | cat`, dune
     runtest) waits on the child after the suite finished. EOF on the
     child's pipe must arrive when the child exits (milliseconds), not
     when its sleeper dies (10 s). *)
  if Sys.win32 then skip ~reason:"no /bin/sleep on Windows" ();
  with_temp_root @@ fun root ->
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
  let started = Unix.gettimeofday () in
  let chunk = Bytes.create 4096 in
  let rec drain () =
    if Unix.read out_read chunk 0 (Bytes.length chunk) > 0 then drain ()
  in
  drain ();
  let elapsed = Unix.gettimeofday () -. started in
  Unix.close out_read;
  (match Unix.waitpid [] pid with
  | _, Unix.WEXITED 0 -> ()
  | _ -> fail "cloexec child did not exit cleanly");
  check "the pipe closes when the suite exits, not when its sleeper dies"
    (elapsed < 5.0)

(* Re-exec dispatch for the child above; this suite's own toplevel calls
   it before its run. Never returns for a child invocation. *)
let dispatch_child () =
  match Array.to_list Sys.argv with
  | [ _; "--capture-cloexec-child"; log_dir ] -> child_spawn_holder log_dir
  | _ -> ()

(* The log fd itself is close-on-exec *)

let test_log_fd_is_cloexec () =
  (* The companion of cli/F-5: the .output fd must be as close-on-exec as
     the saved dups. An exec'd child writes through the redirected fds 1-2
     and must not also inherit the raw log fd. The child probes which of
     its fds 3-9 alias its own stdout — the log file during capture — so
     unrelated descriptors open in this process cannot trip it. *)
  if Sys.win32 then skip ~reason:"no /bin/sh on Windows" ();
  with_temp_root @@ fun root ->
  let cap = Capture.create ~log_dir:root ~suite:"cloexec" () in
  let status = ref (-1) in
  Capture.with_capture cap ~groups:[] ~test_name:"probe" (fun () ->
      status :=
        Sys.command
          "for fd in 3 4 5 6 7 8 9; do if [ /dev/fd/$fd -ef /dev/fd/1 ]; then \
           echo \"LEAK:$fd\"; fi; done; echo probed");
  check_int "probe shell exits 0" ~expected:0 ~actual:!status;
  let content = read_file (concat_all root [ "cloexec"; "probe.output" ]) in
  check "the child still writes through the redirected fd 1"
    (contains content "probed");
  check "no fd aliasing the log file leaks to the child"
    (not (contains content "LEAK:"))

let tests =
  [
    test "fd-level round trip into the per-test file" test_fd_round_trip;
    test "incremental consumption windows" test_incremental_consumption;
    test "an exception restores the descriptors" test_exception_restores;
    test "a failed setup leaves the descriptors untouched"
      test_setup_failure_isolation;
    test "a failed cleanup drain still restores" test_drain_failure_restores;
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
let () = Windtrap.run "capture" tests
