(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Capture = Windtrap.Private.Capture
module Failure = Windtrap.Private.Failure
module Loc = Windtrap.Private.Loc
module Os = Windtrap.Private.Os

let strf = Printf.sprintf
let read path = In_channel.with_open_bin path In_channel.input_all
let write path s = Out_channel.with_open_bin path (fun oc -> output_string oc s)
let repeat n s = String.concat "" (List.init n (fun _ -> s))
let letters n = String.init n (fun i -> Char.chr (Char.code 'a' + (i mod 26)))
let posix_only () = if Sys.win32 then skip ~reason:"POSIX only" ()

(* The regular files under [dir], relative to it, sorted. *)
let rec files dir =
  let under name =
    let path = Filename.concat dir name in
    if Sys.is_directory path then List.map (Filename.concat name) (files path)
    else [ name ]
  in
  List.concat_map under
    (List.sort String.compare (Array.to_list (Sys.readdir dir)))

(* A state that captures under a fresh directory, and the log of its test
   [t] outside any group. *)
let state () =
  let root = temp_dir () in
  (Capture.create ~log_dir:root ~suite:"s" (), Filename.concat root "s/t.output")

(* An attempt nests inside the runner's own capture, so the real descriptors 1
   and 2 of a test are the runner's log. An assertion made inside an attempt
   would print into that attempt's log: the bodies return values, and the
   tests judge them after. *)
let attempt cap fn = Capture.with_capture cap ~groups:[] ~test_name:"t" fn

let identity fd =
  let st = Unix.fstat fd in
  (st.Unix.st_dev, st.Unix.st_ino)

let descriptors () = (identity Unix.stdout, identity Unix.stderr)

(* Where descriptors 1 and 2 point: where they pointed when [before] was
   taken, to the file [log], or elsewhere. *)
let pointing ~before ~log =
  let at fd before =
    let id = identity fd in
    if id = before then "as before"
    else if
      Sys.file_exists log
      &&
      let st = Unix.stat log in
      id = (st.Unix.st_dev, st.Unix.st_ino)
    then "log"
    else "elsewhere"
  in
  let out, err = before in
  strf "%s, %s" (at Unix.stdout out) (at Unix.stderr err)

let tail_row = function
  | None -> "none"
  | Some (t : Failure.tail) -> strf "%S, %d omitted" t.text t.omitted_bytes

(* [f ()] with a byte buffered for descriptor 2 and descriptor 2 read-only,
   so that a flush of [stderr] fails. A closed descriptor 2 would be reused
   by the first file the code under test opens. *)
let unwritable_stderr f =
  let saved = Unix.dup ~cloexec:true Unix.stderr in
  Fun.protect
    ~finally:(fun () ->
      Unix.dup2 saved Unix.stderr;
      Unix.close saved;
      try flush stderr with Sys_error _ -> ())
    (fun () ->
      prerr_string " ";
      let read_only = Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0 in
      Unix.dup2 read_only Unix.stderr;
      Unix.close read_only;
      f ())

(* The message and location of the failure that [f ()] raises. *)
let refused f =
  match f () with
  | _ -> "no failure"
  | exception Failure.Check_failure { Failure.kind = Failure.Message m; loc; _ }
    ->
      let at =
        match loc with
        | Some l -> strf "%s:%d" l.Loc.file l.Loc.line
        | None -> "no location"
      in
      strf "%s, at %s" m.Failure.kept at
  | exception Failure.Check_failure _ -> "a failure that is not a message"

(* Capture state *)

let creates_nothing () =
  let log_dir = Filename.concat (temp_dir ()) "logs" in
  let cap = Capture.create ~log_dir ~suite:"s" () in
  let before = Sys.file_exists log_dir in
  Capture.with_capture cap ~groups:[] ~test_name:"t" ignore;
  equal
    (pair bool (list string))
    (false, [ "s/t.output" ])
    (before, files log_dir)

let logs_of ~suite ~groups ~test_name =
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite () in
  Capture.with_capture cap ~groups ~test_name ignore;
  files root

let sanitized components =
  String.concat "/" (List.map Os.sanitize_component components)

let layouts =
  [
    ("plain names", ("s", [ "outer"; "inner" ], "t"), "s/outer/inner/t.output");
    ("no group", ("s", [], "t"), "s/t.output");
    ( "names with a space, a slash and a colon",
      ("my suite", [ "a/b" ], "x:y"),
      sanitized [ "my suite"; "a/b"; "x:y" ] ^ ".output" );
    ( "names that would leave the directory",
      (".", [ ".." ], ""),
      sanitized [ "."; ".."; "" ] ^ ".output" );
  ]

let punctuation () =
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  let names = [ "parse: empty"; "parse, empty" ] in
  List.iter
    (fun name ->
      Capture.with_capture cap ~groups:[] ~test_name:name (fun () ->
          print_string name))
    names;
  equal
    (slist string String.compare)
    names
    (List.map (fun f -> read (Filename.concat root f)) (files root))

let same_log () =
  let root = temp_dir () in
  List.iter
    (fun text ->
      let cap = Capture.create ~log_dir:root ~suite:"s" () in
      Capture.with_capture cap ~groups:[ "g" ] ~test_name:"t" (fun () ->
          print_string text))
    [ "first"; "second" ];
  equal
    (list (pair string string))
    [ ("s/g/t.output", "second") ]
    (List.map (fun f -> (f, read (Filename.concat root f))) (files root))

let disabled_abandon () =
  let cap, log = state () in
  let before = descriptors () in
  equal string "log, log"
    (attempt cap (fun () ->
         Capture.abandon Capture.disabled;
         pointing ~before ~log))

let stream_refusal =
  "this test requires capture; rerun without --stream, at test_capture.ml:42"

let refused_output () =
  Capture.output ~__POS__:("test_capture.ml", 42, 3, 9) Capture.disabled

let state_group =
  group "Capture state"
    [
      test "create makes nothing until an attempt runs" creates_nothing;
      cases
        "a log is <log_dir>/<suite>/<groups...>/<test>.output, each component \
         sanitized"
        ~name:(fun (name, _, _) -> name)
        layouts
        (fun (_, (suite, groups, test_name), log) ->
          equal (list string) [ log ] (logs_of ~suite ~groups ~test_name));
      test "names that differ only in punctuation have their own logs"
        punctuation;
      test "a second state of the same suite writes the same log over the first"
        same_log;
      test "disabled runs the function on the real descriptors" (fun () ->
          let before = descriptors () in
          equal string "as before, as before"
            (Capture.with_capture Capture.disabled ~groups:[ "g" ]
               ~test_name:"t" (fun () -> pointing ~before ~log:"")));
      test "disabled has no tail" (fun () ->
          equal string "none" (tail_row (Capture.output_tail Capture.disabled)));
      test "abandon on disabled leaves another state's attempt redirected"
        disabled_abandon;
      test "output on disabled fails the test at its position, naming --stream"
        (fun () -> equal string stream_refusal (refused refused_output));
      test "output fails the same way inside an attempt of disabled" (fun () ->
          equal string stream_refusal
            (Capture.with_capture Capture.disabled ~groups:[] ~test_name:"t"
               (fun () -> refused refused_output)));
    ]

(* Capturing *)

let drained write =
  let cap, log = state () in
  attempt cap (fun () ->
      write ();
      Capture.drain ();
      read log)

let buffers =
  [
    ("the standard output channel", fun () -> print_string "x");
    ("the standard error channel", fun () -> prerr_string "x");
    ("the standard formatter", fun () -> Format.printf "x");
    ("the error formatter", fun () -> Format.eprintf "x");
  ]

let own_formatter () =
  let ppf = Format.formatter_of_out_channel stdout in
  let seen = drained (fun () -> Format.fprintf ppf "x") in
  Format.pp_print_flush ppf ();
  equal string "" seen

let writers =
  buffers
  @ [
      ( "a write to descriptor 1",
        fun () -> ignore (Unix.write_substring Unix.stdout "x" 0 1) );
      ( "a write to descriptor 2",
        fun () -> ignore (Unix.write_substring Unix.stderr "x" 0 1) );
      ("a subprocess", fun () -> ignore (Sys.command "printf x"));
    ]

let arrival_order () =
  let cap, log = state () in
  attempt cap (fun () ->
      List.iter
        (fun (oc, s) ->
          output_string oc s;
          flush oc)
        [ (stdout, "1"); (stderr, "2"); (stdout, "3"); (stderr, "4") ]);
  equal string "1234" (read log)

let buffered_before () =
  let cap, log = state () in
  print_string "before";
  attempt cap (fun () -> print_string "inside");
  equal string "inside" (read log)

let written_after () =
  let cap, log = state () in
  attempt cap (fun () -> print_string "inside");
  print_string "after";
  flush stdout;
  equal string "inside" (read log)

let retried () =
  let cap, log = state () in
  let first =
    attempt cap (fun () ->
        print_string "first attempt";
        Capture.output cap)
  in
  let second =
    attempt cap (fun () ->
        print_string "second";
        let read = Capture.output cap in
        print_string ", rest";
        read)
  in
  equal (list string)
    [ "first attempt"; "second"; "second, rest"; "\", rest\", 0 omitted" ]
    [ first; second; read log; tail_row (Capture.output_tail cap) ]

(* What an attempt of [fn] comes to: what [with_capture] raised, whether [fn]
   ran, where descriptors 1 and 2 point after it, and the tail of [cap]. A
   drain that failed leaves its byte in [stderr], flushed after the attempt
   to the real descriptor 2. *)
let attempt_row ?(groups = []) cap fn =
  let before = descriptors () in
  let ran = ref false in
  let raised =
    match
      Capture.with_capture cap ~groups ~test_name:"t" (fun () ->
          ran := true;
          fn ())
    with
    | () -> "returned"
    | exception Unix.Unix_error _ -> "raised Unix_error"
    | exception Sys_error _ -> "raised Sys_error"
    | exception e -> "raised " ^ Printexc.to_string e
  in
  (try flush stderr with Sys_error _ -> ());
  strf "%s, %s, descriptors %s, tail %s" raised
    (if !ran then "ran" else "did not run")
    (pointing ~before ~log:"")
    (tail_row (Capture.output_tail cap))

let unwritable_log () =
  let root = temp_dir () in
  let cap = Capture.create ~log_dir:root ~suite:"s" () in
  Capture.with_capture cap ~groups:[] ~test_name:"before" (fun () ->
      print_string "before");
  write (Filename.concat root "s/g") "";
  attempt_row ~groups:[ "g" ] cap ignore

let failed_first_drain () =
  posix_only ();
  let cap, _ = state () in
  attempt cap (fun () -> print_string "before");
  unwritable_stderr (fun () -> attempt_row cap ignore)

let failed_last_drain ~raising () =
  let cap, _ = state () in
  attempt_row cap (fun () ->
      print_string "x";
      prerr_string " ";
      Unix.close Unix.stderr;
      if raising then raise Not_found)

let exits =
  [
    ( "the function returns",
      (fun () ->
        let cap, _ = state () in
        attempt_row cap (fun () -> print_string "x")),
      "returned, ran, descriptors as before, as before, tail \"x\", 0 omitted"
    );
    ( "the function raises",
      (fun () ->
        let cap, _ = state () in
        attempt_row cap (fun () ->
            print_string "x";
            failwith "boom")),
      "raised Failure(\"boom\"), ran, descriptors as before, as before, tail \
       \"x\", 0 omitted" );
    ( "the log cannot be created",
      unwritable_log,
      "raised Unix_error, did not run, descriptors as before, as before, tail \
       none" );
    ( "the first drain fails",
      failed_first_drain,
      "raised Sys_error, did not run, descriptors as before, as before, tail \
       none" );
    ( "the last drain fails",
      failed_last_drain ~raising:false,
      "raised Sys_error, ran, descriptors as before, as before, tail \"x\", 0 \
       omitted" );
    ( "the function raises and the last drain fails",
      failed_last_drain ~raising:true,
      "raised Not_found, ran, descriptors as before, as before, tail \"x\", 0 \
       omitted" );
  ]

(* A shell that prints each of its descriptors from 3 to 63 that is open on
   the file of its descriptor 0 or of its descriptor 1. A duplicate takes the
   lowest free descriptor, and this process holds far fewer than 64. On macOS
   a [/dev/fd] path compares equal to another [/dev/fd] path only, never to
   the file's own path. *)
let probe =
  {|fd=3
while [ $fd -lt 64 ]; do
  if [ /dev/fd/$fd -ef /dev/fd/0 ]; then echo "saved $fd"; fi
  if [ /dev/fd/$fd -ef /dev/fd/1 ]; then echo "log $fd"; fi
  fd=$((fd + 1))
done
echo probed|}

(* The inner attempt saves descriptors that point to the outer log, which the
   probe reads as its standard input. *)
let inherits_nothing () =
  posix_only ();
  let root = temp_dir () in
  let outer = Capture.create ~log_dir:root ~suite:"outer" () in
  let inner = Capture.create ~log_dir:root ~suite:"inner" () in
  let stdin = Filename.concat root "outer/t.output" in
  let command = Filename.quote_command "sh" ~stdin [ "-c"; probe ] in
  attempt outer (fun () ->
      attempt inner (fun () -> ignore (Sys.command command)));
  equal string "probed\n" (read (Filename.concat root "inner/t.output"))

let abandoned () =
  let cap, log = state () in
  let before = descriptors () in
  let inside =
    attempt cap (fun () ->
        print_string "buffered";
        Capture.abandon cap;
        pointing ~before ~log)
  in
  let after = pointing ~before ~log in
  equal (list string)
    [
      "as before, as before";
      "as before, as before";
      "buffered";
      "\"buffered\", 0 omitted";
    ]
    [ inside; after; read log; tail_row (Capture.output_tail cap) ]

let abandon_failed_drain () =
  let cap, log = state () in
  let before = descriptors () in
  let inside =
    attempt cap (fun () ->
        prerr_string " ";
        Unix.close Unix.stderr;
        Capture.abandon cap;
        pointing ~before ~log)
  in
  (try flush stderr with Sys_error _ -> ());
  equal string "as before, as before" inside

let abandon_unredirected () =
  let cap, log = state () in
  let before = descriptors () in
  Capture.abandon cap;
  attempt cap ignore;
  Capture.abandon cap;
  equal string "as before, as before" (pointing ~before ~log)

let capturing =
  group "Capturing"
    [
      cases "drain forces the standard buffers through to descriptors 1 and 2"
        ~name:fst buffers (fun (_, write) -> equal string "x" (drained write));
      test "drain reaches no formatter of the user's" own_formatter;
      test "drain raises Sys_error when a flush fails" (fun () ->
          posix_only ();
          raises_match Exn.sys_error (fun () -> unwritable_stderr Capture.drain));
      test "with_capture is what its function returns" (fun () ->
          let cap, _ = state () in
          equal int 42 (attempt cap (fun () -> 42)));
      test "descriptors 1 and 2 point to the log while the function runs"
        (fun () ->
          let cap, log = state () in
          let before = descriptors () in
          equal string "log, log"
            (attempt cap (fun () -> pointing ~before ~log)));
      cases
        "every write to descriptors 1 and 2 reaches the log, a subprocess's \
         included"
        ~name:fst writers (fun (_, write) ->
          let cap, log = state () in
          attempt cap write;
          equal string "x" (read log));
      test "standard output and error are one text, in the order of arrival"
        arrival_order;
      test "what was buffered before an attempt stays out of its log"
        buffered_before;
      test "writes after an attempt do not reach its log" written_after;
      test "a retry truncates the log and starts the cursor over" retried;
      cases
        "with_capture puts the descriptors back on every exit, and keeps a log \
         only of an attempt that ran"
        ~name:(fun (name, _, _) -> name)
        exits
        (fun (_, scenario, row) -> equal string row (scenario ()));
      test "a subprocess inherits no descriptor beyond 1 and 2" inherits_nothing;
      test
        "abandon puts the real descriptors back from inside an attempt, \
         draining into the log"
        abandoned;
      test "abandon ignores a failed drain" abandon_failed_drain;
      test "abandon with nothing redirected restores nothing"
        abandon_unredirected;
    ]

(* Reading captured output *)

let windows () =
  let cap, _ = state () in
  let during =
    attempt cap (fun () ->
        let at_start = Capture.output cap in
        print_string "alpha";
        let first = Capture.output cap in
        print_string "beta";
        Format.printf "gamma";
        let buffered = Capture.output cap in
        let again = Capture.output cap in
        ignore (Unix.write_substring Unix.stdout "delta" 0 5);
        let raw = Capture.output cap in
        print_string "epsilon";
        [ at_start; first; buffered; again; raw ])
  in
  let after = Capture.output cap in
  let last = Capture.output cap in
  equal (list string)
    [ ""; "alpha"; "betagamma"; ""; "delta"; "epsilon"; "" ]
    (during @ [ after; last ])

let log_gone () =
  posix_only ();
  let cap, log = state () in
  let first, refusal, back =
    attempt cap (fun () ->
        print_string "abc";
        let first = Capture.output cap in
        print_string "def";
        Sys.rename log (log ^ ".away");
        let refusal =
          refused (fun () ->
              Capture.output ~__POS__:("test_capture.ml", 7, 0, 5) cap)
        in
        Sys.rename (log ^ ".away") log;
        (first, refusal, Capture.output cap))
  in
  equal string
    (strf
       "this test's captured output cannot be read: %s: No such file or \
        directory, at test_capture.ml:7"
       log)
    refusal;
  equal (pair string string) ("abc", "def") (first, back)

let truncated () =
  let cap, _ = state () in
  let rows =
    attempt cap (fun () ->
        print_string "abcdef";
        let read = Capture.output cap in
        Unix.ftruncate Unix.stdout 2;
        let after = Capture.output cap in
        [ read; after; tail_row (Capture.output_tail cap) ])
  in
  equal (list string) [ "abcdef"; ""; "\"\", 0 omitted" ] rows

(* The tail after an attempt that wrote [read], read it with [output], then
   wrote [unread]: its length, the bytes it omits, whether it is the end of
   [unread], and whether it starts on a code point. *)
let tail_of ~read unread =
  let cap, _ = state () in
  attempt cap (fun () ->
      print_string read;
      ignore (Capture.output cap);
      print_string unread);
  let tail = require_some (Capture.output_tail cap) in
  let text = tail.Failure.text in
  strf "%d bytes, %d omitted, %s, %s" (String.length text)
    tail.Failure.omitted_bytes
    (if String.ends_with ~suffix:text unread then "the end" else "not the end")
    (if text = "" || Char.code text.[0] land 0xC0 <> 0x80 then "on a code point"
     else "inside a sequence")

let after_cursor =
  [
    ("nothing after the last output", "", "0 bytes, 0 omitted");
    ("5 bytes after it", "after", "5 bytes, 0 omitted");
    ("8,292 bytes after it", String.make 8_292 'x', "8192 bytes, 100 omitted");
  ]

let bounded =
  [
    ("4 bytes", "tiny", "4 bytes, 0 omitted, the end, on a code point");
    ( "8,192 bytes",
      letters 8_192,
      "8192 bytes, 0 omitted, the end, on a code point" );
    ( "11,192 bytes",
      letters 11_192,
      "8192 bytes, 3000 omitted, the end, on a code point" );
    ( "three-byte code points, cut one byte past a lead",
      repeat 2_830 "\xE2\x82\xAC",
      "8190 bytes, 300 omitted, the end, on a code point" );
    ( "four-byte code points and a letter, cut one byte past a lead",
      repeat 2_058 "\xF0\x9F\x92\xA9" ^ "z",
      "8189 bytes, 44 omitted, the end, on a code point" );
    ( "continuation bytes with no lead in reach",
      String.make 8_248 '\x80',
      "8192 bytes, 56 omitted, the end, inside a sequence" );
    ( "a continuation byte in a tail that is not cut",
      "\x80abc",
      "4 bytes, 0 omitted, the end, inside a sequence" );
  ]

let whole_log () =
  let cap, log = state () in
  let payload = letters 11_192 in
  attempt cap (fun () -> print_string payload);
  let tail = require_some (Capture.output_tail cap) in
  equal (option string) (Some log) tail.Failure.log_path;
  equal string payload (read log)

let reading =
  group "Reading captured output"
    [
      cases "with no current log, output is \"\" and output_tail is None"
        ~name:fst
        [
          ("a new state", fun () -> fst (state ()));
          ( "after a failed setup",
            fun () ->
              let cap, log = state () in
              write (Filename.dirname log) "";
              (try attempt cap ignore with Unix.Unix_error _ -> ());
              cap );
        ]
        (fun (_, make) ->
          let cap = make () in
          equal (pair string string) ("", "none")
            (Capture.output cap, tail_row (Capture.output_tail cap)));
      test "output is what the attempt wrote since the last call, drained first"
        windows;
      test
        "output fails the test when the log cannot be opened, naming the log, \
         and keeps its cursor"
        log_gone;
      test "a log truncated below the cursor has no unread bytes" truncated;
      cases
        "output_tail is the end of what the attempt wrote after output's cursor"
        ~name:(fun (name, _, _) -> name)
        after_cursor
        (fun (_, unread, row) ->
          equal string
            (row ^ ", the end, on a code point")
            (tail_of ~read:"compared" unread));
      cases "output_tail keeps at most 8,192 bytes, cut on a code point"
        ~name:(fun (name, _, _) -> name)
        bounded
        (fun (_, unread, row) -> equal string row (tail_of ~read:"" unread));
      test "the tail names the log, which holds every byte" whole_log;
      test "output_tail inside an attempt misses what a buffer holds" (fun () ->
          let cap, _ = state () in
          equal string "\"\", 0 omitted"
            (attempt cap (fun () ->
                 print_string "abc";
                 tail_row (Capture.output_tail cap))));
      test "output_tail drains nothing, so a flush that would fail is not made"
        (fun () ->
          posix_only ();
          let cap, _ = state () in
          attempt cap (fun () -> print_string "abc");
          equal string "\"abc\", 0 omitted"
            (unwritable_stderr (fun () -> tail_row (Capture.output_tail cap))));
      test "output_tail is None when the log can no longer be opened" (fun () ->
          let cap, log = state () in
          attempt cap (fun () -> print_string "abc");
          Sys.remove log;
          equal string "none" (tail_row (Capture.output_tail cap)));
    ]

let () = exit (run "capture" [ state_group; capturing; reading ])
