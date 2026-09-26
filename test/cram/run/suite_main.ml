(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The suite the sessions in this directory run. The default declaration
   is five tests, one of them failing and one reading a file baseline, so
   that a session selects the run it needs from the command line. Each
   other declaration, selected by FACADE_FIXTURE, stands alone because it
   is a property of the suite rather than of a flag: a duplicate path, a
   focus, a test that calls [exit], output that bypasses the capture, a
   failing property. *)

open Windtrap

let default =
  [
    group "math"
      [
        test "adds" (fun () -> equal int 4 (2 + 2));
        test "subtracts" (fun () -> equal int 0 (2 - 2));
      ];
    (* The one failing test: scenarios select it by name to fail a run and
       exclude it by name to pass one. *)
    test "boom" (fun () -> equal ~msg:"deliberate" int 1 2);
    slow "crawls" (fun () -> is_true true);
    (* The baseline the sessions plant, at a path both files agree on. *)
    test "greeting" (fun () ->
        expect_file "hello from the fixture\n" "test/cram/run/greeting.expected");
  ]

let focused =
  [
    focus (test "focused" (fun () -> is_true true));
    test "unfocused" (fun () -> is_true true);
  ]

let duplicate =
  [
    group "dup" [ test "twice" (fun () -> is_true true) ];
    group "dup" [ test "twice" (fun () -> is_true true) ];
  ]

(* Prints, then fails: the one test whose report carries a captured tail
   and the full log's path. *)
let noisy =
  [
    test "noisy" (fun () ->
        print_string "hello from noisy\n";
        equal ~msg:"deliberate" int 1 2);
  ]

(* Output that does not go through the report's formatter: left unflushed
   in the [stdout] channel, written straight to descriptor 1, and written
   by a subprocess through its own C stdio. *)
let streamed =
  [
    test "channel" (fun () -> print_string "through the stdout channel\n");
    test "descriptor" (fun () ->
        let line = "through descriptor 1\n" in
        ignore (Unix.write_substring Unix.stdout line 0 (String.length line)));
    test "subprocess" (fun () ->
        let pid =
          Unix.create_process "echo"
            [| "echo"; "through a subprocess" |]
            Unix.stdin Unix.stdout Unix.stderr
        in
        ignore (Unix.waitpid [] pid));
    test "fails" (fun () ->
        print_string "before the failure\n";
        equal ~msg:"deliberate" int 1 2);
  ]

(* A stale baseline beside a failing assertion: the run keeps no
   correction, whatever it was asked to do with one. *)
let masked =
  [
    test "masked" (fun () ->
        expect_file "fresh from the fixture\n" "test/cram/run/masked.expected";
        equal ~msg:"deliberate" int 1 2);
  ]

(* The second test calls [exit], which must not end the run: the third
   still runs, and fails. *)
let exits =
  [
    test "before" (fun () -> is_true true);
    test "bomb" (fun () -> Stdlib.exit 0);
    test "after" (fun () -> equal ~msg:"deliberate" int 1 2);
  ]

(* A property that fails on its first case: its report ends on the replay
   line, the command a report spells for the way the run was started. *)
let property = [ prop "boom" Gen.int (fun _ -> equal int 1 2) ]

(* Two properties that fail on a case the seed picks, beside a test that
   passes: one replay line reruns the two, each on its own case. *)
let properties =
  [
    test "passes" (fun () -> is_true true);
    prop "even" Gen.int (fun n -> is_true (n mod 2 = 0));
    prop "small" Gen.int (fun n -> is_true (abs n < 1000));
  ]

(* A literal that holds, a stale literal and a stale file baseline, beside
   a failing property: the one accept: line of the report, pasted, rewrites
   the two stale baselines and no other. *)
let accepts =
  [
    test "holds" (fun () -> expect "same" @@ __POS_OF__ "same");
    test "stale literal" (fun () -> expect "fresh" @@ __POS_OF__ "stale");
    test "stale file" (fun () ->
        expect_file "fresh from the fixture\n" "test/cram/run/accepts.expected");
    prop "small" Gen.int (fun n -> is_true (abs n < 1000));
  ]

(* The third test says it is ready, by creating [ready] in the working
   directory, and then waits for the signal that ends the run. *)
let waiting =
  [
    test "passes" (fun () -> is_true true);
    test "fails" (fun () -> equal ~msg:"deliberate" int 1 2);
    group "deep"
      [
        test "waits" (fun () ->
            print_string "captured, never shown\n";
            close_out (open_out "ready");
            Unix.sleepf 60.);
      ];
    test "never reached" (fun () -> is_true true);
  ]

let () =
  let tests =
    match Sys.getenv_opt "FACADE_FIXTURE" with
    | Some "focus" -> focused
    | Some "duplicate" -> duplicate
    | Some "noisy" -> noisy
    | Some "stream" -> streamed
    | Some "masked" -> masked
    | Some "exits" -> exits
    | Some "property" -> property
    | Some "properties" -> properties
    | Some "accepts" -> accepts
    | Some "waiting" -> waiting
    | Some ("" | "default") | None -> default
    | Some other -> invalid_arg ("suite_main: unknown FACADE_FIXTURE " ^ other)
  in
  exit (run "fixture" tests)
