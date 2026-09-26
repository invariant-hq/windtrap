(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The suite the cram sessions in this directory drive. What the facade's
   [run] does with a command line is the subject, so the suite itself
   stays small enough for the sessions to pin its transcripts byte for
   byte: the default declaration is five tests, no clock, no network, one
   file baseline.

   More declarations, selected by FACADE_FIXTURE, because two of the
   things [run] refuses are properties of a suite rather than of a flag
   (a duplicate path and a committed focus), and neither can coexist with
   the tests every other scenario selects from; a flaky test, a noisy
   failing test, the streamed tests, a test that fails beside a stale
   baseline, a retried test over a stale baseline, a stale baseline
   before a passing test, a test that calls [exit], a failing property,
   stale baselines beside a failing property, an expected failure, two
   fixtures whose release fails and a test that waits for a signal
   likewise stand alone, so the transcripts every other session pins stay
   exactly what they are. *)

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
        expect_file "hello from the fixture\n" "test/cli/greeting.expected");
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

(* Fails once, then passes: the retry is in-process, so a counter is
   the whole mechanism. *)
let flaky =
  let attempts = ref 0 in
  [
    test ~retries:1 "flaky" (fun () ->
        incr attempts;
        if !attempts = 1 then fail "first attempt");
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
   by a subprocess through its own C stdio. The last test fails, so its
   block is there to sit under its row. *)
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
        expect_file "fresh from the fixture\n" "test/cli/masked.expected";
        equal ~msg:"deliberate" int 1 2);
  ]

(* A stale baseline under [~retries]: deterministic, so a second attempt
   could only agree with what the first one recorded. *)
let retried =
  [
    test ~retries:1 "retried" (fun () ->
        expect_file "fresh from the fixture\n" "test/cli/retried.expected");
  ]

(* A stale baseline, then a test that passes: under [--corrected] the
   first test's only failure is a kept correction, and [-x] still stops
   the run on it. *)
let stops =
  [
    test "stale" (fun () ->
        expect_file "fresh from the fixture\n" "test/cli/stops.expected");
    test "after" (fun () -> is_true true);
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
        expect_file "fresh from the fixture\n" "test/cli/accepts.expected");
    prop "small" Gen.int (fun n -> is_true (abs n < 1000));
  ]

(* An expected failure whose own message is the sentence the runner
   writes for an unexpected pass: the report must still read it as the
   expected failure it is. *)
let collide =
  [
    xfail
      (test "collide" (fun () -> fail "expected to fail, but the test passed"));
  ]

(* A fixture whose release raises after the one test, which passes. *)
let leaky = fixture ~teardown:(fun () -> failwith "release-boom") ignore
let release = [ test "touches the fixture" (fun () -> leaky ()) ]

(* A fixture whose release starts a run of its own, while this one is
   still executing. *)
let nesting = fixture ~teardown:(fun () -> ignore (run "inner" [])) ignore
let nested = [ test "touches the fixture" (fun () -> nesting ()) ]

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

(* [no-argv] runs the property suite as a host that passes [run] no
   command line at all. *)
let () =
  let argv, tests =
    match Sys.getenv_opt "FACADE_FIXTURE" with
    | Some "focus" -> (Sys.argv, focused)
    | Some "duplicate" -> (Sys.argv, duplicate)
    | Some "flaky" -> (Sys.argv, flaky)
    | Some "noisy" -> (Sys.argv, noisy)
    | Some "stream" -> (Sys.argv, streamed)
    | Some "masked" -> (Sys.argv, masked)
    | Some "retried" -> (Sys.argv, retried)
    | Some "stops" -> (Sys.argv, stops)
    | Some "exits" -> (Sys.argv, exits)
    | Some "property" -> (Sys.argv, property)
    | Some "properties" -> (Sys.argv, properties)
    | Some "accepts" -> (Sys.argv, accepts)
    | Some "no-argv" -> ([||], property)
    | Some "collide" -> (Sys.argv, collide)
    | Some "release" -> (Sys.argv, release)
    | Some "nested" -> (Sys.argv, nested)
    | Some "waiting" -> (Sys.argv, waiting)
    | Some ("" | "default") | None -> (Sys.argv, default)
    | Some other -> invalid_arg ("suite_main: unknown FACADE_FIXTURE " ^ other)
  in
  exit (run ~argv "fixture" tests)
