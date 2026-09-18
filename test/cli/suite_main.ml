(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The suite the cram sessions in this directory drive. What the facade's [run] does with a
   command line is the subject, so the suite itself stays small enough
   for the driver to pin its transcripts byte for byte: five tests, no
   clock, no network, one file baseline.

   Eight declarations, selected by FACADE_FIXTURE, because two of the
   things [run] refuses are properties of a suite rather than of a flag —
   a duplicate path and a committed focus — and neither can coexist with
   the tests every other scenario selects from; a flaky test, a noisy
   failing test, the streamed tests, and a test that fails beside a stale
   baseline likewise stand alone,
   so the transcripts every other session pins stay exactly what they
   are. *)

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

let () =
  exit
  @@ run "fixture"
       (match Sys.getenv_opt "FACADE_FIXTURE" with
       | Some "focus" -> focused
       | Some "duplicate" -> duplicate
       | Some "flaky" -> flaky
       | Some "noisy" -> noisy
       | Some "stream" -> streamed
       | Some "masked" -> masked
       | Some ("" | "default") | None -> default
       | Some other ->
           invalid_arg ("suite_main: unknown FACADE_FIXTURE " ^ other))
