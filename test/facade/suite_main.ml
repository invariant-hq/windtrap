(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The suite test_facade.ml drives. What the facade's [run] does with a
   command line is the subject, so the suite itself stays small enough
   for the driver to pin its transcripts byte for byte: five tests, no
   clock, no network, one snapshot.

   Three declarations, selected by FACADE_FIXTURE, because two of the
   things [run] refuses are properties of a suite rather than of a flag —
   a duplicate path and a committed focus — and neither can coexist with
   the tests every other scenario selects from. *)

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
    (* [~pos] rather than the captured location: the baseline the driver
       plants is addressed by this file's name, and a heuristic is no
       basis for a path two files have to agree on. *)
    test ~pos:__POS__ "greeting" (fun () ->
        snapshot ~pos:__POS__ "greeting" "hello from the fixture\n");
  ]

let focus =
  [
    ftest "focused" (fun () -> is_true true);
    test "unfocused" (fun () -> is_true true);
  ]

let duplicate =
  [
    group "dup" [ test "twice" (fun () -> is_true true) ];
    group "dup" [ test "twice" (fun () -> is_true true) ];
  ]

let () =
  exit
  @@ run "fixture"
       (match Sys.getenv_opt "FACADE_FIXTURE" with
       | Some "focus" -> focus
       | Some "duplicate" -> duplicate
       | Some ("" | "default") | None -> default
       | Some other ->
           invalid_arg ("suite_main: unknown FACADE_FIXTURE " ^ other))
