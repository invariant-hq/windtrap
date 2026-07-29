(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Synthetic run data shared by the renderer tests: one result per failure
   kind (equality with diff, raise, snapshot missing and mismatch, property
   with inner failure, body + teardown pair), plus pass/skip/retry results,
   and a bounded captured tail with a drop count. Everything is deterministic:
   fixed durations, a fixed root seed, locations pointing at files that do not
   exist in the test's working directory (so source excerpts stay silent). *)

open Windtrap
open Windtrap.Private

let loc file line = { Loc.file; line; column = 2 }

let root =
  match Seed.of_string "s1:7be1d2c904aa31f5" with
  | Ok s -> s
  | Error e -> failwith e

let result ?(attempts = 1) ?(duration = 0.0002) ?prop_stats ?srandom_root
    ?(slow_tagged = false) ?xfail ?counted path outcome =
  (* [counted] defaults to the runner's rule (Runner.counts_failed): a
     [Fail] counts unless the test is xfail-annotated — then it is an
     excused expected failure; passes and skips never count. The
     unexpected-pass fixture overrides the default. *)
  let counted =
    match counted with
    | Some counted -> counted
    | None -> (
        match outcome with
        | Failure.Fail _ -> xfail = None
        | Failure.Pass | Failure.Skip _ -> false)
  in
  {
    Run.path;
    outcome;
    counted;
    xfail;
    slow_tagged;
    duration;
    attempts;
    prop_stats;
    srandom_root;
  }

let tail =
  Failure.tail
    ~log_path:"_build/_tests/mylib/latest/users/sessions-after-login.output"
    ~omitted_bytes:12034
    "[debug] session table resize 2 -> 4\n\
     [debug] carol: ghost session from pool reuse\n"

let eq_failure =
  Failure.equality
    ~loc:(loc "test/test_users.ml" 31)
    ~expected:{|[("alice", [1; 2; 3]); ("bob", [4])]|}
    ~actual:{|[("alice", [1; 2; 3]); ("bob", [4; 5]); ("carol", [])]|} ()

let raise_failure =
  Failure.raised
    ~loc:(loc "test/test_parser.ml" 12)
    ~expected:{|Parse_error("empty")|} ~actual:"Not_found"
    ~backtrace:"Raised at Parser.parse in file \"lib/parser.ml\", line 40" ()

let snap_missing =
  Failure.snapshot ~loc:(loc "test/test_cli.ml" 9) ~name:"help"
    ~path:"test/__snapshots__/test_cli/help.snap"
    (Failure.Missing
       { proposed = "Usage: mytool [OPTIONS] COMMAND\nCommands:\n  run\n" })

let snap_mismatch =
  Failure.snapshot
    ~loc:(loc "test/test_cli.ml" 14)
    ~name:"version" ~path:"test/__snapshots__/test_cli/version.snap"
    (Failure.Mismatch
       {
         expected = "line one\nline two\nline three\n";
         actual = "line one\nline 2\nline three\n";
       })

let prop_failure =
  Failure.property
    ~loc:(loc "test/test_geo.ml" 17)
    ~inner:
      (Failure.equality
         ~loc:(loc "test/test_geo.ml" 18)
         ~expected:"true" ~actual:"false" ())
    ~rendered:"Rect (2, 0)" ~case_index:12 ~shrink_steps:4 ~root ~examples:false
    ()

let body_teardown =
  [
    Failure.message ~loc:(loc "test/test_db.ml" 21) "body exploded";
    Failure.with_phase Failure.Teardown (Failure.message "teardown exploded");
  ]

(* ───── B-package fixtures (amendments B1/B12/B13) ─────
   Additions only: the values above feed byte-exact goldens. *)

(* B1: same constructor, different message payloads — the message-diff case. *)
let raise_message_failure =
  Failure.raised
    ~loc:(loc "test/test_bounds.ml" 8)
    ~expected:{|Invalid_argument("index 3 out of bounds")|}
    ~actual:{|Invalid_argument("index 4 out of bounds")|} ~same_constructor:true
    ~expected_message:"index 3 out of bounds"
    ~actual_message:"index 4 out of bounds" ()

(* B12: an xfail annotation, an excused failing result (annotated, not
   counted), and the runner's synthesized unexpected-pass result (annotated
   and counted — the record's bit keeps it loud). *)
let xfail_reason = { Test_tree.reason = Some "issue #42" }

let excused_result =
  result ~xfail:xfail_reason
    [ "known"; "broken carry" ]
    (Failure.Fail
       [
         Failure.equality
           ~loc:(loc "test/test_carry.ml" 5)
           ~expected:"1" ~actual:"2" ();
       ])

let xpass_result =
  result ~xfail:xfail_reason ~counted:true
    [ "known"; "fixed already" ]
    (Failure.Fail
       [
         Failure.message
           ~loc:(loc "test/test_carry.ml" 9)
           "expected to fail (issue #42), but the test passed";
       ])

(* B13: one test whose failure list mixes two subtest entries (labeled
   through the msg slot, as Run.subtest records them) and one plain body
   failure. *)
let subtest_failure name =
  {
    (Failure.equality
       ~loc:(loc "test/test_backend.ml" 40)
       ~expected:"[1; 2]" ~actual:"[1; 3]" ())
    with
    Failure.msg = Some ("contract \u{203a} " ^ name);
  }

let subtest_result =
  result [ "backend"; "contract" ]
    (Failure.Fail
       [
         subtest_failure "shape [0]";
         subtest_failure "shape [2]";
         Failure.message ~loc:(loc "test/test_backend.ml" 61) "final check";
       ])

let results =
  [
    result [ "math"; "addition" ] Failure.Pass ~duration:0.0001;
    result
      [ "users"; "sessions after login" ]
      (Failure.Fail [ Failure.with_output_tail tail eq_failure ]);
    result [ "parser"; "rejects empty" ] (Failure.Fail [ raise_failure ]);
    result [ "cli"; "cli help" ] (Failure.Fail [ snap_missing ])
      ~duration:0.0003;
    result [ "cli"; "snapshot drift" ] (Failure.Fail [ snap_mismatch ]);
    result
      [ "geo"; "area non-negative" ]
      (Failure.Fail [ prop_failure ]) ~duration:0.018;
    result [ "db"; "insert" ] (Failure.Fail body_teardown);
    result [ "flaky"; "eventually" ] Failure.Pass ~attempts:3;
    result [ "slow"; "big sort" ] Failure.Pass ~duration:2.5;
    result [ "slow"; "hash" ] Failure.Pass ~duration:3.0;
    result [ "platform"; "windows paths" ] (Failure.Skip (Some "unix only"));
  ]

let failed_count = 6
let duration = 6.5
