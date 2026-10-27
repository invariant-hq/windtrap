(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Failure = Windtrap.Private.Failure
module Report_json = Windtrap.Private.Report_json
module Test_tree = Windtrap.Private.Test_tree
module Fixtures = Render_fixtures

let document ?(selected = 1) ?(release_failures = []) results =
  Report_json.render ~suite:"s" ~seed:Fixtures.root ~selected ~duration:0.5
    ~results ~release_failures ()

(* The line of the one test of a document. *)
let line result =
  match String.split_on_char '\n' (document [ result ]) with
  | _ :: test :: _ -> String.trim test
  | _ -> "no test line"

(* The closing counts of a document, its last object. *)
let counts ?selected ?release_failures results =
  let d = document ?selected ?release_failures results in
  let key = "\"counts\": " in
  let rec from i =
    if i < 0 then "no counts"
    else if String.sub d i (String.length key) = key then
      let start = i + String.length key in
      String.sub d start (String.length d - start - 2)
    else from (i - 1)
  in
  from (String.length d - String.length key)

let result ?xfail ?counted ?(duration = 0.25) path outcome =
  Fixtures.result ?xfail ?counted ~duration path outcome

let verdicts =
  [
    ( "a pass",
      ( result [ "p" ] Failure.Pass,
        {|{"path": ["p"], "verdict": "pass", "attempts": 1, "duration": 0.25, "failures": [], "scope": {"tolerances": []}},|}
      ) );
    ( "a failure, located",
      ( result [ "g"; "f" ] (Failure.Fail [ Fixtures.eq_failure ]),
        {|{"path": ["g", "f"], "verdict": "fail", "attempts": 1, "duration": 0.25, "failures": [{"headline": "expected [(\"alice\", [1; 2; 3]); (\"bob\", [4])], got [(\"alice\", [1; 2; 3]); (\"bob\"…", "file": "test/test_users.ml", "line": 31}], "scope": {"tolerances": []}},|}
      ) );
    ( "a vacuous test, with its reason",
      ( result [ "v" ] (Failure.Fail [ Failure.vacuous "no row" ]),
        {|{"path": ["v"], "verdict": "vacuous", "reason": "no row", "attempts": 1, "duration": 0.25, "failures": [{"headline": "vacuous: no row"}], "scope": {"tolerances": []}},|}
      ) );
    ( "a skip, with its reason",
      ( result [ "s" ] (Failure.Skip (Some "later")),
        {|{"path": ["s"], "verdict": "skip", "reason": "later", "attempts": 1, "duration": 0.25, "failures": [], "scope": {"tolerances": []}},|}
      ) );
    ( "an expected failure, vacuous or not",
      ( result
          ~xfail:{ Test_tree.reason = None }
          [ "x" ]
          (Failure.Fail [ Failure.vacuous "no row" ]),
        {|{"path": ["x"], "verdict": "xfail", "attempts": 1, "duration": 0.25, "failures": [{"headline": "vacuous: no row"}], "scope": {"tolerances": []}},|}
      ) );
  ]

(* A second test makes the first line end in a comma. *)
let first_line result =
  match String.split_on_char '\n' (document ~selected:2 [ result; result ]) with
  | _ :: test :: _ -> String.trim test
  | _ -> "no test line"

let encoding () =
  equal string
    {|{"path": ["q\"b\\s\n\t\r\u0001\u007fé �"], "verdict": "pass", "attempts": 1, "duration": null, "failures": [], "scope": {"tolerances": []}}|}
    (line
       (result ~duration:Float.nan
          [ "q\"b\\s\n\t\r\001\127\u{e9} \xff" ]
          Failure.Pass));
  equal string
    {|{"path": ["p"], "verdict": "pass", "attempts": 1, "duration": 0.1, "failures": [], "scope": {"tolerances": []}}|}
    (line (result ~duration:0.1 [ "p" ] Failure.Pass))

let closing_counts () =
  equal string
    {|{"tests": 6, "passed": 1, "failed": 2, "vacuous": 1, "skipped": 0, "expected_failures": 1, "flaky": 1, "not_run": 2, "release_failures": 1, "duration": 0.5}|}
    (counts ~selected:6
       ~release_failures:[ Fixtures.release_failure ]
       [
         Fixtures.result ~attempts:2 [ "flaky" ] Failure.Pass;
         result [ "f" ] (Failure.Fail [ Failure.message "m" ]);
         result [ "v" ] (Failure.Fail [ Failure.vacuous "no row" ]);
         result
           ~xfail:{ Test_tree.reason = None }
           [ "x" ]
           (Failure.Fail [ Failure.message "m" ]);
       ])

let () =
  exit
    (run "report_json"
       [
         cases "a test states its verdict, its reason and its failures" fst
           verdicts (fun (_, (r, expected)) ->
             equal string expected (first_line r));
         test
           "strings escape what JSON needs, malformed UTF-8 is U+FFFD and a \
            float not finite is null"
           encoding;
         test "the counts close the document as the summary counts"
           closing_counts;
       ])
