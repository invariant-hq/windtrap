(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Report_junit: golden document over a small synthetic run,
   well-formedness of the full fixture run (checked with the minimal
   Xml_check parser), the flaky-pass note, the ANSI-in-JUnit
   impossibility, XML 1.0 range sanitization of hostile payloads,
   escaping, counts, the report's path, and the checker's own sanity. *)

open Windtrap
open Windtrap.Private
module Fixtures = Render_fixtures

let check name cond = is_true ~msg:name cond
let check_string name ~expected ~actual = equal ~msg:name string expected actual
let check_contains name ~sub s = Windtrap.contains ~msg:name ~sub s
let check_absent name ~sub s = not_contains ~msg:name ~sub s

let check_well_formed name doc =
  match Xml_check.check doc with
  | Ok () -> ()
  | Error m -> failf "%s: %s\n  in:\n%s" name m doc

(* The golden document *)

let small_results =
  [
    Fixtures.result [ "math"; "addition" ] Failure.Pass ~duration:0.0001;
    Fixtures.result
      [ "users"; "sessions after login" ]
      (Failure.Fail
         [ Failure.with_output_tail Fixtures.tail Fixtures.eq_failure ]);
    Fixtures.result
      [ "platform"; "windows paths" ]
      (Failure.Skip (Some "unix only"));
  ]

let test_golden () =
  let actual =
    Report_junit.render ~suite:"mylib" ~results:small_results ~duration:1.234 ()
  in
  expect_file actual "test/unit/expected/test_report_junit/document.expected";
  check_well_formed "golden document is well-formed" actual

(* The full fixture run *)

let full () =
  Report_junit.render ~suite:"mylib" ~results:Fixtures.results
    ~duration:Fixtures.duration ()

let test_full_run () =
  let doc = full () in
  check_well_formed "full fixture document is well-formed" doc;
  check_contains "counts derive from results"
    ~sub:{|tests="11" failures="6" errors="0" skipped="1" time="6.500"|} doc;
  check_contains "acceptance command inside failure text"
    ~sub:"accept: dune promote" doc;
  check_contains "replay line inside failure text"
    ~sub:
      "replay: WINDTRAP_SEED=s1:7be1d2c904aa31f5 WINDTRAP_FILTER='geo › area \
       non-negative' dune runtest"
    doc;
  check_contains "teardown failure is a second element"
    ~sub:{|<failure message="teardown exploded">|} doc;
  check_contains "headline in message attribute"
    ~sub:{|message="expect_file &quot;test/help.expected&quot;: no baseline"|}
    doc

(* The invocation-spelled hints (D5 §1) *)

let test_invocation_hints () =
  (* The JUnit body carries the same hint bytes as the terminal block:
     both derive from the one startup-computed invocation. *)
  let invocation = `Exe "dune exec qa/x/t.exe --" in
  let doc =
    Report_junit.render ~invocation ~suite:"mylib"
      ~results:
        [
          Fixtures.result [ "cli"; "cli help" ]
            (Failure.Fail [ Fixtures.snap_missing ]);
          Fixtures.result
            [ "geo"; "area non-negative" ]
            (Failure.Fail [ Fixtures.prop_failure ]);
        ]
      ~duration:0.1 ()
  in
  check_well_formed "invocation document is well-formed" doc;
  let terminal_line ~filter f =
    let block =
      Windtrap.Private.Pp.str "%a"
        (fun ppf f -> Report.pp_failure ~ansi:false ~filter ~invocation ppf f)
        f
    in
    List.find
      (fun line ->
        String.starts_with ~prefix:"    accept:" line
        || String.starts_with ~prefix:"    replay:" line)
      (String.split_on_char '\n' block)
  in
  let accept = terminal_line ~filter:"cli › cli help" Fixtures.snap_missing in
  check_contains "accept hint bytes equal the terminal block's" ~sub:accept doc;
  check_string "accept hint completes the executable"
    ~expected:
      "    accept: dune exec qa/x/t.exe -- -u, then review with git diff"
    ~actual:accept;
  let replay =
    terminal_line ~filter:"geo › area non-negative" Fixtures.prop_failure
  in
  check_string "replay hint completes the executable"
    ~expected:
      "    replay: dune exec qa/x/t.exe -- --seed s1:7be1d2c904aa31f5 -f 'geo \
       › area non-negative'"
    ~actual:replay;
  check_contains "replay hint bytes equal the terminal block's" ~sub:replay doc

(* Expected failures (amendment B12) *)

let test_excused_as_skipped () =
  let results =
    [
      Fixtures.result [ "ok" ] Failure.Pass;
      Fixtures.excused_result;
      Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "boom" ]);
    ]
  in
  let doc = Report_junit.render ~suite:"s" ~results ~duration:0.5 () in
  check_well_formed "excused document is well-formed" doc;
  check_contains "excused failure maps to skipped-with-message"
    ~sub:{|<skipped message="expected failure: issue #42"/>|} doc;
  check_absent "excused failures emit no failure element"
    ~sub:{|<failure message="expected 1, got 2"|} doc;
  check_contains "counts: excused is a skip, not a failure"
    ~sub:{|tests="3" failures="1" errors="0" skipped="1"|} doc;
  let no_reason =
    Report_junit.render ~suite:"s"
      ~results:
        [
          {
            Fixtures.excused_result with
            Run.xfail = Some { Test_tree.reason = None };
          };
        ]
      ~duration:0.1 ()
  in
  check_contains "reasonless excused message"
    ~sub:{|<skipped message="expected failure"/>|} no_reason;
  (* The record's bit decides: an unexpected pass carries the annotation
     but counted, so it emits a failure element, not a skip. *)
  let xpass =
    Report_junit.render ~suite:"s" ~results:[ Fixtures.xpass_result ]
      ~duration:0.1 ()
  in
  check_contains "an unexpected pass still counts as a failure"
    ~sub:{|failures="1"|} xpass;
  check_absent "an unexpected pass is not a skip" ~sub:"<skipped" xpass

(* Subtests (amendment B13) *)

let test_subtests_as_testcases () =
  let doc =
    Report_junit.render ~suite:"mylib"
      ~results:[ Fixtures.subtest_result ]
      ~duration:0.7 ()
  in
  expect_file doc "test/unit/expected/test_report_junit/subtests.expected";
  check_well_formed "subtest document is well-formed" doc

let test_subtests_only () =
  (* A test whose every failure is a subtest entry: the parent testcase
     carries no failure element; the failures count comes from the subtest
     testcases alone. *)
  let doc =
    Report_junit.render ~suite:"s"
      ~results:
        [
          Fixtures.result [ "backend"; "contract" ]
            (Failure.Fail [ Fixtures.subtest_failure "shape [0]" ]);
        ]
      ~duration:0.1 ()
  in
  check_well_formed "subtests-only document is well-formed" doc;
  check_contains "subtests-only counts" ~sub:{|tests="2" failures="1"|} doc;
  check_contains "parent testcase closes without failure"
    ~sub:
      {|<testcase name="backend › contract" classname="s.backend" time="0.000">
    </testcase>|}
    doc

let test_subtest_user_msg_name () =
  (* A subtest entry whose assertion also carried a user [?msg]: the
     testcase name is the displayed label — the sub-case components joined,
     the user text appended after ": " (Report.labeled_msg). *)
  let entry =
    {
      (Failure.equality ~msg:"user context" ~expected:"1" ~actual:"2" ()) with
      Failure.subtest = [ "contract"; "shape [0]" ];
    }
  in
  let doc =
    Report_junit.render ~suite:"s"
      ~results:
        [ Fixtures.result [ "backend"; "contract" ] (Failure.Fail [ entry ]) ]
      ~duration:0.1 ()
  in
  check_well_formed "user-msg subtest document is well-formed" doc;
  check_contains "subtest testcase name is the displayed label"
    ~sub:
      {|<testcase name="contract › shape [0]: user context" classname="s.backend"|}
    doc

(* Transport validity *)

let fail_result path failure = Fixtures.result path (Failure.Fail [ failure ])

let test_ansi_impossible () =
  let ansi = "\027[31mred\027[0m" in
  let tail = Failure.tail ~log_path:"log" (ansi ^ " tail text\n") in
  let f =
    Failure.with_output_tail tail
      (Failure.equality ~msg:ansi ~expected:(ansi ^ " expected")
         ~actual:"\027]0;title\007 actual" ())
  in
  let results =
    [
      fail_result [ "suite"; ansi ^ " name" ] f;
      Fixtures.result [ "s"; "skip" ] (Failure.Skip (Some (ansi ^ " reason")));
    ]
  in
  let doc = Report_junit.render ~suite:ansi ~results ~duration:0.1 () in
  check_absent "no ESC byte anywhere in the document" ~sub:"\027" doc;
  check_contains "stripped payload text survives" ~sub:"red tail text" doc;
  (* Two ways to keep ESC out of XML, and the body uses the one that keeps
     the bytes: [pp_failure] escapes comparison data before this transport
     ever sees it, so a styled expected value arrives readable instead of
     stripped down to its letters. The captured tail keeps the old
     treatment — it is a log excerpt with a full-log path. *)
  check_contains "the failure body carries the value's own bytes, escaped"
    ~sub:{|\x1b[31mred\x1b[0m expected|} doc;
  check_contains "and the OSC-carrying side too"
    ~sub:{|\x1b]0;title\x07 actual|} doc;
  check_well_formed "ANSI-stripped document is well-formed" doc

let test_xml_range () =
  let hostile = "a\x01b\x0cc\xffd" in
  let doc =
    Report_junit.render ~suite:"s"
      ~results:
        [
          fail_result [ hostile ]
            (Failure.equality ~expected:hostile ~actual:"ok" ());
        ]
      ~duration:0.1 ()
  in
  check_absent "control byte removed" ~sub:"\x01" doc;
  check_absent "form feed removed" ~sub:"\x0c" doc;
  check_absent "malformed UTF-8 byte removed" ~sub:"\xff" doc;
  check_contains "invalid characters become U+FFFD"
    ~sub:"a\u{FFFD}b\u{FFFD}c\u{FFFD}d" doc;
  check_well_formed "sanitized document is well-formed" doc

let test_escaping () =
  let nasty = {|a<b>&"c'|} in
  let doc =
    Report_junit.render ~suite:nasty
      ~results:[ fail_result [ nasty ] (Failure.message ("text " ^ nasty)) ]
      ~duration:0.1 ()
  in
  check_contains "attribute escaping"
    ~sub:{|name="a&lt;b&gt;&amp;&quot;c&apos;"|} doc;
  check_contains "text escaping" ~sub:"text a&lt;b&gt;&amp;\"c'" doc;
  check_well_formed "escaped document is well-formed" doc

let test_hostile_tail () =
  let tail = Failure.tail ~log_path:"log" "ok\x01 \027[31mred\027[0m \xff\n" in
  let doc =
    Report_junit.render ~suite:"s"
      ~results:
        [
          fail_result [ "t" ]
            (Failure.with_output_tail tail (Failure.message "boom"));
        ]
      ~duration:0.1 ()
  in
  check_absent "tail control byte removed" ~sub:"\x01" doc;
  check_absent "tail ESC removed" ~sub:"\027" doc;
  check_absent "tail malformed UTF-8 removed" ~sub:"\xff" doc;
  check_well_formed "hostile tail document is well-formed" doc

(* A pass that needed a retry: JUnit has no state for it, so the fact
   rides the one element every consumer allows on a testcase. *)
let test_flaky_note () =
  let doc =
    Report_junit.render ~suite:"s"
      ~results:
        [
          Fixtures.result [ "flaky"; "eventually" ] Failure.Pass ~attempts:3;
          Fixtures.result [ "steady" ] Failure.Pass;
        ]
      ~duration:0.1 ()
  in
  check_well_formed "flaky document is well-formed" doc;
  check_contains "a flaky pass carries the attempt count in system-out"
    ~sub:
      {|<testcase name="flaky › eventually" classname="s.flaky" time="0.000">
      <system-out>passed on attempt 3</system-out>
    </testcase>|}
    doc;
  check_contains "a first-attempt pass stays a bare testcase"
    ~sub:{|<testcase name="steady" classname="s" time="0.000"/>|} doc;
  check_contains "a flaky pass is not a failure"
    ~sub:{|tests="2" failures="0" errors="0" skipped="0"|} doc

(* One process per suite is the normal case under `dune runtest`, so a
   single fixed path would have each suite overwrite the last. The [.xml]
   suffix is what tells the two intents apart. *)
let test_path () =
  check_string "an .xml target is used verbatim" ~expected:"reports/r.xml"
    ~actual:(Report_junit.path ~suite:"mylib" "reports/r.xml");
  check_string "a directory target gets one file per suite"
    ~expected:(Filename.concat "reports" "mylib.xml")
    ~actual:(Report_junit.path ~suite:"mylib" "reports");
  check_string "two suites, one directory, two files"
    ~expected:(Filename.concat "reports" "parser.xml")
    ~actual:(Report_junit.path ~suite:"parser" "reports");
  (* A suite name is not a filename until it is made one: an inline
     partition's [lib/parser.ml] lands in the directory, not under it. *)
  let partition = Report_junit.path ~suite:"lib/parser.ml" "reports" in
  check "a suite name never escapes its directory"
    (Filename.dirname partition = "reports");
  check "and never keeps a path separator"
    (not (String.contains (Filename.basename partition) '/'));
  check "two partitions of one library get two files"
    (partition <> Report_junit.path ~suite:"lib/lexer.ml" "reports")

let test_empty_run () =
  let doc = Report_junit.render ~suite:"empty" ~results:[] ~duration:0.0 () in
  check_well_formed "empty run document is well-formed" doc;
  check_contains "empty run counts are zero"
    ~sub:{|tests="0" failures="0" errors="0" skipped="0"|} doc

(* The checker itself *)

let test_checker_sanity () =
  let ok s = Xml_check.check s = Ok () in
  let rejected s =
    match Xml_check.check s with Error _ -> true | Ok () -> false
  in
  check "checker accepts a minimal document" (ok "<a/>");
  check "checker accepts attributes, text, entities"
    (ok "<a x='1' y=\"2\">t&amp;u<b/></a>");
  check "checker rejects mismatched tags" (rejected "<a><b></a>");
  check "checker rejects unquoted attributes" (rejected "<a x=1/>");
  check "checker rejects unknown entities" (rejected "<a>&nope;</a>");
  check "checker rejects raw ampersands" (rejected "<a>t & u</a>");
  check "checker rejects control bytes" (rejected "<a>\x01</a>");
  check "checker rejects trailing content" (rejected "<a/><b/>")

let tests =
  [
    test "golden document" test_golden;
    test "full fixture run is well-formed" test_full_run;
    test "bodies carry the invocation-spelled hints (D5 §1)"
      test_invocation_hints;
    test "excused failures report as skipped" test_excused_as_skipped;
    test "subtests become testcases" test_subtests_as_testcases;
    test "subtest-only failures" test_subtests_only;
    test "subtest user msg naming" test_subtest_user_msg_name;
    test "ANSI cannot reach a JUnit document" test_ansi_impossible;
    test "XML 1.0 range sanitization" test_xml_range;
    test "escaping" test_escaping;
    test "hostile captured tail" test_hostile_tail;
    test "flaky pass note" test_flaky_note;
    test "the report's path" test_path;
    test "empty run" test_empty_run;
    test "the checker's own sanity" test_checker_sanity;
  ]

let () = exit @@ Windtrap.run "report_junit" tests
