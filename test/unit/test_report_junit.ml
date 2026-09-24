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
  contains ~msg:"counts derive from results"
    ~sub:{|tests="11" failures="6" errors="0" skipped="1" time="6.500"|} doc;
  contains ~msg:"acceptance command inside failure text"
    ~sub:"accept: dune promote" doc;
  contains ~msg:"replay line inside failure text"
    ~sub:
      "replay: WINDTRAP_SEED=s1:7be1d2c904aa31f5 WINDTRAP_FILTER='geo › area \
       non-negative' dune runtest"
    doc;
  contains ~msg:"teardown failure is a second element"
    ~sub:{|<failure message="teardown exploded">|} doc;
  contains ~msg:"headline in message attribute"
    ~sub:{|message="expect_file &quot;test/help.expected&quot;: no baseline"|}
    doc

(* The message attribute: the failure as one sentence *)

let test_message_forms () =
  let doc =
    Report_junit.render ~suite:"s" ~duration:0.1
      ~results:
        [
          Fixtures.result [ "sides" ]
            (Failure.Fail
               [
                 Failure.equality ~msg:"deliberate" ~expected:"1" ~actual:"2" ();
               ]);
          Fixtures.result [ "diff" ]
            (Failure.Fail
               [ Failure.equality ~expected:"a\nb\nc" ~actual:"a\nB\nc" () ]);
          Fixtures.result [ "baseline" ]
            (Failure.Fail [ Fixtures.snap_mismatch ]);
          Fixtures.result [ "long" ]
            (Failure.Fail
               [
                 Failure.equality ~expected:(String.make 100 'x') ~actual:"y" ();
               ]);
        ]
      ()
  in
  check_well_formed "the document is well-formed" doc;
  contains ~msg:"the user message, a colon, then the sentence"
    ~sub:{|<failure message="deliberate: expected 1, got 2">|} doc;
  contains ~msg:"a diff is a sentence counting its lines"
    ~sub:{|<failure message="expected and actual differ (5 diff lines)">|} doc;
  contains ~msg:"a baseline is its first fact line"
    ~sub:{|<failure message="expect: mismatch">|} doc;
  contains ~msg:"80 code points, then an ellipsis"
    ~sub:({|<failure message="expected |} ^ String.make 71 'x' ^ "\u{2026}\">")
    doc;
  not_contains ~msg:"no em dash in the document" ~sub:"\u{2014}" doc;
  not_contains ~msg:"no em dash in the full fixture's document" ~sub:"\u{2014}"
    (full ());
  (* The failure text is the block's lines below the title. *)
  contains ~msg:"the failure text is the block's lines"
    ~sub:"\">    deliberate\n    expected  1\n    actual    2\n</failure>" doc;
  not_contains ~msg:"no failure text carries a rerun hint" ~sub:"rerun:" doc;
  not_contains ~msg:"nor does the full fixture's document" ~sub:"rerun:"
    (full ())

(* A withheld correction is the failure's, so the document projects it as
   the terminal block does. *)

let test_withheld_correction () =
  let doc =
    Report_junit.render ~suite:"s" ~duration:0.1
      ~results:
        [
          Fixtures.result [ "both" ]
            (Failure.Fail
               [
                 Failure.message "boom";
                 Failure.with_withheld Failure.Failed_outside
                   Fixtures.snap_mismatch;
               ]);
        ]
      ()
  in
  check_well_formed "the document is well-formed" doc;
  not_contains ~msg:"no acceptance the run could not honour" ~sub:"accept:" doc;
  contains ~msg:"the reason closes the failure text"
    ~sub:
      "    no correction was kept: the test also failed outside its \
       expectations; fix that failure and rerun\n\
       </failure>"
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
  contains ~msg:"accept hint bytes equal the terminal block's" ~sub:accept doc;
  equal ~msg:"accept hint completes the executable, scoped to the test" string
    "    accept: dune exec qa/x/t.exe -- -u -f 'cli › cli help'" accept;
  let replay =
    terminal_line ~filter:"geo › area non-negative" Fixtures.prop_failure
  in
  equal ~msg:"replay hint completes the executable" string
    "    replay: dune exec qa/x/t.exe -- --seed s1:7be1d2c904aa31f5 -f 'geo › \
     area non-negative'"
    replay;
  contains ~msg:"replay hint bytes equal the terminal block's" ~sub:replay doc

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
  contains ~msg:"excused failure maps to skipped-with-message"
    ~sub:{|<skipped message="expected failure: issue #42"/>|} doc;
  not_contains ~msg:"excused failures emit no failure element"
    ~sub:{|<failure message="expected 1; actual 2"|} doc;
  contains ~msg:"counts: excused is a skip, not a failure"
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
  contains ~msg:"reasonless excused message"
    ~sub:{|<skipped message="expected failure"/>|} no_reason;
  (* The record's bit decides: an unexpected pass carries the annotation
     but counted, so it emits a failure element, not a skip. *)
  let xpass =
    Report_junit.render ~suite:"s" ~results:[ Fixtures.xpass_result ]
      ~duration:0.1 ()
  in
  contains ~msg:"an unexpected pass still counts as a failure"
    ~sub:{|failures="1"|} xpass;
  not_contains ~msg:"an unexpected pass is not a skip" ~sub:"<skipped" xpass

(* Subtests (amendment B13) *)

let test_subtests_as_testcases () =
  let doc =
    Report_junit.render ~suite:"mylib"
      ~results:[ Fixtures.subtest_result ]
      ~duration:0.7 ()
  in
  expect_file doc "test/unit/expected/test_report_junit/subtests.expected";
  check_well_formed "subtest document is well-formed" doc;
  (* Sibling subtests fail alike: the label is what tells their messages
     apart. *)
  contains ~msg:"a subtest's message opens with its label"
    ~sub:
      {|<failure message="contract › shape [0]: expected [1; 2], got [1; 3]">|}
    doc;
  contains ~msg:"and its sibling's with its own"
    ~sub:
      {|<failure message="contract › shape [2]: expected [1; 2], got [1; 3]">|}
    doc

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
  contains ~msg:"subtests-only counts" ~sub:{|tests="2" failures="1"|} doc;
  contains ~msg:"parent testcase closes without failure"
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
  contains ~msg:"subtest testcase name is the displayed label"
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
  not_contains ~msg:"no ESC byte anywhere in the document" ~sub:"\027" doc;
  contains ~msg:"stripped payload text survives" ~sub:"red tail text" doc;
  (* Two ways to keep ESC out of XML, and the body uses the one that keeps
     the bytes: [pp_failure] escapes comparison data before this transport
     ever sees it, so a styled expected value arrives readable instead of
     stripped down to its letters. The captured tail keeps the old
     treatment — it is a log excerpt with a full-log path. *)
  contains ~msg:"the failure body carries the value's own bytes, escaped"
    ~sub:{|\x1b[31mred\x1b[0m expected|} doc;
  contains ~msg:"and the OSC-carrying side too" ~sub:{|\x1b]0;title\x07 actual|}
    doc;
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
  not_contains ~msg:"control byte removed" ~sub:"\x01" doc;
  not_contains ~msg:"form feed removed" ~sub:"\x0c" doc;
  not_contains ~msg:"malformed UTF-8 byte removed" ~sub:"\xff" doc;
  contains ~msg:"invalid characters become U+FFFD"
    ~sub:"a\u{FFFD}b\u{FFFD}c\u{FFFD}d" doc;
  check_well_formed "sanitized document is well-formed" doc

let test_escaping () =
  let nasty = {|a<b>&"c'|} in
  let doc =
    Report_junit.render ~suite:nasty
      ~results:[ fail_result [ nasty ] (Failure.message ("text " ^ nasty)) ]
      ~duration:0.1 ()
  in
  contains ~msg:"attribute escaping"
    ~sub:{|name="a&lt;b&gt;&amp;&quot;c&apos;"|} doc;
  contains ~msg:"text escaping" ~sub:"text a&lt;b&gt;&amp;\"c'" doc;
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
  not_contains ~msg:"tail control byte removed" ~sub:"\x01" doc;
  not_contains ~msg:"tail ESC removed" ~sub:"\027" doc;
  not_contains ~msg:"tail malformed UTF-8 removed" ~sub:"\xff" doc;
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
  contains ~msg:"a flaky pass carries the attempt count in system-out"
    ~sub:
      {|<testcase name="flaky › eventually" classname="s.flaky" time="0.000">
      <system-out>passed on attempt 3</system-out>
    </testcase>|}
    doc;
  contains ~msg:"a first-attempt pass stays a bare testcase"
    ~sub:{|<testcase name="steady" classname="s" time="0.000"/>|} doc;
  contains ~msg:"a flaky pass is not a failure"
    ~sub:{|tests="2" failures="0" errors="0" skipped="0"|} doc

(* One process per suite is the normal case under `dune runtest`, so a
   single fixed path would have each suite overwrite the last. The [.xml]
   suffix is what tells the two intents apart. *)
let test_path () =
  equal ~msg:"an .xml target is used verbatim" string "reports/r.xml"
    (Report_junit.path ~suite:"mylib" "reports/r.xml");
  equal ~msg:"a directory target gets one file per suite" string
    (Filename.concat "reports" "mylib.xml")
    (Report_junit.path ~suite:"mylib" "reports");
  equal ~msg:"two suites, one directory, two files" string
    (Filename.concat "reports" "parser.xml")
    (Report_junit.path ~suite:"parser" "reports");
  (* A suite name is not a filename until it is made one: an inline
     partition's [lib/parser.ml] lands in the directory, not under it. *)
  let partition = Report_junit.path ~suite:"lib/parser.ml" "reports" in
  is_true ~msg:"a suite name never escapes its directory"
    (Filename.dirname partition = "reports");
  is_true ~msg:"and never keeps a path separator"
    (not (String.contains (Filename.basename partition) '/'));
  is_true ~msg:"two partitions of one library get two files"
    (partition <> Report_junit.path ~suite:"lib/lexer.ml" "reports")

let test_empty_run () =
  let doc = Report_junit.render ~suite:"empty" ~results:[] ~duration:0.0 () in
  check_well_formed "empty run document is well-formed" doc;
  contains ~msg:"empty run counts are zero"
    ~sub:{|tests="0" failures="0" errors="0" skipped="0"|} doc

(* The checker itself *)

let test_checker_sanity () =
  let ok s = Xml_check.check s = Ok () in
  let rejected s =
    match Xml_check.check s with Error _ -> true | Ok () -> false
  in
  is_true ~msg:"checker accepts a minimal document" (ok "<a/>");
  is_true ~msg:"checker accepts attributes, text, entities"
    (ok "<a x='1' y=\"2\">t&amp;u<b/></a>");
  is_true ~msg:"checker rejects mismatched tags" (rejected "<a><b></a>");
  is_true ~msg:"checker rejects unquoted attributes" (rejected "<a x=1/>");
  is_true ~msg:"checker rejects unknown entities" (rejected "<a>&nope;</a>");
  is_true ~msg:"checker rejects raw ampersands" (rejected "<a>t & u</a>");
  is_true ~msg:"checker rejects control bytes" (rejected "<a>\x01</a>");
  is_true ~msg:"checker rejects trailing content" (rejected "<a/><b/>")

let tests =
  [
    test "golden document" test_golden;
    test "full fixture run is well-formed" test_full_run;
    test "the message attribute's forms" test_message_forms;
    test "bodies carry the invocation-spelled hints (D5 §1)"
      test_invocation_hints;
    test "excused failures report as skipped" test_excused_as_skipped;
    test "a withheld correction offers no acceptance" test_withheld_correction;
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
