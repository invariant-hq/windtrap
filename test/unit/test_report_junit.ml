(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Report_junit: golden document over a small synthetic run,
   well-formedness of the full fixture run (checked with the minimal
   Xml_check parser), the flaky-pass note, the ANSI-in-JUnit
   impossibility, control bytes escaped and the XML 1.0 range of hostile
   payloads,
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
    Fixtures.timed_result;
  ]

let test_golden () =
  let actual =
    Report_junit.render ~suite:"mylib" ~results:small_results
      ~release_failures:[ Fixtures.release_failure ]
      ~duration:1.234 ()
  in
  expect_file actual "test/unit/expected/test_report_junit/document.expected";
  check_well_formed "golden document is well-formed" actual

(* The full fixture run *)

let full () =
  Report_junit.render ~suite:"mylib" ~results:Fixtures.results
    ~release_failures:[ Fixtures.release_failure ]
    ~duration:Fixtures.duration ()

let test_full_run () =
  let doc = full () in
  check_well_formed "full fixture document is well-formed" doc;
  contains ~msg:"counts derive from results"
    ~sub:{|tests="13" failures="7" errors="0" skipped="1" time="6.500"|} doc;
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

(* The message attribute: the failure as one sentence, its headline. The
   headline's forms are Report_sections', pinned in test_report; this pins
   that each failure's attribute is its headline. *)

let test_message_forms () =
  let failures =
    [
      Failure.equality ~msg:"deliberate" ~expected:"1" ~actual:"2" ();
      Failure.equality ~expected:"a\nb\nc" ~actual:"a\nB\nc" ();
      Fixtures.snap_mismatch;
      Failure.equality ~expected:(String.make 100 'x') ~actual:"y" ();
    ]
  in
  let doc =
    Report_junit.render ~release_failures:[] ~suite:"s" ~duration:0.1
      ~results:
        (List.mapi
           (fun i f -> Fixtures.result [ string_of_int i ] (Failure.Fail [ f ]))
           failures)
      ()
  in
  check_well_formed "the document is well-formed" doc;
  List.iter
    (fun f ->
      contains ~msg:"the message attribute is the failure's headline"
        ~sub:(Printf.sprintf {|<failure message="%s">|} (Report.headline f))
        doc)
    failures;
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
    Report_junit.render ~release_failures:[] ~suite:"s" ~duration:0.1
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

(* The invocation-spelled hints *)

let test_invocation_hints () =
  (* The JUnit body carries the same hint bytes as the terminal block:
     both derive from the one startup-computed invocation. *)
  let invocation = `Exe "dune exec qa/x/t.exe --" in
  let doc =
    Report_junit.render ~release_failures:[] ~invocation ~suite:"mylib"
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

(* Expected failures *)

let test_excused_as_skipped () =
  let results =
    [
      Fixtures.result [ "ok" ] Failure.Pass;
      Fixtures.excused_result;
      Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "boom" ]);
    ]
  in
  let doc =
    Report_junit.render ~release_failures:[] ~suite:"s" ~results ~duration:0.5
      ()
  in
  check_well_formed "excused document is well-formed" doc;
  contains ~msg:"excused failure maps to skipped-with-message"
    ~sub:{|<skipped message="expected failure: issue #42"/>|} doc;
  (* Alone, so the document's other failure cannot hide one. *)
  not_contains ~msg:"excused failures emit no failure element" ~sub:"<failure"
    (Report_junit.render ~release_failures:[] ~suite:"s"
       ~results:[ Fixtures.excused_result ]
       ~duration:0.1 ());
  contains ~msg:"counts: excused is a skip, not a failure"
    ~sub:{|tests="3" failures="1" errors="0" skipped="1"|} doc;
  let no_reason =
    Report_junit.render ~release_failures:[] ~suite:"s"
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
    Report_junit.render ~release_failures:[] ~suite:"s"
      ~results:[ Fixtures.xpass_result ] ~duration:0.1 ()
  in
  contains ~msg:"an unexpected pass still counts as a failure"
    ~sub:{|failures="1"|} xpass;
  not_contains ~msg:"an unexpected pass is not a skip" ~sub:"<skipped" xpass

(* Subtests *)

let test_subtests_as_testcases () =
  let doc =
    Report_junit.render ~release_failures:[] ~suite:"mylib"
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
    Report_junit.render ~release_failures:[] ~suite:"s"
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
    Report_junit.render ~release_failures:[] ~suite:"s"
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
  let doc =
    Report_junit.render ~release_failures:[] ~suite:ansi ~results ~duration:0.1
      ()
  in
  not_contains ~msg:"no ESC byte anywhere in the document" ~sub:"\027" doc;
  (* ESC is escaped as every control byte is, in every field, so a styled
     value arrives readable instead of stripped down to its letters. *)
  contains ~msg:"the captured tail keeps its bytes, escaped"
    ~sub:{|\x1b[31mred\x1b[0m tail text|} doc;
  contains ~msg:"the failure body carries the value's own bytes, escaped"
    ~sub:{|\x1b[31mred\x1b[0m expected|} doc;
  contains ~msg:"and the OSC-carrying side too" ~sub:{|\x1b]0;title\x07 actual|}
    doc;
  contains ~msg:"an attribute escapes them too"
    ~sub:{|<skipped message="\x1b[31mred\x1b[0m reason"/>|} doc;
  check_well_formed "escaped document is well-formed" doc

let test_xml_range () =
  (* A control byte, a form feed, a malformed byte, and U+FFFE: valid
     UTF-8, yet no XML character. The two control bytes are escaped
     first; the other two are no XML character. *)
  let hostile = "a\x01b\x0cc\xffd\u{FFFE}e" in
  let doc =
    Report_junit.render ~release_failures:[] ~suite:"s"
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
  contains ~msg:"control bytes escaped, invalid characters become U+FFFD"
    ~sub:"a\\x01b\\x0cc\u{FFFD}d\u{FFFD}e" doc;
  check_well_formed "sanitized document is well-formed" doc

let test_escaping () =
  let nasty = {|a<b>&"c'|} in
  let doc =
    Report_junit.render ~release_failures:[] ~suite:nasty
      ~results:[ fail_result [ nasty ] (Failure.message ("text " ^ nasty)) ]
      ~duration:0.1 ()
  in
  contains ~msg:"attribute escaping"
    ~sub:{|name="a&lt;b&gt;&amp;&quot;c&apos;"|} doc;
  contains ~msg:"text escaping" ~sub:"text a&lt;b&gt;&amp;\"c'" doc;
  check_well_formed "escaped document is well-formed" doc

(* A pass that needed a retry: JUnit has no state for it, so the fact
   rides the one element every consumer allows on a testcase. *)
let test_flaky_note () =
  let doc =
    Report_junit.render ~release_failures:[] ~suite:"s"
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
  let doc =
    Report_junit.render ~release_failures:[] ~suite:"empty" ~results:[]
      ~duration:0.0 ()
  in
  check_well_formed "empty run document is well-formed" doc;
  contains ~msg:"empty run counts are zero"
    ~sub:{|tests="0" failures="0" errors="0" skipped="0"|} doc

let test_dotted_classname () =
  let doc =
    Report_junit.render ~release_failures:[] ~suite:"s"
      ~results:[ Fixtures.result [ "a.b"; "t.c" ] Failure.Pass ]
      ~duration:0.1 ()
  in
  contains ~msg:"a dot inside a group name stays a dot"
    ~sub:{|<testcase name="a.b › t.c" classname="s.a.b" time="0.000"/>|} doc

(* The captured output of a failing test: the first failure that carries a
   tail, a subtest's included, and a line before or after it only when there
   is something to say. *)
let test_first_tail () =
  let first = Failure.tail "first tail\n" in
  let second =
    Failure.tail ~log_path:"second.output" ~omitted_bytes:9 "second tail\n"
  in
  let doc =
    Report_junit.render ~release_failures:[] ~suite:"s"
      ~results:
        [
          Fixtures.result [ "t" ]
            (Failure.Fail
               [
                 Failure.with_output_tail first
                   {
                     (Failure.message "in a subtest") with
                     subtest = [ "t"; "u" ];
                   };
                 Failure.with_output_tail second (Failure.message "own");
               ]);
        ]
      ~duration:0.1 ()
  in
  check_well_formed "tail document is well-formed" doc;
  contains ~msg:"the first tail, whole, with neither line"
    ~sub:"<system-out>first tail\n</system-out>" doc;
  not_contains ~msg:"not the second" ~sub:"second tail" doc

let test_armed_hints () =
  let armed = "lib/a.ml:1:0:add" in
  let doc =
    Report_junit.render ~release_failures:[] ~armed ~suite:"s"
      ~results:
        [
          Fixtures.result
            [ "geo"; "area non-negative" ]
            (Failure.Fail [ Fixtures.prop_failure ]);
          Fixtures.result [ "cli"; "cli help" ]
            (Failure.Fail [ Fixtures.snap_missing ]);
        ]
      ~duration:0.1 ()
  in
  List.iter
    (fun line -> contains ~msg:"the armed run's hint line" ~sub:line doc)
    (Report_sections.hints ~armed ~filter:(Some "geo › area non-negative")
       [ Fixtures.prop_failure ]);
  contains ~msg:"the replay arms the mutant" ~sub:armed doc;
  not_contains ~msg:"an armed run accepts nothing" ~sub:"accept:" doc

(* Writing: the files [write] makes, as the file system shows them. *)

let write ?(suite = "s") ?(results = [ Fixtures.timed_result ]) target =
  Report_junit.write ~invocation:`Mirrors ~suite ~duration:0.1 ~results
    ~release_failures:[] target

let read_file path = In_channel.with_open_bin path In_channel.input_all

let test_write_files () =
  let root = temp_dir () in
  let shared = Filename.concat root "all.xml" in
  write ~suite:"first" shared;
  write ~suite:"second" shared;
  contains ~msg:"two suites, one .xml: the last one wins"
    ~sub:{|<testsuite name="second"|} (read_file shared);
  not_contains ~msg:"whole" ~sub:{|name="first"|} (read_file shared);
  let dir = Filename.concat root "a/b/c" in
  write dir;
  is_true ~msg:"a directory target is made with its parents"
    (Sys.file_exists (Filename.concat dir "s.xml"));
  let big = Filename.concat root "big.xml" in
  write ~results:Fixtures.results big;
  write big;
  equal ~msg:"an existing report is replaced whole" string
    (Report_junit.render ~release_failures:[] ~suite:"s"
       ~results:[ Fixtures.timed_result ] ~duration:0.1 ())
    (read_file big);
  equal ~msg:"nothing of a document reaches the terminal" string "" (output ())

let test_write_failure () =
  let root = temp_dir () in
  let target = Filename.concat root "missing/r.xml" in
  write target;
  is_false ~msg:"the parent of an .xml target is never made"
    (Sys.file_exists (Filename.concat root "missing"));
  contains ~msg:"one warning on standard error, and write returns" ~sub:"JUnit"
    (output ())

(* The paths a document prints: a file baseline's and a full log's. *)
let test_project_root_paths () =
  let root = temp_dir () in
  setenv "WINDTRAP_PROJECT_ROOT" (Some root);
  let failure =
    Failure.with_output_tail
      (Failure.tail
         ~log_path:(Filename.concat root "_build/_tests/s/t.output")
         "out\n")
      (Failure.baseline
         (Failure.File (Filename.concat root "test/help.expected"))
         (Failure.Missing { proposed = Failure.text "x\n" }))
  in
  let doc =
    Report_junit.render ~release_failures:[] ~suite:"s"
      ~results:[ Fixtures.result [ "t" ] (Failure.Fail [ failure ]) ]
      ~duration:0.1 ()
  in
  contains ~msg:"a file baseline prints under the root"
    ~sub:"test/help.expected" doc;
  contains ~msg:"a full log too" ~sub:"full log: _build/_tests/s/t.output" doc;
  not_contains ~msg:"never absolute" ~sub:root doc

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
  is_true ~msg:"checker accepts multi-byte characters"
    (ok "<a x='\u{e9}'>\u{20ac}\u{1d11e}</a>");
  is_true ~msg:"checker rejects a repeated attribute"
    (rejected "<a x='1' x='2'/>");
  is_true ~msg:"checker rejects invalid UTF-8" (rejected "<a>\xff</a>");
  is_true ~msg:"checker rejects a non-character" (rejected "<a>\u{fffe}</a>");
  is_true ~msg:"checker rejects a control byte in an attribute"
    (rejected "<a x='\x01'/>");
  is_true ~msg:"checker rejects a reference to a non-character"
    (rejected "<a>&#0;</a>");
  is_true ~msg:"checker rejects trailing content" (rejected "<a/><b/>")

let tests =
  [
    test "golden document" test_golden;
    test "full fixture run is well-formed" test_full_run;
    test "the message attribute is the headline" test_message_forms;
    test "bodies carry the invocation-spelled hints" test_invocation_hints;
    test "excused failures report as skipped" test_excused_as_skipped;
    test "a withheld correction offers no acceptance" test_withheld_correction;
    test "subtests become testcases" test_subtests_as_testcases;
    test "subtest-only failures" test_subtests_only;
    test "subtest user msg naming" test_subtest_user_msg_name;
    test "ANSI cannot reach a JUnit document" test_ansi_impossible;
    test "XML 1.0 range sanitization" test_xml_range;
    test "escaping" test_escaping;
    test "flaky pass note" test_flaky_note;
    test "the report's path" test_path;
    test "empty run" test_empty_run;
    test "a dot in a group name is not escaped" test_dotted_classname;
    test "system-out holds the first tail" test_first_tail;
    test "an armed run's hints" test_armed_hints;
    test "the files write makes" test_write_files;
    test "a report that cannot be written warns" test_write_failure;
    test "paths print against the project root" test_project_root_paths;
    test "the checker's own sanity" test_checker_sanity;
  ]

let () = exit @@ Windtrap.run "report_junit" tests
