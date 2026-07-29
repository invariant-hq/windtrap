(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Render_junit: golden document over a small synthetic run,
   well-formedness of the full fixture run (checked with the minimal
   Xml_check parser), the ANSI-in-JUnit impossibility, XML 1.0 range
   sanitization of hostile payloads, escaping, counts, and the checker's own
   sanity. *)

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

(* ───── The golden document ───── *)

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

let expected_small =
  {|<?xml version="1.0" encoding="UTF-8"?>
<testsuites name="windtrap" tests="3" failures="1" errors="0" skipped="1" time="1.234">
  <testsuite name="mylib" tests="3" failures="1" errors="0" skipped="1" time="1.234">
    <testcase name="math › addition" classname="mylib.math" time="0.000"/>
    <testcase name="users › sessions after login" classname="mylib.users" time="0.000">
      <failure message="expected [(&quot;alice&quot;, [1; 2; 3]); (&quot;bob&quot;, [4])], got [(&quot;alice&quot;, [1; 2; 3]); (&quot;bob&quot;, [4; 5]); (&quot;carol&quot;, [])]">    test/test_users.ml:31
    expected  [("alice", [1; 2; 3]); ("bob", [4])]
    actual    [("alice", [1; 2; 3]); ("bob", [4; 5]); ("carol", [])]
                                               ~~~~~~~~~~~~~~~~~~
</failure>
      <system-out>[12034 earlier bytes omitted]
[debug] session table resize 2 -&gt; 4
[debug] carol: ghost session from pool reuse

full log: _build/_tests/mylib/latest/users/sessions-after-login.output</system-out>
    </testcase>
    <testcase name="platform › windows paths" classname="mylib.platform" time="0.000">
      <skipped message="unix only"/>
    </testcase>
  </testsuite>
</testsuites>
|}

let test_golden () =
  let actual =
    Render_junit.render ~suite:"mylib" ~results:small_results ~duration:1.234 ()
  in
  check_string "golden JUnit document" ~expected:expected_small ~actual;
  check_well_formed "golden document is well-formed" actual

(* ───── The full fixture run ───── *)

let full () =
  Render_junit.render ~suite:"mylib" ~results:Fixtures.results
    ~duration:Fixtures.duration ()

let test_full_run () =
  let doc = full () in
  check_well_formed "full fixture document is well-formed" doc;
  check_contains "counts derive from results"
    ~sub:{|tests="11" failures="6" errors="0" skipped="1" time="6.500"|} doc;
  check_contains "acceptance command inside failure text"
    ~sub:"accept: WINDTRAP_UPDATE=1 dune runtest, then review with git diff" doc;
  check_contains "replay line inside failure text"
    ~sub:
      "replay: WINDTRAP_SEED=s1:7be1d2c904aa31f5 WINDTRAP_FILTER='geo › area \
       non-negative' dune runtest"
    doc;
  check_contains "teardown failure is a second element"
    ~sub:{|<failure message="teardown exploded">|} doc;
  check_contains "headline in message attribute"
    ~sub:{|message="snapshot &quot;help&quot;: no baseline"|} doc

(* ───── The invocation-spelled hints (D5 §1) ───── *)

let test_invocation_hints () =
  (* The JUnit body carries the same hint bytes as the terminal block:
     both derive from the one startup-computed invocation. *)
  let invocation = `Exe "dune exec qa/x/t.exe --" in
  let doc =
    Render_junit.render ~invocation ~suite:"mylib"
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
        (fun ppf f -> Render.pp_failure ~ansi:false ~filter ~invocation ppf f)
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

(* ───── Expected failures (amendment B12) ───── *)

let test_excused_as_skipped () =
  let results =
    [
      Fixtures.result [ "ok" ] Failure.Pass;
      Fixtures.excused_result;
      Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "boom" ]);
    ]
  in
  let doc = Render_junit.render ~suite:"s" ~results ~duration:0.5 () in
  check_well_formed "excused document is well-formed" doc;
  check_contains "excused failure maps to skipped-with-message"
    ~sub:{|<skipped message="expected failure: issue #42"/>|} doc;
  check_absent "excused failures emit no failure element"
    ~sub:{|<failure message="expected 1, got 2"|} doc;
  check_contains "counts: excused is a skip, not a failure"
    ~sub:{|tests="3" failures="1" errors="0" skipped="1"|} doc;
  let no_reason =
    Render_junit.render ~suite:"s"
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
    Render_junit.render ~suite:"s" ~results:[ Fixtures.xpass_result ]
      ~duration:0.1 ()
  in
  check_contains "an unexpected pass still counts as a failure"
    ~sub:{|failures="1"|} xpass;
  check_absent "an unexpected pass is not a skip" ~sub:"<skipped" xpass

(* ───── Subtests (amendment B13) ───── *)

let subtest_golden =
  {|<?xml version="1.0" encoding="UTF-8"?>
<testsuites name="windtrap" tests="3" failures="3" errors="0" skipped="0" time="0.700">
  <testsuite name="mylib" tests="3" failures="3" errors="0" skipped="0" time="0.700">
    <testcase name="backend › contract" classname="mylib.backend" time="0.000">
      <failure message="final check">    test/test_backend.ml:61
    final check
</failure>
    </testcase>
    <testcase name="contract › shape [0]" classname="mylib.backend" time="0.000">
      <failure message="contract › shape [0] — expected [1; 2], got [1; 3]">    test/test_backend.ml:40
    contract › shape [0]
    expected  [1; 2]
    actual    [1; 3]
                  ~
</failure>
    </testcase>
    <testcase name="contract › shape [2]" classname="mylib.backend" time="0.000">
      <failure message="contract › shape [2] — expected [1; 2], got [1; 3]">    test/test_backend.ml:40
    contract › shape [2]
    expected  [1; 2]
    actual    [1; 3]
                  ~
</failure>
    </testcase>
  </testsuite>
</testsuites>
|}

let test_subtests_as_testcases () =
  let doc =
    Render_junit.render ~suite:"mylib"
      ~results:[ Fixtures.subtest_result ]
      ~duration:0.7 ()
  in
  check_string "subtest golden document" ~expected:subtest_golden ~actual:doc;
  check_well_formed "subtest document is well-formed" doc

let test_subtests_only () =
  (* A test whose every failure is a subtest entry: the parent testcase
     carries no failure element; the failures count comes from the subtest
     testcases alone. *)
  let doc =
    Render_junit.render ~suite:"s"
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
  (* A subtest entry whose assertion also carried a user [?msg]: the label
     rides the msg slot with the user text appended, and the testcase name is
     that slot verbatim — the label cannot be split back out (documented). *)
  let entry =
    {
      (Failure.equality ~expected:"1" ~actual:"2" ()) with
      Failure.msg = Some "contract \u{203a} shape [0]: user context";
    }
  in
  let doc =
    Render_junit.render ~suite:"s"
      ~results:
        [ Fixtures.result [ "backend"; "contract" ] (Failure.Fail [ entry ]) ]
      ~duration:0.1 ()
  in
  check_well_formed "user-msg subtest document is well-formed" doc;
  check_contains "subtest testcase name is the msg slot verbatim"
    ~sub:
      {|<testcase name="contract › shape [0]: user context" classname="s.backend"|}
    doc

(* ───── Transport validity ───── *)

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
  let doc = Render_junit.render ~suite:ansi ~results ~duration:0.1 () in
  check_absent "no ESC byte anywhere in the document" ~sub:"\027" doc;
  check_contains "stripped payload text survives" ~sub:"red tail text" doc;
  check_well_formed "ANSI-stripped document is well-formed" doc

let test_xml_range () =
  let hostile = "a\x01b\x0cc\xffd" in
  let doc =
    Render_junit.render ~suite:"s"
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
    Render_junit.render ~suite:nasty
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
    Render_junit.render ~suite:"s"
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

let test_empty_run () =
  let doc = Render_junit.render ~suite:"empty" ~results:[] ~duration:0.0 () in
  check_well_formed "empty run document is well-formed" doc;
  check_contains "empty run counts are zero"
    ~sub:{|tests="0" failures="0" errors="0" skipped="0"|} doc

(* ───── The checker itself ───── *)

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
    test "empty run" test_empty_run;
    test "the checker's own sanity" test_checker_sanity;
  ]
