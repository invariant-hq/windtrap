(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Render_github: golden ::error annotation, %0A/%0D/%25 data
   encoding, %3A/%2C property encoding, ANSI stripping, group folding
   commands, and the run-level annotations block. *)

open Windtrap
open Windtrap.Private
module Fixtures = Render_fixtures

let check name cond = is_true ~msg:name cond
let check_string name ~expected ~actual = equal ~msg:name string expected actual
let check_contains name ~sub s = Windtrap.contains ~msg:name ~sub s
let check_absent name ~sub s = not_contains ~msg:name ~sub s

let count_occurrences ~sub s =
  let sub_len = String.length sub in
  let rec go acc i =
    if i + sub_len > String.length s then acc
    else if String.sub s i sub_len = sub then go (acc + 1) (i + sub_len)
    else go acc (i + 1)
  in
  go 0 0

(* Golden annotation *)

let expected_annotation =
  {|::error file=test/test_users.ml,line=31,title=Test failure%3A users › sessions after login::    test/test_users.ml:31%0A    expected  [("alice", [1; 2; 3]); ("bob", [4])]%0A    actual    [("alice", [1; 2; 3]); ("bob", [4; 5]); ("carol", [])]%0A                                               ~~~~~~~~~~~~~~~~~~
|}

let test_golden () =
  let actual =
    Render_github.annotation
      ~path:[ "users"; "sessions after login" ]
      Fixtures.eq_failure
  in
  check_string "golden ::error annotation" ~expected:expected_annotation ~actual

(* Encoding *)

let test_data_encoding () =
  let f = Failure.message "50% done\r\nnext: a,b" in
  let a = Render_github.annotation ~path:[ "t" ] f in
  check_contains "percent encoded first" ~sub:"50%25 done%0D%0A    next" a;
  check_contains "colons and commas untouched in message data" ~sub:"next: a,b"
    a;
  check "annotation is one command line"
    (String.length a > 0
    && a.[String.length a - 1] = '\n'
    && count_occurrences ~sub:"\n" a = 1)

let test_property_encoding () =
  let f =
    Failure.message
      ~loc:{ Loc.file = "dir,x:y/test.ml"; line = 7; column = 0 }
      "boom"
  in
  let a = Render_github.annotation ~path:[ "suite: a,b"; "case" ] f in
  check_contains "file property encodes delimiters"
    ~sub:"file=dir%2Cx%3Ay/test.ml,line=7," a;
  check_contains "title encodes delimiters"
    ~sub:"title=Test failure%3A suite%3A a%2Cb › case::" a

let test_no_location () =
  let a = Render_github.annotation ~path:[ "t" ] (Failure.message "boom") in
  check_contains "no location: title only" ~sub:"::error title=" a;
  check_absent "no location: no file property" ~sub:"file=" a

let test_replay_info () =
  let a =
    Render_github.annotation
      ~path:[ "geo"; "area non-negative" ]
      Fixtures.prop_failure
  in
  check_contains "property annotation carries the replay line"
    ~sub:
      "replay: WINDTRAP_SEED=s1:7be1d2c904aa31f5 WINDTRAP_FILTER='geo › area \
       non-negative' dune runtest"
    a;
  check_contains "counterexample in the message"
    ~sub:"counterexample (case 12, shrunk 4 steps): Rect (2, 0)" a

let test_invocation_hints () =
  (* Annotation messages carry the same hint bytes as the terminal block —
     both derive from the one startup-computed invocation (D5 §1). *)
  let invocation = `Exe "dune exec qa/x/t.exe --" in
  let a =
    Render_github.annotation ~invocation
      ~path:[ "geo"; "area non-negative" ]
      Fixtures.prop_failure
  in
  check_contains "replay hint spelled from the invocation, %0A-encoded"
    ~sub:
      "%0A    replay: dune exec qa/x/t.exe -- --seed s1:7be1d2c904aa31f5 -f \
       'geo › area non-negative'"
    a;
  check_absent "no Mirrors spelling under Exe" ~sub:"WINDTRAP_SEED" a;
  let block =
    Render_github.annotations ~invocation
      [
        Fixtures.result [ "cli"; "cli help" ]
          (Failure.Fail [ Fixtures.snap_missing ]);
      ]
  in
  check_contains "annotations thread the invocation to accept hints"
    ~sub:"%0A    accept: dune exec qa/x/t.exe -- -u, then review with git diff"
    block

let test_ansi_stripped () =
  let f =
    Failure.equality ~expected:"\027[32mgreen\027[0m" ~actual:"plain" ()
  in
  let a = Render_github.annotation ~path:[ "t" ] f in
  check_absent "ANSI stripped from annotations" ~sub:"\027" a;
  check_contains "stripped payload survives" ~sub:"green" a

(* Folding *)

let test_groups () =
  check_string "group start" ~expected:"::group::mylib\n"
    ~actual:(Render_github.group_start "mylib");
  check_string "group end" ~expected:"::endgroup::\n"
    ~actual:Render_github.group_end;
  check_string "group name newline encoded" ~expected:"::group::a%0Ab\n"
    ~actual:(Render_github.group_start "a\nb")

(* Expected failures and subtests (amendments B12/B13) *)

let test_excused_filtered () =
  (* Classification is record-driven: an excused expected failure — a
     failing record that did not count — annotates nothing, while the
     unexpected-pass record (counted, annotation and all) stays loud. *)
  let results =
    [
      Fixtures.excused_result;
      Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "boom" ]);
    ]
  in
  let block = Render_github.annotations results in
  check "excused failures produce no annotation"
    (count_occurrences ~sub:"::error " block = 1);
  check_absent "excused test absent from the block" ~sub:"broken carry" block;
  check_contains "counted failures still annotate"
    ~sub:"title=Test failure%3A bad::" block;
  check_string "all failures excused, no output" ~expected:""
    ~actual:(Render_github.annotations [ Fixtures.excused_result ]);
  check_contains "an unexpected pass still annotates"
    ~sub:"title=Test failure%3A known › fixed already::"
    (Render_github.annotations [ Fixtures.xpass_result ])

let test_subtest_annotations () =
  let block = Render_github.annotations [ Fixtures.subtest_result ] in
  check "one annotation per failure entry, subtests included"
    (count_occurrences ~sub:"::error " block = 3);
  check_contains "subtest annotations are titled by the parent test"
    ~sub:"title=Test failure%3A backend › contract::" block;
  check_contains "subtest annotations point into the parent's body"
    ~sub:"file=test/test_backend.ml,line=40," block;
  check_contains "the subtest label leads the message"
    ~sub:"::    test/test_backend.ml:40%0A    contract › shape [0]%0A" block

(* Run-level annotations *)

let test_annotations () =
  let block = Render_github.annotations Fixtures.results in
  check "one command per failure entry (teardown pair gives two)"
    (count_occurrences ~sub:"::error " block = 7);
  check "every command on its own line" (count_occurrences ~sub:"\n" block = 7);
  check_contains "paths name the failing tests"
    ~sub:"title=Test failure%3A db › insert::" block;
  check_string "no failures, no output" ~expected:""
    ~actual:
      (Render_github.annotations
         [
           Fixtures.result [ "ok" ] Failure.Pass;
           Fixtures.result [ "s" ] (Failure.Skip None);
         ]);
  check_string "empty run, no output" ~expected:""
    ~actual:(Render_github.annotations [])

let tests =
  [
    test "golden annotation" test_golden;
    test "data encoding (%0A/%0D/%25)" test_data_encoding;
    test "property encoding (%3A/%2C)" test_property_encoding;
    test "annotation without a location" test_no_location;
    test "replay info" test_replay_info;
    test "invocation-spelled hints (D5 §1)" test_invocation_hints;
    test "ANSI stripped" test_ansi_stripped;
    test "group folding commands" test_groups;
    test "excused failures filtered" test_excused_filtered;
    test "subtest annotations" test_subtest_annotations;
    test "run-level annotations block" test_annotations;
  ]
