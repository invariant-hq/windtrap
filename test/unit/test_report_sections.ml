(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Check = Windtrap.Private.Check
module Failure = Windtrap.Private.Failure
module Loc = Windtrap.Private.Loc
module Os = Windtrap.Private.Os
module Run = Windtrap.Private.Run
module Sections = Windtrap.Private.Report_sections
module Text = Windtrap.Private.Text
module Fixtures = Render_fixtures

let strf = Printf.sprintf
let root = Fixtures.root
let seed = "s1:7be1d2c904aa31f5"
let repeat n s = String.concat "" (List.init n (fun _ -> s))
let exe = `Exe "./t.exe"
let armed = "lib/calc.ml:9:12:add"

let entry ?(ansi = false) ?(terminal = ansi) ?excerpt ?hints ?filter ?invocation
    ?armed f =
  Format.asprintf "%a"
    (Sections.pp_failure ~ansi ~terminal ?excerpt ?hints ?filter ?invocation
       ?armed)
    f

let printed ?(ansi = false) sections =
  Format.asprintf "%a" (fun ppf -> Sections.print ~out:ppf ~ansi) sections

let lines s = String.split_on_char '\n' s

let is_mark_line l =
  String.contains l '~' && String.for_all (fun c -> c = ' ' || c = '~') l

let without_marks s =
  String.concat "\n" (List.filter (fun l -> not (is_mark_line l)) (lines s))

(* The control bytes of [s] but LF and TAB, as [\xNN]. *)
let controls s =
  List.filter_map
    (fun c ->
      if (c < ' ' && c <> '\n' && c <> '\t') || c = '\127' then
        Some (strf "\\x%02x" (Char.code c))
      else None)
    (List.of_seq (String.to_seq s))

let raised_by f =
  match f () with
  | () -> None
  | exception Failure.Check_failure f -> Some { f with Failure.loc = None }

(* The failure a verb raised, so that an entry is of a real payload. Its
   location is this file's, which a gallery does not pin. *)
let caught f = require_some ~msg:"the verb raised its failure" (raised_by f)

let gallery name entries =
  Gallery.check
    ("test/unit/expected/test_report_sections/" ^ name ^ ".expected")
    entries

(* Failure projections: headlines *)

let unequal expected actual = Failure.equality ~expected ~actual ()

let labelled ?msg label =
  let f = Failure.equality ?msg ~expected:"[1; 2]" ~actual:"[1; 3]" () in
  { f with Failure.subtest = [ "contract"; label ] }

let with_msg msg f = { f with Failure.msg = Some (Failure.text msg) }

let prop ?shrink_end ?rendering ?summary ?(examples = false) ~case ~steps
    rendered =
  Failure.property ?shrink_end ?rendering ?summary ~rendered ~case_index:case
    ~shrink_steps:steps ~root ~examples ()

let not_contains_failure =
  Failure.containment ~found_at:10 ~demand:Failure.Anywhere ~needle:"secret"
    ~haystack:"0123456789secret-end" ()

let chain = "connect send disconnect authenticate"

(* [in_order] over ["connect"; "authenticate"; "disconnect"]: the search for
   "disconnect" resumed at 36, past "authenticate", and its occurrence at 13
   is behind it. *)
let out_of_order =
  Failure.containment ~found_at:13
    ~demand:(Failure.Ordered { index = 2; resumed_at = 36 })
    ~needle:"disconnect" ~haystack:chain ()

let missing_element =
  Failure.containment
    ~demand:(Failure.Ordered { index = 2; resumed_at = 36 })
    ~needle:"teardown" ~haystack:chain ()

let path = "sessions/ghost/session.json"

let prefix_absent =
  Failure.containment ~demand:Failure.Prefix ~needle:"users/" ~haystack:path ()

let prefix_elsewhere =
  Failure.containment ~demand:Failure.Prefix ~found_at:9 ~needle:"ghost"
    ~haystack:path ()

let suffix_elsewhere =
  Failure.containment ~demand:Failure.Suffix ~found_at:0 ~needle:"session"
    ~haystack:path ()

let timed_case ?count ~examples case_index passed =
  Failure.timeout
    ~case:{ Failure.case_index; examples; passed; root; count }
    0.5

let long_headline = "expected " ^ repeat 71 "\u{00e9}" ^ "\u{2026}"

let headline_rows =
  [
    ("equality", (unequal "true" "false", "expected true, got false"));
    ( "a negated equality",
      ( Failure.equality ~not_:true ~expected:"3" ~actual:"3" (),
        "both sides equal: 3" ) );
    ( "a raise of the wrong exception",
      ( Failure.raised ~expected:"A" ~actual:"B" (),
        "expected exception A, raised B" ) );
    ( "a raise with nothing raised",
      (Failure.raised ~expected:"A" (), "expected exception A, none raised") );
    ( "a predicate's miss",
      ( Failure.raised ~actual:"B" ~predicate:true (),
        "exception did not satisfy the predicate: B" ) );
    ( "a predicate that saw nothing raised",
      (Failure.raised ~predicate:true (), "expected an exception, none raised")
    );
    ( "an uncaught exception",
      (Failure.raised ~actual:"Not_found" (), "uncaught exception: Not_found")
    );
    ( "a raise that wanted any exception",
      (Failure.raised (), "expected an exception, none raised") );
    ( "a predicate's claim is the expected side",
      ( Failure.predicate ~claim:"a power of two" "12",
        "expected a power of two, got 12" ) );
    ("equal renderings", (unequal "nan" "nan", "both sides render as: nan"));
    ( "a diff counts the lines the block prints",
      (unequal "a\nb\nc" "a\nB\nc", "expected and actual differ (5 diff lines)")
    );
    ( "a diff of a side that spans lines against one that does not",
      (unequal "a\nb" "c", "expected and actual differ (4 diff lines)") );
    ( "a difference in a trailing newline alone",
      ( unequal "a\nb" "a\nb\n",
        "values differ only by a trailing newline (on the actual side)" ) );
    ( "a missing file baseline",
      (Fixtures.snap_missing, {|expect_file "test/help.expected": no baseline|})
    );
    ("a literal mismatch", (Fixtures.snap_mismatch, "expect: mismatch"));
    ( "an exact literal mismatch",
      ( Failure.baseline
          (Failure.Literal { exact = true })
          (Failure.Mismatch
             { expected = Failure.text "a\n"; actual = Failure.text "b\n" }),
        "expect_exact: mismatch" ) );
    ( "an unresolvable path",
      ( Failure.baseline (Failure.File "../x")
          (Failure.Unresolvable { candidate = "/tmp/x" }),
        {|expect_file "../x": cannot resolve the path under the project root|}
      ) );
    ( "a property",
      ( Fixtures.prop_failure,
        "property failed (case 12, shrunk 4 steps): Rect (2, 0)" ) );
    ( "one shrink step is singular",
      (prop ~case:0 ~steps:1 "0", "property failed (case 0, shrunk 1 step): 0")
    );
    ( "a pre-image is marked as the block marks it",
      ( prop ~rendering:Failure.Pre_image ~case:0 ~steps:0 "20",
        "property failed (case 0): computed from 20" ) );
    ( "a summarized counterexample is its summary",
      ( prop ~summary:"2 calls, last: get" ~case:0 ~steps:2
          " #  call\n 1  inc\n 2  get",
        "property failed (case 0, shrunk 2 steps): 2 calls, last: get" ) );
    ( "a timed-out shrink search",
      ( prop ~shrink_end:(Failure.Timed_out 0.3) ~case:4 ~steps:2 "9",
        "property failed (case 4, shrunk 2 steps, shrinking timed out): 9" ) );
    ( "a spent shrink budget",
      ( prop ~shrink_end:Failure.Budget_spent ~case:4 ~steps:50 "9",
        "property failed (case 4, shrunk 50 steps, shrink limit reached): 9" )
    );
    ( "a candidate that raised",
      ( prop
          ~shrink_end:
            (Failure.Candidate_raised
               (Failure.text {|Failure("no small values")|}))
          ~case:4 ~steps:3 "9",
        "property failed (case 4, shrunk 3 steps, shrinking stopped): 9" ) );
    ( "an example carries no shrink clause",
      ( prop ~shrink_end:Failure.Budget_spent ~examples:true ~case:0 ~steps:0 "9",
        "property failed (example 1): 9" ) );
    ( "a property's timeout in a case",
      ( timed_case ~examples:false 7 7,
        "timed out after 0.5s in case 7 (7 passed)" ) );
    ("a message", (Failure.message "boom", "boom"));
    ( "an empty message is named",
      (Failure.message "", "(empty failure message)") );
    ( "the user message leads, then a colon",
      (with_msg "deliberate" (unequal "1" "2"), "deliberate: expected 1, got 2")
    );
    ( "a user message before a message",
      (with_msg "context" (Failure.message "boom"), "context: boom") );
    ( "the subtest label leads",
      ( labelled "shape [0]",
        "contract \u{203a} shape [0]: expected [1; 2], got [1; 3]" ) );
    ( "a sibling subtest reads by its own label",
      ( labelled "shape [2]",
        "contract \u{203a} shape [2]: expected [1; 2], got [1; 3]" ) );
    ( "the label, the user message, then the sentence",
      ( labelled ~msg:"deliberate" "shape [0]",
        "contract \u{203a} shape [0]: deliberate: expected [1; 2], got [1; 3]"
      ) );
    ( "a found needle names its offset",
      (not_contains_failure, {|needle "secret" found at byte 10|}) );
    ( "a missing needle names the haystack's size",
      ( Failure.containment ~demand:Failure.Anywhere ~needle:"NOPE"
          ~haystack:(String.make 20_006 'a') (),
        {|needle "NOPE" not found (20006-byte haystack)|} ) );
    ( "an absent prefix",
      (prefix_absent, {|prefix "users/" not found (27-byte haystack)|}) );
    ( "a misplaced prefix",
      (prefix_elsewhere, {|prefix "ghost" found at byte 9, not at the start|})
    );
    ( "a misplaced suffix",
      (suffix_elsewhere, {|suffix "session" found at byte 0, not at the end|})
    );
    ( "an element out of order names its offset and the cursor",
      ( out_of_order,
        {|element 2 "disconnect" out of order: at byte 13, before byte 36|} ) );
    ( "a missing element names the cursor and the haystack's size",
      ( missing_element,
        {|element 2 "teardown" not found at or after byte 36 (36-byte haystack)|}
      ) );
    ( "a cut message",
      (Failure.message (String.make 70_000 'm'), String.make 80 'm' ^ "\u{2026}")
    );
    ( "two sides cut to the same kept bytes",
      ( unequal (String.make 65_536 'a' ^ "x") (String.make 65_536 'a' ^ "y"),
        "the sides agree on the 65536 bytes a failure keeps of each (expected \
         65537 bytes\u{2026}" ) );
  ]

let headline_row (_, (f, expected)) =
  equal string expected (Sections.headline f)

let headline_cut () =
  equal string long_headline
    (Sections.headline (unequal (repeat 300 "\u{00e9}") "y"))

let headline_whitespace () =
  equal string "a b c d\x01e"
    (Sections.headline (Failure.message "a\tb\rc\nd\x01e"));
  equal string "\027[31mred\027[0m alert"
    (Sections.headline (Failure.message "\027[31mred\027[0m alert"))

let collision = with_msg "contract \u{203a} shape [0]" (Failure.message "boom")

let subtest_rows =
  [
    ( "a failure recorded in a subtest",
      (Fixtures.subtest_failure "shape [0]", true) );
    ("a failure recorded outside one", (Failure.message "boom", false));
    ("a user message that spells a subtest's label", (collision, false));
  ]

let labeled_rows =
  [
    ("no message and no subtest", (Failure.message "boom", None));
    ("a message", (with_msg "context" (Failure.message "boom"), Some "context"));
    ( "a subtest, its test's own name first",
      (labelled "shape [0]", Some "contract \u{203a} shape [0]") );
    ( "a subtest and a message",
      ( labelled ~msg:"deliberate" "shape [0]",
        Some "contract \u{203a} shape [0]: deliberate" ) );
  ]

(* Failure projections: entries *)

let hostile = "\027[31mred\027[0m"
let refined = unequal "the quick brown fox" "the quick brawn fox"
let escaped_pair = unequal "\027[31mred\027[0m" "\027[32mred\027[0m"

let equality_entries () =
  [
    ("a pair that refines marks each changed span under its side", entry refined);
    ( "on a terminal, colour marks the changed spans in bold and no ~ line \
       prints",
      entry ~ansi:true refined );
    ( "off a terminal, the ~ lines print under colour too",
      entry ~ansi:true ~terminal:false refined );
    ( "a changed span of spaces keeps its ~ line under colour",
      entry ~ansi:true (unequal "a long enough  value" "a long enough value") );
    ( "a pair the refinement declines prints each side whole",
      entry (unequal "true" "false") );
    ( "and under colour each side whole in its colour",
      entry ~ansi:true (unequal "true" "false") );
    ( "the ~ line counts code points, not bytes",
      entry
        (unequal "the caf\u{00E9} is brown today"
           "the caf\u{00E9} is brawn today") );
    ( "a tab on a side: both values, no ~ line",
      entry (unequal "the\tquick brown fox" "the\tquick brawn fox") );
    ("a span at the start", entry (unequal "Xbcdefghij" "Ybcdefghij"));
    ("a span at the end", entry (unequal "abcdefghiX" "abcdefghiY"));
    ( "a pure insertion is marked under actual only",
      entry (unequal "user:alice" "user:alice:admin") );
    ( "a pure deletion is marked under expected only",
      entry (unequal "user:alice:admin" "user:alice") );
    ("two changed spans on one line", entry (unequal "a1b2c" "a9b8c"));
    ( "the last code point of fixed width is marked",
      entry (unequal "\u{24F}abc1" "\u{24F}abc2") );
    ( "one side without fixed widths draws no ~ line",
      entry (unequal "ab\u{65E5}" "abc") );
    ( "a value's escape sequences print as text, an OSC one too",
      entry (unequal (hostile ^ " one") "\027]0;title\007 two") );
    ( "a value's control bytes print escaped, the ~ lines under the escapes",
      entry escaped_pair );
    ( "a marked control byte widens its ~ to its four columns",
      entry (unequal "plain text here" "plain\027text here") );
    ( "a deleted control byte is marked under expected, as wide",
      entry (unequal "plain\027text here" "plaintext here") );
    ( "under colour the changed byte of an escaped value is bold, and the \
       value's own sequence is text",
      entry ~ansi:true escaped_pair );
    ( "one side elided, coloured, prints each side whole in its colour",
      entry ~ansi:true (unequal (String.make 810 'x') (String.make 790 'x')) );
    ( "a ?msg prints above the values",
      entry (Failure.equality ~msg:"context note" ~expected:"1" ~actual:"2" ())
    );
    ( "multi-line values are a unified diff under --- expected and +++ actual",
      entry (unequal "a\nb\nc" "a\nB\nc") );
    ( "a side that spans lines against one that does not",
      entry (unequal "a\nb" "c") );
    ("a side with no lines", entry (unequal "" "a\nb"));
    ("an empty - line answered by a + line", entry (unequal "a\n\nc" "a\nx\nc"));
    ( "a difference in a trailing newline is said, its side named: actual",
      entry (unequal "a\nb" "a\nb\n") );
    ( "a difference in a trailing newline is said, its side named: expected",
      entry (unequal "a\nb\n" "a\nb") );
    ( "identical renderings print once, then say that the printer shows less",
      entry (unequal "nan" "nan") );
    ( "identical multi-line renderings print once, indented",
      entry (unequal "line a\nline b" "line a\nline b") );
    ( "a - and a + line that differ in a trailing blank: a ~ under the blank",
      entry (unequal "line one \nline two" "line one\nline two") );
    ( "an inserted trailing run is marked at its columns",
      entry (unequal "ab\nline two" "ab  \nline two") );
    ( "a trailing tab: the ~ line repeats the line's tabs, context keeps its \
       blanks",
      entry (unequal "a\tx\t\ncommon \ny" "a\tx\ncommon \ny") );
    ( "a visible difference draws no mark",
      entry (unequal "foo \nline two" "bar\nline two") );
    ( "a run of changes is no pair and draws no mark",
      entry (unequal "a \nb \nz" "a\nb\nz") );
    ("blanks shared past the stem", entry (unequal "x  \ny" "x \ny"));
    ( "under colour the trailing blank's ~ is red, outside the line's style",
      entry ~ansi:true (unequal "line one \nline two" "line one\nline two") );
    ( "under colour a diff's expected lines are green and its actual lines red",
      entry ~ansi:true (unequal "keep\nexpected\n" "keep\nactual\n") );
    ( "every line of a diff prints escaped",
      entry
        (caught (fun () ->
             Check.equal Testable.text "header\n\027[31malert\027[0m\nfooter"
               "header\n\027[32malert\027[0m\nfooter")) );
    ( "a carriage return prints as its escape",
      entry (unequal "one\ntwo\r\nthree" "one\ntwo\nthree") );
    ( "a claim takes the expected side and is never refined",
      entry (Failure.predicate ~claim:"value satisfying the predicate" "-3") );
    ( "a multi-line value against a claim prints as a block",
      entry
        (Failure.predicate ~claim:"value satisfying the predicate" "[0; 1;\n 2]")
    );
    ( "under colour each line of that block is red, no style spans two",
      entry ~ansi:true
        (Failure.predicate ~claim:"value satisfying the predicate" "[0; 1;\n 2]")
    );
    ( "a match's claim is not refined either",
      entry (Failure.predicate ~claim:"a match" "Error \"boom\"") );
    ( "every C0 byte and DEL print as \\xNN",
      entry
        (Failure.predicate ~claim:"a clean value" "a\x00b\x07c\rd\x7fe\x1ff") );
    ( "TAB stays inside a line and LF breaks the block",
      entry (Failure.predicate ~claim:"a clean value" "one\ttwo\nthree") );
    ( "two single-line sides cut to the same kept bytes say what they agree on",
      entry
        (unequal (String.make 65_536 'a' ^ "x") (String.make 65_536 'a' ^ "y"))
    );
    ( "so do two multi-line sides",
      let lines =
        String.concat "\n" (List.init 8_000 (fun i -> strf "line %05d" i))
      in
      entry (unequal (lines ^ "\nA") (lines ^ "\nB")) );
  ]

let containment_entries () =
  let found_multi =
    Failure.containment ~found_at:14 ~demand:Failure.Anywhere ~needle:"secret"
      ~haystack:"line one\nthe1 secret here\nline three" ()
  in
  let forty =
    String.concat "\n" (List.init 40 (strf "line %02d filler filler"))
  in
  let found ~found_at needle haystack =
    Failure.containment ~found_at ~demand:Failure.Anywhere ~needle ~haystack ()
  in
  let hostile_found =
    caught (fun () -> Check.not_contains ~sub:"red" "\027[31mred\027[0m text")
  in
  [
    ( "a found needle: its offset, and a ~ line under the occurrence",
      entry not_contains_failure );
    ( "on a terminal, colour marks the occurrence in bold red and no ~ line \
       prints",
      entry ~ansi:true not_contains_failure );
    ( "off a terminal, the ~ line prints under colour too",
      entry ~ansi:true ~terminal:false not_contains_failure );
    ( "a multi-line haystack prints as a block",
      entry
        (Failure.containment ~demand:Failure.Anywhere ~needle:"user=bob"
           ~haystack:"line one\nline two user=alice\nline three" ()) );
    ("an occurrence in a block is marked under its line", entry found_multi);
    ("and under colour on its line, unmarked", entry ~ansi:true found_multi);
    ( "a missing needle shows the first ten lines, and says which bytes",
      entry
        (Failure.containment ~demand:Failure.Anywhere ~needle:"NOPE"
           ~haystack:forty ()) );
    ("an absent prefix", entry prefix_absent);
    ("a prefix found elsewhere", entry prefix_elsewhere);
    ("a suffix found elsewhere", entry suffix_elsewhere);
    ( "an absent suffix shows the end of the haystack",
      entry
        (Failure.containment ~demand:Failure.Suffix ~needle:"END"
           ~haystack:forty ()) );
    ( "an element out of order: its index, both offsets, its occurrence marked",
      entry out_of_order );
    ( "and under colour its occurrence in bold red",
      entry ~ansi:true out_of_order );
    ("a missing element names the cursor alone", entry missing_element);
    ( "a haystack's control bytes print escaped",
      entry
        (caught (fun () -> Check.contains ~sub:"NOPE" "\027[31mred\027[0m text"))
    );
    ( "the ~ line sits under the escaped occurrence, the needle keeps its \
       escapes",
      entry hostile_found );
    ( "under colour the occurrence is bold inside the escaped haystack",
      entry ~ansi:true hostile_found );
    ("an occurrence at byte 0", entry (found ~found_at:0 "ab" "abc"));
    ("an empty needle", entry (found ~found_at:0 "" "abc"));
    ("an occurrence at a line's start", entry (found ~found_at:2 "b" "a\nb"));
    ("an occurrence at a newline", entry (found ~found_at:1 "\nb" "a\nb"));
    ("an occurrence across lines", entry (found ~found_at:1 "a\nb" "xa\nb"));
  ]

let frames n =
  String.concat "" (List.init n (fun i -> strf "frame %d\n" (i + 1)))

let with_frames n = Failure.raised ~actual:"Not_found" ~backtrace:(frames n) ()

let raise_entries () =
  let head = String.make 65_536 'a' in
  [
    ( "a raise of another exception: both, then the backtrace",
      entry Fixtures.raise_failure );
    ( "a wrong message names the constructor once, the messages marked",
      entry Fixtures.raise_message_failure );
    ( "and under colour its changed spans in bold",
      entry ~ansi:true Fixtures.raise_message_failure );
    ( "the same constructor without a recorded diff prints both whole",
      entry
        (Failure.raised ~expected:{|Failure("boom")|}
           ~actual:{|Failure("boom!")|} ()) );
    ( "a predicate's miss prints the exception under its sentence",
      entry
        (Failure.raised ~actual:{|Invalid_argument("nope")|} ~predicate:true ())
    );
    ( "nothing raised against an expectation",
      entry (Failure.raised ~expected:{|Failure("boom")|} ()) );
    ( "nothing raised against a predicate",
      entry (Failure.raised ~predicate:true ()) );
    ( "a multi-line expectation that nothing met",
      entry (Failure.raised ~expected:"Parse_error(\n  line 3)" ()) );
    ( "a multi-line exception is a block under its anchor",
      entry
        (Failure.raised ~expected:{|Failure("boom")|}
           ~actual:"Parse_error(\n  line 3)" ()) );
    ( "an uncaught exception: the sentence, the exception under it",
      entry (Failure.raised ~actual:"Not_found" ()) );
    ( "a multi-line uncaught exception is a block under the sentence",
      entry (Failure.raised ~actual:"Parse_error(\n  line 3)" ()) );
    ("nothing raised and nothing expected", entry (Failure.raised ()));
    ( "ten frames of a backtrace, then the count of the rest",
      entry (with_frames 13) );
    ("the count line is faint as the frames", entry ~ansi:true (with_frames 13));
    ("ten frames print whole", entry (with_frames 10));
    ( "two messages cut to the same kept bytes say what they agree on",
      entry
        (Failure.raised ~expected:"Failure(_)" ~actual:"Failure(_)"
           ~message_diff:
             {
               Failure.constructor = "Failure";
               expected_message = Failure.text (head ^ "x");
               actual_message = Failure.text (head ^ "y");
             }
           ()) );
  ]

let file_mismatch path expected actual =
  Failure.baseline (Failure.File path)
    (Failure.Mismatch
       { expected = Failure.text expected; actual = Failure.text actual })

let missing ?(baseline = Failure.File "p.expected") proposed =
  Failure.baseline baseline
    (Failure.Missing { proposed = Failure.text proposed })

let outside = Failure.with_withheld Failure.Failed_outside

let baseline_entries () =
  let literal exact expected actual =
    Failure.baseline
      (Failure.Literal { exact })
      (Failure.Mismatch
         { expected = Failure.text expected; actual = Failure.text actual })
  in
  let proposal n = String.concat "" (List.init n (strf "line %d\n")) in
  let file = file_mismatch "p.expected" "a\n" "b\n" in
  [
    ( "a missing file: the proposal under its count, then the command that \
       creates the file and promotes it",
      entry Fixtures.snap_missing );
    ( "under a launcher, the command is -u for the block's test",
      entry ~invocation:(`Exe "./_build/default/qa/x/t.exe")
        ~filter:"cli \u{203a} cli help" Fixtures.snap_missing );
    ( "a literal: its hunks with no head, then dune promote of its source file",
      entry Fixtures.snap_mismatch );
    ( "a file: dune promote of its path",
      entry (file_mismatch "test/help.expected" "a\n" "b\n") );
    ( "a quote in the test's path is closed around",
      entry ~invocation:exe ~filter:"it's" Fixtures.snap_mismatch );
    ("expect names the flexible verb", entry (literal false "a\n" "b\n"));
    ("expect_exact names the exact verb", entry (literal true "a\n" "b\n"));
    ( "a difference in a trailing newline is said in words",
      entry (literal true "exact" "exact\n") );
    ( "a trailing blank is marked under its - line",
      entry (literal true "a \nb\n" "a\nb\n") );
    ( "a baseline's control bytes print escaped",
      entry (literal false "\027[1mbold\027[0m\n" "bold\n") );
    ( "an unresolvable file: the rule, the candidate, the remedy, no command",
      entry
        (Failure.baseline (Failure.File "../n.expected")
           (Failure.Unresolvable { candidate = "some/candidate" })) );
    ( "an unresolvable literal names expect",
      entry
        (Failure.baseline
           (Failure.Literal { exact = false })
           (Failure.Unresolvable { candidate = "/elsewhere/t.ml" })) );
    ( "a proposal of 25 lines prints 20 and the count of the rest",
      entry (missing (proposal 25)) );
    ( "a proposal of 20 lines prints whole",
      entry
        (missing ~baseline:(Failure.Literal { exact = false }) (proposal 20)) );
    ("a proposal of one line", entry (missing "only\n"));
    ( "under colour the + lines are red, their indentation plain",
      entry ~ansi:true (missing "only\n") );
    ( "two sides cut to the same kept bytes say what they agree on",
      entry
        (file_mismatch "p.expected"
           (String.make 65_536 'a' ^ "x")
           (String.make 65_536 'a' ^ "y")) );
    ( "a withheld literal by hand: the reason, and nothing after it",
      entry ~invocation:exe ~filter:"t" (outside Fixtures.snap_mismatch) );
    ( "a withheld file by hand: the reason, and nothing after it",
      entry ~invocation:exe ~filter:"t" (outside file) );
    ( "a withheld literal under a build action: nothing to promote",
      entry ~filter:"t" (outside Fixtures.snap_mismatch) );
    ( "a withheld file under a build action: nothing to promote",
      entry ~filter:"t" (outside file) );
    ( "a withheld missing file: its proposal, and no file to touch",
      entry ~filter:"t" (outside Fixtures.snap_missing) );
  ]

let property_entries () =
  let pre_image =
    prop ~rendering:Failure.Pre_image ~case:19 ~steps:9
      "[2; 3] -> ([1.; 2.], [0.; 0.])"
  in
  let program =
    prop ~summary:"3 calls, last: get" ~case:0 ~steps:2
      " #  reference before  call\n\
      \ 1                    let c1 = create ()\n\
      \ 2  0                 inc c1 3\n\
      \ 3  3                 get c1"
  in
  [
    ( "the counterexample with its case and steps, the inner failure at its \
       location, the replay",
      entry Fixtures.prop_failure );
    ("one shrink step is singular", entry (prop ~case:0 ~steps:1 "0"));
    ( "an example is numbered from one and has no replay",
      entry (prop ~examples:true ~case:0 ~steps:0 "Rect (2, 0)") );
    ("a pre-image is marked on the head and explained under it", entry pre_image);
    ("and the explanation is faint, line by line", entry ~ansi:true pre_image);
    ( "a multi-line pre-image",
      entry
        (prop ~rendering:Failure.Pre_image ~case:3 ~steps:0 "1 ->\n  [2; 3]") );
    ( "a multi-line counterexample is a block under its head",
      entry (prop ~case:3 ~steps:0 "Rect\n  (2, 0)") );
    ( "a summarized counterexample: the summary on the head, the table under it",
      entry program );
    ("and only the table's header row is faint", entry ~ansi:true program);
    ( "a timed-out search says so under the counterexample",
      entry (prop ~shrink_end:(Failure.Timed_out 0.3) ~case:4 ~steps:2 "9") );
    ( "a spent budget says so under the counterexample",
      entry (prop ~shrink_end:Failure.Budget_spent ~case:4 ~steps:50 "9") );
    ( "a candidate that raised is named, then the search's stop",
      entry
        (prop
           ~shrink_end:
             (Failure.Candidate_raised
                (Failure.text {|Failure("no small values")|}))
           ~case:4 ~steps:3 "9") );
    ( "an inner failure without a location is which failed with",
      entry
        (Failure.property ~inner:(unequal "true" "false") ~rendered:"7"
           ~case_index:0 ~shrink_steps:0 ~root ~examples:false ()) );
    ( "an uncaught exception inside a property",
      entry
        (Failure.property
           ~inner:(Failure.raised ~actual:"Dune__exe__V.Boom(50)" ())
           ~rendered:"50" ~case_index:3 ~shrink_steps:2 ~root ~examples:false ())
    );
  ]

(* Located failures of every kind, at one site. *)
let declared = Fixtures.loc "test/test_users.ml" 88
let located f = { f with Failure.loc = Some declared }

let uncaught =
  Failure.raised ~actual:"Not_found"
    ~backtrace:"Raised at Parser.parse in file \"lib/parser.ml\", line 40" ()

let teardown = Failure.with_phase Failure.Teardown

(* The source files live under a project root of the test's own, so that an
   entry names them by the relative path it prints. *)
let with_sources () =
  let dir = temp_dir () in
  setenv "WINDTRAP_PROJECT_ROOT" (Some dir);
  let write name text =
    Out_channel.with_open_bin (Filename.concat dir name) (fun oc ->
        output_string oc text)
  in
  write "excerpt_src.ml" "let one = 1\n    \tlet two = 2\nlet three = 3\n";
  write "excerpt_wild.ml"
    ("  let red = \"\027[31mred\027[0m\" (* \r \007 *)\r\n"
   ^ String.make 900 'x' ^ "\n\n")

let at_source ?(file = "excerpt_src.ml") line =
  Failure.equality
    ~loc:{ Loc.file; line; column = 0 }
    ~expected:"1" ~actual:"2" ()

let entry_entries () =
  with_sources ();
  let tail = located (unequal "1" "2") in
  [
    ("a location opens the entry, bare", entry tail);
    ("under colour the location is faint", entry ~ansi:true tail);
    ("a located file baseline", entry (located Fixtures.snap_missing));
    ("a located literal", entry (located Fixtures.snap_mismatch));
    ("a located message", entry (located (Failure.message "boom")));
    ("a located raise", entry (located Fixtures.raise_failure));
    ("a located property", entry (located Fixtures.prop_failure));
    ("a located uncaught exception", entry (located uncaught));
    ("a recorded location", entry Fixtures.eq_failure);
    ( "a timeout: the location, then the fact",
      entry (located (Failure.timeout 0.2)) );
    ( "a property's timeout names its case and the passes, and replays",
      entry ~invocation:exe ~filter:"p"
        (located (timed_case ~count:500 ~examples:false 7 7)) );
    ( "an example's timeout has no replay",
      entry ~invocation:exe ~filter:"p"
        (located (timed_case ~examples:true 1 1)) );
    ("no location: the entry opens on its facts", entry (unequal "1" "2"));
    ( "a phase is its tag before the location",
      entry (teardown (located uncaught)) );
    ( "before a failure in tail position",
      entry (Failure.with_phase Failure.Setup tail) );
    ( "before a recorded line",
      entry (teardown (Failure.message ~loc:declared "could not restore")) );
    ( "a fixture release names the fixture's site",
      entry
        (Failure.with_phase Failure.Release
           (Failure.message ~loc:declared "db: release raised Exit")) );
    ( "under colour the tag is yellow and the location faint",
      entry ~ansi:true
        (teardown (Failure.message ~loc:declared "could not restore")) );
    ( "a phase without a location stands alone",
      entry (teardown (Failure.message "x")) );
    ( "a source line under its location, dedented, a blank line after it",
      entry ~excerpt:true (at_source 2) );
    ( "under colour its gutter is faint",
      entry ~ansi:true ~excerpt:true (at_source 2) );
    ("without ~excerpt, no source line", entry (at_source 2));
    ( "an inner failure keeps its source line, with no blank line after it",
      entry ~excerpt:true
        (Failure.property
           ~loc:{ Loc.file = "excerpt_src.ml"; line = 1; column = 0 }
           ~inner:(at_source 2) ~rendered:"0" ~case_index:0 ~shrink_steps:0
           ~root ~examples:false ()) );
    ( "a source line's control bytes print escaped, and a CRLF line ends at \
       its text",
      entry ~excerpt:true (at_source ~file:"excerpt_wild.ml" 1) );
    ( "and under colour",
      entry ~ansi:true ~excerpt:true (at_source ~file:"excerpt_wild.ml" 1) );
    ( "an empty source line is its gutter alone",
      entry ~excerpt:true (at_source ~file:"excerpt_wild.ml" 3) );
    ( "an unreadable file prints neither the line nor the blank line",
      entry ~excerpt:true (at_source ~file:"does_not_exist.ml" 2) );
    ( "a subtest's name, in the column of the values under it",
      entry (Fixtures.subtest_failure "shape [0]") );
    ( "a ?msg keeps its lines, control bytes escaped",
      entry
        (Failure.equality ~msg:"first\nsecond\x07" ~expected:"1" ~actual:"2" ())
    );
    ("an empty message is named", entry (Failure.message ""));
    ( "an entry with nothing to accept or replay ends on its facts",
      entry ~invocation:exe ~filter:"math \u{203a} adds" (Failure.message "b")
    );
    ( "under a build action too",
      entry ~filter:"math \u{203a} adds" (Failure.message "b") );
    ( "and in an armed run",
      entry ~invocation:exe ~armed ~filter:"math \u{203a} adds"
        (Failure.message "b") );
    ( "a message's escape prints as text",
      entry (Failure.message (hostile ^ " boom")) );
    ( "and under colour as text too",
      entry ~ansi:true (Failure.message (hostile ^ " boom")) );
  ]

(* Laws of the entries *)

let refinement_pairs =
  [
    ("a span at the start", unequal "Xbcdefghij" "Ybcdefghij");
    ("a pure insertion", unequal "user:alice" "user:alice:admin");
    ("a pure deletion", unequal "user:alice:admin" "user:alice");
    ("a tab on a side", unequal "the\tquick brown fox" "the\tquick brawn fox");
    ("a declined pair", unequal "ab" "cd");
    ("a diff", unequal "one\ntwo\nthree\n" "one\nTWO\nthree\n");
    ("a found needle", not_contains_failure);
    ( "an occurrence in a block",
      Failure.containment ~found_at:14 ~demand:Failure.Anywhere ~needle:"secret"
        ~haystack:"line one\nthe1 secret here\nline three" () );
    ("an element out of order", out_of_order);
    ( "an occurrence among escapes",
      caught (fun () -> Check.not_contains ~sub:"red" "\027[31mred\027[0m text")
    );
  ]

let colour_replaces_marks (_, f) =
  equal text (without_marks (entry f)) (Gallery.unstyled (entry ~ansi:true f))

let plain_rows =
  [
    ( "a trailing blank's ~, on a terminal",
      (true, unequal "line one \nline two" "line one\nline two") );
    ("a found needle, off a terminal", (false, not_contains_failure));
  ]

let colour_keeps_marks (_, (terminal, f)) =
  equal text (entry f) (Gallery.unstyled (entry ~ansi:true ~terminal f))

let unaligned_rows =
  [
    ("a tab", unequal "the\tquick brown fox" "the\tquick brawn fox");
    ( "a code point past U+024F",
      unequal "\u{65E5}\u{672C} quick brown fox"
        "\u{65E5}\u{672C} quick brawn fox" );
    ("malformed UTF-8", unequal "\xff quick brown fox" "\xff quick brawn fox");
    ( "a combining accent",
      unequal "cafe\u{0301} quick brown fox" "cafe\u{0301} quick brawn fox" );
    ("an emoji", unequal "\u{1F642} quick brown fox" "\u{1F642} quick brawn fox");
    ("short values, which refinement declines", unequal "ab" "cd");
    ("an elided side", unequal (String.make 900 'a' ^ "x") "ax");
    ("a claim", Failure.predicate ~claim:"value satisfying the predicate" "-3");
    ("a match's claim", Failure.predicate ~claim:"a match" "Error \"boom\"");
    ("a raise, whose anchors state the difference", Fixtures.raise_failure);
  ]

let no_mark (_, f) =
  equal (list string) [] (List.filter is_mark_line (lines (entry f)))

let hostile_rows =
  [
    ( "a value, a styled one and an OSC one",
      unequal (hostile ^ " one") "\027]0;title\007 two" );
    ("a message", Failure.message (hostile ^ " boom"));
    ("a refined pair", escaped_pair);
    ( "a diff",
      caught (fun () ->
          Check.equal Testable.text "header\n\027[31malert\027[0m\nfooter"
            "header\n\027[32malert\027[0m\nfooter") );
    ( "a carriage return in a diff",
      unequal "one\ntwo\r\nthree" "one\ntwo\nthree" );
    ( "a haystack",
      caught (fun () -> Check.contains ~sub:"NOPE" "\027[31mred\027[0m text") );
    ( "a found needle's haystack",
      caught (fun () -> Check.not_contains ~sub:"red" "\027[31mred\027[0m text")
    );
    ( "a claim's value",
      Failure.predicate ~claim:"a clean value" "a\x00b\x07c\rd\x7fe\x1ff" );
    ( "a baseline",
      Failure.baseline
        (Failure.Literal { exact = false })
        (Failure.Mismatch
           {
             expected = Failure.text "\027[1mbold\027[0m\n";
             actual = Failure.text "bold\n";
           }) );
  ]

let no_control (_, f) = equal (list string) [] (controls (entry f))

let never_runs_rows =
  [
    ("a message", (Failure.message (hostile ^ " boom"), hostile));
    ("a refined pair", (escaped_pair, "\027[31mred"));
    ( "a haystack",
      ( caught (fun () ->
            Check.not_contains ~sub:"red" "\027[31mred\027[0m text"),
        "\027[31mred\027[0m text" ) );
  ]

let never_runs (_, (f, sequence)) =
  not_contains ~sub:sequence (entry ~ansi:true f)

let raw_bytes () =
  let f = caught (fun () -> Check.equal Testable.string "\027" "\\x1b") in
  let sides = function
    | { Failure.kind = Failure.Equality { expected; actual; _ }; _ } ->
        Some (expected.kept, actual.kept)
    | _ -> None
  in
  equal (pair string string) ({|"\027"|}, {|"\\x1b"|}) (require_match sides f)

let collision_is_no_rendering () =
  not_contains ~sub:"both sides render as"
    (entry (caught (fun () -> Check.equal Testable.text "\027" "\\x1b")))

(* Bounds *)

let side c n = String.concat "\n" (List.init n (strf "%c%d" c))

let diff_bound () =
  let e = entry (unequal (side 'e' 500) (side 'a' 500)) in
  let hunk_line l =
    (not (List.mem l [ "    --- expected"; "    +++ actual" ]))
    && List.exists
         (fun prefix -> String.starts_with ~prefix l)
         [ "    @@"; "    -"; "    +" ]
  in
  equal int 200 (List.length (List.filter hunk_line (lines e)));
  ends_with ~affix:"    \u{2026} (+801 more diff lines)\n" e

let diff_at_bound () =
  not_contains ~sub:"more diff lines"
    (entry (unequal (side 'e' 100) (side 'a' 99)))

let baseline_diff_bound () =
  let e =
    entry
      (file_mismatch "p.expected" (side 'e' 300 ^ "\n") (side 'a' 300 ^ "\n"))
  in
  ends_with
    ~affix:"(+401 more diff lines)\n    accept: dune promote p.expected\n" e

let e n = repeat n "\u{00e9}"

let elided_value () =
  equal text
    ("    expected  " ^ e 200 ^ "\u{2026} (4 bytes elided)" ^ e 199 ^ "x\n"
   ^ "    actual    " ^ e 200 ^ "\u{2026} (4 bytes elided)" ^ e 199 ^ "y\n")
    (entry (unequal (e 401 ^ "x") (e 401 ^ "y")))

let whole_value () =
  let b = entry (unequal (e 399 ^ "ax") (e 399 ^ "ay")) in
  equal (list string)
    [
      "    expected  " ^ e 399 ^ "ax";
      String.make 414 ' ' ^ "~";
      "    actual    " ^ e 399 ^ "ay";
      String.make 414 ' ' ^ "~";
      "";
    ]
    (lines b)

let carried_count () =
  let v = "\001" ^ String.make 900 'a' in
  equal text
    ("    both sides equal: \\x01" ^ String.make 399 'a'
   ^ "\u{2026} (101 bytes elided)" ^ String.make 400 'a' ^ "\n")
    (entry (Failure.equality ~not_:true ~expected:v ~actual:v ()))

let elided_counterexample () =
  equal text
    ("    counterexample (case 0): " ^ String.make 400 'a'
   ^ "\u{2026} (1 bytes elided)" ^ String.make 400 'a'
   ^ "\n    replay: WINDTRAP_SEED=" ^ seed ^ " dune runtest\n")
    (entry (prop ~case:0 ~steps:0 (String.make 801 'a')))

let elided_needle () =
  let nichi n = repeat n "\\230\\151\\165" in
  let b =
    entry
      (Failure.containment ~demand:Failure.Anywhere
         ~needle:(repeat 300 "\u{65e5}") ~haystack:"hay" ())
  in
  equal string
    ("    needle    \"" ^ nichi 133 ^ "\u{2026} (102 bytes elided)" ^ nichi 133
   ^ "\": not found")
    (List.hd (lines b))

let lines_not_elided () =
  let v = String.make 500 'a' ^ "\n" ^ String.make 500 'b' in
  equal text
    ("    both sides equal:\n      " ^ String.make 500 'a' ^ "\n      "
   ^ String.make 500 'b' ^ "\n")
    (entry (Failure.equality ~not_:true ~expected:v ~actual:v ()))

let elided_source () =
  with_sources ();
  let b = entry ~excerpt:true (at_source ~file:"excerpt_wild.ml" 2) in
  equal string
    ("      2 \u{2502} " ^ String.make 400 'x' ^ "\u{2026} (100 bytes elided)"
   ^ String.make 400 'x')
    (List.nth (lines b) 1)

let cut_marker = "... (truncated; 70000 bytes total)"

let cut_message () =
  ends_with
    ~affix:(String.make 20 'm' ^ cut_marker ^ "\n")
    (entry (Failure.message (String.make 70_000 'm')))

let cut_value () =
  let b = entry (unequal (String.make 70_000 'm') "m") in
  equal string
    ("    expected  " ^ String.make 400 'm' ^ "\u{2026} (64770 bytes elided)"
   ^ String.make 366 'm' ^ cut_marker)
    (List.hd (lines b))

let one_side_cut () =
  let b =
    entry
      (unequal ("a\n" ^ String.make 70_000 'x') ("a\n" ^ String.make 100 'x'))
  in
  ends_with
    ~affix:
      "    (the diff covers the first 65536 of the 70002 bytes of expected)\n"
    b;
  not_contains ~sub:"truncated" b

let both_sides_cut () =
  let b =
    entry
      (file_mismatch "p.expected"
         ("a\n" ^ String.make 70_000 'x')
         ("b\n" ^ String.make 70_001 'x'))
  in
  contains
    ~sub:
      "\n\
      \    (the diff covers the first 65536 of the 70002 bytes of expected and \
       the first 65536 of the 70003 bytes of actual)\n"
    b;
  not_contains ~sub:"trailing newline" b

let head_window () =
  let b =
    entry
      (Failure.containment ~demand:Failure.Anywhere ~needle:"NOPE"
         ~haystack:(String.make 20_006 'a') ())
  in
  equal (list string)
    [
      "    needle    \"NOPE\": not found";
      "    haystack  " ^ String.make 1024 'a';
      "    (excerpt: bytes 0-1023 of a 20006-byte haystack)";
      "";
    ]
    (lines b)

let found_window () =
  let haystack = String.make 2_994 'x' ^ "secret" in
  let b =
    entry
      (Failure.containment ~found_at:2_994 ~demand:Failure.Anywhere
         ~needle:"secret" ~haystack ())
  in
  contains ~sub:("    haystack  " ^ haystack ^ "\n") b;
  not_contains ~sub:"(excerpt:" b

let cursor_window () =
  let b =
    entry
      (Failure.containment
         ~demand:(Failure.Ordered { index = 1; resumed_at = 9_000 })
         ~needle:"NOPE" ~haystack:(String.make 10_000 'a') ())
  in
  equal string "    (excerpt: bytes 4904-9999 of a 10000-byte haystack)"
    (List.nth (lines b) 3)

let far_found ?(demand = Failure.Anywhere) ~found_at needle haystack =
  entry (Failure.containment ~found_at ~demand ~needle ~haystack ())

let tilde_column b = String.index (List.find is_mark_line (lines b)) '~'

let far_occurrence () =
  let b =
    far_found ~found_at:10_000 "NEEDLE"
      (String.make 10_000 'a' ^ "NEEDLE" ^ String.make 10_000 'b')
  in
  let haystack =
    List.find (Text.contains_substring ~pattern:"aNEEDLE") (lines b)
  in
  equal int
    (require_some (Text.first_occurrence ~pattern:"NEEDLE" haystack))
    (tilde_column b)

let cut_occurrence () =
  let needle = String.make 5_000 'n' in
  let b =
    far_found ~found_at:10_000 needle
      (String.make 10_000 'a' ^ needle ^ String.make 5_000 'b')
  in
  equal int 4096
    (String.length
       (String.concat ""
          (List.map
             (fun l -> String.concat "" (String.split_on_char ' ' l))
             (List.filter is_mark_line (lines b)))))

let occurrence_left_behind () =
  let b =
    far_found
      ~demand:(Failure.Ordered { index = 1; resumed_at = 9_000 })
      ~found_at:0 "a" (String.make 10_000 'a')
  in
  equal (list string) [] (List.filter is_mark_line (lines b))

(* Excerpts *)

let root_first () =
  let root = temp_dir () and cwd = temp_dir () in
  let write dir text =
    Out_channel.with_open_bin (Filename.concat dir "x.ml") (fun oc ->
        output_string oc text)
  in
  write root "under the root\n";
  write cwd "as given\n";
  setenv "WINDTRAP_PROJECT_ROOT" (Some root);
  chdir cwd;
  let source () =
    List.nth
      (lines
         (entry ~excerpt:true
            (Failure.message
               ~loc:{ Loc.file = "x.ml"; line = 1; column = 0 }
               "m")))
      1
  in
  equal string "      1 \u{2502} under the root" (source ());
  Sys.remove (Filename.concat root "x.ml");
  equal string "      1 \u{2502} as given" (source ())

let absolute_source () =
  let file = Filename.concat (temp_dir ()) "x.ml" in
  Out_channel.with_open_bin file (fun oc -> output_string oc "absolute\n");
  equal string "      1 \u{2502} absolute"
    (List.nth
       (lines
          (entry ~excerpt:true
             (Failure.message ~loc:{ Loc.file; line = 1; column = 0 } "m")))
       1)

let under_dune () =
  let f =
    Failure.equality
      ~loc:
        { Loc.file = "test/unit/test_report_sections.ml"; line = 1; column = 0 }
      ~expected:"1" ~actual:"2" ()
  in
  contains ~sub:"\n      1 \u{2502} (*---" (entry ~excerpt:true f)

let hints_default () =
  let entry ?hints f =
    Format.asprintf "%a"
      (fun ppf f -> Sections.pp_failure ~ansi:false ?hints ppf f)
      f
  in
  contains ~sub:"replay:" (entry Fixtures.prop_failure);
  not_contains ~sub:"replay:" (entry ~hints:false Fixtures.prop_failure)

(* Hints and the run's commands *)

let kept_none =
  "no correction was kept: the test also failed outside its expectations; fix \
   that failure and rerun"

let refused line =
  Failure.with_withheld
    (Failure.Refused { line; reason = "the source file cannot be read: x" })
    Fixtures.snap_mismatch

let refused_fact line =
  strf "correction refused (line %d): the source file cannot be read: x" line

let plain_failure = Failure.message "b"

let all_kinds =
  [
    plain_failure;
    Fixtures.prop_failure;
    Fixtures.snap_mismatch;
    Fixtures.snap_missing;
  ]

let hint_rows =
  [
    ("a message has none", (None, `Mirrors, [ plain_failure ], []));
    ( "a literal under a build action: dune promote of its source file",
      ( None,
        `Mirrors,
        [ Fixtures.snap_mismatch ],
        [ "accept: dune promote test/test_cli.ml" ] ) );
    ( "a file under a build action: dune promote of its path",
      ( None,
        `Mirrors,
        [ file_mismatch "test/help.expected" "a\n" "b\n" ],
        [ "accept: dune promote test/help.expected" ] ) );
    ( "a missing file under a build action: touch it first",
      ( None,
        `Mirrors,
        [ Fixtures.snap_missing ],
        [
          "accept: touch 'test/help.expected' && dune runtest; dune promote \
           test/help.expected";
        ] ) );
    ( "two files under a build action: two lines",
      ( None,
        `Mirrors,
        [ Fixtures.snap_mismatch; Fixtures.snap_missing ],
        [
          "accept: dune promote test/test_cli.ml";
          "accept: touch 'test/help.expected' && dune runtest; dune promote \
           test/help.expected";
        ] ) );
    ( "equal lines print once",
      ( None,
        `Mirrors,
        [ Fixtures.snap_mismatch; Fixtures.snap_mismatch ],
        [ "accept: dune promote test/test_cli.ml" ] ) );
    ("by hand, no command: the report accepts once", (None, exe, all_kinds, []));
    ( "an armed run's baseline failures have none",
      ( Some armed,
        `Mirrors,
        [ Fixtures.snap_mismatch; Fixtures.snap_missing ],
        [] ) );
    ( "a withheld correction is a fact line, once, and nothing after it",
      ( None,
        exe,
        [
          plain_failure;
          outside Fixtures.snap_mismatch;
          outside Fixtures.snap_missing;
        ],
        [ kept_none ] ) );
    ( "a withheld correction under a build action: no command either",
      (None, `Mirrors, [ outside Fixtures.snap_mismatch ], [ kept_none ]) );
    ( "a property beside it adds no line: the report replays it",
      ( None,
        exe,
        [ Fixtures.prop_failure; outside Fixtures.snap_mismatch ],
        [ kept_none ] ) );
    ( "a skip beside the expectation has its own reason",
      ( None,
        exe,
        [ Failure.with_withheld Failure.Skipped Fixtures.snap_mismatch ],
        [
          "no correction was kept: the test also skipped; skip before the \
           expectation or not at all, and rerun";
        ] ) );
    ( "a refused literal beside a kept one: its fact",
      (None, exe, [ refused 4; Fixtures.snap_mismatch ], [ refused_fact 4 ]) );
    ( "each refused literal has its line, then a fact of the attempt",
      ( None,
        exe,
        [
          plain_failure;
          outside (refused 4);
          outside (refused 9);
          outside Fixtures.snap_mismatch;
        ],
        [ refused_fact 4; refused_fact 9; kept_none ] ) );
    ( "a conflict has its own fact",
      ( None,
        exe,
        [ Failure.with_withheld Failure.Conflict Fixtures.snap_mismatch ],
        [
          "no correction was kept: another check of this baseline produced a \
           different text earlier in the run";
        ] ) );
    ( "an unresolvable path never had a correction",
      ( None,
        exe,
        [
          outside
            (Failure.baseline (Failure.File "../x")
               (Failure.Unresolvable { candidate = "/tmp/x" }));
        ],
        [] ) );
    ( "an armed run says nothing of corrections",
      (Some armed, exe, [ plain_failure; outside Fixtures.snap_mismatch ], [])
    );
  ]

(* A row under [`Mirrors] leaves the invocation to its default. *)
let default_or (invocation : Run.invocation) =
  match invocation with `Mirrors -> None | `Exe _ -> Some invocation

let hint_row (_, (armed, invocation, failures, expected)) =
  equal (list string) expected
    (Sections.hints ?armed ?invocation:(default_or invocation) failures)

let narrowed =
  {
    (Run.default_config ()) with
    Run.filter = [ "geo" ];
    exclude = [ "slow" ];
    tags = [ "prop" ];
    exclude_tags = [ "flaky" ];
    shard = Some (1, 2);
    log_dir = "/nowhere/logs";
  }

let failed_run = { narrowed with Run.filter = []; failed_only = true }
let qa = `Exe "./_build/default/qa/x/t.exe"

let accept_rows =
  [
    ( "under a build action there is none: each block names its file",
      (None, `Mirrors, `Filter (Some "t"), all_kinds, None) );
    ( "none without a baseline",
      ( None,
        exe,
        `Filter (Some "t"),
        [ plain_failure; Fixtures.prop_failure ],
        None ) );
    ( "one for every baseline of the test",
      ( None,
        exe,
        `Filter (Some "t"),
        all_kinds,
        Some "accept: ./t.exe -u -f 't'" ) );
    ( "every test when there is no filter",
      (None, exe, `Filter None, all_kinds, Some "accept: ./t.exe -u") );
    ( "the executable completed, scoped to the test",
      ( None,
        qa,
        `Filter (Some "cli \u{203a} cli help"),
        [ Fixtures.snap_missing ],
        Some "accept: ./_build/default/qa/x/t.exe -u -f 'cli \u{203a} cli help'"
      ) );
    ( "a quote in the path is closed around",
      ( None,
        exe,
        `Filter (Some "it's"),
        [ Fixtures.snap_mismatch ],
        Some "accept: ./t.exe -u -f 'it'\\''s'" ) );
    ( "an armed run accepts nothing",
      (Some armed, exe, `Filter (Some "t"), [ Fixtures.snap_mismatch ], None) );
    ( "a withheld correction is accepted by no command",
      ( None,
        exe,
        `Filter (Some "t"),
        [
          plain_failure;
          outside Fixtures.snap_mismatch;
          outside Fixtures.snap_missing;
        ],
        None ) );
    ( "a refused literal beside a kept one: the kept one is accepted",
      ( None,
        exe,
        `Filter (Some "t"),
        [ refused 4; Fixtures.snap_mismatch ],
        Some "accept: ./t.exe -u -f 't'" ) );
    ( "a run's selection is restated",
      ( None,
        exe,
        `Run narrowed,
        [ Fixtures.snap_mismatch ],
        Some
          "accept: ./t.exe -u -f 'geo' -e 'slow' --tag prop --exclude-tag \
           flaky --shard 1/2" ) );
    ( "a run given --failed restates it after the -o it reads under",
      ( None,
        exe,
        `Run failed_run,
        [ Fixtures.snap_mismatch ],
        Some
          "accept: ./t.exe -u -e 'slow' --tag prop --exclude-tag flaky --shard \
           1/2 -o /nowhere/logs --failed" ) );
  ]

let accept_row (_, (armed, invocation, tests, failures, expected)) =
  equal (option string) expected
    (Sections.accept ?armed ?invocation:(default_or invocation) ~tests failures)

let counted count =
  Failure.property ~count ~rendered:"0" ~case_index:499 ~shrink_steps:1 ~root
    ~examples:false ()

let replay_rows =
  let replay s = Some ("replay: " ^ s) in
  [
    ( "a build action's: the seed's mirror in front of dune runtest",
      ( None,
        `Mirrors,
        `Filter None,
        [ Fixtures.prop_failure ],
        replay ("WINDTRAP_SEED=" ^ seed ^ " dune runtest") ) );
    ( "its filter is quoted for a shell",
      ( None,
        `Mirrors,
        `Filter (Some "it's \u{203a} tricky"),
        [ Fixtures.prop_failure ],
        replay
          ("WINDTRAP_SEED=" ^ seed
         ^ " WINDTRAP_FILTER='it'\\''s \u{203a} tricky' dune runtest") ) );
    ( "a count restates its mirror",
      ( None,
        `Mirrors,
        `Filter None,
        [ counted 1000 ],
        replay
          ("WINDTRAP_SEED=" ^ seed ^ " WINDTRAP_PROP_COUNT=1000 dune runtest")
      ) );
    ( "a launcher's: the seed, then the filter",
      ( None,
        qa,
        `Filter (Some "mod7"),
        [ Fixtures.prop_failure ],
        replay ("./_build/default/qa/x/t.exe --seed " ^ seed ^ " -f 'mod7'") )
    );
    ( "without a filter, the seed alone",
      ( None,
        qa,
        `Filter None,
        [ Fixtures.prop_failure ],
        replay ("./_build/default/qa/x/t.exe --seed " ^ seed) ) );
    ( "a count restates --prop-count",
      ( None,
        exe,
        `Filter (Some "late"),
        [ counted 1000 ],
        replay ("./t.exe --seed " ^ seed ^ " --prop-count 1000 -f 'late'") ) );
    ( "a spent budget adds no clause",
      ( None,
        exe,
        `Filter (Some "late"),
        [
          Failure.property ~count:1000 ~rendered:"0" ~case_index:499
            ~shrink_steps:10_000 ~shrink_end:Failure.Budget_spent ~root
            ~examples:false ();
        ],
        replay ("./t.exe --seed " ^ seed ^ " --prop-count 1000 -f 'late'") ) );
    ( "an armed run's keeps the mutant armed",
      ( Some armed,
        exe,
        `Filter (Some "mod7"),
        [ Fixtures.prop_failure ],
        replay ("./t.exe --arm " ^ armed ^ " --seed " ^ seed ^ " -f 'mod7'") )
    );
    ( "an identifier a shell would split is quoted",
      ( Some "my lib/calc.ml:9:12:add",
        exe,
        `Filter None,
        [ Fixtures.prop_failure ],
        replay ("./t.exe --arm 'my lib/calc.ml:9:12:add' --seed " ^ seed) ) );
    ( "an armed build action's names the mutation backend",
      ( Some armed,
        `Mirrors,
        `Filter (Some "mod7"),
        [ Fixtures.prop_failure ],
        replay
          ("WINDTRAP_MUTATE_ARM=" ^ armed ^ " WINDTRAP_SEED=" ^ seed
         ^ " WINDTRAP_FILTER='mod7' dune runtest --instrument-with \
            ppx_windtrap.mutate") ) );
    ( "a control byte in a path takes the $'...' form",
      ( None,
        exe,
        `Filter (Some "it's\ttwo\nlines\027[0m"),
        [ Fixtures.prop_failure ],
        replay ("./t.exe --seed " ^ seed ^ " -f $'it\\'s\\ttwo\\nlines\\x1b[0m'")
      ) );
    ( "a property's timeout in a case replays it",
      ( None,
        exe,
        `Filter (Some "p"),
        [ timed_case ~count:500 ~examples:false 7 7 ],
        replay ("./t.exe --seed " ^ seed ^ " --prop-count 500 -f 'p'") ) );
    ( "an example drew nothing",
      ( None,
        exe,
        `Filter None,
        [ prop ~examples:true ~case:0 ~steps:0 "0" ],
        None ) );
    ( "an example's timeout drew nothing",
      (None, exe, `Filter (Some "p"), [ timed_case ~examples:true 1 1 ], None)
    );
    ( "nor did a baseline",
      (None, exe, `Filter None, [ Fixtures.snap_mismatch ], None) );
    ( "the largest count a failure needs",
      ( None,
        exe,
        `Run (Run.default_config ()),
        [ counted 1000; counted 500; plain_failure ],
        replay ("./t.exe --seed " ^ seed ^ " --prop-count 1000") ) );
    ( "a run's selection is restated, with no -o it does not read",
      ( None,
        exe,
        `Run narrowed,
        [ Fixtures.prop_failure ],
        replay
          ("./t.exe --seed " ^ seed
         ^ " -f 'geo' -e 'slow' --tag prop --exclude-tag flaky --shard 1/2") )
    );
    ( "a run given --failed restates it after its -o",
      ( None,
        exe,
        `Run failed_run,
        [ Fixtures.prop_failure ],
        replay
          ("./t.exe --seed " ^ seed
         ^ " -e 'slow' --tag prop --exclude-tag flaky --shard 1/2 -o \
            /nowhere/logs --failed") ) );
    ( "and without -o when the store lies where it defaults to",
      ( None,
        exe,
        `Run { (Run.default_config ()) with Run.failed_only = true },
        [ Fixtures.prop_failure ],
        replay ("./t.exe --seed " ^ seed ^ " --failed") ) );
    ( "a build action's restates the selection's mirrors",
      ( None,
        `Mirrors,
        `Run { (Run.default_config ()) with Run.filter = [ "geo" ] },
        [ Fixtures.prop_failure ],
        replay ("WINDTRAP_SEED=" ^ seed ^ " WINDTRAP_FILTER='geo' dune runtest")
      ) );
    ( "an armed build action's run names the backend",
      ( Some armed,
        `Mirrors,
        `Run (Run.default_config ()),
        [ Fixtures.prop_failure ],
        replay
          ("WINDTRAP_MUTATE_ARM=" ^ armed ^ " WINDTRAP_SEED=" ^ seed
         ^ " dune runtest --instrument-with ppx_windtrap.mutate") ) );
  ]

let replay_row (_, (armed, invocation, tests, failures, expected)) =
  equal (option string) expected
    (Sections.replay ?armed ?invocation:(default_or invocation) ~tests failures)

let failure_projections =
  group "Failure projections"
    [
      cases "a headline is one line of the failure's facts" ~name:fst
        headline_rows headline_row;
      test "a headline past 80 code points is cut and ends in an ellipsis"
        headline_cut;
      test "a headline turns TAB, CR and LF into spaces and keeps other bytes"
        headline_whitespace;
      cases "is_subtest_failure holds iff the failure was recorded in a subtest"
        ~name:fst subtest_rows (fun (_, (f, expected)) ->
          equal bool expected (Sections.is_subtest_failure f));
      cases "labeled_msg is the subtest's label, then the message" ~name:fst
        labeled_rows (fun (_, (f, expected)) ->
          equal (option string) expected (Sections.labeled_msg f));
      group "pp_failure"
        [
          test "equality entries print as their gallery" (fun () ->
              gallery "equality" (equality_entries ()));
          test "containment entries print as their gallery" (fun () ->
              gallery "containment" (containment_entries ()));
          test "raise entries print as their gallery" (fun () ->
              gallery "raise" (raise_entries ()));
          test "baseline entries print as their gallery" (fun () ->
              gallery "baseline" (baseline_entries ()));
          test "property entries print as their gallery" (fun () ->
              gallery "property" (property_entries ()));
          test "an entry's lines print as their gallery" (fun () ->
              gallery "entry" (entry_entries ()));
          cases "on a terminal, colour replaces the ~ lines and nothing else"
            ~name:fst refinement_pairs colour_replaces_marks;
          cases "the ~ lines print under colour where colour cannot show"
            ~name:fst plain_rows colour_keeps_marks;
          cases "no ~ line prints where it would not align" ~name:fst
            unaligned_rows no_mark;
          cases "a plain entry holds no control byte but LF and TAB" ~name:fst
            hostile_rows no_control;
          cases "a payload's sequence never runs under colour" ~name:fst
            never_runs_rows never_runs;
          test "equality compares the raw bytes and the payload keeps them"
            raw_bytes;
          test "two values the escape would merge are not called one rendering"
            collision_is_no_rendering;
          test "an entry reads its source under the project root, then as given"
            root_first;
          test "an entry reads an absolute path as given" absolute_source;
          test "a path relative to the project root is found under dune"
            under_dune;
          test "an entry carries its hints by default" hints_default;
          group "bounds"
            [
              test "max_lines is 10" (fun () -> equal int 10 Sections.max_lines);
              test
                "a diff prints 200 lines of hunks, then the count of the rest"
                diff_bound;
              test "a diff of 200 lines of hunks prints whole" diff_at_bound;
              test "a baseline's diff is bounded and keeps its accept"
                baseline_diff_bound;
              test
                "a value over 800 bytes keeps 400 of each end, cut on code \
                 points"
                elided_value;
              test "a value of 800 bytes prints whole, marked" whole_value;
              test "the elided count is of the carried bytes" carried_count;
              test "a counterexample is elided as a value" elided_counterexample;
              test "a needle is elided in its carried bytes, inside its quotes"
                elided_needle;
              test "a multi-line value is not elided" lines_not_elided;
              test "a source line is elided as a value" elided_source;
              test "a cut message ends in the marker" cut_message;
              test "a cut value ends in the marker, after the elision" cut_value;
              test "one cut side: a line after the hunks names the cut"
                one_side_cut;
              test "two cut sides are both named" both_sides_cut;
              test "a missing needle shows the haystack's first KiB" head_window;
              test "a found needle shows the whole stored window" found_window;
              test "the window of an element out of order is not cut"
                cursor_window;
              test "an occurrence deep in a haystack is marked where it prints"
                far_occurrence;
              test "an occurrence cut by the window is marked up to its end"
                cut_occurrence;
              test "an occurrence the window left behind is not marked"
                occurrence_left_behind;
            ];
        ];
      cases "hints are the block's fact lines and dune promote lines" ~name:fst
        hint_rows hint_row;
      cases "accept is one -u line under a launcher" ~name:fst accept_rows
        accept_row;
      cases "replay reruns the tests with the values they drew" ~name:fst
        replay_rows replay_row;
    ]

(* Names and command words *)

let shell_rows =
  [
    ("the bare-word alphabet as is", ("aZ9_-./:=+,@%", "aZ9_-./:=+,@%"));
    ("the empty word is quoted", ("", "''"));
    ("a space quotes the word", ("a b", "'a b'"));
    ("a control byte takes the $'...' form", ("a\nb", "$'a\\nb'"));
  ]

let dune_exec_rows =
  [
    ("a path with a slash", (false, "test/t.exe", "dune exec test/t.exe --"));
    ("a bare name is made a path", (false, "t.exe", "dune exec ./t.exe --"));
    ( "the mutation backend before the path",
      ( true,
        "test/t.exe",
        "dune exec --instrument-with ppx_windtrap.mutate test/t.exe --" ) );
    ( "a path a shell would split",
      (false, "my dir/t.exe", "dune exec 'my dir/t.exe' --") );
  ]

let names =
  group "Names and command words"
    [
      test "release_title is fixture release" (fun () ->
          equal string "fixture release" Sections.release_title);
      cases "shell_word" ~name:fst shell_rows (fun (_, (s, expected)) ->
          equal string expected (Sections.shell_word s));
      cases "dune_exec" ~name:fst dune_exec_rows
        (fun (_, (mutate, path, expected)) ->
          let command = function `Exe cmd -> Some cmd | `Mirrors -> None in
          equal string expected
            (require_match command (Sections.dune_exec ~mutate path)));
    ]

(* The section vocabulary *)

let hostile_line = [ Sections.plain "a\nb\tc"; Sections.styled `Red "\027[0m" ]

let style_rows =
  [
    ("bold", (`Bold, "1"));
    ("faint", (`Faint, "2"));
    ("red", (`Red, "31"));
    ("green", (`Green, "32"));
    ("yellow", (`Yellow, "33"));
    ("bold red is one sequence", (`Bold_red, "1;31"));
    ("bold green is one sequence", (`Bold_green, "1;32"));
  ]

let column = { Sections.gap = ""; align = `Left; width = None }

let print_entries () =
  [
    ( "a row of empty cells",
      printed
        [
          Sections.Rows
            {
              margin = "  ";
              columns = [ column ];
              rows = [ [ Sections.plain "" ] ];
            };
        ] );
    ( "marked lines three apart share a region",
      printed
        [
          Sections.Excerpt
            { source = "1\n2\n3\n4\n5\n6\n7\n"; marked_lines = [ 1; 4 ] };
        ] );
    ( "marked lines at and past the edges",
      printed
        [
          Sections.Excerpt { source = "a\nb\nc\n"; marked_lines = [ 3; 7; 0 ] };
        ] );
  ]

let rows_print () =
  let rows =
    Sections.Rows
      {
        margin = "  ";
        columns = [ column; { column with Sections.gap = " " } ];
        rows =
          [
            [ Sections.plain "a"; Sections.plain ""; Sections.plain "dropped" ];
            [ Sections.plain "bb"; Sections.plain "c" ];
          ];
      }
  in
  equal text "  a\n  bb c\n" (printed [ rows ])

let print_flushes () =
  let b = Buffer.create 64 in
  Sections.print
    ~out:(Format.formatter_of_buffer b)
    ~ansi:false
    [ Sections.Line [ Sections.plain "x" ] ];
  equal string "x\n" (Buffer.contents b)

let long_rule () =
  let label = String.make 30 'l' in
  let r = Sections.rule ~width:20 (Some label) in
  contains ~sub:label r;
  greater int ~than:20 (Text.length_utf8 r)

let vocabulary =
  group "The section vocabulary"
    [
      test "render escapes every span and styles nothing without ansi"
        (fun () ->
          equal string "a\\x0ab\tc\\x1b[0m"
            (Sections.render ~ansi:false hostile_line));
      test "render wraps an escaped span in its style under ansi" (fun () ->
          equal string "a\\x0ab\tc\027[31m\\x1b[0m\027[0m"
            (Sections.render ~ansi:true hostile_line));
      test "render leaves an empty styled span bare" (fun () ->
          equal string ""
            (Sections.render ~ansi:true [ Sections.styled `Faint "" ]));
      cases "render writes each style's sequence" ~name:fst style_rows
        (fun (_, (style, code)) ->
          equal string
            ("\027[" ^ code ^ "mx\027[0m")
            (Sections.render ~ansi:true [ Sections.styled style "x" ]));
      test "width counts each escape's four columns" (fun () ->
          equal int 15 (Sections.width hostile_line));
      test "a rule is width columns" (fun () ->
          equal int 20 (Text.length_utf8 (Sections.rule ~width:20 None)));
      test "a long label is whole and takes the rule past its width" long_rule;
      test "print writes a hint unstyled" (fun () ->
          equal string "cmd --flag\n"
            (printed ~ansi:true [ Sections.Hint "cmd --flag" ]));
      test
        "print strips a row's trailing spaces and drops cells past its columns"
        rows_print;
      test "print flushes its formatter" print_flushes;
      test "rows and excerpts print as their gallery" (fun () ->
          gallery "print" (print_entries ()));
    ]

(* Coverage *)

let lines_of ranges =
  List.concat_map (fun (s, e) -> List.init (e - s + 1) (fun i -> s + i)) ranges

let coverage_file ?source ?(stale = false) file visited total uncovered =
  {
    Sections.file;
    visited;
    total;
    uncovered = lines_of uncovered;
    source;
    stale;
  }

let table =
  {
    Sections.visited = 312;
    total = 437;
    files =
      [
        coverage_file "lib/env.ml" 50 52 [ (88, 89) ];
        coverage_file "lib/eval.ml" 69 119
          [
            (41, 47);
            (60, 60);
            (93, 104);
            (131, 131);
            (140, 152);
            (160, 170);
            (180, 180);
            (190, 195);
            (200, 200);
            (210, 210);
            (220, 230);
          ];
        coverage_file "lib/lexer.ml" 38 38 [];
        coverage_file "lib/parser.ml" 91 142
          [
            (17, 17);
            (52, 58);
            (77, 77);
            (102, 119);
            (140, 140);
            (151, 160);
            (170, 170);
            (180, 180);
            (190, 190);
            (200, 200);
          ];
        coverage_file "lib/printer.ml" 64 86 [ (23, 31); (70, 74); (90, 90) ];
      ];
  }

let source_of texts =
  let last = List.fold_left (fun n (line, _) -> max n line) 0 texts in
  String.concat "\n"
    (List.init last (fun i ->
         Option.value ~default:"" (List.assoc_opt (i + 1) texts)))
  ^ "\n"

let source_view =
  let env =
    source_of
      [
        (87, "  | Some frame ->");
        (88, "      if frame.sealed then invalid_arg \"Env.set: sealed frame\"");
        (89, "      else Hashtbl.replace frame.vars name v");
        (90, "  | None -> raise Not_found");
      ]
  and eval =
    source_of
      [
        (40, "  | Let (x, e, body) ->");
        (41, "      let v = eval env e in");
        (42, "      eval (Env.bind env x v) body");
        (43, "  | If (c, t, e) ->");
        (59, "  | Div (a, b) ->");
        (60, "      if eval env b = Int 0 then raise Division_by_zero");
        (61, "      else div (eval env a) (eval env b)");
      ]
  in
  {
    Sections.visited = 119;
    total = 171;
    files =
      [
        coverage_file ~source:env "lib/env.ml" 50 52 [ (88, 89) ];
        coverage_file ~source:eval "lib/eval.ml" 69 119 [ (41, 42); (60, 60) ];
      ];
  }

let coverage ?(ansi = false) ?(mode = `Report) ?min c =
  printed ~ansi (Sections.coverage_report ~mode ~min c)

let one_file (f : Sections.coverage_file) =
  { Sections.visited = f.visited; total = f.total; files = [ f ] }

let coverage_entries () =
  let escapes =
    one_file
      (coverage_file
         ~source:"let plain = 1\nlet red = \"\027[31mred\r\"\nlet c = 3\n"
         "esc.ml" 1 2
         [ (2, 2) ])
  in
  [
    ("the table under a gate it misses", coverage ~ansi:true ~min:80. table);
    ("the table under a gate it meets", coverage ~ansi:true ~min:70. table);
    ("the table without colour", coverage ~min:80. table);
    ( "the source view, under a gate",
      coverage ~ansi:true ~mode:`Full ~min:80. source_view );
    ("the source view without colour", coverage ~mode:`Full ~min:80. source_view);
    ( "a region at the top of a file has no line above it",
      coverage ~mode:`Full
        (one_file
           (coverage_file ~source:"let a = 1\nlet b = 2\nlet c = 3\n" "top.ml" 1
              2
              [ (1, 1) ])) );
    ( "a long path keeps the header's hint",
      coverage
        (one_file
           (coverage_file "examples/07-coverage/a_long_module.ml" 1 2
              [ (9, 9) ])) );
    ( "a label is a floor on its column's width",
      coverage (one_file (coverage_file "a.ml" 4 8 [ (3, 3) ])) );
    ( "points wider than their header",
      coverage (one_file (coverage_file "a.ml" 100 200 [])) );
    ( "unvisited points without a line say why, and a stale file what to do",
      coverage
        {
          Sections.visited = 2;
          total = 4;
          files =
            [
              coverage_file "examples/07-coverage/half_a.ml" 1 2 [];
              coverage_file ~stale:true "examples/07-coverage/half_b.ml" 1 2 [];
            ];
        } );
    ( "a row at the gate is plain and one under it red",
      coverage ~ansi:true ~min:50.
        {
          Sections.visited = 9;
          total = 20;
          files =
            [ coverage_file "at.ml" 5 10 []; coverage_file "under.ml" 4 10 [] ];
        } );
    ( "without a gate a row below 80 is red and no row is green or yellow",
      coverage ~ansi:true
        {
          Sections.visited = 24;
          total = 30;
          files =
            [
              coverage_file "full.ml" 10 10 [];
              coverage_file "most.ml" 8 10 [];
              coverage_file "some.ml" 6 10 [];
            ];
        } );
    ("a source's control bytes print as text", coverage ~mode:`Full escapes);
    ("and under colour", coverage ~ansi:true ~mode:`Full escapes);
  ]

let last_line out =
  match List.rev (lines out) with
  | "" :: last :: _ -> last
  | _ -> "\u{ab}the output does not end on a newline\u{bb}"

let outcome_rows =
  [
    ( "the table, no gate",
      (`Report, None, table, "coverage: 71.4% (312/437 points)") );
    ( "the source view, no gate",
      (`Full, None, source_view, "coverage: 69.6% (119/171 points)") );
    ( "the source view, a gate missed",
      ( `Full,
        Some 80.,
        source_view,
        "coverage: 69.6% (119/171 points), minimum 80%: FAILED" ) );
    ( "the source view, a gate met",
      ( `Full,
        Some 69.5,
        source_view,
        "coverage: 69.6% (119/171 points), minimum 69.5%: ok" ) );
  ]

let outcome_once () =
  let out = coverage ~min:80. table in
  equal (list string)
    [ "coverage: 71.4% (312/437 points), minimum 80%: FAILED" ]
    (List.filter (String.starts_with ~prefix:"coverage: ") (lines out))

let range_row n name =
  let f =
    coverage_file name 0 60 (List.init n (fun i -> ((i * 4) + 1, (i * 4) + 2)))
  in
  List.nth (lines (coverage (one_file f))) 1

let eight = "1-2, 5-6, 9-10, 13-14, 17-18, 21-22, 25-26, 29-30"

let long_path =
  "a/very/long/path/to/a/barely/tested/module/in/a/deep/tree/wide.ml"

let range_rows =
  [
    ( "eight ranges, then the count of the rest",
      ( 30,
        "lib/wide.ml",
        "    0.0%    0/60     lib/wide.ml   " ^ eight ^ " (+22 more)" ) );
    ( "eight ranges print whole",
      (8, "lib/wide.ml", "    0.0%    0/60     lib/wide.ml   " ^ eight) );
    ( "a long path does not cut the ranges",
      ( 9,
        long_path,
        "    0.0%    0/60     " ^ long_path ^ "   " ^ eight ^ " (+1 more)" ) );
  ]

let outcome_colour_rows =
  let ok = "\027[32mok\027[0m" and failed = "\027[31mFAILED\027[0m" in
  [
    ( "no gate: 80 percent is plain",
      (None, 80, 100, "coverage: 80.0% (80/100 points)") );
    ( "no gate: below 80 is red",
      (None, 799, 1000, "coverage: \027[31m79.9%\027[0m (799/1000 points)") );
    ( "a gate met at its boundary: plain, and ok green",
      (Some 70., 70, 100, "coverage: 70.0% (70/100 points), minimum 70%: " ^ ok)
    );
    ( "a gate missed: red, and FAILED red",
      ( Some 70.,
        699,
        1000,
        "coverage: \027[31m69.9%\027[0m (699/1000 points), minimum 70%: "
        ^ failed ) );
    ( "a gate above 80 reddens what no gate would not",
      ( Some 90.,
        85,
        100,
        "coverage: \027[31m85.0%\027[0m (85/100 points), minimum 90%: " ^ failed
      ) );
    ( "the gate compares the unrounded percentage",
      ( Some 66.7,
        2,
        3,
        "coverage: \027[31m66.7%\027[0m (2/3 points), minimum 66.7%: " ^ failed
      ) );
    ("no point: 100% and plain", (None, 0, 0, "coverage: 100.0% (0/0 points)"));
    ( "the minimum prints as given",
      ( Some 99.99999,
        1,
        1,
        "coverage: 100.0% (1/1 points), minimum 99.99999%: " ^ ok ) );
    ( "a whole minimum without a point",
      (Some 80.0, 80, 100, "coverage: 80.0% (80/100 points), minimum 80%: " ^ ok)
    );
  ]

let outcome_colour (_, (min, visited, total, expected)) =
  equal string (expected ^ "\n")
    (coverage ~ansi:true ?min { Sections.visited; total; files = [] })

let coverage_group =
  group "Coverage"
    [
      test "percent is 100 when there is no point" (fun () ->
          equal float_exact 100. (Sections.percent ~visited:0 ~total:0));
      test "percent is unrounded" (fun () ->
          equal (float 1e-9) (200. /. 3.) (Sections.percent ~visited:2 ~total:3));
      cases "coverage_line colours the percentage and the gate" ~name:fst
        outcome_colour_rows outcome_colour;
      test "coverage reports print as their gallery" (fun () ->
          gallery "coverage" (coverage_entries ()));
      cases "the outcome is the last line in every mode" ~name:fst outcome_rows
        (fun (_, (mode, min, c, expected)) ->
          equal string expected (last_line (coverage ~mode ?min c)));
      test "the outcome prints once, the gate with it" outcome_once;
      cases "a row shows eight ranges, then the count of the rest" ~name:fst
        range_rows (fun (_, (n, name, expected)) ->
          equal string expected (range_row n name));
    ]

(* Mutation *)

let calc_source =
  source_of
    [
      (13, "  | Sub -> a - b");
      (21, "let sign n = if n > 0 then 1 else 0");
      (40, "let clamp lo hi n = if n < lo then lo else if n > hi then hi else n");
      (41, "let pred n = n - 1");
    ]

let witness ?exe ?file test line =
  {
    Sections.test;
    loc = Option.map (fun file -> { Loc.file; line; column = 0 }) file;
    exe;
  }

let mutant ?(source = calc_source) id line before after =
  { Sections.id; line; before; after; source = Some source }

let add_survivor =
  {
    Sections.mutant = mutant "lib/calc.ml:13:11:add" 13 "a - b" "a + b";
    witnesses =
      [
        witness ~file:"test/test_calc.ml" "subtraction \u{203a} stays positive"
          19;
      ];
  }

let ge_survivor =
  {
    Sections.mutant = mutant "lib/calc.ml:21:16:ge" 21 "n > 0" "n >= 0";
    witnesses =
      [
        witness ~file:"test/test_calc.ml" "sign of a negative" 31;
        witness ~file:"test/test_calc.ml" "sign of a positive" 30;
      ];
  }

let memo_missed =
  {
    Sections.mutant =
      mutant
        ~source:(source_of [ (4, "  | None -> let v = a + b in") ])
        "lib/memo.ml:4:17:sub" 4 "a + b" "a - b";
    invocation = `Exe "./test_memo.exe";
  }

let loop_report =
  {
    Sections.survivors = [ add_survivor; ge_survivor ];
    not_evaluated = [];
    unreached = [ ("lib/calc.ml", 40); ("lib/calc.ml", 41) ];
    outside_tests = [];
    killed = 3;
    not_tested = 0;
    scope = Sections.Suite;
  }

let eq_survivor =
  {
    Sections.mutant =
      mutant
        ~source:
          (source_of
             [ (60, "      if eval env b = Int 0 then raise Division_by_zero") ])
        "lib/eval.ml:60:24:eq" 60 "eval env b = Int 0" "eval env b <> Int 0";
    witnesses =
      [
        witness ~exe:"test_eval.exe" "division \u{203a} divides" 0;
        witness ~exe:"test_eval.exe" "division \u{203a} rounds toward zero" 0;
        witness ~exe:"test_printer.exe" "round trip \u{203a} arithmetic" 0;
      ];
  }

let not_survivor =
  {
    Sections.mutant =
      mutant
        ~source:
          (source_of
             [ (102, "    if at_end p then Error (Unexpected_eof p.pos)") ])
        "lib/parser.ml:102:9:not" 102 "at_end p" "not (at_end p)";
    witnesses =
      [
        witness ~exe:"test_parser.exe" "errors \u{203a} unexpected end of input"
          0;
      ];
  }

let merge_report =
  {
    Sections.survivors = [ eq_survivor; not_survivor ];
    not_evaluated = [];
    unreached =
      List.map (fun line -> ("lib/text.ml", line)) [ 12; 13; 14; 32 ]
      @ [ ("lib/run.ml", 40); ("lib/run.ml", 40); ("lib/report.ml", 61) ];
    outside_tests = [];
    killed = 16;
    not_tested = 0;
    scope = Sections.Executables 3;
  }

let merge_invocation =
  `Exe "dune exec --instrument-with ppx_windtrap.mutate test/test_eval.exe --"

let at_rest ?(ansi = false) ?(invocation = `Mirrors) m =
  printed ~ansi (Sections.mutation_report ~invocation m)

let closing ?(ansi = false) ?(invocation = exe) ?(config = Fun.id) m =
  printed ~ansi
    (Sections.mutation_closing
       ~config:(config { (Run.default_config ()) with Run.invocation })
       m)

let with_source source (s : Sections.survivor) =
  let m : Sections.mutant = s.mutant in
  { s with Sections.mutant = { m with source } }

let reached_report =
  { loop_report with Sections.survivors = []; not_evaluated = [ memo_missed ] }

let mutation_entries () =
  [
    ( "a survivor's block: its rewrite, its source line, the tests that ran it",
      printed ~ansi:true (Sections.survivor_block ~exe_width:None ge_survivor)
    );
    ( "a survivor on line 1",
      printed
        (Sections.survivor_block ~exe_width:None
           {
             Sections.mutant =
               mutant ~source:"let x = 1\n" "a.ml:1:8:add" 1 "1" "2";
             witnesses = [ witness "t" 0 ];
           }) );
    ( "an executable column at least exe_width wide, empty for a test that \
       names none",
      printed
        (Sections.survivor_block ~exe_width:(Some 12)
           {
             add_survivor with
             Sections.witnesses =
               [
                 witness ~exe:"a.exe" "with an executable" 0;
                 witness "without one" 0;
               ];
           }) );
    ( "a source line's control bytes print as text",
      printed ~ansi:true
        (Sections.survivor_block ~exe_width:None
           (with_source
              (Some (source_of [ (13, "\t  | Sub -> \027[31ma\r - b") ]))
              add_survivor)) );
    ( "an unreadable source drops the source line and nothing else",
      printed
        (Sections.survivor_block ~exe_width:None
           (with_source None not_survivor)) );
    ( "a line past the end of the source prints no source line",
      printed
        (Sections.survivor_block ~exe_width:None
           (with_source (Some "one line\n") not_survivor)) );
    ( "a merge's report: survivors counted, the executables one column",
      at_rest ~ansi:true ~invocation:merge_invocation merge_report );
    ("and without colour", at_rest ~invocation:merge_invocation merge_report);
    ( "never reached alone: its rule, its rows, the outcome",
      at_rest { merge_report with Sections.survivors = [] } );
    ("survivors alone", at_rest { merge_report with Sections.unreached = [] });
    ( "neither: the outcome alone",
      at_rest { merge_report with Sections.survivors = []; unreached = [] } );
    ( "never reached: one row per file, the count right-aligned",
      at_rest ~ansi:true
        {
          Sections.survivors = [];
          not_evaluated = [];
          unreached =
            List.init 60 (fun i -> ("lib/report.ml", (i * 3) + 10))
            @ List.init 60 (fun i -> ("lib/report.ml", (i * 3) + 10))
            @ [
                ("lib/a.ml", 7);
                ("lib/a.ml", 8);
                ("lib/a.ml", 8);
                ("lib/b.ml", 1);
              ];
          outside_tests = [];
          killed = 1;
          not_tested = 0;
          scope = Sections.Executables 1;
        } );
    ( "not evaluated: each mutant with the command that arms it",
      at_rest ~ansi:true reached_report );
    ( "evaluated outside tests follows never reached",
      at_rest
        {
          loop_report with
          Sections.survivors = [];
          outside_tests =
            [ ("lib/table.ml", 7); ("lib/calc.ml", 1); ("lib/calc.ml", 2) ];
        } );
    ( "a loop's closing: the rule, the sections, the command, the outcome",
      closing ~ansi:true
        ~invocation:
          (`Exe
             "dune exec --instrument-with ppx_windtrap.mutate \
              test/test_calc.exe --")
        loop_report );
    ( "a loop's not-evaluated section keeps the run's selection",
      closing
        ~config:(fun c -> { c with Run.filter = [ "memoized" ] })
        reached_report );
    ( "outside tests alone, in a loop: a blank line opens it",
      closing
        {
          loop_report with
          Sections.survivors = [];
          unreached = [];
          outside_tests = [ ("lib/table.ml", 7) ];
        } );
    ( "an interrupted loop counts what it did not test",
      closing
        ~invocation:
          (`Exe
             "dune exec --instrument-with ppx_windtrap.mutate \
              test/test_calc.exe --")
        {
          loop_report with
          Sections.survivors = [ add_survivor ];
          killed = 0;
          not_tested = 2;
        } );
  ]

let summary ?(survivors = []) ?(not_evaluated = []) ?(unreached = [])
    ?(outside_tests = []) ?(not_tested = 0) ~killed scope =
  last_line
    (at_rest
       {
         Sections.survivors;
         not_evaluated;
         unreached;
         outside_tests;
         killed;
         not_tested;
         scope;
       })

let two_lines = [ ("lib/calc.ml", 22); ("lib/calc.ml", 31) ]

let summary_rows =
  let s = [ add_survivor ] in
  [
    ( "a suite, clean",
      (fun () -> summary ~killed:5 Sections.Suite),
      "mutants: 5 reached by this suite, 5 killed" );
    ( "a selection, clean",
      (fun () -> summary ~killed:2 (Sections.Selected 2)),
      "mutants: 2 reached by the 2 selected tests, 2 killed" );
    ( "one selected test",
      (fun () -> summary ~killed:1 (Sections.Selected 1)),
      "mutants: 1 reached by the 1 selected test, 1 killed" );
    ( "executables, clean",
      (fun () -> summary ~killed:14 (Sections.Executables 3)),
      "mutants: 14 reached, 14 killed, 3 executables" );
    ( "one executable",
      (fun () -> summary ~killed:3 (Sections.Executables 1)),
      "mutants: 3 reached, 3 killed, 1 executable" );
    ( "nothing reached, nothing killed",
      (fun () -> summary ~killed:0 Sections.Suite),
      "mutants: 0 reached by this suite" );
    ( "nothing reached in a merge is all never reached",
      (fun () ->
        summary ~unreached:two_lines ~killed:0 (Sections.Executables 1)),
      "mutants: 0 reached, 2 never reached, 1 executable" );
    ( "a suite with a survivor",
      (fun () -> summary ~survivors:s ~killed:4 Sections.Suite),
      "mutants: 1 survived of 5 reached by this suite, 4 killed" );
    ( "a selection with a survivor",
      (fun () -> summary ~survivors:s ~killed:1 (Sections.Selected 3)),
      "mutants: 1 survived of 2 reached by the 3 selected tests, 1 killed" );
    ( "executables, survivors and never reached",
      (fun () ->
        summary ~survivors:s ~unreached:two_lines ~killed:11
          (Sections.Executables 3)),
      "mutants: 1 survived of 12 reached, 11 killed, 2 never reached, 3 \
       executables" );
    ( "no killed term when none was killed",
      (fun () -> summary ~survivors:s ~killed:0 Sections.Suite),
      "mutants: 1 survived of 1 reached by this suite" );
    ( "what an interrupted loop did not test is reached, and last",
      (fun () ->
        summary ~survivors:s ~unreached:two_lines ~not_tested:3 ~killed:2
          Sections.Suite),
      "mutants: 1 survived of 6 reached by this suite, 2 killed, 2 never \
       reached, 3 not tested" );
    ( "a site evaluated outside tests is not reached",
      (fun () ->
        summary ~survivors:s ~unreached:two_lines
          ~outside_tests:[ ("lib/calc.ml", 3) ]
          ~killed:2 Sections.Suite),
      "mutants: 1 survived of 3 reached by this suite, 2 killed, 2 never \
       reached, 1 evaluated outside tests" );
    ( "a mutant its child did not evaluate is reached",
      (fun () ->
        summary ~survivors:s ~not_evaluated:[ memo_missed ] ~killed:2
          Sections.Suite),
      "mutants: 1 survived of 4 reached by this suite, 2 killed, 1 not \
       evaluated" );
  ]

let block witnesses =
  printed
    (Sections.survivor_block ~exe_width:None
       { add_survivor with Sections.witnesses })

let calc_one = witness ~file:"test/test_calc.ml" "calc \u{203a} sub to zero" 19
let eval_one = witness ~file:"test/test_eval.ml" "eval \u{203a} Sub node" 31
let named = { calc_one with Sections.exe = Some "test_calc.exe" }

let sentence_rows =
  [
    ("one test", ([ calc_one ], "    1 test ran this line and did not fail:"));
    ( "two tests",
      ([ calc_one; eval_one ], "    2 tests ran this line and none failed:") );
    ( "one executable is tests alone",
      ( [ named; { eval_one with Sections.exe = Some "test_calc.exe" } ],
        "    2 tests ran this line and none failed:" ) );
    ( "several executables are counted",
      ( [ named; { eval_one with Sections.exe = Some "test_eval.exe" } ],
        "    2 tests in 2 executables ran this line and none failed:" ) );
  ]

let sentence (_, (witnesses, expected)) =
  equal string expected (List.nth (lines (block witnesses)) 3)

let padded_names () =
  equal (list string)
    [
      "      calc \u{203a} sub to zero  test/test_calc.ml:19";
      "      eval \u{203a} Sub node     test/test_eval.ml:31";
    ]
    (List.filteri
       (fun i _ -> i >= 4 && i < 6)
       (lines (block [ calc_one; eval_one ])))

let exe_column () =
  let at_rest witnesses =
    at_rest
      {
        loop_report with
        Sections.survivors = [ { add_survivor with Sections.witnesses } ];
      }
  in
  not_contains ~sub:"test_calc.exe" (at_rest [ calc_one; eval_one ]);
  contains
    ~sub:
      "\n\
      \      test_calc.exe  calc \u{203a} sub to zero  test/test_calc.ml:19\n\
      \                     eval \u{203a} Sub node     test/test_eval.ml:31\n"
    (at_rest [ named; eval_one ])

let reproduce ?invocation ?config m =
  List.filter
    (String.starts_with ~prefix:"reproduce: ")
    (lines (closing ?invocation ?config m))

let selection_config c =
  {
    c with
    Run.filter = [ "stays positive" ];
    exclude = [ "slow"; "flaky io" ];
    tags = [ "unit"; "fast" ];
    exclude_tags = [ "flaky" ];
    shard = Some (2, 4);
    failed_only = true;
  }

let reproduce_rows =
  let calc =
    `Exe "dune exec --instrument-with ppx_windtrap.mutate test/test_calc.exe --"
  in
  let quoted =
    {
      loop_report with
      Sections.survivors =
        [
          {
            add_survivor with
            Sections.mutant =
              {
                add_survivor.Sections.mutant with
                Sections.id = "lib/my calc.ml:13:11:add";
              };
          };
        ];
    }
  in
  [
    ( "under dune: dune exec, the backend before the target",
      ( calc,
        Fun.id,
        loop_report,
        [
          "reproduce: dune exec --instrument-with ppx_windtrap.mutate \
           test/test_calc.exe -- --arm lib/calc.ml:13:11:add";
        ] ) );
    ( "by hand: the bare executable",
      ( `Exe "./test_calc.exe",
        Fun.id,
        loop_report,
        [ "reproduce: ./test_calc.exe --arm lib/calc.ml:13:11:add" ] ) );
    ( "under a build action: the mirror, and a run dune does not replay",
      ( `Mirrors,
        Fun.id,
        loop_report,
        [
          "reproduce: WINDTRAP_MUTATE_ARM=lib/calc.ml:13:11:add dune runtest \
           --force --instrument-with ppx_windtrap.mutate";
        ] ) );
    ( "it arms the first survivor printed",
      ( exe,
        Fun.id,
        { loop_report with Sections.survivors = [ ge_survivor; add_survivor ] },
        [ "reproduce: ./t.exe --arm lib/calc.ml:21:16:ge" ] ) );
    ( "an identifier a shell would split is quoted",
      ( exe,
        Fun.id,
        quoted,
        [ "reproduce: ./t.exe --arm 'lib/my calc.ml:13:11:add'" ] ) );
    ( "a narrowed run restates each selection flag",
      ( exe,
        selection_config,
        loop_report,
        [
          "reproduce: ./t.exe --arm lib/calc.ml:13:11:add -f 'stays positive' \
           -e 'slow' -e 'flaky io' --tag unit --tag fast --exclude-tag flaky \
           --shard 2/4 --failed";
        ] ) );
    ( "under a build action, their mirrors, and none for --failed or two \
       patterns",
      ( `Mirrors,
        selection_config,
        loop_report,
        [
          "reproduce: WINDTRAP_MUTATE_ARM=lib/calc.ml:13:11:add \
           WINDTRAP_FILTER='stays positive' WINDTRAP_TAG=unit,fast \
           WINDTRAP_EXCLUDE_TAG=flaky WINDTRAP_SHARD=2/4 dune runtest --force \
           --instrument-with ppx_windtrap.mutate";
        ] ) );
    ( "no survivor, nothing to arm",
      (exe, Fun.id, { loop_report with Sections.survivors = [] }, []) );
    ("no survivor among not evaluated ones", (exe, Fun.id, reached_report, []));
  ]

let reproduce_row (_, (invocation, config, m, expected)) =
  equal (list string) expected (reproduce ~invocation ~config m)

let arm_mirror () =
  equal (list string)
    [
      "    arm: WINDTRAP_MUTATE_ARM=lib/memo.ml:4:17:sub dune runtest --force \
       --instrument-with ppx_windtrap.mutate";
    ]
    (List.filter
       (String.starts_with ~prefix:"    arm: ")
       (lines
          (at_rest
             {
               reached_report with
               Sections.not_evaluated =
                 [ { memo_missed with Sections.invocation = `Mirrors } ];
             })))

let mutation_group =
  group "Mutation"
    [
      test "survivor blocks and reports print as their gallery" (fun () ->
          gallery "mutation" (mutation_entries ()));
      cases "the reproduce command arms the first survivor" ~name:fst
        reproduce_rows reproduce_row;
      test "a not-evaluated mutant's arm command under a build action"
        arm_mirror;
      cases "the outcome line"
        ~name:(fun (name, _, _) -> name)
        summary_rows
        (fun (_, summary, expected) -> equal string expected (summary ()));
      cases "the sentence counts the reaching tests and their executables"
        ~name:fst sentence_rows sentence;
      test "the reaching tests' names are padded to the widest" padded_names;
      test "the executable column appears when a reaching test names one"
        exe_column;
    ]

let () =
  exit
    (run "report_sections"
       [
         failure_projections; names; vocabulary; coverage_group; mutation_group;
       ])
