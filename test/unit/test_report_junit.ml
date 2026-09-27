(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Failure = Windtrap.Private.Failure
module Loc = Windtrap.Private.Loc
module Pp = Windtrap.Private.Pp
module Os = Windtrap.Private.Os
module Report_junit = Windtrap.Private.Report_junit
module Report_sections = Windtrap.Private.Report_sections
module Run = Windtrap.Private.Run
module Test_tree = Windtrap.Private.Test_tree

(* The XML reader *)

(* The document is read back as a tree by a reader of the XML 1.0 it uses: an
   optional declaration and one element, attributes quoted and named once,
   text and values made of XML characters and references to them. DOCTYPE,
   CDATA and processing instructions are refused, since the document writes
   none. A value reads back decoded, and a raw TAB, LF or CR in an attribute
   reads as a space, as XML 1.0 normalizes it. *)

type element = {
  tag : string;
  attributes : (string * string) list;
  children : node list;
}

and node = Element of element | Text of string

exception Malformed of string

let malformed fmt = Printf.ksprintf (fun m -> raise (Malformed m)) fmt

let is_xml_char u =
  u = 0x9 || u = 0xA || u = 0xD
  || (0x20 <= u && u <= 0xD7FF)
  || (0xE000 <= u && u <= 0xFFFD)
  || (0x10000 <= u && u <= 0x10FFFF)

let number ~hex digits =
  let digit = function
    | '0' .. '9' -> true
    | 'a' .. 'f' | 'A' .. 'F' -> hex
    | _ -> false
  in
  if digits = "" || not (String.for_all digit digits) then None
  else int_of_string_opt ((if hex then "0x" else "") ^ digits)

let read s =
  let len = String.length s in
  let pos = ref 0 in
  let peek () = if !pos < len then Some s.[!pos] else None in
  let looking_at p =
    String.length p <= len - !pos && String.sub s !pos (String.length p) = p
  in
  let skip p =
    if looking_at p then pos := !pos + String.length p
    else malformed "expected %S at byte %d" p !pos
  in
  let rec skip_space () =
    match peek () with
    | Some (' ' | '\t' | '\n' | '\r') ->
        incr pos;
        skip_space ()
    | Some _ | None -> ()
  in
  let character b =
    let d = String.get_utf_8_uchar s !pos in
    let u = Uchar.to_int (Uchar.utf_decode_uchar d) in
    if not (Uchar.utf_decode_is_valid d) then
      malformed "invalid UTF-8 at byte %d" !pos;
    if not (is_xml_char u) then
      malformed "U+%04X is no XML character at byte %d" u !pos;
    Buffer.add_substring b s !pos (Uchar.utf_decode_length d);
    pos := !pos + Uchar.utf_decode_length d
  in
  let name () =
    let start = !pos in
    let rec loop () =
      match peek () with
      | Some ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | '.' | ':') ->
          incr pos;
          loop ()
      | Some _ | None -> ()
    in
    loop ();
    if !pos = start then malformed "no name at byte %d" start;
    String.sub s start (!pos - start)
  in
  let reference b =
    let start = !pos in
    let semi =
      match String.index_from_opt s start ';' with
      | Some i -> i
      | None -> malformed "unterminated reference at byte %d" start
    in
    let body = String.sub s (start + 1) (semi - start - 1) in
    let code =
      if String.starts_with ~prefix:"#x" body then
        number ~hex:true (String.sub body 2 (String.length body - 2))
      else if String.starts_with ~prefix:"#" body then
        number ~hex:false (String.sub body 1 (String.length body - 1))
      else None
    in
    (match (body, code) with
    | "lt", _ -> Buffer.add_char b '<'
    | "gt", _ -> Buffer.add_char b '>'
    | "amp", _ -> Buffer.add_char b '&'
    | "quot", _ -> Buffer.add_char b '"'
    | "apos", _ -> Buffer.add_char b '\''
    | _, Some u when is_xml_char u -> Buffer.add_utf_8_uchar b (Uchar.of_int u)
    | _ -> malformed "bad reference &%s; at byte %d" body start);
    pos := semi + 1
  in
  let rec value ~quote ~start b =
    match peek () with
    | None -> malformed "unterminated attribute at byte %d" start
    | Some c when c = quote -> incr pos
    | Some '<' -> malformed "raw '<' in an attribute at byte %d" !pos
    | Some '&' ->
        reference b;
        value ~quote ~start b
    | Some ('\t' | '\n' | '\r') ->
        Buffer.add_char b ' ';
        incr pos;
        value ~quote ~start b
    | Some _ ->
        character b;
        value ~quote ~start b
  in
  let rec attributes acc =
    skip_space ();
    match peek () with
    | Some ('/' | '>') | None -> List.rev acc
    | Some _ ->
        let start = !pos in
        let key = name () in
        if List.mem_assoc key acc then
          malformed "attribute %s repeated at byte %d" key start;
        skip "=";
        let quote =
          match peek () with
          | Some (('"' | '\'') as q) ->
              incr pos;
              q
          | Some _ | None -> malformed "unquoted attribute at byte %d" !pos
        in
        let b = Buffer.create 16 in
        value ~quote ~start b;
        attributes ((key, Buffer.contents b) :: acc)
  in
  let rec element () =
    skip "<";
    let tag = name () in
    let attributes = attributes [] in
    if looking_at "/>" then begin
      skip "/>";
      { tag; attributes; children = [] }
    end
    else begin
      skip ">";
      let children = content [] (Buffer.create 64) in
      skip "</";
      let closing = name () in
      if closing <> tag then malformed "</%s> closes <%s>" closing tag;
      skip_space ();
      skip ">";
      { tag; attributes; children }
    end
  and content acc b =
    let flush acc =
      if Buffer.length b = 0 then acc
      else begin
        let t = Buffer.contents b in
        Buffer.clear b;
        Text t :: acc
      end
    in
    if looking_at "</" then List.rev (flush acc)
    else
      match peek () with
      | None -> List.rev (flush acc)
      | Some '<' ->
          let acc = flush acc in
          let e = element () in
          content (Element e :: acc) b
      | Some '&' ->
          reference b;
          content acc b
      | Some _ ->
          character b;
          content acc b
  in
  let rec declaration_end i =
    if i + 1 >= len then malformed "unterminated declaration"
    else if s.[i] = '?' && s.[i + 1] = '>' then i + 2
    else declaration_end (i + 1)
  in
  match
    skip_space ();
    if looking_at "<?xml" then pos := declaration_end !pos;
    skip_space ();
    let root = element () in
    skip_space ();
    if !pos <> len then malformed "trailing content at byte %d" !pos;
    root
  with
  | root -> Ok root
  | exception Malformed m -> Error m

(* Projections of a document *)

let parsed doc = require_ok ~pp:Format.pp_print_string (read doc)

let elements e =
  List.filter_map (function Element e -> Some e | Text _ -> None) e.children

let text_of e =
  String.concat ""
    (List.filter_map
       (function Text t -> Some t | Element _ -> None)
       e.children)

let attribute key e = List.assoc_opt key e.attributes
let one = function [ x ] -> Some x | _ -> None
let testcases_of doc = elements (require_match one (elements (parsed doc)))

let children tag doc =
  List.concat_map
    (fun t -> List.filter (fun c -> c.tag = tag) (elements t))
    (testcases_of doc)

(* An element's tag and its attributes, in the order of the document. *)
let heading e =
  String.concat " " (e.tag :: List.map (fun (k, v) -> k ^ "=" ^ v) e.attributes)

(* A testcase as its name, classname and time, then its children: each by its
   tag, a skipped element with its message. *)
let row t =
  let child c =
    match (c.tag, attribute "message" c) with
    | "skipped", Some m -> "skipped: " ^ m
    | tag, _ -> tag
  in
  let fields =
    List.map
      (fun k -> Option.value ~default:"-" (attribute k t))
      [ "name"; "classname"; "time" ]
  in
  let inner = List.map child (elements t) in
  String.concat " | "
    (fields @ if inner = [] then [] else [ String.concat ", " inner ])

let rows doc = List.map row (testcases_of doc)

let counts_of doc =
  let suite = require_match one (elements (parsed doc)) in
  String.concat " "
    (List.filter_map
       (fun k -> Option.map (fun v -> k ^ "=" ^ v) (attribute k suite))
       [ "tests"; "failures"; "errors"; "skipped" ])

let messages tag doc = List.filter_map (attribute "message") (children tag doc)
let texts tag doc = List.map text_of (children tag doc)

(* Writing and reading back *)

let write ?(invocation = `Mirrors) ?armed ?(suite = "s") ?(duration = 0.1)
    ?(releases = []) results target =
  Report_junit.write ~invocation ?armed ~suite ~duration ~results
    ~release_failures:releases target

let read_file path = In_channel.with_open_bin path In_channel.input_all

let junit ?invocation ?armed ?suite ?duration ?releases results =
  let file = Filename.concat (temp_dir ()) "report.xml" in
  write ?invocation ?armed ?suite ?duration ?releases results file;
  read_file file

let files dir = List.sort String.compare (Array.to_list (Sys.readdir dir))

(* Rows *)

let result = Render_fixtures.result
let fail path failures = result path (Failure.Fail failures)
let label f = require_some (Report_sections.labeled_msg f)

let subtest ?msg name =
  {
    (Failure.equality ?msg ~expected:"1" ~actual:"2" ()) with
    Failure.subtest = [ "contract"; name ];
  }

(* Every kind of failure that the fixture run holds, and the headline forms
   it does not: a message, sides of several lines, a long side, a withheld
   correction and subtests. *)
let failing =
  Render_fixtures.results
  @ [
      fail [ "forms"; "a message" ]
        [ Failure.equality ~msg:"deliberate" ~expected:"1" ~actual:"2" () ];
      fail [ "forms"; "lines" ]
        [ Failure.equality ~expected:"a\nb\nc" ~actual:"a\nB\nc" () ];
      fail [ "forms"; "a long side" ]
        [ Failure.equality ~expected:(String.make 100 'x') ~actual:"y" () ];
      fail [ "withheld" ]
        [
          Failure.message "boom";
          Failure.with_withheld Failure.Failed_outside
            Render_fixtures.snap_mismatch;
        ];
      Render_fixtures.subtest_result;
      fail [ "laws"; "a law" ] [ Render_fixtures.law_failure ];
      fail [ "laws"; "a failed term" ] [ Render_fixtures.law_term_failure ];
    ]

let release_failures = [ Render_fixtures.release_failure ]

(* The failures that a document writes, in its order, each with the path of
   its test: a counted row's own failures, then its subtests', then those of
   the releases. *)
let written rows releases =
  let row (r : Run.result) =
    match r.outcome with
    | Failure.Fail fs when r.counted ->
        let subtests, own =
          List.partition Report_sections.is_subtest_failure fs
        in
        let path = Test_tree.path_to_string r.path in
        List.map (fun f -> (path, f)) (own @ subtests)
    | Failure.Fail _ | Failure.Pass | Failure.Skip _ -> []
  in
  List.concat_map row rows
  @ List.map (fun f -> (Report_sections.release_title, f)) releases

(* The document *)

let small_run () =
  let doc =
    junit ~suite:"mylib" ~duration:1.234 ~releases:release_failures
      [
        result [ "math"; "addition" ] Failure.Pass ~duration:0.0001;
        fail
          [ "users"; "sessions after login" ]
          [
            Failure.with_output_tail Render_fixtures.tail
              Render_fixtures.eq_failure;
          ];
        result [ "platform"; "windows paths" ] (Failure.Skip (Some "unix only"));
        Render_fixtures.timed_result;
      ]
  in
  expect_file doc "test/unit/expected/test_report_junit/document.expected";
  is_ok ~pp:Format.pp_print_string (read doc)

let subtest_run () =
  let doc =
    junit ~suite:"mylib" ~duration:0.7 [ Render_fixtures.subtest_result ]
  in
  expect_file doc "test/unit/expected/test_report_junit/subtests.expected"

let root_and_suite () =
  let doc =
    junit ~suite:"mylib" ~duration:Render_fixtures.duration
      ~releases:release_failures Render_fixtures.results
  in
  let root = parsed doc in
  equal (list string)
    [
      "testsuites name=windtrap tests=13 failures=7 errors=0 skipped=1 \
       time=6.500";
      "testsuite name=mylib tests=13 failures=7 errors=0 skipped=1 time=6.500";
    ]
    (List.map heading (root :: elements root))

let document =
  group "The document"
    [
      test "a small run's document" small_run;
      test "a run with subtests" subtest_run;
      test
        "the root is testsuites named windtrap, holding one testsuite named \
         after the suite, both with the counts and the run's duration"
        root_and_suite;
    ]

(* Testcases *)

let each_row () =
  let doc =
    junit ~suite:"mylib" ~releases:release_failures Render_fixtures.results
  in
  equal (list string)
    [
      "math › addition | mylib.math | 0.000";
      "users › sessions after login | mylib.users | 0.000 | failure, system-out";
      "parser › rejects empty | mylib.parser | 0.000 | failure";
      "cli › cli help | mylib.cli | 0.000 | failure";
      "cli › version drift | mylib.cli | 0.000 | failure";
      "geo › area non-negative | mylib.geo | 0.018 | failure";
      "db › insert | mylib.db | 0.000 | failure, failure";
      "flaky › eventually | mylib.flaky | 0.000 | system-out";
      "slow › big sort | mylib.slow | 2.500";
      "slow › hash | mylib.slow | 3.000";
      "platform › windows paths | mylib.platform | 0.000 | skipped: unix only";
      "math › multiplication | mylib.math | 0.042";
      "fixture release | mylib | 0.000 | failure";
    ]
    (rows doc)

let retried_pass () =
  let doc =
    junit
      [
        result [ "flaky"; "eventually" ] Failure.Pass ~attempts:3;
        result [ "steady" ] Failure.Pass;
      ]
  in
  equal (list string)
    [
      "flaky › eventually | s.flaky | 0.000 | system-out"; "steady | s | 0.000";
    ]
    (rows doc);
  equal (list string) [ "passed on attempt 3" ] (texts "system-out" doc)

let messages_are_headlines () =
  let doc = junit ~releases:release_failures failing in
  equal (list string)
    (List.map
       (fun (_, f) -> Report_sections.headline f)
       (written failing release_failures))
    (messages "failure" doc)

(* The failures of [failing], and one whose location names a readable line,
   which an excerpt would print. *)
let entries (_, invocation, armed) =
  let source = Filename.concat (temp_dir ()) "t.ml" in
  Out_channel.with_open_bin source (fun oc ->
      Out_channel.output_string oc "let the_located_line = ()\n");
  let located =
    fail [ "located" ]
      [ Failure.message ~loc:{ Loc.file = source; line = 1; column = 0 } "m" ]
  in
  let rows = failing @ [ located ] in
  let doc = junit ~invocation ?armed ~releases:release_failures rows in
  let entry (filter, f) =
    Pp.str "%a"
      (fun ppf ->
        Report_sections.pp_failure ~ansi:false ~filter ~invocation ?armed ppf)
      f
  in
  equal (list text)
    (List.map entry (written rows release_failures))
    (texts "failure" doc)

let first_tail () =
  let in_subtest =
    { (Failure.message "in a subtest") with subtest = [ "t"; "u" ] }
  in
  let doc =
    junit
      [
        fail [ "t" ]
          [
            Failure.with_output_tail (Failure.tail "first tail\n") in_subtest;
            Failure.with_output_tail
              (Failure.tail ~log_path:"second.output" ~omitted_bytes:9
                 "second tail\n")
              (Failure.message "own");
          ];
      ]
  in
  equal (list string)
    [
      "t | s | 0.000 | failure, system-out";
      label in_subtest ^ " | s | 0.000 | failure";
    ]
    (rows doc);
  equal (list string) [ "first tail\n" ] (texts "system-out" doc)

let law_projection () =
  let doc =
    junit
      [
        fail [ "laws"; "a law" ] [ Render_fixtures.law_failure ];
        fail [ "laws"; "a failed term" ] [ Render_fixtures.law_term_failure ];
      ]
  in
  equal (list string)
    [
      "associative: op (op a b) c = op a (op b c)"; "round trip: g (f x) failed";
    ]
    (messages "failure" doc);
  match texts "failure" doc with
  | [ law; term ] ->
      contains ~sub:"    op b c         -1\n    op (op a b) c  -4\n" law;
      contains ~sub:"    g (f x) failed at:\n      test/test_version.ml:32\n"
        term
  | texts -> failf "%d failure texts" (List.length texts)

let exe = `Exe "dune exec qa/x/t.exe --"
let armed = "lib/a.ml:1:0:add"

let testcases =
  group "Testcases"
    [
      test
        "each row is a testcase, in order, named by its path, classed by the \
         suite and its groups, timed in seconds with three decimals"
        each_row;
      test "a dot inside a name is not escaped in the classname" (fun () ->
          equal (list string)
            [ "a.b › t.c | s.a.b | 0.000" ]
            (rows (junit [ result [ "a.b"; "t.c" ] Failure.Pass ])));
      test
        "a pass is an empty testcase, and a pass on a retry holds a system-out \
         that reads passed on attempt N"
        retried_pass;
      test "a skip holds a skipped element, whose message is its reason"
        (fun () ->
          equal (list string)
            [ "a | s | 0.000 | skipped: unix only"; "b | s | 0.000 | skipped" ]
            (rows
               (junit
                  [
                    result [ "a" ] (Failure.Skip (Some "unix only"));
                    result [ "b" ] (Failure.Skip None);
                  ])));
      test
        "a counted failure holds one failure element for each of its own \
         failures, whose message is its headline"
        messages_are_headlines;
      cases
        "a failure's text is its entry without excerpt, ending in the hint, \
         accept and replay lines of its test's path"
        ~name:(fun (name, _, _) -> name)
        [
          ("dune's mirrors", `Mirrors, None);
          ("an executable", exe, None);
          ("an armed run", `Mirrors, Some armed);
          ("an armed executable", exe, Some armed);
        ]
        entries;
      test
        "a system-out follows the failures and holds the first captured tail, \
         a subtest's included"
        first_tail;
      test
        "a law's failure is its law and equation, and its text lists its terms"
        law_projection;
    ]

(* Subtests *)

let own_and_subtests () =
  let first = subtest "shape [0]" in
  let second = subtest ~msg:"user context" "shape [1]" in
  let doc =
    junit
      [
        result ~duration:0.5 [ "backend"; "contract" ]
          (Failure.Fail [ first; Failure.message "final check"; second ]);
      ]
  in
  equal (list string)
    [
      "backend › contract | s.backend | 0.500 | failure";
      label first ^ " | s.backend | 0.000 | failure";
      label second ^ " | s.backend | 0.000 | failure";
    ]
    (rows doc)

let only_subtests () =
  let only = subtest "shape [0]" in
  let doc = junit [ fail [ "backend"; "contract" ] [ only ] ] in
  equal (list string)
    [
      "backend › contract | s.backend | 0.000";
      label only ^ " | s.backend | 0.000 | failure";
    ]
    (rows doc)

let subtests =
  group "Subtests"
    [
      test
        "each subtest failure is a testcase after its test's, with its \
         classname, named by its label, holding its failure, at time 0.000"
        own_and_subtests;
      test "a test whose every failure is a subtest's holds no failure"
        only_subtests;
    ]

(* Fixture releases *)

let failed_releases () =
  let second =
    Failure.with_phase Failure.Release (Failure.message "release raised")
  in
  let doc =
    junit
      ~releases:[ Render_fixtures.release_failure; second ]
      [ Render_fixtures.timed_result ]
  in
  let release = Report_sections.release_title ^ " | s | 0.000 | failure" in
  equal (list string)
    [ "math › multiplication | s.math | 0.042"; release; release ]
    (rows doc)

let releases =
  group "Fixture releases"
    [
      test
        "each failed release is a testcase after the rows, named by the \
         release title, classed by the suite, at time 0.000"
        failed_releases;
    ]

(* Expected failures *)

let no_reason =
  { Render_fixtures.excused_result with xfail = Some { reason = None } }

let expected_failures =
  group "Expected failures"
    [
      test
        "an uncounted Fail row holds only a skipped element: expected failure, \
         and its reason" (fun () ->
          equal (list string)
            [
              "known › broken carry | s.known | 0.000 | skipped: expected \
               failure: issue #42";
              "known › broken carry | s.known | 0.000 | skipped: expected \
               failure";
            ]
            (rows (junit [ Render_fixtures.excused_result; no_reason ])));
      test "a counted Fail row is a failure, an xfail annotation or not"
        (fun () ->
          equal (list string)
            [ "known › fixed already | s.known | 0.000 | failure" ]
            (rows (junit [ Render_fixtures.xpass_result ])));
    ]

(* Counts *)

let counts =
  group "Counts"
    [
      cases "tests, failures and skipped count the testcases; errors is 0"
        ~name:(fun (name, _, _, _) -> name)
        [
          ("an empty run", [], [], "tests=0 failures=0 errors=0 skipped=0");
          ( "a failing row counts one failure, however many elements",
            [ fail [ "db"; "insert" ] Render_fixtures.body_teardown ],
            [],
            "tests=1 failures=1 errors=0 skipped=0" );
          ( "a subtest failure counts one test and one failure",
            [ Render_fixtures.subtest_result ],
            [],
            "tests=3 failures=3 errors=0 skipped=0" );
          ( "a row whose failures are all subtests' counts no failure",
            [ fail [ "backend"; "contract" ] [ subtest "shape [0]" ] ],
            [],
            "tests=2 failures=1 errors=0 skipped=0" );
          ( "a failed release counts one test and one failure",
            [ Render_fixtures.timed_result ],
            release_failures,
            "tests=2 failures=1 errors=0 skipped=0" );
          ( "a skip and an expected failure count one skip each",
            [
              result [ "ok" ] Failure.Pass;
              Render_fixtures.excused_result;
              fail [ "bad" ] [ Failure.message "boom" ];
              result [ "later" ] (Failure.Skip None);
            ],
            [],
            "tests=4 failures=1 errors=0 skipped=2" );
          ( "a pass on a retry counts no failure",
            [ result [ "flaky" ] Failure.Pass ~attempts:3 ],
            [],
            "tests=1 failures=0 errors=0 skipped=0" );
          ( "an unexpected pass counts one failure",
            [ Render_fixtures.xpass_result ],
            [],
            "tests=1 failures=1 errors=0 skipped=0" );
        ]
        (fun (_, rows, releases, expected) ->
          equal string expected (counts_of (junit ~releases rows)));
    ]

(* Validity *)

(* [input] as the reason of a skip, an attribute, and as a captured tail,
   element text. *)
let reads_back (_, input, attribute, text) =
  let doc =
    junit
      [
        result [ "skipped" ] (Failure.Skip (Some input));
        fail [ "failed" ]
          [
            Failure.with_output_tail (Failure.tail input) (Failure.message "m");
          ];
      ]
  in
  equal (list string) [ attribute ] (messages "skipped" doc);
  equal (list string) [ text ] (texts "system-out" doc)

let ansi = "\027[31mred\027[0m"
let edges = "\u{D7FF}\u{E000}\u{FFFD}\u{10000}\u{10FFFF}"

let payload =
  let fragment =
    Gen.of_list
      [
        "a";
        "\u{e9}";
        "<";
        ">";
        "&";
        "\"";
        "'";
        "\t";
        "\n";
        "\r";
        "\x00";
        "\x1b";
        "\x7f";
        "\xc3";
        "\xff";
        "\u{D7FF}";
        "\u{FFFE}";
        "\u{FFFF}";
        "\u{10FFFF}";
        "]]>";
      ]
  in
  Gen.with_pp
    (fun ppf s -> Format.fprintf ppf "%S" s)
    (Gen.one_of [ Gen.string; Gen.map (String.concat "") (Gen.list fragment) ])

(* Each payload in every string that the rows and the releases supply. The
   payloads of a case share one document, since each document is a file
   written and read back. *)
let well_formed payloads =
  let rows s =
    [
      fail [ s; s ]
        [
          Failure.with_output_tail
            (Failure.tail ~log_path:s s)
            (Failure.message s);
          { (Failure.message s) with subtest = [ s; s ] };
        ];
      result [ s ] (Failure.Skip (Some s));
      { Render_fixtures.excused_result with xfail = Some { reason = Some s } };
    ]
  in
  let release s = Failure.with_phase Failure.Release (Failure.message s) in
  let doc =
    junit
      ~suite:(String.concat "" payloads)
      ~releases:(List.map release payloads)
      (List.concat_map rows payloads)
  in
  is_ok ~pp:Format.pp_print_string (read doc)

let validity =
  group "Validity"
    [
      cases
        "a string reads back escaped, controls as \\xNN, and reduced to the \
         Char range of XML 1.0"
        ~name:(fun (name, _, _, _) -> name)
        [
          ("the markup characters", {|a<b>&"c'|}, {|a<b>&"c'|}, {|a<b>&"c'|});
          ( "ESC, in an escape sequence",
            ansi,
            {|\x1b[31mred\x1b[0m|},
            {|\x1b[31mred\x1b[0m|} );
          ( "BEL, closing an OSC sequence",
            "\027]0;title\007",
            {|\x1b]0;title\x07|},
            {|\x1b]0;title\x07|} );
          ( "a control byte and a form feed",
            "a\x01b\x0cc",
            {|a\x01b\x0cc|},
            {|a\x01b\x0cc|} );
          ("a malformed byte", "c\xffd", "c\u{FFFD}d", "c\u{FFFD}d");
          ( "the noncharacters U+FFFE and U+FFFF",
            "\u{FFFE}\u{FFFF}",
            "\u{FFFD}\u{FFFD}",
            "\u{FFFD}\u{FFFD}" );
          ("the edges of the Char range", edges, edges, edges);
          ("TAB", "a\tb", "a\tb", "a\tb");
          ("LF, kept in text only", "a\nb", {|a\x0ab|}, "a\nb");
          ("CR", "a\rb", {|a\x0db|}, {|a\x0db|});
        ]
        reads_back;
      prop "no payload makes the document malformed" ~count:25
        (Gen.list payload) well_formed
        ~examples:
          [
            [
              "a\x01b\x0cc\xffd\u{FFFE}e";
              ansi;
              "\027]0;title\007";
              {|a<b>&"c'|};
              edges ^ "\u{FFFE}\u{FFFF}";
            ];
          ];
    ]

(* Determinism *)

let under_the_root () =
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
  let doc = junit [ fail [ "t" ] [ failure ] ] in
  contains ~sub:"full log: _build/_tests/s/t.output"
    (String.concat "" (texts "system-out" doc));
  contains ~sub:"test/help.expected" (String.concat "" (texts "failure" doc));
  not_contains ~sub:root doc

let determinism =
  group "Determinism"
    [
      test "the paths of a document are relative to the project root"
        under_the_root;
    ]

(* Writing *)

let xml_target () =
  let root = temp_dir () in
  let shared = Filename.concat root "all.xml" in
  write ~suite:"first" Render_fixtures.results shared;
  write ~suite:"second" [ Render_fixtures.timed_result ] shared;
  let alone = Filename.concat root "alone.xml" in
  write ~suite:"second" [ Render_fixtures.timed_result ] alone;
  equal (list string) [ "all.xml"; "alone.xml" ] (files root);
  equal text (read_file alone) (read_file shared);
  equal string "" (output ())

let directory_target () =
  let dir = Filename.concat (temp_dir ()) "a/b/c" in
  write ~suite:"mylib" [] dir;
  write ~suite:"parser" [] dir;
  equal (list string) [ "mylib.xml"; "parser.xml" ] (files dir)

let partitions () =
  let dir = temp_dir () in
  write ~suite:"lib/parser.ml" [] dir;
  write ~suite:"lib/lexer.ml" [] dir;
  equal (list string)
    (List.sort String.compare
       [
         Os.sanitize_component "lib/parser.ml" ^ ".xml";
         Os.sanitize_component "lib/lexer.ml" ^ ".xml";
       ])
    (files dir)

(* [root] holds one regular file, and [target root] cannot be written. *)
let unwritable (_, target) =
  let root = temp_dir () in
  Out_channel.with_open_bin (Filename.concat root "file") ignore;
  write [] (target root);
  equal (list string) [ "file" ] (files root);
  starts_with ~affix:"windtrap: warning: "
    (require_match one (String.split_on_char '\n' (String.trim (output ()))))

let writing =
  group "Writing"
    [
      test
        "a target that ends in .xml is that file, as given, and of two suites \
         the last replaces it whole, printing nothing"
        xml_target;
      test
        "any other target is a directory, made with its parents, holding a \
         file per suite"
        directory_target;
      test "a suite's file is its name made one path component" partitions;
      cases "a report that cannot be written is one warning, and write returns"
        ~name:fst
        [
          ( "an .xml target whose directory is missing",
            fun root -> Filename.concat root "missing/r.xml" );
          ( "a directory target below a regular file",
            fun root -> Filename.concat root "file/reports" );
        ]
        unwritable;
    ]

(* The reader *)

let reader =
  group "The reader"
    [
      cases "the reader accepts a well-formed document" ~name:Fun.id
        [
          "<a/>";
          "<a x='1' y=\"2\">t&amp;u<b/></a>";
          "<a x='\u{e9}'>\u{20ac}\u{1d11e}</a>";
        ] (fun doc -> is_ok ~pp:Format.pp_print_string (read doc));
      cases "the reader refuses a malformed document" ~name:fst
        [
          ("mismatched tags", "<a><b></a>");
          ("an unquoted attribute", "<a x=1/>");
          ("an unknown entity", "<a>&nope;</a>");
          ("a raw ampersand", "<a>t & u</a>");
          ("a control byte", "<a>\x01</a>");
          ("a repeated attribute", "<a x='1' x='2'/>");
          ("invalid UTF-8", "<a>\xff</a>");
          ("a noncharacter", "<a>\u{fffe}</a>");
          ("a control byte in an attribute", "<a x='\x01'/>");
          ("a reference to no character", "<a>&#0;</a>");
          ("trailing content", "<a/><b/>");
        ]
        (fun (_, doc) -> is_error (read doc));
      test
        "the reader decodes references, and reads a raw TAB, LF or CR in an \
         attribute as a space" (fun () ->
          let a =
            require_ok (read "<a x='&#9;&#x41;&lt;\t\n\r'>&amp;&gt;</a>")
          in
          equal
            (pair (option string) string)
            (Some "\tA<   ", "&>")
            (attribute "x" a, text_of a));
    ]

let () =
  exit
  @@ run "report_junit"
       [
         document;
         testcases;
         subtests;
         releases;
         expected_failures;
         counts;
         validity;
         determinism;
         writing;
         reader;
       ]
