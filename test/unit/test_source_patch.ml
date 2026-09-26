(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Loc = Windtrap.Private.Loc
module Source_patch = Windtrap.Private.Source_patch
module Text = Windtrap.Private.Text

let strf = Printf.sprintf

(* Positions *)

(* The position that [__POS_OF__] records for the first [needle] of [source]:
   its line and its column. The end column is not read. *)
let at needle source =
  let offset = require_some (Text.first_occurrence ~pattern:needle source) in
  let before = String.sub source 0 offset in
  let line = List.length (String.split_on_char '\n' before) in
  let bol =
    match String.rindex_opt before '\n' with Some i -> i + 1 | None -> 0
  in
  ("test/t.ml", line, offset - bol, 0)

let line n _source = ("test/t.ml", n, 0, 0)

(* The site of the expect test whose head is the first [head] of [source] and
   whose body ends at the end of the first [body]: the head's line and column,
   and the end of the body counted from the start of the head's line. *)
let test_at ~head ~body source =
  let file, line, column, _ = at head source in
  let head = require_some (Text.first_occurrence ~pattern:head source) in
  let body_start = require_some (Text.first_occurrence ~pattern:body source) in
  (file, line, column, body_start + String.length body - (head - column))

let shifted ?(column = 0) ?(stop = 0) site source =
  let file, line, c, s = site source in
  (file, line, c + column, s + stop)

(* Patches and their results *)

let flexible ~literal content site =
  Source_patch.patch ~site ~literal ~style:Flexible content

let exact ~literal content site =
  Source_patch.patch ~site ~literal ~style:Exact content

let trailing content site = Source_patch.trailing ~site content
let pos_text (file, line, first, last) = strf "%s:%d:%d-%d" file line first last

let refusal =
  let pp ppf = function
    | Source_patch.No_literal p ->
        Format.fprintf ppf "No_literal %s" (pos_text p)
    | Drifted p -> Format.fprintf ppf "Drifted %s" (pos_text p)
  in
  let equal a b =
    match (a, b) with
    | Source_patch.No_literal p, Source_patch.No_literal q
    | Drifted p, Drifted q ->
        p = q
    | (No_literal _ | Drifted _), _ -> false
  in
  Testable.make ~pp ~equal

let applied = result string refusal
let rewritten text _site = Ok text
let no_literal site = Error (Source_patch.No_literal site)
let drifted site = Error (Source_patch.Drifted site)

type row = {
  name : string;
  source : string;
  site : string -> Loc.pos;
  patch : Loc.pos -> Source_patch.patch;
  result : Loc.pos -> (string, Source_patch.error) result;
}

let row name source ~site patch result = { name; source; site; patch; result }

let applies claim rows =
  cases claim rows
    ~name:(fun r -> r.name)
    (fun r ->
      let site = r.site r.source in
      equal applied (r.result site)
        (Source_patch.apply r.source [ r.patch site ]))

(* [spelled] after [__POS_OF__ ], with nothing after it. *)
let after_pos spelled = "let () = expect x @@ __POS_OF__ " ^ spelled

(* The literal [spelled] decodes to [value]: an exact patch of that value is
   accepted and writes [z] in the literal's delimiter. *)
let decodes name spelled value =
  let z = if spelled.[0] = '{' then "{|z|}" else {|"z"|} in
  row name (after_pos spelled) ~site:(at "__POS_OF__")
    (exact ~literal:value "z")
    (rewritten (after_pos z))

let drifts name spelled value =
  row name (after_pos spelled) ~site:(at "__POS_OF__")
    (flexible ~literal:value "new")
    drifted

(* Flexible text *)

let normalized claim rows =
  cases claim rows
    ~name:(fun (s, _) -> strf "%S" s)
    (fun (s, form) -> equal string form (Source_patch.normalize s))

let flexible_text =
  group "Flexible text"
    [
      normalized
        "normalize strips every line of its trailing space, TAB, LF, VT, FF \
         and CR"
        [
          ("a\nb", "a\nb");
          ("a  \nb\t", "a\nb");
          ("a\r\nb\r\n", "a\nb");
          ("a \x0c\nb\x0b", "a\nb");
        ];
      normalized "normalize drops the blank lines at both ends, and no other"
        [
          ("\n\n  x\n\n", "x");
          ("  a\n\n  b", "a\n\nb");
          ("", "");
          (" \n\t\n", "");
          ("\x0b\n a \x0c\n\x0c", "a");
        ];
      normalized
        "normalize dedents by the least count of leading spaces, and a line \
         loses all its leading whitespace"
        [
          ("    a\n      b", "a\n  b");
          ("\ta\n\t\tb", "a\nb");
          (" \ta\n  b", "a\n b");
        ];
    ]

(* Sites *)

let sites =
  group "Sites"
    [
      applies
        "a site is __POS_OF__ before its literal, in parentheses or not, the \
         literal itself, or an expect node"
        [
          row "__POS_OF__ then the literal"
            "let () = expect (f ()) @@ __POS_OF__ \"old\"\n"
            ~site:(at "__POS_OF__")
            (flexible ~literal:"old" "new")
            (rewritten "let () = expect (f ()) @@ __POS_OF__ \"new\"\n");
          row "__POS_OF__ in parentheses"
            "let () = expect (f ()) (__POS_OF__ \"old\")\n"
            ~site:(at "(__POS_OF__")
            (flexible ~literal:"old" "new")
            (rewritten "let () = expect (f ()) (__POS_OF__ \"new\")\n");
          row "the literal on the line after __POS_OF__"
            "let () =\n  expect (f ())\n    (__POS_OF__\n       {|old|})\n"
            ~site:(at "(__POS_OF__")
            (flexible ~literal:"old" "new")
            (rewritten
               "let () =\n  expect (f ())\n    (__POS_OF__\n       {| new |})\n");
          row "the literal itself" "  [%expect {|old|}]\n" ~site:(at "{|old|}")
            (flexible ~literal:"old" "new")
            (rewritten "  [%expect {| new |}]\n");
          row "a [%expect …] node" "  [%expect {|old|}]\n" ~site:(at "[%")
            (flexible ~literal:"old" "new")
            (rewritten "  [%expect {| new |}]\n");
          row "a {%expect|…|} node" "  {%expect|old|}\n" ~site:(at "{%")
            (flexible ~literal:"old" "new")
            (rewritten "  {%expect| new |}\n");
          row "a {%expect tag|…|tag} node" "  {%expect t|old|t}\n"
            ~site:(at "{%")
            (flexible ~literal:"old" "new")
            (rewritten "  {%expect t| new |t}\n");
          row "a node without payload, compiled to \"\"" "  [%expect]\n"
            ~site:(at "[%")
            (flexible ~literal:"" "new")
            (rewritten "  [%expect {| new |}]\n");
          row "a literal at the first byte of the source" "{|old|}\n"
            ~site:(line 1)
            (flexible ~literal:"old" "new")
            (rewritten "{| new |}\n");
        ];
    ]

(* Refusals *)

let refusals =
  group "Refusals"
    [
      applies
        "a patch is No_literal when anything but whitespace, (, __POS_OF__ and \
         a node's head stands before its literal"
        [
          row "a comment"
            (after_pos "(* c *) \"old\"\n")
            ~site:(at "__POS_OF__")
            (flexible ~literal:"old" "new")
            no_literal;
          row "an identifier" (after_pos "\"old\"\n") ~site:(at "expect")
            (flexible ~literal:"old" "new")
            no_literal;
          row "a line past the end of the source" "let x = 1\n" ~site:(line 9)
            (flexible ~literal:"" "") no_literal;
          row "a literal never closed" (after_pos "{|old\n")
            ~site:(at "__POS_OF__")
            (flexible ~literal:"old" "new")
            no_literal;
          row "a node cut by the end of the source" "  [%expect" ~site:(at "[%")
            (flexible ~literal:"" "new")
            no_literal;
        ];
      applies "a patch is No_literal when the source ends before its literal"
        (List.map
           (fun spelled ->
             row (strf "%S" spelled) (after_pos spelled) ~site:(at "__POS_OF__")
               (flexible ~literal:"old" "new")
               no_literal)
           [
             "";
             "  ";
             "{";
             "{%";
             "{|old";
             "\"old";
             "\"old\\";
             "\"\\06";
             "\"\\u{41";
           ]);
      applies
        "a patch is Drifted when its literal is not what the source decodes to"
        [
          drifts "an edited literal" "{|edited|}" "old";
          row "a node without payload and a literal other than \"\""
            "  [%expect]\n" ~site:(at "[%")
            (flexible ~literal:"stale" "new")
            drifted;
        ];
      cases "error_message is one sentence that names neither file nor line"
        ~name:fst
        [
          ( "No_literal",
            ( Source_patch.No_literal ("test/t.ml", 4, 2, 0),
              "no string literal at the recorded position" ) );
          ( "Drifted",
            ( Drifted ("test/t.ml", 4, 2, 0),
              "the literal differs from the value the binary was compiled \
               with; rebuild and rerun" ) );
        ]
        (fun (_, (error, sentence)) ->
          equal string sentence (Source_patch.error_message error));
    ]

(* Decoding *)

let decoding =
  group "Decoding"
    [
      applies "a literal decodes as the compiler reads it"
        [
          decodes "\\n, \\b, \\r, \\' and \\space" {|"\n\b\r\'\ "|} "\n\b\r' ";
          decodes "\\t, \\\" and \\\\" {|"a\tb\"c\\d"|} "a\tb\"c\\d";
          decodes "decimal, octal and hexadecimal escapes"
            {|"\065\o101\x4a\x4A"|} "AAJJ";
          decodes "Unicode escapes" {|"\u{e9}\u{1F600}"|} "\u{e9}\u{1F600}";
          decodes "escapes the lexer rejects, kept as written"
            {|"\q\o108\256\999\u{}\u{D800}\u{41"|}
            {|\q\o108\256\999\u{}\u{D800}\u{41|};
          decodes "a continuation drops the LF and the next line's blanks"
            "\"a\\\n \t b\"" "ab";
          decodes "a continuation drops a CR LF" "\"a\\\r\n   b\"" "ab";
          decodes "a backslash before a lone CR is no continuation" "\"a\\\rb\""
            "a\\\rb";
          decodes "an escaped CR and LF are the two bytes" {|"a\r\nb"|} "a\r\nb";
          decodes "a CR LF of the source in a quoted literal is LF" "\"a\r\nb\""
            "a\nb";
          decodes "a lone CR of the source in a quoted literal is kept"
            "\"a\rb\"" "a\rb";
          decodes "a CR LF of the source in a tagged literal is LF" "{|a\r\n|}"
            "a\n";
          decodes "tagged contents may open with }" "{|}old|}" "}old";
        ];
      applies "a literal whose CR before an LF the compiler may keep is Drifted"
        [
          drifts "a quoted CR LF, which OCaml 5.0 and 5.1 keep" "\"a\r\nb\""
            "a\r\nb";
          drifts "a tagged CR LF, which OCaml 5.0 and 5.1 keep" "{|a\r\nb|}"
            "a\r\nb";
          drifts "a quoted CR CR LF, one CR of which no compiler drops"
            "\"a\r\r\nb\"" "a\r\nb";
          drifts "a tagged CR CR LF, one CR of which no compiler drops"
            "{|a\r\r\nb|}" "a\r\nb";
        ];
    ]

(* Rewriting *)

(* A literal on a line indented by two columns, so [c] is 2. *)
let quoted = "  expect x @@ __POS_OF__ \"old\"\n"
let tagged = "  expect x @@ __POS_OF__ {|old|}\n"

let laid_out name source content literal =
  row name source ~site:(at "__POS_OF__")
    (flexible ~literal:"old" content)
    (rewritten ("  expect x @@ __POS_OF__ " ^ literal ^ "\n"))

let multi_line =
  "let () =\n  expect (f ()) @@ __POS_OF__ {|\n    old\n  |};\n  ()\n"

(* The literal that [written] holds after the prefix [before], up to the line
   feed that ends [written], as the compiler reads it: a tagged literal is its
   raw contents, and a quoted one is [String.escaped]'s. *)
let written_value ~before written =
  let n = String.length before in
  let literal = String.sub written n (String.length written - n - 1) in
  let length = String.length literal in
  if literal.[0] = '"' then Scanf.unescaped (String.sub literal 1 (length - 2))
  else
    let bar = String.index literal '|' in
    String.sub literal (bar + 1) (length - (2 * (bar + 1)))

let reads_back_flexibly (lines, spelled, column) =
  let content = String.concat "\n" lines in
  let before = String.make column ' ' ^ "let () = f @@ __POS_OF__ " in
  let source = before ^ spelled ^ "\n" in
  let site = ("test/t.ml", 1, column + String.length "let () = f @@ ", 0) in
  let written =
    require_ok
      (Source_patch.apply source [ flexible ~literal:"old" content site ])
  in
  equal string
    (Source_patch.normalize content)
    (Source_patch.normalize (written_value ~before written))

let reads_back_exactly (content, spelled) =
  let source = "let () = f @@ " ^ spelled ^ "\n" in
  let site = ("test/t.ml", 1, String.length "let () = f @@ ", 0) in
  let corrected =
    require_ok (Source_patch.apply source [ exact ~literal:"old" content site ])
  in
  is_ok ~pp:(Testable.pp refusal)
    (Source_patch.apply corrected [ exact ~literal:content "z" site ])

let rewriting =
  group "Rewriting"
    [
      applies "a flexible patch lays the lines of normalize content out"
        [
          laid_out "no line in a quoted literal is empty" quoted "\n" {|""|};
          laid_out "no line in a tagged literal is one space" tagged "\n"
            "{| |}";
          laid_out "one line in a quoted literal is bare" quoted "hi\n" {|"hi"|};
          laid_out "one line in a tagged literal stands between two spaces"
            tagged "hi\n" "{| hi |}";
          laid_out
            "several lines in a quoted literal are one space in, between a \
             space and a newline"
            quoted "a\n  b" {|" \n a\n   b\n "|};
          laid_out
            "several lines in a tagged literal are c + 2 spaces in, and the \
             delimiter closes on c + 2 spaces"
            tagged "a\n  b\n" "{|\n    a\n      b\n    |}";
          laid_out "a blank line in a quoted literal is empty" quoted "a\n\nb"
            {|" \n a\n\n b\n "|};
          laid_out "a blank line in a tagged literal is empty" tagged "a\n\nb"
            "{|\n    a\n\n    b\n    |}";
          row "c is the indentation of the line of the site"
            "    expect x @@ __POS_OF__ {|old|}\n" ~site:(at "__POS_OF__")
            (flexible ~literal:"old" "a\n  b\n")
            (rewritten
               "    expect x @@ __POS_OF__ {|\n      a\n        b\n      |}\n");
          row "a literal over several lines is replaced whole" multi_line
            ~site:(at "__POS_OF__")
            (flexible ~literal:"\n    old\n  " "one\n  two")
            (rewritten
               "let () =\n\
               \  expect (f ()) @@ __POS_OF__ {|\n\
               \    one\n\
               \      two\n\
               \    |};\n\
               \  ()\n");
          laid_out "a quoted literal escapes a non-ASCII byte in decimal" quoted
            "café" {|"caf\195\169"|};
        ];
      applies "a tag grows by xxx until the contents hold neither delimiter"
        [
          row "a tag that conflicts with nothing is kept"
            (after_pos "{t|old|t}\n") ~site:(at "__POS_OF__")
            (flexible ~literal:"old" "new")
            (rewritten (after_pos "{t| new |t}\n"));
          laid_out "|} in the contents" tagged "a |} b" "{xxx| a |} b |xxx}";
          row "{t| in the contents" (after_pos "{t|old|t}\n")
            ~site:(at "__POS_OF__")
            (exact ~literal:"old" "a {t| b")
            (rewritten (after_pos "{txxx|a {t| b|txxx}\n"));
          row "|t} then {txxx| in the contents" (after_pos "{t|old|t}\n")
            ~site:(at "__POS_OF__")
            (exact ~literal:"old" "|t} {txxx|")
            (rewritten (after_pos "{txxxxxx||t} {txxx||txxxxxx}\n"));
          row "a node's head is followed by a space before a grown tag"
            "  {%expect|old|}\n" ~site:(at "{%")
            (flexible ~literal:"old" "a |} b")
            (rewritten "  {%expect xxx| a |} b |xxx}\n");
        ];
      applies
        "an exact patch writes its contents as given, in a quoted literal when \
         they hold a CR"
        [
          row "in a quoted literal, escaped" (after_pos "\"old\"\n")
            ~site:(at "__POS_OF__")
            (exact ~literal:"old" "a\"b\\c\nd")
            (rewritten (after_pos {|"a\"b\\c\nd"|} ^ "\n"));
          row "in a tagged literal, raw" (after_pos "{|old|}\n")
            ~site:(at "__POS_OF__")
            (exact ~literal:"old" " a\n  b")
            (rewritten (after_pos "{| a\n  b|}\n"));
          row "a CR in a tagged literal" (after_pos "{|old|}\n")
            ~site:(at "__POS_OF__")
            (exact ~literal:"old" "a\r\nb")
            (rewritten (after_pos "\"a\\r\\nb\"\n"));
          row "a CR in a [%ext …] node" "  [%expect_exact {|old|}]\n"
            ~site:(at "[%")
            (exact ~literal:"old" "a\r\nb")
            (rewritten "  [%expect_exact \"a\\r\\nb\"]\n");
          row "a CR in a node without payload" "  [%expect_exact]\n"
            ~site:(at "[%")
            (exact ~literal:"" "a\r\nb")
            (rewritten "  [%expect_exact \"a\\r\\nb\"]\n");
          row "a CR in a {%ext|…|} node, which becomes [%ext …]"
            "  {%expect_exact|old|}\n" ~site:(at "{%")
            (exact ~literal:"old" "a\r\nb")
            (rewritten "  [%expect_exact \"a\\r\\nb\"]\n");
        ];
      prop "the literal of a flexible patch normalizes to normalize content"
        Gen.(
          triple
            (list ~size:(int_range 0 6)
               (string_of ~size:(int_range 0 12)
                  (of_list [ ' '; ' '; 'a'; 'b'; '\t'; '|'; '}' ])))
            (of_list [ "\"old\""; "{|old|}"; "{t|old|t}" ])
            (int_range 0 8))
        reads_back_flexibly;
      prop "the literal of an exact patch decodes to its contents"
        Gen.(
          pair
            (string_of ~size:(int_range 0 12)
               (of_list [ 'a'; ' '; '\r'; '\n'; '|'; '}'; '"'; '\\' ]))
            (of_list
               [
                 "__POS_OF__ {|old|}";
                 "__POS_OF__ \"old\"";
                 "[%expect_exact {|old|}]";
                 "{%expect_exact|old|}";
               ]))
        reads_back_exactly;
    ]

(* Trailing nodes *)

let expect_test = "let%expect_test _ =\n  f ()\n"
let expect_test_site = test_at ~head:"let%" ~body:"f ()"

let trailing_nodes =
  group "Trailing nodes"
    [
      applies
        "a trailing node follows the body after ; and a newline, two columns \
         right of the test's head"
        [
          row "one line"
            "let%expect_test _ =\n  print_string \"x\"\n\nlet () = ()\n"
            ~site:(test_at ~head:"let%" ~body:"print_string \"x\"")
            (trailing "x\n")
            (rewritten
               "let%expect_test _ =\n\
               \  print_string \"x\";\n\
               \  [%expect {| x |}]\n\n\
                let () = ()\n");
          row "several lines, under a nested head"
            "module M = struct\n  let%expect_test _ =\n    f ()\nend\n"
            ~site:(test_at ~head:"let%" ~body:"f ()")
            (trailing "a\n  b\n")
            (rewritten
               "module M = struct\n\
               \  let%expect_test _ =\n\
               \    f ();\n\
               \    [%expect {|\n\
               \      a\n\
               \        b\n\
               \      |}]\n\
                end\n");
          row "the [%%expect_test …] form" "[%%expect_test let _ = f ()]\n"
            ~site:(test_at ~head:"[%%" ~body:"f ()")
            (trailing "y")
            (rewritten "[%%expect_test let _ = f ();\n  [%expect {| y |}]]\n");
          row "a body that ends the source" "let%expect_test _ = f ()"
            ~site:(test_at ~head:"let%" ~body:"f ()")
            (trailing "z")
            (rewritten "let%expect_test _ = f ();\n  [%expect {| z |}]");
        ];
      applies
        "a trailing node is refused unless the test's head is at its site and \
         the body ends on a token there"
        [
          row "no head at the column" expect_test
            ~site:(shifted ~column:1 expect_test_site)
            (trailing "x") drifted;
          row "a body that ends on a blank" expect_test
            ~site:(shifted ~stop:1 expect_test_site)
            (trailing "x") drifted;
          row "a body that ends past the source" expect_test
            ~site:(shifted ~stop:100 expect_test_site)
            (trailing "x") drifted;
          row "a body that ends at the head" expect_test
            ~site:(fun source ->
              let file, line, column, _ = expect_test_site source in
              (file, line, column, column))
            (trailing "x") drifted;
          row "a line past the end of the source" expect_test ~site:(line 9)
            (trailing "x") no_literal;
        ];
    ]

(* The result *)

let two_literals =
  "let () = expect a @@ __POS_OF__ {|x|};\n  expect b @@ __POS_OF__ \"y\"\n"

let in_position_order () =
  let first = at "__POS_OF__ {|x|}" two_literals in
  let second = at "__POS_OF__ \"y\"" two_literals in
  equal applied
    (Ok
       "let () = expect a @@ __POS_OF__ {| longer text |};\n\
       \  expect b @@ __POS_OF__ \"z\"\n")
    (Source_patch.apply two_literals
       [
         flexible ~literal:"y" "z" second;
         flexible ~literal:"x" "longer text" first;
       ])

let first_refused () =
  let first = at "__POS_OF__ {|x|}" two_literals in
  let second = at "__POS_OF__ \"y\"" two_literals in
  equal applied (drifted second)
    (Source_patch.apply two_literals
       [
         flexible ~literal:"stale" "z" second;
         flexible ~literal:"stale" "w" first;
       ]);
  equal applied (drifted second)
    (Source_patch.apply two_literals
       [ flexible ~literal:"x" "w" first; flexible ~literal:"stale" "z" second ])

let crlf_source () =
  let source =
    "let () =\r\n\
    \  expect (f ()) @@ __POS_OF__ {|\r\n\
    \    old\r\n\
    \  |};\r\n\
    \  ()\r\n"
  in
  equal applied
    (Ok
       "let () =\r\n\
       \  expect (f ()) @@ __POS_OF__ {|\n\
       \    one\n\
       \    two\n\
       \    |};\r\n\
       \  ()\r\n")
    (Source_patch.apply source
       [ flexible ~literal:"\n    old\n  " "one\ntwo" (at "__POS_OF__" source) ])

let with_a_literal () =
  let source = expect_test ^ "let () = expect x @@ __POS_OF__ {|old|}\n" in
  equal applied
    (Ok
       "let%expect_test _ =\n\
       \  f ();\n\
       \  [%expect {| x |}]\n\
        let () = expect x @@ __POS_OF__ {| new |}\n")
    (Source_patch.apply source
       [
         trailing "x" (expect_test_site source);
         flexible ~literal:"old" "new" (at "__POS_OF__" source);
       ])

let the_result =
  group "The result"
    [
      test "patches apply in the order of their positions, whatever the list's"
        in_position_order;
      test "no patch leaves the source as it is" (fun () ->
          equal applied (Ok two_literals) (Source_patch.apply two_literals []));
      test
        "a patch given twice applies once, past the one patch per literal that \
         apply asks for" (fun () ->
          let patch =
            flexible ~literal:"x" "w" (at "__POS_OF__" two_literals)
          in
          equal applied
            (Source_patch.apply two_literals [ patch ])
            (Source_patch.apply two_literals [ patch; patch ]));
      test "a CR LF source keeps its CRs, and a new literal's lines end in LF"
        crlf_source;
      test "a trailing node and a literal of one source apply together"
        with_a_literal;
      test "the error is the first refused patch of the list, and none applies"
        first_refused;
    ]

let () =
  exit
    (run "source_patch"
       [
         flexible_text;
         sites;
         refusals;
         decoding;
         rewriting;
         trailing_nodes;
         the_result;
       ])
