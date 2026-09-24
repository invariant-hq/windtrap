(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Source_patch: the flexible normalization, literal rendering,
   and the rewriting of literals at __POS_OF__ positions — quoted and
   tagged delimiters, multi-line re-indentation, tag growth on conflict,
   the drift refusal, several patches in one file, and a literal inside
   parentheses. Everything here drives [P.apply] on strings. *)

open Windtrap
open Windtrap.Private
module P = Source_patch

let registered = ref []
let reg name body = registered := Windtrap.test name body :: !registered

(* The [__POS_OF__]-shaped position of [needle]'s first occurrence in
   [source]: its 1-based line and its column; the end column is not read. *)
let pos_of ?(file = "test/t.ml") source needle =
  let offset =
    match Text.first_occurrence ~pattern:needle source with
    | Some offset -> offset
    | None -> failf "pos_of: %S not in the source" needle
  in
  let line = ref 1 and bol = ref 0 in
  String.iteri
    (fun i c ->
      if i < offset && c = '\n' then begin
        incr line;
        bol := i + 1
      end)
    source;
  (file, !line, offset - !bol, 0)

let apply source patches =
  match P.apply source patches with
  | Ok text -> text
  | Error error -> failf "refused: %s" (P.error_message error)

let flexible ~site ~literal content =
  P.patch ~site ~literal ~style:P.Flexible content

let exact ~site ~literal content = P.patch ~site ~literal ~style:P.Exact content

(* Normalization *)

let () =
  reg "normalize: ppx_expect's flexible form" @@ fun () ->
  let n = P.normalize in
  equal ~msg:"identity on plain text" string "a\nb" (n "a\nb");
  equal ~msg:"right-strips every line" string "a\nb" (n "a  \nb\t");
  equal ~msg:"drops blank edges" string "x" (n "\n\n  x\n\n");
  equal ~msg:"dedents to the minimum indent" string "a\n  b"
    (n "    a\n      b");
  equal ~msg:"keeps interior blank lines" string "a\n\nb" (n "  a\n\n  b");
  equal ~msg:"empty is empty" string "" (n "");
  equal ~msg:"whitespace-only is empty" string "" (n " \n\t\n");
  equal ~msg:"CRLF reads as LF" string "a\nb" (n "a\r\nb\r\n")

(* Literal rendering *)

let () =
  reg "literal: tags grow until the contents hold neither delimiter"
  @@ fun () ->
  equal ~msg:"plain tag" string "{|x|}" (P.literal ~delimiter:(P.Tag "") "x");
  equal ~msg:"a closing delimiter inside grows the tag" string
    "{xxx|a |} b|xxx}"
    (P.literal ~delimiter:(P.Tag "") "a |} b");
  equal ~msg:"a named tag is kept" string "{t|x|t}"
    (P.literal ~delimiter:(P.Tag "t") "x");
  equal ~msg:"quoted contents are escaped onto one line" string {|"a\"b\\c\nd"|}
    (P.literal ~delimiter:P.Quote "a\"b\\c\nd")

let () =
  reg "format_flexible: one line padded, several lines indented at column + 2"
  @@ fun () ->
  equal ~msg:"single line under a tag" string " hi "
    (P.format_flexible ~delimiter:(P.Tag "") ~column:4 "hi\n");
  equal ~msg:"single line under quotes" string "hi"
    (P.format_flexible ~delimiter:P.Quote ~column:4 "hi\n");
  equal ~msg:"empty output under a tag" string " "
    (P.format_flexible ~delimiter:(P.Tag "") ~column:4 "\n");
  equal ~msg:"several lines under a tag" string "\n      a\n        b\n      "
    (P.format_flexible ~delimiter:(P.Tag "") ~column:4 "a\n  b\n")

(* Quoted literals *)

let () =
  reg "quoted literal" @@ fun () ->
  let source = "let () = expect (f ()) @@ __POS_OF__ \"old\"\n" in
  let site = pos_of source "__POS_OF__" in
  equal ~msg:"the literal is replaced, escaped and on one line" string
    "let () = expect (f ()) @@ __POS_OF__ \"new\"\n"
    (apply source [ flexible ~site ~literal:"old" "new" ]);
  equal ~msg:"exact contents are escaped verbatim" string
    "let () = expect (f ()) @@ __POS_OF__ \"a\\nb\"\n"
    (apply source [ exact ~site ~literal:"old" "a\nb" ]);
  (* Escapes decode when the literal is compared with the compiled value. *)
  let escaped =
    "let () = expect x @@ __POS_OF__ \"a\\tb\\\"c\\\\d\\065\\x41\"\n"
  in
  let site = pos_of escaped "__POS_OF__" in
  equal ~msg:"escapes decode to the compiled value" string
    "let () = expect x @@ __POS_OF__ \"z\"\n"
    (apply escaped [ flexible ~site ~literal:"a\tb\"c\\dAA" "z" ])

(* Tagged literals *)

let () =
  reg "tagged literal" @@ fun () ->
  let source = "let () = expect (f ()) @@ __POS_OF__ {|old|}\n" in
  let site = pos_of source "__POS_OF__" in
  equal ~msg:"a one-line flexible correction is padded" string
    "let () = expect (f ()) @@ __POS_OF__ {| new |}\n"
    (apply source [ flexible ~site ~literal:"old" "new" ]);
  let named = "let () = expect (f ()) @@ __POS_OF__ {t|old|t}\n" in
  let site = pos_of named "__POS_OF__" in
  equal ~msg:"a named tag is kept" string
    "let () = expect (f ()) @@ __POS_OF__ {t| new |t}\n"
    (apply named [ flexible ~site ~literal:"old" "new" ])

let () =
  reg "multi-line correction re-indents to the call's line" @@ fun () ->
  let source =
    "let () =\n  expect (f ()) @@ __POS_OF__ {|\n    old\n  |};\n  ()\n"
  in
  let site = pos_of source "__POS_OF__" in
  equal ~msg:"lines land at the line's indentation + 2" string
    "let () =\n\
    \  expect (f ()) @@ __POS_OF__ {|\n\
    \    one\n\
    \      two\n\
    \    |};\n\
    \  ()\n"
    (apply source [ flexible ~site ~literal:"\n    old\n  " "one\n  two" ])

let () =
  reg "tag conflict grows the tag" @@ fun () ->
  let source = "let () = expect (f ()) @@ __POS_OF__ {|old|}\n" in
  let site = pos_of source "__POS_OF__" in
  equal ~msg:"the contents hold |} so the tag becomes xxx" string
    "let () = expect (f ()) @@ __POS_OF__ {xxx| a |} b |xxx}\n"
    (apply source [ flexible ~site ~literal:"old" "a |} b" ])

(* Refusals *)

let () =
  reg "drift refusal" @@ fun () ->
  let source = "let () = expect (f ()) @@ __POS_OF__ {|edited|}\n" in
  let site = pos_of source "__POS_OF__" in
  (match P.apply source [ flexible ~site ~literal:"old" "new" ] with
  | Error (P.Drifted p) ->
      is_true ~msg:"the refusal names the site" (p = site);
      is_false ~msg:"the message names no file: its caller does"
        (Text.contains_substring ~pattern:"test/t.ml"
           (P.error_message (P.Drifted p)))
  | Ok _ | Error (P.No_literal _) -> is_true ~msg:"drift is refused" false);
  let site = pos_of source "expect" in
  match P.apply source [ flexible ~site ~literal:"old" "new" ] with
  | Error (P.No_literal p) ->
      is_true ~msg:"no literal at the position" (p = site)
  | Ok _ | Error (P.Drifted _) ->
      is_true ~msg:"a non-literal position is refused" false

let () =
  reg "a position past the end of the file is refused" @@ fun () ->
  match
    P.apply "let x = 1\n" [ flexible ~site:("t.ml", 9, 0, 0) ~literal:"" "" ]
  with
  | Error (P.No_literal _) -> is_true ~msg:"no such line" true
  | Ok _ | Error (P.Drifted _) -> is_true ~msg:"no such line" false

(* CRLF sources: line offsets, delimiters and the compiled value all read
   through the carriage returns, and the rest of the file keeps them. *)

let () =
  reg "a CRLF source file" @@ fun () ->
  let source =
    "let () =\r\n\
    \  expect (f ()) @@ __POS_OF__ {|\r\n\
    \    old\r\n\
    \  |};\r\n\
    \  ()\r\n"
  in
  let site = pos_of source "__POS_OF__" in
  (* The lexer reads a CRLF newline inside a literal as LF, so the value
     the binary was compiled with has none. *)
  equal ~msg:"the tagged literal is found and its CRLF contents decode" string
    "let () =\r\n\
    \  expect (f ()) @@ __POS_OF__ {|\n\
    \    one\n\
    \    two\n\
    \    |};\r\n\
    \  ()\r\n"
    (apply source [ flexible ~site ~literal:"\n    old\n  " "one\ntwo" ]);
  (* A quoted literal continued over a CRLF line break: the lexer drops
     the backslash, the newline and the next line's leading blanks. *)
  let continued = "let () = expect x @@ __POS_OF__ \"a\\\r\n   b\"\r\n" in
  let site = pos_of continued "__POS_OF__" in
  equal ~msg:"a CRLF continuation decodes to the compiled value" string
    "let () = expect x @@ __POS_OF__ \"z\"\r\n"
    (apply continued [ flexible ~site ~literal:"ab" "z" ])

(* Several patches *)

let () =
  reg "two patches in one file apply once, offsets adjusted" @@ fun () ->
  let source =
    "let () = expect a @@ __POS_OF__ {|x|};\n  expect b @@ __POS_OF__ \"y\"\n"
  in
  let first = pos_of source "__POS_OF__ {|x|}" in
  let second = pos_of source "__POS_OF__ \"y\"" in
  equal ~msg:"both literals are replaced, the second after the first grew"
    string
    "let () = expect a @@ __POS_OF__ {| longer text |};\n\
    \  expect b @@ __POS_OF__ \"z\"\n"
    (apply source
       [
         flexible ~site:second ~literal:"y" "z";
         flexible ~site:first ~literal:"x" "longer text";
       ]);
  equal ~msg:"the rest of the file is byte-identical" string
    "let () = expect a @@ __POS_OF__ {|x|};\n  expect b @@ __POS_OF__ \"y\"\n"
    (apply source [])

(* Position shapes *)

let () =
  reg "a literal inside parentheses" @@ fun () ->
  let source = "let () = expect (f ()) (__POS_OF__ \"old\")\n" in
  let site = pos_of source "(__POS_OF__" in
  equal ~msg:"the parenthesis and the token are lexed past" string
    "let () = expect (f ()) (__POS_OF__ \"new\")\n"
    (apply source [ flexible ~site ~literal:"old" "new" ]);
  let broken =
    "let () =\n  expect (f ())\n    (__POS_OF__\n       {|old|})\n"
  in
  let site = pos_of broken "(__POS_OF__" in
  equal ~msg:"the literal may sit on the next line" string
    "let () =\n  expect (f ())\n    (__POS_OF__\n       {| new |})\n"
    (apply broken [ flexible ~site ~literal:"old" "new" ]);
  (* The rewriter's shape: the position names the literal itself. *)
  let node = "  [%expect {|old|}]\n" in
  let site = pos_of node "{|old|}" in
  equal ~msg:"a position on the literal itself is the literal" string
    "  [%expect {| new |}]\n"
    (apply node [ flexible ~site ~literal:"old" "new" ])

(* Whitespace *)

let () =
  reg "normalize: VT and FF are whitespace, and tabs are not indentation"
  @@ fun () ->
  let n = P.normalize in
  equal ~msg:"VT and FF strip and blank like spaces" string "a"
    (n "\x0b\n a \x0c\n\x0c");
  equal ~msg:"a tab-indented block loses its relative indentation" string "a\nb"
    (n "\ta\n\t\tb")

(* Expect nodes *)

let () =
  reg "a node without payload compiles to \"\" and gets a literal" @@ fun () ->
  let node = "  [%expect]\n" in
  let site = pos_of node "[%expect]" in
  equal ~msg:"a space and a {|...|} before the closing bracket" string
    "  [%expect {| new |}]\n"
    (apply node [ flexible ~site ~literal:"" "new" ]);
  match P.apply node [ flexible ~site ~literal:"stale" "new" ] with
  | Error (P.Drifted p) -> is_true ~msg:"the site" (p = site)
  | Ok _ | Error (P.No_literal _) ->
      fail "a payloadless node with a non-empty literal is Drifted"

let () =
  reg "the head of a tagged expect node is kept" @@ fun () ->
  let node = "  {%expect t|old|t}\n" in
  let site = pos_of node "{%expect" in
  equal ~msg:"the head and its tag" string "  {%expect t| new |t}\n"
    (apply node [ flexible ~site ~literal:"old" "new" ])

(* Quoted literals *)

let () =
  reg "a quoted literal escapes non-ASCII bytes as decimal" @@ fun () ->
  let source = "let () = expect x @@ __POS_OF__ \"old\"\n" in
  let site = pos_of source "__POS_OF__" in
  equal ~msg:"é is two decimal escapes" string
    "let () = expect x @@ __POS_OF__ \"caf\\195\\169\"\n"
    (apply source [ flexible ~site ~literal:"old" "café" ])

let () =
  reg "a quoted literal lays several lines out one space in" @@ fun () ->
  let source = "  expect x @@ __POS_OF__ \"old\"\n" in
  let site = pos_of source "__POS_OF__" in
  equal
    ~msg:"a space and a newline, each line one space in, a newline and a space"
    string "  expect x @@ __POS_OF__ \" \\n a\\n   b\\n \"\n"
    (apply source [ flexible ~site ~literal:"old" "a\n  b" ])

(* Refusals *)

let () =
  reg "anything but whitespace, (, __POS_OF__ and a node head is no literal"
  @@ fun () ->
  let commented = "let () = expect x @@ __POS_OF__ (* c *) \"old\"\n" in
  (match
     P.apply commented
       [ flexible ~site:(pos_of commented "__POS_OF__") ~literal:"old" "new" ]
   with
  | Error (P.No_literal _) -> ()
  | Ok _ | Error (P.Drifted _) -> fail "a comment before the literal");
  let unclosed = "let () = expect x @@ __POS_OF__ {|old\n" in
  match
    P.apply unclosed
      [ flexible ~site:(pos_of unclosed "__POS_OF__") ~literal:"old" "new" ]
  with
  | Error (P.No_literal _) -> ()
  | Ok _ | Error (P.Drifted _) -> fail "a literal never closed"

let () =
  reg "the two refusals' sentences" @@ fun () ->
  let site = ("test/t.ml", 4, 2, 0) in
  equal ~msg:"No_literal" string "no string literal at the recorded position"
    (P.error_message (P.No_literal site));
  equal ~msg:"Drifted" string
    "the literal differs from the value the binary was compiled with; rebuild \
     and rerun"
    (P.error_message (P.Drifted site))

let () =
  reg "a CR before an LF inside a literal is refused as drifted" @@ fun () ->
  let drifted source literal =
    match
      P.apply source
        [ flexible ~site:(pos_of source "__POS_OF__") ~literal "new" ]
    with
    | Error (P.Drifted _) -> true
    | Ok _ | Error (P.No_literal _) -> false
  in
  let one = "let () = expect x @@ __POS_OF__ {|a\r\nb|}\n" in
  is_true ~msg:"OCaml 5.0 and 5.1 keep the CR in the compiled value"
    (drifted one "a\r\nb");
  let two = "let () = expect x @@ __POS_OF__ {|a\r\r\nb|}\n" in
  is_true ~msg:"no compiler drops two CRs, the decoder does"
    (drifted two "a\r\nb")

let () =
  reg "the first refused patch of the list is the error, and none applies"
  @@ fun () ->
  let source =
    "let () = expect a @@ __POS_OF__ {|x|};\n  expect b @@ __POS_OF__ {|y|}\n"
  in
  let first = pos_of source "__POS_OF__ {|x|}" in
  let second = pos_of source "__POS_OF__ {|y|}" in
  (match
     P.apply source
       [
         flexible ~site:second ~literal:"stale" "z";
         flexible ~site:first ~literal:"stale" "w";
       ]
   with
  | Error (P.Drifted p) ->
      is_true ~msg:"the list's first, not the file's" (p = second)
  | Ok _ | Error (P.No_literal _) -> fail "two refused patches");
  match
    P.apply source
      [
        flexible ~site:first ~literal:"x" "w";
        flexible ~site:second ~literal:"stale" "z";
      ]
  with
  | Error (P.Drifted p) -> is_true ~msg:"one refusal refuses all" (p = second)
  | Ok _ | Error (P.No_literal _) -> fail "a refused patch among good ones"

(* The layout's law *)

let () =
  registered :=
    prop "a flexible correction normalizes to the content's normal form"
      Gen.(
        triple
          (list ~size:(int_range 0 6)
             (string_of ~size:(int_range 0 12)
                (of_list [ ' '; ' '; 'a'; 'b'; '\t'; '|'; '}' ])))
          (of_list [ P.Quote; P.Tag ""; P.Tag "t" ])
          (int_range 0 8))
      (fun (lines, delimiter, column) ->
        let content = String.concat "\n" lines in
        equal ~msg:"normalize of the written contents" string
          (P.normalize content)
          (P.normalize (P.format_flexible ~delimiter ~column content)))
    :: !registered

let () =
  reg "fix_tag grows a tag until the contents hold neither delimiter"
  @@ fun () ->
  equal ~msg:"no conflict: as given" string "t" (P.fix_tag ~contents:"x" "t");
  equal ~msg:"an opening delimiter inside" string "txxx"
    (P.fix_tag ~contents:"a {t| b" "t");
  equal ~msg:"both, grown twice" string "txxxxxx"
    (P.fix_tag ~contents:"|t} {txxx|" "t")

let tests = List.rev !registered
let () = exit @@ Windtrap.run "source_patch" tests
