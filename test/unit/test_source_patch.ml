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
let check name cond = is_true ~msg:name cond
let check_string name ~expected ~actual = equal ~msg:name string expected actual

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

let flexible ~pos ~literal content =
  P.patch ~pos ~literal ~style:P.Flexible content

let exact ~pos ~literal content = P.patch ~pos ~literal ~style:P.Exact content

(* Normalization *)

let () =
  reg "normalize: ppx_expect's flexible form" @@ fun () ->
  let n = P.normalize in
  check_string "identity on plain text" ~expected:"a\nb" ~actual:(n "a\nb");
  check_string "right-strips every line" ~expected:"a\nb" ~actual:(n "a  \nb\t");
  check_string "drops blank edges" ~expected:"x" ~actual:(n "\n\n  x\n\n");
  check_string "dedents to the minimum indent" ~expected:"a\n  b"
    ~actual:(n "    a\n      b");
  check_string "keeps interior blank lines" ~expected:"a\n\nb"
    ~actual:(n "  a\n\n  b");
  check_string "empty is empty" ~expected:"" ~actual:(n "");
  check_string "whitespace-only is empty" ~expected:"" ~actual:(n " \n\t\n");
  check_string "CRLF reads as LF" ~expected:"a\nb" ~actual:(n "a\r\nb\r\n")

(* Literal rendering *)

let () =
  reg "literal: tags grow until the contents hold neither delimiter"
  @@ fun () ->
  check_string "plain tag" ~expected:"{|x|}"
    ~actual:(P.literal ~delimiter:(P.Tag "") "x");
  check_string "a closing delimiter inside grows the tag"
    ~expected:"{xxx|a |} b|xxx}"
    ~actual:(P.literal ~delimiter:(P.Tag "") "a |} b");
  check_string "a named tag is kept" ~expected:"{t|x|t}"
    ~actual:(P.literal ~delimiter:(P.Tag "t") "x");
  check_string "quoted contents are escaped onto one line"
    ~expected:{|"a\"b\\c\nd"|}
    ~actual:(P.literal ~delimiter:P.Quote "a\"b\\c\nd")

let () =
  reg "format_flexible: one line padded, several lines indented at column + 2"
  @@ fun () ->
  check_string "single line under a tag" ~expected:" hi "
    ~actual:(P.format_flexible ~delimiter:(P.Tag "") ~column:4 "hi\n");
  check_string "single line under quotes" ~expected:"hi"
    ~actual:(P.format_flexible ~delimiter:P.Quote ~column:4 "hi\n");
  check_string "empty output under a tag" ~expected:" "
    ~actual:(P.format_flexible ~delimiter:(P.Tag "") ~column:4 "\n");
  check_string "several lines under a tag"
    ~expected:"\n      a\n        b\n      "
    ~actual:(P.format_flexible ~delimiter:(P.Tag "") ~column:4 "a\n  b\n")

(* Quoted literals *)

let () =
  reg "quoted literal" @@ fun () ->
  let source = "let () = expect (f ()) @@ __POS_OF__ \"old\"\n" in
  let pos = pos_of source "__POS_OF__" in
  check_string "the literal is replaced, escaped and on one line"
    ~expected:"let () = expect (f ()) @@ __POS_OF__ \"new\"\n"
    ~actual:(apply source [ flexible ~pos ~literal:"old" "new" ]);
  check_string "exact contents are escaped verbatim"
    ~expected:"let () = expect (f ()) @@ __POS_OF__ \"a\\nb\"\n"
    ~actual:(apply source [ exact ~pos ~literal:"old" "a\nb" ]);
  (* Escapes decode when the literal is compared with the compiled value. *)
  let escaped =
    "let () = expect x @@ __POS_OF__ \"a\\tb\\\"c\\\\d\\065\\x41\"\n"
  in
  let pos = pos_of escaped "__POS_OF__" in
  check_string "escapes decode to the compiled value"
    ~expected:"let () = expect x @@ __POS_OF__ \"z\"\n"
    ~actual:(apply escaped [ flexible ~pos ~literal:"a\tb\"c\\dAA" "z" ])

(* Tagged literals *)

let () =
  reg "tagged literal" @@ fun () ->
  let source = "let () = expect (f ()) @@ __POS_OF__ {|old|}\n" in
  let pos = pos_of source "__POS_OF__" in
  check_string "a one-line flexible correction is padded"
    ~expected:"let () = expect (f ()) @@ __POS_OF__ {| new |}\n"
    ~actual:(apply source [ flexible ~pos ~literal:"old" "new" ]);
  let named = "let () = expect (f ()) @@ __POS_OF__ {t|old|t}\n" in
  let pos = pos_of named "__POS_OF__" in
  check_string "a named tag is kept"
    ~expected:"let () = expect (f ()) @@ __POS_OF__ {t| new |t}\n"
    ~actual:(apply named [ flexible ~pos ~literal:"old" "new" ])

let () =
  reg "multi-line correction re-indents to the call's line" @@ fun () ->
  let source =
    "let () =\n  expect (f ()) @@ __POS_OF__ {|\n    old\n  |};\n  ()\n"
  in
  let pos = pos_of source "__POS_OF__" in
  check_string "lines land at the line's indentation + 2"
    ~expected:
      "let () =\n\
      \  expect (f ()) @@ __POS_OF__ {|\n\
      \    one\n\
      \      two\n\
      \    |};\n\
      \  ()\n"
    ~actual:
      (apply source [ flexible ~pos ~literal:"\n    old\n  " "one\n  two" ])

let () =
  reg "tag conflict grows the tag" @@ fun () ->
  let source = "let () = expect (f ()) @@ __POS_OF__ {|old|}\n" in
  let pos = pos_of source "__POS_OF__" in
  check_string "the contents hold |} so the tag becomes xxx"
    ~expected:"let () = expect (f ()) @@ __POS_OF__ {xxx| a |} b |xxx}\n"
    ~actual:(apply source [ flexible ~pos ~literal:"old" "a |} b" ])

(* Refusals *)

let () =
  reg "drift refusal" @@ fun () ->
  let source = "let () = expect (f ()) @@ __POS_OF__ {|edited|}\n" in
  let pos = pos_of source "__POS_OF__" in
  (match P.apply source [ flexible ~pos ~literal:"old" "new" ] with
  | Error (P.Drifted p) ->
      check "the refusal names the site" (p = pos);
      check "the message names the file and line"
        (Text.contains_substring ~pattern:"test/t.ml:1"
           (P.error_message (P.Drifted p)))
  | Ok _ | Error (P.No_literal _) -> check "drift is refused" false);
  let pos = pos_of source "expect" in
  match P.apply source [ flexible ~pos ~literal:"old" "new" ] with
  | Error (P.No_literal p) -> check "no literal at the position" (p = pos)
  | Ok _ | Error (P.Drifted _) ->
      check "a non-literal position is refused" false

let () =
  reg "a position past the end of the file is refused" @@ fun () ->
  match
    P.apply "let x = 1\n" [ flexible ~pos:("t.ml", 9, 0, 0) ~literal:"" "" ]
  with
  | Error (P.No_literal _) -> check "no such line" true
  | Ok _ | Error (P.Drifted _) -> check "no such line" false

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
  let pos = pos_of source "__POS_OF__" in
  check_string "the tagged literal is found and its CRLF contents decode"
    ~expected:
      "let () =\r\n\
      \  expect (f ()) @@ __POS_OF__ {|\n\
      \    one\n\
      \    two\n\
      \    |};\r\n\
      \  ()\r\n"
    ~actual:
      (apply source [ flexible ~pos ~literal:"\r\n    old\r\n  " "one\ntwo" ]);
  (* A quoted literal continued over a CRLF line break: the lexer drops
     the backslash, the newline and the next line's leading blanks. *)
  let continued = "let () = expect x @@ __POS_OF__ \"a\\\r\n   b\"\r\n" in
  let pos = pos_of continued "__POS_OF__" in
  check_string "a CRLF continuation decodes to the compiled value"
    ~expected:"let () = expect x @@ __POS_OF__ \"z\"\r\n"
    ~actual:(apply continued [ flexible ~pos ~literal:"ab" "z" ])

(* Several patches *)

let () =
  reg "two patches in one file apply once, offsets adjusted" @@ fun () ->
  let source =
    "let () = expect a @@ __POS_OF__ {|x|};\n  expect b @@ __POS_OF__ \"y\"\n"
  in
  let first = pos_of source "__POS_OF__ {|x|}" in
  let second = pos_of source "__POS_OF__ \"y\"" in
  check_string "both literals are replaced, the second after the first grew"
    ~expected:
      "let () = expect a @@ __POS_OF__ {| longer text |};\n\
      \  expect b @@ __POS_OF__ \"z\"\n"
    ~actual:
      (apply source
         [
           flexible ~pos:second ~literal:"y" "z";
           flexible ~pos:first ~literal:"x" "longer text";
         ]);
  check_string "the rest of the file is byte-identical"
    ~expected:
      "let () = expect a @@ __POS_OF__ {|x|};\n  expect b @@ __POS_OF__ \"y\"\n"
    ~actual:(apply source [])

(* Position shapes *)

let () =
  reg "a literal inside parentheses" @@ fun () ->
  let source = "let () = expect (f ()) (__POS_OF__ \"old\")\n" in
  let pos = pos_of source "(__POS_OF__" in
  check_string "the parenthesis and the token are lexed past"
    ~expected:"let () = expect (f ()) (__POS_OF__ \"new\")\n"
    ~actual:(apply source [ flexible ~pos ~literal:"old" "new" ]);
  let broken =
    "let () =\n  expect (f ())\n    (__POS_OF__\n       {|old|})\n"
  in
  let pos = pos_of broken "(__POS_OF__" in
  check_string "the literal may sit on the next line"
    ~expected:"let () =\n  expect (f ())\n    (__POS_OF__\n       {| new |})\n"
    ~actual:(apply broken [ flexible ~pos ~literal:"old" "new" ]);
  (* The rewriter's shape: the position names the literal itself. *)
  let node = "  [%expect {|old|}]\n" in
  let pos = pos_of node "{|old|}" in
  check_string "a position on the literal itself is the literal"
    ~expected:"  [%expect {| new |}]\n"
    ~actual:(apply node [ flexible ~pos ~literal:"old" "new" ])

let tests = List.rev !registered
let () = exit @@ Windtrap.run "source_patch" tests
