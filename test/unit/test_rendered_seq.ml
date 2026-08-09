(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Rendered_seq: the reader from a printed [[…]] / [[|…|]] back to
   its elements. [Diff.sequences] reaches it only indirectly, where a
   misparse surfaces as a missing or coarse diff rather than as a parse
   failure — these tests name the reader's own outcome instead: the bracket
   form, the canonical elements, their byte extents, and every rendering it
   declines. *)

open Windtrap
module Rendered_seq = Windtrap.Private.Rendered_seq

let check name cond = is_true ~msg:name cond
let check_int name ~expected ~actual = equal ~msg:name int expected actual

let check_strings name ~expected ~actual =
  equal ~msg:name (list string) expected actual

(* Helpers *)

let parse = Rendered_seq.parse

let elements name s =
  match parse s with
  | Some (_, es) -> es
  | None -> fail (name ^ ": parse declined " ^ String.escaped s)

let canonicals name s =
  Array.to_list
    (Array.map
       (fun (e : Rendered_seq.element) -> e.canonical)
       (elements name s))

(* The raw text each extent covers, read back out of the source rendering:
   the contract is a byte range into [s], so it is checked against [s]. *)
let slices name s =
  Array.to_list
    (Array.map
       (fun (e : Rendered_seq.element) ->
         String.sub s e.extent.Rendered_seq.start e.extent.Rendered_seq.length)
       (elements name s))

let kind name s =
  match parse s with
  | Some (k, _) -> k
  | None -> fail (name ^ ": parse declined " ^ String.escaped s)

let declines name s =
  match parse s with
  | None -> ()
  | Some (_, es) ->
      fail
        (Printf.sprintf "%s: parse accepted %s as %d elements" name
           (String.escaped s) (Array.length es))

(* Bracket forms *)

let test_kinds () =
  check "a list rendering is `List" (kind "list" "[1; 2]" = `List);
  check "an array rendering is `Array" (kind "array" "[|1; 2|]" = `Array);
  check_strings "array elements drop the bars" ~expected:[ "1"; "2" ]
    ~actual:(canonicals "array" "[|1; 2|]");
  check_int "an empty list has no elements" ~expected:0
    ~actual:(Array.length (elements "empty list" "[]"));
  check_int "an empty array has no elements" ~expected:0
    ~actual:(Array.length (elements "empty array" "[||]"));
  check "an empty array is still `Array" (kind "empty array" "[||]" = `Array)

(* Splitting: only top-level separators, and only outside literals *)

let test_splitting () =
  check_strings "top-level semicolons split" ~expected:[ "1"; "2"; "3" ]
    ~actual:(canonicals "flat" "[1; 2; 3]");
  check_strings "a semicolon inside brackets belongs to its element"
    ~expected:[ "[1; 2]"; "[3]" ]
    ~actual:(canonicals "nested list" "[[1; 2]; [3]]");
  check_strings "nested arrays nest through their brackets"
    ~expected:[ "[|1; 2|]"; "[|3|]" ]
    ~actual:(canonicals "nested array" "[[|1; 2|]; [|3|]]");
  check_strings "parentheses and braces nest too"
    ~expected:[ "(1, 2)"; "{a = 1; b = 2}" ]
    ~actual:(canonicals "tuples" "[(1, 2); {a = 1; b = 2}]");
  check_strings "a semicolon inside a string literal is text"
    ~expected:[ {|"a; b"|}; {|"c"|} ]
    ~actual:(canonicals "string" {|["a; b"; "c"]|});
  check_strings "a bracket inside a string literal is text"
    ~expected:[ {|"]["|}; "1" ]
    ~actual:(canonicals "bracket in string" {|["]["; 1]|});
  check_strings "an escaped quote does not close the literal"
    ~expected:[ {|"say \"hi\"; bye"|}; "1" ]
    ~actual:(canonicals "escaped quote" {|["say \"hi\"; bye"; 1]|});
  check_strings "an escaped backslash before the close does not run on"
    ~expected:[ {|"tail\\"|}; "1" ]
    ~actual:(canonicals "escaped backslash" {|["tail\\"; 1]|});
  check_strings "a semicolon inside a char literal is text"
    ~expected:[ {|';'|}; {|'['|} ]
    ~actual:(canonicals "char literals" {|[';'; '[']|});
  check_strings "escaped char literals of both lengths copy whole"
    ~expected:[ {|'\n'|}; {|'\000'|}; {|'\xFF'|} ]
    ~actual:(canonicals "char escapes" {|['\n'; '\000'; '\xFF']|});
  (* A quote that starts no literal shape is an ordinary character; the scan
     must not swallow the rest of the rendering looking for its partner. *)
  check_strings "a bare apostrophe is an ordinary character"
    ~expected:[ "don't"; "x" ]
    ~actual:(canonicals "apostrophe" "[don't; x]")

(* Canonicalization *)

let test_canonicalization () =
  check_strings "a wrapped rendering reads as the flat one"
    ~expected:[ "1"; "2"; "3" ]
    ~actual:(canonicals "wrapped" "[1;\n 2;\n 3]");
  check_strings "whitespace runs collapse to one space" ~expected:[ "(1, 2)" ]
    ~actual:(canonicals "collapse" "[(1,\n\t  2)]");
  check_strings "leading and trailing whitespace is dropped"
    ~expected:[ "1"; "2" ]
    ~actual:(canonicals "outer ws" "[  1  ;  2  ]");
  check_strings "whitespace inside a string literal is content"
    ~expected:[ {|"a  b"|}; {|"c
d"|} ]
    ~actual:(canonicals "ws in string" "[\"a  b\"; \"c\nd\"]");
  check_strings "whitespace inside a char literal is content"
    ~expected:[ {|' '|}; "x" ]
    ~actual:(canonicals "ws in char" {|[' '; x]|});
  (* Same elements, different boxes: canonical forms must agree, which is
     what makes the element comparison wrap-insensitive. *)
  let flat = canonicals "flat" {|[("a", [1; 2]); ("b", [3])]|} in
  let wrapped =
    canonicals "wrapped" "[(\"a\", [1;\n     2]);\n (\"b\", [3])]"
  in
  check_strings "two boxes of the same value canonicalize alike" ~expected:flat
    ~actual:wrapped

(* Extents *)

let test_extents () =
  check_strings "extents cover the elements and nothing else"
    ~expected:[ "1"; "2" ] ~actual:(slices "flat" "[1; 2]");
  check_strings "extents exclude the separator and dropped whitespace"
    ~expected:[ "1"; "2" ]
    ~actual:(slices "padded" "[  1  ;  2  ]");
  check_strings "a wrapped element's extent is its own bytes"
    ~expected:[ "(1,\n 2)"; "3" ]
    ~actual:(slices "wrapped" "[(1,\n 2); 3]");
  check_strings "extents are absolute offsets into the whole string"
    ~expected:[ "1"; "2" ]
    ~actual:(slices "outer ws" "  [1; 2]  ");
  check_strings "array extents skip the bars" ~expected:[ "1"; "2" ]
    ~actual:(slices "array" "[|1; 2|]");
  (* Multi-byte elements: the scan only ever cuts at ASCII delimiters, so an
     extent can neither begin nor end inside a UTF-8 sequence. *)
  check_strings "extents never split a UTF-8 sequence"
    ~expected:[ "\xC3\xA9"; "\xC3\xB8" ]
    ~actual:(slices "utf8" "[\xC3\xA9; \xC3\xB8]");
  (* Ascending and disjoint, separated by at least their [';']. *)
  let es = elements "disjoint" "[10; 200; 3]" in
  let ok = ref true in
  let prev_end = ref (-1) in
  Array.iter
    (fun (e : Rendered_seq.element) ->
      let s = e.extent.Rendered_seq.start
      and l = e.extent.Rendered_seq.length in
      if l <= 0 || s <= !prev_end then ok := false;
      prev_end := s + l)
    es;
  check "extents are ascending, positive, and disjoint" !ok

(* Declining *)

let test_declines () =
  declines "empty string" "";
  declines "whitespace only" "   ";
  declines "a scalar" "true";
  declines "a record" "{x = 1}";
  declines "a bare bracket" "[";
  declines "unclosed inner bracket" "[1; (2]";
  declines "unopened inner bracket" "[1)]";
  declines "unterminated string literal" {|["a; b]|};
  (* The escape scan must not read past the closing bracket either. *)
  declines "a backslash with nothing left to escape" {|["a\]|};
  declines "an empty element" "[1;; 2]";
  declines "a leading separator" "[; 1]";
  declines "a trailing separator" "[1; 2;]";
  declines "a lone separator" "[;]";
  (* The whole point of declining: a caller may not read [None] as
     agreement, so equal-looking-but-unreadable renderings decline too. *)
  declines "unbalanced brackets in both halves" "[(; (]"

let tests =
  [
    test "bracket forms and empty sequences" test_kinds;
    test "splitting: top-level separators only" test_splitting;
    test "canonical form" test_canonicalization;
    test "extents" test_extents;
    test "conservative declines" test_declines;
  ]
