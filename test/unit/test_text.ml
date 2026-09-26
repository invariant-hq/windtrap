(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Text = Windtrap.Private.Text

let name3 (name, _, _) = name
let window = pair int string

(* Well-formed UTF-8: up to 24 code points of one to four bytes, line feeds
   among them. *)
let utf8 =
  Gen.with_pp
    (fun ppf s -> Format.fprintf ppf "%S" s)
    (Gen.map (String.concat "")
       (Gen.list ~size:(Gen.int_range 0 24)
          (Gen.of_list [ "a"; "b"; "\n"; "é"; "€"; "𝄞" ])))

let on_code_point s i = i = String.length s || Char.code s.[i] land 0xC0 <> 0x80

(* Newlines *)

let newlines =
  group "Newlines"
    [
      cases
        "normalize_newlines replaces each CRLF, and each other CR, by one LF"
        ~name:name3
        [
          ("LF alone", "a\nb\n", "a\nb\n");
          ("CRLF", "a\r\nb\r\n", "a\nb\n");
          ("a lone CR", "a\rb", "a\nb");
          ("mixed endings", "a\r\nb\rc\nd", "a\nb\nc\nd");
          ("a CR that ends the string", "a\r", "a\n");
          ("a CR before a CRLF", "a\r\r\nb", "a\n\nb");
          ("the empty string", "", "");
        ]
        (fun (_, s, normalized) ->
          equal string normalized (Text.normalize_newlines s));
      cases "ensure_trailing_newline appends LF to a string that lacks one"
        ~name:name3
        [
          ("no final LF", "a", "a\n");
          ("a final LF", "a\n", "a\n");
          ("the empty string", "", "\n");
        ]
        (fun (_, s, ensured) ->
          equal string ensured (Text.ensure_trailing_newline s));
      cases "split_lines splits at each LF, a final LF ending the last line"
        ~name:name3
        [
          ("a final LF", "a\nb\n", [ "a"; "b" ]);
          ("no final LF", "a\nb", [ "a"; "b" ]);
          ("an empty last line", "a\n\n", [ "a"; "" ]);
          ("an empty line inside", "a\n\nb", [ "a"; ""; "b" ]);
          ("one LF", "\n", [ "" ]);
          ("the empty string", "", []);
          ("CRLF, the CR kept", "a\r\nb\r\n", [ "a\r"; "b\r" ]);
        ]
        (fun (_, s, lines) -> equal (list string) lines (Text.split_lines s));
    ]

(* Lengths and cuts *)

let lengths =
  [
    ("ASCII", "hello", 5);
    ("two-byte code points", "héllo", 5);
    ("a four-byte code point", "\u{1F42B}", 1);
    ("the empty string", "", 0);
    ("a letter and a combining accent", "e\u{0301}", 2);
    ("a wide character", "\u{6F22}", 1);
    ("two stray bytes", "\xff\xfe", 2);
    ("a sequence cut short", "\xe2\x82", 1);
  ]

let truncations =
  [
    ("shorter than the bound", (5, "abc"), "abc");
    ("at the bound", (5, "abcde"), "abcde");
    ("past the bound", (5, "abcdefgh"), "ab...");
    ("two-byte code points under the bound", (3, "éé"), "éé");
    ("two-byte code points at a bound of 3", (3, "ééé"), "ééé");
    ("two-byte code points at a bound of 4", (4, "éééé"), "éééé");
    ("two-byte code points past a bound of 3", (3, "ééééé"), "...");
    ("two-byte code points past the bound", (5, "éééééé"), "éé...");
    ("a sequence cut short", (5, "\xe2\x82abcdef"), "\xe2\x82a...");
    ("a bound of 2", (2, "abc"), "..");
    ("a bound of 1", (1, "abc"), ".");
    ("a bound of 0", (0, "abc"), "");
    ("the empty string under a bound of 0", (0, ""), "");
    ("a negative bound", (-4, "abc"), "");
  ]

let windows =
  [
    ( "head, a string within the bound is whole",
      (fun () -> Text.window ~bytes:3 Head "abc"),
      (0, "abc") );
    ("head, cut", (fun () -> Text.window ~bytes:4 Head "abcdefgh"), (0, "abcd"));
    ( "head, a cut inside a code point moves back",
      (fun () -> Text.window ~bytes:3 Head "éé"),
      (0, "é") );
    ("head, a bound of 0", (fun () -> Text.window ~bytes:0 Head "abc"), (0, ""));
    ( "head, a negative bound is 0",
      (fun () -> Text.window ~bytes:(-1) Head "abc"),
      (0, "") );
    ( "head, a malformed sequence moves the cut back three bytes at most",
      (fun () -> Text.window ~bytes:5 Head "a\x80\x80\x80\x80\x80\x80"),
      (0, "a\x80") );
    ( "head, a cut never moves before the start",
      (fun () -> Text.window ~bytes:1 Head "\x80\x80"),
      (0, "") );
    ( "head, the first two lines",
      (fun () -> Text.window ~lines:2 ~bytes:100 Head "a\nb\nc\nd\n"),
      (0, "a\nb\n") );
    ( "head, fewer lines than the bound",
      (fun () -> Text.window ~lines:5 ~bytes:100 Head "a\nb\n"),
      (0, "a\nb\n") );
    ( "head, the byte bound under a line bound",
      (fun () -> Text.window ~lines:2 ~bytes:3 Head "a\nb\nc\n"),
      (0, "a\nb") );
    ( "tail, a string within the bound is whole",
      (fun () -> Text.window ~bytes:3 Tail "abc"),
      (0, "abc") );
    ( "tail, a string at the bound is whole, a continuation byte first",
      (fun () -> Text.window ~bytes:2 Tail "\x80\x80"),
      (0, "\x80\x80") );
    ("tail, cut", (fun () -> Text.window ~bytes:4 Tail "abcdefgh"), (4, "efgh"));
    ( "tail, a cut inside a code point moves forward",
      (fun () -> Text.window ~bytes:3 Tail "éé"),
      (2, "é") );
    ( "tail, a bound of 0 keeps nothing, at the end",
      (fun () -> Text.window ~bytes:0 Tail "abc"),
      (3, "") );
    ( "tail, a malformed sequence moves the cut forward three bytes at most",
      (fun () -> Text.window ~bytes:5 Tail "\x80\x80\x80\x80\x80\x80a"),
      (5, "\x80a") );
    ( "tail, the last two lines, a final LF ending the last",
      (fun () -> Text.window ~lines:2 ~bytes:100 Tail "a\nb\nc\nd\n"),
      (4, "c\nd\n") );
    ( "tail, the last two lines, no final LF",
      (fun () -> Text.window ~lines:2 ~bytes:100 Tail "a\nb\nc\nd"),
      (4, "c\nd") );
    ( "around, a string within the bound is whole",
      (fun () -> Text.window ~bytes:4 (Around 3) "abcd"),
      (0, "abcd") );
    ( "around, centred on the offset",
      (fun () -> Text.window ~bytes:4 (Around 5) "abcdefghij"),
      (3, "defg") );
    ( "around, a cut inside a code point moves inward",
      (fun () -> Text.window ~bytes:4 (Around 3) "abééé"),
      (1, "bé") );
    ( "around, lines bound nothing",
      (fun () -> Text.window ~lines:1 ~bytes:4 (Around 4) "a\nb\nc\nd\n"),
      (2, "b\nc\n") );
  ]

let anchored =
  let open Gen in
  with_pp
    (fun ppf (s, bytes, at) ->
      Format.fprintf ppf "%S, %d bytes, %s" s bytes
        (match at with
        | Text.Head -> "Head"
        | Tail -> "Tail"
        | Around i -> "Around " ^ string_of_int i))
    (let* s, bytes = pair utf8 (int_range (-2) 40) in
     let+ at =
       one_of
         [
           constant Text.Head;
           constant Text.Tail;
           map (fun i -> Text.Around i) (int_range 0 (String.length s));
         ]
     in
     (s, bytes, at))

(* The part is a slice of [s] within the bound, cut on code points; a head
   starts [s] and a tail ends it. *)
let window_law (s, bytes, at) =
  let offset, part = Text.window ~bytes at s in
  let stop = offset + String.length part in
  cover "cut" (String.length s > bytes);
  equal string (String.sub s offset (String.length part)) part;
  at_most int ~than:(max 0 bytes) (String.length part);
  satisfies ~claim:"a start on a code point" int (on_code_point s) offset;
  satisfies ~claim:"an end on a code point" int (on_code_point s) stop;
  match at with
  | Head -> equal int 0 offset
  | Tail -> equal int (String.length s) stop
  | Around _ -> ()

(* Well-formed UTF-8 moves a cut back by at most three bytes. *)
let longest_law (s, bytes, at) =
  let _, part = Text.window ~bytes at s in
  match at with
  | Head | Tail ->
      at_least int
        ~than:(min (String.length s) (max 0 bytes) - 3)
        (String.length part)
  | Around _ -> ()

let line_bound =
  Gen.with_pp
    (fun ppf (s, lines, tail) ->
      Format.fprintf ppf "%S, %d lines, %s" s lines
        (if tail then "Tail" else "Head"))
    (Gen.triple utf8 (Gen.int_range 1 4) Gen.bool)

let lines_law (s, lines, tail) =
  let at = if tail then Text.Tail else Head in
  let _, part = Text.window ~lines ~bytes:100 at s in
  at_most int ~than:lines (List.length (Text.split_lines part))

let elisions =
  [
    ("at the bound, unchanged", (8, Fun.id, "abcdefgh"), "abcdefgh");
    ( "past the bound, half from each end",
      (8, Fun.id, "abcdefghij"),
      "abcd… (2 bytes elided)ghij" );
    ( "a cut inside a code point moves away from the middle",
      (6, Fun.id, "éééééé"),
      "é… (8 bytes elided)é" );
    ( "an odd bound rounds each half down",
      (7, Fun.id, "abcdefghij"),
      "abc… (4 bytes elided)hij" );
    ("a bound of 0 keeps no byte", (0, Fun.id, "ab"), "… (2 bytes elided)");
    ("the empty string under a bound of 0", (0, Fun.id, ""), "");
    ( "show prints each kept end, and the count is of the input",
      (8, String.escaped, "a\tbcdegh\ni"),
      "a\\tbc… (2 bytes elided)gh\\ni" );
    ("show prints a string that fits", (8, String.escaped, "a\tb"), "a\\tb");
  ]

let byte_truncations =
  [
    ("a bound of 0", (0, "abc"), "<truncated>");
    ("a negative bound", (-1, "abc"), "<truncated>");
    ("at the bound", (3, "abc"), "abc");
    ("past the bound", (4, "abcdefgh"), "abcd... (truncated; 8 bytes total)");
    ( "a cut inside a code point moves back",
      (3, "éé"),
      "é... (truncated; 4 bytes total)" );
    ( "an odd bound over two-byte code points",
      (5, "ééééé"),
      "éé... (truncated; 10 bytes total)" );
  ]

let lengths_and_cuts =
  group "Lengths and cuts"
    [
      cases "length_utf8 counts code points as String.get_utf_8_uchar decodes"
        ~name:name3 lengths (fun (_, s, n) -> equal int n (Text.length_utf8 s));
      cases
        "truncate_utf8 n s is s when it fits, else n - 3 code points then ..."
        ~name:name3 truncations (fun (_, (n, s), truncated) ->
          equal string truncated (Text.truncate_utf8 n s));
      prop "truncate_utf8 n s never has more than n code points"
        (Gen.pair (Gen.int_range 0 12) utf8)
        (fun (n, s) ->
          at_most int ~than:n (Text.length_utf8 (Text.truncate_utf8 n s)));
      cases "window keeps the part of a string at an end or around an offset"
        ~name:name3 windows (fun (_, window_of, kept) ->
          equal window kept (window_of ()));
      prop "window keeps a slice within the bound, cut on code points" anchored
        window_law;
      prop "a head or tail window moves its cut by three bytes at most" anchored
        longest_law;
      prop "a head or tail window holds at most its lines" line_bound lines_law;
      xfail ~reason:"window raises Invalid_argument for an anchor past the end"
        (test "window never raises, whatever the anchor" (fun () ->
             ignore (Text.window ~bytes:4 (Around 1_000) "abcdefghij")));
      xfail ~reason:"a tail window ignores a bound of 0 lines"
        (test "a tail window under a bound of 0 lines is empty" (fun () ->
             equal string ""
               (snd (Text.window ~lines:0 ~bytes:100 Tail "a\nb\n"))));
      test "mark_truncated appends the marker with the total length" (fun () ->
          equal string "ab... (truncated; 9 bytes total)"
            (Text.mark_truncated ~length:9 "ab"));
      cases
        "truncate_bytes_utf8 n s is s when it fits, else its marked head window"
        ~name:name3 byte_truncations (fun (_, (n, s), truncated) ->
          equal string truncated (Text.truncate_bytes_utf8 n s));
      cases
        "elide_middle keeps half of the bound from each end and counts the rest"
        ~name:name3 elisions (fun (_, (n, show, s), elided) ->
          equal string elided (Text.elide_middle n ~show s));
      test "elide_middle raises Invalid_argument for a negative bound"
        (fun () ->
          raises (Invalid_argument "Text.elide_middle: negative bound")
            (fun () -> Text.elide_middle (-1) ~show:Fun.id "a"));
    ]

(* Search *)

let occurrences =
  [
    ("in the middle", (None, "ell", "hello"), Some 1);
    ("at the start", (None, "he", "hello"), Some 0);
    ("the first of several", (None, "a", "banana"), Some 1);
    ("the empty pattern", (None, "", "hello"), Some 0);
    ("an absent pattern", (None, "z", "hello"), None);
    ("a pattern longer than the string", (None, "hello!", "hello"), None);
    ("past an earlier occurrence", (Some 2, "a", "banana"), Some 3);
    ("at start itself", (Some 3, "a", "banana"), Some 3);
    ("none left after start", (Some 6, "a", "banana"), None);
    ("the empty pattern at start", (Some 4, "", "hello"), Some 4);
    ("start at the end of the string", (Some 5, "o", "hello"), None);
  ]

let containments =
  [
    ("in the middle", ("ell", "hello"), true);
    ("at the start", ("he", "hello"), true);
    ("at the end", ("lo", "hello"), true);
    ("the empty pattern in the empty string", ("", ""), true);
    ("an absent pattern", ("z", "hello"), false);
    ("a pattern longer than the string", ("hello!", "hello"), false);
    ("after a partial match", ("aab", "aaab"), true);
  ]

let ab = Gen.string_of ~size:(Gen.int_range 0 12) (Gen.of_list [ 'a'; 'b' ])

let searched =
  let open Gen in
  with_pp
    (fun ppf (start, pattern, s) ->
      Format.fprintf ppf "%S in %S from %d" pattern s start)
    (let* pattern = string_of ~size:(int_range 0 3) (of_list [ 'a'; 'b' ]) in
     let* s = ab in
     let+ start = int_range 0 (String.length s) in
     (start, pattern, s))

let reference ~start ~pattern s =
  let n = String.length pattern in
  let rec scan i =
    if i + n > String.length s then None
    else if String.equal (String.sub s i n) pattern then Some i
    else scan (i + 1)
  in
  scan start

let search =
  group "Search"
    [
      cases "first_occurrence is the offset of the first occurrence from start"
        ~name:name3 occurrences (fun (_, (start, pattern, s), offset) ->
          equal (option int) offset (Text.first_occurrence ?start ~pattern s));
      prop "first_occurrence is the least offset from start where pattern is"
        searched (fun (start, pattern, s) ->
          equal (option int)
            (reference ~start ~pattern s)
            (Text.first_occurrence ~start ~pattern s));
      cases "first_occurrence raises Invalid_argument for a start outside s"
        ~name:fst
        [ ("a negative start", -1); ("a start past the end", 4) ]
        (fun (_, start) ->
          raises_match (Exn.invalid_arg ~substring:"start") (fun () ->
              Text.first_occurrence ~start ~pattern:"a" "abc"));
      cases "contains_substring tells whether pattern occurs in s" ~name:name3
        containments (fun (_, (pattern, s), found) ->
          equal bool found (Text.contains_substring ~pattern s));
      prop "contains_substring is true iff first_occurrence finds the pattern"
        (Gen.pair ab ab) (fun (pattern, s) ->
          equal bool
            (Option.is_some (Text.first_occurrence ~pattern s))
            (Text.contains_substring ~pattern s));
    ]

(* Control bytes *)

let escaped c =
  if (Char.code c < 0x20 && c <> '\t') || Char.code c = 0x7F then
    Printf.sprintf "\\x%02x" (Char.code c)
  else String.make 1 c

let every_byte () =
  let bytes = List.init 256 Char.chr in
  equal (list string) (List.map escaped bytes)
    (List.map (fun c -> Text.escape_controls (String.make 1 c)) bytes)

let controls =
  group "Control bytes"
    [
      test
        "escape_controls writes each byte below 0x20 but TAB, and DEL, as \
         \\xNN, and passes every other"
        every_byte;
      cases "escape_controls escapes each control byte of a string" ~name:name3
        [
          ("no control byte", "plain \t\xc3\xa9 \xff", "plain \t\xc3\xa9 \xff");
          ( "C0 bytes, LF and CR included, and DEL",
            "\x00a\nb\rc\027[31m\x1f\x7f",
            "\\x00a\\x0ab\\x0dc\\x1b[31m\\x1f\\x7f" );
        ]
        (fun (_, s, shown) -> equal string shown (Text.escape_controls s));
      test "escape_controls prints the four characters \\x1b as it prints ESC"
        (fun () ->
          equal string
            (Text.escape_controls "\027")
            (Text.escape_controls "\\x1b"));
    ]

let () = exit (run "text" [ newlines; lengths_and_cuts; search; controls ])
