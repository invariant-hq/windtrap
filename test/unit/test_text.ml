(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Text = Windtrap.Private.Text

let tests =
  [
    test "normalize_newlines maps every ending to LF" (fun () ->
        equal ~msg:"LF-only text unchanged" string "a\nb\n"
          (Text.normalize_newlines "a\nb\n");
        equal ~msg:"CRLF becomes LF" string "a\nb\n"
          (Text.normalize_newlines "a\r\nb\r\n");
        equal ~msg:"lone CR becomes LF" string "a\nb"
          (Text.normalize_newlines "a\rb");
        equal ~msg:"mixed endings" string "a\nb\nc\nd"
          (Text.normalize_newlines "a\r\nb\rc\nd");
        equal ~msg:"CR at end of string" string "a\n"
          (Text.normalize_newlines "a\r");
        equal ~msg:"empty string" string "" (Text.normalize_newlines ""));
    test "ensure_trailing_newline" (fun () ->
        equal ~msg:"appends missing newline" string "a\n"
          (Text.ensure_trailing_newline "a");
        equal ~msg:"keeps existing newline" string "a\n"
          (Text.ensure_trailing_newline "a\n");
        equal ~msg:"empty becomes newline" string "\n"
          (Text.ensure_trailing_newline ""));
    test "split_lines: a single trailing newline is not a line" (fun () ->
        equal ~msg:"trailing newline dropped" (list string) [ "a"; "b" ]
          (Text.split_lines "a\nb\n");
        equal ~msg:"no trailing newline" (list string) [ "a"; "b" ]
          (Text.split_lines "a\nb");
        equal ~msg:"only one trailing empty line is dropped" (list string)
          [ "a"; "" ] (Text.split_lines "a\n\n");
        equal ~msg:"interior empty lines kept" (list string) [ "a"; ""; "b" ]
          (Text.split_lines "a\n\nb");
        equal ~msg:"empty string has no lines" (list string) []
          (Text.split_lines ""));
    test "split_lines keeps the CR of a CRLF line" (fun () ->
        equal (list string) [ "a\r"; "b\r" ] (Text.split_lines "a\r\nb\r\n"));
    test "length_utf8 counts characters, not bytes" (fun () ->
        equal ~msg:"ascii length" int 5 (Text.length_utf8 "hello");
        equal ~msg:"two-byte chars" int 5 (Text.length_utf8 "héllo");
        equal ~msg:"four-byte emoji counts once" int 1
          (Text.length_utf8 "\240\159\144\171");
        equal ~msg:"empty length" int 0 (Text.length_utf8 ""));
    (* No grapheme knowledge: a letter and its combining accent are two code
       points, and a wide character is one. *)
    test "length_utf8 counts code points, not graphemes or columns" (fun () ->
        equal ~msg:"e and a combining acute" int 2
          (Text.length_utf8 "e\u{0301}");
        equal ~msg:"a wide character" int 1 (Text.length_utf8 "\u{6f22}"));
    test "a malformed sequence counts one code point per replacement" (fun () ->
        equal ~msg:"two stray bytes are two" int 2 (Text.length_utf8 "\xff\xfe");
        equal ~msg:"a cut-short sequence is one" int 1
          (Text.length_utf8 "\xe2\x82");
        equal ~msg:"a cut keeps the replacement's bytes whole" string
          "\xe2\x82a..."
          (Text.truncate_utf8 5 "\xe2\x82abcdef"));
    test "truncate_utf8 keeps whole characters" (fun () ->
        equal ~msg:"short string unchanged" string "abc"
          (Text.truncate_utf8 5 "abc");
        equal ~msg:"exact length unchanged" string "abcde"
          (Text.truncate_utf8 5 "abcde");
        (* The ellipsis is inside the bound: a display budget that the
           result can exceed is not a budget, and the live tail sized to
           the terminal wrapped because of it. *)
        equal ~msg:"long ascii truncated with ellipsis" string "ab..."
          (Text.truncate_utf8 5 "abcdefgh");
        equal ~msg:"the result never exceeds the budget" int 5
          (Text.length_utf8 (Text.truncate_utf8 5 "abcdefgh"));
        equal ~msg:"multibyte within char budget unchanged" string "éé"
          (Text.truncate_utf8 3 "éé");
        equal ~msg:"multibyte truncation keeps whole chars" string "..."
          (Text.truncate_utf8 3 "ééééé");
        equal ~msg:"multibyte truncation within a wider budget" string "éé..."
          (Text.truncate_utf8 5 "éééééé");
        equal ~msg:"budget of one keeps one marker char" string "."
          (Text.truncate_utf8 1 "abc");
        equal ~msg:"budget of zero keeps nothing" string ""
          (Text.truncate_utf8 0 "abc");
        equal ~msg:"empty string fits any budget" string ""
          (Text.truncate_utf8 0 ""));
    test "truncate_utf8 under 3 gives a prefix of the ellipsis" (fun () ->
        equal ~msg:"budget of two" string ".." (Text.truncate_utf8 2 "abc");
        equal ~msg:"a negative budget gives nothing and never raises" string ""
          (Text.truncate_utf8 (-4) "abc"));
    test "elide_middle keeps both ends and counts what it left out" (fun () ->
        equal ~msg:"at the bound: unchanged" string "abcdefgh"
          (Text.elide_middle 8 ~show:Fun.id "abcdefgh");
        equal ~msg:"over the bound: half from each end" string
          "abcd\u{2026} (2 bytes elided)ghij"
          (Text.elide_middle 8 ~show:Fun.id "abcdefghij");
        (* Six two-byte characters under a bound of 6: each half is 3 bytes,
           and a cut inside a character moves away from the middle. *)
        equal ~msg:"cuts land on code points, short of the half" string
          "\u{00e9}\u{2026} (8 bytes elided)\u{00e9}"
          (Text.elide_middle 6 ~show:Fun.id
             "\u{00e9}\u{00e9}\u{00e9}\u{00e9}\u{00e9}\u{00e9}");
        equal ~msg:"an odd bound rounds each half down" string
          "abc\u{2026} (4 bytes elided)hij"
          (Text.elide_middle 7 ~show:Fun.id "abcdefghij");
        (* [show] prints what is kept and never what is counted: the cut and
           the count are made in the carried bytes. *)
        equal ~msg:"show applies to each kept end, the count is of the input"
          string "a\\tbc\u{2026} (2 bytes elided)gh\\ni"
          (Text.elide_middle 8 ~show:String.escaped "a\tbcdegh\ni");
        equal ~msg:"show applies to a value that fits" string "a\\tb"
          (Text.elide_middle 8 ~show:String.escaped "a\tb");
        raises (Invalid_argument "Text.elide_middle: negative bound") (fun () ->
            Text.elide_middle (-1) ~show:Fun.id "a"));
    test "truncate_bytes_utf8 never splits a character" (fun () ->
        equal ~msg:"non-positive budget" string "<truncated>"
          (Text.truncate_bytes_utf8 0 "abc");
        equal ~msg:"negative budget" string "<truncated>"
          (Text.truncate_bytes_utf8 (-1) "abc");
        equal ~msg:"fits in budget unchanged" string "abc"
          (Text.truncate_bytes_utf8 3 "abc");
        equal ~msg:"byte truncation notes total" string
          "abcd... (truncated; 8 bytes total)"
          (Text.truncate_bytes_utf8 4 "abcdefgh");
        (* "éé" is 4 bytes; a 3-byte budget must back up to the 2-byte
           boundary rather than split the second é. *)
        equal ~msg:"never splits a multibyte char" string
          "\195\169... (truncated; 4 bytes total)"
          (Text.truncate_bytes_utf8 3 "éé");
        (* An odd budget over two-byte characters: the prefix backs up to
           the last whole character, never ending half-way through one. *)
        equal ~msg:"truncated prefix ends on a char boundary" string
          "éé... (truncated; 10 bytes total)"
          (Text.truncate_bytes_utf8 5 "ééééé"));
    test "prefix_bytes_utf8 keeps the prefix and no marker" (fun () ->
        equal ~msg:"fits: unchanged" string "abc"
          (Text.prefix_bytes_utf8 3 "abc");
        equal ~msg:"cut" string "abcd" (Text.prefix_bytes_utf8 4 "abcdefgh");
        equal ~msg:"never splits a multibyte char" string "\195\169"
          (Text.prefix_bytes_utf8 3 "éé");
        equal ~msg:"non-positive budget" string ""
          (Text.prefix_bytes_utf8 0 "abc");
        equal ~msg:"the marker is spelled apart" string
          "ab... (truncated; 9 bytes total)"
          (Text.mark_truncated ~length:9 "ab"));
    test "first_occurrence returns the byte offset" (fun () ->
        equal ~msg:"match in the middle" (option int) (Some 1)
          (Text.first_occurrence ~pattern:"ell" "hello");
        equal ~msg:"match at the start" (option int) (Some 0)
          (Text.first_occurrence ~pattern:"he" "hello");
        equal ~msg:"first of several occurrences" (option int) (Some 1)
          (Text.first_occurrence ~pattern:"a" "banana");
        equal ~msg:"empty pattern occurs at zero" (option int) (Some 0)
          (Text.first_occurrence ~pattern:"" "hello");
        equal ~msg:"absent pattern" (option int) None
          (Text.first_occurrence ~pattern:"z" "hello");
        equal ~msg:"pattern longer than the string" (option int) None
          (Text.first_occurrence ~pattern:"hello!" "hello"));
    test "first_occurrence searches from ~start" (fun () ->
        equal ~msg:"skips an earlier occurrence" (option int) (Some 3)
          (Text.first_occurrence ~start:2 ~pattern:"a" "banana");
        equal ~msg:"a match at ~start itself counts" (option int) (Some 3)
          (Text.first_occurrence ~start:3 ~pattern:"a" "banana");
        equal ~msg:"no occurrence left" (option int) None
          (Text.first_occurrence ~start:6 ~pattern:"a" "banana");
        equal ~msg:"the empty pattern occurs at ~start" (option int) (Some 4)
          (Text.first_occurrence ~start:4 ~pattern:"" "hello");
        equal ~msg:"~start at the end is in range" (option int) None
          (Text.first_occurrence ~start:5 ~pattern:"o" "hello");
        raises_match ~msg:"a negative ~start is a programmer error"
          (Exn.invalid_arg ~substring:"start") (fun () ->
            Text.first_occurrence ~start:(-1) ~pattern:"a" "abc");
        raises_match ~msg:"a ~start past the end is a programmer error"
          (Exn.invalid_arg ~substring:"start") (fun () ->
            Text.first_occurrence ~start:4 ~pattern:"a" "abc"));
    test "contains_substring" (fun () ->
        is_true ~msg:"finds substring in middle"
          (Text.contains_substring ~pattern:"ell" "hello");
        is_true ~msg:"finds substring at start"
          (Text.contains_substring ~pattern:"he" "hello");
        is_true ~msg:"finds substring at end"
          (Text.contains_substring ~pattern:"lo" "hello");
        is_true ~msg:"empty pattern always matches"
          (Text.contains_substring ~pattern:"" "");
        is_false ~msg:"absent substring"
          (Text.contains_substring ~pattern:"z" "hello");
        is_false ~msg:"pattern longer than string"
          (Text.contains_substring ~pattern:"hello!" "hello");
        is_true ~msg:"repeated prefix backtracking"
          (Text.contains_substring ~pattern:"aab" "aaab"));
    test "escape_controls spells every control byte but TAB" (fun () ->
        equal ~msg:"plain text unchanged" string "plain \t\xc3\xa9 \xff"
          (Text.escape_controls "plain \t\xc3\xa9 \xff");
        equal ~msg:"C0 bytes, LF and CR included, and DEL" string
          "\\x00a\\x0ab\\x0dc\\x1b[31m\\x1f\\x7f"
          (Text.escape_controls "\x00a\nb\rc\027[31m\x1f\x7f"));
  ]

let () = exit @@ Windtrap.run "text" tests
