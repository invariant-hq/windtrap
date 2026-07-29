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
    test "length_utf8 counts characters, not bytes" (fun () ->
        equal ~msg:"ascii length" int 5 (Text.length_utf8 "hello");
        equal ~msg:"two-byte chars" int 5 (Text.length_utf8 "héllo");
        equal ~msg:"four-byte emoji counts once" int 1
          (Text.length_utf8 "\240\159\144\171");
        equal ~msg:"empty length" int 0 (Text.length_utf8 ""));
    test "truncate_utf8 keeps whole characters" (fun () ->
        equal ~msg:"short string unchanged" string "abc"
          (Text.truncate_utf8 5 "abc");
        equal ~msg:"exact length unchanged" string "abcde"
          (Text.truncate_utf8 5 "abcde");
        equal ~msg:"long ascii truncated with ellipsis" string "abcd..."
          (Text.truncate_utf8 5 "abcdefgh");
        equal ~msg:"multibyte within char budget unchanged" string "éé"
          (Text.truncate_utf8 3 "éé");
        equal ~msg:"multibyte truncation keeps whole chars" string "éé..."
          (Text.truncate_utf8 3 "ééééé");
        equal ~msg:"budget of one keeps only the marker" string "..."
          (Text.truncate_utf8 1 "abc");
        equal ~msg:"budget of zero keeps only the marker" string "..."
          (Text.truncate_utf8 0 "abc");
        equal ~msg:"empty string fits any budget" string ""
          (Text.truncate_utf8 0 ""));
    test "truncate_bytes_utf8 never splits a character" (fun () ->
        equal ~msg:"non-positive budget" string "<truncated>"
          (Text.truncate_bytes_utf8 0 "abc");
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
        is_true ~msg:"truncated prefix ends on a char boundary"
          (let out = Text.truncate_bytes_utf8 5 "ééééé" in
           String.length out > 0 && Char.code out.[0] land 0xC0 <> 0x80));
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
    test "strip_ansi removes escapes and keeps text" (fun () ->
        equal ~msg:"plain text unchanged" string "plain"
          (Text.strip_ansi "plain");
        equal ~msg:"strips color sequence" string "redtext"
          (Text.strip_ansi "\027[31mred\027[0mtext");
        equal ~msg:"strips multi-parameter csi" string "x"
          (Text.strip_ansi "\027[1;32;44mx");
        equal ~msg:"strips private-mode csi" string "x"
          (Text.strip_ansi "\027[?25lx");
        equal ~msg:"strips cursor movement" string "ab"
          (Text.strip_ansi "a\027[2Kb");
        equal ~msg:"strips osc terminated by bel" string "ab"
          (Text.strip_ansi "a\027]0;title\007b");
        equal ~msg:"strips osc terminated by st" string "ab"
          (Text.strip_ansi "a\027]8;;http://x\027\\b");
        equal ~msg:"strips two-byte escape" string "ab"
          (Text.strip_ansi "a\027cb");
        equal ~msg:"drops truncated escape at end" string "a"
          (Text.strip_ansi "a\027[31");
        equal ~msg:"drops trailing lone esc" string "a"
          (Text.strip_ansi "a\027");
        equal ~msg:"keeps newlines and text intact" string "a\nb"
          (Text.strip_ansi "\027[1ma\n\027[31mb\027[0m"));
  ]
