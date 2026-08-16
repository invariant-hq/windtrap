(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Text utilities: newline canonicalization, UTF-8-aware truncation, ANSI
    stripping, substring search.

    All functions are pure and total. Truncation respects UTF-8 sequence
    boundaries so a multi-byte character is never split; it makes no
    grapheme-cluster or display-width claims. *)

(** {1:newlines Newlines} *)

val normalize_newlines : string -> string
(** [normalize_newlines s] is [s] with every line ending (CR, CRLF) replaced by
    LF. *)

val ensure_trailing_newline : string -> string
(** [ensure_trailing_newline s] is [s] with a final ["\n"] appended when [s]
    does not already end in one. [ensure_trailing_newline ""] is ["\n"]. *)

val split_lines : string -> string list
(** [split_lines s] is the lines of [s], split on LF. A single trailing newline
    terminates the last line rather than opening an empty one, so ["a\nb\n"] and
    ["a\nb"] are both [["a"; "b"]]; every other empty line is kept.
    [split_lines ""] is [[]]. *)

(** {1:utf8 UTF-8-aware operations} *)

val length_utf8 : string -> int
(** [length_utf8 s] is the number of UTF-8 code points in [s]. Malformed bytes
    count one code point per replacement, following {!String.get_utf_8_uchar}.
*)

val truncate_utf8 : int -> string -> string
(** [truncate_utf8 n s] is [s] when [s] holds at most [n] code points; otherwise
    it is the first [n - 3] code points of [s] followed by ["..."].

    The result never exceeds [n] code points — the ellipsis is inside the bound,
    not added to it — so a caller sizing a line to the terminal gets a line that
    fits. For [n <= 3] a too-long [s] truncates to the first [n] characters of
    ["..."]. Never splits a UTF-8 sequence. *)

val truncate_bytes_utf8 : int -> string -> string
(** [truncate_bytes_utf8 n s] is [s] when it is at most [n] bytes long;
    otherwise it is the longest prefix of [s] of at most [n] bytes that ends on
    a code-point boundary, followed by an explicit truncation marker stating the
    original byte count. It is ["<truncated>"] when [n <= 0]. The marker makes
    the result longer than [n] bytes; callers bounding storage should budget for
    it. *)

(** {1:search Search} *)

val first_occurrence : ?start:int -> pattern:string -> string -> int option
(** [first_occurrence ~pattern s] is the byte offset of the first occurrence of
    [pattern] in [s] as a byte substring at or after [start] (defaults to [0]),
    and [None] when [pattern] does not occur there. An empty [pattern] occurs
    at [start].

    Raises [Invalid_argument] if [start] is negative or past the end of [s]. A
    [start] equal to [String.length s] is in range and searches nothing. *)

val contains_substring : pattern:string -> string -> bool
(** [contains_substring ~pattern s] is [true] iff [s] contains [pattern] as a
    byte substring, i.e. iff {!first_occurrence} finds an occurrence. An empty
    [pattern] always matches. *)

val count_occurrences : pattern:string -> string -> int
(** [count_occurrences ~pattern s] is the number of occurrences of [pattern] in
    [s], counted leftmost-first and non-overlapping: each match resumes the
    scan at its end, so ["aa"] occurs once in ["aaa"] and twice in ["aaaa"].

    The empty pattern occurs at every byte position and at the end, so its
    count is [String.length s + 1] — the one reading under which an empty match
    advances the scan by a byte instead of never terminating. *)

(** {1:ansi ANSI escapes} *)

val strip_ansi : string -> string
(** [strip_ansi s] is [s] with ANSI escape sequences removed: CSI sequences (ESC
    and an opening bracket, up to and including the final byte), OSC sequences
    (ESC and a closing bracket, up to BEL or the ESC-backslash terminator), and
    other two-byte ESC escapes. A truncated sequence at the end of [s] is
    dropped. Used by renderers whose transport forbids escape codes (e.g. JUnit
    XML). *)
