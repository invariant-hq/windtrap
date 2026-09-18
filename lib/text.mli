(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Text utilities: newline canonicalization, UTF-8-aware truncation, ANSI
    stripping, substring search.

    All functions are pure and total. Truncation never splits a UTF-8 sequence
    and makes no grapheme-cluster or display-width claims. *)

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
    the first [n - 3] code points of [s] followed by ["..."], so the result
    never exceeds [n] code points. For [n <= 3] a too-long [s] truncates to the
    first [n] characters of ["..."]. *)

val truncate_bytes_utf8 : int -> string -> string
(** [truncate_bytes_utf8 n s] is [s] when it is at most [n] bytes long;
    otherwise the longest prefix of [s] of at most [n] bytes ending on a
    code-point boundary, followed by a truncation marker stating the original
    byte count, which makes the result longer than [n] bytes. It is
    ["<truncated>"] when [n <= 0]. *)

val elide_middle : int -> show:(string -> string) -> string -> string
(** [elide_middle n ~show s] is [show s] when [s] is at most [n] bytes long;
    otherwise [show] of the longest prefix and of the longest suffix of [s] of
    at most [n / 2] bytes each, both cut on code-point boundaries, around
    ["… (N bytes elided)"], [N] being the bytes of [s] left out. [show] is how
    the kept bytes print (an escaping, say): the cut and the count are made in
    [s], never in what [show] returns.

    Raises [Invalid_argument] if [n] is negative. *)

(** {1:search Search} *)

val first_occurrence : ?start:int -> pattern:string -> string -> int option
(** [first_occurrence ~pattern s] is the byte offset of the first occurrence of
    [pattern] in [s] at or after [start] (default [0]), or [None]. An empty
    [pattern] occurs at [start].

    Raises [Invalid_argument] if [start] is negative or past the end of [s];
    [String.length s] is in range and searches nothing. *)

val contains_substring : pattern:string -> string -> bool
(** [contains_substring ~pattern s] is [true] iff [s] contains [pattern] as a
    byte substring. An empty [pattern] always matches. *)

(** {1:ansi ANSI escapes} *)

val strip_ansi : string -> string
(** [strip_ansi s] is [s] with ANSI escape sequences removed: CSI sequences (ESC
    [ up to and including the final byte), OSC sequences (ESC ] up to BEL or
    ESC-backslash), and other two-byte ESC escapes. A truncated sequence at the
    end of [s] is dropped. *)
