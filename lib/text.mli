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

(** {1:utf8 UTF-8-aware operations} *)

val length_utf8 : string -> int
(** [length_utf8 s] is the number of UTF-8 code points in [s]. Malformed bytes
    count one code point per replacement, following {!String.get_utf_8_uchar}.
*)

val truncate_utf8 : int -> string -> string
(** [truncate_utf8 n s] is [s] when [s] holds at most [n] code points; otherwise
    it is the first [n - 1] code points of [s] followed by ["..."]. When
    [n <= 1] that prefix is empty, so a too-long [s] truncates to ["..."] alone.
    Never splits a UTF-8 sequence. *)

val truncate_bytes_utf8 : int -> string -> string
(** [truncate_bytes_utf8 n s] is [s] when it is at most [n] bytes long;
    otherwise it is the longest prefix of [s] of at most [n] bytes that ends on
    a code-point boundary, followed by an explicit truncation marker stating the
    original byte count. It is ["<truncated>"] when [n <= 0]. The marker makes
    the result longer than [n] bytes; callers bounding storage should budget for
    it. *)

(** {1:search Search} *)

val contains_substring : pattern:string -> string -> bool
(** [contains_substring ~pattern s] is [true] iff [s] contains [pattern] as a
    byte substring. An empty [pattern] always matches. *)

(** {1:ansi ANSI escapes} *)

val strip_ansi : string -> string
(** [strip_ansi s] is [s] with ANSI escape sequences removed: CSI sequences (ESC
    and an opening bracket, up to and including the final byte), OSC sequences
    (ESC and a closing bracket, up to BEL or the ESC-backslash terminator), and
    other two-byte ESC escapes. A truncated sequence at the end of [s] is
    dropped. Used by renderers whose transport forbids escape codes (e.g. JUnit
    XML). *)
