(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Strings read as text: lines, UTF-8 code points, byte search, control bytes
    and ANSI escape sequences.

    Every function is pure, and {!elide_middle} is as pure as the [show] that it
    is given. Lengths are in code points. No function knows grapheme clusters or
    display widths, so a combining sequence or a wide character occupies a
    number of columns on a screen that {!length_utf8} does not give. *)

(** {1:newlines Newlines} *)

val normalize_newlines : string -> string
(** [normalize_newlines s] is [s] with each CRLF pair, and each other CR,
    replaced by one LF. *)

val ensure_trailing_newline : string -> string
(** [ensure_trailing_newline s] is [s] when [s] ends with LF, and [s ^ "\n"]
    otherwise, so [""] becomes ["\n"]. *)

val split_lines : string -> string list
(** [split_lines s] is the lines of [s], split at each LF. A final LF ends the
    last line and opens no empty one, so ["a\nb\n"] and ["a\nb"] both give
    [["a"; "b"]]. Every other empty line is kept, and [""] gives [[]]. CR is no
    separator, so a line of a CRLF text keeps its CR. *)

(** {1:utf8 Lengths and cuts}

    No cut splits a well-formed UTF-8 sequence. A malformed sequence is read as
    [String.get_utf_8_uchar] decodes it, one code point per replacement
    character. *)

val length_utf8 : string -> int
(** [length_utf8 s] is the number of UTF-8 code points of [s]. *)

val truncate_utf8 : int -> string -> string
(** [truncate_utf8 n s] is [s] when [s] has at most [n] code points, and
    otherwise the first [n - 3] code points of [s] followed by ["..."]. The
    ellipsis is inside the bound, so the result never has more than [n] code
    points, unlike that of {!truncate_bytes_utf8}. For [n <= 3] a longer [s]
    gives the first [n] bytes of ["..."]. It never raises. *)

val truncate_bytes_utf8 : int -> string -> string
(** [truncate_bytes_utf8 n s] is ["<truncated>"] when [n <= 0]. It is otherwise
    [s] when [s] is at most [n] bytes long, and else a prefix of [s] followed by
    a marker that gives the length of [s] in bytes. The prefix is the longest of
    at most [n] bytes that ends on a code-point boundary.

    The marker comes on top of the bound, so the result is longer than [n]
    bytes. A caller that bounds storage must budget for it. The marker is
    [... (truncated; N bytes total)]. It never raises. *)

val elide_middle : int -> show:(string -> string) -> string -> string
(** [elide_middle n ~show s] is [show s] when [s] is at most [n] bytes long.
    Otherwise it is [show] of a prefix of [s], a marker that gives the number of
    bytes of [s] left out, and [show] of a suffix of [s]. The prefix and the
    suffix are each the longest of at most [n / 2] bytes that is cut on a
    code-point boundary. The marker is [… (N bytes elided)].

    [show] is how the kept bytes print, an escaping for example. The cut and the
    count are made in [s], never in what [show] returns. Raises
    [Invalid_argument] if [n] is negative. *)

(** {1:search Search} *)

val first_occurrence : ?start:int -> pattern:string -> string -> int option
(** [first_occurrence ?start ~pattern s] is the byte offset of the first
    occurrence of [pattern] in [s] at or after byte [start], or [None] when
    there is none. [start] defaults to [0]. Bytes are compared as they are,
    without case folding or normalization, and an empty [pattern] occurs at
    [start]. Raises [Invalid_argument] if [start] is negative or greater than
    [String.length s]. *)

val contains_substring : pattern:string -> string -> bool
(** [contains_substring ~pattern s] is [true] iff [first_occurrence ~pattern s]
    is not [None]. *)

(** {1:controls Control bytes} *)

val escape_controls : string -> string
(** [escape_controls s] is [s] with each byte below [0x20] but TAB, and DEL,
    written as [\xNN] in lowercase hexadecimal. LF is escaped too, so the result
    is one line. Every other byte passes. It is not injective: the four
    characters [\x1b] print as ESC does. *)

(** {1:ansi ANSI escape sequences} *)

val strip_ansi : string -> string
(** [strip_ansi s] is [s] without its ANSI escape sequences. It removes three
    forms:
    - A CSI sequence, from ESC [\[] up to and including the first byte in the
      range [0x40] to [0x7e].
    - An OSC sequence, from ESC [\]] up to and including BEL, or ESC and a
      backslash.
    - Any other ESC, with the byte that follows it.

    A sequence that the end of [s] cuts short is removed too. Only ESC opens a
    sequence, so the 8-bit CSI byte [0x9b] and every other control byte stay. It
    never raises. *)
