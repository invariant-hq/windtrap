(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Strings read as text: lines, UTF-8 code points, byte search and control
    bytes.

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

    A length counts code points as [String.get_utf_8_uchar] decodes them. A cut
    never splits a well-formed UTF-8 sequence; on a malformed one it moves by at
    most three bytes. *)

val length_utf8 : string -> int
(** [length_utf8 s] is the number of UTF-8 code points of [s]. *)

val truncate_utf8 : int -> string -> string
(** [truncate_utf8 n s] is [s] when [s] has at most [n] code points, and
    otherwise the first [n - 3] code points of [s] followed by ["..."], so the
    result never has more than [n] code points. For [n <= 3] a longer [s] gives
    the first [n] bytes of ["..."]. It never raises. *)

(** The part of a string that a {!window} keeps. *)
type at =
  | Head  (** The start of the string. *)
  | Tail  (** The end of the string. *)
  | Around of int  (** The bytes around this offset, centred on it. *)

val window : ?lines:int -> bytes:int -> at -> string -> int * string
(** [window ~bytes at s] is [(offset, part)]: [part] is the longest part of [s]
    at [at] that holds at most [bytes] bytes, and [offset] is where it starts in
    [s]. It is [(0, s)] when [s] holds at most [bytes] bytes. With [lines], a
    [Head] or [Tail] part also holds at most that many lines, a final newline
    ending the last line; [lines] does not bound an [Around] part. A negative
    [bytes] is [0]. It never raises. *)

val mark_truncated : length:int -> string -> string
(** [mark_truncated ~length kept] is [kept] followed by the marker
    [... (truncated; N bytes total)], where [N] is [length]. *)

val truncate_bytes_utf8 : int -> string -> string
(** [truncate_bytes_utf8 n s] is [s] when [s] is at most [n] bytes long, and
    else the {!Head} {!window} of [n] bytes of [s], marked by {!mark_truncated}.
    The marker comes on top of the bound. For [n <= 0] it is ["<truncated>"]. It
    never raises. *)

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
