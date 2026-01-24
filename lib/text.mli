(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Text and string operations with UTF-8 support. *)

val normalize_newlines : string -> string
(** [normalize_newlines s] converts all line endings (CR, CRLF) to LF. *)

val length_utf8 : string -> int
(** [length_utf8 s] returns the number of UTF-8 codepoints in [s]. *)

val truncate_utf8 : int -> string -> string
(** [truncate_utf8 n s] truncates [s] to at most [n - 1] UTF-8 characters and
    appends ["..."]. Returns [s] unchanged when it is already short enough. *)

val truncate_bytes_utf8 : int -> string -> string
(** [truncate_bytes_utf8 n s] truncates [s] to at most [n] bytes, respecting
    UTF-8 character boundaries. Backs up to the last complete character if [n]
    falls in the middle of a multi-byte sequence. Appends a note with the total
    byte count if truncated. Returns ["<truncated>"] when [n <= 0]. *)

val contains_substring : pattern:string -> string -> bool
(** [contains_substring ~pattern s] returns [true] if [s] contains [pattern] as
    a substring. An empty [pattern] always matches. *)
