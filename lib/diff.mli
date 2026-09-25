(*---------------------------------------------------------------------------
   Copyright (c) 2020-2021 Craig Ferguson
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The difference between two texts, as data for a renderer.

    {!val:hunks} is the difference of two texts line by line, as unified hunks,
    and {!refine} is the changed byte ranges of two strings. Both return data
    without styling, labels or a cut for display, which are a renderer's.

    Both functions are pure. Past a size bound {!val:hunks} gives a region whole
    and {!refine} is [None]. Both results show the degradation, so a bound never
    makes two different inputs look equal. *)

(** {1:hunks Line hunks} *)

(** The type for the lines of a hunk. A line is stored without the LF that ended
    it ({!Text.split_lines}). *)
type line =
  | Keep of string  (** A line of both texts, which is context. *)
  | Delete of string  (** A line of [expected] only. *)
  | Insert of string  (** A line of [actual] only. *)

type hunk = {
  expected_start : int;
      (** The 1-based number in [expected] of the first [Keep] or [Delete] line
          of the hunk. When the hunk has none, it is the number of the next line
          of [expected], which is one more than the unified format gives an
          empty side. A renderer of [@@] heads must then subtract [1]. *)
  expected_count : int;  (** The number of [Keep] and [Delete] lines. *)
  actual_start : int;
      (** As [expected_start], in [actual] and for [Keep] and [Insert] lines. *)
  actual_count : int;  (** The number of [Keep] and [Insert] lines. *)
  lines : line list;
      (** The lines in the order of the texts. Within a run of changes the
          [Delete] lines come before the [Insert] lines. *)
}
(** The type for unified hunks. A hunk is a changed region with up to [context]
    unchanged lines before it and after it.

    The [Keep] and [Delete] lines of a hunk, in order, are the lines
    [expected_start] to [expected_start + expected_count - 1] of [expected]. Its
    [Keep] and [Insert] lines are the same range of [actual]. *)

val hunks :
  ?context:int -> expected:string -> actual:string -> unit -> hunk list
(** [hunks ?context ~expected ~actual ()] is the changed regions between
    [expected] and [actual], in the order of the texts and without overlap.
    {!Text.split_lines} splits the texts, and two lines are equal when their
    bytes are, trailing blanks included.

    [context] is the number of unchanged lines kept on each side of a region,
    and defaults to [3]. Two regions with at most [2 * context] unchanged lines
    between them are one hunk.

    The result is [[]] iff the two texts split into equal lists of lines. The
    split drops a single trailing newline, so ["a"] and ["a\n"] give [[]], and
    [[]] does not prove that the strings are equal. A caller to whom that
    newline matters must compare the strings itself, or pass texts in a
    canonical or an encoded form.

    The differing region lies between the common first lines and the common last
    lines of the two texts. When it holds more than 2000 lines, both sides
    summed ([myers_line_limit]), or needs more than 1000 edits
    ([myers_max_edits]), the result is one hunk. It gives every [expected] line
    of the region as a [Delete], then every [actual] line as an [Insert], around
    the usual context. That hunk omits no line and its size has no bound, so a
    renderer must bound what it shows. Below the two bounds the difference is
    minimal, and which minimal one is returned is unspecified.

    Raises [Invalid_argument] if [context] is negative. *)

(** {1:refinement Character refinement} *)

type span = { start : int; length : int }
(** The type for byte ranges: [length] bytes from the byte offset [start] of the
    string given to {!refine}. A span of {!refine} begins and ends on the
    boundary of a UTF-8 code point. *)

type refinement = {
  expected_spans : span list;  (** The changed ranges of [expected]. *)
  actual_spans : span list;  (** The changed ranges of [actual]. *)
}
(** The type for the results of {!refine}. Each list is ascending, without
    overlap and coalesced, which means that adjacent changed code points form
    one span.

    [expected] without its spans and [actual] without its spans are the same
    string, so a renderer can mark each side alone. *)

val refine : expected:string -> actual:string -> refinement option
(** [refine ~expected ~actual] is the changed ranges of the two strings,
    compared code point by code point under a minimal script of insertions,
    deletions and substitutions. Equal strings give [Some] of two empty lists.

    The result is [None] when marks would not help a reader. It has two causes:
    - The marks would cover half or more of the code points of a side
      ([noise_threshold]). The share is taken per side, over the whole string.
      ["13"] against ["14"] is thus [None].
    - The differing region, between the common first and the common last code
      points, holds [ma] code points of [expected] and [mb] of [actual], and
      [(ma + 1) * (mb + 1)] is above 4000000 ([dp_cell_limit]).

    Malformed UTF-8 is compared byte for byte, one unit at a time as
    [String.get_utf_8_uchar] decodes it. *)
