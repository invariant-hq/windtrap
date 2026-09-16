(*---------------------------------------------------------------------------
   Copyright (c) 2020-2021 Craig Ferguson
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Diff data between two texts: unified line hunks and character refinement
    spans.

    Data for renderers, never presentation: no styling, labels or display
    truncation. Renderers call {!hunks} on multi-line payloads (baseline
    contents, long renderings) and {!refine} on a pair of differing lines or
    short renderings. Both are pure; above internal size bounds the result
    degrades ({!hunks} to a whole-region replacement, {!refine} to [None]) but a
    difference is never reported as absent. The guards and the refinement noise
    cutoff are implementation constants, not contract. *)

(** {1:hunks Line hunks} *)

(** The type for one line of a hunk, stored without its terminating newline. *)
type line =
  | Keep of string  (** Present in both texts (context). *)
  | Delete of string  (** Present only in [expected]. *)
  | Insert of string  (** Present only in [actual]. *)

type hunk = {
  expected_start : int;
      (** 1-based line number in [expected] of the hunk's first expected-side
          line, or the line the insertion precedes when it has none. *)
  expected_count : int;
      (** Number of expected-side lines in the hunk ({!Keep} + {!Delete}). *)
  actual_start : int;  (** As {!expected_start}, for [actual]. *)
  actual_count : int;
      (** Number of actual-side lines in the hunk ({!Keep} + {!Insert}). *)
  lines : line list;
      (** The hunk's lines in text order. Within a run of changes, deletions
          precede insertions. *)
}
(** The type for unified-diff hunks: a changed region with up to [context]
    unchanged lines on each side. *)

val hunks :
  ?context:int -> expected:string -> actual:string -> unit -> hunk list
(** [hunks ~expected ~actual ()] is the changed regions between the two texts
    compared line by line, with [context] (default [3]) unchanged lines around
    each region; regions at most [2 * context] lines apart merge. Texts split on
    ['\n'] and a single trailing newline is not significant. [[]] iff both texts
    split into equal line lists. Above an internal size bound a region is
    reported as all deletions then all insertions rather than a minimal diff.

    Raises [Invalid_argument] if [context < 0]. *)

(** {1:refinement Character refinement} *)

type span = { start : int; length : int }
(** The type for byte ranges: [length] bytes at offset [start]. Spans from
    {!refine} begin and end on UTF-8 code-point boundaries. *)

type refinement = {
  expected_spans : span list;  (** Changed ranges of [expected]. *)
  actual_spans : span list;  (** Changed ranges of [actual]. *)
}
(** The type for refinement results. Span lists are ascending, non-overlapping
    and coalesced. Equal inputs have two empty lists. *)

val refine : expected:string -> actual:string -> refinement option
(** [refine ~expected ~actual] is the changed regions of the two strings,
    compared code point by code point with a minimal edit script. [None] when
    highlighting would not help and the renderer should show both strings plain:
    marking would cover half or more of a side's code points, or the differing
    region exceeds an internal size guard. Malformed UTF-8 is compared
    byte-faithfully, one replacement-sized unit at a time, following
    {!String.get_utf_8_uchar}. *)
