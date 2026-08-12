(*---------------------------------------------------------------------------
   Copyright (c) 2020-2021 Craig Ferguson
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Diff data between two texts: unified line hunks and character refinement
    spans.

    [Diff] computes {e data} for renderers, never presentation: no styling, no
    labels, no display truncation — those exist only in renderers. Renderers
    call {!hunks} on multi-line payloads (snapshot contents, long pp renderings)
    and {!refine} on a pair of differing lines or short renderings to obtain the
    changed regions to highlight.

    Both functions are pure and guarded: above internal size bounds the result
    degrades — {!hunks} to a whole-region replacement, {!refine} to [None] — but
    a difference is never silently reported as absent. The guards and the
    refinement noise cutoff are implementation constants, not contract. *)

(** {1:hunks Line hunks} *)

(** The type for one line of a hunk. Lines are stored without their terminating
    newline. *)
type line =
  | Keep of string  (** Present in both texts (context). *)
  | Delete of string  (** Present only in [expected]. *)
  | Insert of string  (** Present only in [actual]. *)

type hunk = {
  expected_start : int;
      (** 1-based line number in [expected] of the hunk's first expected-side
          line. When the hunk has no expected-side lines, the line number the
          insertion precedes. *)
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
(** [hunks ~expected ~actual ()] is the list of changed regions between the two
    texts, compared line by line, with [context] unchanged lines (default [3])
    retained around each region; regions separated by at most [2 * context]
    unchanged lines merge into one hunk.

    Texts split on ['\n']; a single trailing newline is not significant (["a"]
    and ["a\n"] split identically). Callers for whom it is significant compare
    canonicalized or encoded text — snapshots force a trailing newline, string
    witnesses render with [%S].

    [hunks] is [[]] iff both texts split into equal line lists. Above an
    internal size bound on the differing region, the region's lines are reported
    as all deletions followed by all insertions instead of a minimal diff —
    complete, never omitted; renderers bound the display.

    Raises [Invalid_argument] if [context < 0]. *)

(** {1:refinement Character refinement} *)

type span = { start : int; length : int }
(** The type for byte ranges: [length] bytes starting at offset [start]. Spans
    produced by {!refine} always begin and end on UTF-8 code-point boundaries —
    a multi-byte character is never split. *)

type refinement = {
  expected_spans : span list;  (** Changed ranges of [expected]. *)
  actual_spans : span list;  (** Changed ranges of [actual]. *)
}
(** The type for refinement results. Span lists are ascending, non-overlapping,
    and coalesced: adjacent changed characters form one span. Equal inputs have
    two empty lists. *)

val refine : expected:string -> actual:string -> refinement option
(** [refine ~expected ~actual] is the changed regions of the two strings,
    compared code point by code point with a minimal edit script — the highlight
    data under a renderer's [~~~] markers.

    [None] means highlighting would not help and the renderer should show the
    two strings plain: marking would cover half or more of a side's code points,
    or the differing region exceeds an internal size guard. The coverage rule is
    what a highlight is for — pointing at a small part of a mostly shared value.
    Two values that merely differ get no marks, since tildes scattered over them
    draw the eye to coincidental alignments rather than to a change. Malformed
    UTF-8 is compared byte-faithfully, one replacement-sized unit at a time,
    following {!String.get_utf_8_uchar}. *)
