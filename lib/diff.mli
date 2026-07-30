(*---------------------------------------------------------------------------
   Copyright (c) 2020-2021 Craig Ferguson
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Diff data between two texts: unified line hunks and character refinement
    spans.

    [Diff] computes {e data} for renderers, never presentation: no styling, no
    labels, no display truncation (RFC Law 4 — those exist only in renderers).
    Renderers call {!hunks} on multi-line payloads (snapshot contents, long pp
    renderings) and {!refine} on a pair of differing lines or short renderings
    to obtain the changed regions to highlight.

    Both functions are pure and guarded: above internal size bounds the result
    degrades — {!hunks} to a whole-region replacement, {!refine} to [None] — but
    a difference is never silently reported as absent. The guards and the
    refinement noise cutoff are implementation constants (RFC Unresolved
    questions), not contract.

    {!sequences} is the third grain (amendment B7): when both renderings parse
    as OCaml-style list or array renderings — the output of the [Testable]
    container printers — it compares them element by element, so renderers can
    state the first differing index and the mismatch count instead of leaving a
    hundred-element diff to speak for itself. *)

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
    a multi-byte character is never split (RFC "Snapshots" canonicalization). *)

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
    two strings plain: the strings differ in more than a noise cutoff (currently
    80%) of their code points, or their differing region exceeds an internal
    size guard. Malformed UTF-8 is compared byte-faithfully, one
    replacement-sized unit at a time, following {!String.get_utf_8_uchar}. *)

(** {1:sequences Sequence elements}

    Element-grain comparison of two {e rendered} sequences (amendment B7). The
    inputs are the payload strings of an equality failure, not the original
    values: parsing recognizes the source-like renderings the [Testable] [list],
    [array], and [slist] printers produce — [[e1; e2; …]] and [[|e1; e2; …|]] —
    including the line breaks their compacting boxes insert in long renderings.
    Anything else — and any rendering this conservative parser cannot account
    for, such as unbalanced brackets inside a custom printer's output — yields
    [None], never a wrong element count. *)

type mismatch = {
  index : int;
      (** Zero-based position of the element in the side(s) that carry it. *)
  expected : string option;
      (** The expected-side element, canonically rendered; [None] when the
          element exists only on the actual side (an insertion). *)
  actual : string option;
      (** The actual-side element, canonically rendered; [None] when the element
          exists only on the expected side (a deletion). *)
}
(** The type for the first differing element under the alignment. Both sides
    present is a replacement; one side [None] is an element without a
    counterpart. Element strings are canonical: whitespace runs outside string
    and character literals collapse to a single space, so an element compares
    and prints the same wherever the rendering's line breaks fell. *)

type seq_diff = {
  kind : [ `List | `Array ];
      (** The bracket form both renderings used ([[…]] or [[|…|]]). *)
  expected_length : int;  (** Element count of the [expected] rendering. *)
  actual_length : int;  (** Element count of the [actual] rendering. *)
  differing : int;
      (** Differing elements under the element-grain alignment (the line-diff
          algorithm over canonical elements): each replaced pair counts once,
          and each element present on only one side counts once. Alignment, not
          position — a single inserted or removed element is one difference, not
          a shifted disagreement at every later index. *)
  first : mismatch option;
      (** The first non-aligned element; [None] iff [differing] is [0] (the
          sequences are equal or differ only in layout). *)
}
(** The type for element-grain comparisons of two rendered sequences. *)

val sequences : expected:string -> actual:string -> seq_diff option
(** [sequences ~expected ~actual] is the element-by-element comparison of the
    two renderings, or [None] when they do not both parse as sequence renderings
    of the same {!seq_diff.kind}.

    Elements are compared as canonical strings (see {!mismatch}), which follows
    the printed values, not the witness's equality — exactly what a reader of
    the failure sees. A lossy element printer can therefore leave [differing] at
    [0] for unequal values; renderers fall back to their identical-rendering
    explanation in that case. *)
