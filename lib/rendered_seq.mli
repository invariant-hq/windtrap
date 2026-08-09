(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A reader from a printed OCaml sequence back to its elements.

    An assertion witness is a printer and an equality, nothing more — the
    deliberate narrowness described in [doc/dev/architecture.md] under "Two
    second-order waists". So a renderer that wants to say "the two lists differ
    at index 3" has only the {e printed} string to work from: the elements it
    needs were flattened into text before it ever saw them. This module reads
    them back, each with the byte range it occupies in the rendering it came
    from, so the caller can both compare elements and mark them.

    It is a single conservative scan, not an OCaml parser: it accepts the
    renderings the [Testable] container printers produce and {e declines}
    everything else, including renderings that merely look close. Declining is
    the whole safety property — see {!section:declining}. *)

(** {1:grammar The grammar} *)

type kind = [ `List | `Array ]
(** The type for the bracket form of a rendering: [`List] for [[e1; e2; …]],
    [`Array] for [[|e1; e2; …|]]. *)

(** Outer whitespace aside, a rendering is a [`List] or [`Array] bracket pair
    around elements separated by [';'] at bracket depth zero.

    - [(…)], [[…]] and [{…}] nest and must balance. A [';'] inside them belongs
      to the enclosing element, so a list of lists or of records reads as one
      element per top-level entry.
    - String literals ([" … "], with backslash escapes) and the character
      literals ['c'], ['\c'], ['\000'] and ['\xFF'] are copied verbatim:
      brackets, semicolons and whitespace inside them are content, not syntax.
    - An element is otherwise any non-empty run of bytes. Nothing checks that it
      is a well-formed OCaml value — a custom printer's output passes as readily
      as an [int].

    An empty sequence (["[]"], ["[||]"]) reads as zero elements. *)

(** {1:canonical Canonical form and extents} *)

type extent = { start : int; length : int }
(** The type for byte ranges: [length] bytes starting at offset [start]. *)

type element = { canonical : string; extent : extent }
(** The type for one element of a rendering.

    [canonical] is the element with whitespace runs {e outside} string and
    character literals collapsed to a single space, and leading and trailing
    whitespace dropped. A rendering carries whatever line breaks the printer's
    box happened to insert, and those are layout rather than content:
    canonicalization is what makes ["[1; 2]"] and ["[1;\n 2]"] read as the same
    two elements, and it is why [canonical] is the string to compare and to
    show.

    [extent] is the element's byte range in the string passed to {!parse}: from
    its first non-whitespace byte through its last. It excludes the [';'] that
    separates it from its neighbour and the whitespace [canonical] dropped, so
    marking it marks the element and nothing around it. Extents are ascending
    and disjoint — consecutive ones are separated by at least their [';'] — and
    never begin or end inside a UTF-8 sequence, since the scan only ever cuts at
    ASCII delimiters. *)

(** {1:parsing Parsing} *)

val parse : string -> (kind * element array) option
(** [parse s] is the bracket form of [s] and its elements in rendering order, or
    [None] when [s] is not a rendering this reader can account for — see
    {!section:declining}. *)

(** {1:declining Declining}

    {!parse} is [None] whenever the scan cannot account for the whole string:
    [s] is not bracketed at all, its brackets do not balance, a string literal
    is unterminated, or an element is empty (["[a;; b]"], ["[1; 2;]"]).

    Read that [None] as {e "I could not account for this rendering"}, never as
    {e "the sequences agree"}. A caller that treats it as agreement turns every
    rendering outside the grammar into a silently passing comparison; the
    obligation is to fall back to a coarser grain — lines, characters, or the
    two values shown plain. *)
