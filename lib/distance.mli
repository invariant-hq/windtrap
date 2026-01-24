(*---------------------------------------------------------------------------
   Copyright (c) 2020-2021 Craig Ferguson
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   Levenshtein edit distance adapted from Craig Ferguson's work on Alcotest
   (https://github.com/mirage/alcotest/pull/247).
  ---------------------------------------------------------------------------*)

(** Levenshtein edit distance for subcomponent diffing.

    Computes minimal edit scripts between sequences, used internally by
    {!Testable} to produce highlighted diffs of expected vs. actual values. *)

type index = int
(** Zero-based position within a sequence. *)

(** A single edit operation in an edit script. Indices refer to positions in the
    original (expected) and revised (actual) sequences. *)
type command =
  | Insert of { expected : index; actual : index }
      (** Element at [actual] in the actual sequence has no counterpart at
          [expected] in the expected sequence. *)
  | Delete of { expected : index }
      (** Element at [expected] in the expected sequence was removed. *)
  | Substitute of { expected : index; actual : index }
      (** Element at [expected] was replaced by the element at [actual]. *)

type edit_script = command list
(** A minimal sequence of edit operations transforming the expected sequence
    into the actual one. Empty when the sequences are equal. *)

type ('a, _) container =
  | Array : ('a, 'a array) container
  | List : ('a, 'a list) container
  | String : (char, string) container
      (** Witness type selecting the concrete container representation. *)

val levenshtein :
  ('a, 'c) container -> equal:('a -> 'a -> bool) -> 'c -> 'c -> edit_script
(** [levenshtein container ~equal expected actual] computes a minimal edit
    script from [expected] to [actual].

    Common prefix and suffix are stripped in O(n) before the O(n*m) dynamic
    programming phase, where n and m are the lengths of the differing subranges.
    Returns [[]] when the sequences are equal. *)

val edit_indices : edit_script -> int list * int list
(** [edit_indices script] returns [(expected_indices, actual_indices)] where
    each list contains the positions of elements affected by edits. Indices are
    returned in ascending order. *)
