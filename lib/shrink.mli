(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(** The reducer of failing choice sequences.

    A case is a choice sequence that a parse read, with the spans and the
    dependents it marked (see {!Choice}). The reducer edits the sequence of a
    failing case and parses the edit again. It keeps an edit when the case still
    fails and the sequence the parse read is shortlex smaller: shorter, or as
    long and smaller at its first difference, each choice compared unsigned.
    Each kept edit is a strictly smaller sequence, so the search ends.

    @canonical Windtrap.Private.Shrink *)

type case = {
  choices : int64 array;
  spans : Choice.span list;
  dependents : Choice.dependent list;
}
(** The type for what a parse read. *)

(** The type for what a test makes of an edit. *)
type verdict =
  | Fails of { parsed : case; keep : unit -> unit }
      (** The edit fails as the case did, and its parse read [parsed]. [keep] is
          called once if the reducer keeps it. *)
  | Passes  (** The edit passes, or fails in another way. *)
  | Discards  (** The edit was discarded. *)
  | Not_smaller
      (** The edit parsed to a sequence no smaller than the case kept, so no law
          ran on it. *)

type ending =
  | Converged  (** No edit of the last case was kept. *)
  | Budget_spent  (** The budget was spent, and an edit was left. *)

val shortlex : int64 array -> int64 array -> int
(** [shortlex a b] orders by length, then at the first difference, unsigned. *)

val shrink :
  budget:int ->
  cost:int ->
  (int64 array -> verdict) ->
  case ->
  case * int * ending
(** [shrink ~budget ~cost test case] is the smallest case found from the failing
    [case], the number of edits kept and how the search ended. It runs three
    passes to a fixed point:
    + Delete each span, by start, the longest first.
    + Delete each run of [8], then [4], [2] and [1] choices, from the end.
    + Minimise each choice in order: to [0], else by binary search between the
      greatest value found passing and the current one.

    A deletion lowers each dependent after it by the positions it counts past
    inside the deleted choices, in the same edit, and never below [0].

    Each [test] costs [cost] of [budget], [1] when it [Discards] and nothing
    when it is [Not_smaller]. An edit tried before is not tested again and costs
    nothing. The search ends with {!Budget_spent} when a further edit would run
    with the budget spent. It raises what [test] raises, and the case kept last
    is then the caller's to know, through [keep]. *)

val candidates : case -> int64 array Seq.t
(** [candidates case] is the edits that one step of the passes of {!shrink}
    makes from [case], in their order: each span deleted, each run deleted, then
    each choice lowered to [0] and to values that each close half the gap to it.
    A reader that keeps an edit only when its parse read a shortlex smaller
    sequence ends, since every chain of such edits is finite. *)
