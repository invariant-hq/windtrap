(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The errors that comparisons under a tolerance measure.

    A comparison under {!Testable.float} or {!Testable.float_rel}, whatever
    witness holds it, as a {!Testable.list} of them does, measures its error,
    [|a -. b|], and the bound it allows,
    [Float.max abs (rel *. Float.max (abs_float a) (abs_float b))], the relative
    part being [0.] when a side is not finite. Two equal sides have error [0.],
    and a comparison whose error is not finite, one side NaN or infinite,
    measures nothing. A witness without a tolerance measures nothing.

    {!Check.equal} reads the measure through {!observed} and records it for the
    report (see {!Check.val-tolerances}).

    @canonical Windtrap.Private.Tolerance *)

type error = {
  error : float;  (** The error of the comparison, [|a -. b|]. *)
  tolerance : float;  (** The bound the comparison allowed. *)
}
(** The type for the error of one comparison under a tolerance. *)

val observing : unit -> bool
(** [observing ()] is [true] iff an {!observed} runs on the calling domain, so
    that a comparison need not compute its bound when no one reads it. *)

val observe : float -> float -> unit
(** [observe error tolerance] offers the comparison of [error] against
    [tolerance] to the innermost {!observed} running on the calling domain, and
    does nothing when none runs. *)

val observed : (unit -> 'a) -> 'a * error option
(** [observed fn] is [fn ()] with the comparison, among those that {!observe}
    offered while [fn ()] ran on the calling domain, whose error is the largest
    share of its tolerance, the larger error between two equal shares: [None]
    when none was offered. An [observed] within [fn] keeps its comparisons from
    the enclosing one. What [fn] raises is raised again. *)
