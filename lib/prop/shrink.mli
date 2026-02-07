(*---------------------------------------------------------------------------
   Copyright (c) 2013-2017 Guillaume Bury, Simon Cruanes, Vincent Hugot,
                           Jan Midtgaard
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: BSD-2-Clause
  ---------------------------------------------------------------------------*)

(** Shrinking utilities for property testing.

    Shrinking produces smaller values to find minimal counterexamples. The main
    strategy is binary search toward a destination value. *)

val int_towards : int -> int -> int Seq.t
(** [int_towards dest x] produces a sequence of integers starting at [dest] and
    converging toward [x] by halving the distance at each step. The sequence
    includes [dest] but excludes [x]. Returns an empty sequence when [dest = x].
*)

val int32_towards : int32 -> int32 -> int32 Seq.t
(** [int32_towards dest x] is like {!int_towards} for [int32]. *)

val int64_towards : int64 -> int64 -> int64 Seq.t
(** [int64_towards dest x] is like {!int_towards} for [int64]. *)

val nativeint_towards : nativeint -> nativeint -> nativeint Seq.t
(** [nativeint_towards dest x] is like {!int_towards} for [nativeint]. *)

val float_towards : float -> float -> float Seq.t
(** [float_towards dest x] is like {!int_towards} for [float]. Produces at most
    15 elements, since float division does not converge as reliably as integer
    division. Stops early if the step falls below {!Float.epsilon}. *)
