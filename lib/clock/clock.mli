(*---------------------------------------------------------------------------
   Copyright (c) 2015 The mtime programmers. All rights reserved.
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Monotonic clock for timing tests.

    Provides access to a monotonic clock suitable for measuring elapsed time.
    Unlike wall-clock time, monotonic time never goes backwards and is
    unaffected by system time adjustments.

    Derived from {{:https://erratique.ch/software/mtime}mtime}. *)

type counter
(** An opaque value representing a point in monotonic time. Use with {!count} or
    {!count_s} to measure elapsed time. *)

val counter : unit -> counter
(** [counter ()] samples the current monotonic time. *)

val count : counter -> int64
(** [count start] returns the elapsed nanoseconds since [start]. The result is
    always non-negative. *)

val count_s : counter -> float
(** [count_s start] returns the elapsed seconds since [start] as a [float]. The
    result is always non-negative. *)
