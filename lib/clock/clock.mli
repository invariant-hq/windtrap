(*---------------------------------------------------------------------------
   Copyright (c) 2015 The mtime programmers. All rights reserved.
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Monotonic clock for timing tests.

    Monotonic time never goes backwards and is unaffected by system clock
    adjustments, which makes it the right base for per-test durations and the
    slowest-test report. Sample a {!counter} before the measured work and read
    the elapsed time with {!count} or {!count_s}.

    Derived from {{:https://erratique.ch/software/mtime}mtime}. *)

type counter
(** The type for points in monotonic time. *)

val counter : unit -> counter
(** [counter ()] samples the current monotonic time.

    Raises [Sys_error] if the platform's monotonic clock is unavailable or
    fails. *)

val count : counter -> int64
(** [count start] is the number of nanoseconds elapsed since [start]. The result
    is non-negative. *)

val count_s : counter -> float
(** [count_s start] is [count start] in seconds. *)
