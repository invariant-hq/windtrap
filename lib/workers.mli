(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Worker domains.

    A pool holds [n] domains that a test spawns once and reuses for every run of
    its jobs. {!run} hands job [i] to worker [i], and the jobs start together.
    The domain that calls {!run} never blocks on a worker: it waits in spins and
    short sleeps, so a signal handler, a limit's included, runs on it while a
    job deadlocks.

    {b A worker never dies.} Everything a job raises is caught, a control,
    [Sys.Break] and [Out_of_memory] included, and handed back to the caller of
    {!run} with its backtrace. An asynchronous exception that lands in the
    worker's own loop, or in its handling of an earlier one, is the ending of
    the job it holds, if any. Only one that lands while the domain starts,
    before its loop, as a memory profiler's callback may raise, ends the worker,
    and {!run} then waits for it as for a job that never ends. Between runs a
    worker waits in a blocking section; it spins only at the start of a run.

    {b Signals.} A worker is born blocking [SIGALRM], [SIGINT], [SIGTERM] and
    [SIGHUP], except on Windows, and so is every domain it spawns. The runtime
    runs their handlers on a domain that does not block them, such as the one
    that waits in {!run}, so the runner's handlers never run on a worker.

    {b What a worker runs as the caller would.} A worker records backtraces iff
    the domain that spawned the pool did, and it flushes its own
    [Format.std_formatter] and [Format.err_formatter] after each job, so its
    output reaches the standard streams while the run that gave the job is still
    open. *)

type t
(** The type for pools of worker domains. *)

val spawn : int -> t
(** [spawn n] is a pool of [n] workers, each waiting for a job. Raises what
    [Domain.spawn] raises, as [Failure] when the runtime has no domain left, or
    what a signal handler raises meanwhile, after the workers already spawned
    have been told to exit and joined. Raises [Invalid_argument] if [n < 1]. *)

exception Stuck of exn * Printexc.raw_backtrace
(** Raised by {!run} when a job is still running after its grace: the exception
    that interrupted the wait, with its backtrace. *)

val run : t -> grace:(exn -> float) -> (unit -> unit) array -> unit
(** [run t ~grace jobs] runs [jobs.(i)] on worker [i], all of them started
    together once every worker has taken its job, and returns once they have all
    ended. It then raises what the first job by index that raised raised, with
    its backtrace. Writes a job makes before it ends are visible to the caller
    when [run] returns or raises.

    While the jobs run, the caller spins, then sleeps in short steps. When a
    signal handler raises [e] there, [run] waits at most [grace e] more seconds,
    ignoring what a handler raises then. It raises [e] again with its backtrace
    when every job has ended by then, and {!Stuck} otherwise: [t] is then stuck,
    and a job may run on forever.

    Raises [Invalid_argument] if [jobs] does not hold one job per worker or if
    [t] is stuck. *)

val join : t -> unit
(** [join t] tells every worker of [t] to exit and joins them, unless [t] is
    stuck: its workers are then left as they are, since one may never return.
    [t] must not be used afterwards. A signal handler that raises during the
    join ends it, and the workers not joined yet exit on their own. *)
