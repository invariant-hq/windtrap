(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The process boundary: a run whose tests run in worker processes.

    {!execute} is {!Run.execute}'s contract, kept by two kinds of process. The
    parent, the process that called it, runs no test: it selects, gives the
    events to the observer, judges the baseline checks of a correcting mode,
    keeps the store, writes the corrections and returns the outcome. Its workers
    run the tests with {!Run.serve}, one at a time each, and report what happens
    over a pipe. The parent decides every verdict from those reports, from how a
    worker ends and from how long it takes, so code under test cannot end the
    run or set its exit code.

    {b Workers.} A worker is a fork of the parent made when a case needs one, so
    it starts from what module initialisation built and nothing a test left. It
    becomes a session of its own: a terminal's signal reaches the parent alone,
    which sends it on, and the parent kills the worker's process group when it
    ends. Its temporary directories ({!Run.temp_dir}) live under a directory of
    its own, which the parent removes. A process that spawned a domain cannot
    fork, so it executes [Sys.executable_name] again with [Sys.argv] for each
    worker, and the call of {!enter} of the same rank in the new process serves.

    Up to [config.jobs] workers run at once, and one under [config.stream], so
    that what the tests write to the real descriptors keeps its order. A worker
    ends, and the next case starts in a new one, when:
    - its case ended it: a call to [exit] or [Unix._exit], a signal, or a fatal
      exception. The case fails, saying which.
    - its case outlived its kill: [3 * limit * (retries + 1)] seconds after it
      started, the time it waited for the parent's verdict on its checks left
      out. The case fails as {!Failure.timeout} of its limit.
    - its case spawned a domain, which could still run in the next case. The
      worker retires: it releases its fixtures, and the case's row stands. A
      worker that a test ended releases nothing.

    {b Order.} The rows reach the observer in the order of the selection, a
    case's events with them, and the events of the case the observer is at as
    they happen. Under a correcting mode the parent makes the checks of a case
    when it reaches it, after every earlier one, in its own registry, so the
    conflicts, the first reads and the kept corrections are an in-process run's
    at any number of workers. Under [config.bail] the cases that other workers
    ran past the stop are not reported, and their checks are never made.

    {b Signals.} While it runs, the parent handles [SIGINT], [SIGTERM] and
    [SIGHUP] and sends the first to every worker, each of which ends as
    {!Run.execute} does on it. A worker that dies of one of them after it said
    so ends the run too. The parent then gives {!Run.Interrupted}, with the test
    or the release that a worker named, and dies by the signal.

    {b Coverage.} A worker hands the parent the coverage it counted as it ends,
    and the parent's dump at exit holds it, so a run leaves one dump. A worker
    that a signal or [Unix._exit] ended loses what it counted. *)

val execute :
  ?on_event:(Run.event -> unit) ->
  Run.config ->
  suite:string ->
  Test_tree.t list ->
  (Run.outcome, Run.startup_error) result
(** [execute config ~suite tests] is [Run.execute config ~suite tests] with the
    tests run by workers, as above. Under [config.in_process], and on Windows,
    which has no [fork], it is {!Run.execute}. *)

val enter : suite:string -> Test_tree.t list -> [ `Parent | `Skip ]
(** [enter ~suite tests] is [`Parent], unless the process is a worker that its
    parent executed again: then [`Skip] for the calls before the one that
    started the worker, which the parent made, and for that call it serves and
    never returns. [Windtrap.run] calls it first. *)
