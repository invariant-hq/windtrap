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

    {b Workers.} The facade ([{2:process}] in windtrap.mli) states how a worker
    starts, lives and ends; {!enter} and {!execute} keep it. A worker of the
    first [Windtrap.run] call of the process the executable started as is the
    executable started again, with [Sys.argv]: the call of {!enter} of the same
    rank serves, once it checked that it declared the parent's suite. Until then
    its standard output and error are a log in its temporary directory, which
    the parent reads only if the worker ends first. Any other run forks its
    workers when a case needs one, unless the process spawned a domain. As a
    session of its own, a worker leaves a terminal's signal to the parent, which
    sends it on. The parent kills its process group when it ends, and removes
    the directory of its {!Run.temp_dir}s.

    Up to [config.jobs] workers run at once, and one under [config.stream], so
    that what the tests write to the real descriptors keeps its order. A case
    whose worker ends fails, saying how. The kill of its limit comes
    [3 * limit * (retries + 1)] seconds after it started, [limit] being
    {!Run.limit}'s, the time it waited for the parent's verdict on its checks
    left out, and fails it as {!Failure.timeout} of its limit; for the first
    case of a worker that started the executable again, the time before it
    served is left out too. The row of a case that retires its worker stands.

    {b Order.} The rows reach the observer in the order of the selection, a
    case's events with them, and the events of the case the observer is at as
    they happen. Under a correcting mode the parent makes the checks of a case
    when it reaches it, after every earlier one, in its own registry, so the
    conflicts, the first reads and the kept corrections are an in-process run's
    at any number of workers. Under [config.bail] the cases that other workers
    ran past the stop are not reported, and their checks are never made.

    {b Signals.} The parent sends the first of the signals that the facade names
    to every worker, each of which ends as {!Run.execute} does on it, then gives
    {!Run.Interrupted}, with the test or the release that a worker named, and
    dies by the signal. A worker that dies of one after it said so ends the run
    too.

    {b Coverage.} A worker hands the parent the coverage it counted as it ends,
    and the parent's dump at exit holds it, so a run leaves one dump. A worker
    that a signal or [Unix._exit] ended loses what it counted.

    {b Mutants.} Under [--mutate] and [--arm] ({!Run.type-mutation}) a worker
    also tells its parent which mutants each test evaluated, and how many times:
    what [Windtrap_runtime.Mutate.drain] gives when the test ends, its window
    opening when the test starts. What it evaluates outside every test, in a
    fixture release, and what the parent evaluated as its modules initialised,
    belong to no test. An [--arm] worker arms its mutant as it starts, and its
    evaluations count from there. *)

val execute :
  ?on_reach:
    (string list option -> (Windtrap_runtime.Mutate.id * int) list -> unit) ->
  ?on_event:(Run.event -> unit) ->
  Run.config ->
  suite:string ->
  Test_tree.t list ->
  (Run.outcome, Run.startup_error) result
(** [execute config ~suite tests] is [Run.execute config ~suite tests] with the
    tests run by workers, as above. Under [config.in_process], on Windows, which
    has no [fork], and in a process that can neither fork nor start the
    executable again to reach the run (it spawned a domain, and is a fork or not
    inside a [Windtrap.run] call), it is {!Run.execute}.

    Under mutation, [on_reach test sites] receives the {{!mutants}evaluations}
    of each window that evaluated a mutant: [test] is [Some path] for a test's,
    given before the parent reaches its row, and [None] for those of no test,
    the parent's first. Defaults to ignoring them. *)

(** {1:fresh Fresh workers}

    A mutant runs in a worker of its own, started as {!execute} starts one,
    before any test ran in it, so it starts from module initialisation and
    nothing another mutant, or the dry run, left behind: a cache the dry run
    filled cannot hide it. *)

type job = {
  cases : string list;  (** The cases to run, by path string, in this order. *)
  deadline : float;
      (** The seconds the job may take from its first worker's start, after
          which its process group is killed. *)
  bail : bool;  (** Whether the job stops at its first counted failure. *)
  armed : (Windtrap_runtime.Mutate.id * int) option;
      (** The mutant that each of its workers arms before its first case, with
          its budget ([Windtrap_runtime.Mutate.arm]). *)
}
(** The type for a job: some cases of the suite, run in one fresh worker. *)

(** The type for how a job ended. *)
type ended =
  | Ran  (** It ran its cases, up to its stop, and released its fixtures. *)
  | Lost of string
      (** Its worker ended first, which the phrase says, such as
          ["was killed by SIGSEGV"] or
          ["ended when code under test called exit"]. *)
  | Expired  (** It passed its deadline. *)

type ran = {
  results : Run.result list;  (** The rows of the cases that ended, in order. *)
  release_failures : Failure.t list;
      (** The failures of its releases, and those of no case. *)
  armed_hits : int;
      (** The evaluations of the armed site that its workers counted
          ([Windtrap_runtime.Mutate.armed_hits]), [0] when none is armed. *)
  ended : ended;  (** How it ended. *)
}
(** The type for what a job did. *)

val can_spawn : unit -> bool
(** [can_spawn ()] is [true] iff this process can start a worker for the run
    under way: it is not on Windows, and it can fork, or it is the process the
    executable started as, inside a [Windtrap.run] call. *)

val fresh :
  Run.config ->
  suite:string ->
  Test_tree.t list ->
  job list ->
  on_ran:(int -> ran -> unit) ->
  on_signal:(int -> unit) ->
  unit
(** [fresh config ~suite tests jobs ~on_ran ~on_signal] runs each of [jobs] in a
    worker of its own, started as {!execute} starts one, up to [config.jobs] at
    once, in the order of the list, and gives [on_ran i ran] for job [i] in that
    order, once every earlier one was given. {!can_spawn} must hold.

    A worker of a job serves its cases with {!Run.serve} under [config] with
    {!Baseline.Check}, no [stream] and no JUnit file, its capture logs under its
    temporary directory, and [/dev/null] as its standard output and error. The
    parent sends the job's cases one at a time, and [Finish] after the last, or
    after the first counted failure under [bail]. A worker that retires leaves
    the rest of the job to a fresh worker, which arms the mutant again. Coverage
    counted by a worker is dropped, since a mutant changed what the program
    does.

    While it runs, the parent handles [SIGINT], [SIGTERM], [SIGHUP] and
    [SIGPIPE], and tells [on_signal] of each as it arrives, in the handler,
    which must only record it. The first ends every job that runs, with its
    process group, and [fresh] returns once no worker is left; the jobs not yet
    given are never given. An exception that [on_ran] raises ends every job
    likewise, and leaves [fresh]. *)

val enter : suite:string -> Test_tree.t list -> (unit -> int) -> int
(** [enter ~suite tests run] is [run ()], the code of a [Windtrap.run] call,
    which [Windtrap.run] gives it, unless the process is a worker that started
    the executable again. Then, for each call before the one that started the
    worker, it is the code that the call returned in the parent, and that call
    serves and never returns. A worker whose [tests] are not its parent's writes
    why and exits [2]. A fork of a worker makes its calls. *)
