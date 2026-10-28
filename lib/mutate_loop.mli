(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The mutation loop: the parent process of a mutation run.

    {!execute_and_report} is the one call through which a run becomes a mutation
    run, and the entry point of a suite makes it in place of {!Report.run}. The
    process that makes it runs no test, a mutant's least of all: the dry run,
    the probe, the mutants and [--arm] run in worker processes ([Supervisor]).

    The module decides which mutants survived, in which order and with which
    reaching tests. What a run tests or arms arrives on [config.mutation]
    ({!Run.type-mutation}), which {!Cli} resolves.

    @canonical Windtrap.Private.Mutate_loop *)

(** {1:running Running} *)

val execute_and_report :
  suite:string ->
  Run.config ->
  Test_tree.t list ->
  (Run.outcome, Run.startup_error) result
(** [execute_and_report ~suite config tests] is {!Report.run} over the same
    arguments, in the mode that [config.mutation] names, and the caller finishes
    its work on the result as it does after {!Report.run}.
    - Under {!Run.No_mutation} it is [Report.run ~suite config tests] and
      nothing else, in an instrumented build as in any other.
    - Under {!Run.Armed} it is the {{!section-armed}armed run}.
    - Under {!Run.Loop} it is the {{!section-loop}loop}.

    A loop that ran whole is [Ok] of its dry run, whatever it found: only
    [windtrap mutants], which merges the verdict files of every executable,
    gates on survivors. A loop that refused to start or could not go on, and an
    [--arm] identifier that is refused, are
    [Error (Run.Mutation_refused reason)], whose exit code is [1]. Each says its
    reason on standard error, and none falls back to a default and runs on. *)

(** {1:loop The loop}

    The loop proceeds in this order.
    + It fixes the population, which is the catalogue of the executable
      ({!val:Windtrap_runtime.Mutate.catalogue}) narrowed to the scope, without
      the mutants that [[@mutate off]] dismisses. The scope ({!Run.scope}) is
      the list of source-path prefixes that [--mutate] gave, and the lines of
      [--lines] or [--diff]. A mutant is in scope when the list is empty, or
      when one prefix is a prefix of its recorded file ([String.starts_with]),
      and when it lies on those lines, if any. A mutant out of scope is neither
      tested nor recorded.
    + It runs the dry run, which is {!Report.run} with [Supervisor.execute] over
      [config] with [junit] cleared, its workers telling it which tests evaluate
      each mutant. Every other field is that of [config], [baseline] included,
      so the dry run of a [--mutate -u] run accepts what a [-u] run accepts. A
      site that no test reached and that was evaluated outside every test, at
      module initialization or in a fixture release, is evaluated outside tests
      ({!Windtrap_runtime.Verdicts.Outside_tests}). A site that only tests
      marked xfail evaluated is unreached.
    + It runs the jobs ([Supervisor.fresh]), each in a worker of its own, which
      ran no test before it: the determinism probe, which is the tests that the
      dry run executed, unarmed, and then one job per reached mutant, in the
      order of the catalogue, with the mutant armed and its reaching tests up to
      the first failure. Up to [config.jobs] run at once, and they are given in
      that order. The probe agrees iff it executed as many tests, skipped as
      many and counted no failure.
    + Once the last job is given it writes the verdict file.

    A job starts from module initialization, so what the dry run left, a table
    it filled or a lazy value it forced, is not there: the mutant is judged on
    what its tests do. Nothing that a job prints is visible, its baselines are
    checked read-only and no correction is recorded.

    The root seed is {!Cli.mutation_seed} unless [--seed] gives one
    ({!Cli.settings}), so a property draws the same cases in the dry run, the
    probe and every job, and on every run of one binary.

    {b Verdicts.} A mutant is {!Windtrap_runtime.Verdicts.Killed} when its job
    counted a failure, that of a fixture release included. It is
    {!Windtrap_runtime.Verdicts.Unknown} when its job ended before its tests
    did: its worker was killed by a signal or exited, code under test calling
    [exit] included, or it passed its deadline. Otherwise it is
    {!Windtrap_runtime.Verdicts.Not_evaluated} when its job did not evaluate the
    armed site ({!Windtrap_runtime.Mutate.armed_hits} is [0]), as when the dry
    run reached the site in a test only because of what an earlier test had
    done, and survived when it did. The report lists a not-evaluated and an
    unknown mutant with the [--arm] command that runs it again, and names it no
    survivor.

    {b Tests marked xfail.} A test marked xfail ({!Test_tree.val-xfail}) reaches
    no mutant and kills none. The dry run and the probe run it, no mutant's job
    runs it, and no survivor names it.

    {b Limits.} Nothing bounds a whole run. A job that passes its deadline has
    its process group killed, and the loop goes on. The deadline is the time
    that the dry run took, read on the monotonic clock ({!Os.val-counter}), plus
    a share for the tests of the job. The share is ten times
    ([deadline_multiplier]) what the dry run measured for those tests, and at
    least one second. The probe's tests are every executed test.

    The runaway budget of a job ({!Windtrap_runtime.Mutate.arm}) is
    [(hits * 8) + 1000] ([budget_of]) for a site that the dry run evaluated
    [hits] times.

    {b The verdict file.} The file holds one record per reached mutant, one
    [Outside_tests] record per mutant of the population evaluated outside tests,
    and one [Unreached] record per unreached one. It is written to
    {!Windtrap_runtime.Verdicts.output_file} of the executable, by
    {!Windtrap_runtime.Verdicts.save}. Under a scope, the records of the files
    outside it stay when the file there was written by this build
    ({!Windtrap_runtime.Verdicts.writer_identity}) for a tested suite, and are
    dropped otherwise. A file that cannot be written ([Sys_error]) is one
    sentence on standard error above the outcome line.

    Only a loop that ran whole, over the whole suite, writes its verdicts. A run
    whose selection narrows the suite says so on standard error above the
    outcome line. It leaves a file that this build wrote, and otherwise writes a
    refused file, as below, so the merge names the suite. [filter], [exclude],
    [tags], [exclude_tags], [failed_only], [shard] and an active focus narrow
    the suite. The scope of [--mutate] does not.

    {b What the environment asks.} The facade's [WINDTRAP_MUTATE] rule holds
    when [config.broadcast.mutate] does: an empty population and a selection
    that keeps no test ({!Run.list_selection}) are no refusal, and the result is
    that of the ordinary run under {!Run.No_mutation}, before which an empty
    population says why on standard error. Every other refusal stands.

    {b Refusals.} Each of these is [Error (Run.Mutation_refused reason)] with
    one sentence on standard error ({!Os.say}), and but the first and the empty
    population each writes the verdict file as refused, with the reason
    ({!Windtrap_runtime.Verdicts.save}): its records hold the population as
    unknown, or, past a whole green dry run, the reached mutants as unknown and
    the others as that dry run found them. They are tried in this order:
    - The platform is Windows, where windtrap starts no worker process.
    - This process cannot start the jobs' workers ([Supervisor.can_spawn]): it
      spawned a domain, so it cannot fork, and a new start of the executable
      would not reach this run.
    - The dry run is refused at startup. {!Report.run} has said why.
    - No test ran in the dry run, or it is red.
    - The population is empty, unless the scope's lines name no line, or hold no
      site of a file of the catalogue that they name. The loop then says so on
      standard error and is [Ok] of its dry run, with no verdict file.
    - The probe disagrees with the dry run, and the sentence names the tests
      that failed in it, or its worker ended early or passed its deadline. *)

(** {1:armed The armed run}

    Under {!Run.Armed} the identifier is read by
    {!Windtrap_runtime.Mutate.id_of_string} and armed by
    {!Windtrap_runtime.Mutate.arm}, here and in each worker of an ordinary
    [Supervisor.execute] run.
    - An identifier that is malformed, ambiguous, or
      {!Windtrap_runtime.Mutate.Unmatched} within a file that this executable
      catalogues is [Error (Run.Mutation_refused _)], with the sentence of
      {!Windtrap_runtime.Mutate.pp_arm_error} on standard error, before anything
      runs.
    - {!Windtrap_runtime.Mutate.Uncatalogued} is no refusal. The same sentence
      goes to standard error before the transcript, and the result is that of
      the ordinary run under {!Run.No_mutation}.
    - With the mutant armed, it runs {!Report.run} with [baseline] set to
      {!Baseline.Check}, so it records no correction.
    - Its verdict leaves out the tests marked xfail, as the loop does. The
      unexpected pass of such a test fails the run and kills no mutant, and a
      site that only such tests evaluated is not reached
      ({!Report.mutation_not_reached}).

    The result of an armed run is that of {!Report.run}, so its exit code is
    that of the ordinary run, [2] included, and it writes its JUnit file. *)

(** {1:signals Signals}

    From the end of the dry run until its verdict file is written, the loop
    handles [SIGINT], [SIGTERM] and [SIGHUP]. One that arrives while a job runs
    ends every job, with its process group, and the mutants not yet given get no
    verdict. The loop then reports what it found, writes no verdict file, and
    dies by the same signal last, so its parent sees a death by signal and no
    [at_exit] function runs.

    A signal that arrives once the last job was given stops nothing. The verdict
    file is written, a second signal excepted, and the report ends as usual. The
    loop then dies by that signal, and says nothing more.

    [SIGPIPE] is handled over the same span. When the reader of standard output
    has gone away, the write that finds it gone fails. The loop then stops as it
    does for the other signals, except that it says nothing and tries no closing
    report, and it dies by [SIGPIPE]. When the process was started with
    [SIGPIPE] ignored, the [Sys_error] of the failed write escapes
    {!execute_and_report} instead. *)

(** {1:effects Effects}

    Beyond the effects of {!Report.run} and [Supervisor.fresh], the loop reads
    the verdict file of its executable and writes it. For the source line of a
    survivor it reads the recorded file of the mutant, under {!Os.project_root}
    first and then as given. *)
