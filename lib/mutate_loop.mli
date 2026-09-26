(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The mutation loop: the parent process of a mutation run.

    {!execute_and_report} is the one call through which a run becomes a mutation
    run, and the entry point of a suite makes it in place of {!Report.run}.

    The module decides which mutants survived, in which order and with which
    reaching tests. What a run tests or arms arrives on [config.mutation]
    ({!Run.type-mutation}), which {!Cli} resolves. *)

(** {1:running Running} *)

(** The type for what {!execute_and_report} did with the run. *)
type run =
  | Ran of (Run.outcome, Run.startup_error) result
      (** The result of {!Report.run}, which was called once. The caller
          finishes its work on it as it does after {!Report.run}. *)
  | Reported of int
      (** The module has printed everything, and the process must exit with this
          code, which is [0] or [1]. The caller must do none of the work that
          follows a run. *)

val execute_and_report : suite:string -> Run.config -> Test_tree.t list -> run
(** [execute_and_report ~suite config tests] is {!Report.run} over the same
    arguments, in the mode that [config.mutation] names.
    - Under {!Run.No_mutation} it is [Ran (Report.run ~suite config tests)] and
      nothing else, in an instrumented build as in any other.
    - Under {!Run.Armed} it is the {{!section-armed}armed run}.
    - Under {!Run.Loop} it is the {{!section-loop}loop}, and the result is
      [Reported], but for a [WINDTRAP_MUTATE] that finds nothing to test.

    [Reported 0] is a loop that ran whole, whatever it found. Only
    [windtrap mutants], which merges the verdict files of every executable,
    gates on survivors (guarantee 12 of [doc/dev/architecture.md]). [Reported 1]
    is a loop that refused to start or could not go on, or an [--arm] identifier
    that is refused. Each says its reason on standard error, and none falls back
    to a default and runs on. *)

(** {1:loop The loop}

    The loop proceeds in this order, and its children run one at a time.
    + It fixes the population, which is the catalogue of the executable
      ({!val:Windtrap_runtime.Mutate.catalogue}) narrowed to the scope, without
      the mutants that [[@mutate off]] dismisses. The scope is the list of
      source-path prefixes that [--mutate] gave. A mutant is in scope when the
      list is empty, or when one prefix is a prefix of its recorded file
      ([String.starts_with]). A mutant out of scope is neither forked nor
      recorded.
    + It runs the dry run, which is {!Report.run} over [config] with [junit]
      cleared. Every other field is that of [config], [baseline] included, so
      the dry run of a [--mutate -u] run accepts what a [-u] run accepts. A site
      that the dry run evaluated only outside a test, at module initialization
      or in a fixture release, counts as unreached. So does a site that only
      tests marked xfail evaluated.
    + It runs the determinism probe, which is one unarmed child over the tests
      that the dry run executed. The probe agrees iff it executed as many tests,
      skipped as many and counted no failure.
    + It forks one child per reached mutant, in the order of the catalogue, and
      never forks an unreached one.
    + When the last child has ended it writes the verdict file.

    {b Children.} A child runs {!Run.execute} over {!Run.for_subset} of
    [config], with the reaching tests of its mutant as the allowlist. It checks
    baselines read-only and records no correction. Nothing that a child prints
    is visible, because both of its standard descriptors are [/dev/null]. No
    [at_exit] function runs in a child.

    {b Verdicts.} A mutant is {!Windtrap_runtime.Verdicts.Killed} when its child
    counted a failure, that of a fixture release included. It is also killed
    when its child reported no survivor, because the child recorded no test,
    passed its deadline, did not exit with [0], or left no complete line.

    {b Tests marked xfail.} A test marked xfail ({!Test_tree.val-xfail}) reaches
    no mutant and kills none. The dry run and the probe run it, no child runs
    it, and no survivor names it.

    {b Limits.} Nothing bounds a whole run. A child that passes its deadline has
    its process group killed, and the loop goes on. The deadline is the time
    that the dry run took, read on the monotonic clock ({!Os.val-counter}), plus
    a share for the tests of the child. The share is ten times
    ([deadline_multiplier]) what the dry run measured for those tests, and at
    least one second. The probe has the same deadline, over every executed test.

    The runaway budget of a child ({!Windtrap_runtime.Mutate.arm}) is
    [(hits * 8) + 1000] ([budget_of]) for a site that the dry run evaluated
    [hits] times.

    {b The verdict file.} The file holds one record per reached mutant, and one
    [Unreached] record per unreached mutant of the population. It is written to
    {!Windtrap_runtime.Verdicts.output_file} of the executable, by
    {!Windtrap_runtime.Verdicts.save}. Under a scope, the records of the files
    outside it stay when the file there was written by this build
    ({!Windtrap_runtime.Verdicts.writer_identity}), and are dropped otherwise. A
    file that cannot be written ([Sys_error]) is one sentence on standard error
    above the outcome line, and the result is still [Reported 0].

    Only a loop that ran whole, over the whole suite, writes it. A run whose
    selection narrows the suite writes none and says so on standard error above
    the outcome line. [filter], [exclude], [tags], [exclude_tags],
    [failed_only], [shard] and an active focus narrow the suite. The scope of
    [--mutate] does not.

    {b What the environment asks.} [WINDTRAP_MUTATE] reaches every test
    executable of a project, so an executable that cannot honour it is not in
    error. When [config.broadcast.mutate] holds, an empty population and a
    selection that keeps no test ({!Run.list_selection}) are no refusal. The
    result is then [Ran] of the ordinary run under {!Run.No_mutation}, before
    which an empty population says why on standard error. Every other refusal
    stands.

    {b Refusals.} Each of these is [Reported 1] with one sentence on standard
    error ({!Os.say}). The first six are tried in this order, and the last has
    no one place in it:
    - The platform is Windows, which has no [Unix.fork].
    - The dry run is refused at startup. {!Report.run} has said why, and the
      code is [1] whatever {!Run.startup_exit_code} is.
    - The exit code of the dry run is not [0], because no test ran ([2]) or
      because the run is red.
    - The population is empty.
    - The probe disagrees with the dry run, and the sentence names the tests
      that failed in it, or it passes its deadline, is refused at startup, or
      reports nothing readable.
    - A child cannot arm its mutant, or is refused at startup.
    - The supervision fails. The scratch directory cannot be created, which is
      tried before the probe, or [pipe], [fork] or [waitpid] fails for the probe
      or for a child. [fork] fails in a process that has spawned a domain, in
      the dry run or before it, and the sentence then says so. *)

(** {1:armed The armed run}

    Under {!Run.Armed} the identifier is read by
    {!Windtrap_runtime.Mutate.id_of_string} and armed by
    {!Windtrap_runtime.Mutate.arm} in this process, which forks nothing.
    - An identifier that is malformed, ambiguous, or
      {!Windtrap_runtime.Mutate.Unmatched} within a file that this executable
      catalogues is [Reported 1], with the sentence of
      {!Windtrap_runtime.Mutate.pp_arm_error} on standard error, before anything
      runs.
    - {!Windtrap_runtime.Mutate.Uncatalogued} is no refusal. The same sentence
      goes to standard error before the transcript, and the result is [Ran] of
      the ordinary run under {!Run.No_mutation}.
    - With the mutant armed, it runs {!Report.run} with [baseline] set to
      {!Baseline.Check}, so it records no correction.
    - Its verdict leaves out the tests marked xfail, as the loop does. The
      unexpected pass of such a test fails the run and kills no mutant, and a
      site that only such tests evaluated is not reached
      ({!Report.mutation_not_reached}).

    The result of an armed run is [Ran], so its exit code is that of the
    ordinary run, [2] included, and it writes its JUnit file. *)

(** {1:signals Signals}

    From before its scratch directory exists until its verdict file is written,
    the loop handles [SIGINT], [SIGTERM] and [SIGHUP] as {!Run.execute} does
    during a run (see its {{!Run.section-signals}signals}).

    A child is a session of its own ([setsid]), which a terminal's signal does
    not reach. The handler therefore kills the process group of the running
    child, and the mutant of that child gets no verdict, whatever the child
    reported. The loop then removes its scratch directory and writes no verdict
    file. It dies by the same signal last, so its parent sees a death by signal
    and no [at_exit] function runs.

    A signal that arrives once the last child has ended stops nothing. The
    verdict file is written, a second signal excepted, and the report ends as
    usual. The loop then dies by that signal, and says nothing more.

    [SIGPIPE] is handled over the same span. When the reader of standard output
    has gone away, the write that finds it gone fails. The loop then stops as it
    does for the other signals, except that it says nothing and tries no closing
    report, and it dies by [SIGPIPE]. When the process was started with
    [SIGPIPE] ignored, the [Sys_error] of the failed write escapes
    {!execute_and_report} instead, after the scratch directory is removed. *)

(** {1:effects Effects}

    Beyond the effects of {!Report.run}, the loop calls [fork], [waitpid],
    [pipe] and [select], and each child calls [setsid]. The loop calls [kill] on
    the process group of a child that expired or was interrupted. It creates one
    scratch directory per run under [Filename.get_temp_dir_name ()], and removes
    it however the loop ends, a second signal excepted. For the source line of a
    survivor it reads the recorded file of the mutant, under {!Os.project_root}
    first and then as given. *)
