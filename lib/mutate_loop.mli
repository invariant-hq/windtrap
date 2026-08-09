(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The mutation loop: the parent process of a mutation run.

    Core windtrap's whole coupling to mutation is {!execute_and_report}, called
    at run entry by the two thin drivers in place of
    {!Driver.execute_and_report} — plus one read-only flag on the expect
    correction path ({!Ppx_runtime.enter_armed}, Law 16d, reached through this
    module's [~armed] argument, never as a dependency). Everything else lives
    here and in the stdlib-only runtime {!Windtrap_mutate}: the dry run and its
    reach map, the determinism probe, the forced-fail check, the fork loop, the
    verdict file, and the report.

    {b Why this module wraps the run rather than being called around it.} The
    three things a mutation run must do — announce an armed mutant {e before}
    any other output, print the discovery line {e after} the summary, and run
    the suite again once per mutant {e after} the dry run — bracket the run on
    both sides. A seam that only fired at run entry would need a second seam at
    run end, and a seam that only fired at run end could not announce. So the
    run is this module's argument, not its caller's: it is one call, in one
    place, and there is nothing for a runner to get out of order.

    {b Modes} (from {!Cli.mutation}, and the catalogue):

    - No mutant registered in this executable: the run is an ordinary run and
      nothing here does anything.
    - [WINDTRAP_MUTATE] off or unset, no [WINDTRAP_MUTATE_ARM]: the ordinary run
      plus the discovery line — what this build {e could} do, and what to type.
      It counts the mutants the loop would test, so a site dismissed by
      [[@mutate off]] is offered nowhere and appears in no denominator. Adding
      the backend never makes a test run longer than it was.
    - [WINDTRAP_MUTATE_ARM] set and naming a site {e this} executable
      catalogues: one mutant armed for the whole process, the announcement first
      (Law 16b), read-only checking (Law 16d), and [mutant killed.] after a
      transcript that failed.
    - [WINDTRAP_MUTATE_ARM] set and naming a file this executable catalogues no
      site in ({!Windtrap_mutate.Uncatalogued}): the ordinary run, plus one line
      on [stderr] saying this binary holds no such mutant. It is {e not} a
      refusal, and that is the difference between the report's headline remedy
      working and failing. One identifier is armed across a whole project at
      once —
      [WINDTRAP_MUTATE_ARM=<id> dune runtest --instrument-with
       ppx_windtrap.mutate], since no single binary can be named by a command
      that links none — so in a project with several [(test)] stanzas the
      identifier reaches every instrumented executable, and all but one of them
      were built from other sources. Exiting [1] there would fail the build
      {e because} the one binary that has the mutant armed it correctly. Nothing
      is hidden by running on: an executable holding none of that file's sites
      produces no verdict about them either way.
    - [WINDTRAP_MUTATE=1] (or [report]): the loop below, which takes the process
      over and reports on its own.

    {b The loop.}

    + {b Dry run.} The suite runs once, normally, nothing armed, printing its
      ordinary summary line. The reach map is built from
      {!Driver.execute_and_report}'s [?on_event] — a {e second} subscriber
      composed after the transcript's, never replacing it. A red dry run, an
      empty one, or one with no mutant to test aborts: a score over a suite that
      does not pass is not a score.
    + {b Determinism probe.} One unarmed fork re-runs the suite. Disagreement
      with the dry run aborts, naming the tests that disagreed — mutation
      results over a non-deterministic suite are not a weaker number, they are
      not a number.
    + {b Forced-fail check.} The mutant the most tests reach is armed and those
      tests run. If nothing fails the loop aborts, because the commonest
      misconfiguration — the backend on the test executable but not on the
      library under test — produces exactly that, and it costs one child to rule
      out. Its verdict is kept, so the check is free for a suite that passes it.
    + {b The loop.} One [fork] per reached, non-dismissed mutant. The child
      arms, runs that mutant's reaching tests under [bail = Some 1], and writes
      one line on a pipe. {b The verdict never rides an exit code}: the inline
      runner returns [0] for a corrections-covered expect failure, so an exit
      code cannot tell a killed mutant from a survivor.
    + {b Report.} One verdict file under [_build/_mutants] (so [windtrap mutate]
      can merge the several test executables that cover one library) and the
      report through {!Render.mutation_report}.

    {b Not in this slice.} Per-mutant deadlines, process groups and
    [WINDTRAP_MUTATE_JOBS]: the deadline is one whole-loop [Unix.setitimer],
    complemented by the runtime's runaway hit-count budget. It costs the
    per-mutant granularity, not the diagnosis — an expiry still names the mutant
    it was on, and aborts the run rather than scoring it. [report] mode's
    dismissed, not-armable and timeout tables are likewise later, so [report]
    currently runs the loop and prints the default report — and with no
    not-armable table, a site the dry run only evaluated {e outside} a test
    (module initialization, a fixture release) is listed as unreached rather
    than as not armable. Both are "no test evaluates this" and neither is
    forked, so the score is right and only the remedy the reader is offered is
    imprecise; {!Windtrap_mutate}'s reach protocol already separates the two,
    and the table is what is missing. Mutation needs [Unix.fork] and therefore
    declines by name on Windows.

    {b Exit codes} (Law 16e): [0] when the loop completed, {e whatever it found}
    — a survivor never fails a build in this release — and [1] when it refused
    to start or could not finish, each with its own message on [stderr]. Never
    [2]: "nothing ran" is a statement about a test selection, and a mutation run
    does not make one. *)

(** {1:running Running} *)

(** The type for what {!execute_and_report} did with the run. *)
type run =
  | Ran of (Runner.outcome * Run.result list, Runner.startup_error) result
      (** The suite ran once, ordinarily — no loop, or a loop that never
          started. The caller finishes its own post-run work on it (JUnit, the
          focus warning, the correction protocol, the exit) exactly as it would
          have on {!Driver.execute_and_report}'s result. *)
  | Reported of int
      (** The mutation run took the process over and has printed everything it
          has to say. Nothing about the underlying run is the caller's business
          — a loop's dry run is not the process's verdict — and the process
          exits with this code. *)

val execute_and_report :
  armed:(unit -> unit) ->
  invocation:Render.invocation ->
  seed:Seed.seed option ->
  selection:string option ->
  github:bool ->
  output:[ `Quiet | `Compact | `Verbose ] ->
  coverage_mode:[ `Summary | `Report | `Full | `Off ] ->
  config:Run.config ->
  suite:string ->
  Test_tree.t list ->
  run
(** [execute_and_report ~armed ~invocation … tests] is the mutation-aware run
    entry: {!Driver.execute_and_report} with the same arguments and the same
    meaning, wrapped in whichever of the modes above this process is in. In an
    uninstrumented build, in a [--list] run, and whenever the environment asks
    for nothing, it is exactly [Ran (Driver.execute_and_report … tests)] — same
    transcript, same bytes, same cost.

    [armed] is what a process about to run with a mutant armed owes the inline
    (ppx) runtime — {!Ppx_runtime.enter_armed}, which turns checking read-only
    (Law 16d) and clears the cross-run tables a forked child must not inherit.
    It is an argument and not a call because {!Ppx_runtime} sits {e above} this
    module: depending on it here would be a cycle, and a mutable hook would be
    that same cycle hidden behind a [ref] that a link order could leave unset.
    Both thin drivers pass {!Ppx_runtime.enter_armed}. It is called in each
    forked child before its first test, and once in the parent under
    [WINDTRAP_MUTATE_ARM]; it is never called by a run that arms nothing.

    An unrecognized [WINDTRAP_MUTATE] or [WINDTRAP_MUTATE_LIMIT], asking for the
    loop and an armed mutant at once, and a [WINDTRAP_MUTATE_ARM] that is
    malformed, ambiguous, or {!Windtrap_mutate.Unmatched} within a file this
    executable catalogues, are refusals: the message names the variable or the
    candidates and the result is [Reported 1]. A refusal is never a silently
    defaulted run. The one arming failure that is not a refusal is
    {!Windtrap_mutate.Uncatalogued}, above.

    Inside the loop, arming stays strict: each forked child arms a mutant the
    parent took from {e this} binary's own catalogue, through the byte span
    {!Windtrap_mutate.selector_of_mutant} gives it, so a child that fails to arm
    has hit a bug — it reports an error line, which aborts the whole run without
    a score, rather than running a green suite with nothing armed and calling
    the result a survivor. The leniency above is about a project-level
    instruction from outside the process, and reaches no further than the one
    place that reads it.

    Effects: the union of {!Driver.execute_and_report}'s and, under the loop,
    [fork]/[waitpid]/[pipe]/[setitimer], one scratch log directory per run
    (removed at the end), and one verdict file under
    {!Windtrap_mutate.output_file}. Children never reach [Stdlib]'s exit
    machinery: every exception, fatal included, is caught, reduced to a verdict
    line, and followed by [Unix._exit] — otherwise a child dying of
    [Out_of_memory] would run the coverage at-exit dump against a path resolved
    before the fork and overwrite the parent's [.coverage] (Law 16e). *)

(** {1:signal The instrumentation signal} *)

val instrumented : unit -> bool
(** [instrumented ()] is [true] iff this executable links any instrumented
    module — iff {!Windtrap_mutate.catalogue} is non-empty. A binary with
    mutants registered was necessarily built with the mutation backend, so this
    is what command hints key on: a hint that spelled a [dune exec] without
    [--instrument-with ppx_windtrap.mutate] would have dune rebuild the target
    {e uninstrumented}, and the [arm] line of every survivor block would name a
    command that arms nothing. *)
