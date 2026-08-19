(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The mutation loop: the parent process of a mutation run.

    Core windtrap's whole coupling to mutation is one dispatch call:
    the two thin drivers call {!execute_and_report} at run entry in place
    of [Driver.execute_and_report] — plus one read-only flag on the
    expect correction path ([Ppx_runtime.enter_armed], Law 16d, registered
    in [Registry.on_armed] and fired here, never a dependency in either
    direction). Everything else lives here and in the stdlib-only runtime
    {!Windtrap_mutate}: the dry run and its reach map, the determinism
    probe, the forced-fail check, the fork loop, the verdict file, and
    the report, projected into [Render]'s sections.

    {b Why this module wraps the run rather than being called around it.} The
    three things a mutation run must do — announce an armed mutant {e before}
    any other output, print the discovery line {e after} the summary, and run
    the suite again once per mutant {e after} the dry run — bracket the run on
    both sides. A seam that only fired at run entry would need a second seam at
    run end, and a seam that only fired at run end could not announce. So the
    run is this module's argument, not its caller's: it is one call, in one
    place, and there is nothing for a runner to get out of order.

    {b Modes} (from [Cli.mutation], and the catalogue):

    - No mutant registered in this executable: the run is an ordinary run and
      nothing here does anything.
    - [WINDTRAP_MUTATE] off or unset, no [WINDTRAP_MUTATE_ARM]: the ordinary run
      plus the discovery line — what this build {e could} do, and what to type.
      It counts the mutants the loop would test, so a site dismissed by
      [[@mutate off]] is offered nowhere and appears in no denominator. Adding
      the backend never makes a test run longer than it was.
    - [WINDTRAP_MUTATE_ARM] set and naming a site {e this} executable
      catalogues: one mutant armed for the whole process, the announcement first
      (Law 16b), read-only checking (Law 16d), and one closing line after the
      transcript — [mutant killed.] when a test failed, and otherwise
      [mutant survived: …] or [mutant not evaluated: …] by whether the armed
      site was evaluated, because a green transcript alone cannot tell "the
      tests prove nothing about this site" from "no selected test ran the
      line". A run that exited [2] gets no closing line: a selection that
      matched nothing says something about the filter and nothing about the
      mutant (Law 16c).
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
    - [WINDTRAP_MUTATE=1]: the loop below, which takes the process over and
      reports on its own.
    - [WINDTRAP_MUTATE=admit]: the admission machine — the run's ordinary test
      selection becomes an admission set, the loop arms only faults those tests
      reach, and every selected test is ruled ADMITTED (it named the fault it
      kills, with its cause when the kill was not an ordinary failure),
      UNJUSTIFIED (it watched faults on its lines and never failed — a failure
      block, and the run exits [1]), or NO SITES (it reaches nothing the
      operators can break — a stated fact, never a finding). Selection is the
      author's, never inferred: [-f]/[-e], the tag knobs, [--failed] and an
      in-source focus designate; [--shard] and [--quick] do not. A run that
      makes no selection designates every test it executed — the ask an alias
      spells where a per-invocation filter cannot — and says so in one
      [stderr] line naming the survey as the other question. A selection
      matching nothing refuses under the standalone runner and, under the
      inline runner's project-wide invocation, declines in one [stderr] line
      and lets the ordinary run stand — the {!Windtrap_mutate.Uncatalogued}
      softness, for the same reason; a run that designates everything and
      still executes nothing refuses in its own words, naming the suite rather
      than a filter no one set. Per selected test the loop tries the
      undismissed faults it reaches, most-run first, at most
      [WINDTRAP_MUTATE_TRY] (default 25, [0] for all): a fault counts as tried
      only when the test ran to a pass or fail outcome under it — a skip
      watched nothing, and a shared fork the test merely rode along in charges
      nothing, though a kill observed there still admits. Batches are
      union-scheduled across the selection and children run without bail,
      reporting one incremental line per test event so a crash or a deadline
      kill is attributable and every earlier outcome kept. The forced-fail
      check does not apply, the determinism probe is skipped when nothing
      reaches a site, and {b an admission run persists nothing}: no verdict
      file is written, none is read, and an existing one is left byte-intact.

    {b The loop.}

    + {b Dry run.} The suite runs once, normally, nothing armed, printing its
      ordinary summary line. The reach map is built from
      [Driver.execute_and_report]'s [?on_event] — a {e second} subscriber
      composed after the transcript's, never replacing it. A red dry run, an
      empty one, or one with no mutant to test aborts: a score over a suite that
      does not pass is not a score.
    + {b Determinism probe.} One unarmed fork re-runs the suite. Disagreement
      with the dry run aborts, naming the tests that disagreed — mutation
      results over a non-deterministic suite are not a weaker number, they are
      not a number.
    + {b Forced-fail check.} The mutant the most tests reach is armed and those
      tests run. If nothing fails the run warns above its report, because the
      commonest misconfiguration — the backend on the test executable but not
      on the library under test — produces exactly that, and it costs one child
      to rule out. Its verdict is kept, so the check is free for a suite that
      passes it, and it is a warning rather than a refusal because a
      legitimately weak file produces the same signature and is owed its
      score.
    + {b The loop.} One [fork] per reached, non-dismissed mutant. The child
      arms, runs that mutant's reaching tests under [bail = Some 1], and writes
      one line on a pipe. {b The verdict never rides an exit code}: the inline
      runner returns [0] for a corrections-covered expect failure, so an exit
      code cannot tell a killed mutant from a survivor. Each child runs in a
      session of its own under a deadline derived from the dry run — its wall
      clock, which bounds any child's fork and module initialization, plus ten
      times the scheduled tests' own measured time, with a one-second floor
      and never a knob. A child that exceeds it is killed with its whole
      process group — anything a test spawned included — and the mutant is
      scored killed: a mutant that blocks (a flipped comparison
      deadlocking a pipe reader, where the runtime's runaway hit-count budget
      sees nothing) made the suite hang, and a hang is a noticed change on the
      crash kill's own reasoning. In admission the kill is attributed to the
      one test that started and never reported, admitted with cause
      [timeout]; every outcome the child had already delivered is kept.
    + {b Report.} One verdict file under [_build/_mutants] (so [windtrap mutate]
      can merge the several test executables that cover one library) and the
      report through [Render.mutation_report]. A run whose selection narrows
      the suite — a filter, an exclude, a tag selection, a shard, [--quick],
      [--failed], or an in-source focus — still completes and reports, but
      writes no verdict file and says so in one line: its verdicts are relative
      to the selection, the file carries no partial-run marking, and a written
      one would stand in the project merge as this executable's whole answer.
      [WINDTRAP_MUTATE_ONLY] does not narrow: it changes which mutants exist,
      not which tests judge them, so a scoped run's records are project-true for
      this executable and still write.

    {b Not in this slice.} Children run one at a time, and the per-child
    deadline is the only clock: there is no ceiling on a whole run, so a
    parent-side pathology is stopped by the user rather than by the tool.
    There is no not-armable table, so a site the dry run only evaluated
    {e outside} a test
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
    does not make one. Exception, per the amendment's reserved survivor-driven
    clause: an [admit] run — which judges a test selection at its author's
    request — additionally exits [1] when a selected test killed nothing it
    reached (any UNJUSTIFIED ruling); NO SITES alone is never red. *)

(** {1:running Running} *)

type run =
  | Ran of (Runner.outcome, Runner.startup_error) result
      (** The suite ran once, ordinarily — no loop, or a loop that never
          started. The caller finishes its own post-run work on it (JUnit, the
          focus warning, the correction protocol, the exit) exactly as it would
          have on [Driver.execute_and_report]'s result. *)
  | Reported of int
      (** The mutation run took the process over and has printed everything it
          has to say. Nothing about the underlying run is the caller's business
          — a loop's dry run is not the process's verdict — and the process
          exits with this code. *)
(** The type for what {!execute_and_report} did with the run. *)

val execute_and_report : Driver.t -> Test_tree.t list -> run
(** [execute_and_report spine tests] is the mutation-aware run entry:
    [Driver.execute_and_report] over the same spine record with the same
    meaning, wrapped in whichever of the modes above this process is in. In an
    uninstrumented build, in a [--list] run, and whenever the environment asks
    for nothing, it is exactly [Ran (Driver.execute_and_report spine tests)] —
    same transcript, same bytes, same cost. The loop threads [spine] whole,
    replacing [spine.config] per child (the pruned selection, the child's own
    log directory, read-only checking); the children run through
    [Driver.plan]/[Driver.execute] — a session with no reporting.

    What a process about to run with a mutant armed owes the inline (ppx)
    runtime — [Ppx_runtime.enter_armed], which turns checking read-only
    (Law 16d) and clears the cross-run tables a forked child must not inherit —
    arrives through [Registry.armed_hooks] rather than as an argument or a
    dependency: the runtime sits {e above} this module and registers at its
    module load, whatever the link order, and this module fires the registered
    hooks in registration order. They are fired in each forked child before its
    first test, and once in the parent under [WINDTRAP_MUTATE_ARM]; they are
    never fired by a run that arms nothing.

    An unrecognized [WINDTRAP_MUTATE] or [WINDTRAP_MUTATE_LIMIT], asking for the
    loop and an armed mutant at once, and a [WINDTRAP_MUTATE_ARM] that is
    malformed, ambiguous, or {!Windtrap_mutate.Unmatched} within a file this
    executable catalogues, are refusals: the message names the variable or the
    candidates and the result is [Reported 1]. A refusal is never a silently
    defaulted run. The one arming failure that is not a refusal is
    {!Windtrap_mutate.Uncatalogued}, above.

    Inside the loop, arming stays strict: each forked child arms a mutant the
    parent took from {e this} binary's own catalogue, by that mutant's own
    identifier, so a child that fails to arm
    has hit a bug — it reports an error line, which aborts the whole run without
    a score, rather than running a green suite with nothing armed and calling
    the result a survivor. The leniency above is about a project-level
    instruction from outside the process, and reaches no further than the one
    place that reads it.

    Effects: the union of [Driver.execute_and_report]'s and, under the loop,
    [fork]/[waitpid]/[pipe]/[select], [setsid] in each child and
    [kill] of an expired child's process group, one scratch log directory per
    run
    (removed at the end), and one verdict file under
    {!Windtrap_mutate.output_file}. Children never reach [Stdlib]'s exit
    machinery: every exception, fatal included, is caught, reduced to a verdict
    line, and followed by [Unix._exit] — otherwise a child dying of
    [Out_of_memory] would run the coverage at-exit dump against a path resolved
    before the fork and overwrite the parent's [.coverage] (Law 16e). *)

(** {1:report The report projection} *)

val render_data :
  resolve_source:(string -> string option) ->
  loc_of:(string -> Loc.t option) ->
  duration:float option ->
  seed:Seed.seed option ->
  siblings:bool ->
  total:int ->
  Windtrap_mutate.t ->
  Render.mutation
(** [render_data ~resolve_source ~loc_of ~duration ~seed ~siblings ~total t] is
    the report [t] draws: survivor blocks ordered by witness count descending,
    the unreached lines grouped by file, and the counts. Everything comes from
    the records, which is why they carry the renderings — so this projection is
    also the one [windtrap mutate] makes over verdict files it did not write,
    and the two reports cannot drift in data the way [Render] already stops
    them drifting in layout.

    [resolve_source file] is the file's text for the excerpt row, [None] when it
    cannot be read; [loc_of test] is a witness's declaration site, [None] for a
    caller that does not link the test tree. [total] is the population the score
    reads against — the catalogue minus the dismissed for a run, the merged
    record count for the merge. *)

(** {1:signal The instrumentation signal} *)

val instrumented : unit -> bool
(** [instrumented ()] is [true] iff this executable links any instrumented
    module — iff {!Windtrap_mutate.catalogue} is non-empty. A binary with
    mutants registered was necessarily built with the mutation backend, so this
    is what command hints key on: a hint that spelled a [dune exec] without
    [--instrument-with ppx_windtrap.mutate] would have dune rebuild the target
    {e uninstrumented}, and the [arm] line of every survivor block would name a
    command that arms nothing. *)
