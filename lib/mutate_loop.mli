(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The mutation loop: the parent process of a mutation run.

    Core windtrap's whole coupling to mutation is one dispatch call — the
    facade's [run] calls {!execute_and_report} at run entry in place of
    {!Report.run}. Everything else lives here and in the stdlib-only runtime —
    {!Windtrap_runtime.Mutate} for the catalogue, the guard and the reach map,
    {!Windtrap_runtime.Verdicts} for the verdict file: the dry run and its reach
    map, the determinism probe, the fork loop, the verdict file, and the report.
    The runtime reads no environment: which mutants a run tests (the scope,
    source-path prefixes) and which one a process arms are read by [Cli] and
    [Env] and applied here.

    {b Why this module wraps the run rather than being called around it.} The
    two things a mutation run must do — announce an armed mutant {e before} any
    other output, and run the suite again once per mutant {e after} the dry run
    — bracket the run on both sides. A seam that only fired at run entry would
    need a second seam at run end, and a seam that only fired at run end could
    not announce. So the run is this module's argument, not its caller's: it is
    one call, in one place, and there is nothing for a runner to get out of
    order.

    [Cli.mutation] decides which mode this process is in;
    [doc/manual/mutation.md] is the chapter that teaches them, and Law 16 in
    [doc/dev/architecture.md] is the durable record of what each owes. In an
    uninstrumented build, in a [--list] run, and whenever the environment asks
    for nothing, this module does nothing at all.

    {b Exit codes} (Law 16e). [0] when the loop completed, {e whatever it found}
    — a survivor is one suite's view, and only the aggregate
    ([windtrap mutants]) gates on survivors — and [1] when it refused to start
    or could not finish, each with its own message on [stderr]. Never [2]:
    "nothing ran" is a statement about a test selection, and a mutation run does
    not make one.

    {b Not in this slice.} Children run one at a time, and the per-child
    deadline is the only clock: nothing bounds a whole run, so a parent-side
    pathology is stopped by the user rather than by the tool. There is no
    not-armable table either, so a site the dry run evaluated only {e outside} a
    test (module initialization, a fixture release) is recorded as unreached
    rather than as not armable — both are "no test evaluates this" and neither
    is forked, so the score is right and only the offered remedy is imprecise.
    Mutation needs [Unix.fork] and declines by name on Windows. *)

(** {1:running Running} *)

(** The type for what {!execute_and_report} did with the run. *)
type run =
  | Ran of (Run.outcome, Run.startup_error) result
      (** The suite ran once, ordinarily — no loop, or a loop that never
          started. The caller finishes its own post-run work on it (the focus
          warning, the correction protocol, the exit) exactly as it would have
          on {!Report.run}'s result. *)
  | Reported of int
      (** The mutation run took the process over and has printed everything it
          has to say. Nothing about the underlying run is the caller's business
          — a loop's dry run is not the process's verdict — and the process
          exits with this code. *)

val execute_and_report : suite:string -> Run.config -> Test_tree.t list -> run
(** [execute_and_report ~suite config tests] is the mutation-aware run entry:
    {!Report.run} over the same configuration with the same meaning, wrapped in
    whichever mode this process is in. When the environment asks for nothing it
    is exactly [Ran (Report.run ~suite config tests)] — same transcript, same
    bytes, same cost. Forked children run {!Run.execute} silently over
    {!Run.for_subset} of [config] and the reaching tests as their allowlist.

    A process with a mutant armed checks baselines read-only (Law 16d): its
    run's [Run.config.baseline] is [Baseline.Check], in each forked child and in
    the parent under [WINDTRAP_MUTATE_ARM], so no correction is recorded and
    nothing reaches [dune promote] from a mutated run.

    {b Refusals}, each [Reported 1] with its own message naming the variable or
    the candidates, never a silently defaulted run: an unrecognized
    [WINDTRAP_MUTATE]; asking for the loop and an armed mutant at once; a
    [WINDTRAP_MUTATE_ARM] that is malformed, ambiguous, or
    {!Windtrap_runtime.Mutate.Unmatched} within a file this executable
    catalogues; a red or empty dry run; an uninstrumented executable, or a scope
    that leaves it no mutant; a probe disagreement; and a supervision error. The
    one arming failure that is {e not} a refusal is
    {!Windtrap_runtime.Mutate.Uncatalogued} — one identifier is handed to every
    test executable of a project at once, and all but one of them were built
    from other sources.

    Arming inside the loop stays strict all the same: a child arms a mutant the
    parent took from {e this} binary's own catalogue, so a child that fails to
    arm has hit a bug, and it reports an error line that aborts the run without
    a score rather than running a green suite with nothing armed and calling the
    result a survivor.

    Effects: the union of {!Report.run}'s and, under the loop,
    [fork]/[waitpid]/[pipe]/[select], [setsid] in each child, [kill] of an
    expired child's process group, one scratch log directory per run (removed at
    the end), and one verdict file under
    {!Windtrap_runtime.Verdicts.output_file}. Children never reach [Stdlib]'s
    exit machinery: every exception, fatal included, is caught, reduced to a
    verdict line, and followed by [Unix._exit] — otherwise a child dying of
    [Out_of_memory] would run the coverage at-exit dump against a path resolved
    before the fork and overwrite the parent's [.coverage] (Law 16e). *)
