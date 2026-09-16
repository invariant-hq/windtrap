(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The mutation loop: the parent process of a mutation run.

    The facade's [run] calls {!execute_and_report} in place of {!Report.run};
    that call is core windtrap's whole coupling to mutation. The dry run and its
    reach map, the determinism probe, the fork loop, the verdict file and the
    report live here, over the stdlib-only runtime ({!Windtrap_runtime.Mutate}
    for the catalogue, the guard and the reach map, {!Windtrap_runtime.Verdicts}
    for the verdict file). Which mutants a run tests and which one a process
    arms arrive on {!Run.config.mutation}; in a [--list] run and under
    {!Run.No_mutation} this module does nothing.

    Children run one at a time under a per-child deadline; nothing bounds a
    whole run. A site the dry run evaluated only outside a test is recorded as
    unreached. Mutation needs [Unix.fork] and declines by name on Windows. *)

(** {1:running Running} *)

(** The type for what {!execute_and_report} did with the run. *)
type run =
  | Ran of (Run.outcome, Run.startup_error) result
      (** The suite ran once, ordinarily: no loop, or a loop that never started.
          The caller finishes its post-run work (the focus warning, the
          correction protocol, the exit) as on {!Report.run}'s result. *)
  | Reported of int
      (** The mutation run took the process over and has printed everything; the
          process exits with this code. *)

val execute_and_report : suite:string -> Run.config -> Test_tree.t list -> run
(** [execute_and_report ~suite config tests] is {!Report.run} over [config]
    wrapped in the mode [config.mutation] names; under {!Run.No_mutation} it is
    exactly [Ran (Report.run ~suite config tests)]. Forked children run
    {!Run.execute} silently over {!Run.for_subset} of [config] with the reaching
    tests as their allowlist. A process with a mutant armed, a child or the
    parent under [--arm], checks baselines read-only (guarantee 12).

    Exit codes: [Reported 0] when the loop completed, whatever it found (only
    the aggregate, [windtrap mutants], gates on survivors); [Reported 1] with a
    message on [stderr] when it refused to start or could not finish: an [--arm]
    identifier that is malformed, ambiguous or
    {!Windtrap_runtime.Mutate.Unmatched} within a file this executable
    catalogues, a red or empty dry run, a [--mutate] of an executable that links
    no instrumented module or whose prefixes leave it no mutant, a probe
    disagreement, a supervision error, or a child that failed to arm a mutant
    from this binary's own catalogue. Never [2].
    {!Windtrap_runtime.Mutate.Uncatalogued} under [--arm] is not a refusal: one
    line on [stderr], then [Ran] of the ordinary run. Asking for both flags is
    [Cli]'s refusal.

    Effects: {!Report.run}'s and, under the loop, [fork]/[waitpid]/[pipe]/
    [select], [setsid] in each child, [kill] of an expired child's process
    group, one scratch log directory per run (removed at the end), and one
    verdict file under {!Windtrap_runtime.Verdicts.output_file}. Children never
    reach [Stdlib]'s exit machinery: every exception, fatal included, is reduced
    to a verdict line followed by [Unix._exit]. *)
