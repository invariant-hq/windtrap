(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The mutation loop: the parent process of a mutation run.

    Core windtrap's whole coupling to mutation is one dispatch call — the two
    thin drivers call {!execute_and_report} at run entry in place of
    [Driver.execute_and_report] — plus one read-only flag on the expect
    correction path ([Ppx_runtime.enter_armed], Law 16d, registered in
    {!on_armed} and fired here, never a dependency in either direction).
    Everything else lives here and in the stdlib-only runtime
    {!Windtrap_mutate}: the dry run and its reach map, the determinism probe,
    the forced-fail check, the fork loop, the verdict file, and the report.

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
    — a survivor never fails a build in this release — and [1] when it refused
    to start or could not finish, each with its own message on [stderr]. Never
    [2]: "nothing ran" is a statement about a test selection, and a mutation run
    does not make one.

    {b Not in this slice.} Children run one at a time, and the per-child
    deadline is the only clock: nothing bounds a whole run, so a parent-side
    pathology is stopped by the user rather than by the tool. There is no
    not-armable table either, so a site the dry run evaluated only {e outside} a
    test (module initialization, a fixture release) is listed as unreached
    rather than as not armable — both are "no test evaluates this" and neither
    is forked, so the score is right and only the offered remedy is imprecise.
    Mutation needs [Unix.fork] and declines by name on Windows. *)

(** {1:running Running} *)

(** The type for what {!execute_and_report} did with the run. *)
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

val execute_and_report : Driver.t -> Test_tree.t list -> run
(** [execute_and_report spine tests] is the mutation-aware run entry:
    [Driver.execute_and_report] over the same spine record with the same
    meaning, wrapped in whichever mode this process is in. When the environment
    asks for nothing it is exactly [Ran (Driver.execute_and_report spine tests)]
    — same transcript, same bytes, same cost.

    What a process about to run with a mutant armed owes the inline (ppx)
    runtime — [Ppx_runtime.enter_armed], which turns checking read-only (Law
    16d) and clears the cross-run tables a forked child must not inherit —
    arrives through {!on_armed} rather than as an argument or a dependency: the
    runtime sits {e above} this module and registers at its module load,
    whatever the link order. The hooks fire in each forked child before its
    first test, and once in the parent under [WINDTRAP_MUTATE_ARM]; never in a
    run that arms nothing.

    {b Refusals}, each [Reported 1] with its own message naming the variable or
    the candidates, never a silently defaulted run: an unrecognized
    [WINDTRAP_MUTATE]; asking for the loop and an armed mutant at once; a
    [WINDTRAP_MUTATE_ARM] that is malformed, ambiguous, or
    {!Windtrap_mutate.Unmatched} within a file this executable catalogues; a red
    or empty dry run; a probe disagreement; and a supervision error. The one
    arming failure that is {e not} a refusal is {!Windtrap_mutate.Uncatalogued}
    — one identifier is handed to every test executable of a project at once,
    and all but one of them were built from other sources.

    Arming inside the loop stays strict all the same: a child arms a mutant the
    parent took from {e this} binary's own catalogue, so a child that fails to
    arm has hit a bug, and it reports an error line that aborts the run without
    a score rather than running a green suite with nothing armed and calling the
    result a survivor.

    Effects: the union of [Driver.execute_and_report]'s and, under the loop,
    [fork]/[waitpid]/[pipe]/[select], [setsid] in each child, [kill] of an
    expired child's process group, one scratch log directory per run (removed at
    the end), and one verdict file under {!Windtrap_mutate.output_file}.
    Children never reach [Stdlib]'s exit machinery: every exception, fatal
    included, is caught, reduced to a verdict line, and followed by [Unix._exit]
    — otherwise a child dying of [Out_of_memory] would run the coverage at-exit
    dump against a path resolved before the fork and overwrite the parent's
    [.coverage] (Law 16e). *)

(** {1:armed The armed hooks} *)

val on_armed : (unit -> unit) -> unit
(** [on_armed hook] registers [hook] to run in every process that arms a mutant
    (Law 16d), before the process's first test — in each forked child, and once
    in the parent under [WINDTRAP_MUTATE_ARM]. A run that arms nothing fires
    nothing.

    The one cross-package registration point, and the library's second ambient
    cell beside {!Run}'s slot: the inline (ppx) runtime lives {e above} this
    module and cannot be named from it, so what a process about to arm owes it —
    read-only checking, and the clearing of the cross-run tables a forked child
    must not inherit ([Ppx_runtime.enter_armed]) — is registered rather than
    passed. Registration is a module-load act; hooks are never unregistered,
    fire in registration order, and are read at fire time, so registration order
    and link order need not agree. *)

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
    and the two reports cannot drift in data the way [Render] already stops them
    drifting in layout.

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
