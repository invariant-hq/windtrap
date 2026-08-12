(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The run-interception slot: one registration point, consulted by every thin
    driver at run entry.

    The mutation loop wraps whole runs — it announces an armed mutant before any
    output, prints its discovery line after the summary, and forks the suite
    once per mutant after the dry run — so it sits {e above} the drivers' shared
    wiring, and the thin drivers cannot name it without core depending upward.
    This module is the seam that removes the name: the loop installs an
    {!type:interceptor} here at its module load, and the drivers hand it the run
    ({!interceptor}) instead of calling the loop. The loop lives in its own
    library, [windtrap.mutation], and that self-install is the whole mechanism:
    the [dune] stanza linking the library is what arms mutation, explicit and
    greppable — a test stanza's own entry for the standalone runner,
    [ppx_windtrap.runtime]'s dependency for every generated inline runner.

    This is the library's second documented ambient slot, beside {!Run}'s. Same
    defense: it is written once at module load by explicitly linked code, read
    once at run entry, never per-test, and it forecloses no parallelism.

    {b Armed hooks} (Law 16d). A process about to run with a mutant armed owes
    the inline (ppx) runtime two things — read-only checking, and the clearing
    of the cross-run tables a forked child must not inherit
    ([Ppx_runtime.enter_armed]). The runtime sits above the loop just as the
    loop sits above the drivers, so the debt is registered rather than passed:
    the runtime calls {!on_armed} at its module load, and the interceptor fires
    every registered hook, in registration order, in each process that arms —
    each forked child before its first test, and once in the parent under
    [WINDTRAP_MUTATE_ARM]. A run that arms nothing fires nothing.

    The slot and the hooks are deliberately independent: installation and
    registration are module-load acts of two different units, in whichever order
    the link puts them, and the firing side reads {!armed_hooks} at fire time —
    so hooks registered before or after the install are honored alike, and an
    interceptor invoked without an install still honors them. *)

(** {1:slot The slot} *)

(** The type for what an interceptor did with the run. Constructionally
    [Mutate_loop]'s answer, defined here because the drivers that dispatch on it
    must not name the loop. *)
type verdict =
  | Ran of (Runner.outcome, Runner.startup_error) result
      (** The suite ran once, ordinarily — no loop, or a loop that never
          started. The caller finishes its own post-run work on it (JUnit, the
          focus warning, the correction protocol, the exit) exactly as it would
          have on {!Driver.execute_and_report}'s result. *)
  | Reported of int
      (** The interceptor took the process over and has printed everything it
          has to say; the process exits with this code. *)

type interceptor = Driver.t -> Test_tree.t list -> verdict
(** The type for installed interceptors: the run entry the drivers call in place
    of {!Driver.execute_and_report}, over the same spine record with the same
    meaning. *)

val install : interceptor -> unit
(** [install run] fills the slot with [run]. One interceptor exists per process
    — installing is a module-load act of the one library that owns the loop, and
    a later install replaces the slot whole. *)

val interceptor : unit -> interceptor option
(** [interceptor ()] is the installed interceptor, or [None] when no library
    installed one — the drivers then run the suite through
    {!Driver.execute_and_report} unwrapped. *)

(** {1:hooks Armed hooks} *)

val on_armed : (unit -> unit) -> unit
(** [on_armed hook] registers [hook] to run in every process that arms a mutant
    (Law 16d), before the process's first test. Registration is a module-load
    act; hooks are never unregistered and fire in registration order. *)

val armed_hooks : unit -> (unit -> unit) list
(** [armed_hooks ()] is the registered hooks in registration order — the firing
    side's read. It answers whether or not an interceptor is installed, so a
    directly-invoked interceptor still honors every registered hook. *)
