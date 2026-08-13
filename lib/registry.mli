(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The armed hooks: the one cross-package registration point (Law 16d).

    A process about to run with a mutant armed owes the inline (ppx) runtime
    two things — read-only checking, and the clearing of the cross-run tables a
    forked child must not inherit ([Ppx_runtime.enter_armed]). The runtime
    lives in [ppx_windtrap], above the core, so [Mutate_loop] cannot name it;
    the debt is registered rather than passed: the runtime calls {!on_armed} at
    its module load, and the loop fires every registered hook, in registration
    order, in each process that arms — each forked child before its first test,
    and once in the parent under [WINDTRAP_MUTATE_ARM]. A run that arms nothing
    fires nothing.

    This is the library's second documented ambient cell, beside {!Run}'s slot.
    Same defense: it is written at module load by explicitly linked code, read
    only when a mutant arms, never per-test, and it forecloses no
    parallelism. *)

val on_armed : (unit -> unit) -> unit
(** [on_armed hook] registers [hook] to run in every process that arms a mutant
    (Law 16d), before the process's first test. Registration is a module-load
    act; hooks are never unregistered and fire in registration order. *)

val armed_hooks : unit -> (unit -> unit) list
(** [armed_hooks ()] is the registered hooks in registration order — the firing
    side's read, taken at fire time so registration order and link order need
    not agree. *)
