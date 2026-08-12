(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Not mutated. This module is part of the machinery a mutation run uses
   to judge mutants — the scheduler, the ambient run state, the reporting
   spine, the loop itself — so a mutant here is armed inside the process
   that is supposed to detect it. The failure mode is not a false
   survivor but a hang or a corrupted verdict: a mutated bail counter or
   timeout does not fail the reaching tests, it stops them from
   finishing. Coverage still measures these files; only mutation is off.
   Everything below the scheduler — the verbs, the generators, the
   diffing, the renderers — is mutated. *)
[@@@mutate exclude_file]

(* The slot and the hooks are two independent cells on purpose:
   installation and registration are module-load acts of two different
   units — the loop's owner installs, the inline runtime registers — in
   whichever order the link puts them, and the firing side reads the hook
   cell at fire time. A hook list carried inside the installed value
   would lose every hook registered before the install. *)

type verdict =
  | Ran of (Runner.outcome, Runner.startup_error) result
  | Reported of int

type interceptor = Driver.t -> Test_tree.t list -> verdict

let slot : interceptor option ref = ref None
let hooks : (unit -> unit) list ref = ref []
let install run = slot := Some run
let interceptor () = !slot
let on_armed hook = hooks := hook :: !hooks
let armed_hooks () = List.rev !hooks
