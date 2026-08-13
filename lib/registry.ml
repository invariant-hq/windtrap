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

(* Registration is a module-load act of another package's unit (the
   inline runtime lives in ppx_windtrap), and the firing side reads the
   cell at fire time — so hooks registered before or after the loop's own
   load are honored alike, whatever order the link put the
   initializers in. *)

let hooks : (unit -> unit) list ref = ref []
let on_armed hook = hooks := hook :: !hooks
let armed_hooks () = List.rev !hooks
