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

(* Ambient-reading wrappers, exactly the facade's pattern: read the one
   documented slot, dispatch on explicit state. Semantics live in Run and
   Capture. *)

let add_failure failure = Run.add_failure (Run.current_frame ()) failure
let current_path () = Run.path (Run.current_frame ())
let failure_count () = List.length (Run.failures (Run.current_frame ()))
let captured_output ?pos () = Capture.output ?pos (Run.capture (Run.current ()))
