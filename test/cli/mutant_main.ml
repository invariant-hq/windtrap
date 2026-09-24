(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The suite instrumented.t drives: one test over an instrumented module,
   so that a mutation run and an armed run happen in a real process. *)

open Windtrap

let () =
  exit
    (run "mutant"
       [ test "adds" (fun () -> equal int 4 (Mutant_subject.add 2 2)) ])
