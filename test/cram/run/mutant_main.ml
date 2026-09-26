(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The second stanza of broadcast.t: one test over a module instrumented in
   every build, so that WINDTRAP_MUTATE finds a mutant in this suite and
   none in the other. *)

open Windtrap

let () =
  exit
    (run "mutant"
       [ test "adds" (fun () -> equal int 4 (Mutant_subject.add 2 2)) ])
