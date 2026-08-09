(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A suite whose single test terminates only because [step] counts down.
   Unmutated the dry run evaluates the site five times, so the loop hands
   the child a budget of a few thousand hits; mutated, the child spins
   until the guard raises and the test fails. The mutant is killed by its
   budget, not by the clock, and the whole run takes milliseconds. *)

open Windtrap

let rec drain n =
  if n = 0 then 0 else drain (Mutate_loop_spinner.Spinner.step n)

let () =
  run "spin" [ test "counts down to zero" (fun () -> equal int 0 (drain 5)) ]
