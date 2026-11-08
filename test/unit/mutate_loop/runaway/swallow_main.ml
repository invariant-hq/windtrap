(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The runaway fixture's site, with the code under test catching every
   exception around it, as a parser that reads a raising parse as a
   rejected input does. Mutated, the child spins until the guard raises,
   the handler turns the exception into the end of the count, and the test
   passes: the window in which the site ran away still kills the mutant. *)

open Windtrap

let step n = try Mutate_loop_spinner.Spinner.step n with _ -> 0
let rec drain n = if n = 0 then 0 else drain (step n)

let () =
  exit
  @@ run "swallow"
       [ test "counts down to zero" (fun () -> equal int 0 (drain 5)) ]
