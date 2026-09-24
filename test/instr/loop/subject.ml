(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The code under mutation. Exactly five sites (one per verdict the loop
   must produce, plus one dismissed) and nothing else the instrumenter
   can reach: no comparison in a boolean context, no connective, no
   toplevel arithmetic. The suite's counts are therefore exact rather than
   approximately right, and a change to the operator set that grew the
   population here would fail the tests loudly instead of quietly
   shifting a number. *)

(* Killed: three tests pin the result. *)
let sub a b = a - b

(* Survived: two tests run it and neither depends on what it does. *)
let widen a b = a + b

(* Unreached: nothing calls it. *)
let orphan a b = a + b

(* Killed by a crash: the [crash] fixture's test leaves through
   [Unix._exit] when this answer changes, so the child dies without
   writing a verdict line and the parent has to conclude one from the
   silence (child hygiene). *)
let crasher a b = a - b

(* Dismissed, and reached: a test below runs it and does not pin it, so
   the only thing keeping it out of the report is [@mutate off]. Drop the
   dismissal filter and it becomes a second survivor and a fifth
   denominator, which is exactly what the reader dismissed it to stop. *)
let dismissed a b = (a + b) [@mutate off "the boundary is unobservable"]
