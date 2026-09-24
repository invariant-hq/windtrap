(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The library the two executables below disagree about. Exactly four
   mutation sites (one arithmetic operator per binding and nothing else
   the instrumenter can reach), so the merged report's counts are exact
   rather than approximately right, and a change to the operator set that
   grew the population here would fail the test loudly instead of quietly
   shifting a number.

   [add] is pinned by one executable and merely reached by the other,
   [sub] the other way round: each executable alone reports a false
   survivor, and the merge is the only thing that removes both. [shared]
   is reached weakly by both and survives everywhere; [never] is called
   by neither. *)

let add a b = a + b
let sub a b = a - b
let shared a b = a + b
let never a b = a - b
