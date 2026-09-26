(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The library that pins_add.ml and pins_sub.ml disagree about. It has four
   mutation sites, one operator per binding, so a merged report's counts are
   exact. [add] is pinned by one executable and only reached by the other,
   [sub] the other way round, [shared] is reached by both and pinned by
   neither, and [never] is called by neither. *)

let add a b = a + b
let sub a b = a - b
let shared a b = a + b
let never a b = a - b
