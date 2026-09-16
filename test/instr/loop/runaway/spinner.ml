(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Exactly one mutation site, and arming it turns a terminating loop into a
   non-terminating one. That is the hang the runtime's runaway hit-count
   budget exists for, and the budget must catch it before the per-child
   deadline waits out its one-second floor: a spin burns hits, and a
   counted guard is a diagnosis where a clock is a mop.

   The loop itself lives in the uninstrumented runner beside this file, so
   that its [=] comparison is not a second site here. *)

let step n = n - 1
