(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Exactly one mutation site, and arming it turns a terminating loop into a
   non-terminating one. That is the hang the runtime's runaway hit-count
   budget exists for, and in this slice it is the only protection against
   it: the whole-loop [Unix.setitimer] has a sixty-second floor, and the
   per-mutant deadline is a later release.

   The loop itself lives in the uninstrumented runner beside this file, so
   that its [=] comparison is not a second site here. *)

let step n = n - 1
