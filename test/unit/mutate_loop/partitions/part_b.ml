(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Pins [widen] and runs [sub] without pinning it. *)

module Subject = Mutate_loop_subject.Subject

let%test "widen is pinned" = Windtrap.equal Windtrap.int 7 (Subject.widen 3 4)
let%test "sub is run, not pinned" = Windtrap.is_true (Subject.sub 10 4 <> 0)
