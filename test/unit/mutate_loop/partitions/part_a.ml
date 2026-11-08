(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Pins [sub] and runs [widen] without pinning it. *)

module Subject = Mutate_loop_subject.Subject

let%test "sub is pinned" = Windtrap.equal Windtrap.int 6 (Subject.sub 10 4)
let%test "widen is run, not pinned" = Windtrap.is_true (Subject.widen 3 4 <> 0)
