(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A failed subtest lets the body go on to the stale node. *)

let%expect_test "an assertion failure beside a stale payload" =
  Windtrap.subtest "the assertion" (fun () -> Windtrap.equal Windtrap.int 1 2);
  print_string "fresh output";
  [%expect {| stale payload |}]
