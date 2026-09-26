(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Each body ends on a failing assertion, in tail position as written. *)

let%test "a let%test body" =
  let two = 1 + 1 in
  Windtrap.(equal int 1 two)

let%expect_test "a let%expect_test body" =
  print_string "x";
  [%expect {| x |}];
  Windtrap.is_true false
