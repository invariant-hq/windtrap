(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let%expect_test "a stale payload" =
  print_string "fresh output";
  [%expect {| stale payload |}]
