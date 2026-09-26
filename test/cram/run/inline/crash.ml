(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The payload matches, so the exception is the test's one failure. *)

let%expect_test "an uncaught exception" =
  print_string "before the raise";
  [%expect {| before the raise |}];
  failwith "boom"
