(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The default ambient [Expect_test_config], re-exported as a top-level module
    for [let%expect_test]-generated code (RFC compat mechanism (b)). See
    [lib/expect_test_config.mli] for the contract, including how a user module
    of the same name shadows this one and why monadic (Async-style) configs fail
    to compile. *)

include module type of Windtrap__Expect_test_config
