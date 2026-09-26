(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* An override of run must call f once. One that calls it twice runs every
   expectation of the body twice. The one that never calls it is in
   ../correction/unreached.ml. *)

module Twice = struct
  let sanitized = ref 0

  module Expect_test_config = struct
    include Expect_test_config

    let run f =
      sanitized := 0;
      f ();
      f ();
      Windtrap.equal Windtrap.int 2 !sanitized

    let sanitize s =
      incr sanitized;
      s
  end

  let%expect_test "an override that calls the body twice runs its node twice" =
    print_string "twice";
    [%expect {| twice |}]
end
