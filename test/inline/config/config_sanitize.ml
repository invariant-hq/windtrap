(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The generated code applies [sanitize] to what each node reads. *)

module Expect_test_config = struct
  include Expect_test_config

  (* Digits become N and spaces _, so a comparison of the raw output, or of
     the output after whitespace normalization, fails. *)
  let sanitize s =
    String.map (function '0' .. '9' -> 'N' | ' ' -> '_' | c -> c) s
end

let%expect_test "sanitize rewrites the compared text" =
  print_string "run 42";
  [%expect {| run_NN |}]

let%expect_test "sanitize sees the output before whitespace is normalized" =
  print_string "  x";
  [%expect {| __x |}]

let%expect_test "an [%expect.output] reads the sanitized text" =
  print_string "7";
  let captured = [%expect.output] in
  Windtrap.equal Windtrap.string "N" captured

module Never_runs = struct
  module Expect_test_config = struct
    include Expect_test_config

    let run _ = ()
  end

  (* A body with a node fails, as that node is never reached; see
     ../correction/unreached.ml. *)
  let%expect_test "a run that never calls a body without a node checks nothing"
      =
    print_string "anything"
end
