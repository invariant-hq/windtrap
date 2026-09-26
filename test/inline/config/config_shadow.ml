(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The generated code names Expect_test_config unqualified. *)

(* The definition of Expect_test_config in scope where a test is written governs
   it, so the default governs this test and the module below governs the tests
   after it. *)
let%expect_test "above the override, the default sanitizer" =
  print_string "abc";
  [%expect {| abc |}]

module Expect_test_config = struct
  include Expect_test_config

  let running = ref false

  let run f =
    running := true;
    Fun.protect ~finally:(fun () -> running := false) f

  let sanitize = String.uppercase_ascii
end

(* The string-extension spelling [{%expect_exact|...|}] is an [[%expect_exact]]
   node too, and reads through the override. *)
let%expect_test "below the override, its sanitizer" =
  print_string "abc";
  [%expect {| ABC |}];
  print_string "exact";
  {%expect_exact|EXACT|}

let%expect_test "below the override, its run" =
  Windtrap.is_true !Expect_test_config.running

(* The body of a let%test does not go through run. *)
let%test "a let%test body runs outside the override's run" =
  Windtrap.is_false !Expect_test_config.running

(* A body that calls Windtrap.output itself reads the output unsanitized. *)
let%expect_test "Windtrap.output reads the output unsanitized" =
  print_string "abc";
  Windtrap.equal Windtrap.string "abc" (Windtrap.output ())
