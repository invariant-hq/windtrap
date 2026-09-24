(* The generated code names Expect_test_config unqualified. Each rule is
   named by its id in ../../RULES.md and the line of the interface that
   states it: ppx_windtrap.mli (pwt) or expect_test_config.mli (etc). *)

(* E27, pwt:29-32: the definition of Expect_test_config in scope where a
   test is written governs it, so the default governs this test and the
   module below governs the tests after it. *)
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

(* E9, pwt:35-38: the string-extension spelling [{%expect_exact|...|}] is an
   [[%expect_exact]] node too, and reads through the override. *)
let%expect_test "below the override, its sanitizer" =
  print_string "abc";
  [%expect {| ABC |}];
  print_string "exact";
  {%expect_exact|EXACT|}

let%expect_test "below the override, its run" =
  assert !Expect_test_config.running

(* E20, pwt:47-49, etc:34-35: the body of a let%test does not go through
   run. *)
let%test "a let%test body runs outside the override's run" =
  assert (not !Expect_test_config.running)

(* E32, etc:50: a body that calls Windtrap.output itself reads the output
   unsanitized. *)
let%expect_test "Windtrap.output reads the output unsanitized" =
  print_string "abc";
  assert (Windtrap.output () = "abc")

(* E29, pwt:42-43: nothing is checked after the body: a node it never
   reaches, and output written after its last node, fail nothing. *)
let%expect_test "an unreached node and trailing output fail nothing" =
  if Sys.opaque_identity false then [%expect {| never |}];
  print_string "trailing"
