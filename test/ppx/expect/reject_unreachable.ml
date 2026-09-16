(* Fixture: [%expect.unreachable] is not implemented and must be
   rejected at expansion (RFC compat mechanism (a)). *)

let%expect_test "unreachable" =
  if false then [%expect.unreachable];
  [%expect {| |}]
