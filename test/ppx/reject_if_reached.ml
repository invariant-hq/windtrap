(* Fixture: [%expect.if_reached] is not implemented and must be
   rejected at expansion (RFC compat mechanism (a)). *)

let%expect_test "if reached" =
  if false then [%expect.if_reached {| never |}];
  [%expect {| |}]
