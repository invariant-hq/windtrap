let%expect_test "if reached" =
  if false then [%expect.if_reached {| never |}];
  [%expect {| |}]
