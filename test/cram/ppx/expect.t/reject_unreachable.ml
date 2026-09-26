let%expect_test "unreachable" =
  if false then [%expect.unreachable];
  [%expect {| |}]
