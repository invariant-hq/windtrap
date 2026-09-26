let%expect_test "bad payload" =
  print_string "x";
  [%expect 42]
