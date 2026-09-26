let%expect_test "n" =
  print_string "x";
  ignore [%expect.output "x"]
