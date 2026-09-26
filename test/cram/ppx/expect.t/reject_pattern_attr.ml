let%expect_test ("named" [@expect.uncaught_exn {| boom |}]) =
  print_string "x";
  [%expect {| x |}]
