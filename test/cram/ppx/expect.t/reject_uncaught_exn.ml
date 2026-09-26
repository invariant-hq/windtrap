let%expect_test "raises" =
  failwith "boom";
  [%expect {| |}]
[@@expect.uncaught_exn {| (Failure boom) |}]
