(* Fixture: ppx_expect's [@@expect.uncaught_exn] is not implemented and
   must be rejected at expansion (RFC compat mechanism (a)). *)

let%expect_test "raises" =
  failwith "boom";
  [%expect {| |}]
[@@expect.uncaught_exn {| (Failure boom) |}]
