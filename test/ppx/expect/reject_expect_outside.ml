(* Fixture: [%expect] outside a let%expect_test body is an error, not a
   silently unexpanded node. *)

let f () = [%expect {| nothing |}]
