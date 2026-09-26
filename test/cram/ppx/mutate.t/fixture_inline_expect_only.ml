(* A file that holds an extension node named [expect_test], and none named
   [test], declares inline tests and is returned as parsed. *)

let add a b = a + b

let%expect_test "adds" =
  print_int (add 1 2);
  [%expect {| 3 |}]
