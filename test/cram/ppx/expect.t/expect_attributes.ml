(* The attributes of a test and of its nodes. *)

(* Every attribute of the name but [[@tags]], and every attribute of the
   binding, [[@@tags]] included, is dropped: the test has the tags ["kept"]
   alone. *)
let%expect_test ("dropped" [@tags "kept"] [@other]) = () [@@tags "dropped"]

(* Each node's attributes are carried onto the expression that replaces it. *)
let%expect_test "carried" =
  print_string "x";
  [%expect {| x |}] [@carried];
  ignore ([%expect.output] [@carried_output])
