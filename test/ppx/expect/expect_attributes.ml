(* The attributes of a test and of its nodes. Each rule is named by its id in
   RULES.md and the line of ppx_windtrap.mli that states it. *)

(* E7, pwt:26-27: every attribute of the name but [[@tags]], and every
   attribute of the binding, [[@@tags]] included, is dropped: the test has
   the tags ["kept"] alone. *)
let%expect_test ("dropped" [@tags "kept"] [@other]) = () [@@tags "dropped"]

(* E11, pwt:41: each node's attributes are carried onto the expression that
   replaces it. *)
let%expect_test "carried" =
  print_string "x";
  [%expect {| x |}] [@carried];
  ignore ([%expect.output] [@carried_output])
