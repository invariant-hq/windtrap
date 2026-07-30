(* Fixture: an [%expect] payload must be a string literal. *)

let%expect_test "bad payload" =
  print_string "x";
  [%expect 42]
