let%expect_test "n" =
  print_string "x";
  ignore [%expect.output "x"]

(* E13, pwt:80-81: an [[%expect.output]] with a payload is refused. *)
