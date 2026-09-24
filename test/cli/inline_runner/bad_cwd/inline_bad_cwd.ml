(* One stale expect payload: the driver (see ./dune) runs it from a
   scratch cwd where the source cannot be read, so no correction can be
   recorded, and the run must stay a loud failure, never a silent pass. *)

let%expect_test "stale payload" =
  print_string "fresh output";
  [%expect {| stale payload |}]
