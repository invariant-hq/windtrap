(* One stale expect payload: the runner records a correction for this
   file, and the driver (see ./dune) runs it from a scratch cwd where
   the source cannot be read — so the correction cannot be written, and
   the run must stay a loud failure, never a silent pass. *)

let%expect_test "stale payload" =
  print_string "fresh output";
  [%expect {| stale payload |}]
