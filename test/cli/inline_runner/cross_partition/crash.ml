(* An uncaught exception, in its own partition. Never a correction: the
   runner classifies the exception as the test's failure, and a test with
   a failure that is not a baseline mismatch records no correction. The
   payload below matches, so this partition records nothing at all,
   which is what makes it the control for "an update run cannot make a
   crash promotable". *)

let%expect_test "an uncaught exception" =
  print_string "before the raise";
  [%expect {| before the raise |}];
  failwith "boom"
