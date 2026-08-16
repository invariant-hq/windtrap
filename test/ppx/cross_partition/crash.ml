(* An uncaught exception, in its own partition. Never a correction: the
   runtime resolves the nodes reached before the raise and lets the runner
   classify the exception (Ppx_runtime.run_expect_body's exception branch).
   The payload below matches, so this partition records nothing at all —
   which is what makes it the control for "an update run cannot make a
   crash promotable". *)

let%expect_test "an uncaught exception" =
  print_string "before the raise";
  [%expect {| before the raise |}];
  failwith "boom"
