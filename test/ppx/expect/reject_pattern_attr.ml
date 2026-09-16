(* Fixture: an expect-family attribute on the test-name pattern must be
   rejected (RFC compat mechanism (a)) — never silently dropped with the
   discarded pattern. *)

let%expect_test ("named" [@expect.uncaught_exn {| boom |}]) =
  print_string "x";
  [%expect {| x |}]
