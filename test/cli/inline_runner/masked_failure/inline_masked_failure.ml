(* An expect body that fails an assertion AND leaves a stale payload.

   [Windtrap.subtest] records the assertion failure on the frame and
   carries on, so the body reaches the stale node. A correction is
   recorded only for a test whose every failure is a baseline mismatch:
   this one must exit 1 with no .corrected, or dune would stage the
   correction as promotable and [dune promote] would bless output the
   assertion already said was wrong (the masked-failure rule of the
   baselines chapter). *)

let%expect_test "an assertion failure beside a stale payload" =
  Windtrap.subtest "the assertion" (fun () -> Windtrap.equal Windtrap.int 1 2);
  print_string "fresh output";
  [%expect {| stale payload |}]
