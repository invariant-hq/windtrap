(* An expect body that fails an assertion AND leaves a stale payload.

   [Windtrap.subtest] records the assertion failure on the frame and
   carries on, so the body returns normally and the expect node resolves
   to an ordinary correction. If the protocol's "covered" bit is read from
   the node state alone, the partition exits 0, dune stages the correction
   as promotable, and [dune promote] blesses output the assertion already
   said was wrong — Law 11's "masked assertion failures". *)

let%expect_test "an assertion failure beside a stale payload" =
  Windtrap.subtest "the assertion" (fun () -> Windtrap.equal Windtrap.int 1 2);
  print_string "fresh output";
  [%expect {| stale payload |}]
