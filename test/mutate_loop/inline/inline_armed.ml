(* An expect test whose output changes when a mutant is armed.

   Law 16(d): while a mutant is armed an [%expect] mismatch is a plain
   failure — no [.corrected] is written and dune's promotion protocol is
   not consulted. Without it, a loop of a few hundred children would
   generate a few hundred correction files in the sandbox, from output an
   armed mutant produced on purpose, and Law 1 (nothing writes source)
   would fall over. The payload below is the UNMUTATED answer, so the
   partition is green unarmed and red — with nothing on disk — armed. *)

let%expect_test "widen" =
  print_int (Mutate_loop_subject.Subject.widen 3 4);
  [%expect {| 7 |}]
