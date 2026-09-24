(* Fixture: [%expect.unreachable] is not implemented and must be
   rejected at expansion (RFC compat mechanism (a)). *)

let%expect_test "unreachable" =
  if false then [%expect.unreachable];
  [%expect {| |}]

(* Rules pinned here, by id in RULES.md and interface line: E15, pwt:84-86. *)
