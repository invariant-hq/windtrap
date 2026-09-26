(* Fixture: an [%expect] payload must be a string literal. *)

let%expect_test "bad payload" =
  print_string "x";
  [%expect 42]

(* Rules pinned here, by id in RULES.md and interface line: E14, pwt:79-80;
   E30, pwt:60-61. *)
