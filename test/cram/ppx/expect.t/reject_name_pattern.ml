let%expect_test name = print_string "x"

(* E3, pwt:69-71: a name that is neither a string literal nor [_] is
   refused. *)

(* Rules pinned here, by id in RULES.md and interface line: E30, pwt:60-61. *)
