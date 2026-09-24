let%expect_test ("n" [@tags 42]) = ()

(* E6, pwt:77-78: a [[@tags]] payload that is neither a string literal nor
   a tuple of them is refused. *)

(* Rules pinned here, by id in RULES.md and interface line: E30, pwt:60-61. *)
