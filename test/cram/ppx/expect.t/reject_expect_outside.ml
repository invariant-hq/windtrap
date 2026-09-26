(* Fixture: [%expect] outside a let%expect_test body is an error, not a
   silently unexpanded node. *)

let f () = [%expect {| nothing |}]

(* Rules pinned here, by id in RULES.md and interface line: E16, pwt:82-83. *)
