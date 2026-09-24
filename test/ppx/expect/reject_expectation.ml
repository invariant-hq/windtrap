(* Fixture: [%expectation] (and family) is not implemented and must be
   rejected at expansion — here outside any expect test, caught by the
   leftover scan (RFC compat mechanism (a)). *)

let check () = ignore [%expectation {| x |}]

(* Rules pinned here, by id in RULES.md and interface line: E17, pwt:84-86. *)
