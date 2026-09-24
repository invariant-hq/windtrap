(* No instrumentable block anywhere: the file must pass through unchanged,
   with no registration module (an empty table would only add noise to
   reports). *)

type t = A | B

let x = 1
let s = "hello"

(* Rules pinned here, by id in RULES.md and interface line: C71, cov:202-203. *)
