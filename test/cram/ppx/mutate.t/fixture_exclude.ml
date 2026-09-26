[@@@mutate exclude_file]

(* Excluded at the file level: passes through unchanged, with no
   preamble. *)

let sum a b = a + b
let ordered a b = if a < b then 1 else 0

(* Rules pinned here, by id in RULES.md and interface line: M45, mut:141-142. *)
