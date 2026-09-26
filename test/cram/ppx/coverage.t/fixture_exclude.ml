[@@@coverage exclude_file]

(* The whole file is excluded: no marks, no registration module. *)

let f n = if n > 0 then 1 else 0

(* Rules pinned here, by id in RULES.md and interface line: C69, cov:168-169. *)
