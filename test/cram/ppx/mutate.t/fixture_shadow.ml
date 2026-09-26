(* A file that visibly rebinds an operator gets no sites for that
   family, and only that family: [ari]'s armed arm names an operator the
   source did not write, so it must not be emitted where [+] means
   something else. The comparisons and the connectives are untouched
   here, so [cmp] and [con] still fire. What an [open] brings in cannot
   be seen; that residue is one compile error at the user's own source
   location, with [[@@@mutate exclude_file]] as the remedy. *)

module Vec = struct
  type t = { x : int; y : int }

  let ( + ) a b = { x = a.x + b.x; y = a.y + b.y }
end

let move a b = Vec.( + ) a b
let ordered a b = if a < b then 1 else 0
let both a b = a && b
let untouched a b = a - b

(* Rules pinned here, by id in RULES.md and interface line: M32, mut:34-40. *)
