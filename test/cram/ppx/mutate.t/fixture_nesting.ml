(* Placement rules 1 and 2. *)

(* Rule 2: a connective whose left or right operand is itself a
   connective carries no site. [&&] is right-associative, so [a && b && c]
   is [a && (b && c)] and only [b && c] is mutated. *)
let chain a b c = a && b && c
let mixed a b c = (a && b) || c
let deep a b c d = a || (b && (c || d))

(* Rule 1: where [cmp] or [con] fires on a condition, [neg] does not. *)
let by_cmp a b = if a < b then 1 else 0
let by_con a b = if a && b then 1 else 0
let by_neg a = if a then 1 else 0

(* Rules pinned here, by id in RULES.md and interface line: M2, mut:56-58;
   M16, mut:81-83; M17, mut:84-86. *)
