(* [cmp]: the six comparisons, each in a boolean context. The four
   ordering rewrites swap their operands under negation and the two
   equality rewrites do not; this golden is the only thing that pins
   which is which, because a suite that merely runs the code passes
   either way. *)

let below a b = if a < b then 1 else 0
let at_most a b = if a <= b then 1 else 0

let above a b =
  while a > b do
    ()
  done

let at_least a b = match a with _ when a >= b -> 1 | _ -> 0
let same a b = if a = b then 1 else 0
let differs a b = if a <> b then 1 else 0

(* Direct operands of [&&] and [||] are boolean contexts too. *)
let window lo hi x = x >= lo && x <= hi

(* Outside a boolean context there is no [cmp] site: the original program
   only typechecks as a boolean where the syntax says so, and a ppx
   cannot see a comparison shadowed through an [open]. Neither of these
   carries a mutant. *)
let ok a b = a < b
let count a b = List.length (List.filter (fun x -> x < a) b)

(* Rules pinned here, by id in RULES.md and interface line: M3, mut:59-62;
   M4, mut:74-75; M5, mut:62; M48, mut:155; M49, mut:157-160;
   M55, mut:182-183. *)
