(* [neg]: the condition of an [if] or a [while], and a [when] guard, when
   that condition is neither a comparison nor a connective (placement
   rule 1 gives those priority). The condition is bound once, so it is
   evaluated exactly as often as before, and the armed arm names
   [Stdlib.not] rather than [not] - a user-shadowed [not] must not be
   able to decide what a mutant means. *)

let pick flag x y = if flag then x else y
let announce flag = if flag then print_string "on"

let drain ready step =
  while ready () do
    step ()
  done

let classify p x = match x with y when p y -> "yes" | _ -> "no"

(* A condition that is itself an [if] is negated as a whole, and its own
   condition is a condition in its own right. *)
let nested a b = if if a then b else false then 1 else 0

(* Rules pinned here, by id in RULES.md and interface line: M1, mut:56-58;
   M49, mut:161-164. *)
