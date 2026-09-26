(* Entry points: the blocks and the arms the other fixtures leave out. *)

(* A coercion on the leaf body stays around the visit. *)
let widen (x : [ `A ]) = (x :> [ `A | `B ])

(* A refutation arm has no point; the [Left] arm has one. *)
type empty = |

let refute (x : (int, empty) Either.t) =
  match x with Left n -> n | Right _ -> .

(* An arm whose body carries [@coverage off] has no point. *)
let quiet n = match n with 0 -> "zero" | _ -> ("other" [@coverage off])

(* A [lazy] of a trivial value under a coercion is left alone. *)
let nothing = lazy (None :> int option)

(* The deprecated [&] is handled as [&&], its right operand an entry point. *)
let both x y = x & y

(* The deprecated [or] is handled as [||], each operand marked at its last
   byte. *)
let either x y = x or y

(* In tail position, a right operand of [||] that is a method call stays the
   [else] branch and has no point. *)
let ask x (o : < ok : bool >) = x || o#ok

(* In tail position, a right operand of [||] that applies a trivial primitive is
   demoted to a condition and marked. *)
let either_not x y = x || not y

(* What follows an [if] without [else] in a sequence is an entry point. *)
let warn flag =
  if flag then print_string "watch out";
  print_newline ()
