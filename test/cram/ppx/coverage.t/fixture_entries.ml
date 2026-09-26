(* Entry points: the blocks and the arms the other fixtures leave out. Each
   rule is named by its id in ../RULES.md and the line of instrument.mli that
   states it. *)

(* C3, cov:34-35: a coercion on the leaf body stays around the visit. *)
let widen (x : [ `A ]) = (x :> [ `A | `B ])

(* C10, cov:55-56: a refutation arm has no point; the [Left] arm has one. *)
type empty = |

let refute (x : (int, empty) Either.t) =
  match x with Left n -> n | Right _ -> .

(* C11, cov:56: an arm whose body carries [@coverage off] has no point. *)
let quiet n = match n with 0 -> "zero" | _ -> ("other" [@coverage off])

(* C15, cov:44: a [lazy] of a trivial value under a coercion is left alone. *)
let nothing = lazy (None :> int option)

(* C21, cov:50-51: the deprecated [&] is handled as [&&], its right operand
   an entry point. *)
let both x y = x & y

(* C23, cov:50-51: the deprecated [or] is handled as [||], each operand
   marked at its last byte. *)
let either x y = x or y

(* C26, cov:61-62: in tail position, a right operand of [||] that is a method
   call stays the [else] branch and has no point. *)
let ask x (o : < ok : bool >) = x || o#ok

(* C28, cov:60-62: in tail position, a right operand of [||] that applies a
   trivial primitive is demoted to a condition and marked. *)
let either_not x y = x || not y

(* C29, cov:52-53: what follows an [if] without [else] in a sequence is an
   entry point. *)
let warn flag =
  if flag then print_string "watch out";
  print_newline ()
