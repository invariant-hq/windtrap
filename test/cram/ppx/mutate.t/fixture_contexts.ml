(* Boolean contexts, and the applications that are no sites. *)

(* A direct operand of [||] is a boolean context, so its comparison carries
   [cmp]; the [||] carries [con]. *)
let either a b c = a < b || c

(* A boolean context does not reach through a [let] body, a type constraint or
   [not]: each condition below carries [neg] as a whole, and the comparison
   inside it carries nothing. *)
let through_let a b =
  if
    let x = a in
    x < b
  then 1
  else 0

let through_constraint a b = if (a < b : bool) then 1 else 0
let through_not a b = if not (a < b) then 1 else 0

(* The operands of a connective that carries no mutant because an operand is a
   connective are boolean contexts all the same: [a < b] carries [cmp], and [c
   || d] carries [con]. *)
let skipped a b c d = if a < b && (c || d) then 1 else 0

(* The deprecated [&] and [or] are not sites; in a condition each carries
   [neg]. *)
let old_and a b = a & b
let old_or a b = a or b
let old_condition a b = if a & b then 1 else 0

(* A qualified operator is not a site; in a condition, a qualified comparison
   carries [neg]. *)
let qualified a b = Stdlib.( + ) a b
let qualified_condition a b = if Float.( < ) a b then 1 else 0

(* A labelled or a partial application of an operator is not a site; a total
   unlabelled one, which [a + b] is, is. *)
let partial = ( + ) 1
let labelled a b = ( + ) ~a b
let total a b = a + b
