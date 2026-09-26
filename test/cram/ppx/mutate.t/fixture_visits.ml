(* M58, mut:199-205: the mutation rewriter over a file that holds the
   coverage rewriter's visits. *)

(* [a || b] reaches the rewriter as the [if] of the coverage rewriter: it
   carries no [con] mutant, and each operand, now a condition, carries
   [neg]. *)
let either a b = a || b

(* The [before] and [after] texts of a site hold the visits inside it. *)
let lt f g x = if f x < g x then 1 else 0
