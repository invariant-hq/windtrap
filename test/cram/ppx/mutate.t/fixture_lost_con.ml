(* A file that rebinds [&&] through an [external].

   A value description named after an operator removes its family, as a
   variable pattern does, and the family is [con], [||] included. A
   condition that is a connective is then no longer one, and carries [neg].
   Its operands are no boolean context, so [a < b] in [compared] carries
   nothing. [cmp] on a condition and [ari] are untouched. *)

external ( && ) : bool -> bool -> bool = "%sequand"

let both a b = if a && b then 1 else 0
let either a b = if a || b then 1 else 0
let compared a b c = if a < b && c then 1 else 0
let ordered a b = if a < b then a + b else a - b
