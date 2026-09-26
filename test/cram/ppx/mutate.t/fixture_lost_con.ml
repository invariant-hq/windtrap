(* A file that rebinds [&&] through an [external]. Each rule is named by its
   id in ../RULES.md and the line of instrument.mli that states it.

   M33, mut:39-40: a value description named after an operator removes its
   family, as a variable pattern does.
   M34, mut:36-37: the family is [con], [||] included.
   M19, mut:87-88: a condition that is a connective is then no longer one,
   and carries [neg]; M6, mut:68-69: its operands are no boolean context,
   so [a < b] in [compared] carries nothing. [cmp] on a condition and [ari]
   are untouched. *)

external ( && ) : bool -> bool -> bool = "%sequand"

let both a b = if a && b then 1 else 0
let either a b = if a || b then 1 else 0
let compared a b c = if a < b && c then 1 else 0
let ordered a b = if a < b then a + b else a - b
