(* A file that rebinds a comparison and [not]. Each rule is named by its id
   in ../RULES.md and the line of instrument.mli that states it.

   M34, mut:36-37: a variable pattern named after [<=] removes the whole
   [cmp] family, [<] included, as [+] removes [ari].
   M19, mut:87-88: a condition that is a comparison is then no longer one,
   and carries [neg].
   M35, mut:37-38: [neg] names [Stdlib.not] and is never lost, even in a
   file that rebinds [not]. [ari] and [con] are untouched. *)

let ( <= ) a b = compare a b <= 0
let not b = b
let ordered a b = if a < b then a + b else a - b
let both a b = if a && b then 1 else 0
