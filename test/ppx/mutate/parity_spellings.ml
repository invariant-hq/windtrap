(* Parity fixture: the exclusion grammar's legal spellings, byte-identical
   to its twin in the sibling instrumenter's suite modulo the attribute
   namespace. The runtest mirror rule pins the twins equal after
   normalization, and each suite pins that its own driver accepts the
   file — together they hold the manual's promise that both backends
   read the same spellings. *)

let expr_off x = (x + 1) [@mutate off]
let binding_off = List.length [ 1 ] [@@mutate off]

[@@@mutate off]

let region_off y = y * 2

[@@@mutate on]

let after_region z = z - 1
