(* Parity fixture: the exclusion grammar's legal spellings. Each suite
   pins that its own driver accepts this file, the mutation suite with the
   namespace swapped; together they hold the manual's promise that both
   backends read the same spellings. *)

let expr_off x = (x + 1) [@coverage off]
let expr_off_reason x = (x + 1) [@coverage off "reason"]
let binding_off = List.length [ 1 ] [@@coverage off]
let binding_off_reason = List.length [ 1 ] [@@coverage off "reason"]

[@@@coverage off]

let region_off y = y * 2

[@@@coverage on]
[@@@coverage off "reason"]

let region_off_reason y = y * 2

[@@@coverage on]

let after_region z = z - 1
