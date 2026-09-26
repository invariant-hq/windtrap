(* M26, mut:104: the payloads of attributes and of extension nodes are never
   mutated: the [+] inside them carries no site, and the one beside them
   does. *)

let extended = [%ext a + b]
let attributed = (0 [@attr a + b])
let written a b = a + b
