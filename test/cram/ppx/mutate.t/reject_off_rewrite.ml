(* A bare [off] dismisses every mutant, so [off] takes no rewrite name. *)

let f n = (n + 1) [@mutate off sub "equal at zero"]
