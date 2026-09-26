(* M20, mut:89-93 and M40, mut:129-131: the link of a chain is no site, so
   [[@mutate off]] on it records nothing, and the [b - d] inside it is left
   as written. The outer [+] carries the chain's one mutant. *)
let link a b c d = (a + (b - d)) [@mutate off] + c
