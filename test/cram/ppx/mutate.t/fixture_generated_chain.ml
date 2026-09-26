(* The chain rule holds under an outer application that is no site. The outer
   [+] of [generated] is generated code, and its written link [a + b] carries no
   mutant either. The copy of [copied] drops its outer [+] as a duplicate,
   and the link of the copy stays suppressed. *)
let generated a b c = (a + b + c) [@generated]
let copied f a b c = f (a + b + c) [@@duplicate]
