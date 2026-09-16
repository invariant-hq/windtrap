(* The library under test: the coverage example's calculator, carrying
   the mutate backend. Its comparisons and arithmetic are the mutation
   sites. [abs] carries a dismissed mutant: [n >= 0] and its mutant
   [n > 0] agree at zero — both arms yield 0 — so no test can tell them
   apart, and the attribute records that reasoning where git blame sees
   it. *)

type op = Add | Sub | Mul | Div

let apply op a b =
  match op with
  | Add -> a + b
  | Sub -> a - b
  | Mul -> a * b
  | Div -> if b = 0 then invalid_arg "Calc.apply: division by zero" else a / b

let abs n =
  if (n >= 0) [@mutate off "both arms yield 0 at n = 0"] then n else -n
