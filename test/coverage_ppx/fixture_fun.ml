(* Function leaf bodies only: one point per curried chain, at the leaf;
   [function] arms are arm points, not body points; a constraint is
   transparent (the point lands inside it). *)

let add a b = a + b
let curried = fun a -> fun b -> a + b
let annotated x : int = x + 1
let constrained = fun x -> (x + 1 : int)
let arms = function 0 -> "zero" | _ -> "nonzero"
let mixed x = function 0 -> x | n -> n + x

let sequenced () =
  print_string "a";
  print_string "b"
