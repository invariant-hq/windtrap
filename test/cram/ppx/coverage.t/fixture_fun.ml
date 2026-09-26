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

(* Optional-argument defaults are blocks: the default runs only when the
   caller omits the argument, so its entry is a point and a call inside
   it carries an out-edge. A default that is itself a function gets its
   leaf body marked like any other. *)
let defaulted ?(x = String.length "abc") () = x
let default_fn ?(l = fun () -> ()) () = l
