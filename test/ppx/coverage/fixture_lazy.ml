(* Lazy bodies are instrumented - except trivial syntactic values, which
   [lazy] compiles as already forced: a visit under those would change the
   compilation of the [lazy] (the donor guard, Law 13). *)

let computed = lazy (1 + 2)
let const = lazy 42
let alias = lazy const
let none = lazy None
let thunk = lazy (fun x -> x)
let constrained = lazy (42 : int)
