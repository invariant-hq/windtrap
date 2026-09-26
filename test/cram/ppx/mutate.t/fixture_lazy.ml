(* [lazy] applied to a trivial syntactic value compiles as already
   forced, so the subtree is left alone: [thunk]'s [a + b] carries no
   mutant. A [lazy] of a non-value is traversed normally. No guard the
   four shipped operators emit can land on a trivial syntactic value, so
   what this exclusion costs today is only the mutants inside such a
   body; it is kept so the exclusion exists before an operator that can
   reach it does. *)

let thunk a b = lazy (fun () -> a + b)
let forced a b = lazy (a + b)
let plain x = lazy x

(* The predicate looks through a type constraint, so a constrained
   function is still trivial and its body is still left alone; a
   constrained application is not, and is traversed. *)
let annotated a b = lazy (fun () -> a + b : unit -> int)
let computed a b = lazy (a + b : int)
