(* Still out of scope under expression grade (Law 14 as amended):
   constant bindings allocate no point; applications of trivial
   primitives (operators, [raise], [ignore], ...) carry no out-edge; a
   fully labeled - partial - application carries no out-edge; a call in
   tail position carries no out-edge at the call (its edge is attributed
   to the caller's first non-tail application); [assert false] stays
   untouched. [add] and friends get their one leaf-body entry point and
   nothing else. *)

let top_level = 1
let greeting = "hello"
let add a b = a + b
let negate b = not b
let vanish x = ignore x
let labeled_only ~f = f ~x:1
let tail_call x = add x 1
let never () = assert false
