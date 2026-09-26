(* Still out of scope under expression grade:
   constant bindings allocate no point; applications of trivial
   primitives (operators, [raise], [ignore], ...) carry no out-edge; a
   fully labeled - partial - application carries no out-edge; a call in
   tail position carries no out-edge at the call (its edge is attributed
   to the caller's first non-tail application); [assert false] stays
   untouched. [add] and friends get their one leaf-body entry point and
   nothing else. The primitive and labelled applications are bound by a
   [let], a position where any other application is wrapped. *)

let top_level = 1
let greeting = "hello"
let add a b = a + b

let negate b =
  let r = not b in
  r

let vanish x =
  let () = ignore x in
  ()

let labeled_only ~f =
  let r = f ~x:1 in
  r

let tail_call x = add x 1
let never () = assert false
