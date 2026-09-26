(* An application of a trivial primitive carries no out-edge. Each is bound by a
   [let], a position where any other application is wrapped, as [f] is at the
   end; the [&&] and [&] bodies keep the entry point of their right operand. *)

let primitives a b x y r l s e f =
  let _ = a && b in
  let _ = a & b in
  let _ = not a in
  let _ = x = y in
  let _ = x <> y in
  let _ = x < y in
  let _ = x <= y in
  let _ = x > y in
  let _ = x >= y in
  let _ = x == y in
  let _ = x != y in
  let _ = ref x in
  let _ = !r in
  let _ = r := x in
  let _ = l @ l in
  let _ = s ^ s in
  let _ = x + y in
  let _ = x - y in
  let _ = x * y in
  let _ = x / y in
  let _ = 1. +. 2. in
  let _ = 1. -. 2. in
  let _ = 1. *. 2. in
  let _ = 1. /. 2. in
  let _ = x mod y in
  let _ = x land y in
  let _ = x lor y in
  let _ = x lxor y in
  let _ = x lsl y in
  let _ = x lsr y in
  let _ = x asr y in
  let _ = raise e in
  let _ = raise_notrace e in
  let _ = failwith s in
  let _ = ignore x in
  let _ = Sys.opaque_identity x in
  let _ = Obj.magic x in
  let _ = x##y in
  let _ = f x in
  ()
