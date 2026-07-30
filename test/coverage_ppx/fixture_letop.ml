(* Letop bodies. Nested letops each mark their own body; the binding
   expressions get no points (they are not blocks). *)

let ( let* ) x f = f x
let ( and* ) a b = (a, b)

let sum =
  let* a = 1 in
  let* b = 2 in
  a + b

let pair =
  let* a = 1 and* b = 2 in
  (a, b)
