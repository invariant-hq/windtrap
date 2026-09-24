(* Right arms of [||] in tail position that are not applications, part
   two: the forms that bind before their body ([let], [let open],
   [let module], [let exception], a binding operator). The body inherits
   tail position, so the arm must stay the [else] branch as written, with
   the recursive call bare; dropping a shape from the guard demotes it to
   an [if] condition and wraps the call. The semantics suite runs [let]
   deep under a bounded stack; this golden pins every shape. *)

let rec or_let n =
  n = 0
  ||
  let next = n - 1 in
  or_let next

let rec or_open n =
  n = 0
  ||
  let open Stdlib in
  or_open (n - 1)

let rec or_letmodule n =
  n = 0
  ||
  let module M = Stdlib in
  or_letmodule (n - 1)

let rec or_letexception n =
  n = 0
  ||
  let exception E in
  or_letexception (n - 1)

let ( let* ) x f = f x

let rec or_letop n =
  n = 0
  ||
  let* m = n - 1 in
  or_letop m

(* Rules pinned here, by id in RULES.md and interface line: C27, cov:62-65. *)
