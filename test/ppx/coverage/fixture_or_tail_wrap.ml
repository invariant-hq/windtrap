(* Right arms of [||] in tail position that are not applications, part
   three: the forms that wrap their last expression (a sequence, a type
   constraint, a coercion). The last expression inherits tail position, so
   the arm must stay the [else] branch as written, with the recursive call
   bare; dropping a shape from the guard demotes it to an [if] condition
   and wraps the call. *)

let rec or_seq n =
  n = 0
  ||
  (ignore n;
   or_seq (n - 1))

let rec or_constraint n = n = 0 || (or_constraint (n - 1) : bool)
let rec or_coerce n = n = 0 || (or_coerce (n - 1) :> bool)
