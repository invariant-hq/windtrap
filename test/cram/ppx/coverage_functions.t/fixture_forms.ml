(* The expression forms the other fixtures leave out. A call inside one is
   out of tail position and wrapped, but in the body of a [let module], a
   [let exception] or a [let open], which inherits the position of the
   form. *)

type r = { a : int; mutable b : int }

let tuple f x = (f x, f x)
let variant f x = `V (f x)
let record f r = { r with a = f r.a }
let field f r = (f r).a
let setfield f r = r.b <- f r.b
let array f x = [| f x |]

let scopes f x =
  ignore
    (let module M = struct
       let y = f x
     end in
     let exception E of int in
     let open M in
     E (f y))

let tail_scopes f x =
  let module M = struct end in
  let exception E in
  M.(f x)

class counter =
  object
    val mutable v = 0
    method set f = v <- f v
    method copy f = {<v = f v>}
  end

let immediate f =
  object
    method get = f 1
  end

module type S = sig
  val v : int
end

let pack f =
  (module struct
    let v = f 1
  end : S)

(* A top-level expression is traversed where a value binding is, and left as
   written inside a region. *)
;;

print_int (fst (tuple succ 1));;

[@@@coverage off];;

print_int (fst (tuple succ 1));;

class dark =
  object
    method get = succ 1
  end

[@@@coverage on]
