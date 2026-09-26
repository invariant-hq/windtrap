(* In the body of a value binding that carries [[@tail_mod_cons]] or
   [[@ocaml.tail_mod_cons]], at the top level or in a [let ... in], an
   application or a method call has no out-edge, a [new] and an [assert] keep
   theirs, and the entry points are unaffected. [plain], [map] without the
   attribute, has its calls wrapped. *)

let[@tail_mod_cons] rec map f = function
  | [] -> []
  | x :: rest -> f x :: map f rest

let[@ocaml.tail_mod_cons] rec double = function
  | [] -> []
  | x :: rest -> (x * 2) :: double rest

let local l =
  let[@tail_mod_cons] rec go = function
    | [] -> []
    | x :: rest -> succ x :: go rest
  in
  go l

class cell =
  object
    method get = 0
  end

let[@tail_mod_cons] rec cells (o : < get : int >) n =
  if n = 0 then []
  else (
    assert (n > 0);
    ignore o#get;
    new cell :: cells o (n - 1))

let rec plain f = function [] -> [] | x :: rest -> f x :: plain f rest
