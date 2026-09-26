(* Out-edges: where an application, a method call, a [new] or an [assert] is
   wrapped, and the positions whose return an enclosing form already
   observes. *)

class counter =
  object
    method get = 0
  end

(* A [new] in tail position is not wrapped. *)
let make () = new counter

(* A [new] that is not applied, out of tail position, is. *)
let kept () =
  let c = new counter in
  c#get

(* [assert e] is wrapped in tail position and out of it, keyed at the start of
   [e]. *)
let checked x = assert (x > 0)

let checked_then x =
  assert (x > 0);
  x

(* The trivial primitives are matched by spelling, so [Stdlib.( + ) a b] is
   wrapped and [a + b] is not. *)
let qualified a b =
  let r = Stdlib.( + ) a b in
  r

let bare a b =
  let r = a + b in
  r

(* The scrutinee of a [match] has no out-edge. *)
let scrutinee l = match List.rev l with [] -> 0 | x :: _ -> x

(* The condition of an [if] has no out-edge. *)
let condition l = if List.mem 0 l then 1 else 2

(* The applied left operand of [@@] has no out-edge; the right operand and the
   whole application have theirs. *)
let at x =
  let r = Printf.sprintf "%d" @@ succ x in
  r

(* The right operand of [|>] has no out-edge. *)
let piped l =
  let r = l |> List.map succ in
  r

(* [|.] is handled as [|>]: its right operand has no out-edge, and in tail
   position it is not wrapped. *)
let ( |. ) x f = f x

let dotted l =
  let r = l |. List.map succ in
  r

let dotted_tail l = l |. List.map succ

(* A method call in the position of a callee has no out-edge; the application of
   it has one. *)
let callee (o : < get : int -> int >) x =
  let r = o#get x in
  r
