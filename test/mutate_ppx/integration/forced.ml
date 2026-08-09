(* Compiled with the mutation instrumenter unconditionally, under the
   warnings the generated preamble is most likely to trip: an unused
   [open] (33), a field resolved by type-directed disambiguation (42),
   and an unused module (60). The golden expansions in the parent
   directory pin what the instrumenter emits; this module pins that what
   it emits is well-typed OCaml. *)

let pick flag x y = if flag then x else y

let drain ready step =
  while ready () do
    step ()
  done

let classify p x = match x with y when p y -> "yes" | _ -> "no"
let below a b = if a < b then 1 else 0
let at_most a b = if a <= b then 1 else 0
let above a b = if a > b then 1 else 0
let at_least a b = match a with _ when a >= b -> 1 | _ -> 0
let same a b = if a = b then 1 else 0
let differs a b = if a <> b then 1 else 0
let window lo hi x = x >= lo && x <= hi
let both a b = a && b
let either a b = a || b
let chain a b c = a && b && c
let sum a b = a + b
let diff a b = a - b
let fsum a b = a +. b
let fdiff a b = a -. b
let origin = 1 + 2
let scaled a b = (a + b) * 2
let rec search p = function [] -> false | x :: rest -> p x || search p rest

let countdown n =
  let r = ref n in
  while !r > 0 do
    decr r
  done;
  !r

let tagged a b = match a with _ when a = b -> "same" | _ -> "differs"
let checked a b = assert (a < b)
let thunk a b = lazy (fun () -> a + b)
let forced a b = lazy (a + b)

let cap want =
  if (want > 16) [@mutate off "both arms yield 16 at the boundary"] then want
  else 16

let untouched a b = a + b [@@mutate off]

(* A record field beside the generated table: the preamble's fields are
   all qualified, so warning 42 stays silent. *)
type point = { line : int; col : int }

let point line col = { line; col }
let describe p = p.line + p.col
