(* [con]: the connective swap. One branch expresses both connectives, so
   the right operand appears once, stays in tail position, and is
   evaluated on exactly the original schedule. The [(… : bool)]
   constraint keeps the comparison an integer compare rather than a call
   into [caml_notequal]; the [Stdlib.] qualification answers the emission
   law against a user-shadowed [( = )]. *)

let both a b = a && b
let either a b = a || b
let gate ready x = if ready && x then 1 else 0
let rec search p = function [] -> false | x :: rest -> p x || search p rest

(* Rules pinned here, by id in RULES.md and interface line: M2, mut:56-58;
   M9, mut:63-64. *)
