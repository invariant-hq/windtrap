(* M25, mut:100-103: a [lazy] of a trivial value carries no mutant as a
   condition either, where any other expression carries [neg]. *)
let deferred x = if lazy x then 1 else 0
let plain x = if x then 1 else 0
