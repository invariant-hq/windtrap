(* The identifier collision drop, reached from hand-written code rather
   than from a rewriter's duplicated locations.

   A mutant is named [<file>:<line>:<col>:<rewrite>], where line and
   column are the first byte of the mutated expression. A chain of one
   left-associative operator starts every one of its nodes at that same
   byte, so [a + b + c] is [(a + b) + c] and BOTH nodes are a [sub]
   rewrite at the column of [a]. The identifier cannot separate them, so
   the inner one is dropped: a chain of n operators of one family carries
   one mutant, not n-1.

   Mixing families costs nothing, because the rewrite name is part of the
   key - and the nested guards that produces are what pins that the
   reserved binder namespace is really site-indexed. *)

let three a b c = a + b + c
let minus a b c = a - b - c
let floats a b c = a +. b +. c

(* Different rewrites at one column: both survive, one guard inside the
   other's operand, with distinct binders. *)
let mixed a b c = a + b - c

(* The same collision on the comparison family is unreachable: [cmp]
   fires in a boolean context, and a comparison's operand never is one.
   Only the outer comparison carries a site here. *)
let chained a b c = if a < b < c then 1 else 0
