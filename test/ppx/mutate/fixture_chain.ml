(* The chain rule: a chain of n operators of one family carries one
   mutant, not n-1. The traversal is top-down, so the outermost node
   allocates its site and then suppresses its left operand when that
   operand applies the same operator; the suppressed node suppresses its
   own left operand in turn, so the rule runs the whole left spine.

   The rule is STRUCTURAL - it reads the operator - and the parenthesized
   forms below are why. A bare chain's nodes all start at the same byte,
   so a rule keyed on line and column would happen to collapse them; but
   OCaml's parser gives a parenthesized expression a location starting at
   its [(], so [f (a + b + c)] would then carry two mutants where
   [a + b + c] carries one. Parenthesizing an expression must not change
   how many mutants it carries.

   Mixing families costs nothing, because the rewrite name is part of the
   identity - and the nested guards that produces are what pins that the
   reserved binder namespace is really site-indexed. *)

let three a b c = a + b + c
let minus a b c = a - b - c
let floats a b c = a +. b +. c
let conn a b c = if a && b && c then 1 else 0

(* The same chains where the parser locates the outer node at a [(]
   instead of at the chain's first operand: one mutant each, exactly as
   above. Note these all use parentheses ocamlformat keeps. It removes
   redundant ones, including the [if (a && b && c) then] spelling, so a
   bracketed chain can only be pinned where the brackets are load-bearing
   syntax. An argument is the common case anyway, and the case the defect
   was found in. *)
let parens a b c = ignore (a + b + c)
let in_arg f a b c = f (a + b + c)
let conn_arg f a b c = f (a && b && c)

(* Not a chain, and deliberately two mutants: the right operand is not on
   the left spine, so an explicitly right-nested pair is two distinct
   sites. That is a difference in the source's structure, not in its
   brackets. *)
let siblings a b c d = a + b + (c + d)

(* Different rewrites at one column: both survive, one guard inside the
   other's operand, with distinct binders. *)
let mixed a b c = a + b - c

(* The same collision on the comparison family is unreachable: [cmp]
   fires in a boolean context, and a comparison's operand never is one.
   Only the outer comparison carries a site here. *)
let chained a b c = if a < b < c then 1 else 0

(* Rules pinned here, by id in RULES.md and interface line: M17, mut:84-86;
   M20, mut:89-93; M22, mut:68-72; M23, mut:94-95; M48, mut:159;
   M49, mut:161-164. *)
