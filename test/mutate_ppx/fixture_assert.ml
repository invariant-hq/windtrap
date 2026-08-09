(* [assert] is rewritten to a polymorphic raise, so mutating anything
   under it breaks typing in exactly the arms where it appears: the whole
   subtree is left alone. Code beside an assertion is mutated normally. *)

let checked a b =
  assert (a < b);
  a + b

let conjunction a b = assert (a && b)
let arithmetic a b = assert (a + b > 0)

(* Reached through a boolean context, an [assert] is still skipped: as an
   [if] condition it carries no [neg], as a connective operand it carries
   no [cmp] and no [neg]. Everything beside it is mutated as usual - the
   [&&] and the [a < b] below both carry their own site. *)
let as_condition () = if assert false then 1 else 0
let as_operand a b = a < b && assert false

(* An [assert] beside a mutation site in a sequence. The condition of the
   enclosing [if] is the sequence, so it carries a [neg]; the [a > b]
   inside it does not carry a [cmp], because the boolean context does not
   propagate through [;] - and nothing under the [assert] is touched. *)
let sequenced a b =
  if
    assert (a < b);
    a > b
  then 1
  else 0
