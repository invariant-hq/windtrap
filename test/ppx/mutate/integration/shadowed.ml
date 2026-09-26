(* The emission law, made falsifiable.

   Every arm of a guard must mention only identifiers already present in
   the original expression plus [Stdlib]-qualified names, because all of
   a project's mutants share one binary and one ill-typed arm is a broken
   build for the whole project. This file shadows every unqualified name
   a guard could otherwise reach - the value [not], and the TYPE [bool],
   which the [con] guard constrains its binder with - and then writes one
   expression per operator, so the guards are actually emitted here.

   It compiles only because the instrumenter writes [Stdlib.not] and
   [(… : Stdlib.Bool.t)]. Dropping either qualification fails this
   module and nothing else in the tree. There is no [Stdlib.bool]: the
   predefined types are not re-exported from [Stdlib], which is why the
   constraint names the alias module.

   The comparison and connective operators are deliberately NOT shadowed
   here: rebinding them would switch their families off through the
   file-level capability gate, and no guard would be emitted to test. The
   gate itself is pinned by test/cram/ppx/mutate.t/fixture_shadow.ml. *)

type bool = Yes | No

let not = function Yes -> No | No -> Yes
let flip x = not x

(* [neg]: the armed arm is [Stdlib.not] on a predefined boolean. *)
let pick flag x y = if flag then x else y

(* [con]: the armed arm compares the binder against the guard with
   [Stdlib.( <> )] under a [Stdlib.Bool.t] constraint. *)
let both p q = p && q

(* [cmp]: the armed arm is [Stdlib.not] applied to the source's own
   comparison, on swapped operands. *)
let below a b = if a < b then 1 else 0

(* [ari]: the one arm that names an unqualified operator, which is why it
   is gated on the file not rebinding one. *)
let sum a b = a + b

(* Rules pinned here, by id in RULES.md and interface line: M35, mut:37-38;
   M36, mut:28-30. *)
