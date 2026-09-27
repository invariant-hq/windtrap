(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The textbook laws.

    A law checks its clauses in the order given below, and each clause is one
    equation over named terms. When a clause does not hold, the law raises a
    {!Failure.Check_failure} of one {!Failure.constructor-Law} failure, built by
    {!Failure.law}. Its [law] is the name given below, its [clause] and
    [equation] those of the clause, and its [terms] what the law was given and
    computed for the clause, in the order {!Failure.constructor-Law} states,
    printed then and only then. Its [msg] is the law's [?msg], and its location
    follows {!Loc.resolve} as a verb's does.

    A term computed by a function the law was given ends the law as a
    {!Failure.Failed} term when the function fails or raises, with the terms
    before it: the function is called through {!Failure.catch}, the failure is
    {!Failure.of_fault} of what it raised, and a control is raised again. The
    equality, the order and the printer of a witness are called directly, and
    what they raise escapes the law, as it escapes a verb. In {!equivalence} and
    {!order} they are the functions under test, and they are still called
    directly.

    {b Demands.} {!equivalence}, {!order}, {!partial_order}, {!idempotent},
    {!involutive}, {!monotone} and {!ignores} register the {!Property.cover}
    demands their docs name in {!Run.prop_context}, and none when it is [None].
    A demand is labelled ["<law>: <label>"], then [" (<msg>)"] when the law's
    [?msg] is given. A demand on a changed value reads structural difference,
    [Stdlib.compare x y <> 0], and a pair that [compare] refuses differs, since
    [compare] answers [0] for a physically equal pair before it looks inside.

    In the terms below, [cmp] is the order of the witness, a comparison prints
    as the [int] it returned, a [leq] as the [bool] it returned, and a [=]
    between two terms prints as the [bool] of the witness's equality. *)

(** {1:witnesses Equivalences and orders} *)

val equivalence :
  ?__POS__:Loc.pos ->
  ?msg:string ->
  ?respell:('a -> 'a) ->
  'a Testable.t ->
  'a * 'a ->
  unit
(** [equivalence ?respell w (a, b)] checks, as ["equivalence"]:
    - ["reflexive"] [a = a], then [b = b], over the value and the comparison.
    - ["symmetric"] [a = b iff b = a], over [a], [b] and the two comparisons.
      Demands ["an unequal pair"].

    Given [respell] ([r]), with [r a] a term of each clause:
    - ["respelled"] [a = r a], then [r a = a]. Demands ["r a differs from a"].
    - ["transitive"] [a = r (r a)], then [r a = b iff a = b]. *)

val order :
  ?__POS__:Loc.pos ->
  ?msg:string ->
  ?respell:('a -> 'a) ->
  'a Testable.t ->
  'a * 'a * 'a ->
  unit
(** [order ?respell w (a, b, c)] checks, as ["order"], with [a], [b] and [c] the
    terms of each clause that names them:
    - ["reflexive"] [cmp x x = 0] for [a], [b], [c].
    - ["antisymmetric"] [sign (cmp x y) = -sign (cmp y x)] for the pairs [a b],
      [a c], [b c].
    - ["transitive"] [cmp x y <= 0 and cmp y z <= 0 imply cmp x z <= 0] for each
      of the six orderings of the three, [cmp x z] computed only when the
      premise holds.
    - ["agrees with equal"] [cmp x y = 0 iff x = y] for the three pairs. Demands
      ["an unequal pair"], [a] and [b] unequal.

    Given [respell] ([r]), over [a] and [r a]:
    - ["respelled"] [cmp a (r a) = 0]. Demands ["r a differs from a"].
    - ["agrees with equal"] [cmp a (r a) = 0 iff a = r a].
    - ["respelled"] [sign (cmp (r a) b) = sign (cmp a b)], [b] a term too.

    Raises [Invalid_argument] if [w] has no order, before any clause. *)

val partial_order :
  ?__POS__:Loc.pos ->
  ?msg:string ->
  'a Testable.t ->
  ('a -> 'a -> bool) ->
  'a * 'a * 'a ->
  unit
(** [partial_order w leq (a, b, c)] checks, as ["partial order"], with [a], [b]
    and [c] the terms of each clause that names them:
    - ["reflexive"] [leq x x] for [a], [b], [c].
    - ["antisymmetric"] [leq x y and leq y x imply x = y] for the pairs [a b],
      [a c], [b c], [x = y] computed only when the premise holds.
    - ["transitive"] [leq x y and leq y z imply leq x z] for each of the six
      orderings of the three, [leq x z] computed only when the premise holds.
      Demands ["a strict chain"]: an ordering whose premise holds, over three
      values no two of which are equal. *)

(** {1:operations Laws of operations} *)

val associative :
  ?__POS__:Loc.pos ->
  ?msg:string ->
  'a Testable.t ->
  ('a -> 'a -> 'a) ->
  'a * 'a * 'a ->
  unit
(** [associative w op (a, b, c)] checks [op (op a b) c = op a (op b c)] as
    ["associative"], over [op a b] and [op b c]. *)

val commutative :
  ?__POS__:Loc.pos ->
  ?msg:string ->
  'a Testable.t ->
  ('a -> 'a -> 'a) ->
  'a * 'a ->
  unit
(** [commutative w op (a, b)] checks [op a b = op b a] as ["commutative"]. *)

val neutral :
  ?__POS__:Loc.pos ->
  ?msg:string ->
  'a Testable.t ->
  ('a -> 'a -> 'a) ->
  'a ->
  'a ->
  unit
(** [neutral w op e x] checks [op e x = x], then [op x e = x], as ["neutral"].
*)

val absorbing :
  ?__POS__:Loc.pos ->
  ?msg:string ->
  'a Testable.t ->
  ('a -> 'a -> 'a) ->
  'a ->
  'a ->
  unit
(** [absorbing w op z x] checks [op z x = z], then [op x z = z], as
    ["absorbing"]. *)

val invertible :
  ?__POS__:Loc.pos ->
  ?msg:string ->
  'a Testable.t ->
  ('a -> 'a -> 'a) ->
  'a ->
  ('a -> 'a) ->
  'a ->
  unit
(** [invertible w op e inv x] checks [op x (inv x) = e], then
    [op (inv x) x = e], as ["invertible"], over [inv x]. [inv x] is computed
    once. *)

val distributive :
  ?__POS__:Loc.pos ->
  ?msg:string ->
  'a Testable.t ->
  ('a -> 'a -> 'a) ->
  over:('a -> 'a -> 'a) ->
  'a * 'a * 'a ->
  unit
(** [distributive w op ~over (a, b, c)] checks
    [op a (over b c) = over (op a b) (op a c)], then
    [op (over a b) c = over (op a c) (op b c)], as ["distributive"]. [op a c] is
    computed once. *)

val idempotent :
  ?__POS__:Loc.pos -> ?msg:string -> 'a Testable.t -> ('a -> 'a) -> 'a -> unit
(** [idempotent w f x] checks [f (f x) = f x] as ["idempotent"]. Demands
    ["f x differs from x"]. *)

val involutive :
  ?__POS__:Loc.pos -> ?msg:string -> 'a Testable.t -> ('a -> 'a) -> 'a -> unit
(** [involutive w f x] checks [f (f x) = x] as ["involutive"], over [f x].
    Demands ["f x differs from x"]. *)

val commutes :
  ?__POS__:Loc.pos ->
  ?msg:string ->
  'a Testable.t ->
  ('a -> 'a) ->
  ('a -> 'a) ->
  'a ->
  unit
(** [commutes w f g x] checks [f (g x) = g (f x)] as ["commutes"], over [g x]
    and [f x]. *)

val homomorphic :
  ?__POS__:Loc.pos ->
  ?msg:string ->
  'a Testable.t ->
  'b Testable.t ->
  ('a -> 'b) ->
  ('a -> 'a -> 'a) ->
  ('b -> 'b -> 'b) ->
  'a * 'a ->
  unit
(** [homomorphic wa wb f op op' (a, b)] checks [f (op a b) = op' (f a) (f b)]
    under [wb] as ["homomorphic"], over [op a b], [f a] and [f b]. *)

(** {1:conversions Conversions and relations} *)

val round_trip :
  ?__POS__:Loc.pos ->
  ?msg:string ->
  'a Testable.t ->
  'b Testable.t ->
  ('a -> 'b) ->
  ('b -> 'a) ->
  'a ->
  unit
(** [round_trip wa wb f g x] checks [g (f x) = x] under [wa] as ["round trip"],
    over [f x] printed by [wb]. *)

val monotone :
  ?__POS__:Loc.pos ->
  ?msg:string ->
  'a Testable.t ->
  'b Testable.t ->
  ('a -> 'b) ->
  'a * 'a ->
  unit
(** [monotone wa wb f (a, b)] sorts the pair by [wa]'s order and checks
    [a <= b implies f a <= f b] as ["monotone"], over [cmp a b], [f a], [f b]
    and [cmp (f a) (f b)]. When [cmp a b = 0] it holds iff
    [cmp (f a) (f b) = 0], since each of [a] and [b] is below the other. Demands
    ["a strict pair"].

    Raises [Invalid_argument] if [wa] or [wb] has no order, before any clause.
*)

val ignores :
  ?__POS__:Loc.pos ->
  ?msg:string ->
  'a Testable.t ->
  'b Testable.t ->
  ('a -> 'b) ->
  ('a -> 'a) ->
  'a ->
  unit
(** [ignores wa wb f g x] checks [f (g x) = f x] under [wb] as ["ignores"], over
    [g x]. Demands ["g x differs from x"]. *)

val preserves :
  ?__POS__:Loc.pos ->
  ?msg:string ->
  'a Testable.t ->
  ('a -> 'a) ->
  ('a -> bool) ->
  'a ->
  unit
(** [preserves w f inv x] checks, as ["preserves"]:
    - ["premise"] [inv x].
    - [inv x implies inv (f x)], over [x], [inv x], [f x] and [inv (f x)]. *)
