(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(** Memoized lazy rose trees of shrink candidates.

    [Shrink_tree] is the shrink search space shared by generators and the
    property engine: a strict root value with a lazy, ordered sequence of
    candidate subtrees. Child sequences are memoized one cell at a time,
    including raised exceptions, so a shrink search that revisits a branch never
    re-runs user code for it.

    The sequences passed to {!make} and the functions passed to {!map} and
    {!bind} are caller code and run at the documented forcing points. Generator
    callers must keep them deterministic, effect-free, and independent of random
    state: sampling must capture all random choices before constructing a tree,
    and this module itself performs no random operations. *)

type 'a t
(** The type for a value and its ordered shrink candidates.

    Every forced child-sequence cell is evaluated at most once. Repeatedly
    forcing a cell returns the same child, or reraises its cached exception,
    without evaluating its source again. Unforced tails stay unevaluated, and
    trees may be infinite. *)

(** {1:constructors Constructors} *)

val make : root:'a -> children:'a t Seq.t -> 'a t
(** [make ~root ~children] is a tree rooted at [root] whose immediate candidates
    are produced by [children].

    Construction does not force [children]. Each cell of [children] is forced at
    most once through the resulting tree. *)

val leaf : 'a -> 'a t
(** [leaf root] is a tree rooted at [root] with no candidates. *)

(** {1:queries Queries} *)

val root : 'a t -> 'a
(** [root tree] is [tree]'s root value. It forces no child. *)

val children : 'a t -> 'a t Seq.t
(** [children tree] produces [tree]'s immediate candidates in declared order.

    The returned sequence is persistent: repeated forcing physically reuses a
    successfully produced child, and if forcing a cell raised, subsequent forces
    reraise the cached exception without re-evaluation. Forcing a cell does not
    force its tail. *)

(** {1:composition Composition} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f tree] maps [f] over every value in [tree], preserving shape and child
    order.

    [f] is applied to the root during construction; descendants are mapped when
    their sequence cells are forced, each at most once. Effects or exceptions
    from [f] occur at those points — mapping does not make an effectful callback
    pure. The functor laws hold when [f] is deterministic and effect-free. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind tree f] substitutes a tree for every value of [tree]. Its root is
    [root (f (root tree))]. Its immediate candidates first rebind [tree]'s
    candidates in order — the outer value shrinks before the inner one — then
    continue with the immediate candidates of [f (root tree)].

    [f] is applied to [tree]'s root during construction and to descendant roots
    at their forcing points, each at most once, under the same determinism
    obligation as {!map}. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair left right] combines two trees. Its root is [(root left, root right)].
    Its immediate candidates first reduce [left], in order, retaining [right],
    then reduce [right], in order, retaining [left]. The right sequence stays
    unforced until the left one is exhausted. *)

val list : 'a t list -> 'a list t
(** [list trees] combines element trees into a tree of lists.

    The root holds the element roots in input order. For a non-empty input,
    immediate candidates come in this order:

    - the empty list;
    - contiguous full-chunk removals, with descending power-of-two chunk sizes
      strictly below the input length and increasing non-overlapping starts;
    - single-element reductions from left to right, preserving each element's
      child order.

    Candidate trees recursively follow the same rules. Constructing the root is
    stack-safe for large flat lists and forces no element children. *)
