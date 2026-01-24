(*---------------------------------------------------------------------------
   Copyright (c) 2013-2017 Guillaume Bury, Simon Cruanes, Vincent Hugot,
                           Jan Midtgaard
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: BSD-2-Clause
  ---------------------------------------------------------------------------*)

(** Shrink trees for integrated shrinking.

    A shrink tree couples a generated value with its lazy shrink candidates.
    This ensures shrinks automatically respect generator invariants. *)

type 'a t =
  | Tree of 'a * 'a t Seq.t
      (** A tree with a root value and lazy children representing shrink
          candidates. *)

val root : 'a t -> 'a
(** [root t] returns the root value of the tree. *)

val children : 'a t -> 'a t Seq.t
(** [children t] returns the lazy sequence of shrink candidate trees. *)

val pure : 'a -> 'a t
(** [pure x] creates a tree with [x] as root and no shrinks. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f t] applies [f] to all values in the tree. *)

val ap : ('a -> 'b) t -> 'a t -> 'b t
(** [ap ft xt] applies function tree [ft] to value tree [xt]. Shrinks are
    interleaved from both trees. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind t f] applies [f] to the root of [t] and merges the resulting shrink
    candidates from both [t] and [f]'s output. *)

val make_primitive : ('a -> 'a Seq.t) -> 'a -> 'a t
(** [make_primitive shrink x] creates a tree rooted at [x] whose children are
    produced by [shrink x], with [shrink] applied recursively to each child. The
    tree is built lazily. *)

val add_shrink_invariant : ('a -> bool) -> 'a t -> 'a t
(** [add_shrink_invariant p t] filters shrink candidates to only those
    satisfying predicate [p]. *)

val opt : 'a t -> 'a option t
(** [opt t] lifts a tree to an option tree, shrinking towards [None]. *)

val sequence_list : 'a t list -> 'a list t
(** [sequence_list ts] combines a list of trees into a tree of lists. Shrink
    candidates are interleaved from all element trees via applicative
    combination. *)

val build_list_shrink_tree : 'a t list -> 'a list t Seq.t
(** [build_list_shrink_tree ts] produces shrink candidates for a list of trees.
    For small lists (fewer than 4 elements), tries dropping each element in
    turn. For larger lists, uses bisection to try removing halves first, then
    falls back to element-wise shrinking. *)

(** {1 Operators} *)

val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
(** [t >|= f] is [map f t]. *)

val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
(** [ft <*> xt] is [ap ft xt]. *)

val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
(** [t >>= f] is [bind t f]. *)

val liftA2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
(** [liftA2 f a b] lifts a binary function over two trees. *)
