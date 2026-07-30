(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Test tags and tag-based selection.

    Tags are plain strings attached to tests and groups ([?tags:string list] on
    the declaration surface); a test's effective tag set is the union of its own
    tags and its ancestors'. Selection ([--tag], [--exclude-tag], [--quick]) is
    expressed as a {!predicate} over tag sets.

    Two tag names carry built-in meaning: {!slow} (pre-applied by the [slow]
    test constructor, dropped by [--quick]) and {!disabled} (dropped by
    {!default_predicate}). *)

(** {1:tags Tag sets} *)

type t
(** The type for immutable sets of tag names. *)

val empty : t
(** [empty] is the set with no tags. *)

val of_list : string list -> t
(** [of_list names] is the set of the tags in [names]. *)

val add : string -> t -> t
(** [add name tags] is [tags] with [name] added. *)

val union : t -> t -> t
(** [union parent child] is the union of both sets — a test's effective tags
    given its ancestry. *)

val mem : string -> t -> bool
(** [mem name tags] is [true] iff [name] is in [tags]. *)

val is_empty : t -> bool
(** [is_empty tags] is [true] iff [tags] has no elements. *)

val to_list : t -> string list
(** [to_list tags] is the elements of [tags] in increasing order. *)

(** {1:known Well-known tags} *)

val slow : string
(** [slow] is ["slow"]: pre-applied by the [slow] declaration constructor and
    dropped by the [--quick] flag. *)

val disabled : string
(** [disabled] is ["disabled"]: tests carrying it are skipped by
    {!default_predicate} without any flag. *)

(** {1:predicates Selection predicates}

    A predicate holds a set of required tags and a set of dropped tags. A tag
    set is accepted when it contains every required tag and none of the dropped
    ones. A tag cannot be both required and dropped: adding it to one set
    removes it from the other, so the last flag wins. *)

type predicate
(** The type for tag selection predicates. *)

val accept_all : predicate
(** [accept_all] accepts every tag set. *)

val default_predicate : predicate
(** [default_predicate] is {!accept_all} with {!disabled} dropped — the runner's
    starting point. *)

val require : string -> predicate -> predicate
(** [require name p] is [p] requiring [name]; [name] is no longer dropped. *)

val drop : string -> predicate -> predicate
(** [drop name p] is [p] dropping [name]; [name] is no longer required. *)

val accepts : predicate -> t -> bool
(** [accepts p tags] is [true] iff [tags] contains every required tag of [p] and
    none of its dropped tags. *)
