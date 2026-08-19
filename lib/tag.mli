(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Test tags and tag-based selection.

    Tags are plain strings attached to tests and groups ([?tags:string list] on
    the declaration surface); a test's effective tag set is the union of its own
    tags and its ancestors'. Selection ([--tag], [--exclude-tag]) is expressed
    as a {!predicate} over tag sets.

    One tag name carries built-in meaning: {!slow}, pre-applied by the [slow]
    test constructor. It is an ordinary tag: [--exclude-tag slow] drops it. *)

(** {1:tags Tag sets} *)

type t
(** The type for immutable sets of tag names. *)

val empty : t
(** [empty] is the set with no tags. *)

val of_list : string list -> t
(** [of_list names] is the set of the tags in [names]. *)

val union : t -> t -> t
(** [union parent child] is the union of both sets — a test's effective tags
    given its ancestry. *)

val mem : string -> t -> bool
(** [mem name tags] is [true] iff [name] is in [tags]. *)

(** {1:known Well-known tags} *)

val slow : string
(** [slow] is ["slow"]: pre-applied by the [slow] declaration constructor. *)

(** {1:predicates Selection predicates}

    A predicate holds a set of required tags and a set of dropped tags. A tag
    set is accepted when it contains every required tag and none of the dropped
    ones. A tag cannot be both required and dropped: adding it to one set
    removes it from the other, so the last flag wins.

    Selection starts from {!any} and refines it with {!require} and {!drop}, one
    call per flag. *)

type predicate
(** The type for tag selection predicates. *)

val any : predicate
(** [any] requires nothing and drops nothing: it accepts every tag set. *)

val require : string -> predicate -> predicate
(** [require name p] is [p] requiring [name]; [name] is no longer dropped. *)

val drop : string -> predicate -> predicate
(** [drop name p] is [p] dropping [name]; [name] is no longer required. *)

val accepts : predicate -> t -> bool
(** [accepts p tags] is [true] iff [tags] contains every required tag of [p] and
    none of its dropped tags. *)
