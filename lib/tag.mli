(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Test metadata tags.

    Tags allow attaching metadata to tests for filtering and categorization.
    Speed is inherited: a test's effective speed is determined by the innermost
    explicitly-set speed in its ancestry chain. *)

(** {1 Speed Levels} *)

type speed =
  | Quick  (** Fast tests, run by default. *)
  | Slow  (** Slow tests, skipped with [--quick]. *)

(** {1 Tag Set} *)

type t
(** An immutable set of tags attached to a test. *)

val empty : t
(** The empty tag set. Speed is unspecified (inherits from parent). *)

val speed : speed -> t
(** [speed s] creates a tag set with the given speed level. *)

val add_label : string -> t -> t
(** [add_label label t] adds a string label to [t]. *)

val labels : string list -> t
(** [labels ls] creates a tag set containing only the given labels. *)

val merge : t -> t -> t
(** [merge a b] combines two tag sets. Speed from [b] takes precedence when [b]
    has an explicit speed; otherwise [a]'s speed is kept. Labels are unioned. *)

(** {1 Querying} *)

val get_speed : t -> speed
(** [get_speed t] returns the speed level. Defaults to [Quick] when no speed was
    explicitly set. *)

val get_labels : t -> string list
(** [get_labels t] returns all labels in sorted order. *)

val has_label : string -> t -> bool
(** [has_label label t] returns [true] if [t] contains [label]. *)

(** {1 Tag Predicates}

    Predicates provide flexible tag-based filtering with required and dropped
    tags. A test is skipped if:
    - It lacks any of the required tags, OR
    - It has any of the dropped tags.

    A tag cannot be both required and dropped. Adding a tag to one set
    automatically removes it from the other. *)

type predicate
(** A predicate specifying which tags to require or drop. *)

val initial_predicate : predicate
(** Default predicate that drops tests with the ["disabled"] label. *)

val empty_predicate : predicate
(** Predicate that accepts all tests. *)

val drop_tag : string -> predicate -> predicate
(** [drop_tag tag pred] adds [tag] to the dropped set. If [tag] was previously
    required, it is removed from the required set. *)

val require_tag : string -> predicate -> predicate
(** [require_tag tag pred] adds [tag] to the required set. If [tag] was
    previously dropped, it is removed from the dropped set. *)

val filter_predicate : predicate -> t -> [ `Run | `Skip ]
(** [filter_predicate pred tags] returns [`Run] if [tags] satisfies [pred], or
    [`Skip] otherwise. *)
