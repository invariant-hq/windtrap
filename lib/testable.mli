(*---------------------------------------------------------------------------
   Copyright (c) 2020-2021 Craig Ferguson
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Type-safe equality witnesses for assertions and property testing.

    A testable bundles pretty-printing, equality, and optionally a random
    generator for a given type. Built-in testables are provided for primitives
    and common containers. Compose them with {!pair}, {!list}, {!option}, etc.

    Container testables ({!list}, {!array}, {!string}) produce highlighted diffs
    on assertion failure, pinpointing changed elements. *)

type 'a t
(** A testable witness for values of type ['a]. *)

(** {1 Result of a Custom Check} *)

type 'a check_result =
  | Pass  (** The values are equal. *)
  | Fail of {
      expected_str : string;
      actual_str : string;
      diff : Failure.diff_output option;
    }  (** The values differ, with optional structured diff. *)

(** {1 Construction} *)

val make :
  pp:'a Pp.t ->
  equal:('a -> 'a -> bool) ->
  ?gen:'a Windtrap_prop.Gen.t ->
  ?check:('a -> 'a -> 'a check_result) ->
  unit ->
  'a t
(** [make ~pp ~equal ()] creates a testable.

    @param gen
      Random generator for property-based testing with {!Windtrap.prop}.
    @param check
      Custom comparison that produces structured diff output on failure. When
      provided, assertions use [check] instead of [equal]. *)

(** {1 Accessors} *)

val pp : 'a t -> 'a Pp.t
(** [pp t] returns the pretty-printer. *)

val equal : 'a t -> 'a -> 'a -> bool
(** [equal t] returns the equality function. *)

val gen : 'a t -> 'a Windtrap_prop.Gen.t option
(** [gen t] returns the random generator, if any. *)

val check : 'a t -> ('a -> 'a -> 'a check_result) option
(** [check t] returns the custom check function, if any. *)

val to_string : 'a t -> 'a -> string
(** [to_string t v] pretty-prints [v] to a string using [t]'s printer. *)

(** {1 Primitive Testables} *)

val unit : unit t
val bool : bool t
val int : int t
val int32 : int32 t
val int64 : int64 t
val nativeint : nativeint t

val float : float -> float t
(** [float eps] creates a testable with absolute tolerance [eps]. Two floats [a]
    and [b] are equal when [|a - b| <= eps]. NaN equals NaN for testing
    purposes. *)

val float_rel : rel:float -> abs:float -> float t
(** [float_rel ~rel ~abs] creates a testable with both relative and absolute
    tolerance. Two floats [a] and [b] are equal when their difference is within
    [abs] or within [rel * max(|a|, |b|)]. NaN equals NaN. *)

val char : char t
val string : string t
val bytes : bytes t

(** {1 Container Testables} *)

val option : 'a t -> 'a option t
val result : 'a t -> 'b t -> ('a, 'b) result t

val either : 'a t -> 'b t -> ('a, 'b) Either.t t
(** [either left right] creates a testable for [Either.t] values. *)

val list : 'a t -> 'a list t
(** [list t] creates a testable for lists. Produces highlighted diffs on
    failure, pinpointing changed elements using Levenshtein edit distance. *)

val array : 'a t -> 'a array t
(** [array t] creates a testable for arrays. Produces highlighted diffs on
    failure, like {!list}. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

(** {1 Combinators} *)

val pass : 'a t
(** [pass] always considers values equal. Useful for ignoring parts of a
    structure. *)

val slist : 'a t -> ('a -> 'a -> int) -> 'a list t
(** [slist t cmp] compares lists as sets, ignoring order. Both lists are sorted
    with [cmp] before comparison. *)

val of_equal : ('a -> 'a -> bool) -> 'a t
(** [of_equal eq] creates a testable from an equality function. Uses
    ["<opaque>"] as the printed representation. *)

val contramap : ('a -> 'b) -> 'b t -> 'a t
(** [contramap f t] transforms values with [f] before comparing with [t].

    The resulting testable has no generator.

    {[
      type user = { id : int; name : string }

      let user_id = Testable.contramap (fun u -> u.id) Testable.int
    ]} *)

val seq : 'a t -> 'a Seq.t t
(** [seq t] creates a testable for sequences. Consumes both sequences during
    comparison. *)

val lazy_t : 'a t -> 'a Lazy.t t
(** [lazy_t t] creates a testable for lazy values. Forces both values before
    comparing. *)
