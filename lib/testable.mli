(*---------------------------------------------------------------------------
   Copyright (c) 2020-2021 Craig Ferguson
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The assertion-side witness: printing and equality, nothing else.

    An ['a t] tells the equality assertions how to compare values of type ['a]
    and how to render them into failure payloads. Both functions must be total:
    they run on every assertion, and a raising printer or equality turns a
    report into a crash.

    Build witnesses with {!make} (equality is required — polymorphic equality is
    the explicitly named {!structural}), compose them with the container
    instances and {!contramap}, and ignore components with {!pass}. Diffing is
    not here: renderers compute diffs from the printed values (Law 4), so every
    type gets highlighted diffs from its [pp] alone. Generation is not here
    either: random generation lives in [Gen], the property-side witness. The two
    never merge (Law 6). *)

(** {1:types Types} *)

type 'a t
(** The type for witnesses over values of type ['a]: a printer and an equality,
    both total. *)

(** {1:constructors Constructors} *)

val make :
  pp:(Format.formatter -> 'a -> unit) -> equal:('a -> 'a -> bool) -> 'a t
(** [make ~pp ~equal] is a witness comparing with [equal] and printing with
    [pp]. Both must be total over the values the tests exercise. *)

val structural : pp:(Format.formatter -> 'a -> unit) -> 'a t
(** [structural ~pp] is [make ~pp] with polymorphic structural equality
    [Stdlib.( = )] — the choice is explicit in the name. Structural equality
    raises on functional values and loops on cyclic ones; give such types a real
    [equal] via {!make}. *)

(** The module shape {!of_module} consumes: a type with the conventional
    [t]/[pp]/[equal] trio. Modules with additional members match unchanged. *)
module type WITNESS = sig
  type t
  (** The type being witnessed. *)

  val pp : Format.formatter -> t -> unit
  (** [pp ppf v] formats [v] into failure payloads. Must be total. *)

  val equal : t -> t -> bool
  (** [equal a b] is [true] iff [a] and [b] are equal. Must be total. *)
end

val of_module : (module WITNESS with type t = 'a) -> 'a t
(** [of_module (module M)] is [make ~pp:M.pp ~equal:M.equal]: the witness of a
    module following the conventional naming, written without repeating the
    module name.

    {[
      let policy = Testable.of_module (module Policy)
    ]} *)

val of_equal : ('a -> 'a -> bool) -> 'a t
(** [of_equal equal] is a witness comparing with [equal] and printing every
    value as ["<abstract>"].

    {b Warning.} The footgun is deliberate and visible: failures involving this
    witness show [<abstract>] on both sides and cannot show a diff. Reach for
    {!make} as soon as the type has any printable rendering. *)

val contramap : ('a -> 'b) -> 'b t -> 'a t
(** [contramap f w] compares and prints values of type ['a] through their image
    under [f]: equality is [w]'s on [f a] and [f b], and failures render [f a],
    not [a].

    {[
      type user = { id : int; name : string }

      let user_id = Testable.(contramap (fun u -> u.id) int)
    ]} *)

val pass : 'a t
(** [pass] considers all values equal and prints ["<pass>"]. Use it to ignore a
    component of a composed witness, e.g. [pair string pass]. *)

(** {1:observers Observers} *)

val pp : 'a t -> Format.formatter -> 'a -> unit
(** [pp w ppf v] formats [v] with [w]'s printer. *)

val equal : 'a t -> 'a -> 'a -> bool
(** [equal w a b] is [true] iff [w]'s equality considers [a] and [b] equal. *)

val to_string : 'a t -> 'a -> string
(** [to_string w v] is [v] printed with [w]'s printer as a string — the
    rendering assertion verbs store in failure payloads. *)

(** {1:instances Instances}

    Base-type witnesses print source-like renderings ([%S] for strings, [%C] for
    chars, [%g] for the tolerance float witnesses), so failure payloads read
    like OCaml values. *)

val unit : unit t
val bool : bool t
val char : char t

val string : string t
(** [string] prints with [%S]: quoted, with escapes, on one line. *)

val bytes : bytes t
val int : int t
val int32 : int32 t
val int64 : int64 t
val nativeint : nativeint t

val float_exact : float t
(** [float_exact] compares floats exactly: [a] and [b] are equal iff both are
    NaN, or they are bit-for-bit the same float. In particular:

    - NaN equals NaN — every NaN, regardless of payload or sign — so a test can
      assert that a function actually returns NaN, unlike with IEEE 754 equality
      (and unlike {!float}).
    - [0.] and [-0.] are {e not} equal (unlike [Stdlib.Float.equal]): producing
      the wrong zero is an observable bug — [1. /. -0.] is negative infinity.
    - An infinity is equal only to an infinity of the same sign.

    Failures print the shortest decimal that round-trips to the exact value
    ([0.1 +. 0.2] prints ["0.30000000000000004"], never ["0.3"]; [-0.] prints
    ["-0"]), so unequal floats never render identically. *)

val float : float -> float t
(** [float eps] compares with absolute tolerance: [a] and [b] are equal when
    [a = b] or [|a -. b| <= eps]. NaN follows IEEE 754: it is equal to nothing,
    not even itself — assert a NaN result with {!float_exact}. An infinity is
    equal only to an infinity of the same sign; no finite [eps] bridges an
    infinite and a finite value. [0.] and [-0.] are equal. *)

val float_rel : rel:float -> abs:float -> float t
(** [float_rel ~rel ~abs] compares with combined tolerance: [a] and [b] are
    equal when [a = b], when their absolute difference is within [abs]
    (near-zero values), or when it is within
    [rel *. Float.max (abs_float a) (abs_float b)] (large values). NaN follows
    IEEE 754, as with {!float}: it is equal to nothing — assert a NaN result
    with {!float_exact}. An infinity is equal only to an infinity of the same
    sign: no finite tolerance applies when either side is infinite. *)

(** {1:containers Containers} *)

val option : 'a t -> 'a option t
val result : 'a t -> 'e t -> ('a, 'e) result t
val either : 'a t -> 'b t -> ('a, 'b) Either.t t
val list : 'a t -> 'a list t
val array : 'a t -> 'a array t

val slist : 'a t -> ('a -> 'a -> int) -> 'a list t
(** [slist w cmp] compares lists as multisets: both are sorted with [cmp] before
    elementwise comparison with [w], so order is ignored but multiplicity is
    not. Printing shows the list sorted with [cmp] — the order the equality
    compared — so a failure's diff shows the multiset difference, never the
    incidental arrival order. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
