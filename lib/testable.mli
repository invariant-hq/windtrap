(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The assertion-side witness: printing, equality, and an optional order.

    An ['a t] tells the equality verbs how to compare and render values of type
    ['a], and the ordering verbs, when it carries an order, how to rank them.
    Every function must be total: a raising printer or equality turns a report
    into a crash. Build witnesses with {!make}, give them an order with
    {!with_compare}, compose with the container instances and {!contramap}.
    Diffs are computed by renderers from the printed values; generation lives in
    [Gen]. *)

(** {1:types Types} *)

type 'a t
(** The type for witnesses: a printer, an equality and optionally an order, all
    total. The equality is applied [expected] first, [actual] second; the
    equality verbs never read the order and the ordering verbs never read the
    equality. *)

(** {1:constructors Constructors} *)

val make :
  pp:(Format.formatter -> 'a -> unit) -> equal:('a -> 'a -> bool) -> 'a t
(** [make ~pp ~equal] is a witness comparing with [equal], printing with [pp],
    and carrying no order. [equal] receives [expected] first, [actual] second,
    so an asymmetric [equal] treats the two sides differently; prefer a
    symmetric one, especially for tolerances ({!float_rel} is the worked
    example). *)

val with_compare : ('a -> 'a -> int) -> 'a t -> 'a t
(** [with_compare compare w] is [w] ordered by [compare], replacing any order it
    had. [compare] follows [Stdlib.compare]'s contract and must be total. The
    order is never reconciled with [w]'s equality. A witness without one makes
    every ordering verb raise [Invalid_argument] naming this function. *)

val structural : pp:(Format.formatter -> 'a -> unit) -> 'a t
(** [structural ~pp] is [make ~pp] with [Stdlib.( = )] and [Stdlib.compare].
    Both raise on functional values and loop on cyclic ones. *)

val of_equal : ('a -> 'a -> bool) -> 'a t
(** [of_equal equal] is a witness comparing with [equal], printing every value
    as ["<abstract>"], and carrying no order. Failures show [<abstract>] on both
    sides and no diff; reach for {!make} as soon as the type prints. *)

val contramap : ('a -> 'b) -> 'b t -> 'a t
(** [contramap f w] compares, orders and prints values of type ['a] through
    their image under [f]; failures render [f a], not [a].
    [contramap String.length int] orders strings by length. *)

val pass : 'a t
(** [pass] considers all values equal, prints ["<pass>"], and carries no order:
    it ignores a component of a composed witness, e.g. [pair string pass]. *)

(** {1:observers Observers} *)

val pp : 'a t -> Format.formatter -> 'a -> unit
(** [pp w ppf v] formats [v] with [w]'s printer. *)

val equal : 'a t -> 'a -> 'a -> bool
(** [equal w a b] is [true] iff [w]'s equality considers [a] and [b] equal; the
    verbs pass [expected] as [a] and [actual] as [b]. *)

val compare : 'a t -> ('a -> 'a -> int) option
(** [compare w] is [w]'s order when it carries one (the base-type instances,
    {!structural}, anything through {!with_compare}, and a {!contramap} of
    those) and [None] otherwise. *)

val to_string : 'a t -> 'a -> string
(** [to_string w v] is [v] printed with [w]'s printer as a string, the rendering
    stored in failure payloads. *)

(** {1:instances Instances}

    Base-type witnesses print source-like renderings ([%S] for strings, [%C] for
    chars, [%g] for the tolerance float witnesses) and carry their module's
    order ([Int.compare], [String.compare], …). *)

val unit : unit t
val bool : bool t
val char : char t

val string : string t
(** [string] prints with [%S]: quoted, escaped, on one line. *)

val text : string t
(** [text] is {!string} printed verbatim: no quotes, no escapes, newlines kept,
    so failures diff it line by line. The witness for multi-line text; prefer
    {!string} for single-line values, where the quotes distinguish [""], [" "]
    and ["\t"]. *)

val bytes : bytes t
val int : int t
val int32 : int32 t
val int64 : int64 t
val nativeint : nativeint t

(** Under {!float} and {!float_rel} NaN is equal to nothing, itself included;
    under {!float_exact} every NaN equals every NaN. Under all three an infinity
    is equal only to an infinity of the same sign; [0.] and [-0.] are equal
    under the tolerance witnesses and distinct under {!float_exact}. All three
    order with [Float.compare]. *)

val float_exact : float t
(** [float_exact] compares floats bit for bit, all NaNs identified. Failures
    print the shortest decimal that round-trips to the exact value ([0.1 +. 0.2]
    prints ["0.30000000000000004"]), so unequal floats never render identically.
*)

val float : float -> float t
(** [float eps] compares with absolute tolerance: [a] and [b] are equal when
    [a = b] or [|a -. b| <= eps].

    Raises [Invalid_argument] if [eps] is not strictly positive (NaN included);
    exactness is spelled {!float_exact}. *)

val float_rel : rel:float -> abs:float -> float t
(** [float_rel ~rel ~abs] compares with combined tolerance: [a] and [b] are
    equal when [a = b], when [|a -. b| <= abs], or when
    [|a -. b| <= rel *. Float.max (abs_float a) (abs_float b)].

    Raises [Invalid_argument] if either bound is negative or NaN, or if both are
    zero; one zero bound is a purely absolute or purely relative tolerance. *)

(** {1:containers Containers}

    No container witness carries an order, whatever its components carry; spell
    the one you mean with {!with_compare}. *)

val option : 'a t -> 'a option t
val result : 'a t -> 'e t -> ('a, 'e) result t
val either : 'a t -> 'b t -> ('a, 'b) Either.t t
val list : 'a t -> 'a list t
val array : 'a t -> 'a array t

val slist : 'a t -> ('a -> 'a -> int) -> 'a list t
(** [slist w cmp] is [contramap (List.sort cmp) (list w)]: lists compared, and
    printed, as multisets. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
