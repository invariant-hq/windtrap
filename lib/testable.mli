(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Assertion witnesses.

    A witness for a type is a printer, an equality and an optional order. An
    assertion verb compares its two values under the witness's equality and
    prints both with its printer when they differ. The ordering verbs rank
    values under its order. {!make} builds a witness from a printer and an
    equality, {!with_compare} gives it an order, and {!contramap} and the
    {{!section-containers}container constructors} compose witnesses. *)

(** {1:witnesses Witnesses} *)

type 'a t
(** The type for witnesses of ['a] values: a printer, an equality and an
    optional order. The printer and the equality must be total, and an exception
    from either escapes the verb. The equality is applied to the expected value
    first and the actual value second.

    The equality verbs never read the order, and the ordering verbs never read
    the equality. An ordering verb on a witness without order raises
    [Invalid_argument] (see {!with_compare}). *)

val make :
  pp:(Format.formatter -> 'a -> unit) -> equal:('a -> 'a -> bool) -> 'a t
(** [make ~pp ~equal] is the witness that prints with [pp] and compares with
    [equal], with no order. An asymmetric [equal] treats the expected and the
    actual side differently. Prefer a symmetric one, as {!float_rel}'s is. *)

val with_compare : ('a -> 'a -> int) -> 'a t -> 'a t
(** [with_compare cmp w] is [w] ordered by [cmp], replacing any order [w] had.
    [cmp] must be a total order with the contract of [Stdlib.compare]. It is not
    checked against [w]'s equality. *)

val structural : pp:(Format.formatter -> 'a -> unit) -> 'a t
(** [structural ~pp] is
    [with_compare Stdlib.compare (make ~pp ~equal:Stdlib.( = ))].

    {b Warning.} Polymorphic equality and comparison raise on functional values
    and loop on cyclic ones. *)

val of_equal : ('a -> 'a -> bool) -> 'a t
(** [of_equal equal] is {!make} with [equal] and a printer that prints every
    value as [<abstract>]. A failure then shows [<abstract>] on both sides and
    no diff. Prefer {!make} once the type has a printer. *)

val contramap : ('a -> 'b) -> 'b t -> 'a t
(** [contramap f w] is the witness that prints, compares and orders a value [a]
    as [w] does [f a]. It carries an order iff [w] does.
    [contramap String.length int] compares and orders strings by length. *)

val pass : 'a t
(** [pass] is the witness under which all values are equal. It prints every
    value as [<pass>] and carries no order. [pair string pass] compares pairs by
    their first component. *)

val pp : 'a t -> Format.formatter -> 'a -> unit
(** [pp w] is [w]'s printer. *)

val equal : 'a t -> 'a -> 'a -> bool
(** [equal w a b] is [true] iff [a] and [b] are equal under [w]. *)

val compare : 'a t -> ('a -> 'a -> int) option
(** [compare w] is [w]'s order, or [None] when [w] carries none. *)

val to_string : 'a t -> 'a -> string
(** [to_string w v] is [v] printed with [w]'s printer, as a string. Failures
    store their values in this rendering. *)

(** {1:instances Instances}

    Each instance carries the order of its type's module, [Int.compare] for
    {!int}. *)

val unit : unit t
(** [unit] is the witness for [unit]. *)

val bool : bool t
(** [bool] is the witness for [bool]. *)

val char : char t
(** [char] is the witness for [char], printed with [%C]. *)

val string : string t
(** [string] is the witness for [string], printed with [%S]: quoted, escaped, on
    one line. *)

val text : string t
(** [text] is {!string} printed verbatim, newlines kept. Failures on [text] diff
    line by line. Multi-line values take [text]. Single-line values take
    {!string}, whose quotes tell [""], [" "] and ["\t"] apart. *)

val bytes : bytes t
(** [bytes] is the witness for [bytes], printed as a string literal. *)

val int : int t
(** [int] is the witness for [int]. *)

val int32 : int32 t
(** [int32] is the witness for [int32]. *)

val int64 : int64 t
(** [int64] is the witness for [int64]. *)

val nativeint : nativeint t
(** [nativeint] is the witness for [nativeint]. *)

(** {2:floats Floats}

    The three witnesses order with [Float.compare], whatever the tolerance. An
    infinity is equal only to an infinity of the same sign. Under {!float} and
    {!float_rel}, NaN is equal to nothing and [0.] equals [-0.]. Under
    {!float_exact}, every NaN equals every NaN and [0.] differs from [-0.]. *)

val float_exact : float t
(** [float_exact] compares floats bit for bit. It prints the shortest decimal
    that round-trips to the value, [0.1 +. 0.2] as [0.30000000000000004]. Two
    unequal floats never print alike. *)

val float : float -> float t
(** [float eps] compares with absolute tolerance [eps]. [a] and [b] are equal
    when [a = b] or [|a -. b| <= eps]. It prints with [%g]. Raises
    [Invalid_argument] if [eps] is not strictly positive, NaN included. *)

val float_rel : rel:float -> abs:float -> float t
(** [float_rel ~rel ~abs] compares with relative tolerance [rel] and absolute
    tolerance [abs]. [a] and [b] are equal when [a = b], when [|a -. b| <= abs],
    or when [|a -. b| <= rel *. Float.max (abs_float a) (abs_float b)]. One zero
    bound switches that component off. It prints with [%g]. Raises
    [Invalid_argument] if a bound is negative or NaN, or if both are zero. *)

(** {1:containers Containers}

    No container witness carries an order, whatever its components carry.
    {!with_compare} gives one. *)

val option : 'a t -> 'a option t
(** [option w] is the witness for ['a option] with elements under [w]. [Some a]
    and [Some b] are equal iff [a] and [b] are equal under [w]. [None] is equal
    only to [None]. *)

val result : 'a t -> 'e t -> ('a, 'e) result t
(** [result ok error] is the witness for [('a, 'e) result], [ok] for the [Ok]
    side and [error] for the [Error] side. Two values are equal iff they are on
    the same side and equal under that side's witness. *)

val either : 'a t -> 'b t -> ('a, 'b) Either.t t
(** [either left right] is {!result} for [Either.t], [left] for the [Left] side
    and [right] for the [Right] side. A value prints as [Left (v)] or
    [Right (v)]. *)

val list : 'a t -> 'a list t
(** [list w] is the witness for ['a list] with elements under [w]. Two lists are
    equal iff they have the same length and are equal element by element. A list
    prints as [[a; b; c]]. *)

val array : 'a t -> 'a array t
(** [array w] is {!list} for ['a array]. An array prints as [[|a; b; c|]]. *)

val slist : 'a t -> ('a -> 'a -> int) -> 'a list t
(** [slist w cmp] is [contramap (List.sort cmp) (list w)]. It compares and
    prints lists as multisets, sorted with [cmp]. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair a b] is the witness for pairs, [a] for the first component and [b] for
    the second. Two pairs are equal iff their components are equal under [a] and
    [b]. *)

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** [triple a b c] is {!pair} for triples. *)

val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
(** [quad a b c d] is {!pair} for quadruples. *)
