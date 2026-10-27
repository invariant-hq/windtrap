(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Assertion witnesses.

    A witness for a type is a printer, an equality and an optional order.
    {!make} builds a witness from a printer and an equality, {!with_compare}
    gives it an order, and {!contramap} and the
    {{!section-containers}container constructors} compose witnesses. *)

(** {1:witnesses Witnesses} *)

type 'a t
(** The type for witnesses of ['a] values (see {!Windtrap.type-testable}). *)

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
    [with_compare Stdlib.compare (make ~pp ~equal:Stdlib.( = ))]. *)

val of_equal : ('a -> 'a -> bool) -> 'a t
(** [of_equal equal] is {!make} with [equal] and a printer that prints every
    value as [<abstract>]. Prefer {!make} once the type has a printer. *)

val contramap : ('a -> 'b) -> 'b t -> 'a t
(** [contramap f w] is the witness that prints, compares and orders a value [a]
    as [w] does [f a]. It carries an order iff [w] does.
    [contramap String.length int] compares and orders strings by length. *)

val pass : 'a t
(** See {!Windtrap.pass}. *)

val pp : 'a t -> Format.formatter -> 'a -> unit
(** [pp w] is [w]'s printer. *)

val equal : 'a t -> 'a -> 'a -> bool
(** [equal w a b] is [true] iff [a] and [b] are equal under [w]. *)

val compare : 'a t -> ('a -> 'a -> int) option
(** [compare w] is [w]'s order, or [None] when [w] carries none. *)

val to_string : 'a t -> 'a -> string
(** [to_string w v] is [v] printed with [w]'s printer, as a string. *)

(** {1:instances Instances} *)

val unit : unit t
(** See {!Windtrap.unit}. *)

val bool : bool t
(** See {!Windtrap.bool}. *)

val char : char t
(** See {!Windtrap.char}. *)

val uchar : Uchar.t t
(** See {!Windtrap.uchar}. *)

val string : string t
(** See {!Windtrap.string}. *)

val text : string t
(** See {!Windtrap.text}. *)

val bytes : bytes t
(** See {!Windtrap.bytes}. *)

val int : int t
(** See {!Windtrap.int}. *)

val int32 : int32 t
(** See {!Windtrap.int32}. *)

val int64 : int64 t
(** See {!Windtrap.int64}. *)

val nativeint : nativeint t
(** See {!Windtrap.nativeint}. *)

(** {2:floats Floats} *)

val float_exact : float t
(** See {!Windtrap.float_exact}. *)

val float : float -> float t
(** See {!Windtrap.float}. *)

val float_rel : rel:float -> abs:float -> float t
(** See {!Windtrap.float_rel}. *)

(** {1:containers Containers} *)

val option : 'a t -> 'a option t
(** See {!Windtrap.option}. *)

val result : 'a t -> 'e t -> ('a, 'e) result t
(** See {!Windtrap.result}. *)

val either : 'a t -> 'b t -> ('a, 'b) Either.t t
(** See {!Windtrap.either}. *)

val list : 'a t -> 'a list t
(** See {!Windtrap.list}. *)

val array : 'a t -> 'a array t
(** See {!Windtrap.array}. *)

val slist : 'a t -> ('a -> 'a -> int) -> 'a list t
(** See {!Windtrap.slist}. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** See {!Windtrap.pair}. *)

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** See {!Windtrap.triple}. *)

val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
(** See {!Windtrap.quad}. *)
