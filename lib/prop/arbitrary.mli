(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Arbitrary values bundle generators with printers.

    An [Arbitrary.t] combines a {!Gen.t} with a printer for displaying
    counterexamples. Most users should use [Testable] values with
    [Windtrap.prop] instead of using this module directly. *)

type 'a t
(** An arbitrary value: generator + printer. *)

val make : gen:'a Gen.t -> print:('a -> string) -> 'a t
(** [make ~gen ~print] creates an arbitrary from a generator and printer. *)

val gen : 'a t -> 'a Gen.t
(** [gen arb] returns the generator. *)

val print : 'a t -> 'a -> string
(** [print arb x] formats [x] for display. *)

(** {1 Primitives} *)

val unit : unit t
(** Generates [()]. *)

val bool : bool t
(** Generates [true] or [false]. Shrinks toward [false]. *)

val int : int t
(** Generates integers. Shrinks toward [0]. *)

val int_range : int -> int -> int t
(** [int_range low high] generates integers in [[low, high]]. Shrinks toward
    value closest to [0] within range.

    @raise Invalid_argument if [high < low]. *)

val int32 : int32 t
(** Generates 32-bit integers using full range. Shrinks toward [0l]. *)

val int32_range : int32 -> int32 -> int32 t
(** [int32_range low high] generates 32-bit integers in [[low, high]]. Shrinks
    toward value closest to [0l] within range.

    @raise Invalid_argument if [high < low]. *)

val int64 : int64 t
(** Generates 64-bit integers using full range. Shrinks toward [0L]. *)

val int64_range : int64 -> int64 -> int64 t
(** [int64_range low high] generates 64-bit integers in [[low, high]]. Shrinks
    toward value closest to [0L] within range.

    @raise Invalid_argument if [high < low]. *)

val float : float t
(** Generates floats. Shrinks toward [0.0]. *)

val char : char t
(** Generates characters in the [0..255] byte range. *)

val string : string t
(** Generates strings of bytes. *)

val bytes : bytes t
(** Generates bytes. *)

(** {1 Containers} *)

val option : 'a t -> 'a option t
(** [option arb] generates [None] or [Some x]. Shrinks toward [None]. *)

val result : 'a t -> 'e t -> ('a, 'e) result t
(** [result ok_arb err_arb] generates [Ok] or [Error]. *)

val list : 'a t -> 'a list t
(** [list arb] generates lists. Shrinks toward empty and smaller elements. *)

val array : 'a t -> 'a array t
(** [array arb] generates arrays. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair a b] generates pairs. *)

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** [triple a b c] generates triples. *)

val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
(** [quad a b c d] generates 4-tuples. *)

(** {1 Choice} *)

val oneof : 'a t list -> 'a t
(** [oneof arbs] picks one arbitrary uniformly at random. Uses the printer from
    the first element for all values.

    @raise Invalid_argument if [arbs] is empty. *)

val oneofl : print:('a -> string) -> 'a list -> 'a t
(** [oneofl ~print xs] picks one element uniformly at random. Shrinks toward
    earlier elements in the list.

    @raise Invalid_argument if [xs] is empty. *)

(** {1 Transformers} *)

val map : print:('b -> string) -> ('a -> 'b) -> 'a t -> 'b t
(** [map ~print f arb] maps [f] over generated values. Requires new printer
    since type changes. *)

val filter : ('a -> bool) -> 'a t -> 'a t
(** [filter p arb] filters shrink candidates to only those satisfying [p]. This
    does {e not} filter generated values; use {!Prop.assume} to discard invalid
    test cases during generation. *)
