(*---------------------------------------------------------------------------
   Copyright (c) 2013-2017 Guillaume Bury, Simon Cruanes, Vincent Hugot,
                           Jan Midtgaard
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: BSD-2-Clause
  ---------------------------------------------------------------------------*)

(** Random value generators with integrated shrinking.

    Generators produce shrink trees, coupling generated values with their shrink
    candidates. This ensures shrinks automatically respect generator invariants
    (e.g., [int_range 10 100] shrinks stay within bounds). *)

type 'a t = Random.State.t -> 'a Tree.t
(** A generator takes random state and produces a shrink tree. *)

(** {1 Combinators} *)

val pure : 'a -> 'a t
(** [pure x] always generates [x] with no shrinks. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f gen] applies [f] to generated values. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind gen f] generates with [gen], then uses result to select next
    generator. *)

val ap : ('a -> 'b) t -> 'a t -> 'b t
(** [ap fgen xgen] applies generated function to generated value. Shrinks are
    interleaved from both generators. *)

(** {1 Primitives} *)

val make_primitive :
  gen:(Random.State.t -> 'a) -> shrink:('a -> 'a Seq.t) -> 'a t
(** [make_primitive ~gen ~shrink] builds a generator from a raw generation
    function and a shrink function. This is the escape hatch for building custom
    generators when the standard combinators are not sufficient. *)

val unit : unit t
(** Always generates [()]. *)

val bool : bool t
(** Generates [true] or [false]. Shrinks toward [false]. *)

val int : int t
(** Generates integers in full range. Shrinks toward [0]. *)

val int_range : ?origin:int -> int -> int -> int t
(** [int_range low high] generates integers in [[low, high]]. Shrinks toward
    [origin] (default: closest to 0 within range).

    @raise Invalid_argument if [high < low]. *)

val nat : int t
(** Generates non-negative integers in [[0, 10_000]], biased toward small
    values. Shrinks toward [0]. *)

val small_int : int t
(** Generates integers in [[-10_000, 10_000]], biased toward small absolute
    values. Shrinks toward [0]. *)

val int32 : int32 t
(** Generates 32-bit integers using full range. Shrinks toward [0l]. *)

val int32_range : ?origin:int32 -> int32 -> int32 -> int32 t
(** [int32_range low high] generates 32-bit integers in [[low, high]]. Shrinks
    toward [origin] (default: closest to [0l] within range).

    @raise Invalid_argument if [high < low]. *)

val int64 : int64 t
(** Generates 64-bit integers using full range. Shrinks toward [0L]. *)

val int64_range : ?origin:int64 -> int64 -> int64 -> int64 t
(** [int64_range low high] generates 64-bit integers in [[low, high]]. Shrinks
    toward [origin] (default: closest to [0L] within range).

    @raise Invalid_argument if [high < low]. *)

val nativeint : nativeint t
(** Generates native integers using full platform range. Shrinks toward [0n]. *)

val float : float t
(** Generates floats using bit manipulation over the full IEEE 754 range,
    including NaN, infinity, and subnormal values. Shrinks toward [0.0]. *)

val float_range : ?origin:float -> float -> float -> float t
(** [float_range low high] generates floats between [low] (inclusive) and [high]
    (exclusive). Shrinks toward [origin] (default: closest to [0.0] within
    range).

    @raise Invalid_argument if [high < low]. *)

val char : char t
(** Generates printable ASCII characters ([' '] to ['~']). Shrinks toward [' '].
*)

val char_range : ?origin:char -> char -> char -> char t
(** [char_range low high] generates characters in [[low, high]]. Shrinks toward
    [origin] (default: [low]).

    @raise Invalid_argument if [high < low]. *)

val string : string t
(** Generates strings of printable ASCII characters. Length biased toward small
    values (see {!nat}). Shrinks toward shorter strings and simpler characters.
*)

val string_of : char t -> string t
(** [string_of char_gen] generates strings using [char_gen] for characters.
    Length biased toward small values (see {!nat}). *)

val string_size : int t -> char t -> string t
(** [string_size size_gen char_gen] generates strings with length from
    [size_gen] and characters from [char_gen]. *)

val bytes : bytes t
(** Generates bytes of printable ASCII characters. Length biased toward small
    values. *)

(** {1 Containers} *)

val option : ?ratio:float -> 'a t -> 'a option t
(** [option gen] generates [None] or [Some x].
    @param ratio Probability of [Some] (default: 0.85). Shrinks toward [None].
*)

val result : ?ratio:float -> 'a t -> 'e t -> ('a, 'e) result t
(** [result ok_gen err_gen] generates [Ok] or [Error].
    @param ratio Probability of [Ok] (default: 0.75). *)

val either : ?ratio:float -> 'a t -> 'b t -> ('a, 'b) Either.t t
(** [either left_gen right_gen] generates [Left] or [Right].
    @param ratio Probability of [Left] (default: 0.5). *)

val list : 'a t -> 'a list t
(** [list gen] generates lists. Length biased toward small. Uses efficient
    bisection shrinking for large lists. *)

val list_size : int t -> 'a t -> 'a list t
(** [list_size size_gen gen] generates lists with size from [size_gen]. *)

val array : 'a t -> 'a array t
(** [array gen] generates arrays. Like [list] but returns array. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair a b] generates pairs. *)

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** [triple a b c] generates triples. *)

val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
(** [quad a b c d] generates 4-tuples. *)

(** {1 Choice} *)

val oneof : 'a t list -> 'a t
(** [oneof gens] picks one generator uniformly at random.

    @raise Invalid_argument if [gens] is empty. *)

val oneofl : 'a list -> 'a t
(** [oneofl xs] picks one element uniformly at random. Shrinks toward earlier
    elements in the list.

    @raise Invalid_argument if [xs] is empty. *)

val frequency : (int * 'a t) list -> 'a t
(** [frequency weighted_gens] picks a generator with probability proportional to
    its weight. Negative weights are treated as 0.

    @raise Invalid_argument
      if [weighted_gens] is empty or total weight is less than 1. *)

(** {1 Size Control} *)

val sized : (int -> 'a t) -> 'a t
(** [sized f] generates a size in [[0, 1000]] biased toward small values, then
    calls [f] with that size. *)

(** {1 Shrink Control} *)

val no_shrink : 'a t -> 'a t
(** [no_shrink gen] removes all shrinks from generated trees. *)

val add_shrink_invariant : ('a -> bool) -> 'a t -> 'a t
(** [add_shrink_invariant p gen] filters shrinks to only those satisfying [p].
*)

(** {1 Recursive Generators} *)

val delay : (unit -> 'a t) -> 'a t
(** [delay f] delays generator construction. Useful for recursive generators. *)

val fix : ('a t -> 'a t) -> 'a t
(** [fix f] creates a recursive generator. [f] receives the generator being
    defined as its argument.

    {[
      type tree = Leaf | Node of tree * tree

      let tree_gen =
        Gen.fix (fun self ->
            Gen.frequency
              [
                (3, Gen.pure Leaf);
                ( 1,
                  let+ l, r = Gen.pair self self in
                  Node (l, r) );
              ])
    ]} *)

(** {1 Search} *)

val find : ?count:int -> f:('a -> bool) -> 'a t -> Random.State.t -> 'a option
(** [find ~f gen state] attempts to find a generated value satisfying [f]. Tries
    up to [count] generations (default: 100). Returns [None] if no match is
    found. Does not shrink; returns the first match. *)

(** {1 Let Syntax} *)

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
(** [let+ x = gen in e] is [map (fun x -> e) gen]. *)

val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
(** [gen1 and+ gen2] is [pair gen1 gen2]. *)

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
(** [let* x = gen in e] is [bind gen (fun x -> e)]. *)

(** {1 Operators} *)

val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
(** [gen >|= f] is [map f gen]. *)

val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
(** [gen >>= f] is [bind gen f]. *)
