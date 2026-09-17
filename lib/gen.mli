(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(** Random value generators with integrated shrinking and printing.

    The internal interface. The vocabulary below is what {!Windtrap.Gen} narrows
    this module to, and its contract — each generator's distribution, shrink
    order and printing — is stated there. What is here and not there is
    {!Engine}: the side of a generator the property engine and {!Stateful} reach
    for. *)

(** {1:generators Generators} *)

type 'a t
(** The type for generators of values of type ['a]. *)

(** {1:numeric Numeric generators} *)

val int : int t
(** Uniform over the full [int] range; shrinks toward [0]. *)

val nat : int t
(** Below [10_000], biased toward small values; shrinks toward [0]. *)

val small_int : int t
(** {!nat}'s magnitude with either sign; shrinks toward [0]. *)

val int_range : int -> int -> int t
(** [int_range low high] is uniform in \[[low];[high]\]; shrinks toward the
    in-range point closest to [0]. Sampling raises [Invalid_argument] if
    [high < low]. *)

val int32 : int32 t
(** Uniform over the full [int32] range; shrinks toward [0l]. *)

val int64 : int64 t
(** Uniform over the full [int64] range; shrinks toward [0L]. *)

val nativeint : nativeint t
(** Uniform over the full [nativeint] range; shrinks toward [0n]. *)

val float : float t
(** A finite float from uniform bit patterns; shrinks toward [0.]. *)

val float_range : float -> float -> float t
(** [float_range low high] is uniform in \[[low];[high]\]; shrinks toward the
    in-range point closest to [0.]. Sampling raises [Invalid_argument] on a
    non-finite or inverted range. *)

(** {1:base Unit, booleans, characters, strings} *)

val unit : unit t
(** [()], with no candidates. *)

val bool : bool t
(** [true] or [false] with equal probability; [true] shrinks to [false]. *)

val char : char t
(** A uniform byte, NUL and non-ASCII included; shrinks toward ['a']. *)

val char_range : char -> char -> char t
(** [char_range low high] is uniform in \[[low];[high]\] by byte order; shrinks
    toward the in-range character closest to ['a']. Sampling raises
    [Invalid_argument] if [high < low]. *)

val string : string t
(** [string_of char]. *)

val string_of : ?size:int t -> char t -> string t
(** [string_of ?size char] has a {!list}-shaped length (default {!nat}) and
    [char] characters; always prints. Sampling raises [Invalid_argument] on a
    negative length. *)

val bytes : bytes t
(** [bytes_of char]. *)

val bytes_of : ?size:int t -> char t -> bytes t
(** {!string_of} converted to [bytes]. *)

val list : ?size:int t -> 'a t -> 'a list t
(** [list ?size gen]: a length from [size] (default {!nat}) of [gen] values;
    shrinks the structure first, then elements left to right. Sampling raises
    [Invalid_argument] on a negative length. *)

val array : ?size:int t -> 'a t -> 'a array t
(** {!list} converted to an array. *)

val option : 'a t -> 'a option t
(** [None] with probability 0.15; every [Some]'s first candidate is [None]. *)

val result : 'a t -> 'e t -> ('a, 'e) result t
(** [Ok] with probability 0.75; a candidate never crosses constructors. *)

val either : 'a t -> 'b t -> ('a, 'b) Either.t t
(** [Left] or [Right] with equal probability; a candidate never crosses
    constructors. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** Both components; shrinks the left first, then the right
    ({!Engine.Shrink_tree.pair}). *)

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** {!pair} for three components. *)

val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
(** {!pair} for four components. *)

val constant : 'a -> 'a t
(** Always the value, with no candidates and no printer. *)

val of_list : 'a list -> 'a t
(** One of the values, uniformly; shrinks toward the head; no printer. Sampling
    raises [Invalid_argument] on an empty list. *)

val one_of : 'a t list -> 'a t
(** One of the generators, uniformly; the choice shrinks toward earlier
    generators, then the value with its own. Sampling raises [Invalid_argument]
    on an empty list. *)

val frequency : (int * 'a t) list -> 'a t
(** {!one_of} weighted; the choice itself does not shrink. Sampling raises
    [Invalid_argument] on an empty list, a negative weight or weights summing
    below [1]. *)

val such_that : ('a -> bool) -> 'a t -> 'a t
(** [such_that p gen] re-samples [gen] up to 100 times for a value satisfying
    [p] and filters candidates by [p]; sampling raises {!Engine.Rejected} when
    no draw does. *)

(** {1:composition Composition} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f gen] shrinks wherever [gen] shrinks; no printer. [f] must be pure. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind gen f] shrinks the outer value first, re-generating with [f], then the
    inner; no printer. [f] must be pure. *)

val with_pp : (Format.formatter -> 'a -> unit) -> 'a t -> 'a t
(** [with_pp pp gen] is [gen] printing with [pp]. *)

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
(** [let+ x = gen in e] is [map (fun x -> e) gen]. *)

val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
(** [gen1 and+ gen2] is [pair gen1 gen2]. *)

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
(** [let* x = gen in e] is [bind gen (fun x -> e)]. *)

(** {1:engine Engine interface} *)

(** What the property engine and {!Stateful} reach for, and nothing a test
    writes. No stability promise beyond the frozen value stream ({!Seed}). *)
module Engine : sig
  (** Memoized lazy rose trees of shrink candidates: a strict root value with an
      ordered sequence of candidate subtrees, each cell forced at most once (a
      raised exception included), so a shrink search that revisits a branch
      never re-runs user code. The sequences passed to {!make} and the functions
      passed to {!map} run at the forcing points; a sampler must capture every
      random choice before building a tree. *)
  module Shrink_tree : sig
    type 'a t
    (** The type for a value and its ordered shrink candidates. Trees may be
        infinite. *)

    val make : root:'a -> children:'a t Seq.t -> 'a t
    (** [make ~root ~children] is a tree rooted at [root] whose immediate
        candidates are [children]. Construction forces nothing. *)

    val leaf : 'a -> 'a t
    (** [leaf root] is a tree rooted at [root] with no candidates. *)

    val root : 'a t -> 'a
    (** [root tree] is [tree]'s root value. Forces no child. *)

    val children : 'a t -> 'a t Seq.t
    (** [children tree] is [tree]'s immediate candidates in declared order. The
        sequence is persistent: a forced cell yields the same child, or reraises
        its cached exception, without re-evaluation; forcing a cell does not
        force its tail. *)

    val map : ('a -> 'b) -> 'a t -> 'b t
    (** [map f tree] maps [f] over every value, preserving shape and order. [f]
        runs on the root at once and on each descendant when its cell is forced,
        at most once. *)

    val pair : 'a t -> 'b t -> ('a * 'b) t
    (** [pair left right] is rooted at [(root left, root right)]; its candidates
        reduce [left] in order, retaining [right], then [right], retaining
        [left]. [right]'s sequence stays unforced until [left]'s is exhausted.
    *)

    val list : 'a t list -> 'a list t
    (** [list trees] is rooted at the element roots in order. For a non-empty
        input the candidates are, in order: the empty list; contiguous
        full-chunk removals with descending power-of-two chunk sizes strictly
        below the length and increasing non-overlapping starts; single-element
        reductions left to right, each in its element's child order. Candidates
        follow the same rules recursively. Building the root is stack-safe and
        forces no element child. *)
  end

  exception Rejected
  (** Raised by {!sample} when a {!Gen.such_that} filter exhausts its resample
      budget: a generation-time discard. Forcing candidates never raises it; a
      rejected candidate is skipped. *)

  type 'a sample
  (** The type for a drawn value with its counterexample rendering: the body
      runs on {!value}, the report prints {!render}. *)

  val sample : 'a t -> Seed.state -> 'a sample Shrink_tree.t
  (** [sample gen state] draws one value and its shrink tree from [state], a
      pure function of both. Only the root is drawn eagerly; candidates are
      forced, memoized, by traversing the tree.

      Raises {!Rejected} on a generation-time discard and [Invalid_argument] on
      malformed generator arguments; forcing candidates can raise
      [Invalid_argument] but never {!Rejected}. *)

  val value : 'a sample -> 'a
  (** [value sample] is the drawn value. *)

  (** The type for a counterexample's text: [Value] is the value, through the
      printer of the generator that drew it; [Pre_image] is what a printerless
      [map] or [bind] computed it from (Printing, above). *)
  type 'a rendering = Value of 'a | Pre_image of 'a

  val render : 'a sample -> string rendering
  (** [render sample] is the counterexample text for [sample]; nothing is
      formatted before this call. A sample with nothing to print renders as
      [Value] of the placeholder [<no printer: attach one with Gen.with_pp>].
      Never raises: a printer that does renders as [<printer raised ...>]. *)

  val render_value : 'a t -> 'a -> string
  (** [render_value gen v] is [v] through [gen]'s printer, or {!render}'s
      placeholder when [gen] has none. Never raises. *)

  val run : 'a t -> Seed.state -> 'a Shrink_tree.t * Seed.state
  (** [run gen state] is the tree of values {!sample} draws from [state], with
      the successor state. Raises as {!sample} does. *)

  val make :
    ?pp:(Format.formatter -> 'a -> unit) ->
    (Seed.state -> 'a Shrink_tree.t * Seed.state) ->
    'a t
  (** [make ?pp draw] is the generator drawing with [draw], a value tree and the
      successor state as {!run} produces, and printing with [pp] or not at all.
  *)
end
