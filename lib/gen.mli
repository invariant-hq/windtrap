(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(** Random generators with integrated shrinking and printing.

    A generator couples three things that move together: a draw from a
    {!Seed.state}, the lazy tree of the drawn value's shrink candidates, and how
    each value of that tree prints. The values below are the vocabulary that
    tests compose.

    {b Shrinking.} A value is drawn with its candidates, and every candidate
    satisfies the constraints of its generator. A new generator must keep that
    invariant.

    {b Printing.} A generator prints through two channels. Its optional printer
    prints a bare value (see {!Engine.render_value}). Each node of a sampled
    tree carries its own rendering (see {!Engine.render}), which is drawn and
    shrunk with the value. A container derives both by one rule, and prints iff
    every component prints.

    A rendering is a [Value], a [Pre_image] or nothing (see
    {!Engine.type-rendering}). A composite renders iff every part renders, and
    as a [Value] iff every part is one.

    {b Validation.} A constructor never raises. Every check of an argument runs
    inside the draw, so a malformed generator raises [Invalid_argument] when it
    first samples, inside the test that uses it. A new generator must keep its
    checks there, and so must the draw given to {!Engine.make}.

    {b Determinism.} A {!Seed.state} is immutable and a draw returns its
    successor. The combinators that generate again while shrinking ({!bind},
    {!one_of}, {!list} under an explicit size) split a state off at sampling and
    run every re-generation on it. That makes a sampled tree a pure function of
    the generator and the state, whatever the order its cells are forced in. The
    functions given to {!map}, {!bind} and {!such_that} must be pure for that to
    hold. The values that a recorded seed replays depend on the words that each
    generator draws and on their order. *)

(* The facade declares the values below again, under a narrowed signature, with
   the contract that a test's author reads ([module Gen] in windtrap.mli). A
   change to one text is a change to the other. *)

type 'a t
(** The type for generators of ['a] values: an optional printer and a draw.*)

(** {1:numeric Numbers}

    The nine generators print OCaml literals ([3], [3l], [3L], [3n]). A float
    prints as the shortest decimal that round-trips.

    {!int}, {!int_range}, {!int32}, {!int64} and {!nativeint} draw a corner case
    with probability 0.1, each corner equally likely, and draw uniformly
    otherwise. The corners of a range are its bounds, its origin and the
    origin's neighbours inside the range. The corners of a whole type are [0],
    [1], [-1] and the type's two extremes.

    The candidates of an integer [x] with origin [o] are [o] first, then values
    that each close half of the remaining gap to [x], which is not a candidate
    itself. Every candidate lies between [o] and [x]. A candidate's own
    candidates are built the same way toward [o], so every candidate is strictly
    nearer [o] than its parent and the tree is finite in depth. Floats follow
    the same scheme, cut at [15] candidates per node. *)

val int : int t
(** [int] generates an [int] over the whole range. It shrinks toward [0]. *)

val nat : int t
(** [nat] generates a natural number below [10_000], small values more often:
    50% below [10], 25% below [100], 20% below [1_000], 5% below [10_000]. It
    shrinks toward [0]. *)

val small_int : int t
(** [small_int] generates an integer in \[[-9_999];[9_999]\]. Its magnitude is
    distributed as the values of {!nat} are, and it is negated with probability
    0.5. It shrinks toward [0]. *)

val int_range : int -> int -> int t
(** [int_range low high] generates an integer in \[[low];[high]\]. Its origin is
    the point of the range closest to [0]. Sampling raises [Invalid_argument] if
    [high < low]. *)

val int32 : int32 t
(** [int32] is {!int} for the whole [int32] range. It shrinks toward [0l]. *)

val int64 : int64 t
(** [int64] is {!int} for the whole [int64] range. It shrinks toward [0L]. *)

val nativeint : nativeint t
(** [nativeint] is {!int} for the whole [nativeint] range. It shrinks toward
    [0n]. *)

val float : float t
(** [float] generates a finite float, uniformly among the finite bit patterns.
    It shrinks toward [0.]. *)

val float_range : float -> float -> float t
(** [float_range low high] generates a float in \[[low];[high]\], uniformly. Its
    origin is the point of the range closest to [0.]. Sampling raises
    [Invalid_argument] if a bound is not finite, if [high < low], or if
    [high -. low] overflows, checked in that order. *)

(** {1:base Unit, booleans, characters and strings} *)

val unit : unit t
(** [unit] generates [()], which has no candidates and prints as [()]. Drawing
    it consumes no randomness. *)

val bool : bool t
(** [bool] generates [true] or [false] with equal probability. [true] has one
    candidate, [false]. *)

val char : char t
(** [char] generates a byte uniformly over the 256 characters. It shrinks toward
    ['a'], by the integer scheme over the byte code. It prints with [%C]. *)

val char_range : char -> char -> char t
(** [char_range low high] generates a character in \[[low];[high]\], in byte
    order, uniformly. Its origin is the character of the range closest to ['a'].
    Sampling raises [Invalid_argument] if [high < low]. *)

val string : string t
(** [string] is [string_of char]. *)

val string_of : ?size:int t -> char t -> string t
(** [string_of ?size char] generates a string whose length follows [size] and
    whose characters follow [char]. It is built on the tree of elements that
    {!list} is built on, so its length, its shrink order and its raise are
    {!list}'s.

    It always prints, with [%S]. Every node is printed from the values of its
    characters, so [char]'s printer and renderings are never read. *)

val bytes : bytes t
(** [bytes] is [bytes_of char]. *)

val bytes_of : ?size:int t -> char t -> bytes t
(** [bytes_of ?size char] is {!string_of} for [bytes]. It prints as
    [Bytes.of_string "…"]. *)

(** {1:containers Containers} *)

val list : ?size:int t -> 'a t -> 'a list t
(** [list ?size gen] generates a list of [gen] values whose length follows
    [size]. It prints as [[a; b; c]].

    With the default [size], the length is below [4] in 50% of the draws, below
    [8] in 25%, below [16] in 20% and below [64] in 5%, uniformly within each
    stratum. The mean is about 5, so a [list (list int)] holds about 22 integers
    and a case draws in microseconds. The elements are drawn in order.

    A candidate is a shorter list, or a list of the same length with one element
    replaced by one of its candidates. The order of the candidates is not part
    of the contract.

    With an explicit [size], the length of a candidate is the drawn length or
    one of its candidates in [size]'s tree. A state is split off at sampling and
    the elements of every candidate length are drawn again from it. A candidate
    whose re-generation discards is skipped.

    Raises [Invalid_argument] if [size] generates a negative length: at sampling
    for the drawn length, and at the forcing of a candidate for a candidate
    length. The message names [Gen.list], whichever of {!list}, {!array},
    {!string_of} and {!bytes_of} raised. *)

val array : ?size:int t -> 'a t -> 'a array t
(** [array ?size gen] is {!list} for arrays. It prints as [[|a; b; c|]]. *)

val option : 'a t -> 'a option t
(** [option gen] generates [None] with probability 0.15 and [Some v] otherwise,
    with [v] drawn from [gen]. [None] is the first candidate of every [Some]
    node of the tree, and the payload then shrinks as [v] does. A [None] node
    always renders, as [None], even when [gen] has no printer. *)

val result : 'a t -> 'e t -> ('a, 'e) result t
(** [result ok error] generates [Error] of an [error] value with probability
    0.25 and [Ok] of an [ok] value otherwise. A payload shrinks with its
    generator, and no candidate changes constructor. *)

val either : 'a t -> 'b t -> ('a, 'b) Either.t t
(** [either left right] is {!result} for [Either.t], each side with equal
    probability. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair a b] generates both components, [a]'s first. Its tree is
    {!Engine.Shrink_tree.pair}, so the first component shrinks, then the second.
*)

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** [triple a b c] is {!pair} for three components. Its tree is a right-nested
    {!Engine.Shrink_tree.pair}, so it shrinks from the left. *)

val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
(** [quad a b c d] is {!triple} for four components. *)

(** {1:choice Constants, choices and filters} *)

val constant : ?pp:(Format.formatter -> 'a -> unit) -> 'a -> 'a t
(** [constant ~pp v] generates [v], which has no candidates. Drawing it consumes
    no randomness. [pp] prints [v], as {!with_pp} would. Without it there is no
    printer and no pre-image to fall back on, so a container or a {!map} over it
    has nothing to print (see {!Engine.render}). *)

val of_list : ?pp:(Format.formatter -> 'a -> unit) -> 'a list -> 'a t
(** [of_list ~pp values] generates an element of [values], each with equal
    probability. It draws an index, which shrinks toward [0] by the integer
    scheme. [pp] prints the element, as {!with_pp} would; without it, as for
    {!constant}, there is no printer. Sampling raises [Invalid_argument] if
    [values] is empty. *)

val one_of : 'a t list -> 'a t
(** [one_of gens] generates with one generator of [gens], each with equal
    probability. It draws an index, which shrinks toward [0]. A state is split
    off at sampling, and the drawn branch and every earlier branch that the
    integer scheme proposes generate from it. The earlier branches come first,
    then the candidates of the drawn value. A branch whose re-generation
    discards is skipped.

    A sampled value renders as the branch that drew it renders. The generator's
    printer is the first branch's when every branch has one, and absent
    otherwise. The branches generate one type, and their printers are expected
    to agree. Sampling raises [Invalid_argument] if [gens] is empty. *)

val frequency : (int * 'a t) list -> 'a t
(** [frequency weighted] generates with one generator of [weighted], each with a
    probability proportional to its weight. The chosen generator runs on the
    state as it stands. The candidates of the drawn value include those of the
    generator that drew it, and every candidate is a value of one of the
    generators. The printer is derived as {!one_of}'s is.

    Sampling raises [Invalid_argument] if [weighted] is empty, if a weight is
    negative, or if the weights sum to less than [1], checked in that order. *)

val such_that : ('a -> bool) -> 'a t -> 'a t
(** [such_that p gen] generates a [gen] value that satisfies [p], in at most 100
    draws, the first included. Sampling discards, raising
    [Failure.Control `Discard], when no draw satisfies [p]. A candidate that
    fails [p] is dropped with its whole subtree. [such_that] keeps [gen]'s
    printer. [p] must be pure. *)

(** {1:composition Composition} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f gen] generates [f v] for [v] from [gen], and shrinks as [gen] does.
    It has no printer. A node renders as its argument renders, with a [Value]
    turned into a [Pre_image] (see {!Engine.type-rendering}).

    [f] must be pure. It runs on the root at sampling, and on a candidate when
    its cell is forced, at most once per node. What [f] raises at sampling
    escapes {!Engine.val-sample}. What it raises on a candidate escapes the
    forcing of that cell, which caches it (see {!Engine.Shrink_tree}), except a
    [Failure.Control `Discard], which skips the candidate. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind gen f] generates [v] with [gen], then a value with [f v]. A state is
    split off at sampling, and [f v] runs on it for the drawn [v] and again for
    every candidate of [v]. The candidates of [v] come first, then those of the
    inner value. A candidate whose re-generation discards is skipped, and any
    other exception escapes the forcing. [f] must be pure.

    It has no printer. A node renders as the inner value does when that
    rendering is a [Value] or nothing. When it is a [Pre_image], the node
    renders as the pre-image [outer -> inner], or as nothing when the outer
    value has nothing to print. *)

val with_pp : (Format.formatter -> 'a -> unit) -> 'a t -> 'a t
(** [with_pp pp gen] is [gen] printing with [pp]. It sets the generator's
    printer, and it replaces the rendering of every node of a sampled tree,
    candidates included, by a [Value] through [pp]. A deriving combinator over
    the result keeps the printer. *)

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
(** [let+ x = gen in e] is [map (fun x -> e) gen]. *)

val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
(** [gen1 and+ gen2] is [pair gen1 gen2]. *)

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
(** [let* x = gen in e] is [bind gen (fun x -> e)]. *)

(** {1:engine Engine} *)

module Engine : sig
  (** The engine's side of a generator. Nothing here belongs to the vocabulary
      that tests compose, and nothing here is stable beyond the frozen stream of
      {!Seed}. *)

  (** {1:trees Shrink trees} *)

  module Shrink_tree : sig
    (** Memoized lazy rose trees of shrink candidates.

        A tree is a strict root and a lazy, ordered sequence of candidate
        subtrees. Every cell of a sequence is forced at most once, a raised
        exception included, so a search that visits a branch again never runs
        caller code again. A cell whose forcing raised caches the exception and
        never yields its tail, so every sibling behind it is unreachable.

        The sequences given to {!make} and the functions given to {!map} are
        caller code, and they run at the forcing points. They must be
        deterministic and free of effects, and they must not read a random state
        whose value depends on the order of forcing. *)

    type 'a t
    (** The type for a value and its ordered shrink candidates. A tree may be
        infinite, since nothing here bounds its depth or its breadth. *)

    val make : root:'a -> children:'a t Seq.t -> 'a t
    (** [make ~root ~children] is the tree rooted at [root] whose immediate
        candidates are [children]. It forces nothing, and it is what memoizes.
        Every cell of [children] is forced at most once through the resulting
        tree, so a combinator that builds its children with [Seq.map] or
        [Seq.filter_map] must wrap them in [make]. *)

    val leaf : 'a -> 'a t
    (** [leaf root] is [make ~root ~children:Seq.empty]. *)

    val root : 'a t -> 'a
    (** [root tree] is [tree]'s root value. It forces no cell. *)

    val children : 'a t -> 'a t Seq.t
    (** [children tree] is [tree]'s immediate candidates, in the order given to
        {!make}. The sequence is persistent. A forced cell yields the same
        child, or raises its cached exception, without evaluating anything
        again. Forcing a cell does not force its tail. *)

    val map : ('a -> 'b) -> 'a t -> 'b t
    (** [map f tree] is [tree] with [f] applied to every value, shape and order
        kept, except that a descendant on which [f] raises a
        [Failure.Control `Discard] is skipped with its subtree. [f] runs on the
        root at once, and on a descendant when its cell is forced, at most once.
        What [f] raises on the root escapes [map]. What it raises on a
        descendant, a discard excepted, escapes the forcing of that cell, which
        caches it. *)

    val pair : 'a t -> 'b t -> ('a * 'b) t
    (** [pair left right] is rooted at [(root left, root right)]. Its candidates
        reduce [left] in order with [right] kept, then [right] in order with
        [left] kept. Every candidate follows the same rule, so a candidate that
        reduced [left] offers the reductions of [right] again. [right]'s
        sequence stays unforced until [left]'s is exhausted. *)

    val list : 'a t list -> 'a list t
    (** [list trees] is rooted at the roots of [trees], in order. The immediate
        candidates of a non-empty list are, in order:
        + The empty list.
        + The list less one contiguous chunk. The chunk lengths are the powers
          of two from the largest strictly below the length down to [1]. For
          each length the chunks start at [0] and advance by that length, and a
          chunk is removed only where it fits whole.
        + The list with one element replaced by one of its candidates, elements
          from the left, each in its own candidate order.

        Every candidate follows the same rules. It shares the untouched element
        trees, so an element's cells are still forced at most once across
        candidates. The empty list has no candidates. A candidate is a strictly
        shorter list, or the same list with one element reduced, so the tree is
        finite in depth when the element trees are. Building the root is
        stack-safe and forces no cell. *)
  end

  (** {1:sampling Sampling} *)

  type 'a sample
  (** The type for a drawn value with its rendering. Every node of a sampled
      tree is one, candidates included. *)

  val sample : 'a t -> Seed.state -> 'a sample Shrink_tree.t
  (** [sample gen state] draws one value and its shrink tree from [state]. Only
      the root is drawn, and the candidates are generated and memoized when the
      tree is traversed. The successor state is dropped, and {!draw} returns it.

      Raises [Failure.Control `Discard] on a discard at generation time,
      [Invalid_argument] on a malformed generator argument, and whatever a
      function of [gen] raises.

      Forcing a candidate never raises [Failure.Control `Discard]: a candidate
      whose generation discards is skipped, and the search goes on with its
      siblings. It raises whatever else the generation of that candidate raises:
      - What a function given to {!Gen.map}, {!Gen.bind} or {!Gen.such_that}
        raises.
      - The [Invalid_argument] of a re-generation: a negative candidate length
        under a sized {!Gen.list}, or a malformed generator that {!Gen.bind}'s
        function builds for a candidate.
      - Whatever the [draw] of a {!make} raises.
      - A [Failure.Control (`Timeout _)] delivered in the meantime. *)

  val draw : 'a t -> Seed.state -> 'a sample Shrink_tree.t * Seed.state
  (** [draw gen state] is {!val-sample} of [gen] and [state], and the successor
      state. Raises as {!val-sample} does. *)

  val value : 'a sample -> 'a
  (** [value sample] is the drawn value. *)

  (** {1:renderings Rendering} *)

  (** The type for the text of a sample: the value, or what the value was
      computed from. A sample with nothing to print has no case of its own. It
      renders as a [Value], the placeholder of {!render}, whose text carries its
      own remedy. *)
  type 'a rendering =
    | Value of 'a
        (** The value, through the printer of the generator that drew it. *)
    | Pre_image of 'a
        (** What a {!Gen.map} or a {!Gen.bind} without printer computed the
            value from, each part printed by the nearest generator that prints.
        *)

  val render : 'a sample -> string rendering
  (** [render sample] is the text of [sample]. Nothing is formatted before this
      call, and every call formats again, through [Format.asprintf] at its
      default margin. A sample with nothing to print renders as [Value] of the
      placeholder [<no printer: attach one with Gen.with_pp>].

      A printer that raises turns the whole text into [<printer raised EXN>],
      [EXN] being the exception as [Failure.exn_to_string] prints it, a
      [Failure.Control] included. The guard is around the whole document, and
      only what [Failure.catch] never returns leaves it. *)

  val prints : 'a sample -> bool
  (** [prints sample] is [false] iff [sample] has nothing to print, so that
      {!render} gives its placeholder. It formats nothing. *)

  val render_value : 'a t -> 'a -> string
  (** [render_value gen v] is [v] through [gen]'s printer, or {!render}'s
      placeholder when [gen] has none. It reads the generator's printer and
      never a node, so a value of a {!Gen.map} or a {!Gen.bind} without
      {!Gen.with_pp} is the placeholder here, where a sample of the same
      generator renders as a pre-image. It has {!render}'s guard. *)

  (** {1:building Building generators} *)

  val run : 'a t -> Seed.state -> 'a Shrink_tree.t * Seed.state
  (** [run gen state] is the tree of the values that {!val-sample} draws from
      [state], without their renderings, and the successor state. Raises as
      {!val-sample} does. *)

  val make :
    ?pp:(Format.formatter -> 'a -> unit) ->
    (Seed.state -> 'a Shrink_tree.t * Seed.state) ->
    'a t
  (** [make ?pp draw] is the generator that draws with [draw] and prints with
      [pp]. [draw] returns a tree of values and the successor state, as {!run}
      does. With [pp], every node renders as a [Value] through [pp], whatever
      the generators that [draw] ran rendered as. Without it, every node has
      nothing to print, as under {!Gen.constant}. [make] checks nothing. [draw]
      must itself keep the rules of validation and determinism that the module's
      preamble states. *)
end
