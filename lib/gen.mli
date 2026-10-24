(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(** Random generators with integrated shrinking and printing.

    A generator parses a sequence of choices ({!Choice}) into a value and how
    the value prints. Generating records the choices that encode what the
    generator drew from a {!Seed.state}; replaying them parses the same value;
    shrinking edits them ({!Shrink}). The values below are the vocabulary that
    tests compose.

    {b Shrinking.} Every sequence parses, so every candidate is a value of its
    generator and satisfies its constraints. A choice of [0] is the simplest:
    the value that a generator parses from zeros is its shrink target, which
    each generator states. A parse marks a span over the choices of each call
    of a generator. A new generator must keep both rules.

    {b Printing.} A generator prints through two channels. Its optional printer
    prints a bare value (see {!Engine.render_value}). Each sample carries its
    own rendering (see {!Engine.render}), which is parsed with the value. A
    container derives both by one rule, and prints iff every component prints.

    A rendering is a [Value], a [Pre_image] or nothing (see
    {!Engine.type-rendering}). A composite renders iff every part renders, and
    as a [Value] iff every part is one.

    {b Validation.} A constructor never raises. Every check of an argument runs
    inside the draw, so a malformed generator raises [Invalid_argument] when it
    first samples, inside the test that uses it. A new generator must keep its
    checks there, and so must the draw given to {!Engine.make}.

    {b Determinism.} A value is a function of the generator and the choices it
    parses, and generating consumes the stream of a {!Seed.state} as 0.2's
    rose-tree generators did: {!bind}, {!one_of}, {!such_that} and {!list}
    under an explicit size draw their inner part from a state split off, so
    every state draws the value 0.2 drew from it. The functions given to
    {!map}, {!bind} and {!such_that} must be pure for that to hold. The values
    that a recorded seed replays depend on the words that each generator draws
    and on their order. *)

(* The facade declares the values below again, under a narrowed signature, with
   the contract that a test's author reads ([module Gen] in windtrap.mli). A
   change to one text is a change to the other. *)

type 'a t
(** The type for generators of ['a] values: an optional printer and a draw.*)

(** {1:numeric Numbers}

    The thirteen generators print OCaml literals ([3], [3l], [3L], [3n]). A
    finite float prints as the shortest decimal that round-trips, the others as
    [infinity], [neg_infinity] and [nan].

    {!int}, {!int32}, {!int64}, {!nativeint}, {!float} and their ranges draw a
    corner case with probability 0.1, each corner equally likely, and draw
    uniformly otherwise. The corners of a range are its bounds, its origin and
    the origin's neighbours inside the range ([Float.pred] and [Float.succ] of a
    float origin), and [-0.] too for a float range whose [low] is negative and
    whose [high] is not. The corners of a whole integer type are [0], [1], [-1]
    and the type's two extremes. The corners of {!float} are [0.], [-0.], [1.],
    [-1.], and of each sign the least subnormal, the least normal and the
    greatest finite float.

    An integer with origin [o] reads its distance to [o], after a side choice
    ([0] above) when the range extends on both sides of [o], so it shrinks
    toward [o] by its distance first. A float reads a sign, unless its range
    has one, and its magnitude in the order of Hypothesis's lexicographic
    encoding: an integral magnitude below 2{^ 56} first, as itself, then the
    others by exponent, [0] up, then [-1] down, with the fraction bits
    reversed. It shrinks toward [0.], an integer before a fraction, and [-0.]
    toward [0.]. A float of {!float_range} is clamped into the range, so its
    zeros parse to the origin. *)

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

val int32_range : int32 -> int32 -> int32 t
(** [int32_range low high] is {!int_range} for [int32]. Its origin is the point
    of the range closest to [0l]. Sampling raises [Invalid_argument] if
    [high < low]. *)

val int64 : int64 t
(** [int64] is {!int} for the whole [int64] range. It shrinks toward [0L]. *)

val int64_range : int64 -> int64 -> int64 t
(** [int64_range low high] is {!int_range} for [int64]. Its origin is the point
    of the range closest to [0L]. Sampling raises [Invalid_argument] if
    [high < low]. A range of 2{^ 63} values or more draws whole words until one
    falls in it, as at least half of them do. *)

val nativeint : nativeint t
(** [nativeint] is {!int} for the whole [nativeint] range. It shrinks toward
    [0n]. *)

val nativeint_range : nativeint -> nativeint -> nativeint t
(** [nativeint_range low high] is {!int64_range} for [nativeint]. Its origin is
    the point of the range closest to [0n]. Sampling raises [Invalid_argument]
    if [high < low]. *)

val float : float t
(** [float] generates a finite float, uniformly among the finite bit patterns
    apart from its corners. Unlike {!int}, it leaves out part of its type, the
    non-finite values, which {!any_float} draws. It shrinks toward [0.]. *)

val any_float : float t
(** [any_float] generates [nan], [infinity] or [neg_infinity] with probability
    0.1, each equally likely, and a {!float} otherwise. It reads a class first,
    finite, [infinity], [neg_infinity] or [nan], then a float as {!float} does,
    so it shrinks toward [0.] and a non-finite value toward a finite one. *)

val float_range : float -> float -> float t
(** [float_range low high] generates a float in \[[low];[high]\], uniformly
    apart from its corners. Its origin is the point of the range closest to
    [0.], and [-0.] when [high] is [-0.]. Sampling raises [Invalid_argument] if
    a bound is not finite, if [high < low], or if [high -. low] overflows,
    checked in that order. *)

(** {1:base Unit, booleans, characters and strings} *)

val unit : unit t
(** [unit] generates [()], which reads no choice and prints as [()]. Drawing it
    consumes no randomness. *)

val bool : bool t
(** [bool] generates [true] or [false] with equal probability. It shrinks
    toward [false]. *)

val char : char t
(** [char] generates a byte uniformly over the 256 characters. It shrinks toward
    ['a'], as an integer over the byte code. It prints with [%C]. *)

val char_range : char -> char -> char t
(** [char_range low high] generates a character in \[[low];[high]\], in byte
    order, uniformly. Its origin is the character of the range closest to ['a'].
    Sampling raises [Invalid_argument] if [high < low]. *)

val uchar : Uchar.t t
(** [uchar] generates a Unicode scalar value, never a surrogate. The length of
    its UTF-8 encoding, 1 to 4 bytes, is drawn with equal probability, then the
    value uniformly among those of that length.

    It draws a corner with probability 0.1, each corner equally likely: U+0000,
    U+007F, U+0080, U+07FF, U+0800, U+FFFF, U+10000 and U+10FFFF, the bounds of
    the lengths; U+D7FF and U+E000, the edges of the surrogates; and U+FFFD, the
    replacement character.

    It shrinks toward [Uchar.of_char 'a'], as an integer over the scalar values
    in order, so no candidate is a surrogate. It prints as [Uchar.of_int 0xE9],
    the code point in uppercase hexadecimal. *)

val string : string t
(** [string] is [string_of char]. *)

val string_of : ?size:int t -> char t -> string t
(** [string_of ?size char] generates a string whose length follows [size] and
    whose characters follow [char]. It parses as {!list} does, so its length,
    its shrinking and its raise are {!list}'s.

    It always prints, with [%S]. A string is printed from the values of its
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

    Without [size], a "one more" choice ([1]) comes before each element, the
    two one span, and a [0] ends the list, so it shrinks toward [[]] and a
    deleted span deletes an element. With an explicit [size], the length is
    [size]'s value and the elements follow, so every candidate's length is a
    value of [size].

    Raises [Invalid_argument] if [size] generates a negative length, when it
    parses. The message names [Gen.list], whichever of {!list}, {!array},
    {!string_of} and {!bytes_of} raised. *)

val array : ?size:int t -> 'a t -> 'a array t
(** [array ?size gen] is {!list} for arrays. It prints as [[|a; b; c|]]. *)

val option : 'a t -> 'a option t
(** [option gen] generates [None] with probability 0.15 and [Some v] otherwise,
    with [v] drawn from [gen]. It shrinks toward [None], and the payload as [v]
    does. [None] always renders, as [None], even when [gen] has no printer. *)

val result : 'a t -> 'e t -> ('a, 'e) result t
(** [result ok error] generates [Error] of an [error] value with probability
    0.25 and [Ok] of an [ok] value otherwise. It shrinks toward [Ok], and a
    payload with its generator. *)

val either : 'a t -> 'b t -> ('a, 'b) Either.t t
(** [either left right] is {!result} for [Either.t], each side with equal
    probability, shrinking toward [Left]. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair a b] generates both components, [a]'s first, and parses them in that
    order. *)

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** [triple a b c] is {!pair} for three components. *)

val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
(** [quad a b c d] is {!triple} for four components. *)

(** {1:choice Constants, choices and filters} *)

val constant : ?pp:(Format.formatter -> 'a -> unit) -> 'a -> 'a t
(** [constant ~pp v] generates [v] and reads no choice. Drawing it consumes no
    randomness. [pp] prints [v], as {!with_pp} would. Without it there is no
    printer and no pre-image to fall back on, so a container or a {!map} over it
    has nothing to print (see {!Engine.render}). *)

val of_list : ?pp:(Format.formatter -> 'a -> unit) -> 'a list -> 'a t
(** [of_list ~pp values] generates an element of [values], each with equal
    probability. It reads an index, which shrinks toward [0]. [pp] prints the
    element, as {!with_pp} would; without it, as for
    {!constant}, there is no printer. Sampling raises [Invalid_argument] if
    [values] is empty. *)

val subsequence : ?pp:(Format.formatter -> 'a -> unit) -> 'a list -> 'a list t
(** [subsequence ~pp values] generates some elements of [values], not
    necessarily adjacent, in their order in [values]. It draws a length as
    [int_range 0 n] draws one, [n] being the length of [values], so [0], [1] and
    [n] are corners, and every choice of positions of that length is equally
    likely.

    It reads a keep choice per value, [1] to keep, so it shrinks toward [[]].
    [pp] prints an element, and a subsequence prints as [[a; b; c]]. Without it
    there is no printer, as for {!of_list}. *)

val permutation : ?pp:(Format.formatter -> 'a -> unit) -> 'a list -> 'a list t
(** [permutation ~pp values] generates [values] shuffled: each ordering of its
    positions is equally likely. It reads its inversion table: for each position
    of [values], how many later positions come before it, so it shrinks toward
    [values] as given, and lowering one entry moves one element left past later
    ones and keeps the others in their order. [pp] prints an element, and a
    permutation prints as [[a; b; c]]. Without it there is no printer, as for
    {!of_list}. *)

val one_of : 'a t list -> 'a t
(** [one_of gens] generates with one generator of [gens], each with equal
    probability. It reads an index, which shrinks toward [0], then the branch,
    so a candidate of an earlier branch parses from what the drawn one read.

    A sampled value renders as the branch that drew it renders. The generator's
    printer is the first branch's when every branch has one, and absent
    otherwise. The branches generate one type, and their printers are expected
    to agree. Sampling raises [Invalid_argument] if [gens] is empty. *)

val frequency : (int * 'a t) list -> 'a t
(** [frequency weighted] generates with one generator of [weighted], each with a
    probability proportional to its weight. It reads the weighted point it drew,
    so it shrinks toward the first generator of positive weight and never
    parses one of weight [0]. The printer is derived as {!one_of}'s is.

    Sampling raises [Invalid_argument] if [weighted] is empty, if a weight is
    negative, or if the weights sum to less than [1], checked in that order. *)

val such_that : ('a -> bool) -> 'a t -> 'a t
(** [such_that p gen] generates a [gen] value that satisfies [p], in at most 100
    draws, the first included, each one span. Sampling discards, raising
    [Failure.Control `Discard], when no draw satisfies [p], so every candidate
    satisfies [p], and a deletion drops the draws that failed it. [such_that]
    keeps [gen]'s printer. [p] must be pure. *)

(** {1:composition Composition} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f gen] generates [f v] for [v] from [gen], and shrinks as [gen] does.
    It has no printer. A sample renders as its argument renders, with a [Value]
    turned into a [Pre_image] (see {!Engine.type-rendering}).

    [f] must be pure. It runs at every parse. What it raises escapes the parse,
    so it fails a generated case and makes a shrink candidate one that is
    skipped. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind gen f] generates [v] with [gen], then a value with [f v] from the
    choices that follow, so the two shrink together: a candidate of [v] parses
    its inner value from what the drawn one read. [f] must be pure, and runs at
    every parse.

    It has no printer. A node renders as the inner value does when that
    rendering is a [Value] or nothing. When it is a [Pre_image], the node
    renders as the pre-image [outer -> inner], or as nothing when the outer
    value has nothing to print. *)

val with_pp : (Format.formatter -> 'a -> unit) -> 'a t -> 'a t
(** [with_pp pp gen] is [gen] printing with [pp]. It sets the generator's
    printer, and it replaces the rendering of every sample by a [Value] through
    [pp]. A deriving combinator over the result keeps the printer. *)

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
      {!Seed}.

      Shrink trees remain for a reader that searches a tree: [Stateful], whose
      programs are drawn as trees through {!make} until they parse choices,
      and the arguments it draws through {!draw}, whose tree is a view of the
      reducer's edits. *)

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
  (** The type for a parsed value with its rendering. Every node of a sampled
      tree is one, candidates included. *)

  val sample : 'a t -> Seed.state -> 'a sample Shrink_tree.t
  (** [sample gen state] draws one value from [state], as the root of a tree.
      The successor state is dropped, and {!draw} returns it.

      Raises [Failure.Control `Discard] on a discard at generation time,
      [Invalid_argument] on a malformed generator argument, and whatever a
      function of [gen] raises.

      The candidates of a node are the edits of one step of the reducer
      ({!Shrink.candidates}) that parse to a smaller sequence, each sequence
      once, parsed when their cell is forced, so the tree is finite. A candidate
      whose parse discards or raises a fault is skipped, and a
      [Failure.Control] other than a discard escapes the forcing. A generator
      of {!make} has the tree that its [draw] builds. *)

  val draw : 'a t -> Seed.state -> 'a sample Shrink_tree.t * Seed.state
  (** [draw gen state] is {!val-sample} of [gen] and [state], and the successor
      state. Raises as {!val-sample} does. *)

  val value : 'a sample -> 'a
  (** [value sample] is the drawn value. *)

  val parse : 'a t -> Choice.t -> 'a sample
  (** [parse gen src] is the value that [gen] parses from [src], with its
      rendering. It raises as {!val-sample} does at sampling. A generator made
      by {!make} reads one choice, the seed it draws its tree from: it replays
      and never shrinks inside another generator. *)

  val tree :
    'a t -> (Seed.state -> 'a sample Shrink_tree.t * Seed.state) option
  (** [tree gen] is the draw of a generator made by {!make}, whose value no
      choice sequence describes, and [None] for every other generator. *)

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

  val literal : 'a sample -> bool
  (** [literal sample] is [true] iff [sample] renders as a [Value] through this
      module's own printers alone, which print OCaml, so that its text can be
      pasted into a test. A printer given to {!Gen.with_pp}, {!Gen.constant},
      {!Gen.of_list} or {!make} prints what it likes, and a value through it is
      no literal. *)

  val render_value : 'a t -> 'a -> string
  (** [render_value gen v] is [v] through [gen]'s printer, or {!render}'s
      placeholder when [gen] has none. It reads the generator's printer and
      never a node, so a value of a {!Gen.map} or a {!Gen.bind} without
      {!Gen.with_pp} is the placeholder here, where a sample of the same
      generator renders as a pre-image.

      A printer that raises turns the text into [<printer raised EXN>], as under
      {!render}, except that a [Failure.Control] is raised again: a bare value
      is formatted before a run, as an example of a property, and the control
      belongs to that run. *)

  (** {1:building Building generators} *)

  val run : 'a t -> Seed.state -> 'a Shrink_tree.t * Seed.state
  (** [run gen state] is the tree of the values that {!val-sample} draws from
      [state], without their renderings, and the successor state. Raises as
      {!val-sample} does. *)

  val make : ?pp:(Format.formatter -> 'a -> unit) -> (Choice.t -> 'a) -> 'a t
  (** [make ?pp parse] is the generator whose value [parse] parses from a
      source: it reads its choices with {!Choice.choice}, drawing each from the
      source's stream when the source generates ({!Choice.sample}), marks its
      spans and dependents, and parses other generators with {!parse}. A
      generator parses its source as {!parse} does, inside one span, and so
      shrinks as the reducer edits what [parse] read. With [pp], the value
      renders as a [Value] through [pp]. Without it, it has nothing to print, as
      under {!Gen.constant}. [make] checks nothing. [parse] must itself keep the
      rules of validation and determinism that the module's preamble states, and
      parse every sequence, reading [0] past its end. *)

  val make_tree :
    ?pp:(Format.formatter -> 'a -> unit) ->
    (Seed.state -> 'a Shrink_tree.t * Seed.state) ->
    'a t
  (** [make_tree ?pp draw] is the generator that draws with [draw] and prints
      with [pp]. [draw] returns a tree of values and the successor state, as
      {!run} does. With [pp], every node renders as a [Value] through [pp].
      Without it, every node has nothing to print. *)
end
