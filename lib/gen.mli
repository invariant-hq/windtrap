(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(** Random value generators with integrated shrinking and printing.

    An ['a t] draws a value from a {!Seed.state}, carries the lazy tree of its
    shrink candidates, and knows how values print in counterexamples. Every
    generator shrinks, and candidates satisfy the same constraints as generated
    values (guarantee 6).

    {b Printing.} Primitives print; containers and choices derive their printer
    from their components'; {!constant}, {!of_list}, {!map} and {!bind} (and so
    [let+], [and+], [let*]) have none. A counterexample whose generator has no
    printer renders as its {e pre-image}: the same shape, with every printerless
    [map] or [bind] result replaced by what it was computed from, down to the
    nearest generator that prints. Where nothing prints at all the
    counterexample renders as a placeholder naming {!with_pp}.

    {b Validation.} Constructors never raise: malformed arguments ([one_of []],
    [int_range 3 1]) raise [Invalid_argument] when the generator first samples,
    inside the running test's boundary.

    Callbacks passed to {!map}, {!bind} and {!such_that} must be pure: the
    shrink search runs them, memoized, when it forces candidates. *)

(** {1:generators Generators} *)

type 'a t
(** The type for generators of values of type ['a]. *)

(** {1:numeric Numeric generators} *)

val int : int t
(** [int] generates a uniformly distributed integer over the full [int] range.
    Candidates shrink toward [0]. *)

val nat : int t
(** [nat] generates a natural number below [10_000], biased toward small values:
    50% below [10], 25% below [100], 20% below [1_000], 5% below [10_000].
    Candidates shrink toward [0]. Use it for sizes, lengths, and counts. *)

val small_int : int t
(** [small_int] generates an integer whose magnitude follows {!nat} — inside
    \[[-9_999];[9_999]\], biased toward small magnitudes, either sign.
    Candidates shrink toward [0]. Use it instead of {!int} when full-range
    values would overflow the arithmetic under test. *)

val int_range : int -> int -> int t
(** [int_range low high] generates an integer in \[[low];[high]\], uniformly.
    Candidates shrink toward the in-range point closest to [0] and stay in
    range.

    Sampling raises [Invalid_argument] if [high < low]. *)

val int32 : int32 t
(** [int32] generates a uniformly distributed [int32] over the full 32-bit
    range. Candidates shrink toward [0l]. *)

val int64 : int64 t
(** [int64] generates a uniformly distributed [int64] over the full 64-bit
    range. Candidates shrink toward [0L]. *)

val nativeint : nativeint t
(** [nativeint] generates a uniformly distributed [nativeint] over the full
    native word range. Candidates shrink toward [0n]. *)

val float : float t
(** [float] generates a finite float by drawing uniform IEEE 754 bit patterns
    and rejecting non-finite ones, so magnitudes spread over the full exponent
    range, including subnormals. Candidates shrink toward [0.]. *)

val float_range : float -> float -> float t
(** [float_range low high] generates a float in \[[low];[high]\], uniformly.
    Candidates shrink toward the in-range point closest to [0.] and stay in
    range.

    Sampling raises [Invalid_argument] if [high < low], if either bound is not
    finite, or if [high -. low] overflows to infinity. *)

(** {1:base Unit, booleans, characters, strings} *)

val unit : unit t
(** [unit] generates [()], with no shrink candidates, and prints [()]. *)

val bool : bool t
(** [bool] generates [true] or [false] with equal probability. [true] shrinks to
    [false]. *)

val char : char t
(** [char] generates a uniformly distributed byte: each of the 256 characters —
    the NUL byte ['\x00'] and bytes above 127 included — appears with
    probability 1/256. Candidates shrink toward ['a']. Use {!char_range} or
    {!of_list} for character subsets. *)

val char_range : char -> char -> char t
(** [char_range low high] generates a character in \[[low];[high]\] (byte
    order), uniformly. Candidates shrink toward the in-range character closest
    to ['a'] and stay in range: [char_range 'a' 'z'] shrinks toward ['a'],
    [char_range 'A' 'Z'] toward ['Z'], and [char_range '0' '9'] toward ['9'].

    Sampling raises [Invalid_argument] if [high < low]. *)

val string : string t
(** [string] is [string_of char]: a string whose length follows {!nat}'s
    distribution and whose characters follow {!char} — NUL and non-ASCII bytes
    included. Shrinking removes chunks of characters — the empty string is the
    first candidate — then shrinks characters individually toward ['a']. Use
    {!string_of} to control the length or character distribution. *)

val string_of : ?size:int t -> char t -> string t
(** [string_of ?size char] generates a string whose length follows [size]
    (default {!nat}) and whose characters are drawn from [char]. It always
    prints, as a quoted string. Shrinking follows {!list}'s rule.

    Sampling raises [Invalid_argument] if [size] produces a negative length. *)

val bytes : bytes t
(** [bytes] is [bytes_of char]: {!string} converted to [bytes] — same length
    distribution, uniform bytes, same shrinking. *)

val bytes_of : ?size:int t -> char t -> bytes t
(** [bytes_of char_gen] is {!string_of} converted to [bytes]: same length and
    alphabet control, same shrinking, and it keeps its printer. *)

val list : ?size:int t -> 'a t -> 'a list t
(** [list gen] generates a list of [gen] values whose length follows [size]
    (default {!nat}). With the default size, candidates first shrink the
    structure (the empty list, then removal of contiguous chunks of descending
    power-of-two length), then elements individually left to right. With an
    explicit [size], lengths follow [size]'s own candidates, so a constraint
    such as [~size:(int_range 2 5)] holds for every candidate.

    Sampling raises [Invalid_argument] if [size] produces a negative length. *)

val array : ?size:int t -> 'a t -> 'a array t
(** [array ?size gen] is [list ?size gen] converted to an array. *)

val option : 'a t -> 'a option t
(** [option gen] generates [None] with probability 0.15 and [Some] of a [gen]
    value otherwise. The first candidate of every [Some] is [None]; the payload
    then shrinks with [gen]. *)

val result : 'a t -> 'e t -> ('a, 'e) result t
(** [result ok err] generates [Ok] of an [ok] value with probability 0.75 and
    [Error] of an [err] value otherwise. Payloads shrink with their generator; a
    candidate never crosses constructors. *)

val either : 'a t -> 'b t -> ('a, 'b) Either.t t
(** [either left right] generates [Left] of a [left] value or [Right] of a
    [right] value with equal probability. Payloads shrink with their generator;
    a candidate never crosses constructors. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair a b] generates both components. Candidates shrink the left component
    first, then the right (see {!Private.Shrink_tree.pair}). *)

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** [triple a b c] is like {!pair} for three components, shrinking
    left-to-right. *)

val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
(** [quad a b c d] is like {!pair} for four components, shrinking left-to-right.
*)

val constant : 'a -> 'a t
(** [constant v] always generates [v], with no shrink candidates. It has no
    printer until {!with_pp} attaches one. *)

val of_list : 'a list -> 'a t
(** [of_list values] generates a value of [values], each with equal probability.
    Candidates shrink toward the head of [values], so order it simplest first.
    No printer until {!with_pp} attaches one.

    Sampling raises [Invalid_argument] if [values] is empty. *)

val one_of : 'a t list -> 'a t
(** [one_of gens] picks one generator from [gens] uniformly and generates with
    it. The choice shrinks toward earlier generators, re-generating from the
    same random capital and skipping a branch whose re-generation is rejected;
    the chosen value shrinks with its own generator. When every generator
    prints, a counterexample prints with the branch that drew it and an
    [~examples] value with the first branch's; otherwise the pre-image rule
    applies.

    Sampling raises [Invalid_argument] if [gens] is empty. *)

val frequency : (int * 'a t) list -> 'a t
(** [frequency weighted] picks a generator with probability proportional to its
    weight and generates with it. The choice itself does not shrink; the chosen
    value shrinks with its generator. Printing derives as in {!one_of}.

    Sampling raises [Invalid_argument] if [weighted] is empty, if any weight is
    negative, or if the weights sum to less than [1]. *)

val such_that : ('a -> bool) -> 'a t -> 'a t
(** [such_that p gen] generates [gen] values satisfying [p], re-sampling up to
    100 times; candidates are filtered by [p]. Keeps [gen]'s printer. If no draw
    satisfies [p], sampling raises {!Private.Rejected} and the property engine
    counts a discard. For rare, cheap conditions; build structural constraints
    into the generator instead. *)

(** {1:composition Composition} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f gen] generates [f v] for [v] generated by [gen], shrinking wherever
    [gen] shrinks. No printer: a counterexample renders as its pre-image, [v] as
    [gen] renders it, until {!with_pp} attaches one. [f] must be pure. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind gen f] generates [v] with [gen], then generates with [f v]. Candidates
    first shrink [v], re-generating with [f] on the same random capital and
    skipping a rejected re-generation, then shrink the inner value. No printer:
    a counterexample renders as the inner value when [f v] prints and as the
    pre-image [v -> inner] otherwise. [f] must be pure. *)

val with_pp : (Format.formatter -> 'a -> unit) -> 'a t -> 'a t
(** [with_pp pp gen] is [gen] printing with [pp], the assertion vocabulary's
    printer type. An explicit printer wins over a pre-image, a derived printer
    or nothing. *)

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
(** [let+ x = gen in e] is [map (fun x -> e) gen]. *)

val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
(** [gen1 and+ gen2] is [pair gen1 gen2]. *)

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
(** [let* x = gen in e] is [bind gen (fun x -> e)]. *)

(** {1:engine Engine interface} *)

(** What the property engine and {!Stateful} reach for, and nothing a test
    writes. No stability promise beyond the frozen value stream ({!Seed}). *)
module Private : sig
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
