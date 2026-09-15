(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(** Random value generators with integrated shrinking and printing.

    An ['a t] couples three inseparable concerns: drawing a value from a
    {!Seed.state}, the lazy tree of shrink candidates for the drawn value, and
    how values print in counterexamples. There is no user-written shrinker
    anywhere: every generator shrinks, and shrink candidates satisfy the same
    constraints as generated values — an {!int_range} candidate stays in bounds,
    a {!such_that} candidate satisfies its predicate.

    Start from primitives ({!int}, {!float}, {!string}, ...), combine with
    containers ({!list}, {!pair}, ...) and choice ({!of_list}, {!one_of},
    {!frequency}), transform with {!map}, {!bind}, or the binding operators, and
    attach a printer with {!with_pp}.

    {b Printing.} A counterexample renders with its generator's printer.
    Primitives print; containers and choices derive their printer from their
    components'; {!constant} and {!of_list} have none, their values being
    arbitrary; {!map} and {!bind} — and so [let+], [and+] and [let*] — have none
    either. A counterexample whose generator has no printer renders as its
    {e pre-image}: the same shape, with every printerless [map] or [bind] result
    replaced by what it was computed from, down to the nearest generator that
    prints (the rule per combinator is at {!map} and {!bind}). The failure
    report marks a pre-image as such. Where nothing prints at all — a
    {!constant} or {!of_list} leaf without {!with_pp} — the counterexample
    renders as a placeholder naming the remedy.

    {b Validation.} Generator constructors never raise: malformed arguments
    ([one_of []], [int_range 3 1]) are reported by raising [Invalid_argument]
    when the generator first samples, inside the running test's exception
    boundary.

    Callbacks passed to {!map}, {!bind} and {!such_that} must be pure: the
    shrink search runs them — memoized, at most once per tree node — when it
    forces candidates. *)

(** {1:generators Generators} *)

type 'a t
(** The type for generators of values of type ['a]: generation from a
    {!Seed.state}, integrated shrinking, and counterexample printing,
    inseparable. *)

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
(** [string_of ?size char] generates a string whose length follows the size
    generator, [size] defaulting to {!nat}, and whose characters are drawn from
    [char]. The result always prints, as a quoted string, even when [char] is
    printerless.

    Shrinking follows {!list}'s rule. With the default size, candidates first
    shrink the structure — the empty string, then removal of contiguous chunks —
    and then shrink characters individually with [char]'s own candidates. With
    an explicit [size], lengths follow [size]'s shrink candidates — a length
    constraint such as [~size:(int_range 2 5)] holds for every candidate — and
    characters shrink individually.

    Sampling raises [Invalid_argument] if [size] produces a negative length. *)

val bytes : bytes t
(** [bytes] is [bytes_of char]: {!string} converted to [bytes] — same length
    distribution, uniform bytes, same shrinking. *)

val bytes_of : ?size:int t -> char t -> bytes t
(** [bytes_of char_gen] is {!string_of} converted to [bytes]: same length and
    alphabet control, same shrinking, and it keeps a printer where a
    [map Bytes.of_string] over {!string_of} would forfeit one. *)

val list : ?size:int t -> 'a t -> 'a list t
(** [list gen] generates a list of [gen] values whose length follows the size
    generator, [size] defaulting to {!nat}.

    Shrinking depends on [size]. With the default, candidates first shrink the
    structure — the empty list, then removal of contiguous chunks of descending
    power-of-two length — and then shrink elements individually, left to right.
    With an explicit [size], list lengths follow [size]'s own shrink candidates
    — so a length constraint such as [~size:(int_range 2 5)] holds for every
    candidate — and elements shrink individually.

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
    first, then the right (see {!Shrink_tree.pair}). *)

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
    Candidates shrink toward the head: the first candidate of any value is the
    head of [values], then values at intermediate positions — order [values]
    with the simplest value first. It has no printer until {!with_pp} attaches
    one.

    Sampling raises [Invalid_argument] if [values] is empty. *)

val one_of : 'a t list -> 'a t
(** [one_of gens] picks one generator from [gens] uniformly and generates with
    it. The choice shrinks toward earlier generators — a candidate may
    re-generate from an earlier branch using the same random capital, and a
    branch whose re-generation is rejected is skipped — and the chosen value
    shrinks with its own generator.

    When every generator of [gens] prints, the choice prints: a counterexample
    with the branch that drew it, an [~examples] value with the first branch's.
    Otherwise a counterexample renders by the pre-image rule with the drawing
    branch's rendering.

    Sampling raises [Invalid_argument] if [gens] is empty. *)

val frequency : (int * 'a t) list -> 'a t
(** [frequency weighted] picks a generator with probability proportional to its
    weight and generates with it. The choice itself does not shrink; the chosen
    value shrinks with its generator. Printing derives as in {!one_of}.

    Sampling raises [Invalid_argument] if [weighted] is empty, if any weight is
    negative, or if the weights sum to less than [1]. *)

val such_that : ('a -> bool) -> 'a t -> 'a t
(** [such_that p gen] generates [gen] values satisfying [p], re-sampling up to
    100 times; shrink candidates are filtered by [p], so every candidate
    satisfies it. The result keeps [gen]'s printer. If no draw satisfies [p],
    sampling raises {!Private.Rejected} and the property engine counts the case
    as a discard.

    [p] is for rare, cheap conditions; when the constraint is structural, build
    a generator that satisfies it by construction instead. *)

(** {1:composition Composition} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f gen] generates [f v] for [v] generated by [gen], shrinking wherever
    [gen] shrinks. The result has no printer: a counterexample renders as its
    pre-image, [v] as [gen] renders it, until {!with_pp} attaches one. [f] must
    be pure: the shrink search applies it, memoized, when forcing candidates. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind gen f] generates [v] with [gen], then generates with [f v]. Candidates
    first shrink [v] — re-generating with [f] on the same random capital — then
    shrink the inner value; a candidate whose re-generation is rejected by a
    {!such_that} is skipped. The result has no printer: a counterexample renders
    as the inner value when [f v] prints, and as the pre-image [v -> inner]
    otherwise, each side as its generator renders it. [f] must be pure. *)

val with_pp : (Format.formatter -> 'a -> unit) -> 'a t -> 'a t
(** [with_pp pp gen] is [gen] printing with [pp] — the same printer type the
    assertion vocabulary uses, so one printer feeds both worlds. An explicit
    printer wins over whatever [gen] would have rendered: a pre-image, a derived
    printer, or nothing. *)

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
(** [let+ x = gen in e] is [map (fun x -> e) gen]. *)

val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
(** [gen1 and+ gen2] is [pair gen1 gen2]. *)

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
(** [let* x = gen in e] is [bind gen (fun x -> e)]. *)

(** {1:engine Engine interface} *)

(** The engine interface: what the property runner and {!Stateful} reach for,
    and nothing a test writes. Not part of the vocabulary above, and carrying no
    stability promise beyond the frozen value stream ({!Seed}). *)
module Private : sig
  exception Rejected
  (** Raised by {!sample} when a {!Gen.such_that} filter exhausts its resample
      budget — a generation-time discard, which the engine counts as one.
      Forcing shrink candidates never raises it: a candidate whose re-generation
      is rejected is skipped and the search continues with its siblings. *)

  type 'a sample
  (** The type for a drawn value together with its counterexample rendering.
      Every node of a sampled tree is one: the body runs on {!value}, the report
      prints {!render}. *)

  val sample : 'a t -> Seed.state -> 'a sample Shrink_tree.t
  (** [sample gen state] draws one value and its shrink tree from [state], a
      pure function of both. Only the root is drawn eagerly; candidates are
      forced — memoized, user callbacks included — by traversing the tree.

      Raises {!Rejected} on a generation-time discard and [Invalid_argument] on
      malformed generator arguments. Forcing candidates can raise
      [Invalid_argument] too, but never {!Rejected}. *)

  val value : 'a sample -> 'a
  (** [value sample] is the drawn value. *)

  (** The type for a counterexample's text: [Value] is the value, through the
      printer of the generator that drew it; [Pre_image] is what a printerless
      [map] or [bind] computed it from (Printing, above). *)
  type 'a rendering = Value of 'a | Pre_image of 'a

  val render : 'a sample -> string rendering
  (** [render sample] is the counterexample text for [sample]. Nothing is
      formatted before this call. A sample with nothing to print renders as
      [Value] of the placeholder [<no printer: attach one with Gen.with_pp>].
      Never raises — a printer that does renders as [<printer raised ...>]. *)

  val render_value : 'a t -> 'a -> string
  (** [render_value gen v] is [v] through [gen]'s printer, and the placeholder
      of {!render} when [gen] has none — for a bare value with no tree to render
      from: an [~examples] entry, a {!Stateful} argument. Never raises. *)

  val run : 'a t -> Seed.state -> 'a Shrink_tree.t * Seed.state
  (** [run gen state] is the tree of values {!sample} draws from [state], with
      the successor state. Raises as {!sample} does. *)

  val make :
    ?pp:(Format.formatter -> 'a -> unit) ->
    (Seed.state -> 'a Shrink_tree.t * Seed.state) ->
    'a t
  (** [make ?pp draw] is the generator drawing with [draw] — a value tree and
      the successor state, as {!run} produces — and printing with [pp], or not
      at all. It is how {!Stateful} assembles a tree {!Gen}'s combinators cannot
      express. *)
end
