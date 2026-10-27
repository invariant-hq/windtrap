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

    {b Shrinking.} Every sequence parses, which gives the facade's shrinking, a
    choice of [0] is the simplest, and a parse marks a span over the choices of
    each call of a generator. A new generator must keep these rules.

    {b Printing.} A generator prints through two channels. Its optional printer
    prints a bare value (see {!Engine.render_value}). Each sample carries its
    own rendering (see {!Engine.render}), which is parsed with the value. A
    container derives both by one rule.

    A rendering is a [Value], a [Pre_image] or nothing (see
    {!Engine.type-rendering}). A composite renders iff every part renders, and
    as a [Value] iff every part is one.

    {b Validation.} Every check of an argument runs inside the draw, which gives
    the facade's validation. A new generator must keep its checks there, and so
    must the draw given to {!Engine.make}.

    {b Determinism.} A value is a function of the generator and the choices it
    parses, and a generated value a function of the generator and the
    {!Seed.state} it starts from, given the facade's purity. {!bind}, {!one_of},
    {!such_that} and {!list} under an explicit size draw their inner part from a
    state split off, as 0.2's rose-tree generators did. The values that a
    recorded seed replays depend on the words that each generator draws and on
    their order, so a change of distribution changes them; the values that
    recorded choices replay do not.

    {b Repetition.} Each generator has a key: its type and range for an integer
    generator, which every generator of that range shares, and its own for any
    other. A generating call whose key earlier calls of the case read choices
    under draws one word, which it does not record, and with probability 1/8
    parses the choices of one of those calls again, each equally likely, so that
    it repeats that call's value: its samplers still run, and the copied choices
    replace what they drew. A call inside such a copy neither copies nor counts
    as a call of its key. A replaying source copies nothing: the copied choices
    are in its sequence. *)

(* The facade ([module Gen] in windtrap.mli) states each value's contract for
   a test's author. The docs here state only what it does not: the choices a
   value reads, how it renders and how it draws. *)

type 'a t
(** The type for generators of ['a] values: an optional printer and a draw.*)

(** {1:numeric Numbers}

    An integer reads its distance to its origin [o], the value it shrinks
    toward, after a side choice ([0] above) when the range extends on both sides
    of [o], so it shrinks toward [o] by its distance first. A float reads a
    sign, unless its range has one, and its magnitude in the order of
    Hypothesis's lexicographic encoding: an integral magnitude below 2{^ 56}
    first, as itself, then the others by exponent, [0] up, then [-1] down, with
    the fraction bits reversed. A float of {!float_range} is clamped into the
    range, so its zeros parse to the origin.

    A range's draw, apart from its corners, is uniform over the range, as in
    0.2: a bounded draw over its span, or, for a range of 2{^ 63} values or
    more, whole words until one falls in it. The parse reads it, as any value,
    by its side and distance. A whole type, whose range is its type's own
    [min_int] to [max_int], draws instead the distance to [o] on a side chosen
    in proportion to its count of values, counted up to 257 for the side above,
    which holds [o], and up to 256 for the side below, which starts one past it.
    Each side reads one word, whose bits choose a bit length [k], over 0..8 or
    over 0..the side's bit length with equal probability, and, one time in four,
    an edge of [k] bits: 2{^ k-1} (1/4), 2{^ k} - 1 (1/2) or 2{^ k} (1/4),
    clipped to the side. Otherwise it draws the distance uniformly among the
    values of [k] bits. *)

val int : int t
(** See the facade's [Gen.int]. *)

val nat : int t
(** See the facade's [Gen.nat]. *)

val small_int : int t
(** See the facade's [Gen.small_int]. *)

val int_range : int -> int -> int t
(** See the facade's [Gen.int_range]. *)

val int32 : int32 t
(** See the facade's [Gen.int32]. *)

val int32_range : int32 -> int32 -> int32 t
(** See the facade's [Gen.int32_range]. *)

val int64 : int64 t
(** See the facade's [Gen.int64]. *)

val int64_range : int64 -> int64 -> int64 t
(** See the facade's [Gen.int64_range]. *)

val nativeint : nativeint t
(** See the facade's [Gen.nativeint]. *)

val nativeint_range : nativeint -> nativeint -> nativeint t
(** See the facade's [Gen.nativeint_range]. *)

val float : float t
(** See the facade's [Gen.float]. *)

val any_float : float t
(** [any_float] reads a class first, finite, [infinity], [neg_infinity] or
    [nan], then a float as {!float} does. *)

val float_range : float -> float -> float t
(** See the facade's [Gen.float_range]. *)

(** {1:base Unit, booleans, characters and strings} *)

val unit : unit t
(** [unit] reads no choice, and drawing it consumes no randomness. *)

val bool : bool t
(** See the facade's [Gen.bool]. *)

val char : char t
(** [char] shrinks as an integer over the byte code. *)

val char_range : char -> char -> char t
(** See the facade's [Gen.char_range]. *)

val uchar : Uchar.t t
(** [uchar] draws the length of its UTF-8 encoding, then the value. It shrinks
    as an integer over the scalar values in order, so no candidate is a
    surrogate. *)

val string : string t
(** See the facade's [Gen.string]. *)

val string_of : ?size:int t -> char t -> string t
(** [string_of ?size char] parses as {!list} does, and never reads [char]'s
    renderings. *)

val bytes : bytes t
(** See the facade's [Gen.bytes]. *)

val bytes_of : ?size:int t -> char t -> bytes t
(** See the facade's [Gen.bytes_of]. *)

(** {1:containers Containers} *)

val list : ?size:int t -> 'a t -> 'a list t
(** [list ?size gen] draws its elements in order.

    Without [size], a "one more" choice ([1]) comes before each element, the two
    one span, and a [0] ends the list, so a deleted span deletes an element.
    With an explicit [size], the length is [size]'s value and the elements
    follow. *)

val array : ?size:int t -> 'a t -> 'a array t
(** See the facade's [Gen.array]. *)

val option : 'a t -> 'a option t
(** See the facade's [Gen.option]. *)

val result : 'a t -> 'e t -> ('a, 'e) result t
(** See the facade's [Gen.result]. *)

val either : 'a t -> 'b t -> ('a, 'b) Either.t t
(** See the facade's [Gen.either]. *)

val pair : 'a t -> 'b t -> ('a * 'b) t
(** [pair a b] parses its components in the order it generates them. *)

val triple : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
(** See the facade's [Gen.triple]. *)

val quad : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t
(** See the facade's [Gen.quad]. *)

(** {1:choice Constants, choices and filters} *)

val constant : ?pp:(Format.formatter -> 'a -> unit) -> 'a -> 'a t
(** [constant ~pp v] reads no choice, and drawing it consumes no randomness.
    Without [pp] it has no pre-image to fall back on either (see
    {!Engine.render}). *)

val of_list : ?pp:(Format.formatter -> 'a -> unit) -> 'a list -> 'a t
(** [of_list ~pp values] reads an index. *)

val subsequence : ?pp:(Format.formatter -> 'a -> unit) -> 'a list -> 'a list t
(** [subsequence ~pp values] reads a keep choice per value, [1] to keep. *)

val permutation : ?pp:(Format.formatter -> 'a -> unit) -> 'a list -> 'a list t
(** [permutation ~pp values] reads its inversion table: for each position of
    [values], how many later positions come before it. Lowering one entry moves
    one element left past later ones and keeps the others in their order. *)

val one_of : 'a t list -> 'a t
(** [one_of gens] reads an index, then the branch, so a candidate of an earlier
    branch parses from what the drawn one read. The generator's printer is the
    first branch's when every branch has one, and absent otherwise. The branches
    generate one type, and their printers are expected to agree. *)

val frequency : (int * 'a t) list -> 'a t
(** [frequency weighted] reads the weighted point it drew, so it never parses a
    generator of weight [0]. Its printer is derived as {!one_of}'s is. *)

val such_that : ('a -> bool) -> 'a t -> 'a t
(** [such_that p gen] makes each draw one span, and discards by raising
    [Failure.Control `Discard]. *)

(** {1:composition Composition} *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f gen] renders a sample as its argument renders, with a [Value] turned
    into a [Pre_image] (see {!Engine.type-rendering}). [f] runs at every parse,
    and what it raises escapes the parse. *)

val bind : 'a t -> ('a -> 'b t) -> 'b t
(** [bind gen f] runs [f] at every parse. *)

val with_pp : (Format.formatter -> 'a -> unit) -> 'a t -> 'a t
(** [with_pp pp gen] sets the generator's printer, and replaces the rendering of
    every sample by a [Value] through [pp]. A deriving combinator over the
    result keeps the printer. *)

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
(** See the facade's [Gen.( let+ )]. *)

val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
(** See the facade's [Gen.( and+ )]. *)

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
(** See the facade's [Gen.( let* )]. *)

(** {1:engine Engine} *)

module Engine : sig
  (** The engine's side of a generator. Nothing here belongs to the vocabulary
      that tests compose, and nothing here is stable beyond the frozen stream of
      {!Seed}. *)

  (** {1:parsing Parsing} *)

  type 'a sample
  (** The type for a parsed value with its rendering. *)

  val value : 'a sample -> 'a
  (** [value sample] is the drawn value. *)

  val parse : 'a t -> Choice.t -> 'a sample
  (** [parse gen src] is the value that [gen] parses from [src], with its
      rendering, the choices it read marked as one span of [src]. A generating
      [src] draws the value from its stream ({!Choice.generating}), and a
      replaying one parses its choices again.

      Raises [Failure.Control `Discard] on a discard, [Invalid_argument] on a
      malformed generator argument, and whatever a function of [gen] raises. *)

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
end
