(*--------------------------------------------------------------------------
  Copyright (c) 2026 Thibaut Mattio. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(** Deterministic randomness for property tests.

    A run has one root seed, which [--seed] or its mirror [WINDTRAP_SEED] sets
    and {!random} draws otherwise. Per-case seeds derive from the root, the path
    of the test and the index of the case (guarantee 7 of
    [doc/dev/architecture.md]), which is {!derive}. A generator threads an
    immutable SplitMix64 {!state} made from that seed. The property path draws
    its randomness from nowhere else, and no value here reads or writes the
    global [Random] state.

    All the arithmetic is on [int64] and wraps modulo 2{^ 64}, so a token, a
    derived seed and a stream are the same on every machine and under every
    version of OCaml.

    {b Stability.} Four definitions are frozen: the token format ({!of_string},
    {!to_string}), {!derive}, the stream ({!make}, {!bits64}, {!below}) and
    {!split}. The [s1] prefix of a token names this version of all four, so a
    change to one of them needs another prefix. Under the same prefix the change
    is silent, and a recorded token then replays other values. *)

(** {1:seeds Seeds and tokens} *)

type seed = int64
(** The type for seeds, root or derived. Every 64-bit pattern is a seed, and the
    sign has no meaning. *)

val of_string : string -> (seed, string) result
(** [of_string text] is [Ok seed] iff [text] is [s1:] followed by 16 lowercase
    hexadecimal digits, the most significant first. Nothing is trimmed and no
    case is folded, so an uppercase digit, a surrounding space and another
    version prefix are refused. Any other spelling is [Error message], where
    [message] states the accepted form in words that are not part of the
    contract. *)

val to_string : seed -> string
(** [to_string seed] is the token of [seed], 19 bytes: [s1:] then 16 lowercase
    hexadecimal digits, the most significant first. [of_string (to_string seed)]
    is [Ok seed]. *)

val random : unit -> seed
(** [random ()] is a fresh seed drawn from the entropy of the operating system.
    It is not cryptographically strong. It is the only value of this module that
    is not a pure function. *)

val derive : root:seed -> path:string -> index:int -> seed
(** [derive ~root ~path ~index] is the seed of case [index] of the test [path]
    under [root]. It is a pure function of the three, so the other tests of a
    suite never change what a property draws, and a recorded root replays every
    generated case.
    - [path] is the path of the test as one string
      ({!Test_tree.path_to_string}), hashed byte by byte. A test that is renamed
      or moved to another group derives other seeds.
    - [index] is the caller's index of the case. {!Property.run} counts its
      generated cases from zero, the discarded ones included.

    The definition is version 1, the one that the [s1] prefix names:
    - [hash64 path] is 64-bit FNV-1a over the bytes of [path] in order. It
      starts from [0xcbf29ce484222325], and for each byte it takes the exclusive
      or with the byte, then the product by [0x100000001b3].
    - [mix64 z] is the SplitMix64 finalizer. With logical shifts, [z] becomes
      [z lxor (z lsr 30)], is multiplied by [0xbf58476d1ce4e5b9], becomes
      [z lxor (z lsr 27)], is multiplied by [0x94d049bb133111eb], and ends as
      [z lxor (z lsr 31)].
    - [derive ~root ~path ~index] is
      [mix64 (mix64 (root lxor hash64 path) + 0x9e3779b97f4a7c15 * index)].

    A change to the definition changes what every recorded token replays. *)

(** {1:sampling Sampling states} *)

type state
(** The type for immutable SplitMix64 stream states: a 64-bit position and a
    64-bit odd increment. Every operation returns a successor, and a state that
    is used twice gives the same words twice. *)

val make : seed -> state
(** [make seed] is the stream positioned at [seed] with the golden-ratio
    increment [0x9e3779b97f4a7c15]. Equal seeds give equal streams. *)

val bits64 : state -> int64 * state
(** [bits64 state] is the next word of the stream and the successor state. The
    successor's position is the position plus the increment, and the word is
    [mix64] of that new position (see {!derive}). Every bit of the word is
    significant, the sign bit included. Prefer {!below} for a bounded value,
    since a remainder of the word is biased. *)

val below : bound:int64 -> state -> int64 * state
(** [below ~bound state] is an unbiased [value] with [0L <= value < bound], and
    the successor state. [bound] must be in \[[1L];[Int64.max_int]\].

    It draws {!bits64} words and rejects those that are below 2{^ 64} mod
    [bound] as unsigned integers, then takes the unsigned remainder of the first
    word it keeps. The successor is past every word drawn, so one call can
    consume several words of the stream. A bound of [1L] consumes one word and
    gives [0L].

    Raises [Invalid_argument] if [bound <= 0L], before it draws a word. *)

val split : state -> state * state
(** [split state] is [(fresh, continued)], two successor streams that are
    functions of [state] alone, with [fresh] statistically independent of
    [continued]. A client that cannot thread one linear stream, as {!Gen.bind}
    cannot, gives [fresh] away and threads [continued] on.

    The construction is the split of Steele, Lea and Vigna's SplittableRandom.
    With [p1] and [p2] the next two positions of [state], [fresh] starts at
    [mix64 p1] with the increment [mix_gamma p2], and [continued] resumes at
    [p2] with the increment of [state]. [mix_gamma z] is the MurmurHash3
    finalizer, which is [mix64] with the three shifts at [33] and the
    multipliers [0xff51afd7ed558ccd] and [0xc4ceb9fe1a85ec53], and then
    [z lor 1]. When [z lxor (z lsr 1)] then has fewer than 24 bits set, the
    result is [z lxor 0xaaaaaaaaaaaaaaaa]. *)
