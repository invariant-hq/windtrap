(*--------------------------------------------------------------------------
  Copyright (c) 2026 Thibaut Mattio. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(** Deterministic randomness for property tests.

    A run has one root seed, which [--seed] or its mirror [WINDTRAP_SEED] sets
    and {!random} draws otherwise. Per-case seeds derive from the root, the path
    of the test and the index of the case, which is {!derive}. A generator
    threads an immutable SplitMix64 {!state} made from that seed. The property
    path draws its randomness from nowhere else, and no value here reads or
    writes the global [Random] state.

    All the arithmetic is on [int64] and wraps modulo 2{^ 64}, so a token, a
    derived seed and a stream are the same on every machine and under every
    version of OCaml.

    {b Stability.} Four definitions are frozen: the token format ({!of_string},
    {!to_string}), {!derive}, the stream ({!make}, {!bits64}, {!below}) and
    {!split}. The [s1] prefix of a token names this version of all four, so a
    change to one of them needs another prefix. Under the same prefix the change
    is silent, and a recorded token then replays other values. What a generator
    makes of the stream is no part of the four, so a recorded token replays the
    same values within one version of the library only. *)

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
    generated case of the same version of the library.
    - [path] is the path of the test as one string
      ({!Test_tree.path_to_string}), hashed byte by byte. A test that is renamed
      or moved to another group derives other seeds.
    - [index] is the caller's index of the case. *)

(** {1:sampling Sampling states} *)

type state
(** The type for immutable SplitMix64 stream states: a 64-bit position and a
    64-bit odd increment. Every operation returns a successor, and a state that
    is used twice gives the same words twice. *)

val make : seed -> state
(** [make seed] is the stream positioned at [seed] with the golden-ratio
    increment [0x9e3779b97f4a7c15]. Equal seeds give equal streams. *)

val bits64 : state -> int64 * state
(** [bits64 state] is the next word of the stream and the successor state. Every
    bit of the word is significant, the sign bit included. Prefer {!below} for a
    bounded value, since a remainder of the word is biased. *)

val below : bound:int64 -> state -> int64 * state
(** [below ~bound state] is an unbiased [value] with [0L <= value < bound], and
    the successor state. [bound] must be in \[[1L];[Int64.max_int]\].

    The successor is past every word drawn, so one call can consume several
    words of the stream. A bound of [1L] consumes one word and gives [0L].

    Raises [Invalid_argument] if [bound <= 0L], before it draws a word. *)

val split : state -> state * state
(** [split state] is [(fresh, continued)], two successor streams that are
    functions of [state] alone, with [fresh] statistically independent of
    [continued]. A client that cannot thread one linear stream, as {!Gen.bind}
    cannot, gives [fresh] away and threads [continued] on.

    The construction is the split of Steele, Lea and Vigna's SplittableRandom.
*)
