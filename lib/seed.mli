(*--------------------------------------------------------------------------
  Copyright (c) 2026 Thibaut Mattio. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(** Deterministic randomness for property tests.

    A run has one root seed, printed as an [s1:] token in the run header and
    settable with [--seed] or [WINDTRAP_SEED]; every property case derives its
    own seed with {!derive}, and generators thread an immutable SplitMix64
    {!state}. This module is the only randomness source in the property path,
    and no ambient or process-global randomness participates, so replaying a
    root token reproduces every generated value across machines, OCaml versions
    and suite compositions. The token format, {!derive} and the {!state} stream
    are frozen under the [s1] prefix: changing any of them re-keys recorded
    failures (guarantee 7). *)

(** {1:seeds Seeds and tokens} *)

type seed = int64
(** The type for seeds: 64-bit patterns, root or derived. Every pattern is
    valid; the sign has no meaning. *)

val of_string : string -> (seed, string) result
(** [of_string text] is [Ok seed] if [text] is exactly [s1:] followed by 16
    lowercase hexadecimal digits, most-significant nibble first, and
    [Error message] for any other spelling. The message is for users, not for
    matching. *)

val to_string : seed -> string
(** [to_string seed] is the canonical 19-byte token, [s1:] followed by 16
    lowercase hexadecimal digits. [of_string (to_string seed)] is [Ok seed]. *)

val random : unit -> seed
(** [random ()] is a fresh seed from operating-system entropy, for runs with no
    [--seed]. Not cryptographically strong. *)

(** {1:derivation Per-case derivation} *)

val derive : root:seed -> path:string -> index:int -> seed
(** [derive ~root ~path ~index] is the seed of generated case [index]
    (zero-based) of the test named by [path] under [root], a pure function of
    the three. The frozen definition, over 64-bit arithmetic:

    - [hash64 path] is 64-bit FNV-1a over [path]'s bytes in order: starting from
      [0xcbf29ce484222325], each byte is XORed in and the result multiplied by
      [0x100000001b3].
    - [mix64 z] is the SplitMix64 finalizer:
      [z ^= z >> 30; z *= 0xbf58476d1ce4e5b9; z ^= z >> 27; z *=
       0x94d049bb133111eb; z ^= z >> 31] (shifts are logical).
    - [derive ~root ~path ~index] is
      [mix64 (mix64 (root lxor hash64 path) + 0x9e3779b97f4a7c15 * index)]. *)

(** {1:sampling Sampling states} *)

type state
(** The type for immutable SplitMix64 stream states: a 64-bit position and a
    64-bit odd increment. Every operation returns successors; reusing a state
    repeats its stream. *)

val make : seed -> state
(** [make seed] is the stream positioned at [seed] with the golden-ratio
    increment [0x9e3779b97f4a7c15]. Equal seeds give equal streams. *)

val bits64 : state -> int64 * state
(** [bits64 state] is the stream's next word and the successor state: the
    increment is added to the position and the [mix64] (see {!derive}) of the
    new position is returned. Every bit is significant, the sign bit included.
*)

val below : bound:int64 -> state -> int64 * state
(** [below ~bound state] is an unbiased [value] with
    [0L <= value && value < bound], and the successor state, for [bound] in
    \[[1L];[Int64.max_int]\]. It draws {!bits64} words, rejecting unsigned words
    below 2{^ 64} mod [bound], then takes the unsigned remainder; the successor
    reflects every drawn word. Bound [1L] consumes one word and returns [0L].

    Raises [Invalid_argument] if [bound <= 0L], before consuming any word. *)

val split : state -> state * state
(** [split state] is [(fresh, continued)], two deterministic successor streams,
    [fresh] statistically independent of [continued]. The frozen construction is
    Steele, Lea and Vigna's [SplittableRandom] split: with [p1] and [p2] the
    next two positions of [state], [fresh] starts at [mix64 p1] with increment
    [mix_gamma p2] and [continued] resumes at [p2] with its increment unchanged.
    [mix_gamma z] is the MurmurHash3 finalizer
    ([z ^= z >> 33; z *= 0xff51afd7ed558ccd; z ^= z >> 33; z *=
      0xc4ceb9fe1a85ec53; z ^= z >> 33]) forced odd with [z lor 1] and, when
    [z lxor (z >> 1)] has fewer than 24 bits set, XORed with
    [0xaaaaaaaaaaaaaaaa]. *)
