(*--------------------------------------------------------------------------
  Copyright (c) 2026 Thibaut Mattio. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(** Deterministic randomness for property tests.

    [Seed] is the only randomness source in the property path. A run has one
    {e root} seed — printed as an [s1:] token in the run header and settable
    with [--seed] or [WINDTRAP_SEED] — from which every property case derives
    its own seed with {!derive}. Generators consume randomness by threading an
    immutable SplitMix64 {!state}; no ambient or process-global randomness
    participates, so replaying a root token reproduces every generated value
    across machines, OCaml versions, and suite compositions.

    {b Stability.} The token format ({!of_string}, {!to_string}), the {!derive}
    definition, and the {!state} stream ({!make}, {!bits64}, {!below}, {!split})
    are frozen at windtrap 3.0. Changing any of them silently re-keys recorded
    failures and reopens the design. The exact algorithms are specified in each
    value's documentation. *)

(** {1:seeds Seeds and tokens} *)

type seed = int64
(** The type for seeds: 64-bit patterns, root or derived. Every pattern is
    valid. Signed interpretation has no semantic meaning. *)

val of_string : string -> (seed, string) result
(** [of_string text] is [Ok seed] if [text] is exactly [s1:] followed by 16
    lowercase hexadecimal digits, most-significant nibble first.

    [Error message] rejects every other spelling — decimal, signed, uppercase,
    whitespace-padded, short, long, and unknown-version forms. The message is
    meant for users and is not stable for programmatic matching. *)

val to_string : seed -> string
(** [to_string seed] is [seed]'s canonical 19-byte token: [s1:] followed by 16
    lowercase hexadecimal digits. [of_string (to_string seed)] is [Ok seed] for
    every [seed]. *)

val random : unit -> seed
(** [random ()] is a fresh seed drawn from operating-system entropy, for runs
    with no [--seed]. It is the one non-deterministic operation of this module;
    the drawn root is printed so the run stays replayable. The value is not
    cryptographically strong. *)

(** {1:derivation Per-case derivation} *)

val derive : root:seed -> path:string -> index:int -> seed
(** [derive ~root ~path ~index] is the seed for generated case [index]
    (zero-based) of the test named by [path] under [root]. It is a pure function
    of exactly these three inputs, so suite composition never perturbs another
    test's stream and any failure replays from the printed root token.

    The frozen version-1 definition, over 64-bit arithmetic:

    - [hash64 path] is 64-bit FNV-1a over [path]'s bytes in order: starting from
      [0xcbf29ce484222325], each byte is XORed in and the result multiplied by
      [0x100000001b3].
    - [mix64 z] is the SplitMix64 finalizer:
      [z ^= z >> 30; z *= 0xbf58476d1ce4e5b9; z ^= z >> 27; z *=
       0x94d049bb133111eb; z ^= z >> 31] (shifts are logical).
    - [derive ~root ~path ~index] is
      [mix64 (mix64 (root lxor hash64 path) + 0x9e3779b97f4a7c15 * index)].

    Renaming or moving a test intentionally re-keys its stream. *)

(** {1:sampling Sampling states} *)

type state
(** The type for immutable SplitMix64 stream states. A state holds a 64-bit
    position and a 64-bit odd increment. Reusing a state repeats the same suffix
    of its stream; every operation returns successors instead of mutating. *)

val make : seed -> state
(** [make seed] is the stream positioned at [seed] with the golden-ratio
    increment [0x9e3779b97f4a7c15]. Equal seeds give equal streams. *)

val bits64 : state -> int64 * state
(** [bits64 state] is the stream's next full-width word and the successor state.
    The frozen transition adds the state's increment to its position and returns
    the [mix64] (see {!derive}) of the new position. Every output bit is
    significant, including the sign bit; use {!below} for a bounded result. *)

val below : bound:int64 -> state -> int64 * state
(** [below ~bound state] is an unbiased [value] with
    [0L <= value && value < bound], and the successor state. [bound] must be in
    \[[1L];[Int64.max_int]\].

    Sampling draws {!bits64} words, rejecting unsigned words below 2{^ 64} mod
    [bound], then takes the unsigned remainder; the successor reflects every
    drawn word, so one call may consume several stream steps. Bound [1L]
    consumes one word and returns [0L].

    Raises [Invalid_argument] if [bound <= 0L], before consuming any word. *)

val split : state -> state * state
(** [split state] is [(fresh, continued)]: two deterministic successor streams
    for use where one linear stream cannot be threaded (e.g. through [Gen]'s
    [bind]). [fresh] is statistically independent of [continued]; both are pure
    functions of [state].

    The frozen construction is Steele, Lea and Vigna's [SplittableRandom] split,
    which replaces [Random.State.split] here: with [p1] and [p2] the next two
    positions of [state], [fresh] starts at [mix64 p1] with increment
    [mix_gamma p2], and [continued] resumes [state] at [p2] with its increment
    unchanged. [mix_gamma z] is the MurmurHash3 finalizer
    ([z ^= z >> 33; z *= 0xff51afd7ed558ccd; z ^= z >> 33; z *=
      0xc4ceb9fe1a85ec53; z ^= z >> 33]) forced odd with [z lor 1] and, when
    [z lxor (z >> 1)] has fewer than 24 bits set, XORed with
    [0xaaaaaaaaaaaaaaaa] to guarantee an irregular bit pattern. *)
