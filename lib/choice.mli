(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(** Choice sequences.

    A generator parses a sequence of choices: each choice is an unsigned 64-bit
    integer at most a bound that the parse states when it reads it. A
    {e source} is the cursor of one parse. It either generates, recording the
    choices that encode what the generator drew from a {!Seed.state}, or
    replays a given sequence. Parsing the choices a source recorded, by the same
    generator, builds the same value.

    A source records, beside the choices, the spans of choices that one
    generator call read and the {e dependents}: choices that count past earlier
    positions, which a deletion of those positions must lower (see
    {!counts_past}). The reducer ({!Shrink}) reads both. *)

type t
(** The type for the source of one parse. It is mutable and used once. *)

type span = { start : int; stop : int }
(** The type for the choices at positions \[[start];[stop]\[, which one
    generator call read. *)

type dependent = { at : int; over : int list }
(** The type for the choice at position [at], whose value counts past the
    positions [over], each of them before [at] and inside one value that the
    choice counts. *)

val generating : Seed.state -> t
(** [generating state] is a source that generates from [state]. *)

val replaying : int64 array -> t
(** [replaying choices] is a source that replays [choices]. A choice read past
    their end is [0], and a replayed choice above its bound reads as the bound,
    so every sequence parses. *)

(** {1:reading Reading} *)

val sample : t -> (Seed.state -> 'a * Seed.state) -> 'a option
(** [sample src draw] is [Some v] for the [v] that [draw] draws from the stream
    of a generating [src], which then goes on from [draw]'s successor, and
    [None] when [src] replays. It records nothing: a generator samples its value
    with it, then records the choices that encode the value. *)

val split : t -> (unit -> 'a) -> 'a
(** [split src f] is [f ()]. When [src] generates, [f] draws from a stream that
    {!Seed.split} splits off, and [src] then goes on from the stream that the
    split continues, as 0.2's generators drew a re-generated part. *)

val choice : t -> bound:int64 -> int64 option -> int64
(** [choice src ~bound drawn] reads one choice in \[[0];[bound]\], unsigned, and
    records it with its bound. A generating [src] reads [drawn], which must be
    [Some v], and a replaying one its next choice, clamped to [bound], or [0]
    past the end of its sequence. A [bound] of [0L] reads [0L] and records
    nothing. *)

val span : t -> (unit -> 'a) -> 'a
(** [span src f] is [f ()], and marks the choices that [f] read as one span when
    it read any. *)

val respelled : t -> unit
(** [respelled src] records that what [src] parses has another spelling,
    smaller or not: the value is a value of another sequence too. Counting the
    values of a domain skips such a sequence. *)

val counts_past : t -> at:int -> int list -> unit
(** [counts_past src ~at over] records that the choice at position [at] counts
    past the positions [over], as a reference to the k-th newest value of a
    kind counts past one position inside each of the k newer values. A deletion
    that removes some of [over] lowers that choice by as many (see {!Shrink}).
    An empty [over] records nothing. *)

val position : t -> int
(** [position src] is the number of choices read so far. *)

(** {1:read What a parse read} *)

val choices : t -> int64 array
(** [choices src] is the choices read, in order. *)

val bounds : t -> int64 array
(** [bounds src] is the bound of each choice of {!choices}. *)

val spans : t -> span list
(** [spans src] is the spans marked, in no particular order. *)

val dependents : t -> dependent list
(** [dependents src] is the dependents recorded, in no particular order. *)

val redundant : t -> bool
(** [redundant src] is whether a parse called {!respelled} on [src]. *)

val state : t -> Seed.state option
(** [state src] is the stream that a generating [src] goes on from, and [None]
    when it replays. *)

(** {1:tokens Replay tokens} *)

val token : int64 array -> digest:string -> string
(** [token choices ~digest] is the replay token of a case: [r1:], the choices
    as unsigned LEB128 in unpadded base64url, [:] and [digest], which must be
    non-empty and hold no [:]. *)

val of_token : string -> (int64 array * string, string) result
(** [of_token text] is the choices and the digest of the token [text], or an
    error that says what a token is. *)
