(*--------------------------------------------------------------------------
  Copyright (c) 2026 Thibaut Mattio. All rights reserved.
  SPDX-License-Identifier: ISC

  SplitMix64 constants, the s1: token codec, and the rejection-sampled
  bounded draw. The split construction follows Steele, Lea and Vigna, "Fast
  Splittable Pseudorandom Number Generators" (OOPSLA 2014).
  --------------------------------------------------------------------------*)

(* Every definition here but [random] is frozen under the token prefix [s1],
   as seed.mli states. *)

(* Seeds and tokens *)

type seed = int64

let to_string seed = Printf.sprintf "s1:%016Lx" seed

(* A text is a token iff it is the printing of the seed it reads, so
   [to_string] alone defines the format. *)
let of_string text =
  match Scanf.sscanf_opt text "s1:%Lx%!" Fun.id with
  | Some seed when String.equal (to_string seed) text -> Ok seed
  | Some _ | None ->
      Error "seed must be s1: followed by 16 lowercase hexadecimal digits"

let random () = Random.State.bits64 (Random.State.make_self_init ())

let mix64 z =
  let z =
    Int64.(mul (logxor z (shift_right_logical z 30)) 0xbf58476d1ce4e5b9L)
  in
  let z =
    Int64.(mul (logxor z (shift_right_logical z 27)) 0x94d049bb133111ebL)
  in
  Int64.(logxor z (shift_right_logical z 31))

let golden_gamma = 0x9e3779b97f4a7c15L
let fnv_offset_basis = 0xcbf29ce484222325L
let fnv_prime = 0x100000001b3L

let hash64 text =
  let add hash byte =
    Int64.mul (Int64.logxor hash (Int64.of_int (Char.code byte))) fnv_prime
  in
  String.fold_left add fnv_offset_basis text

let derive ~root ~path ~index =
  let stream = mix64 (Int64.logxor root (hash64 path)) in
  mix64 (Int64.add stream (Int64.mul golden_gamma (Int64.of_int index)))

(* Sampling states *)

type state = { position : int64; gamma : int64 (* odd *) }

let make seed = { position = seed; gamma = golden_gamma }

let bits64 { position; gamma } =
  let position = Int64.add position gamma in
  (mix64 position, { position; gamma })

(* The rejection loop has no step bound. It ends with probability 1: the
   threshold is below [bound], so fewer than half of the words are rejected
   whatever [bound] is. *)
let below ~bound state =
  if Int64.compare bound 0L <= 0 then
    invalid_arg "Seed.below: non-positive bound";
  let threshold = Int64.unsigned_rem (Int64.neg bound) bound in
  let rec sample state =
    let word, state = bits64 state in
    if Int64.unsigned_compare word threshold < 0 then sample state
    else (Int64.unsigned_rem word bound, state)
  in
  sample state

let popcount z =
  let z =
    Int64.(sub z (logand (shift_right_logical z 1) 0x5555555555555555L))
  in
  let z =
    Int64.(
      add
        (logand z 0x3333333333333333L)
        (logand (shift_right_logical z 2) 0x3333333333333333L))
  in
  let z =
    Int64.(logand (add z (shift_right_logical z 4)) 0x0f0f0f0f0f0f0f0fL)
  in
  Int64.(to_int (shift_right_logical (mul z 0x0101010101010101L) 56))

(* The increment of a split stream is SplittableRandom's: the MurmurHash3
   finalizer forced odd, with too regular a bit pattern broken up. *)
let mix_gamma z =
  let z =
    Int64.(mul (logxor z (shift_right_logical z 33)) 0xff51afd7ed558ccdL)
  in
  let z =
    Int64.(mul (logxor z (shift_right_logical z 33)) 0xc4ceb9fe1a85ec53L)
  in
  let z = Int64.(logor (logxor z (shift_right_logical z 33)) 1L) in
  if popcount (Int64.logxor z (Int64.shift_right_logical z 1)) < 24 then
    Int64.logxor z 0xaaaaaaaaaaaaaaaaL
  else z

let split { position; gamma } =
  let first = Int64.add position gamma in
  let second = Int64.add first gamma in
  let fresh = { position = mix64 first; gamma = mix_gamma second } in
  (fresh, { position = second; gamma })
