(*--------------------------------------------------------------------------
  Copyright (c) 2026 Thibaut Mattio. All rights reserved.
  SPDX-License-Identifier: ISC

  SplitMix64 constants, the s1: token codec, and the rejection-sampled
  bounded draw. The split construction follows Steele, Lea and Vigna, "Fast
  Splittable Pseudorandom Number Generators" (OOPSLA 2014).
  --------------------------------------------------------------------------*)

type seed = int64

(* Token codec. Format frozen at v3.0: "s1:" then 16 lowercase hex digits,
   most-significant nibble first. *)

let token_prefix = "s1:"
let hex_digits = "0123456789abcdef"

let invalid_token =
  "seed must be s1: followed by 16 lowercase hexadecimal digits"

let hex_value = function
  | '0' .. '9' as digit -> Some (Char.code digit - Char.code '0')
  | 'a' .. 'f' as digit -> Some (Char.code digit - Char.code 'a' + 10)
  | _ -> None

let of_string text =
  if
    String.length text <> 19
    || String.unsafe_get text 0 <> 's'
    || String.unsafe_get text 1 <> '1'
    || String.unsafe_get text 2 <> ':'
  then Error invalid_token
  else
    let rec decode index seed =
      if index = 19 then Ok seed
      else
        match hex_value (String.unsafe_get text index) with
        | None -> Error invalid_token
        | Some value ->
            decode (index + 1) Int64.(logor (shift_left seed 4) (of_int value))
    in
    decode 3 0L

let to_string seed =
  let text = Bytes.create 19 in
  Bytes.blit_string token_prefix 0 text 0 3;
  for index = 0 to 15 do
    let shift = 4 * (15 - index) in
    let value = Int64.(to_int (logand (shift_right_logical seed shift) 0xfL)) in
    Bytes.unsafe_set text (index + 3) (String.unsafe_get hex_digits value)
  done;
  Bytes.unsafe_to_string text

(* SplitMix64 core. The constants and the transition are the reference
   algorithm's and are frozen at v3.0. *)

let golden_gamma = 0x9e3779b97f4a7c15L

let mix64 z =
  let z =
    Int64.(mul (logxor z (shift_right_logical z 30)) 0xbf58476d1ce4e5b9L)
  in
  let z =
    Int64.(mul (logxor z (shift_right_logical z 27)) 0x94d049bb133111ebL)
  in
  Int64.(logxor z (shift_right_logical z 31))

(* Derivation. Frozen at v3.0: hash64 is 64-bit FNV-1a over the
   path's bytes; the case seed is
   mix64 (mix64 (root lxor hash64 path) + golden_gamma * index). *)

let fnv_offset_basis = 0xcbf29ce484222325L
let fnv_prime = 0x100000001b3L

let hash64 text =
  let hash = ref fnv_offset_basis in
  String.iter
    (fun byte ->
      hash :=
        Int64.mul (Int64.logxor !hash (Int64.of_int (Char.code byte))) fnv_prime)
    text;
  !hash

let derive ~root ~path ~index =
  let stream = mix64 (Int64.logxor root (hash64 path)) in
  mix64 (Int64.add stream (Int64.mul golden_gamma (Int64.of_int index)))

let random () = Random.State.bits64 (Random.State.make_self_init ())

(* Sampling states. *)

type state = { position : int64; gamma : int64 }

let make seed = { position = seed; gamma = golden_gamma }

let bits64 { position; gamma } =
  let position = Int64.add position gamma in
  (mix64 position, { position; gamma })

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

(* Gamma derivation for split streams: the MurmurHash3 finalizer forced odd,
   with sparse or regular bit patterns broken up, per SplittableRandom. *)
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
