(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(* The literals are frozen promises of seed.mli, so each is an equal against
   a golden. Each was computed from the documented definitions with an
   arbitrary-precision reference in Python, independent of [Seed]. *)

open Windtrap
module Seed = Windtrap.Private.Seed

let pp_word ppf w = Format.fprintf ppf "0x%016Lx" w
let word = Testable.make ~pp:pp_word ~equal:Int64.equal

let rec words n state =
  if n = 0 then []
  else
    let w, state = Seed.bits64 state in
    w :: words (n - 1) state

(* Seeds and tokens *)

let tokens =
  [
    (0x0000000000000000L, "s1:0000000000000000");
    (0x0000000000000001L, "s1:0000000000000001");
    (0x0123456789abcdefL, "s1:0123456789abcdef");
    (0x7fffffffffffffffL, "s1:7fffffffffffffff");
    (0x8000000000000000L, "s1:8000000000000000");
    (0xffffffffffffffffL, "s1:ffffffffffffffff");
  ]

let refused =
  [
    ("the empty string", "");
    ("the prefix alone", "s1:");
    ("15 digits", "s1:000000000000000");
    ("17 digits", "s1:00000000000000000");
    ("17 digits past 64 bits", "s1:10000000000000000");
    ("an uppercase digit", "s1:000000000000000A");
    ("two uppercase digits", "s1:00000000000000FF");
    ("a letter past f", "s1:000000000000000g");
    ("a trailing NUL", "s1:0000000000000000\x00");
    ("a trailing space", "s1:0000000000000000 ");
    ("a leading space", " s1:0000000000000000");
    ("a space after the prefix", "s1: 000000000000000");
    ("an uppercase prefix", "S1:0000000000000000");
    ("a padded version", "s01:0000000000000000");
    ("another version", "s2:0000000000000000");
    ("no prefix", "0000000000000000");
    ("an OCaml hexadecimal literal", "0x0000000000000000");
    ("a decimal integer", "42");
    ("a plus sign", "+1");
    ("a minus sign", "-1");
    ("0x after the prefix", "s1:0x0000000000000f");
    ("0X after the prefix", "s1:0X0000000000000f");
    ("an underscore", "s1:0000_0000000000f");
    ("a plus sign after the prefix", "s1:+00000000000000f");
    ("a minus sign after the prefix", "s1:-00000000000000f");
  ]

let derived =
  [
    ("the zero root, the empty path", 0L, "", 0, 0x8932c885a3fe960eL);
    ("the zero root", 0L, "parser/round trip", 0, 0x538edc95fb385999L);
    ("index 1", 0L, "parser/round trip", 1, 0xf8e85f6cf19007ecL);
    ("index 41", 0L, "parser/round trip", 41, 0x78ab4d30d5215124L);
    ("a root", 0x7be1d2c904aa31f5L, "parser/round trip", 0, 0xd58e166440b6c9a8L);
    ( "a path one byte longer",
      0x7be1d2c904aa31f5L,
      "parser/round trips",
      0,
      0xec0ade52565505bcL );
    ( "the all-ones root",
      0xffffffffffffffffL,
      "suite/group/leaf name",
      99,
      0x58c1fd40393ccc12L );
    ("the high-bit root", 0x8000000000000000L, "a", 7, 0x61aea7fc71d6f792L);
    ("a lone NUL", 0L, "\x00", 0, 0x82b038124ef3e1a9L);
    ("an embedded NUL", 0L, "a\x00b", 0, 0xffc9f641efb3864aL);
    ( "UTF-8",
      0x7be1d2c904aa31f5L,
      "caf\xc3\xa9/na\xc3\xafve \xe2\x9c\x93",
      2,
      0xdfaae0f8797a54f4L );
    ( "bytes that are not UTF-8",
      0x7be1d2c904aa31f5L,
      "\xff\xfe raw \x80 bytes",
      5,
      0x04b13d61346b793bL );
  ]

let round_trip seed =
  let token = Seed.to_string seed in
  equal int 19 (String.length token);
  equal (result word string) (Ok seed) (Seed.of_string token)

(* Every value of the module, then one draw of the global state against a
   copy taken before them: a value that read the global state advanced it,
   and one that wrote it replaced it. *)
let global_state_alone () =
  let before = Random.get_state () in
  ignore (Seed.random () : Seed.seed);
  ignore (Seed.of_string "s1:0000000000000001");
  ignore (Seed.to_string 1L : string);
  ignore (Seed.derive ~root:1L ~path:"a" ~index:0 : Seed.seed);
  let state = Seed.make 1L in
  ignore (Seed.bits64 state);
  ignore (Seed.below ~bound:7L state);
  ignore (Seed.split state);
  equal int (Random.State.bits before) (Random.bits ())

let seeds_and_tokens =
  group "Seeds and tokens"
    [
      cases
        "to_string is s1: then 16 lowercase hexadecimal digits, the most \
         significant first"
        ~name:snd tokens (fun (seed, token) ->
          equal string token (Seed.to_string seed));
      cases "of_string reads the seed of a token" ~name:snd tokens
        (fun (seed, token) ->
          equal (result word string) (Ok seed) (Seed.of_string token));
      prop "of_string reads back the 19-byte token of every seed" Gen.int64
        round_trip;
      cases "of_string refuses any other spelling" ~name:fst refused
        (fun (_, text) -> is_error ~pp:pp_word (Seed.of_string text));
      test "random draws another seed at each call" (fun () ->
          not_equal word (Seed.random ()) (Seed.random ()));
      cases "derive is frozen, the path hashed byte by byte"
        ~name:(fun (name, _, _, _, _) -> name)
        derived
        (fun (_, root, path, index, seed) ->
          equal word seed (Seed.derive ~root ~path ~index));
      test "a trailing NUL byte of the path derives another seed" (fun () ->
          not_equal word
            (Seed.derive ~root:0L ~path:"a" ~index:0)
            (Seed.derive ~root:0L ~path:"a\x00" ~index:0));
      test "no value reads or writes the global Random state" global_state_alone;
    ]

(* Sampling states *)

let streams =
  [
    ( "the zero seed",
      0x0000000000000000L,
      [
        0xe220a8397b1dcdafL;
        0x6e789e6aa1b965f4L;
        0x06c45d188009454fL;
        0xf88bb8a8724c81ecL;
        0x1b39896a51a8749bL;
        0x53cb9f0c747ea2eaL;
      ] );
    ( "the seed 1",
      0x0000000000000001L,
      [
        0x910a2dec89025cc1L;
        0xbeeb8da1658eec67L;
        0xf893a2eefb32555eL;
        0x71c18690ee42c90bL;
        0x71bb54d8d101b5b9L;
        0xc34d0bff90150280L;
      ] );
    ( "the all-ones seed",
      0xffffffffffffffffL,
      [
        0xe4d971771b652c20L;
        0xe99ff867dbf682c9L;
        0x382ff84cb27281e9L;
        0x6d1db36ccba982d2L;
        0xb4a0472e578069aeL;
        0xd31dadbda438bb33L;
      ] );
    ( "the high-bit seed",
      0x8000000000000000L,
      [
        0x481ec0a212a9f3dbL;
        0xc46fa638a6309012L;
        0x61a685ffc80a8140L;
        0x592e268383e356f9L;
        0x0c8881ee746884d3L;
        0x4d7e6a268a67c5ffL;
      ] );
  ]

let reused () =
  let state = Seed.make 0L in
  let _, next = Seed.bits64 state in
  ignore (words 4 state);
  ignore (words 4 next);
  equal (list word)
    [ 0xe220a8397b1dcdafL; 0x6e789e6aa1b965f4L; 0x6e789e6aa1b965f4L ]
    (words 2 state @ words 1 next)

(* The second word of the zero seed's stream follows every draw from it
   that keeps its first word. *)
let second = 0x6e789e6aa1b965f4L

(* The rejected rows take a bound just past 2{^ 62}, which rejects about a
   quarter of the words; the threshold row's first word is 6, which is
   2{^ 64} mod 10, the least word a bound of 10 keeps. *)
let bounded =
  [
    ("a bound of 1", 0L, 1L, 0L, second);
    ("a bound of 2", 0L, 2L, 1L, second);
    ("a bound of 3", 0L, 3L, 1L, second);
    ("a bound of 10", 0L, 10L, 5L, second);
    ("a bound of 256", 0L, 0x100L, 0xafL, second);
    ("the greatest bound", 0L, Int64.max_int, 0x6220a8397b1dcdb0L, second);
    ("a bound past 2^62", 0L, 0x4000000000000001L, 0x2220a8397b1dcdacL, second);
    ( "one rejected word",
      0x0123456789abcdefL,
      0x4000000000000001L,
      0x1573529b34a1d090L,
      0x2f90b72e996dccbeL );
    ( "two rejected words",
      0x14L,
      0x4000000000000001L,
      0x0079d22ed225a1f6L,
      0x5c83eea29361787cL );
    ( "a first word equal to the threshold",
      0x9cd9f015db4e58b7L,
      10L,
      6L,
      0xc83014e7d2248e0dL );
  ]

let below_then_next (_, seed, bound, value, next) =
  let drawn, rest = Seed.below ~bound (Seed.make seed) in
  equal (list word) [ value; next ] (drawn :: words 1 rest)

(* The reference computes 2{^ 64} mod [bound] and a word's unsigned remainder
   one bit at a time, sharing neither [Int64.unsigned_rem] nor negation with
   [Seed.below]. Its intermediates stay below 2{^ 63} for a bound of at most
   2{^ 62}. *)
let power_of_two_64_mod bound =
  let rec loop bits r =
    if bits = 64 then r else loop (bits + 1) (Int64.rem (Int64.mul r 2L) bound)
  in
  loop 0 1L

let unsigned_mod w bound =
  let rec loop bit r =
    if bit < 0 then r
    else
      let b = Int64.logand (Int64.shift_right_logical w bit) 1L in
      loop (bit - 1) (Int64.rem (Int64.add (Int64.mul r 2L) b) bound)
  in
  loop 63 0L

let reference_below ~bound state =
  let threshold = power_of_two_64_mod bound in
  let rec draw state =
    let w, state = Seed.bits64 state in
    if Int64.compare w 0L >= 0 && Int64.compare w threshold < 0 then draw state
    else (unsigned_mod w bound, state)
  in
  draw state

let agrees_with_reference (seed, bound) =
  let state = Seed.make seed in
  let value, rest = Seed.below ~bound state in
  let expected, expected_rest = reference_below ~bound state in
  equal (list word) (expected :: words 1 expected_rest) (value :: words 1 rest)

let reference_examples =
  List.concat_map
    (fun seed ->
      List.map
        (fun bound -> (seed, bound))
        [ 1L; 2L; 3L; 5L; 10L; 17L; 257L; 65_537L ])
    [ 0L; 1L; 20L; 0x0123456789abcdefL; 0x8000000000000000L; -1L ]

let small_bound =
  Gen.map (fun b -> Int64.(add 1L (logand b 0x3fffffffffffffffL))) Gen.int64

let any_bound = Gen.map (fun b -> Int64.(max 1L (logand b max_int))) Gen.int64

let in_range (seed, bound) =
  let value, _ = Seed.below ~bound (Seed.make seed) in
  at_least int64 ~than:0L value;
  less int64 ~than:bound value

let split_streams =
  [
    ( "the zero seed",
      0L,
      [ 0x184c6c53fb60892dL; 0xd08944b9dffc3e93L ],
      [ 0x06c45d188009454fL; 0xf88bb8a8724c81ecL ] );
    ( "another seed",
      0x0123456789abcdefL,
      [ 0x06b8ca9bda6b2d7cL; 0x428c1e7538260226L ],
      [ 0x2f90b72e996dccbeL; 0xa2d419334c4667ecL ] );
    ( "an increment of too regular a bit pattern",
      0xc3910c8d016b07d6L,
      [ 0xf9a602a17425332eL; 0x237d30830cf39d4bL ],
      [ 0xe220a8397b1dcdafL; 0x6e789e6aa1b965f4L ] );
  ]

let split_words (_, seed, fresh, continued) =
  let f, c = Seed.split (Seed.make seed) in
  equal (list word) (fresh @ continued) (words 2 f @ words 2 c)

let sampling_states =
  group "Sampling states"
    [
      cases "make's stream is frozen"
        ~name:(fun (name, _, _) -> name)
        streams
        (fun (_, seed, expected) ->
          equal (list word) expected (words 6 (Seed.make seed)));
      test "a state used twice gives the same words twice" reused;
      cases "below is frozen, and its successor is past every word it drew"
        ~name:(fun (name, _, _, _, _) -> name)
        bounded below_then_next;
      prop "below draws from 0 to bound - 1"
        ~examples:[ (0L, 1L); (0L, Int64.max_int) ]
        (Gen.pair Gen.int64 any_bound)
        in_range;
      prop
        "below rejects the words under 2^64 mod bound and keeps the remainder \
         of the next"
        ~examples:reference_examples
        (Gen.pair Gen.int64 small_bound)
        agrees_with_reference;
      cases "below raises Invalid_argument on a bound below 1"
        ~name:Int64.to_string [ 0L; -1L; Int64.min_int ] (fun bound ->
          raises_match (Exn.invalid_arg ~substring:"Seed.below") (fun () ->
              Seed.below ~bound (Seed.make 0L)));
      cases "split is frozen"
        ~name:(fun (name, _, _, _) -> name)
        split_streams split_words;
    ]

let () = exit (run "seed" [ seeds_and_tokens; sampling_states ])
