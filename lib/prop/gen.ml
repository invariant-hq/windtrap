(*---------------------------------------------------------------------------
   Copyright (c) 2013-2017 Guillaume Bury, Simon Cruanes, Vincent Hugot,
                           Jan Midtgaard
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: BSD-2-Clause

   Generator design and several implementations (nat distribution, float bit
   manipulation, option/result ratios) derived from QCheck2
   (https://github.com/c-cube/qcheck).
  ---------------------------------------------------------------------------*)

type 'a t = Random.State.t -> 'a Tree.t

(* ───── Combinators ───── *)

let pure x _ = Tree.pure x
let map f gen st = Tree.map f (gen st)
let ( >|= ) gen f = map f gen

let ap fgen xgen st =
  let st' = Random.State.split st in
  Tree.ap (fgen st) (xgen st')

let bind gen f st =
  let st' = Random.State.split st in
  Tree.bind (gen st) (fun x -> f x (Random.State.copy st'))

let ( >>= ) = bind
let ( let+ ) gen f = map f gen

let ( and+ ) a b st =
  let st' = Random.State.split st in
  Tree.liftA2 (fun x y -> (x, y)) (a st) (b st')

let ( let* ) = bind

(* ───── Primitives ───── *)

let make_primitive ~gen ~shrink st = Tree.make_primitive shrink (gen st)
let unit _ = Tree.pure ()

let bool st =
  if Random.State.bool st then Tree.Tree (true, Seq.return (Tree.pure false))
  else Tree.pure false

(* Random.State.int bound is 2^30-1 on all platforms, not max_int *)
let max_random_int = (1 lsl 30) - 1

let int_pos st =
  let x = Random.State.int st max_random_int in
  Tree.make_primitive (fun n -> Shrink.int_towards 0 n) x

let int st =
  if Random.State.bool st then Tree.map (fun n -> -n - 1) (int_pos st)
  else int_pos st

let pick_origin_in_range ~low ~high ~goal =
  if goal < low then low else if goal > high then high else goal

let int_range ?origin low high st =
  if high < low then invalid_arg "Gen.int_range: high < low";
  let n =
    if low = high then low
    else if low >= 0 || high < 0 then
      (* Range is entirely non-negative or entirely negative *)
      let range = high - low + 1 in
      if range <= 0 then
        (* Overflow: range exceeds max_int, fall back to float *)
        low
        + int_of_float
            (Random.State.float st
               (float_of_int high -. float_of_int low +. 1.0))
      else low + Random.State.int st range
    else
      (* Range spans zero: choose side proportionally, then sample within it *)
      let f_low = float_of_int low in
      let f_high = float_of_int high in
      let ratio = -.f_low /. (1.0 +. f_high -. f_low) in
      if Random.State.float st 1.0 <= ratio then
        if -low > 0 then -Random.State.int st (-low) - 1 else low
      else if high + 1 > 0 then Random.State.int st (high + 1)
      else high
  in
  let origin =
    pick_origin_in_range ~low ~high ~goal:(Option.value origin ~default:0)
  in
  Tree.make_primitive (fun x -> Shrink.int_towards origin x) n

let nat st =
  let p = Random.State.float st 1.0 in
  let x =
    if p < 0.5 then Random.State.int st 10
    else if p < 0.75 then Random.State.int st 100
    else if p < 0.95 then Random.State.int st 1_000
    else Random.State.int st 10_000
  in
  Tree.make_primitive (fun n -> Shrink.int_towards 0 n) x

let small_int st =
  if Random.State.bool st then nat st else Tree.map Int.neg (nat st)

let int32 st =
  (* Random.State.bits yields 30 bits; two calls cover the full 32-bit range *)
  let low = Int32.of_int (Random.State.bits st) in
  let high = Int32.of_int (Random.State.bits st land 0x3) in
  let bits = Int32.logor low (Int32.shift_left high 30) in
  Tree.make_primitive (fun n -> Shrink.int32_towards 0l n) bits

let pick_origin_int32 ~low ~high ~goal =
  if goal < low then low else if goal > high then high else goal

let int32_range ?origin low high st =
  if high < low then invalid_arg "Gen.int32_range: high < low";
  (* Compute range in int64 to avoid int32 overflow *)
  let range = Int64.sub (Int64.of_int32 high) (Int64.of_int32 low) in
  let range = Int64.add range 1L in
  let n =
    if Int64.compare range (Int64.of_int max_random_int) <= 0 then
      Int32.add low (Int32.of_int (Random.State.int st (Int64.to_int range)))
    else
      (* Range exceeds single random call capacity; use float approximation *)
      let f_range = Int64.to_float range in
      let offset = Int64.of_float (Random.State.float st f_range) in
      Int32.add low (Int64.to_int32 offset)
  in
  let origin =
    pick_origin_int32 ~low ~high ~goal:(Option.value origin ~default:0l)
  in
  Tree.make_primitive (fun x -> Shrink.int32_towards origin x) n

let int64 st =
  (* Random.State.bits yields 30 bits; three calls cover the full 64-bit range *)
  let low = Int64.of_int (Random.State.bits st) in
  let mid = Int64.shift_left (Int64.of_int (Random.State.bits st)) 30 in
  let high =
    Int64.shift_left (Int64.of_int (Random.State.bits st land 0xF)) 60
  in
  let bits = Int64.(logor high (logor mid low)) in
  Tree.make_primitive (fun n -> Shrink.int64_towards 0L n) bits

let pick_origin_int64 ~low ~high ~goal =
  if goal < low then low else if goal > high then high else goal

let int64_range ?origin low high st =
  if high < low then invalid_arg "Gen.int64_range: high < low";
  (* int64 ranges can exceed max representable int, so always use float *)
  let f_low = Int64.to_float low in
  let f_high = Int64.to_float high in
  let f_range = f_high -. f_low +. 1.0 in
  let n =
    let offset = Int64.of_float (Random.State.float st f_range) in
    Int64.add low offset
  in
  let origin =
    pick_origin_int64 ~low ~high ~goal:(Option.value origin ~default:0L)
  in
  Tree.make_primitive (fun x -> Shrink.int64_towards origin x) n

let nativeint st =
  if Sys.int_size >= 63 then
    (* 64-bit platform: assemble from two 30-bit calls *)
    let low = Nativeint.of_int (Random.State.bits st) in
    let high =
      Nativeint.shift_left (Nativeint.of_int (Random.State.bits st)) 30
    in
    let bits = Nativeint.logor low high in
    if Random.State.bool st then
      Tree.make_primitive (fun n -> Shrink.nativeint_towards 0n n) bits
    else
      Tree.make_primitive
        (fun n -> Shrink.nativeint_towards 0n n)
        (Nativeint.neg bits)
  else
    (* 32-bit platform: same as int *)
    Tree.map Nativeint.of_int (int st)

let float st =
  (* Assemble 64 random bits and reinterpret as IEEE 754 double to cover the
     full float range including NaN, infinity, and subnormals *)
  let bits =
    let left = Int64.(shift_left (of_int (Random.State.bits st land 0xF)) 60) in
    let middle = Int64.(shift_left (of_int (Random.State.bits st)) 30) in
    let right = Int64.of_int (Random.State.bits st) in
    Int64.(logor left (logor middle right))
  in
  let x = Int64.float_of_bits bits in
  Tree.make_primitive (fun f -> Shrink.float_towards 0.0 f) x

let float_range ?origin low high st =
  if high < low then invalid_arg "Gen.float_range: high < low";
  let x = low +. Random.State.float st (high -. low) in
  let origin =
    pick_origin_in_range ~low ~high ~goal:(Option.value origin ~default:0.0)
  in
  Tree.make_primitive (fun f -> Shrink.float_towards origin f) x

let char_range ?origin low high =
  let lo = Char.code low in
  let hi = Char.code high in
  let origin = Option.map Char.code origin in
  map Char.chr (int_range ?origin lo hi)

let char = char_range ' ' '~'

(* ───── Containers ───── *)

let option ?(ratio = 0.85) gen st =
  let p = Random.State.float st 1.0 in
  if p < 1.0 -. ratio then Tree.pure None else Tree.opt (gen st)

let result ?(ratio = 0.75) ok_gen err_gen st =
  let p = Random.State.float st 1.0 in
  if p < 1.0 -. ratio then Tree.map (fun e -> Error e) (err_gen st)
  else Tree.map (fun o -> Ok o) (ok_gen st)

let either ?(ratio = 0.5) left_gen right_gen st =
  let p = Random.State.float st 1.0 in
  if p < ratio then Tree.map (fun x -> Either.Left x) (left_gen st)
  else Tree.map (fun x -> Either.Right x) (right_gen st)

let list_size size_gen gen st =
  let st' = Random.State.split st in
  Tree.bind (size_gen st) (fun size ->
      let st' = Random.State.copy st' in
      let rec build n acc =
        if n <= 0 then
          let l = List.rev acc in
          Tree.Tree (List.map Tree.root l, Tree.build_list_shrink_tree l)
        else build (n - 1) (gen st' :: acc)
      in
      build size [])

let list gen = list_size nat gen
let array gen st = Tree.map Array.of_list (list gen st)

let pair a b =
  let+ x = a and+ y = b in
  (x, y)

let triple a b c =
  let+ x, y = pair a b and+ z = c in
  (x, y, z)

let quad a b c d =
  let+ x, y, z = triple a b c and+ w = d in
  (x, y, z, w)

(* ───── Choice ───── *)

let oneof gens st =
  match gens with
  | [] -> invalid_arg "Gen.oneof: empty list"
  | _ ->
      let i = Random.State.int st (List.length gens) in
      (List.nth gens i) st

let oneofl xs st =
  match xs with
  | [] -> invalid_arg "Gen.oneofl: empty list"
  | _ ->
      let i = Random.State.int st (List.length xs) in
      let x = List.nth xs i in
      (* Shrink by walking backwards through earlier list positions *)
      let earlier () =
        Seq.unfold
          (fun j ->
            if j <= 0 then None
            else Some (Tree.pure (List.nth xs (j - 1)), j - 1))
          i ()
      in
      Tree.Tree (x, earlier)

let frequency weighted_gens st =
  match weighted_gens with
  | [] -> invalid_arg "Gen.frequency: empty list"
  | _ ->
      let total =
        List.fold_left (fun acc (w, _) -> acc + max 0 w) 0 weighted_gens
      in
      if total < 1 then invalid_arg "Gen.frequency: total weight < 1";
      let pick = Random.State.int st total in
      let rec choose acc = function
        | [] -> assert false
        | (w, g) :: rest ->
            let w = max 0 w in
            if pick < acc + w then g st else choose (acc + w) rest
      in
      choose 0 weighted_gens

(* ───── Strings ───── *)

let string_size size_gen char_gen st =
  let size_tree = size_gen st in
  Tree.bind size_tree (fun size ->
      let st' = Random.State.copy st in
      let chars = List.init size (fun _ -> char_gen st') in
      Tree.map
        (fun char_list ->
          let a = Array.of_list char_list in
          String.init (Array.length a) (Array.get a))
        (Tree.sequence_list chars))

let string_of char_gen = string_size nat char_gen
let string = string_of char
let bytes st = map Bytes.of_string string st

(* ───── Size Control ───── *)

let sized f st =
  let p = Random.State.float st 1.0 in
  let size =
    if p < 0.5 then Random.State.int st 10
    else if p < 0.75 then Random.State.int st 100
    else if p < 0.95 then Random.State.int st 500
    else Random.State.int st 1000
  in
  f size st

(* ───── Shrink Control ───── *)

let no_shrink gen st =
  let (Tree.Tree (x, _)) = gen st in
  Tree.pure x

let add_shrink_invariant p gen st = Tree.add_shrink_invariant p (gen st)

(* ───── Recursive Generators ───── *)

let delay f st = f () st

let fix f =
  let rec gen st = f gen st in
  gen

(* ───── Search ───── *)

let find ?(count = 100) ~f gen st =
  let rec loop n =
    if n <= 0 then None
    else
      let (Tree.Tree (x, _)) = gen st in
      if f x then Some x else loop (n - 1)
  in
  loop count
