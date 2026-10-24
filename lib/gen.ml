(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(* The distributions (stratified nat, float bit patterns) follow QCheck2's
   tuning (https://github.com/c-cube/qcheck); the float encoding is
   Hypothesis's (MacIver and Donaldson, ECOOP 2020). *)

(* Shrink trees *)

(* A strict root and a memoized sequence of candidate subtrees. A cell is
   forced at most once, an exception included, so a search that visits a
   branch again never runs user code again. Stateful's programs are trees
   until they parse choices. *)
module Shrink_tree = struct
  type 'a t = { root : 'a; children : 'a t Seq.t }

  let rec memoize cells =
    let cell =
      lazy
        (match cells () with
        | Seq.Nil -> Seq.Nil
        | Seq.Cons (child, rest) -> Seq.Cons (child, memoize rest))
    in
    fun () -> Lazy.force cell

  let make ~root ~children = { root; children = memoize children }
  let leaf root = make ~root ~children:Seq.empty
  let root tree = tree.root
  let children tree = tree.children

  (* A candidate that discards is skipped: its cell would cache the discard
     and hide every later sibling. A discard of the root propagates, as a
     discard at generation time. *)
  let unless_discarded f x =
    match Failure.catch (fun () -> f x) with
    | Ok y -> Some y
    | Error `Discard -> None
    | Error c -> Failure.reraise c

  let rec map f tree =
    make ~root:(f tree.root)
      ~children:(Seq.filter_map (unless_discarded (map f)) tree.children)

  let rec pair left right =
    make ~root:(left.root, right.root)
      ~children:
        (Seq.append
           (Seq.map (fun child -> pair child right) left.children)
           (Seq.map (fun child -> pair left child) right.children))

  (* A candidate is a fresh array; [trees] is never written, and the
     untouched element trees are shared. *)
  let rec of_array trees =
    let length = Array.length trees in
    if length = 0 then leaf []
    else
      let without ~first ~chunk =
        Array.init (length - chunk) (fun i ->
            if i < first then trees.(i) else trees.(i + chunk))
      in
      let rec chunks chunk first () =
        if chunk = 0 then Seq.Nil
        else if first + chunk <= length then
          Seq.Cons
            (of_array (without ~first ~chunk), chunks chunk (first + chunk))
        else chunks (chunk / 2) 0 ()
      in
      let rec reductions index candidates () =
        match candidates () with
        | Seq.Cons (candidate, rest) ->
            let trees = Array.copy trees in
            trees.(index) <- candidate;
            Seq.Cons (of_array trees, reductions index rest)
        | Seq.Nil ->
            let next = index + 1 in
            if next = length then Seq.Nil
            else reductions next trees.(next).children ()
      in
      let rec largest_power_below power =
        if 2 * power >= length then power else largest_power_below (2 * power)
      in
      let first_chunk = if length > 1 then largest_power_below 1 else 0 in
      make
        ~root:(Array.fold_right (fun tree roots -> tree.root :: roots) trees [])
        ~children:
          (Seq.cons (leaf [])
             (Seq.append (chunks first_chunk 0)
                (reductions 0 trees.(0).children)))

  let list trees = of_array (Array.of_list trees)
end

(* Renderings *)

(* A node carries its value and how the value prints as a counterexample:
   both are parsed from the same choices, so what a search minimises is what
   the report prints. [Value] prints the value through its generator's
   printer; [Pre_image] prints what a printerless [map] or [bind] computed
   it from; [None] has nothing to print. A document is formatted only when a
   failure is reported. [literal] says that the value printed through this
   module's printers alone, which print OCaml. *)
type 'a rendering = Value of 'a | Pre_image of 'a

(* [Arrow] is a [bind]'s pre-image, [outer -> inner], so an outer that is
   itself an arrow can be parenthesised. *)
type doc = Atom of (Format.formatter -> unit) | Arrow of doc * doc
type 'a node = { value : 'a; shown : doc rendering option; literal : bool }

type 'a t = {
  pp : (Format.formatter -> 'a -> unit) option;
  draw : 'a draw;
}

and 'a draw =
  | Parse of (Choice.t -> 'a node)
  | Tree of (Seed.state -> 'a node Shrink_tree.t * Seed.state)

let atom pp v = Atom (fun ppf -> pp ppf v)

let rec pp_doc ppf = function
  | Atom print -> print ppf
  | Arrow (outer, inner) ->
      Pp.pf ppf "@[<hov 2>%a ->@ %a@]" pp_operand outer pp_doc inner

and pp_operand ppf = function
  | Arrow _ as doc -> Pp.pf ppf "(%a)" pp_doc doc
  | doc -> pp_doc ppf doc

(* Printers *)

let pp_int32 ppf n = Pp.pf ppf "%ldl" n
let pp_int64 ppf n = Pp.pf ppf "%LdL" n
let pp_nativeint ppf n = Pp.pf ppf "%ndn" n
let pp_unit ppf () = Pp.string ppf "()"
let pp_char ppf c = Pp.pf ppf "%C" c
let pp_uchar ppf u = Pp.pf ppf "Uchar.of_int 0x%X" (Uchar.to_int u)
let pp_string ppf s = Pp.pf ppf "%S" s
let pp_bytes ppf b = Pp.pf ppf "Bytes.of_string %S" (Bytes.to_string b)

let pp_list pp_elt ppf values =
  Pp.pf ppf "@[<hov 1>[%a]@]"
    (Format.pp_print_list ~pp_sep:Pp.semi pp_elt)
    values

let pp_array pp_elt ppf values =
  Pp.pf ppf "@[<hov 2>[|%a|]@]"
    (Format.pp_print_list ~pp_sep:Pp.semi pp_elt)
    (Array.to_list values)

let pp_constructor name pp_arg ppf v =
  Pp.pf ppf "@[<hov 2>%s@ (%a)@]" name pp_arg v

let pp_option pp_some ppf = function
  | None -> Pp.string ppf "None"
  | Some v -> pp_constructor "Some" pp_some ppf v

let pp_result pp_ok pp_error ppf = function
  | Ok v -> pp_constructor "Ok" pp_ok ppf v
  | Error e -> pp_constructor "Error" pp_error ppf e

let pp_either pp_left pp_right ppf = function
  | Either.Left v -> pp_constructor "Left" pp_left ppf v
  | Either.Right v -> pp_constructor "Right" pp_right ppf v

(* A tuple of values prints as the tuple of their documents, so a pre-image
   reads like the value would. *)
let pp_tuple ppf docs =
  let pp_comma ppf () = Pp.pf ppf ",@ " in
  Pp.pf ppf "@[<hov 1>(%a)@]"
    (Format.pp_print_list ~pp_sep:pp_comma pp_doc)
    docs

let pp_pair pp_a pp_b ppf (a, b) = pp_tuple ppf [ atom pp_a a; atom pp_b b ]

let pp_triple pp_a pp_b pp_c ppf (a, b, c) =
  pp_tuple ppf [ atom pp_a a; atom pp_b b; atom pp_c c ]

let pp_quad pp_a pp_b pp_c pp_d ppf (a, b, c, d) =
  pp_tuple ppf [ atom pp_a a; atom pp_b b; atom pp_c c; atom pp_d d ]

(* Nodes *)

let value node = node.value

let printed ?(literal = false) pp value =
  { value; shown = Some (Value (atom pp value)); literal }

let literal pp value = printed ~literal:true pp value
let unprintable value = { value; shown = None; literal = false }

(* A composite renders when every part renders, and as a value when every
   part is one; [pp] lays out the parts' documents in order. *)
let composite pp parts =
  let rec gather pre_image docs = function
    | [] ->
        let doc = atom pp (List.rev docs) in
        Some (if pre_image then Pre_image doc else Value doc)
    | None :: _ -> None
    | Some (Value doc) :: rest -> gather pre_image (doc :: docs) rest
    | Some (Pre_image doc) :: rest -> gather true (doc :: docs) rest
  in
  gather false [] parts

(* What a composite reads of each part. *)
type part = doc rendering option * bool

let part node : part = (node.shown, node.literal)

let composed pp value (parts : part list) =
  {
    value;
    shown = composite pp (List.map fst parts);
    literal = List.for_all snd parts;
  }

let constructor name inject node =
  let apply doc = atom (pp_constructor name pp_doc) doc in
  {
    node with
    value = inject node.value;
    shown =
      Option.map
        (function
          | Value doc -> Value (apply doc)
          | Pre_image doc -> Pre_image (apply doc))
        node.shown;
  }

(* A [map]'s result renders as its argument, marked as a pre-image. *)
let pre_image =
  Option.map (function Value doc -> Pre_image doc | shown -> shown)

(* A [bind]'s result renders as the inner value when the inner prints it, and
   as [outer -> inner] when the inner is itself a pre-image. *)
let bound outer inner =
  match inner with
  | None | Some (Value _) -> inner
  | Some (Pre_image doc) ->
      Option.map
        (fun (Value outer | Pre_image outer) -> Pre_image (Arrow (outer, doc)))
        outer

(* Parsing *)

(* Every call of a generator is a span. A tree, which no choice describes,
   parses from a seed it reads as one choice: it replays and never shrinks
   there. *)
let parse gen src =
  match gen.draw with
  | Parse parse -> Choice.span src (fun () -> parse src)
  | Tree draw ->
      let seed =
        Choice.choice src ~bound:(-1L) (Choice.sample src Seed.bits64)
      in
      Shrink_tree.root (fst (draw (Seed.make seed)))

(* The choice that encodes what [sample] draws, through [encode]. *)
let read src ~bound encode sample =
  Choice.choice src ~bound
    (Choice.sample src (fun state ->
         let x, state = sample state in
         (encode x, state)))

let word of_bits state =
  let bits, state = Seed.bits64 state in
  (of_bits bits, state)

(* Numbers *)

(* One draw in ten is one of [corners], each with equal probability; the
   others are [draw]'s. *)
let with_corners corners draw state =
  let pick, state = Seed.below ~bound:10L state in
  if Int64.equal pick 0L then
    let count = Int64.of_int (List.length corners) in
    let index, state = Seed.below ~bound:count state in
    (List.nth corners (Int64.to_int index), state)
  else draw state

module type Integer = sig
  type t

  val zero : t
  val compare : t -> t -> int
  val min : t -> t -> t
  val max : t -> t -> t
  val pred : t -> t
  val succ : t -> t
  val to_int64 : t -> int64
  val of_int64 : int64 -> t
end

module Int_ = struct
  include Int

  let to_int64 = Int64.of_int
  let of_int64 = Int64.to_int
end

module Int32_ = struct
  include Int32

  let to_int64 = Int64.of_int32
  let of_int64 = Int64.to_int32
end

module Int64_ = struct
  include Int64

  let to_int64 = Fun.id
  let of_int64 = Fun.id
end

module Nativeint_ = struct
  include Nativeint

  let to_int64 = Int64.of_nativeint
  let of_int64 = Int64.to_nativeint
end

(* The value of \[[low];[high]\] that [sample] draws, read as its distance
   to [origin], after a side choice ([0] above) when the range extends on
   both sides of it. Both sides count from [origin], which so has two
   spellings, as the prototype of shrinking.md reads an integer: one choice
   of rank, alternating sides, measured worse. [encode] maps a sampled value
   into the range, as an [int64]. *)
let ranked src ~low ~high ~origin encode sample =
  let x =
    Choice.sample src (fun state ->
        let x, state = sample state in
        (encode x, state))
  in
  let above = Int64.sub high origin and below = Int64.sub origin low in
  let below_side =
    if Int64.equal below 0L then false
    else if Int64.equal above 0L then true
    else
      let side x = if Int64.compare x origin < 0 then 1L else 0L in
      Int64.equal (Choice.choice src ~bound:1L (Option.map side x)) 1L
  in
  if below_side then
    let distance = Option.map (Int64.sub origin) x in
    Int64.sub origin (Choice.choice src ~bound:below distance)
  else
    let distance = Option.map (fun x -> Int64.sub x origin) x in
    Int64.add origin (Choice.choice src ~bound:above distance)

(* An integer generator; [check] validates the arguments inside the parse. *)
let integer (type n) (module N : Integer with type t = n) pp ?(check = ignore)
    ~low ~high ~origin sample =
  let low = N.to_int64 low and high = N.to_int64 high in
  let origin = N.to_int64 origin in
  {
    pp = Some pp;
    draw =
      Parse
        (fun src ->
          check ();
          let v = ranked src ~low ~high ~origin N.to_int64 sample in
          literal pp (N.of_int64 v));
  }

(* The range generator [name] of an integer type: its origin is the point
   closest to [0], and [uniform] draws once [low <= high] holds. *)
let range (type n) (module N : Integer with type t = n) ~name pp uniform low
    high =
  let origin = N.max low (N.min high N.zero) in
  (* The neighbours stay inside the range, where they cannot overflow. *)
  let corners =
    List.sort_uniq N.compare
      ([ low; high; origin ]
      @ (if N.compare origin low > 0 then [ N.pred origin ] else [])
      @ if N.compare origin high < 0 then [ N.succ origin ] else [])
  in
  let check () =
    if N.compare high low < 0 then invalid_arg (name ^ ": high < low")
  in
  integer (module N) pp ~check ~low ~high ~origin (with_corners corners uniform)

let range_uniform low high state =
  let low64 = Int64.of_int low in
  let span = Int64.sub (Int64.of_int high) low64 in
  (* The whole [int] range counts one value more than [Int64.max_int]. *)
  if Int64.equal span Int64.max_int then word Int64.to_int state
  else
    let offset, state = Seed.below ~bound:(Int64.succ span) state in
    (Int64.to_int (Int64.add low64 offset), state)

let int_range low high =
  range (module Int_) ~name:"Gen.int_range" Pp.int (range_uniform low high) low
    high

let int = int_range min_int max_int

(* A uniform [int64] in \[[low];[high]\], through [of_int64]. A span that
   [Seed.below] cannot bound takes whole words, and at least half of them
   fall in the range. *)
let between of_int64 low high state =
  let span = Int64.sub high low in
  if Int64.compare span 0L >= 0 && Int64.compare span Int64.max_int < 0 then
    let offset, state = Seed.below ~bound:(Int64.succ span) state in
    (of_int64 (Int64.add low offset), state)
  else
    let rec draw state =
      let bits, state = Seed.bits64 state in
      if Int64.compare low bits <= 0 && Int64.compare bits high <= 0 then
        (of_int64 bits, state)
      else draw state
    in
    draw state

(* 50% below [b0], 25% below [b1], 20% below [b2], 5% below [b3]. *)
let draw_strata (b0, b1, b2, b3) state =
  let stratum, state = Seed.below ~bound:100L state in
  let bound =
    if stratum < 50L then b0
    else if stratum < 75L then b1
    else if stratum < 95L then b2
    else b3
  in
  let value, state = Seed.below ~bound state in
  (Int64.to_int value, state)

let draw_nat = draw_strata (10L, 100L, 1_000L, 10_000L)
let draw_length = draw_strata (4L, 8L, 16L, 64L)
let nat = integer (module Int_) Pp.int ~low:0 ~high:9_999 ~origin:0 draw_nat

let small_int =
  integer (module Int_) Pp.int ~low:(-9_999) ~high:9_999 ~origin:0
    (fun state ->
      let sign, state = Seed.below ~bound:2L state in
      let magnitude, state = draw_nat state in
      ((if Int64.equal sign 1L then -magnitude else magnitude), state))

(* A whole type: its corners are [0], [1], [-1] and its extremes. *)
let whole (type n) (module N : Integer with type t = n) pp ~min_int ~max_int
    ~minus_one ~one of_bits =
  integer (module N) pp ~low:min_int ~high:max_int ~origin:N.zero
    (with_corners [ min_int; minus_one; N.zero; one; max_int ] (word of_bits))

let int32 =
  whole (module Int32_) pp_int32 ~min_int:Int32.min_int ~max_int:Int32.max_int
    ~minus_one:(-1l) ~one:1l Int64.to_int32

let int32_range low high =
  let uniform =
    between Int64.to_int32 (Int64.of_int32 low) (Int64.of_int32 high)
  in
  range (module Int32_) ~name:"Gen.int32_range" pp_int32 uniform low high

let int64 =
  whole (module Int64_) pp_int64 ~min_int:Int64.min_int ~max_int:Int64.max_int
    ~minus_one:(-1L) ~one:1L Fun.id

let int64_range low high =
  let uniform = between Fun.id low high in
  range (module Int64_) ~name:"Gen.int64_range" pp_int64 uniform low high

let nativeint =
  whole
    (module Nativeint_)
    pp_nativeint ~min_int:Nativeint.min_int ~max_int:Nativeint.max_int
    ~minus_one:(-1n) ~one:1n Int64.to_nativeint

let nativeint_range low high =
  let uniform =
    between Int64.to_nativeint (Int64.of_nativeint low)
      (Int64.of_nativeint high)
  in
  range
    (module Nativeint_)
    ~name:"Gen.nativeint_range" pp_nativeint uniform low high

(* Floats *)

(* A magnitude's lexicographic encoding, Hypothesis's: an integral float
   below 2^56 is that integer; any other is the tag 2^63, its exponent
   reordered and its fraction bits reversed. *)
let mantissa_bits = 52
let mantissa_mask = Int64.pred (Int64.shift_left 1L mantissa_bits)
let tag = Int64.min_int
let integral_limit = 0x1p56
let integral_mask = Int64.pred (Int64.shift_left 1L 56)

(* Exponents 0, 1, ... first, then -1, -2, ..., then the non-finite one. *)
let encoded_exponent biased =
  if biased = 2047 then 2047
  else if biased >= 1023 then biased - 1023
  else 1024 + (1022 - biased)

let decoded_exponent index =
  if index = 2047 then 2047
  else if index <= 1023 then index + 1023
  else 1022 - (index - 1024)

let reverse_bits v count =
  let rec go v count acc =
    if count = 0 then acc
    else
      go (Int64.shift_right_logical v 1) (count - 1)
        (Int64.logor (Int64.shift_left acc 1) (Int64.logand v 1L))
  in
  go v count 0L

(* An involution on the mantissa of a biased exponent. *)
let fraction_reversed biased mantissa =
  let unbiased = biased - 1023 in
  if unbiased <= 0 then reverse_bits mantissa mantissa_bits
  else if unbiased <= 51 then
    let count = mantissa_bits - unbiased in
    let mask = Int64.pred (Int64.shift_left 1L count) in
    let fraction = Int64.logand mantissa mask in
    Int64.logor (Int64.logxor mantissa fraction) (reverse_bits fraction count)
  else mantissa

let lex_of_magnitude m =
  if Float.is_integer m && m < integral_limit then Int64.of_float m
  else
    let bits = Int64.bits_of_float m in
    let biased = Int64.to_int (Int64.shift_right_logical bits mantissa_bits) in
    let mantissa = Int64.logand bits mantissa_mask in
    let exponent = Int64.of_int (encoded_exponent biased) in
    Int64.logor tag
      (Int64.logor
         (Int64.shift_left exponent mantissa_bits)
         (fraction_reversed biased mantissa))

let magnitude_of_lex lex =
  if Int64.compare lex 0L >= 0 then
    Int64.to_float (Int64.logand lex integral_mask)
  else
    let index =
      Int64.to_int
        (Int64.logand (Int64.shift_right_logical lex mantissa_bits) 0x7ffL)
    in
    let biased = decoded_exponent index in
    let mantissa = fraction_reversed biased (Int64.logand lex mantissa_mask) in
    let exponent = Int64.shift_left (Int64.of_int biased) mantissa_bits in
    Int64.float_of_bits (Int64.logor exponent mantissa)

(* The last finite magnitude of the order: index 2046, the subnormals. *)
let finite_bound =
  Int64.logor tag
    (Int64.logor (Int64.shift_left 2046L mantissa_bits) mantissa_mask)

(* A sign, unless [signs] fixes one, then a magnitude. *)
let read_finite src ~signs sample =
  let x = Choice.sample src sample in
  let negative =
    match signs with
    | `Both ->
        Int64.equal
          (Choice.choice src ~bound:1L
             (Option.map (fun x -> if Float.sign_bit x then 1L else 0L) x))
          1L
    | `Negative -> true
    | `Positive -> false
  in
  let magnitude =
    magnitude_of_lex
      (Choice.choice src ~bound:finite_bound
         (Option.map (fun x -> lex_of_magnitude (Float.abs x)) x))
  in
  if negative then -.magnitude else magnitude

(* The corners are the zeros, [1.] and [-1.], and of each sign the least
   subnormal, the least normal and the greatest finite float. Rejection keeps
   the bit-pattern distribution; about 0.05% of the patterns are not finite. *)
let draw_float =
  let rec finite state =
    let value, state = word Int64.float_of_bits state in
    if Float.is_finite value then (value, state) else finite state
  in
  with_corners
    [
      0.0;
      -0.0;
      1.0;
      -1.0;
      Float.succ 0.0;
      Float.pred 0.0;
      Float.min_float;
      -.Float.min_float;
      Float.max_float;
      -.Float.max_float;
    ]
    finite

let float =
  {
    pp = Some Pp.float_exact;
    draw =
      Parse
        (fun src ->
          literal Pp.float_exact (read_finite src ~signs:`Both draw_float));
  }

(* A class first: finite, [infinity], [neg_infinity] or [nan]. A sign and a
   magnitude follow whatever the class, read as [0] beside a non-finite
   value, so lowering the class to finite reads no further choice. *)
let non_finite = [| Float.infinity; Float.neg_infinity; Float.nan |]

let float_class x =
  if Float.is_nan x then 3L
  else if Float.equal x Float.infinity then 1L
  else if Float.equal x Float.neg_infinity then 2L
  else 0L

let any_float =
  let sample =
    with_corners [ Float.nan; Float.infinity; Float.neg_infinity ] draw_float
  in
  {
    pp = Some Pp.float_exact;
    draw =
      Parse
        (fun src ->
          (* The value is drawn once; its class and its parts encode it. *)
          let x = Choice.sample src sample in
          let kind = Choice.choice src ~bound:3L (Option.map float_class x) in
          let finite state =
            match x with
            | Some x when Float.is_finite x -> (x, state)
            | Some _ | None -> (0.0, state)
          in
          let v = read_finite src ~signs:`Both finite in
          let v =
            if Int64.equal kind 0L then v
            else non_finite.(Int64.to_int kind - 1)
          in
          literal Pp.float_exact v);
  }

(* Floats in the order where [-0.] is below [0.]. *)
let order_key x =
  let bits = Int64.bits_of_float x in
  if Int64.compare bits 0L < 0 then Int64.logxor bits Int64.max_int else bits

(* A range whose [high] is [-0.] has [-0.] as its origin, so it shrinks
   toward a value of its own sign. The corners are ordered by their bits,
   which tell the zeros apart; [float_range 0. 1.] means no [-0.], though
   [0. <= -0.] holds. A parsed float is clamped into the range, so its zeros
   parse to the origin. *)
let float_range low high =
  let origin =
    if low > 0.0 then low else if Float.sign_bit high then high else 0.0
  in
  let inside x = low <= x && x <= high in
  let corners =
    List.sort_uniq
      (fun a b -> Int64.compare (Int64.bits_of_float a) (Int64.bits_of_float b))
      (List.filter inside
         [ low; high; origin; Float.pred origin; Float.succ origin ]
      @ if low < 0.0 && 0.0 <= high then [ -0.0 ] else [])
  in
  let uniform state =
    let unit_interval, state =
      word
        (fun bits ->
          Int64.to_float (Int64.shift_right_logical bits 11) *. 0x1p-53)
        state
    in
    (* [high -. low] can round up past the span, and the sum then past
       [high]; it never falls below [low], whose addend is non-negative. A
       sum equal to [high] takes [high]'s sign, as [-0. +. 0.] is [0.]. *)
    let value = low +. (unit_interval *. (high -. low)) in
    ((if value >= high then high else value), state)
  in
  let signs =
    if not (Float.sign_bit low) then `Positive
    else if Float.sign_bit high then `Negative
    else `Both
  in
  let clamp x =
    if Int64.compare (order_key x) (order_key low) < 0 then low
    else if Int64.compare (order_key x) (order_key high) > 0 then high
    else x
  in
  {
    pp = Some Pp.float_exact;
    draw =
      Parse
        (fun src ->
          if not (Float.is_finite low && Float.is_finite high) then
            invalid_arg "Gen.float_range: bounds must be finite";
          if high < low then invalid_arg "Gen.float_range: high < low";
          if high -. low > Float.max_float then
            invalid_arg "Gen.float_range: high -. low > max_float";
          let x = read_finite src ~signs (with_corners corners uniform) in
          literal Pp.float_exact (clamp x));
  }

(* Unit, booleans, characters and strings *)

(* Unlike [constant ()], [unit] prints, so a composition over it keeps its
   printer. *)
let unit =
  { pp = Some pp_unit; draw = Parse (fun _ -> literal pp_unit ()) }

let bool =
  {
    pp = Some Pp.bool;
    draw =
      Parse
        (fun src ->
          let bit = read src ~bound:1L Fun.id (Seed.below ~bound:2L) in
          literal Pp.bool (Int64.equal bit 1L));
  }

let char_range low high =
  let code c = Int64.of_int (Char.code c) in
  let origin =
    Int.max (Char.code low) (Int.min (Char.code high) (Char.code 'a'))
  in
  let sample state =
    let count = Char.code high - Char.code low + 1 in
    let offset, state = Seed.below ~bound:(Int64.of_int count) state in
    (Char.chr (Char.code low + Int64.to_int offset), state)
  in
  {
    pp = Some pp_char;
    draw =
      Parse
        (fun src ->
          if high < low then invalid_arg "Gen.char_range: high < low";
          let v =
            ranked src ~low:(code low) ~high:(code high)
              ~origin:(Int64.of_int origin) code sample
          in
          literal pp_char (Char.chr (Int64.to_int v)));
  }

let char = char_range '\000' '\255'

(* The index of a scalar value is its rank among them: its code point, less
   the 2048 surrogates below it. *)
let uchar_index u =
  let code = Uchar.to_int u in
  if code < 0xD800 then code else code - 0x800

let uchar_of_index index =
  Uchar.of_int (if index < 0xD800 then index else index + 0x800)

(* A length of UTF-8 encoding, 1 to 4 bytes, with equal probability, then a
   value of that length uniformly: over all the scalar values, 94% are 4
   bytes long and one draw in 8,700 is ASCII. *)
let uchar =
  (* The first index of each length, and the end of the last. *)
  let starts = [| 0x0; 0x80; 0x800; 0xF800; 0x10F800 |] in
  (* The bounds of the lengths, the edges of the surrogates and U+FFFD. *)
  let corners =
    List.map Uchar.of_int
      ([ 0x0; 0x7F; 0x80; 0x7FF; 0x800; 0xFFFF; 0x10000; 0x10FFFF ]
      @ [ 0xD7FF; 0xE000; 0xFFFD ])
  in
  let uniform state =
    let length, state = Seed.below ~bound:4L state in
    let first = starts.(Int64.to_int length) in
    let count = starts.(Int64.to_int length + 1) - first in
    let offset, state = Seed.below ~bound:(Int64.of_int count) state in
    (uchar_of_index (first + Int64.to_int offset), state)
  in
  let index u = Int64.of_int (uchar_index u) in
  let origin = index (Uchar.of_char 'a') in
  {
    pp = Some pp_uchar;
    draw =
      Parse
        (fun src ->
          let v =
            ranked src ~low:0L ~high:(Int64.of_int (starts.(4) - 1)) ~origin
              index
              (with_corners corners uniform)
          in
          literal pp_uchar (uchar_of_index (Int64.to_int v)));
  }

(* The elements of [list ?size gen], which [list], [array] and [string_of]
   each assemble into their own node. Without [size], a "one more" choice
   comes before each element, the two one span, and [draw_length] samples
   the length that the choices then encode. *)
let elements ?size gen src =
  match size with
  | None ->
      let length = Choice.sample src draw_length in
      let rec more count nodes =
        let next =
          Choice.span src (fun () ->
              let drawn = Option.map (fun n -> if count < n then 1L else 0L) in
              if Int64.equal (Choice.choice src ~bound:1L (drawn length)) 0L
              then None
              else Some (parse gen src))
        in
        match next with
        | None -> List.rev nodes
        | Some node -> more (count + 1) (node :: nodes)
      in
      more 0 []
  | Some size ->
      let length = (parse size src).value in
      if length < 0 then invalid_arg "Gen.list: negative size";
      Choice.split src (fun () ->
          let rec draw count nodes =
            if count = 0 then List.rev nodes
            else draw (count - 1) (parse gen src :: nodes)
          in
          draw length [])

let string_of ?size char =
  let implode nodes =
    let buffer = Bytes.create (List.length nodes) in
    List.iteri (fun i node -> Bytes.set buffer i node.value) nodes;
    literal pp_string (Bytes.unsafe_to_string buffer)
  in
  {
    pp = Some pp_string;
    draw = Parse (fun src -> implode (elements ?size char src));
  }

let string = string_of char

let bytes_of ?size char =
  let as_bytes node = literal pp_bytes (Bytes.of_string node.value) in
  {
    pp = Some pp_bytes;
    draw = Parse (fun src -> as_bytes (parse (string_of ?size char) src));
  }

let bytes = bytes_of char

(* Containers *)

let list ?size gen =
  {
    pp = Option.map pp_list gen.pp;
    draw =
      Parse
        (fun src ->
          let nodes = elements ?size gen src in
          composed (pp_list pp_doc) (List.map value nodes)
            (List.map part nodes));
  }

let array ?size gen =
  let pp_docs ppf docs = pp_array pp_doc ppf (Array.of_list docs) in
  {
    pp = Option.map pp_array gen.pp;
    draw =
      Parse
        (fun src ->
          let nodes = elements ?size gen src in
          composed pp_docs
            (Array.of_list (List.map value nodes))
            (List.map part nodes));
  }

(* A choice between two constructors, [0] the first: [sample] draws [1] for
   the second, with the probability of 0.2's draw. *)
let second src sample = Int64.equal (read src ~bound:1L Fun.id sample) 1L

let below_percent share state =
  let choice, state = Seed.below ~bound:100L state in
  ((if choice < share then 0L else 1L), state)

let option gen =
  let none = literal Pp.string "None" in
  {
    pp = Option.map pp_option gen.pp;
    draw =
      Parse
        (fun src ->
          if second src (below_percent 15L) then
            constructor "Some" Option.some (parse gen src)
          else { none with value = None });
  }

(* [pp] derives when both parts print. *)
let both derive a b =
  match (a.pp, b.pp) with Some a, Some b -> Some (derive a b) | _ -> None

let result ok error =
  let error_first state =
    let choice, state = Seed.below ~bound:100L state in
    ((if choice < 25L then 1L else 0L), state)
  in
  {
    pp = both pp_result ok error;
    draw =
      Parse
        (fun src ->
          if second src error_first then
            constructor "Error" Result.error (parse error src)
          else constructor "Ok" Result.ok (parse ok src));
  }

let either left right =
  {
    pp = both pp_either left right;
    draw =
      Parse
        (fun src ->
          if second src (Seed.below ~bound:2L) then
            constructor "Right" Either.right (parse right src)
          else constructor "Left" Either.left (parse left src));
  }

let tuple value nodes = composed pp_tuple value nodes

let pair a b =
  {
    pp = both pp_pair a b;
    draw =
      Parse
        (fun src ->
          let a = parse a src in
          let b = parse b src in
          tuple (a.value, b.value) [ part a; part b ]);
  }

let triple a b c =
  {
    pp =
      (match (a.pp, b.pp, c.pp) with
      | Some pp_a, Some pp_b, Some pp_c -> Some (pp_triple pp_a pp_b pp_c)
      | _ -> None);
    draw =
      Parse
        (fun src ->
          let a = parse a src in
          let b = parse b src in
          let c = parse c src in
          tuple (a.value, b.value, c.value) [ part a; part b; part c ]);
  }

let quad a b c d =
  {
    pp =
      (match (a.pp, b.pp, c.pp, d.pp) with
      | Some pp_a, Some pp_b, Some pp_c, Some pp_d ->
          Some (pp_quad pp_a pp_b pp_c pp_d)
      | _ -> None);
    draw =
      Parse
        (fun src ->
          let a = parse a src in
          let b = parse b src in
          let c = parse c src in
          let d = parse d src in
          tuple
            (a.value, b.value, c.value, d.value)
            [ part a; part b; part c; part d ]);
  }

(* Constants, choices and filters *)

(* A listed value renders with [pp] when one is given, as under [with_pp]. *)
let listed ?pp value =
  match pp with None -> unprintable value | Some pp -> printed pp value

let constant ?pp value =
  { pp; draw = Parse (fun _ -> listed ?pp value) }

(* An index below [count], drawn uniformly. *)
let index src count =
  let bound = Int64.of_int count in
  Int64.to_int
    (read src ~bound:(Int64.pred bound) Fun.id (Seed.below ~bound))

let of_list ?pp values =
  let values = Array.of_list values in
  let count = Array.length values in
  {
    pp;
    draw =
      Parse
        (fun src ->
          if count = 0 then invalid_arg "Gen.of_list: empty list";
          listed ?pp values.(index src count));
  }

(* A list of listed values prints its elements with [pp], as [list] does. *)
let listed_list ?pp values =
  match pp with
  | None -> unprintable values
  | Some pp -> printed (pp_list pp) values

(* The length is [int_range 0 n]'s draw. Selection sampling then keeps the
   element at each position with probability [wanted / left], which makes
   every choice of positions of that length equally likely and keeps their
   order. Each position reads a keep choice, [1] to keep. *)
let subsequence ?pp values =
  let count = List.length values in
  let length =
    with_corners
      (List.sort_uniq Int.compare [ 0; count; Int.min 1 count ])
      (range_uniform 0 count)
  in
  {
    pp = Option.map pp_list pp;
    draw =
      Parse
        (fun src ->
          let wanted = ref (Choice.sample src length) in
          let keep left =
            match !wanted with
            | Some 0 -> Some 0L
            | Some w ->
                let pick =
                  Choice.sample src (Seed.below ~bound:(Int64.of_int left))
                in
                let kept = Int64.to_int (Option.get pick) < w in
                if kept then wanted := Some (w - 1);
                Some (if kept then 1L else 0L)
            | None -> None
          in
          let rec select left kept = function
            | [] -> List.rev kept
            | v :: vs ->
                let bit = Choice.choice src ~bound:1L (keep left) in
                select (left - 1)
                  (if Int64.equal bit 1L then v :: kept else kept)
                  vs
          in
          listed_list ?pp (select count [] values));
  }

(* A permutation is drawn as 0.2 drew it, as its Lehmer code: digit [k] is
   the index of the element placed at [k] among those not yet placed, taken
   in their order in [values], drawn uniformly below [n - k]. It is read as
   its inversion table: for each position [i] of [values], the number of
   later positions placed before it, below [n - i], so the last is [0] and
   is not read. Lowering one entry moves one element left past later ones
   and leaves the others in their order, and the zeros are the given
   order. *)
let permutation ?pp values =
  let values = Array.of_list values in
  let count = Array.length values in
  (* The order that a Lehmer code places the indices of [values] in. *)
  let lehmer code =
    let remaining = Array.init count Fun.id in
    Array.init count (fun k ->
        let digit = code.(k) in
        let index = remaining.(digit) in
        Array.blit remaining (digit + 1) remaining digit
          (count - k - 1 - digit);
        index)
  in
  let draw_order state =
    let code = Array.make count 0 in
    let rec draw k state =
      if k >= count - 1 then state
      else
        let digit, state = Seed.below ~bound:(Int64.of_int (count - k)) state in
        code.(k) <- Int64.to_int digit;
        draw (k + 1) state
    in
    let state = draw 0 state in
    (lehmer code, state)
  in
  (* Each index's count of later indices placed before it. *)
  let inversions order =
    Array.init count (fun i ->
        let rec before k later =
          if order.(k) = i then later
          else before (k + 1) (if order.(k) > i then later + 1 else later)
        in
        before 0 0)
  in
  (* [i] goes after [at] of the later indices, which are placed already. *)
  let rec insert at i = function
    | order when at = 0 -> i :: order
    | later :: order -> later :: insert (at - 1) i order
    | [] -> [ i ]
  in
  {
    pp = Option.map pp_list pp;
    draw =
      Parse
        (fun src ->
          let drawn = Option.map inversions (Choice.sample src draw_order) in
          let table =
            Array.init count (fun i ->
                let entry = Option.map (fun table -> Int64.of_int table.(i)) in
                Int64.to_int
                  (Choice.choice src
                     ~bound:(Int64.of_int (count - 1 - i))
                     (entry drawn)))
          in
          let order = ref [] in
          for i = count - 1 downto 0 do
            order := insert table.(i) i !order
          done;
          listed_list ?pp (List.map (fun i -> values.(i)) !order));
  }

(* Branches generate one type, so their printers are expected to agree; a
   sampled value still renders with the branch that drew it. *)
let branches_pp = function
  | gen :: _ as gens when List.for_all (fun gen -> Option.is_some gen.pp) gens
    ->
      gen.pp
  | _ -> None

let one_of gens =
  let pp = branches_pp gens in
  let gens = Array.of_list gens in
  let count = Array.length gens in
  {
    pp;
    draw =
      Parse
        (fun src ->
          if count = 0 then invalid_arg "Gen.one_of: empty list";
          let branch = gens.(index src count) in
          Choice.split src (fun () -> parse branch src));
  }

(* The choice is the weighted point drawn, so a branch of weight [0] is never
   parsed. *)
let frequency weighted =
  let pp = branches_pp (List.map snd weighted) in
  let weighted = Array.of_list weighted in
  {
    pp;
    draw =
      Parse
        (fun src ->
          if Array.length weighted = 0 then
            invalid_arg "Gen.frequency: empty list";
          let total =
            Array.fold_left
              (fun total (weight, _) ->
                if weight < 0 then invalid_arg "Gen.frequency: negative weight";
                total + weight)
              0 weighted
          in
          if total < 1 then invalid_arg "Gen.frequency: total weight < 1";
          let pick = index src total in
          let rec choose index consumed =
            let weight, gen = weighted.(index) in
            if pick < consumed + weight then gen
            else choose (index + 1) (consumed + weight)
          in
          parse (choose 0 0) src);
  }

(* A predicate that needs more draws is a shape to build by construction. *)
let resample_budget = 100

(* Each attempt is a span, so a deletion removes the attempts that failed
   [keep]. *)
let such_that keep gen =
  {
    pp = gen.pp;
    draw =
      Parse
        (fun src ->
          let rec attempt tries =
            if tries = 0 then raise (Failure.Control `Discard)
            else
              let node = Choice.split src (fun () -> parse gen src) in
              if keep node.value then node else attempt (tries - 1)
          in
          attempt resample_budget);
  }

(* Composition *)

let map f gen =
  {
    pp = None;
    draw =
      Parse
        (fun src ->
          let node = parse gen src in
          let shown = pre_image node.shown in
          { value = f node.value; shown; literal = false });
  }

let bind gen f =
  {
    pp = None;
    draw =
      Parse
        (fun src ->
          let outer = parse gen src in
          let inner = Choice.split src (fun () -> parse (f outer.value) src) in
          let shown = bound outer.shown inner.shown in
          { inner with shown; literal = false });
  }

let with_pp pp gen =
  {
    pp = Some pp;
    draw = Parse (fun src -> printed pp (parse gen src).value);
  }

let ( let+ ) gen f = map f gen
let ( and+ ) = pair
let ( let* ) = bind

(* Engine *)

module Engine = struct
  module Shrink_tree = Shrink_tree

  type 'a sample = 'a node

  (* A generator's value as a tree, for a reader that searches trees: a
     node's candidates are the edits of one step of the reducer that parse
     to a shortlex smaller sequence, each sequence once, and a candidate
     whose parse discards or raises a fault is skipped. *)
  let rec tree_of gen (case : Shrink.case) node =
    let seen = Hashtbl.create 16 in
    let candidate choices =
      let src = Choice.replaying choices in
      match Failure.catch (fun () -> parse gen src) with
      | Ok node
        when Shrink.shortlex (Choice.choices src) case.choices < 0
             && not (Hashtbl.mem seen (Choice.choices src)) ->
          Hashtbl.add seen (Choice.choices src) ();
          Some
            (tree_of gen
               {
                 choices = Choice.choices src;
                 spans = Choice.spans src;
                 dependents = Choice.dependents src;
               }
               node)
      | Ok _ | Error (`Discard | #Failure.fault) -> None
      | Error c -> Failure.reraise c
    in
    Shrink_tree.make ~root:node
      ~children:(Seq.filter_map candidate (Shrink.candidates case))

  let draw gen state =
    match gen.draw with
    | Tree draw -> draw state
    | Parse _ ->
        let src = Choice.generating state in
        let node = parse gen src in
        let case : Shrink.case =
          {
            choices = Choice.choices src;
            spans = Choice.spans src;
            dependents = Choice.dependents src;
          }
        in
        (tree_of gen case node, Option.get (Choice.state src))

  let sample gen state = fst (draw gen state)
  let value = value
  let parse = parse
  let tree gen = match gen.draw with Tree draw -> Some draw | Parse _ -> None

  type nonrec 'a rendering = 'a rendering = Value of 'a | Pre_image of 'a

  let no_printer = "<no printer: attach one with Gen.with_pp>"

  let printer_raised c =
    Pp.str "<printer raised %s>" (Failure.caught_to_string c)

  (* A sample renders after a case failed: what its printer raises, a
     control included, becomes the text, so nothing replaces the failure
     found. *)
  let render_with pp v =
    match Failure.catch (fun () -> Pp.to_string pp v) with
    | Ok text -> text
    | Error c -> printer_raised c

  let render node =
    match node.shown with
    | None -> Value no_printer
    | Some (Value doc) -> Value (render_with pp_doc doc)
    | Some (Pre_image doc) -> Pre_image (render_with pp_doc doc)

  let prints node = Option.is_some node.shown

  let literal node =
    match node.shown with Some (Value _) -> node.literal | _ -> false

  (* A bare value renders before its run, where a control belongs to its
     owner: only a fault becomes the text. *)
  let render_value gen v =
    match gen.pp with
    | None -> no_printer
    | Some pp -> (
        match Failure.catch (fun () -> Pp.to_string pp v) with
        | Ok text -> text
        | Error (#Failure.fault as fault) -> printer_raised fault
        | Error (#Failure.control as control) -> Failure.reraise control)

  let run gen state =
    let tree, state = draw gen state in
    (Shrink_tree.map value tree, state)

  let make ?pp draw =
    let node = match pp with Some pp -> printed pp | None -> unprintable in
    {
      pp;
      draw =
        Tree
          (fun state ->
            let tree, state = draw state in
            (Shrink_tree.map node tree, state));
    }
end
