(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [compare] is [None] wherever an order would be a guess, which the
   ordering verbs would accept silently. A container admits several orders,
   and [pass] none that an ordering verb could use. *)
type 'a t = {
  pp : Format.formatter -> 'a -> unit;
  equal : 'a -> 'a -> bool;
  compare : ('a -> 'a -> int) option;
}

(* Witnesses *)

let make ~pp ~equal = { pp; equal; compare = None }
let with_compare compare w = { w with compare = Some compare }
let structural ~pp = with_compare Stdlib.compare (make ~pp ~equal:Stdlib.( = ))
let of_equal equal = make ~pp:(fun ppf _ -> Pp.string ppf Pp.abstract) ~equal

let contramap f w =
  {
    pp = (fun ppf a -> w.pp ppf (f a));
    equal = (fun a b -> w.equal (f a) (f b));
    compare = Option.map (fun cmp a b -> cmp (f a) (f b)) w.compare;
  }

(* A literal, so that [pass] stays polymorphic. An order under which all
   values are equal would make [at_most] always pass and [less] always fail. *)
let pass =
  {
    pp = (fun ppf _ -> Pp.string ppf "<pass>");
    equal = (fun _ _ -> true);
    compare = None;
  }

let pp w = w.pp
let equal w = w.equal
let compare w = w.compare
let to_string w v = Pp.to_string w.pp v

(* Instances *)

module type Ordered = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
end

let of_module (type a) (module M : Ordered with type t = a) ~pp =
  { pp; equal = M.equal; compare = Some M.compare }

let unit = of_module (module Unit) ~pp:(fun ppf () -> Pp.string ppf "()")
let bool = of_module (module Bool) ~pp:Pp.bool
let char = of_module (module Char) ~pp:(fun ppf c -> Pp.pf ppf "%C" c)
let string = of_module (module String) ~pp:(fun ppf s -> Pp.pf ppf "%S" s)

(* Verbatim, so that a multi-line value reaches the report's unified diff. *)
let text = of_module (module String) ~pp:Pp.string

let bytes =
  of_module (module Bytes) ~pp:(fun ppf b -> Pp.pf ppf "%S" (Bytes.to_string b))

let int = of_module (module Int) ~pp:Pp.int
let int32 = of_module (module Int32) ~pp:Pp.int32
let int64 = of_module (module Int64) ~pp:Pp.int64

let nativeint =
  of_module (module Nativeint) ~pp:(fun ppf n -> Pp.pf ppf "%nd" n)

(* Floats *)

(* Not [Float.equal], which makes [0.] and [-0.] equal. The order agrees:
   [Float.compare] ties them, and the sign bit breaks the tie. NaNs tie as
   they are equal, whatever their sign. *)
let float_exact =
  {
    pp = Pp.float_exact;
    equal =
      (fun a b ->
        (Float.is_nan a && Float.is_nan b)
        || Int64.equal (Int64.bits_of_float a) (Int64.bits_of_float b));
    compare =
      Some
        (fun a b ->
          match Float.compare a b with
          | 0 when not (Float.is_nan a) ->
              Bool.compare (Float.sign_bit b) (Float.sign_bit a)
          | order -> order);
  }

(* A NaN side makes [diff] NaN, which no comparison accepts. With an
   infinite side, [rel *. max_ab] is infinite and would make an infinity
   equal to any float, so the relative test needs a finite [max_ab]. *)
let float_rel ~rel ~abs =
  if not (rel >= 0.) then
    invalid_arg "Testable.float_rel: ~rel is negative or NaN";
  if not (abs >= 0.) then
    invalid_arg "Testable.float_rel: ~abs is negative or NaN";
  if rel = 0. && abs = 0. then
    invalid_arg
      "Testable.float_rel: both tolerances are zero; exact equality is spelled \
       float_exact";
  let equal a b =
    let diff = Float.abs (a -. b) in
    let max_ab = Float.max (Float.abs a) (Float.abs b) in
    a = b || diff <= abs || (Float.is_finite max_ab && diff <= rel *. max_ab)
  in
  { pp = (fun ppf f -> Pp.pf ppf "%g" f); equal; compare = Some Float.compare }

(* Checked here so that the refusal names [float]. [not (eps > 0.)] refuses
   NaN too. *)
let float eps =
  if not (eps > 0.) then
    invalid_arg
      "Testable.float: eps is not positive; exact equality is spelled \
       float_exact";
  float_rel ~rel:0. ~abs:eps

(* Containers *)

let option w = make ~pp:(Pp.option w.pp) ~equal:(Option.equal w.equal)

let result ok error =
  make
    ~pp:(Pp.result ~ok:ok.pp ~error:error.pp)
    ~equal:(Result.equal ~ok:ok.equal ~error:error.equal)

let either left right =
  let pp ppf = function
    | Either.Left v -> Pp.pf ppf "Left (%a)" left.pp v
    | Either.Right v -> Pp.pf ppf "Right (%a)" right.pp v
  in
  make ~pp ~equal:(Either.equal ~left:left.equal ~right:right.equal)

let list w = make ~pp:(Pp.brackets (Pp.list w.pp)) ~equal:(List.equal w.equal)

let array w =
  make
    ~pp:(fun ppf a -> Pp.pf ppf "[|%a|]" (Pp.array w.pp) a)
    ~equal:(fun a0 a1 ->
      Array.length a0 = Array.length a1 && Array.for_all2 w.equal a0 a1)

let slist w cmp = contramap (List.sort cmp) (list w)

let pair a b =
  make ~pp:(Pp.pair a.pp b.pp) ~equal:(fun (a0, b0) (a1, b1) ->
      a.equal a0 a1 && b.equal b0 b1)

let triple a b c =
  make
    ~pp:(fun ppf (a0, b0, c0) ->
      Pp.pf ppf "(@[%a,@ %a,@ %a@])" a.pp a0 b.pp b0 c.pp c0)
    ~equal:(fun (a0, b0, c0) (a1, b1, c1) ->
      a.equal a0 a1 && b.equal b0 b1 && c.equal c0 c1)

let quad a b c d =
  make
    ~pp:(fun ppf (a0, b0, c0, d0) ->
      Pp.pf ppf "(@[%a,@ %a,@ %a,@ %a@])" a.pp a0 b.pp b0 c.pp c0 d.pp d0)
    ~equal:(fun (a0, b0, c0, d0) (a1, b1, c1, d1) ->
      a.equal a0 a1 && b.equal b0 b1 && c.equal c0 c1 && d.equal d0 d1)
