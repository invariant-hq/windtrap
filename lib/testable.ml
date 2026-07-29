(*---------------------------------------------------------------------------
   Copyright (c) 2020-2021 Craig Ferguson
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   Instance printers and float tolerance semantics adapted from windtrap v1's
   Testable, itself derived from Craig Ferguson's work on Alcotest
   (https://github.com/mirage/alcotest/pull/247). v3 removes generators and
   diff hooks from the witness (RFC "The witness", Law 6).
  ---------------------------------------------------------------------------*)

type 'a t = { pp : Format.formatter -> 'a -> unit; equal : 'a -> 'a -> bool }

(* ───── Constructors ───── *)

let make ~pp ~equal = { pp; equal }
let structural ~pp = { pp; equal = Stdlib.( = ) }
let of_equal equal = { pp = (fun ppf _ -> Pp.string ppf "<abstract>"); equal }

module type WITNESS = sig
  type t

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end

let of_module (type a) (module M : WITNESS with type t = a) =
  { pp = M.pp; equal = M.equal }

let contramap f w =
  {
    pp = (fun ppf a -> w.pp ppf (f a));
    equal = (fun a b -> w.equal (f a) (f b));
  }

let pass =
  { pp = (fun ppf _ -> Pp.string ppf "<pass>"); equal = (fun _ _ -> true) }

(* ───── Observers ───── *)

let pp w = w.pp
let equal w = w.equal
let to_string w v = Pp.to_string w.pp v

(* ───── Instances ───── *)

let unit =
  { pp = (fun ppf () -> Pp.string ppf "()"); equal = (fun () () -> true) }

let bool = { pp = Pp.bool; equal = Bool.equal }
let char = { pp = (fun ppf c -> Pp.pf ppf "%C" c); equal = Char.equal }
let string = { pp = (fun ppf s -> Pp.pf ppf "%S" s); equal = String.equal }

let bytes =
  {
    pp = (fun ppf b -> Pp.pf ppf "%S" (Bytes.to_string b));
    equal = Bytes.equal;
  }

let int = { pp = Pp.int; equal = Int.equal }
let int32 = { pp = Pp.int32; equal = Int32.equal }
let int64 = { pp = Pp.int64; equal = Int64.equal }

let nativeint =
  { pp = (fun ppf n -> Pp.pf ppf "%nd" n); equal = Nativeint.equal }

let pp_float ppf f = Pp.pf ppf "%g" f
let is_nan f = FP_nan = classify_float f

(* Shortest decimal rendering that round-trips to the exact bits: 15
   significant digits when they suffice, else 16, else 17 (always enough for a
   double). Sign of zero survives ([%g] keeps it; the round-trip check is on
   bits, not IEEE equality). Non-finite values render as [%g] does: [nan],
   [inf], [-inf]. *)
let pp_float_exact ppf f =
  if is_nan f || not (Float.is_finite f) then pp_float ppf f
  else
    let round_trips s =
      Int64.equal
        (Int64.bits_of_float (float_of_string s))
        (Int64.bits_of_float f)
    in
    let s15 = Pp.str "%.15g" f in
    let s =
      if round_trips s15 then s15
      else
        let s16 = Pp.str "%.16g" f in
        if round_trips s16 then s16 else Pp.str "%.17g" f
    in
    Pp.string ppf s

(* Bit equality with all NaNs identified (amendment B8): NaN = NaN whatever the
   payloads, [0.] <> [-0.], an infinity equal only to an infinity of the same
   sign. Not [Stdlib.Float.equal], which conflates the zeros. *)
let float_exact =
  {
    pp = pp_float_exact;
    equal =
      (fun a b ->
        (is_nan a && is_nan b)
        || Int64.equal (Int64.bits_of_float a) (Int64.bits_of_float b));
  }

(* NaN is never equal under the tolerance witnesses: [a = b] is false for
   NaN, and every [<=] comparison against a NaN difference is false. *)
let float eps =
  { pp = pp_float; equal = (fun a b -> a = b || Float.abs (a -. b) <= eps) }

(* Combined tolerance: relative handles large magnitudes, absolute handles
   near-zero values. The relative test requires a finite [max_ab]: with an
   infinite side, [rel *. max_ab] is [infinity] and [diff <= infinity] would
   make [infinity] "equal" to any float (v1's behavior, a latent bug). Equal
   infinities are caught by [a = b]. *)
let float_rel ~rel ~abs =
  {
    pp = pp_float;
    equal =
      (fun a b ->
        if is_nan a || is_nan b then false
        else if a = b then true
        else
          let diff = Float.abs (a -. b) in
          let max_ab = Float.max (Float.abs a) (Float.abs b) in
          diff <= abs || (Float.is_finite max_ab && diff <= rel *. max_ab));
  }

(* ───── Containers ───── *)

let option w =
  {
    pp = Pp.option w.pp;
    equal =
      (fun a b ->
        match (a, b) with
        | None, None -> true
        | Some a, Some b -> w.equal a b
        | Some _, None | None, Some _ -> false);
  }

let result ok_w err_w =
  {
    pp = Pp.result ~ok:ok_w.pp ~error:err_w.pp;
    equal =
      (fun a b ->
        match (a, b) with
        | Ok a, Ok b -> ok_w.equal a b
        | Error a, Error b -> err_w.equal a b
        | Ok _, Error _ | Error _, Ok _ -> false);
  }

let either left_w right_w =
  {
    pp =
      (fun ppf -> function
        | Either.Left x -> Pp.pf ppf "Left (%a)" left_w.pp x
        | Either.Right x -> Pp.pf ppf "Right (%a)" right_w.pp x);
    equal =
      (fun a b ->
        match (a, b) with
        | Either.Left a, Either.Left b -> left_w.equal a b
        | Either.Right a, Either.Right b -> right_w.equal a b
        | Either.Left _, Either.Right _ | Either.Right _, Either.Left _ -> false);
  }

let rec equal_list eq a b =
  match (a, b) with
  | [], [] -> true
  | x :: xs, y :: ys -> eq x y && equal_list eq xs ys
  | [], _ :: _ | _ :: _, [] -> false

let list w =
  { pp = Pp.brackets (Pp.list ~sep:Pp.semi w.pp); equal = equal_list w.equal }

let array w =
  {
    pp = (fun ppf arr -> Pp.pf ppf "[|%a|]" (Pp.array ~sep:Pp.semi w.pp) arr);
    equal =
      (fun a b -> Array.length a = Array.length b && Array.for_all2 w.equal a b);
  }

let slist w cmp =
  let sort = List.sort cmp in
  {
    (* Failures print the sides in the sorted order the equality compared,
       so the diff shows the multiset difference, never the incidental
       arrival order. *)
    pp = (fun ppf l -> Pp.brackets (Pp.list ~sep:Pp.semi w.pp) ppf (sort l));
    equal = (fun a b -> equal_list w.equal (sort a) (sort b));
  }

let pair a_w b_w =
  {
    pp = Pp.pair a_w.pp b_w.pp;
    equal = (fun (a1, b1) (a2, b2) -> a_w.equal a1 a2 && b_w.equal b1 b2);
  }

let triple a_w b_w c_w =
  {
    pp =
      (fun ppf (a, b, c) ->
        Pp.pf ppf "(@[%a,@ %a,@ %a@])" a_w.pp a b_w.pp b c_w.pp c);
    equal =
      (fun (a1, b1, c1) (a2, b2, c2) ->
        a_w.equal a1 a2 && b_w.equal b1 b2 && c_w.equal c1 c2);
  }

let quad a_w b_w c_w d_w =
  {
    pp =
      (fun ppf (a, b, c, d) ->
        Pp.pf ppf "(@[%a,@ %a,@ %a,@ %a@])" a_w.pp a b_w.pp b c_w.pp c d_w.pp d);
    equal =
      (fun (a1, b1, c1, d1) (a2, b2, c2, d2) ->
        a_w.equal a1 a2 && b_w.equal b1 b2 && c_w.equal c1 c2 && d_w.equal d1 d2);
  }
