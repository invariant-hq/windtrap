(*---------------------------------------------------------------------------
   Copyright (c) 2020-2021 Craig Ferguson
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   Instance printers and float tolerance semantics adapted from windtrap v1's
   Testable, itself derived from Craig Ferguson's work on Alcotest
   (https://github.com/mirage/alcotest/pull/247). v3 removes generators and
   diff hooks from the witness: diffs come from printed values, generation
   lives in Gen, and the two witnesses never merge.
  ---------------------------------------------------------------------------*)

type 'a t = {
  pp : Format.formatter -> 'a -> unit;
  equal : 'a -> 'a -> bool;
  order : ('a -> 'a -> int) option;
      (* Ordering is optional because most types have none that a test
         should rely on: a record has no canonical order, and inventing one
         with [Stdlib.compare] is the footgun [structural] names out loud.
         The ordering verbs ask for it and say so when it is missing; every
         other verb ignores it. *)
}

(* Constructors *)

let make ~pp ~equal = { pp; equal; order = None }

(* Order arrives through its own combinator rather than an argument to
   [make]: an optional before labelled arguments is unerasable (warning
   16), so [?order] there would force a trailing [()] on every existing
   call site. This also mirrors [Gen.with_pp] — the capability a witness
   may or may not carry is attached, not configured. *)
let with_order order w = { w with order = Some order }
let structural ~pp = { pp; equal = Stdlib.( = ); order = None }
let of_equal equal =
  { pp = (fun ppf _ -> Pp.string ppf "<abstract>"); equal; order = None }

module type WITNESS = sig
  type t

  val pp : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end

let of_module (type a) (module M : WITNESS with type t = a) =
  { pp = M.pp; equal = M.equal; order = None }

let contramap f w =
  {
    pp = (fun ppf a -> w.pp ppf (f a));
    equal = (fun a b -> w.equal (f a) (f b));
    (* Order travels with everything else: comparing users by id means
       ordering them by id too. *)
    order = Option.map (fun cmp a b -> cmp (f a) (f b)) w.order;
  }

let pass =
  {
    pp = (fun ppf _ -> Pp.string ppf "<pass>");
    equal = (fun _ _ -> true);
    order = None;
  }

(* Observers *)

let pp w = w.pp
let equal w = w.equal
let order w = w.order
let to_string w v = Pp.to_string w.pp v

(* Instances *)

let unit =
  {
    pp = (fun ppf () -> Pp.string ppf "()");
    equal = (fun () () -> true);
    order = None;
  }

let bool = { pp = Pp.bool; equal = Bool.equal; order = Some Bool.compare }
let char =
  { pp = (fun ppf c -> Pp.pf ppf "%C" c); equal = Char.equal;
    order = Some Char.compare }
let string =
  { pp = (fun ppf s -> Pp.pf ppf "%S" s); equal = String.equal;
    order = Some String.compare }

(* Verbatim, so a multi-line value keeps its newlines and its failure reaches
   the renderer's unified-diff path instead of two escaped one-liners.
   [string] keeps [%S] because on a single line the quotes are what tell
   [""], [" "] and ["\t"] apart. *)
let text = { pp = Pp.string; equal = String.equal; order = Some String.compare }

let bytes =
  {
    pp = (fun ppf b -> Pp.pf ppf "%S" (Bytes.to_string b));
    equal = Bytes.equal;
    order = Some Bytes.compare;
  }

let int = { pp = Pp.int; equal = Int.equal; order = Some Int.compare }
let int32 = { pp = Pp.int32; equal = Int32.equal; order = Some Int32.compare }
let int64 = { pp = Pp.int64; equal = Int64.equal; order = Some Int64.compare }

let nativeint =
  {
    pp = (fun ppf n -> Pp.pf ppf "%nd" n);
    equal = Nativeint.equal;
    order = Some Nativeint.compare;
  }

let pp_float ppf f = Pp.pf ppf "%g" f
let is_nan f = FP_nan = classify_float f

(* The shortest round-tripping rendering, shared with [Gen] so a
   counterexample and a bit-exact witness never disagree about a value. *)
let pp_float_exact = Pp.float_exact

(* Bit equality with all NaNs identified: NaN = NaN whatever the
   payloads, [0.] <> [-0.], an infinity equal only to an infinity of the same
   sign. Not [Stdlib.Float.equal], which conflates the zeros. *)
let float_exact =
  {
    pp = pp_float_exact;
    equal =
      (fun a b ->
        (is_nan a && is_nan b)
        || Int64.equal (Int64.bits_of_float a) (Int64.bits_of_float b));
     order = Some Float.compare;
  }

(* NaN is never equal under the tolerance witnesses: [a = b] is false for
   NaN, and every [<=] comparison against a NaN difference is false. *)

(* Any eps <= 0 (NaN included) degenerates the tolerance test to the [a = b]
   shortcut — exact equality wearing a tolerance's syntax. Refused loudly:
   the caller either meant a tolerance and mistyped it, or meant exactness
   and should say so. [not (eps > 0.)] rather than [eps <= 0.] so NaN is
   caught by the same comparison. *)
let float eps =
  if not (eps > 0.) then
    invalid_arg
      "Testable.float: eps is not positive; exact equality is spelled \
       float_exact";
  {
    pp = pp_float;
    equal = (fun a b -> a = b || Float.abs (a -. b) <= eps);
    (* The tolerance is the equality's, not the order's: [greater] asks
       whether one float exceeds another, and a value inside [eps] of the
       bound does not. *)
    order = Some Float.compare;
  }

(* Combined tolerance: relative handles large magnitudes, absolute handles
   near-zero values. The relative test requires a finite [max_ab]: with an
   infinite side, [rel *. max_ab] is [infinity] and [diff <= infinity] would
   make [infinity] "equal" to any float (v1's behavior, a latent bug). Equal
   infinities are caught by [a = b]. *)

(* One zero bound is a real configuration — it switches that component off
   while the other still tolerates — so each bound is only required
   non-negative and non-NaN. Both zero, though, is [float 0.] in more
   letters: exact equality in a tolerance's syntax, refused the same way. *)
let float_rel ~rel ~abs =
  if not (rel >= 0.) then
    invalid_arg "Testable.float_rel: ~rel is negative or NaN";
  if not (abs >= 0.) then
    invalid_arg "Testable.float_rel: ~abs is negative or NaN";
  if rel = 0. && abs = 0. then
    invalid_arg
      "Testable.float_rel: both tolerances are zero; exact equality is \
       spelled float_exact";
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
     order = Some Float.compare;
  }

(* Containers *)

let option w =
  {
    pp = Pp.option w.pp;
    equal =
      (fun a b ->
        match (a, b) with
        | None, None -> true
        | Some a, Some b -> w.equal a b
        | Some _, None | None, Some _ -> false);
     order = None;
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
     order = None;
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
    order = None;
  }

let rec equal_list eq a b =
  match (a, b) with
  | [], [] -> true
  | x :: xs, y :: ys -> eq x y && equal_list eq xs ys
  | [], _ :: _ | _ :: _, [] -> false

let list w =
  {
    pp = Pp.brackets (Pp.list ~sep:Pp.semi w.pp);
    equal = equal_list w.equal;
    order = None;
  }

let array w =
  {
    pp = (fun ppf arr -> Pp.pf ppf "[|%a|]" (Pp.array ~sep:Pp.semi w.pp) arr);
    equal =
      (fun a b -> Array.length a = Array.length b && Array.for_all2 w.equal a b);
     order = None;
  }

let slist w cmp =
  let sort = List.sort cmp in
  {
    (* Failures print the sides in the sorted order the equality compared,
       so the diff shows the multiset difference, never the incidental
       arrival order. *)
    pp = (fun ppf l -> Pp.brackets (Pp.list ~sep:Pp.semi w.pp) ppf (sort l));
    equal = (fun a b -> equal_list w.equal (sort a) (sort b));
    order = None;
  }

let pair a_w b_w =
  {
    pp = Pp.pair a_w.pp b_w.pp;
    equal = (fun (a1, b1) (a2, b2) -> a_w.equal a1 a2 && b_w.equal b1 b2);
     order = None;
  }

let triple a_w b_w c_w =
  {
    pp =
      (fun ppf (a, b, c) ->
        Pp.pf ppf "(@[%a,@ %a,@ %a@])" a_w.pp a b_w.pp b c_w.pp c);
    equal =
      (fun (a1, b1, c1) (a2, b2, c2) ->
        a_w.equal a1 a2 && b_w.equal b1 b2 && c_w.equal c1 c2);
    order = None;
  }

let quad a_w b_w c_w d_w =
  {
    pp =
      (fun ppf (a, b, c, d) ->
        Pp.pf ppf "(@[%a,@ %a,@ %a,@ %a@])" a_w.pp a b_w.pp b c_w.pp c d_w.pp d);
    equal =
      (fun (a1, b1, c1, d1) (a2, b2, c2, d2) ->
        a_w.equal a1 a2 && b_w.equal b1 b2 && c_w.equal c1 c2 && d_w.equal d1 d2);
    order = None;
  }
