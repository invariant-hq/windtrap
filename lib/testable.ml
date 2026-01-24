(*---------------------------------------------------------------------------
   Copyright (c) 2020-2021 Craig Ferguson
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   Diff output and threshold logic adapted from Craig Ferguson's work on
   Alcotest (https://github.com/mirage/alcotest/pull/247).
  ---------------------------------------------------------------------------*)

(* ───── Types ───── *)

type 'a check_result =
  | Pass
  | Fail of {
      expected_str : string;
      actual_str : string;
      diff : Failure.diff_output option;
    }

type 'a t = {
  pp : 'a Pp.t;
  equal : 'a -> 'a -> bool;
  gen : 'a Windtrap_prop.Gen.t option;
  check : ('a -> 'a -> 'a check_result) option;
}

let make ~pp ~equal ?gen ?check () = { pp; equal; gen; check }
let pp t = t.pp
let equal t = t.equal
let gen t = t.gen
let check t = t.check

(* ───── Primitive Testables ───── *)

let unit =
  {
    pp = (fun ppf () -> Pp.string ppf "()");
    equal = ( = );
    gen = Some Windtrap_prop.Gen.unit;
    check = None;
  }

let bool =
  {
    pp = Pp.bool;
    equal = Bool.equal;
    gen = Some Windtrap_prop.Gen.bool;
    check = None;
  }

let int =
  {
    pp = Pp.int;
    equal = Int.equal;
    gen = Some Windtrap_prop.Gen.int;
    check = None;
  }

let int32 =
  {
    pp = Pp.int32;
    equal = Int32.equal;
    gen = Some Windtrap_prop.Gen.int32;
    check = None;
  }

let int64 =
  {
    pp = Pp.int64;
    equal = Int64.equal;
    gen = Some Windtrap_prop.Gen.int64;
    check = None;
  }

(* NaN = NaN for testing purposes, unlike IEEE 754 where NaN <> NaN.
   This lets tests assert that a function producing NaN actually returns NaN. *)
let float eps =
  let is_nan f = FP_nan = classify_float f in
  {
    pp = (fun ppf f -> Pp.pf ppf "%g" f);
    equal =
      (fun a b -> (is_nan a && is_nan b) || a = b || Float.abs (a -. b) <= eps);
    gen = Some Windtrap_prop.Gen.float;
    check = None;
  }

(* Combined tolerance: passes if either absolute or relative tolerance is met.
   Relative tolerance handles large values; absolute tolerance handles near-zero. *)
let float_rel ~rel ~abs =
  let is_nan f = FP_nan = classify_float f in
  {
    pp = (fun ppf f -> Pp.pf ppf "%g" f);
    equal =
      (fun a b ->
        if is_nan a && is_nan b then true
        else if a = b then true
        else
          let diff = Float.abs (a -. b) in
          let max_ab = Float.max (Float.abs a) (Float.abs b) in
          diff <= abs || diff <= rel *. max_ab);
    gen = Some Windtrap_prop.Gen.float;
    check = None;
  }

let char =
  {
    pp = (fun ppf c -> Pp.pf ppf "%C" c);
    equal = Char.equal;
    gen = Some Windtrap_prop.Gen.char;
    check = None;
  }

let bytes =
  {
    pp = (fun ppf b -> Pp.pf ppf "%S" (Bytes.to_string b));
    equal = Bytes.equal;
    gen = Some Windtrap_prop.Gen.bytes;
    check = None;
  }

(* ───── Container Testables ───── *)

let option t =
  {
    pp =
      (fun ppf -> function
        | None -> Pp.pf ppf "None" | Some v -> Pp.pf ppf "Some %a" t.pp v);
    equal =
      (fun a b ->
        match (a, b) with
        | None, None -> true
        | Some a, Some b -> t.equal a b
        | _ -> false);
    gen = Option.map Windtrap_prop.Gen.option t.gen;
    check = None;
  }

let result ok_t err_t =
  {
    pp = Pp.result ~ok:ok_t.pp ~error:err_t.pp;
    equal =
      (fun a b ->
        match (a, b) with
        | Ok a, Ok b -> ok_t.equal a b
        | Error a, Error b -> err_t.equal a b
        | _ -> false);
    gen =
      (match (ok_t.gen, err_t.gen) with
      | Some ok_gen, Some err_gen ->
          Some (Windtrap_prop.Gen.result ok_gen err_gen)
      | _ -> None);
    check = None;
  }

let rec equal_list eq a b =
  match (a, b) with
  | [], [] -> true
  | x :: xs, y :: ys -> eq x y && equal_list eq xs ys
  | _ -> false

(* ───── Diff Infrastructure ───── *)

(* When more than 80% of elements differ, a highlighted diff becomes noise rather
   than signal. Above this threshold we fall back to showing plain expected/actual
   strings without per-element highlighting. *)
let diff_threshold = 4. /. 5.

let edit_ratio ~num_edits ~len_expected ~len_actual =
  let max_len =
    Float.max (Float.of_int len_expected) (Float.of_int len_actual)
  in
  if max_len = 0. then 0. else Float.of_int num_edits /. max_len

let make_highlighted_diff ~pp_elt ~sep ~prefix ~suffix expected actual
    expected_indices actual_indices =
  let format_parts list indices =
    list
    |> List.mapi (fun i x ->
        let text = Pp.str "%a" pp_elt x in
        let highlight = List.mem i indices in
        Failure.{ text; highlight })
    |> List.fold_left
         (fun acc part ->
           match acc with
           | [] -> [ part ]
           | _ -> acc @ [ { Failure.text = sep; highlight = false }; part ])
         []
    |> fun parts ->
    [ { Failure.text = prefix; highlight = false } ]
    @ parts
    @ [ { Failure.text = suffix; highlight = false } ]
  in
  Failure.
    {
      expected_parts = format_parts expected expected_indices;
      actual_parts = format_parts actual actual_indices;
    }

let check_list_with_diff ~elt_equal ~pp_elt expected actual =
  if List.equal elt_equal expected actual then Pass
  else
    let edits = Distance.(levenshtein List) ~equal:elt_equal expected actual in
    let expected_str = Pp.str "[%a]" (Pp.list ~sep:Pp.semi pp_elt) expected in
    let actual_str = Pp.str "[%a]" (Pp.list ~sep:Pp.semi pp_elt) actual in
    let ratio =
      edit_ratio ~num_edits:(List.length edits)
        ~len_expected:(List.length expected) ~len_actual:(List.length actual)
    in
    if ratio > diff_threshold then
      Fail { expected_str; actual_str; diff = None }
    else
      let expected_indices, actual_indices = Distance.edit_indices edits in
      let diff =
        make_highlighted_diff ~pp_elt ~sep:"; " ~prefix:"[" ~suffix:"]" expected
          actual expected_indices actual_indices
      in
      Fail { expected_str; actual_str; diff = Some diff }

let list t =
  let elt_equal = t.equal in
  let pp_elt = t.pp in
  let check expected actual =
    check_list_with_diff ~elt_equal ~pp_elt expected actual
  in
  {
    pp = Pp.brackets (Pp.list ~sep:Pp.semi t.pp);
    equal = equal_list t.equal;
    gen = Option.map Windtrap_prop.Gen.list t.gen;
    check = Some check;
  }

let check_array_with_diff ~elt_equal ~pp_elt expected actual =
  if
    Array.length expected = Array.length actual
    && Array.for_all2 elt_equal expected actual
  then Pass
  else
    let edits = Distance.(levenshtein Array) ~equal:elt_equal expected actual in
    let expected_str =
      Pp.str "[|%a|]" (Pp.array ~sep:Pp.semi pp_elt) expected
    in
    let actual_str = Pp.str "[|%a|]" (Pp.array ~sep:Pp.semi pp_elt) actual in
    let ratio =
      edit_ratio ~num_edits:(List.length edits)
        ~len_expected:(Array.length expected) ~len_actual:(Array.length actual)
    in
    if ratio > diff_threshold then
      Fail { expected_str; actual_str; diff = None }
    else
      let expected_indices, actual_indices = Distance.edit_indices edits in
      let diff =
        make_highlighted_diff ~pp_elt ~sep:"; " ~prefix:"[|" ~suffix:"|]"
          (Array.to_list expected) (Array.to_list actual) expected_indices
          actual_indices
      in
      Fail { expected_str; actual_str; diff = Some diff }

let array t =
  let elt_equal = t.equal in
  let pp_elt = t.pp in
  let check expected actual =
    check_array_with_diff ~elt_equal ~pp_elt expected actual
  in
  {
    pp = (fun ppf arr -> Pp.pf ppf "[|%a|]" (Pp.array ~sep:Pp.semi t.pp) arr);
    equal =
      (fun a b -> Array.length a = Array.length b && Array.for_all2 t.equal a b);
    gen = Option.map Windtrap_prop.Gen.array t.gen;
    check = Some check;
  }

let check_string_with_diff expected actual =
  if String.equal expected actual then Pass
  else
    let edits =
      Distance.(levenshtein String) ~equal:Char.equal expected actual
    in
    let expected_str = Pp.str "%S" expected in
    let actual_str = Pp.str "%S" actual in
    let ratio =
      edit_ratio ~num_edits:(List.length edits)
        ~len_expected:(String.length expected)
        ~len_actual:(String.length actual)
    in
    if ratio > diff_threshold then
      Fail { expected_str; actual_str; diff = None }
    else
      let expected_indices, actual_indices = Distance.edit_indices edits in
      let format_char_parts chars indices =
        chars
        |> List.mapi (fun i c ->
            Failure.{ text = String.make 1 c; highlight = List.mem i indices })
        |> fun parts ->
        [ { Failure.text = "\""; highlight = false } ]
        @ parts
        @ [ { Failure.text = "\""; highlight = false } ]
      in
      let diff =
        Failure.
          {
            expected_parts =
              format_char_parts
                (String.to_seq expected |> List.of_seq)
                expected_indices;
            actual_parts =
              format_char_parts
                (String.to_seq actual |> List.of_seq)
                actual_indices;
          }
      in
      Fail { expected_str; actual_str; diff = Some diff }

let string =
  {
    pp = (fun ppf s -> Pp.pf ppf "%S" s);
    equal = String.equal;
    gen = Some Windtrap_prop.Gen.string;
    check = Some check_string_with_diff;
  }

(* ───── Tuple Testables ───── *)

let pair a_t b_t =
  {
    pp = Pp.pair a_t.pp b_t.pp;
    equal = (fun (a1, b1) (a2, b2) -> a_t.equal a1 a2 && b_t.equal b1 b2);
    gen =
      (match (a_t.gen, b_t.gen) with
      | Some ga, Some gb -> Some (Windtrap_prop.Gen.pair ga gb)
      | _ -> None);
    check = None;
  }

let triple a_t b_t c_t =
  {
    pp =
      (fun ppf (a, b, c) ->
        Pp.pf ppf "(@[%a,@ %a,@ %a@])" a_t.pp a b_t.pp b c_t.pp c);
    equal =
      (fun (a1, b1, c1) (a2, b2, c2) ->
        a_t.equal a1 a2 && b_t.equal b1 b2 && c_t.equal c1 c2);
    gen =
      (match (a_t.gen, b_t.gen, c_t.gen) with
      | Some ga, Some gb, Some gc -> Some (Windtrap_prop.Gen.triple ga gb gc)
      | _ -> None);
    check = None;
  }

let quad a_t b_t c_t d_t =
  {
    pp =
      (fun ppf (a, b, c, d) ->
        Pp.pf ppf "(@[%a,@ %a,@ %a,@ %a@])" a_t.pp a b_t.pp b c_t.pp c d_t.pp d);
    equal =
      (fun (a1, b1, c1, d1) (a2, b2, c2, d2) ->
        a_t.equal a1 a2 && b_t.equal b1 b2 && c_t.equal c1 c2 && d_t.equal d1 d2);
    gen =
      (match (a_t.gen, b_t.gen, c_t.gen, d_t.gen) with
      | Some ga, Some gb, Some gc, Some gd ->
          Some (Windtrap_prop.Gen.quad ga gb gc gd)
      | _ -> None);
    check = None;
  }

(* ───── Combinators ───── *)

let pass =
  {
    pp = (fun ppf _ -> Pp.string ppf "<pass>");
    equal = (fun _ _ -> true);
    gen = None;
    check = None;
  }

let slist t cmp =
  let sort = List.sort cmp in
  {
    pp = Pp.brackets (Pp.list ~sep:Pp.semi t.pp);
    equal = (fun a b -> equal_list t.equal (sort a) (sort b));
    gen = Option.map Windtrap_prop.Gen.list t.gen;
    check = None;
  }

let of_equal equal =
  {
    pp = (fun ppf _ -> Pp.string ppf "<opaque>");
    equal;
    gen = None;
    check = None;
  }

let contramap f t =
  {
    pp = (fun ppf a -> t.pp ppf (f a));
    equal = (fun a b -> t.equal (f a) (f b));
    gen = None;
    check = None;
  }

let seq t =
  let pp ppf s =
    let first = ref true in
    Seq.iter
      (fun x ->
        if !first then first := false else Pp.pf ppf ";@ ";
        t.pp ppf x)
      s
  in
  let equal s1 s2 =
    let rec loop s1 s2 =
      match (s1 (), s2 ()) with
      | Seq.Nil, Seq.Nil -> true
      | Seq.Cons (x, xs), Seq.Cons (y, ys) -> t.equal x y && loop xs ys
      | _ -> false
    in
    loop s1 s2
  in
  make ~pp ~equal ()

let lazy_t t =
  {
    pp = (fun ppf l -> t.pp ppf (Lazy.force l));
    equal = (fun a b -> t.equal (Lazy.force a) (Lazy.force b));
    gen = None;
    check = None;
  }
