(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC

  Integrated shrinking in the QCheck2/Hedgehog design: generators produce
  rose trees of candidates, so shrinking falls out of composition. The
  distributions (stratified nat, float bit patterns) follow QCheck2's
  tuning (https://github.com/c-cube/qcheck); the implementation is
  windtrap's own, built over Seed (SplitMix64) and Shrink_tree.
  --------------------------------------------------------------------------*)

type 'a t = {
  pp : (Format.formatter -> 'a -> unit) option;
  run : Seed.state -> 'a Shrink_tree.t * Seed.state;
}

exception Rejected

(* Printers *)

let render_with pp value =
  try Format.asprintf "%a" pp value
  with exn -> Printf.sprintf "<printer raised %s>" (Printexc.to_string exn)

let pp_int = Format.pp_print_int
let pp_int32 ppf n = Format.fprintf ppf "%ldl" n
let pp_int64 ppf n = Format.fprintf ppf "%LdL" n

(* A counterexample is meant to be copied back into [~examples], so the
   printed float has to be the float that failed: [%.12g] does not
   round-trip, and the value a reader pastes back may not even reproduce
   the failure. *)
let pp_float = Pp.float_exact
let pp_bool = Format.pp_print_bool
let pp_unit ppf () = Format.pp_print_string ppf "()"
let pp_char ppf c = Format.fprintf ppf "%C" c
let pp_string ppf s = Format.fprintf ppf "%S" s
let pp_bytes ppf b = Format.fprintf ppf "Bytes.of_string %S" (Bytes.to_string b)
let pp_semi ppf () = Format.fprintf ppf ";@ "

let pp_list pp_elt ppf values =
  Format.fprintf ppf "@[<hov 1>[%a]@]"
    (Format.pp_print_list ~pp_sep:pp_semi pp_elt)
    values

let pp_array pp_elt ppf values =
  Format.fprintf ppf "@[<hov 2>[|%a|]@]"
    (Format.pp_print_list ~pp_sep:pp_semi pp_elt)
    (Array.to_list values)

let pp_option pp_elt ppf = function
  | None -> Format.pp_print_string ppf "None"
  | Some v -> Format.fprintf ppf "@[<hov 2>Some@ (%a)@]" pp_elt v

let pp_result pp_ok pp_err ppf = function
  | Ok v -> Format.fprintf ppf "@[<hov 2>Ok@ (%a)@]" pp_ok v
  | Error e -> Format.fprintf ppf "@[<hov 2>Error@ (%a)@]" pp_err e

let pp_pair pp_a pp_b ppf (a, b) =
  Format.fprintf ppf "@[<hov 1>(%a,@ %a)@]" pp_a a pp_b b

let pp_triple pp_a pp_b pp_c ppf (a, b, c) =
  Format.fprintf ppf "@[<hov 1>(%a,@ %a,@ %a)@]" pp_a a pp_b b pp_c c

let pp_quad pp_a pp_b pp_c pp_d ppf (a, b, c, d) =
  Format.fprintf ppf "@[<hov 1>(%a,@ %a,@ %a,@ %a)@]" pp_a a pp_b b pp_c c pp_d
    d

(* Shrink candidate sequences (adapted from windtrap v1's shrink module)
   Binary search toward a destination: emit [dest] first, then halve the
   remaining distance, converging on the sampled value without reaching
   it. Candidates stay between [dest] and the value, which is what keeps
   range generators in bounds while shrinking. *)

(* Binary-search shrink candidates: from [dest], repeatedly close half of
   the remaining gap toward [x], stopping just short of [x] itself (the
   value being shrunk is not a candidate). The gap is computed as a
   difference of halves — never [(x - current) / 2], which overflows on
   min_int/max_int spans. *)
let int_towards dest x () =
  let rec steps current () =
    if current = x then Seq.Nil
    else
      let gap = (x / 2) - (current / 2) in
      if gap = 0 then Seq.Cons (current, Seq.empty)
      else Seq.Cons (current, steps (current + gap))
  in
  steps dest ()

let int32_towards dest x () =
  let rec steps current () =
    if Int32.equal current x then Seq.Nil
    else
      let gap = Int32.sub (Int32.div x 2l) (Int32.div current 2l) in
      if Int32.equal gap 0l then Seq.Cons (current, Seq.empty)
      else Seq.Cons (current, steps (Int32.add current gap))
  in
  steps dest ()

let int64_towards dest x () =
  let rec steps current () =
    if Int64.equal current x then Seq.Nil
    else
      let gap = Int64.sub (Int64.div x 2L) (Int64.div current 2L) in
      if Int64.equal gap 0L then Seq.Cons (current, Seq.empty)
      else Seq.Cons (current, steps (Int64.add current gap))
  in
  steps dest ()

(* Float halving can produce arbitrarily many distinct values without
   reaching the target, so cap the sequence to keep shrinking bounded. *)
let float_towards dest x () =
  let rec steps current () =
    if Float.equal current x then Seq.Nil
    else
      let gap = (x /. 2.0) -. (current /. 2.0) in
      if Float.equal gap 0.0 then Seq.Cons (current, Seq.empty)
      else Seq.Cons (current, steps (current +. gap))
  in
  Seq.take 15 (steps dest) ()

(* Tree builders *)

let rec tree_towards shrink x =
  Shrink_tree.make ~root:x ~children:(Seq.map (tree_towards shrink) (shrink x))

(* [Shrink_tree.bind] for candidate re-generation ([bind], [one_of], sized
   [list]): [f] runs a generator, so forcing a shrink candidate can raise
   [Rejected] — a [such_that] in the re-run exhausting its budget for that
   candidate. Memoized child cells cache exceptions, so letting [Rejected]
   escape a cell would also hide every later sibling candidate; skipping the
   candidate keeps the search alive. Rejection of the root propagates: that
   is a generation-time discard. *)
let rec rebind tree f =
  let bound = f (Shrink_tree.root tree) in
  Shrink_tree.make ~root:(Shrink_tree.root bound)
    ~children:
      (Seq.append
         (Seq.filter_map
            (fun candidate ->
              match rebind candidate f with
              | rebound -> Some rebound
              | exception Rejected -> None)
            (Shrink_tree.children tree))
         (Shrink_tree.children bound))

(* Element-wise shrinking only, for lists whose length is constrained by an
   explicit size generator. *)
let rec seq_list = function
  | [] -> Shrink_tree.leaf []
  | tree :: trees ->
      Shrink_tree.map
        (fun (v, vs) -> v :: vs)
        (Shrink_tree.pair tree (seq_list trees))

(* Numeric primitives *)

let int =
  {
    pp = Some pp_int;
    run =
      (fun state ->
        let word, state = Seed.bits64 state in
        (tree_towards (int_towards 0) (Int64.to_int word), state));
  }

(* v1's stratified distribution: 50% below 10, 25% below 100, 20% below
   1_000, 5% below 10_000. *)
let sample_nat state =
  let stratum, state = Seed.below ~bound:100L state in
  let bound =
    if stratum < 50L then 10L
    else if stratum < 75L then 100L
    else if stratum < 95L then 1_000L
    else 10_000L
  in
  let value, state = Seed.below ~bound state in
  (Int64.to_int value, state)

let nat =
  {
    pp = Some pp_int;
    run =
      (fun state ->
        let value, state = sample_nat state in
        (tree_towards (int_towards 0) value, state));
  }

let small_int =
  {
    pp = Some pp_int;
    run =
      (fun state ->
        let sign, state = Seed.below ~bound:2L state in
        let magnitude, state = sample_nat state in
        let value = if Int64.equal sign 1L then -magnitude else magnitude in
        (tree_towards (int_towards 0) value, state));
  }

let int_range low high =
  {
    pp = Some pp_int;
    run =
      (fun state ->
        if high < low then invalid_arg "Gen.int_range: high < low";
        let origin = if low > 0 then low else if high < 0 then high else 0 in
        let low64 = Int64.of_int low in
        let span = Int64.sub (Int64.of_int high) low64 in
        let value, state =
          if Int64.equal span Int64.max_int then
            (* Full int range: cardinality overflows; use a raw word. *)
            let word, state = Seed.bits64 state in
            (Int64.to_int word, state)
          else
            let offset, state = Seed.below ~bound:(Int64.succ span) state in
            (Int64.to_int (Int64.add low64 offset), state)
        in
        (tree_towards (int_towards origin) value, state));
  }

let int32 =
  {
    pp = Some pp_int32;
    run =
      (fun state ->
        let word, state = Seed.bits64 state in
        (tree_towards (int32_towards 0l) (Int64.to_int32 word), state));
  }

let int64 =
  {
    pp = Some pp_int64;
    run =
      (fun state ->
        let word, state = Seed.bits64 state in
        (tree_towards (int64_towards 0L) word, state));
  }

let float =
  {
    pp = Some pp_float;
    run =
      (fun state ->
        (* Rejection keeps the bit-pattern distribution; non-finite patterns
           are about 0.05% of the space, so the loop is short. *)
        let rec finite state =
          let word, state = Seed.bits64 state in
          let value = Int64.float_of_bits word in
          if Float.is_finite value then (value, state) else finite state
        in
        let value, state = finite state in
        (tree_towards (float_towards 0.0) value, state));
  }

let float_range low high =
  {
    pp = Some pp_float;
    run =
      (fun state ->
        if not (Float.is_finite low && Float.is_finite high) then
          invalid_arg "Gen.float_range: bounds must be finite";
        if high < low then invalid_arg "Gen.float_range: high < low";
        if high -. low > Float.max_float then
          invalid_arg "Gen.float_range: high -. low > max_float";
        let origin =
          if low > 0.0 then low else if high < 0.0 then high else 0.0
        in
        let word, state = Seed.bits64 state in
        let unit_interval =
          Int64.to_float (Int64.shift_right_logical word 11) *. 0x1p-53
        in
        let value = low +. (unit_interval *. (high -. low)) in
        (* [high -. low] can round up past the real span, and then
           [low +. u * span] past [high]; clamp to keep the documented
           closed range. [value < low] cannot happen: the addend is
           non-negative and [low] is representable. *)
        let value = if value > high then high else value in
        (tree_towards (float_towards origin) value, state));
  }

(* Unit, booleans, characters, strings *)

(* [pure ()] generates the same value and prints nothing, and a printerless
   leaf forfeits the derived printer of every composition above it, the one
   cost a leaf of a type with exactly one value never has to impose. One
   value means one leaf, built like [bool]'s, and drawing it consumes no
   randomness, so the state passes through. *)
let unit =
  { pp = Some pp_unit; run = (fun state -> (Shrink_tree.leaf (), state)) }

let bool =
  {
    pp = Some pp_bool;
    run =
      (fun state ->
        let bit, state = Seed.below ~bound:2L state in
        let tree =
          if Int64.equal bit 1L then
            Shrink_tree.make ~root:true
              ~children:(Seq.return (Shrink_tree.leaf false))
          else Shrink_tree.leaf false
        in
        (tree, state));
  }

let rec char_tree ~origin code =
  Shrink_tree.make ~root:(Char.chr code)
    ~children:(Seq.map (char_tree ~origin) (int_towards origin code))

let char =
  {
    pp = Some pp_char;
    run =
      (fun state ->
        let code, state = Seed.below ~bound:256L state in
        (char_tree ~origin:(Char.code 'a') (Int64.to_int code), state));
  }

let char_range low high =
  {
    pp = Some pp_char;
    run =
      (fun state ->
        if high < low then invalid_arg "Gen.char_range: high < low";
        let low = Char.code low and high = Char.code high in
        (* The in-range code closest to 'a', mirroring [int_range]'s
           closest-to-0 rule with 'a' as the distinguished point. *)
        let anchor = Char.code 'a' in
        let origin =
          if low > anchor then low else if high < anchor then high else anchor
        in
        let offset, state =
          Seed.below ~bound:(Int64.of_int (high - low + 1)) state
        in
        (char_tree ~origin (low + Int64.to_int offset), state));
  }

(* Containers *)

let sample_elements gen count state =
  let rec loop remaining trees state =
    if remaining = 0 then (List.rev trees, state)
    else
      let tree, state = gen.run state in
      loop (remaining - 1) (tree :: trees) state
  in
  loop count [] state

let list ?size gen =
  let pp = Option.map pp_list gen.pp in
  match size with
  | None ->
      {
        pp;
        run =
          (fun state ->
            let length, state = sample_nat state in
            let trees, state = sample_elements gen length state in
            (Shrink_tree.list trees, state));
      }
  | Some size_gen ->
      {
        pp;
        run =
          (fun state ->
            let size_tree, state = size_gen.run state in
            let fresh, state = Seed.split state in
            let tree =
              rebind size_tree (fun size ->
                  if size < 0 then invalid_arg "Gen.list: negative size";
                  let trees, _ = sample_elements gen size fresh in
                  seq_list trees)
            in
            (tree, state));
      }

let array ?size gen =
  let base = list ?size gen in
  let pp = Option.map pp_array gen.pp in
  {
    pp;
    run =
      (fun state ->
        let tree, state = base.run state in
        (Shrink_tree.map Array.of_list tree, state));
  }

let string_of ?size char_gen =
  (* Length and shrink order follow [list ?size]: with the default (nat)
     size the empty string is the first candidate, then chunk removals,
     then characters; an explicit size constrains every candidate. *)
  let base = list ?size char_gen in
  let implode chars =
    let buffer = Bytes.create (List.length chars) in
    List.iteri (fun index c -> Bytes.set buffer index c) chars;
    Bytes.unsafe_to_string buffer
  in
  {
    pp = Some pp_string;
    run =
      (fun state ->
        let tree, state = base.run state in
        (Shrink_tree.map implode tree, state));
  }

let string = string_of char

let bytes_of ?size char_gen =
  let base = string_of ?size char_gen in
  {
    pp = Some pp_bytes;
    run =
      (fun state ->
        let tree, state = base.run state in
        (Shrink_tree.map Bytes.of_string tree, state));
  }

let bytes = bytes_of char

let option gen =
  let pp = Option.map pp_option gen.pp in
  let none = Shrink_tree.leaf None in
  let rec wrap tree =
    Shrink_tree.make
      ~root:(Some (Shrink_tree.root tree))
      ~children:(Seq.cons none (Seq.map wrap (Shrink_tree.children tree)))
  in
  {
    pp;
    run =
      (fun state ->
        let choice, state = Seed.below ~bound:100L state in
        if choice < 15L then (none, state)
        else
          let tree, state = gen.run state in
          (wrap tree, state));
  }

let result ok err =
  let pp =
    match (ok.pp, err.pp) with
    | Some pp_ok, Some pp_err -> Some (pp_result pp_ok pp_err)
    | _ -> None
  in
  {
    pp;
    run =
      (fun state ->
        let choice, state = Seed.below ~bound:100L state in
        if choice < 25L then
          let tree, state = err.run state in
          (Shrink_tree.map (fun e -> Error e) tree, state)
        else
          let tree, state = ok.run state in
          (Shrink_tree.map (fun v -> Ok v) tree, state));
  }

let pair left right =
  let pp =
    match (left.pp, right.pp) with
    | Some pp_a, Some pp_b -> Some (pp_pair pp_a pp_b)
    | _ -> None
  in
  {
    pp;
    run =
      (fun state ->
        let left_tree, state = left.run state in
        let right_tree, state = right.run state in
        (Shrink_tree.pair left_tree right_tree, state));
  }

let triple a b c =
  let pp =
    match (a.pp, b.pp, c.pp) with
    | Some pp_a, Some pp_b, Some pp_c -> Some (pp_triple pp_a pp_b pp_c)
    | _ -> None
  in
  {
    pp;
    run =
      (fun state ->
        let ta, state = a.run state in
        let tb, state = b.run state in
        let tc, state = c.run state in
        let tree =
          Shrink_tree.map
            (fun (va, (vb, vc)) -> (va, vb, vc))
            (Shrink_tree.pair ta (Shrink_tree.pair tb tc))
        in
        (tree, state));
  }

let quad a b c d =
  let pp =
    match (a.pp, b.pp, c.pp, d.pp) with
    | Some pp_a, Some pp_b, Some pp_c, Some pp_d ->
        Some (pp_quad pp_a pp_b pp_c pp_d)
    | _ -> None
  in
  {
    pp;
    run =
      (fun state ->
        let ta, state = a.run state in
        let tb, state = b.run state in
        let tc, state = c.run state in
        let td, state = d.run state in
        let tree =
          Shrink_tree.map
            (fun (va, (vb, (vc, vd))) -> (va, vb, vc, vd))
            (Shrink_tree.pair ta (Shrink_tree.pair tb (Shrink_tree.pair tc td)))
        in
        (tree, state));
  }

(* Choice and structure *)

(* The printerless leaves: their values are arbitrary, so no printer can be
   inferred. [with_pp] attaches one, and a deriving combinator built over the
   result keeps it. *)
let constant value =
  { pp = None; run = (fun state -> (Shrink_tree.leaf value, state)) }

let pure = constant

let of_list values =
  let values = Array.of_list values in
  {
    pp = None;
    run =
      (fun state ->
        let count = Array.length values in
        if count = 0 then invalid_arg "Gen.of_list: empty list";
        let index, state = Seed.below ~bound:(Int64.of_int count) state in
        let index_tree = tree_towards (int_towards 0) (Int64.to_int index) in
        (Shrink_tree.map (fun index -> values.(index)) index_tree, state));
  }

(* A choice combinator whose branches all print derives its printer (the
   doc law: a composite prints exactly when all its components print).
   Branches generate the same type, so their printers are expected to
   agree; the first branch's is used. *)
let derived_branch_pp gens =
  if
    Array.length gens > 0
    && Array.for_all (fun gen -> Option.is_some gen.pp) gens
  then gens.(0).pp
  else None

let one_of gens =
  let gens = Array.of_list gens in
  let pp = derived_branch_pp gens in
  {
    pp;
    run =
      (fun state ->
        let count = Array.length gens in
        if count = 0 then invalid_arg "Gen.one_of: empty list";
        let index, state = Seed.below ~bound:(Int64.of_int count) state in
        let index = Int64.to_int index in
        let fresh, state = Seed.split state in
        let index_tree = tree_towards (int_towards 0) index in
        let tree =
          rebind index_tree (fun branch -> fst (gens.(branch).run fresh))
        in
        (tree, state));
  }

let frequency weighted =
  let weighted = Array.of_list weighted in
  let pp = derived_branch_pp (Array.map snd weighted) in
  {
    pp;
    run =
      (fun state ->
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
        let pick, state = Seed.below ~bound:(Int64.of_int total) state in
        let pick = Int64.to_int pick in
        let rec choose index consumed =
          let weight, gen = weighted.(index) in
          if pick < consumed + weight then gen
          else choose (index + 1) (consumed + weight)
        in
        (choose 0 0).run state);
  }

(* Composition *)

let map f gen =
  {
    pp = None;
    run =
      (fun state ->
        let tree, state = gen.run state in
        (Shrink_tree.map f tree, state));
  }

let bind gen f =
  {
    pp = None;
    run =
      (fun state ->
        let outer, state = gen.run state in
        let fresh, state = Seed.split state in
        (rebind outer (fun v -> fst ((f v).run fresh)), state));
  }

let rec filter_tree keep tree =
  Shrink_tree.make ~root:(Shrink_tree.root tree)
    ~children:
      (Seq.filter_map
         (fun child ->
           if keep (Shrink_tree.root child) then Some (filter_tree keep child)
           else None)
         (Shrink_tree.children tree))

(* The resample budget is fixed: a predicate that needs more than a hundred
   draws is a generator that should have produced the shape by construction,
   and no per-call number changes that. *)
let resample_budget = 100

let such_that keep gen =
  {
    pp = gen.pp;
    run =
      (fun state ->
        let rec attempt tries state =
          if tries = 0 then raise Rejected
          else
            let fresh, state = Seed.split state in
            let tree, _ = gen.run fresh in
            if keep (Shrink_tree.root tree) then (filter_tree keep tree, state)
            else attempt (tries - 1) state
        in
        attempt resample_budget state);
  }

let with_pp pp gen = { gen with pp = Some pp }
let ( let+ ) gen f = map f gen
let ( and+ ) left right = pair left right
let ( let* ) = bind

(* Engine interface

   The property engine, Stateful, and this library's own tests reach these;
   nothing else does, which is why they are behind [Private] rather than in
   the vocabulary above. [Rejected] stays at the top of the file because
   [such_that] raises it and [rebind] catches it. *)
module Private = struct
  exception Rejected = Rejected

  (* [keep] answers about values, but the pass that runs before assembly must
     act on element *trees*, so the mask is walked against whichever list
     produced it — trees before assembly, values at every node after. A mask
     of the wrong length is a caller error, reported where every other
     malformed argument is: at sample time for the drawn list, at forcing
     time for a candidate. *)
  let survivors keep value_of items =
    let rec select flags items =
      match (flags, items) with
      | [], [] -> []
      | true :: flags, item :: items -> item :: select flags items
      | false :: flags, _ :: items -> select flags items
      | _ ->
          invalid_arg
            "Gen.Private.list_exact: ~keep returned a mask of the wrong length"
    in
    select (keep (List.map value_of items)) items

  (* A fixed draw count with [list]'s default-size move set, plus a mask no
     combinator outside this module can express: [sample] hands back a tree
     but no successor state, so nothing else can sequence [count] draws or
     drop element trees before assembly. [?keep] therefore runs in two
     passes, doing different work in each. The pass before assembly decides
     which element trees exist at all, so a dropped element contributes no
     subtree and nothing below can restore it — nor the values it would have
     shrunk to, which is the case masking on top of an assembled tree gets
     wrong in both directions: a candidate that deletes an element the mask
     was already dropping equals its parent, and one that reduces a dropped
     element into a kept one is longer than its parent. The cascade decides
     which survivors a given candidate keeps, so a deletion or a reduction
     that invalidates a later element drops it in the same candidate.
     [Shrink_tree.map] reaches the root as well as every descendant, so the
     root is masked by both passes — which is why the interface requires
     [keep] to be idempotent, and what makes the root a fixed point of it. *)
  let list_exact ?keep count gen =
    let pp = Option.map pp_list gen.pp in
    {
      pp;
      run =
        (fun state ->
          if count < 0 then
            invalid_arg "Gen.Private.list_exact: negative count";
          let trees, state = sample_elements gen count state in
          match keep with
          | None -> (Shrink_tree.list trees, state)
          | Some keep ->
              let trees = survivors keep Shrink_tree.root trees in
              let tree =
                Shrink_tree.map
                  (fun values -> survivors keep Fun.id values)
                  (Shrink_tree.list trees)
              in
              (tree, state));
    }

  let sample gen state = fst (gen.run state)
  let prints gen = Option.is_some gen.pp

  (* The remedy is not spelled here: a printerless counterexample says what it
     is, and the report says once — under the counterexample, whatever the
     printerless shape — what to do about it. *)
  let no_printer_message = "<no printer>"

  let render gen v =
    match gen.pp with
    | Some pp -> render_with pp v
    | None -> no_printer_message

  let render_value gen v = Option.map (fun pp -> render_with pp v) gen.pp
end
