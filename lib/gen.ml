(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC

  Integrated shrinking in the QCheck2/Hedgehog design: generators produce
  rose trees of candidates, so shrinking falls out of composition. The
  distributions (stratified nat, float bit patterns) follow QCheck2's
  tuning (https://github.com/c-cube/qcheck); the implementation is
  windtrap's own, built over Seed (SplitMix64) and Shrink_tree.
  --------------------------------------------------------------------------*)

(* Provenance: the primitive draws behind a generated value, kept lazy so a
   passing case never renders anything. [Chain] is generation order (bind),
   [Label] a choice combinator's branch, [Tuple]/[Items] grouping. A
   generator that owns a printer collapses its provenance to one [Draw] of
   the printed value, which is how enclosing printerless generators keep
   showing it (the "composition cannot lose the printer" contract). *)
type trace =
  | Empty
  | Draw of string Lazy.t
  | Label of string * trace
  | Tuple of trace list
  | Items of trace list
  | Chain of trace * trace

type 'a sample = { value : 'a; trace : trace }

type 'a t = {
  pp : (Format.formatter -> 'a -> unit) option;
  run : Seed.state -> 'a sample Shrink_tree.t * Seed.state;
}

exception Rejected

(* Printers *)

let render_with pp value =
  try Format.asprintf "%a" pp value
  with exn -> Printf.sprintf "<printer raised %s>" (Printexc.to_string exn)

let draw_of pp value = Draw (lazy (render_with pp value))
let pp_int = Format.pp_print_int
let pp_int32 ppf n = Format.fprintf ppf "%ldl" n
let pp_int64 ppf n = Format.fprintf ppf "%LdL" n
let pp_float = Format.pp_print_float
let pp_bool = Format.pp_print_bool
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

let rec tree_towards pp shrink x =
  Shrink_tree.make
    ~root:{ value = x; trace = draw_of pp x }
    ~children:(Seq.map (tree_towards pp shrink) (shrink x))

let rec plain_towards shrink x =
  Shrink_tree.make ~root:x ~children:(Seq.map (plain_towards shrink) (shrink x))

(* Rewrite every node's provenance to the printed value; applied whenever a
   generator owns a printer. *)
let renormalize pp tree =
  Shrink_tree.map (fun s -> { s with trace = draw_of pp s.value }) tree

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
        (fun (s, ss) -> s :: ss)
        (Shrink_tree.pair tree (seq_list trees))

let label_trace name index inner =
  Label (Printf.sprintf "%s[%d]" name index, inner)

(* Provenance rendering *)

let trace_budget = 200

let flatten trace =
  let rec go acc = function
    | Empty -> acc
    | Chain (a, b) -> go (go acc b) a
    | t -> t :: acc
  in
  go [] trace

let render_trace trace =
  let buffer = Buffer.create 64 in
  let remaining = ref trace_budget in
  let emit text =
    if !remaining <= 0 then raise_notrace Exit;
    let length = String.length text in
    if length <= !remaining then (
      Buffer.add_string buffer text;
      remaining := !remaining - length)
    else
      (* Never split a UTF-8 sequence: if the first excluded byte is a
         continuation byte, back off to the sequence's lead byte. On
         non-UTF-8 printer output this only shortens the cut. *)
      let cut = ref !remaining in
      while !cut > 0 && Char.code text.[!cut] land 0xC0 = 0x80 do
        decr cut
      done;
      Buffer.add_string buffer (String.sub text 0 !cut);
      remaining := 0;
      raise_notrace Exit
  in
  let rec item = function
    | Empty | Chain _ -> assert false (* flattened away *)
    | Draw text -> emit (Lazy.force text)
    | Label (label, inner) -> (
        match flatten inner with
        | [] -> emit label
        | [ single ] ->
            emit label;
            emit " ";
            item single
        | parts ->
            emit label;
            emit " (";
            sequence " " parts;
            emit ")")
    | Tuple traces ->
        emit "(";
        groups ", " traces;
        emit ")"
    | Items traces ->
        emit "[";
        groups "; " traces;
        emit "]"
  and sequence separator = function
    | [] -> ()
    | [ part ] -> item part
    | part :: parts ->
        item part;
        emit separator;
        sequence separator parts
  and group trace =
    match flatten trace with [] -> emit "_" | parts -> sequence " " parts
  and groups separator = function
    | [] -> ()
    | [ trace ] -> group trace
    | trace :: traces ->
        group trace;
        emit separator;
        groups separator traces
  in
  (try sequence " " (flatten trace) with Exit -> Buffer.add_string buffer "…");
  Buffer.contents buffer

(* Numeric primitives *)

let int =
  {
    pp = Some pp_int;
    run =
      (fun state ->
        let word, state = Seed.bits64 state in
        (tree_towards pp_int (int_towards 0) (Int64.to_int word), state));
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
        (tree_towards pp_int (int_towards 0) value, state));
  }

let small_int =
  {
    pp = Some pp_int;
    run =
      (fun state ->
        let sign, state = Seed.below ~bound:2L state in
        let magnitude, state = sample_nat state in
        let value = if Int64.equal sign 1L then -magnitude else magnitude in
        (tree_towards pp_int (int_towards 0) value, state));
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
        (tree_towards pp_int (int_towards origin) value, state));
  }

let int32 =
  {
    pp = Some pp_int32;
    run =
      (fun state ->
        let word, state = Seed.bits64 state in
        (tree_towards pp_int32 (int32_towards 0l) (Int64.to_int32 word), state));
  }

let int64 =
  {
    pp = Some pp_int64;
    run =
      (fun state ->
        let word, state = Seed.bits64 state in
        (tree_towards pp_int64 (int64_towards 0L) word, state));
  }

let float_any =
  {
    pp = Some pp_float;
    run =
      (fun state ->
        let word, state = Seed.bits64 state in
        let value = Int64.float_of_bits word in
        (tree_towards pp_float (float_towards 0.0) value, state));
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
        (tree_towards pp_float (float_towards 0.0) value, state));
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
        (tree_towards pp_float (float_towards origin) value, state));
  }

(* Booleans, characters, strings *)

let bool =
  let leaf_false = { value = false; trace = draw_of pp_bool false } in
  {
    pp = Some pp_bool;
    run =
      (fun state ->
        let bit, state = Seed.below ~bound:2L state in
        let tree =
          if Int64.equal bit 1L then
            Shrink_tree.make
              ~root:{ value = true; trace = draw_of pp_bool true }
              ~children:(Seq.return (Shrink_tree.leaf leaf_false))
          else Shrink_tree.leaf leaf_false
        in
        (tree, state));
  }

let rec char_tree ~origin code =
  Shrink_tree.make
    ~root:{ value = Char.chr code; trace = draw_of pp_char (Char.chr code) }
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

(* Combines element sample trees into a list sample tree; [structural]
   selects chunked structure shrinking (Shrink_tree.list) or element-wise
   only (seq_list, for explicit size generators whose constraint structural
   candidates would violate). *)
let list_of_trees pp ~structural trees =
  let base = if structural then Shrink_tree.list trees else seq_list trees in
  let combine =
    match pp with
    | Some pp ->
        fun samples ->
          let value = List.map (fun s -> s.value) samples in
          { value; trace = draw_of pp value }
    | None ->
        fun samples ->
          {
            value = List.map (fun s -> s.value) samples;
            trace = Items (List.map (fun s -> s.trace) samples);
          }
  in
  Shrink_tree.map combine base

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
            (list_of_trees pp ~structural:true trees, state));
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
                  if size.value < 0 then invalid_arg "Gen.list: negative size";
                  let trees, _ = sample_elements gen size.value fresh in
                  list_of_trees pp ~structural:false trees)
            in
            (tree, state));
      }

let array ?size gen =
  let base = list ?size gen in
  let pp = Option.map pp_array gen.pp in
  let combine =
    match pp with
    | Some pp ->
        fun s ->
          let value = Array.of_list s.value in
          { value; trace = draw_of pp value }
    | None -> fun s -> { value = Array.of_list s.value; trace = s.trace }
  in
  {
    pp;
    run =
      (fun state ->
        let tree, state = base.run state in
        (Shrink_tree.map combine tree, state));
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
        ( Shrink_tree.map
            (fun s ->
              let value = implode s.value in
              { value; trace = draw_of pp_string value })
            tree,
          state ));
  }

let string = string_of char

let bytes_of ?size char_gen =
  let base = string_of ?size char_gen in
  {
    pp = Some pp_bytes;
    run =
      (fun state ->
        let tree, state = base.run state in
        ( Shrink_tree.map
            (fun s ->
              let value = Bytes.of_string s.value in
              { value; trace = draw_of pp_bytes value })
            tree,
          state ));
  }

let bytes = bytes_of char

let option gen =
  let pp = Option.map pp_option gen.pp in
  let none =
    match pp with
    | Some pp -> { value = None; trace = draw_of pp None }
    | None -> { value = None; trace = Draw (lazy "None") }
  in
  let some =
    match pp with
    | Some pp ->
        fun s ->
          let value = Some s.value in
          { value; trace = draw_of pp value }
    | None -> fun s -> { value = Some s.value; trace = Label ("Some", s.trace) }
  in
  {
    pp;
    run =
      (fun state ->
        let choice, state = Seed.below ~bound:100L state in
        if choice < 15L then (Shrink_tree.leaf none, state)
        else
          let tree, state = gen.run state in
          let rec wrap tree =
            Shrink_tree.make
              ~root:(some (Shrink_tree.root tree))
              ~children:
                (Seq.cons (Shrink_tree.leaf none)
                   (Seq.map wrap (Shrink_tree.children tree)))
          in
          (wrap tree, state));
  }

let result ok err =
  let pp =
    match (ok.pp, err.pp) with
    | Some pp_ok, Some pp_err -> Some (pp_result pp_ok pp_err)
    | _ -> None
  in
  let wrap constructor label s =
    let value = constructor s.value in
    match pp with
    | Some pp -> { value; trace = draw_of pp value }
    | None -> { value; trace = Label (label, s.trace) }
  in
  {
    pp;
    run =
      (fun state ->
        let choice, state = Seed.below ~bound:100L state in
        if choice < 25L then
          let tree, state = err.run state in
          (Shrink_tree.map (wrap (fun e -> Error e) "Error") tree, state)
        else
          let tree, state = ok.run state in
          (Shrink_tree.map (wrap (fun v -> Ok v) "Ok") tree, state));
  }

let pair left right =
  let pp =
    match (left.pp, right.pp) with
    | Some pp_a, Some pp_b -> Some (pp_pair pp_a pp_b)
    | _ -> None
  in
  let combine =
    match pp with
    | Some pp ->
        fun (a, b) ->
          let value = (a.value, b.value) in
          { value; trace = draw_of pp value }
    | None ->
        fun (a, b) ->
          { value = (a.value, b.value); trace = Tuple [ a.trace; b.trace ] }
  in
  {
    pp;
    run =
      (fun state ->
        let left_tree, state = left.run state in
        let right_tree, state = right.run state in
        (Shrink_tree.map combine (Shrink_tree.pair left_tree right_tree), state));
  }

let triple a b c =
  let pp =
    match (a.pp, b.pp, c.pp) with
    | Some pp_a, Some pp_b, Some pp_c -> Some (pp_triple pp_a pp_b pp_c)
    | _ -> None
  in
  let combine =
    match pp with
    | Some pp ->
        fun (sa, (sb, sc)) ->
          let value = (sa.value, sb.value, sc.value) in
          { value; trace = draw_of pp value }
    | None ->
        fun (sa, (sb, sc)) ->
          {
            value = (sa.value, sb.value, sc.value);
            trace = Tuple [ sa.trace; sb.trace; sc.trace ];
          }
  in
  {
    pp;
    run =
      (fun state ->
        let ta, state = a.run state in
        let tb, state = b.run state in
        let tc, state = c.run state in
        ( Shrink_tree.map combine (Shrink_tree.pair ta (Shrink_tree.pair tb tc)),
          state ));
  }

let quad a b c d =
  let pp =
    match (a.pp, b.pp, c.pp, d.pp) with
    | Some pp_a, Some pp_b, Some pp_c, Some pp_d ->
        Some (pp_quad pp_a pp_b pp_c pp_d)
    | _ -> None
  in
  let combine =
    match pp with
    | Some pp ->
        fun (sa, (sb, (sc, sd))) ->
          let value = (sa.value, sb.value, sc.value, sd.value) in
          { value; trace = draw_of pp value }
    | None ->
        fun (sa, (sb, (sc, sd))) ->
          {
            value = (sa.value, sb.value, sc.value, sd.value);
            trace = Tuple [ sa.trace; sb.trace; sc.trace; sd.trace ];
          }
  in
  {
    pp;
    run =
      (fun state ->
        let ta, state = a.run state in
        let tb, state = b.run state in
        let tc, state = c.run state in
        let td, state = d.run state in
        ( Shrink_tree.map combine
            (Shrink_tree.pair ta (Shrink_tree.pair tb (Shrink_tree.pair tc td))),
          state ));
  }

(* Choice and structure *)

let constant value =
  {
    pp = None;
    run = (fun state -> (Shrink_tree.leaf { value; trace = Empty }, state));
  }

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
        let choose index =
          { value = values.(index); trace = label_trace "of_list" index Empty }
        in
        let index_tree = plain_towards (int_towards 0) (Int64.to_int index) in
        (Shrink_tree.map choose index_tree, state));
  }

(* A choice combinator whose branches all print derives its printer (the
   doc law: a composite prints exactly when all its components print).
   Branches generate the same type, so their printers are expected to
   agree; the first branch's is used, and [renormalize] keeps direct
   rendering and provenance inside enclosing printerless generators
   consistent with it. *)
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
        let index_tree = plain_towards (int_towards 0) index in
        let tree =
          rebind index_tree (fun branch ->
              let inner, _ = gens.(branch).run fresh in
              Shrink_tree.map
                (fun s ->
                  { s with trace = label_trace "one_of" branch s.trace })
                inner)
        in
        let tree =
          match pp with Some pp -> renormalize pp tree | None -> tree
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
          if pick < consumed + weight then (index, gen)
          else choose (index + 1) (consumed + weight)
        in
        let index, gen = choose 0 0 in
        let tree, state = gen.run state in
        let tree =
          Shrink_tree.map
            (fun s -> { s with trace = label_trace "frequency" index s.trace })
            tree
        in
        let tree =
          match pp with Some pp -> renormalize pp tree | None -> tree
        in
        (tree, state));
  }

(* Composition *)

let map f gen =
  {
    pp = None;
    run =
      (fun state ->
        let tree, state = gen.run state in
        ( Shrink_tree.map (fun s -> { value = f s.value; trace = s.trace }) tree,
          state ));
  }

let bind gen f =
  {
    pp = None;
    run =
      (fun state ->
        let outer, state = gen.run state in
        let fresh, state = Seed.split state in
        let tree =
          rebind outer (fun s ->
              let inner, _ = (f s.value).run fresh in
              Shrink_tree.map
                (fun inner_sample ->
                  {
                    value = inner_sample.value;
                    trace = Chain (s.trace, inner_sample.trace);
                  })
                inner)
        in
        (tree, state));
  }

let sized f = bind nat f

let rec filter_tree keep tree =
  Shrink_tree.make ~root:(Shrink_tree.root tree)
    ~children:
      (Seq.filter_map
         (fun child ->
           if keep (Shrink_tree.root child).value then
             Some (filter_tree keep child)
           else None)
         (Shrink_tree.children tree))

let such_that ?(max_tries = 100) keep gen =
  {
    pp = gen.pp;
    run =
      (fun state ->
        if max_tries < 1 then invalid_arg "Gen.such_that: max_tries < 1";
        let rec attempt tries state =
          if tries = 0 then raise Rejected
          else
            let fresh, state = Seed.split state in
            let tree, _ = gen.run fresh in
            if keep (Shrink_tree.root tree).value then
              (filter_tree keep tree, state)
            else attempt (tries - 1) state
        in
        attempt max_tries state);
  }

let with_pp pp gen =
  {
    pp = Some pp;
    run =
      (fun state ->
        let tree, state = gen.run state in
        (renormalize pp tree, state));
  }

let ( let+ ) gen f = map f gen
let ( and+ ) left right = pair left right
let ( let* ) = bind

(* Engine interface *)

let sample gen state = fst (gen.run state)
let value s = s.value
let no_printer_message = "<no printer — add Gen.with_pp>"

let render gen s =
  match gen.pp with
  | Some pp -> render_with pp s.value
  | None ->
      let body = render_trace s.trace in
      if String.length body = 0 then no_printer_message
      else "<from: " ^ body ^ ">"

let render_value gen v = Option.map (fun pp -> render_with pp v) gen.pp
