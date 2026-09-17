(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC

  Integrated shrinking in the QCheck2/Hedgehog design: generators produce
  rose trees of candidates, so shrinking falls out of composition. The
  distributions (stratified nat, float bit patterns) follow QCheck2's
  tuning (https://github.com/c-cube/qcheck); the implementation is
  windtrap's own, built over Seed (SplitMix64) and the shrink trees below.
  --------------------------------------------------------------------------*)

(* Shrink trees

   Lazy rose trees for integrated shrinking (the QCheck2/Hedgehog design):
   a strict root and a memoized, ordered sequence of candidate subtrees.
   Cells are forced at most once, exceptions included, so a shrink search
   that revisits a branch never re-runs user code. The list shrink removes
   power-of-two chunks before it reduces elements. *)
module Shrink_tree = struct
  type 'a t = { root : 'a; children : 'a t Seq.t }

  let rec memoize sequence =
    let node =
      lazy
        (match sequence () with
        | Seq.Nil -> Seq.Nil
        | Seq.Cons (value, tail) -> Seq.Cons (value, memoize tail))
    in
    fun () -> Lazy.force node

  let make ~root ~children = { root; children = memoize children }
  let leaf root = make ~root ~children:Seq.empty
  let root tree = tree.root
  let children tree = tree.children

  let rec map f tree =
    make ~root:(f tree.root)
      ~children:(Seq.map (fun child -> map f child) tree.children)

  let rec pair left right =
    let left_children = Seq.map (fun child -> pair child right) left.children in
    let right_children =
      Seq.map (fun child -> pair left child) right.children
    in
    make ~root:(left.root, right.root)
      ~children:(Seq.append left_children right_children)

  let roots trees =
    let values = ref [] in
    for index = Array.length trees - 1 downto 0 do
      values := trees.(index).root :: !values
    done;
    !values

  let without_range trees ~first ~length =
    let source_length = Array.length trees in
    let candidate = Array.make (source_length - length) trees.(0) in
    for index = 0 to first - 1 do
      candidate.(index) <- trees.(index)
    done;
    for index = first + length to source_length - 1 do
      candidate.(index - length) <- trees.(index)
    done;
    candidate

  let copy_with trees index value =
    let candidate = Array.copy trees in
    candidate.(index) <- value;
    candidate

  let largest_power_of_two_below length =
    let rec loop power =
      if power > (length - 1) / 2 then power else loop (power * 2)
    in
    if length <= 1 then 0 else loop 1

  let rec list_array trees =
    let length = Array.length trees in
    let rec chunk_candidates chunk first () =
      if chunk = 0 then Seq.Nil
      else if first + chunk <= length then
        let candidate = without_range trees ~first ~length:chunk in
        Seq.Cons (list_array candidate, chunk_candidates chunk (first + chunk))
      else chunk_candidates (chunk / 2) 0 ()
    in
    let rec element_candidates index candidates () =
      match candidates () with
      | Seq.Cons (candidate, tail) ->
          let trees = copy_with trees index candidate in
          Seq.Cons (list_array trees, element_candidates index tail)
      | Seq.Nil ->
          let next = index + 1 in
          if next = length then Seq.Nil
          else element_candidates next trees.(next).children ()
    in
    let structural =
      if length = 0 then Seq.empty
      else
        Seq.cons (leaf [])
          (chunk_candidates (largest_power_of_two_below length) 0)
    in
    let element =
      if length = 0 then Seq.empty else element_candidates 0 trees.(0).children
    in
    make ~root:(roots trees) ~children:(Seq.append structural element)

  let list trees = list_array (Array.of_list trees)
end

(* Renderings

   A shrink tree carries, at every node, the value and how that value
   prints as a counterexample — the two are drawn together and shrink
   together, so what the search minimises and what the report prints
   coincide. A rendering is a lazy document: nothing is formatted until a
   failure is reported. [Value] renders the value itself, through the
   printer of the generator that drew it. [Pre_image] renders what the
   value was computed from, when the generator has no printer of its own —
   a [map]'s argument, a [bind]'s draws — down to the nearest generator
   that prints. [None] is a leaf with nothing to print ([constant],
   [of_list]) and every composition over it; it renders as the one
   placeholder below. The same classification, over the formatted text, is
   what the engine receives. *)

type 'a rendering = Value of 'a | Pre_image of 'a

(* [Arrow] is a [bind]'s pre-image, [outer -> inner]; it is a constructor
   rather than a laid-out atom so an outer that is itself a bind's
   pre-image can be parenthesised. *)
type doc = Atom of (Format.formatter -> unit) | Arrow of doc * doc
type 'a node = { value : 'a; shown : doc rendering option }

(* The one placeholder for a value with no printer, built here and nowhere
   else: a counterexample, an [~examples] value and a stateful argument all
   spell it, and it carries its own remedy. *)
let no_printer = "<no printer: attach one with Gen.with_pp>"

type 'a t = {
  pp : (Format.formatter -> 'a -> unit) option;
  run : Seed.state -> 'a node Shrink_tree.t * Seed.state;
}

exception Rejected

(* Printers *)

let render_with pp value =
  try Format.asprintf "%a" pp value
  with exn -> Printf.sprintf "<printer raised %s>" (Printexc.to_string exn)

let pp_int = Format.pp_print_int
let pp_int32 ppf n = Format.fprintf ppf "%ldl" n
let pp_int64 ppf n = Format.fprintf ppf "%LdL" n
let pp_nativeint ppf n = Format.fprintf ppf "%ndn" n

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
let pp_comma ppf () = Format.fprintf ppf ",@ "

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

let pp_either pp_left pp_right ppf = function
  | Either.Left v -> Format.fprintf ppf "@[<hov 2>Left@ (%a)@]" pp_left v
  | Either.Right v -> Format.fprintf ppf "@[<hov 2>Right@ (%a)@]" pp_right v

let pp_pair pp_a pp_b ppf (a, b) =
  Format.fprintf ppf "@[<hov 1>(%a,@ %a)@]" pp_a a pp_b b

let pp_triple pp_a pp_b pp_c ppf (a, b, c) =
  Format.fprintf ppf "@[<hov 1>(%a,@ %a,@ %a)@]" pp_a a pp_b b pp_c c

let pp_quad pp_a pp_b pp_c pp_d ppf (a, b, c, d) =
  Format.fprintf ppf "@[<hov 1>(%a,@ %a,@ %a,@ %a)@]" pp_a a pp_b b pp_c c pp_d
    d

(* Documents: the printers above, over renderings instead of values. A
   tuple of documents lays out exactly as [pp_pair]/[pp_triple]/[pp_quad]
   lay out a tuple of values, so a pre-image reads like the value would. *)

let rec pp_doc ppf = function
  | Atom print -> print ppf
  | Arrow (outer, inner) ->
      Format.fprintf ppf "@[<hov 2>%a ->@ %a@]" pp_operand outer pp_doc inner

and pp_operand ppf = function
  | Arrow _ as doc -> Format.fprintf ppf "(%a)" pp_doc doc
  | doc -> pp_doc ppf doc

let pp_tuple docs ppf =
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Format.pp_print_list ~pp_sep:pp_comma pp_doc)
    docs

(* Nodes *)

let value node = node.value

let printed pp value =
  { value; shown = Some (Value (Atom (fun ppf -> pp ppf value))) }

let unprintable value = { value; shown = None }

(* The law, per node: a composite renders exactly when every part renders,
   and as the value exactly when every part is one. [layout] receives the
   parts' documents in order. *)
let composite layout parts =
  let rec gather pre_image docs = function
    | [] ->
        let doc = Atom (layout (List.rev docs)) in
        Some (if pre_image then Pre_image doc else Value doc)
    | None :: _ -> None
    | Some (Value doc) :: rest -> gather pre_image (doc :: docs) rest
    | Some (Pre_image doc) :: rest -> gather true (doc :: docs) rest
  in
  gather false [] parts

let wrap layout =
  Option.map (function
    | Value doc -> Value (Atom (layout doc))
    | Pre_image doc -> Pre_image (Atom (layout doc)))

let tuple_node values parts =
  { value = values; shown = composite pp_tuple parts }

let list_node nodes =
  {
    value = List.map value nodes;
    shown =
      composite
        (fun docs ppf -> pp_list pp_doc ppf docs)
        (List.map (fun node -> node.shown) nodes);
  }

(* A [map]'s result renders as its argument: the value the mapping function
   received, marked as such. Nothing changes for an argument that is itself
   a pre-image or has nothing to print. *)
let pre_image =
  Option.map (function Value doc -> Pre_image doc | shown -> shown)

(* A [bind]'s result is the inner value when the inner generator prints —
   that is the value, so the outer draw adds nothing — and [outer -> inner]
   when the inner renders as a pre-image, each side by its own rule. *)
let bound outer inner =
  match inner with
  | None | Some (Value _) -> inner
  | Some (Pre_image doc) ->
      Option.map
        (fun (Value outer | Pre_image outer) -> Pre_image (Arrow (outer, doc)))
        outer

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

let nativeint_towards dest x () =
  let rec steps current () =
    if Nativeint.equal current x then Seq.Nil
    else
      let gap = Nativeint.sub (Nativeint.div x 2n) (Nativeint.div current 2n) in
      if Nativeint.equal gap 0n then Seq.Cons (current, Seq.empty)
      else Seq.Cons (current, steps (Nativeint.add current gap))
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

(* [node] makes each candidate's tree node: a printed node for a primitive,
   the bare index for the choice combinators' index trees. *)
let rec tree_towards node shrink x =
  Shrink_tree.make ~root:(node x)
    ~children:(Seq.map (tree_towards node shrink) (shrink x))

(* [rebind] re-generates candidates for [bind], [one_of] and sized
   [list]: [f] runs a generator, so forcing a shrink candidate can raise
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
        ( tree_towards (printed pp_int) (int_towards 0) (Int64.to_int word),
          state ));
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
        (tree_towards (printed pp_int) (int_towards 0) value, state));
  }

let small_int =
  {
    pp = Some pp_int;
    run =
      (fun state ->
        let sign, state = Seed.below ~bound:2L state in
        let magnitude, state = sample_nat state in
        let value = if Int64.equal sign 1L then -magnitude else magnitude in
        (tree_towards (printed pp_int) (int_towards 0) value, state));
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
        (tree_towards (printed pp_int) (int_towards origin) value, state));
  }

let int32 =
  {
    pp = Some pp_int32;
    run =
      (fun state ->
        let word, state = Seed.bits64 state in
        ( tree_towards (printed pp_int32) (int32_towards 0l)
            (Int64.to_int32 word),
          state ));
  }

let int64 =
  {
    pp = Some pp_int64;
    run =
      (fun state ->
        let word, state = Seed.bits64 state in
        (tree_towards (printed pp_int64) (int64_towards 0L) word, state));
  }

let nativeint =
  {
    pp = Some pp_nativeint;
    run =
      (fun state ->
        let word, state = Seed.bits64 state in
        ( tree_towards (printed pp_nativeint) (nativeint_towards 0n)
            (Int64.to_nativeint word),
          state ));
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
        (tree_towards (printed pp_float) (float_towards 0.0) value, state));
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
        (tree_towards (printed pp_float) (float_towards origin) value, state));
  }

(* Unit, booleans, characters, strings *)

(* [constant ()] would print nothing, and a printerless leaf forfeits the
   derived printer of every composition above it, the one cost a leaf of a
   type with exactly one value never has to impose. One value means one
   leaf, built like [bool]'s, and drawing it consumes no randomness, so the
   state passes through. *)
let unit =
  {
    pp = Some pp_unit;
    run = (fun state -> (Shrink_tree.leaf (printed pp_unit ()), state));
  }

let bool =
  {
    pp = Some pp_bool;
    run =
      (fun state ->
        let bit, state = Seed.below ~bound:2L state in
        let tree =
          if Int64.equal bit 1L then
            Shrink_tree.make ~root:(printed pp_bool true)
              ~children:(Seq.return (Shrink_tree.leaf (printed pp_bool false)))
          else Shrink_tree.leaf (printed pp_bool false)
        in
        (tree, state));
  }

let rec char_tree ~origin code =
  Shrink_tree.make
    ~root:(printed pp_char (Char.chr code))
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

(* The tree of element nodes under [list ?size]: [list], [array] and
   [string_of] each assemble their own node from it. *)
let element_trees ?size gen state =
  match size with
  | None ->
      let length, state = sample_nat state in
      let trees, state = sample_elements gen length state in
      (Shrink_tree.list trees, state)
  | Some size_gen ->
      let size_tree, state = size_gen.run state in
      let fresh, state = Seed.split state in
      let tree =
        rebind size_tree (fun size ->
            if size.value < 0 then invalid_arg "Gen.list: negative size";
            let trees, _ = sample_elements gen size.value fresh in
            seq_list trees)
      in
      (tree, state)

let list ?size gen =
  {
    pp = Option.map pp_list gen.pp;
    run =
      (fun state ->
        let tree, state = element_trees ?size gen state in
        (Shrink_tree.map list_node tree, state));
  }

let array ?size gen =
  let array_node nodes =
    {
      value = Array.of_list (List.map value nodes);
      shown =
        composite
          (fun docs ppf -> pp_array pp_doc ppf (Array.of_list docs))
          (List.map (fun node -> node.shown) nodes);
    }
  in
  {
    pp = Option.map pp_array gen.pp;
    run =
      (fun state ->
        let tree, state = element_trees ?size gen state in
        (Shrink_tree.map array_node tree, state));
  }

let string_of ?size char_gen =
  (* Length and shrink order follow [list ?size]: with the default (nat)
     size the empty string is the first candidate, then chunk removals,
     then characters; an explicit size constrains every candidate. *)
  let implode nodes =
    let buffer = Bytes.create (List.length nodes) in
    List.iteri (fun index node -> Bytes.set buffer index node.value) nodes;
    printed pp_string (Bytes.unsafe_to_string buffer)
  in
  {
    pp = Some pp_string;
    run =
      (fun state ->
        let tree, state = element_trees ?size char_gen state in
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
        ( Shrink_tree.map
            (fun node -> printed pp_bytes (Bytes.of_string node.value))
            tree,
          state ));
  }

let bytes = bytes_of char

let option gen =
  let pp = Option.map pp_option gen.pp in
  let none =
    Shrink_tree.leaf
      {
        value = None;
        shown = Some (Value (Atom (fun ppf -> pp_option pp_doc ppf None)));
      }
  in
  let some node =
    {
      value = Some node.value;
      shown = wrap (fun doc ppf -> pp_option pp_doc ppf (Some doc)) node.shown;
    }
  in
  let rec wrap_tree tree =
    Shrink_tree.make
      ~root:(some (Shrink_tree.root tree))
      ~children:(Seq.cons none (Seq.map wrap_tree (Shrink_tree.children tree)))
  in
  {
    pp;
    run =
      (fun state ->
        let choice, state = Seed.below ~bound:100L state in
        if choice < 15L then (none, state)
        else
          let tree, state = gen.run state in
          (wrap_tree tree, state));
  }

let result ok err =
  let pp =
    match (ok.pp, err.pp) with
    | Some pp_ok, Some pp_err -> Some (pp_result pp_ok pp_err)
    | _ -> None
  in
  let ok_node node =
    {
      value = Ok node.value;
      shown =
        wrap (fun doc ppf -> pp_result pp_doc pp_doc ppf (Ok doc)) node.shown;
    }
  in
  let error_node node =
    {
      value = Error node.value;
      shown =
        wrap (fun doc ppf -> pp_result pp_doc pp_doc ppf (Error doc)) node.shown;
    }
  in
  {
    pp;
    run =
      (fun state ->
        let choice, state = Seed.below ~bound:100L state in
        if choice < 25L then
          let tree, state = err.run state in
          (Shrink_tree.map error_node tree, state)
        else
          let tree, state = ok.run state in
          (Shrink_tree.map ok_node tree, state));
  }

let either left right =
  let pp =
    match (left.pp, right.pp) with
    | Some pp_left, Some pp_right -> Some (pp_either pp_left pp_right)
    | _ -> None
  in
  let left_node node =
    {
      value = Either.Left node.value;
      shown =
        wrap
          (fun doc ppf -> pp_either pp_doc pp_doc ppf (Either.Left doc))
          node.shown;
    }
  in
  let right_node node =
    {
      value = Either.Right node.value;
      shown =
        wrap
          (fun doc ppf -> pp_either pp_doc pp_doc ppf (Either.Right doc))
          node.shown;
    }
  in
  {
    pp;
    run =
      (fun state ->
        let side, state = Seed.below ~bound:2L state in
        if Int64.equal side 0L then
          let tree, state = left.run state in
          (Shrink_tree.map left_node tree, state)
        else
          let tree, state = right.run state in
          (Shrink_tree.map right_node tree, state));
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
        let tree =
          Shrink_tree.map
            (fun (a, b) -> tuple_node (a.value, b.value) [ a.shown; b.shown ])
            (Shrink_tree.pair left_tree right_tree)
        in
        (tree, state));
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
            (fun (a, (b, c)) ->
              tuple_node
                (a.value, b.value, c.value)
                [ a.shown; b.shown; c.shown ])
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
            (fun (a, (b, (c, d))) ->
              tuple_node
                (a.value, b.value, c.value, d.value)
                [ a.shown; b.shown; c.shown; d.shown ])
            (Shrink_tree.pair ta (Shrink_tree.pair tb (Shrink_tree.pair tc td)))
        in
        (tree, state));
  }

(* Choice and structure *)

(* The printerless leaves: their values are arbitrary, so no printer can be
   inferred and no pre-image exists to fall back on. [with_pp] attaches one,
   and a deriving combinator built over the result keeps it. *)
let constant value =
  {
    pp = None;
    run = (fun state -> (Shrink_tree.leaf (unprintable value), state));
  }

let of_list values =
  let values = Array.of_list values in
  {
    pp = None;
    run =
      (fun state ->
        let count = Array.length values in
        if count = 0 then invalid_arg "Gen.of_list: empty list";
        let index, state = Seed.below ~bound:(Int64.of_int count) state in
        let index_tree =
          tree_towards Fun.id (int_towards 0) (Int64.to_int index)
        in
        ( Shrink_tree.map (fun index -> unprintable values.(index)) index_tree,
          state ));
  }

(* A choice combinator whose branches all print derives its printer (the
   doc law: a composite prints exactly when all its components print).
   Branches generate the same type, so their printers are expected to
   agree; the first branch's prints a bare value, and a counterexample
   prints with the branch that drew it. *)
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
        let index_tree = tree_towards Fun.id (int_towards 0) index in
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
        ( Shrink_tree.map
            (fun node -> { value = f node.value; shown = pre_image node.shown })
            tree,
          state ));
  }

let bind gen f =
  {
    pp = None;
    run =
      (fun state ->
        let outer, state = gen.run state in
        let fresh, state = Seed.split state in
        let inner outer =
          Shrink_tree.map
            (fun node -> { node with shown = bound outer.shown node.shown })
            (fst ((f outer.value).run fresh))
        in
        (rebind outer inner, state));
  }

let rec filter_tree keep tree =
  Shrink_tree.make ~root:(Shrink_tree.root tree)
    ~children:
      (Seq.filter_map
         (fun child ->
           if keep (Shrink_tree.root child).value then
             Some (filter_tree keep child)
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
            if keep (Shrink_tree.root tree).value then
              (filter_tree keep tree, state)
            else attempt (tries - 1) state
        in
        attempt resample_budget state);
  }

(* An explicit printer wins over whatever the tree would have rendered —
   a pre-image, a derived printer, nothing — at every node. *)
let with_pp pp gen =
  {
    pp = Some pp;
    run =
      (fun state ->
        let tree, state = gen.run state in
        (Shrink_tree.map (fun node -> printed pp node.value) tree, state));
  }

let ( let+ ) gen f = map f gen
let ( and+ ) left right = pair left right
let ( let* ) = bind

(* Engine interface

   The property engine, Stateful, and this library's own tests reach these;
   nothing else does, which is why they are behind [Engine] rather than in
   the vocabulary above. [Rejected] stays at the top of the file because
   [such_that] raises it and [rebind] catches it. *)
module Engine = struct
  module Shrink_tree = Shrink_tree

  exception Rejected = Rejected

  type 'a sample = 'a node

  let sample gen state = fst (gen.run state)
  let value = value

  type nonrec 'a rendering = 'a rendering = Value of 'a | Pre_image of 'a

  (* Formatting happens here and nowhere earlier: a rendering is a lazy
     document until the engine reports the counterexample. *)
  let render node =
    match node.shown with
    | None -> Value no_printer
    | Some (Value doc) -> Value (render_with pp_doc doc)
    | Some (Pre_image doc) -> Pre_image (render_with pp_doc doc)

  let render_value gen v =
    match gen.pp with Some pp -> render_with pp v | None -> no_printer

  (* The representation, minus the renderings: a generator is a printer and
     a draw of a value tree. [run] forgets each node's rendering and [make]
     rebuilds one from the printer, so a caller assembling its own tree
     ([Stateful]'s repaired program) prints through [?pp] alone. *)
  let run gen state =
    let tree, state = gen.run state in
    (Shrink_tree.map value tree, state)

  let make ?pp draw =
    let node = match pp with Some pp -> printed pp | None -> unprintable in
    {
      pp;
      run =
        (fun state ->
          let tree, state = draw state in
          (Shrink_tree.map node tree, state));
    }
end
