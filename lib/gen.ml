(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(* The distributions (stratified nat, float bit patterns) follow QCheck2's
   tuning (https://github.com/c-cube/qcheck). *)

(* Shrink trees *)

(* A strict root and a memoized sequence of candidate subtrees. A cell is
   forced at most once, an exception included, so a search that visits a
   branch again never runs user code again. *)
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

  (* [f] generates again at every candidate of [tree]; those come first,
     then the candidates of the tree [f] drew. *)
  let rec bind tree f =
    let bound = f tree.root in
    make ~root:bound.root
      ~children:
        (Seq.append
           (Seq.filter_map
              (unless_discarded (fun child -> bind child f))
              tree.children)
           bound.children)

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
   the two are drawn and shrunk together, so what a search minimises is what
   the report prints. [Value] prints the value through its generator's
   printer; [Pre_image] prints what a printerless [map] or [bind] computed
   it from; [None] has nothing to print. A document is formatted only when a
   failure is reported. *)
type 'a rendering = Value of 'a | Pre_image of 'a

(* [Arrow] is a [bind]'s pre-image, [outer -> inner], so an outer that is
   itself an arrow can be parenthesised. *)
type doc = Atom of (Format.formatter -> unit) | Arrow of doc * doc
type 'a node = { value : 'a; shown : doc rendering option }

type 'a t = {
  pp : (Format.formatter -> 'a -> unit) option;
  run : Seed.state -> 'a node Shrink_tree.t * Seed.state;
}

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
let printed pp value = { value; shown = Some (Value (atom pp value)) }
let unprintable value = { value; shown = None }

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

let tuple value parts = { value; shown = composite pp_tuple parts }

let constructor name inject node =
  let apply doc = atom (pp_constructor name pp_doc) doc in
  {
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

(* Draws *)

let map_draw f draw state =
  let tree, state = draw state in
  (Shrink_tree.map f tree, state)

let word of_bits state =
  let bits, state = Seed.bits64 state in
  (of_bits bits, state)

module type Number = sig
  type t

  val zero : t
  val one : t
  val add : t -> t -> t
  val sub : t -> t -> t
  val div : t -> t -> t
  val equal : t -> t -> bool
end

(* The candidates of [x] with origin [origin]: [origin] first, then each
   closes half the remaining gap, short of [x]. The gap is a difference of
   halves, since [(x - current) / 2] overflows across the whole range. *)
let towards (type n) (module N : Number with type t = n) origin x =
  let two = N.add N.one N.one in
  let rec steps current () =
    if N.equal current x then Seq.Nil
    else
      let gap = N.sub (N.div x two) (N.div current two) in
      if N.equal gap N.zero then Seq.Cons (current, Seq.empty)
      else Seq.Cons (current, steps (N.add current gap))
  in
  steps origin

let int_towards origin = towards (module Int) origin

(* Halving floats yields values without end short of [x]. *)
let float_towards origin x = Seq.take 15 (towards (module Float) origin x)

let rec tree_towards node shrink x =
  Shrink_tree.make ~root:(node x)
    ~children:(Seq.map (tree_towards node shrink) (shrink x))

let primitive pp shrink draw =
  {
    pp = Some pp;
    run =
      (fun state ->
        let x, state = draw state in
        (tree_towards (printed pp) shrink x, state));
  }

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

let int_range low high =
  let low64 = Int64.of_int low in
  let span = Int64.sub (Int64.of_int high) low64 in
  let origin = Int.max low (Int.min high 0) in
  (* The neighbours stay inside the range, where they cannot overflow. *)
  let corners =
    List.sort_uniq Int.compare
      ([ low; high; origin ]
      @ (if origin > low then [ origin - 1 ] else [])
      @ if origin < high then [ origin + 1 ] else [])
  in
  let uniform state =
    (* The whole [int] range counts one value more than [Int64.max_int]. *)
    if Int64.equal span Int64.max_int then word Int64.to_int state
    else
      let offset, state = Seed.below ~bound:(Int64.succ span) state in
      (Int64.to_int (Int64.add low64 offset), state)
  in
  primitive Pp.int (int_towards origin) (fun state ->
      if high < low then invalid_arg "Gen.int_range: high < low";
      with_corners corners uniform state)

let int = int_range min_int max_int

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
let nat = primitive Pp.int (int_towards 0) draw_nat

let small_int =
  primitive Pp.int (int_towards 0) (fun state ->
      let sign, state = Seed.below ~bound:2L state in
      let magnitude, state = draw_nat state in
      ((if Int64.equal sign 1L then -magnitude else magnitude), state))

let int32 =
  primitive pp_int32
    (towards (module Int32) 0l)
    (with_corners
       [ Int32.min_int; -1l; 0l; 1l; Int32.max_int ]
       (word Int64.to_int32))

let int64 =
  primitive pp_int64
    (towards (module Int64) 0L)
    (with_corners [ Int64.min_int; -1L; 0L; 1L; Int64.max_int ] (word Fun.id))

let nativeint =
  primitive pp_nativeint
    (towards (module Nativeint) 0n)
    (with_corners
       [ Nativeint.min_int; -1n; 0n; 1n; Nativeint.max_int ]
       (word Int64.to_nativeint))

(* Rejection keeps the bit-pattern distribution; about 0.05% of the patterns
   are not finite. *)
let float =
  let rec finite state =
    let value, state = word Int64.float_of_bits state in
    if Float.is_finite value then (value, state) else finite state
  in
  primitive Pp.float_exact (float_towards 0.0) finite

let float_range low high =
  let origin = if low > 0.0 then low else if high < 0.0 then high else 0.0 in
  primitive Pp.float_exact (float_towards origin) (fun state ->
      if not (Float.is_finite low && Float.is_finite high) then
        invalid_arg "Gen.float_range: bounds must be finite";
      if high < low then invalid_arg "Gen.float_range: high < low";
      if high -. low > Float.max_float then
        invalid_arg "Gen.float_range: high -. low > max_float";
      let unit_interval, state =
        word
          (fun bits ->
            Int64.to_float (Int64.shift_right_logical bits 11) *. 0x1p-53)
          state
      in
      (* [high -. low] can round up past the span, and the sum then past
         [high]; it never falls below [low], whose addend is non-negative. *)
      let value = low +. (unit_interval *. (high -. low)) in
      ((if value > high then high else value), state))

(* Unit, booleans, characters and strings *)

(* Unlike [constant ()], [unit] prints, so a composition over it keeps its
   printer. *)
let unit =
  {
    pp = Some pp_unit;
    run = (fun state -> (Shrink_tree.leaf (printed pp_unit ()), state));
  }

let bool =
  primitive Pp.bool
    (fun b -> if b then Seq.return false else Seq.empty)
    (fun state ->
      let bit, state = Seed.below ~bound:2L state in
      (Int64.equal bit 1L, state))

let char_range low high =
  let origin =
    Int.max (Char.code low) (Int.min (Char.code high) (Char.code 'a'))
  in
  primitive pp_char
    (fun c -> Seq.map Char.chr (int_towards origin (Char.code c)))
    (fun state ->
      if high < low then invalid_arg "Gen.char_range: high < low";
      let count = Char.code high - Char.code low + 1 in
      let offset, state = Seed.below ~bound:(Int64.of_int count) state in
      (Char.chr (Char.code low + Int64.to_int offset), state))

let char = char_range '\000' '\255'

(* The element trees of [list ?size gen], which [list], [array] and
   [string_of] each assemble into their own node. *)
let elements ?size gen state =
  let rec draw count trees state =
    if count = 0 then (List.rev trees, state)
    else
      let tree, state = gen.run state in
      draw (count - 1) (tree :: trees) state
  in
  match size with
  | None ->
      let length, state = draw_length state in
      let trees, state = draw length [] state in
      (Shrink_tree.list trees, state)
  | Some size ->
      (* Under a given length, only the elements shrink. *)
      let rec in_place = function
        | [] -> Shrink_tree.leaf []
        | tree :: trees ->
            Shrink_tree.map
              (fun (v, vs) -> v :: vs)
              (Shrink_tree.pair tree (in_place trees))
      in
      let size_tree, state = size.run state in
      let fresh, state = Seed.split state in
      let tree =
        Shrink_tree.bind size_tree (fun length ->
            if length.value < 0 then invalid_arg "Gen.list: negative size";
            in_place (fst (draw length.value [] fresh)))
      in
      (tree, state)

let string_of ?size char =
  let implode nodes =
    let buffer = Bytes.create (List.length nodes) in
    List.iteri (fun i node -> Bytes.set buffer i node.value) nodes;
    printed pp_string (Bytes.unsafe_to_string buffer)
  in
  { pp = Some pp_string; run = map_draw implode (elements ?size char) }

let string = string_of char

let bytes_of ?size char =
  {
    pp = Some pp_bytes;
    run =
      map_draw
        (fun node -> printed pp_bytes (Bytes.of_string node.value))
        (string_of ?size char).run;
  }

let bytes = bytes_of char

(* Containers *)

let list ?size gen =
  let node nodes =
    {
      value = List.map value nodes;
      shown = composite (pp_list pp_doc) (List.map (fun n -> n.shown) nodes);
    }
  in
  { pp = Option.map pp_list gen.pp; run = map_draw node (elements ?size gen) }

let array ?size gen =
  let node nodes =
    {
      value = Array.of_list (List.map value nodes);
      shown =
        composite
          (fun ppf docs -> pp_array pp_doc ppf (Array.of_list docs))
          (List.map (fun n -> n.shown) nodes);
    }
  in
  { pp = Option.map pp_array gen.pp; run = map_draw node (elements ?size gen) }

let option gen =
  let none =
    Shrink_tree.leaf
      { value = None; shown = Some (Value (atom Pp.string "None")) }
  in
  let rec some tree =
    Shrink_tree.make
      ~root:(constructor "Some" Option.some (Shrink_tree.root tree))
      ~children:(Seq.cons none (Seq.map some (Shrink_tree.children tree)))
  in
  {
    pp = Option.map pp_option gen.pp;
    run =
      (fun state ->
        let choice, state = Seed.below ~bound:100L state in
        if choice < 15L then (none, state)
        else
          let tree, state = gen.run state in
          (some tree, state));
  }

let result ok error =
  {
    pp =
      (match (ok.pp, error.pp) with
      | Some pp_ok, Some pp_error -> Some (pp_result pp_ok pp_error)
      | _ -> None);
    run =
      (fun state ->
        let choice, state = Seed.below ~bound:100L state in
        if choice < 25L then
          map_draw (constructor "Error" Result.error) error.run state
        else map_draw (constructor "Ok" Result.ok) ok.run state);
  }

let either left right =
  {
    pp =
      (match (left.pp, right.pp) with
      | Some pp_left, Some pp_right -> Some (pp_either pp_left pp_right)
      | _ -> None);
    run =
      (fun state ->
        let side, state = Seed.below ~bound:2L state in
        if Int64.equal side 0L then
          map_draw (constructor "Left" Either.left) left.run state
        else map_draw (constructor "Right" Either.right) right.run state);
  }

let pair a b =
  {
    pp =
      (match (a.pp, b.pp) with
      | Some pp_a, Some pp_b -> Some (pp_pair pp_a pp_b)
      | _ -> None);
    run =
      (fun state ->
        let ta, state = a.run state in
        let tb, state = b.run state in
        ( Shrink_tree.map
            (fun (a, b) -> tuple (a.value, b.value) [ a.shown; b.shown ])
            (Shrink_tree.pair ta tb),
          state ));
  }

let triple a b c =
  {
    pp =
      (match (a.pp, b.pp, c.pp) with
      | Some pp_a, Some pp_b, Some pp_c -> Some (pp_triple pp_a pp_b pp_c)
      | _ -> None);
    run =
      (fun state ->
        let ta, state = a.run state in
        let tb, state = b.run state in
        let tc, state = c.run state in
        ( Shrink_tree.map
            (fun (a, (b, c)) ->
              tuple (a.value, b.value, c.value) [ a.shown; b.shown; c.shown ])
            (Shrink_tree.pair ta (Shrink_tree.pair tb tc)),
          state ));
  }

let quad a b c d =
  {
    pp =
      (match (a.pp, b.pp, c.pp, d.pp) with
      | Some pp_a, Some pp_b, Some pp_c, Some pp_d ->
          Some (pp_quad pp_a pp_b pp_c pp_d)
      | _ -> None);
    run =
      (fun state ->
        let ta, state = a.run state in
        let tb, state = b.run state in
        let tc, state = c.run state in
        let td, state = d.run state in
        ( Shrink_tree.map
            (fun (a, (b, (c, d))) ->
              tuple
                (a.value, b.value, c.value, d.value)
                [ a.shown; b.shown; c.shown; d.shown ])
            (Shrink_tree.pair ta (Shrink_tree.pair tb (Shrink_tree.pair tc td))),
          state ));
  }

(* Constants, choices and filters *)

(* A listed value renders with [pp] when one is given, as under [with_pp]. *)
let listed ?pp value =
  match pp with None -> unprintable value | Some pp -> printed pp value

let constant ?pp value =
  { pp; run = (fun state -> (Shrink_tree.leaf (listed ?pp value), state)) }

let of_list ?pp values =
  let values = Array.of_list values in
  let count = Array.length values in
  {
    pp;
    run =
      (fun state ->
        if count = 0 then invalid_arg "Gen.of_list: empty list";
        let index, state = Seed.below ~bound:(Int64.of_int count) state in
        ( tree_towards
            (fun index -> listed ?pp values.(index))
            (int_towards 0) (Int64.to_int index),
          state ));
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
    run =
      (fun state ->
        if count = 0 then invalid_arg "Gen.one_of: empty list";
        let index, state = Seed.below ~bound:(Int64.of_int count) state in
        let fresh, state = Seed.split state in
        let branches =
          tree_towards Fun.id (int_towards 0) (Int64.to_int index)
        in
        ( Shrink_tree.bind branches (fun branch -> fst (gens.(branch).run fresh)),
          state ));
  }

let frequency weighted =
  let pp = branches_pp (List.map snd weighted) in
  let weighted = Array.of_list weighted in
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

(* A predicate that needs more draws is a shape to build by construction. *)
let resample_budget = 100

let such_that keep gen =
  let rec filter tree =
    Shrink_tree.make ~root:(Shrink_tree.root tree)
      ~children:
        (Seq.filter_map
           (fun child ->
             if keep (Shrink_tree.root child).value then Some (filter child)
             else None)
           (Shrink_tree.children tree))
  in
  {
    pp = gen.pp;
    run =
      (fun state ->
        let rec attempt tries state =
          if tries = 0 then raise (Failure.Control `Discard)
          else
            let fresh, state = Seed.split state in
            let tree, _ = gen.run fresh in
            if keep (Shrink_tree.root tree).value then (filter tree, state)
            else attempt (tries - 1) state
        in
        attempt resample_budget state);
  }

(* Composition *)

let map f gen =
  {
    pp = None;
    run =
      map_draw
        (fun node -> { value = f node.value; shown = pre_image node.shown })
        gen.run;
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
        (Shrink_tree.bind outer inner, state));
  }

let with_pp pp gen =
  { pp = Some pp; run = map_draw (fun node -> printed pp node.value) gen.run }

let ( let+ ) gen f = map f gen
let ( and+ ) = pair
let ( let* ) = bind

(* Engine *)

module Engine = struct
  module Shrink_tree = Shrink_tree

  type 'a sample = 'a node

  let draw gen state = gen.run state
  let sample gen state = fst (draw gen state)
  let value = value

  type nonrec 'a rendering = 'a rendering = Value of 'a | Pre_image of 'a

  let no_printer = "<no printer: attach one with Gen.with_pp>"

  (* A printer runs after a case failed: what it raises, a control included,
     becomes the text, so nothing replaces the failure found. *)
  let render_with pp v =
    match Failure.catch (fun () -> Pp.to_string pp v) with
    | Ok text -> text
    | Error c -> Pp.str "<printer raised %s>" (Failure.caught_to_string c)

  let render node =
    match node.shown with
    | None -> Value no_printer
    | Some (Value doc) -> Value (render_with pp_doc doc)
    | Some (Pre_image doc) -> Pre_image (render_with pp_doc doc)

  let prints node = Option.is_some node.shown

  let render_value gen v =
    match gen.pp with Some pp -> render_with pp v | None -> no_printer

  let run gen state = map_draw value gen.run state

  let make ?pp draw =
    let node = match pp with Some pp -> printed pp | None -> unprintable in
    { pp; run = map_draw node draw }
end
