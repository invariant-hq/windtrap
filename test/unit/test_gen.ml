(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(* Tests for Gen: determinism under fixed seeds, distribution smoke tests,
   integrated-shrinking invariants (candidates satisfy generator
   constraints), greedy-shrink termination and minima, printing totality
   including the printerless placeholder, and the shrink tree itself
   ([Gen.Engine.Shrink_tree]) in its own submodule below. *)

open Windtrap
module Seed = Windtrap.Private.Seed
module Gen_engine = Windtrap.Private.Gen_engine
module Shrink_tree = Gen_engine.Shrink_tree
module Pp = Windtrap.Private.Pp

let starts_with prefix text = String.starts_with ~prefix text

(* One fixed root for the whole suite; per-test streams come from indexes.
   Everything below is deterministic across runs and machines. *)
let root = 0x00c0ffee1234abcdL
let state index = Seed.make (Seed.derive ~root ~path:"test_gen" ~index)
let root_value tree = Gen_engine.value (Shrink_tree.root tree)
let rendering tree = Gen_engine.render (Shrink_tree.root tree)

(* The one placeholder a value with no printer renders as, spelled here so
   a drift in [Gen]'s spelling is a failure and not a silently passing
   [contains]. *)
let placeholder = "<no printer: attach one with Gen.with_pp>"

(* The report's spelling of each rendering, for assertions on a value: a
   pre-image is marked so an assertion on the value cannot pass on it. *)
let render tree =
  match rendering tree with
  | Gen_engine.Value text -> text
  | Pre_image text -> "from " ^ text

let samples gen count =
  List.init count (fun index ->
      root_value (Gen_engine.sample gen (state index)))

let find_sample ?(max_index = 10_000) gen accept =
  let rec loop index =
    if index >= max_index then
      failf "no matching sample within %d cases" max_index
    else
      let tree = Gen_engine.sample gen (state index) in
      if accept (root_value tree) then tree else loop (index + 1)
  in
  loop 0

(* The shrink search a user's property gets, judged through its report.
   [shrink ?from ?failing gen] runs [Property.run] under this suite's root
   and path, so its case [i] samples [state i]: the tree [find_sample] and
   [state] hand the tests. The law fails on the first case whose value
   satisfies [from] and [failing] (both default to always), and from then
   on on every candidate that satisfies [failing], so the engine's own
   search descends from that tree. The result is the counterexample as the
   payload renders it, a pre-image marked as [render] marks one, and the
   accepted steps; a search that stopped before converging fails the
   test. *)
let shrink ?(from = fun _ -> true) ?(failing = fun _ -> true) gen =
  let started = ref false in
  let law _ value =
    if (!started || from value) && failing value then begin
      started := true;
      fail "the predicate holds"
    end
  in
  match
    Windtrap.Private.Property.run ~count:(`Declared 10_000) ~root
      ~path:"test_gen" gen law
  with
  | Fail { failure = { kind = Property payload; _ }; _ } ->
      if payload.shrink_end <> Converged then
        failf "the search stopped at %s before it converged"
          payload.rendered.kept;
      let text =
        match payload.rendering with
        | Value -> payload.rendered.kept
        | Pre_image -> "from " ^ payload.rendered.kept
      in
      (text, payload.shrink_steps)
  | _ -> failf "no case in 10000 satisfied the predicate"

let shrinks_to ?from ?failing gen = fst (shrink ?from ?failing gen)

(* Depth-first visit of up to [limit] tree values. *)
let explore ~limit tree visit =
  let visited = ref 0 in
  let rec go tree =
    if !visited >= limit then raise_notrace Exit;
    incr visited;
    visit (root_value tree);
    Seq.iter go (Shrink_tree.children tree)
  in
  try go tree with Exit -> ()

let first_child tree =
  match Shrink_tree.children tree () with
  | Seq.Nil -> failf "expected at least one shrink candidate"
  | Seq.Cons (child, _) -> child

let no_children tree =
  match Shrink_tree.children tree () with
  | Seq.Nil -> true
  | Seq.Cons _ -> false

(* Determinism *)

let same_seed_same_value_and_render () =
  let against : (string * (unit -> string * string)) list =
    let run gen index =
      let once = Gen_engine.sample gen (state index) in
      let twice = Gen_engine.sample gen (state index) in
      (render once, render twice)
    in
    [
      ("int", fun () -> run Gen.int 3);
      ("string", fun () -> run Gen.string 5);
      ("list int", fun () -> run Gen.(list int) 6);
      ("one_of", fun () -> run Gen.(one_of [ constant `A; constant `B ]) 7);
      ("bind", fun () -> run Gen.(bind nat (fun n -> int_range 0 (n + 1))) 8);
      ("of_list", fun () -> run (Gen.of_list [ 1; 2; 3 ]) 9);
      ("char_range", fun () -> run (Gen.char_range 'a' 'z') 10);
    ]
  in
  List.iter
    (fun (name, run) ->
      let once, twice = run () in
      is_true
        ~msg:
          (Printf.sprintf "%s: same seed rendered %S then %S" name once twice)
        (once = twice))
    against

(* Integer generators *)

let int_shrinks_to_zero () =
  equal ~msg:"int shrinks to 0" string "0"
    (shrinks_to ~from:(fun v -> v <> 0) Gen.int)

let int_renders_decimal () =
  let tree = Gen_engine.sample Gen.int (state 0) in
  let rendered = render tree in
  is_true
    ~msg:(Printf.sprintf "int rendered %S for %d" rendered (root_value tree))
    (rendered = string_of_int (root_value tree))

let nat_stays_below_10_000 () =
  List.iter
    (fun v ->
      is_true ~msg:(Printf.sprintf "nat produced %d" v) (v >= 0 && v < 10_000))
    (samples Gen.nat 1_000)

let nat_shrinks_to_zero () =
  equal ~msg:"nat shrinks to 0" string "0"
    (shrinks_to ~from:(fun v -> v > 0) Gen.nat)

let small_int_is_small_and_signed () =
  let values = samples Gen.small_int 500 in
  List.iter
    (fun v ->
      is_true
        ~msg:(Printf.sprintf "small_int produced %d" v)
        (v > -10_000 && v < 10_000))
    values;
  is_true ~msg:"no negative small_int in 500"
    (List.exists (fun v -> v < 0) values);
  is_true ~msg:"no positive small_int in 500"
    (List.exists (fun v -> v > 0) values);
  equal ~msg:"a negative small_int shrinks to 0" string "0"
    (shrinks_to ~from:(fun v -> v < 0) Gen.small_int)

let int_range_stays_in_bounds_while_shrinking () =
  let gen = Gen.int_range 10 100 in
  for index = 0 to 19 do
    let tree = Gen_engine.sample gen (state index) in
    explore ~limit:200 tree (fun v ->
        is_true
          ~msg:(Printf.sprintf "int_range candidate %d out of bounds" v)
          (v >= 10 && v <= 100))
  done;
  equal ~msg:"int_range 10 100 shrinks to 10" string "10"
    (shrinks_to ~from:(fun v -> v > 10) gen)

let int_range_degenerate_is_a_leaf () =
  let tree = Gen_engine.sample (Gen.int_range 5 5) (state 0) in
  is_true
    ~msg:(Printf.sprintf "int_range 5 5 produced %d" (root_value tree))
    (root_value tree = 5);
  is_true ~msg:"int_range 5 5 has shrink candidates" (no_children tree)

let int_range_full_range_works () =
  let gen = Gen.int_range min_int max_int in
  equal ~msg:"the full int_range shrinks to 0" string "0"
    (shrinks_to ~from:(fun v -> v <> 0) gen)

let int_range_invalid_raises_at_sample_time () =
  let gen = Gen.int_range 10 (-10) in
  (* Construction succeeded; sampling reports the error. *)
  match Gen_engine.sample gen (state 0) with
  | exception Invalid_argument _ -> ()
  | _ -> failf "int_range 10 (-10) sampled successfully"

let greedy_shrink_finds_boundary () =
  let gen = Gen.int_range 0 1000 in
  equal ~msg:"the search stops on the boundary" string "51"
    (shrinks_to ~failing:(fun v -> v > 50) gen)

let int32_shrinks_to_zero () =
  equal ~msg:"int32 shrinks to 0" string "0l"
    (shrinks_to ~from:(fun v -> not (Int32.equal v 0l)) Gen.int32)

let int64_shrinks_to_zero () =
  equal ~msg:"int64 shrinks to 0" string "0L"
    (shrinks_to ~from:(fun v -> not (Int64.equal v 0L)) Gen.int64)

let nativeint_shrinks_to_zero_and_prints_as_a_literal () =
  let nonzero v = not (Nativeint.equal v 0n) in
  let tree = find_sample Gen.nativeint nonzero in
  equal ~msg:"nativeint shrinks to 0" string "0n"
    (shrinks_to ~from:nonzero Gen.nativeint);
  let rendered = render tree in
  is_true
    ~msg:(Printf.sprintf "nativeint rendered %S" rendered)
    (rendered = Printf.sprintf "%ndn" (root_value tree));
  let values = samples Gen.nativeint 100 in
  is_true ~msg:"no negative nativeint in 100 draws"
    (List.exists (fun v -> Nativeint.compare v 0n < 0) values);
  (* The full native word, not an [int32] widened: on a 64-bit platform a
     hundred uniform draws cannot all fit in 32 bits. *)
  if Nativeint.size = 64 then
    is_true ~msg:"100 nativeint draws all fit in 32 bits"
      (List.exists
         (fun v ->
           Nativeint.compare (Nativeint.abs v)
             (Nativeint.of_int32 Int32.max_int)
           > 0)
         values)

(* Float generators *)

let float_is_finite () =
  List.iter
    (fun v ->
      is_true ~msg:(Printf.sprintf "float produced %h" v) (Float.is_finite v))
    (samples Gen.float 500);
  equal ~msg:"float shrinks to 0" string "0."
    (shrinks_to ~from:(fun v -> v <> 0.0) Gen.float)

let float_range_stays_in_bounds () =
  let gen = Gen.float_range 2.0 5.0 in
  for index = 0 to 19 do
    let tree = Gen_engine.sample gen (state index) in
    explore ~limit:100 tree (fun v ->
        is_true
          ~msg:(Printf.sprintf "float_range candidate %h out of bounds" v)
          (v >= 2.0 && v <= 5.0))
  done;
  equal ~msg:"float_range 2 5 shrinks to 2" string "2." (shrinks_to gen);
  let negative = Gen.float_range (-5.0) (-2.0) in
  equal ~msg:"float_range -5 -2 shrinks to -2" string "-2."
    (shrinks_to negative)

let float_range_invalid_raises_at_sample_time () =
  let cases =
    [
      ("high < low", Gen.float_range 5.0 2.0);
      ("nan bound", Gen.float_range Float.nan 1.0);
      ("infinite bound", Gen.float_range 0.0 Float.infinity);
      ("overflowing span", Gen.float_range (-.Float.max_float) Float.max_float);
    ]
  in
  List.iter
    (fun (name, gen) ->
      match Gen_engine.sample gen (state 0) with
      | exception Invalid_argument _ -> ()
      | _ -> failf "float_range (%s) sampled successfully" name)
    cases

(* Unit, booleans, characters, strings *)

let unit_generates_and_prints_parentheses () =
  let tree = Gen_engine.sample Gen.unit (state 0) in
  is_true ~msg:"unit has shrink candidates" (no_children tree);
  let rendered = render tree in
  is_true ~msg:(Printf.sprintf "unit rendered %S" rendered) (rendered = "()");
  is_true ~msg:"unit lost its printer"
    (Gen_engine.render_value Gen.unit () = "()");
  (* Why it is not [constant ()]: a deriving composition over [constant ()]
     has no printer to derive from. *)
  let paired = Gen.(pair unit nat) in
  let tree = Gen_engine.sample paired (state 1) in
  let (), n = root_value tree in
  let rendered = render tree in
  is_true
    ~msg:(Printf.sprintf "pair over unit rendered %S" rendered)
    (rendered = Printf.sprintf "((), %d)" n);
  let bare = Gen.(pair (constant ()) nat) in
  is_true ~msg:"pair over [constant ()] claims a printer"
    (Gen_engine.render_value bare ((), 0) = placeholder)

let bool_shrinks_true_to_false () =
  let values = samples Gen.bool 100 in
  is_true ~msg:"no true in 100 bools" (List.mem true values);
  is_true ~msg:"no false in 100 bools" (List.mem false values);
  let tree = find_sample Gen.bool (fun v -> v) in
  is_true ~msg:"true's first candidate is not false"
    (root_value (first_child tree) = false);
  let tree = find_sample Gen.bool (fun v -> not v) in
  is_true ~msg:"false has shrink candidates" (no_children tree)

let char_is_uniform_and_shrinks_to_a () =
  let values = samples Gen.char 300 in
  is_true ~msg:"no byte above 127 in 300 chars"
    (List.exists (fun c -> Char.code c > 127) values);
  is_true ~msg:"no control byte in 300 chars (NUL weight must be 1/256, not 0)"
    (List.exists (fun c -> Char.code c < 32) values);
  equal ~msg:"char shrinks to 'a'" string "'a'"
    (shrinks_to ~from:(fun c -> c <> 'a') Gen.char)

let char_range_stays_in_bounds_and_shrinks_toward_a () =
  let gen = Gen.char_range 'b' 'y' in
  for index = 0 to 19 do
    let tree = Gen_engine.sample gen (state index) in
    explore ~limit:100 tree (fun c ->
        is_true
          ~msg:(Printf.sprintf "char_range candidate %C out of bounds" c)
          (c >= 'b' && c <= 'y'))
  done;
  let tree = find_sample gen (fun c -> c > 'b') in
  equal ~msg:"char_range 'b' 'y' shrinks to 'b'" string "'b'"
    (shrinks_to ~from:(fun c -> c > 'b') gen);
  let rendered = render tree in
  is_true
    ~msg:(Printf.sprintf "char_range rendered %S" rendered)
    (rendered = Printf.sprintf "%C" (root_value tree))

let char_range_outside_a_shrinks_to_nearest_bound () =
  let upper = Gen.char_range 'A' 'Z' in
  equal ~msg:"char_range 'A' 'Z' shrinks to 'Z'" string "'Z'"
    (shrinks_to ~from:(fun c -> c < 'Z') upper);
  let digits = Gen.char_range '0' '9' in
  equal ~msg:"char_range '0' '9' shrinks to '9'" string "'9'"
    (shrinks_to ~from:(fun c -> c < '9') digits);
  List.iter
    (fun index ->
      explore ~limit:100
        (Gen_engine.sample digits (state index))
        (fun c ->
          is_true
            ~msg:(Printf.sprintf "digit candidate %C out of bounds" c)
            (c >= '0' && c <= '9')))
    [ 0; 1; 2 ]

let char_range_degenerate_is_a_leaf_and_invalid_raises () =
  let tree = Gen_engine.sample (Gen.char_range 'x' 'x') (state 0) in
  is_true
    ~msg:(Printf.sprintf "char_range 'x' 'x' produced %C" (root_value tree))
    (root_value tree = 'x');
  is_true ~msg:"char_range 'x' 'x' has shrink candidates" (no_children tree);
  match Gen_engine.sample (Gen.char_range 'z' 'a') (state 0) with
  | exception Invalid_argument _ -> ()
  | _ -> failf "char_range 'z' 'a' sampled successfully"

let string_shrinks_to_empty_and_renders_quoted () =
  let tree = find_sample Gen.string (fun s -> String.length s >= 2) in
  is_true ~msg:"non-empty string's first candidate is not \"\""
    (root_value (first_child tree) = "");
  equal ~msg:"string shrinks to the empty string" string {|""|}
    (shrinks_to ~from:(fun s -> String.length s >= 2) Gen.string);
  let rendered = render tree in
  is_true
    ~msg:(Printf.sprintf "string rendered %S" rendered)
    (rendered = Printf.sprintf "%S" (root_value tree))

let string_of_respects_character_generator () =
  let letters =
    Gen.map (fun c -> Char.chr (97 + (Char.code c mod 16))) Gen.char
  in
  let gen = Gen.string_of letters in
  let in_range c = c >= 'a' && c <= 'p' in
  for index = 0 to 9 do
    let tree = Gen_engine.sample gen (state index) in
    explore ~limit:100 tree (fun s ->
        String.iter
          (fun c ->
            is_true ~msg:(Printf.sprintf "string_of produced %C" c) (in_range c))
          s)
  done

let string_of_size_keeps_length_in_bounds () =
  let gen = Gen.(string_of ~size:(int_range 2 5) char) in
  for index = 0 to 9 do
    let tree = Gen_engine.sample gen (state index) in
    explore ~limit:200 tree (fun s ->
        let n = String.length s in
        is_true
          ~msg:(Printf.sprintf "sized string candidate has length %d" n)
          (n >= 2 && n <= 5))
  done;
  equal ~msg:"a sized string shrinks to its shortest, lowest" string {|"aa"|}
    (shrinks_to gen)

let string_of_negative_size_raises_at_sample_time () =
  let gen = Gen.(string_of ~size:(constant (-1)) char) in
  match Gen_engine.sample gen (state 0) with
  | exception Invalid_argument _ -> ()
  | _ -> failf "negative string size sampled successfully"

let bytes_shrink_to_empty () =
  let nonempty b = Bytes.length b >= 1 in
  let tree = find_sample Gen.bytes nonempty in
  equal ~msg:"bytes shrink to empty" string {|Bytes.of_string ""|}
    (shrinks_to ~from:nonempty Gen.bytes);
  let rendered = render tree in
  is_true
    ~msg:(Printf.sprintf "bytes rendered %S" rendered)
    (starts_with "Bytes.of_string" rendered)

let bytes_of_respects_size_and_character_generator () =
  let gen = Gen.(bytes_of ~size:(constant 3) (char_range 'a' 'z')) in
  for index = 0 to 9 do
    let tree = Gen_engine.sample gen (state index) in
    explore ~limit:100 tree (fun b ->
        is_true
          ~msg:
            (Printf.sprintf "sized bytes candidate has length %d"
               (Bytes.length b))
          (Bytes.length b = 3);
        Bytes.iter
          (fun c ->
            is_true
              ~msg:(Printf.sprintf "bytes_of produced %C" c)
              (c >= 'a' && c <= 'z'))
          b)
  done;
  let tree = Gen_engine.sample gen (state 0) in
  let rendered = render tree in
  is_true
    ~msg:(Printf.sprintf "bytes_of rendered %S" rendered)
    (starts_with "Bytes.of_string" rendered)

(* Containers *)

let list_shrinks_structurally () =
  let gen = Gen.(list int) in
  let tree = find_sample gen (fun l -> List.length l >= 2) in
  is_true ~msg:"non-empty list's first candidate is not []"
    (root_value (first_child tree) = []);
  equal ~msg:"list shrinks to []" string "[]"
    (shrinks_to ~from:(fun l -> List.length l >= 2) gen);
  let rendered = render tree in
  is_true
    ~msg:(Printf.sprintf "list rendered %S" rendered)
    (starts_with "[" rendered)

let list_with_size_keeps_length_in_bounds () =
  let gen = Gen.(list ~size:(int_range 2 5) nat) in
  for index = 0 to 9 do
    let tree = Gen_engine.sample gen (state index) in
    explore ~limit:200 tree (fun l ->
        let n = List.length l in
        is_true
          ~msg:(Printf.sprintf "sized list candidate has length %d" n)
          (n >= 2 && n <= 5))
  done;
  equal ~msg:"a sized list shrinks to its shortest, zeroes" string "[0; 0]"
    (shrinks_to gen)

let list_negative_size_raises_at_sample_time () =
  let gen = Gen.(list ~size:(constant (-1)) nat) in
  match Gen_engine.sample gen (state 0) with
  | exception Invalid_argument _ -> ()
  | _ -> failf "negative size sampled successfully"

let array_shrinks_to_empty () =
  let gen = Gen.(array nat) in
  let nonempty a = Array.length a >= 1 in
  let tree = find_sample gen nonempty in
  equal ~msg:"array shrinks to empty" string "[||]"
    (shrinks_to ~from:nonempty gen);
  let rendered = render tree in
  is_true
    ~msg:(Printf.sprintf "array rendered %S" rendered)
    (starts_with "[|" rendered)

let option_offers_none_first () =
  let gen = Gen.(option nat) in
  let values = samples gen 200 in
  is_true ~msg:"no None in 200 options" (List.mem None values);
  is_true ~msg:"no Some in 200 options" (List.exists Option.is_some values);
  let tree = find_sample gen Option.is_some in
  is_true ~msg:"Some's first candidate is not None"
    (root_value (first_child tree) = None);
  is_true
    ~msg:(Printf.sprintf "Some rendered %S" (render tree))
    (starts_with "Some (" (render tree))

let result_generates_both_constructors () =
  let gen = Gen.(result nat nat) in
  let values = samples gen 500 in
  is_true ~msg:"no Ok in 500 results" (List.exists Result.is_ok values);
  is_true ~msg:"no Error in 500 results" (List.exists Result.is_error values);
  let tree = find_sample gen Result.is_ok in
  explore ~limit:50 tree (fun v ->
      is_true ~msg:"Ok candidate crossed to Error" (Result.is_ok v));
  is_true
    ~msg:(Printf.sprintf "Ok rendered %S" (render tree))
    (starts_with "Ok (" (render tree))

let either_generates_both_constructors () =
  let gen = Gen.(either nat nat) in
  let values = samples gen 500 in
  is_true ~msg:"no Left in 500 eithers" (List.exists Either.is_left values);
  is_true ~msg:"no Right in 500 eithers" (List.exists Either.is_right values);
  let tree = find_sample gen Either.is_right in
  explore ~limit:50 tree (fun v ->
      is_true ~msg:"Right candidate crossed to Left" (Either.is_right v));
  is_true
    ~msg:(Printf.sprintf "Right rendered %S" (render tree))
    (starts_with "Right (" (render tree));
  let tree = find_sample gen Either.is_left in
  is_true
    ~msg:(Printf.sprintf "Left rendered %S" (render tree))
    (starts_with "Left (" (render tree));
  (* Printing derives as for [result]: a printerless side forfeits it, and
     a pre-image side carries through. *)
  is_true ~msg:"either over a printerless side derived a printer"
    (Gen_engine.render_value Gen.(either nat (constant 'k')) (Either.Left 1)
    = placeholder);
  let mapped = Gen.(either (map succ nat) nat) in
  let left = find_sample mapped Either.is_left in
  let n = Either.find_left (root_value left) |> Option.get in
  is_true
    ~msg:(Printf.sprintf "Left of a mapped nat rendered %S" (render left))
    (rendering left = Pre_image (Printf.sprintf "Left (%d)" (n - 1)))

let pair_shrinks_left_first_to_zeroes () =
  let gen = Gen.(pair nat nat) in
  let tree = find_sample gen (fun (a, _) -> a > 0) in
  let _, right = root_value tree in
  is_true ~msg:"pair's first candidate did not shrink the left component to 0"
    (root_value (first_child tree) = (0, right));
  equal ~msg:"pair shrinks to zeroes" string "(0, 0)"
    (shrinks_to ~from:(fun v -> v = root_value tree) gen);
  let tree = Gen_engine.sample gen (state 0) in
  let a, b = root_value tree in
  is_true
    ~msg:(Printf.sprintf "pair rendered %S" (render tree))
    (render tree = Printf.sprintf "(%d, %d)" a b)

let triple_and_quad_shrink_to_zeroes () =
  let nonzero = ( <> ) 0 in
  equal ~msg:"triple shrinks to zeroes" string "(0, 0, 0)"
    (shrinks_to
       ~from:(fun (a, b, c) -> List.exists nonzero [ a; b; c ])
       Gen.(triple nat nat nat));
  equal ~msg:"quad shrinks to zeroes" string "(0, 0, 0, 0)"
    (shrinks_to
       ~from:(fun (a, b, c, d) -> List.exists nonzero [ a; b; c; d ])
       Gen.(quad nat nat nat nat))

(* Choice and structure *)

let constant_is_a_leaf_and_asks_for_a_printer () =
  let gen = Gen.constant 42 in
  let tree = Gen_engine.sample gen (state 0) in
  is_true
    ~msg:(Printf.sprintf "constant produced %d" (root_value tree))
    (root_value tree = 42);
  is_true ~msg:"constant has shrink candidates" (no_children tree);
  (* The rendering names the remedy itself, so a counterexample and a bare
     value spell the same placeholder. *)
  is_true
    ~msg:(Printf.sprintf "constant rendered %S" (render tree))
    (render tree = placeholder);
  is_true ~msg:"constant has a printer"
    (Gen_engine.render_value gen 42 = placeholder)

let of_list_picks_uniformly_and_shrinks_toward_head () =
  let gen = Gen.of_list [ 10; 20; 30 ] in
  let values = samples gen 100 in
  List.iter
    (fun v ->
      is_true
        ~msg:(Printf.sprintf "value %d never chosen in 100" v)
        (List.mem v values))
    [ 10; 20; 30 ];
  let tree = find_sample gen (fun v -> v = 30) in
  is_true ~msg:"the last value's first candidate is not the head"
    (root_value (first_child tree) = 10);
  (* [of_list] prints nothing; a printer changes no candidate. *)
  equal ~msg:"of_list shrinks to the head" string "10"
    (shrinks_to ~from:(fun v -> v = 30) (Gen.with_pp Format.pp_print_int gen));
  let rendered = render tree in
  is_true
    ~msg:(Printf.sprintf "of_list rendered %S" rendered)
    (rendered = placeholder);
  is_true ~msg:"of_list has a printer"
    (Gen_engine.render_value gen 20 = placeholder)

(* One printerless leaf forfeits the derived printer of everything built
   over it, and [with_pp] is the one way back; the printer it attaches then
   feeds the deriving combinators, and the pre-image of a [map]. *)
let leaf_printers_feed_the_derivation_law () =
  let pp = Format.pp_print_int in
  let gen = Gen.with_pp pp (Gen.of_list [ 10; 20; 30 ]) in
  let tree = find_sample gen (fun v -> v = 30) in
  let rendered = render tree in
  is_true
    ~msg:(Printf.sprintf "printed of_list rendered %S, not the value" rendered)
    (rendered = "30");
  is_true ~msg:"printed of_list does not render a bare value"
    (Gen_engine.render_value gen 20 = "20");
  (* The leaf's printer feeds the deriving combinators above it... *)
  let listed = Gen.list gen in
  is_true ~msg:"list over a printed leaf does not render"
    (Gen_engine.render_value listed [ 10; 20 ] = "[10; 20]");
  (* ...and [map], which derives no printer, renders its argument through
     it: the pre-image. *)
  let mapped = Gen.map (fun v -> (v, ())) gen in
  is_true ~msg:"map claimed a printer"
    (Gen_engine.render_value mapped (30, ()) = placeholder);
  let mapped_tree = find_sample mapped (fun (v, ()) -> v = 30) in
  is_true
    ~msg:
      (Printf.sprintf "map over a printed leaf rendered %S" (render mapped_tree))
    (rendering mapped_tree = Pre_image "30");
  let c = Gen.with_pp pp (Gen.constant 7) in
  let c_rendered = render (Gen_engine.sample c (state 0)) in
  is_true
    ~msg:(Printf.sprintf "printed constant rendered %S" c_rendered)
    (c_rendered = "7");
  (* Without one the leaf prints nothing at all. *)
  let bare = Gen.of_list [ 10; 20; 30 ] in
  is_true ~msg:"of_list without a printer prints"
    (Gen_engine.render_value bare 10 = placeholder)

let of_list_singleton_is_a_leaf_and_empty_raises () =
  let tree = Gen_engine.sample (Gen.of_list [ `Only ]) (state 0) in
  is_true ~msg:"of_list singleton produced another value"
    (root_value tree = `Only);
  is_true ~msg:"of_list singleton has shrink candidates" (no_children tree);
  match Gen_engine.sample (Gen.of_list []) (state 0) with
  | exception Invalid_argument _ -> ()
  | _ -> failf "of_list [] sampled successfully"

let one_of_empty_raises_at_sample_time () =
  let gen = Gen.one_of [] in
  match Gen_engine.sample gen (state 0) with
  | exception Invalid_argument _ -> ()
  | _ -> failf "one_of [] sampled successfully"

let one_of_picks_all_branches_and_shrinks_to_earlier () =
  let gen = Gen.(one_of [ constant `A; constant `B ]) in
  let values = samples gen 100 in
  is_true ~msg:"branch 0 never chosen in 100" (List.mem `A values);
  is_true ~msg:"branch 1 never chosen in 100" (List.mem `B values);
  let tree = find_sample gen (fun v -> v = `B) in
  is_true ~msg:"one_of branch 1 did not shrink to branch 0"
    (root_value (first_child tree) = `A);
  let rendered = render tree in
  is_true
    ~msg:(Printf.sprintf "one_of rendered %S" rendered)
    (rendered = placeholder)

let frequency_respects_weights () =
  let gen = Gen.(frequency [ (1, constant `A); (3, constant `B) ]) in
  let values = samples gen 400 in
  let count v = List.length (List.filter (fun x -> x = v) values) in
  is_true ~msg:"weight-1 branch never chosen" (count `A > 0);
  is_true
    ~msg:
      (Printf.sprintf "weight-3 branch not dominant (%d vs %d)" (count `B)
         (count `A))
    (count `B > count `A);
  let tree = find_sample gen (fun v -> v = `B) in
  let rendered = render tree in
  is_true
    ~msg:(Printf.sprintf "frequency rendered %S" rendered)
    (rendered = placeholder)

(* The Gen doc law: a composite prints exactly when all its components
   print, including the choice combinators. *)
let one_of_over_printed_branches_derives_printer () =
  let gen = Gen.(one_of [ int_range 0 9; int_range 100 199 ]) in
  is_true ~msg:"one_of did not derive a printer"
    (Gen_engine.render_value gen 5 = "5");
  let tree = find_sample gen (fun v -> v >= 100) in
  let rendered = render tree in
  is_true
    ~msg:(Printf.sprintf "printed one_of rendered %S, not the value" rendered)
    (rendered = string_of_int (root_value tree));
  (* Shrunk candidates render as values too, including branch re-generation
     candidates. *)
  let child = first_child tree in
  is_true
    ~msg:
      (Printf.sprintf "a shrunk printed one_of candidate rendered %S"
         (render child))
    (render child = string_of_int (root_value child));
  (* The derived printer feeds enclosing deriving combinators, and the
     pre-image of a [map] over the choice. *)
  let paired = Gen.(pair gen nat) in
  let tree = Gen_engine.sample paired (state 0) in
  let a, b = root_value tree in
  is_true
    ~msg:(Printf.sprintf "pair over a printed one_of rendered %S" (render tree))
    (render tree = Printf.sprintf "(%d, %d)" a b);
  let mapped = Gen.map Fun.id gen in
  let tree = Gen_engine.sample mapped (state 1) in
  is_true
    ~msg:(Printf.sprintf "map over a printed one_of rendered %S" (render tree))
    (rendering tree = Pre_image (string_of_int (root_value tree)))

let frequency_over_printed_branches_derives_printer () =
  let gen = Gen.(frequency [ (1, nat); (3, int_range 100 199) ]) in
  is_true ~msg:"frequency did not derive a printer"
    (Gen_engine.render_value gen 7 = "7");
  let tree = Gen_engine.sample gen (state 0) in
  let rendered = render tree in
  is_true
    ~msg:
      (Printf.sprintf "printed frequency rendered %S, not the value" rendered)
    (rendered = string_of_int (root_value tree));
  (* One printerless branch forfeits the derivation for the whole choice. *)
  let mixed = Gen.(frequency [ (1, nat); (1, constant 5) ]) in
  is_true ~msg:"a mixed frequency derived a printer"
    (Gen_engine.render_value mixed 5 = placeholder)

let frequency_invalid_raises_at_sample_time () =
  let cases =
    [
      ("empty", Gen.frequency []);
      ("zero total", Gen.frequency [ (0, Gen.nat) ]);
      ("negative weight", Gen.frequency [ (2, Gen.nat); (-1, Gen.nat) ]);
    ]
  in
  List.iter
    (fun (name, gen) ->
      match Gen_engine.sample gen (state 0) with
      | exception Invalid_argument _ -> ()
      | _ -> failf "frequency (%s) sampled successfully" name)
    cases

let such_that_filters_generation_and_shrinking () =
  let even = Gen.such_that (fun n -> n mod 2 = 0) Gen.int in
  for index = 0 to 9 do
    let tree = Gen_engine.sample even (state index) in
    explore ~limit:100 tree (fun v ->
        is_true
          ~msg:(Printf.sprintf "such_that candidate %d is odd" v)
          (v mod 2 = 0))
  done;
  equal ~msg:"an even int shrinks to 0" string "0"
    (shrinks_to ~from:(fun v -> v <> 0) even);
  is_true ~msg:"such_that dropped the underlying printer"
    (Gen_engine.render_value even 4 = "4")

let such_that_exhaustion_is_a_discard () =
  let gen = Gen.such_that (fun _ -> false) Gen.nat in
  match Gen_engine.sample gen (state 0) with
  | exception Windtrap.Private.Failure.Control `Discard -> ()
  | _ -> failf "unsatisfiable such_that sampled successfully"

(* An [assume] in a mapping function discards: a candidate it rejects is
   skipped, never a cell that raises, and the even ones stay reachable. *)
let a_discarding_map_candidate_is_skipped () =
  let even =
    Gen.map
      (fun n ->
        Windtrap.Private.Property.assume (n mod 2 = 0);
        n)
      Gen.nat
  in
  let rec draw index =
    match Gen_engine.sample even (state index) with
    | tree when root_value tree >= 10 -> tree
    | _ | (exception Windtrap.Private.Failure.Control `Discard) ->
        draw (index + 1)
  in
  let tree = draw 0 in
  let seen = ref 0 in
  explore ~limit:500 tree (fun n ->
      incr seen;
      is_true ~msg:(Printf.sprintf "candidate %d is odd" n) (n mod 2 = 0));
  is_true ~msg:"the even candidates are reached" (!seen > 1)

(* Composition *)

(* A [map] over a printing generator renders its pre-image: the argument
   the function received, through the argument's printer, at the root and
   at every candidate, whose pre-image is the candidate's own. *)
let map_renders_the_pre_image () =
  let gen = Gen.map succ Gen.int in
  let tree = Gen_engine.sample gen (state 1) in
  is_true
    ~msg:
      (Printf.sprintf "mapped int rendered %S for %d" (render tree)
         (root_value tree))
    (rendering tree = Pre_image (string_of_int (root_value tree - 1)));
  let tree = find_sample gen (fun v -> v <> 1) in
  let child = first_child tree in
  is_true
    ~msg:
      (Printf.sprintf "shrunk mapped int rendered %S for %d" (render child)
         (root_value child))
    (rendering child = Pre_image (string_of_int (root_value child - 1)));
  (* The search reports the pre-image of the value it stops at: 11 is the
     least value above 10, computed from 10. *)
  equal ~msg:"a mapped int stops on the boundary, as its pre-image" string
    "from 10"
    (shrinks_to ~from:(fun v -> v <> 1) ~failing:(fun v -> v > 10) gen);
  let rec descend tree =
    match Seq.find (fun c -> root_value c > 10) (Shrink_tree.children tree) with
    | None -> tree
    | Some child -> descend child
  in
  is_true
    ~msg:
      (Printf.sprintf "the minimum's pre-image rendered %S, not 10"
         (render (descend tree)))
    (rendering (descend tree) = Pre_image "10")

(* Nested maps render the outermost available printer along the chain: the
   pre-image of a pre-image is the same pre-image. *)
let nested_maps_render_the_outermost_pre_image () =
  let gen =
    Gen.map
      (fun s -> String.length s)
      (Gen.map String.uppercase_ascii Gen.string)
  in
  let tree = find_sample gen (fun n -> n > 0) in
  match rendering tree with
  | Pre_image text ->
      is_true
        ~msg:(Printf.sprintf "the chain rendered %S, not the drawn string" text)
        (starts_with "\"" text)
  | Value _ -> failf "the chain rendered %S, not a pre-image" (render tree)

(* [and+] is [pair]: two pre-images print as a pair, and a printing
   component keeps its value rendering inside the same pair. *)
let and_plus_renders_as_a_pair () =
  let gen =
    Gen.(
      let+ a = map succ nat
      and+ b = string_of ~size:(int_range 1 1) (char_range 'x' 'x') in
      (a, b))
  in
  let tree = Gen_engine.sample gen (state 2) in
  let a, _ = root_value tree in
  is_true
    ~msg:(Printf.sprintf "let+/and+ rendered %S" (render tree))
    (rendering tree = Pre_image (Printf.sprintf "(%d, \"x\")" (a - 1)))

(* Deriving combinators carry pre-images through: a list of mapped values
   renders as the list of their pre-images. *)
let containers_carry_pre_images () =
  let listed = Gen.(list ~size:(int_range 2 2) (map succ nat)) in
  let tree = Gen_engine.sample listed (state 3) in
  let expected =
    match root_value tree with
    | [ a; b ] -> Printf.sprintf "[%d; %d]" (a - 1) (b - 1)
    | _ -> failf "expected two elements"
  in
  is_true
    ~msg:
      (Printf.sprintf "list of mapped nats rendered %S, not %S" (render tree)
         expected)
    (rendering tree = Pre_image expected);
  let optional = Gen.(option (map succ nat)) in
  let some = find_sample optional Option.is_some in
  let n = Option.get (root_value some) in
  is_true
    ~msg:(Printf.sprintf "Some of a mapped nat rendered %S" (render some))
    (rendering some = Pre_image (Printf.sprintf "Some (%d)" (n - 1)));
  (* [None] has no part computed by the map: it is the value. *)
  let none = find_sample optional Option.is_none in
  is_true
    ~msg:(Printf.sprintf "None rendered %S" (render none))
    (rendering none = Value "None")

(* A leaf with nothing to print forfeits the pre-image of the whole
   composition, exactly as it forfeits the derived printer. *)
let no_printer_anywhere_renders_nothing () =
  let gen = Gen.(map (fun (c, n) -> (c, n)) (pair (constant 'k') nat)) in
  let tree = Gen_engine.sample gen (state 0) in
  is_true
    ~msg:(Printf.sprintf "a map over a constant rendered %S" (render tree))
    (rendering tree = Value placeholder);
  let bound = Gen.(bind nat (fun n -> map (fun c -> (c, n)) (constant 'k'))) in
  let tree = Gen_engine.sample bound (state 0) in
  is_true
    ~msg:(Printf.sprintf "a bind into a constant rendered %S" (render tree))
    (rendering tree = Value placeholder)

let bind_keeps_inner_constraints_while_shrinking () =
  let gen =
    Gen.(
      let* n = int_range 1 3 in
      list ~size:(constant n) nat)
  in
  for index = 0 to 9 do
    let tree = Gen_engine.sample gen (state index) in
    explore ~limit:200 tree (fun l ->
        let n = List.length l in
        is_true
          ~msg:(Printf.sprintf "bound list candidate has length %d" n)
          (n >= 1 && n <= 3))
  done;
  equal ~msg:"a bound list shrinks to its shortest, zeroes" string "[0]"
    (shrinks_to gen)

(* A [bind] renders the inner value when the inner generator prints; the
   pre-image [outer -> inner] when the inner is itself a pre-image; and
   nothing when the inner has nothing to print. *)
let bind_renders_by_its_inner () =
  let printing = Gen.(bind nat (fun n -> int_range n (n + 1))) in
  let tree = Gen_engine.sample printing (state 4) in
  is_true
    ~msg:
      (Printf.sprintf "bind into a printing generator rendered %S" (render tree))
    (rendering tree = Value (string_of_int (root_value tree)));
  let opaque = Gen.(bind nat (fun n -> constant n)) in
  let tree = Gen_engine.sample opaque (state 4) in
  is_true
    ~msg:(Printf.sprintf "bind into a constant rendered %S" (render tree))
    (rendering tree = Value placeholder);
  let chained =
    Gen.(
      let* n = int_range 1 3 in
      let+ xs = list ~size:(constant n) (int_range 7 7) in
      (n, xs))
  in
  let tree = find_sample chained (fun (n, _) -> n > 1) in
  let n, xs = root_value tree in
  let expected =
    Printf.sprintf "%d -> [%s]" n
      (String.concat "; " (List.map string_of_int xs))
  in
  is_true
    ~msg:
      (Printf.sprintf "a bind chain rendered %S, not %S" (render tree) expected)
    (rendering tree = Pre_image expected);
  (* Candidates re-generate the inner value: the pre-image follows. *)
  let child = first_child tree in
  let n, xs = root_value child in
  let expected =
    Printf.sprintf "%d -> [%s]" n
      (String.concat "; " (List.map string_of_int xs))
  in
  is_true
    ~msg:
      (Printf.sprintf "a shrunk bind chain rendered %S, not %S" (render child)
         expected)
    (rendering child = Pre_image expected);
  (* Nested binds read left to right; an outer that is itself a bind's
     pre-image is parenthesised. *)
  let nested =
    Gen.(
      let* a = int_range 1 1 in
      let* b = int_range 2 2 in
      let+ c = int_range 3 3 in
      a + b + c)
  in
  let tree = Gen_engine.sample nested (state 0) in
  is_true
    ~msg:(Printf.sprintf "nested binds rendered %S" (render tree))
    (rendering tree = Pre_image "1 -> 2 -> 3");
  let outer_bind =
    Gen.(
      let* ab =
        let* a = int_range 1 1 in
        let+ b = int_range 2 2 in
        a + b
      in
      let+ c = int_range 3 3 in
      ab + c)
  in
  let tree = Gen_engine.sample outer_bind (state 0) in
  is_true
    ~msg:
      (Printf.sprintf "a bind whose outer is a bind rendered %S" (render tree))
    (rendering tree = Pre_image "(1 -> 2) -> 3")

let letops_compose () =
  let gen =
    Gen.(
      let+ a = nat and+ b = nat in
      a + b)
  in
  equal ~msg:"a let+/and+ sum shrinks to the pre-image of 0" string
    "from (0, 0)"
    (shrinks_to ~from:(fun v -> v > 0) gen)

let with_pp_attaches_a_printer () =
  let custom ppf n = Format.fprintf ppf "N=%d" n in
  let inner = Gen.with_pp custom (Gen.map succ Gen.int) in
  let tree = Gen_engine.sample inner (state 5) in
  is_true ~msg:"with_pp did not render directly"
    (render tree = Printf.sprintf "N=%d" (root_value tree));
  is_true ~msg:"with_pp did not expose the printer"
    (Gen_engine.render_value inner 7 = "N=7");
  (* A [map] above it derives nothing, and renders its argument through
     the attached printer: a pre-image. *)
  let outer = Gen.map (fun n -> -n) inner in
  let tree = Gen_engine.sample outer (state 6) in
  is_true
    ~msg:(Printf.sprintf "mapped with_pp rendered %S" (render tree))
    (rendering tree = Pre_image (Printf.sprintf "N=%d" (-root_value tree)));
  (* On the image, an explicit printer wins over the pre-image. *)
  let printed = Gen.with_pp custom outer in
  let tree = Gen_engine.sample printed (state 6) in
  is_true
    ~msg:(Printf.sprintf "with_pp over a map rendered %S" (render tree))
    (rendering tree = Value (Printf.sprintf "N=%d" (root_value tree)))

let mixed_one_of_derives_no_printer () =
  (* One branch prints, one does not: the choice cannot derive a printer,
     and a counterexample renders with the branch that drew it. *)
  let custom ppf n = Format.fprintf ppf "N=%d" n in
  let gen = Gen.(one_of [ with_pp custom (constant 5); constant 9 ]) in
  is_true ~msg:"a mixed one_of derived a printer"
    (Gen_engine.render_value gen 5 = placeholder);
  let printed = find_sample gen (fun v -> v = 5) in
  is_true
    ~msg:(Printf.sprintf "printed branch rendered %S" (render printed))
    (rendering printed = Value "N=5");
  let printerless = find_sample gen (fun v -> v = 9) in
  is_true
    ~msg:(Printf.sprintf "printerless branch rendered %S" (render printerless))
    (rendering printerless = Value placeholder)

(* The shape example: [map] under each branch makes the choice
   printerless, so a counterexample renders the pre-image of the drawn
   branch, and [with_pp] at the top prints the shape itself. *)
type shape = Circle of float | Rect of float * float

let shape_gen =
  Gen.(
    one_of
      [
        map (fun r -> Circle r) (float_range 0.0 100.0);
        map
          (fun (w, h) -> Rect (w, h))
          (pair (float_range 0.0 100.0) (float_range 0.0 100.0));
      ])

let shape_generator_prints_only_with_pp () =
  let rect_tree =
    find_sample shape_gen (function Rect _ -> true | Circle _ -> false)
  in
  let w, h =
    match root_value rect_tree with
    | Rect (w, h) -> (w, h)
    | Circle _ -> assert false
  in
  is_true
    ~msg:(Printf.sprintf "bare shape rendered %S" (render rect_tree))
    (rendering rect_tree
    = Pre_image (Format.asprintf "(%a, %a)" Pp.float_exact w Pp.float_exact h));
  let pp_shape ppf = function
    | Circle r -> Format.fprintf ppf "Circle %g" r
    | Rect (w, h) -> Format.fprintf ppf "Rect (%g, %g)" w h
  in
  let printed = Gen.with_pp pp_shape shape_gen in
  let tree = Gen_engine.sample printed (state 0) in
  is_true
    ~msg:(Printf.sprintf "with_pp shape rendered %S" (render tree))
    (starts_with "Circle" (render tree) || starts_with "Rect" (render tree))

(* Printing totality *)

let render_is_total_over_shrink_trees () =
  let check_gen : type a. string -> a Gen.t -> unit =
   fun name gen ->
    for index = 0 to 4 do
      let tree = Gen_engine.sample gen (state index) in
      let visited = ref 0 in
      let rec go tree =
        if !visited >= 50 then raise_notrace Exit;
        incr visited;
        let rendered = render tree in
        is_true
          ~msg:(Printf.sprintf "%s rendered an empty string" name)
          (String.length rendered > 0);
        Seq.iter go (Shrink_tree.children tree)
      in
      try go tree with Exit -> ()
    done
  in
  check_gen "int" Gen.int;
  check_gen "list int" Gen.(list int);
  check_gen "shape" shape_gen;
  check_gen "mapped option" Gen.(map (fun o -> o) (option (map succ nat)))

let raising_printer_is_contained () =
  let boom _ _ = failwith "boom" in
  let gen = Gen.with_pp boom Gen.nat in
  let tree = Gen_engine.sample gen (state 0) in
  is_true
    ~msg:(Printf.sprintf "raising printer rendered %S" (render tree))
    (starts_with "<printer raised" (render tree));
  let rendered = Gen_engine.render_value gen 3 in
  is_true
    ~msg:(Printf.sprintf "render_value let the exception through: %S" rendered)
    (starts_with "<printer raised" rendered)

let render_value_reports_printer_presence () =
  is_true ~msg:"int printer missing" (Gen_engine.render_value Gen.int 42 = "42");
  is_true ~msg:"list printer missing"
    (Gen_engine.render_value Gen.(list nat) [ 1; 2 ] = "[1; 2]");
  is_true ~msg:"map kept a printer it cannot have"
    (Gen_engine.render_value (Gen.map succ Gen.int) 3 = placeholder);
  is_true ~msg:"with_pp did not attach its printer"
    (Gen_engine.render_value
       (Gen.with_pp Format.pp_print_int (Gen.map succ Gen.int))
       3
    = "3")

(* Adversarial additions *)

(* A shrink candidate whose re-generation exhausts a [such_that] budget must
   be skipped, not raise: memoized child cells cache exceptions, so a raising
   cell would also hide every later sibling candidate. *)
let rejected_bind_candidates_are_skipped () =
  let gen =
    Gen.(
      bind (int_range 1 10) (fun n ->
          if n = 1 then such_that (fun _ -> false) nat else constant n))
  in
  equal
    ~msg:
      "a bind with a rejecting candidate shrinks to 2: candidate 1 is skipped, \
       its siblings kept"
    string "2"
    (shrinks_to ~from:(fun v -> v >= 3) (Gen.with_pp Format.pp_print_int gen))

let rejected_one_of_candidates_are_skipped () =
  let gen = Gen.(one_of [ such_that (fun _ -> false) nat; constant 7 ]) in
  equal ~msg:"one_of with a rejecting branch stays on 7, in no step"
    (pair string int) ("7", 0)
    (shrink ~from:(fun v -> v = 7) (Gen.with_pp Format.pp_print_int gen))

let int_range_negative_bounds_shrink_to_high () =
  let gen = Gen.int_range (-100) (-10) in
  for index = 0 to 9 do
    let tree = Gen_engine.sample gen (state index) in
    explore ~limit:200 tree (fun v ->
        is_true
          ~msg:
            (Printf.sprintf "int_range -100 -10 candidate %d out of bounds" v)
          (v >= -100 && v <= -10))
  done;
  equal ~msg:"int_range -100 -10 shrinks to -10" string "-10"
    (shrinks_to ~from:(fun v -> v < -10) gen)

let frequency_zero_weight_branch_is_never_chosen () =
  let gen = Gen.(frequency [ (0, constant `A); (1, constant `B) ]) in
  List.iter
    (fun v -> is_true ~msg:"frequency chose a zero-weight branch" (v = `B))
    (samples gen 100)

(* The doc's shrink order for [of_list], pinned exactly: the value at
   position 2 offers the head first, then the intermediate position, and the
   order is deterministic across samplings of the same state. *)
let of_list_candidate_order_is_head_then_intermediates () =
  let gen = Gen.of_list [ 10; 20; 30 ] in
  let tree = find_sample gen (fun v -> v = 30) in
  let candidates =
    List.of_seq (Seq.map root_value (Shrink_tree.children tree))
  in
  is_true
    ~msg:
      (Printf.sprintf
         "candidates of the position-2 value are [%s], not [10; 20]"
         (String.concat "; " (List.map string_of_int candidates)))
    (candidates = [ 10; 20 ])

(* The one bound pair whose width is the whole byte: the draws reach the
   high half, where a width that overflowed would never land. *)
let char_range_full_byte_span_reaches_the_high_half () =
  let values = samples (Gen.char_range '\x00' '\xff') 300 in
  is_true ~msg:"no byte above 127 in 300 draws of the full span"
    (List.exists (fun c -> Char.code c > 127) values)

(* A [such_that] as the size generator: the filtered constraint must hold for
   the drawn length and for every shrink candidate's length. *)
let such_that_size_constrains_every_candidate () =
  let gen = Gen.(list ~size:(such_that (fun n -> n mod 2 = 0) nat) nat) in
  let tree = find_sample gen (fun l -> List.length l >= 2) in
  explore ~limit:300 tree (fun l ->
      is_true
        ~msg:
          (Printf.sprintf "even-size list candidate has odd length %d"
             (List.length l))
        (List.length l mod 2 = 0));
  equal ~msg:"an even-size list shrinks to []" string "[]"
    (shrinks_to ~from:(fun v -> v = root_value tree) gen);
  (* An exhausted size generator is a generation-time discard, like any other
     [such_that] exhaustion. *)
  let starved = Gen.(list ~size:(such_that (fun _ -> false) nat) nat) in
  match Gen_engine.sample starved (state 0) with
  | exception Windtrap.Private.Failure.Control `Discard -> ()
  | _ -> failf "a starved size generator sampled successfully"

(* [such_that] around a sized string: both constraints (fixed length and the
   predicate) hold for the root and every candidate, and the greedy minimum
   is the predicate boundary. *)
let such_that_over_sized_string_keeps_both_constraints () =
  let gen =
    Gen.(
      such_that
        (fun s -> s <> "aaa")
        (string_of ~size:(constant 3) (char_range 'a' 'z')))
  in
  for index = 0 to 4 do
    let tree = Gen_engine.sample gen (state index) in
    explore ~limit:200 tree (fun s ->
        is_true
          ~msg:(Printf.sprintf "candidate %S is not 3 chars" s)
          (String.length s = 3);
        is_true ~msg:"candidate violated the predicate" (s <> "aaa");
        String.iter
          (fun c ->
            is_true
              ~msg:(Printf.sprintf "candidate char %C" c)
              (c >= 'a' && c <= 'z'))
          s);
    let minimum = shrinks_to ~from:(fun v -> v = root_value tree) gen in
    let sorted =
      String.to_seq minimum |> List.of_seq |> List.sort compare |> List.to_seq
      |> String.of_seq
    in
    (* The quotes sort first. *)
    equal
      ~msg:
        (Printf.sprintf "the minimum %s must sit on the predicate boundary"
           minimum)
      string {|""aab|} sorted
  done

(* An explicit [with_pp] must win over the derived choice printer, and a
   [map] above the override renders its pre-image through the override. *)
let with_pp_overrides_derived_choice_printer () =
  let custom ppf n = Format.fprintf ppf "N=%d" n in
  let overridden = Gen.(with_pp custom (one_of [ int_range 0 9; nat ])) in
  is_true ~msg:"with_pp did not override the derived choice printer"
    (Gen_engine.render_value overridden 5 = "N=5");
  let tree = Gen_engine.sample overridden (state 0) in
  is_true
    ~msg:(Printf.sprintf "overridden choice rendered %S" (render tree))
    (render tree = Printf.sprintf "N=%d" (root_value tree));
  let mapped = Gen.map Fun.id overridden in
  let tree = Gen_engine.sample mapped (state 1) in
  is_true
    ~msg:(Printf.sprintf "map over the override rendered %S" (render tree))
    (rendering tree = Pre_image (Printf.sprintf "N=%d" (root_value tree)))

(* An identifier shape met in the field: characters from a frequency over
   char_range and of_list, assembled with a sized string. The composition
   must generate in-alphabet, keep the string printer, and shrink to a
   single boundary character. *)
let evidence_shaped_identifier_generator_composes () =
  let ident_char =
    Gen.(frequency [ (8, char_range 'a' 'z'); (1, of_list [ '-'; '_' ]) ])
  in
  let ident = Gen.(string_of ~size:(int_range 1 8) ident_char) in
  let in_alphabet c = (c >= 'a' && c <= 'z') || c = '-' || c = '_' in
  for index = 0 to 9 do
    let tree = Gen_engine.sample ident (state index) in
    explore ~limit:100 tree (fun s ->
        let n = String.length s in
        is_true
          ~msg:(Printf.sprintf "identifier candidate has length %d" n)
          (n >= 1 && n <= 8);
        String.iter
          (fun c ->
            is_true
              ~msg:(Printf.sprintf "identifier char %C off-alphabet" c)
              (in_alphabet c))
          s)
  done;
  let tree = Gen_engine.sample ident (state 0) in
  let rendered = render tree in
  is_true
    ~msg:(Printf.sprintf "identifier lost its printer: %S" rendered)
    (starts_with "\"" rendered);
  let minimum = shrinks_to ~from:(fun v -> v = root_value tree) ident in
  is_true
    ~msg:
      (Printf.sprintf "identifier shrank to %s, not a single boundary char"
         minimum)
    (minimum = {|"a"|} || minimum = {|"-"|})

(* Contract details: each test below reads one sentence of gen.mli. *)

(* Two states are the same stream when their next two words agree;
   [Seed.state] has no equality of its own. *)
let same_stream a b =
  let word state = fst (Seed.bits64 state) in
  let next state = snd (Seed.bits64 state) in
  Int64.equal (word a) (word b) && Int64.equal (word (next a)) (word (next b))

let successor gen state = snd (Gen_engine.run gen state)

let raises_invalid_arg ~message f =
  match f () with
  | _ -> failf "expected Invalid_argument %S" message
  | exception Invalid_argument m -> equal ~msg:"the message" string message m

let child_values tree =
  List.map
    (fun t -> Gen_engine.value (Shrink_tree.root t))
    (List.of_seq (Shrink_tree.children tree))

(* A size generator whose tree is given by hand: a root length and its
   candidate lengths. *)
let sized root candidates =
  Gen_engine.make (fun state ->
      ( Shrink_tree.make ~root
          ~children:(List.to_seq (List.map Shrink_tree.leaf candidates)),
        state ))

(* The values a recorded seed replays today, at this suite's states. A
   change to the words a generator draws, or to their order, changes them:
   that is a change to every recorded seed, and this is where it shows. *)
let a_recorded_seed_replays_these_values () =
  let replay gen index = render (Gen_engine.sample gen (state index)) in
  equal ~msg:"int" string "2683476424248205541" (replay Gen.int 0);
  equal ~msg:"nat" string "254" (replay Gen.nat 1);
  equal ~msg:"float" string "9.32137030625773e+307" (replay Gen.float 2);
  equal ~msg:"string" string "\"dpsagx\""
    (replay Gen.(string_of ~size:(int_range 0 6) (char_range 'a' 'z')) 3);
  equal ~msg:"list of small_int" string "[2657; -902; -6188; 566; 2]"
    (replay Gen.(list ~size:(int_range 0 5) small_int) 4);
  equal ~msg:"option, one_of and frequency" string "(Some (false), 7, 4)"
    (replay
       Gen.(
         triple (option bool)
           (one_of [ int_range 0 9; int_range 100 109 ])
           (frequency [ (1, int_range 0 9); (3, int_range 100 109) ]))
       5)

(* The choices a recorded seed replays over this suite's first 2000 states.
   A choice moved by one stratum, or a side swapped, changes a count. *)
let a_recorded_seed_replays_these_choices () =
  let count gen p = List.length (List.filter p (samples gen 2_000)) in
  equal ~msg:"sum of nat" int 734774
    (List.fold_left ( + ) 0 (samples Gen.nat 2_000));
  equal ~msg:"None" int 270 (count Gen.(option unit) Option.is_none);
  equal ~msg:"Error" int 473 (count Gen.(result unit unit) Result.is_error);
  equal ~msg:"Left" int 1026 (count Gen.(either unit unit) Either.is_left)

let float_range_draws_inside_its_edges () =
  let sample gen = root_value (Gen_engine.sample gen (state 0)) in
  equal ~msg:"a one-point range" string "1.5"
    (Pp.to_string Pp.float_exact (sample (Gen.float_range 1.5 1.5)));
  is_true ~msg:"a span of max_float"
    (Float.is_finite (sample (Gen.float_range 0. Float.max_float)));
  equal ~msg:"draws of the high bound" int 0
    (List.length
       (List.filter (Float.equal 2.) (samples (Gen.float_range 1. 2.) 1_000)))

let float_range_origin_is_positive_zero () =
  equal ~msg:"from -0." string "0." (shrinks_to (Gen.float_range (-0.) 1.));
  equal ~msg:"to -0." string "0." (shrinks_to (Gen.float_range (-1.) (-0.)))

let int32_and_int64_print_their_literal_suffix () =
  for index = 0 to 19 do
    let tree = Gen_engine.sample Gen.int32 (state index) in
    equal ~msg:"int32" string
      (Printf.sprintf "%ldl" (root_value tree))
      (render tree);
    let tree = Gen_engine.sample Gen.int64 (state index) in
    equal ~msg:"int64" string
      (Printf.sprintf "%LdL" (root_value tree))
      (render tree)
  done

let float_prints_the_shortest_round_trip () =
  for index = 0 to 19 do
    let tree = Gen_engine.sample Gen.float (state index) in
    equal string (Pp.to_string Pp.float_exact (root_value tree)) (render tree)
  done

let a_float_node_has_at_most_15_candidates () =
  let counts =
    List.init 20 (fun index ->
        Seq.length
          (Shrink_tree.children (Gen_engine.sample Gen.float (state index))))
  in
  List.iter (fun n -> at_most ~msg:"candidates" int ~than:15 n) counts;
  (* A float far from 0 halves its gap more than 15 times before it
     converges, so the cut is reached. *)
  mem ~msg:"the cut is reached" int 15 counts

let nat_strata_are_50_25_20_5 () =
  (* Each stratum is uniform below its bound, so the bands overlap: below
     10 is 0.5 + 0.25 * 0.1 + 0.2 * 0.01 + 0.05 * 0.001, and so on. *)
  let values = samples Gen.nat 20_000 in
  let share low high =
    float_of_int
      (List.length (List.filter (fun v -> low <= v && v < high) values))
    /. 20_000.
  in
  let within name expected v =
    is_true
      ~msg:(Printf.sprintf "%s: %.4f, expected about %.4f" name v expected)
      (Float.abs (v -. expected) < 0.015)
  in
  within "below 10" 0.52705 (share 0 10);
  within "10 to 99" 0.24345 (share 10 100);
  within "100 to 999" 0.1845 (share 100 1_000);
  within "1000 to 9999" 0.045 (share 1_000 10_000)

let float_is_uniform_over_bit_patterns () =
  (* Half the finite bit patterns are negative, and half have a magnitude of
     at least 1; a float uniform over the reals would have nearly none below
     1. *)
  let values = samples Gen.float 4_000 in
  let share p = float_of_int (List.length (List.filter p values)) /. 4_000. in
  let about_half name v =
    is_true ~msg:(Printf.sprintf "%s: %.3f" name v) (v > 0.46 && v < 0.54)
  in
  about_half "negative" (share Float.sign_bit);
  about_half "magnitude at least 1" (share (fun v -> Float.abs v >= 1.))

let argument_checks_run_in_their_stated_order () =
  let sample gen () = ignore (Gen_engine.sample gen (state 0)) in
  raises_invalid_arg ~message:"Gen.float_range: bounds must be finite"
    (sample (Gen.float_range 1. Float.neg_infinity));
  raises_invalid_arg ~message:"Gen.float_range: high < low"
    (sample (Gen.float_range 1. 0.));
  raises_invalid_arg ~message:"Gen.float_range: high -. low > max_float"
    (sample (Gen.float_range (-.Float.max_float) Float.max_float));
  raises_invalid_arg ~message:"Gen.frequency: empty list"
    (sample (Gen.frequency []));
  raises_invalid_arg ~message:"Gen.frequency: negative weight"
    (sample (Gen.frequency [ (-1, Gen.nat) ]));
  raises_invalid_arg ~message:"Gen.frequency: total weight < 1"
    (sample (Gen.frequency [ (0, Gen.nat) ]))

let a_negative_length_names_gen_list () =
  let sample gen () = ignore (Gen_engine.sample gen (state 0)) in
  let size = Gen.constant (-1) in
  raises_invalid_arg ~message:"Gen.list: negative size"
    (sample Gen.(list ~size unit));
  raises_invalid_arg ~message:"Gen.list: negative size"
    (sample Gen.(array ~size unit));
  raises_invalid_arg ~message:"Gen.list: negative size"
    (sample Gen.(string_of ~size char));
  raises_invalid_arg ~message:"Gen.list: negative size"
    (sample Gen.(bytes_of ~size char))

let unit_and_constant_consume_no_randomness () =
  let s = state 0 in
  is_true ~msg:"unit" (same_stream s (successor Gen.unit s));
  is_true ~msg:"constant" (same_stream s (successor (Gen.constant 7) s));
  is_false ~msg:"premise: bool consumes" (same_stream s (successor Gen.bool s))

let char_prints_with_percent_c () =
  for index = 0 to 59 do
    let tree = Gen_engine.sample Gen.char (state index) in
    equal string (Printf.sprintf "%C" (root_value tree)) (render tree)
  done;
  let tree = find_sample Gen.char (fun c -> Char.code c < 32) in
  equal ~msg:"a control byte is escaped" string
    (Printf.sprintf "%C" (root_value tree))
    (render tree)

let bytes_prints_as_bytes_of_string () =
  for index = 0 to 19 do
    let tree = Gen_engine.sample Gen.bytes (state index) in
    equal string
      (Printf.sprintf "Bytes.of_string %S" (Bytes.to_string (root_value tree)))
      (render tree)
  done

let list_and_array_print_their_brackets () =
  let size = Gen.int_range 0 4 in
  let items xs = String.concat "; " (List.map string_of_int xs) in
  for index = 0 to 19 do
    let tree = Gen_engine.sample Gen.(list ~size small_int) (state index) in
    equal ~msg:"list" string ("[" ^ items (root_value tree) ^ "]") (render tree);
    let tree = Gen_engine.sample Gen.(array ~size small_int) (state index) in
    equal ~msg:"array" string
      ("[|" ^ items (Array.to_list (root_value tree)) ^ "|]")
      (render tree)
  done

let default_length_is_drawn_as_nat_draws () =
  for index = 0 to 99 do
    equal int
      (root_value (Gen_engine.sample Gen.nat (state index)))
      (List.length
         (root_value (Gen_engine.sample Gen.(list unit) (state index))))
  done

let sized_list_candidates_are_prefixes_then_element_reductions () =
  let gen = Gen.(list ~size:(int_range 0 6) (int_range 1 9)) in
  let tree =
    find_sample gen (fun xs ->
        List.length xs >= 4 && List.for_all (fun x -> x > 1) xs)
  in
  let drawn = root_value tree in
  let length = List.length drawn in
  let candidates = child_values tree in
  let shorter = List.filter (fun xs -> List.length xs < length) candidates in
  let same = List.filter (fun xs -> List.length xs = length) candidates in
  is_true ~msg:"premise: both kinds are there" (shorter <> [] && same <> []);
  equal ~msg:"the length candidates come first"
    (list (list int))
    shorter
    (List.filteri (fun i _ -> i < List.length shorter) candidates);
  List.iter
    (fun xs ->
      equal ~msg:"a shorter candidate is a prefix of the drawn list" (list int)
        (List.filteri (fun i _ -> i < List.length xs) drawn)
        xs)
    shorter;
  let changed xs =
    List.filteri (fun i x -> x <> List.nth drawn i) xs |> List.length
  in
  let first_changed xs =
    let rec go i = function
      | x :: rest -> if x <> List.nth drawn i then i else go (i + 1) rest
      | [] -> i
    in
    go 0 xs
  in
  List.iter (fun xs -> equal ~msg:"one element reduced" int 1 (changed xs)) same;
  let positions = List.map first_changed same in
  equal ~msg:"the reductions go from the left" (list int)
    (List.sort compare positions)
    positions

let a_rejected_sized_list_candidate_is_skipped () =
  (* Every element is rejected, so only the empty list generates: the
     candidate lengths 3 and 2 are skipped, and 0 between them survives. *)
  let gen =
    Gen.list
      ~size:(sized 0 [ 3; 0; 2 ])
      (Gen.such_that (fun _ -> false) Gen.unit)
  in
  equal
    (list (list unit))
    [ [] ]
    (child_values (Gen_engine.sample gen (state 0)))

let a_negative_candidate_length_raises_at_its_forcing () =
  let gen = Gen.list ~size:(sized 2 [ -1 ]) Gen.unit in
  let tree = Gen_engine.sample gen (state 0) in
  equal ~msg:"sampling draws the root" int 2 (List.length (root_value tree));
  raises_invalid_arg ~message:"Gen.list: negative size" (fun () ->
      Shrink_tree.children tree ())

let option_result_and_either_probabilities () =
  let share gen p =
    float_of_int (List.length (List.filter p (samples gen 8_000))) /. 8_000.
  in
  let near name expected v =
    is_true
      ~msg:(Printf.sprintf "%s: %.3f, expected %.2f" name v expected)
      (Float.abs (v -. expected) < 0.02)
  in
  near "None" 0.15 (share Gen.(option unit) Option.is_none);
  near "Error" 0.25 (share Gen.(result unit unit) Result.is_error);
  near "Left" 0.5 (share Gen.(either unit unit) Either.is_left)

let pair_draws_its_first_component_first () =
  for index = 0 to 9 do
    let s = state index in
    let a, after_a = Gen_engine.run Gen.int s in
    let b, after_b = Gen_engine.run Gen.int after_a in
    let ab, after_ab = Gen_engine.run Gen.(pair int int) s in
    equal (pair int int)
      (Shrink_tree.root a, Shrink_tree.root b)
      (Shrink_tree.root ab);
    is_true ~msg:"the successor is past both" (same_stream after_b after_ab)
  done

let triple_and_quad_shrink_from_the_left () =
  let nonzero = ( <> ) 0 in
  let tree =
    find_sample
      Gen.(triple nat nat nat)
      (fun (a, b, c) -> List.for_all nonzero [ a; b; c ])
  in
  let _, b, c = root_value tree in
  equal ~msg:"triple" (triple int int int) (0, b, c)
    (root_value (first_child tree));
  let tree =
    find_sample
      Gen.(quad nat nat nat nat)
      (fun (a, b, c, d) -> List.for_all nonzero [ a; b; c; d ])
  in
  let _, b, c, d = root_value tree in
  equal ~msg:"quad" (quad int int int int) (0, b, c, d)
    (root_value (first_child tree))

let one_of_offers_earlier_branches_then_the_drawn_value () =
  let gen =
    Gen.(one_of [ constant 10; constant 20; constant 30; int_range 40 50 ])
  in
  let tree = find_sample gen (fun v -> v > 41) in
  let drawn = root_value tree in
  let children = child_values tree in
  (* The drawn index is 3, whose integer-scheme candidates are 0, 1, 2. *)
  equal ~msg:"the earlier branches first" (list int) [ 10; 20; 30 ]
    (List.filteri (fun i _ -> i < 3) children);
  let rest = List.filteri (fun i _ -> i >= 3) children in
  is_true ~msg:"then the drawn value's candidates" (rest <> []);
  List.iter
    (fun v ->
      is_true ~msg:"a candidate of the drawn value" (40 <= v && v < drawn))
    rest

let frequency_draws_on_the_state_as_it_stands_and_keeps_its_choice () =
  for index = 0 to 9 do
    let s = state index in
    let _, after_choice = Seed.below ~bound:1L s in
    equal ~msg:"the branch draws right after the choice" int
      (root_value (Gen_engine.sample Gen.int after_choice))
      (root_value (Gen_engine.sample Gen.(frequency [ (1, int) ]) s))
  done;
  let gen = Gen.(frequency [ (1, constant 0); (1, int_range 1 100) ]) in
  let tree = find_sample gen (fun v -> v > 2) in
  explore ~limit:200 tree (fun v ->
      is_true ~msg:"no candidate crosses to the other branch" (v >= 1))

let such_that_draws_at_most_100_times () =
  let draws = ref 0 in
  let counted =
    Gen_engine.make ~pp:Format.pp_print_int (fun state ->
        incr draws;
        Gen_engine.run Gen.int state)
  in
  (match
     Gen_engine.sample (Gen.such_that (fun _ -> false) counted) (state 0)
   with
  | _ -> fail "an unsatisfiable such_that sampled"
  | exception Windtrap.Private.Failure.Control `Discard -> ());
  equal ~msg:"draws, the first included" int 100 !draws

let map_runs_f_once_per_node_and_lets_its_exceptions_escape () =
  let calls = ref 0 in
  let counting v =
    incr calls;
    v
  in
  let tree = Gen_engine.sample (Gen.map counting Gen.int) (state 3) in
  equal ~msg:"the root, at sampling" int 1 !calls;
  let n = Seq.length (Shrink_tree.children tree) in
  ignore (Seq.length (Shrink_tree.children tree));
  equal ~msg:"once per forced node, forced twice" int (1 + n) !calls;
  raises ~msg:"at sampling, out of sample" Exit (fun () ->
      Gen_engine.sample (Gen.map (fun _ -> raise Exit) Gen.int) (state 3));
  let tree =
    Gen_engine.sample
      (Gen.map (fun v -> if v = 0 then raise Exit else v) Gen.int)
      (state 3)
  in
  raises ~msg:"on a candidate, out of its forcing" Exit (fun () ->
      Shrink_tree.children tree ())

let an_exception_of_bind_escapes_the_forcing () =
  let gen =
    Gen.bind (Gen.int_range 0 10) (fun v ->
        if v = 0 then raise Exit else Gen.constant v)
  in
  let tree = find_sample gen (fun v -> v > 0) in
  raises Exit (fun () -> Shrink_tree.children tree ())

let run_returns_the_values_that_sample_draws () =
  let gen = Gen.(list ~size:(int_range 0 4) small_int) in
  for index = 0 to 9 do
    equal (list int)
      (root_value (Gen_engine.sample gen (state index)))
      (Shrink_tree.root (fst (Gen_engine.run gen (state index))))
  done

let render_formats_again_on_every_call () =
  let calls = ref 0 in
  let counting ppf v =
    incr calls;
    Format.pp_print_int ppf v
  in
  let tree = Gen_engine.sample (Gen.with_pp counting Gen.int) (state 0) in
  equal ~msg:"sampling formats nothing" int 0 !calls;
  ignore (render tree);
  ignore (render tree);
  equal ~msg:"each render formats" int 2 !calls;
  let long =
    Gen.with_pp
      (Pp.brackets (Pp.list Pp.int))
      (Gen.constant (List.init 40 (fun i -> 1000 + i)))
  in
  let lines =
    String.split_on_char '\n' (render (Gen_engine.sample long (state 0)))
  in
  is_true ~msg:"a long value breaks" (List.length lines > 1);
  List.iter
    (fun line ->
      at_most ~msg:"at the default margin" int ~than:78 (String.length line))
    lines

let a_raising_printer_renders_the_exception () =
  let rendered exn =
    render
      (Gen_engine.sample (Gen.with_pp (fun _ _ -> raise exn) Gen.nat) (state 0))
  in
  equal string "<printer raised Failure(\"boom\")>"
    (rendered (Stdlib.Failure "boom"));
  equal string "<printer raised windtrap timeout after 1.5s>"
    (rendered (Windtrap.Private.Failure.Control (`Timeout 1.5)));
  List.iter
    (fun exn ->
      match rendered exn with
      | exception raised ->
          is_true ~msg:(Printexc.to_string exn ^ " escapes") (raised == exn)
      | text -> failf "%s was rendered as %S" (Printexc.to_string exn) text)
    [ Sys.Break; Out_of_memory ];
  equal string "<printer raised Stack overflow>" (rendered Stack_overflow)

let make_without_pp_has_nothing_to_print () =
  let gen =
    Gen_engine.make (fun state ->
        ( Shrink_tree.make ~root:1 ~children:(Seq.return (Shrink_tree.leaf 0)),
          state ))
  in
  let tree = Gen_engine.sample gen (state 0) in
  equal ~msg:"the root" string placeholder (render tree);
  equal ~msg:"a candidate" string placeholder (render (first_child tree))

let contract_suite =
  [
    ( "a recorded seed replays these values",
      a_recorded_seed_replays_these_values );
    ( "a recorded seed replays these choices",
      a_recorded_seed_replays_these_choices );
    ("float_range draws inside its edges", float_range_draws_inside_its_edges);
    ( "float_range's origin is positive zero",
      float_range_origin_is_positive_zero );
    ( "int32 and int64 print their literal suffix",
      int32_and_int64_print_their_literal_suffix );
    ( "float prints the shortest round trip",
      float_prints_the_shortest_round_trip );
    ( "a float node has at most 15 candidates",
      a_float_node_has_at_most_15_candidates );
    ("nat strata are 50, 25, 20 and 5 percent", nat_strata_are_50_25_20_5);
    ( "float is uniform over the finite bit patterns",
      float_is_uniform_over_bit_patterns );
    ( "argument checks run in their stated order",
      argument_checks_run_in_their_stated_order );
    ( "a negative length names Gen.list, whoever raised",
      a_negative_length_names_gen_list );
    ( "unit and constant consume no randomness",
      unit_and_constant_consume_no_randomness );
    ("char prints with %C", char_prints_with_percent_c);
    ("bytes prints as Bytes.of_string", bytes_prints_as_bytes_of_string);
    ("list and array print their brackets", list_and_array_print_their_brackets);
    ( "the default length is drawn as nat draws",
      default_length_is_drawn_as_nat_draws );
    ( "a sized list offers prefixes, then element reductions from the left",
      sized_list_candidates_are_prefixes_then_element_reductions );
    ( "a rejected sized-list candidate is skipped",
      a_rejected_sized_list_candidate_is_skipped );
    ( "a negative candidate length raises at its forcing",
      a_negative_candidate_length_raises_at_its_forcing );
    ( "option, result and either draw at 0.15, 0.25 and 0.5",
      option_result_and_either_probabilities );
    ( "pair draws its first component first",
      pair_draws_its_first_component_first );
    ( "triple and quad shrink from the left",
      triple_and_quad_shrink_from_the_left );
    ( "one_of offers the earlier branches, then the drawn value's candidates",
      one_of_offers_earlier_branches_then_the_drawn_value );
    ( "frequency draws on the state as it stands and keeps its choice",
      frequency_draws_on_the_state_as_it_stands_and_keeps_its_choice );
    ("such_that draws at most 100 times", such_that_draws_at_most_100_times);
    ( "map runs f once per node, and its exceptions escape",
      map_runs_f_once_per_node_and_lets_its_exceptions_escape );
    ( "an exception of bind's function escapes the forcing",
      an_exception_of_bind_escapes_the_forcing );
    ( "run returns the values that sample draws",
      run_returns_the_values_that_sample_draws );
    ("render formats again on every call", render_formats_again_on_every_call);
    ( "a raising printer renders the exception",
      a_raising_printer_renders_the_exception );
    ( "make without pp has nothing to print",
      make_without_pp_has_nothing_to_print );
  ]

let suite =
  [
    ("same seed gives same value and render", same_seed_same_value_and_render);
    ("int shrinks to zero", int_shrinks_to_zero);
    ("int renders decimal", int_renders_decimal);
    ("nat stays below 10_000", nat_stays_below_10_000);
    ("nat shrinks to zero", nat_shrinks_to_zero);
    ("small_int is small and signed", small_int_is_small_and_signed);
    ( "int_range stays in bounds while shrinking",
      int_range_stays_in_bounds_while_shrinking );
    ("int_range degenerate is a leaf", int_range_degenerate_is_a_leaf);
    ("int_range full range works", int_range_full_range_works);
    ( "int_range invalid raises at sample time",
      int_range_invalid_raises_at_sample_time );
    ("greedy shrink finds the boundary", greedy_shrink_finds_boundary);
    ("int32 shrinks to zero", int32_shrinks_to_zero);
    ("int64 shrinks to zero", int64_shrinks_to_zero);
    ( "nativeint shrinks to zero and prints as a literal",
      nativeint_shrinks_to_zero_and_prints_as_a_literal );
    ("float is finite", float_is_finite);
    ("float_range stays in bounds", float_range_stays_in_bounds);
    ( "float_range invalid raises at sample time",
      float_range_invalid_raises_at_sample_time );
    ("unit generates and prints ()", unit_generates_and_prints_parentheses);
    ("bool shrinks true to false", bool_shrinks_true_to_false);
    ("char is uniform and shrinks to 'a'", char_is_uniform_and_shrinks_to_a);
    ( "char_range stays in bounds and shrinks toward 'a'",
      char_range_stays_in_bounds_and_shrinks_toward_a );
    ( "char_range outside 'a' shrinks to the nearest bound",
      char_range_outside_a_shrinks_to_nearest_bound );
    ( "char_range degenerate is a leaf and invalid raises",
      char_range_degenerate_is_a_leaf_and_invalid_raises );
    ( "string shrinks to empty and renders quoted",
      string_shrinks_to_empty_and_renders_quoted );
    ( "string_of respects the character generator",
      string_of_respects_character_generator );
    ( "string_of size keeps length in bounds",
      string_of_size_keeps_length_in_bounds );
    ( "string_of negative size raises at sample time",
      string_of_negative_size_raises_at_sample_time );
    ("bytes shrink to empty", bytes_shrink_to_empty);
    ("list shrinks structurally", list_shrinks_structurally);
    ( "list with size keeps length in bounds",
      list_with_size_keeps_length_in_bounds );
    ( "list negative size raises at sample time",
      list_negative_size_raises_at_sample_time );
    ("array shrinks to empty", array_shrinks_to_empty);
    ("option offers None first", option_offers_none_first);
    ("result generates both constructors", result_generates_both_constructors);
    ("either generates both constructors", either_generates_both_constructors);
    ("pair shrinks left first to zeroes", pair_shrinks_left_first_to_zeroes);
    ("triple and quad shrink to zeroes", triple_and_quad_shrink_to_zeroes);
    ( "bytes_of respects size and character generator",
      bytes_of_respects_size_and_character_generator );
    ( "constant is a leaf and asks for a printer",
      constant_is_a_leaf_and_asks_for_a_printer );
    ( "leaf printers feed the derivation law",
      leaf_printers_feed_the_derivation_law );
    ( "of_list picks uniformly and shrinks toward the head",
      of_list_picks_uniformly_and_shrinks_toward_head );
    ( "of_list singleton is a leaf and empty raises",
      of_list_singleton_is_a_leaf_and_empty_raises );
    ("one_of [] raises at sample time", one_of_empty_raises_at_sample_time);
    ( "one_of picks all branches and shrinks to earlier",
      one_of_picks_all_branches_and_shrinks_to_earlier );
    ( "one_of over printed branches derives a printer",
      one_of_over_printed_branches_derives_printer );
    ("frequency respects weights", frequency_respects_weights);
    ( "frequency over printed branches derives a printer",
      frequency_over_printed_branches_derives_printer );
    ( "frequency invalid raises at sample time",
      frequency_invalid_raises_at_sample_time );
    ( "such_that filters generation and shrinking",
      such_that_filters_generation_and_shrinking );
    ("such_that exhaustion is a discard", such_that_exhaustion_is_a_discard);
    ( "a discarding map candidate is skipped",
      a_discarding_map_candidate_is_skipped );
    ("map renders the pre-image", map_renders_the_pre_image);
    ( "nested maps render the outermost pre-image",
      nested_maps_render_the_outermost_pre_image );
    ("and+ renders as a pair", and_plus_renders_as_a_pair);
    ("containers carry pre-images", containers_carry_pre_images);
    ("no printer anywhere renders nothing", no_printer_anywhere_renders_nothing);
    ( "bind keeps inner constraints while shrinking",
      bind_keeps_inner_constraints_while_shrinking );
    ("bind renders by its inner", bind_renders_by_its_inner);
    ("letops compose", letops_compose);
    ("with_pp attaches a printer", with_pp_attaches_a_printer);
    ("mixed one_of derives no printer", mixed_one_of_derives_no_printer);
    ("shape generator prints only with_pp", shape_generator_prints_only_with_pp);
    ("render is total over shrink trees", render_is_total_over_shrink_trees);
    ("raising printer is contained", raising_printer_is_contained);
    ( "render_value reports printer presence",
      render_value_reports_printer_presence );
    ( "rejected bind candidates are skipped",
      rejected_bind_candidates_are_skipped );
    ( "rejected one_of candidates are skipped",
      rejected_one_of_candidates_are_skipped );
    ( "int_range negative bounds shrink to high",
      int_range_negative_bounds_shrink_to_high );
    ( "frequency zero-weight branch is never chosen",
      frequency_zero_weight_branch_is_never_chosen );
    ( "of_list candidate order is head then intermediates",
      of_list_candidate_order_is_head_then_intermediates );
    ( "char_range over the full byte span reaches the high half",
      char_range_full_byte_span_reaches_the_high_half );
    ( "such_that size constrains every candidate",
      such_that_size_constrains_every_candidate );
    ( "such_that over sized string keeps both constraints",
      such_that_over_sized_string_keeps_both_constraints );
    ( "with_pp overrides derived choice printer",
      with_pp_overrides_derived_choice_printer );
    ( "evidence-shaped identifier generator composes",
      evidence_shaped_identifier_generator_composes );
  ]

(* The generator laws, as properties

   Everything above walks a generator from fixed seeds: [samples] and
   [find_sample] draw at chosen indexes, and [shrink] runs the real search
   from one chosen tree. That is deliberate for the tests that must pin an
   exact candidate order or a specific distribution, but it pins
   behaviour at those seeds and says nothing about the rest of the space.

   The laws below are universally quantified statements, which is what
   [prop] is. They exercise the engine end to end under the run's own
   seed: draw, run the body, and (when one breaks) shrink through the
   real search and print a replayable seed. That is the property engine
   testing itself with the property engine, which is the point.

   Each generator is built so the law is checkable from the drawn value
   alone: bounds are drawn first and the value drawn inside them with
   [bind], so a counterexample carries its own parameters. *)

(* [lo, hi, v] with [lo <= hi] and [v] drawn from [int_range lo hi]. *)
let in_range_triple =
  (* [with_pp] on the [map]/[bind] results below: their pre-images would
     read, but the value itself is what a failing law should show. *)
  Gen.with_pp
    (fun ppf (lo, hi, v) -> Format.fprintf ppf "(%d, %d, %d)" lo hi v)
    (Gen.bind
       (Gen.pair (Gen.int_range (-1000) 1000) (Gen.int_range (-1000) 1000))
       (fun (a, b) ->
         let lo = min a b and hi = max a b in
         Gen.map (fun v -> (lo, hi, v)) (Gen.int_range lo hi)))

let law_tests =
  [
    prop "int_range draws inside its bounds" in_range_triple (fun (lo, hi, v) ->
        is_true
          ~msg:(Printf.sprintf "%d <= %d <= %d" lo v hi)
          (lo <= v && v <= hi));
    (* Printing must be total: a counterexample that cannot be rendered
       is a failure the reader never sees. This is the one law whose
       violation would corrupt the report itself. *)
    prop "every drawn value renders"
      (Gen.pair Gen.string (Gen.list Gen.int))
      (fun (s, xs) ->
        let rendered =
          Gen_engine.render_value
            (Gen.pair Gen.string (Gen.list Gen.int))
            (s, xs)
        in
        is_true ~msg:"rendering is non-empty" (String.length rendered > 0);
        is_false ~msg:"a pair of printable generators has no printer"
          (rendered = placeholder));
  ]

(* Shrink trees

   The tree the engine searches, tested on its own: laziness and
   memoization, the functor laws of [map], the candidate order of [pair]
   and [list], stack safety, and greedy-shrink termination. Nothing here
   draws from a seed; the trees are built by hand with [make] and [leaf]. *)

module Shrink_tree_suite = struct
  exception Forced_children

  let show_int_list values =
    values |> List.map string_of_int |> String.concat "; "
    |> Printf.sprintf "[%s]"

  let show_int_lists values =
    values |> List.map show_int_list |> String.concat "; "
    |> Printf.sprintf "[%s]"

  let rec sequence_to_list sequence =
    match sequence () with
    | Seq.Nil -> []
    | Seq.Cons (value, tail) -> value :: sequence_to_list tail

  let take count sequence =
    let rec loop remaining sequence values =
      if remaining = 0 then List.rev values
      else
        match sequence () with
        | Seq.Nil -> List.rev values
        | Seq.Cons (value, tail) -> loop (remaining - 1) tail (value :: values)
    in
    loop count sequence []

  let drop count sequence =
    let rec loop remaining sequence =
      if remaining = 0 then sequence
      else
        match sequence () with
        | Seq.Nil -> Seq.empty
        | Seq.Cons (_, tail) -> loop (remaining - 1) tail
    in
    loop count sequence

  let child_roots tree =
    Shrink_tree.children tree |> sequence_to_list |> List.map Shrink_tree.root

  type 'a observed = Node of 'a * 'a observed list

  let rec observe tree =
    let children =
      Shrink_tree.children tree |> sequence_to_list |> List.map observe
    in
    Node (Shrink_tree.root tree, children)

  let node root children =
    Shrink_tree.make ~root ~children:(List.to_seq children)

  let finite_tree () =
    node 10 [ node 4 [ Shrink_tree.leaf 0; Shrink_tree.leaf 2 ]; node 8 [] ]

  (* Laziness and memoization *)

  let root_and_leaf_do_not_force_children () =
    let calls = ref 0 in
    let tree =
      Shrink_tree.make ~root:42 ~children:(fun () ->
          incr calls;
          Seq.Nil)
    in
    is_true
      ~msg:(Printf.sprintf "make forced children %d times" !calls)
      (!calls = 0);
    is_true
      ~msg:
        (Printf.sprintf "root returned %d instead of 42" (Shrink_tree.root tree))
      (Shrink_tree.root tree = 42);
    is_true
      ~msg:(Printf.sprintf "root forced children %d times" !calls)
      (!calls = 0);
    let leaf = Shrink_tree.leaf 7 in
    is_true ~msg:"leaf root was not retained" (Shrink_tree.root leaf = 7);
    is_true ~msg:"leaf unexpectedly had a child"
      (match Shrink_tree.children leaf () with
      | Seq.Nil -> true
      | Seq.Cons _ -> false)

  let child_head_is_cached_and_physically_reused () =
    let calls = ref 0 in
    let child = Shrink_tree.leaf 1 in
    let tree =
      Shrink_tree.make ~root:2 ~children:(fun () ->
          incr calls;
          Seq.Cons (child, Seq.empty))
    in
    let children = Shrink_tree.children tree in
    let first = children () in
    let second = children () in
    is_true
      ~msg:
        (Printf.sprintf "child head evaluated %d times instead of once" !calls)
      (!calls = 1);
    match (first, second) with
    | Seq.Cons (left, _), Seq.Cons (right, _) ->
        is_true ~msg:"first force did not return the supplied child"
          (left == child);
        is_true ~msg:"second force did not reuse the supplied child"
          (right == child)
    | Seq.Nil, _ | _, Seq.Nil -> failf "cached child disappeared"

  let sequence_tails_are_independently_lazy_and_cached () =
    let head_calls = ref 0 in
    let tail_calls = ref 0 in
    let tail () =
      incr tail_calls;
      Seq.Cons (Shrink_tree.leaf 2, Seq.empty)
    in
    let tree =
      Shrink_tree.make ~root:0 ~children:(fun () ->
          incr head_calls;
          Seq.Cons (Shrink_tree.leaf 1, tail))
    in
    let children = Shrink_tree.children tree in
    let tail =
      match children () with
      | Seq.Nil -> failf "missing first child"
      | Seq.Cons (_, tail) -> tail
    in
    is_true
      ~msg:(Printf.sprintf "head evaluated %d times" !head_calls)
      (!head_calls = 1);
    is_true ~msg:"head force also forced its tail" (!tail_calls = 0);
    ignore (children ());
    is_true ~msg:"head cache was not reused" (!head_calls = 1);
    let first_tail = tail () in
    let second_tail = tail () in
    is_true
      ~msg:
        (Printf.sprintf "tail evaluated %d times instead of once" !tail_calls)
      (!tail_calls = 1);
    match (first_tail, second_tail) with
    | Seq.Cons (left, _), Seq.Cons (right, _) ->
        is_true ~msg:"tail did not physically reuse its child" (left == right)
    | Seq.Nil, _ | _, Seq.Nil -> failf "cached tail disappeared"

  let repeated_head_observations_share_the_successful_tail () =
    let head_calls = ref 0 in
    let tail_calls = ref 0 in
    let tail_child = Shrink_tree.leaf 2 in
    let source_tail () =
      incr tail_calls;
      Seq.Cons (tail_child, Seq.empty)
    in
    let tree =
      Shrink_tree.make ~root:0 ~children:(fun () ->
          incr head_calls;
          Seq.Cons (Shrink_tree.leaf 1, source_tail))
    in
    let children = Shrink_tree.children tree in
    let first_tail =
      match children () with
      | Seq.Nil -> failf "first head observation was empty"
      | Seq.Cons (_, tail) -> tail
    in
    let second_tail =
      match children () with
      | Seq.Nil -> failf "second head observation was empty"
      | Seq.Cons (_, tail) -> tail
    in
    let tails_are_physically_shared = first_tail == second_tail in
    let first_node = first_tail () in
    let second_node = second_tail () in
    is_true
      ~msg:(Printf.sprintf "shared-tail head evaluated %d times" !head_calls)
      (!head_calls = 1);
    is_true
      ~msg:
        (Printf.sprintf "shared successful tail evaluated %d times" !tail_calls)
      (!tail_calls = 1);
    is_true ~msg:"repeated head observations returned different tail closures"
      tails_are_physically_shared;
    match (first_node, second_node) with
    | Seq.Cons (left, _), Seq.Cons (right, _) ->
        is_true ~msg:"first tail force returned a different child"
          (left == tail_child);
        is_true ~msg:"second tail force did not reuse its child"
          (right == tail_child)
    | Seq.Nil, _ | _, Seq.Nil -> failf "shared successful tail disappeared"

  let repeated_nil_force_is_cached () =
    let calls = ref 0 in
    let tree =
      Shrink_tree.make ~root:0 ~children:(fun () ->
          incr calls;
          Seq.Nil)
    in
    let children = Shrink_tree.children tree in
    for _ = 1 to 2 do
      match children () with
      | Seq.Nil -> ()
      | Seq.Cons _ -> failf "empty source unexpectedly returned a child"
    done;
    is_true
      ~msg:
        (Printf.sprintf "empty source evaluated %d times instead of once" !calls)
      (!calls = 1)

  let forcing_exception_is_cached () =
    let calls = ref 0 in
    let error = Forced_children in
    let tree =
      Shrink_tree.make ~root:0 ~children:(fun () ->
          incr calls;
          raise error)
    in
    let children = Shrink_tree.children tree in
    let force () =
      match children () with
      | _ -> failf "exceptional children unexpectedly returned"
      | exception caught ->
          is_true ~msg:"forcing reraised a different exception value"
            (caught == error)
    in
    force ();
    force ();
    is_true
      ~msg:(Printf.sprintf "exceptional source evaluated %d times" !calls)
      (!calls = 1)

  let repeated_head_observations_share_the_exceptional_tail () =
    let head_calls = ref 0 in
    let tail_calls = ref 0 in
    let error = Forced_children in
    let tree =
      Shrink_tree.make ~root:0 ~children:(fun () ->
          incr head_calls;
          Seq.Cons
            ( Shrink_tree.leaf 1,
              fun () ->
                incr tail_calls;
                raise error ))
    in
    let children = Shrink_tree.children tree in
    let first_tail =
      match children () with
      | Seq.Nil -> failf "first head before exceptional tail was missing"
      | Seq.Cons (_, tail) -> tail
    in
    let second_tail =
      match children () with
      | Seq.Nil -> failf "second head before exceptional tail was missing"
      | Seq.Cons (_, tail) -> tail
    in
    let tails_are_physically_shared = first_tail == second_tail in
    let force tail =
      match tail () with
      | _ -> failf "exceptional tail unexpectedly returned"
      | exception caught ->
          is_true ~msg:"exceptional tail reraised a different exception value"
            (caught == error)
    in
    force first_tail;
    force second_tail;
    is_true
      ~msg:(Printf.sprintf "head source evaluated %d times" !head_calls)
      (!head_calls = 1);
    is_true
      ~msg:(Printf.sprintf "exceptional tail evaluated %d times" !tail_calls)
      (!tail_calls = 1);
    is_true
      ~msg:"repeated head observations returned different exceptional tails"
      tails_are_physically_shared

  (* map *)

  let map_preserves_shape_and_order () =
    let actual = Shrink_tree.map (fun value -> value * 3) (finite_tree ()) in
    let expected =
      Node (30, [ Node (12, [ Node (0, []); Node (6, []) ]); Node (24, []) ])
    in
    is_true ~msg:"map changed finite tree shape or order"
      (observe actual = expected)

  let map_obeys_identity_and_composition () =
    let tree = finite_tree () in
    is_true ~msg:"map identity law failed"
      (observe (Shrink_tree.map Fun.id tree) = observe tree);
    let f value = value + 3 in
    let g value = value * 2 in
    let separate = Shrink_tree.map f (Shrink_tree.map g tree) in
    let composed = Shrink_tree.map (fun value -> f (g value)) tree in
    is_true ~msg:"map composition law failed"
      (observe separate = observe composed)

  let map_is_lazy_and_maps_each_node_once () =
    let source_calls = ref 0 in
    let map_calls = ref 0 in
    let source =
      Shrink_tree.make ~root:10 ~children:(fun () ->
          incr source_calls;
          Seq.Cons (Shrink_tree.leaf 5, Seq.empty))
    in
    let mapped =
      Shrink_tree.map
        (fun value ->
          incr map_calls;
          value + 1)
        source
    in
    is_true ~msg:"map did not evaluate exactly the root at construction"
      (!map_calls = 1);
    is_true ~msg:"map construction forced source children" (!source_calls = 0);
    let children = Shrink_tree.children mapped in
    let first = children () in
    is_true
      ~msg:
        (Printf.sprintf "mapped child force evaluated source %d times"
           !source_calls)
      (!source_calls = 1);
    is_true
      ~msg:
        (Printf.sprintf "mapped child was evaluated %d total times" !map_calls)
      (!map_calls = 2);
    ignore (children ());
    is_true ~msg:"repeated mapped force reran source" (!source_calls = 1);
    is_true ~msg:"repeated mapped force reran mapping" (!map_calls = 2);
    match first with
    | Seq.Cons (child, _) ->
        is_true ~msg:"mapped child root was incorrect"
          (Shrink_tree.root child = 6)
    | Seq.Nil -> failf "mapped child was missing"

  let mapped_child_exception_is_cached () =
    let source_calls = ref 0 in
    let map_calls = ref 0 in
    let source =
      Shrink_tree.make ~root:1 ~children:(fun () ->
          incr source_calls;
          Seq.Cons (Shrink_tree.leaf 2, Seq.empty))
    in
    let mapped =
      Shrink_tree.map
        (fun value ->
          incr map_calls;
          if value = 2 then raise Forced_children else value)
        source
    in
    let children = Shrink_tree.children mapped in
    for _ = 1 to 2 do
      match children () with
      | _ -> failf "mapped exceptional child unexpectedly returned"
      | exception Forced_children -> ()
    done;
    is_true
      ~msg:
        (Printf.sprintf "exceptional mapped source ran %d times" !source_calls)
      (!source_calls = 1);
    is_true
      ~msg:(Printf.sprintf "exceptional mapper ran %d times" !map_calls)
      (!map_calls = 2)

  (* pair *)

  let pair_reduces_left_before_right () =
    let left = node 10 [ Shrink_tree.leaf 0; Shrink_tree.leaf 5 ] in
    let right = node 20 [ Shrink_tree.leaf 2; Shrink_tree.leaf 4 ] in
    let tree = Shrink_tree.pair left right in
    is_true ~msg:"pair root did not combine input roots"
      (Shrink_tree.root tree = (10, 20));
    let roots = child_roots tree in
    is_true ~msg:"pair child order was not left-before-right"
      (roots = [ (0, 20); (5, 20); (10, 2); (10, 4) ])

  let pair_does_not_force_right_until_left_is_exhausted () =
    let left_head_calls = ref 0 in
    let left_tail_calls = ref 0 in
    let right_calls = ref 0 in
    let left =
      Shrink_tree.make ~root:1 ~children:(fun () ->
          incr left_head_calls;
          Seq.Cons
            ( Shrink_tree.leaf 0,
              fun () ->
                incr left_tail_calls;
                Seq.Nil ))
    in
    let right =
      Shrink_tree.make ~root:2 ~children:(fun () ->
          incr right_calls;
          Seq.Cons (Shrink_tree.leaf 0, Seq.empty))
    in
    let children = Shrink_tree.children (Shrink_tree.pair left right) in
    let tail =
      match children () with
      | Seq.Nil -> failf "pair lost its left child"
      | Seq.Cons (candidate, tail) ->
          is_true ~msg:"pair's first child was not from the left"
            (Shrink_tree.root candidate = (0, 2));
          tail
    in
    is_true
      ~msg:(Printf.sprintf "left head count was %d" !left_head_calls)
      (!left_head_calls = 1);
    is_true ~msg:"left head forced its tail" (!left_tail_calls = 0);
    is_true ~msg:"right forced before left exhaustion" (!right_calls = 0);
    ignore (tail ());
    is_true ~msg:"left tail was not exhausted exactly once"
      (!left_tail_calls = 1);
    is_true ~msg:"right was not forced after left exhaustion" (!right_calls = 1)

  let pair_is_natural_under_map () =
    let left = node 4 [ Shrink_tree.leaf 0; Shrink_tree.leaf 2 ] in
    let right = node 7 [ Shrink_tree.leaf 1 ] in
    let direct =
      Shrink_tree.pair
        (Shrink_tree.map (fun value -> value + 1) left)
        (Shrink_tree.map (fun value -> value * 2) right)
    in
    let combined =
      Shrink_tree.map
        (fun (left, right) -> (left + 1, right * 2))
        (Shrink_tree.pair left right)
    in
    is_true ~msg:"pair/map naturality law failed"
      (observe direct = observe combined)

  (* list *)

  let list_has_exact_structural_then_element_order () =
    let first = node 10 [ Shrink_tree.leaf 0; Shrink_tree.leaf 5 ] in
    let second = node 20 [ Shrink_tree.leaf 2 ] in
    let tree = Shrink_tree.list [ first; second ] in
    is_true ~msg:"list root did not preserve input order"
      (Shrink_tree.root tree = [ 10; 20 ]);
    let roots = child_roots tree in
    let expected = [ []; [ 20 ]; [ 10 ]; [ 0; 20 ]; [ 5; 20 ]; [ 10; 2 ] ] in
    is_true
      ~msg:
        (Printf.sprintf "expected list children %s, got %s"
           (show_int_lists expected) (show_int_lists roots))
      (roots = expected)

  let list_chunk_schedule_is_deterministic_without_duplicates () =
    let tree =
      [ 1; 2; 3; 4; 5 ] |> List.map Shrink_tree.leaf |> Shrink_tree.list
    in
    let roots = child_roots tree in
    let expected =
      [
        [];
        [ 5 ];
        [ 3; 4; 5 ];
        [ 1; 2; 5 ];
        [ 2; 3; 4; 5 ];
        [ 1; 3; 4; 5 ];
        [ 1; 2; 4; 5 ];
        [ 1; 2; 3; 5 ];
        [ 1; 2; 3; 4 ];
      ]
    in
    is_true
      ~msg:
        (Printf.sprintf "expected chunk schedule %s, got %s"
           (show_int_lists expected) (show_int_lists roots))
      (roots = expected);
    is_true ~msg:"list structural schedule contained duplicate candidates"
      (List.length roots = List.length (List.sort_uniq compare roots))

  let singleton_list_has_no_chunk_removal () =
    (* Length 1 admits no power-of-two chunk strictly below it: the structural
       candidates are exactly the empty list, then element reductions. *)
    let tree = Shrink_tree.list [ node 7 [ Shrink_tree.leaf 3 ] ] in
    is_true ~msg:"singleton root was not [7]" (Shrink_tree.root tree = [ 7 ]);
    let roots = child_roots tree in
    let expected = [ []; [ 3 ] ] in
    is_true
      ~msg:
        (Printf.sprintf "expected singleton children %s, got %s"
           (show_int_lists expected) (show_int_lists roots))
      (roots = expected)

  let list_length_three_uses_uniform_chunking () =
    (* v1's donor special-cased lists shorter than four; v3 must chunk
       uniformly: one removal of chunk two, then the three singles. *)
    let tree = [ 1; 2; 3 ] |> List.map Shrink_tree.leaf |> Shrink_tree.list in
    let roots = child_roots tree in
    let expected = [ []; [ 3 ]; [ 2; 3 ]; [ 1; 3 ]; [ 1; 2 ] ] in
    is_true
      ~msg:
        (Printf.sprintf "expected length-3 schedule %s, got %s"
           (show_int_lists expected) (show_int_lists roots))
      (roots = expected)

  let empty_list_is_a_leaf () =
    let tree = Shrink_tree.list [] in
    is_true ~msg:"empty list root was not empty" (Shrink_tree.root tree = []);
    is_true ~msg:"empty list had a candidate"
      (match Shrink_tree.children tree () with
      | Seq.Nil -> true
      | Seq.Cons _ -> false)

  let list_defers_element_children_until_structure_is_exhausted () =
    let first_calls = ref 0 in
    let second_calls = ref 0 in
    let first =
      Shrink_tree.make ~root:1 ~children:(fun () ->
          incr first_calls;
          Seq.Cons (Shrink_tree.leaf 0, Seq.empty))
    in
    let second =
      Shrink_tree.make ~root:2 ~children:(fun () ->
          incr second_calls;
          Seq.Cons (Shrink_tree.leaf 0, Seq.empty))
    in
    let children = Shrink_tree.children (Shrink_tree.list [ first; second ]) in
    let structural = take 3 children |> List.map Shrink_tree.root in
    is_true ~msg:"two-element structural prefix was incorrect"
      (structural = [ []; [ 2 ]; [ 1 ] ]);
    is_true ~msg:"structural prefix forced first element children"
      (!first_calls = 0);
    is_true ~msg:"structural prefix forced second element children"
      (!second_calls = 0);
    let element_tail = drop 3 children in
    let candidate =
      match element_tail () with
      | Seq.Nil -> failf "missing first element reduction"
      | Seq.Cons (candidate, _) -> candidate
    in
    is_true ~msg:"first element reduction had the wrong root"
      (Shrink_tree.root candidate = [ 0; 2 ]);
    is_true
      ~msg:(Printf.sprintf "first element source ran %d times" !first_calls)
      (!first_calls = 1);
    is_true ~msg:"first element reduction forced second element"
      (!second_calls = 0)

  let list_element_sources_are_forced_once_while_scanning () =
    let first_calls = ref 0 in
    let second_calls = ref 0 in
    let first =
      Shrink_tree.make ~root:1 ~children:(fun () ->
          incr first_calls;
          Seq.Nil)
    in
    let second =
      Shrink_tree.make ~root:2 ~children:(fun () ->
          incr second_calls;
          Seq.Cons (Shrink_tree.leaf 0, Seq.empty))
    in
    let children = Shrink_tree.children (Shrink_tree.list [ first; second ]) in
    let element_tail = drop 3 children in
    let first_force = element_tail () in
    let second_force = element_tail () in
    is_true
      ~msg:
        (Printf.sprintf "empty first element source ran %d times" !first_calls)
      (!first_calls = 1);
    is_true
      ~msg:(Printf.sprintf "second element source ran %d times" !second_calls)
      (!second_calls = 1);
    match (first_force, second_force) with
    | Seq.Cons (left, _), Seq.Cons (right, _) ->
        is_true ~msg:"list did not cache its first element candidate"
          (left == right);
        is_true ~msg:"list scanned to an incorrect element candidate"
          (Shrink_tree.root left = [ 1; 0 ])
    | Seq.Nil, _ | _, Seq.Nil -> failf "list lost its scanned element candidate"

  let list_is_stack_safe_for_large_flat_inputs () =
    let count = 100_000 in
    let trees = List.init count Shrink_tree.leaf in
    let tree = Shrink_tree.list trees in
    is_true ~msg:"large list root had the wrong length"
      (List.length (Shrink_tree.root tree) = count);
    match Shrink_tree.children tree () with
    | Seq.Nil -> failf "large non-empty list had no structural reduction"
    | Seq.Cons (candidate, _) ->
        is_true ~msg:"large list's first candidate was not empty"
          (Shrink_tree.root candidate = [])

  let infinite_depth_is_incrementally_forceable () =
    let source_calls = ref 0 in
    let rec ascending value =
      Shrink_tree.make ~root:value ~children:(fun () ->
          incr source_calls;
          Seq.Cons (ascending (value + 1), Seq.empty))
    in
    let current = ref (Shrink_tree.map succ (ascending 0)) in
    let depth = 50_000 in
    for expected = 1 to depth do
      is_true
        ~msg:
          (Printf.sprintf "deep mapped root was %d instead of %d"
             (Shrink_tree.root !current)
             expected)
        (Shrink_tree.root !current = expected);
      if expected < depth then
        match Shrink_tree.children !current () with
        | Seq.Nil -> failf "infinite tree ended at depth %d" expected
        | Seq.Cons (child, _) -> current := child
    done;
    is_true
      ~msg:
        (Printf.sprintf "deep traversal forced %d source cells instead of %d"
           !source_calls (depth - 1))
      (!source_calls = depth - 1)

  let greedy_shrink_terminates_at_a_local_minimum () =
    (* Integer trees whose candidates count down by one: [n]'s candidates are
       [0] then [n - 1], recursively. *)
    let rec int_tree value =
      Shrink_tree.make ~root:value ~children:(fun () ->
          if value = 0 then Seq.Nil
          else
            Seq.Cons
              ( Shrink_tree.leaf 0,
                fun () -> Seq.Cons (int_tree (value - 1), Seq.empty) ))
    in
    let fails values = List.exists (fun value -> value >= 5) values in
    let start = Shrink_tree.list (List.map int_tree [ 9; 7; 5; 9 ]) in
    is_true ~msg:"starting counterexample must fail"
      (fails (Shrink_tree.root start));
    (* The engine's own search over this tree: a generator that draws it. *)
    let gen =
      Gen_engine.make
        ~pp:(fun ppf values ->
          Format.pp_print_string ppf (show_int_list values))
        (fun state -> (start, state))
    in
    let minimum, steps = shrink ~failing:fails gen in
    equal ~msg:"the search stops at the local minimum" string "[5]" minimum;
    at_most ~msg:"the search terminates promptly" int ~than:20 steps

  let list_root_forces_no_cell () =
    let forced = ref 0 in
    let element root =
      Shrink_tree.make ~root ~children:(fun () ->
          incr forced;
          Seq.Nil)
    in
    let tree = Shrink_tree.list [ element 1; element 2; element 3 ] in
    equal ~msg:"the root" (list int) [ 1; 2; 3 ] (Shrink_tree.root tree);
    equal ~msg:"no element cell was forced" int 0 !forced

  (* Every operation of the module, then one draw of the global state against
     a copy taken before them: a random operation would have moved it. *)
  let performs_no_random_operation () =
    let before = Random.get_state () in
    let rec countdown n =
      Shrink_tree.make ~root:n ~children:(fun () ->
          if n = 0 then Seq.Nil else Seq.Cons (countdown (n - 1), Seq.empty))
    in
    let tree =
      Shrink_tree.pair
        (Shrink_tree.map succ (countdown 4))
        (Shrink_tree.list [ countdown 2; Shrink_tree.leaf 9 ])
    in
    let rec visit tree = Seq.iter visit (Shrink_tree.children tree) in
    visit tree;
    equal ~msg:"the next global draw is the one due" int
      (Random.State.bits before) (Random.bits ())

  let suite =
    [
      ("list's root forces no cell", list_root_forces_no_cell);
      ("performs no random operation", performs_no_random_operation);
      ( "root and leaf do not force children",
        root_and_leaf_do_not_force_children );
      ( "child head is cached and physically reused",
        child_head_is_cached_and_physically_reused );
      ( "sequence tails are independently lazy and cached",
        sequence_tails_are_independently_lazy_and_cached );
      ( "repeated head observations share the successful tail",
        repeated_head_observations_share_the_successful_tail );
      ("repeated Nil force is cached", repeated_nil_force_is_cached);
      ("forcing exception is cached", forcing_exception_is_cached);
      ( "repeated head observations share the exceptional tail",
        repeated_head_observations_share_the_exceptional_tail );
      ("map preserves shape and order", map_preserves_shape_and_order);
      ("map obeys identity and composition", map_obeys_identity_and_composition);
      ( "map is lazy and maps each node once",
        map_is_lazy_and_maps_each_node_once );
      ("mapped child exception is cached", mapped_child_exception_is_cached);
      ("pair reduces left before right", pair_reduces_left_before_right);
      ( "pair does not force right until left is exhausted",
        pair_does_not_force_right_until_left_is_exhausted );
      ("pair is natural under map", pair_is_natural_under_map);
      ( "list has exact structural then element order",
        list_has_exact_structural_then_element_order );
      ( "list chunk schedule is deterministic without duplicates",
        list_chunk_schedule_is_deterministic_without_duplicates );
      ( "singleton list has no chunk removal",
        singleton_list_has_no_chunk_removal );
      ( "list length three uses uniform chunking",
        list_length_three_uses_uniform_chunking );
      ("empty list is a leaf", empty_list_is_a_leaf);
      ( "list defers element children until structure is exhausted",
        list_defers_element_children_until_structure_is_exhausted );
      ( "list element sources are forced once while scanning",
        list_element_sources_are_forced_once_while_scanning );
      ( "list is stack safe for large flat inputs",
        list_is_stack_safe_for_large_flat_inputs );
      ( "infinite depth is incrementally forceable",
        infinite_depth_is_incrementally_forceable );
      ( "greedy shrink terminates at a local minimum",
        greedy_shrink_terminates_at_a_local_minimum );
    ]

  let tests = List.map (fun (name, fn) -> test name fn) suite
end

let tests =
  List.map (fun (name, fn) -> test name fn) (suite @ contract_suite)
  @ [ group "shrink tree" Shrink_tree_suite.tests ]
  @ law_tests

let () = exit @@ Windtrap.run "gen" tests
