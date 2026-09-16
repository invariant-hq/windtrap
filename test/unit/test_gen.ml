(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(* Tests for Gen: determinism under fixed seeds, distribution smoke tests,
   integrated-shrinking invariants (candidates satisfy generator
   constraints), greedy-shrink termination and minima, printing totality
   including the printerless placeholder, and the shrink tree itself
   ([Gen.Private.Shrink_tree]) in its own submodule below. *)

open Windtrap
module Seed = Windtrap.Private.Seed
module Shrink_tree = Windtrap.Gen.Private.Shrink_tree
module Pp = Windtrap.Private.Pp

(* Printf-style shims over windtrap's [fail], preserving the bodies'
   [check cond "fmt" args] and [failf "fmt" args] call shape. *)
let failf format = Printf.ksprintf (fun message -> fail message) format

let check condition format =
  Printf.ksprintf (fun message -> if not condition then fail message) format

let starts_with prefix text = String.starts_with ~prefix text

let show_ints values =
  "[" ^ String.concat "; " (List.map string_of_int values) ^ "]"

let show_int_lists lists = String.concat " " (List.map show_ints lists)

(* One fixed root for the whole suite; per-test streams come from indexes.
   Everything below is deterministic across runs and machines. *)
let root = 0x00c0ffee1234abcdL
let state index = Seed.make (Seed.derive ~root ~path:"test_gen" ~index)
let root_value tree = Gen.Private.value (Shrink_tree.root tree)
let rendering tree = Gen.Private.render (Shrink_tree.root tree)

(* The one placeholder a value with no printer renders as, spelled here so
   a drift in [Gen]'s spelling is a failure and not a silently passing
   [contains]. *)
let placeholder = "<no printer: attach one with Gen.with_pp>"

(* The report's spelling of each rendering, for assertions on a value: a
   pre-image is marked so an assertion on the value cannot pass on it. *)
let render tree =
  match rendering tree with
  | Gen.Private.Value text -> text
  | Pre_image text -> "from " ^ text

let samples gen count =
  List.init count (fun index ->
      root_value (Gen.Private.sample gen (state index)))

let find_sample ?(max_index = 10_000) gen accept =
  let rec loop index =
    if index >= max_index then
      failf "no matching sample within %d cases" max_index
    else
      let tree = Gen.Private.sample gen (state index) in
      if accept (root_value tree) then tree else loop (index + 1)
  in
  loop 0

(* Greedy integrated shrinking, the engine's strategy: repeatedly move to
   the first candidate that still satisfies [failing]. Returns the local
   minimum and the number of steps. *)
let minimize ?(max_steps = 1_000) failing tree =
  let rec loop steps tree =
    if steps > max_steps then failf "greedy shrink exceeded %d steps" max_steps
    else
      match
        Seq.find
          (fun child -> failing (root_value child))
          (Shrink_tree.children tree)
      with
      | None -> (root_value tree, steps)
      | Some child -> loop (steps + 1) child
  in
  loop 0 tree

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
      let once = Gen.Private.sample gen (state index) in
      let twice = Gen.Private.sample gen (state index) in
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
      check (once = twice) "%s: same seed rendered %S then %S" name once twice)
    against

let different_indexes_vary () =
  let values = samples Gen.int 20 in
  let distinct = List.sort_uniq compare values in
  check
    (List.length distinct > 10)
    "expected variety across indexes, got %d distinct of 20"
    (List.length distinct)

(* Integer generators *)

let int_shrinks_to_zero () =
  let tree = find_sample Gen.int (fun v -> v <> 0) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 0) "int minimized to %d, not 0" minimum

let int_renders_decimal () =
  let tree = Gen.Private.sample Gen.int (state 0) in
  let rendered = render tree in
  check
    (rendered = string_of_int (root_value tree))
    "int rendered %S for %d" rendered (root_value tree)

let nat_distribution_is_stratified () =
  let values = samples Gen.nat 1_000 in
  List.iter (fun v -> check (v >= 0 && v < 10_000) "nat produced %d" v) values;
  let below_ten = List.length (List.filter (fun v -> v < 10) values) in
  let above_thousand = List.length (List.filter (fun v -> v >= 1_000) values) in
  check (below_ten >= 300) "only %d of 1000 nats below 10" below_ten;
  check (above_thousand >= 1) "no nat above 1000 in 1000 draws (%d)"
    above_thousand

let nat_shrinks_to_zero () =
  let tree = find_sample Gen.nat (fun v -> v > 0) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 0) "nat minimized to %d" minimum

let small_int_is_small_and_signed () =
  let values = samples Gen.small_int 500 in
  List.iter
    (fun v -> check (v > -10_000 && v < 10_000) "small_int produced %d" v)
    values;
  check (List.exists (fun v -> v < 0) values) "no negative small_int in 500";
  check (List.exists (fun v -> v > 0) values) "no positive small_int in 500";
  let tree = find_sample Gen.small_int (fun v -> v < 0) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 0) "small_int minimized to %d" minimum

let int_range_stays_in_bounds_while_shrinking () =
  let gen = Gen.int_range 10 100 in
  for index = 0 to 19 do
    let tree = Gen.Private.sample gen (state index) in
    explore ~limit:200 tree (fun v ->
        check (v >= 10 && v <= 100) "int_range candidate %d out of bounds" v)
  done;
  let tree = find_sample gen (fun v -> v > 10) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 10) "int_range 10 100 minimized to %d, not 10" minimum

let int_range_degenerate_is_a_leaf () =
  let tree = Gen.Private.sample (Gen.int_range 5 5) (state 0) in
  check (root_value tree = 5) "int_range 5 5 produced %d" (root_value tree);
  check (no_children tree) "int_range 5 5 has shrink candidates"

let int_range_full_range_works () =
  let gen = Gen.int_range min_int max_int in
  let tree = find_sample gen (fun v -> v <> 0) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 0) "full int_range minimized to %d" minimum

let int_range_invalid_raises_at_sample_time () =
  let gen = Gen.int_range 10 (-10) in
  (* Construction succeeded; sampling reports the error. *)
  match Gen.Private.sample gen (state 0) with
  | exception Invalid_argument _ -> ()
  | _ -> failf "int_range 10 (-10) sampled successfully"

let greedy_shrink_finds_boundary () =
  let gen = Gen.int_range 0 1000 in
  let tree = find_sample gen (fun v -> v > 50) in
  let minimum, _ = minimize (fun v -> v > 50) tree in
  check (minimum = 51) "boundary shrink reached %d, not 51" minimum

let int32_shrinks_to_zero () =
  let tree = find_sample Gen.int32 (fun v -> not (Int32.equal v 0l)) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (Int32.equal minimum 0l) "int32 minimized to %ld" minimum

let int64_shrinks_to_zero () =
  let tree = find_sample Gen.int64 (fun v -> not (Int64.equal v 0L)) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (Int64.equal minimum 0L) "int64 minimized to %Ld" minimum

let nativeint_shrinks_to_zero_and_prints_as_a_literal () =
  let tree = find_sample Gen.nativeint (fun v -> not (Nativeint.equal v 0n)) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (Nativeint.equal minimum 0n) "nativeint minimized to %nd" minimum;
  let rendered = render tree in
  check
    (rendered = Printf.sprintf "%ndn" (root_value tree))
    "nativeint rendered %S" rendered;
  let values = samples Gen.nativeint 100 in
  check
    (List.exists (fun v -> Nativeint.compare v 0n < 0) values)
    "no negative nativeint in 100 draws";
  (* The full native word, not an [int32] widened: on a 64-bit platform a
     hundred uniform draws cannot all fit in 32 bits. *)
  if Nativeint.size = 64 then
    check
      (List.exists
         (fun v ->
           Nativeint.compare (Nativeint.abs v)
             (Nativeint.of_int32 Int32.max_int)
           > 0)
         values)
      "100 nativeint draws all fit in 32 bits"

(* Float generators *)

let float_is_finite () =
  List.iter
    (fun v -> check (Float.is_finite v) "float produced %h" v)
    (samples Gen.float 500);
  let tree = find_sample Gen.float (fun v -> v <> 0.0) in
  let minimum, steps = minimize (fun _ -> true) tree in
  check (minimum = 0.0) "float minimized to %h in %d steps" minimum steps

let float_range_stays_in_bounds () =
  let gen = Gen.float_range 2.0 5.0 in
  for index = 0 to 19 do
    let tree = Gen.Private.sample gen (state index) in
    explore ~limit:100 tree (fun v ->
        check (v >= 2.0 && v <= 5.0) "float_range candidate %h out of bounds" v)
  done;
  let tree = Gen.Private.sample gen (state 0) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 2.0) "float_range 2 5 minimized to %h" minimum;
  let negative = Gen.float_range (-5.0) (-2.0) in
  let tree = Gen.Private.sample negative (state 1) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = -2.0) "float_range -5 -2 minimized to %h" minimum

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
      match Gen.Private.sample gen (state 0) with
      | exception Invalid_argument _ -> ()
      | _ -> failf "float_range (%s) sampled successfully" name)
    cases

(* Unit, booleans, characters, strings *)

let unit_generates_and_prints_parentheses () =
  let tree = Gen.Private.sample Gen.unit (state 0) in
  check (no_children tree) "unit has shrink candidates";
  let rendered = render tree in
  check (rendered = "()") "unit rendered %S" rendered;
  check (Gen.Private.render_value Gen.unit () = "()") "unit lost its printer";
  (* Why it is not [constant ()]: a deriving composition over [constant ()]
     has no printer to derive from. *)
  let paired = Gen.(pair unit nat) in
  let tree = Gen.Private.sample paired (state 1) in
  let (), n = root_value tree in
  let rendered = render tree in
  check
    (rendered = Printf.sprintf "((), %d)" n)
    "pair over unit rendered %S" rendered;
  let bare = Gen.(pair (constant ()) nat) in
  check
    (Gen.Private.render_value bare ((), 0) = placeholder)
    "pair over [constant ()] claims a printer"

let bool_shrinks_true_to_false () =
  let values = samples Gen.bool 100 in
  check (List.mem true values) "no true in 100 bools";
  check (List.mem false values) "no false in 100 bools";
  let tree = find_sample Gen.bool (fun v -> v) in
  check
    (root_value (first_child tree) = false)
    "true's first candidate is not false";
  let tree = find_sample Gen.bool (fun v -> not v) in
  check (no_children tree) "false has shrink candidates"

let char_is_uniform_and_shrinks_to_a () =
  let values = samples Gen.char 300 in
  check
    (List.exists (fun c -> Char.code c > 127) values)
    "no byte above 127 in 300 chars";
  check
    (List.exists (fun c -> Char.code c < 32) values)
    "no control byte in 300 chars (NUL weight must be 1/256, not 0)";
  let tree = find_sample Gen.char (fun c -> c <> 'a') in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 'a') "char minimized to %C" minimum

let char_range_stays_in_bounds_and_shrinks_toward_a () =
  let gen = Gen.char_range 'b' 'y' in
  for index = 0 to 19 do
    let tree = Gen.Private.sample gen (state index) in
    explore ~limit:100 tree (fun c ->
        check (c >= 'b' && c <= 'y') "char_range candidate %C out of bounds" c)
  done;
  let tree = find_sample gen (fun c -> c > 'b') in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 'b') "char_range 'b' 'y' minimized to %C, not 'b'" minimum;
  let rendered = render tree in
  check
    (rendered = Printf.sprintf "%C" (root_value tree))
    "char_range rendered %S" rendered

let char_range_outside_a_shrinks_to_nearest_bound () =
  let upper = Gen.char_range 'A' 'Z' in
  let tree = find_sample upper (fun c -> c < 'Z') in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 'Z') "char_range 'A' 'Z' minimized to %C, not 'Z'" minimum;
  let digits = Gen.char_range '0' '9' in
  let tree = find_sample digits (fun c -> c < '9') in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = '9') "char_range '0' '9' minimized to %C, not '9'" minimum;
  List.iter
    (fun index ->
      explore ~limit:100
        (Gen.Private.sample digits (state index))
        (fun c ->
          check (c >= '0' && c <= '9') "digit candidate %C out of bounds" c))
    [ 0; 1; 2 ]

let char_range_degenerate_is_a_leaf_and_invalid_raises () =
  let tree = Gen.Private.sample (Gen.char_range 'x' 'x') (state 0) in
  check
    (root_value tree = 'x')
    "char_range 'x' 'x' produced %C" (root_value tree);
  check (no_children tree) "char_range 'x' 'x' has shrink candidates";
  match Gen.Private.sample (Gen.char_range 'z' 'a') (state 0) with
  | exception Invalid_argument _ -> ()
  | _ -> failf "char_range 'z' 'a' sampled successfully"

let string_shrinks_to_empty_and_renders_quoted () =
  let tree = find_sample Gen.string (fun s -> String.length s >= 2) in
  check
    (root_value (first_child tree) = "")
    "non-empty string's first candidate is not \"\"";
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = "") "string minimized to %S" minimum;
  let rendered = render tree in
  check
    (rendered = Printf.sprintf "%S" (root_value tree))
    "string rendered %S" rendered

let string_of_respects_character_generator () =
  let letters =
    Gen.map (fun c -> Char.chr (97 + (Char.code c mod 16))) Gen.char
  in
  let gen = Gen.string_of letters in
  let in_range c = c >= 'a' && c <= 'p' in
  for index = 0 to 9 do
    let tree = Gen.Private.sample gen (state index) in
    explore ~limit:100 tree (fun s ->
        String.iter (fun c -> check (in_range c) "string_of produced %C" c) s)
  done

let string_of_size_keeps_length_in_bounds () =
  let gen = Gen.(string_of ~size:(int_range 2 5) char) in
  for index = 0 to 9 do
    let tree = Gen.Private.sample gen (state index) in
    explore ~limit:200 tree (fun s ->
        let n = String.length s in
        check (n >= 2 && n <= 5) "sized string candidate has length %d" n)
  done;
  let tree = Gen.Private.sample gen (state 0) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = "aa") "sized string minimized to %S, not \"aa\"" minimum

let string_of_negative_size_raises_at_sample_time () =
  let gen = Gen.(string_of ~size:(constant (-1)) char) in
  match Gen.Private.sample gen (state 0) with
  | exception Invalid_argument _ -> ()
  | _ -> failf "negative string size sampled successfully"

let bytes_shrink_to_empty () =
  let tree = find_sample Gen.bytes (fun b -> Bytes.length b >= 1) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check
    (Bytes.length minimum = 0)
    "bytes minimized to %d bytes" (Bytes.length minimum);
  let rendered = render tree in
  check (starts_with "Bytes.of_string" rendered) "bytes rendered %S" rendered

let bytes_of_respects_size_and_character_generator () =
  let gen = Gen.(bytes_of ~size:(constant 3) (char_range 'a' 'z')) in
  for index = 0 to 9 do
    let tree = Gen.Private.sample gen (state index) in
    explore ~limit:100 tree (fun b ->
        check
          (Bytes.length b = 3)
          "sized bytes candidate has length %d" (Bytes.length b);
        Bytes.iter
          (fun c -> check (c >= 'a' && c <= 'z') "bytes_of produced %C" c)
          b)
  done;
  let tree = Gen.Private.sample gen (state 0) in
  let rendered = render tree in
  check (starts_with "Bytes.of_string" rendered) "bytes_of rendered %S" rendered

(* Containers *)

let list_shrinks_structurally () =
  let gen = Gen.(list int) in
  let tree = find_sample gen (fun l -> List.length l >= 2) in
  check
    (root_value (first_child tree) = [])
    "non-empty list's first candidate is not []";
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = []) "list minimized to a %d-element list"
    (List.length minimum);
  let rendered = render tree in
  check (starts_with "[" rendered) "list rendered %S" rendered

let list_with_size_keeps_length_in_bounds () =
  let gen = Gen.(list ~size:(int_range 2 5) nat) in
  for index = 0 to 9 do
    let tree = Gen.Private.sample gen (state index) in
    explore ~limit:200 tree (fun l ->
        let n = List.length l in
        check (n >= 2 && n <= 5) "sized list candidate has length %d" n)
  done;
  let tree = Gen.Private.sample gen (state 0) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check
    (minimum = [ 0; 0 ])
    "sized list minimized to length %d" (List.length minimum)

let list_negative_size_raises_at_sample_time () =
  let gen = Gen.(list ~size:(constant (-1)) nat) in
  match Gen.Private.sample gen (state 0) with
  | exception Invalid_argument _ -> ()
  | _ -> failf "negative size sampled successfully"

let array_shrinks_to_empty () =
  let gen = Gen.(array nat) in
  let tree = find_sample gen (fun a -> Array.length a >= 1) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check
    (Array.length minimum = 0)
    "array minimized to %d elements" (Array.length minimum);
  let rendered = render tree in
  check (starts_with "[|" rendered) "array rendered %S" rendered

let option_offers_none_first () =
  let gen = Gen.(option nat) in
  let values = samples gen 200 in
  check (List.mem None values) "no None in 200 options";
  check (List.exists Option.is_some values) "no Some in 200 options";
  let tree = find_sample gen Option.is_some in
  check
    (root_value (first_child tree) = None)
    "Some's first candidate is not None";
  check (starts_with "Some (" (render tree)) "Some rendered %S" (render tree)

let result_generates_both_constructors () =
  let gen = Gen.(result nat nat) in
  let values = samples gen 500 in
  check (List.exists Result.is_ok values) "no Ok in 500 results";
  check (List.exists Result.is_error values) "no Error in 500 results";
  let tree = find_sample gen Result.is_ok in
  explore ~limit:50 tree (fun v ->
      check (Result.is_ok v) "Ok candidate crossed to Error");
  check (starts_with "Ok (" (render tree)) "Ok rendered %S" (render tree)

let either_generates_both_constructors () =
  let gen = Gen.(either nat nat) in
  let values = samples gen 500 in
  check (List.exists Either.is_left values) "no Left in 500 eithers";
  check (List.exists Either.is_right values) "no Right in 500 eithers";
  (* Equal probability, unlike [result]'s 3:1 — pinned loosely enough to
     hold for any seed. *)
  let lefts = List.length (List.filter Either.is_left values) in
  check
    (lefts >= 175 && lefts <= 325)
    "%d of 500 eithers were Left, not about half" lefts;
  let tree = find_sample gen Either.is_right in
  explore ~limit:50 tree (fun v ->
      check (Either.is_right v) "Right candidate crossed to Left");
  check (starts_with "Right (" (render tree)) "Right rendered %S" (render tree);
  let tree = find_sample gen Either.is_left in
  check (starts_with "Left (" (render tree)) "Left rendered %S" (render tree);
  (* Printing derives as for [result]: a printerless side forfeits it, and
     a pre-image side carries through. *)
  check
    (Gen.Private.render_value Gen.(either nat (constant 'k')) (Either.Left 1)
    = placeholder)
    "either over a printerless side derived a printer";
  let mapped = Gen.(either (map succ nat) nat) in
  let left = find_sample mapped Either.is_left in
  let n = Either.find_left (root_value left) |> Option.get in
  check
    (rendering left = Pre_image (Printf.sprintf "Left (%d)" (n - 1)))
    "Left of a mapped nat rendered %S" (render left)

let pair_shrinks_left_first_to_zeroes () =
  let gen = Gen.(pair nat nat) in
  let tree = find_sample gen (fun (a, _) -> a > 0) in
  let _, right = root_value tree in
  check
    (root_value (first_child tree) = (0, right))
    "pair's first candidate did not shrink the left component to 0";
  let minimum, _ = minimize (fun _ -> true) tree in
  check
    (minimum = (0, 0))
    "pair minimized to (%d, %d)" (fst minimum) (snd minimum);
  let tree = Gen.Private.sample gen (state 0) in
  let a, b = root_value tree in
  check
    (render tree = Printf.sprintf "(%d, %d)" a b)
    "pair rendered %S" (render tree)

let triple_and_quad_minimize_to_zeroes () =
  let triple_tree = Gen.Private.sample Gen.(triple nat nat nat) (state 2) in
  let minimum, _ = minimize (fun _ -> true) triple_tree in
  check (minimum = (0, 0, 0)) "triple minimized elsewhere";
  let quad_tree = Gen.Private.sample Gen.(quad nat nat nat nat) (state 3) in
  let minimum, _ = minimize (fun _ -> true) quad_tree in
  check (minimum = (0, 0, 0, 0)) "quad minimized elsewhere"

(* Choice and structure *)

let constant_is_a_leaf_and_asks_for_a_printer () =
  let gen = Gen.constant 42 in
  let tree = Gen.Private.sample gen (state 0) in
  check (root_value tree = 42) "constant produced %d" (root_value tree);
  check (no_children tree) "constant has shrink candidates";
  (* The rendering names the remedy itself, so a counterexample and a bare
     value spell the same placeholder. *)
  check (render tree = placeholder) "constant rendered %S" (render tree);
  check (Gen.Private.render_value gen 42 = placeholder) "constant has a printer"

let of_list_picks_uniformly_and_shrinks_toward_head () =
  let gen = Gen.of_list [ 10; 20; 30 ] in
  let values = samples gen 100 in
  List.iter
    (fun v -> check (List.mem v values) "value %d never chosen in 100" v)
    [ 10; 20; 30 ];
  let tree = find_sample gen (fun v -> v = 30) in
  check
    (root_value (first_child tree) = 10)
    "the last value's first candidate is not the head";
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 10) "of_list minimized to %d, not the head" minimum;
  let rendered = render tree in
  check (rendered = placeholder) "of_list rendered %S" rendered;
  check (Gen.Private.render_value gen 20 = placeholder) "of_list has a printer"

(* One printerless leaf forfeits the derived printer of everything built
   over it, and [with_pp] is the one way back; the printer it attaches then
   feeds the deriving combinators, and the pre-image of a [map]. *)
let leaf_printers_feed_the_derivation_law () =
  let pp = Format.pp_print_int in
  let gen = Gen.with_pp pp (Gen.of_list [ 10; 20; 30 ]) in
  let tree = find_sample gen (fun v -> v = 30) in
  let rendered = render tree in
  check (rendered = "30") "printed of_list rendered %S, not the value" rendered;
  check
    (Gen.Private.render_value gen 20 = "20")
    "printed of_list does not render a bare value";
  (* The leaf's printer feeds the deriving combinators above it... *)
  let listed = Gen.list gen in
  check
    (Gen.Private.render_value listed [ 10; 20 ] = "[10; 20]")
    "list over a printed leaf does not render";
  (* ...and [map], which derives no printer, renders its argument through
     it: the pre-image. *)
  let mapped = Gen.map (fun v -> (v, ())) gen in
  check
    (Gen.Private.render_value mapped (30, ()) = placeholder)
    "map claimed a printer";
  let mapped_tree = find_sample mapped (fun (v, ()) -> v = 30) in
  check
    (rendering mapped_tree = Pre_image "30")
    "map over a printed leaf rendered %S" (render mapped_tree);
  let c = Gen.with_pp pp (Gen.constant 7) in
  let c_rendered = render (Gen.Private.sample c (state 0)) in
  check (c_rendered = "7") "printed constant rendered %S" c_rendered;
  (* Without one the leaf prints nothing at all. *)
  let bare = Gen.of_list [ 10; 20; 30 ] in
  check
    (Gen.Private.render_value bare 10 = placeholder)
    "of_list without a printer prints"

let of_list_singleton_is_a_leaf_and_empty_raises () =
  let tree = Gen.Private.sample (Gen.of_list [ `Only ]) (state 0) in
  check (root_value tree = `Only) "of_list singleton produced another value";
  check (no_children tree) "of_list singleton has shrink candidates";
  match Gen.Private.sample (Gen.of_list []) (state 0) with
  | exception Invalid_argument _ -> ()
  | _ -> failf "of_list [] sampled successfully"

let one_of_empty_raises_at_sample_time () =
  let gen = Gen.one_of [] in
  match Gen.Private.sample gen (state 0) with
  | exception Invalid_argument _ -> ()
  | _ -> failf "one_of [] sampled successfully"

let one_of_picks_all_branches_and_shrinks_to_earlier () =
  let gen = Gen.(one_of [ constant `A; constant `B ]) in
  let values = samples gen 100 in
  check (List.mem `A values) "branch 0 never chosen in 100";
  check (List.mem `B values) "branch 1 never chosen in 100";
  let tree = find_sample gen (fun v -> v = `B) in
  check
    (root_value (first_child tree) = `A)
    "one_of branch 1 did not shrink to branch 0";
  let rendered = render tree in
  check (rendered = placeholder) "one_of rendered %S" rendered

let frequency_respects_weights () =
  let gen = Gen.(frequency [ (1, constant `A); (3, constant `B) ]) in
  let values = samples gen 400 in
  let count v = List.length (List.filter (fun x -> x = v) values) in
  check (count `A > 0) "weight-1 branch never chosen";
  check
    (count `B > count `A)
    "weight-3 branch not dominant (%d vs %d)" (count `B) (count `A);
  let tree = find_sample gen (fun v -> v = `B) in
  let rendered = render tree in
  check (rendered = placeholder) "frequency rendered %S" rendered

(* The Gen doc law: a composite prints exactly when all its components
   print — including the choice combinators. *)
let one_of_over_printed_branches_derives_printer () =
  let gen = Gen.(one_of [ int_range 0 9; int_range 100 199 ]) in
  check (Gen.Private.render_value gen 5 = "5") "one_of did not derive a printer";
  let tree = find_sample gen (fun v -> v >= 100) in
  let rendered = render tree in
  check
    (rendered = string_of_int (root_value tree))
    "printed one_of rendered %S, not the value" rendered;
  (* Shrunk candidates render as values too, including branch re-generation
     candidates. *)
  let child = first_child tree in
  check
    (render child = string_of_int (root_value child))
    "a shrunk printed one_of candidate rendered %S" (render child);
  (* The derived printer feeds enclosing deriving combinators, and the
     pre-image of a [map] over the choice. *)
  let paired = Gen.(pair gen nat) in
  let tree = Gen.Private.sample paired (state 0) in
  let a, b = root_value tree in
  check
    (render tree = Printf.sprintf "(%d, %d)" a b)
    "pair over a printed one_of rendered %S" (render tree);
  let mapped = Gen.map Fun.id gen in
  let tree = Gen.Private.sample mapped (state 1) in
  check
    (rendering tree = Pre_image (string_of_int (root_value tree)))
    "map over a printed one_of rendered %S" (render tree)

let frequency_over_printed_branches_derives_printer () =
  let gen = Gen.(frequency [ (1, nat); (3, int_range 100 199) ]) in
  check
    (Gen.Private.render_value gen 7 = "7")
    "frequency did not derive a printer";
  let tree = Gen.Private.sample gen (state 0) in
  let rendered = render tree in
  check
    (rendered = string_of_int (root_value tree))
    "printed frequency rendered %S, not the value" rendered;
  (* One printerless branch forfeits the derivation for the whole choice. *)
  let mixed = Gen.(frequency [ (1, nat); (1, constant 5) ]) in
  check
    (Gen.Private.render_value mixed 5 = placeholder)
    "a mixed frequency derived a printer"

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
      match Gen.Private.sample gen (state 0) with
      | exception Invalid_argument _ -> ()
      | _ -> failf "frequency (%s) sampled successfully" name)
    cases

let such_that_filters_generation_and_shrinking () =
  let even = Gen.such_that (fun n -> n mod 2 = 0) Gen.int in
  for index = 0 to 9 do
    let tree = Gen.Private.sample even (state index) in
    explore ~limit:100 tree (fun v ->
        check (v mod 2 = 0) "such_that candidate %d is odd" v)
  done;
  let tree = find_sample even (fun v -> v <> 0) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 0) "even int minimized to %d" minimum;
  check
    (Gen.Private.render_value even 4 = "4")
    "such_that dropped the underlying printer"

let such_that_exhaustion_is_a_discard () =
  let gen = Gen.such_that (fun _ -> false) Gen.nat in
  match Gen.Private.sample gen (state 0) with
  | exception Gen.Private.Rejected -> ()
  | _ -> failf "unsatisfiable such_that sampled successfully"

(* Composition *)

(* A [map] over a printing generator renders its pre-image: the argument
   the function received, through the argument's printer — at the root and
   at every candidate, whose pre-image is the candidate's own. *)
let map_renders_the_pre_image () =
  let gen = Gen.map succ Gen.int in
  let tree = Gen.Private.sample gen (state 1) in
  check
    (rendering tree = Pre_image (string_of_int (root_value tree - 1)))
    "mapped int rendered %S for %d" (render tree) (root_value tree);
  let tree = find_sample gen (fun v -> v <> 1) in
  let child = first_child tree in
  check
    (rendering child = Pre_image (string_of_int (root_value child - 1)))
    "shrunk mapped int rendered %S for %d" (render child) (root_value child);
  (* Greedy shrinking reports the pre-image of the value it stops at. *)
  let minimum, _ = minimize (fun v -> v > 10) tree in
  check (minimum = 11) "mapped int minimized to %d, not 11" minimum;
  let rec descend tree =
    match Seq.find (fun c -> root_value c > 10) (Shrink_tree.children tree) with
    | None -> tree
    | Some child -> descend child
  in
  check
    (rendering (descend tree) = Pre_image "10")
    "the minimum's pre-image rendered %S, not 10"
    (render (descend tree))

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
      check (starts_with "\"" text)
        "the chain rendered %S, not the drawn string" text
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
  let tree = Gen.Private.sample gen (state 2) in
  let a, _ = root_value tree in
  check
    (rendering tree = Pre_image (Printf.sprintf "(%d, \"x\")" (a - 1)))
    "let+/and+ rendered %S" (render tree)

(* Deriving combinators carry pre-images through: a list of mapped values
   renders as the list of their pre-images. *)
let containers_carry_pre_images () =
  let listed = Gen.(list ~size:(int_range 2 2) (map succ nat)) in
  let tree = Gen.Private.sample listed (state 3) in
  let expected =
    match root_value tree with
    | [ a; b ] -> Printf.sprintf "[%d; %d]" (a - 1) (b - 1)
    | _ -> failf "expected two elements"
  in
  check
    (rendering tree = Pre_image expected)
    "list of mapped nats rendered %S, not %S" (render tree) expected;
  let optional = Gen.(option (map succ nat)) in
  let some = find_sample optional Option.is_some in
  let n = Option.get (root_value some) in
  check
    (rendering some = Pre_image (Printf.sprintf "Some (%d)" (n - 1)))
    "Some of a mapped nat rendered %S" (render some);
  (* [None] has no part computed by the map: it is the value. *)
  let none = find_sample optional Option.is_none in
  check (rendering none = Value "None") "None rendered %S" (render none)

(* A leaf with nothing to print forfeits the pre-image of the whole
   composition, exactly as it forfeits the derived printer. *)
let no_printer_anywhere_renders_nothing () =
  let gen = Gen.(map (fun (c, n) -> (c, n)) (pair (constant 'k') nat)) in
  let tree = Gen.Private.sample gen (state 0) in
  check
    (rendering tree = Value placeholder)
    "a map over a constant rendered %S" (render tree);
  let bound = Gen.(bind nat (fun n -> map (fun c -> (c, n)) (constant 'k'))) in
  let tree = Gen.Private.sample bound (state 0) in
  check
    (rendering tree = Value placeholder)
    "a bind into a constant rendered %S" (render tree)

let bind_keeps_inner_constraints_while_shrinking () =
  let gen =
    Gen.(
      let* n = int_range 1 3 in
      list ~size:(constant n) nat)
  in
  for index = 0 to 9 do
    let tree = Gen.Private.sample gen (state index) in
    explore ~limit:200 tree (fun l ->
        let n = List.length l in
        check (n >= 1 && n <= 3) "bound list candidate has length %d" n)
  done;
  let tree = Gen.Private.sample gen (state 0) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = [ 0 ]) "bound list minimized to length %d"
    (List.length minimum)

(* A [bind] renders the inner value when the inner generator prints; the
   pre-image [outer -> inner] when the inner is itself a pre-image; and
   nothing when the inner has nothing to print. *)
let bind_renders_by_its_inner () =
  let printing = Gen.(bind nat (fun n -> int_range n (n + 1))) in
  let tree = Gen.Private.sample printing (state 4) in
  check
    (rendering tree = Value (string_of_int (root_value tree)))
    "bind into a printing generator rendered %S" (render tree);
  let opaque = Gen.(bind nat (fun n -> constant n)) in
  let tree = Gen.Private.sample opaque (state 4) in
  check
    (rendering tree = Value placeholder)
    "bind into a constant rendered %S" (render tree);
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
  check
    (rendering tree = Pre_image expected)
    "a bind chain rendered %S, not %S" (render tree) expected;
  (* Candidates re-generate the inner value: the pre-image follows. *)
  let child = first_child tree in
  let n, xs = root_value child in
  let expected =
    Printf.sprintf "%d -> [%s]" n
      (String.concat "; " (List.map string_of_int xs))
  in
  check
    (rendering child = Pre_image expected)
    "a shrunk bind chain rendered %S, not %S" (render child) expected;
  (* Nested binds read left to right; an outer that is itself a bind's
     pre-image is parenthesised. *)
  let nested =
    Gen.(
      let* a = int_range 1 1 in
      let* b = int_range 2 2 in
      let+ c = int_range 3 3 in
      a + b + c)
  in
  let tree = Gen.Private.sample nested (state 0) in
  check
    (rendering tree = Pre_image "1 -> 2 -> 3")
    "nested binds rendered %S" (render tree);
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
  let tree = Gen.Private.sample outer_bind (state 0) in
  check
    (rendering tree = Pre_image "(1 -> 2) -> 3")
    "a bind whose outer is a bind rendered %S" (render tree)

let letops_compose () =
  let gen =
    Gen.(
      let+ a = nat and+ b = nat in
      a + b)
  in
  let tree = find_sample gen (fun v -> v > 0) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 0) "let+/and+ sum minimized to %d" minimum

let with_pp_attaches_a_printer () =
  let custom ppf n = Format.fprintf ppf "N=%d" n in
  let inner = Gen.with_pp custom (Gen.map succ Gen.int) in
  let tree = Gen.Private.sample inner (state 5) in
  check
    (render tree = Printf.sprintf "N=%d" (root_value tree))
    "with_pp did not render directly";
  check
    (Gen.Private.render_value inner 7 = "N=7")
    "with_pp did not expose the printer";
  (* A [map] above it derives nothing, and renders its argument through
     the attached printer: a pre-image. *)
  let outer = Gen.map (fun n -> -n) inner in
  let tree = Gen.Private.sample outer (state 6) in
  check
    (rendering tree = Pre_image (Printf.sprintf "N=%d" (-root_value tree)))
    "mapped with_pp rendered %S" (render tree);
  (* On the image, an explicit printer wins over the pre-image. *)
  let printed = Gen.with_pp custom outer in
  let tree = Gen.Private.sample printed (state 6) in
  check
    (rendering tree = Value (Printf.sprintf "N=%d" (root_value tree)))
    "with_pp over a map rendered %S" (render tree)

let mixed_one_of_derives_no_printer () =
  (* One branch prints, one does not: the choice cannot derive a printer,
     and a counterexample renders with the branch that drew it. *)
  let custom ppf n = Format.fprintf ppf "N=%d" n in
  let gen = Gen.(one_of [ with_pp custom (constant 5); constant 9 ]) in
  check
    (Gen.Private.render_value gen 5 = placeholder)
    "a mixed one_of derived a printer";
  let printed = find_sample gen (fun v -> v = 5) in
  check
    (rendering printed = Value "N=5")
    "printed branch rendered %S" (render printed);
  let printerless = find_sample gen (fun v -> v = 9) in
  check
    (rendering printerless = Value placeholder)
    "printerless branch rendered %S" (render printerless)

(* The RFC's shape example: [map] under each branch makes the choice
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
  check
    (rendering rect_tree
    = Pre_image (Format.asprintf "(%a, %a)" Pp.float_exact w Pp.float_exact h))
    "bare shape rendered %S" (render rect_tree);
  let pp_shape ppf = function
    | Circle r -> Format.fprintf ppf "Circle %g" r
    | Rect (w, h) -> Format.fprintf ppf "Rect (%g, %g)" w h
  in
  let printed = Gen.with_pp pp_shape shape_gen in
  let tree = Gen.Private.sample printed (state 0) in
  check
    (starts_with "Circle" (render tree) || starts_with "Rect" (render tree))
    "with_pp shape rendered %S" (render tree)

(* Printing totality *)

let render_is_total_over_shrink_trees () =
  let check_gen : type a. string -> a Gen.t -> unit =
   fun name gen ->
    for index = 0 to 4 do
      let tree = Gen.Private.sample gen (state index) in
      let visited = ref 0 in
      let rec go tree =
        if !visited >= 50 then raise_notrace Exit;
        incr visited;
        let rendered = render tree in
        check (String.length rendered > 0) "%s rendered an empty string" name;
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
  let tree = Gen.Private.sample gen (state 0) in
  check
    (starts_with "<printer raised" (render tree))
    "raising printer rendered %S" (render tree);
  let rendered = Gen.Private.render_value gen 3 in
  check
    (starts_with "<printer raised" rendered)
    "render_value let the exception through: %S" rendered

let render_value_reports_printer_presence () =
  check (Gen.Private.render_value Gen.int 42 = "42") "int printer missing";
  check
    (Gen.Private.render_value Gen.(list nat) [ 1; 2 ] = "[1; 2]")
    "list printer missing";
  check
    (Gen.Private.render_value (Gen.map succ Gen.int) 3 = placeholder)
    "map kept a printer it cannot have"

(* Adversarial additions *)

(* [find_sample] for generators whose sampling can legitimately discard. *)
let find_sample_skipping_discards ?(max_index = 10_000) gen accept =
  let rec loop index =
    if index >= max_index then
      failf "no matching sample within %d cases" max_index
    else
      match Gen.Private.sample gen (state index) with
      | tree -> if accept (root_value tree) then tree else loop (index + 1)
      | exception Gen.Private.Rejected -> loop (index + 1)
  in
  loop 0

(* A shrink candidate whose re-generation exhausts a [such_that] budget must
   be skipped, not raise: memoized child cells cache exceptions, so a raising
   cell would also hide every later sibling candidate. *)
let rejected_bind_candidates_are_skipped () =
  let gen =
    Gen.(
      bind (int_range 1 10) (fun n ->
          if n = 1 then such_that (fun _ -> false) nat else constant n))
  in
  let tree = find_sample_skipping_discards gen (fun v -> v >= 3) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 2)
    "bind with a rejecting candidate minimized to %d, not 2 (candidate 1 must \
     be skipped, siblings kept)"
    minimum

let rejected_one_of_candidates_are_skipped () =
  let gen = Gen.(one_of [ such_that (fun _ -> false) nat; constant 7 ]) in
  let tree = find_sample_skipping_discards gen (fun v -> v = 7) in
  let minimum, steps = minimize (fun _ -> true) tree in
  check
    (minimum = 7 && steps = 0)
    "one_of with a rejecting branch shrank to %d in %d steps" minimum steps

let int_range_negative_bounds_shrink_to_high () =
  let gen = Gen.int_range (-100) (-10) in
  for index = 0 to 9 do
    let tree = Gen.Private.sample gen (state index) in
    explore ~limit:200 tree (fun v ->
        check
          (v >= -100 && v <= -10)
          "int_range -100 -10 candidate %d out of bounds" v)
  done;
  let tree = find_sample gen (fun v -> v < -10) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = -10) "int_range -100 -10 minimized to %d, not -10" minimum

let frequency_zero_weight_branch_is_never_chosen () =
  let gen = Gen.(frequency [ (0, constant `A); (1, constant `B) ]) in
  List.iter
    (fun v -> check (v = `B) "frequency chose a zero-weight branch")
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
  check
    (candidates = [ 10; 20 ])
    "candidates of the position-2 value are [%s], not [10; 20]"
    (String.concat "; " (List.map string_of_int candidates))

let char_range_full_byte_span_behaves_like_char () =
  let gen = Gen.char_range '\x00' '\xff' in
  let values = samples gen 300 in
  check
    (List.exists (fun c -> Char.code c > 127) values)
    "no byte above 127 in 300 draws of the full span";
  let tree = find_sample gen (fun c -> c <> 'a') in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 'a') "full-span char_range minimized to %C, not 'a'" minimum

(* A [such_that] as the size generator: the filtered constraint must hold for
   the drawn length and for every shrink candidate's length. *)
let such_that_size_constrains_every_candidate () =
  let gen = Gen.(list ~size:(such_that (fun n -> n mod 2 = 0) nat) nat) in
  let tree = find_sample gen (fun l -> List.length l >= 2) in
  explore ~limit:300 tree (fun l ->
      check
        (List.length l mod 2 = 0)
        "even-size list candidate has odd length %d" (List.length l));
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = []) "even-size list minimized to length %d"
    (List.length minimum);
  (* An exhausted size generator is a generation-time discard, like any other
     [such_that] exhaustion. *)
  let starved = Gen.(list ~size:(such_that (fun _ -> false) nat) nat) in
  match Gen.Private.sample starved (state 0) with
  | exception Gen.Private.Rejected -> ()
  | _ -> failf "a starved size generator sampled successfully"

(* [such_that] around a sized string: both constraints — fixed length and the
   predicate — hold for the root and every candidate, and the greedy minimum
   is the predicate boundary. *)
let such_that_over_sized_string_keeps_both_constraints () =
  let gen =
    Gen.(
      such_that
        (fun s -> s <> "aaa")
        (string_of ~size:(constant 3) (char_range 'a' 'z')))
  in
  for index = 0 to 4 do
    let tree = Gen.Private.sample gen (state index) in
    explore ~limit:200 tree (fun s ->
        check (String.length s = 3) "candidate %S is not 3 chars" s;
        check (s <> "aaa") "candidate violated the predicate";
        String.iter
          (fun c -> check (c >= 'a' && c <= 'z') "candidate char %C" c)
          s);
    let minimum, _ = minimize (fun _ -> true) tree in
    let sorted =
      String.to_seq minimum |> List.of_seq |> List.sort compare |> List.to_seq
      |> String.of_seq
    in
    check (sorted = "aab")
      "the greedy minimum must sit on the predicate boundary, got %S" minimum
  done

(* An explicit [with_pp] must win over the derived choice printer, and a
   [map] above the override renders its pre-image through the override. *)
let with_pp_overrides_derived_choice_printer () =
  let custom ppf n = Format.fprintf ppf "N=%d" n in
  let overridden = Gen.(with_pp custom (one_of [ int_range 0 9; nat ])) in
  check
    (Gen.Private.render_value overridden 5 = "N=5")
    "with_pp did not override the derived choice printer";
  let tree = Gen.Private.sample overridden (state 0) in
  check
    (render tree = Printf.sprintf "N=%d" (root_value tree))
    "overridden choice rendered %S" (render tree);
  let mapped = Gen.map Fun.id overridden in
  let tree = Gen.Private.sample mapped (state 1) in
  check
    (rendering tree = Pre_image (Printf.sprintf "N=%d" (root_value tree)))
    "map over the override rendered %S" (render tree)

(* The B5 evidence shape (lpath test_lpath.ml:88-95): identifier characters
   from a frequency over char_range and of_list, assembled with a sized
   string. The composition must generate in-alphabet, keep the string
   printer, and minimize to a single boundary character. *)
let evidence_shaped_identifier_generator_composes () =
  let ident_char =
    Gen.(frequency [ (8, char_range 'a' 'z'); (1, of_list [ '-'; '_' ]) ])
  in
  let ident = Gen.(string_of ~size:(int_range 1 8) ident_char) in
  let in_alphabet c = (c >= 'a' && c <= 'z') || c = '-' || c = '_' in
  for index = 0 to 9 do
    let tree = Gen.Private.sample ident (state index) in
    explore ~limit:100 tree (fun s ->
        let n = String.length s in
        check (n >= 1 && n <= 8) "identifier candidate has length %d" n;
        String.iter
          (fun c -> check (in_alphabet c) "identifier char %C off-alphabet" c)
          s)
  done;
  let tree = Gen.Private.sample ident (state 0) in
  let rendered = render tree in
  check (starts_with "\"" rendered) "identifier lost its printer: %S" rendered;
  let minimum, _ = minimize (fun _ -> true) tree in
  check
    (minimum = "a" || minimum = "-")
    "identifier minimized to %S, not a single boundary char" minimum

let suite =
  [
    ("same seed gives same value and render", same_seed_same_value_and_render);
    ("different indexes vary", different_indexes_vary);
    ("int shrinks to zero", int_shrinks_to_zero);
    ("int renders decimal", int_renders_decimal);
    ("nat distribution is stratified", nat_distribution_is_stratified);
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
    ("triple and quad minimize to zeroes", triple_and_quad_minimize_to_zeroes);
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
    ( "char_range full byte span behaves like char",
      char_range_full_byte_span_behaves_like_char );
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

   Everything above walks a generator by hand: [samples] and
   [find_sample] draw from fixed seeds and [minimize] re-implements the
   shrink search. That is deliberate for the tests that must pin an exact
   candidate order or a specific distribution — but it pins behaviour at
   those seeds and says nothing about the rest of the space, and it never
   goes through [Property], so a bug in how Gen and the case loop fit
   together is invisible to it.

   The laws below are universally quantified statements, which is what
   [prop] is. They are also the only tests in this file that exercise the
   engine end to end: draw, run the body, and — when one breaks —
   shrink through the real search and print a replayable seed. That is
   the property engine testing itself with the property engine, which is
   the point.

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
    (* such_that's contract is about candidates as much as draws, and the
       shrink search is what visits candidates — so a violation here is
       reported only because the body runs under the engine. *)
    prop "such_that draws satisfy the predicate"
      (Gen.such_that (fun n -> n mod 3 = 0) (Gen.int_range (-300) 300))
      (fun n -> equal ~msg:"divisible by three" int 0 (n mod 3));
    prop "option is Some or None and never raises" (Gen.option Gen.int)
      (fun o ->
        is_true ~msg:"total" (match o with None -> true | Some _ -> true));
    (* Printing must be total: a counterexample that cannot be rendered
       is a failure the reader never sees. This is the one law whose
       violation would corrupt the report itself. *)
    prop "every drawn value renders"
      (Gen.pair Gen.string (Gen.list Gen.int))
      (fun (s, xs) ->
        let rendered =
          Gen.Private.render_value
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

  (* Printf-style shims over windtrap's [fail]: the bodies below assert with
     [check cond "fmt" args] and bail with [failf "fmt" args]. *)
  let failf format = Printf.ksprintf (fun message -> fail message) format

  let check condition format =
    Printf.ksprintf (fun message -> if not condition then fail message) format

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
    check (!calls = 0) "make forced children %d times" !calls;
    check
      (Shrink_tree.root tree = 42)
      "root returned %d instead of 42" (Shrink_tree.root tree);
    check (!calls = 0) "root forced children %d times" !calls;
    let leaf = Shrink_tree.leaf 7 in
    check (Shrink_tree.root leaf = 7) "leaf root was not retained";
    check
      (match Shrink_tree.children leaf () with
      | Seq.Nil -> true
      | Seq.Cons _ -> false)
      "leaf unexpectedly had a child"

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
    check (!calls = 1) "child head evaluated %d times instead of once" !calls;
    match (first, second) with
    | Seq.Cons (left, _), Seq.Cons (right, _) ->
        check (left == child) "first force did not return the supplied child";
        check (right == child) "second force did not reuse the supplied child"
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
    check (!head_calls = 1) "head evaluated %d times" !head_calls;
    check (!tail_calls = 0) "head force also forced its tail";
    ignore (children ());
    check (!head_calls = 1) "head cache was not reused";
    let first_tail = tail () in
    let second_tail = tail () in
    check (!tail_calls = 1) "tail evaluated %d times instead of once"
      !tail_calls;
    match (first_tail, second_tail) with
    | Seq.Cons (left, _), Seq.Cons (right, _) ->
        check (left == right) "tail did not physically reuse its child"
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
    check (!head_calls = 1) "shared-tail head evaluated %d times" !head_calls;
    check (!tail_calls = 1) "shared successful tail evaluated %d times"
      !tail_calls;
    check tails_are_physically_shared
      "repeated head observations returned different tail closures";
    match (first_node, second_node) with
    | Seq.Cons (left, _), Seq.Cons (right, _) ->
        check (left == tail_child) "first tail force returned a different child";
        check (right == tail_child) "second tail force did not reuse its child"
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
    check (!calls = 1) "empty source evaluated %d times instead of once" !calls

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
          check (caught == error) "forcing reraised a different exception value"
    in
    force ();
    force ();
    check (!calls = 1) "exceptional source evaluated %d times" !calls

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
          check (caught == error)
            "exceptional tail reraised a different exception value"
    in
    force first_tail;
    force second_tail;
    check (!head_calls = 1) "head source evaluated %d times" !head_calls;
    check (!tail_calls = 1) "exceptional tail evaluated %d times" !tail_calls;
    check tails_are_physically_shared
      "repeated head observations returned different exceptional tails"

  (* map *)

  let map_preserves_shape_and_order () =
    let actual = Shrink_tree.map (fun value -> value * 3) (finite_tree ()) in
    let expected =
      Node (30, [ Node (12, [ Node (0, []); Node (6, []) ]); Node (24, []) ])
    in
    check (observe actual = expected) "map changed finite tree shape or order"

  let map_obeys_identity_and_composition () =
    let tree = finite_tree () in
    check
      (observe (Shrink_tree.map Fun.id tree) = observe tree)
      "map identity law failed";
    let f value = value + 3 in
    let g value = value * 2 in
    let separate = Shrink_tree.map f (Shrink_tree.map g tree) in
    let composed = Shrink_tree.map (fun value -> f (g value)) tree in
    check (observe separate = observe composed) "map composition law failed"

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
    check (!map_calls = 1)
      "map did not evaluate exactly the root at construction";
    check (!source_calls = 0) "map construction forced source children";
    let children = Shrink_tree.children mapped in
    let first = children () in
    check (!source_calls = 1) "mapped child force evaluated source %d times"
      !source_calls;
    check (!map_calls = 2) "mapped child was evaluated %d total times"
      !map_calls;
    ignore (children ());
    check (!source_calls = 1) "repeated mapped force reran source";
    check (!map_calls = 2) "repeated mapped force reran mapping";
    match first with
    | Seq.Cons (child, _) ->
        check (Shrink_tree.root child = 6) "mapped child root was incorrect"
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
    check (!source_calls = 1) "exceptional mapped source ran %d times"
      !source_calls;
    check (!map_calls = 2) "exceptional mapper ran %d times" !map_calls

  (* pair *)

  let pair_reduces_left_before_right () =
    let left = node 10 [ Shrink_tree.leaf 0; Shrink_tree.leaf 5 ] in
    let right = node 20 [ Shrink_tree.leaf 2; Shrink_tree.leaf 4 ] in
    let tree = Shrink_tree.pair left right in
    check
      (Shrink_tree.root tree = (10, 20))
      "pair root did not combine input roots";
    let roots = child_roots tree in
    check
      (roots = [ (0, 20); (5, 20); (10, 2); (10, 4) ])
      "pair child order was not left-before-right"

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
          check
            (Shrink_tree.root candidate = (0, 2))
            "pair's first child was not from the left";
          tail
    in
    check (!left_head_calls = 1) "left head count was %d" !left_head_calls;
    check (!left_tail_calls = 0) "left head forced its tail";
    check (!right_calls = 0) "right forced before left exhaustion";
    ignore (tail ());
    check (!left_tail_calls = 1) "left tail was not exhausted exactly once";
    check (!right_calls = 1) "right was not forced after left exhaustion"

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
    check (observe direct = observe combined) "pair/map naturality law failed"

  (* list *)

  let list_has_exact_structural_then_element_order () =
    let first = node 10 [ Shrink_tree.leaf 0; Shrink_tree.leaf 5 ] in
    let second = node 20 [ Shrink_tree.leaf 2 ] in
    let tree = Shrink_tree.list [ first; second ] in
    check
      (Shrink_tree.root tree = [ 10; 20 ])
      "list root did not preserve input order";
    let roots = child_roots tree in
    let expected = [ []; [ 20 ]; [ 10 ]; [ 0; 20 ]; [ 5; 20 ]; [ 10; 2 ] ] in
    check (roots = expected) "expected list children %s, got %s"
      (show_int_lists expected) (show_int_lists roots)

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
    check (roots = expected) "expected chunk schedule %s, got %s"
      (show_int_lists expected) (show_int_lists roots);
    check
      (List.length roots = List.length (List.sort_uniq compare roots))
      "list structural schedule contained duplicate candidates"

  let singleton_list_has_no_chunk_removal () =
    (* Length 1 admits no power-of-two chunk strictly below it: the structural
       candidates are exactly the empty list, then element reductions. *)
    let tree = Shrink_tree.list [ node 7 [ Shrink_tree.leaf 3 ] ] in
    check (Shrink_tree.root tree = [ 7 ]) "singleton root was not [7]";
    let roots = child_roots tree in
    let expected = [ []; [ 3 ] ] in
    check (roots = expected) "expected singleton children %s, got %s"
      (show_int_lists expected) (show_int_lists roots)

  let list_length_three_uses_uniform_chunking () =
    (* v1's donor special-cased lists shorter than four; v3 must chunk
       uniformly: one removal of chunk two, then the three singles. *)
    let tree = [ 1; 2; 3 ] |> List.map Shrink_tree.leaf |> Shrink_tree.list in
    let roots = child_roots tree in
    let expected = [ []; [ 3 ]; [ 2; 3 ]; [ 1; 3 ]; [ 1; 2 ] ] in
    check (roots = expected) "expected length-3 schedule %s, got %s"
      (show_int_lists expected) (show_int_lists roots)

  let empty_list_is_a_leaf () =
    let tree = Shrink_tree.list [] in
    check (Shrink_tree.root tree = []) "empty list root was not empty";
    check
      (match Shrink_tree.children tree () with
      | Seq.Nil -> true
      | Seq.Cons _ -> false)
      "empty list had a candidate"

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
    check
      (structural = [ []; [ 2 ]; [ 1 ] ])
      "two-element structural prefix was incorrect";
    check (!first_calls = 0) "structural prefix forced first element children";
    check (!second_calls = 0) "structural prefix forced second element children";
    let element_tail = drop 3 children in
    let candidate =
      match element_tail () with
      | Seq.Nil -> failf "missing first element reduction"
      | Seq.Cons (candidate, _) -> candidate
    in
    check
      (Shrink_tree.root candidate = [ 0; 2 ])
      "first element reduction had the wrong root";
    check (!first_calls = 1) "first element source ran %d times" !first_calls;
    check (!second_calls = 0) "first element reduction forced second element"

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
    check (!first_calls = 1) "empty first element source ran %d times"
      !first_calls;
    check (!second_calls = 1) "second element source ran %d times" !second_calls;
    match (first_force, second_force) with
    | Seq.Cons (left, _), Seq.Cons (right, _) ->
        check (left == right) "list did not cache its first element candidate";
        check
          (Shrink_tree.root left = [ 1; 0 ])
          "list scanned to an incorrect element candidate"
    | Seq.Nil, _ | _, Seq.Nil -> failf "list lost its scanned element candidate"

  let list_is_stack_safe_for_large_flat_inputs () =
    let count = 100_000 in
    let trees = List.init count Shrink_tree.leaf in
    let tree = Shrink_tree.list trees in
    check
      (List.length (Shrink_tree.root tree) = count)
      "large list root had the wrong length";
    match Shrink_tree.children tree () with
    | Seq.Nil -> failf "large non-empty list had no structural reduction"
    | Seq.Cons (candidate, _) ->
        check
          (Shrink_tree.root candidate = [])
          "large list's first candidate was not empty"

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
      check
        (Shrink_tree.root !current = expected)
        "deep mapped root was %d instead of %d"
        (Shrink_tree.root !current)
        expected;
      if expected < depth then
        match Shrink_tree.children !current () with
        | Seq.Nil -> failf "infinite tree ended at depth %d" expected
        | Seq.Cons (child, _) -> current := child
    done;
    check
      (!source_calls = depth - 1)
      "deep traversal forced %d source cells instead of %d" !source_calls
      (depth - 1)

  let post_sample_forcing_needs_no_random_calls () =
    let random_calls = ref 0 in
    let random_word () =
      incr random_calls;
      !random_calls * 17 mod 31
    in
    let sample () =
      let root = random_word () in
      let first = random_word () in
      let second = random_word () in
      let captured = [ first; second ] in
      let rec tree value candidates =
        Shrink_tree.make ~root:value
          ~children:
            (Seq.map
               (fun candidate -> tree candidate [])
               (List.to_seq candidates))
      in
      tree root captured
    in
    let tree = sample () in
    let calls_after_sample = !random_calls in
    ignore (observe tree);
    ignore (observe tree);
    check
      (!random_calls = calls_after_sample)
      "forcing a sampled tree made %d additional random calls"
      (!random_calls - calls_after_sample)

  (* Greedy shrink termination: descend into the first candidate that still
     fails the property, exactly like the property engine's shrink loop. *)

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
    check (fails (Shrink_tree.root start)) "starting counterexample must fail";
    let steps = ref 0 in
    let budget = 1_000 in
    let rec minimize tree =
      if !steps > budget then failf "greedy shrink exceeded %d steps" budget;
      let rec first_failing candidates =
        match candidates () with
        | Seq.Nil -> None
        | Seq.Cons (candidate, tail) ->
            if fails (Shrink_tree.root candidate) then Some candidate
            else first_failing tail
      in
      match first_failing (Shrink_tree.children tree) with
      | None -> Shrink_tree.root tree
      | Some candidate ->
          incr steps;
          minimize candidate
    in
    let minimum = minimize start in
    check (minimum = [ 5 ]) "greedy shrink reached %s instead of [5]"
      (show_int_list minimum);
    check (!steps <= 20) "greedy shrink took %d steps" !steps

  let suite =
    [
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
      ( "post-sample forcing needs no random calls",
        post_sample_forcing_needs_no_random_calls );
      ( "greedy shrink terminates at a local minimum",
        greedy_shrink_terminates_at_a_local_minimum );
    ]

  let tests = List.map (fun (name, fn) -> test name fn) suite
end

let tests =
  List.map (fun (name, fn) -> test name fn) suite
  @ [ group "shrink tree" Shrink_tree_suite.tests ]
  @ law_tests

let () = exit @@ Windtrap.run "gen" tests
