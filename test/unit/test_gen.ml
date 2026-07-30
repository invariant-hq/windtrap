(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(* Tests for Gen: determinism under fixed seeds, distribution smoke tests,
   integrated-shrinking invariants (candidates satisfy generator
   constraints), greedy-shrink termination and minima, and printing
   totality including shrunk-draw provenance. *)

open Windtrap
module Seed = Windtrap.Private.Seed
module Shrink_tree = Windtrap.Private.Shrink_tree

(* Printf-style shims over windtrap's [fail], preserving the bodies'
   [check cond "fmt" args] and [failf "fmt" args] call shape. *)
let failf format = Printf.ksprintf (fun message -> fail message) format

let check condition format =
  Printf.ksprintf (fun message -> if not condition then fail message) format

let starts_with prefix text = String.starts_with ~prefix text

let contains needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec loop index =
    if index + n > h then false
    else if String.sub haystack index n = needle then true
    else loop (index + 1)
  in
  loop 0

(* One fixed root for the whole suite; per-test streams come from indexes.
   Everything below is deterministic across runs and machines. *)
let root = 0x00c0ffee1234abcdL
let state index = Seed.make (Seed.derive ~root ~path:"test_gen" ~index)
let root_value tree = Gen.value (Shrink_tree.root tree)

let samples gen count =
  List.init count (fun index -> root_value (Gen.sample gen (state index)))

let find_sample ?(max_index = 10_000) gen accept =
  let rec loop index =
    if index >= max_index then
      failf "no matching sample within %d cases" max_index
    else
      let tree = Gen.sample gen (state index) in
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

(* ───── Determinism ───── *)

let same_seed_same_value_and_render () =
  let against : (string * (unit -> string * string)) list =
    let run gen index =
      let once = Gen.sample gen (state index) in
      let twice = Gen.sample gen (state index) in
      ( Gen.render gen (Shrink_tree.root once),
        Gen.render gen (Shrink_tree.root twice) )
    in
    [
      ("int", fun () -> run Gen.int 3);
      ("float_any", fun () -> run Gen.float_any 4);
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

(* ───── Integer generators ───── *)

let int_shrinks_to_zero () =
  let tree = find_sample Gen.int (fun v -> v <> 0) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 0) "int minimized to %d, not 0" minimum

let int_renders_decimal () =
  let tree = Gen.sample Gen.int (state 0) in
  let rendered = Gen.render Gen.int (Shrink_tree.root tree) in
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
    let tree = Gen.sample gen (state index) in
    explore ~limit:200 tree (fun v ->
        check (v >= 10 && v <= 100) "int_range candidate %d out of bounds" v)
  done;
  let tree = find_sample gen (fun v -> v > 10) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 10) "int_range 10 100 minimized to %d, not 10" minimum

let int_range_degenerate_is_a_leaf () =
  let tree = Gen.sample (Gen.int_range 5 5) (state 0) in
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
  match Gen.sample gen (state 0) with
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

(* ───── Float generators ───── *)

let float_is_finite () =
  List.iter
    (fun v -> check (Float.is_finite v) "float produced %h" v)
    (samples Gen.float 500);
  let tree = find_sample Gen.float (fun v -> v <> 0.0) in
  let minimum, steps = minimize (fun _ -> true) tree in
  check (minimum = 0.0) "float minimized to %h in %d steps" minimum steps

let float_any_covers_nan_and_shrink_terminates () =
  let tree =
    find_sample ~max_index:500_000 Gen.float_any (fun v -> Float.is_nan v)
  in
  (* Greedy shrink from NaN terminates: the only candidate is 0. and it is
     not NaN, so NaN itself is the minimum. *)
  let minimum, steps = minimize Float.is_nan tree in
  check (Float.is_nan minimum) "nan shrink moved to %h" minimum;
  check (steps = 0) "nan shrink took %d steps" steps;
  let zero, _ = minimize (fun _ -> true) tree in
  check (zero = 0.0) "nan minimized to %h under always-failing" zero

let float_range_stays_in_bounds () =
  let gen = Gen.float_range 2.0 5.0 in
  for index = 0 to 19 do
    let tree = Gen.sample gen (state index) in
    explore ~limit:100 tree (fun v ->
        check (v >= 2.0 && v <= 5.0) "float_range candidate %h out of bounds" v)
  done;
  let tree = Gen.sample gen (state 0) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 2.0) "float_range 2 5 minimized to %h" minimum;
  let negative = Gen.float_range (-5.0) (-2.0) in
  let tree = Gen.sample negative (state 1) in
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
      match Gen.sample gen (state 0) with
      | exception Invalid_argument _ -> ()
      | _ -> failf "float_range (%s) sampled successfully" name)
    cases

(* ───── Booleans, characters, strings ───── *)

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
    let tree = Gen.sample gen (state index) in
    explore ~limit:100 tree (fun c ->
        check (c >= 'b' && c <= 'y') "char_range candidate %C out of bounds" c)
  done;
  let tree = find_sample gen (fun c -> c > 'b') in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 'b') "char_range 'b' 'y' minimized to %C, not 'b'" minimum;
  let rendered = Gen.render gen (Shrink_tree.root tree) in
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
        (Gen.sample digits (state index))
        (fun c ->
          check (c >= '0' && c <= '9') "digit candidate %C out of bounds" c))
    [ 0; 1; 2 ]

let char_range_degenerate_is_a_leaf_and_invalid_raises () =
  let tree = Gen.sample (Gen.char_range 'x' 'x') (state 0) in
  check
    (root_value tree = 'x')
    "char_range 'x' 'x' produced %C" (root_value tree);
  check (no_children tree) "char_range 'x' 'x' has shrink candidates";
  match Gen.sample (Gen.char_range 'z' 'a') (state 0) with
  | exception Invalid_argument _ -> ()
  | _ -> failf "char_range 'z' 'a' sampled successfully"

let string_shrinks_to_empty_and_renders_quoted () =
  let tree = find_sample Gen.string (fun s -> String.length s >= 2) in
  check
    (root_value (first_child tree) = "")
    "non-empty string's first candidate is not \"\"";
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = "") "string minimized to %S" minimum;
  let rendered = Gen.render Gen.string (Shrink_tree.root tree) in
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
    let tree = Gen.sample gen (state index) in
    explore ~limit:100 tree (fun s ->
        String.iter (fun c -> check (in_range c) "string_of produced %C" c) s)
  done

let string_of_size_keeps_length_in_bounds () =
  let gen = Gen.(string_of ~size:(int_range 2 5) char) in
  for index = 0 to 9 do
    let tree = Gen.sample gen (state index) in
    explore ~limit:200 tree (fun s ->
        let n = String.length s in
        check (n >= 2 && n <= 5) "sized string candidate has length %d" n)
  done;
  let tree = Gen.sample gen (state 0) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = "aa") "sized string minimized to %S, not \"aa\"" minimum

let string_of_negative_size_raises_at_sample_time () =
  let gen = Gen.(string_of ~size:(constant (-1)) char) in
  match Gen.sample gen (state 0) with
  | exception Invalid_argument _ -> ()
  | _ -> failf "negative string size sampled successfully"

let bytes_shrink_to_empty () =
  let tree = find_sample Gen.bytes (fun b -> Bytes.length b >= 1) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check
    (Bytes.length minimum = 0)
    "bytes minimized to %d bytes" (Bytes.length minimum);
  let rendered = Gen.render Gen.bytes (Shrink_tree.root tree) in
  check (starts_with "Bytes.of_string" rendered) "bytes rendered %S" rendered

let bytes_of_respects_size_and_character_generator () =
  let gen = Gen.(bytes_of ~size:(constant 3) (char_range 'a' 'z')) in
  for index = 0 to 9 do
    let tree = Gen.sample gen (state index) in
    explore ~limit:100 tree (fun b ->
        check
          (Bytes.length b = 3)
          "sized bytes candidate has length %d" (Bytes.length b);
        Bytes.iter
          (fun c -> check (c >= 'a' && c <= 'z') "bytes_of produced %C" c)
          b)
  done;
  let tree = Gen.sample gen (state 0) in
  let rendered = Gen.render gen (Shrink_tree.root tree) in
  check (starts_with "Bytes.of_string" rendered) "bytes_of rendered %S" rendered

(* ───── Containers ───── *)

let list_shrinks_structurally () =
  let gen = Gen.(list int) in
  let tree = find_sample gen (fun l -> List.length l >= 2) in
  check
    (root_value (first_child tree) = [])
    "non-empty list's first candidate is not []";
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = []) "list minimized to a %d-element list"
    (List.length minimum);
  let rendered = Gen.render gen (Shrink_tree.root tree) in
  check (starts_with "[" rendered) "list rendered %S" rendered

let list_with_size_keeps_length_in_bounds () =
  let gen = Gen.(list ~size:(int_range 2 5) nat) in
  for index = 0 to 9 do
    let tree = Gen.sample gen (state index) in
    explore ~limit:200 tree (fun l ->
        let n = List.length l in
        check (n >= 2 && n <= 5) "sized list candidate has length %d" n)
  done;
  let tree = Gen.sample gen (state 0) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check
    (minimum = [ 0; 0 ])
    "sized list minimized to length %d" (List.length minimum)

let list_negative_size_raises_at_sample_time () =
  let gen = Gen.(list ~size:(constant (-1)) nat) in
  match Gen.sample gen (state 0) with
  | exception Invalid_argument _ -> ()
  | _ -> failf "negative size sampled successfully"

let array_shrinks_to_empty () =
  let gen = Gen.(array nat) in
  let tree = find_sample gen (fun a -> Array.length a >= 1) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check
    (Array.length minimum = 0)
    "array minimized to %d elements" (Array.length minimum);
  let rendered = Gen.render gen (Shrink_tree.root tree) in
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
  check
    (starts_with "Some (" (Gen.render gen (Shrink_tree.root tree)))
    "Some rendered %S"
    (Gen.render gen (Shrink_tree.root tree))

let result_generates_both_constructors () =
  let gen = Gen.(result nat nat) in
  let values = samples gen 500 in
  check (List.exists Result.is_ok values) "no Ok in 500 results";
  check (List.exists Result.is_error values) "no Error in 500 results";
  let tree = find_sample gen Result.is_ok in
  explore ~limit:50 tree (fun v ->
      check (Result.is_ok v) "Ok candidate crossed to Error");
  check
    (starts_with "Ok (" (Gen.render gen (Shrink_tree.root tree)))
    "Ok rendered %S"
    (Gen.render gen (Shrink_tree.root tree))

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
  let tree = Gen.sample gen (state 0) in
  let a, b = root_value tree in
  check
    (Gen.render gen (Shrink_tree.root tree) = Printf.sprintf "(%d, %d)" a b)
    "pair rendered %S"
    (Gen.render gen (Shrink_tree.root tree))

let triple_and_quad_minimize_to_zeroes () =
  let triple_tree = Gen.sample Gen.(triple nat nat nat) (state 2) in
  let minimum, _ = minimize (fun _ -> true) triple_tree in
  check (minimum = (0, 0, 0)) "triple minimized elsewhere";
  let quad_tree = Gen.sample Gen.(quad nat nat nat nat) (state 3) in
  let minimum, _ = minimize (fun _ -> true) quad_tree in
  check (minimum = (0, 0, 0, 0)) "quad minimized elsewhere"

(* ───── Choice and structure ───── *)

let constant_is_a_leaf_and_asks_for_a_printer () =
  let gen = Gen.constant 42 in
  let tree = Gen.sample gen (state 0) in
  check (root_value tree = 42) "constant produced %d" (root_value tree);
  check (no_children tree) "constant has shrink candidates";
  check
    (Gen.render gen (Shrink_tree.root tree) = "<no printer — add Gen.with_pp>")
    "constant rendered %S"
    (Gen.render gen (Shrink_tree.root tree));
  check (Gen.render_value gen 42 = None) "constant has a printer"

let pure_is_constant () =
  let gen = Gen.pure 42 in
  let tree = Gen.sample gen (state 0) in
  check (root_value tree = 42) "pure produced %d" (root_value tree);
  check (no_children tree) "pure has shrink candidates";
  check (Gen.render_value gen 42 = None) "pure has a printer"

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
  let rendered = Gen.render gen (Shrink_tree.root tree) in
  check (rendered = "<from: of_list[2]>") "of_list rendered %S" rendered;
  check (Gen.render_value gen 20 = None) "of_list has a printer"

let of_list_singleton_is_a_leaf_and_empty_raises () =
  let tree = Gen.sample (Gen.of_list [ `Only ]) (state 0) in
  check (root_value tree = `Only) "of_list singleton produced another value";
  check (no_children tree) "of_list singleton has shrink candidates";
  match Gen.sample (Gen.of_list []) (state 0) with
  | exception Invalid_argument _ -> ()
  | _ -> failf "of_list [] sampled successfully"

let one_of_empty_raises_at_sample_time () =
  let gen = Gen.one_of [] in
  match Gen.sample gen (state 0) with
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
  let rendered = Gen.render gen (Shrink_tree.root tree) in
  check (rendered = "<from: one_of[1]>") "one_of rendered %S" rendered

let frequency_respects_weights_and_labels () =
  let gen = Gen.(frequency [ (1, constant `A); (3, constant `B) ]) in
  let values = samples gen 400 in
  let count v = List.length (List.filter (fun x -> x = v) values) in
  check (count `A > 0) "weight-1 branch never chosen";
  check
    (count `B > count `A)
    "weight-3 branch not dominant (%d vs %d)" (count `B) (count `A);
  let tree = find_sample gen (fun v -> v = `B) in
  let rendered = Gen.render gen (Shrink_tree.root tree) in
  check (rendered = "<from: frequency[1]>") "frequency rendered %S" rendered

(* The Gen doc law: a composite prints exactly when all its components
   print — including the choice combinators. *)
let one_of_over_printed_branches_derives_printer () =
  let gen = Gen.(one_of [ int_range 0 9; int_range 100 199 ]) in
  check (Gen.render_value gen 5 = Some "5") "one_of did not derive a printer";
  let tree = find_sample gen (fun v -> v >= 100) in
  let rendered = Gen.render gen (Shrink_tree.root tree) in
  check
    (rendered = string_of_int (root_value tree))
    "printed one_of rendered %S, not the value" rendered;
  (* Shrunk candidates render as values too, including branch re-generation
     candidates. *)
  let child = first_child tree in
  check
    (Gen.render gen (Shrink_tree.root child) = string_of_int (root_value child))
    "a shrunk printed one_of candidate rendered %S"
    (Gen.render gen (Shrink_tree.root child));
  (* The derived printer feeds enclosing generators, both derived pps and
     printerless provenance: composition cannot lose the printer. *)
  let paired = Gen.(pair gen nat) in
  let tree = Gen.sample paired (state 0) in
  let a, b = root_value tree in
  check
    (Gen.render paired (Shrink_tree.root tree) = Printf.sprintf "(%d, %d)" a b)
    "pair over a printed one_of rendered %S"
    (Gen.render paired (Shrink_tree.root tree));
  let mapped = Gen.map Fun.id gen in
  let tree = Gen.sample mapped (state 1) in
  check
    (Gen.render mapped (Shrink_tree.root tree)
    = Printf.sprintf "<from: %d>" (root_value tree))
    "map over a printed one_of rendered %S"
    (Gen.render mapped (Shrink_tree.root tree))

let frequency_over_printed_branches_derives_printer () =
  let gen = Gen.(frequency [ (1, nat); (3, int_range 100 199) ]) in
  check (Gen.render_value gen 7 = Some "7") "frequency did not derive a printer";
  let tree = Gen.sample gen (state 0) in
  let rendered = Gen.render gen (Shrink_tree.root tree) in
  check
    (rendered = string_of_int (root_value tree))
    "printed frequency rendered %S, not the value" rendered;
  (* One printerless branch forfeits the derivation for the whole choice. *)
  let mixed = Gen.(frequency [ (1, nat); (1, constant 5) ]) in
  check (Gen.render_value mixed 5 = None) "a mixed frequency derived a printer"

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
      match Gen.sample gen (state 0) with
      | exception Invalid_argument _ -> ()
      | _ -> failf "frequency (%s) sampled successfully" name)
    cases

let sized_shrinks_the_size_first () =
  let gen = Gen.sized (fun n -> Gen.(list ~size:(constant n) nat)) in
  let tree = find_sample gen (fun l -> List.length l >= 1) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = []) "sized list minimized to length %d" (List.length minimum)

let such_that_filters_generation_and_shrinking () =
  let even = Gen.such_that (fun n -> n mod 2 = 0) Gen.int in
  for index = 0 to 9 do
    let tree = Gen.sample even (state index) in
    explore ~limit:100 tree (fun v ->
        check (v mod 2 = 0) "such_that candidate %d is odd" v)
  done;
  let tree = find_sample even (fun v -> v <> 0) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 0) "even int minimized to %d" minimum;
  check
    (Gen.render_value even 4 = Some "4")
    "such_that dropped the underlying printer"

let such_that_exhaustion_is_a_discard () =
  let gen = Gen.such_that ~max_tries:3 (fun _ -> false) Gen.nat in
  (match Gen.sample gen (state 0) with
  | exception Gen.Rejected -> ()
  | _ -> failf "unsatisfiable such_that sampled successfully");
  let gen = Gen.such_that ~max_tries:0 (fun _ -> true) Gen.nat in
  match Gen.sample gen (state 0) with
  | exception Invalid_argument _ -> ()
  | _ -> failf "such_that ~max_tries:0 sampled successfully"

(* ───── Composition and provenance ───── *)

let map_provenance_shows_the_underlying_draw () =
  let gen = Gen.map succ Gen.int in
  let tree = Gen.sample gen (state 1) in
  let rendered = Gen.render gen (Shrink_tree.root tree) in
  check
    (rendered = Printf.sprintf "<from: %d>" (root_value tree - 1))
    "mapped int rendered %S for %d" rendered (root_value tree);
  (* Provenance follows shrinking: a candidate renders its own draws. *)
  let tree = find_sample gen (fun v -> v <> 1) in
  let child = first_child tree in
  let rendered = Gen.render gen (Shrink_tree.root child) in
  check
    (rendered = Printf.sprintf "<from: %d>" (root_value child - 1))
    "shrunk mapped int rendered %S for %d" rendered (root_value child)

let bind_keeps_inner_constraints_while_shrinking () =
  let gen =
    Gen.(
      let* n = int_range 1 3 in
      list ~size:(constant n) nat)
  in
  for index = 0 to 9 do
    let tree = Gen.sample gen (state index) in
    explore ~limit:200 tree (fun l ->
        let n = List.length l in
        check (n >= 1 && n <= 3) "bound list candidate has length %d" n)
  done;
  let tree = Gen.sample gen (state 0) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = [ 0 ]) "bound list minimized to length %d"
    (List.length minimum)

let bind_provenance_chains_draws () =
  let gen = Gen.(bind nat (fun n -> constant n)) in
  let tree = Gen.sample gen (state 4) in
  let rendered = Gen.render gen (Shrink_tree.root tree) in
  check
    (rendered = Printf.sprintf "<from: %d>" (root_value tree))
    "bind rendered %S for %d" rendered (root_value tree)

let letops_compose () =
  let gen =
    Gen.(
      let+ a = nat and+ b = nat in
      a + b)
  in
  let tree = find_sample gen (fun v -> v > 0) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 0) "let+/and+ sum minimized to %d" minimum

let with_pp_attaches_and_survives_map () =
  let custom ppf n = Format.fprintf ppf "N=%d" n in
  let inner = Gen.with_pp custom (Gen.map succ Gen.int) in
  let tree = Gen.sample inner (state 5) in
  check
    (Gen.render inner (Shrink_tree.root tree)
    = Printf.sprintf "N=%d" (root_value tree))
    "with_pp did not render directly";
  check
    (Gen.render_value inner 7 = Some "N=7")
    "with_pp did not expose the printer";
  (* Mapping over a printed generator keeps the printed form in the
     provenance: composition cannot lose the printer. *)
  let outer = Gen.map (fun n -> -n) inner in
  let tree = Gen.sample outer (state 6) in
  let rendered = Gen.render outer (Shrink_tree.root tree) in
  check
    (rendered = Printf.sprintf "<from: N=%d>" (-root_value tree))
    "mapped with_pp rendered %S for %d" rendered (root_value tree)

let mixed_one_of_provenance_keeps_branch_printer () =
  (* One branch prints, one does not: the choice cannot derive a printer,
     and provenance keeps the printed branch's rendering inside its label. *)
  let custom ppf n = Format.fprintf ppf "N=%d" n in
  let gen = Gen.(one_of [ with_pp custom (constant 5); constant 9 ]) in
  check (Gen.render_value gen 5 = None) "a mixed one_of derived a printer";
  let printed = find_sample gen (fun v -> v = 5) in
  let rendered = Gen.render gen (Shrink_tree.root printed) in
  check
    (rendered = "<from: one_of[0] N=5>")
    "labelled printed branch rendered %S" rendered;
  let printerless = find_sample gen (fun v -> v = 9) in
  let rendered = Gen.render gen (Shrink_tree.root printerless) in
  check
    (rendered = "<from: one_of[1]>")
    "labelled printerless branch rendered %S" rendered

(* The RFC's shape example, without [with_pp]: the counterexample renders
   as labelled primitive draws. *)
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

let shape_provenance_matches_the_rfc_form () =
  let rect_tree =
    find_sample shape_gen (function Rect _ -> true | Circle _ -> false)
  in
  let rendered = Gen.render shape_gen (Shrink_tree.root rect_tree) in
  check
    (starts_with "<from: one_of[1] (" rendered && contains ", " rendered)
    "Rect provenance rendered %S" rendered;
  let circle_tree =
    find_sample shape_gen (function Circle _ -> true | Rect _ -> false)
  in
  let rendered = Gen.render shape_gen (Shrink_tree.root circle_tree) in
  check
    (starts_with "<from: one_of[0] " rendered)
    "Circle provenance rendered %S" rendered;
  let pp_shape ppf = function
    | Circle r -> Format.fprintf ppf "Circle %g" r
    | Rect (w, h) -> Format.fprintf ppf "Rect (%g, %g)" w h
  in
  let printed = Gen.with_pp pp_shape shape_gen in
  let tree = Gen.sample printed (state 0) in
  check
    (starts_with "Circle" (Gen.render printed (Shrink_tree.root tree))
    || starts_with "Rect" (Gen.render printed (Shrink_tree.root tree)))
    "with_pp shape rendered %S"
    (Gen.render printed (Shrink_tree.root tree))

(* ───── Printing totality ───── *)

let render_is_total_over_shrink_trees () =
  let check_gen : type a. string -> a Gen.t -> unit =
   fun name gen ->
    for index = 0 to 4 do
      let tree = Gen.sample gen (state index) in
      let visited = ref 0 in
      let rec go tree =
        if !visited >= 50 then raise_notrace Exit;
        incr visited;
        let rendered = Gen.render gen (Shrink_tree.root tree) in
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
  let tree = Gen.sample gen (state 0) in
  check
    (starts_with "<printer raised" (Gen.render gen (Shrink_tree.root tree)))
    "raising printer rendered %S"
    (Gen.render gen (Shrink_tree.root tree));
  match Gen.render_value gen 3 with
  | Some rendered ->
      check
        (starts_with "<printer raised" rendered)
        "render_value let the exception through: %S" rendered
  | None -> failf "with_pp lost its printer"

let provenance_is_bounded () =
  let gen = Gen.(map (fun l -> l) (list ~size:(constant 300) nat)) in
  let tree = Gen.sample gen (state 0) in
  let rendered = Gen.render gen (Shrink_tree.root tree) in
  check
    (String.length rendered <= 250)
    "provenance rendered %d bytes" (String.length rendered);
  check
    (String.length rendered >= 3
    && String.sub rendered (String.length rendered - 4) 4 = "…>")
    "truncated provenance does not end with an ellipsis: %S" rendered

let render_value_reports_printer_presence () =
  check (Gen.render_value Gen.int 42 = Some "42") "int printer missing";
  check
    (Gen.render_value Gen.(list nat) [ 1; 2 ] = Some "[1; 2]")
    "list printer missing";
  check
    (Gen.render_value (Gen.map succ Gen.int) 3 = None)
    "map kept a printer it cannot have"

(* ───── Adversarial additions ───── *)

(* [find_sample] for generators whose sampling can legitimately discard. *)
let find_sample_skipping_discards ?(max_index = 10_000) gen accept =
  let rec loop index =
    if index >= max_index then
      failf "no matching sample within %d cases" max_index
    else
      match Gen.sample gen (state index) with
      | tree -> if accept (root_value tree) then tree else loop (index + 1)
      | exception Gen.Rejected -> loop (index + 1)
  in
  loop 0

(* A shrink candidate whose re-generation exhausts a [such_that] budget must
   be skipped, not raise: memoized child cells cache exceptions, so a raising
   cell would also hide every later sibling candidate. *)
let rejected_bind_candidates_are_skipped () =
  let gen =
    Gen.(
      bind (int_range 1 10) (fun n ->
          if n = 1 then such_that ~max_tries:1 (fun _ -> false) nat
          else constant n))
  in
  let tree = find_sample_skipping_discards gen (fun v -> v >= 3) in
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = 2)
    "bind with a rejecting candidate minimized to %d, not 2 (candidate 1 must \
     be skipped, siblings kept)"
    minimum

let rejected_one_of_candidates_are_skipped () =
  let gen =
    Gen.(one_of [ such_that ~max_tries:1 (fun _ -> false) nat; constant 7 ])
  in
  let tree = find_sample_skipping_discards gen (fun v -> v = 7) in
  let minimum, steps = minimize (fun _ -> true) tree in
  check
    (minimum = 7 && steps = 0)
    "one_of with a rejecting branch shrank to %d in %d steps" minimum steps

let int_range_negative_bounds_shrink_to_high () =
  let gen = Gen.int_range (-100) (-10) in
  for index = 0 to 9 do
    let tree = Gen.sample gen (state index) in
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

(* Structural validity only: correct lead/continuation shapes, no overlong or
   surrogate analysis — enough to detect a truncation that splits a char. *)
let is_valid_utf8 text =
  let length = String.length text in
  let rec go index =
    if index >= length then true
    else
      let byte = Char.code text.[index] in
      if byte < 0x80 then go (index + 1)
      else
        let width =
          if byte land 0xE0 = 0xC0 then 2
          else if byte land 0xF0 = 0xE0 then 3
          else if byte land 0xF8 = 0xF0 then 4
          else 0
        in
        if width = 0 || index + width > length then false
        else
          let continuation offset =
            Char.code text.[index + offset] land 0xC0 = 0x80
          in
          continuation 1
          && (width < 3 || continuation 2)
          && (width < 4 || continuation 3)
          && go (index + width)
  in
  go 0

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
  let starved =
    Gen.(list ~size:(such_that ~max_tries:1 (fun _ -> false) nat) nat)
  in
  match Gen.sample starved (state 0) with
  | exception Gen.Rejected -> ()
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
    let tree = Gen.sample gen (state index) in
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

(* An explicit [with_pp] must win over B6's derived printer everywhere: when
   the choice renders directly and inside the provenance of an enclosing
   printerless generator (no resurrection of the derived printer). *)
let with_pp_overrides_derived_choice_printer () =
  let custom ppf n = Format.fprintf ppf "N=%d" n in
  let overridden = Gen.(with_pp custom (one_of [ int_range 0 9; nat ])) in
  check
    (Gen.render_value overridden 5 = Some "N=5")
    "with_pp did not override the derived choice printer";
  let tree = Gen.sample overridden (state 0) in
  check
    (Gen.render overridden (Shrink_tree.root tree)
    = Printf.sprintf "N=%d" (root_value tree))
    "overridden choice rendered %S"
    (Gen.render overridden (Shrink_tree.root tree));
  let mapped = Gen.map Fun.id overridden in
  let tree = Gen.sample mapped (state 1) in
  check
    (Gen.render mapped (Shrink_tree.root tree)
    = Printf.sprintf "<from: N=%d>" (root_value tree))
    "the derived printer resurfaced in provenance: %S"
    (Gen.render mapped (Shrink_tree.root tree))

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
    let tree = Gen.sample ident (state index) in
    explore ~limit:100 tree (fun s ->
        let n = String.length s in
        check (n >= 1 && n <= 8) "identifier candidate has length %d" n;
        String.iter
          (fun c -> check (in_alphabet c) "identifier char %C off-alphabet" c)
          s)
  done;
  let tree = Gen.sample ident (state 0) in
  let rendered = Gen.render ident (Shrink_tree.root tree) in
  check (starts_with "\"" rendered) "identifier lost its printer: %S" rendered;
  let minimum, _ = minimize (fun _ -> true) tree in
  check
    (minimum = "a" || minimum = "-")
    "identifier minimized to %S, not a single boundary char" minimum

let provenance_truncation_respects_utf8 () =
  List.iter
    (fun pad ->
      (* [pad] shifts the byte at which the budget cuts, so at least one case
         lands the cut inside a two-byte character. *)
      let wide ppf _ =
        Format.pp_print_string ppf
          (pad ^ String.concat "" (List.init 200 (fun _ -> "é")))
      in
      let gen = Gen.(map Fun.id (with_pp wide nat)) in
      let tree = Gen.sample gen (state 0) in
      let rendered = Gen.render gen (Shrink_tree.root tree) in
      check (is_valid_utf8 rendered)
        "truncated provenance (pad %S) is not valid UTF-8: %S" pad rendered)
    [ ""; "x" ]

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
    ("float is finite", float_is_finite);
    ( "float_any covers nan and shrink terminates",
      float_any_covers_nan_and_shrink_terminates );
    ("float_range stays in bounds", float_range_stays_in_bounds);
    ( "float_range invalid raises at sample time",
      float_range_invalid_raises_at_sample_time );
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
    ( "bytes_of respects size and character generator",
      bytes_of_respects_size_and_character_generator );
    ("list shrinks structurally", list_shrinks_structurally);
    ( "list with size keeps length in bounds",
      list_with_size_keeps_length_in_bounds );
    ( "list negative size raises at sample time",
      list_negative_size_raises_at_sample_time );
    ("array shrinks to empty", array_shrinks_to_empty);
    ("option offers None first", option_offers_none_first);
    ("result generates both constructors", result_generates_both_constructors);
    ("pair shrinks left first to zeroes", pair_shrinks_left_first_to_zeroes);
    ("triple and quad minimize to zeroes", triple_and_quad_minimize_to_zeroes);
    ( "constant is a leaf and asks for a printer",
      constant_is_a_leaf_and_asks_for_a_printer );
    ("pure is constant", pure_is_constant);
    ( "of_list picks uniformly and shrinks toward the head",
      of_list_picks_uniformly_and_shrinks_toward_head );
    ( "of_list singleton is a leaf and empty raises",
      of_list_singleton_is_a_leaf_and_empty_raises );
    ("one_of [] raises at sample time", one_of_empty_raises_at_sample_time);
    ( "one_of picks all branches and shrinks to earlier",
      one_of_picks_all_branches_and_shrinks_to_earlier );
    ( "one_of over printed branches derives a printer",
      one_of_over_printed_branches_derives_printer );
    ( "frequency respects weights and labels",
      frequency_respects_weights_and_labels );
    ( "frequency over printed branches derives a printer",
      frequency_over_printed_branches_derives_printer );
    ( "frequency invalid raises at sample time",
      frequency_invalid_raises_at_sample_time );
    ("sized shrinks the size first", sized_shrinks_the_size_first);
    ( "such_that filters generation and shrinking",
      such_that_filters_generation_and_shrinking );
    ("such_that exhaustion is a discard", such_that_exhaustion_is_a_discard);
    ( "map provenance shows the underlying draw",
      map_provenance_shows_the_underlying_draw );
    ( "bind keeps inner constraints while shrinking",
      bind_keeps_inner_constraints_while_shrinking );
    ("bind provenance chains draws", bind_provenance_chains_draws);
    ("letops compose", letops_compose);
    ("with_pp attaches and survives map", with_pp_attaches_and_survives_map);
    ( "mixed one_of provenance keeps branch printer",
      mixed_one_of_provenance_keeps_branch_printer );
    ( "shape provenance matches the RFC form",
      shape_provenance_matches_the_rfc_form );
    ("render is total over shrink trees", render_is_total_over_shrink_trees);
    ("raising printer is contained", raising_printer_is_contained);
    ("provenance is bounded", provenance_is_bounded);
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
    ("provenance truncation respects UTF-8", provenance_truncation_respects_utf8);
  ]

let tests = List.map (fun (name, fn) -> test name fn) suite
