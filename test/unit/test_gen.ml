(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(* Tests for Gen: determinism under fixed seeds, distribution smoke tests,
   integrated-shrinking invariants (candidates satisfy generator
   constraints), greedy-shrink termination and minima, and printing
   totality including the printerless placeholder. *)

open Windtrap
module Seed = Windtrap.Private.Seed
module Shrink_tree = Windtrap.Private.Shrink_tree
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

(* The report's spelling of each rendering, for assertions on a value: a
   pre-image is marked so an assertion on the value cannot pass on it. *)
let render tree =
  match rendering tree with
  | Gen.Private.Value text -> text
  | Pre_image text -> "from " ^ text
  | No_printer -> "<no printer>"

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
  check (Gen.Private.prints Gen.unit) "unit reports no printer";
  let rendered = render tree in
  check (rendered = "()") "unit rendered %S" rendered;
  check
    (Gen.Private.render_value Gen.unit () = Some "()")
    "unit lost its printer";
  (* Why it is not [pure ()]: a deriving composition over [pure ()] has no
     printer to derive from. *)
  let paired = Gen.(pair unit nat) in
  let tree = Gen.Private.sample paired (state 1) in
  let (), n = root_value tree in
  let rendered = render tree in
  check
    (rendered = Printf.sprintf "((), %d)" n)
    "pair over unit rendered %S" rendered;
  let bare = Gen.(pair (constant ()) nat) in
  check
    (not (Gen.Private.prints bare))
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

let list_exact_draws_exactly_n () =
  let gen = Gen.Private.list_exact 7 Gen.nat in
  for index = 0 to 9 do
    let tree = Gen.Private.sample gen (state index) in
    let drawn = List.length (root_value tree) in
    check (drawn = 7) "list_exact 7 drew %d elements" drawn;
    (* Only the drawn length is fixed: shrinking removes elements, so no
       candidate is longer. *)
    explore ~limit:200 tree (fun l ->
        check
          (List.length l <= 7)
          "list_exact candidate has length %d" (List.length l))
  done;
  let tree = Gen.Private.sample (Gen.Private.list_exact 0 Gen.nat) (state 0) in
  check (root_value tree = []) "list_exact 0 drew a non-empty list";
  check (no_children tree) "list_exact 0 has shrink candidates"

(* The whole point of [list_exact] over [list ~size:(constant n)]: the
   structural move set — the empty list, then contiguous chunk removals at
   descending power-of-two sizes — instead of element-wise shrinking only.
   The expectation is rebuilt here from the documented rule, so the test
   pins the whole prefix rather than a sample of it. *)
let list_exact_shrinks_with_the_structural_move_set () =
  let distinct l = List.length (List.sort_uniq compare l) = 5 in
  let gen = Gen.Private.list_exact 5 Gen.int in
  let tree = find_sample gen distinct in
  let root = root_value tree in
  let without first size =
    List.filteri (fun index _ -> index < first || index >= first + size) root
  in
  (* Chunk sizes descend from the largest power of two strictly below 5;
     starts are non-overlapping and a chunk running past the end is not
     offered. *)
  let rec removals size first acc =
    if size = 0 then List.rev acc
    else if first + size <= 5 then
      removals size (first + size) (without first size :: acc)
    else removals (size / 2) 0 acc
  in
  let expected = [] :: removals 4 0 [] in
  let candidates =
    List.of_seq (Seq.map root_value (Shrink_tree.children tree))
  in
  let structural =
    List.filteri (fun i _ -> i < List.length expected) candidates
  in
  check (structural = expected) "structural candidates of %s were %s, not %s"
    (show_ints root)
    (show_int_lists structural)
    (show_int_lists expected);
  let minimum, _ = minimize (fun _ -> true) tree in
  check (minimum = []) "list_exact minimized to a %d-element list"
    (List.length minimum);
  let sized = Gen.(list ~size:(constant 5) int) in
  let sized_tree = find_sample sized distinct in
  check
    (Seq.for_all
       (fun child -> List.length (root_value child) = 5)
       (Shrink_tree.children sized_tree))
    "the ?size path offered a shorter candidate"

(* [?keep] masks the drawn values before the tree is assembled, so a dropped
   element contributes no subtree at all. Observable three ways: the root is
   the unmasked draw filtered — same seed, same draws; no immediate candidate
   equals its parent, which masking on top of an assembled tree produces by
   deleting an element the mask already dropped; and, for a mask that is not
   closed under shrinking, no candidate anywhere is longer than the root,
   which masking on top produces by reducing a dropped element into a kept
   one. *)
let list_exact_keep_masks_the_drawn_values () =
  let even n = n mod 2 = 0 in
  let element = Gen.int_range 0 9 in
  let masked = Gen.Private.list_exact ~keep:(List.map even) 6 element in
  let drawn = Gen.Private.list_exact 6 element in
  for index = 0 to 9 do
    let root = root_value (Gen.Private.sample masked (state index)) in
    let unmasked = root_value (Gen.Private.sample drawn (state index)) in
    check
      (root = List.filter even unmasked)
      "masked root %s is not the even elements of %s" (show_ints root)
      (show_ints unmasked)
  done;
  let tree = find_sample masked (fun l -> l <> [] && List.length l < 6) in
  let root = root_value tree in
  Seq.iter
    (fun child ->
      check
        (root_value child <> root)
        "a candidate of %s equals its parent" (show_ints root))
    (Shrink_tree.children tree);
  (* [n < 50] is not closed under shrinking: a dropped element's own
     candidates run toward [0], where the mask keeps them, so masking on top
     of an assembled tree would restore a dropped element as a reduced one —
     a candidate longer than the root. *)
  let gen =
    Gen.Private.list_exact
      ~keep:(List.map (fun n -> n < 50))
      6 (Gen.int_range 0 99)
  in
  let shortened = ref 0 in
  for index = 0 to 9 do
    let tree = Gen.Private.sample gen (state index) in
    let bound = List.length (root_value tree) in
    if bound < 6 then incr shortened;
    explore ~limit:200 tree (fun l ->
        check
          (List.length l <= bound)
          "a candidate of length %d exceeds the masked root's %d"
          (List.length l) bound)
  done;
  check (!shortened > 0) "the mask dropped nothing in 10 draws — vacuous"

(* And [?keep] runs again on every candidate: reducing an element can
   invalidate it, and the cascade drops it in the same candidate. *)
let list_exact_keep_reapplies_to_every_candidate () =
  let big n = n >= 5 in
  let gen = Gen.Private.list_exact ~keep:(List.map big) 5 (Gen.int_range 0 9) in
  for index = 0 to 9 do
    let tree = Gen.Private.sample gen (state index) in
    explore ~limit:300 tree (fun l ->
        List.iter
          (fun n -> check (big n) "a candidate kept the dropped element %d" n)
          l)
  done;
  (* Not vacuous: every element's first candidate is [0], which the mask
     drops — so reducing the head yields the tail, the same list the
     single-element chunk removal already yields, and the tail appears
     twice. Without the cascade the reduction would yield [0 :: tail]. *)
  let tree = find_sample gen (fun l -> List.length l >= 2) in
  let root = root_value tree in
  let candidates =
    List.of_seq (Seq.map root_value (Shrink_tree.children tree))
  in
  let tails =
    List.length (List.filter (fun l -> l = List.tl root) candidates)
  in
  check (tails >= 2)
    "the head's reduction was not re-masked: %d candidates of %s equal %s" tails
    (show_ints root)
    (show_ints (List.tl root))

(* [keep] is caller code that the shrink search runs, so it obeys the same
   rule as every other callback here: once per tree node, memoized, never
   again when a search revisits the branch. The two calls the root pays are
   the pass before assembly and the cascade's rewrite of the root. *)
let list_exact_keep_runs_once_per_node () =
  let calls = ref 0 in
  let keep values =
    incr calls;
    List.map (fun n -> n < 50) values
  in
  let gen = Gen.Private.list_exact ~keep 4 (Gen.int_range 0 99) in
  let tree = Gen.Private.sample gen (state 0) in
  check (!calls = 2) "sampling called ~keep %d times, not twice" !calls;
  let children = List.of_seq (Shrink_tree.children tree) in
  let expected = 2 + List.length children in
  check (!calls = expected)
    "forcing %d candidates called ~keep %d times, not %d" (List.length children)
    !calls expected;
  let before = !calls in
  List.iter (fun child -> ignore (root_value child)) children;
  ignore (List.of_seq (Shrink_tree.children tree));
  check (!calls = before) "re-forcing the same nodes called ~keep %d more times"
    (!calls - before)

(* The masks a state-dependent repair actually produces at the edges: one
   that admits no call at all, and a zero-length draw. Both must be defined
   and neither may leave a candidate behind. *)
let list_exact_keep_degenerate_masks () =
  let drop_all =
    Gen.Private.list_exact ~keep:(List.map (fun _ -> false)) 5 Gen.nat
  in
  let tree = Gen.Private.sample drop_all (state 0) in
  check
    (root_value tree = [])
    "a mask dropping everything left %s"
    (show_ints (root_value tree));
  check (no_children tree) "an emptied masked list has shrink candidates";
  (* At [n = 0] the mask still runs, on the empty list, both times. *)
  let lengths = ref [] in
  let keep values =
    lengths := List.length values :: !lengths;
    List.map (fun _ -> true) values
  in
  let tree =
    Gen.Private.sample (Gen.Private.list_exact ~keep 0 Gen.nat) (state 0)
  in
  check (root_value tree = []) "list_exact 0 drew a non-empty list";
  check
    (!lengths = [ 0; 0 ])
    "~keep saw lists of lengths %s at n = 0" (show_ints !lengths)

let list_exact_wrong_length_mask_raises () =
  let gen = Gen.Private.list_exact ~keep:(fun _ -> []) 3 Gen.nat in
  (match Gen.Private.sample gen (state 0) with
  | exception Invalid_argument _ -> ()
  | _ -> failf "a wrong-length mask sampled successfully");
  (* Right at the root, wrong on a candidate: the error surfaces where the
     candidate is forced, like any other malformed generator argument. *)
  let late =
    Gen.Private.list_exact
      ~keep:(fun values ->
        if List.length values = 3 then [ true; true; true ] else [])
      3 Gen.nat
  in
  let tree = Gen.Private.sample late (state 0) in
  match Seq.iter (fun _ -> ()) (Shrink_tree.children tree) with
  | exception Invalid_argument _ -> ()
  | () -> failf "a wrong-length mask on a candidate did not raise"

let list_exact_negative_count_raises_at_sample_time () =
  match Gen.Private.sample (Gen.Private.list_exact (-1) Gen.nat) (state 0) with
  | exception Invalid_argument _ -> ()
  | _ -> failf "list_exact (-1) sampled successfully"

let list_exact_printer_derives_from_the_element_generator () =
  let gen = Gen.Private.list_exact 3 Gen.nat in
  check
    (Gen.Private.render_value gen [ 1; 2 ] = Some "[1; 2]")
    "list_exact lost the element printer";
  let tree = Gen.Private.sample gen (state 0) in
  let rendered = render tree in
  check (starts_with "[" rendered) "list_exact rendered %S" rendered;
  check
    (Gen.Private.prints
       (Gen.Private.list_exact ~keep:(List.map (fun _ -> true)) 3 Gen.nat))
    "a masked list_exact lost the element printer";
  let printerless = Gen.Private.list_exact 3 (Gen.constant 5) in
  check
    (Gen.Private.render_value printerless [ 5 ] = None)
    "list_exact derived a printer from a printerless element generator"

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
  (* The rendering says what it is; naming the remedy is the report's job,
     driven by [prints] — so the advice appears once, whichever of the
     printerless shapes the engine produced. *)
  check (render tree = "<no printer>") "constant rendered %S" (render tree);
  check (not (Gen.Private.prints gen)) "constant reports a printer";
  check (Gen.Private.render_value gen 42 = None) "constant has a printer"

let pure_is_constant () =
  let gen = Gen.pure 42 in
  let tree = Gen.Private.sample gen (state 0) in
  check (root_value tree = 42) "pure produced %d" (root_value tree);
  check (no_children tree) "pure has shrink candidates";
  check (Gen.Private.render_value gen 42 = None) "pure has a printer"

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
  check (rendered = "<no printer>") "of_list rendered %S" rendered;
  check (Gen.Private.render_value gen 20 = None) "of_list has a printer"

(* One printerless leaf forfeits the derived printer of everything built
   over it, and [with_pp] is the one way back; the printer it attaches then
   feeds the deriving combinators, and the pre-image of a [map]. *)
let leaf_printers_feed_the_derivation_law () =
  let pp = Format.pp_print_int in
  let gen = Gen.with_pp pp (Gen.of_list [ 10; 20; 30 ]) in
  check (Gen.Private.prints gen) "with_pp over of_list reports no printer";
  let tree = find_sample gen (fun v -> v = 30) in
  let rendered = render tree in
  check (rendered = "30") "printed of_list rendered %S, not the value" rendered;
  check
    (Gen.Private.render_value gen 20 = Some "20")
    "printed of_list does not render a bare value";
  (* The leaf's printer feeds the deriving combinators above it... *)
  let listed = Gen.list gen in
  check (Gen.Private.prints listed) "list over a printed leaf derived nothing";
  check
    (Gen.Private.render_value listed [ 10; 20 ] = Some "[10; 20]")
    "list over a printed leaf does not render";
  (* ...and [map], which derives no printer, renders its argument through
     it: the pre-image. *)
  let mapped = Gen.map (fun v -> (v, ())) gen in
  check (not (Gen.Private.prints mapped)) "map claimed a printer";
  let mapped_tree = find_sample mapped (fun (v, ()) -> v = 30) in
  check
    (rendering mapped_tree = Pre_image "30")
    "map over a printed leaf rendered %S" (render mapped_tree);
  let c = Gen.with_pp pp (Gen.constant 7) in
  check (Gen.Private.prints c) "with_pp over constant reports no printer";
  let c_rendered = render (Gen.Private.sample c (state 0)) in
  check (c_rendered = "7") "printed constant rendered %S" c_rendered;
  (* Without one the leaf prints nothing at all. *)
  let bare = Gen.of_list [ 10; 20; 30 ] in
  check (not (Gen.Private.prints bare)) "of_list without a printer prints"

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
  check (rendered = "<no printer>") "one_of rendered %S" rendered

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
  check (rendered = "<no printer>") "frequency rendered %S" rendered

(* The Gen doc law: a composite prints exactly when all its components
   print — including the choice combinators. *)
let one_of_over_printed_branches_derives_printer () =
  let gen = Gen.(one_of [ int_range 0 9; int_range 100 199 ]) in
  check
    (Gen.Private.render_value gen 5 = Some "5")
    "one_of did not derive a printer";
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
    (Gen.Private.render_value gen 7 = Some "7")
    "frequency did not derive a printer";
  let tree = Gen.Private.sample gen (state 0) in
  let rendered = render tree in
  check
    (rendered = string_of_int (root_value tree))
    "printed frequency rendered %S, not the value" rendered;
  (* One printerless branch forfeits the derivation for the whole choice. *)
  let mixed = Gen.(frequency [ (1, nat); (1, constant 5) ]) in
  check
    (Gen.Private.render_value mixed 5 = None)
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
    (Gen.Private.render_value even 4 = Some "4")
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
  | Value _ | No_printer ->
      failf "the chain rendered %S, not a pre-image" (render tree)

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
    (rendering tree = No_printer)
    "a map over a constant rendered %S" (render tree);
  let bound = Gen.(bind nat (fun n -> map (fun c -> (c, n)) (constant 'k'))) in
  let tree = Gen.Private.sample bound (state 0) in
  check
    (rendering tree = No_printer)
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
    (rendering tree = No_printer)
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
    (Gen.Private.render_value inner 7 = Some "N=7")
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
    (Gen.Private.render_value gen 5 = None)
    "a mixed one_of derived a printer";
  let printed = find_sample gen (fun v -> v = 5) in
  check
    (rendering printed = Value "N=5")
    "printed branch rendered %S" (render printed);
  let printerless = find_sample gen (fun v -> v = 9) in
  check
    (rendering printerless = No_printer)
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
  match Gen.Private.render_value gen 3 with
  | Some rendered ->
      check
        (starts_with "<printer raised" rendered)
        "render_value let the exception through: %S" rendered
  | None -> failf "with_pp lost its printer"

let render_value_reports_printer_presence () =
  check (Gen.Private.render_value Gen.int 42 = Some "42") "int printer missing";
  check
    (Gen.Private.render_value Gen.(list nat) [ 1; 2 ] = Some "[1; 2]")
    "list printer missing";
  check
    (Gen.Private.render_value (Gen.map succ Gen.int) 3 = None)
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
    (Gen.Private.render_value overridden 5 = Some "N=5")
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
    ("list_exact draws exactly n", list_exact_draws_exactly_n);
    ( "list_exact shrinks with the structural move set",
      list_exact_shrinks_with_the_structural_move_set );
    ( "list_exact ?keep masks the drawn values",
      list_exact_keep_masks_the_drawn_values );
    ( "list_exact ?keep re-applies to every candidate",
      list_exact_keep_reapplies_to_every_candidate );
    ("list_exact ?keep runs once per node", list_exact_keep_runs_once_per_node);
    ("list_exact ?keep degenerate masks", list_exact_keep_degenerate_masks);
    ("list_exact wrong-length mask raises", list_exact_wrong_length_mask_raises);
    ( "list_exact negative count raises at sample time",
      list_exact_negative_count_raises_at_sample_time );
    ( "list_exact printer derives from the element generator",
      list_exact_printer_derives_from_the_element_generator );
    ("array shrinks to empty", array_shrinks_to_empty);
    ("option offers None first", option_offers_none_first);
    ("result generates both constructors", result_generates_both_constructors);
    ("pair shrinks left first to zeroes", pair_shrinks_left_first_to_zeroes);
    ("triple and quad minimize to zeroes", triple_and_quad_minimize_to_zeroes);
    ( "bytes_of respects size and character generator",
      bytes_of_respects_size_and_character_generator );
    ( "constant is a leaf and asks for a printer",
      constant_is_a_leaf_and_asks_for_a_printer );
    ( "leaf printers feed the derivation law",
      leaf_printers_feed_the_derivation_law );
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
    prop "list_exact draws the requested length"
      (Gen.with_pp
         (fun ppf (n, xs) ->
           Format.fprintf ppf "(%d, [%s])" n
             (String.concat "; " (List.map string_of_int xs)))
         (Gen.bind (Gen.int_range 0 32) (fun n ->
              Gen.map (fun xs -> (n, xs)) (Gen.Private.list_exact n Gen.int))))
      (fun (n, xs) -> equal ~msg:"length" int n (List.length xs));
    prop "option is Some or None and never raises" (Gen.option Gen.int)
      (fun o ->
        is_true ~msg:"total" (match o with None -> true | Some _ -> true));
    (* Printing must be total: a counterexample that cannot be rendered
       is a failure the reader never sees. This is the one law whose
       violation would corrupt the report itself. *)
    prop "every drawn value renders"
      (Gen.pair Gen.string (Gen.list Gen.int))
      (fun (s, xs) ->
        match
          Gen.Private.render_value
            (Gen.pair Gen.string (Gen.list Gen.int))
            (s, xs)
        with
        | Some rendered ->
            is_true ~msg:"rendering is non-empty" (String.length rendered > 0)
        | None -> fail "a pair of printable generators has no printer");
  ]

let tests = List.map (fun (name, fn) -> test name fn) suite @ law_tests
let () = Windtrap.run "gen" tests
