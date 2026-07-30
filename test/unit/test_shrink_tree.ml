(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC

  Adapted from windtrap-next's test/internal/test_shrink_tree.ml, extended
  with bind and greedy-shrink-termination coverage.
  --------------------------------------------------------------------------*)

open Windtrap
module Shrink_tree = Windtrap.Private.Shrink_tree

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

let node root children = Shrink_tree.make ~root ~children:(List.to_seq children)

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
  check (!tail_calls = 1) "tail evaluated %d times instead of once" !tail_calls;
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
  check (!map_calls = 1) "map did not evaluate exactly the root at construction";
  check (!source_calls = 0) "map construction forced source children";
  let children = Shrink_tree.children mapped in
  let first = children () in
  check (!source_calls = 1) "mapped child force evaluated source %d times"
    !source_calls;
  check (!map_calls = 2) "mapped child was evaluated %d total times" !map_calls;
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

(* bind *)

let bind_substitutes_and_orders_outer_before_inner () =
  let tree = node 2 [ Shrink_tree.leaf 0; Shrink_tree.leaf 1 ] in
  let f value = node (value * 10) [ Shrink_tree.leaf ((value * 10) + 1) ] in
  let actual = Shrink_tree.bind tree f in
  let expected =
    Node
      ( 20,
        [
          (* rebound outer candidates, in order *)
          Node (0, [ Node (1, []) ]);
          Node (10, [ Node (11, []) ]);
          (* then the inner tree's own candidates *)
          Node (21, []);
        ] )
  in
  check (observe actual = expected) "bind shape or candidate order is wrong"

let bind_applies_f_lazily_and_once_per_node () =
  let f_calls = ref 0 in
  let source_calls = ref 0 in
  let tree =
    Shrink_tree.make ~root:2 ~children:(fun () ->
        incr source_calls;
        Seq.Cons (Shrink_tree.leaf 1, Seq.empty))
  in
  let f value =
    incr f_calls;
    Shrink_tree.leaf (value * 10)
  in
  let bound = Shrink_tree.bind tree f in
  check (!f_calls = 1) "construction applied f %d times instead of once"
    !f_calls;
  check (!source_calls = 0) "construction forced the outer children";
  check (Shrink_tree.root bound = 20) "bound root is not f's root";
  let children = Shrink_tree.children bound in
  (match children () with
  | Seq.Nil -> failf "bound tree lost its rebound candidate"
  | Seq.Cons (candidate, _) ->
      check (Shrink_tree.root candidate = 10) "rebound candidate root is wrong");
  check (!f_calls = 2) "forcing one candidate applied f %d times" !f_calls;
  ignore (children ());
  check (!f_calls = 2) "repeated force reran f";
  check (!source_calls = 1) "outer children were forced %d times" !source_calls

let bind_obeys_the_monad_laws_on_finite_trees () =
  let f value = node (value + 1) [ Shrink_tree.leaf (value * 2) ] in
  let g value = node (value * 3) [ Shrink_tree.leaf (value + 5) ] in
  let tree = finite_tree () in
  check
    (observe (Shrink_tree.bind (Shrink_tree.leaf 4) f) = observe (f 4))
    "bind left identity law failed";
  check
    (observe (Shrink_tree.bind tree Shrink_tree.leaf) = observe tree)
    "bind right identity law failed";
  let left = Shrink_tree.bind (Shrink_tree.bind tree f) g in
  let right =
    Shrink_tree.bind tree (fun value -> Shrink_tree.bind (f value) g)
  in
  check (observe left = observe right) "bind associativity law failed"

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
    ("root and leaf do not force children", root_and_leaf_do_not_force_children);
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
    ("map is lazy and maps each node once", map_is_lazy_and_maps_each_node_once);
    ("mapped child exception is cached", mapped_child_exception_is_cached);
    ( "bind substitutes and orders outer before inner",
      bind_substitutes_and_orders_outer_before_inner );
    ( "bind applies f lazily and once per node",
      bind_applies_f_lazily_and_once_per_node );
    ( "bind obeys the monad laws on finite trees",
      bind_obeys_the_monad_laws_on_finite_trees );
    ("pair reduces left before right", pair_reduces_left_before_right);
    ( "pair does not force right until left is exhausted",
      pair_does_not_force_right_until_left_is_exhausted );
    ("pair is natural under map", pair_is_natural_under_map);
    ( "list has exact structural then element order",
      list_has_exact_structural_then_element_order );
    ( "list chunk schedule is deterministic without duplicates",
      list_chunk_schedule_is_deterministic_without_duplicates );
    ("singleton list has no chunk removal", singleton_list_has_no_chunk_removal);
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
