(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Mutsem_fixtures compiles covsem_fixtures.ml, mutsem_order.ml and
   mutsem_boom.ml through ppx_windtrap.mutate, and Mutsem_baseline compiles
   the same sources with no rewriter. Every claim compares the instrumented
   copy with its twin, never with an expectation that can be edited to match
   a defect. *)

open Windtrap
module Mutate = Windtrap_runtime.Mutate

let strf = Printf.sprintf

(* An instrumented module satisfies its twin's signature: no binding lost a
   type variable or changed shape. The rewriter adds a module named after the
   source path, so a twin instrumented by accident has one the instrumented
   copy lacks, and these coercions no longer compile. *)
module _ : module type of Mutsem_baseline.Covsem_fixtures =
  Mutsem_fixtures.Covsem_fixtures

module _ : module type of Mutsem_baseline.Mutsem_order =
  Mutsem_fixtures.Mutsem_order

module _ : module type of Mutsem_baseline.Mutsem_boom =
  Mutsem_fixtures.Mutsem_boom

module type Fixture = module type of Mutsem_baseline.Covsem_fixtures

type fixture = (module Fixture)

let instrumented : fixture = (module Mutsem_fixtures.Covsem_fixtures)
let twin : fixture = (module Mutsem_baseline.Covsem_fixtures)

(* Witnesses *)

(* [observe reset f] is what [f] shows, or the exception it raises. The trace
   is emptied first: a witness that raises leaves its tags behind. *)
let observe reset f =
  ignore (reset () : string);
  match f () with s -> s | exception e -> "raised " ^ Printexc.to_string e

let battery reset witnesses =
  List.map (fun (name, f) -> (name, observe reset f)) witnesses

let instrumented_battery () =
  battery Mutsem_fixtures.Mutsem_order.trace
    Mutsem_fixtures.Mutsem_order.witnesses

let twin_battery () =
  battery Mutsem_baseline.Mutsem_order.trace
    Mutsem_baseline.Mutsem_order.witnesses

(* The registry *)

(* Under --instrument-with the windtrap core this suite links registers sites
   of its own. Basenames, since the twin's copies sit one directory down. *)
let fixture_sources =
  [ "covsem_fixtures.ml"; "mutsem_boom.ml"; "mutsem_order.ml" ]

let is_fixture (id : Mutate.id) =
  List.mem (Filename.basename id.file) fixture_sources

let fixture_catalogue () =
  List.filter (fun (m : Mutate.mutant) -> is_fixture m.id) (Mutate.catalogue ())

let reach = list (pair string int)

let drain_fixtures () =
  let rows (r : Mutate.reached) =
    if is_fixture r.mutant.id then Some (r.mutant.id.rewrite, r.hits) else None
  in
  List.filter_map rows (Mutate.drain ())

(* [reached f] is [f ()] and the fixture mutants its evaluation reached, as
   [(rewrite, evaluations)] in catalogue order. *)
let reached f =
  ignore (Mutate.drain () : Mutate.reached list);
  Mutate.next_epoch ();
  let value = f () in
  (value, drain_fixtures ())

(* The first drain holds what module initialization reached, and any test may
   drain. *)
let reached_at_load = drain_fixtures ()

(* [arm] disarms the armed mutant before it resolves, so arming an identifier
   of no catalogued file leaves nothing armed. *)
let disarm () =
  let nowhere =
    { Mutate.file = "<nowhere>.ml"; line = 1; col = 0; rewrite = "not" }
  in
  match Mutate.arm nowhere with
  | Error (Mutate.Uncatalogued _) -> ()
  | Ok _ | Error (Malformed _ | Unmatched _ | Ambiguous _) ->
      fail "an identifier of no catalogued file armed a mutant"

let family (m : Mutate.mutant) =
  match m.id.rewrite with
  | "not" -> "neg"
  | "lt" | "le" | "gt" | "ge" | "eq" | "neq" -> "cmp"
  | "and" | "or" -> "con"
  | "add" | "sub" | "fadd" | "fsub" -> "ari"
  | other -> "unknown " ^ other

let families mutants = List.sort_uniq String.compare (List.map family mutants)

let duplicates names =
  let rec loop acc = function
    | a :: (b :: _ as rest) ->
        loop (if String.equal a b then a :: acc else acc) rest
    | [ _ ] | [] -> List.rev acc
  in
  loop [] (List.sort String.compare names)

(* Registration *)

let sources () =
  let file (m : Mutate.mutant) = Windtrap_test_support.slashed m.id.file in
  List.sort_uniq String.compare (List.map file (fixture_catalogue ()))

let in_this_directory name = "test/unit/semantics/mutate/" ^ name

let registration =
  group "Registration"
    [
      test "the three instrumented sources register at load, and no other"
        (fun () ->
          equal (list string)
            (List.map in_this_directory fixture_sources)
            (sources ()));
      test "no site is evaluated at module load" (fun () ->
          equal reach [] reached_at_load);
    ]

(* The catalogue *)

let catalogue =
  group "The catalogue"
    [
      test "the four operator families are catalogued" (fun () ->
          equal (list string)
            [ "ari"; "cmp"; "con"; "neg" ]
            (families (fixture_catalogue ())));
      test "an identifier names one site" (fun () ->
          let id (m : Mutate.mutant) = Mutate.id_to_string m.id in
          equal (list string) []
            (duplicates (List.map id (fixture_catalogue ()))));
    ]

(* Operand order *)

(* The order the rewriter's tuple binding fixes is the compiler's, which the
   language leaves open: a compiler that changes it fails these rows with
   windtrap unchanged, and the rows below then name the damage. *)
let compiler_order =
  let module Twin = Mutsem_baseline.Mutsem_order in
  [
    ("a < b", "t | r,l", fun () -> Twin.show (Twin.cmp_lt 1 2));
    ("a + b", "3 | r,l", fun () -> Twin.show (Twin.ari_add 1 2));
    ("a +. b", "3.75 | r,l", fun () -> Twin.show (Twin.ari_fadd 1.5 2.25));
    ( "the escaping exception of a < b",
      "r | r",
      fun () -> Twin.show (Twin.cmp_exception_order ()) );
    ( "the escaping exception of a + b",
      "r | r",
      fun () -> Twin.show (Twin.ari_exception_order ()) );
    ("a = b", "f | r,l", fun () -> Twin.show (Twin.cmp_eq 1 2));
  ]

let as_twin (name, f) =
  let twin_f =
    require_some ~msg:"the twin's witness of that name"
      (List.assoc_opt name Mutsem_baseline.Mutsem_order.witnesses)
  in
  equal string
    (observe Mutsem_baseline.Mutsem_order.trace twin_f)
    (observe Mutsem_fixtures.Mutsem_order.trace f)

let operand_order =
  group "Operand order"
    [
      cases "the compiler evaluates the right operand first"
        ~name:(fun (name, _, _) -> name)
        compiler_order
        (fun (_, expected, f) ->
          equal string expected (observe Mutsem_baseline.Mutsem_order.trace f));
      cases "an instrumented witness evaluates as its twin" ~name:fst
        Mutsem_fixtures.Mutsem_order.witnesses as_twin;
    ]

(* Arming *)

(* A rewriter that emitted no guard would pass every comparison with the
   twin; a family whose mutants the witnesses cannot see is compared in
   vain. The budget stops a mutated loop that no longer ends. *)
let observable_families () =
  let original = twin_battery () in
  let changes (m : Mutate.mutant) =
    ignore
      (require_ok ~pp:Mutate.pp_arm_error (Mutate.arm ~budget:1_000_000 m.id)
        : Mutate.mutant);
    Mutate.reset_reach ();
    let armed = instrumented_battery () in
    disarm ();
    armed <> original
  in
  let mutants =
    List.filter
      (fun (m : Mutate.mutant) ->
        String.equal (Filename.basename m.id.file) "mutsem_order.ml")
      (fixture_catalogue ())
  in
  equal (list string)
    [ "ari"; "cmp"; "con"; "neg" ]
    (families (List.filter changes mutants));
  equal ~msg:"after disarming"
    (list (pair string string))
    original (instrumented_battery ())

let arming =
  group "Arming"
    [
      test "every operator family has a mutant a witness observes"
        observable_families;
    ]

(* Tail calls *)

let non_tail_growth () =
  let growth (shallow, deep) = deep - shallow in
  equal ~msg:"the instrumented copy" int 99_000
    (growth (Mutsem_fixtures.Mutsem_order.control_depths ()));
  equal ~msg:"the twin" int 99_000
    (growth (Mutsem_baseline.Mutsem_order.control_depths ()))

(* A tail call leaves the depth at the base case the same at 1,000 and at
   100,000 levels. Depths are not compared across the twins, whose inlining
   differs. *)
let constant_depths rows =
  equal
    (list (pair string int))
    (List.map (fun (name, shallow, _) -> (name, shallow)) rows)
    (List.map (fun (name, _, deep) -> (name, deep)) rows)

let rec non_tail_map f = function
  | [] -> []
  | x :: xs -> f x :: non_tail_map f xs

(* A lost tail_mod_cons consumes stack, which only a bounded stack shows: the
   dune action sets OCAMLRUNPARAM=l=1M, where this length overflows. *)
let tail_mod_cons () =
  let xs = List.init 2_000_000 Fun.id in
  raises ~msg:"the stack is not bounded: the dune action sets l=1M"
    Stack_overflow (fun () -> non_tail_map succ xs);
  equal (list int)
    (Mutsem_baseline.Covsem_fixtures.tmc_map succ xs)
    (Mutsem_fixtures.Covsem_fixtures.tmc_map succ xs)

let tail_calls =
  group "Tail calls"
    [
      test "the depth grows one frame per level of a non-tail call"
        non_tail_growth;
      test "an instrumented tail call leaves no frame" (fun () ->
          constant_depths (Mutsem_fixtures.Mutsem_order.tail_depths ()));
      test "a tail call of the twin leaves no frame" (fun () ->
          constant_depths (Mutsem_baseline.Mutsem_order.tail_depths ()));
      test "a tail_mod_cons map runs in constant stack" tail_mod_cons;
    ]

(* Results *)

let traced show (value, tags) =
  strf "%s %s" (show value) (String.concat "," tags)

let outcomes : (string * (fixture -> string)) list =
  [
    ("countdown 1,000", fun (module X : Fixture) -> X.countdown 1_000);
    ("even 1,000", fun (module X : Fixture) -> Bool.to_string (X.even 1_000));
    ("odd 1,001", fun (module X : Fixture) -> Bool.to_string (X.odd 1_001));
    ( "cps_count 1,000",
      fun (module X : Fixture) -> Int.to_string (X.cps_count 1_000 Fun.id) );
    ( "pipe_down 1,000",
      fun (module X : Fixture) -> Int.to_string (X.pipe_down 1_000) );
    ("any_odd 7", fun (module X : Fixture) -> Bool.to_string (X.any_odd 7));
    ("any_odd 8", fun (module X : Fixture) -> Bool.to_string (X.any_odd 8));
    ("all_even 4", fun (module X : Fixture) -> Bool.to_string (X.all_even 4));
    ("all_even 3", fun (module X : Fixture) -> Bool.to_string (X.all_even 3));
    ("or_let 1,000", fun (module X : Fixture) -> Bool.to_string (X.or_let 1_000));
    ( "or_match 1,000",
      fun (module X : Fixture) -> Bool.to_string (X.or_match 1_000) );
    ("or_if 1,000", fun (module X : Fixture) -> Bool.to_string (X.or_if 1_000));
    ("or_try 1,000", fun (module X : Fixture) -> Bool.to_string (X.or_try 1_000));
    ( "tmc_map over 100 elements",
      fun (module X : Fixture) ->
        String.concat ","
          (List.map Int.to_string (X.tmc_map succ (List.init 100 Fun.id))) );
    ( "order_witness true",
      fun (module X : Fixture) -> traced Int.to_string (X.order_witness true) );
    ( "order_witness false",
      fun (module X : Fixture) -> traced Int.to_string (X.order_witness false)
    );
    ( "or_trace true true",
      fun (module X : Fixture) -> traced Bool.to_string (X.or_trace true true)
    );
    ( "or_trace true false",
      fun (module X : Fixture) -> traced Bool.to_string (X.or_trace true false)
    );
    ( "or_trace false true",
      fun (module X : Fixture) -> traced Bool.to_string (X.or_trace false true)
    );
    ( "or_trace false false",
      fun (module X : Fixture) -> traced Bool.to_string (X.or_trace false false)
    );
    ( "and_trace true true",
      fun (module X : Fixture) -> traced Bool.to_string (X.and_trace true true)
    );
    ( "and_trace true false",
      fun (module X : Fixture) -> traced Bool.to_string (X.and_trace true false)
    );
    ( "and_trace false true",
      fun (module X : Fixture) -> traced Bool.to_string (X.and_trace false true)
    );
    ( "and_trace false false",
      fun (module X : Fixture) ->
        traced Bool.to_string (X.and_trace false false) );
    ( "arg_order",
      fun (module X : Fixture) ->
        traced (fun (a, b) -> strf "(%d, %d)" a b) (X.arg_order ()) );
    ("seq_order", fun (module X : Fixture) -> String.concat "," (X.seq_order ()));
    ("pipeline 3", fun (module X : Fixture) -> Int.to_string (X.pipeline 3));
    ( "pipeline_bound 3",
      fun (module X : Fixture) -> Int.to_string (X.pipeline_bound 3) );
    ( "sum_object [1; 2; 3]",
      fun (module X : Fixture) -> Int.to_string (X.sum_object [ 1; 2; 3 ]) );
    ( "poke on a new adder",
      fun (module X : Fixture) -> Int.to_string (X.poke (new X.adder)) );
    ("sum_while 10", fun (module X : Fixture) -> Int.to_string (X.sum_while 10));
    ("sum_while 0", fun (module X : Fixture) -> Int.to_string (X.sum_while 0));
    ("sum_for 10", fun (module X : Fixture) -> Int.to_string (X.sum_for 10));
    ( "letop_sum 40 2",
      fun (module X : Fixture) -> Int.to_string (X.letop_sum 40 2) );
    ("bucket 5", fun (module X : Fixture) -> X.bucket 5);
    ("bucket 50", fun (module X : Fixture) -> X.bucket 50);
    ("bucket 500", fun (module X : Fixture) -> X.bucket 500);
    ("safe_div 7 0", fun (module X : Fixture) -> Int.to_string (X.safe_div 7 0));
    ("safe_div 7 2", fun (module X : Fixture) -> Int.to_string (X.safe_div 7 2));
    ( "dispatch `Add 2 3",
      fun (module X : Fixture) -> Int.to_string (X.dispatch `Add 2 3) );
    ( "dispatch `Sub 7 3",
      fun (module X : Fixture) -> Int.to_string (X.dispatch `Sub 7 3) );
    ( "tap_ok ret_unit",
      fun (module X : Fixture) -> Bool.to_string (X.tap_ok X.ret_unit) );
    ( "tap_raise raise_unit",
      fun (module X : Fixture) ->
        match X.tap_raise X.raise_unit with
        | _ -> "returned"
        | exception Exit -> "raised Exit" );
  ]

let results =
  group "Results"
    [
      cases "the instrumented fixture computes its twin's result" ~name:fst
        outcomes (fun (_, f) -> equal string (f twin) (f instrumented));
    ]

(* Laziness *)

let lazy_reach () =
  let thunk, forced, runs = Mutsem_fixtures.Covsem_fixtures.make_thunk () in
  is_false ~msg:"forced before the first force" !forced;
  let first = reached (fun () -> Lazy.force thunk) in
  let again = reached (fun () -> Lazy.force thunk) in
  equal (pair int reach) (42, [ ("sub", 1); ("sub", 1) ]) first;
  equal (pair int reach) (42, []) again;
  equal ~msg:"runs of the body" int 1 !runs

let trivial_lazy () =
  let is_val = Lazy.is_val (Mutsem_fixtures.Covsem_fixtures.trivial ()) in
  equal ~msg:"as uninstrumented" bool (Lazy.is_val (lazy 42)) is_val;
  equal ~msg:"as the twin's" bool
    (Lazy.is_val (Mutsem_baseline.Covsem_fixtures.trivial ()))
    is_val

let laziness =
  group "Laziness"
    [
      test "the instrumented lazies force as the twin's" (fun () ->
          equal string
            (Mutsem_baseline.Mutsem_order.lazy_witness ())
            (Mutsem_fixtures.Mutsem_order.lazy_witness ()));
      test "a lazy body's sites are reached at the first force, once" lazy_reach;
      test "a trivial lazy is a value" trivial_lazy;
    ]

(* Reach counts *)

(* [sum_while n] evaluates its loop condition [n + 1] times and its sum [n]
   times, so a guarded operand evaluated twice shows in the counts. *)
let loops = [ (10, (55, [ ("lt", 11); ("sub", 10) ])); (0, (0, [ ("lt", 1) ])) ]

let reach_counts =
  group "Reach counts"
    [
      cases "a loop's sites count every evaluation"
        ~name:(fun (n, _) -> strf "sum_while %d" n)
        loops
        (fun (n, expected) ->
          equal (pair int reach) expected
            (reached (fun () -> Mutsem_fixtures.Covsem_fixtures.sum_while n)));
      test "a skipped right arm adds no evaluation" (fun () ->
          equal
            (pair (pair bool (list string)) reach)
            ((false, [ "left" ]), [ ("or", 1) ])
            (reached (fun () ->
                 Mutsem_fixtures.Covsem_fixtures.and_trace false true)));
    ]

let () =
  exit
    (run "mutate semantics"
       [
         registration;
         catalogue;
         operand_order;
         arming;
         tail_calls;
         results;
         laziness;
         reach_counts;
       ])
