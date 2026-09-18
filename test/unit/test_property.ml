(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(* Tests for Property: engine determinism under a fixed root, examples-first
   ordering and numbering, discard bookkeeping and give-up, collect/classify/
   cover accounting, greedy same-kind shrinking with its step cap, and the
   structure of the typed failure payload. *)

open Windtrap
open Windtrap.Private
module Shrink_tree = Gen_engine.Shrink_tree

(* Printf-style shims over windtrap's [fail]. [Check.*] calls inside
   property bodies are the probes the engine catches; only these shims
   escape to the runner. *)
let failf format = Printf.ksprintf (fun message -> Windtrap.fail message) format

let check condition format =
  Printf.ksprintf
    (fun message -> if not condition then Windtrap.fail message)
    format

let contains needle haystack = Text.contains_substring ~pattern:needle haystack

(* One fixed root for most tests: outcomes are deterministic across runs and
   machines (guarantee 7), so every assertion below is exact. *)
let root = 0x00c0ffee1234abcdL

let property_payload (failure : Failure.t) =
  match failure.Failure.kind with
  | Failure.Property
      { rendered; case_index; shrink_steps; timed_out; root; examples; inner }
    ->
      (rendered, case_index, shrink_steps, timed_out, root, examples, inner)
  | _ -> failf "expected a Property failure kind"

let shrink_exhausted (failure : Failure.t) =
  match failure.Failure.kind with
  | Failure.Property { shrink_exhausted; _ } -> shrink_exhausted
  | _ -> failf "expected a Property failure kind"

let payload_count (failure : Failure.t) =
  match failure.Failure.kind with
  | Failure.Property { count; _ } -> count
  | _ -> failf "expected a Property failure kind"

let expect_fail = function
  | Property.Fail { failure; stats } -> (failure, stats)
  | Property.Pass _ -> failf "expected Fail, got Pass"
  | Property.Coverage_failed _ -> failf "expected Fail, got Coverage_failed"
  | Property.Gave_up _ -> failf "expected Fail, got Gave_up"

let expect_pass = function
  | Property.Pass stats -> stats
  | Property.Fail { failure; _ } ->
      failf "expected Pass, got Fail: %s"
        (let r, _, _, _, _, _, _ = property_payload failure in
         r)
  | Property.Coverage_failed _ -> failf "expected Pass, got Coverage_failed"
  | Property.Gave_up _ -> failf "expected Pass, got Gave_up"

let expect_gave_up = function
  | Property.Gave_up stats -> stats
  | Property.Pass _ -> failf "expected Gave_up, got Pass"
  | Property.Fail _ -> failf "expected Gave_up, got Fail"
  | Property.Coverage_failed _ -> failf "expected Gave_up, got Coverage_failed"

let expect_coverage_failed = function
  | Property.Coverage_failed stats -> stats
  | Property.Pass _ -> failf "expected Coverage_failed, got Pass"
  | Property.Fail _ -> failf "expected Coverage_failed, got Fail"
  | Property.Gave_up _ -> failf "expected Coverage_failed, got Gave_up"

(* The value generated for case [index] of [path] under [root], as the engine
   derives it — used to predict and replay engine streams. *)
let value_at gen ~root ~path ~index =
  Gen_engine.value
    (Shrink_tree.root
       (Gen_engine.sample gen (Seed.make (Seed.derive ~root ~path ~index))))

(* Search for a root whose first failing generated case satisfies
   [first_ok] — keeps same-kind shrink tests deterministic without
   depending on one lucky constant. *)
let find_root gen ~path ~fails ~first_ok =
  let first_failing root =
    let rec scan index =
      if index >= 300 then None
      else
        let value = value_at gen ~root ~path ~index in
        if fails value then Some value else scan (index + 1)
    in
    scan 0
  in
  let rec try_root candidate =
    if candidate > 1_000 then failf "no suitable root below 1000"
    else
      let root = Int64.of_int candidate in
      match first_failing root with
      | Some value when first_ok value -> root
      | _ -> try_root (candidate + 1)
  in
  try_root 0

(* Determinism and replay *)

let same_inputs_same_outcome () =
  let path = "determinism" in
  let body _ x = Check.is_true (x < 800) in
  (* Both runs happen at one call site: captured assertion locations are
     stack-derived, so distinct call sites would differ there while the
     engine's own data stays identical. *)
  let outcomes =
    List.map
      (fun () -> Property.run ~root ~path (Gen.int_range 0 1000) body)
      [ (); () ]
  in
  let first, second =
    match outcomes with [ a; b ] -> (a, b) | _ -> assert false
  in
  check (first = second) "same root, path, and count must reproduce the outcome";
  let failure, _ = expect_fail first in
  let rendered, case_index, _, timed_out, recorded_root, examples, _ =
    property_payload failure
  in
  check (recorded_root = root) "failure must record the run's root seed";
  check (timed_out = None) "an ordinary failure carries no timed_out mark";
  check (not examples) "a generated case must not be flagged as an example";
  check (int_of_string rendered >= 800) "counterexample must fail the body";
  (* Replay contract: the recorded case index re-derives a failing value. *)
  let replayed =
    value_at (Gen.int_range 0 1000) ~root ~path ~index:case_index
  in
  check (replayed >= 800) "case %d must re-derive a failing value, got %d"
    case_index replayed

let different_path_different_stream () =
  let gen = Gen.int64 in
  let first = value_at gen ~root ~path:"stream one" ~index:0 in
  let second = value_at gen ~root ~path:"stream two" ~index:0 in
  check (first <> second)
    "distinct paths must not share a stream (adding a property never perturbs \
     another)"

(* Examples *)

let examples_run_first_in_order () =
  let seen = ref [] in
  let body _ x = seen := x :: !seen in
  let stats =
    expect_pass
      (Property.run ~root ~path:"examples order" ~count:(`Declared 3)
         ~examples:[ 1000; 2000 ] (Gen.int_range 0 5) body)
  in
  let order = List.rev !seen in
  check (List.length order = 5) "expected 2 examples + 3 generated bodies";
  check
    (match order with 1000 :: 2000 :: _ -> true | _ -> false)
    "examples must run first, in list order";
  List.iteri
    (fun position value ->
      if position >= 2 then
        check (value <= 5) "generated cases must follow the examples")
    order;
  check (stats.Property.cases = 5) "examples must count as passing cases"

let failing_example_fails_fast_with_printer () =
  let generated = ref 0 in
  let body _ x =
    if x >= 0 then incr generated;
    Check.is_true (x <> 7)
  in
  let outcome =
    Property.run ~root ~path:"example fail" ~examples:[ 3; 7; 11 ] Gen.int body
  in
  let failure, _ = expect_fail outcome in
  let rendered, case_index, shrink_steps, _, _, examples, inner =
    property_payload failure
  in
  check examples "the failure must be flagged as an example";
  check (case_index = 1) "example case_index must be its zero-based position";
  check (shrink_steps = 0) "examples are never shrunk";
  check (rendered = "7") "a failing example prints via the generator's printer";
  check (!generated = 2) "a failing example must stop the run";
  match inner with
  | Some { Failure.kind = Failure.Equality _; _ } -> ()
  | _ -> failf "expected the inner assertion failure of the example"

(* An example is a bare value with no tree to render a pre-image from, so a
   printerless generator renders it as the one placeholder, which carries
   its own remedy. *)
let failing_example_without_printer_renders_placeholder () =
  let printerless = Gen.map (fun x -> x) Gen.int in
  let outcome =
    Property.run ~root ~path:"example placeholder" ~examples:[ 1; 2 ]
      printerless (fun _ x -> Check.is_true (x <> 2))
  in
  let failure, _ = expect_fail outcome in
  let rendered, case_index, _, _, _, examples, _ = property_payload failure in
  check examples "the failure must be flagged as an example";
  check (case_index = 1) "case_index must be the example's position";
  check
    (rendered = "<no printer: attach one with Gen.with_pp>")
    "a printerless example renders the placeholder, got %S" rendered;
  match failure.Failure.kind with
  | Failure.Property { rendering = Failure.Value; _ } -> ()
  | _ -> failf "the placeholder is the rendered text, not a payload flag"

let discarding_example_is_counted_and_skipped () =
  let stats =
    expect_pass
      (Property.run ~root ~path:"example discard" ~count:(`Declared 2)
         ~examples:[ 1; 2; 3 ] (Gen.int_range 0 9) (fun _ x ->
           Property.assume (x <> 2)))
  in
  check (stats.Property.cases >= 4) "examples 1 and 3 plus 2 generated pass";
  check (stats.Property.discards >= 1) "the discarded example must be counted"

let examples_count_in_coverage_denominator () =
  let body ctx x = Property.cover ctx "zero" (x = 0) in
  let stats =
    expect_pass
      (Property.run ~root ~path:"examples cover" ~count:(`Declared 2)
         ~examples:[ 0; 0; 0; 0 ] (Gen.constant 1) body)
  in
  check (stats.Property.cases = 6) "4 examples + 2 generated cases";
  match stats.Property.coverage with
  | [ { Property.label = "zero"; hits = 4; satisfied = true; _ } ] -> ()
  | _ -> failf "expected zero covered by the 4 examples out of 6 cases"

(* Discards and give-up *)

let assume_exhaustion_gives_up () =
  let stats =
    expect_gave_up
      (Property.run ~root ~path:"give up" ~count:(`Declared 10) Gen.int
         (fun _ _ -> Property.reject ()))
  in
  check (stats.Property.cases = 0) "no case can pass";
  check
    (stats.Property.discards = 21)
    "the default budget allows 2 * count discards, the 21st gives up, got %d"
    stats.Property.discards

let generation_rejection_gives_up () =
  let ran = ref 0 in
  let gen = Gen.such_that (fun _ -> false) Gen.int in
  let stats =
    expect_gave_up
      (Property.run ~root ~path:"gen give up" ~count:(`Declared 3) gen
         (fun _ _ -> incr ran))
  in
  check (!ran = 0) "the body must never run when generation rejects";
  check
    (stats.Property.discards = 7)
    "every rejection must count as a discard up to the budget, got %d"
    stats.Property.discards

let explicit_max_discard_bounds_discards () =
  let attempts = ref 0 in
  let body _ x =
    incr attempts;
    Property.assume (x < 0)
  in
  let stats =
    expect_gave_up
      (Property.run ~root ~path:"max discard" ~count:(`Declared 5)
         ~max_discard:7 (Gen.int_range 0 9) body)
  in
  check (!attempts = 8) "the discard exceeding the budget gives up, got %d"
    !attempts;
  check (stats.Property.discards = 8) "all attempts discarded"

let max_discard_zero_gives_up_on_first_discard () =
  let stats =
    expect_gave_up
      (Property.run ~root ~path:"no discards" ~count:(`Declared 5)
         ~max_discard:0 Gen.int (fun _ _ -> Property.reject ()))
  in
  check (stats.Property.cases = 0) "no case can pass";
  check
    (stats.Property.discards = 1)
    "the first discard must give up, got %d" stats.Property.discards;
  (* A property that never discards is unaffected by a zero budget. *)
  let stats =
    expect_pass
      (Property.run ~root ~path:"no discards pass" ~count:(`Declared 5)
         ~max_discard:0 Gen.int (fun _ _ -> ()))
  in
  check (stats.Property.cases = 5) "all cases must pass under a zero budget"

let discarding_examples_consume_the_budget () =
  let stats =
    expect_gave_up
      (Property.run ~root ~path:"example budget" ~count:(`Declared 3)
         ~max_discard:2 ~examples:[ 1; 1; 1 ] (Gen.constant 0) (fun _ x ->
           Property.assume (x <> 1)))
  in
  check (stats.Property.cases = 0) "every example must discard";
  check
    (stats.Property.discards = 3)
    "example discards must count toward the budget, got %d"
    stats.Property.discards

let budget_is_checked_before_the_count_goal () =
  (* Even a vacuous count cannot mask a blown budget: with [count = 0] the
     case-count goal is met before any generation, but a discarding example
     has already exceeded [max_discard = 0]. *)
  let stats =
    expect_gave_up
      (Property.run ~root ~path:"budget first" ~count:(`Declared 0)
         ~max_discard:0 ~examples:[ 1 ] (Gen.constant 0) (fun _ _ ->
           Property.reject ()))
  in
  check
    (stats.Property.discards = 1)
    "the discarding example must give up the run, got %d discards"
    stats.Property.discards

let passing_examples_do_not_consume_the_budget () =
  let stats =
    expect_pass
      (Property.run ~root ~path:"pass no budget" ~count:(`Declared 3)
         ~max_discard:0 ~examples:[ 1; 2; 3 ] (Gen.constant 0) (fun _ _ -> ()))
  in
  check (stats.Property.cases = 6) "3 examples + 3 generated cases must pass";
  check (stats.Property.discards = 0) "nothing discards"

let mixed_discards_still_pass () =
  (* Half the space discards; the budget of 2 * count absorbs it. *)
  let stats =
    expect_pass
      (Property.run ~root ~path:"mixed discards" ~count:(`Declared 20)
         (Gen.int_range 0 9) (fun _ x -> Property.assume (x mod 2 = 0)))
  in
  check (stats.Property.cases = 20) "count cases must pass";
  check (stats.Property.discards > 0) "odd draws must discard"

(* Labelling *)

let classify_partitions_cases () =
  let body ctx x =
    Property.classify ctx "even" (x mod 2 = 0);
    Property.classify ctx "odd" (x mod 2 <> 0)
  in
  let stats =
    expect_pass
      (Property.run ~root ~path:"classify" ~count:(`Declared 50) Gen.int body)
  in
  check (stats.Property.cases = 50) "all cases pass";
  let total = List.fold_left (fun acc (_, n) -> acc + n) 0 stats.collected in
  check (total = 50) "each case must carry exactly one label, got %d" total;
  check
    (List.map fst stats.Property.collected
    = List.sort compare (List.map fst stats.Property.collected))
    "collected labels must be sorted"

let collect_counts_each_case_once () =
  let body ctx _ =
    Property.collect ctx "case";
    Property.collect ctx "case"
  in
  let stats =
    expect_pass
      (Property.run ~root ~path:"collect once" ~count:(`Declared 10) Gen.int
         body)
  in
  match stats.Property.collected with
  | [ ("case", 10) ] -> ()
  | _ -> failf "a label must count once per case"

let discarded_cases_do_not_commit_labels () =
  let body ctx x =
    Property.collect ctx "attempt";
    Property.assume (x mod 2 = 0)
  in
  let stats =
    expect_pass
      (Property.run ~root ~path:"discard labels" ~count:(`Declared 10)
         (Gen.int_range 0 9) body)
  in
  match stats.Property.collected with
  | [ ("attempt", 10) ] ->
      check (stats.Property.discards > 0) "some cases must have discarded"
  | _ -> failf "discarded cases must not commit their labels"

let shrink_runs_do_not_pollute_tables () =
  let body ctx x =
    Property.collect ctx "ran";
    Check.is_true (x < 5)
  in
  let outcome =
    Property.run ~root ~path:"shrink labels" (Gen.int_range 0 100) body
  in
  let _, stats = expect_fail outcome in
  check
    (stats.Property.collected = [ ("ran", stats.Property.cases) ])
    "shrink re-runs must accumulate into a scratch context only"

(* Coverage *)

let cover_satisfied_passes () =
  (* Presence, not proportion: one passing case marking the label is the
     whole demand, so a label hit once in twenty-five passes as surely as
     one hit every time. *)
  let body ctx x = Property.cover ctx "zero" (x = 0) in
  let stats =
    expect_pass
      (Property.run ~root ~path:"cover pass" ~count:(`Declared 25)
         ~examples:[ 0 ] (Gen.constant 1) body)
  in
  match stats.Property.coverage with
  | [ { Property.label = "zero"; hits = 1; satisfied = true } ] -> ()
  | _ -> failf "expected one satisfied coverage entry"

let cover_unsatisfied_fails_at_end () =
  let body ctx x = Property.cover ctx "zero" (x = 0) in
  let stats =
    expect_coverage_failed
      (Property.run ~root ~path:"cover fail" ~count:(`Declared 10)
         (Gen.constant 1) body)
  in
  check (stats.Property.cases = 10) "the full case count must still pass";
  match stats.Property.coverage with
  | [ { Property.label = "zero"; hits = 0; satisfied = false } ] -> ()
  | _ -> failf "expected one unsatisfied coverage entry"

let cover_registers_even_when_condition_is_false () =
  let body ctx x = if x mod 2 = 0 then Property.cover ctx "never" false in
  let stats =
    expect_coverage_failed
      (Property.run ~root ~path:"cover register" ~count:(`Declared 10)
         (Gen.constant 0) body)
  in
  match stats.Property.coverage with
  | [ { Property.label = "never"; hits = 0; satisfied = false } ] -> ()
  | _ -> failf "a demand must register even when its condition is false"

(* Shrinking *)

let shrinks_to_minimal_counterexample () =
  let body _ x = Check.equal Testable.int 0 x in
  let failure, _ =
    expect_fail (Property.run ~root ~path:"shrink minimal" Gen.int body)
  in
  let rendered, _, shrink_steps, _, _, _, inner = property_payload failure in
  check
    (rendered = "1" || rendered = "-1")
    "int must shrink to a unit magnitude, got %S" rendered;
  check (shrink_steps > 0) "shrinking must have taken steps";
  match inner with
  | Some { Failure.kind = Failure.Equality { expected; actual; not_ }; _ } ->
      check (expected = "0") "inner expected side is the assertion's";
      check (actual = rendered) "inner failure must describe the shrunk case";
      check (not not_) "equal is not negated"
  | _ -> failf "expected the inner Equality failure at the shrunk case"

(* The budget is fixed and sized against the primitives' descent: a quad
   of int64, every component shrunk toward its origin under a threshold
   law, is the yardstick the number was chosen against — it converges,
   far inside the budget, and is reported as converged. *)
let shrink_budget_covers_a_quad_of_int64 () =
  let gen = Gen.quad Gen.int64 Gen.int64 Gen.int64 Gen.int64 in
  let far x = Int64.compare (Int64.abs x) 3L >= 0 in
  let law _ (a, b, c, d) =
    if far a && far b && far c && far d then raise Exit
  in
  let failure, _ = expect_fail (Property.run ~root ~path:"quad" gen law) in
  let _, _, shrink_steps, _, _, _, _ = property_payload failure in
  check (shrink_steps > 0) "the search must have taken steps";
  check (shrink_steps <= 256)
    "a quad of int64 converges within one step per bit, took %d" shrink_steps;
  check
    (not (shrink_exhausted failure))
    "a converged search is not reported as stopped"

let assertion_shrink_skips_exception_candidates () =
  let path = "same-kind assertion" in
  let gen = Gen.int_range 0 100 in
  let fails value = value >= 1 in
  let root = find_root gen ~path ~fails ~first_ok:(fun value -> value >= 2) in
  let body _ x =
    if x = 1 then raise Exit else if x >= 2 then Check.fail "wanted" else ()
  in
  let failure, _ = expect_fail (Property.run ~root ~path gen body) in
  let rendered, _, _, _, _, _, inner = property_payload failure in
  check (rendered = "2")
    "the assertion goal must skip the exception trap at 1, got %S" rendered;
  match inner with
  | Some { Failure.kind = Failure.Message "wanted"; _ } -> ()
  | _ -> failf "expected the inner Message failure at the shrunk case"

let exception_shrink_skips_assertion_candidates () =
  let path = "same-kind exception" in
  let gen = Gen.int_range 0 100 in
  let fails value = value >= 1 in
  let root = find_root gen ~path ~fails ~first_ok:(fun value -> value >= 2) in
  let body _ x =
    if x = 1 then Check.fail "assertion trap"
    else if x >= 2 then raise Exit
    else ()
  in
  let failure, _ = expect_fail (Property.run ~root ~path gen body) in
  let rendered, _, _, _, _, _, inner = property_payload failure in
  check (rendered = "2")
    "the exception goal must skip the assertion trap at 1, got %S" rendered;
  match inner with
  | Some { Failure.kind = Failure.Raise { actual = Some text; _ }; _ } ->
      check (contains "Exit" text) "the inner failure must render Exit"
  | _ -> failf "expected the inner Raise failure at the shrunk case"

let shrink_rejects_discarding_candidates () =
  let path = "shrink discards" in
  let gen = Gen.int_range 0 100 in
  (* [10;100] fails, [1;9] discards, 0 passes: the shrink descent must stop
     at the discard band instead of accepting a discarding candidate. *)
  let fails value = value >= 10 in
  let root = find_root gen ~path ~fails ~first_ok:(fun value -> value >= 20) in
  let body _ x =
    if x >= 10 then Check.fail "big" else if x >= 1 then Property.assume false
  in
  (* One call site for both runs: captured assertion locations are
     stack-derived and a tail-called [Check.fail] resolves to the caller of
     [run], so distinct call sites would differ there. *)
  let outcomes =
    List.map (fun () -> Property.run ~root ~path gen body) [ (); () ]
  in
  let first, second =
    match outcomes with [ a; b ] -> (a, b) | _ -> assert false
  in
  check (first = second)
    "a run with interleaved discards and shrinking must be deterministic";
  let failure, stats = expect_fail first in
  let rendered, case_index, shrink_steps, _, _, _, _ =
    property_payload failure
  in
  let final = int_of_string rendered in
  let original = value_at gen ~root ~path ~index:case_index in
  check (final >= 10) "shrinking must not cross the discard band, got %d" final;
  check (final < original) "the counterexample must shrink below %d" original;
  check (shrink_steps > 0) "shrinking must have taken steps";
  (* The descent tries discarding candidates (the band sits between the
     counterexample and 0); those scratch runs must stay out of the run's
     discard count, which covers exactly the main-loop draws in [1;9]. *)
  let expected_discards =
    let rec scan index acc =
      if index >= case_index then acc
      else
        let value = value_at gen ~root ~path ~index in
        scan (index + 1) (if value >= 1 && value <= 9 then acc + 1 else acc)
    in
    scan 0 0
  in
  check
    (stats.Property.discards = expected_discards)
    "shrink-time discards must not count in stats.discards: expected %d, got %d"
    expected_discards stats.Property.discards

let skip_candidate_is_rejected_during_shrink () =
  let path = "skip candidate" in
  let gen = Gen.int_range 0 100 in
  let fails value = value >= 2 in
  let root = find_root gen ~path ~fails ~first_ok:(fun value -> value >= 3) in
  let body _ x =
    if x = 1 then raise (Failure.Skip_test (Some "trap"))
    else if x >= 2 then Check.fail "wanted"
  in
  match Property.run ~root ~path gen body with
  | exception Failure.Skip_test _ ->
      failf "a skipping shrink candidate must not skip the test"
  | outcome ->
      let failure, _ = expect_fail outcome in
      let rendered, _, _, timed_out, _, _, _ = property_payload failure in
      check (rendered = "2")
        "the skip trap at 1 must be rejected during shrinking, got %S" rendered;
      check (timed_out = None)
        "a rejected skipping candidate must not mark the failure timed out"

let rendering_of (failure : Failure.t) =
  match failure.Failure.kind with
  | Failure.Property { rendering; _ } -> rendering
  | _ -> failf "expected a Property failure kind"

(* A counterexample with nothing to print renders as the one placeholder,
   which carries its own remedy: the payload flags it as the value, and the
   renderer adds nothing. *)
let printerless_counterexample_renders_placeholder () =
  let printerless = Gen.map (fun x -> x * 2) (Gen.constant 7) in
  let failure, _ =
    expect_fail
      (Property.run ~root ~path:"placeholder" printerless (fun _ x ->
           Check.is_true (x < 10)))
  in
  let rendered, _, _, _, _, _, _ = property_payload failure in
  check
    (rendered = "<no printer: attach one with Gen.with_pp>")
    "a printerless counterexample renders the placeholder, got %S" rendered;
  check
    (rendering_of failure = Failure.Value)
    "a printerless counterexample must be flagged as the value"

(* The pre-image reported is the pre-image of the shrunk value: the search
   walks one tree, so the two cannot drift apart. *)
let mapped_counterexample_renders_its_shrunk_pre_image () =
  let mapped = Gen.map (fun x -> x * 2) (Gen.int_range 0 50) in
  let failure, _ =
    expect_fail
      (Property.run ~root ~path:"pre-image" mapped (fun _ x ->
           Check.is_true (x < 10)))
  in
  let rendered, _, _, _, _, _, _ = property_payload failure in
  check (rendered = "5")
    "the pre-image of the minimal counterexample 10 is 5, got %S" rendered;
  check
    (rendering_of failure = Failure.Pre_image)
    "a mapped counterexample must be flagged as a pre-image";
  (* An explicit printer on the image wins, and the flag says value. *)
  let printed = Gen.with_pp Format.pp_print_int mapped in
  let failure, _ =
    expect_fail
      (Property.run ~root ~path:"pre-image" printed (fun _ x ->
           Check.is_true (x < 10)))
  in
  let rendered, _, _, _, _, _, _ = property_payload failure in
  check (rendered = "10") "with_pp on the image rendered %S, not 10" rendered;
  check
    (rendering_of failure = Failure.Value)
    "a with_pp counterexample must be flagged as a value"

(* Timeout vs the shrink search (D2) *)

let timeout_during_first_candidate_keeps_unshrunk () =
  (* Call 1 is the failing case; call 2 (the first shrink candidate) raises
     the per-test alarm. The search must end at the unshrunk original with
     the timed_out mark — never abort the test, never lose the
     counterexample. *)
  let path = "timeout first candidate" in
  let calls = ref 0 in
  let body _ _ =
    incr calls;
    if !calls = 1 then Check.fail "original" else raise (Failure.Timeout 0.25)
  in
  let outcome = Property.run ~root ~path Gen.int body in
  let failure, _ = expect_fail outcome in
  let rendered, case_index, shrink_steps, timed_out, _, examples, _ =
    property_payload failure
  in
  check (timed_out = Some 0.25) "the failure must carry the timeout limit";
  check (shrink_steps = 0) "no candidate was accepted, got %d" shrink_steps;
  check (not examples) "the case is generated, not an example";
  let original = value_at Gen.int ~root ~path ~index:case_index in
  check
    (rendered = string_of_int original)
    "the unshrunk original must be reported, got %S" rendered

let timeout_after_accepted_steps_keeps_best_so_far () =
  (* Cases and candidates fail above a threshold — the greedy descent must
     walk the halving chain, rejecting the passing dest-first candidates —
     until the counter raises the alarm: the search must stop at the last
     accepted node, never discard it. *)
  let calls = ref 0 in
  let body _ x =
    incr calls;
    if !calls >= 5 then raise (Failure.Timeout 0.1)
    else if abs x >= 10 then Check.fail "big"
  in
  let outcome = Property.run ~root ~path:"timeout mid shrink" Gen.int body in
  let failure, _ = expect_fail outcome in
  let rendered, _, shrink_steps, timed_out, _, _, inner =
    property_payload failure
  in
  check (timed_out = Some 0.1) "the failure must carry the timeout limit";
  check (shrink_steps >= 1) "accepted steps must be kept, got %d" shrink_steps;
  check
    (abs (int_of_string rendered) >= 10)
    "the best-so-far node must still fail the body, got %S" rendered;
  match inner with
  | Some { Failure.kind = Failure.Message "big"; _ } -> ()
  | _ -> failf "the inner failure must describe the last accepted node"

let timeout_during_generation_escapes_unchanged () =
  let gen = Gen.map (fun _ -> raise (Failure.Timeout 0.5)) Gen.int in
  match Property.run ~root ~path:"gen timeout" gen (fun _ _ -> ()) with
  | exception Failure.Timeout limit ->
      check (limit = 0.5) "Timeout must keep its limit, got %g" limit
  | _ -> failf "a Timeout raised at sample time must escape the engine"
  | exception other ->
      failf "expected Timeout, got %s" (Printexc.to_string other)

let skip_during_generation_escapes_unchanged () =
  let gen =
    Gen.map (fun _ -> raise (Failure.Skip_test (Some "no data"))) Gen.int
  in
  match Property.run ~root ~path:"gen skip" gen (fun _ _ -> ()) with
  | exception Failure.Skip_test (Some "no data") -> ()
  | _ -> failf "a Skip_test raised at sample time must escape the engine"
  | exception other ->
      failf "expected Skip_test, got %s" (Printexc.to_string other)

(* Failure payload plumbing *)

let msg_and_loc_are_preserved () =
  let loc = { Loc.file = "test/example.ml"; line = 41; column = 2 } in
  let body _ x = Check.equal ~msg:"labelled" Testable.int 0 x in
  let failure, _ =
    expect_fail (Property.run ~loc ~root ~path:"payload" Gen.int body)
  in
  check
    (failure.Failure.loc = Some loc)
    "the engine must stamp the declaration loc on the failure";
  let _, _, _, _, _, _, inner = property_payload failure in
  match inner with
  | Some { Failure.msg = Some "labelled"; _ } -> ()
  | _ -> failf "the inner failure must keep the assertion's ?msg"

let generator_crash_is_a_failure () =
  let gen = Gen.int_range 5 1 in
  let failure, _ =
    expect_fail (Property.run ~root ~path:"gen crash" gen (fun _ _ -> ()))
  in
  let rendered, case_index, shrink_steps, _, _, examples, inner =
    property_payload failure
  in
  check
    (rendered = "<generator raised before producing a value>")
    "a crashing generator renders the placeholder, got %S" rendered;
  check (case_index = 0) "the crash happens on the first attempt";
  check (shrink_steps = 0) "nothing can shrink without a sample";
  check (not examples) "the crash is a generated case";
  match inner with
  | Some { Failure.kind = Failure.Raise { actual = Some text; _ }; _ } ->
      check (contains "Invalid_argument" text) "the crash must be rendered"
  | _ -> failf "expected an inner Raise failure for the generator crash"

let control_exceptions_propagate () =
  (match
     Property.run ~root ~path:"skip" Gen.int (fun _ _ ->
         raise (Failure.Skip_test (Some "not here")))
   with
  | exception Failure.Skip_test (Some "not here") -> ()
  | _ -> failf "Skip_test must escape the engine unchanged"
  | exception other ->
      failf "expected Skip_test, got %s" (Printexc.to_string other));
  match
    Property.run ~root ~path:"timeout" Gen.int (fun _ _ ->
        raise (Failure.Timeout 0.5))
  with
  | exception Failure.Timeout limit ->
      check (limit = 0.5) "Timeout must keep its limit, got %g" limit
  | _ -> failf "Timeout must escape the engine unchanged"
  | exception other ->
      failf "expected Timeout, got %s" (Printexc.to_string other)

(* Configuration *)

let huge_count_does_not_overflow_the_budget () =
  (* The default max_discard is 2 * count; beyond max_int / 2 it must clamp
     rather than wrap negative and give up before running any case. *)
  let outcome =
    Property.run ~root ~path:"huge count"
      ~count:(`Declared ((max_int / 2) + 1))
      (Gen.constant 0)
      (fun _ _ -> Check.fail "stop at the first case")
  in
  let _, stats = expect_fail outcome in
  check (stats.Property.cases = 0) "the first case must fail immediately"

let count_zero_passes_vacuously () =
  let ran = ref 0 in
  let stats =
    expect_pass
      (Property.run ~root ~path:"count zero" ~count:(`Declared 0) Gen.int
         (fun _ _ -> incr ran))
  in
  check (!ran = 0) "no generated case may run";
  check (stats.Property.cases = 0) "no case passed";
  check (stats.Property.coverage = []) "no coverage was requested"

let negative_configuration_is_invalid () =
  let invalid configure =
    match configure () with
    | exception Invalid_argument _ -> ()
    | _ -> failf "negative configuration must raise Invalid_argument"
  in
  invalid (fun () ->
      Property.run ~root ~path:"bad" ~count:(`Declared (-1)) Gen.int (fun _ _ ->
          ()));
  invalid (fun () ->
      Property.run ~root ~path:"bad" ~max_discard:(-1) Gen.int (fun _ _ -> ()))

let assume_and_reject_raise_discard () =
  (match Property.assume true with () -> ());
  (match Property.assume false with
  | exception Property.Discard -> ()
  | _ -> failf "assume false must raise Discard");
  match Property.reject () with
  | exception Property.Discard -> ()
  | _ -> failf "reject must raise Discard"

(* Suite *)

(* A search stopped by its step budget and one that converged both read
   "shrunk N steps"; only the flag tells them apart, and without it a user
   cannot know whether the reported counterexample is minimal. The budget
   is fixed, so spending it takes a generator whose tree is one long
   chain — [n] down to [0], one accepted step per node under a law that
   fails on every value, so the descent ends only at the leaf. *)
let spent_shrink_budget_is_marked () =
  let chain n =
    let rec tree k =
      Shrink_tree.make ~root:k ~children:(fun () ->
          if k = 0 then Seq.Nil else Seq.Cons (tree (k - 1), Seq.empty))
    in
    Gen_engine.make ~pp:Format.pp_print_int (fun state -> (tree n, state))
  in
  let law _ (_ : int) = raise Exit in
  let budget = Property.shrink_budget in
  check (budget = 10_000) "the budget is the documented number, got %d" budget;
  let failure, _ =
    expect_fail (Property.run ~root ~path:"budget" (chain (budget + 1)) law)
  in
  let rendered, _, shrink_steps, _, _, _, _ = property_payload failure in
  check (shrink_exhausted failure) "a truncated search is marked";
  check (shrink_steps = budget) "it stopped at the budget, took %d" shrink_steps;
  check (rendered = "1") "and reports the best node reached, got %s" rendered;
  let failure, _ =
    expect_fail (Property.run ~root ~path:"budget" (chain budget) law)
  in
  let rendered, _, shrink_steps, _, _, _, _ = property_payload failure in
  check (not (shrink_exhausted failure)) "a converged search is not marked";
  check (shrink_steps = budget) "even one that spent every step, took %d"
    shrink_steps;
  check (rendered = "0") "and reports the minimal node, got %s" rendered

(* Forcing a candidate can raise — here a [map] whose function divides by
   the drawn value. The memoized cell caches the exception, so the siblings
   behind it are unreachable and the descent stops; what it must not do is
   report that as convergence, which told the reader a counterexample was
   minimal when the search never finished. *)
let a_raising_candidate_stops_the_search_visibly () =
  (* [int_range] shrinks toward the in-range point closest to zero, so the
     mapped function raises on the first candidate of any root above the
     bound — while the root itself, being above it, maps fine. *)
  let gen =
    Gen.map
      (fun n -> if n = 10 then failwith "forcing raised" else n)
      (Gen.int_range 10 50)
  in
  let root_value =
    Gen_engine.value
      (Shrink_tree.root
         (Gen_engine.sample gen
            (Seed.make (Seed.derive ~root ~path:"raising-candidate" ~index:0))))
  in
  check (root_value > 10) "the fixture's root is the raising value itself";
  let failure, _ =
    expect_fail
      (Property.run ~root ~path:"raising-candidate" gen (fun _ _ ->
           Check.fail "always"))
  in
  check (shrink_exhausted failure)
    "a descent stopped by a raising candidate reads as converged"

(* The count and its provenance are one argument, so the engine can never be
   handed a number without being told whether a replay needs the flag. *)
let count_provenance_decides_the_payload () =
  let body _ _ = failf "always" in
  let run_with count =
    let failure, _ =
      expect_fail
        (Property.run ?count ~root ~path:"count provenance" Gen.int body)
    in
    payload_count failure
  in
  check
    (run_with (Some (`Config 7)) = Some 7)
    "a config-sourced count rides the payload: the hint must restate the flag";
  check
    (run_with (Some (`Declared 7)) = None)
    "a declared count rides nothing: the declaration site replays by itself";
  check
    (run_with None = None)
    "the engine default rides nothing: a replay needs no flag to reproduce it"

(* The summary is the declarer's, applied to the value the failure reports:
   the shrunk counterexample, or the failing example. *)
let the_summary_is_of_the_reported_counterexample () =
  let summary_of (failure : Failure.t) =
    match failure.Failure.kind with
    | Failure.Property { summary; _ } -> summary
    | _ -> failf "expected a Property failure kind"
  in
  let summary value = if value = 0 then None else Some (Pp.str "n=%d" value) in
  let body _ value = Check.is_true (value < 10) in
  let failure, _ =
    expect_fail
      (Property.run ~summary ~root ~path:"summary" (Gen.int_range 0 1000) body)
  in
  check
    (summary_of failure = Some "n=10")
    "the summary is not the shrunk counterexample's: %s"
    (Option.value (summary_of failure) ~default:"absent");
  let failure, _ =
    expect_fail
      (Property.run ~summary ~examples:[ 50 ] ~root ~path:"summary"
         (Gen.int_range 0 1000) body)
  in
  check
    (summary_of failure = Some "n=50")
    "a failing example's summary is %s"
    (Option.value (summary_of failure) ~default:"absent");
  let failure, _ =
    expect_fail
      (Property.run ~summary ~root ~path:"summary" (Gen.int_range 0 1000)
         (fun _ _ -> failf "always"))
  in
  check
    (summary_of failure = None)
    "a value its declarer does not summarize carries a summary";
  let failure, _ =
    expect_fail (Property.run ~root ~path:"summary" (Gen.int_range 0 1000) body)
  in
  check (summary_of failure = None) "a property declared without one has one"

let suite =
  [
    ( "the summary is of the reported counterexample",
      the_summary_is_of_the_reported_counterexample );
    ("same inputs, same outcome", same_inputs_same_outcome);
    ("different path, different stream", different_path_different_stream);
    ("examples run first, in order", examples_run_first_in_order);
    ( "failing example fails fast with printer",
      failing_example_fails_fast_with_printer );
    ( "failing example without printer renders placeholder",
      failing_example_without_printer_renders_placeholder );
    ( "discarding example is counted and skipped",
      discarding_example_is_counted_and_skipped );
    ( "examples count in coverage denominator",
      examples_count_in_coverage_denominator );
    ("assume exhaustion gives up", assume_exhaustion_gives_up);
    ("generation rejection gives up", generation_rejection_gives_up);
    ( "explicit max_discard bounds discards",
      explicit_max_discard_bounds_discards );
    ( "max_discard zero gives up on the first discard",
      max_discard_zero_gives_up_on_first_discard );
    ( "discarding examples consume the budget",
      discarding_examples_consume_the_budget );
    ( "budget is checked before the count goal",
      budget_is_checked_before_the_count_goal );
    ( "passing examples do not consume the budget",
      passing_examples_do_not_consume_the_budget );
    ("mixed discards still pass", mixed_discards_still_pass);
    ("classify partitions cases", classify_partitions_cases);
    ("collect counts each case once", collect_counts_each_case_once);
    ( "discarded cases do not commit labels",
      discarded_cases_do_not_commit_labels );
    ("shrink runs do not pollute tables", shrink_runs_do_not_pollute_tables);
    ("cover satisfied passes", cover_satisfied_passes);
    ("cover unsatisfied fails at end", cover_unsatisfied_fails_at_end);
    ( "cover registers even when condition is false",
      cover_registers_even_when_condition_is_false );
    ("shrinks to minimal counterexample", shrinks_to_minimal_counterexample);
    ( "the shrink budget covers a quad of int64",
      shrink_budget_covers_a_quad_of_int64 );
    ( "assertion shrink skips exception candidates",
      assertion_shrink_skips_exception_candidates );
    ( "exception shrink skips assertion candidates",
      exception_shrink_skips_assertion_candidates );
    ( "shrink rejects discarding candidates",
      shrink_rejects_discarding_candidates );
    ( "skip candidate is rejected during shrink",
      skip_candidate_is_rejected_during_shrink );
    ( "timeout during the first candidate keeps the unshrunk counterexample",
      timeout_during_first_candidate_keeps_unshrunk );
    ( "timeout after accepted steps keeps the best-so-far",
      timeout_after_accepted_steps_keeps_best_so_far );
    ( "timeout during generation escapes unchanged",
      timeout_during_generation_escapes_unchanged );
    ( "skip during generation escapes unchanged",
      skip_during_generation_escapes_unchanged );
    ( "printerless counterexample renders the placeholder",
      printerless_counterexample_renders_placeholder );
    ( "mapped counterexample renders its shrunk pre-image",
      mapped_counterexample_renders_its_shrunk_pre_image );
    ("msg and loc are preserved", msg_and_loc_are_preserved);
    ("generator crash is a failure", generator_crash_is_a_failure);
    ("control exceptions propagate", control_exceptions_propagate);
    ( "huge count does not overflow the budget",
      huge_count_does_not_overflow_the_budget );
    ("count zero passes vacuously", count_zero_passes_vacuously);
    ("negative configuration is invalid", negative_configuration_is_invalid);
    ("assume and reject raise Discard", assume_and_reject_raise_discard);
    ("a spent shrink budget is distinguishable", spent_shrink_budget_is_marked);
    ( "a raising candidate stops the search visibly",
      a_raising_candidate_stops_the_search_visibly );
    ( "count provenance decides the payload",
      count_provenance_decides_the_payload );
  ]

let tests = List.map (fun (name, fn) -> Windtrap.test name fn) suite
let () = exit @@ Windtrap.run "property" tests
