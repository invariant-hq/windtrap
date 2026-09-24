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

let contains needle haystack = Text.contains_substring ~pattern:needle haystack

(* One fixed root for most tests: outcomes are deterministic across runs and
   machines (guarantee 7), so every assertion below is exact. *)
let root = 0x00c0ffee1234abcdL

let property_payload (failure : Failure.t) =
  match failure.Failure.kind with
  | Failure.Property
      { rendered; case_index; shrink_steps; shrink_end; root; examples; inner }
    ->
      let timed_out =
        match shrink_end with
        | Failure.Timed_out limit -> Some limit
        | _ -> None
      in
      (rendered, case_index, shrink_steps, timed_out, root, examples, inner)
  | _ -> failf "expected a Property failure kind"

let shrink_end (failure : Failure.t) =
  match failure.Failure.kind with
  | Failure.Property { shrink_end; _ } -> shrink_end
  | _ -> failf "expected a Property failure kind"

(* The search stopped before it converged, at the budget or at a candidate
   that raised. *)
let shrink_exhausted failure =
  match shrink_end failure with
  | Failure.Budget_spent | Failure.Candidate_raised _ -> true
  | Failure.Converged | Failure.Timed_out _ -> false

let payload_count (failure : Failure.t) =
  match failure.Failure.kind with
  | Failure.Property { count; _ } -> count
  | _ -> failf "expected a Property failure kind"

(* The inner failure of a Fail, as [Printexc.to_string] rendered the law's or
   the generator's exception. *)
let inner_exception (failure : Failure.t) =
  let _, _, _, _, _, _, inner = property_payload failure in
  match inner with
  | Some { Failure.kind = Failure.Raise { actual; _ }; _ } -> actual
  | _ -> None

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
  is_true ~msg:"same root, path, and count must reproduce the outcome"
    (first = second);
  let failure, _ = expect_fail first in
  let rendered, case_index, _, timed_out, recorded_root, examples, _ =
    property_payload failure
  in
  is_true ~msg:"failure must record the run's root seed" (recorded_root = root);
  is_true ~msg:"an ordinary failure carries no timed_out mark" (timed_out = None);
  is_true ~msg:"a generated case must not be flagged as an example"
    (not examples);
  is_true ~msg:"counterexample must fail the body"
    (int_of_string rendered >= 800);
  (* Replay contract: the recorded case index re-derives a failing value. *)
  let replayed =
    value_at (Gen.int_range 0 1000) ~root ~path ~index:case_index
  in
  is_true
    ~msg:
      (Printf.sprintf "case %d must re-derive a failing value, got %d"
         case_index replayed)
    (replayed >= 800)

let different_path_different_stream () =
  let gen = Gen.int64 in
  let first = value_at gen ~root ~path:"stream one" ~index:0 in
  let second = value_at gen ~root ~path:"stream two" ~index:0 in
  is_true
    ~msg:
      "distinct paths must not share a stream (adding a property never \
       perturbs another)"
    (first <> second)

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
  is_true ~msg:"expected 2 examples + 3 generated bodies" (List.length order = 5);
  is_true ~msg:"examples must run first, in list order"
    (match order with 1000 :: 2000 :: _ -> true | _ -> false);
  List.iteri
    (fun position value ->
      if position >= 2 then
        is_true ~msg:"generated cases must follow the examples" (value <= 5))
    order;
  is_true ~msg:"examples must count as passing cases" (stats.Property.cases = 5)

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
  is_true ~msg:"the failure must be flagged as an example" examples;
  is_true ~msg:"example case_index must be its zero-based position"
    (case_index = 1);
  is_true ~msg:"examples are never shrunk" (shrink_steps = 0);
  is_true ~msg:"a failing example prints via the generator's printer"
    (rendered = "7");
  is_true ~msg:"a failing example must stop the run" (!generated = 2);
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
  is_true ~msg:"the failure must be flagged as an example" examples;
  is_true ~msg:"case_index must be the example's position" (case_index = 1);
  is_true
    ~msg:
      (Printf.sprintf "a printerless example renders the placeholder, got %S"
         rendered)
    (rendered = "<no printer: attach one with Gen.with_pp>");
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
  is_true ~msg:"examples 1 and 3 plus 2 generated pass"
    (stats.Property.cases >= 4);
  is_true ~msg:"the discarded example must be counted"
    (stats.Property.discards >= 1)

let examples_count_as_cases_and_mark_cover_labels () =
  let body ctx x = Property.cover ctx "zero" (x = 0) in
  let stats =
    expect_pass
      (Property.run ~root ~path:"examples cover" ~count:(`Declared 2)
         ~examples:[ 0; 0; 0; 0 ] (Gen.constant 1) body)
  in
  is_true ~msg:"4 examples + 2 generated cases" (stats.Property.cases = 6);
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
  is_true ~msg:"no case can pass" (stats.Property.cases = 0);
  is_true
    ~msg:
      (Printf.sprintf
         "the default budget allows 2 * count discards, the 21st gives up, got \
          %d"
         stats.Property.discards)
    (stats.Property.discards = 21)

let generation_rejection_gives_up () =
  let ran = ref 0 in
  let gen = Gen.such_that (fun _ -> false) Gen.int in
  let stats =
    expect_gave_up
      (Property.run ~root ~path:"gen give up" ~count:(`Declared 3) gen
         (fun _ _ -> incr ran))
  in
  is_true ~msg:"the body must never run when generation rejects" (!ran = 0);
  is_true
    ~msg:
      (Printf.sprintf
         "every rejection must count as a discard up to the budget, got %d"
         stats.Property.discards)
    (stats.Property.discards = 7)

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
  is_true
    ~msg:
      (Printf.sprintf "the discard exceeding the budget gives up, got %d"
         !attempts)
    (!attempts = 8);
  is_true ~msg:"all attempts discarded" (stats.Property.discards = 8)

let max_discard_zero_gives_up_on_first_discard () =
  let stats =
    expect_gave_up
      (Property.run ~root ~path:"no discards" ~count:(`Declared 5)
         ~max_discard:0 Gen.int (fun _ _ -> Property.reject ()))
  in
  is_true ~msg:"no case can pass" (stats.Property.cases = 0);
  is_true
    ~msg:
      (Printf.sprintf "the first discard must give up, got %d"
         stats.Property.discards)
    (stats.Property.discards = 1);
  (* A property that never discards is unaffected by a zero budget. *)
  let stats =
    expect_pass
      (Property.run ~root ~path:"no discards pass" ~count:(`Declared 5)
         ~max_discard:0 Gen.int (fun _ _ -> ()))
  in
  is_true ~msg:"all cases must pass under a zero budget"
    (stats.Property.cases = 5)

let discarding_examples_consume_the_budget () =
  let stats =
    expect_gave_up
      (Property.run ~root ~path:"example budget" ~count:(`Declared 3)
         ~max_discard:2 ~examples:[ 1; 1; 1 ] (Gen.constant 0) (fun _ x ->
           Property.assume (x <> 1)))
  in
  is_true ~msg:"every example must discard" (stats.Property.cases = 0);
  is_true
    ~msg:
      (Printf.sprintf "example discards must count toward the budget, got %d"
         stats.Property.discards)
    (stats.Property.discards = 3)

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
  is_true
    ~msg:
      (Printf.sprintf
         "the discarding example must give up the run, got %d discards"
         stats.Property.discards)
    (stats.Property.discards = 1)

let passing_examples_do_not_consume_the_budget () =
  let stats =
    expect_pass
      (Property.run ~root ~path:"pass no budget" ~count:(`Declared 3)
         ~max_discard:0 ~examples:[ 1; 2; 3 ] (Gen.constant 0) (fun _ _ -> ()))
  in
  is_true ~msg:"3 examples + 3 generated cases must pass"
    (stats.Property.cases = 6);
  is_true ~msg:"nothing discards" (stats.Property.discards = 0)

let mixed_discards_still_pass () =
  (* Half the space discards; the budget of 2 * count absorbs it. *)
  let stats =
    expect_pass
      (Property.run ~root ~path:"mixed discards" ~count:(`Declared 20)
         (Gen.int_range 0 9) (fun _ x -> Property.assume (x mod 2 = 0)))
  in
  is_true ~msg:"count cases must pass" (stats.Property.cases = 20);
  is_true ~msg:"odd draws must discard" (stats.Property.discards > 0)

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
  is_true ~msg:"all cases pass" (stats.Property.cases = 50);
  let total = List.fold_left (fun acc (_, n) -> acc + n) 0 stats.collected in
  is_true
    ~msg:(Printf.sprintf "each case must carry exactly one label, got %d" total)
    (total = 50);
  is_true ~msg:"collected labels must be sorted"
    (List.map fst stats.Property.collected
    = List.sort compare (List.map fst stats.Property.collected))

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
      is_true ~msg:"some cases must have discarded" (stats.Property.discards > 0)
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
  is_true ~msg:"shrink re-runs must accumulate into a scratch context only"
    (stats.Property.collected = [ ("ran", stats.Property.cases) ])

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
  is_true ~msg:"the full case count must still pass" (stats.Property.cases = 10);
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
  is_true
    ~msg:(Printf.sprintf "int must shrink to a unit magnitude, got %S" rendered)
    (rendered = "1" || rendered = "-1");
  is_true ~msg:"shrinking must have taken steps" (shrink_steps > 0);
  match inner with
  | Some { Failure.kind = Failure.Equality { expected; actual; not_ }; _ } ->
      is_true ~msg:"inner expected side is the assertion's" (expected = "0");
      is_true ~msg:"inner failure must describe the shrunk case"
        (actual = rendered);
      is_true ~msg:"equal is not negated" (not not_)
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
  is_true ~msg:"the search must have taken steps" (shrink_steps > 0);
  is_true
    ~msg:
      (Printf.sprintf
         "a quad of int64 converges within one step per bit, took %d"
         shrink_steps)
    (shrink_steps <= 256);
  is_true ~msg:"a converged search is not reported as stopped"
    (not (shrink_exhausted failure))

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
  is_true
    ~msg:
      (Printf.sprintf
         "the assertion goal must skip the exception trap at 1, got %S" rendered)
    (rendered = "2");
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
  is_true
    ~msg:
      (Printf.sprintf
         "the exception goal must skip the assertion trap at 1, got %S" rendered)
    (rendered = "2");
  match inner with
  | Some { Failure.kind = Failure.Raise { actual = Some text; _ }; _ } ->
      is_true ~msg:"the inner failure must render Exit" (contains "Exit" text)
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
  is_true
    ~msg:"a run with interleaved discards and shrinking must be deterministic"
    (first = second);
  let failure, stats = expect_fail first in
  let rendered, case_index, shrink_steps, _, _, _, _ =
    property_payload failure
  in
  let final = int_of_string rendered in
  let original = value_at gen ~root ~path ~index:case_index in
  is_true
    ~msg:
      (Printf.sprintf "shrinking must not cross the discard band, got %d" final)
    (final >= 10);
  is_true
    ~msg:(Printf.sprintf "the counterexample must shrink below %d" original)
    (final < original);
  is_true ~msg:"shrinking must have taken steps" (shrink_steps > 0);
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
  is_true
    ~msg:
      (Printf.sprintf
         "shrink-time discards must not count in stats.discards: expected %d, \
          got %d"
         expected_discards stats.Property.discards)
    (stats.Property.discards = expected_discards)

let skip_candidate_is_rejected_during_shrink () =
  let path = "skip candidate" in
  let gen = Gen.int_range 0 100 in
  let fails value = value >= 2 in
  let root = find_root gen ~path ~fails ~first_ok:(fun value -> value >= 3) in
  let body _ x =
    if x = 1 then raise (Failure.Control (`Skip (Some "trap")))
    else if x >= 2 then Check.fail "wanted"
  in
  match Property.run ~root ~path gen body with
  | exception Failure.Control (`Skip _) ->
      failf "a skipping shrink candidate must not skip the test"
  | outcome ->
      let failure, _ = expect_fail outcome in
      let rendered, _, _, timed_out, _, _, _ = property_payload failure in
      is_true
        ~msg:
          (Printf.sprintf
             "the skip trap at 1 must be rejected during shrinking, got %S"
             rendered)
        (rendered = "2");
      is_true
        ~msg:"a rejected skipping candidate must not mark the failure timed out"
        (timed_out = None)

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
  is_true
    ~msg:
      (Printf.sprintf
         "a printerless counterexample renders the placeholder, got %S" rendered)
    (rendered = "<no printer: attach one with Gen.with_pp>");
  is_true ~msg:"a printerless counterexample must be flagged as the value"
    (rendering_of failure = Failure.Value)

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
  is_true
    ~msg:
      (Printf.sprintf
         "the pre-image of the minimal counterexample 10 is 5, got %S" rendered)
    (rendered = "5");
  is_true ~msg:"a mapped counterexample must be flagged as a pre-image"
    (rendering_of failure = Failure.Pre_image);
  (* An explicit printer on the image wins, and the flag says value. *)
  let printed = Gen.with_pp Format.pp_print_int mapped in
  let failure, _ =
    expect_fail
      (Property.run ~root ~path:"pre-image" printed (fun _ x ->
           Check.is_true (x < 10)))
  in
  let rendered, _, _, _, _, _, _ = property_payload failure in
  is_true
    ~msg:(Printf.sprintf "with_pp on the image rendered %S, not 10" rendered)
    (rendered = "10");
  is_true ~msg:"a with_pp counterexample must be flagged as a value"
    (rendering_of failure = Failure.Value)

(* Timeout vs the shrink search (D2) *)

let timeout_during_first_candidate_keeps_unshrunk () =
  (* Call 1 is the failing case; call 2 (the first shrink candidate) raises
     the per-test alarm. The search must end at the unshrunk original with
     the timeout mark — never abort the test, never lose the
     counterexample. *)
  let path = "timeout first candidate" in
  let calls = ref 0 in
  let body _ _ =
    incr calls;
    if !calls = 1 then Check.fail "original"
    else raise (Failure.Control (`Timeout 0.25))
  in
  let outcome = Property.run ~root ~path Gen.int body in
  let failure, _ = expect_fail outcome in
  let rendered, case_index, shrink_steps, timed_out, _, examples, _ =
    property_payload failure
  in
  is_true ~msg:"the failure must carry the timeout limit" (timed_out = Some 0.25);
  is_true
    ~msg:(Printf.sprintf "no candidate was accepted, got %d" shrink_steps)
    (shrink_steps = 0);
  is_true ~msg:"the case is generated, not an example" (not examples);
  let original = value_at Gen.int ~root ~path ~index:case_index in
  is_true
    ~msg:
      (Printf.sprintf "the unshrunk original must be reported, got %S" rendered)
    (rendered = string_of_int original)

let timeout_after_accepted_steps_keeps_best_so_far () =
  (* Cases and candidates fail above a threshold — the greedy descent must
     walk the halving chain, rejecting the passing dest-first candidates —
     until the counter raises the alarm: the search must stop at the last
     accepted node, never discard it. *)
  let calls = ref 0 in
  let body _ x =
    incr calls;
    if !calls >= 5 then raise (Failure.Control (`Timeout 0.1))
    else if abs x >= 10 then Check.fail "big"
  in
  let outcome = Property.run ~root ~path:"timeout mid shrink" Gen.int body in
  let failure, _ = expect_fail outcome in
  let rendered, _, shrink_steps, timed_out, _, _, inner =
    property_payload failure
  in
  is_true ~msg:"the failure must carry the timeout limit" (timed_out = Some 0.1);
  is_true
    ~msg:(Printf.sprintf "accepted steps must be kept, got %d" shrink_steps)
    (shrink_steps >= 1);
  is_true
    ~msg:
      (Printf.sprintf "the best-so-far node must still fail the body, got %S"
         rendered)
    (abs (int_of_string rendered) >= 10);
  match inner with
  | Some { Failure.kind = Failure.Message "big"; _ } -> ()
  | _ -> failf "the inner failure must describe the last accepted node"

let timeout_during_generation_escapes_unchanged () =
  let gen = Gen.map (fun _ -> raise (Failure.Control (`Timeout 0.5))) Gen.int in
  match Property.run ~root ~path:"gen timeout" gen (fun _ _ -> ()) with
  | exception Failure.Control (`Timeout limit) ->
      is_true
        ~msg:(Printf.sprintf "Timeout must keep its limit, got %g" limit)
        (limit = 0.5)
  | _ -> failf "a Timeout raised at sample time must escape the engine"
  | exception other ->
      failf "expected Timeout, got %s" (Printexc.to_string other)

let skip_during_generation_escapes_unchanged () =
  let gen =
    Gen.map (fun _ -> raise (Failure.Control (`Skip (Some "no data")))) Gen.int
  in
  match Property.run ~root ~path:"gen skip" gen (fun _ _ -> ()) with
  | exception Failure.Control (`Skip (Some "no data")) -> ()
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
  is_true ~msg:"the engine must stamp the declaration loc on the failure"
    (failure.Failure.loc = Some loc);
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
  is_true
    ~msg:
      (Printf.sprintf "a crashing generator renders the placeholder, got %S"
         rendered)
    (rendered = "<generator raised before producing a value>");
  is_true ~msg:"the crash happens on the first attempt" (case_index = 0);
  is_true ~msg:"nothing can shrink without a sample" (shrink_steps = 0);
  is_true ~msg:"the crash is a generated case" (not examples);
  match inner with
  | Some { Failure.kind = Failure.Raise { actual = Some text; _ }; _ } ->
      is_true ~msg:"the crash must be rendered"
        (contains "Invalid_argument" text)
  | _ -> failf "expected an inner Raise failure for the generator crash"

let control_exceptions_propagate () =
  (match
     Property.run ~root ~path:"skip" Gen.int (fun _ _ ->
         raise (Failure.Control (`Skip (Some "not here"))))
   with
  | exception Failure.Control (`Skip (Some "not here")) -> ()
  | _ -> failf "Skip_test must escape the engine unchanged"
  | exception other ->
      failf "expected Skip_test, got %s" (Printexc.to_string other));
  match
    Property.run ~root ~path:"timeout" Gen.int (fun _ _ ->
        raise (Failure.Control (`Timeout 0.5)))
  with
  | exception Failure.Control (`Timeout limit) ->
      is_true
        ~msg:(Printf.sprintf "Timeout must keep its limit, got %g" limit)
        (limit = 0.5)
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
  is_true ~msg:"the first case must fail immediately" (stats.Property.cases = 0)

let count_zero_passes_vacuously () =
  let ran = ref 0 in
  let stats =
    expect_pass
      (Property.run ~root ~path:"count zero" ~count:(`Declared 0) Gen.int
         (fun _ _ -> incr ran))
  in
  is_true ~msg:"no generated case may run" (!ran = 0);
  is_true ~msg:"no case passed" (stats.Property.cases = 0);
  is_true ~msg:"no coverage was requested" (stats.Property.coverage = [])

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
  | exception Failure.Control `Discard -> ()
  | _ -> failf "assume false must raise Discard");
  match Property.reject () with
  | exception Failure.Control `Discard -> ()
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
  is_true
    ~msg:(Printf.sprintf "the budget is the documented number, got %d" budget)
    (budget = 10_000);
  let failure, _ =
    expect_fail (Property.run ~root ~path:"budget" (chain (budget + 1)) law)
  in
  let rendered, _, shrink_steps, _, _, _, _ = property_payload failure in
  is_true ~msg:"a truncated search is marked" (shrink_exhausted failure);
  is_true
    ~msg:(Printf.sprintf "it stopped at the budget, took %d" shrink_steps)
    (shrink_steps = budget);
  is_true
    ~msg:(Printf.sprintf "and reports the best node reached, got %s" rendered)
    (rendered = "1");
  let failure, _ =
    expect_fail (Property.run ~root ~path:"budget" (chain budget) law)
  in
  let rendered, _, shrink_steps, _, _, _, _ = property_payload failure in
  is_true ~msg:"a converged search is not marked"
    (not (shrink_exhausted failure));
  is_true
    ~msg:(Printf.sprintf "even one that spent every step, took %d" shrink_steps)
    (shrink_steps = budget);
  is_true
    ~msg:(Printf.sprintf "and reports the minimal node, got %s" rendered)
    (rendered = "0")

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
  is_true ~msg:"the fixture's root is the raising value itself" (root_value > 10);
  let failure, _ =
    expect_fail
      (Property.run ~root ~path:"raising-candidate" gen (fun _ _ ->
           Check.fail "always"))
  in
  (* The forcing's exception is named in the payload, and only there: it is
     neither the counterexample nor the law's failure. *)
  equal ~msg:"the stop names the forcing's exception" string
    {|Failure("forcing raised")|}
    (match shrink_end failure with
    | Failure.Candidate_raised text -> text
    | _ -> fail "a descent stopped by a raising candidate reads as converged");
  let rendered, _, _, _, _, _, _ = property_payload failure in
  List.iter
    (fun text ->
      is_false ~msg:"the counterexample is not the forcing's exception"
        (contains "forcing raised" text))
    (rendered :: Option.to_list (inner_exception failure))

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
  is_true
    ~msg:
      "a config-sourced count rides the payload: the hint must restate the flag"
    (run_with (Some (`Config 7)) = Some 7);
  is_true
    ~msg:
      "a declared count rides nothing: the declaration site replays by itself"
    (run_with (Some (`Declared 7)) = None);
  is_true
    ~msg:
      "the engine default rides nothing: a replay needs no flag to reproduce it"
    (run_with None = None)

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
  is_true
    ~msg:
      (Printf.sprintf "the summary is not the shrunk counterexample's: %s"
         (Option.value (summary_of failure) ~default:"absent"))
    (summary_of failure = Some "n=10");
  let failure, _ =
    expect_fail
      (Property.run ~summary ~examples:[ 50 ] ~root ~path:"summary"
         (Gen.int_range 0 1000) body)
  in
  is_true
    ~msg:
      (Printf.sprintf "a failing example's summary is %s"
         (Option.value (summary_of failure) ~default:"absent"))
    (summary_of failure = Some "n=50");
  let failure, _ =
    expect_fail
      (Property.run ~summary ~root ~path:"summary" (Gen.int_range 0 1000)
         (fun _ _ -> failf "always"))
  in
  is_true ~msg:"a value its declarer does not summarize carries a summary"
    (summary_of failure = None);
  let failure, _ =
    expect_fail (Property.run ~root ~path:"summary" (Gen.int_range 0 1000) body)
  in
  is_true ~msg:"a property declared without one has one"
    (summary_of failure = None)

let exit_and_fatal_exceptions =
  [ Failure.Control `Exit; Sys.Break; Out_of_memory ]

(* An exit is the runner's and a fatal exception stops the run, so neither
   is a failure of the case to shrink. *)
let a_law_s_exit_and_fatal_exceptions_pass_through () =
  List.iter
    (fun exn ->
      let name = Printexc.to_string exn in
      match
        Property.run ~root ~path:"fatal law" Gen.int (fun _ _ -> raise exn)
      with
      | exception raised -> is_true ~msg:(name ^ " escapes") (raised = exn)
      | _ -> failf "%s became an outcome" name)
    exit_and_fatal_exceptions

let a_law_s_stack_overflow_fails_the_case () =
  let failure, _ =
    expect_fail
      (Property.run ~root ~path:"overflow" Gen.int (fun _ _ ->
           raise Stack_overflow))
  in
  equal (option string) (Some "Stack overflow") (inner_exception failure)

let a_generator_s_exception_fails_the_case_unshrunk () =
  let raising exn = Gen_engine.make (fun _ -> raise exn) in
  let failed exn =
    let failure, _ =
      expect_fail
        (Property.run ~root ~path:"raising gen" (raising exn) (fun _ _ -> ()))
    in
    let rendered, _, shrink_steps, _, _, _, inner = property_payload failure in
    equal ~msg:"the placeholder" string
      "<generator raised before producing a value>" rendered;
    equal ~msg:"unshrunk" int 0 shrink_steps;
    inner
  in
  (match failed Not_found with
  | Some { Failure.kind = Failure.Raise { actual = Some "Not_found"; _ }; _ } ->
      ()
  | _ -> fail "the inner failure is not the generator's exception");
  (match
     failed (Failure.Check_failure (Failure.message "from the generator"))
   with
  | Some { Failure.kind = Failure.Message "from the generator"; _ } -> ()
  | _ -> fail "the inner failure is not the generator's assertion");
  List.iter
    (fun exn ->
      let name = Printexc.to_string exn in
      match
        Property.run ~root ~path:"raising gen" (raising exn) (fun _ _ -> ())
      with
      | exception raised -> is_true ~msg:(name ^ " escapes") (raised = exn)
      | _ -> failf "%s became an outcome" name)
    exit_and_fatal_exceptions

let a_generator_that_discards_discards_the_case () =
  let always = Gen_engine.make (fun _ -> raise (Failure.Control `Discard)) in
  (match Property.run ~root ~path:"discarding gen" always (fun _ _ -> ()) with
  | Property.Gave_up { Property.discards; _ } ->
      is_true ~msg:"every draw discarded" (discards > 0)
  | _ -> failf "a generator that always discards must give up");
  let odd_discarded =
    Gen.map
      (fun n ->
        Property.assume (n mod 2 = 0);
        n)
      Gen.int
  in
  match
    Property.run ~root ~path:"assume in map" odd_discarded (fun _ _ -> ())
  with
  | Property.Pass { Property.discards; cases; _ } ->
      equal ~msg:"every case passed" int 100 cases;
      is_true ~msg:"the odd draws were discarded" (discards > 0)
  | _ -> failf "an assume in Gen.map must discard the case, not fail it"

let a_context_used_after_its_run () =
  let kept = ref None in
  ignore
    (expect_pass
       (Property.run ~root ~path:"late context" ~count:(`Declared 3)
          (Gen.constant 0) (fun ctx _ -> kept := Some ctx)));
  let ctx = Option.get !kept in
  Property.collect ctx "late";
  Property.cover ctx "late cover" true;
  let stats =
    expect_pass
      (Property.run ~root ~path:"late context" ~count:(`Declared 3)
         (Gen.constant 0) (fun _ _ -> ()))
  in
  equal ~msg:"the next run reads none of its marks"
    (list (pair string int))
    [] stats.Property.collected;
  is_true ~msg:"and none of its demands" (stats.Property.coverage = [])

let a_cover_reached_in_a_dropped_case_stays_registered () =
  let unsatisfied label = function
    | [ { Property.label = l; hits = 0; satisfied = false } ] -> l = label
    | _ -> false
  in
  let stats =
    expect_gave_up
      (Property.run ~root ~path:"cover discard" ~count:(`Declared 5)
         (Gen.constant 0) (fun ctx _ ->
           Property.cover ctx "discarded" true;
           Property.reject ()))
  in
  is_true ~msg:"a discarded case"
    (unsatisfied "discarded" stats.Property.coverage);
  let _, stats =
    expect_fail
      (Property.run ~root ~path:"cover fail" (Gen.constant 0) (fun ctx _ ->
           Property.cover ctx "failed" true;
           Check.fail "always"))
  in
  is_true ~msg:"a failing case" (unsatisfied "failed" stats.Property.coverage);
  let stats =
    expect_pass
      (Property.run ~root ~path:"cover unreached" ~count:(`Declared 5)
         (Gen.constant 0) (fun ctx v ->
           if v <> 0 then Property.cover ctx "unreached" true))
  in
  is_true ~msg:"a cover no case reaches registers nothing"
    (stats.Property.coverage = [])

let an_unmarked_cover_never_turns_a_fail_into_another_outcome () =
  let _, stats =
    expect_fail
      (Property.run ~root ~path:"cover then fail" (Gen.int_range 0 100)
         (fun ctx v ->
           Property.cover ctx "never" false;
           Check.is_true (v < 50)))
  in
  is_true ~msg:"the demand is unsatisfied, and the outcome still Fail"
    (match stats.Property.coverage with
    | [ { Property.satisfied = false; _ } ] -> true
    | _ -> false)

let an_inner_raise_carries_the_backtrace_when_recorded () =
  let backtrace recording =
    let saved = Printexc.backtrace_status () in
    Printexc.record_backtrace recording;
    Fun.protect ~finally:(fun () -> Printexc.record_backtrace saved)
    @@ fun () ->
    let failure, _ =
      expect_fail
        (Property.run ~root ~path:"inner backtrace" Gen.int (fun _ _ ->
             raise Not_found))
    in
    let _, _, _, _, _, _, inner = property_payload failure in
    match inner with
    | Some { Failure.kind = Failure.Raise { backtrace; _ }; _ } -> backtrace
    | _ -> failf "expected an inner Raise"
  in
  is_true ~msg:"recorded" (Option.is_some (backtrace true));
  is_none ~msg:"not recorded" (backtrace false)

let examples_draw_no_seed () =
  (* Two passing examples, then the law fails on the first generated case:
     that case is index 0, the seed the examples would have drawn. *)
  let path = "examples draw nothing" in
  let failure, _ =
    expect_fail
      (Property.run ~root ~path ~examples:[ -1; -2 ] Gen.int (fun _ v ->
           Check.is_true (v < 0)))
  in
  let _, case_index, _, _, _, examples, _ = property_payload failure in
  is_false ~msg:"a generated case" examples;
  is_true ~msg:"premise: case 0 fails"
    (value_at Gen.int ~root ~path ~index:0 >= 0);
  equal ~msg:"the first generated case is index 0" int 0 case_index

let a_timeout_while_formatting_does_not_leave_run () =
  let gen =
    Gen.with_pp
      (fun _ _ -> raise (Failure.Control (`Timeout 0.25)))
      (Gen.int_range 0 10)
  in
  let failure, _ =
    expect_fail
      (Property.run ~root ~path:"formatting timeout" gen (fun _ _ ->
           Check.fail "always"))
  in
  let rendered, _, _, timed_out, _, _, _ = property_payload failure in
  equal ~msg:"the guard's text" string
    (Printf.sprintf "<printer raised %s>"
       (Printexc.to_string (Failure.Control (`Timeout 0.25))))
    rendered;
  is_none ~msg:"not a timed-out search" timed_out

let a_replay_descends_the_same_path_whatever_the_configuration () =
  let path = "replay path" in
  let body _ x = Check.is_true (x < 700) in
  let payload count max_discard =
    let failure, _ =
      expect_fail
        (Property.run ~count ?max_discard ~root ~path (Gen.int_range 0 1000)
           body)
    in
    let rendered, case_index, shrink_steps, _, _, _, _ =
      property_payload failure
    in
    (rendered, case_index, shrink_steps)
  in
  let reference = payload (`Config 100) None in
  List.iter
    (fun (count, max_discard) ->
      equal (triple string int int) reference (payload count max_discard))
    [ (`Config 1_000, None); (`Declared 5_000, Some 7); (`Config 300, Some 0) ]

let suite =
  [
    ( "a law's exit and fatal exceptions pass through",
      a_law_s_exit_and_fatal_exceptions_pass_through );
    ( "a law's Stack_overflow fails the case",
      a_law_s_stack_overflow_fails_the_case );
    ( "a generator's exception fails the case unshrunk",
      a_generator_s_exception_fails_the_case_unshrunk );
    ( "a generator that discards discards the case",
      a_generator_that_discards_discards_the_case );
    ("a context used after its run", a_context_used_after_its_run);
    ( "a cover reached in a dropped case stays registered",
      a_cover_reached_in_a_dropped_case_stays_registered );
    ( "an unmarked cover never turns a Fail into another outcome",
      an_unmarked_cover_never_turns_a_fail_into_another_outcome );
    ( "an inner Raise carries the backtrace when recorded",
      an_inner_raise_carries_the_backtrace_when_recorded );
    ("examples draw no seed", examples_draw_no_seed);
    ( "a timeout while formatting does not leave run",
      a_timeout_while_formatting_does_not_leave_run );
    ( "a replay descends the same path whatever the configuration",
      a_replay_descends_the_same_path_whatever_the_configuration );
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
    ( "examples count as cases and mark cover labels",
      examples_count_as_cases_and_mark_cover_labels );
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
