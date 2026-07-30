(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC

  The case loop structure and label bookkeeping derive from windtrap v1's
  prop/prop.ml, rebuilt over Seed (per-case derivation), Gen (integrated
  shrinking), and Failure (typed counterexamples).
  --------------------------------------------------------------------------*)

(* Discarding *)

exception Discard

let assume condition = if not condition then raise Discard
let reject () = raise Discard

(* Labelling context

   One context per [run] invocation (nothing here is global).
   [case_collect]/[case_cover_hits] hold the current case's marks; they
   commit into [collect_counts]/[cover_hits] only when the case passes, so
   discarded and failing cases contribute nothing. [cover_requirements] is
   run-scoped: a requirement registers on first call and must keep one
   threshold for the whole run. *)

type context = {
  case_collect : (string, unit) Hashtbl.t;
  case_cover_hits : (string, unit) Hashtbl.t;
  collect_counts : (string, int) Hashtbl.t;
  cover_requirements : (string, float) Hashtbl.t;
  cover_hits : (string, int) Hashtbl.t;
}

let make_context () =
  {
    case_collect = Hashtbl.create 8;
    case_cover_hits = Hashtbl.create 8;
    collect_counts = Hashtbl.create 32;
    cover_requirements = Hashtbl.create 16;
    cover_hits = Hashtbl.create 16;
  }

let reset_case ctx =
  Hashtbl.reset ctx.case_collect;
  Hashtbl.reset ctx.case_cover_hits

let commit_case ctx =
  let bump counts label =
    let next = Option.value ~default:0 (Hashtbl.find_opt counts label) + 1 in
    Hashtbl.replace counts label next
  in
  Hashtbl.iter (fun label () -> bump ctx.collect_counts label) ctx.case_collect;
  Hashtbl.iter (fun label () -> bump ctx.cover_hits label) ctx.case_cover_hits

let collect ctx label = Hashtbl.replace ctx.case_collect label ()
let classify ctx label condition = if condition then collect ctx label

let cover ctx ~label ~at_least condition =
  if Float.is_nan at_least || at_least < 0.0 || at_least > 100.0 then
    invalid_arg "cover: at_least must be in [0.0, 100.0]";
  (match Hashtbl.find_opt ctx.cover_requirements label with
  | None -> Hashtbl.add ctx.cover_requirements label at_least
  | Some previous when Float.equal previous at_least -> ()
  | Some previous ->
      invalid_arg
        (Printf.sprintf "cover: label %S has conflicting thresholds (%g vs %g)"
           label previous at_least));
  if condition then begin
    Hashtbl.replace ctx.case_collect label ();
    Hashtbl.replace ctx.case_cover_hits label ()
  end

(* Outcomes *)

type cover_status = {
  label : string;
  required : float;
  actual : float;
  hits : int;
  satisfied : bool;
}

type stats = {
  cases : int;
  discards : int;
  collected : (string * int) list;
  coverage : cover_status list;
}

type outcome =
  | Pass of stats
  | Fail of { failure : Failure.t; stats : stats }
  | Coverage_failed of stats
  | Gave_up of stats

let sorted_bindings table =
  Hashtbl.to_seq table |> List.of_seq
  |> List.sort (fun (a, _) (b, _) -> compare a b)

let stats_of ~cases ~discards ctx =
  let coverage =
    sorted_bindings ctx.cover_requirements
    |> List.map (fun (label, required) ->
        let hits =
          Option.value ~default:0 (Hashtbl.find_opt ctx.cover_hits label)
        in
        let actual =
          if cases <= 0 then 0.0
          else float_of_int hits *. 100.0 /. float_of_int cases
        in
        (* The tolerance absorbs float division noise so an exactly met
           threshold never reads as missed. *)
        let satisfied = not (actual +. 1e-9 < required) in
        { label; required; actual; hits; satisfied })
  in
  { cases; discards; collected = sorted_bindings ctx.collect_counts; coverage }

(* Running one case

   [run_case] classifies one body invocation. Control exceptions are not
   re-raised here: the case loops re-raise them (with their backtrace) while
   the shrink search rejects a skipping candidate — so a candidate that
   skips cannot turn a recorded failure into a skip — and ends on a timeout
   (see [shrink]). *)

type failure_class = Assertion of Failure.t | Exception of exn * string option

type case_result =
  | Passed
  | Discarded
  | Failed of failure_class
  | Control of exn * Printexc.raw_backtrace

let run_case ctx body value =
  reset_case ctx;
  match body ctx value with
  | () -> Passed
  | exception Discard -> Discarded
  | exception Failure.Check_failure failure -> Failed (Assertion failure)
  | exception ((Failure.Skip_test _ | Failure.Timeout _) as control) ->
      Control (control, Printexc.get_raw_backtrace ())
  | exception exn -> Failed (Exception (exn, Failure.recorded_backtrace ()))

(* Shrinking

   Greedy descent over the sample's shrink tree: move to the first candidate
   whose body run fails in the same way — assertion failures accept any
   assertion failure, exception failures any non-assertion exception (v1
   semantics) — and stop when no candidate is accepted, the step cap is
   reached, or forcing a candidate raises (a memoized cell caches its
   exception, so its siblings are unreachable). A [Failure.Timeout] raised
   anywhere in the search also stops it, at the last accepted node: the
   whole-test budget can end the search but never erase a counterexample
   already found. Candidate runs use a scratch context: their labels
   never pollute the committed tables. The accepted classification is
   captured during the descent, so the final inner failure needs no extra
   body run. *)

let same_kind original candidate =
  match (original, candidate) with
  | Assertion _, Failed (Assertion _ as accepted)
  | Exception _, Failed (Exception _ as accepted) ->
      Some accepted
  | _, (Passed | Discarded | Failed _ | Control _) -> None

let shrink ~max_shrink ~body tree first_class =
  let scratch = make_context () in
  let accept candidate_tree =
    let value = Gen.value (Shrink_tree.root candidate_tree) in
    match run_case scratch body value with
    | Control ((Failure.Timeout _ as timeout), backtrace) ->
        (* The per-test alarm fired inside a candidate: a fact about the
           whole test, not this candidate — end the search (caught below). *)
        Printexc.raise_with_backtrace timeout backtrace
    | result -> same_kind first_class result
  in
  let rec first_accepted seq =
    match seq () with
    (* The explicit re-raise is load-bearing: the catch-all otherwise eats a
       handler-raised [Timeout] delivered during [Seq] forcing. *)
    | exception (Failure.Timeout _ as timeout) -> raise timeout
    | exception _ -> None
    | Seq.Nil -> None
    | Seq.Cons (candidate, rest) -> (
        match accept candidate with
        | Some accepted -> Some (candidate, accepted)
        | None -> first_accepted rest)
  in
  (* Best-so-far state lives in refs updated at each accepted step, so a
     [Timeout] firing at any poll point leaves them at the last accepted
     node. The descent wrapper below is the single place in the engine that
     consumes a timeout — everywhere else it propagates to the runner. *)
  let best = ref (tree, 0, first_class) in
  let timed_out = ref None in
  (try
     let rec descend steps tree cls =
       if steps < max_shrink then
         match first_accepted (Shrink_tree.children tree) with
         | None -> ()
         | Some (candidate, accepted) ->
             best := (candidate, steps + 1, accepted);
             descend (steps + 1) candidate accepted
     in
     descend 0 tree first_class
   with Failure.Timeout limit -> timed_out := Some limit);
  let tree, steps, cls = !best in
  (tree, steps, cls, !timed_out)

(* The engine *)

let default_count = 100
let default_max_shrink = 100

let inner_failure = function
  | Assertion failure -> failure
  | Exception (exn, backtrace) ->
      Failure.raised ~actual:(Printexc.to_string exn) ?backtrace ()

let run ?loc ?(count = default_count) ?config_count ?max_discard
    ?(max_shrink = default_max_shrink) ?(examples = []) ~root ~path gen body =
  if count < 0 then invalid_arg "Property.run: count must be non-negative";
  if max_shrink < 0 then
    invalid_arg "Property.run: max_shrink must be non-negative";
  let max_discard =
    match max_discard with
    (* The default budget clamps: [2 * count] overflows for counts beyond
       [max_int / 2], and a negative budget would give up before any case. *)
    | None -> if count > max_int / 2 then max_int else 2 * count
    | Some limit ->
        if limit < 0 then
          invalid_arg "Property.run: max_discard must be non-negative"
        else limit
  in
  let ctx = make_context () in
  let cases = ref 0 in
  let discards = ref 0 in
  let stats () = stats_of ~cases:!cases ~discards:!discards ctx in
  let fail ~rendered ~case_index ~shrink_steps ?timed_out ~examples cls =
    let failure =
      Failure.property ?loc ~inner:(inner_failure cls) ?timed_out
        ?count:config_count ~rendered ~case_index ~shrink_steps ~root ~examples
        ()
    in
    Fail { failure; stats = stats () }
  in
  (* Examples run first, unshrunk, unseeded, numbered separately. *)
  let rec run_examples index = function
    | [] -> None
    | value :: rest -> (
        match run_case ctx body value with
        | Passed ->
            commit_case ctx;
            incr cases;
            run_examples (index + 1) rest
        | Discarded ->
            incr discards;
            run_examples (index + 1) rest
        | Control (control, backtrace) ->
            Printexc.raise_with_backtrace control backtrace
        | Failed cls ->
            let rendered =
              match Gen.render_value gen value with
              | Some text -> text
              | None -> Printf.sprintf "<example %d>" (index + 1)
            in
            Some
              (fail ~rendered ~case_index:index ~shrink_steps:0 ~examples:true
                 cls))
  in
  match run_examples 0 examples with
  | Some outcome -> outcome
  | None ->
      let rec generate ~passed ~attempts =
        (* Budget before goal: a run whose discards exceed the budget gives
           up even when [count] is already met — examples can discard past
           the budget before any generation, including when [count] is 0. *)
        if !discards > max_discard then Gave_up (stats ())
        else if passed >= count then
          let final = stats () in
          if List.for_all (fun status -> status.satisfied) final.coverage then
            Pass final
          else Coverage_failed final
        else
          let state = Seed.make (Seed.derive ~root ~path ~index:attempts) in
          match Gen.sample gen state with
          | exception Gen.Rejected ->
              incr discards;
              generate ~passed ~attempts:(attempts + 1)
          | exception ((Failure.Skip_test _ | Failure.Timeout _) as control) ->
              (* Control exceptions delivered inside the generator (an alarm
                 at a poll point, a generator-callback skip) keep their
                 meaning: the runner classifies them, they are never a
                 generator crash. *)
              Printexc.raise_with_backtrace control
                (Printexc.get_raw_backtrace ())
          | exception exn ->
              let backtrace = Failure.recorded_backtrace () in
              fail ~rendered:"<generator raised before producing a value>"
                ~case_index:attempts ~shrink_steps:0 ~examples:false
                (Exception (exn, backtrace))
          | tree -> (
              match run_case ctx body (Gen.value (Shrink_tree.root tree)) with
              | Passed ->
                  commit_case ctx;
                  incr cases;
                  generate ~passed:(passed + 1) ~attempts:(attempts + 1)
              | Discarded ->
                  incr discards;
                  generate ~passed ~attempts:(attempts + 1)
              | Control (control, backtrace) ->
                  Printexc.raise_with_backtrace control backtrace
              | Failed cls ->
                  let final_tree, steps, final_cls, timed_out =
                    shrink ~max_shrink ~body tree cls
                  in
                  let rendered = Gen.render gen (Shrink_tree.root final_tree) in
                  fail ~rendered ~case_index:attempts ~shrink_steps:steps
                    ?timed_out ~examples:false final_cls)
      in
      generate ~passed:0 ~attempts:0
