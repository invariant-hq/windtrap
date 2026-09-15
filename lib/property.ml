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
   [case_collect] holds the current case's marks; they commit into
   [collect_counts] only when the case passes, so discarded and failing
   cases contribute nothing. [required] is run-scoped: a label registers on
   first call and is answered at the end of the run. *)

type context = {
  case_collect : (string, unit) Hashtbl.t;
  collect_counts : (string, int) Hashtbl.t;
  required : (string, unit) Hashtbl.t;
}

let make_context () =
  {
    case_collect = Hashtbl.create 8;
    collect_counts = Hashtbl.create 32;
    required = Hashtbl.create 16;
  }

let reset_case ctx = Hashtbl.reset ctx.case_collect

let commit_case ctx =
  Hashtbl.iter
    (fun label () ->
      let next =
        Option.value ~default:0 (Hashtbl.find_opt ctx.collect_counts label) + 1
      in
      Hashtbl.replace ctx.collect_counts label next)
    ctx.case_collect

let collect ctx label = Hashtbl.replace ctx.case_collect label ()
let classify ctx label condition = if condition then collect ctx label

(* Presence, not proportion. The requirement registers wherever [cover] is
   written, and the marks are [classify]'s — so a label the run never marks
   on a passing case is the failure, and one it marks on every case is the
   same pass as one it marks on a tenth of them. *)
let cover ctx label condition =
  Hashtbl.replace ctx.required label ();
  classify ctx label condition

(* Outcomes *)

type cover_status = { label : string; hits : int; satisfied : bool }

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
  let collected = sorted_bindings ctx.collect_counts in
  let coverage =
    sorted_bindings ctx.required
    |> List.map (fun (label, ()) ->
        let hits = Option.value ~default:0 (List.assoc_opt label collected) in
        { label; hits; satisfied = hits > 0 })
  in
  { cases; discards; collected; coverage }

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
    let value = Gen.Private.value (Shrink_tree.root candidate_tree) in
    match run_case scratch body value with
    | Control ((Failure.Timeout _ as timeout), backtrace) ->
        (* The per-test alarm fired inside a candidate: a fact about the
           whole test, not this candidate — end the search (caught below). *)
        Printexc.raise_with_backtrace timeout backtrace
    | result -> same_kind first_class result
  in
  (* Three outcomes, and the third is why this is not an option. Forcing a
     candidate can raise — a [map]'s function, a [Gen.Private.list_exact]
     mask — and a memoized cell caches the exception, so the siblings behind
     it are unreachable and the descent must stop. What it must not do is
     stop the way convergence stops: that reported a truncated search as a
     minimal counterexample, which is the one thing a shrink report cannot
     get wrong. *)
  let rec first_accepted seq =
    match seq () with
    (* The explicit re-raise is load-bearing: the catch-all otherwise eats a
       handler-raised [Timeout] delivered during [Seq] forcing. *)
    | exception (Failure.Timeout _ as timeout) -> raise timeout
    | exception _ -> `Stopped
    | Seq.Nil -> `Converged
    | Seq.Cons (candidate, rest) -> (
        match accept candidate with
        | Some accepted -> `Accepted (candidate, accepted)
        | None -> first_accepted rest)
  in
  (* Best-so-far state lives in refs updated at each accepted step, so a
     [Timeout] firing at any poll point leaves them at the last accepted
     node. The descent wrapper below is the single place in the engine that
     consumes a timeout — everywhere else it propagates to the runner. *)
  let best = ref (tree, 0, first_class) in
  let timed_out = ref None in
  (* A descent that stopped is not a descent that converged, and the two used
     to render identically — a truncated search and a minimal counterexample
     both read "shrunk 100 steps". Set by the two stops the search survives:
     the step budget, and a candidate whose forcing raised. *)
  let exhausted = ref false in
  (try
     (* Probe first, then read the budget: a search whose last accepted step
        landed exactly on the budget with no further candidate had already
        converged, and reporting it as truncated would tell the reader the
        counterexample may not be minimal when it is. *)
     let rec descend steps tree =
       match first_accepted (Shrink_tree.children tree) with
       | `Converged -> ()
       | `Stopped -> exhausted := true
       | `Accepted (candidate, accepted) ->
           if steps >= max_shrink then exhausted := true
           else begin
             best := (candidate, steps + 1, accepted);
             descend (steps + 1) candidate
           end
     in
     descend 0 tree
   with Failure.Timeout limit -> timed_out := Some limit);
  let tree, steps, cls = !best in
  (tree, steps, cls, !timed_out, !exhausted)

(* The engine *)

(* The generated-case count when none is supplied. Not exported: the only
   caller that ever needed the number was a ceiling that no longer exists,
   and [run]'s .mli states it. *)
let default_count = 100
let default_max_shrink = 100

let inner_failure = function
  | Assertion failure -> failure
  | Exception (exn, backtrace) ->
      Failure.raised ~actual:(Printexc.to_string exn) ?backtrace ()

let run ?loc ?count ?max_discard ?max_shrink ?(examples = []) ~root ~path gen
    body =
  (* The case count and where it came from are one argument, because neither
     fact is usable without the other: a config-sourced count rides the
     failure payload so the replay hint can restate the flag, a
     declaration-site count replays by itself, and the engine default needs
     no hint at all. *)
  let count, config_count =
    match count with
    | None -> (default_count, None)
    | Some (`Declared n) -> (n, None)
    | Some (`Config n) -> (n, Some n)
  in
  if count < 0 then invalid_arg "Property.run: count must be non-negative";
  (* [max_shrink] carries its provenance in its own option, and needs no
     companion: with no declaration-site spelling for it, a supplied budget
     is always the run configuration's, so it rides the payload as it stands
     — a replay under the default budget would stop the descent elsewhere
     and report a different counterexample. *)
  let shrink_budget = Option.value max_shrink ~default:default_max_shrink in
  if shrink_budget < 0 then
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
  let fail ~rendered ~case_index ~shrink_steps ?timed_out
      ?(shrink_exhausted = false) ~examples ?rendering cls =
    let failure =
      Failure.property ?loc ~inner:(inner_failure cls) ?timed_out
        ?count:config_count ?max_shrink ~rendered ~case_index ~shrink_steps
        ~shrink_exhausted ~root ~examples ?rendering ()
    in
    Fail { failure; stats = stats () }
  in
  (* One case, and the bookkeeping every case source shares: a pass commits
     its labels and counts, a discard spends the budget, a control exception
     is the runner's and keeps the backtrace it was raised with. Only a
     failure differs between the two loops, so only a failure comes back. *)
  let run_one value =
    match run_case ctx body value with
    | Passed ->
        commit_case ctx;
        incr cases;
        `Passed
    | Discarded ->
        incr discards;
        `Discarded
    | Control (control, backtrace) ->
        Printexc.raise_with_backtrace control backtrace
    | Failed cls -> `Failed cls
  in
  (* Examples run first, unshrunk, unseeded, numbered separately. *)
  let rec run_examples index = function
    | [] -> None
    | value :: rest -> (
        match run_one value with
        | `Passed | `Discarded -> run_examples (index + 1) rest
        | `Failed cls ->
            let rendered, rendering =
              match Gen.Private.render_value gen value with
              | Some text -> (text, Failure.Value)
              | None ->
                  ( Printf.sprintf "<example %d>" (index + 1),
                    Failure.Placeholder )
            in
            Some
              (fail ~rendered ~case_index:index ~shrink_steps:0 ~examples:true
                 ~rendering cls))
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
          match Gen.Private.sample gen state with
          | exception Gen.Private.Rejected ->
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
              match run_one (Gen.Private.value (Shrink_tree.root tree)) with
              | `Passed -> generate ~passed:(passed + 1) ~attempts:(attempts + 1)
              | `Discarded -> generate ~passed ~attempts:(attempts + 1)
              | `Failed cls ->
                  let final_tree, steps, final_cls, timed_out, exhausted =
                    shrink ~max_shrink:shrink_budget ~body tree cls
                  in
                  let rendered, rendering =
                    match Gen.Private.render (Shrink_tree.root final_tree) with
                    | Value text -> (text, Failure.Value)
                    | Pre_image text -> (text, Failure.Pre_image)
                    | No_printer -> ("<no printer>", Failure.Placeholder)
                  in
                  fail ~rendered ~case_index:attempts ~shrink_steps:steps
                    ?timed_out ~shrink_exhausted:exhausted ~examples:false
                    ~rendering final_cls)
      in
      generate ~passed:0 ~attempts:0
