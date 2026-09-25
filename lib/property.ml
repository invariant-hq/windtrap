(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC

  The case loop structure and label bookkeeping derive from windtrap v1's
  prop/prop.ml, rebuilt over Seed (per-case derivation), Gen (integrated
  shrinking), and Failure (typed counterexamples).
  --------------------------------------------------------------------------*)

(* Discarding *)

let assume condition = if not condition then raise (Failure.Control `Discard)
let reject () = raise (Failure.Control `Discard)

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
   written, and the marks are [classify]'s, so a label the run never marks
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

   [run_case] runs the law once on a fresh set of marks. The callers decide
   what a control means: the case loops raise it again, except a discard,
   and the shrink search rejects the candidate, so that a candidate cannot
   turn a recorded failure into a skip, or ends on a timeout (see
   [shrink]). *)

let run_case ctx body value =
  reset_case ctx;
  Failure.catch (fun () -> body ctx value)

(* Shrinking

   Greedy descent over the sample's shrink tree: move to the first candidate
   whose body run fails in the same way (assertion failures accept any
   assertion failure, exception failures any non-assertion exception, as
   in v1) and stop when no candidate is accepted, the step cap is
   reached, or forcing a candidate raises (a memoized cell caches its
   exception, so its siblings are unreachable). A timeout raised
   anywhere in the search also stops it, at the last accepted node: the
   whole-test budget can end the search but never erase a counterexample
   already found. Candidate runs use a scratch context: their labels
   never pollute the committed tables. The accepted classification is
   captured during the descent, so the final inner failure needs no extra
   body run. The descent is bounded: an accepted step descends one level of
   the sample's tree, which is finite in depth for [Gen]'s generators, and
   [shrink_budget] bounds the accepted steps on any other tree. Nothing but
   the per-test timeout bounds the candidates probed at one node. *)

let same_kind (original : Failure.fault) candidate =
  match (original, candidate) with
  | `Assertion _, Error (`Assertion _ as accepted)
  | `Exception _, Error (`Exception _ as accepted) ->
      Some accepted
  | _, (Ok () | Error _) -> None

let shrink ~budget ~body tree first_class =
  let scratch = make_context () in
  let accept candidate_tree =
    let value = Gen.Engine.value (Gen.Engine.Shrink_tree.root candidate_tree) in
    match run_case scratch body value with
    | Error (`Timeout _ as timeout) ->
        (* The per-test alarm fired inside a candidate: a fact about the
           whole test, not this candidate; end the search (caught below). *)
        Failure.reraise timeout
    | result -> same_kind first_class result
  in
  (* Three outcomes, and the third is why this is not an option. Forcing a
     candidate can raise (a [map]'s function, a stateful [~pre]) and a
     memoized cell caches the exception, so the siblings behind it are
     unreachable and the descent must stop. What it must not do is stop the
     way convergence stops: that reported a truncated search as a minimal
     counterexample, which is the one thing a shrink report cannot get
     wrong. *)
  let rec first_accepted seq =
    match Failure.catch seq with
    | Error (`Timeout _ as timeout) -> Failure.reraise timeout
    | Error c -> `Stopped (Failure.caught_to_string c)
    | Ok Seq.Nil -> `Converged
    | Ok (Seq.Cons (candidate, rest)) -> (
        match accept candidate with
        | Some accepted -> `Accepted (candidate, accepted)
        | None -> first_accepted rest)
  in
  (* Best-so-far state lives in refs updated at each accepted step, so a
     timeout firing at any poll point leaves them at the last accepted node.
     The descent wrapper below is the one handler of a timeout in this
     module. One delivered while the counterexample is formatted is caught
     by the guard of [Gen.Engine.render]; everywhere else it propagates to
     the runner. *)
  let best = ref (tree, 0, first_class) in
  (* A descent that stopped is not a descent that converged, and the two used
     to render identically. A truncated search and a minimal counterexample
     both read "shrunk 100 steps". Set by the two stops the search survives:
     the step budget, and a candidate whose forcing raised. *)
  let stop = ref Failure.Converged in
  (* Probe first, then read the budget: a search whose last accepted step
     landed exactly on the budget with no further candidate had already
     converged, and reporting it as truncated would tell the reader the
     counterexample may not be minimal when it is. *)
  let rec descend steps tree =
    match first_accepted (Gen.Engine.Shrink_tree.children tree) with
    | `Converged -> ()
    | `Stopped text -> stop := Failure.Candidate_raised (Failure.text text)
    | `Accepted (candidate, accepted) ->
        if steps >= budget then stop := Failure.Budget_spent
        else begin
          best := (candidate, steps + 1, accepted);
          descend (steps + 1) candidate
        end
  in
  (match Failure.catch (fun () -> descend 0 tree) with
  | Ok () -> ()
  | Error (`Timeout limit) -> stop := Failure.Timed_out limit
  | Error c -> Failure.reraise c);
  let tree, steps, cls = !best in
  (tree, steps, cls, !stop)

(* The engine *)

(* The generated-case count when none is supplied. Not exported: the only
   caller that ever needed the number was a ceiling that no longer exists,
   and [run]'s .mli states it. *)
let default_count = 100

(* The accepted-step budget of a shrink search: fixed, so a replay under
   the same root descends the same path to the same node and prints the
   same counterexample. A knob here made the printed value depend on its
   setting. Sized against the primitives' descent: an integer's candidates
   halve the gap to its origin, so each accepted step at least halves the
   distance to the smallest failing value, and a 64-bit integer takes at
   most 64 steps under any threshold law, one that fails iff the value lies
   at least some distance from the origin; a quad of them at most 256, and
   a list one step per deleted chunk or shrunk element. 10_000 is about
   forty such quads, or a list of a hundred and fifty full-range integers
   each shrunk bit by bit. A law that is not a threshold can accept more
   steps per integer, each still strictly nearer the origin. A change to a
   primitive's candidates reopens this sizing. A search that spends it is
   reported as stopped ([Failure.Budget_spent]) rather than minimal; the
   per-test timeout, not this number, bounds a search that must not run
   away. *)
let shrink_budget = 10_000

let inner_failure : Failure.fault -> Failure.t = function
  | `Assertion failure -> failure
  | `Exception (exn, backtrace) ->
      Failure.raised
        ~actual:(Failure.exn_to_string exn)
        ~backtrace:(Failure.backtrace_to_string backtrace)
        ()

let run ?loc ?count ?max_discard ?(examples = []) ?summary ~root ~path gen body
    =
  (* A config-sourced count rides the failure payload so the replay hint can
     restate the flag; a declared count and the default replay by
     themselves. *)
  let count, config_count =
    match count with
    | None -> (default_count, None)
    | Some (`Declared n) -> (n, None)
    | Some (`Config n) -> (n, Some n)
  in
  if count < 0 then invalid_arg "Property.run: count must be non-negative";
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
  let summarize value = Option.bind summary (fun summary -> summary value) in
  let fail ?summary ~rendered ~case_index ~shrink_steps ?shrink_end ~examples
      ?rendering cls =
    (* [rendering] defaults to the value: a placeholder carries its own
       remedy in its text, so the payload need not classify it. *)
    let failure =
      Failure.property ?loc ~inner:(inner_failure cls) ?count:config_count
        ?summary ~rendered ~case_index ~shrink_steps ?shrink_end ~root ~examples
        ?rendering ()
    in
    Fail { failure; stats = stats () }
  in
  (* The test's limit, expired in a case before any case failed. The
     runner knows the test and the limit, the engine alone which case ran
     and how many passed before it, which a replay needs: the failure is
     built here. *)
  let timed_out ~case_index ~examples limit =
    let case =
      {
        Failure.case_index;
        examples;
        passed = !cases;
        root;
        count = config_count;
      }
    in
    Fail { failure = Failure.timeout ?loc ~case limit; stats = stats () }
  in
  (* One case, and the bookkeeping every case source shares: a pass commits
     its labels and counts, a discard spends the budget, a timeout ends the
     run in the case, any other control is the runner's. Only a failure and
     a timeout differ between the two loops, so only they come back. *)
  let run_one value =
    match run_case ctx body value with
    | Ok () ->
        commit_case ctx;
        incr cases;
        `Passed
    | Error `Discard ->
        incr discards;
        `Discarded
    | Error (#Failure.fault as fault) -> `Failed fault
    | Error (`Timeout limit) -> `Timed_out limit
    | Error (#Failure.control as control) -> Failure.reraise control
  in
  (* Examples run first, unshrunk, unseeded, numbered separately. *)
  let rec run_examples index = function
    | [] -> None
    | value :: rest -> (
        match run_one value with
        | `Passed | `Discarded -> run_examples (index + 1) rest
        | `Timed_out limit ->
            Some (timed_out ~case_index:index ~examples:true limit)
        | `Failed cls ->
            let rendered = Gen.Engine.render_value gen value in
            Some
              (fail ?summary:(summarize value) ~rendered ~case_index:index
                 ~shrink_steps:0 ~examples:true cls))
  in
  match run_examples 0 examples with
  | Some outcome -> outcome
  | None ->
      let rec generate ~passed ~attempts =
        (* Budget before goal: a run whose discards exceed the budget gives
           up even when [count] is already met. Examples can discard past
           the budget before any generation, including when [count] is 0. *)
        if !discards > max_discard then Gave_up (stats ())
        else if passed >= count then
          let final = stats () in
          if List.for_all (fun status -> status.satisfied) final.coverage then
            Pass final
          else Coverage_failed final
        else
          let state = Seed.make (Seed.derive ~root ~path ~index:attempts) in
          match Failure.catch (fun () -> Gen.Engine.sample gen state) with
          | Error `Discard ->
              incr discards;
              generate ~passed ~attempts:(attempts + 1)
          | Error (#Failure.fault as fault) ->
              fail ~rendered:"<generator raised before producing a value>"
                ~case_index:attempts ~shrink_steps:0 ~examples:false fault
          | Error (`Timeout limit) ->
              timed_out ~case_index:attempts ~examples:false limit
          | Error (#Failure.control as control) ->
              (* Delivered inside the generator (an alarm at a poll point, a
                 skip in a generator's function): the runner's, never a
                 generator crash. *)
              Failure.reraise control
          | Ok tree -> (
              match
                run_one (Gen.Engine.value (Gen.Engine.Shrink_tree.root tree))
              with
              | `Passed -> generate ~passed:(passed + 1) ~attempts:(attempts + 1)
              | `Discarded -> generate ~passed ~attempts:(attempts + 1)
              | `Timed_out limit ->
                  timed_out ~case_index:attempts ~examples:false limit
              | `Failed cls ->
                  let final_tree, steps, final_cls, shrink_end =
                    shrink ~budget:shrink_budget ~body tree cls
                  in
                  let final = Gen.Engine.Shrink_tree.root final_tree in
                  let rendered, rendering =
                    match Gen.Engine.render final with
                    | Value text -> (text, Failure.Value)
                    | Pre_image text -> (text, Failure.Pre_image)
                  in
                  fail
                    ?summary:(summarize (Gen.Engine.value final))
                    ~rendered ~case_index:attempts ~shrink_steps:steps
                    ~shrink_end ~examples:false ~rendering final_cls)
      in
      generate ~passed:0 ~attempts:0
