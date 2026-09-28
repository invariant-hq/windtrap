(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(* Discarding *)

let reject () = raise (Failure.Control `Discard)
let assume condition = if not condition then reject ()

(* Broken oracles *)

exception Oracle_failure of Failure.t

(* Labelling *)

(* The bookkeeping of one run. A case runs on fresh [marks], which commit into
   [counts] when it passes; the [demands] belong to the run. *)
type context = {
  marks : (string, unit) Hashtbl.t;
  counts : (string, int) Hashtbl.t; (* passing cases per label *)
  demands : (string, unit) Hashtbl.t; (* the [cover] labels reached *)
  mutable cases : int; (* the passing cases *)
  mutable discards : int;
}

let make_context () =
  {
    marks = Hashtbl.create 8;
    counts = Hashtbl.create 32;
    demands = Hashtbl.create 16;
    cases = 0;
    discards = 0;
  }

let scratch = make_context
let collect ctx label = Hashtbl.replace ctx.marks label ()
let classify ctx label condition = if condition then collect ctx label

let cover ctx label condition =
  Hashtbl.replace ctx.demands label ();
  classify ctx label condition

let run_case ctx law value =
  Hashtbl.reset ctx.marks;
  Failure.catch (fun () -> law ctx value)

let hits ctx label = Option.value ~default:0 (Hashtbl.find_opt ctx.counts label)

let commit ctx =
  let add label () = Hashtbl.replace ctx.counts label (hits ctx label + 1) in
  Hashtbl.iter add ctx.marks;
  ctx.cases <- ctx.cases + 1

let discard ctx = ctx.discards <- ctx.discards + 1

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
  List.sort
    (fun (a, _) (b, _) -> String.compare a b)
    (List.of_seq (Hashtbl.to_seq table))

let stats ctx =
  let status (label, ()) =
    let hits = hits ctx label in
    { label; hits; satisfied = hits > 0 }
  in
  {
    cases = ctx.cases;
    discards = ctx.discards;
    collected = sorted_bindings ctx.counts;
    coverage = List.map status (sorted_bindings ctx.demands);
  }

(* Shrinking *)

(* Sized by measurement against the integer descent. Under a threshold law
   (one that fails iff the value lies at least some distance from the
   origin) a quad of 64-bit integers converges in about 1_600 runs. A list
   of twenty of them under a fixed length spends the budget, since every
   step probes the converged elements before it again. A law over a list of
   8_558 elements ran 280 times per second, so the budget ends even that
   search in about 36 seconds. A change to a primitive's candidates reopens
   this sizing. *)
let shrink_budget = 10_000
let root_value tree = Gen.Engine.value (Gen.Engine.Shrink_tree.root tree)

(* A failure is an assertion, a broken oracle or any other exception, and
   the search keeps to the class of the first. *)
type failure_class = Assertion | Oracle | Other

let failure_class : Failure.fault -> failure_class = function
  | `Assertion _ -> Assertion
  | `Exception (Oracle_failure _, _) -> Oracle
  | `Exception _ -> Other

let same_class a b = failure_class a = failure_class b

(* The search terminates: [shrink_budget] bounds the runs of the law, and a
   node's candidates that run no law (a discarding re-generation, a filtered
   candidate) are finite for [Gen]'s generators. A candidate that discards
   costs one run, since a law that repeats its case discards before it
   repeats. *)
let shrink ~cost ~output law tree fault =
  let scratch = make_context () in
  (* A timeout can fire at any poll point of the search, which then ends at
     the last accepted node. A node is accepted with the output of its run
     or not at all, so the two never come from different runs. The failing
     case is accepted from the start, without output until [output] returns
     on it. A route holds the index of each accepted candidate among its
     siblings, the last first. *)
  let best = ref (tree, [], fault, None) in
  let rec descend ~runs route tree =
    let rec first_accepted ~runs index candidates =
      match Failure.catch candidates with
      | Error (`Timeout _ as timeout) -> Failure.reraise timeout
      | Error c ->
          (* A memoized cell keeps what its forcing raised, so the siblings
             behind it are unreachable. *)
          Failure.Candidate_raised (Failure.text (Failure.caught_to_string c))
      | Ok Seq.Nil -> Failure.Converged
      | Ok (Seq.Cons _) when runs >= shrink_budget -> Failure.Budget_spent
      | Ok (Seq.Cons (candidate, rest)) -> (
          match run_case scratch law (root_value candidate) with
          | Error (`Timeout _ as timeout) -> Failure.reraise timeout
          | Error (#Failure.fault as accepted) when same_class fault accepted ->
              let route = index :: route in
              best := (candidate, route, accepted, output ());
              descend ~runs:(runs + cost) route candidate
          | Error `Discard -> first_accepted ~runs:(runs + 1) (index + 1) rest
          | Ok () | Error (#Failure.fault | #Failure.control) ->
              first_accepted ~runs:(runs + cost) (index + 1) rest)
    in
    first_accepted ~runs 0 (Gen.Engine.Shrink_tree.children tree)
  in
  let search () =
    best := (tree, [], fault, output ());
    descend ~runs:0 [] tree
  in
  let shrink_end =
    match Failure.catch search with
    | Ok shrink_end -> shrink_end
    | Error (`Timeout limit) -> Failure.Timed_out limit
    | Error c -> Failure.reraise c
  in
  let tree, route, fault, tail = !best in
  (Gen.Engine.Shrink_tree.root tree, List.rev route, fault, tail, shrink_end)

(* The node of [tree] that [route] reaches, or [None] when a step names no
   candidate. A tree sampled again from a pure generator holds every route
   that a search took on it. *)
let rec follow tree = function
  | [] -> Some tree
  | index :: route -> (
      match Seq.drop index (Gen.Engine.Shrink_tree.children tree) () with
      | Seq.Cons (child, _) -> follow child route
      | Seq.Nil -> None)

(* Running *)

let default_count = 100

(* What the law raised, as a failure: an [Oracle_failure]'s payload as it
   is. *)
let inner_failure : Failure.fault -> Failure.t = function
  | `Exception (Oracle_failure failure, _) -> failure
  | fault -> Failure.of_fault fault

let run ?loc ?count ?max_discard ?(examples = []) ?(summary = Fun.const None)
    ?(prints_run = false) ?(cost = 1) ?(deterministic = true)
    ?(output = Fun.const None) ~root ~path gen law =
  let count, config_count =
    match count with
    | None -> (default_count, None)
    | Some (`Declared n) -> (n, None)
    | Some (`Config n) -> (n, Some n)
  in
  if count < 0 then invalid_arg "Property.run: count must be non-negative";
  if cost < 1 then invalid_arg "Property.run: cost must be positive";
  let max_discard =
    match max_discard with
    | None -> if count > max_int / 2 then max_int else 2 * count
    | Some limit when limit < 0 ->
        invalid_arg "Property.run: max_discard must be non-negative"
    | Some limit -> limit
  in
  let ctx = make_context () in
  let fail ~case_index ~examples ?summary ?rendering ?(shrink_steps = 0)
      ?shrink_end ?tail ?failed_again ~rendered fault =
    let failure =
      Failure.property ?loc ~inner:(inner_failure fault) ?count:config_count
        ?summary ~rendered ~case_index ~shrink_steps ?shrink_end ~root ~examples
        ?rendering ?failed_again ()
    in
    let failure =
      match tail with
      | Some tail -> Failure.with_output_tail tail failure
      | None -> failure
    in
    Fail { failure; stats = stats ctx }
  in
  (* The counterexample drawn again from the case's [state] along the
     search's [route], as a replay draws it: a pure [gen] draws it as it was
     first drawn, whatever a run did to the value it was given. That path was
     drawn once, so only the limit can cut the draw. *)
  let draw_again state route =
    match
      Failure.catch (fun () -> follow (Gen.Engine.sample gen state) route)
    with
    | Ok tree -> Option.map Gen.Engine.Shrink_tree.root tree
    | Error _ -> None
  in
  (* A law that does not fail again on its own counterexample is no function
     of it. It runs on the counterexample [drawn] again. *)
  let run_again drawn =
    if not deterministic then None
    else
      match Lazy.force drawn with
      | None -> None
      | Some node -> (
          match run_case (scratch ()) law (Gen.Engine.value node) with
          | Error #Failure.fault -> Some true
          | Ok () | Error (`Skip _ | `Exit | `Discard) -> Some false
          | Error (`Timeout _) -> None)
  in
  (* Built here: only the engine knows the case the limit cut and the cases
     that passed before it, which a replay needs. *)
  let timed_out ~case_index ~examples limit =
    let case =
      {
        Failure.case_index;
        examples;
        passed = ctx.cases;
        root;
        count = config_count;
      }
    in
    Fail { failure = Failure.timeout ?loc ~case limit; stats = stats ctx }
  in
  let check law value =
    match run_case ctx law value with
    | Ok () ->
        commit ctx;
        `Passed
    | Error `Discard ->
        discard ctx;
        `Discarded
    | Error (#Failure.fault as fault) -> `Failed fault
    | Error (`Timeout limit) -> `Timed_out limit
    | Error (#Failure.control as control) -> Failure.reraise control
  in
  let rec generate ~passed index =
    if ctx.discards > max_discard then Gave_up (stats ctx)
    else if passed >= count then
      let stats = stats ctx in
      if List.for_all (fun status -> status.satisfied) stats.coverage then
        Pass stats
      else Coverage_failed stats
    else
      let state = Seed.make (Seed.derive ~root ~path ~index) in
      match Failure.catch (fun () -> Gen.Engine.sample gen state) with
      | Error `Discard ->
          discard ctx;
          generate ~passed (index + 1)
      | Error (#Failure.fault as fault) ->
          fail ~case_index:index ~examples:false
            ~rendered:"<generator raised before producing a value>" fault
      | Error (`Timeout limit) ->
          timed_out ~case_index:index ~examples:false limit
      | Error (#Failure.control as control) -> Failure.reraise control
      | Ok tree -> (
          match check law (root_value tree) with
          | `Passed -> generate ~passed:(passed + 1) (index + 1)
          | `Discarded -> generate ~passed (index + 1)
          | `Timed_out limit ->
              timed_out ~case_index:index ~examples:false limit
          | `Failed fault ->
              let node, route, fault, tail, shrink_end =
                shrink ~cost ~output law tree fault
              in
              (* A run can change the value it is given, so the
                 counterexample prints drawn again, unless [gen]'s printer
                 prints the run. The same draw then runs again, once
                 printed. *)
              let drawn = lazy (draw_again state route) in
              let shown =
                if prints_run then node
                else Option.value ~default:node (Lazy.force drawn)
              in
              let rendered, rendering =
                match Gen.Engine.render shown with
                | Value text -> (text, Failure.Value)
                | Pre_image text -> (text, Failure.Pre_image)
              in
              let summary = summary (Gen.Engine.value shown) in
              (* The timeout spent the test's alarm: a run now has no limit. *)
              let failed_again =
                match shrink_end with
                | Failure.Timed_out _ -> None
                | Converged | Budget_spent | Candidate_raised _ ->
                    run_again drawn
              in
              fail ~case_index:index ~examples:false ?summary ~rendering
                ~shrink_steps:(List.length route) ~shrink_end ?tail
                ?failed_again ~rendered fault)
  in
  let rec run_examples index = function
    | [] -> generate ~passed:0 0
    | value :: rest -> (
        (* An example prints as it was given, before its run can change it.
           Its printer runs first in that run, so a control that the printer
           raises acts as the law's would. *)
        let rendered = ref None in
        let summarized = ref None in
        let printed_first ctx value =
          rendered := Some (Gen.Engine.render_value gen value);
          summarized := summary value;
          law ctx value
        in
        match check printed_first value with
        | `Passed | `Discarded -> run_examples (index + 1) rest
        | `Timed_out limit -> timed_out ~case_index:index ~examples:true limit
        | `Failed fault -> (
            (* The limit can expire while the output is read, after the
               example failed. *)
            let tail =
              match Failure.catch output with
              | Ok tail -> tail
              | Error (`Timeout _) -> None
              | Error c -> Failure.reraise c
            in
            (* [render_value] makes a fault of the printer its text, so the
               example that failed was rendered. *)
            match !rendered with
            | Some rendered ->
                fail ~case_index:index ~examples:true ?summary:!summarized ?tail
                  ~rendered fault
            | None -> assert false))
  in
  run_examples 0 examples
