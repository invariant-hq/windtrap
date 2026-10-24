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

(* Sized by measurement against the integer descent of 0.2's trees. Under a
   threshold law (one that fails iff the value lies at least some distance
   from the origin) a quad of 64-bit integers converged in about 1_600 runs.
   A law over a list of 8_558 elements ran 280 times per second, so the
   budget ends even that search in about 36 seconds. A change to the passes
   reopens this sizing. *)
let shrink_budget = 10_000

(* A failure is an assertion, a broken oracle or any other exception, and
   the search keeps to the class of the first. *)
type failure_class = Assertion | Oracle | Other

let failure_class : Failure.fault -> failure_class = function
  | `Assertion _ -> Assertion
  | `Exception (Oracle_failure _, _) -> Oracle
  | `Exception _ -> Other

let same_class a b = failure_class a = failure_class b

(* What a search found: the node that prints, the node to run once more,
   drawn again, and the failure and output of its run. *)
type 'a shrunk = {
  shown : 'a Gen.Engine.sample;
  again : 'a Gen.Engine.sample option Lazy.t;
  fault : Failure.fault;
  tail : Failure.tail option;
  steps : int;
  ending : Failure.shrink_end;
  token : string option;
}

(* A timeout can fire at any poll point of a search, which then ends at the
   last accepted case. A case is accepted with the output of its run or not
   at all, so the two never come from different runs. The failing case is
   accepted from the start, without output until [output] returns on it. *)
let searched search =
  match Failure.catch search with
  | Ok ending -> ending
  | Error (`Timeout limit) -> Failure.Timed_out limit
  | Error c -> Failure.reraise c

let case_of src : Shrink.case =
  {
    choices = Choice.choices src;
    spans = Choice.spans src;
    dependents = Choice.dependents src;
  }

(* The token's digest is of the value's printing, so a replay that parses
   another value is stale even where the choices still parse. *)
let digest text = String.sub (Digest.to_hex (Digest.string text)) 0 8

let rendered_text node =
  match Gen.Engine.render node with Value text | Pre_image text -> text

(* The text of a counterexample, and whether it is an OCaml value. *)
let rendered node =
  match Gen.Engine.render node with
  | Value text when Gen.Engine.literal node -> (text, Failure.Ocaml)
  | Value text -> (text, Failure.Value)
  | Pre_image text -> (text, Failure.Pre_image)

let parse_again gen choices =
  match
    Failure.catch (fun () -> Gen.Engine.parse gen (Choice.replaying choices))
  with
  | Ok node -> Some node
  | Error _ -> None

(* The reducer edits the failing case's choices and parses each edit
   again. An edit whose parse discards or raises a fault is rejected at the
   cost of a discard, and one whose parse reads no smaller sequence runs no
   law; a candidate is a value of the generator by construction, so no
   candidate can end the search. Each run of the law costs [cost]. *)
let shrink_choices ~cost ~output ~shrinking ~prints_run gen law case node fault
    =
  let scratch = make_context () in
  let best = ref (case, node, fault, None) and steps = ref 0 in
  let test choices =
    let src = Choice.replaying choices in
    match Failure.catch (fun () -> Gen.Engine.parse gen src) with
    | Error (`Timeout _ as timeout) -> Failure.reraise timeout
    | Error (`Discard | #Failure.fault) -> Shrink.Discards
    | Error (#Failure.control as control) -> Failure.reraise control
    | Ok _ when
        let kept, _, _, _ = !best in
        Shrink.shortlex (Choice.choices src) kept.Shrink.choices >= 0 ->
        Shrink.Not_smaller
    | Ok node -> (
        shrinking !steps;
        match run_case scratch law (Gen.Engine.value node) with
        | Error (`Timeout _ as timeout) -> Failure.reraise timeout
        | Error (#Failure.fault as accepted) when same_class fault accepted ->
            let parsed = case_of src in
            let keep () =
              let tail = output () in
              incr steps;
              best := (parsed, node, accepted, tail)
            in
            Fails { parsed; keep }
        | Error `Discard -> Discards
        | Ok () | Error (#Failure.fault | #Failure.control) -> Passes)
  in
  let search () =
    best := (case, node, fault, output ());
    match Shrink.shrink ~budget:shrink_budget ~cost test case with
    | _, _, Shrink.Converged -> Failure.Converged
    | _, _, Shrink.Budget_spent -> Failure.Budget_spent
  in
  let ending = searched search in
  let case, node, fault, tail = !best in
  (* The final case is parsed again for printing: a run can change the value
     it is given, and a parse builds it as it was drawn. *)
  let again = lazy (parse_again gen case.choices) in
  let shown =
    if prints_run then node else Option.value ~default:node (Lazy.force again)
  in
  let token =
    Choice.token case.choices ~digest:(digest (rendered_text shown))
  in
  { shown; again; fault; tail; steps = !steps; ending; token = Some token }

(* Rose trees, until stateful programs parse choices: the search descends
   the tree of a sample, greedily, to the first candidate that fails in the
   class of the first failure. A route holds the index of each accepted
   candidate among its siblings, the last first. *)
let shrink_tree ~cost ~output ~shrinking ~prints_run gen law state tree fault =
  let scratch = make_context () in
  let root_value tree = Gen.Engine.value (Gen.Engine.Shrink_tree.root tree) in
  let best = ref (tree, [], fault, None) in
  let rec descend ~runs ~steps route tree =
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
          shrinking steps;
          match run_case scratch law (root_value candidate) with
          | Error (`Timeout _ as timeout) -> Failure.reraise timeout
          | Error (#Failure.fault as accepted) when same_class fault accepted ->
              let route = index :: route in
              best := (candidate, route, accepted, output ());
              descend ~runs:(runs + cost) ~steps:(steps + 1) route candidate
          | Error `Discard -> first_accepted ~runs:(runs + 1) (index + 1) rest
          | Ok () | Error (#Failure.fault | #Failure.control) ->
              first_accepted ~runs:(runs + cost) (index + 1) rest)
    in
    first_accepted ~runs 0 (Gen.Engine.Shrink_tree.children tree)
  in
  let search () =
    best := (tree, [], fault, output ());
    descend ~runs:0 ~steps:0 [] tree
  in
  let ending = searched search in
  let tree, route, fault, tail = !best in
  let node = Gen.Engine.Shrink_tree.root tree in
  (* The counterexample drawn again from the case's [state] along the
     search's [route], as a replay draws it: a pure [gen] draws it as it was
     first drawn, whatever a run did to the value it was given. *)
  let rec follow tree = function
    | [] -> Some tree
    | index :: route -> (
        match Seq.drop index (Gen.Engine.Shrink_tree.children tree) () with
        | Seq.Cons (child, _) -> follow child route
        | Seq.Nil -> None)
  in
  let again =
    lazy
      (match
         Failure.catch (fun () ->
             follow (Gen.Engine.sample gen state) (List.rev route))
       with
      | Ok tree -> Option.map Gen.Engine.Shrink_tree.root tree
      | Error _ -> None)
  in
  let shown =
    if prints_run then node else Option.value ~default:node (Lazy.force again)
  in
  { shown; again; fault; tail; steps = List.length route; ending; token = None }

(* Running *)

let default_count = 100

(* What the law raised, as a failure: an [Oracle_failure]'s payload as it
   is. *)
let inner_failure : Failure.fault -> Failure.t = function
  | `Exception (Oracle_failure failure, _) -> failure
  | fault -> Failure.of_fault fault

let run ?loc ?count ?max_discard ?(examples = []) ?(summary = Fun.const None)
    ?(prints_run = false) ?(cost = 1) ?(deterministic = true)
    ?(output = Fun.const None) ?(shrinking = ignore) ?replay ~root ~path gen
    law =
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
      ?shrink_end ?tail ?failed_again ?token ~rendered fault =
    let failure =
      Failure.property ?loc ~inner:(inner_failure fault) ?count:config_count
        ?summary ~rendered ~case_index ~shrink_steps ?shrink_end ~root ~examples
        ?rendering ?failed_again ?token ()
    in
    let failure =
      match tail with
      | Some tail -> Failure.with_output_tail tail failure
      | None -> failure
    in
    Fail { failure; stats = stats ctx }
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
  (* A case drawn from [state]: its node, and the search from it. *)
  let draw state =
    match Gen.Engine.tree gen with
    | Some _ ->
        let tree = Gen.Engine.sample gen state in
        ( Gen.Engine.Shrink_tree.root tree,
          shrink_tree ~cost ~output ~shrinking ~prints_run gen law state tree )
    | None ->
        let src = Choice.generating state in
        let node = Gen.Engine.parse gen src in
        ( node,
          shrink_choices ~cost ~output ~shrinking ~prints_run gen law
            (case_of src) node )
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
      match Failure.catch (fun () -> draw state) with
      | Error `Discard ->
          discard ctx;
          generate ~passed (index + 1)
      | Error (#Failure.fault as fault) ->
          fail ~case_index:index ~examples:false
            ~rendered:"<generator raised before producing a value>" fault
      | Error (`Timeout limit) ->
          timed_out ~case_index:index ~examples:false limit
      | Error (#Failure.control as control) -> Failure.reraise control
      | Ok (node, shrink) -> (
          match check law (Gen.Engine.value node) with
          | `Passed -> generate ~passed:(passed + 1) (index + 1)
          | `Discarded -> generate ~passed (index + 1)
          | `Timed_out limit ->
              timed_out ~case_index:index ~examples:false limit
          | `Failed fault ->
              let found = shrink fault in
              let rendered, rendering = rendered found.shown in
              let summary = summary (Gen.Engine.value found.shown) in
              (* The timeout spent the test's alarm: a run now has no limit. *)
              let failed_again =
                match found.ending with
                | Failure.Timed_out _ -> None
                | Converged | Budget_spent | Candidate_raised _ ->
                    run_again found.again
              in
              fail ~case_index:index ~examples:false ?summary ~rendering
                ~shrink_steps:found.steps ~shrink_end:found.ending
                ?tail:found.tail ?failed_again ?token:found.token ~rendered
                found.fault)
  in
  (* The output can be cut by the limit after the case failed. *)
  let output_after_failure () =
    match Failure.catch output with
    | Ok tail -> tail
    | Error (`Timeout _) -> None
    | Error c -> Failure.reraise c
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
            let tail = output_after_failure () in
            (* [render_value] makes a fault of the printer its text, so the
               example that failed was rendered. *)
            match !rendered with
            | Some rendered ->
                fail ~case_index:index ~examples:true ?summary:!summarized ?tail
                  ~rendered fault
            | None -> assert false))
  in
  (* A replayed case is neither seeded nor shrunk, as an example, and keeps
     its token. A token that its generator no longer parses to the value it
     recorded is refused as stale: it never checks another value. *)
  let replayed token =
    let refused why =
      let message = Pp.str "--replay %s: %s" token why in
      Fail { failure = Failure.message ?loc message; stats = stats ctx }
    in
    match (Choice.of_token token, Gen.Engine.tree gen) with
    | Error why, _ -> refused why
    | Ok _, Some _ -> refused "no choice sequence describes this test's cases"
    | Ok (choices, digested), None -> (
        let src = Choice.replaying choices in
        match Failure.catch (fun () -> Gen.Engine.parse gen src) with
        | Error (`Timeout limit) -> timed_out ~case_index:0 ~examples:true limit
        | Error (`Discard | #Failure.fault) ->
            refused "stale: its generator no longer parses a value from it"
        | Error (#Failure.control as control) -> Failure.reraise control
        | Ok node -> (
            let text, rendering = rendered node in
            if choices <> Choice.choices src || digested <> digest text then
              refused
                (Pp.str "stale: its generator now parses %s, not the value it \
                   recorded" text)
            else
              match check law (Gen.Engine.value node) with
              | `Passed -> Pass (stats ctx)
              | `Discarded -> refused "the law discarded its case"
              | `Timed_out limit ->
                  timed_out ~case_index:0 ~examples:true limit
              | `Failed fault ->
                  let tail = output_after_failure () in
                  let summary = summary (Gen.Engine.value node) in
                  fail ~case_index:0 ~examples:true ?summary ~rendering ?tail
                    ~token ~rendered:text fault))
  in
  match replay with
  | Some token -> replayed token
  | None -> run_examples 0 examples
