(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(* Discarding *)

let reject () = raise (Failure.Control `Discard)
let assume condition = if not condition then reject ()

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

(* Sized against the primitives' descent. An integer's candidates halve the
   gap to its origin, so each accepted step at least halves the distance to
   the smallest failing value. A 64-bit integer then takes at most 64 steps
   under a threshold law (one that fails iff the value lies at least some
   distance from the origin), a quad of them at most 256, and a list one step
   per deleted chunk or shrunk element. 10_000 is about forty such quads, or a
   list of a hundred and fifty full-range integers each shrunk bit by bit. A
   law that is not a threshold can accept more steps per integer, each still
   strictly nearer the origin. A change to a primitive's candidates reopens
   this sizing. *)
let shrink_budget = 10_000
let root_value tree = Gen.Engine.value (Gen.Engine.Shrink_tree.root tree)

(* A failure is an assertion or any other exception, and the search keeps to
   the class of the first. *)
let same_class (a : Failure.fault) (b : Failure.fault) =
  match (a, b) with
  | `Assertion _, `Assertion _ | `Exception _, `Exception _ -> true
  | `Assertion _, `Exception _ | `Exception _, `Assertion _ -> false

(* The search terminates: an accepted step descends one level of the sample's
   tree, which is finite in depth for [Gen]'s generators, and [shrink_budget]
   bounds the accepted steps on any other tree. Only the test's timeout bounds
   the candidates probed at one node. *)
let shrink law tree fault =
  let scratch = make_context () in
  (* A timeout can fire at any poll point of the search, which then ends at
     the last accepted node. *)
  let best = ref (tree, 0, fault) in
  let rec descend steps tree =
    let rec first_accepted candidates =
      match Failure.catch candidates with
      | Error (`Timeout _ as timeout) -> Failure.reraise timeout
      | Error c ->
          (* A memoized cell keeps what its forcing raised, so the siblings
             behind it are unreachable. *)
          Failure.Candidate_raised (Failure.text (Failure.caught_to_string c))
      | Ok Seq.Nil -> Failure.Converged
      | Ok (Seq.Cons (candidate, rest)) -> (
          match run_case scratch law (root_value candidate) with
          | Error (`Timeout _ as timeout) -> Failure.reraise timeout
          | Error (#Failure.fault as accepted) when same_class fault accepted ->
              if steps >= shrink_budget then Failure.Budget_spent
              else begin
                best := (candidate, steps + 1, accepted);
                descend (steps + 1) candidate
              end
          | Ok () | Error (#Failure.fault | #Failure.control) ->
              first_accepted rest)
    in
    first_accepted (Gen.Engine.Shrink_tree.children tree)
  in
  let shrink_end =
    match Failure.catch (fun () -> descend 0 tree) with
    | Ok shrink_end -> shrink_end
    | Error (`Timeout limit) -> Failure.Timed_out limit
    | Error c -> Failure.reraise c
  in
  let tree, steps, fault = !best in
  (Gen.Engine.Shrink_tree.root tree, steps, fault, shrink_end)

(* Running *)

let default_count = 100

let inner_failure : Failure.fault -> Failure.t = function
  | `Assertion failure -> failure
  | `Exception (exn, backtrace) ->
      Failure.raised
        ~actual:(Failure.exn_to_string exn)
        ~backtrace:(Failure.backtrace_to_string backtrace)
        ()

let run ?loc ?count ?max_discard ?(examples = []) ?(summary = Fun.const None)
    ~root ~path gen law =
  let count, config_count =
    match count with
    | None -> (default_count, None)
    | Some (`Declared n) -> (n, None)
    | Some (`Config n) -> (n, Some n)
  in
  if count < 0 then invalid_arg "Property.run: count must be non-negative";
  let max_discard =
    match max_discard with
    | None -> if count > max_int / 2 then max_int else 2 * count
    | Some limit when limit < 0 ->
        invalid_arg "Property.run: max_discard must be non-negative"
    | Some limit -> limit
  in
  let ctx = make_context () in
  let fail ~case_index ~examples ?summary ?rendering ?(shrink_steps = 0)
      ?shrink_end ~rendered fault =
    let failure =
      Failure.property ?loc ~inner:(inner_failure fault) ?count:config_count
        ?summary ~rendered ~case_index ~shrink_steps ?shrink_end ~root ~examples
        ?rendering ()
    in
    Fail { failure; stats = stats ctx }
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
  let check value =
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
          match check (root_value tree) with
          | `Passed -> generate ~passed:(passed + 1) (index + 1)
          | `Discarded -> generate ~passed (index + 1)
          | `Timed_out limit ->
              timed_out ~case_index:index ~examples:false limit
          | `Failed fault ->
              let node, shrink_steps, fault, shrink_end =
                shrink law tree fault
              in
              let rendered, rendering =
                match Gen.Engine.render node with
                | Value text -> (text, Failure.Value)
                | Pre_image text -> (text, Failure.Pre_image)
              in
              fail ~case_index:index ~examples:false
                ?summary:(summary (Gen.Engine.value node))
                ~rendering ~shrink_steps ~shrink_end ~rendered fault)
  in
  let rec run_examples index = function
    | [] -> generate ~passed:0 0
    | value :: rest -> (
        match check value with
        | `Passed | `Discarded -> run_examples (index + 1) rest
        | `Timed_out limit -> timed_out ~case_index:index ~examples:true limit
        | `Failed fault ->
            let rendered = Gen.Engine.render_value gen value in
            fail ~case_index:index ~examples:true ?summary:(summary value)
              ~rendered fault)
  in
  run_examples 0 examples
