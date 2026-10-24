(*--------------------------------------------------------------------------
  Copyright (c) 2026 Invariant Systems. All rights reserved.
  SPDX-License-Identifier: ISC
  --------------------------------------------------------------------------*)

(* Discarding *)

let reject () = raise (Failure.Control `Discard)
let assume condition = if not condition then reject ()

(* Broken oracles *)

exception Oracle_failure of Failure.t

(* Runs that print themselves *)

exception
  Run_failure of {
    fault : Failure.fault;
    record : string;
    summary : string option;
  }

(* Labelling *)

(* The bookkeeping of one run. A case runs on fresh [marks], which commit into
   [counts] when it passes; the [demands] belong to the run. *)
type context = {
  marks : (string, unit) Hashtbl.t;
  counts : (string, int) Hashtbl.t; (* passing cases per label *)
  demands : (string, unit) Hashtbl.t; (* the [cover] labels reached *)
  mutable cases : int; (* the passing cases *)
  mutable discards : int;
  mutable whole : int option; (* the values of a domain checked whole *)
}

let make_context () =
  {
    marks = Hashtbl.create 8;
    counts = Hashtbl.create 32;
    demands = Hashtbl.create 16;
    cases = 0;
    discards = 0;
    whole = None;
  }

let scratch = make_context
let collect ctx label = Hashtbl.replace ctx.marks label ()
let classify ctx label condition = if condition then collect ctx label

let cover ctx label condition =
  Hashtbl.replace ctx.demands label ();
  classify ctx label condition

(* What a run that failed printed of its case, from a [Run_failure]. *)
type record = { text : string; summary : string option }

(* A case runs [runs] times on one set of marks, and fails with the first run
   that fails, and the record that run printed. *)
let run_case ~runs ctx law value =
  Hashtbl.reset ctx.marks;
  let rec from run =
    match Failure.catch (fun () -> law ctx value) with
    | Ok () when run < runs -> from (run + 1)
    | Ok () -> Ok ()
    | Error (`Exception (Run_failure { fault; record; summary }, _)) ->
        Error ((fault :> Failure.caught), Some { text = record; summary })
    | Error c -> Error (c, None)
  in
  from 1

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
  whole : int option;
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
    whole = ctx.whole;
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
   drawn again, the failure and the record of its run, and its output. *)
type 'a shrunk = {
  shown : 'a Gen.Engine.sample;
  again : 'a Gen.Engine.sample option Lazy.t;
  fault : Failure.fault;
  record : record option;
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

(* The text of a counterexample, and whether it is an OCaml value. A case
   whose run printed a record prints as that record. *)
let rendered ?record node =
  match record with
  | Some record -> (record.text, Failure.Value)
  | None -> (
      match Gen.Engine.render node with
      | Value text when Gen.Engine.literal node -> (text, Failure.Ocaml)
      | Value text -> (text, Failure.Value)
      | Pre_image text -> (text, Failure.Pre_image))

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
   candidate can end the search. Each case costs its [runs]. *)
let shrink_choices ~runs ~output ~shrinking gen law case node (fault, record) =
  let scratch = make_context () in
  let best = ref (case, node, fault, record, None) and steps = ref 0 in
  let test choices =
    let src = Choice.replaying choices in
    match Failure.catch (fun () -> Gen.Engine.parse gen src) with
    | Error (`Timeout _ as timeout) -> Failure.reraise timeout
    | Error (`Discard | #Failure.fault) -> Shrink.Discards
    | Error (#Failure.control as control) -> Failure.reraise control
    | Ok _
      when let kept, _, _, _, _ = !best in
           Shrink.shortlex (Choice.choices src) kept.Shrink.choices >= 0 ->
        Shrink.Not_smaller
    | Ok node -> (
        shrinking !steps;
        match run_case ~runs scratch law (Gen.Engine.value node) with
        | Error ((`Timeout _ as timeout), _) -> Failure.reraise timeout
        | Error ((#Failure.fault as accepted), record)
          when same_class fault accepted ->
            let parsed = case_of src in
            let keep () =
              let tail = output () in
              incr steps;
              best := (parsed, node, accepted, record, tail)
            in
            Fails { parsed; keep }
        | Error (`Discard, _) -> Discards
        | Ok () | Error ((#Failure.fault | #Failure.control), _) -> Passes)
  in
  let search () =
    best := (case, node, fault, record, output ());
    match Shrink.shrink ~budget:shrink_budget ~cost:runs test case with
    | _, _, Shrink.Converged -> Failure.Converged
    | _, _, Shrink.Budget_spent -> Failure.Budget_spent
  in
  let ending = searched search in
  let case, node, fault, record, tail = !best in
  (* The final case is parsed again for printing: a run can change the value
     it is given, and a parse builds it as it was drawn. *)
  let again = lazy (parse_again gen case.choices) in
  let shown = Option.value ~default:node (Lazy.force again) in
  let token =
    Choice.token case.choices ~digest:(digest (rendered_text shown))
  in
  {
    shown;
    again;
    fault;
    record;
    tail;
    steps = !steps;
    ending;
    token = Some token;
  }

(* Finite domains *)

(* The values of [gen], every sequence it reads, in lexicographic order:
   each parse reads zeros past its prefix, and the next prefix raises the
   last choice below its bound. [None] once more than [limit] values are
   found, or when a parse raises a fault, which a generated case then
   reports. A domain without end gives [None] early: once the parses have
   read [16 * (limit + 1)] choices, when a parse reads more than
   [longest] choices, as a list does, and when a choice's bound alone
   exceeds that budget, as an [int]'s does. A sequence that discards or
   that spells again another's value is no value. *)
let longest = 64

let enumerate gen limit =
  let budget = 16 * (limit + 1) in
  let wide bound = Int64.unsigned_compare bound (Int64.of_int budget) > 0 in
  let rec next prefix ~read values count =
    if count > limit || read > budget then None
    else
      let src = Choice.replaying prefix in
      let parsed = Failure.catch (fun () -> Gen.Engine.parse gen src) in
      let choices = Choice.choices src and bounds = Choice.bounds src in
      let count =
        if Array.length choices > longest || Array.exists wide bounds then
          limit + 1
        else count
      in
      let values, count =
        match parsed with
        | Ok node when not (Choice.redundant src) ->
            ((choices, node) :: values, count + 1)
        | Ok _ | Error `Discard -> (values, count)
        | Error (`Timeout _ as timeout) -> Failure.reraise timeout
        | Error (#Failure.fault | #Failure.control) -> (values, limit + 1)
      in
      let rec raised i =
        if i < 0 then None
        else if Int64.unsigned_compare choices.(i) bounds.(i) < 0 then
          Some
            (Array.append (Array.sub choices 0 i) [| Int64.succ choices.(i) |])
        else raised (i - 1)
      in
      if count > limit then None
      else
        match raised (Array.length choices - 1) with
        | None -> Some (List.rev values)
        | Some prefix ->
            next prefix ~read:(read + Array.length choices) values count
  in
  next [||] ~read:0 [] 0

(* Running *)

let default_count = 100

(* What the law raised, as a failure: an [Oracle_failure]'s payload as it
   is. *)
let inner_failure : Failure.fault -> Failure.t = function
  | `Exception (Oracle_failure failure, _) -> failure
  | fault -> Failure.of_fault fault

let run ?loc ?count ?max_discard ?(examples = []) ?(summary = Fun.const None)
    ?(runs = 1) ?(output = Fun.const None) ?(shrinking = ignore) ?replay
    ?(replayed_by = "--replay") ?case ?only ~root ~path gen law =
  let count, config_count =
    match count with
    | None -> (default_count, None)
    | Some (`Declared n) -> (n, None)
    | Some (`Config n) -> (n, Some n)
  in
  if count < 0 then invalid_arg "Property.run: count must be non-negative";
  if runs < 1 then invalid_arg "Property.run: runs must be positive";
  let max_discard =
    match max_discard with
    | None -> if count > max_int / 2 then max_int else 2 * count
    | Some limit when limit < 0 ->
        invalid_arg "Property.run: max_discard must be non-negative"
    | Some limit -> limit
  in
  let ctx = make_context () in
  let fail ~case_index ~examples ?record ?summary ?rendering ?(shrink_steps = 0)
      ?shrink_end ?tail ?failed_again ?token ~rendered fault =
    let summary =
      match record with Some record -> record.summary | None -> summary
    in
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
     of it. It runs on the counterexample [drawn] again, unless it runs a case
     several times, which says that a case may pass where it failed. *)
  let run_again drawn =
    if runs > 1 then None
    else
      match Lazy.force drawn with
      | None -> None
      | Some node -> (
          match run_case ~runs (scratch ()) law (Gen.Engine.value node) with
          | Error (#Failure.fault, _) -> Some true
          | Ok () | Error ((`Skip _ | `Exit | `Discard), _) -> Some false
          | Error (`Timeout _, _) -> None)
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
    match run_case ~runs ctx law value with
    | Ok () ->
        commit ctx;
        `Passed
    | Error (`Discard, _) ->
        discard ctx;
        `Discarded
    | Error ((#Failure.fault as fault), record) -> `Failed (fault, record)
    | Error (`Timeout limit, _) -> `Timed_out limit
    | Error ((#Failure.control as control), _) -> Failure.reraise control
  in
  (* A case drawn from [state]: its node, and the search from it. *)
  let draw state =
    let src = Choice.generating state in
    let node = Gen.Engine.parse gen src in
    (node, shrink_choices ~runs ~output ~shrinking gen law (case_of src) node)
  in
  let judged () =
    if ctx.discards > max_discard then Gave_up (stats ctx)
    else
      let stats = stats ctx in
      if List.for_all (fun status -> status.satisfied) stats.coverage then
        Pass stats
      else Coverage_failed stats
  in
  let rec generate ~passed index =
    if ctx.discards > max_discard then Gave_up (stats ctx)
    else if passed >= count then judged ()
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
          | `Failed failed ->
              let found = shrink failed in
              let record = found.record in
              let rendered, rendering = rendered ?record found.shown in
              let summary = summary (Gen.Engine.value found.shown) in
              (* The timeout spent the test's alarm: a run now has no limit. *)
              let failed_again =
                match found.ending with
                | Failure.Timed_out _ -> None
                | Converged | Budget_spent -> run_again found.again
              in
              fail ~case_index:index ~examples:false ?record ?summary ~rendering
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
  (* A domain of at most [count] values is checked whole, in place of the
     generated cases: every value runs, every value refuted is reported, as
     it was drawn, and none is shrunk. *)
  let whole values =
    let domain = List.length values in
    (* A domain checked in part is not checked whole. *)
    let only = Option.value only ~default:(Fun.const true) in
    if List.for_all only (List.init domain Fun.id) then ctx.whole <- Some domain;
    let refuted ?also (index, choices, node, (fault, record), tail) =
      let node = Option.value ~default:node (parse_again gen choices) in
      let token = Choice.token choices ~digest:(digest (rendered_text node)) in
      let rendered, rendering = rendered ?record node in
      let summary = Option.bind record (fun record -> record.summary) in
      let failure =
        Failure.property ?loc ~inner:(inner_failure fault) ?count:config_count
          ?summary ~rendered ~case_index:index ~shrink_steps:0 ~root
          ~examples:false ~rendering ~token ~domain ?also ()
      in
      match tail with
      | Some tail -> Failure.with_output_tail tail failure
      | None -> failure
    in
    let rec check_all index found = function
      | [] -> (
          match List.rev found with
          | [] -> judged ()
          | first :: rest ->
              let also = List.map refuted rest in
              Fail { failure = refuted ~also first; stats = stats ctx })
      | _ :: rest when not (only index) -> check_all (index + 1) found rest
      | (choices, node) :: rest -> (
          match check law (Gen.Engine.value node) with
          | `Passed | `Discarded -> check_all (index + 1) found rest
          | `Timed_out limit ->
              timed_out ~case_index:index ~examples:false limit
          | `Failed failed ->
              let tail = output_after_failure () in
              let found = (index, choices, node, failed, tail) :: found in
              check_all (index + 1) found rest)
    in
    check_all 0 [] values
  in
  let generated () =
    let finite () = if count = 0 then None else enumerate gen count in
    match Failure.catch finite with
    | Ok (Some values) -> whole values
    | Ok None -> generate ~passed:0 0
    | Error (`Timeout limit) -> timed_out ~case_index:0 ~examples:false limit
    | Error c -> Failure.reraise c
  in
  let rec run_examples index = function
    | [] -> generated ()
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
        | `Failed (fault, record) -> (
            let tail = output_after_failure () in
            (* [render_value] makes a fault of the printer its text, so the
               example that failed was rendered. *)
            match (record, !rendered) with
            | Some record, _ ->
                fail ~case_index:index ~examples:true ~record ?tail
                  ~rendered:record.text fault
            | None, Some rendered ->
                fail ~case_index:index ~examples:true ?summary:!summarized ?tail
                  ~rendered fault
            | None, None -> assert false))
  in
  (* A replayed case is neither seeded nor shrunk, as an example, and keeps
     its token. A token that its generator no longer parses to the value it
     recorded is refused as stale: it never checks another value. *)
  let replayed token =
    let refused why =
      let message = Pp.str "%s %s: %s" replayed_by token why in
      Fail { failure = Failure.message ?loc message; stats = stats ctx }
    in
    match Choice.of_token token with
    | Error why -> refused why
    | Ok (choices, digested) -> (
        let src = Choice.replaying choices in
        match Failure.catch (fun () -> Gen.Engine.parse gen src) with
        | Error (`Timeout limit) -> timed_out ~case_index:0 ~examples:true limit
        | Error (`Discard | #Failure.fault) ->
            refused "stale: its generator no longer parses a value from it"
        | Error (#Failure.control as control) -> Failure.reraise control
        | Ok node -> (
            let text = rendered_text node in
            if choices <> Choice.choices src || digested <> digest text then
              refused
                (Pp.str
                   "stale: its generator now parses %s, not the value it \
                    recorded"
                   text)
            else
              match check law (Gen.Engine.value node) with
              | `Passed -> Pass (stats ctx)
              | `Discarded -> refused "the law discarded its case"
              | `Timed_out limit -> timed_out ~case_index:0 ~examples:true limit
              | `Failed (fault, record) ->
                  let tail = output_after_failure () in
                  let summary = summary (Gen.Engine.value node) in
                  let rendered, rendering = rendered ?record node in
                  fail ~case_index:0 ~examples:true ?record ?summary ~rendering
                    ?tail ~token ~rendered fault))
  in
  (* A case from bytes is any value the generator parses: no token to go
     stale, no seed and no search. Its timeout belongs to no drawn case, and
     a case outside the law's domain, discarded, checks nothing, so it skips
     rather than pass or fail. *)
  let from_bytes (name, choices) =
    let timed_out limit =
      Fail { failure = Failure.timeout ?loc limit; stats = stats ctx }
    in
    let discarded () =
      Failure.reraise
        (`Skip
           (Some (name ^ ": the case was discarded, so nothing was checked")))
    in
    let src = Choice.replaying choices in
    match Failure.catch (fun () -> Gen.Engine.parse gen src) with
    | Error (`Timeout limit) -> timed_out limit
    | Error `Discard -> discarded ()
    | Error (#Failure.fault as fault) ->
        fail ~case_index:0 ~examples:true
          ~rendered:"<generator raised before producing a value>" fault
    | Error (#Failure.control as control) -> Failure.reraise control
    | Ok node -> (
        match check law (Gen.Engine.value node) with
        | `Passed -> Pass (stats ctx)
        | `Discarded -> discarded ()
        | `Timed_out limit -> timed_out limit
        | `Failed (fault, record) ->
            let tail = output_after_failure () in
            let summary = summary (Gen.Engine.value node) in
            let rendered, rendering = rendered ?record node in
            let token =
              Choice.token (Choice.choices src)
                ~digest:(digest (rendered_text node))
            in
            fail ~case_index:0 ~examples:true ?record ?summary ~rendering ?tail
              ~token ~rendered fault)
  in
  match (case, replay) with
  | Some case, _ -> from_bytes case
  | None, Some token -> replayed token
  | None, None -> run_examples 0 examples
