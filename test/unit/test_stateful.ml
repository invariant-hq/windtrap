(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Stateful: the command type, the program generator and its
   repair, the executor, and the program printer. *)

open Windtrap
open Windtrap.Private
module Tag = Test_tree.Tag
module Shrink_tree = Gen_engine.Shrink_tree

let contains needle haystack = Text.contains_substring ~pattern:needle haystack
let show_names names = "[" ^ String.concat "; " names ^ "]"
let show_ints values = show_names (List.map string_of_int values)

(* One fixed root for the whole suite; per-test streams come from indexes.
   Everything below is deterministic across runs and machines. *)
let root = 0x00c0ffee1234abcdL
let state index = Seed.make (Seed.derive ~root ~path:"test_stateful" ~index)
let root_value tree = Gen_engine.value (Shrink_tree.root tree)
let program_at gen index = root_value (Gen_engine.sample gen (state index))

(* [~scope] over a system that needs no acquisition at all: what most of
   the tests below exercise is the program, not the resource. *)
let unit_scope run = run ()

(* The program type is abstract, and the counterexample a user reads is the
   printer's, so the tests read a program back the same way: the header row
   and then one [N  [<cell>]  name [arg]] row per step, the step's text
   starting at the header's [call] column. *)
let render gen program = Gen_engine.render_value gen program
let lines_of gen program = Text.split_lines (render gen program)

let names gen program =
  match lines_of gen program with
  | [] | [ _ ] -> []
  | header :: rows ->
      let start =
        match Text.first_occurrence ~pattern:"call" header with
        | Some start -> start
        | None -> failf "the header row names no call column: %S" header
      in
      List.mapi
        (fun index row ->
          if
            not
              (String.starts_with
                 ~prefix:(string_of_int (index + 1) ^ "  ")
                 (String.trim row))
          then
            failf "row %d of the program does not carry its number: %S"
              (index + 1) row;
          let step = String.sub row start (String.length row - start) in
          match String.index_opt step ' ' with
          | Some space -> String.sub step 0 space
          | None -> step)
        rows

let summary_of program =
  match Stateful.summary program with
  | Some summary -> summary
  | None -> failf "the program has no summary"

let names_at gen index = names gen (program_at gen index)

(* The one placeholder a value with no printer renders as. *)
let placeholder = "<no printer: attach one with Gen.with_pp>"

let expect_check_failure what fn =
  match fn () with
  | () -> failf "%s: expected a Check_failure, nothing was raised" what
  | exception Failure.Check_failure failure -> failure

let failure_msg (failure : Failure.t) =
  match failure.Failure.msg with
  | Some msg -> msg.Failure.kept
  | None -> failf "the failure carries no ~msg"

let raised_actual (failure : Failure.t) =
  match failure.Failure.kind with
  | Failure.Raise { actual = Some actual; _ } -> actual.Failure.kept
  | _ -> failf "expected a Raise failure kind with an actual side"

let expect_fail = function
  | Property.Fail { failure; stats } -> (failure, stats)
  | Property.Pass _ -> failf "expected Fail, got Pass"
  | Property.Coverage_failed _ -> failf "expected Fail, got Coverage_failed"
  | Property.Gave_up _ -> failf "expected Fail, got Gave_up"

let expect_pass = function
  | Property.Pass stats -> stats
  | Property.Fail _ -> failf "expected Pass, got Fail"
  | Property.Coverage_failed _ -> failf "expected Pass, got Coverage_failed"
  | Property.Gave_up _ -> failf "expected Pass, got Gave_up"

let property_payload (failure : Failure.t) =
  match failure.Failure.kind with
  | Failure.Property { rendered; case_index; shrink_steps; rendering; inner; _ }
    ->
      (rendered.Failure.kept, case_index, shrink_steps, rendering, inner)
  | _ -> failf "expected a Property failure kind"

let failure_block failure =
  let buf = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buf in
  Report.pp_failure ~ansi:false ppf failure;
  Format.pp_print_flush ppf ();
  Buffer.contents buf

(* The specs

   A bounded counter, floored at zero and capped at [counter_cap]. Both
   commands are nullary, so a drawn program is exactly a name sequence and
   repair's verdict on it is recomputable in the test. *)

let counter_cap = 3

let counter_commands =
  [
    Stateful.call "inc"
      ~pre:(fun model -> model < counter_cap)
      ~next:(fun model -> model + 1)
      (fun _ () -> ());
    Stateful.call "dec"
      ~pre:(fun model -> model > 0)
      ~next:(fun model -> model - 1)
      (fun _ () -> ());
  ]

(* The same two commands with no precondition, in the same order over the
   same (nullary) argument generator: [~pre] is bound into a call after its
   argument is drawn, so the same seed draws the same name sequence, the
   drawn program repair is folded over. *)
let counter_draws =
  [
    Stateful.call "inc" ~next:(fun model -> model + 1) (fun _ () -> ());
    Stateful.call "dec" ~next:(fun model -> model - 1) (fun _ () -> ());
  ]

let counter_step model = function
  | "inc" -> model + 1
  | "dec" -> model - 1
  | name -> failf "unknown command %S" name

(* Repair's own rule, spelled independently: keep a call iff its [~pre] holds
   in the model the calls before it produced, and thread [~next] through the
   kept ones only. *)
let counter_repair drawn =
  let rec go model = function
    | [] -> []
    | "inc" :: rest when model < counter_cap -> "inc" :: go (model + 1) rest
    | "dec" :: rest when model > 0 -> "dec" :: go (model - 1) rest
    | ("inc" | "dec") :: rest -> go model rest
    | name :: _ -> failf "unknown command %S" name
  in
  go 0 drawn

(* Five distinct nullary commands, and the same five with no precondition.
   The name space is wide enough that a candidate substituting one command
   for another shows up as a call sequence that is not a subsequence of the
   drawn one; with two names, coincidence hides it. *)
let wide_names = [ "alpha"; "bravo"; "charlie"; "delta"; "echo" ]

let wide_commands ~pre =
  List.mapi
    (fun index name ->
      Stateful.call name
        ?pre:(if pre then Some (fun model -> model mod 5 <> index) else None)
        ~next:(fun model -> model + index + 1)
        (fun _ () -> ()))
    wide_names

let never_commands =
  [ Stateful.call "never" ~pre:(fun _ -> false) ~next:Fun.id (fun _ () -> ()) ]

let tick_commands =
  [ Stateful.call "tick" ~next:(fun model -> model + 1) (fun _ () -> ()) ]

(* A set model with an argument-dependent precondition: legality is not a
   function of the command name, so the check has to be the body's own. It
   sees the pre-state and re-derives its own precondition there. *)
module Ints = Set.Make (Int)

let set_commands illegal =
  let note name legal model arg =
    if not (legal model arg) then illegal := Pp.str "%s %d" name arg :: !illegal
  in
  let absent model arg = not (Ints.mem arg model) in
  let present model arg = Ints.mem arg model in
  [
    Stateful.command "add" (Gen.int_range 0 4) ~pre:absent
      ~next:(fun model arg -> Ints.add arg model)
      (fun model arg () -> note "add" absent model arg);
    Stateful.command "remove" (Gen.int_range 0 4) ~pre:present
      ~next:(fun model arg -> Ints.remove arg model)
      (fun model arg () -> note "remove" present model arg);
  ]

(* A queue whose [pop] returns the newest element instead of the oldest:
   right for one element, wrong from the second on. Deterministic, which is
   what a shrink search needs. *)
module Bad_queue = struct
  type t = { mutable items : int list }

  let create () = { items = [] }
  let push queue item = queue.items <- queue.items @ [ item ]
  let size queue = List.length queue.items

  let pop queue =
    match List.rev queue.items with
    | [] -> invalid_arg "pop: empty"
    | newest :: rest ->
        queue.items <- List.rev rest;
        newest
end

let queue_cap = 4

let queue_commands =
  [
    Stateful.command "push" (Gen.int_range 0 9)
      ~pre:(fun model _ -> List.length model < queue_cap)
      ~next:(fun model item -> model @ [ item ])
      (fun _ item queue -> Bad_queue.push queue item);
    Stateful.call "pop"
      ~pre:(fun model -> model <> [])
      ~next:List.tl
      (fun model queue -> Check.equal int (List.hd model) (Bad_queue.pop queue));
  ]

let queue_gen ?(steps = 8) () =
  Stateful.program ~steps ~model:([] : int list) queue_commands

let queue_invariant model queue =
  Check.equal int (List.length model) (Bad_queue.size queue)

(* Repair *)

(* The root program contains exactly the calls the model-threading fold
   keeps, checked against the drawn program, which the unconditioned twin
   of the same spec draws from the same seed. *)
let repair_keeps_exactly_the_fold_s_calls () =
  let repaired = Stateful.program ~steps:20 ~model:0 counter_commands in
  let drawn = Stateful.program ~steps:20 ~model:0 counter_draws in
  let dropped = ref 0 in
  for index = 0 to 29 do
    let drawn = names_at drawn index in
    is_true
      ~msg:
        (Printf.sprintf "the unconditioned program made %d of 20 calls"
           (List.length drawn))
      (List.length drawn = 20);
    let expected = counter_repair drawn in
    let kept = names_at repaired index in
    is_true
      ~msg:
        (Printf.sprintf "repair of %s kept %s, not %s" (show_names drawn)
           (show_names kept) (show_names expected))
      (kept = expected);
    dropped := !dropped + (20 - List.length kept)
  done;
  is_true ~msg:"the precondition dropped nothing in 30 draws: vacuous"
    (!dropped > 0);
  (* [?steps] is a work budget with a documented default. *)
  let default = names_at (Stateful.program ~model:0 counter_draws) 0 in
  is_true
    ~msg:
      (Printf.sprintf "the default ?steps drew %d calls, not 20"
         (List.length default))
    (List.length default = 20)

(* A state-dependent [~pre] really filters: the trajectory it admits stays
   inside the model's bounds, and a precondition no state satisfies deletes
   its command from every program. *)
let a_state_dependent_precondition_filters () =
  let gen = Stateful.program ~steps:20 ~model:0 counter_commands in
  let shortened = ref 0 in
  for index = 0 to 19 do
    let kept = names_at gen index in
    if List.length kept < 20 then incr shortened;
    ignore
      (List.fold_left
         (fun model name ->
           let model = counter_step model name in
           is_true
             ~msg:
               (Printf.sprintf "the kept calls %s left the model at %d"
                  (show_names kept) model)
             (model >= 0 && model <= counter_cap);
           model)
         0 kept
        : int)
  done;
  is_true ~msg:"no program was shortened in 20 draws: vacuous" (!shortened > 0);
  let never = Stateful.program ~steps:8 ~model:0 never_commands in
  for index = 0 to 4 do
    is_true ~msg:"an unsatisfiable ~pre left calls in the program"
      (names_at never index = [])
  done

(* Every forced node of the shrink tree contains only legal calls. The check
   is each body's own: it re-derives its precondition against the pre-state
   the executor hands it, so an illegal survivor anywhere in the tree is
   recorded. *)
let every_forced_node_holds_only_legal_calls () =
  let illegal = ref [] in
  let gen =
    Stateful.program ~steps:12 ~model:Ints.empty (set_commands illegal)
  in
  let budget = 3_000 in
  let nodes = ref 0 in
  let rec go tree =
    if !nodes >= budget then raise_notrace Exit;
    incr nodes;
    Stateful.execute ~scope:unit_scope (root_value tree);
    Seq.iter go (Shrink_tree.children tree)
  in
  let index = ref 0 in
  (try
     while !nodes < budget do
       go (Gen_engine.sample gen (state !index));
       incr index;
       if !index > 40 then raise_notrace Exit
     done
   with Exit -> ());
  is_true
    ~msg:(Printf.sprintf "only %d nodes were forced" !nodes)
    (!nodes >= budget);
  is_true
    ~msg:
      (Printf.sprintf "%d illegal calls survived repair over %d nodes: %s"
         (List.length !illegal) !nodes
         (show_names (List.filteri (fun i _ -> i < 5) !illegal)))
    (!illegal = [])

(* Root masking *)

(* At depth 1 no candidate equals its parent and none is longer. With
   nullary commands [Gen.unit] is a leaf, so every immediate move is a
   deletion and a name list is the whole program; masking applied on top of
   an assembled tree would delete a call the mask already dropped and hand
   back the parent. *)
let root_candidates_are_strictly_monotone () =
  let gen = Stateful.program ~steps:12 ~model:0 counter_commands in
  let checked = ref 0 in
  for index = 0 to 19 do
    let tree = Gen_engine.sample gen (state index) in
    let parent = names gen (root_value tree) in
    if parent <> [] && List.length parent < 12 then begin
      incr checked;
      Seq.iter
        (fun child ->
          let child = names gen (root_value child) in
          is_true
            ~msg:
              (Printf.sprintf "a candidate of %s equals its parent"
                 (show_names parent))
            (child <> parent);
          is_true
            ~msg:
              (Printf.sprintf "a candidate of %s is longer: %s"
                 (show_names parent) (show_names child))
            (List.length child <= List.length parent))
        (Shrink_tree.children tree)
    end
  done;
  is_true ~msg:"no repaired-and-shortened program in 20 draws: vacuous"
    (!checked > 0)

(* And with generated arguments in play: a dropped call contributes no
   subtree, so no reduction anywhere can restore it and no candidate of the
   root is longer than the root. *)
let root_candidates_of_an_argument_spec_are_no_longer () =
  let illegal = ref [] in
  let gen =
    Stateful.program ~steps:10 ~model:Ints.empty (set_commands illegal)
  in
  let checked = ref 0 in
  for index = 0 to 19 do
    let tree = Gen_engine.sample gen (state index) in
    let parent = List.length (names gen (root_value tree)) in
    if parent < 10 then incr checked;
    Seq.iter
      (fun child ->
        let child = List.length (names gen (root_value child)) in
        is_true
          ~msg:
            (Printf.sprintf "a candidate of a %d-call program made %d calls"
               parent child)
          (child <= parent))
      (Shrink_tree.children tree)
  done;
  is_true ~msg:"the mask dropped nothing in 20 draws: vacuous" (!checked > 0)

let rec is_subsequence sub whole =
  match (sub, whole) with
  | [], _ -> true
  | _, [] -> false
  | x :: sub', y :: whole' ->
      if x = y then is_subsequence sub' whole' else is_subsequence sub whole'

(* Deeper than depth 1 the monotonicity weakens by design (a candidate can
   repeat its parent or re-legalise a call its parent dropped), but the
   vocabulary does not: shrinking never substitutes one command for another
   and never invents one, so every node's calls are a subsequence of the
   calls the program was drawn from. *)
let no_node_invents_or_substitutes_a_call () =
  let repaired =
    Stateful.program ~steps:14 ~model:0 (wide_commands ~pre:true)
  in
  let drawn = Stateful.program ~steps:14 ~model:0 (wide_commands ~pre:false) in
  let nodes = ref 0 in
  for index = 0 to 4 do
    let drawn = names_at drawn index in
    let budget = !nodes + 400 in
    let rec go tree =
      if !nodes >= budget then raise_notrace Exit;
      incr nodes;
      let kept = names repaired (root_value tree) in
      is_true
        ~msg:
          (Printf.sprintf
             "a node's calls %s are not a subsequence of the drawn %s"
             (show_names kept) (show_names drawn))
        (is_subsequence kept drawn);
      Seq.iter go (Shrink_tree.children tree)
    in
    try go (Gen_engine.sample repaired (state index)) with Exit -> ()
  done;
  is_true
    ~msg:(Printf.sprintf "only %d nodes were forced" !nodes)
    (!nodes >= 2_000)

(* One weight-1 branch per command: the command list is a list, not a
   priority, so no command is starved and none crowds the others out. *)
let every_command_is_drawn_about_equally_often () =
  let gen = Stateful.program ~steps:20 ~model:0 (wide_commands ~pre:false) in
  let counts = List.map (fun name -> (name, ref 0)) wide_names in
  let total = ref 0 in
  for index = 0 to 19 do
    List.iter
      (fun name ->
        incr total;
        match List.assoc_opt name counts with
        | Some count -> incr count
        | None -> failf "the program made an undeclared call %S" name)
      (names_at gen index)
  done;
  is_true
    ~msg:(Printf.sprintf "20 unconditioned draws of 20 made %d calls" !total)
    (!total = 400);
  let uniform = float_of_int !total /. float_of_int (List.length wide_names) in
  List.iter
    (fun (name, count) ->
      let drawn = float_of_int !count in
      is_true
        ~msg:
          (Printf.sprintf
             "%s was drawn %.0f times of %d, nowhere near the uniform %.0f" name
             drawn !total uniform)
        (drawn >= uniform /. 2. && drawn <= uniform *. 2.))
    counts

let control_spec exn phase =
  [
    Stateful.call "raiser"
      ?pre:
        (match phase with `Pre -> Some (fun _ -> raise exn) | `Next -> None)
      ~next:(fun model -> match phase with `Next -> raise exn | `Pre -> model)
      (fun _ () -> ());
  ]

(* A [~pre] or [~next] that raises is a specification bug, and it is
   reported as one: the exception escapes the generator (so the engine
   fails the case where it was drawn, unshrunk, with the backtrace), wrapped
   to name the operation, the step and the function that raised. *)
exception Pre_boom
exception Next_boom

let raising_spec phase =
  [
    Stateful.call "benign" ~next:(fun model -> model + 1) (fun _ () -> ());
    Stateful.call "boom"
      ?pre:
        (match phase with
        | `Pre -> Some (fun _ -> raise Pre_boom)
        | `Next -> None)
      ~next:(fun model ->
        match phase with `Next -> raise Next_boom | `Pre -> model)
      (fun _ () -> ());
  ]

let a_raising_pre_or_next_is_a_specification_bug () =
  List.iter
    (fun (phase, spelling, needle) ->
      let gen = Stateful.program ~steps:6 ~model:0 (raising_spec phase) in
      let rec first_raise index =
        if index >= 50 then failf "no program drew boom within 50 samples"
        else
          match Gen_engine.sample gen (state index) with
          | exception raised -> Printexc.to_string raised
          | _ -> first_raise (index + 1)
      in
      let message = first_raise 0 in
      List.iter
        (fun part ->
          is_true
            ~msg:
              (Printf.sprintf "%s's message %S lacks %S" spelling message part)
            (contains part message))
        [ "call "; "boom, " ^ spelling ^ " raised"; needle ];
      (* Through the engine: the failing case is the drawn one, unshrunk,
         and the inner failure is the wrapped exception. *)
      let outcome =
        Property.run ~count:(`Declared 50) ~root ~path:("raising " ^ spelling)
          gen (fun _ program -> Stateful.execute ~scope:unit_scope program)
      in
      let failure, _ = expect_fail outcome in
      let rendered, _, shrink_steps, _, inner = property_payload failure in
      is_true
        ~msg:(Printf.sprintf "%s rendered %S" spelling rendered)
        (rendered = "<generator raised before producing a value>");
      is_true
        ~msg:(Printf.sprintf "%s was shrunk %d steps" spelling shrink_steps)
        (shrink_steps = 0);
      match inner with
      | Some inner -> (
          let actual = raised_actual inner in
          is_true
            ~msg:(Printf.sprintf "%s's inner failure read %S" spelling actual)
            (contains needle actual && contains (spelling ^ " raised") actual);
          (* The wrapper carries the raise's own backtrace: the frame that
             raised is this file's, not the engine's. *)
          match inner.Failure.kind with
          | Failure.Raise
              { backtrace = Some { Failure.kept = backtrace; _ }; _ } ->
              is_true
                ~msg:
                  (Printf.sprintf
                     "%s's backtrace does not name the raising frame:\n%s"
                     spelling backtrace)
                (contains "test_stateful.ml" backtrace)
          | _ -> failf "%s's inner failure carries no backtrace" spelling)
      | None -> failf "%s reported no inner failure" spelling)
    [ (`Pre, "~pre", "Pre_boom"); (`Next, "~next", "Next_boom") ]

(* A [~pre] that raises on a shrink candidate but not on the drawn program
   raises while the candidate is forced, where the engine stops the search
   rather than lose the failure in hand: the report is the drawn program's
   own failure, marked as possibly not minimal, and the specification bug
   surfaces on a case that draws it. *)
exception Candidate_boom

let a_specification_bug_met_while_shrinking_stops_the_search () =
  let commands =
    [
      Stateful.call "inc" ~next:(fun model -> model + 1) (fun _ () -> ());
      Stateful.call "check"
        ~pre:(fun model -> if model = 0 then raise Candidate_boom else true)
        ~next:Fun.id
        (fun _ () -> Check.fail "the body");
    ]
  in
  let gen = Stateful.program ~steps:6 ~model:0 commands in
  (* A path whose first case draws [inc] before any [check]: that program
     repairs cleanly and its body fails at the first [check], and every
     candidate that deletes the leading [inc] raises in [~pre]. *)
  let rec clean index =
    if index >= 50 then failf "no clean program with a check within 50 paths"
    else
      let path = "candidate " ^ string_of_int index in
      let seed = Seed.make (Seed.derive ~root ~path ~index:0) in
      match names gen (root_value (Gen_engine.sample gen seed)) with
      | "inc" :: rest when List.mem "check" rest -> path
      | _ -> clean (index + 1)
      | exception _ -> clean (index + 1)
  in
  let path = clean 0 in
  let outcome =
    Property.run ~count:(`Declared 1) ~root ~path gen (fun _ program ->
        Stateful.execute ~scope:unit_scope program)
  in
  let failure, _ = expect_fail outcome in
  match failure.Failure.kind with
  | Failure.Property { rendered = { kept = rendered; _ }; shrink_end; inner; _ }
    -> (
      is_true ~msg:"the search did not name the ~pre that stopped it"
        (match shrink_end with
        | Failure.Candidate_raised { kept = text; _ } ->
            contains "~pre raised" text && contains "Candidate_boom" text
        | _ -> false);
      is_true
        ~msg:
          (Printf.sprintf "the drawn program's failure was replaced: %S"
             rendered)
        (contains "check" rendered);
      match inner with
      | Some { Failure.kind = Failure.Message { kept = "the body"; _ }; _ } ->
          ()
      | _ -> failf "the inner failure is not the body's")
  | _ -> failf "expected a Property failure kind"

(* The exceptions about the run or the case rather than the model escape
   [~pre] and [~next] as themselves: a discard there discards the case. *)
let control_exceptions_escape_pre_and_next_unconverted () =
  let cases =
    [
      ("Skip", Failure.Control (`Skip (Some "why")));
      ("Timeout", Failure.Control (`Timeout 0.5));
      ("Exit_attempt", Failure.Control `Exit);
      ("Discard", Failure.Control `Discard);
      ("Sys.Break", Sys.Break);
    ]
  in
  List.iter
    (fun (label, exn) ->
      List.iter
        (fun (phase, spelling) ->
          let gen =
            Stateful.program ~steps:4 ~model:0 (control_spec exn phase)
          in
          match Gen_engine.sample gen (state 0) with
          | exception raised ->
              is_true
                ~msg:
                  (Printf.sprintf "%s from %s came back as %s" label spelling
                     (Printexc.to_string raised))
                (raised = exn)
          | _ -> failf "%s from %s was swallowed" label spelling)
        [ (`Pre, "~pre"); (`Next, "~next") ])
    cases

(* The failing step points at the command. A body is idiomatically one
   assertion in tail position, and under the runner's [Loc.delimit] barrier
   nothing is capturable when it raises, so the failure arrives with no
   location and the command's own site fills it. A body that did record a
   site keeps it, being nearer the failure. Both halves raise the payload
   directly rather than through [Check], whose capture succeeds outside a
   run and would hide the case this exists to pin. *)
let a_failing_step_points_at_its_command () =
  let site = ("declared.ml", 42, 7, 11) in
  let executed ?loc () =
    let spec =
      [
        Stateful.call ~__POS__:site "boom" ~next:Fun.id (fun _ () ->
            raise
              (Failure.Check_failure
                 (Failure.equality ?loc ~expected:"1" ~actual:"2" ())));
      ]
    in
    let program =
      root_value
        (Gen_engine.sample (Stateful.program ~steps:1 ~model:0 spec) (state 0))
    in
    expect_check_failure "a located step" (fun () ->
        Stateful.execute ~scope:unit_scope program)
  in
  (match (executed ()).Failure.loc with
  | Some loc ->
      is_true
        ~msg:(Printf.sprintf "the step was located at %s" (Loc.to_string loc))
        (loc.Loc.file = "declared.ml" && loc.Loc.line = 42)
  | None -> failf "a locationless step reported no location");
  let own = Loc.of_pos ("body.ml", 9, 0, 4) in
  match (executed ~loc:own ()).Failure.loc with
  | Some loc ->
      is_true
        ~msg:
          (Printf.sprintf "the body's own site was overwritten by %s"
             (Loc.to_string loc))
        (loc.Loc.file = "body.ml")
  | None -> failf "the body-located step reported no location"

(* An assertion in [~pre] is the model being written wrong, so it is
   reported as a specification bug naming the operation and the step, not as
   what the assertion says. *)
let assertions_from_pre_are_specification_bugs () =
  let cases =
    [
      ( "Check_failure",
        Failure.Check_failure (Failure.equality ~expected:"1" ~actual:"2" ()),
        "windtrap assertion failure" );
    ]
  in
  List.iter
    (fun (label, exn, needle) ->
      let gen = Stateful.program ~steps:4 ~model:0 (control_spec exn `Pre) in
      match Gen_engine.sample gen (state 0) with
      | exception raised ->
          let message = Printexc.to_string raised in
          is_true
            ~msg:(Printf.sprintf "%s escaped ~pre as itself" label)
            (message <> Printexc.to_string exn);
          List.iter
            (fun part ->
              is_true
                ~msg:
                  (Printf.sprintf "%s from ~pre read %S, lacking %S" label
                     message part)
                (contains part message))
            [ "call 1: raiser, ~pre raised"; needle ]
      | _ -> failf "%s from ~pre was swallowed" label)
    cases

(* The same partition inside a body, where the executor rather than repair
   decides. A [Check_failure] keeps its class and gains the step label; the
   four control exceptions and the fatal one come back untouched; anything
   else is narrowed into the assertion class. *)
let control_exceptions_escape_a_body_unconverted () =
  let raising exn =
    [
      Stateful.call "boom"
        ~next:(fun model -> model + 1)
        (fun _ () -> raise exn);
    ]
  in
  let program_of exn =
    let gen = Stateful.program ~steps:1 ~model:0 (raising exn) in
    program_at gen 0
  in
  List.iter
    (fun (label, exn) ->
      match Stateful.execute ~scope:unit_scope (program_of exn) with
      | exception raised ->
          is_true
            ~msg:
              (Printf.sprintf "%s from a body came back as %s" label
                 (Printexc.to_string raised))
            (raised = exn)
      | () -> failf "%s from a body was swallowed" label)
    [
      ("Skip_test", Failure.Control (`Skip (Some "why")));
      ("Timeout", Failure.Control (`Timeout 0.5));
      ("Exit_attempt", Failure.Control `Exit);
      ("Discard", Failure.Control `Discard);
      ("Sys.Break", Sys.Break);
    ];
  (* A Check_failure is already the class the narrowing aims at: it keeps
     its payload and gains the step label, joined onto the user's ~msg,
     which is flattened first, since the slot renders as one line. *)
  let asserted =
    expect_check_failure "an asserting body" (fun () ->
        Stateful.execute ~scope:unit_scope
          (program_of
             (Failure.Check_failure
                {
                  (Failure.message "nope") with
                  Failure.msg = Some (Failure.text "note\nand more");
                })))
  in
  is_true
    ~msg:
      (Printf.sprintf "an assertion failure was labelled %S"
         (failure_msg asserted))
    (failure_msg asserted = "call 1 of 1: boom; note and more");
  is_true ~msg:"an assertion failure lost its payload"
    (asserted.Failure.kind = Failure.Message (Failure.text "nope"));
  (* And anything else is narrowed, under the same label. *)
  let narrowed =
    expect_check_failure "a raising body" (fun () ->
        Stateful.execute ~scope:unit_scope (program_of Not_found))
  in
  is_true
    ~msg:
      (Printf.sprintf "a narrowed exception was labelled %S"
         (failure_msg narrowed))
    (failure_msg narrowed = "call 1 of 1: boom");
  is_true
    ~msg:
      (Printf.sprintf "a narrowed exception rendered %S"
         (raised_actual narrowed))
    (raised_actual narrowed = "Not_found")

(* The executor *)

let counter_program ?(steps = 4) index =
  program_at (Stateful.program ~steps ~model:0 counter_draws) index

(* The printer is the same whatever the steps, so one generator reads every
   counter program back. *)
let counter_names program =
  names (Stateful.program ~model:0 counter_draws) program

let one_call_program exn =
  program_at
    (Stateful.program ~steps:1 ~model:0
       [
         Stateful.call "boom"
           ~next:(fun model -> model + 1)
           (fun _ () -> raise exn);
       ])
    0

(* The scope owns release, so a [Fun.protect] inside it fires on every
   path [execute] leaves, and on none it does not: a scope that raises
   while acquiring never reached its own release. *)
let a_scope_releases_on_every_path () =
  let paths =
    [
      ("pass", counter_program 0);
      ( "body failure",
        one_call_program (Failure.Check_failure (Failure.message "nope")) );
      ("skip", one_call_program (Failure.Control (`Skip (Some "why"))));
      ("timeout", one_call_program (Failure.Control (`Timeout 0.5)));
      ("uncaught", one_call_program Not_found);
    ]
  in
  List.iter
    (fun (label, program) ->
      let released = ref 0 in
      (try
         Stateful.execute
           ~scope:(fun run ->
             Fun.protect ~finally:(fun () -> incr released) (fun () -> run ()))
           program
       with _ -> ());
      is_true
        ~msg:(Printf.sprintf "the %s path released %d times" label !released)
        (!released = 1))
    paths;
  let released = ref 0 in
  let failing_acquisition run =
    let sut = raise Not_found in
    Fun.protect ~finally:(fun () -> incr released) (fun () -> run sut)
  in
  (match Stateful.execute ~scope:failing_acquisition (counter_program 0) with
  | exception Not_found -> ()
  | exception exn ->
      failf "a scope that raised while acquiring came back as %s"
        (Printexc.to_string exn)
  | () -> failf "a scope that raised while acquiring was swallowed");
  is_true
    ~msg:
      (Printf.sprintf "a scope that acquired nothing released %d times"
         !released)
    (!released = 0)

(* What the scope raises after the program returns is a release that
   failed. On the failing path it is dropped: the reported failure is the
   program's. On the passing path there is no failure to outrank, so the
   release's is the failure. *)
let a_release_failure_never_replaces_the_program_s () =
  let failing =
    one_call_program (Failure.Check_failure (Failure.message "the body"))
  in
  (* A scope whose release raises on both paths. Hand-rolled rather than
     [Fun.protect] with a raising [~finally], which would deliver
     [Fun.Finally_raised] in place of the program's failure, the caveat
     a scope author owns. *)
  let releasing exn run =
    match run () with () -> raise exn | exception _ -> raise exn
  in
  let failure =
    expect_check_failure "a program failure under a raising release" (fun () ->
        Stateful.execute ~scope:(releasing Not_found) failing)
  in
  is_true ~msg:"the release's exception replaced the program's failure"
    (failure.Failure.kind = Failure.Message (Failure.text "the body"));
  (* Passing path: the release's exception is the only one there is, and it
     propagates as itself; [execute] converts nothing outside a step. *)
  (match Stateful.execute ~scope:(releasing Not_found) (counter_program 0) with
  | exception Not_found -> ()
  | exception exn ->
      failf "a passing-path release raised %s" (Printexc.to_string exn)
  | () -> failf "a passing-path release failure was swallowed");
  (* Except for a control and a fatal exception, which are about the run or
     the case: a timeout delivered in a candidate's release outranks the
     failure in hand, or the engine would accept it as a shrink step and
     report a converged counterexample. A timeout that cuts a [Fun.protect]
     release is unwrapped and outranks it too. *)
  List.iter
    (fun (label, exn, scope) ->
      match Stateful.execute ~scope failing with
      | exception raised when raised = exn -> ()
      | exception Failure.Check_failure _ ->
          failf "a %s from a failing path's release was dropped" label
      | exception raised ->
          failf "the release's %s came back as %s" label
            (Printexc.to_string raised)
      | () -> failf "the failing program did not fail")
    (List.map
       (fun (label, exn) -> (label, exn, releasing exn))
       [
         ("Skip", Failure.Control (`Skip None));
         ("Timeout", Failure.Control (`Timeout 0.5));
         ("Exit_attempt", Failure.Control `Exit);
         ("Discard", Failure.Control `Discard);
         ("Sys.Break", Sys.Break);
       ]
    @ [
        ( "Timeout in a finally",
          Failure.Control (`Timeout 0.5),
          fun run ->
            Fun.protect
              ~finally:(fun () -> raise (Failure.Control (`Timeout 0.5)))
              run );
      ])

(* A scope that returns without running the program fails the case rather
   than passing it: a program that never ran is not a passing program. *)
let a_scope_that_never_runs_the_program_fails_the_case () =
  let failure =
    expect_check_failure "a scope that never called back" (fun () ->
        Stateful.execute ~scope:(fun _ -> ()) (counter_program 0))
  in
  is_true
    ~msg:
      (Printf.sprintf "a scope that never called back failed with %S"
         (Printexc.to_string (Failure.Check_failure failure)))
    (failure.Failure.kind
    = Failure.Message
        (Failure.text
           "the scope returned without running the program; a scope must call \
            its callback exactly once"));
  (* It carries the declaration site: it is the one failure with no
     assertion of its own to be located by. *)
  let loc = { Loc.file = "spec.ml"; line = 42; column = 7 } in
  let located =
    expect_check_failure "a located missing body" (fun () ->
        Stateful.execute ~loc ~scope:(fun _ -> ()) (counter_program 0))
  in
  is_true ~msg:"the missing-program failure lost the declaration site"
    (located.Failure.loc = Some loc);
  (* A scope that raises or skips instead of calling back has already said
     what happened, and says it rather than this. *)
  (match
     Stateful.execute ~scope:(fun _ -> raise Not_found) (counter_program 0)
   with
  | exception Not_found -> ()
  | exception exn ->
      failf "a scope that raised instead of calling back reported %s"
        (Printexc.to_string exn)
  | () -> failf "a scope that raised instead of calling back was swallowed");
  (* And through the engine, where failing the case is what the reader
     meets: it is in the assertion class, so the search converges on the
     empty program and the message is the counterexample's inner failure. *)
  let outcome =
    Property.run ~count:(`Declared 4) ~root ~path:"no-body" (queue_gen ())
      (fun _ program -> Stateful.execute ~scope:(fun _ -> ()) program)
  in
  let reported, _ = expect_fail outcome in
  let rendered, _, _, _, inner = property_payload reported in
  is_true
    ~msg:
      (Printf.sprintf "a scope that never called back converged on %S" rendered)
    (rendered = "(no commands)");
  is_true
    ~msg:"the counterexample's inner failure is not the missing-program one"
    (Option.map (fun (inner : Failure.t) -> inner.Failure.kind) inner
    = Some failure.Failure.kind);
  let block = failure_block reported in
  is_true
    ~msg:(Printf.sprintf "the reader is not told what went wrong:\n%s" block)
    (contains "must call its callback exactly once" block)

(* A second call is the harness itself being wrong, not a counterexample:
   [Invalid_argument] at the call, and it outranks whatever else the case
   had to say, including a scope that swallows it, which would otherwise
   report a program that ran twice as a pass. *)
let a_scope_that_runs_the_program_twice_is_invalid () =
  let runs = ref 0 in
  let twice run =
    run ();
    incr runs;
    run ();
    incr runs
  in
  (match Stateful.execute ~scope:twice (counter_program 0) with
  | exception Invalid_argument message ->
      is_true
        ~msg:(Printf.sprintf "the double-call error said %S" message)
        (contains "exactly once" message && contains "stateful" message)
  | exception exn ->
      failf "a scope that called back twice raised %s" (Printexc.to_string exn)
  | () -> failf "a scope that called back twice was accepted");
  is_true
    ~msg:(Printf.sprintf "the program ran under %d of the two calls" !runs)
    (!runs = 1);
  (* Swallowed by the scope, and still fatal to the case. *)
  (match
     Stateful.execute
       ~scope:(fun run ->
         run ();
         try run () with Invalid_argument _ -> ())
       (counter_program 0)
   with
  | exception Invalid_argument _ -> ()
  | exception exn ->
      failf "a swallowed double call came back as %s" (Printexc.to_string exn)
  | () -> failf "a swallowed double call passed the case");
  (* And it outranks the program's own failure: a case whose harness is
     wrong has no counterexample to report. *)
  (match
     Stateful.execute
       ~scope:(fun run ->
         (try run () with Failure.Check_failure _ -> ());
         run ())
       (one_call_program (Failure.Check_failure (Failure.message "the body")))
   with
  | exception Invalid_argument _ -> ()
  | exception exn ->
      failf "a double call after a failing program came back as %s"
        (Printexc.to_string exn)
  | () -> failf "a double call after a failing program was accepted");
  (* And through the engine: the misuse is classified like any exception,
     so the search re-runs the broken scope and converges on the empty
     program, accurately, since a scope that calls back twice does so
     whatever the program says. The message, not the counterexample, is
     the diagnosis, and the reader must be shown it. *)
  let outcome =
    Property.run ~count:(`Declared 4) ~root ~path:"double-call" (queue_gen ())
      (fun _ program ->
        Stateful.execute
          ~scope:(fun run ->
            run (Bad_queue.create ());
            run (Bad_queue.create ()))
          program)
  in
  let reported, _ = expect_fail outcome in
  let rendered, _, _, _, _ = property_payload reported in
  is_true
    ~msg:
      (Printf.sprintf
         "a double-calling scope converged on %S instead of the empty program"
         rendered)
    (rendered = "(no commands)");
  let block = failure_block reported in
  is_true
    ~msg:
      (Printf.sprintf "the reader is not told the harness is wrong:\n%s" block)
    (contains "called its callback twice" block)

(* Before the callback the scope is acquiring, and what it raises there
   propagates as itself (unconverted and unlabelled) so an assertion is
   an exception-class failure, a skip skips the whole test, and an alarm
   ends the run. *)
let a_scope_that_raises_before_the_callback_propagates_unconverted () =
  List.iter
    (fun (label, exn) ->
      match
        Stateful.execute ~scope:(fun _ -> raise exn) (counter_program 0)
      with
      | exception raised ->
          is_true
            ~msg:
              (Printf.sprintf "%s from an acquiring scope came back as %s" label
                 (Printexc.to_string raised))
            (raised = exn)
      | () -> failf "%s from an acquiring scope was swallowed" label)
    [
      ("Not_found", Not_found);
      ("Check_failure", Failure.Check_failure (Failure.message "nope"));
      ("Skip_test", Failure.Control (`Skip (Some "why")));
      ("Timeout", Failure.Control (`Timeout 0.5));
      ("Exit_attempt", Failure.Control `Exit);
      ("Discard", Failure.Control `Discard);
      ("Sys.Break", Sys.Break);
    ];
  (* And through the engine, where the classification is what it means: a
     scope that skips before acquiring skips the test. *)
  match
    Property.run ~count:(`Declared 4) ~root ~path:"unavailable" (queue_gen ())
      (fun _ program ->
        Stateful.execute
          ~scope:(fun _ -> raise (Failure.Control (`Skip (Some "no server"))))
          program)
  with
  | exception Failure.Control (`Skip (Some "no server")) -> ()
  | exception exn ->
      failf "a skipping scope reached the runner as %s" (Printexc.to_string exn)
  | _ -> failf "a skipping scope did not skip the test"

(* The program's own failure crosses the scope's frames as itself: the
   step label, the payload and the assertion class all survive a scope
   that catches and re-raises, and a scope that swallows it cannot turn a
   failing case green. *)
let a_failing_program_keeps_its_identity_through_the_scope () =
  let failing =
    one_call_program (Failure.Check_failure (Failure.message "the body"))
  in
  let expect what scope =
    let failure =
      expect_check_failure what (fun () -> Stateful.execute ~scope failing)
    in
    is_true
      ~msg:
        (Printf.sprintf "%s reported %S" what
           (Printexc.to_string (Failure.Check_failure failure)))
      (failure.Failure.kind = Failure.Message (Failure.text "the body"));
    is_true
      ~msg:(Printf.sprintf "%s was labelled %S" what (failure_msg failure))
      (failure_msg failure = "call 1 of 1: boom")
  in
  expect "a transparent scope" unit_scope;
  expect "a scope that cleans up and re-raises" (fun run ->
      match run () with
      | () -> ()
      | exception exn ->
          let backtrace = Printexc.get_raw_backtrace () in
          Printexc.raise_with_backtrace exn backtrace);
  expect "a scope that swallows the failure" (fun run ->
      try run () with Failure.Check_failure _ -> ())

(* [scope] runs once per [execute], so once per generated case and once
   per shrink candidate, counted across a real failing run. *)
let the_scope_runs_once_per_case_and_per_shrink_candidate () =
  let scopes = ref 0 and releases = ref 0 and executions = ref 0 in
  let outcome =
    Property.run ~count:(`Declared 40) ~root ~path:"lifecycle" (queue_gen ())
      (fun _ program ->
        incr executions;
        Stateful.execute
          ~scope:(fun run ->
            incr scopes;
            Fun.protect
              ~finally:(fun () -> incr releases)
              (fun () -> run (Bad_queue.create ())))
          program)
  in
  let failure, _ = expect_fail outcome in
  let _, case_index, shrink_steps, _, _ = property_payload failure in
  is_true
    ~msg:(Printf.sprintf "%d scopes for %d executions" !scopes !executions)
    (!scopes = !executions);
  is_true
    ~msg:(Printf.sprintf "%d releases for %d scopes" !releases !scopes)
    (!releases = !scopes);
  is_true ~msg:"the search took no shrink step" (shrink_steps > 0);
  is_true
    ~msg:
      (Printf.sprintf
         "%d executions for a failure at case %d; no candidate got its own \
          system"
         !executions case_index)
    (!executions > case_index + 1)

(* The invariant runs on the fresh system before step 1 (which is what
   makes the empty program a real test) and after every step, under labels
   that tell the two apart. *)
let the_invariant_runs_before_step_one_and_after_every_step () =
  let program = counter_program 0 in
  let drawn = counter_names program in
  let total = List.length drawn in
  is_true
    ~msg:(Printf.sprintf "the unconditioned program made %d of 4 calls" total)
    (total = 4);
  let seen = ref [] in
  Stateful.execute
    ~invariant:(fun model () -> seen := model :: !seen)
    ~scope:unit_scope program;
  let expected =
    List.rev
      (List.fold_left
         (fun acc name -> counter_step (List.hd acc) name :: acc)
         [ 0 ] drawn)
  in
  is_true
    ~msg:
      (Printf.sprintf "the invariant saw %s, not %s"
         (show_ints (List.rev !seen))
         (show_ints expected))
    (List.rev !seen = expected);
  (* The empty program is a real test: the fresh-system check still runs. *)
  let empty =
    program_at (Stateful.program ~steps:5 ~model:0 never_commands) 0
  in
  let ran = ref 0 in
  Stateful.execute ~invariant:(fun _ () -> incr ran) ~scope:unit_scope empty;
  is_true
    ~msg:(Printf.sprintf "the empty program ran the invariant %d times" !ran)
    (!ran = 1);
  (* Distinct labels, and a user ~msg joined onto them. *)
  let fresh =
    expect_check_failure "the fresh-system invariant" (fun () ->
        Stateful.execute
          ~invariant:(fun _ () -> Check.is_true ~msg:"note" false)
          ~scope:unit_scope program)
  in
  is_true
    ~msg:
      (Printf.sprintf "the fresh-system invariant failure was labelled %S"
         (failure_msg fresh))
    (failure_msg fresh = "invariant on the fresh system; note");
  let visits = ref 0 in
  let after =
    expect_check_failure "the post-step invariant" (fun () ->
        Stateful.execute
          ~invariant:(fun _ () ->
            incr visits;
            if !visits = 2 then Check.fail "nope")
          ~scope:unit_scope program)
  in
  is_true
    ~msg:
      (Printf.sprintf "the post-step invariant failure was labelled %S"
         (failure_msg after))
    (failure_msg after
    = Pp.str "invariant after call 1 of %d: %s" total (List.hd drawn))

(* The narrowing and the propagating set are the executor's, not the body's:
   an invariant is held to exactly the same partition, at both of the sites
   it runs from. *)
let an_invariant_is_narrowed_and_propagates_like_a_body () =
  let program = counter_program 0 in
  let drawn = counter_names program in
  let total = List.length drawn in
  is_true
    ~msg:(Printf.sprintf "the unconditioned program made %d of 4 calls" total)
    (total = 4);
  (* Narrowed into the assertion class, under each site's own label. *)
  let fresh =
    expect_check_failure "a raising fresh-system invariant" (fun () ->
        Stateful.execute
          ~invariant:(fun _ () -> raise Not_found)
          ~scope:unit_scope program)
  in
  is_true
    ~msg:
      (Printf.sprintf "a raising fresh-system invariant was labelled %S"
         (failure_msg fresh))
    (failure_msg fresh = "invariant on the fresh system");
  is_true
    ~msg:
      (Printf.sprintf "a raising fresh-system invariant rendered %S"
         (raised_actual fresh))
    (raised_actual fresh = "Not_found");
  let visits = ref 0 in
  let after =
    expect_check_failure "a raising post-step invariant" (fun () ->
        Stateful.execute
          ~invariant:(fun _ () ->
            incr visits;
            if !visits = 2 then raise Not_found)
          ~scope:unit_scope program)
  in
  is_true
    ~msg:
      (Printf.sprintf "a raising post-step invariant was labelled %S"
         (failure_msg after))
    (failure_msg after
    = Pp.str "invariant after call 1 of %d: %s" total (List.hd drawn));
  is_true
    ~msg:
      (Printf.sprintf "a raising post-step invariant rendered %S"
         (raised_actual after))
    (raised_actual after = "Not_found");
  (* And the propagating set escapes an invariant unconverted, from both. *)
  List.iter
    (fun (label, exn) ->
      (match
         Stateful.execute
           ~invariant:(fun _ () -> raise exn)
           ~scope:unit_scope program
       with
      | exception raised ->
          is_true
            ~msg:
              (Printf.sprintf
                 "%s from the fresh-system invariant came back as %s" label
                 (Printexc.to_string raised))
            (raised = exn)
      | () -> failf "%s from the fresh-system invariant was swallowed" label);
      let visits = ref 0 in
      match
        Stateful.execute
          ~invariant:(fun _ () ->
            incr visits;
            if !visits = 2 then raise exn)
          ~scope:unit_scope program
      with
      | exception raised ->
          is_true
            ~msg:
              (Printf.sprintf "%s from a post-step invariant came back as %s"
                 label
                 (Printexc.to_string raised))
            (raised = exn)
      | () -> failf "%s from a post-step invariant was swallowed" label)
    [
      ("Skip_test", Failure.Control (`Skip (Some "why")));
      ("Timeout", Failure.Control (`Timeout 0.5));
      ("Exit_attempt", Failure.Control `Exit);
      ("Discard", Failure.Control `Discard);
      ("Sys.Break", Sys.Break);
    ]

(* The printer *)

let empty_program_prints_no_commands () =
  let gen = Stateful.program ~steps:5 ~model:0 never_commands in
  let program = program_at gen 0 in
  is_true ~msg:"the program was not empty" (names gen program = []);
  is_true
    ~msg:(Printf.sprintf "the empty program rendered %S" (render gen program))
    (render gen program = "(no commands)");
  is_true ~msg:"the empty program, which prints no table, has a summary"
    (Stateful.summary program = None)

(* The table is a header row over the calls, with no model column when the
   model has no printer; a step whose argument renders as ["()"] (every
   [call]) prints as its name alone. The summary is not the table's: it
   rides the failure beside it. *)
let unit_arguments_are_suppressed_under_a_summary_line () =
  let gen = Stateful.program ~steps:3 ~model:0 counter_draws in
  let program = program_at gen 0 in
  let drawn = names gen program in
  let expected =
    " #  call"
    :: List.mapi (fun index name -> Pp.str " %d  %s" (index + 1) name) drawn
  in
  is_true
    ~msg:(Printf.sprintf "a nullary program rendered %S" (render gen program))
    (lines_of gen program = expected);
  is_true
    ~msg:
      (Printf.sprintf "a model without a printer drew a model column: %S"
         (render gen program))
    (not (contains "model" (render gen program)));
  is_true
    ~msg:
      (Printf.sprintf "a three-call program's summary was %S"
         (summary_of program))
    (summary_of program = Pp.str "3 calls, last: %s" (List.nth drawn 2));
  let one = Stateful.program ~steps:1 ~model:0 counter_draws in
  let program = program_at one 0 in
  is_true
    ~msg:
      (Printf.sprintf "a one-call program's summary was %S" (summary_of program))
    (summary_of program
    = Pp.str "1 call, last: %s" (List.hd (names one program)))

(* The model cell is the state each call was made {e in}: a fold of
   [~next] that stops one transition short, so the initial model is visible
   and the last row shows a pre-state too. It sits under [model before],
   between the number and the call, padded to the column. *)
let the_model_column_shows_the_pre_state () =
  let gen =
    Stateful.program ~steps:12 ~model:0 ~pp_model:Format.pp_print_int
      counter_draws
  in
  let program = program_at gen 0 in
  let drawn = names gen program in
  let total = List.length drawn in
  is_true
    ~msg:(Printf.sprintf "the unconditioned program made %d of 12 calls" total)
    (total = 12);
  let cells =
    List.rev
      (snd
         (List.fold_left
            (fun (model, acc) name ->
              (counter_step model name, string_of_int model :: acc))
            (0, []) drawn))
  in
  is_true
    ~msg:
      (Printf.sprintf
         "the model never left one digit; a wide cell is untested: %s"
         (show_names cells))
    (List.exists (fun cell -> String.length cell = 2) cells);
  let expected =
    " #  model before  call"
    :: List.mapi
         (fun index (cell, name) ->
           Pp.str "%2d  %-12s  %s" (index + 1) cell name)
         (List.combine cells drawn)
  in
  is_true
    ~msg:
      (Printf.sprintf "the model column rendered as:\n%s\nnot:\n%s"
         (render gen program)
         (String.concat "\n" expected))
    (lines_of gen program = expected)

(* A [pp_model] that raises costs its own cell and no more: left to
   [Gen_engine.render], it would collapse the whole program to one
   [<printer raised ...>] marker and the reader would lose the program. *)
let a_raising_pp_model_costs_one_cell () =
  let pp_model ppf model =
    if model = 2 then raise Not_found else Format.pp_print_int ppf model
  in
  let gen = Stateful.program ~steps:6 ~model:0 ~pp_model tick_commands in
  let program = program_at gen 0 in
  is_true ~msg:"the tick program lost calls"
    (List.length (names gen program) = 6);
  let marker = "<pp_model raised Not_found>" in
  let cells = [ "0"; "1"; marker; "3"; "4"; "5" ] in
  (* The marker is the widest cell, 27 columns, and sets the column. *)
  let expected =
    " #  model before                 call"
    :: List.mapi
         (fun index cell -> Pp.str " %d  %-27s  tick" (index + 1) cell)
         cells
  in
  is_true
    ~msg:
      (Printf.sprintf "a raising pp_model rendered:\n%s\nnot:\n%s"
         (render gen program)
         (String.concat "\n" expected))
    (lines_of gen program = expected)

(* A model cell is one cell of a row, so it is bounded in code points. *)
let a_long_model_cell_truncates () =
  let pp_model ppf _ = Format.pp_print_string ppf (String.make 100 'm') in
  let gen = Stateful.program ~steps:2 ~model:0 ~pp_model tick_commands in
  let program = program_at gen 0 in
  let expected = String.make 57 'm' ^ "..." in
  is_true
    ~msg:(Printf.sprintf "a long model cell rendered %S" (render gen program))
    (lines_of gen program
    = [
        " #  model before" ^ String.make 48 ' ' ^ "  call";
        " 1  " ^ expected ^ "  tick";
        " 2  " ^ expected ^ "  tick";
      ])

(* An argument whose own generator has no printer renders as the one
   placeholder, remedy included; the call names and the program shape
   survive, and the program itself still prints. *)
let a_printerless_argument_degrades_to_a_placeholder () =
  let commands =
    [
      Stateful.command "opaque" (Gen.constant 5)
        ~next:(fun model _ -> model)
        (fun _ _ () -> ());
    ]
  in
  let gen = Stateful.program ~steps:2 ~model:0 commands in
  let program = program_at gen 0 in
  is_true
    ~msg:
      (Printf.sprintf "a printerless argument rendered %S" (render gen program))
    (lines_of gen program
    = [ " #  call"; " 1  opaque " ^ placeholder; " 2  opaque " ^ placeholder ])

(* An argument rides the failure payload, so it is bounded in bytes, with a
   marker stating the original size. *)
let a_long_argument_truncates () =
  let big = String.make 300 'x' in
  let commands =
    [
      Stateful.command "write"
        (Gen.with_pp Format.pp_print_string (Gen.constant big))
        ~next:(fun model _ -> model)
        (fun _ _ () -> ());
    ]
  in
  let gen = Stateful.program ~steps:2 ~model:0 commands in
  let program = program_at gen 0 in
  let expected = String.make 200 'x' ^ "... (truncated; 300 bytes total)" in
  is_true
    ~msg:(Printf.sprintf "a long argument rendered %S" (render gen program))
    (lines_of gen program
    = [ " #  call"; " 1  write " ^ expected; " 2  write " ^ expected ])

(* Hard newlines only: a name and a model cell are each flattened to one
   line, so a step stays one row. *)
let newlines_in_names_and_cells_are_flattened () =
  let commands = [ Stateful.call "two\nlines" ~next:Fun.id (fun _ () -> ()) ] in
  let pp_model ppf model = Format.fprintf ppf "a\nb%d" model in
  let gen = Stateful.program ~steps:2 ~model:0 ~pp_model commands in
  let program = program_at gen 0 in
  is_true
    ~msg:
      (Printf.sprintf "newlines survived the printer: %S" (render gen program))
    (lines_of gen program
    = [
        " #  model before  call";
        " 1  a b0          two lines";
        " 2  a b0          two lines";
      ]);
  is_true
    ~msg:
      (Printf.sprintf "newlines survived the summary: %S" (summary_of program))
    (summary_of program = "2 calls, last: two lines")

(* A program longer than 40 calls prints its first and last 20 with a
   calls-omitted line between; the number column is as wide as the last
   call's number. *)
let long_programs_truncate_with_a_step_omitted_line () =
  let gen = Stateful.program ~steps:50 ~model:0 counter_draws in
  let program = program_at gen 0 in
  (* The printer omits the middle, so the names come from the executor: the
     model moves by one per call, and the invariant sees every model. *)
  let drawn =
    let seen = ref [] in
    Stateful.execute
      ~invariant:(fun model () -> seen := model :: !seen)
      ~scope:unit_scope program;
    let rec step = function
      | before :: (after :: _ as rest) ->
          (if after > before then "inc" else "dec") :: step rest
      | _ -> []
    in
    step (List.rev !seen)
  in
  is_true
    ~msg:
      (Printf.sprintf "the unconditioned program made %d of 50 calls"
         (List.length drawn))
    (List.length drawn = 50);
  let row index = Pp.str "%2d  %s" (index + 1) (List.nth drawn index) in
  let expected =
    (" #  call" :: List.init 20 row)
    @ [ "\u{2026} (10 calls omitted)" ]
    @ List.init 20 (fun index -> row (30 + index))
  in
  is_true
    ~msg:
      (Printf.sprintf "a 50-step program rendered:\n%s\nnot:\n%s"
         (render gen program)
         (String.concat "\n" expected))
    (lines_of gen program = expected);
  equal ~msg:"the summary counts the calls, not the rows that print" string
    (Pp.str "50 calls, last: %s" (List.nth drawn 49))
    (summary_of program)

(* Malformed arguments are reported at sample time, inside the running
   test's exception boundary. *)
let a_malformed_declaration_raises_at_sample_time () =
  let raises_naming_stateful what gen =
    match Gen_engine.sample gen (state 0) with
    | exception Invalid_argument message ->
        starts_with ~msg:what ~affix:"Windtrap.stateful: " message
    | _ -> failf "%s sampled successfully" what
  in
  raises_naming_stateful "an empty command list"
    (Stateful.program ~steps:4 ~model:0 []);
  (* At [?steps:0] no element is drawn, so the branch-level report never
     fires. A test declaring no commands must not pass vacuously. *)
  raises_naming_stateful "an empty command list at ?steps:0"
    (Stateful.program ~steps:0 ~model:0 []);
  raises_naming_stateful "a negative ?steps"
    (Stateful.program ~steps:(-1) ~model:0 counter_draws)

(* Integration *)

(* Through the facade, whose [command] is abstract: this is the surface a
   user meets. Everything [stateful] hands to the declaration layer is
   visible on the flattened case: the tags [--tag] selects on, the
   per-test limit, and the declaration site. *)
let tick_facade =
  [ Windtrap.call "tick" ~next:(fun model -> model + 1) (fun _ () -> ()) ]

let flattened tree =
  match Test_tree.flatten [ tree ] with
  | [ case ] -> case
  | cases -> failf "expected one flattened case, got %d" (List.length cases)

let stateful_declares_a_prop_node_with_its_tags_timeout_and_site () =
  let pos = ("spec.ml", 42, 0, 7) in
  let case =
    flattened
      (Windtrap.stateful ~__POS__:pos ~tags:[ "custom" ] ~timeout:2.5 "spec"
         ~model:0 ~scope:unit_scope tick_facade)
  in
  let selects tag = Tag.accepts (Tag.require tag Tag.any) case.Test_tree.tags in
  is_true ~msg:"--tag prop did not select a stateful test" (selects "prop");
  is_true ~msg:"--tag stateful did not select a stateful test"
    (selects "stateful");
  is_true ~msg:"the declared tags were dropped" (selects "custom");
  is_true ~msg:"an undeclared tag selected the test" (not (selects "absent"));
  is_true
    ~msg:
      (Printf.sprintf "the case was named %s" (show_names case.Test_tree.path))
    (case.Test_tree.path = [ "spec" ]);
  is_true ~msg:"the declared ?timeout did not reach the test node"
    (case.Test_tree.timeout = Some 2.5);
  is_true ~msg:"the declared ?__POS__ did not reach the test node"
    (case.Test_tree.loc = Some (Loc.of_pos pos))

(* [stateful] is [Run.prop] over [program] with [execute] as its law, and
   the only way to see that wiring is to run the node it declares. The body
   returns on a pass and raises the property's failure otherwise; either is
   an outcome, so the evidence is the lifecycle the run left behind, and any
   other exception the body raises escapes to fail the test. *)
let run_declared_body tree =
  match (flattened tree).Test_tree.body with
  | Test_tree.Scoped _ ->
      failf "the declared node scopes a resource, not a plain test"
  | Test_tree.Body body -> (
      match body () with
      | () -> ()
      | exception Failure.Check_failure { Failure.kind = Failure.Property _; _ }
        ->
          ())

let stateful_runs_one_fresh_system_per_case_over_steps_calls () =
  let scopes = ref 0 and releases = ref 0 in
  let bodies = ref 0 and invariants = ref 0 in
  let commands =
    [
      Windtrap.call "tick"
        ~next:(fun model -> model + 1)
        (fun _ () -> incr bodies);
    ]
  in
  run_declared_body
    (Windtrap.stateful ~count:3 ~steps:3 "wiring" ~model:0
       ~invariant:(fun _ () -> incr invariants)
       ~scope:(fun run ->
         incr scopes;
         Fun.protect ~finally:(fun () -> incr releases) (fun () -> run ()))
       commands);
  (* Every case passes and none discards, so the declared ?count is the
     number of cases, where the engine's default would be 100. *)
  equal ~msg:"the declared ?count of 3 is the number of cases" int 3 !scopes;
  is_true
    ~msg:(Printf.sprintf "%d releases for %d systems" !releases !scopes)
    (!releases = !scopes);
  (* Every call of every case is legal, so ?steps is the program's length:
     three bodies and four invariant checks per system. *)
  is_true
    ~msg:
      (Printf.sprintf
         "%d bodies over %d systems; ?steps:3 did not reach the generator"
         !bodies !scopes)
    (!bodies = 3 * !scopes);
  is_true
    ~msg:
      (Printf.sprintf "%d invariant checks over %d systems, not %d" !invariants
         !scopes (4 * !scopes))
    (!invariants = 4 * !scopes)

(* [?pp_model] reaches the printer the engine renders a counterexample
   with, and reaches it with the pre-states. *)
let stateful_threads_pp_model_into_the_counterexample () =
  let seen = ref [] in
  let pp_model ppf model =
    seen := model :: !seen;
    Format.pp_print_int ppf model
  in
  let commands =
    [
      Windtrap.call "tick"
        ~next:(fun model -> model + 1)
        (fun model () -> Check.is_true ~msg:"the third call" (model < 2));
    ]
  in
  (* Every call is legal, so each case draws exactly three of them and only
     the third fails: no candidate of the drawn program fails, so the
     counterexample is the three-call program and its column is 0, 1, 2. *)
  run_declared_body
    (Windtrap.stateful ~count:3 ~steps:3 ~pp_model "failing" ~model:0
       ~scope:unit_scope commands);
  is_true ~msg:"~pp_model never reached the counterexample printer" (!seen <> []);
  is_true
    ~msg:
      (Printf.sprintf
         "the model column printed %s, not the pre-states of a 3-call program"
         (show_ints (List.sort_uniq compare !seen)))
    (List.for_all (fun model -> model >= 0 && model <= 2) !seen);
  is_true ~msg:"the model column stopped short of the last step"
    (List.mem 2 !seen)

let the_same_seed_reproduces_the_same_counterexample () =
  let once () =
    (* The whole stream, not just where it converged: a stateful test is
       [prop] over a derived generator, so nothing between the seed and the
       law may carry state from one run to the next. *)
    let trace = ref [] in
    let outcome =
      Property.run ~count:(`Declared 40) ~root ~path:"replay" (queue_gen ())
        (fun _ program ->
          trace := names (queue_gen ()) program :: !trace;
          Stateful.execute ~scope:(fun run -> run (Bad_queue.create ())) program)
    in
    let failure, _ = expect_fail outcome in
    let rendered, case, steps, _, _ = property_payload failure in
    (List.rev !trace, rendered, case, steps)
  in
  (* Both runs happen at one call site: captured assertion locations are
     stack-derived, so distinct call sites would differ there. *)
  let trace, rendered, case, steps = once () in
  let trace', rendered', case', steps' = once () in
  is_true
    ~msg:
      (Printf.sprintf
         "the run executed %d programs; the trace is too short to be evidence"
         (List.length trace))
    (List.length trace > 20);
  is_true
    ~msg:
      (Printf.sprintf
         "the two runs executed %d and %d programs, differing first at %d"
         (List.length trace) (List.length trace')
         (let rec first index = function
            | a :: al, b :: bl ->
                if a = b then first (index + 1) (al, bl) else index
            | _ -> min (List.length trace) (List.length trace')
          in
          first 0 (trace, trace')))
    (trace = trace');
  is_true
    ~msg:
      (Printf.sprintf "the counterexample differed:\n%s\nand\n%s" rendered
         rendered')
    (rendered = rendered');
  is_true
    ~msg:
      (Printf.sprintf "the failing case index differed: %d and %d" case case')
    (case = case');
  is_true
    ~msg:
      (Printf.sprintf "the shrink step count differed: %d and %d" steps steps')
    (steps = steps')

(* The generator prints, always (even over a command whose own argument
   generator does not) so a printerless stateful counterexample is
   unreachable and the report's [Gen.with_pp] remedy line never fires. *)
let the_program_generator_always_prints () =
  let opaque =
    [
      Stateful.command "opaque" (Gen.constant 5)
        ~next:(fun model _ -> model)
        (fun _ _ () -> ());
    ]
  in
  is_true ~msg:"Gen.constant grew a printer; the test is vacuous"
    (Gen_engine.render_value (Gen.constant 5) 5 = placeholder);
  let gen = Stateful.program ~steps:4 ~model:0 opaque in
  is_true ~msg:"a program over a printerless command carries no printer"
    (render gen (program_at gen 0) <> placeholder);
  let outcome =
    Property.run ~count:(`Declared 5) ~root ~path:"printerless"
      (Stateful.program ~steps:4 ~model:0 opaque) (fun _ _ ->
        Check.fail "always")
  in
  let failure, _ = expect_fail outcome in
  let _, _, _, rendering, _ = property_payload failure in
  is_true
    ~msg:"a stateful counterexample reported as something other than the value"
    (rendering = Failure.Value);
  is_true ~msg:"the printerless remedy line fired on a stateful counterexample"
    (not (contains "Gen.with_pp" (failure_block failure)))

(* End to end *)

(* A real buggy system, a real failing run, and the shape of the report the
   reader gets. *)
let a_buggy_system_renders_a_diagnosable_failure () =
  let outcome =
    Property.run ~count:(`Declared 40) ~summary:Stateful.summary ~root
      ~path:"bad_queue" (queue_gen ()) (fun _ program ->
        Stateful.execute ~invariant:queue_invariant
          ~scope:(fun run -> run (Bad_queue.create ()))
          program)
  in
  let failure, _ = expect_fail outcome in
  let rendered, _, shrink_steps, _, inner = property_payload failure in
  let block = failure_block failure in
  is_true ~msg:"the search took no shrink step" (shrink_steps > 0);
  (* The search converges on the shortest disagreement the bug admits: one
     element pops correctly, two do not, and the arguments have to differ or
     the wrong element is the right one. *)
  let expected =
    String.concat "\n" [ " #  call"; " 1  push 0"; " 2  push 1"; " 3  pop" ]
  in
  is_true
    ~msg:
      (Printf.sprintf "the counterexample rendered:\n%s\nnot:\n%s" rendered
         expected)
    (rendered = expected);
  (match failure.Failure.kind with
  | Failure.Property { summary; _ } ->
      is_true
        ~msg:
          (Printf.sprintf "the failure's summary is %s"
             (Option.fold summary ~none:"absent"
                ~some:(fun (s : Failure.text) -> s.kept)))
        (Option.map (fun (s : Failure.text) -> s.kept) summary
        = Some "3 calls, last: pop")
  | _ -> failf "expected a Property failure kind");
  (match inner with
  | Some inner ->
      is_true
        ~msg:
          (Printf.sprintf "the inner failure was labelled %S"
             (failure_msg inner))
        (failure_msg inner = "call 3 of 3: pop");
      is_true ~msg:"the inner failure is not the body's own equality"
        (inner.Failure.kind
        = Failure.Equality
            {
              expected = Failure.text "0";
              actual = Failure.text "1";
              not_ = false;
              diffable = true;
            })
  | None -> failf "the counterexample reported no inner failure");
  List.iter
    (fun needle ->
      is_true
        ~msg:(Printf.sprintf "the rendered failure lacks %S:\n%s" needle block)
        (contains needle block))
    [
      "): 3 calls, last: pop\n\
      \       #  call\n\
      \       1  push 0\n\
      \       2  push 1\n\
      \       3  pop\n\
      \    which failed at:\n";
      "      call 3 of 3: pop\n";
      "expected  0";
      "actual    1";
      "replay:";
    ]

(* The suite *)

(* Contract details: each test below reads one sentence of stateful.mli. *)

let line_of ((_, line, _, _) : Loc.pos) = line

(* A one-call program over [commands] whose single command fails with no
   location of its own. *)
let failing_loc commands =
  let program = program_at (Stateful.program ~steps:1 ~model:0 commands) 0 in
  (expect_check_failure "the failing call" (fun () ->
       Stateful.execute ~scope:unit_scope program))
    .Failure.loc

(* The bodies raise on their own lines, away from the declarations, so a
   capture at the failure would give another line. *)
let boom _ () _ = raise Exit
let boom_call _ () = raise Exit

let command_and_call_capture_their_declaration_site () =
  let p, c = (__POS__, Stateful.command "boom" Gen.unit ~next:Fun.const boom) in
  (match failing_loc [ c ] with
  | Some loc ->
      equal ~msg:"command: the line of its call" int (line_of p) loc.Loc.line
  | None -> fail "command captured no location");
  let p, c = (__POS__, Stateful.call "boom" ~next:Fun.id boom_call) in
  match failing_loc [ c ] with
  | Some loc ->
      equal ~msg:"call: the line of its caller" int (line_of p) loc.Loc.line
  | None -> fail "call captured no location"

let a_mapped_argument_prints_as_the_placeholder () =
  let gen =
    Stateful.program ~steps:1 ~model:0
      [
        Stateful.command "set" (Gen.map succ Gen.nat) ~next:Fun.const
          (fun _ _ () -> ());
      ]
  in
  equal (list string)
    [ " #  call"; " 1  set " ^ placeholder ]
    (lines_of gen (program_at gen 0))

let a_model_cell_is_cut_at_60_code_points () =
  let pp_model ppf _ =
    Format.pp_print_string ppf
      (String.concat "" (List.init 100 (fun _ -> "\u{00e9}")))
  in
  let gen = Stateful.program ~steps:1 ~model:0 ~pp_model tick_commands in
  let cell = String.concat "" (List.init 57 (fun _ -> "\u{00e9}")) ^ "..." in
  equal (list string)
    [
      " #  model before" ^ String.make 48 ' ' ^ "  call";
      " 1  " ^ cell ^ "  tick";
    ]
    (lines_of gen (program_at gen 0))

(* The number in a specification bug's message counts the kept calls: the
   drawn calls that repair dropped before it do not count. *)
let a_specification_bug_numbers_the_kept_calls () =
  let drawn = ref [] in
  let log name = drawn := name :: !drawn in
  let commands =
    [
      Stateful.call "first"
        ~pre:(fun model ->
          log "first";
          model = 0)
        ~next:succ
        (fun _ () -> ());
      Stateful.call "never"
        ~pre:(fun _ ->
          log "never";
          false)
        ~next:Fun.id
        (fun _ () -> ());
      Stateful.call "boom"
        ~pre:(fun model ->
          log "boom";
          if model >= 1 then failwith "nth" else false)
        ~next:Fun.id
        (fun _ () -> ());
    ]
  in
  let gen = Stateful.program ~steps:12 ~model:0 commands in
  let rec find index =
    if index >= 200 then fail "no program raised after a dropped call"
    else begin
      drawn := [];
      match Gen_engine.sample gen (state index) with
      | _ -> find (index + 1)
      | exception exn ->
          (* The raising boom is the last pre evaluated; it was drawn after
             at least one call that repair dropped. *)
          if List.length !drawn > 2 then Printexc.to_string exn
          else find (index + 1)
    end
  in
  equal string {|call 2: boom, ~pre raised Failure("nth")|} (find 0)

let fatal_exceptions =
  [ ("Sys.Break", Sys.Break); ("Out_of_memory", Out_of_memory) ]

let every_fatal_exception_escapes_as_itself () =
  List.iter
    (fun (label, exn) ->
      List.iter
        (fun phase ->
          let gen =
            Stateful.program ~steps:4 ~model:0 (control_spec exn phase)
          in
          match Gen_engine.sample gen (state 0) with
          | exception raised when raised == exn -> ()
          | exception raised ->
              failf "%s from repair came back as %s" label
                (Printexc.to_string raised)
          | _ -> failf "%s from repair was swallowed" label)
        [ `Pre; `Next ];
      (match Stateful.execute ~scope:unit_scope (one_call_program exn) with
      | exception raised when raised == exn -> ()
      | exception raised ->
          failf "%s from a body came back as %s" label
            (Printexc.to_string raised)
      | () -> failf "%s from a body was swallowed" label);
      let failing =
        one_call_program (Failure.Check_failure (Failure.message "the body"))
      in
      match
        Stateful.execute
          ~scope:(fun run ->
            match run () with () -> raise exn | exception _ -> raise exn)
          failing
      with
      | exception raised when raised == exn -> ()
      | exception raised ->
          failf "%s from a failing path's scope came back as %s" label
            (Printexc.to_string raised)
      | () -> failf "the failing program did not fail")
    fatal_exceptions

let the_summary_names_the_last_call_not_the_failing_one () =
  let commands =
    [
      Stateful.call "fail"
        ~pre:(fun model -> model = 0)
        ~next:succ
        (fun _ () -> Check.fail "the first call");
      Stateful.call "ok"
        ~pre:(fun model -> model > 0)
        ~next:succ
        (fun _ () -> ());
    ]
  in
  let gen = Stateful.program ~steps:6 ~model:0 commands in
  let program =
    let rec find index =
      if index >= 50 then fail "no program of fail, then ok"
      else
        let program = program_at gen index in
        match names gen program with
        | "fail" :: _ :: _ -> program
        | _ -> find (index + 1)
    in
    find 0
  in
  let total = List.length (names gen program) in
  equal ~msg:"the summary" (option string)
    (Some (Printf.sprintf "%d calls, last: ok" total))
    (Stateful.summary program);
  equal ~msg:"the label" string
    (Printf.sprintf "call 1 of %d: fail" total)
    (failure_msg
       (expect_check_failure "the first call" (fun () ->
            Stateful.execute ~scope:unit_scope program)))

let an_invariant_failure_has_no_command_location () =
  let site = ("declared.ml", 7, 0, 3) in
  let program =
    program_at
      (Stateful.program ~steps:1 ~model:0
         [ Stateful.call ~__POS__:site "tick" ~next:succ (fun _ () -> ()) ])
      0
  in
  let loc_under invariant =
    (expect_check_failure "the invariant" (fun () ->
         Stateful.execute ~invariant ~scope:unit_scope program))
      .Failure.loc
  in
  is_true ~msg:"on the fresh system: none"
    (loc_under (fun _ () -> raise Exit) = None);
  is_true ~msg:"after a call: none either"
    (loc_under (fun model () -> if model = 1 then raise Exit) = None);
  let own = ("invariant.ml", 3, 0, 5) in
  is_true ~msg:"an invariant that locates its failure keeps its own"
    (loc_under (fun model () ->
         if model = 1 then fail ~__POS__:own "the state is wrong")
    = Some (Loc.of_pos own))

let an_invalid_timeout_raises_at_declaration () =
  List.iter
    (fun timeout ->
      raises_match ~msg:(string_of_float timeout) Exn.invalid_arg (fun () ->
          Windtrap.stateful ~timeout "t" ~model:0 ~scope:unit_scope tick_facade))
    [ 0.; -1.; Float.nan; Float.infinity ]

let suite =
  [
    ( "command and call capture their declaration site",
      command_and_call_capture_their_declaration_site );
    ( "a mapped argument prints as the placeholder",
      a_mapped_argument_prints_as_the_placeholder );
    ( "a model cell is cut at 60 code points",
      a_model_cell_is_cut_at_60_code_points );
    ( "a specification bug numbers the kept calls",
      a_specification_bug_numbers_the_kept_calls );
    ( "every fatal exception escapes as itself",
      every_fatal_exception_escapes_as_itself );
    ( "the summary names the last call, not the failing one",
      the_summary_names_the_last_call_not_the_failing_one );
    ( "an invariant failure has no command location",
      an_invariant_failure_has_no_command_location );
    ( "an invalid timeout raises at declaration",
      an_invalid_timeout_raises_at_declaration );
    ( "repair keeps exactly the fold's calls",
      repair_keeps_exactly_the_fold_s_calls );
    ("a state-dependent ~pre filters", a_state_dependent_precondition_filters);
    ( "every forced node holds only legal calls",
      every_forced_node_holds_only_legal_calls );
    ( "root candidates are strictly monotone",
      root_candidates_are_strictly_monotone );
    ( "root candidates of an argument spec are no longer",
      root_candidates_of_an_argument_spec_are_no_longer );
    ( "no node invents or substitutes a call",
      no_node_invents_or_substitutes_a_call );
    ( "every command is drawn about equally often",
      every_command_is_drawn_about_equally_often );
    ( "a raising ~pre or ~next is a specification bug",
      a_raising_pre_or_next_is_a_specification_bug );
    ( "a specification bug met while shrinking stops the search",
      a_specification_bug_met_while_shrinking_stops_the_search );
    ( "control exceptions escape ~pre and ~next unconverted",
      control_exceptions_escape_pre_and_next_unconverted );
    ( "a failing step points at its command",
      a_failing_step_points_at_its_command );
    ( "assertions from ~pre are specification bugs",
      assertions_from_pre_are_specification_bugs );
    ( "control exceptions escape a body unconverted",
      control_exceptions_escape_a_body_unconverted );
    ("a scope releases on every path", a_scope_releases_on_every_path);
    ( "a release failure never replaces the program's",
      a_release_failure_never_replaces_the_program_s );
    ( "a scope that never runs the program fails the case",
      a_scope_that_never_runs_the_program_fails_the_case );
    ( "a scope that runs the program twice is invalid",
      a_scope_that_runs_the_program_twice_is_invalid );
    ( "a scope that raises before the callback propagates unconverted",
      a_scope_that_raises_before_the_callback_propagates_unconverted );
    ( "a failing program keeps its identity through the scope",
      a_failing_program_keeps_its_identity_through_the_scope );
    ( "the scope runs once per case and per shrink candidate",
      the_scope_runs_once_per_case_and_per_shrink_candidate );
    ( "the invariant runs before step one and after every step",
      the_invariant_runs_before_step_one_and_after_every_step );
    ( "an invariant is narrowed and propagates like a body",
      an_invariant_is_narrowed_and_propagates_like_a_body );
    ("the empty program prints (no commands)", empty_program_prints_no_commands);
    ( "unit arguments are suppressed under a summary line",
      unit_arguments_are_suppressed_under_a_summary_line );
    ( "the model column shows the pre-state",
      the_model_column_shows_the_pre_state );
    ("a raising pp_model costs one cell", a_raising_pp_model_costs_one_cell);
    ("a long model cell truncates", a_long_model_cell_truncates);
    ( "a printerless argument degrades to a placeholder",
      a_printerless_argument_degrades_to_a_placeholder );
    ("a long argument truncates", a_long_argument_truncates);
    ( "newlines in names and cells are flattened",
      newlines_in_names_and_cells_are_flattened );
    ( "long programs truncate with a calls-omitted line",
      long_programs_truncate_with_a_step_omitted_line );
    ( "a malformed declaration raises at sample time",
      a_malformed_declaration_raises_at_sample_time );
    ( "stateful declares a prop node with its tags, timeout and site",
      stateful_declares_a_prop_node_with_its_tags_timeout_and_site );
    ( "stateful runs one fresh system per case over ?steps calls",
      stateful_runs_one_fresh_system_per_case_over_steps_calls );
    ( "stateful threads ~pp_model into the counterexample",
      stateful_threads_pp_model_into_the_counterexample );
    ( "the same seed reproduces the same counterexample",
      the_same_seed_reproduces_the_same_counterexample );
    ("the program generator always prints", the_program_generator_always_prints);
    ( "a buggy system renders a diagnosable failure",
      a_buggy_system_renders_a_diagnosable_failure );
  ]

let tests = List.map (fun (name, fn) -> test name fn) suite
let () = exit @@ Windtrap.run "stateful" tests
