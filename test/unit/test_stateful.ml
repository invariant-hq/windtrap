(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Stateful: the command type, the program generator and its
   repair, poisoning, the executor, and the program printer. *)

open Windtrap
open Windtrap.Private

(* Printf-style shims over windtrap's [fail]. [Check.*] calls inside command
   bodies are the probes the engine and the executor catch; only these shims
   escape to the runner. *)
let failf format = Printf.ksprintf (fun message -> Windtrap.fail message) format

let check condition format =
  Printf.ksprintf
    (fun message -> if not condition then Windtrap.fail message)
    format

let contains needle haystack = Text.contains_substring ~pattern:needle haystack
let show_names names = "[" ^ String.concat "; " names ^ "]"
let show_ints values = show_names (List.map string_of_int values)

(* One fixed root for the whole suite; per-test streams come from indexes.
   Everything below is deterministic across runs and machines. *)
let root = 0x00c0ffee1234abcdL
let state index = Seed.make (Seed.derive ~root ~path:"test_stateful" ~index)
let root_value tree = Shrink_tree.root tree
let program_at gen index = root_value (Gen.sample gen (state index))
let names = Stateful.command_names

let render gen program =
  match Gen.render_value gen program with
  | Some text -> text
  | None -> failf "the program generator carries no printer"

let lines_of gen program = Text.split_lines (render gen program)

let expect_check_failure what fn =
  match fn () with
  | () -> failf "%s: expected a Check_failure, nothing was raised" what
  | exception Failure.Check_failure failure -> failure

let failure_msg (failure : Failure.t) =
  match failure.Failure.msg with
  | Some msg -> msg
  | None -> failf "the failure carries no ~msg"

let raised_actual (failure : Failure.t) =
  match failure.Failure.kind with
  | Failure.Raise { actual = Some actual; _ } -> actual
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
  | Failure.Property
      { rendered; case_index; shrink_steps; printerless; inner; _ } ->
      (rendered, case_index, shrink_steps, printerless, inner)
  | _ -> failf "expected a Property failure kind"

let failure_block failure =
  let buf = Buffer.create 512 in
  let ppf = Format.formatter_of_buffer buf in
  Render.pp_failure ~ansi:false ppf failure;
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
   argument is drawn, so the same seed draws the same name sequence — the
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
   drawn one — with two names, coincidence hides it. *)
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
   function of the command name, so the check has to be the body's own — it
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
   keeps — checked against the drawn program, which the unconditioned twin
   of the same spec draws from the same seed. *)
let repair_keeps_exactly_the_fold_s_calls () =
  let repaired = Stateful.program ~steps:20 ~model:0 counter_commands in
  let drawn = Stateful.program ~steps:20 ~model:0 counter_draws in
  let dropped = ref 0 in
  for index = 0 to 29 do
    let drawn = names (program_at drawn index) in
    check
      (List.length drawn = 20)
      "the unconditioned program made %d of 20 calls" (List.length drawn);
    let expected = counter_repair drawn in
    let kept = names (program_at repaired index) in
    check (kept = expected) "repair of %s kept %s, not %s" (show_names drawn)
      (show_names kept) (show_names expected);
    dropped := !dropped + (20 - List.length kept)
  done;
  check (!dropped > 0) "the precondition dropped nothing in 30 draws — vacuous";
  (* [?steps] is a work budget with a documented default. *)
  let default =
    names (program_at (Stateful.program ~model:0 counter_draws) 0)
  in
  check
    (List.length default = 20)
    "the default ?steps drew %d calls, not 20" (List.length default)

(* A state-dependent [~pre] really filters: the trajectory it admits stays
   inside the model's bounds, and a precondition no state satisfies deletes
   its command from every program. *)
let a_state_dependent_precondition_filters () =
  let gen = Stateful.program ~steps:20 ~model:0 counter_commands in
  let shortened = ref 0 in
  for index = 0 to 19 do
    let kept = names (program_at gen index) in
    if List.length kept < 20 then incr shortened;
    ignore
      (List.fold_left
         (fun model name ->
           let model = counter_step model name in
           check
             (model >= 0 && model <= counter_cap)
             "the kept calls %s left the model at %d" (show_names kept) model;
           model)
         0 kept
        : int)
  done;
  check (!shortened > 0) "no program was shortened in 20 draws — vacuous";
  let never = Stateful.program ~steps:8 ~model:0 never_commands in
  for index = 0 to 4 do
    check
      (names (program_at never index) = [])
      "an unsatisfiable ~pre left calls in the program"
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
    Stateful.execute ~setup:(fun () -> ()) (root_value tree);
    Seq.iter go (Shrink_tree.children tree)
  in
  let index = ref 0 in
  (try
     while !nodes < budget do
       go (Gen.sample gen (state !index));
       incr index;
       if !index > 40 then raise_notrace Exit
     done
   with Exit -> ());
  check (!nodes >= budget) "only %d nodes were forced" !nodes;
  check (!illegal = []) "%d illegal calls survived repair over %d nodes: %s"
    (List.length !illegal) !nodes
    (show_names (List.filteri (fun i _ -> i < 5) !illegal))

(* Root masking *)

(* At depth 1 no candidate equals its parent and none is longer. With
   nullary commands [Gen.unit] is a leaf, so every immediate move is a
   deletion and a name list is the whole program — masking applied on top of
   an assembled tree would delete a call the mask already dropped and hand
   back the parent. *)
let root_candidates_are_strictly_monotone () =
  let gen = Stateful.program ~steps:12 ~model:0 counter_commands in
  let checked = ref 0 in
  for index = 0 to 19 do
    let tree = Gen.sample gen (state index) in
    let parent = names (root_value tree) in
    if parent <> [] && List.length parent < 12 then begin
      incr checked;
      Seq.iter
        (fun child ->
          let child = names (root_value child) in
          check (child <> parent) "a candidate of %s equals its parent"
            (show_names parent);
          check
            (List.length child <= List.length parent)
            "a candidate of %s is longer: %s" (show_names parent)
            (show_names child))
        (Shrink_tree.children tree)
    end
  done;
  check (!checked > 0) "no repaired-and-shortened program in 20 draws — vacuous"

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
    let tree = Gen.sample gen (state index) in
    let parent = List.length (names (root_value tree)) in
    if parent < 10 then incr checked;
    Seq.iter
      (fun child ->
        let child = List.length (names (root_value child)) in
        check (child <= parent) "a candidate of a %d-call program made %d calls"
          parent child)
      (Shrink_tree.children tree)
  done;
  check (!checked > 0) "the mask dropped nothing in 20 draws — vacuous"

let rec is_subsequence sub whole =
  match (sub, whole) with
  | [], _ -> true
  | _, [] -> false
  | x :: sub', y :: whole' ->
      if x = y then is_subsequence sub' whole' else is_subsequence sub whole'

(* Deeper than depth 1 the monotonicity weakens by design — a candidate can
   repeat its parent or re-legalise a call its parent dropped — but the
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
    let drawn = names (program_at drawn index) in
    let budget = !nodes + 400 in
    let rec go tree =
      if !nodes >= budget then raise_notrace Exit;
      incr nodes;
      let kept = names (root_value tree) in
      check
        (is_subsequence kept drawn)
        "a node's calls %s are not a subsequence of the drawn %s"
        (show_names kept) (show_names drawn);
      Seq.iter go (Shrink_tree.children tree)
    in
    try go (Gen.sample repaired (state index)) with Exit -> ()
  done;
  check (!nodes >= 2_000) "only %d nodes were forced" !nodes

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
      (names (program_at gen index))
  done;
  check (!total = 400) "20 unconditioned draws of 20 made %d calls" !total;
  let uniform = float_of_int !total /. float_of_int (List.length wide_names) in
  List.iter
    (fun (name, count) ->
      let drawn = float_of_int !count in
      check
        (drawn >= uniform /. 2. && drawn <= uniform *. 2.)
        "%s was drawn %.0f times of %d, nowhere near the uniform %.0f" name
        drawn !total uniform)
    counts

(* Poisoning *)

exception Pre_boom
exception Next_boom

let poison_spec ~body_ran phase =
  [
    Stateful.call "benign" ~next:(fun model -> model + 1) (fun _ () -> ());
    Stateful.call "boom"
      ?pre:
        (match phase with
        | `Pre -> Some (fun _ -> raise Pre_boom)
        | `Next -> None)
      ~next:(fun model ->
        match phase with `Next -> raise Next_boom | `Pre -> model)
      (fun _ () -> body_ran := true);
  ]

let find_poisoned gen =
  let rec loop index =
    if index >= 200 then failf "no poisoned program within 200 samples"
    else
      let program = program_at gen index in
      match List.rev (names program) with
      | "boom" :: _ -> program
      | _ -> loop (index + 1)
  in
  loop 0

(* A [~pre] that raises truncates the program at its own step, keeps that
   step as the program's last call, and does not run its body. *)
let a_raising_pre_poisons_and_withholds_the_body () =
  let body_ran = ref false in
  let gen = Stateful.program ~steps:6 ~model:0 (poison_spec ~body_ran `Pre) in
  let program = find_poisoned gen in
  let kept = names program in
  let total = List.length kept in
  check
    (List.nth kept (total - 1) = "boom")
    "the poisoned step is not the program's last call: %s" (show_names kept);
  check
    (not
       (List.exists
          (fun name -> name = "boom")
          (List.filteri (fun i _ -> i < total - 1) kept)))
    "the program was not truncated at the poison: %s" (show_names kept);
  let failure =
    expect_check_failure "a ~pre poison" (fun () ->
        Stateful.execute ~setup:(fun () -> ()) program)
  in
  check
    (failure_msg failure
    = Pp.str "step %d of %d: boom \u{2014} ~pre raised" total total)
    "the poison message was %S" (failure_msg failure);
  check
    (contains "Pre_boom" (raised_actual failure))
    "the poison did not name the exception: %S" (raised_actual failure);
  check (not !body_ran) "a ~pre poison ran the step's body"

(* A [~next] that raises means [~pre] held: only the model after the call is
   unknown, so the body does run — under the step's own attribution — before
   the poison is reported. No invariant check follows it. *)
let a_raising_next_poisons_and_runs_the_body () =
  let body_ran = ref false in
  let gen = Stateful.program ~steps:6 ~model:0 (poison_spec ~body_ran `Next) in
  let program = find_poisoned gen in
  let kept = names program in
  let total = List.length kept in
  let invariants = ref 0 in
  let failure =
    expect_check_failure "a ~next poison" (fun () ->
        Stateful.execute
          ~invariant:(fun _ () -> incr invariants)
          ~setup:(fun () -> ())
          program)
  in
  check
    (failure_msg failure
    = Pp.str "step %d of %d: boom \u{2014} ~next raised" total total)
    "the poison message was %S" (failure_msg failure);
  check
    (contains "Next_boom" (raised_actual failure))
    "the poison did not name the exception: %S" (raised_actual failure);
  check !body_ran "a ~next poison withheld the step's body";
  (* One on the fresh system, one after each step before the poisoned one,
     and none after the poisoned step itself. *)
  check (!invariants = total)
    "the invariant ran %d times for a %d-step poisoned program" !invariants
    total

(* The body of a [~next]-poisoned step runs under the step's own
   attribution and {e before} the poison is reported, so a failure of that
   body is the reported failure — the poison surfaces on some other case,
   where the body agrees with the model. *)
let a_failing_body_outranks_the_next_poison_it_precedes () =
  let commands =
    [
      Stateful.call "benign" ~next:(fun model -> model + 1) (fun _ () -> ());
      Stateful.call "boom"
        ~next:(fun _ -> raise Next_boom)
        (fun _ () -> Check.fail "the body");
    ]
  in
  let program = find_poisoned (Stateful.program ~steps:6 ~model:0 commands) in
  let total = List.length (names program) in
  let failure =
    expect_check_failure "a failing body before a ~next poison" (fun () ->
        Stateful.execute ~setup:(fun () -> ()) program)
  in
  check
    (failure.Failure.kind = Failure.Message "the body")
    "the poison replaced the body's failure: %S"
    (Printexc.to_string (Failure.Check_failure failure));
  check
    (failure_msg failure = Pp.str "step %d of %d: boom" total total)
    "the body's failure was labelled %S" (failure_msg failure)

(* The declaration site is stamped on the poisoned-program failure, which is
   the one failure with no assertion site of its own — and on that one
   only, or every counterexample would point at the [stateful] declaration
   instead of at the check that broke. *)
let a_poisoned_program_carries_the_declaration_site () =
  let body_ran = ref false in
  let gen = Stateful.program ~steps:6 ~model:0 (poison_spec ~body_ran `Pre) in
  let program = find_poisoned gen in
  let loc = { Loc.file = "spec.ml"; line = 42; column = 7 } in
  let failure =
    expect_check_failure "a located poison" (fun () ->
        Stateful.execute ~loc ~setup:(fun () -> ()) program)
  in
  check (failure.Failure.loc = Some loc) "the poison lost the declaration site";
  let failing =
    program_at
      (Stateful.program ~steps:1 ~model:0
         [
           Stateful.call "boom"
             ~next:(fun model -> model + 1)
             (fun _ () -> raise Not_found);
         ])
      0
  in
  let ordinary =
    expect_check_failure "a body failure under a located execute" (fun () ->
        Stateful.execute ~loc ~setup:(fun () -> ()) failing)
  in
  check
    (ordinary.Failure.loc <> Some loc)
    "the declaration site was stamped on a failure that is not the poison's"

let control_spec exn phase =
  [
    Stateful.call "raiser"
      ?pre:
        (match phase with `Pre -> Some (fun _ -> raise exn) | `Next -> None)
      ~next:(fun model -> match phase with `Next -> raise exn | `Pre -> model)
      (fun _ () -> ());
  ]

(* The partition: what [execute] refuses to convert is what repair refuses
   to poison, so these escape the generator unchanged from [~pre] and from
   [~next] alike. *)
let control_exceptions_escape_pre_and_next_unconverted () =
  (* Repair's partition is narrower than a body's: only what is about the
     run escapes. [Skip_test], [Check_failure] and [Discard] are about the
     model here, and poison — see the test below. *)
  let cases =
    [
      ("Timeout", Failure.Timeout 0.5);
      ("Exit_attempt", Failure.Exit_attempt);
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
          match Gen.sample gen (state 0) with
          | exception raised ->
              check (raised = exn) "%s from %s came back as %s" label spelling
                (Printexc.to_string raised)
          | _ -> failf "%s from %s was swallowed" label spelling)
        [ (`Pre, "~pre"); (`Next, "~next") ])
    cases

(* The failing step points at the command. A body is idiomatically one
   assertion in tail position, and under the runner's [Loc.delimit] barrier
   nothing is capturable when it raises — so the failure arrives with no
   location and the command's own site fills it. A body that did record a
   site keeps it, being nearer the failure. Both halves raise the payload
   directly rather than through [Check], whose capture succeeds outside a
   run and would hide the case this exists to pin. *)
let a_failing_step_points_at_its_command () =
  let site = ("declared.ml", 42, 7, 11) in
  let executed ?loc () =
    let spec =
      [
        Stateful.call ~pos:site "boom" ~next:Fun.id (fun _ () ->
            raise
              (Failure.Check_failure
                 (Failure.equality ?loc ~expected:"1" ~actual:"2" ())));
      ]
    in
    let program =
      Shrink_tree.root
        (Gen.sample (Stateful.program ~steps:1 ~model:0 spec) (state 0))
    in
    expect_check_failure "a located step" (fun () ->
        Stateful.execute ~setup:(fun () -> ()) program)
  in
  (match (executed ()).Failure.loc with
  | Some loc ->
      check
        (loc.Loc.file = "declared.ml" && loc.Loc.line = 42)
        "the step was located at %s" (Loc.to_string loc)
  | None -> failf "a locationless step reported no location");
  let own = Loc.of_pos ("body.ml", 9, 0, 4) in
  match (executed ~loc:own ()).Failure.loc with
  | Some loc ->
      check (loc.Loc.file = "body.ml")
        "the body's own site was overwritten by %s" (Loc.to_string loc)
  | None -> failf "the body-located step reported no location"

(* The three exceptions a body treats as control, which repair does not:
   at generation time an assertion, a skip and a discard are all the model
   being written wrong, so each poisons and the report names the command,
   the step and the phase instead of losing the payload inside the
   generator. *)
let assertions_skips_and_discards_from_pre_poison () =
  let cases =
    [
      ( "Check_failure",
        Failure.Check_failure (Failure.equality ~expected:"1" ~actual:"2" ()),
        "windtrap assertion failure" );
      ("Skip_test", Failure.Skip_test (Some "why"), "windtrap skip: why");
      ("Discard", Property.Discard, "Discard");
    ]
  in
  List.iter
    (fun (label, exn, needle) ->
      let gen = Stateful.program ~steps:4 ~model:0 (control_spec exn `Pre) in
      (* It reaches the program rather than the generator: sampling
         succeeds, where before it raised. *)
      let program =
        match Gen.sample gen (state 0) with
        | exception raised ->
            failf "%s from ~pre escaped the generator as %s" label
              (Printexc.to_string raised)
        | tree -> Shrink_tree.root tree
      in
      let kept = names program in
      let total = List.length kept in
      check (total > 0) "%s from ~pre produced an empty program" label;
      let failure =
        expect_check_failure label (fun () ->
            Stateful.execute ~setup:(fun () -> ()) program)
      in
      check
        (failure_msg failure
        = Pp.str "step %d of %d: raiser \u{2014} ~pre raised" total total)
        "%s from ~pre was labelled %S" label (failure_msg failure);
      check
        (contains needle (raised_actual failure))
        "%s from ~pre did not name the exception: %S" label
        (raised_actual failure))
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
      match Stateful.execute ~setup:(fun () -> ()) (program_of exn) with
      | exception raised ->
          check (raised = exn) "%s from a body came back as %s" label
            (Printexc.to_string raised)
      | () -> failf "%s from a body was swallowed" label)
    [
      ("Skip_test", Failure.Skip_test (Some "why"));
      ("Timeout", Failure.Timeout 0.5);
      ("Exit_attempt", Failure.Exit_attempt);
      ("Discard", Property.Discard);
      ("Sys.Break", Sys.Break);
    ];
  (* A Check_failure is already the class the narrowing aims at: it keeps
     its payload and gains the step label, joined onto the user's ~msg —
     which is flattened first, since the slot renders as one line. *)
  let asserted =
    expect_check_failure "an asserting body" (fun () ->
        Stateful.execute
          ~setup:(fun () -> ())
          (program_of
             (Failure.Check_failure
                {
                  (Failure.message "nope") with
                  Failure.msg = Some "note\nand more";
                })))
  in
  check
    (failure_msg asserted = "step 1 of 1: boom \u{2014} note and more")
    "an assertion failure was labelled %S" (failure_msg asserted);
  check
    (asserted.Failure.kind = Failure.Message "nope")
    "an assertion failure lost its payload";
  (* And anything else is narrowed, under the same label. *)
  let narrowed =
    expect_check_failure "a raising body" (fun () ->
        Stateful.execute ~setup:(fun () -> ()) (program_of Not_found))
  in
  check
    (failure_msg narrowed = "step 1 of 1: boom")
    "a narrowed exception was labelled %S" (failure_msg narrowed);
  check
    (raised_actual narrowed = "Not_found")
    "a narrowed exception rendered %S" (raised_actual narrowed)

(* The executor *)

let counter_program ?(steps = 4) index =
  program_at (Stateful.program ~steps ~model:0 counter_draws) index

(* [teardown] releases on every path [execute] leaves, and on none it does
   not: a [setup] that raises owes no teardown. *)
let teardown_runs_on_every_path () =
  let body_ran = ref false in
  let poisoned =
    find_poisoned
      (Stateful.program ~steps:6 ~model:0 (poison_spec ~body_ran `Pre))
  in
  let raising exn =
    [
      Stateful.call "boom"
        ~next:(fun model -> model + 1)
        (fun _ () -> raise exn);
    ]
  in
  let one_call exn =
    program_at (Stateful.program ~steps:1 ~model:0 (raising exn)) 0
  in
  let paths =
    [
      ("pass", counter_program 0);
      ("body failure", one_call (Failure.Check_failure (Failure.message "nope")));
      ("skip", one_call (Failure.Skip_test (Some "why")));
      ("timeout", one_call (Failure.Timeout 0.5));
      ("uncaught", one_call Not_found);
      ("poison", poisoned);
    ]
  in
  List.iter
    (fun (label, program) ->
      let released = ref 0 in
      (try
         Stateful.execute
           ~teardown:(fun () -> incr released)
           ~setup:(fun () -> ())
           program
       with _ -> ());
      check (!released = 1) "the %s path released %d times" label !released)
    paths;
  let released = ref 0 in
  (match
     Stateful.execute
       ~teardown:(fun () -> incr released)
       ~setup:(fun () -> raise Not_found)
       (counter_program 0)
   with
  | exception Not_found -> ()
  | exception exn ->
      failf "a raising setup came back as %s" (Printexc.to_string exn)
  | () -> failf "a raising setup was swallowed");
  check (!released = 0) "a raising setup was owed a teardown, and got %d"
    !released

(* On the failing path the teardown's exception is dropped: the reported
   failure is the body's. On the passing path there is no failure to
   outrank, so the teardown's is the failure. *)
let a_teardown_failure_never_replaces_the_body_s () =
  let raising exn =
    [
      Stateful.call "boom"
        ~next:(fun model -> model + 1)
        (fun _ () -> raise exn);
    ]
  in
  let failing =
    program_at
      (Stateful.program ~steps:1 ~model:0
         (raising (Failure.Check_failure (Failure.message "the body"))))
      0
  in
  let failure =
    expect_check_failure "a body failure under a raising teardown" (fun () ->
        Stateful.execute
          ~teardown:(fun () -> raise Not_found)
          ~setup:(fun () -> ())
          failing)
  in
  check
    (failure.Failure.kind = Failure.Message "the body")
    "the teardown's exception replaced the body's failure";
  (* Passing path: the teardown's exception is the only one there is, and it
     propagates as itself — [execute] converts nothing outside a step. *)
  (match
     Stateful.execute
       ~teardown:(fun () -> raise Not_found)
       ~setup:(fun () -> ())
       (counter_program 0)
   with
  | exception Not_found -> ()
  | exception exn ->
      failf "a passing-path teardown raised %s" (Printexc.to_string exn)
  | () -> failf "a passing-path teardown failure was swallowed");
  (* Except for the exceptions that end the run: a [Timeout] delivered in a
     candidate's teardown outranks the failure in hand, or the engine would
     accept it as a shrink step and report a converged counterexample. The
     three fatal ones outrank it for the same reason. *)
  List.iter
    (fun (label, exn) ->
      match
        Stateful.execute
          ~teardown:(fun () -> raise exn)
          ~setup:(fun () -> ())
          failing
      with
      | exception raised when raised = exn -> ()
      | exception Failure.Check_failure _ ->
          failf "a %s from a failing path's teardown was dropped" label
      | exception raised ->
          failf "the teardown's %s came back as %s" label
            (Printexc.to_string raised)
      | () -> failf "the failing program did not fail")
    [ ("Timeout", Failure.Timeout 0.5); ("Sys.Break", Sys.Break) ]

(* [setup] runs once per [execute] — so once per generated case and once per
   shrink candidate, counted across a real failing run. *)
let setup_runs_once_per_case_and_per_shrink_candidate () =
  let setups = ref 0 and releases = ref 0 and executions = ref 0 in
  let outcome =
    Property.run ~count:(`Declared 40) ~max_shrink:20 ~root ~path:"lifecycle"
      (queue_gen ()) (fun _ program ->
        incr executions;
        Stateful.execute
          ~teardown:(fun _ -> incr releases)
          ~setup:(fun () ->
            incr setups;
            Bad_queue.create ())
          program)
  in
  let failure, _ = expect_fail outcome in
  let _, case_index, shrink_steps, _, _ = property_payload failure in
  check (!setups = !executions) "%d setups for %d executions" !setups
    !executions;
  check (!releases = !setups) "%d releases for %d setups" !releases !setups;
  check (shrink_steps > 0) "the search took no shrink step";
  check
    (!executions > case_index + 1)
    "%d executions for a failure at case %d — no candidate got its own system"
    !executions case_index

(* The invariant runs on the fresh system before step 1 — which is what
   makes the empty program a real test — and after every step, under labels
   that tell the two apart. *)
let the_invariant_runs_before_step_one_and_after_every_step () =
  let program = counter_program 0 in
  let drawn = names program in
  let total = List.length drawn in
  check (total = 4) "the unconditioned program made %d of 4 calls" total;
  let seen = ref [] in
  Stateful.execute
    ~invariant:(fun model () -> seen := model :: !seen)
    ~setup:(fun () -> ())
    program;
  let expected =
    List.rev
      (List.fold_left
         (fun acc name -> counter_step (List.hd acc) name :: acc)
         [ 0 ] drawn)
  in
  check
    (List.rev !seen = expected)
    "the invariant saw %s, not %s"
    (show_ints (List.rev !seen))
    (show_ints expected);
  (* The empty program is a real test: the fresh-system check still runs. *)
  let empty =
    program_at (Stateful.program ~steps:5 ~model:0 never_commands) 0
  in
  let ran = ref 0 in
  Stateful.execute ~invariant:(fun _ () -> incr ran) ~setup:(fun () -> ()) empty;
  check (!ran = 1) "the empty program ran the invariant %d times" !ran;
  (* Distinct labels, and a user ~msg joined onto them. *)
  let fresh =
    expect_check_failure "the fresh-system invariant" (fun () ->
        Stateful.execute
          ~invariant:(fun _ () -> Check.is_true ~msg:"note" false)
          ~setup:(fun () -> ())
          program)
  in
  check
    (failure_msg fresh = "invariant on the fresh system \u{2014} note")
    "the fresh-system invariant failure was labelled %S" (failure_msg fresh);
  let visits = ref 0 in
  let after =
    expect_check_failure "the post-step invariant" (fun () ->
        Stateful.execute
          ~invariant:(fun _ () ->
            incr visits;
            if !visits = 2 then Check.fail "nope")
          ~setup:(fun () -> ())
          program)
  in
  check
    (failure_msg after
    = Pp.str "invariant after step 1 of %d: %s" total (List.hd drawn))
    "the post-step invariant failure was labelled %S" (failure_msg after)

(* The narrowing and the propagating set are the executor's, not the body's:
   an invariant is held to exactly the same partition, at both of the sites
   it runs from. *)
let an_invariant_is_narrowed_and_propagates_like_a_body () =
  let program = counter_program 0 in
  let drawn = names program in
  let total = List.length drawn in
  check (total = 4) "the unconditioned program made %d of 4 calls" total;
  (* Narrowed into the assertion class, under each site's own label. *)
  let fresh =
    expect_check_failure "a raising fresh-system invariant" (fun () ->
        Stateful.execute
          ~invariant:(fun _ () -> raise Not_found)
          ~setup:(fun () -> ())
          program)
  in
  check
    (failure_msg fresh = "invariant on the fresh system")
    "a raising fresh-system invariant was labelled %S" (failure_msg fresh);
  check
    (raised_actual fresh = "Not_found")
    "a raising fresh-system invariant rendered %S" (raised_actual fresh);
  let visits = ref 0 in
  let after =
    expect_check_failure "a raising post-step invariant" (fun () ->
        Stateful.execute
          ~invariant:(fun _ () ->
            incr visits;
            if !visits = 2 then raise Not_found)
          ~setup:(fun () -> ())
          program)
  in
  check
    (failure_msg after
    = Pp.str "invariant after step 1 of %d: %s" total (List.hd drawn))
    "a raising post-step invariant was labelled %S" (failure_msg after);
  check
    (raised_actual after = "Not_found")
    "a raising post-step invariant rendered %S" (raised_actual after);
  (* And the propagating set escapes an invariant unconverted, from both. *)
  List.iter
    (fun (label, exn) ->
      (match
         Stateful.execute
           ~invariant:(fun _ () -> raise exn)
           ~setup:(fun () -> ())
           program
       with
      | exception raised ->
          check (raised = exn)
            "%s from the fresh-system invariant came back as %s" label
            (Printexc.to_string raised)
      | () -> failf "%s from the fresh-system invariant was swallowed" label);
      let visits = ref 0 in
      match
        Stateful.execute
          ~invariant:(fun _ () ->
            incr visits;
            if !visits = 2 then raise exn)
          ~setup:(fun () -> ())
          program
      with
      | exception raised ->
          check (raised = exn) "%s from a post-step invariant came back as %s"
            label
            (Printexc.to_string raised)
      | () -> failf "%s from a post-step invariant was swallowed" label)
    [
      ("Skip_test", Failure.Skip_test (Some "why"));
      ("Timeout", Failure.Timeout 0.5);
      ("Exit_attempt", Failure.Exit_attempt);
      ("Discard", Property.Discard);
      ("Sys.Break", Sys.Break);
    ]

(* The printer *)

let empty_program_prints_no_commands () =
  let gen = Stateful.program ~steps:5 ~model:0 never_commands in
  let program = program_at gen 0 in
  check (names program = []) "the program was not empty";
  check
    (render gen program = "(no commands)")
    "the empty program rendered %S" (render gen program)

(* The summary comes first, and a step whose argument renders as ["()"] —
   every [call] — prints as its name alone. *)
let unit_arguments_are_suppressed_under_a_summary_line () =
  let gen = Stateful.program ~steps:3 ~model:0 counter_draws in
  let program = program_at gen 0 in
  let drawn = names program in
  let expected =
    Pp.str "3 calls, last: %s" (List.nth drawn 2)
    :: List.mapi (fun index name -> Pp.str "%d  %s" (index + 1) name) drawn
  in
  check
    (lines_of gen program = expected)
    "a nullary program rendered %S" (render gen program);
  let one = Stateful.program ~steps:1 ~model:0 counter_draws in
  let program = program_at one 0 in
  check
    (List.hd (lines_of one program)
    = Pp.str "1 call, last: %s" (List.hd (names program)))
    "a one-call program's summary was %S"
    (List.hd (lines_of one program))

(* The model column is the state each call was made {e in}: a fold of
   [~next] that stops one transition short, so the initial model is visible
   and the last row shows a pre-state too. *)
let the_model_column_shows_the_pre_state () =
  let gen =
    Stateful.program ~steps:12 ~model:0 ~pp_model:Format.pp_print_int
      counter_draws
  in
  let program = program_at gen 0 in
  let drawn = names program in
  let total = List.length drawn in
  check (total = 12) "the unconditioned program made %d of 12 calls" total;
  let cells =
    List.rev
      (snd
         (List.fold_left
            (fun (model, acc) name ->
              (counter_step model name, string_of_int model :: acc))
            (0, []) drawn))
  in
  check
    (List.exists (fun cell -> String.length cell = 2) cells)
    "the model never left one digit — the padding is untested: %s"
    (show_names cells);
  let width =
    List.fold_left (fun w cell -> max w (String.length cell)) 0 cells
  in
  let expected =
    Pp.str "12 calls, last: %s" (List.nth drawn (total - 1))
    :: List.mapi
         (fun index (cell, name) ->
           cell
           ^ String.make (width - String.length cell) ' '
           ^ "  "
           ^ Pp.str "%2d" (index + 1)
           ^ "  " ^ name)
         (List.combine cells drawn)
  in
  check
    (lines_of gen program = expected)
    "the model column rendered as:\n%s\nnot:\n%s" (render gen program)
    (String.concat "\n" expected)

(* A [pp_model] that raises costs its own cell and no more: [Gen.render]
   would collapse the whole program to one marker while [printerless] stays
   false, so no remedy line fires and the reader loses the program. *)
let a_raising_pp_model_costs_one_cell () =
  let pp_model ppf model =
    if model = 2 then raise Not_found else Format.pp_print_int ppf model
  in
  let gen = Stateful.program ~steps:6 ~model:0 ~pp_model tick_commands in
  let program = program_at gen 0 in
  check (List.length (names program) = 6) "the tick program lost calls";
  let marker = "<pp_model raised Not_found>" in
  let cells = [ "0"; "1"; marker; "3"; "4"; "5" ] in
  let width = String.length marker in
  let expected =
    "6 calls, last: tick"
    :: List.mapi
         (fun index cell ->
           cell
           ^ String.make (width - String.length cell) ' '
           ^ "  "
           ^ string_of_int (index + 1)
           ^ "  tick")
         cells
  in
  check
    (lines_of gen program = expected)
    "a raising pp_model rendered:\n%s\nnot:\n%s" (render gen program)
    (String.concat "\n" expected)

(* A poisoned program prints like any other. The model column re-applies
   only the transitions repair itself applied, so the one it skips is the
   last call's — which for a [~next] poison is the transition that raised,
   and evaluating it would raise inside the printer. *)
let a_poisoned_program_prints_its_model_column () =
  let body_ran = ref false in
  let gen =
    Stateful.program ~steps:6 ~model:0 ~pp_model:Format.pp_print_int
      (poison_spec ~body_ran `Next)
  in
  let program = find_poisoned gen in
  let drawn = names program in
  let total = List.length drawn in
  check (total >= 2) "the poisoned program made %d call — nothing precedes it"
    total;
  check
    (drawn = List.init (total - 1) (fun _ -> "benign") @ [ "boom" ])
    "the poisoned program was %s" (show_names drawn);
  let cell_width = String.length (string_of_int (total - 1)) in
  let number_width = String.length (string_of_int total) in
  let expected =
    Pp.str "%d call%s, last: boom" total (if total = 1 then "" else "s")
    :: List.mapi
         (fun index name ->
           let cell = string_of_int index in
           let number = string_of_int (index + 1) in
           cell
           ^ String.make (cell_width - String.length cell) ' '
           ^ "  "
           ^ String.make (number_width - String.length number) ' '
           ^ number ^ "  " ^ name)
         drawn
  in
  check
    (lines_of gen program = expected)
    "a poisoned program rendered:\n%s\nnot:\n%s" (render gen program)
    (String.concat "\n" expected)

(* A model cell is a column, so it is bounded in code points. *)
let a_long_model_cell_truncates () =
  let pp_model ppf _ = Format.pp_print_string ppf (String.make 100 'm') in
  let gen = Stateful.program ~steps:2 ~model:0 ~pp_model tick_commands in
  let program = program_at gen 0 in
  let expected = String.make 57 'm' ^ "..." in
  check
    (lines_of gen program
    = [ "2 calls, last: tick"; expected ^ "  1  tick"; expected ^ "  2  tick" ]
    )
    "a long model cell rendered %S" (render gen program)

(* An argument whose own generator has no printer renders as the placeholder
   [Gen.render] would have used for it; the step names and the program shape
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
  check (Gen.prints gen) "a printerless argument made the program printerless";
  let program = program_at gen 0 in
  check
    (lines_of gen program
    = [
        "2 calls, last: opaque";
        "1  opaque <no printer>";
        "2  opaque <no printer>";
      ])
    "a printerless argument rendered %S" (render gen program)

(* An argument rides the failure payload, so it is bounded in bytes, with a
   marker stating the original size. *)
let a_long_argument_truncates () =
  let big = String.make 300 'x' in
  let commands =
    [
      Stateful.command "write"
        (Gen.constant ~pp:Format.pp_print_string big)
        ~next:(fun model _ -> model)
        (fun _ _ () -> ());
    ]
  in
  let gen = Stateful.program ~steps:2 ~model:0 commands in
  let program = program_at gen 0 in
  let expected = String.make 200 'x' ^ "... (truncated; 300 bytes total)" in
  check
    (lines_of gen program
    = [ "2 calls, last: write"; "1  write " ^ expected; "2  write " ^ expected ]
    )
    "a long argument rendered %S" (render gen program)

(* Hard newlines only: a name and a model cell are each flattened to one
   line, so a step stays one row. *)
let newlines_in_names_and_cells_are_flattened () =
  let commands = [ Stateful.call "two\nlines" ~next:Fun.id (fun _ () -> ()) ] in
  let pp_model ppf model = Format.fprintf ppf "a\nb%d" model in
  let gen = Stateful.program ~steps:2 ~model:0 ~pp_model commands in
  let program = program_at gen 0 in
  check
    (lines_of gen program
    = [ "2 calls, last: two lines"; "a b0  1  two lines"; "a b0  2  two lines" ]
    )
    "newlines survived the printer: %S" (render gen program)

(* A program longer than 40 steps prints its first and last 20 with a
   step-omitted line between; both columns are measured over the rows that
   print. *)
let long_programs_truncate_with_a_step_omitted_line () =
  let gen = Stateful.program ~steps:50 ~model:0 counter_draws in
  let program = program_at gen 0 in
  let drawn = names program in
  check
    (List.length drawn = 50)
    "the unconditioned program made %d of 50 calls" (List.length drawn);
  let row index = Pp.str "%2d  %s" (index + 1) (List.nth drawn index) in
  let expected =
    (Pp.str "50 calls, last: %s" (List.nth drawn 49) :: List.init 20 row)
    @ [ "\u{2026} (10 steps omitted)" ]
    @ List.init 20 (fun index -> row (30 + index))
  in
  check
    (lines_of gen program = expected)
    "a 50-step program rendered:\n%s\nnot:\n%s" (render gen program)
    (String.concat "\n" expected)

(* Both columns are measured over the rows that print, so a wide model cell
   inside the omitted middle indents nothing. *)
let the_model_column_is_measured_over_the_printed_rows () =
  let wide = String.make 20 'w' in
  let pp_model ppf model =
    if model = 25 then Format.pp_print_string ppf wide
    else Format.pp_print_int ppf model
  in
  let gen = Stateful.program ~steps:50 ~model:0 ~pp_model tick_commands in
  let program = program_at gen 0 in
  let total = List.length (names program) in
  check (total = 50) "the tick program made %d of 50 calls" total;
  (* The cell that never prints is the widest one there is: rows 21 to 30
     are omitted, and the model before row 26 is 25. *)
  let row index =
    let cell = string_of_int index in
    cell
    ^ String.make (2 - String.length cell) ' '
    ^ "  "
    ^ Pp.str "%2d" (index + 1)
    ^ "  tick"
  in
  let expected =
    ("50 calls, last: tick" :: List.init 20 row)
    @ [ "\u{2026} (10 steps omitted)" ]
    @ List.init 20 (fun index -> row (30 + index))
  in
  check
    (contains wide (render gen program) = false)
    "the omitted middle's model cell printed after all";
  check
    (lines_of gen program = expected)
    "a truncated program's model column rendered:\n%s\nnot:\n%s"
    (render gen program)
    (String.concat "\n" expected)

(* Malformed arguments are reported at sample time, inside the running
   test's exception boundary. *)
let a_malformed_declaration_raises_at_sample_time () =
  (match Gen.sample (Stateful.program ~steps:4 ~model:0 []) (state 0) with
  | exception Invalid_argument message ->
      check
        (contains "stateful" message)
        "the empty-command error said %S" message
  | _ -> failf "an empty command list sampled successfully");
  (* At [?steps:0] no element is drawn, so the branch-level report never
     fires — a test declaring no commands must not pass vacuously. *)
  (match Gen.sample (Stateful.program ~steps:0 ~model:0 []) (state 0) with
  | exception Invalid_argument message ->
      check
        (contains "stateful" message)
        "the empty-command error at ?steps:0 said %S" message
  | _ -> failf "an empty command list at ?steps:0 sampled successfully");
  match
    Gen.sample (Stateful.program ~steps:(-1) ~model:0 counter_draws) (state 0)
  with
  | exception Invalid_argument _ -> ()
  | _ -> failf "a negative ?steps sampled successfully"

(* Integration *)

(* Through the facade, whose [command] is abstract: this is the surface a
   user meets. Everything [stateful] hands to the declaration layer is
   visible on the flattened case — the tags [--tag] selects on, the
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
      (Windtrap.stateful ~pos ~tags:[ "custom" ] ~timeout:2.5 "spec" ~model:0
         ~setup:(fun () -> ())
         tick_facade)
  in
  let selects tag =
    Tag.accepts (Tag.require tag Tag.default_predicate) case.Test_tree.tags
  in
  check (selects "prop") "--tag prop did not select a stateful test";
  check (selects "stateful") "--tag stateful did not select a stateful test";
  check (selects "custom") "the declared tags were dropped";
  check (not (selects "absent")) "an undeclared tag selected the test";
  check
    (case.Test_tree.path = [ "spec" ])
    "the case was named %s"
    (show_names case.Test_tree.path);
  check
    (case.Test_tree.timeout = Some 2.5)
    "the declared ?timeout did not reach the test node";
  check
    (case.Test_tree.loc = Some (Loc.of_pos pos))
    "the declared ?pos did not reach the test node"

(* [stateful] is [Runner.prop] over [program] with [execute] as its law, and
   the only way to see that wiring is to run the node it declares. The body
   ends by raising the engine's outcome in a constructor [Runner]'s
   interface does not export, so the evidence is the lifecycle the run left
   behind rather than the outcome value. *)
let run_declared_body tree =
  match (flattened tree).Test_tree.body with
  | Test_tree.Bracket _ | Test_tree.Scoped _ ->
      failf "the declared node scopes a resource, not a plain test"
  | Test_tree.Body body -> (
      match body () with
      | () -> failf "the property body returned without an engine outcome"
      | exception ((Failure.Check_failure _ | Invalid_argument _) as raised) ->
          raise raised
      | exception _ -> ())

let stateful_runs_one_fresh_system_per_case_over_steps_calls () =
  let setups = ref 0 and releases = ref 0 in
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
       ~teardown:(fun () -> incr releases)
       ~setup:(fun () -> incr setups)
       commands);
  check (!setups > 0) "the declared body ran no case at all";
  (* The declared ?count is an upper bound here rather than an equality: a
     run may lower it with --max-prop-count, but nothing may raise it, and
     the engine default of 100 would. *)
  check (!setups <= 3) "the declared ?count of 3 ran %d cases" !setups;
  check (!releases = !setups) "%d releases for %d systems" !releases !setups;
  (* Every call of every case is legal, so ?steps is the program's length:
     three bodies and four invariant checks per system. *)
  check
    (!bodies = 3 * !setups)
    "%d bodies over %d systems — ?steps:3 did not reach the generator" !bodies
    !setups;
  check
    (!invariants = 4 * !setups)
    "%d invariant checks over %d systems, not %d" !invariants !setups
    (4 * !setups)

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
       ~setup:(fun () -> ())
       commands);
  check (!seen <> []) "~pp_model never reached the counterexample printer";
  check
    (List.for_all (fun model -> model >= 0 && model <= 2) !seen)
    "the model column printed %s, not the pre-states of a 3-call program"
    (show_ints (List.sort_uniq compare !seen));
  check (List.mem 2 !seen) "the model column stopped short of the last step"

let the_same_seed_reproduces_the_same_counterexample () =
  let once () =
    (* The whole stream, not just where it converged: a stateful test is
       [prop] over a derived generator, so nothing between the seed and the
       law may carry state from one run to the next. *)
    let trace = ref [] in
    let outcome =
      Property.run ~count:(`Declared 40) ~root ~path:"replay" (queue_gen ())
        (fun _ program ->
          trace := names program :: !trace;
          Stateful.execute ~setup:Bad_queue.create program)
    in
    let failure, _ = expect_fail outcome in
    let rendered, case, steps, _, _ = property_payload failure in
    (List.rev !trace, rendered, case, steps)
  in
  (* Both runs happen at one call site: captured assertion locations are
     stack-derived, so distinct call sites would differ there. *)
  let trace, rendered, case, steps = once () in
  let trace', rendered', case', steps' = once () in
  check
    (List.length trace > 20)
    "the run executed %d programs — the trace is too short to be evidence"
    (List.length trace);
  check (trace = trace')
    "the two runs executed %d and %d programs, differing first at %d"
    (List.length trace) (List.length trace')
    (let rec first index = function
       | a :: al, b :: bl -> if a = b then first (index + 1) (al, bl) else index
       | _ -> min (List.length trace) (List.length trace')
     in
     first 0 (trace, trace'));
  check (rendered = rendered') "the counterexample differed:\n%s\nand\n%s"
    rendered rendered';
  check (case = case') "the failing case index differed: %d and %d" case case';
  check (steps = steps') "the shrink step count differed: %d and %d" steps
    steps'

let collect_and_cover_work_inside_command_bodies () =
  let frame = Run.current_frame () in
  let labelled ~unreachable =
    [
      Stateful.call "tick"
        ~next:(fun model -> model + 1)
        (fun model () ->
          Windtrap.collect "ticked";
          Windtrap.classify "past three" (model > 3);
          Windtrap.cover ~label:"reached five" ~at_least:1.
            (if unreachable then model >= 500 else model >= 5));
    ]
  in
  let run ~unreachable path =
    Property.run ~count:(`Declared 10) ~root ~path
      (Stateful.program ~steps:12 ~model:0 (labelled ~unreachable))
      (fun context program ->
        Run.with_prop_context frame context (fun () ->
            Stateful.execute ~setup:(fun () -> ()) program))
  in
  let stats = expect_pass (run ~unreachable:false "labels") in
  check
    (List.assoc_opt "ticked" stats.Property.collected = Some 10)
    "collect from a command body reported %s"
    (show_names (List.map fst stats.Property.collected));
  check
    (List.assoc_opt "past three" stats.Property.collected = Some 10)
    "classify from a command body did not mark every case";
  (match stats.Property.coverage with
  | [ status ] ->
      check
        (status.Property.label = "reached five" && status.Property.satisfied)
        "the coverage requirement was %s at %.1f%%" status.Property.label
        status.Property.actual
  | statuses ->
      failf "expected one coverage entry, got %d" (List.length statuses));
  (* And a requirement registered from a command body can fail the run. *)
  match run ~unreachable:true "labels-unreachable" with
  | Property.Coverage_failed stats -> (
      match stats.Property.coverage with
      | [ status ] ->
          check
            (not status.Property.satisfied)
            "an unreachable requirement reported satisfied"
      | statuses ->
          failf "expected one coverage entry, got %d" (List.length statuses))
  | _ -> failf "an unreachable cover requirement did not fail the run"

(* The generator prints, always — even over a command whose own argument
   generator does not — so a printerless stateful counterexample is
   unreachable and the report's [Gen.with_pp] remedy line never fires. *)
let the_program_generator_always_prints () =
  let opaque =
    [
      Stateful.command "opaque" (Gen.constant 5)
        ~next:(fun model _ -> model)
        (fun _ _ () -> ());
    ]
  in
  check
    (not (Gen.prints (Gen.constant 5)))
    "Gen.constant grew a printer — the test is vacuous";
  check
    (Gen.prints (Stateful.program ~steps:4 ~model:0 opaque))
    "a program over a printerless command carries no printer";
  let outcome =
    Property.run ~count:(`Declared 5) ~root ~path:"printerless"
      (Stateful.program ~steps:4 ~model:0 opaque) (fun _ _ ->
        Check.fail "always")
  in
  let failure, _ = expect_fail outcome in
  let _, _, _, printerless, _ = property_payload failure in
  check (not printerless) "a stateful counterexample reported as printerless";
  check
    (not (contains "Gen.with_pp" (failure_block failure)))
    "the printerless remedy line fired on a stateful counterexample"

(* End to end *)

(* A real buggy system, a real failing run, and the shape of the report the
   reader gets. *)
let a_buggy_system_renders_a_diagnosable_failure () =
  let outcome =
    Property.run ~count:(`Declared 40) ~root ~path:"bad_queue" (queue_gen ())
      (fun _ program ->
        Stateful.execute ~invariant:queue_invariant ~setup:Bad_queue.create
          program)
  in
  let failure, _ = expect_fail outcome in
  let rendered, _, shrink_steps, _, inner = property_payload failure in
  let block = failure_block failure in
  check (shrink_steps > 0) "the search took no shrink step";
  (* The search converges on the shortest disagreement the bug admits: one
     element pops correctly, two do not, and the arguments have to differ or
     the wrong element is the right one. *)
  let expected =
    String.concat "\n"
      [ "3 calls, last: pop"; "1  push 0"; "2  push 1"; "3  pop" ]
  in
  check (rendered = expected) "the counterexample rendered:\n%s\nnot:\n%s"
    rendered expected;
  (match inner with
  | Some inner ->
      check
        (failure_msg inner = "step 3 of 3: pop")
        "the inner failure was labelled %S" (failure_msg inner);
      check
        (inner.Failure.kind
        = Failure.Equality { expected = "0"; actual = "1"; not_ = false })
        "the inner failure is not the body's own equality"
  | None -> failf "the counterexample reported no inner failure");
  List.iter
    (fun needle ->
      check (contains needle block) "the rendered failure lacks %S:\n%s" needle
        block)
    [
      "counterexample (";
      "3 calls, last: pop";
      "which failed at:";
      "step 3 of 3: pop";
      "expected  0";
      "actual    1";
      "replay:";
    ]

(* The suite *)

let suite =
  [
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
    ( "a raising ~pre poisons and withholds the body",
      a_raising_pre_poisons_and_withholds_the_body );
    ( "a raising ~next poisons and runs the body",
      a_raising_next_poisons_and_runs_the_body );
    ( "a failing body outranks the ~next poison it precedes",
      a_failing_body_outranks_the_next_poison_it_precedes );
    ( "a poisoned program carries the declaration site",
      a_poisoned_program_carries_the_declaration_site );
    ( "control exceptions escape ~pre and ~next unconverted",
      control_exceptions_escape_pre_and_next_unconverted );
    ( "a failing step points at its command",
      a_failing_step_points_at_its_command );
    ( "assertions, skips and discards from ~pre poison",
      assertions_skips_and_discards_from_pre_poison );
    ( "control exceptions escape a body unconverted",
      control_exceptions_escape_a_body_unconverted );
    ("teardown runs on every path", teardown_runs_on_every_path);
    ( "a teardown failure never replaces the body's",
      a_teardown_failure_never_replaces_the_body_s );
    ( "setup runs once per case and per shrink candidate",
      setup_runs_once_per_case_and_per_shrink_candidate );
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
    ( "a poisoned program prints its model column",
      a_poisoned_program_prints_its_model_column );
    ("a long model cell truncates", a_long_model_cell_truncates);
    ( "a printerless argument degrades to a placeholder",
      a_printerless_argument_degrades_to_a_placeholder );
    ("a long argument truncates", a_long_argument_truncates);
    ( "newlines in names and cells are flattened",
      newlines_in_names_and_cells_are_flattened );
    ( "long programs truncate with a step-omitted line",
      long_programs_truncate_with_a_step_omitted_line );
    ( "the model column is measured over the printed rows",
      the_model_column_is_measured_over_the_printed_rows );
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
    ( "collect and cover work inside command bodies",
      collect_and_cover_work_inside_command_bodies );
    ("the program generator always prints", the_program_generator_always_prints);
    ( "a buggy system renders a diagnosable failure",
      a_buggy_system_renders_a_diagnosable_failure );
  ]

let tests = List.map (fun (name, fn) -> test name fn) suite
