(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Commands

   ['arg] is existential because a command list is heterogeneous in its
   arguments. ['res] is not a parameter at all: the body produces and checks
   a call's result in one expression, so no result ever crosses a boundary
   and there is no universe for it to belong to. *)

type ('model, 'sut) command =
  | Command : {
      name : string;
      gen : 'arg Gen.t;
      pre : 'model -> 'arg -> bool;
      next : 'model -> 'arg -> 'model;
      body : 'model -> 'arg -> 'sut -> unit;
    }
      -> ('model, 'sut) command

let command ?(pre = fun _ _ -> true) name gen ~next body =
  Command { name; gen; pre; next; body }

(* [call] is [command] at ['arg = unit], and [Gen.unit] rather than
   [Gen.pure ()] is load-bearing: [pure] without [?pp] carries no printer,
   [frequency] derives one only when every branch has one, and one nullary
   command would then make the whole program printerless. *)
let call ?pre name ~next body =
  command
    ?pre:(Option.map (fun pre model () -> pre model) pre)
    name Gen.unit
    ~next:(fun model () -> next model)
    (fun model () sut -> body model sut)

(* Calls

   One drawn call: the command's five facts with the argument already bound
   into each of them, which discharges the existential at the one point
   where ['arg] is still in scope. [arg] is the argument's rendering,
   deferred — a program is drawn, executed and discarded thousands of times
   per failing test and printed once. *)

type ('model, 'sut) call = {
  name : string;
  arg : string option Lazy.t;
  pre : 'model -> bool;
  next : 'model -> 'model;
  body : 'model -> 'sut -> unit;
}

(* Programs *)

(* Which of the two functions repair evaluates raised. *)
type poisoned_phase = Pre | Next

type poison = {
  command : string;
  step : int; (* 1-based, and always the program's last step *)
  phase : poisoned_phase;
  exn : exn;
  backtrace : string option;
}

type ('model, 'sut) program = {
  initial : 'model;
  calls : ('model, 'sut) call list;
  poison : poison option;
}

let command_names program = List.map (fun call -> call.name) program.calls

(* Exception classes

   The exceptions this module never converts and never swallows: the five
   control exceptions, each a statement about the run rather than about this
   program, and the three no failure boundary may absorb. One partition,
   shared by repair and by [normalize], so that a [Timeout] delivered inside
   a precondition and one delivered inside a body mean the same thing.
   Converting [Skip_test] would make a skip a reported counterexample,
   converting [Property.Discard] would break [assume] inside a body, and
   converting [Timeout] would defeat the shrink search's deadline. *)
let propagates = function
  | Failure.Check_failure _ | Failure.Skip_test _ | Failure.Timeout _
  | Failure.Exit_attempt | Property.Discard ->
      true
  | exn -> Failure.is_fatal exn

(* Repair

   The model-threading fold that decides which drawn calls a program makes:
   a call is kept iff its [~pre] holds in the model the calls before it
   produced, and [~next] threads through the kept ones only. It returns the
   three things its two callers need — the mask [Gen.list_exact] applies
   before it assembles the shrink tree and again at every node, the calls
   that survive, and the poison.

   It is total on everything [propagates] does not name. A [~pre] or
   [~next] that raises stops the fold at its own step, which is kept as the
   program's last call and marked; letting the exception escape instead
   would put it inside the generator, where sampling reports
   [<generator raised before producing a value>] and forcing a candidate
   abandons the entire remaining sibling sequence and reports the truncated
   descent as converged — a specification bug reading as an engine result. *)
let repair model calls =
  let poisoned call phase exn step rest =
    (* Read before anything else can raise. *)
    let backtrace = Failure.recorded_backtrace () in
    ( true :: List.map (fun _ -> false) rest,
      [ call ],
      Some { command = call.name; step; phase; exn; backtrace } )
  in
  let rec go model step = function
    | [] -> ([], [], None)
    | call :: rest -> (
        match call.pre model with
        | exception exn when not (propagates exn) ->
            poisoned call Pre exn step rest
        | false ->
            let mask, kept, poison = go model step rest in
            (false :: mask, kept, poison)
        | true -> (
            match call.next model with
            | exception exn when not (propagates exn) ->
                poisoned call Next exn step rest
            | model ->
                let mask, kept, poison = go model (step + 1) rest in
                (true :: mask, call :: kept, poison)))
  in
  go model 1 calls

(* [Gen.list_exact]'s mask. It is idempotent, as that interface requires:
   re-running the fold on the calls it kept re-derives the same trajectory,
   under which every one of them is legal. *)
let keep model calls =
  let mask, _, _ = repair model calls in
  mask

(* The masked call list read as a program. The fold runs a second time —
   [?keep] answers with a mask, which cannot carry the poison or the model
   trajectory — and on the masked list it drops nothing, so the calls it
   returns are the calls it was given, truncated at a poisoned step. *)
let interpret model calls =
  let _, kept, poison = repair model calls in
  { initial = model; calls = kept; poison }

(* The program printer *)

let empty_program = "(no commands)"

(* [Gen.render_value] answers [None] rather than a placeholder when the
   argument's own generator has no printer, so the column spells the one
   [Gen.render] would have used for it. *)
let missing_printer = "<no printer>"

(* Self-bounding. An argument rides the failure payload, which is capped at
   64 KiB while the terminal block prints every line it is given, so it is
   bounded in bytes; a model cell is a column, so it is bounded in code
   points. A program longer than [step_cap] steps prints its first and last
   [context_steps]. *)
let argument_bytes = 200
let model_cell_chars = 60
let step_cap = 40
let context_steps = 20

let one_line text =
  String.concat " " (Text.split_lines (Text.normalize_newlines text))

let plural count = if count = 1 then "" else "s"

(* [Gen.render] collapses a raising printer to one [<printer raised ...>]
   for the whole value, which would cost the reader the entire program while
   [printerless] stays false, so no remedy line fires. One bad cell must
   cost one cell. *)
let cell pp_model model =
  let text =
    match Format.asprintf "%a" pp_model model with
    | text -> text
    | exception exn -> Pp.str "<pp_model raised %s>" (Printexc.to_string exn)
  in
  Text.truncate_utf8 model_cell_chars (one_line text)

(* The model before each step: a fold of [~next] over the program, with no
   execution recording, so the column is present on every row including the
   failing one and the initial model is visible. It re-applies only the
   transitions repair itself applied, to the same states and without
   raising; the one it skips is the last call's, which is the transition a
   poison stopped at and the one whose result no row would show. *)
let model_cells pp_model program =
  let rec go model = function
    | [] -> []
    | [ _last ] -> [ cell pp_model model ]
    | call :: rest -> cell pp_model model :: go (call.next model) rest
  in
  go program.initial program.calls

let argument call =
  match Lazy.force call.arg with
  | None -> Some missing_printer
  | Some text -> (
      match one_line text with
      (* The printer's only type-blind special case: a command with no
         generated argument reads [3  pop], not [3  pop ()]. *)
      | "()" -> None
      | text -> Some (Text.truncate_bytes_utf8 argument_bytes text))

let step_text call =
  match argument call with
  | None -> call.name
  | Some argument -> call.name ^ " " ^ argument

let pad_right width text =
  text ^ String.make (max 0 (width - Text.length_utf8 text)) ' '

let pad_left width text =
  String.make (max 0 (width - Text.length_utf8 text)) ' ' ^ text

let widest texts =
  List.fold_left (fun width text -> max width (Text.length_utf8 text)) 0 texts

(* Layout is this module's own: hard newlines only, columns padded here.
   [Gen] renders through [Format.asprintf] at the default 78-column margin,
   so a printer relying on soft breaks would have the engine re-wrap the
   program behind its back. Both columns are measured over the rows that
   print, so a wide model cell inside the omitted middle indents nothing. *)
let program_text ?pp_model program =
  match program.calls with
  (* The empty program is a reachable counterexample — a [~setup] that
     raises, or an [?invariant] that rejects the fresh system, shrinks to it
     in one step — and an empty rendering would take the renderer's
     single-line branch and print a bare colon. *)
  | [] -> empty_program
  | calls ->
      let total = List.length calls in
      let cells =
        match pp_model with
        | None -> List.map (fun _ -> None) calls
        | Some pp_model -> List.map Option.some (model_cells pp_model program)
      in
      let steps =
        List.mapi
          (fun index (call, cell) -> (index + 1, cell, call))
          (List.combine calls cells)
      in
      let head, omitted, tail =
        if total <= step_cap then (steps, 0, [])
        else
          ( List.filteri (fun index _ -> index < context_steps) steps,
            total - (2 * context_steps),
            List.filteri (fun index _ -> index >= total - context_steps) steps
          )
      in
      let number_width = String.length (string_of_int total) in
      let cell_width =
        widest (List.filter_map (fun (_, cell, _) -> cell) (head @ tail))
      in
      let row (number, cell, call) =
        let model =
          match cell with
          | None -> ""
          | Some cell -> pad_right cell_width cell ^ "  "
        in
        model
        ^ pad_left number_width (string_of_int number)
        ^ "  " ^ step_text call
      in
      let omission =
        if omitted = 0 then []
        else [ Pp.str "\u{2026} (%d step%s omitted)" omitted (plural omitted) ]
      in
      (* The summary comes first, and says [last], not [failing at]: the
         printer is a pure function of the program and does not know which
         step failed. [Render.headline] flattens newlines and truncates to
         60 code points, and that headline is the JUnit message attribute
         and the [-v] one-liner. *)
      let summary =
        Pp.str "%d call%s, last: %s" total (plural total)
          (List.nth calls (total - 1)).name
      in
      String.concat "\n"
        ((summary :: List.map row head) @ omission @ List.map row tail)

let pp_program ?pp_model ppf program =
  Format.pp_print_string ppf (program_text ?pp_model program)

(* The generator *)

let default_steps = 20
let no_commands = "Windtrap.stateful: no commands to draw from"

(* One branch per command: the argument generator with the command's facts
   bound into a call. [Gen.map] loses the printer, deliberately and
   harmlessly — the program printer replaces it at the top. *)
let branch (Command { name; gen; pre; next; body }) =
  let name = one_line name in
  Gen.map
    (fun argument ->
      {
        name;
        arg = lazy (Gen.render_value gen argument);
        pre = (fun model -> pre model argument);
        next = (fun model -> next model argument);
        body = (fun model sut -> body model argument sut);
      })
    gen

(* Weight 1 per branch, and [frequency] rather than [one_of] because
   [frequency]'s choice itself does not shrink: shrinking never turns one
   command into another, and the order of the command list carries no
   meaning — it is a list, not a priority.

   An empty command list has no branch to draw. Reporting that at sample
   time puts it inside the running test's exception boundary, and naming
   [stateful] beats naming a combinator the user did not write. *)
let choice commands =
  match commands with
  | [] -> Gen.map (fun () -> invalid_arg no_commands) Gen.unit
  | commands ->
      Gen.frequency (List.map (fun command -> (1, branch command)) commands)

let program ?(steps = default_steps) ?pp_model ~model commands =
  let read calls =
    (* [?steps:0] draws no element, so the empty-list report above never
       fires; a test declaring no commands must not pass vacuously. *)
    (match commands with [] -> invalid_arg no_commands | _ :: _ -> ());
    interpret model calls
  in
  Gen.list_exact ~keep:(keep model) steps (choice commands)
  |> Gen.map read
  |> Gen.with_pp (pp_program ?pp_model)

(* Step attribution

   The failing step is named in the [msg] slot, which renders as a single
   line, so a user [?msg] is flattened and joined into it. This is data
   construction, not rendering. *)

let relabel label (failure : Failure.t) =
  let msg =
    match failure.Failure.msg with
    | None -> label
    | Some user -> label ^ " \u{2014} " ^ one_line user
  in
  { failure with Failure.msg = Some msg }

(* [label] is a thunk: it is spelled once per failure, not once per step. *)
let attributed label fn =
  match fn () with
  | () -> ()
  | exception Failure.Check_failure failure ->
      Printexc.raise_with_backtrace
        (Failure.Check_failure (relabel (label ()) failure))
        (Printexc.get_raw_backtrace ())

let at_step ?(after = false) ~name ~step ~total fn =
  attributed
    (fun () ->
      if after then Pp.str "invariant after step %d of %d: %s" step total name
      else Pp.str "step %d of %d: %s" step total name)
    fn

(* Exception-class narrowing

   The engine's shrink acceptance distinguishes exactly two classes, so a
   descent that starts at an assertion failure rejects every candidate
   failing by an exception, and vice versa. Re-raising a non-control,
   non-fatal exception as a [Check_failure] carrying the payload
   [Property.inner_failure] would have built keeps the report byte-identical
   and changes only the class. *)
let normalize fn =
  match fn () with
  | () -> ()
  | exception exn when propagates exn ->
      Printexc.raise_with_backtrace exn (Printexc.get_raw_backtrace ())
  | exception exn ->
      let backtrace = Failure.recorded_backtrace () in
      raise
        (Failure.Check_failure
           (Failure.raised ~actual:(Printexc.to_string exn) ?backtrace ()))

(* The executor *)

let poison_failure ?loc ~total poison =
  let phase = match poison.phase with Pre -> "~pre" | Next -> "~next" in
  Failure.raised ?loc
    ~msg:
      (Pp.str "step %d of %d: %s \u{2014} %s raised" poison.step total
         poison.command phase)
    ~actual:(Printexc.to_string poison.exn)
    ?backtrace:poison.backtrace ()

let run_program ?loc ?invariant program sut =
  let total = List.length program.calls in
  let check model =
    match invariant with None -> () | Some invariant -> invariant model sut
  in
  (* On the freshly created system, before step 1: it is what makes the
     empty program a real test. *)
  attributed
    (fun () -> "invariant on the fresh system")
    (fun () -> normalize (fun () -> check program.initial));
  let rec go step model = function
    | [] -> ()
    | call :: rest -> (
        match program.poison with
        | Some poison when poison.step = step ->
            (* A [~pre] poison means the call is not known to be legal, so
               its body must not run; a [~next] poison means [~pre] held and
               only the model after the call is unknown, so it does. Either
               way the step is the program's last and no model follows it. *)
            if poison.phase = Next then
              at_step ~name:call.name ~step ~total (fun () ->
                  normalize (fun () -> call.body model sut));
            raise (Failure.Check_failure (poison_failure ?loc ~total poison))
        | Some _ | None ->
            (* [normalize] runs inside [at_step], so a system exception is
               converted to the assertion class before the step prefix is
               attached and both carry the same label. *)
            at_step ~name:call.name ~step ~total (fun () ->
                normalize (fun () -> call.body model sut));
            (* Repair already evaluated this transition without raising. *)
            let model = call.next model in
            at_step ~after:true ~name:call.name ~step ~total (fun () ->
                normalize (fun () -> check model));
            go (step + 1) model rest)
  in
  go 1 program.initial program.calls

(* Exceptions that describe the run rather than this program, and are
   therefore worth more than the failure in hand: the timeout that ends the
   whole property — swallowing it would leave the shrink search running past
   a deadline the alarm has already spent — and the three no boundary may
   absorb. *)
let ends_the_run exn =
  match exn with Failure.Timeout _ -> true | exn -> Failure.is_fatal exn

let execute ?loc ?invariant ?teardown ~setup program =
  let sut = setup () in
  let release () =
    match teardown with None -> () | Some teardown -> teardown sut
  in
  (* Never [Fun.protect] at this boundary. It raises [Fun.Finally_raised] in
     place of the work exception, which would replace the counterexample's
     assertion with a cleanup error and hide a [Failure.Timeout] from the
     engine's shrink acceptance — an alarm delivered inside a candidate's
     teardown would then be accepted as a shrink step and reported as a
     converged, minimal counterexample. A teardown failure is reported only
     when the body succeeded; on the failing path the teardown's own
     exception is dropped, matching the engine's one-failure-per-case
     shape. *)
  match run_program ?loc ?invariant program sut with
  | () -> release ()
  | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      (match release () with
      | () -> ()
      | exception released when not (ends_the_run released) -> ());
      Printexc.raise_with_backtrace exn backtrace

(* The entry point *)

(* Stateful tests carry both tags: ["prop"] because they are properties —
   [--tag prop] selects them and the run header prints the root seed exactly
   when the suite declares one — and ["stateful"] so a suite can select or
   exclude them on their own cost profile. *)
let prop_tag = "prop"
let stateful_tag = "stateful"

let stateful ?pos ?tags ?timeout ?count ?steps ?pp_model ?invariant ?teardown
    name ~model ~setup commands =
  (* The declaration site, for the one failure with no site of its own: a
     poisoned program's, since [command] records no [?pos] and the command's
     name is its identity in the report. *)
  let loc = Loc.resolve ?pos () in
  let tags = prop_tag :: stateful_tag :: Option.value ~default:[] tags in
  Runner.prop ?pos ~tags ?timeout ?count name
    (program ?steps ?pp_model ~model commands) (fun program ->
      execute ?loc ?invariant ?teardown ~setup program)
