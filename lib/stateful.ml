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
      loc : Loc.t option;
    }
      -> ('model, 'sut) command

(* The declaration site, captured here rather than at the failure: a body is
   idiomatically one assertion in tail position, whose frame is gone by the
   time it raises, so [Loc.capture] there answers [None] and the step is
   reported without a location. Capturing where the command is written
   points the report at the code that failed, which is the same fallback
   [Run.add_failure] makes for a test — one level finer. *)
let command ?__POS__ ?(pre = fun _ _ -> true) name gen ~next body =
  Command { name; gen; pre; next; body; loc = Loc.resolve ?__POS__ () }

(* [call] is [command] at ['arg = unit] over [Gen.unit], whose printer is
   what lets a nullary step print as its name alone. *)
let call ?__POS__ ?pre name ~next body =
  (* [?__POS__] forwards; without one, [command]'s own capture walks past
     both of these frames — they are windtrap's — and lands on the caller. *)
  command ?__POS__
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
  loc : Loc.t option;
  arg : string Lazy.t;
  pre : 'model -> bool;
  next : 'model -> 'model;
  body : 'model -> 'sut -> unit;
}

(* Programs *)

type ('model, 'sut) program = {
  initial : 'model;
  calls : ('model, 'sut) call list;
}

(* Exception classes

   What a *body* raises and this module never converts nor swallows: the
   five control exceptions, each a statement about the run rather than about
   this program, and the three no failure boundary may absorb. Converting
   [Skip_test] would make a skip a reported counterexample, converting
   [Property.Discard] would break [assume] inside a body, and converting
   [Timeout] would defeat the shrink search's deadline. *)
let propagates = function
  | Failure.Check_failure _ | Failure.Skip_test _ | Failure.Timeout _
  | Failure.Exit_attempt | Property.Discard ->
      true
  | exn -> Failure.is_fatal exn

(* A [~pre] or [~next] that raises is a specification bug, not a
   counterexample. It escapes into the generator — the engine reports it
   once, unshrunk, with its backtrace — wrapped so the report names the
   operation, the step and the function. Only the exceptions about the run
   rather than the model escape as themselves. *)
exception
  Specification_raised of {
    name : string;
    step : int;
    phase : string;
    exn : exn;
  }

let () =
  Printexc.register_printer (function
    | Specification_raised { name; step; phase; exn } ->
        Some
          (Pp.str "step %d: %s \u{2014} %s raised %s" step name phase
             (Printexc.to_string exn))
    | _ -> None)

let about_the_run = function
  | Failure.Timeout _ | Failure.Exit_attempt -> true
  | exn -> Failure.is_fatal exn

let specification ~name ~step ~phase f =
  match f () with
  | result -> result
  | exception exn when not (about_the_run exn) ->
      Printexc.raise_with_backtrace
        (Specification_raised { name; step; phase; exn })
        (Printexc.get_raw_backtrace ())

(* Repair

   The model-threading fold that decides which drawn calls a program makes:
   a call is kept iff its [~pre] holds in the model the calls before it
   produced, and [~next] threads through the kept ones only. It answers with
   a mask over the calls it was given, which [program] applies before it
   assembles the shrink tree and again at every node. *)
let repair model calls =
  let rec go model step = function
    | [] -> []
    | call :: rest ->
        let name = call.name in
        if specification ~name ~step ~phase:"~pre" (fun () -> call.pre model)
        then
          let model =
            specification ~name ~step ~phase:"~next" (fun () -> call.next model)
          in
          true :: go model (step + 1) rest
        else false :: go model step rest
  in
  go model 1 calls

let rec select mask items =
  match (mask, items) with
  | true :: mask, item :: items -> item :: select mask items
  | false :: mask, _ :: items -> select mask items
  | _ -> []

(* The program printer *)

let empty_program = "(no commands)"

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

(* [Gen.Private.render] collapses a raising printer to one [<printer raised ...>]
   for the whole value, which would cost the reader the entire program while
   the rendering stays a value, so no remedy line fires. One bad cell must
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
   raising; the one it skips is the last call's, whose result no row would
   show. *)
let model_cells pp_model program =
  let rec go model = function
    | [] -> []
    | [ _last ] -> [ cell pp_model model ]
    | call :: rest -> cell pp_model model :: go (call.next model) rest
  in
  go program.initial program.calls

let argument call =
  match one_line (Lazy.force call.arg) with
  (* The printer's only type-blind special case: a command with no
     generated argument reads [3  pop], not [3  pop ()]. *)
  | "()" -> None
  | text -> Some (Text.truncate_bytes_utf8 argument_bytes text)

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
  (* The empty program is a reachable counterexample — a [~scope] that
     raises while acquiring, or an [?invariant] that rejects the fresh
     system, shrinks to it in one step — and an empty rendering would take
     the renderer's single-line branch and print a bare colon. *)
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
let branch (Command { name; gen; pre; next; body; loc }) =
  let name = one_line name in
  Gen.map
    (fun argument ->
      {
        name;
        loc;
        arg = lazy (Gen.Private.render_value gen argument);
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

(* [steps] calls drawn with [Gen.list]'s default-size move set — so the
   length shrinks by chunk deletion — under a mask no combinator expresses:
   repair runs on the drawn calls before the tree is assembled, so a dropped
   call contributes no subtree at all, and again at every node, so a call
   that a deletion elsewhere invalidates is dropped in the same candidate.
   Masking on top of an assembled tree gets both wrong: a candidate deleting
   a call the mask already dropped equals its parent, and one reducing a
   dropped call's argument into a kept one is longer than its parent. *)
let program ?(steps = default_steps) ?pp_model ~model commands =
  let element = choice commands in
  let keep = repair model in
  Gen.Private.make ~pp:(pp_program ?pp_model) (fun state ->
      (* [?steps:0] draws no element, so the branch-level report never
         fires; a test declaring no commands must not pass vacuously. *)
      (match commands with [] -> invalid_arg no_commands | _ :: _ -> ());
      if steps < 0 then invalid_arg "Windtrap.stateful: negative steps";
      let rec draw remaining trees state =
        if remaining = 0 then (List.rev trees, state)
        else
          let tree, state = Gen.Private.run element state in
          draw (remaining - 1) (tree :: trees) state
      in
      let trees, state = draw steps [] state in
      let trees = select (keep (List.map Shrink_tree.root trees)) trees in
      let tree =
        Shrink_tree.map
          (fun calls -> { initial = model; calls = select (keep calls) calls })
          (Shrink_tree.list trees)
      in
      (tree, state))

(* Step attribution

   The failing step is named in the [msg] slot, which renders as a single
   line, so a user [?msg] is flattened and joined into it. This is data
   construction, not rendering. *)

let relabel ?loc label (failure : Failure.t) =
  let msg =
    match failure.Failure.msg with
    | None -> label
    | Some user -> label ^ " \u{2014} " ^ one_line user
  in
  (* The command's declaration site fills in only where the assertion left
     none — which is the common case, since a body is idiomatically one
     assertion in tail position. A body that did record its own site keeps
     it: it is nearer the failure than the declaration is. *)
  let loc = match failure.Failure.loc with None -> loc | some -> some in
  { failure with Failure.msg = Some msg; loc }

(* [label] is a thunk: it is spelled once per failure, not once per step. *)
let attributed ?loc label fn =
  match fn () with
  | () -> ()
  | exception Failure.Check_failure failure ->
      Printexc.raise_with_backtrace
        (Failure.Check_failure (relabel ?loc (label ()) failure))
        (Printexc.get_raw_backtrace ())

let at_step ?(after = false) ?loc ~name ~step ~total fn =
  attributed ?loc
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

let run_program ?invariant program sut =
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
    | call :: rest ->
        (* [normalize] runs inside [at_step], so a system exception is
           converted to the assertion class before the step prefix is
           attached and both carry the same label. *)
        at_step ?loc:call.loc ~name:call.name ~step ~total (fun () ->
            normalize (fun () -> call.body model sut));
        (* Repair already evaluated this transition without raising. *)
        let model = call.next model in
        at_step ~after:true ?loc:call.loc ~name:call.name ~step ~total
          (fun () -> normalize (fun () -> check model));
        go (step + 1) model rest
  in
  go 1 program.initial program.calls

(* Exceptions that describe the run rather than this program, and are
   therefore worth more than the failure in hand: the timeout that ends the
   whole property — swallowing it would leave the shrink search running past
   a deadline the alarm has already spent — and the three no boundary may
   absorb. *)
let ends_the_run exn =
  match exn with Failure.Timeout _ -> true | exn -> Failure.is_fatal exn

(* The two ways a scope can fail its side of the contract, and they are
   different in kind. A scope that never runs the program fails the case:
   a program that did not run is not a passing program, and silently green
   is the worst outcome available here. A second call is the harness
   itself being wrong — one execution is what the whole case is keyed by,
   and the system the first call used is spent — so it is
   [Invalid_argument] at the call. The engine classifies that like any
   exception, so the search re-runs the broken scope and converges on the
   empty program: accurate — a scope that calls back twice does so
   whatever the program says — and the message, not the counterexample,
   is what diagnoses it. No non-ASCII in [called_twice]:
   [Printexc.to_string] renders [Invalid_argument] payloads with [%S]. *)
let no_program =
  "the scope returned without running the program — a scope must call its \
   callback exactly once"

let called_twice =
  "Windtrap.stateful: the scope called its callback twice; a scope must call \
   it exactly once"

let execute ?loc ?invariant ~scope program =
  let entries = ref 0 in
  let misused = ref None in
  let failed = ref None in
  let run sut =
    incr entries;
    if !entries > 1 then begin
      (* Recorded as well as raised: a scope that swallows it would
         otherwise report a program that ran twice as a pass. *)
      let exn = Invalid_argument called_twice in
      misused := Some exn;
      raise exn
    end;
    match run_program ?invariant program sut with
    | () -> ()
    | exception exn ->
        (* Recorded before it is re-raised through [scope]'s frames, so a
           scope that cancels or releases on the exception path sees it and
           one that swallows it cannot turn a failing case green. *)
        let backtrace = Printexc.get_raw_backtrace () in
        failed := Some (exn, backtrace);
        Printexc.raise_with_backtrace exn backtrace
  in
  let escaped =
    match scope run with
    | () -> None
    | exception exn -> Some (exn, Printexc.get_raw_backtrace ())
  in
  match (!misused, escaped, !failed) with
  (* The harness being wrong outranks whatever else the case had to say,
     and it keeps the backtrace of the second call when the scope let it
     out — which is the one frame a reader needs. *)
  | Some exn, Some (raised, backtrace), _ when raised == exn ->
      Printexc.raise_with_backtrace raised backtrace
  | Some exn, _, _ -> raise exn
  | None, None, None ->
      if !entries = 0 then
        raise (Failure.Check_failure (Failure.message ?loc no_program))
  | None, None, Some (exn, backtrace) ->
      (* [scope] swallowed the program's failure; [execute] does not. *)
      Printexc.raise_with_backtrace exn backtrace
  | None, Some (exn, backtrace), None ->
      (* The scope's own, and unconverted either way: before the callback
         it is an acquisition that failed — or a skip declining a system
         the machine cannot build — and after it returned it is a release
         that failed with no failure in hand to outrank. *)
      Printexc.raise_with_backtrace exn backtrace
  | None, Some (exn, backtrace), Some (failure, failure_backtrace) ->
      (* A release that raised over a failing program. The cleanup error
         must not replace the counterexample, and a [Failure.Timeout]
         hidden behind one would be accepted by the engine as a shrink
         step and reported as a converged, minimal counterexample. So the
         program's failure is the failure, except for the exceptions that
         end the whole run and outrank it — and except for itself, on its
         way out through the scope. *)
      if exn == failure || ends_the_run exn then
        Printexc.raise_with_backtrace exn backtrace
      else Printexc.raise_with_backtrace failure failure_backtrace

(* The entry point *)

(* Stateful tests carry both tags: ["prop"] because they are properties —
   [--tag prop] selects them and the run header prints the root seed exactly
   when the suite declares one — and ["stateful"] so a suite can select or
   exclude them on their own cost profile. *)
let prop_tag = "prop"
let stateful_tag = "stateful"

let stateful ?__POS__ ?tags ?timeout ?count ?steps ?pp_model ?invariant name
    ~model ~scope commands =
  (* The declaration site, for the one failure with no site of its own: a
     scope that never ran the program. *)
  let loc = Loc.resolve ?__POS__ () in
  let tags = prop_tag :: stateful_tag :: Option.value ~default:[] tags in
  Runner.prop ?__POS__ ~tags ?timeout ?count name
    (program ?steps ?pp_model ~model commands) (fun program ->
      execute ?loc ?invariant ~scope program)
