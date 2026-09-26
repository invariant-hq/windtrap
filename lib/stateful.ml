(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

module Shrink_tree = Gen.Engine.Shrink_tree

(* Commands *)

(* ['arg] is existential, so one list holds commands of every argument type.
   A body checks its call's result itself, so no result type is a
   parameter. *)
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

let command ?__POS__ ?(pre = fun _ _ -> true) name gen ?(next = fun m _ -> m)
    body =
  Command { name; gen; pre; next; body; loc = Loc.resolve ?__POS__ () }

let call ?__POS__ ?pre name ?next body =
  command ?__POS__
    ?pre:(Option.map (fun pre model () -> pre model) pre)
    name Gen.unit
    ?next:(Option.map (fun next model () -> next model) next)
    (fun model () sut -> body model sut)

(* Programs *)

(* A drawn call: its command's functions with the argument bound, where the
   existential ends. [arg] is rendered on demand, since a program is drawn
   and run thousands of times per failing test and printed once. *)
type ('model, 'sut) call = {
  command : int; (* the command's first position in the list *)
  name : string;
  loc : Loc.t option;
  arg : string Lazy.t;
  pre : 'model -> bool;
  next : 'model -> 'model;
  body : 'model -> 'sut -> unit;
}

type ('model, 'sut) program = {
  initial : 'model;
  calls : ('model, 'sut) call list;
}

(* Repair *)

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
          (Pp.str "call %d: %s, %s raised %s" step name phase
             (Failure.exn_to_string exn))
    | _ -> None)

let specification ~name ~step ~phase f =
  match Failure.catch f with
  | Ok result -> result
  | Error (`Exception (exn, backtrace)) ->
      Printexc.raise_with_backtrace
        (Specification_raised { name; step; phase; exn })
        backtrace
  | Error (`Assertion failure) ->
      raise
        (Specification_raised
           { name; step; phase; exn = Failure.Check_failure failure })
  | Error (#Failure.control as c) -> Failure.reraise c

(* [repair model ~call items] is the [items] whose calls repair keeps, [step]
   counting the kept calls from one. *)
let repair model ~call items =
  let rec keep model step = function
    | [] -> []
    | item :: items ->
        let { name; pre; next; _ } = call item in
        if specification ~name ~step ~phase:"~pre" (fun () -> pre model) then
          let model =
            specification ~name ~step ~phase:"~next" (fun () -> next model)
          in
          item :: keep model (step + 1) items
        else keep model step items
  in
  keep model 1 items

(* Printing *)

(* An argument rides the failure payload, which is capped at 64 KiB, so it
   is bounded in bytes; a model cell is a column, so it is bounded in code
   points. A longer program prints its first and last [context_calls]. *)
let argument_bytes = 200
let model_cell_chars = 60
let context_calls = 20

let one_line text =
  String.concat " " (Text.split_lines (Text.normalize_newlines text))

let call_count n = Pp.str "%d call%s" n (if n = 1 then "" else "s")
let pad width text = String.make (width - Text.length_utf8 text) ' '

let step_text call =
  match one_line (Lazy.force call.arg) with
  (* [Gen.unit] prints [()], so a command made by [call] prints its name. *)
  | "()" -> call.name
  | arg -> call.name ^ " " ^ Text.truncate_bytes_utf8 argument_bytes arg

(* Guarded here: the engine's guard would replace the whole table. *)
let cell pp_model model =
  let text =
    match Failure.catch (fun () -> Pp.to_string pp_model model) with
    | Ok text -> text
    | Error c -> Pp.str "<pp_model raised %s>" (Failure.caught_to_string c)
  in
  Text.truncate_utf8 model_cell_chars (one_line text)

(* The model before each call. [next] runs unguarded: repair applied it to
   the same models without raising. No row shows the last call's result. *)
let model_cells pp_model program =
  let rec cells model = function
    | [] -> []
    | [ _last ] -> [ cell pp_model model ]
    | call :: calls -> cell pp_model model :: cells (call.next model) calls
  in
  cells program.initial program.calls

(* Hard newlines and padding only: the engine renders through
   [Format.asprintf] at its default margin, which would re-wrap break
   hints. *)
let program_text ?pp_model program =
  match program.calls with
  (* A reachable counterexample: an empty text would print a bare colon. *)
  | [] -> "(no commands)"
  | calls ->
      let total = List.length calls in
      let cells =
        match pp_model with
        | None -> List.map (fun _ -> None) calls
        | Some pp_model -> List.map Option.some (model_cells pp_model program)
      in
      let rows =
        List.mapi
          (fun i (call, cell) -> (string_of_int (i + 1), cell, step_text call))
          (List.combine calls cells)
      in
      let omitted = total - (2 * context_calls) in
      let head, omission, tail =
        if omitted <= 0 then (rows, [], [])
        else
          ( List.filteri (fun i _ -> i < context_calls) rows,
            [ Pp.str "\u{2026} (%s omitted)" (call_count omitted) ],
            List.filteri (fun i _ -> i >= total - context_calls) rows )
      in
      let header =
        ("#", Option.map (fun _ -> "model before") pp_model, "call")
      in
      let number_width = max 2 (String.length (string_of_int total)) in
      let cell_width =
        List.fold_left
          (fun width (_, cell, _) ->
            max width (Text.length_utf8 (Option.value cell ~default:"")))
          0
          ((header :: head) @ tail)
      in
      let row (number, cell, step) =
        pad number_width number ^ number
        ^ (match cell with
          | None -> ""
          | Some cell -> "  " ^ cell ^ pad cell_width cell)
        ^ "  " ^ step
      in
      String.concat "\n"
        ((row header :: List.map row head) @ omission @ List.map row tail)

let pp_program ?pp_model ppf program =
  Pp.string ppf (program_text ?pp_model program)

let summary program =
  match List.rev program.calls with
  | [] -> None
  | last :: _ ->
      let total = call_count (List.length program.calls) in
      Some (Pp.str "%s, last: %s" total last.name)

(* Generating *)

(* [first_positions commands] is, for each command, the position of its first
   occurrence in [commands], so a command listed twice is one command. *)
let first_positions commands =
  let rec first i c = function
    | [] -> assert false (* [c] is a member *)
    | c' :: cs -> if c' == c then i else first (i + 1) c cs
  in
  List.map (fun c -> first 0 c commands) commands

(* Repair runs on the drawn calls before the tree is assembled, so a dropped
   call contributes no subtree, and again at every node, so a call that a
   deletion elsewhere makes illegal goes in the same candidate. A candidate
   that repeats its parent cannot loop the search: every accepted step
   descends one level of [Shrink_tree.list]'s tree, which is finite in depth
   when the argument trees are. The length is fixed, since chunk deletion
   already shrinks it. *)
let program ?(steps = 20) ?pp_model ~model commands =
  let call_gen command (Command { name; gen; pre; next; body; loc }) =
    let name = one_line name in
    Gen.map
      (fun arg ->
        {
          command;
          name;
          loc;
          arg = lazy (Gen.Engine.render_value gen arg);
          pre = (fun model -> pre model arg);
          next = (fun model -> next model arg);
          body = (fun model sut -> body model arg sut);
        })
      gen
  in
  (* The index is drawn with [Seed.below] and has no tree, so a candidate
     never turns one command into another. *)
  let calls =
    Array.of_list (List.map2 call_gen (first_positions commands) commands)
  in
  let draw_call state =
    let count = Int64.of_int (Array.length calls) in
    let index, state = Seed.below ~bound:count state in
    Gen.Engine.run calls.(Int64.to_int index) state
  in
  Gen.Engine.make ~pp:(pp_program ?pp_model) (fun state ->
      (match commands with
      | [] -> invalid_arg "Windtrap.stateful: no commands to draw from"
      | _ :: _ -> ());
      if steps < 0 then invalid_arg "Windtrap.stateful: negative steps";
      let rec draw n trees state =
        if n = 0 then (List.rev trees, state)
        else
          let tree, state = draw_call state in
          draw (n - 1) (tree :: trees) state
      in
      let trees, state = draw steps [] state in
      let trees = repair model ~call:Shrink_tree.root trees in
      let tree =
        Shrink_tree.map
          (fun calls ->
            { initial = model; calls = repair model ~call:Fun.id calls })
          (Shrink_tree.list trees)
      in
      (tree, state))

(* Executing *)

(* [label] is spelled only on failure. A headline prints [msg] on one line,
   so the user's own is flattened into it. *)
let attributed ?loc label fn =
  let fail (failure : Failure.t) =
    let msg =
      match failure.msg with
      | None -> label ()
      | Some msg -> label () ^ "; " ^ one_line msg.kept
    in
    let loc = match failure.loc with None -> loc | Some _ -> failure.loc in
    raise
      (Failure.Check_failure { failure with msg = Some (Failure.text msg); loc })
  in
  match Failure.catch fn with
  | Ok () -> ()
  | Error (`Assertion failure) -> fail failure
  | Error (`Exception (exn, backtrace)) ->
      fail
        (Failure.raised
           ~actual:(Failure.exn_to_string exn)
           ~backtrace:(Failure.backtrace_to_string backtrace)
           ())
  | Error (#Failure.control as c) -> Failure.reraise c

let run_program ?invariant program sut =
  let total = List.length program.calls in
  let invariant = Option.value invariant ~default:(fun _ _ -> ()) in
  attributed
    (fun () -> "invariant on the fresh system")
    (fun () -> invariant program.initial sut);
  let rec run step model = function
    | [] -> ()
    | call :: calls ->
        attributed ?loc:call.loc
          (fun () -> Pp.str "call %d of %d: %s" step total call.name)
          (fun () -> call.body model sut);
        (* Unguarded: repair applied this [next] to this model. *)
        let model = call.next model in
        attributed
          (fun () ->
            Pp.str "invariant after call %d of %d: %s" step total call.name)
          (fun () -> invariant model sut);
        run (step + 1) model calls
  in
  run 1 program.initial program.calls

(* A scope that never calls back fails the case, since a program that did
   not run must not pass. A second call is a bug of the harness, raised as
   [Invalid_argument]: the search then converges on the empty program, and
   the message diagnoses it. [misused] and [failed] are recorded as well as
   raised, so a scope that swallows them cannot turn the case green. *)
let execute ?loc ?invariant ~scope program =
  let ran = ref false in
  let misused = ref None in
  let failed = ref None in
  let run sut =
    if !ran then begin
      (* ASCII only: [Printexc] prints this payload with [%S]. *)
      let exn =
        Invalid_argument
          "Windtrap.stateful: the scope called its callback twice; a scope \
           must call it exactly once"
      in
      misused := Some exn;
      raise exn
    end;
    ran := true;
    match Failure.catch (fun () -> run_program ?invariant program sut) with
    | Ok () -> ()
    | Error c ->
        failed := Some c;
        Failure.reraise c
  in
  let escaped =
    match Failure.catch (fun () -> scope run) with
    | Ok () -> None
    | Error c -> Some c
  in
  match (!misused, escaped, !failed) with
  (* Raised again with the second call's backtrace. *)
  | Some exn, Some (`Exception (raised, _) as c), _ when raised == exn ->
      Failure.reraise c
  | Some exn, _, _ -> raise exn
  (* A control outranks the program's failure: a timeout behind it would be
     accepted as a shrink step. *)
  | None, Some (#Failure.control as c), _ -> Failure.reraise c
  | None, _, Some c | None, Some c, None -> Failure.reraise c
  | None, None, None ->
      if not !ran then
        raise
          (Failure.Check_failure
             (Failure.message ?loc
                "the scope returned without running the program; a scope must \
                 call its callback exactly once"))

(* Declaring *)

let never_called ?loc ~cases names =
  Failure.message ?loc
    (Pp.str
       "never called: %s (over %d passing cases); a command is called only \
        where its ~pre holds"
       (String.concat ", " (List.map (Pp.str "%S") names))
       cases)

(* [Test_tree.Tag.prop] because a stateful test is a property: [--tag prop]
   selects it, and its report shows the root seed. The law returns only on a
   passing program, and [Run.property] returns only when every case passed, so
   [called] then marks the commands of the passing programs. *)
let stateful ?__POS__ ?tags ?timeout ?count ?steps ?pp_model ?invariant name
    ~model ~scope commands =
  let loc = Loc.resolve ?__POS__ () in
  let tags =
    Test_tree.Tag.prop :: "stateful" :: Option.value ~default:[] tags
  in
  let gen = program ?steps ?pp_model ~model commands in
  let body () =
    let called = Array.make (List.length commands) false in
    let cases = ref 0 in
    Run.property ?loc ?count ~summary gen (fun program ->
        execute ?loc ?invariant ~scope program;
        incr cases;
        List.iter (fun call -> called.(call.command) <- true) program.calls);
    let never =
      List.filteri
        (fun i position -> i = position && not called.(i))
        (first_positions commands)
    in
    if !cases > 0 && never <> [] then
      let name i =
        match List.nth commands i with Command c -> one_line c.name
      in
      raise
        (Failure.Check_failure
           (never_called ?loc ~cases:!cases (List.map name never)))
  in
  Test_tree.test ?__POS__ ~tags ?timeout name body
