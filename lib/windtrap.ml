(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Not mutated: see [Mutate_loop]. *)
[@@@mutate exclude_file]

(* Every value here is another module's, except the operations that reach
   the running test through [Run]'s ambient slot and [run], which composes
   [Cli], [Mutate_loop] and [Report]. *)

(* Types *)

type test = Test_tree.t

module Testable = Testable

(* Declaring tests *)

let test = Test_tree.test
let group = Test_tree.group
let slow = Test_tree.slow
let cases = Test_tree.cases
let bracket = Test_tree.bracket
let scoped = Test_tree.scoped
let fixture = Run.fixture
let focus = Test_tree.focus
let xfail = Test_tree.xfail

(* Assertions *)

(* [Check] is the assertion verbs, with the types [pos], [printer] and
   [testable] they take. *)
include Check
module Law = Law

(* Inside a run the runner consumes a failure. One raised outside a run, at
   module top level or in a script, escapes uncaught and prints as its
   headline. *)
let () =
  Printexc.register_printer (function
    | Failure.Check_failure failure ->
        Some ("windtrap assertion failure: " ^ Report.headline failure)
    | _ -> None)

(* Witnesses *)

let unit = Testable.unit
let bool = Testable.bool
let char = Testable.char
let string = Testable.string
let text = Testable.text
let bytes = Testable.bytes
let int = Testable.int
let int32 = Testable.int32
let int64 = Testable.int64
let nativeint = Testable.nativeint
let float_exact = Testable.float_exact
let float = Testable.float
let float_rel = Testable.float_rel
let option = Testable.option
let result = Testable.result
let either = Testable.either
let list = Testable.list
let array = Testable.array
let slist = Testable.slist
let pair = Testable.pair
let triple = Testable.triple
let quad = Testable.quad
let pass = Testable.pass

(* Properties *)

module Gen_engine = Gen.Engine
module Gen = Gen

let prop ?__POS__ ?tags ?timeout ?count ?max_discard ?examples name gen law =
  let tags = Test_tree.Tag.prop :: Option.value ~default:[] tags in
  Run.prop ?__POS__ ~tags ?timeout ?count ?max_discard ?examples name gen law

let assume = Property.assume
let reject = Property.reject

(* The frame refuses first, outside a test and on another domain. *)
let prop_context op =
  let (_ : Run.frame) = Run.current_frame () in
  match Run.prop_context () with
  | Some context -> context
  | None ->
      invalid_arg
        (op
       ^ " only works while a property body runs: declare the test with [prop] \
          and call it from the property's law")

let collect label = Property.collect (prop_context "collect") label
let classify label cond = Property.classify (prop_context "classify") label cond
let cover label cond = Property.cover (prop_context "cover") label cond

(* Stateful tests *)

type ('r, 's) abstract = ('r, 's) Stateful.abstract
type ('r, 's, 'p) fn = ('r, 's, 'p) Stateful.fn
type command = Stateful.command

let abstract = Stateful.abstract
let ( @-> ) = Stateful.( @-> )
let ( ^-> ) = Stateful.( ^-> )
let returns = Stateful.returns
let makes = Stateful.makes
let judges = Stateful.judges
let command = Stateful.command

(* Unlike [Run.prop], [Stateful.stateful] adds the ["prop"] tag itself. *)
let stateful = Stateful.stateful

(* Baselines *)

(* The literal's position locates the failure: the compiler recorded it for
   the call, and a correction rewrites the literal there. *)
let check_literal ~exact actual (pos, value) =
  Run.check_baseline ~loc:(Loc.of_pos pos)
    (Baseline.Literal { pos; value; exact })
    actual

let expect actual literal = check_literal ~exact:false actual literal
let expect_exact actual literal = check_literal ~exact:true actual literal

let expect_file ?__POS__ actual path =
  Run.check_baseline ?loc:(Loc.resolve ?__POS__ ()) (Baseline.File path) actual

(* Captured output *)

let output () = Capture.output (Run.capture (Run.current ()))

(* The running test *)

let current_test = Run.current_test
let subtest = Run.subtest
let temp_dir = Run.temp_dir
let temp_file = Run.temp_file
let setenv = Run.setenv
let chdir = Run.chdir

(* Running *)

(* [dune subst], which [dune-release distrib] runs in the clone it archives,
   replaces the watermark with the tag's version. A tree built unsubstituted,
   such as a checkout, still holds it and reports "dev". *)
let version =
  let watermark = "%%VERSION%%" in
  if String.starts_with ~prefix:"%" watermark then "dev" else watermark

(* Under dune, where [dune runtest] and [dune exec] both set [INSIDE_DUNE],
   the command is a [dune exec] of this executable. An executable with
   mutants was built with the mutation backend, and a [dune exec] without
   [--instrument-with], which dune takes before the target, would rebuild it
   uninstrumented. Module initialisation is over, so the catalogue is
   complete. *)
let invocation ~corrected argv0 : Run.invocation =
  if corrected || argv0 = "" then `Mirrors
  else if not (Os.inside_dune ()) then `Exe (Report_sections.shell_word argv0)
  else
    let exe =
      if Filename.is_relative argv0 then Filename.concat (Sys.getcwd ()) argv0
      else argv0
    in
    Report_sections.dune_exec
      ~mutate:(Windtrap_runtime.Mutate.catalogue () <> [])
      (Os.display_path exe)

(* [-l] is refused as a run would be, and its standard output holds the
   paths alone. *)
let list_selection ~suite config tests =
  match Run.list_selection config ~suite tests with
  | Error error ->
      Os.say (Run.startup_message error);
      Run.startup_exit_code error
  | Ok [] ->
      (* The [list:] hint of an empty run leads here, so an empty listing says
         why it is empty. *)
      let declared = List.length (Test_tree.flatten tests) in
      let focused = Test_tree.focus_sites tests <> [] in
      let selection = Report.selection_description ~focused config in
      Option.iter
        (fun reason -> Os.say ("no tests selected: " ^ reason ^ "."))
        (Report.empty_selection_reason ~declared ~selection);
      0
  | Ok paths ->
      List.iter (fun path -> print_endline (Text.escape_controls path)) paths;
      0

let execute ~suite (config : Run.config) tests =
  match Mutate_loop.execute_and_report ~suite config tests with
  | Mutate_loop.Reported code -> code
  (* [Report.run] said why. *)
  | Ran (Error error) -> Run.startup_exit_code error
  | Ran (Ok outcome) ->
      if outcome.focus_active && not (Os.in_ci ()) then
        Os.warn
          (Pp.str
             "focus is active: %d of %d tests ran; remove the focus before \
              committing"
             (List.length outcome.selected)
             outcome.total);
      let written =
        List.filter
          (function Baseline.Written _ -> true | Refused _ -> false)
          (Baseline.writes (Run.baselines outcome.run))
      in
      (* dune registers a [.corrected] file only after an action that exits 0,
         and the blocks' [accept:] lines were printed before the code was. *)
      if
        config.baseline = Baseline.Corrected
        && written <> [] && outcome.exit_code = 1
      then
        Os.warn
          (Pp.str
             "dune registers a correction for promotion only when the run that \
              wrote it exits 0, so the failures above withhold the \
              correction%s written here. Fix the failures, rerun, then 'dune \
              promote'."
             (if List.length written = 1 then "" else "s"));
      (* The mirrors reach every stanza of a project, and a stanza whose tests
         their selection leaves out holds no mistyped filter. *)
      if
        outcome.exit_code = 2 && outcome.total > 0 && config.broadcast.selection
      then 0
      else outcome.exit_code

let run ?(argv = Sys.argv) suite tests =
  (* [--help], [--version] and a usage error return before [Run.execute]
     would refuse a nested run. *)
  if Run.active () then invalid_arg Run.active_run_error;
  let argv0 = if Array.length argv > 0 then argv.(0) else "" in
  let prog = if argv0 = "" then suite else argv0 in
  let usage_error error =
    Os.say (Cli.error_message error);
    prerr_endline (Cli.usage ~prog);
    2
  in
  (* The pages are flushed, for a caller that does not [exit]. *)
  match Cli.parse argv with
  | Error error -> usage_error error
  | Ok { Cli.help = true; _ } ->
      Printf.printf "%s%!" (Cli.help ~prog);
      0
  | Ok { Cli.version = true; _ } ->
      Printf.printf "windtrap %s\n%!" version;
      0
  | Ok parsed -> (
      match Cli.settings parsed with
      | Error error -> usage_error error
      (* [-l] has no mirror, so the command line alone resolves it. *)
      | Ok config when parsed.list_only = Some true ->
          list_selection ~suite config tests
      | Ok config ->
          let corrected = config.baseline = Baseline.Corrected in
          let invocation = invocation ~corrected argv0 in
          execute ~suite { config with invocation } tests)

(* Private *)

module Private = struct
  module Baseline = Baseline
  module Capture = Capture
  module Check = Check
  module Cli = Cli
  module Diff = Diff
  module Failure = Failure
  module Gen_engine = Gen_engine
  module Loc = Loc
  module Mutate_loop = Mutate_loop
  module Os = Os
  module Pp = Pp
  module Property = Property
  module Report = Report
  module Report_junit = Report_junit
  module Report_sections = Report_sections
  module Run = Run
  module Seed = Seed
  module Source_patch = Source_patch
  module Stateful = Stateful
  module Test_tree = Test_tree
  module Text = Text
  module Workers = Workers
end
