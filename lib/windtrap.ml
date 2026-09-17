(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Not mutated. This module is part of the machinery a mutation run uses
   to judge mutants — the scheduler, the ambient run state, the reporting
   spine, the loop itself — so a mutant here is armed inside the process
   that is supposed to detect it. The failure mode is not a false
   survivor but a hang or a corrupted verdict: a mutated bail counter or
   timeout does not fail the reaching tests, it stops them from
   finishing. Coverage still measures these files; only mutation is off.
   Everything below the scheduler — the verbs, the generators, the
   diffing, the renderers — is mutated. *)
[@@@mutate exclude_file]

(* The facade: flat re-exports of the public surface, the ambient wiring
   (operations that reach the current run through Run's one documented
   slot), and the [run] entry gluing Cli, Run and Report. Wiring only —
   semantics live in the modules below. *)

(* Public modules *)

module Testable = Testable
module Gen = Gen

(* Internal modules (see [Private] in the .mli) *)

module Gen_engine = Gen.Engine

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
end

(* Types *)

type test = Test_tree.t
type pos = string * int * int * int
type 'a printer = Format.formatter -> 'a -> unit
type 'a testable = 'a Testable.t

(* Declaring tests *)

let test = Test_tree.test
let group = Test_tree.group
let slow = Test_tree.slow
let cases = Test_tree.cases
let bracket = Test_tree.bracket
let scoped = Test_tree.scoped
let focus = Test_tree.focus
let xfail = Test_tree.xfail
let fixture = Run.fixture

(* Assertions *)

let equal = Check.equal
let not_equal = Check.not_equal
let is_true = Check.is_true
let is_false = Check.is_false
let contains = Check.contains
let not_contains = Check.not_contains
let in_order = Check.in_order
let starts_with = Check.starts_with
let ends_with = Check.ends_with
let satisfies = Check.satisfies
let less = Check.less
let at_most = Check.at_most
let greater = Check.greater
let at_least = Check.at_least
let mem = Check.mem
let is_none = Check.is_none
let is_some = Check.is_some
let is_ok = Check.is_ok
let is_error = Check.is_error
let require_some = Check.require_some
let require_ok = Check.require_ok
let require_error = Check.require_error
let require_match = Check.require_match
let raises = Check.raises
let raises_match = Check.raises_match

module Exn = Check.Exn

let fail = Check.fail
let failf = Check.failf
let skip = Check.skip

(* Inside a run the control exceptions never reach uncaught-exception
   rendering: the runner's boundary consumes them. One escaping without a
   run (an assertion at module toplevel, a helper script) would print as an
   opaque constructor; render the typed payload instead — a projection on
   the only path with no renderer downstream. *)
let () =
  Printexc.register_printer (function
    | Failure.Check_failure failure ->
        Some ("windtrap assertion failure: " ^ Report.headline failure)
    | Failure.Skip_test reason ->
        Some
          ("windtrap skip"
          ^ match reason with Some reason -> ": " ^ reason | None -> "")
    | _ -> None)

(* Testable instances *)

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

(* Facade [prop] tests carry [Test_tree.Tag.prop]: it makes properties selectable
   ([--tag prop]) and lets the report print the root seed in the header
   exactly when the suite declares property tests. *)
let prop ?__POS__ ?tags ?timeout ?count ?max_discard ?examples name gen law =
  let tags = Test_tree.Tag.prop :: Option.value ~default:[] tags in
  Run.prop ?__POS__ ~tags ?timeout ?count ?max_discard ?examples name gen law

let assume = Property.assume
let reject = Property.reject

(* [Stateful.stateful] applies [Test_tree.Tag.prop] itself, alongside its own
   ["stateful"] tag — so this is a re-export and not a wrapper like [prop]
   above. Adding the tag here again would duplicate it. *)

type ('model, 'sut) command = ('model, 'sut) Stateful.command

let command = Stateful.command
let call = Stateful.call
let stateful = Stateful.stateful

let prop_context op =
  match Run.prop_context (Run.current_frame ()) with
  | Some context -> context
  | None ->
      invalid_arg
        (op
       ^ " only works while a property body runs: declare the test with [prop] \
          and call it from the property's law")

let collect label = Property.collect (prop_context "collect") label
let classify label cond = Property.classify (prop_context "classify") label cond
let cover label cond = Property.cover (prop_context "cover") label cond

(* Baselines *)

(* The literal's position is the failure's location: it is what the
   compiler recorded for the call, and what the correction rewrites. *)
let expect actual (pos, value) =
  Run.check_baseline ~loc:(Loc.of_pos pos)
    (Baseline.Literal { pos; value; exact = false })
    actual

let expect_exact actual (pos, value) =
  Run.check_baseline ~loc:(Loc.of_pos pos)
    (Baseline.Literal { pos; value; exact = true })
    actual

let expect_file actual path =
  Run.check_baseline ?loc:(Loc.capture ()) (Baseline.File path) actual

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

(* The watermark below is a dune substitution point. [dune-release distrib]
   runs [dune subst] in the clone it archives, so the published tarball
   already carries the tag's version before opam ever sees it —
   doc/dev/release.md: "Bump nothing in source: the version comes from the
   git tag". The opam build's own [["dune" "subst"] {dev}] step is not what
   does it: [dev] is false for a release installed from opam-repository, so
   that step only covers a pinned checkout. Every other source — a working
   tree, a plain [git archive] — reaches this line unsubstituted, and the
   watermark is still here: report "dev" rather than a number that would be a
   lie either way. *)
let version =
  let watermark = "%%VERSION%%" in
  if String.length watermark > 0 && watermark.[0] = '%' then "dev"
  else watermark

(* The hint context, computed once at startup and carried in the
   configuration to every hint: under dune, a [dune exec] spelling of
   this executable — truthful for every dune invocation of every stanza
   kind, where [dune runtest] and [dune exec] are indistinguishable (both
   set INSIDE_DUNE); standalone, argv0 verbatim, exactly as the user typed
   it. An embedder passing [~argv:[||]] gets [`Mirrors] — the fixed dune
   wording. A [--corrected] run is dune's — a stanza's action, or the
   inline runner — so its hints spell the mirrors and its acceptance is
   [dune promote], whatever argv says.

   The dune spelling carries [--instrument-with ppx_windtrap.mutate] when
   this executable has mutants registered, and the flag precedes the
   target because dune requires it there. A binary with mutants was
   necessarily built with the backend, so that is the signal: without the
   flag, the [arm] line of every survivor block would tell dune to rebuild
   the target UNINSTRUMENTED, and the command that is supposed to resolve
   the finding would arm nothing. Every other hint spelled from this
   context — [--failed], [-u], the replay line — gains it too, which is
   right for the same reason: re-running the suite without the flag
   rebuilds a different binary. *)
let invocation_of ~corrected argv : Run.invocation =
  let argv0 = if Array.length argv > 0 then argv.(0) else "" in
  if argv0 = "" || corrected then `Mirrors
  else if Os.inside_dune () then begin
    let absolute =
      if Filename.is_relative argv0 then Filename.concat (Sys.getcwd ()) argv0
      else argv0
    in
    let backend =
      (* The runtime's catalogue, not the loop: a binary with mutants
         registered was necessarily built with the mutation backend, and
         the catalogue is the runtime's own record of that — complete by
         now, since module initialization is long over at run entry. *)
      if Windtrap_runtime.Mutate.catalogue () <> [] then
        "--instrument-with ppx_windtrap.mutate "
      else ""
    in
    `Exe ("dune exec " ^ backend ^ Os.display_path absolute ^ " --")
  end
  else `Exe argv0

let print_cli_error ~prog error =
  Format.eprintf "%s@.%s@." (Cli.error_message error) (Cli.usage ~prog)

(* The run: [Report.run] writes the whole transcript. What is
   legitimately the facade's own stays visible here: the argv-computed
   invocation, the focus warning, and the exit code — returned, never
   applied: the process is the caller's. *)
let run_suite ~suite ~config tests =
  (* The mutation seam: one call at run entry, in place of [Report.run].
     Without [--mutate] or [--arm] it is exactly [Report.run] — same
     transcript, same bytes, same cost; with one of them it
     wraps the run on both sides (an armed mutant is announced before any
     output, and the loop forks after the dry run) and may take the
     process over. *)
  match Mutate_loop.execute_and_report ~suite config tests with
  | Mutate_loop.Reported code -> code
  | Mutate_loop.Ran (Error error) ->
      (* The message is already on stderr; only the code is left. *)
      Run.startup_exit_code error
  | Mutate_loop.Ran (Ok outcome) ->
      if
        outcome.Run.focus_active && outcome.Run.exit_code = 0
        && not (Os.in_ci ())
      then
        Format.eprintf
          "warning: focus is active — %d of %d tests ran; remove the focus \
           before committing@."
          (List.length outcome.Run.selected)
          outcome.Run.total;
      (* A [--corrected] run is a build action's, and a build action's
         selection is a [WINDTRAP_*] variable spanning every stanza and
         partition of the tree: a stanza it empties is not a mistyped
         filter, so nothing-ran is not an error there — the "no tests ran"
         line still says so, and the [diff?] that follows is the verdict.
         A suite that declares no tests keeps its 2, since no selection
         emptied it; a usage error never reaches this branch. *)
      let code = outcome.Run.exit_code in
      if
        code = 2 && outcome.Run.total > 0
        && config.Run.baseline = Baseline.Corrected
      then 0
      else code

(* [-l]: a listing is not a transcript and must not be folded into a
   ::group:: section, and a run that runs nothing is a concept no module
   below needs to carry. The startup checks are still the ones a real
   run makes, so a refused [--shard] or [--failed] is refused here too. *)
let run_listing ~suite ~config tests =
  match Run.list_selection config ~suite tests with
  | Error error ->
      prerr_endline (Run.startup_message error);
      Run.startup_exit_code error
  | Ok [] ->
      (* A listing that answered a mistyped filter with silence would be
         the dead end the empty-selection line's own "(list the suite's
         tests with -l)" hint leads to. The hint itself is not repeated:
         the reader is listing. *)
      Option.iter
        (fun reason -> print_endline ("no tests ran: " ^ reason ^ "."))
        (Report.empty_selection_reason
           ~declared:(List.length (Test_tree.flatten tests))
           ~selection:(Report.selection_description config));
      0
  | Ok paths ->
      List.iter print_endline paths;
      0

let run ?(argv = Sys.argv) suite tests =
  (* [Run.active], not a frame probe: the slot also holds the run itself
     between attempts — a fixture release or an observer starting a
     nested run is refused like a test body would be. *)
  if Run.active () then invalid_arg Run.active_run_error;
  let prog =
    if Array.length argv > 0 && argv.(0) <> "" then argv.(0) else suite
  in
  (* Every branch ends in a code, never in [exit]: the caller applies it,
     which is what lets one binary host two suites or post-process a
     run. The two informational pages flush themselves, so the output is
     complete when [run] returns whether or not [exit] follows. *)
  match Cli.parse argv with
  | Error error ->
      print_cli_error ~prog error;
      2
  | Ok parsed when parsed.Cli.help ->
      print_string (Cli.help ~prog);
      flush stdout;
      0
  | Ok parsed when parsed.Cli.version ->
      Printf.printf "windtrap %s\n%!" version;
      0
  | Ok parsed -> (
      match Cli.settings parsed with
      | Error error ->
          print_cli_error ~prog error;
          2
      | Ok config ->
          (* [-l] has no mirror, so [parsed] is its whole resolution, as
             for [--help] and [--version]. *)
          if parsed.Cli.list_only = Some true then
            run_listing ~suite ~config tests
          else
            let config =
              {
                config with
                Run.invocation =
                  invocation_of
                    ~corrected:(config.Run.baseline = Baseline.Corrected)
                    argv;
              }
            in
            run_suite ~suite ~config tests)
