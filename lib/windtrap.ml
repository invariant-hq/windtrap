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
   slot), and the [run] driver gluing Cli, Runner, and the renderers.
   Wiring only — semantics live in the modules below. *)

(* Public modules *)

module Testable = Testable
module Gen = Gen

(* Internal modules (see [Private] in the .mli) *)

module Private = struct
  module Atomic_file = Atomic_file
  module Capture = Capture
  module Check = Check
  module Cli = Cli
  module Clock = Clock
  module Diff = Diff
  module Driver = Driver
  module Env = Env
  module Failure = Failure
  module Loc = Loc
  module Mutate_loop = Mutate_loop
  module Mutate_verdicts = Mutate_verdicts
  module Path_ops = Path_ops
  module Pp = Pp
  module Property = Property
  module Render = Render
  module Render_github = Render_github
  module Render_junit = Render_junit
  module Run = Run
  module Runner = Runner
  module Seed = Seed
  module Shrink_tree = Shrink_tree
  module Snapshot = Snapshot
  module Stateful = Stateful
  module Tag = Tag
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
let ftest = Test_tree.ftest
let fgroup = Test_tree.fgroup
let slow = Test_tree.slow
let cases = Test_tree.cases
let xfail = Test_tree.xfail
let bracket = Test_tree.bracket
let scoped = Test_tree.scoped
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
let mem = Check.mem
let is_none = Check.is_none
let is_some = Check.is_some
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
        Some ("windtrap assertion failure: " ^ Render.headline failure)
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

(* Facade [prop] tests carry this tag: it makes properties selectable
   ([--tag prop]) and lets [run] print the root seed in the header exactly
   when the suite declares property tests. *)
let prop_tag = "prop"

let prop ?pos ?tags ?timeout ?count ?max_discard ?examples name gen law =
  let tags = prop_tag :: Option.value ~default:[] tags in
  Runner.prop ?pos ~tags ?timeout ?count ?max_discard ?examples name gen law

let assume = Property.assume
let reject = Property.reject

(* [Stateful.stateful] applies [prop_tag] itself, alongside its own
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

(* Snapshots *)

let snapshot ?pos name actual = Run.check_snapshot ?pos ~name actual

let snapshot_pp ?pos name pp value =
  Run.check_snapshot ?pos ~name (Pp.to_string pp value)

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

(* The hint context, computed once at startup and threaded to every
   renderer and transport: under dune, a [dune exec] spelling of this
   executable — truthful for every dune invocation of every stanza kind,
   where [dune runtest] and [dune exec] are indistinguishable (both set
   INSIDE_DUNE); standalone, argv0 verbatim, exactly as the user typed it.
   An embedder passing [~argv:[||]] gets [`Mirrors] — the fixed dune
   wording.

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
let invocation_of ~inside_dune argv : Render.invocation =
  let argv0 = if Array.length argv > 0 then argv.(0) else "" in
  if argv0 = "" then `Mirrors
  else if inside_dune then begin
    let absolute =
      if Filename.is_relative argv0 then Filename.concat (Sys.getcwd ()) argv0
      else argv0
    in
    let backend =
      (* The runtime's catalogue, not the loop: a binary with mutants
         registered was necessarily built with the mutation backend, and
         the catalogue is the runtime's own record of that — complete by
         now, since module initialization is long over at run entry. *)
      if Windtrap_mutate.catalogue () <> [] then
        "--instrument-with ppx_windtrap.mutate "
      else ""
    in
    `Exe ("dune exec " ^ backend ^ Path_ops.display absolute ^ " --")
  end
  else `Exe argv0

let print_cli_error ~prog error =
  Format.eprintf "%s@.%s@." (Cli.error_message error) (Cli.usage ~prog)

(* The thin library driver: [Driver.execute_and_report] writes the whole
   transcript, shared byte-for-byte with the inline (ppx) runner. What is
   legitimately this runner's own stays visible here: the parsed-CLI
   resolution sources, the argv-computed invocation, the two header
   policies (the property-aware seed and the selection description),
   GitHub gating, the focus warning, and the process exit. *)
let run_suite ~argv ~suite ~config ~coverage ~render ~output ~junit tests =
  let github = Env.in_github_actions () in
  (* The one invocation every command hint derives from: computed
     here, at startup, and threaded to the renderer and both transports. *)
  let invocation = invocation_of ~inside_dune:(Env.inside_dune ()) argv in
  (* Header-seed policy: the root seed iff the suite declares property
     tests — selection never changes it, so the token stays stable across
     filtered runs. The inline runner always passes [None]. *)
  let seed =
    let has_props =
      List.exists
        (fun case -> Tag.mem prop_tag case.Test_tree.tags)
        (Test_tree.flatten tests)
    in
    if has_props then Some config.Run.seed else None
  in
  let spine =
    {
      Driver.invocation;
      seed;
      selection = Driver.selection_description config;
      github;
      output;
      coverage;
      junit;
      render;
      config;
      suite;
    }
  in
  (* The mutation seam: one call at run entry, in place of the driver's.
     Without a mutation backend and without the variables it is exactly
     [Driver.execute_and_report] — same transcript, same bytes, same
     cost; with them it wraps the run on both sides (an armed mutant is
     announced before any output, and the loop forks after the dry run)
     and may take the process over. *)
  match Mutate_loop.execute_and_report spine tests with
  | Mutate_loop.Reported code -> exit code
  | Mutate_loop.Ran result -> (
      match result with
      | Error error ->
          (* The message is already on stderr; this runner owns the exit. *)
          exit (Runner.startup_exit_code error)
      | Ok outcome ->
          if
            outcome.Runner.focus_active
            && outcome.Runner.exit_code = 0
            && not (Env.in_ci ())
          then
            Format.eprintf
              "warning: focus is active (ftest/fgroup) — %d of %d tests ran; \
               remove the focus before committing@."
              (List.length outcome.Runner.selected)
              outcome.Runner.total;
          exit outcome.Runner.exit_code)

let run ?(argv = Sys.argv) suite tests =
  (* [Run.active], not a frame probe: the slot also holds the run itself
     between attempts — a fixture release or an observer starting a
     nested run is refused like a test body would be. *)
  if Run.active () then invalid_arg Run.active_run_error;
  let prog =
    if Array.length argv > 0 && argv.(0) <> "" then argv.(0) else suite
  in
  match Cli.parse argv with
  | Error error ->
      print_cli_error ~prog error;
      exit 2
  | Ok parsed -> (
      if parsed.Cli.help then begin
        print_string (Cli.help ~prog);
        exit 0
      end;
      if parsed.Cli.version then begin
        Printf.printf "windtrap %s\n" version;
        exit 0
      end;
      match Cli.settings parsed with
      | Error error ->
          print_cli_error ~prog error;
          exit 2
      | Ok { Cli.config; render; coverage; output_level; junit } ->
          (* [-l] before the drive spine: a listing is not a transcript
             and must not be folded into a ::group:: section, and a run
             that runs nothing is a concept no module below needs to
             carry. The startup checks are still the ones a real run
             makes, so a refused [--shard] or [--failed] is refused
             here too. It has no mirror, so [parsed] is its whole
             resolution, as for [--help] and [--version]. *)
          if parsed.Cli.list_only = Some true then
            begin match Runner.list_selection ~config ~suite tests with
            | Error error ->
                prerr_endline (Runner.startup_message error);
                exit (Runner.startup_exit_code error)
            | Ok [] ->
                (* A listing that answered a mistyped filter with silence
                   would be the dead end the empty-selection line's own
                   "(list the suite's tests with -l)" hint leads to. The
                   hint itself is not repeated: the reader is listing. *)
                Option.iter
                  (fun reason ->
                    print_endline ("no tests ran: " ^ reason ^ "."))
                  (Render.empty_selection_reason
                     ~declared:(List.length (Test_tree.flatten tests))
                     ~selection:(Driver.selection_description config));
                exit 0
            | Ok paths ->
                List.iter print_endline paths;
                exit 0
            end;
          run_suite ~argv ~suite ~config ~coverage ~render ~output:output_level
            ~junit tests)
