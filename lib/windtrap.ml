(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The facade: flat re-exports of the public surface, the ambient wiring
   (operations that reach the current run through Run's one documented
   slot), and the [run] driver gluing Cli, Runner, and the renderers.
   Wiring only — semantics live in the modules below. *)

(* ───── Public modules ───── *)

module Testable = Testable
module Gen = Gen

(* ───── Internal modules (see [Private] in the .mli) ───── *)

module Private = struct
  module Atomic_file = Atomic_file
  module Capture = Capture
  module Check = Check
  module Cli = Cli
  module Diff = Diff
  module Driver = Driver
  module Env = Env
  module Expect_test_config = Expect_test_config
  module Failure = Failure
  module Loc = Loc
  module Path_ops = Path_ops
  module Pp = Pp
  module Ppx_runtime = Ppx_runtime
  module Property = Property
  module Render = Render
  module Render_github = Render_github
  module Render_junit = Render_junit
  module Run = Run
  module Runner = Runner
  module Seed = Seed
  module Shrink_tree = Shrink_tree
  module Snapshot = Snapshot
  module Tag = Tag
  module Test_tree = Test_tree
  module Text = Text
end

(* ───── Types ───── *)

type test = Test_tree.t
type pos = string * int * int * int
type 'a printer = Format.formatter -> 'a -> unit
type 'a testable = 'a Testable.t

(* ───── Declaring tests ───── *)

let test = Test_tree.test
let group = Test_tree.group
let ftest = Test_tree.ftest
let fgroup = Test_tree.fgroup
let slow = Test_tree.slow
let cases = Test_tree.cases
let xfail = Test_tree.xfail
let bracket = Test_tree.bracket
let fixture = Run.fixture

(* ───── Assertions ───── *)

let equal = Check.equal
let not_equal = Check.not_equal
let is_true = Check.is_true
let is_false = Check.is_false
let contains = Check.contains
let not_contains = Check.not_contains
let satisfies = Check.satisfies
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
   opaque constructor; render the typed payload instead — a projection, per
   Law 4, on the only path with no renderer downstream. *)
let () =
  Printexc.register_printer (function
    | Failure.Check_failure failure ->
        Some ("windtrap assertion failure: " ^ Render.headline failure)
    | Failure.Skip_test reason ->
        Some
          ("windtrap skip"
          ^ match reason with Some reason -> ": " ^ reason | None -> "")
    | _ -> None)

(* ───── Testable instances ───── *)

let unit = Testable.unit
let bool = Testable.bool
let char = Testable.char
let string = Testable.string
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
let of_equal = Testable.of_equal
let contramap = Testable.contramap

(* ───── Properties ───── *)

(* Facade [prop] tests carry this tag: it makes properties selectable
   ([--tag prop]) and lets [run] print the root seed in the header exactly
   when the suite declares property tests. *)
let prop_tag = "prop"

let prop ?pos ?tags ?count ?examples name gen law =
  let tags = prop_tag :: Option.value ~default:[] tags in
  Runner.prop ?pos ~tags ?count ?examples name gen law

let assume = Property.assume
let reject = Property.reject

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

let cover ~label ~at_least cond =
  Property.cover (prop_context "cover") ~label ~at_least cond

(* ───── Snapshots ───── *)

(* The scoping file is the caller's [~pos] file, else the enclosing test's
   declaration file — never a backtrace frame at snapshot call time (RFC
   "Snapshots", resolution rule): a snapshot reached through a helper in
   another file must not relocate its baseline. *)
let snapshot_check ?pos name actual =
  let frame = Run.current_frame () in
  let scope =
    match pos with Some (file, _, _, _) -> Some file | None -> Run.file frame
  in
  (* Site ladder: explicit pos, else surviving call frame, else the checking
     test's declaration — display and duplicate-identity data only; the
     scope above never reads a frame (RFC "Snapshots"). *)
  let loc =
    match Loc.resolve ?pos () with Some _ as l -> l | None -> Run.loc frame
  in
  Snapshot.check
    (Run.snapshots (Run.run_of_frame frame))
    ?loc
    ~test:(Test_tree.path_to_string (Run.path frame))
    ~scope ~name actual

let snapshot ?pos name actual = snapshot_check ?pos name actual

let snapshot_pp ?pos name pp value =
  snapshot_check ?pos name (Pp.to_string pp value)

(* ───── Captured output ───── *)

let output ?pos () = Capture.output ?pos (Run.capture (Run.current ()))

(* ───── The running test ───── *)

let current_test = Run.current_test
let subtest = Run.subtest
let srandom = Run.srandom
let temp_dir = Run.temp_dir
let temp_file = Run.temp_file

(* ───── Running ───── *)

let version = "3.0.0~dev"

(* The hint context (D5 §1), computed once at startup and threaded to every
   renderer and transport: under dune, a [dune exec] spelling of this
   executable — truthful for every dune invocation of every stanza kind,
   where [dune runtest] and [dune exec] are indistinguishable (both set
   INSIDE_DUNE); standalone, argv0 verbatim, exactly as the user typed it.
   An embedder passing [~argv:[||]] gets [`Mirrors] — the fixed dune
   wording. *)
let invocation_of ~inside_dune argv : Render.invocation =
  let argv0 = if Array.length argv > 0 then argv.(0) else "" in
  if argv0 = "" then `Mirrors
  else if inside_dune then begin
    let absolute =
      if Filename.is_relative argv0 then Filename.concat (Sys.getcwd ()) argv0
      else argv0
    in
    `Exe ("dune exec " ^ Path_ops.display absolute ^ " --")
  end
  else `Exe argv0

let print_cli_error ~prog error =
  Format.eprintf "%s@.%s@." (Cli.error_message error) (Cli.usage ~prog)

let write_junit ~invocation ~suite ~duration ~results path =
  (* Excused-aware (B12): an xfail excused failure emits as a skipped
     testcase — the transport classifies from the result records, matching
     the run it did not fail; [invocation] keeps the hint bytes in failure
     bodies equal to the terminal block's. *)
  let document = Render_junit.render ~invocation ~suite ~results ~duration () in
  match
    let channel = open_out path in
    Fun.protect
      ~finally:(fun () -> close_out channel)
      (fun () -> output_string channel document)
  with
  | () -> ()
  | exception Sys_error message ->
      Format.eprintf "warning: could not write JUnit report: %s@." message

(* The thin library driver: composed from Driver's shared producers — one
   producer per transcript line class, shared with the inline (ppx) runner.
   What is legitimately this runner's own stays visible here: the parsed-CLI
   resolution sources, the argv-computed invocation, the property-aware
   header seed, GitHub gating minus list-only runs, list-only handling,
   JUnit, and the process exit. *)
let run_suite ~argv ~suite ~config ~coverage_mode ~output tests =
  let github = Env.in_github_actions () && not config.Run.list_only in
  (* The one invocation every command hint derives from (D5 §1): computed
     here, at startup, and threaded to the renderer and both transports. *)
  let invocation = invocation_of ~inside_dune:(Env.inside_dune ()) argv in
  let renderer = Driver.renderer ~config ~mode:output ~invocation () in
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
  let on_event = Driver.observe renderer ~seed in
  Driver.github_start ~github suite;
  match Runner.execute ~on_event ~config ~suite tests with
  | Error error ->
      Driver.github_end ~github;
      prerr_endline (Runner.startup_message error);
      exit (Runner.startup_exit_code error)
  | Ok outcome ->
      if config.Run.list_only then begin
        List.iter
          (fun case ->
            print_endline (Test_tree.path_to_string case.Test_tree.path))
          outcome.Runner.selected;
        exit outcome.Runner.exit_code
      end;
      let results = Run.results outcome.Runner.run in
      let duration = outcome.Runner.duration in
      let coverage_data = Driver.snapshot_coverage outcome.Runner.run in
      Render.finish renderer
        ?coverage:(Driver.coverage_summary ~coverage_mode outcome.Runner.run)
        ~results ~duration ();
      Driver.coverage_report renderer ~coverage_mode outcome.Runner.run
        coverage_data;
      Driver.report_snapshots ~out:Format.std_formatter ~output ~invocation
        outcome;
      Driver.github_end ~github;
      Driver.github_annotations ~github ~invocation results;
      Option.iter
        (write_junit ~invocation ~suite ~duration ~results)
        config.Run.junit;
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
      Format.pp_print_flush Format.std_formatter ();
      Format.pp_print_flush Format.err_formatter ();
      exit outcome.Runner.exit_code

let run ?(argv = Sys.argv) suite tests =
  (* [Run.active], not [current_opt] (D1): the slot also holds the run
     itself between attempts — a fixture release or an observer starting a
     nested run is refused like a test body would be. *)
  if Run.active () then
    invalid_arg
      "windtrap: run is already active — a test body cannot start another run";
  let prog =
    if Array.length argv > 0 && argv.(0) <> "" then argv.(0) else suite
  in
  match Cli.parse argv with
  | Error error ->
      print_cli_error ~prog error;
      exit 2
  | Ok parsed ->
      if parsed.Cli.help then begin
        print_string (Cli.help ~prog);
        exit 0
      end;
      if parsed.Cli.version then begin
        Printf.printf "windtrap %s\n" version;
        exit 0
      end;
      let config =
        match Cli.resolve parsed with
        | Ok config -> config
        | Error error ->
            print_cli_error ~prog error;
            exit 2
      in
      let coverage_mode =
        match Cli.coverage_mode parsed with
        | Ok mode -> mode
        | Error error ->
            print_cli_error ~prog error;
            exit 2
      in
      let output = Cli.output_level parsed in
      run_suite ~argv ~suite ~config ~coverage_mode ~output tests
