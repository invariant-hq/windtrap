(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Driver: the shared producers both runners compose a run's
   reporting from. Byte parity between the facade's [run] and the inline
   (ppx) runner is construction — one producer per line class — so the
   pins live here, once, instead of comparing two drivers' transcripts:
   the snapshot/prune report's line classes under both invocations and
   the quiet gate, the observer's header-seed policy (the one observer
   difference between the runners), the GitHub envelope's gating, and
   the coverage seam's mode selection. *)

open Windtrap
open Windtrap.Private
module Fixtures = Render_fixtures

let check name cond = is_true ~msg:name cond
let check_string name ~expected ~actual = equal ~msg:name string expected actual

(* Synthetic outcomes *)

let make_run ?snapshots () =
  let snapshots =
    match snapshots with
    | Some s -> s
    | None -> Snapshot.create ~mode:Snapshot.Check ()
  in
  Run.create (Run.default_config ()) ~capture:Capture.disabled ~snapshots

let outcome ?snapshots ?(orphans = []) ?pruned ?(release_failures = []) () =
  {
    Runner.run = make_run ?snapshots ();
    selected = [];
    total = 0;
    focus_active = false;
    bailed = false;
    failed_paths = [];
    release_failures;
    orphans;
    pruned;
    duration = 0.1;
    exit_code = 0;
  }

let report ?(output = `Compact) ?(invocation = `Mirrors) outcome =
  let buf = Buffer.create 256 in
  let out = Format.formatter_of_buffer buf in
  Driver.report_snapshots ~out ~output ~invocation outcome;
  Format.pp_print_flush out ();
  Buffer.contents buf

(* Fixture release failures reach the sinks

   Releases run after the last test, so their failures are not in
   [Run.results] and every sink projects results — before
   [results_with_releases] the run exited 1 while the terminal said
   everything passed and the JUnit document reported failures="0". Law 8
   requires body and release failures both to be reported. *)

let release_failure name =
  Failure.message (name ^ ": release raised Failure(\"boom\")")
  |> Failure.with_phase Failure.Release

let test_release_failures_reach_the_sinks () =
  let clean = outcome () in
  check "a clean run adds nothing"
    (Driver.results_with_releases clean = Run.results clean.Runner.run);
  let failed = outcome ~release_failures:[ release_failure "db" ] () in
  match Driver.results_with_releases failed with
  | [ r ] ->
      check "the release failure is a counted failure" r.Run.counted;
      check "it carries the Release phase"
        (match r.Run.outcome with
        | Failure.Fail [ f ] -> f.Failure.phase = Failure.Release
        | _ -> false);
      (* Renderers key off the path, so it must not read as a test. *)
      check_string "it reports under its own path" ~expected:"fixture release"
        ~actual:(Test_tree.path_to_string r.Run.path);
      (* The run itself stays untouched: Runner decides "the whole suite
         executed" by comparing the result count against the selected
         count, and an extra row there disables orphans and --prune. *)
      check "the run's own results are unchanged"
        (Run.results failed.Runner.run = [])
  | rs ->
      check
        (Printf.sprintf "expected one synthetic result, got %d" (List.length rs))
        false

(* The snapshot/prune report *)

let test_report_writes () =
  (* One [wrote] line per accepted baseline, paths spelled by
     [Path_ops.display] — the one producer for both runners (ppx/F-6). *)
  let root = temp_dir () in
  let snapshots = Snapshot.create ~root ~mode:Snapshot.Update () in
  Snapshot.check snapshots ~test:"t" ~scope:(Some "qa/x.ml") ~name:"greeting"
    "hello\n";
  let written =
    match Snapshot.writes snapshots with
    | [ (path, Snapshot.Created) ] -> path
    | _ -> failf "expected exactly one Created write"
  in
  check_string "wrote line: Path_ops.display spelling, (new) status"
    ~expected:(Printf.sprintf "wrote %s (new)\n" (Path_ops.display written))
    ~actual:(report (outcome ~snapshots ()));
  check_string "quiet prints no maintenance lines" ~expected:""
    ~actual:(report ~output:`Quiet (outcome ~snapshots ()))

let test_report_prune () =
  let deleted = outcome ~pruned:(Ok [ "/tmp/a.snap"; "/tmp/b.snap" ]) () in
  check_string "granted prune: one line per deleted baseline"
    ~expected:
      (Printf.sprintf "pruned %s\npruned %s\n"
         (Path_ops.display "/tmp/a.snap")
         (Path_ops.display "/tmp/b.snap"))
    ~actual:(report deleted);
  let refusal =
    {
      Snapshot.not_update_run = true;
      filtered = false;
      skipped = 0;
      failed = 2;
      focused = 0;
    }
  in
  check_string "refused prune: stale lines then the explanation"
    ~expected:
      (Printf.sprintf
         "stale baseline: %s\n\
          prune refused: the run was not an update run (-u / \
          WINDTRAP_UPDATE=1); 2 selected test(s) failed\n"
         (Path_ops.display "/tmp/stale.snap"))
    ~actual:
      (report
         (outcome ~orphans:[ "/tmp/stale.snap" ] ~pruned:(Error refusal) ()))

let test_report_orphan_hint () =
  (* The removal hint is spelled from the invocation — the one hint-context
     difference between the runners. *)
  let stale = outcome ~orphans:[ "/tmp/stale.snap" ] () in
  let expected_stale =
    Printf.sprintf "stale baseline: %s\n" (Path_ops.display "/tmp/stale.snap")
  in
  check_string "orphans under Exe: hint completes the executable"
    ~expected:(expected_stale ^ "remove stale baselines: ./t.exe -u --prune\n")
    ~actual:(report ~invocation:(`Exe "./t.exe") stale);
  check_string "orphans under Mirrors: hint spells the environment prefixes"
    ~expected:
      (expected_stale
     ^ "remove stale baselines: WINDTRAP_UPDATE=1 WINDTRAP_PRUNE=1 dune runtest\n"
      )
    ~actual:(report ~invocation:`Mirrors stale);
  check_string "no writes, no orphans, no prune: nothing prints" ~expected:""
    ~actual:(report (outcome ()))

(* The observer's header-seed policy *)

let test_observe_seed_policy () =
  let header ~seed =
    let buf = Buffer.create 64 in
    let out = Format.formatter_of_buffer buf in
    let renderer = Render.create ~out ~ansi:false ~mode:`Verbose () in
    Driver.observe renderer ~seed
      (Runner.Run_started
         { run = make_run (); suite = "s"; total = 2; selected = 2 });
    Format.pp_print_flush out ();
    Buffer.contents buf
  in
  check_string "the facade's seeded header carries the root token"
    ~expected:"s: 2 tests (seed s1:7be1d2c904aa31f5)\n"
    ~actual:(header ~seed:(Some Fixtures.root));
  check_string "the inline runner's seedless header carries none"
    ~expected:"s: 2 tests\n" ~actual:(header ~seed:None)

(* The GitHub envelope *)

let test_github_envelope () =
  (* The producers print to captured stdout; [github:false] gates each.
     The empty fold (open immediately closed) is the startup-error shape:
     the thin drivers close the group before reporting a refusal. *)
  Driver.github_start ~github:true "mylib";
  Driver.github_end ~github:true;
  Driver.github_annotations ~github:true ~invocation:`Mirrors
    [ Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "boom" ]) ];
  let enveloped = output () in
  check "envelope: group open, close, then the annotation block"
    (String.starts_with ~prefix:"::group::mylib\n::endgroup::\n::error "
       enveloped);
  Driver.github_start ~github:false "mylib";
  Driver.github_end ~github:false;
  Driver.github_annotations ~github:false ~invocation:`Mirrors
    [ Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "boom" ]) ];
  check_string "github:false gates every envelope producer" ~expected:""
    ~actual:(output ())

let test_github_envelope_composed () =
  (* The composed envelope, as both thin drivers assemble it: the fold
     opens, the transcript streams inside it, the fold closes, and the
     annotation block follows the close (driver.mli: "The [::group::]
     fold around the transcript and the [::error::] annotation block
     after it" — annotations printed after {!github_end} are never
     folded away). The transcript is the observer's own output on the
     same sink, so this pins the composition, not three isolated
     producers. *)
  Driver.github_start ~github:true "mylib";
  let renderer =
    Render.create ~out:Format.std_formatter ~ansi:false ~mode:`Verbose ()
  in
  Driver.observe renderer ~seed:None
    (Runner.Run_started
       { run = make_run (); suite = "mylib"; total = 1; selected = 1 });
  Format.pp_print_flush Format.std_formatter ();
  Driver.github_end ~github:true;
  Driver.github_annotations ~github:true ~invocation:`Mirrors
    [ Fixtures.result [ "bad" ] (Failure.Fail [ Failure.message "boom" ]) ];
  let enveloped = output () in
  let offset pattern =
    match Text.first_occurrence ~pattern enveloped with
    | Some i -> i
    | None -> failf "missing %S in the composed envelope:\n%s" pattern enveloped
  in
  check "the fold opens first" (offset "::group::mylib\n" = 0);
  check "the transcript streams inside the fold"
    (offset "mylib: 1 test" > 0
    && offset "mylib: 1 test" < offset "\n::endgroup::\n");
  check "the annotation block follows the closed fold"
    (offset "\n::endgroup::\n" < offset "::error ")

(* The coverage seam *)

let test_coverage_seam () =
  let run = make_run () in
  Run.set_coverage run { Run.visited = 3; total = 4; siblings = false };
  check "Summary hands finish the recorded snapshot"
    (Driver.coverage_summary ~coverage_mode:`Summary run
    = Some { Run.visited = 3; total = 4; siblings = false });
  check "Report withholds it (the report prints its own line)"
    (Driver.coverage_summary ~coverage_mode:`Report run = None);
  check "Full withholds it"
    (Driver.coverage_summary ~coverage_mode:`Full run = None);
  check "Off withholds it"
    (Driver.coverage_summary ~coverage_mode:`Off run = None);
  check "Summary without a recorded snapshot is None"
    (Driver.coverage_summary ~coverage_mode:`Summary (make_run ()) = None);
  (* This executable is not instrumented: the seam snapshot is empty and
     must record nothing — the no-op path both runners share on
     uninstrumented runs. *)
  let fresh = make_run () in
  let collection = Driver.snapshot_coverage fresh in
  check "no registrations: the snapshot is empty"
    (Windtrap_coverage.is_empty collection);
  check "no registrations: nothing recorded on the run"
    (Run.coverage fresh = None)

let tests =
  [
    test "snapshot report: wrote lines and the quiet gate" test_report_writes;
    test "snapshot report: prune lines and refusals" test_report_prune;
    test "snapshot report: orphan hints per invocation" test_report_orphan_hint;
    test "observer: header-seed policy" test_observe_seed_policy;
    test "github envelope: bytes and gating" test_github_envelope;
    test "github envelope: composed around a transcript"
      test_github_envelope_composed;
    test "coverage seam: mode selection and the empty snapshot"
      test_coverage_seam;
    test "fixture release failures reach the sinks"
      test_release_failures_reach_the_sinks;
  ]
