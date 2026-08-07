(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The producers both runners compose a run's reporting from: one producer
   per transcript line class, so the facade's [run] and the inline (ppx)
   runner cannot drift apart byte-wise. Differences between the runners are
   parameters here or visible lines in the thin drivers — never forks of a
   producer. *)

(* Path shown in [wrote]/[pruned]/hint lines: the shared [Path_ops.display]
   spelling, so the line classes stay byte-equal to the manual's transcripts
   across both runners. *)
let display_path = Path_ops.display

(* Renderer construction *)

let renderer ~config ~mode ~invocation () =
  let inside_dune = Env.inside_dune () in
  let tty = Env.is_tty_stdout () in
  let ansi =
    Env.resolve_color config.Run.color ~tty ~inside_dune
      ~term_dumb:(Env.term_dumb ())
  in
  (* The mode decides what prints; the sink only decides color ([ansi]) and
     the erasable live tail ([live], TTY only) — no sink changes shape.
     Under GITHUB_ACTIONS the same transcript sits inside the ::group::
     envelope, and the live tail is explicitly off even if stdout is a TTY:
     its erase/redraw control sequences would land verbatim in the CI
     log. *)
  Render.create ~out:Format.std_formatter ~ansi ~mode
    ~live:(tty && not (Env.in_github_actions ()))
    ?columns:(Option.map (Int.max 20) config.Run.columns)
    ?tail_lines:(Option.map (Int.max 0) config.Run.tail_errors)
    ~slow_threshold:config.Run.slow_threshold ~invocation ()

(* The event observer *)

let observe renderer ~seed = function
  | Runner.Run_started { run = _; suite; total = _; selected } ->
      Render.header renderer ~suite ~tests:selected ~seed
  | Runner.Test_started { path } -> Render.begin_test renderer ~path
  | Runner.Test_finished result -> Render.result renderer result
  | Runner.Fixture_release { name } ->
      (* [Render.note] closes a partial glyph row first — a raw printf
         would splice the notice into the compact row — and drops the line
         under [`Quiet]. *)
      Render.note renderer ("releasing " ^ name)

(* The GitHub envelope *)

let github_start ~github suite =
  if github then print_string (Render_github.group_start suite)

let github_end ~github = if github then print_string Render_github.group_end

let github_annotations ~github ~invocation results =
  (* Excused expected failures annotate nothing: an [::error] on a PR
     demands action, and these did not fail the run — the transport
     classifies from the result records. *)
  if github then print_string (Render_github.annotations ~invocation results)

(* The snapshot/prune report *)

let explain_prune_refusal (refusal : Snapshot.prune_refusal) =
  let blockers =
    List.concat
      [
        (if refusal.Snapshot.not_update_run then
           [ "the run was not an update run (-u / WINDTRAP_UPDATE=1)" ]
         else []);
        (if refusal.Snapshot.filtered then [ "a filter narrowed the run" ]
         else []);
        (if refusal.Snapshot.skipped > 0 then
           [ Pp.str "%d selected test(s) skipped" refusal.Snapshot.skipped ]
         else []);
        (if refusal.Snapshot.failed > 0 then
           [ Pp.str "%d selected test(s) failed" refusal.Snapshot.failed ]
         else []);
        (if refusal.Snapshot.focused > 0 then
           [ Pp.str "%d test(s) are focused" refusal.Snapshot.focused ]
         else []);
      ]
  in
  "prune refused: " ^ String.concat "; " blockers

let report_snapshots ~out ~output ~invocation (outcome : Runner.outcome) =
  if output <> `Quiet then begin
    let writes = Snapshot.writes (Run.snapshots outcome.Runner.run) in
    List.iter
      (fun (path, status) ->
        let status =
          match status with
          | Snapshot.Created -> "new"
          | Snapshot.Updated -> "updated"
        in
        Format.fprintf out "wrote %s (%s)@." (display_path path) status)
      writes;
    match outcome.Runner.pruned with
    | Some (Ok deleted) ->
        List.iter
          (fun path -> Format.fprintf out "pruned %s@." (display_path path))
          deleted
    | Some (Error refusal) ->
        List.iter
          (fun path ->
            Format.fprintf out "stale baseline: %s@." (display_path path))
          outcome.Runner.orphans;
        Format.fprintf out "%s@." (explain_prune_refusal refusal)
    | None -> (
        match outcome.Runner.orphans with
        | [] -> ()
        | orphans ->
            List.iter
              (fun path ->
                Format.fprintf out "stale baseline: %s@." (display_path path))
              orphans;
            (* The prune hint derives from the same startup-computed
               invocation as every other command hint. *)
            let command =
              match (invocation : Render.invocation) with
              | `Exe cmd -> cmd ^ " -u --prune"
              | `Mirrors -> "WINDTRAP_UPDATE=1 WINDTRAP_PRUNE=1 dune runtest"
            in
            Format.fprintf out "remove stale baselines: %s@." command)
  end

(* The coverage seam *)

(* Sibling detection: other executables'
   .coverage files beside this process's own dump destination mean the
   in-process number is one executable's view of the code it links, and
   the project number is the merge — the coverage line says so instead of
   posing as the total. Read here, at snapshot time, so renderers stay
   projections of the run record. Best-effort by design: on a cold
   parallel first run a sibling's dump may not exist yet (dumps are
   written atomically at exit, after this snapshot), so the fact can be
   absent once; it is deterministic from the second run on, and a
   spurious sibling (an orphaned dump) only makes the hint advisory,
   never wrong. *)
let coverage_has_siblings () =
  match Windtrap_coverage.dump_destination () with
  | None -> false
  | Some path -> (
      let dir = Filename.dirname path in
      match Sys.readdir dir with
      | exception Sys_error _ -> false
      | entries ->
          Array.exists
            (fun entry ->
              Filename.check_suffix entry ".coverage"
              && Filename.concat dir entry <> path)
            entries)

let snapshot_coverage run =
  (* When instrumented code registered in-process coverage, snapshot it
     into the run record; renderers project it like any other run data. *)
  let collection = Windtrap_coverage.snapshot () in
  if not (Windtrap_coverage.is_empty collection) then begin
    let s = Windtrap_coverage.summary collection in
    Run.set_coverage run
      {
        Run.visited = s.visited;
        total = s.total;
        siblings = coverage_has_siblings ();
      }
  end;
  collection

(* The path a release failure reports under. Not a test path — no test owns
   a release — so it is a single component that cannot collide with one: a
   declared path component may not contain the separator. *)
let release_path = "fixture release"

(* Fixture releases (Run.release_fixtures)

   Releases run after the last test, so a release failure never enters
   [Run.results] — [Runner] carries it beside them, and every sink projects
   results. Give it one: a synthetic result per failure, built here so both
   runners get the same thing and no sink has to learn a second shape.

   Deliberately NOT recorded into the run. [Runner] decides "did the whole
   suite execute" as [List.length results = total], and an extra row there
   would silently disable orphan reporting and [--prune]. *)
let release_results (outcome : Runner.outcome) =
  List.map
    (fun failure ->
      {
        Run.path = [ release_path ];
        outcome = Failure.Fail [ failure ];
        counted = true;
        xfail = None;
        slow_tagged = false;
        duration = 0.;
        attempts = 1;
        prop_stats = None;
        srandom_root = None;
      })
    outcome.Runner.release_failures

let results_with_releases outcome =
  Run.results outcome.Runner.run @ release_results outcome

let coverage_summary ~coverage_mode run =
  match coverage_mode with
  | `Summary -> Run.coverage run
  | `Report | `Full | `Off -> None

let coverage_report renderer ~coverage_mode run collection =
  match coverage_mode with
  | (`Report | `Full) as mode when Run.coverage run <> None ->
      (* Sources are recorded workspace-relative; under `dune runtest` the
         cwd is inside _build, so resolve them like snapshots do. *)
      (* Root discovery reads the cwd, which a test may have removed: the
         report degrades to unresolved sources rather than raising out of
         the reporting path after the tests are already done. *)
      let source_roots =
        match Path_ops.project_root () with
        | root -> [ root ]
        | exception Sys_error _ -> []
      in
      Render.coverage_report renderer ~source_roots ~mode collection
  | `Report | `Full | `Summary | `Off -> ()
