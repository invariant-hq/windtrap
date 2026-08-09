(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* A run's whole reporting, and the producers it is composed from: one
   producer per transcript line class and one order they run in
   ([execute_and_report], at the bottom), so the facade's [run] and the
   inline (ppx) runner cannot drift apart byte-wise. Differences between
   the runners are parameters here or visible lines in the thin drivers —
   never forks of a producer, and never a second composition order. *)

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

(* What narrowed the run, in the words the reader typed. Used only to
   explain an empty selection: a bare "no tests ran." names neither the
   filter that matched nothing nor how many tests there were to match. The
   description is built here, not in Render, because the configuration is
   the driver's to know — Render only phrases the sentence. *)
(* Quoted for the reader, not for OCaml: [%S] would escape the [\u{203a}]
   of a test path into decimal bytes, and this string is meant to be read
   and retyped. Control characters are escaped because a raw newline in a
   filter would break the report's layout. *)
let quote s =
  let escaped =
    String.concat ""
      (List.map
         (fun c ->
           match c with
           | '"' -> "\\\""
           | '\\' -> "\\\\"
           | '\n' -> "\\n"
           | '\t' -> "\\t"
           | '\r' -> "\\r"
           | c when c < ' ' || c = '\127' -> Pp.str "\\x%02x" (Char.code c)
           | c -> String.make 1 c)
         (List.init (String.length s) (String.get s)))
  in
  "\"" ^ escaped ^ "\""

let selection_description (config : Run.config) =
  let quoted values = String.concat ", " (List.map quote values) in
  let parts =
    List.concat
      [
        (match config.Run.filter with
        | Some f -> [ Pp.str "filter %s" (quote f) ]
        | None -> []);
        (match config.Run.exclude with
        | Some e -> [ Pp.str "exclusion %s" (quote e) ]
        | None -> []);
        (match config.Run.tags with
        | [] -> []
        | ts -> [ Pp.str "tag %s" (quoted ts) ]);
        (match config.Run.exclude_tags with
        | [] -> []
        | ts -> [ Pp.str "excluded tag %s" (quoted ts) ]);
        (if config.Run.quick then [ "--quick" ] else []);
        (if config.Run.failed_only then [ "--failed" ] else []);
        (match config.Run.shard with
        | Some (k, n) -> [ Pp.str "shard %d/%d" k n ]
        | None -> []);
      ]
  in
  match parts with
  | [] -> None
  | [ one ] -> Some one
  | many ->
      let last = List.nth many (List.length many - 1) in
      let rest = List.filteri (fun i _ -> i < List.length many - 1) many in
      Some (String.concat ", " rest ^ " and " ^ last)

(* JUnit

   One process per suite is the normal case under `dune runtest` — a
   process per (test) stanza, and one per inline-test library — so a single
   fixed path would have every suite overwrite the last, silently. A value
   naming an [.xml] file stays exactly that, for the one-process
   invocations `--junit` was written for; anything else is a directory, and
   each suite writes its own report into it for CI to glob. *)
let junit_path ~suite target =
  if Filename.check_suffix target ".xml" then target
  else
    Filename.concat target (Path_ops.sanitize_component suite ^ ".xml")

let write_junit ~invocation ~suite ~duration ~results target =
  let path = junit_path ~suite target in
  let document = Render_junit.render ~invocation ~suite ~results ~duration () in
  match
    (* The directory form has to exist before the first suite writes into
       it, and nothing else creates it. *)
    if path != target then Path_ops.mkdir_p (Filename.dirname path);
    Atomic_file.write ~path document
  with
  | () -> ()
  | exception Sys_error message ->
      Format.eprintf "warning: could not write JUnit report: %s@." message
  | exception Unix.Unix_error (error, _, _) ->
      Format.eprintf "warning: could not write JUnit report to %s: %s@."
        (Path_ops.display path)
        (Unix.error_message error)

(* The event observer *)

let observe renderer ~seed ~selection = function
  | Runner.Run_started { run = _; suite; total; selected } ->
      Render.header renderer ~suite ~tests:selected ~declared:total ?selection
        ~seed ()
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

(* The one producer of the stale-baseline lines. [stale_lines] names the
   offending files; a refused [--prune] stops there, because its blockers
   replace the way out. Everywhere else the removal hint follows, spelled —
   like every other command hint — from the run's startup-computed
   invocation. Both spellings feed the advisory block below and the
   [--strict-snapshots] failure that block becomes, so the two cannot
   drift. *)
let stale_lines orphans =
  List.map (fun path -> Pp.str "stale baseline: %s" (display_path path)) orphans

let stale_lines_with_hint ~invocation orphans =
  let command =
    match (invocation : Render.invocation) with
    | `Exe cmd -> cmd ^ " -u --prune"
    | `Mirrors -> "WINDTRAP_UPDATE=1 WINDTRAP_PRUNE=1 dune runtest"
  in
  stale_lines orphans @ [ Pp.str "remove stale baselines: %s" command ]

(* [--strict-snapshots] is a verdict, so it needs a row in the results every
   sink projects — the same reason fixture-release failures get one (see
   [release_results]). Without it the run exits 1 under a summary that says
   every test passed. The row carries the stale lines themselves, and
   [report_snapshots] then drops its advisory copy: one printing, one
   producer. Its path is one component, like a release failure's, so it
   cannot collide with a declared test path. *)
let stale_path = "stale baselines"

(* The verdict itself: [Runner] already folded it into the exit code, and
   this is the one place that re-derives it, for the two sinks that project
   results rather than the outcome. *)
let stale_is_fatal (outcome : Runner.outcome) =
  (Run.config outcome.Runner.run).Run.strict_snapshots
  && outcome.Runner.orphans <> []

let stale_baseline_results ~invocation (outcome : Runner.outcome) =
  if not (stale_is_fatal outcome) then []
  else
    [
      {
        Run.path = [ stale_path ];
        outcome =
          Failure.Fail
            [
              Failure.message
                (String.concat "\n"
                   (stale_lines_with_hint ~invocation outcome.Runner.orphans));
            ];
        counted = true;
        xfail = None;
        slow_tagged = false;
        duration = 0.;
        attempts = 1;
        prop_stats = None;
        srandom_root = None;
      };
    ]

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
    let put lines = List.iter (Format.fprintf out "%s@.") lines in
    (* Under [--strict-snapshots] the same lines already rode into the
       failure section on the stale-baselines row: naming the files twice,
       ten lines apart, is noise, not emphasis. *)
    let advisory =
      if stale_is_fatal outcome then [] else outcome.Runner.orphans
    in
    match outcome.Runner.pruned with
    | Some (Ok deleted) ->
        put (List.map (fun p -> Pp.str "pruned %s" (display_path p)) deleted)
    | Some (Error refusal) ->
        put (stale_lines advisory);
        Format.fprintf out "%s@." (explain_prune_refusal refusal)
    | None -> (
        match advisory with
        | [] -> ()
        | orphans -> put (stale_lines_with_hint ~invocation orphans))
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

(* WINDTRAP_COVERAGE_ONLY: the source prefixes this run's number is about.
   Applied HERE, at the one seam, so the inline line and the report modes
   cannot disagree about what was counted — and not to the .coverage dump,
   which the runtime writes whole because it is what `windtrap coverage`
   merges. Prefix matching, not globbing: the registry's file names are
   the paths the instrumenter recorded, and a prefix is the one predicate
   a reader can apply by eye. *)
let coverage_scope () =
  match Env.coverage_only () with
  | [] -> Fun.id
  | prefixes ->
      Windtrap_coverage.filter (fun file ->
          List.exists (fun prefix -> String.starts_with ~prefix file) prefixes)

let snapshot_coverage run =
  (* When instrumented code registered in-process coverage, snapshot it
     into the run record; renderers project it like any other run data. *)
  let collection = coverage_scope () (Windtrap_coverage.snapshot ()) in
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

(* The execute-and-report spine *)

(* The producers above answer "who writes this line"; this answers "in
   what order", which is the other half of the same doctrine — two
   runners composing the same producers differently drift exactly as
   badly as two runners forking one. So the order lives here once: build
   the renderer and the observer, open the GitHub envelope, run the
   suite, and project the run into every sink.

   What the callers keep is what genuinely differs. The two header
   policies stay parameters, because the runners really do disagree:
   [seed] (the root seed iff the suite declares property tests, [None]
   inline) and [selection] (the description an empty run explains itself
   with, [None] inline — see the .mli). The [Error] arm prints the
   startup message here and hands the error back, because the library
   runner exits on it while the inline runner folds its code into dune's
   promotion protocol. Everything after the report — JUnit, the focus
   warning, .corrected flushing, the exit — is the caller's, and so are
   the invocation context, the GitHub gating decision, and the listing a
   [--list] run prints. *)
let execute_and_report ?(on_event = fun (_ : Runner.event) -> ()) ~invocation
    ~seed ~selection ~github ~output ~coverage_mode ~config ~suite tests =
  let renderer = renderer ~config ~mode:output ~invocation () in
  (* [Runner.execute]'s [?on_event] has one slot and the transcript owns
     it. A second subscriber composes here rather than replacing it, in a
     fixed order — transcript first — so no caller can drop the run's own
     output by subscribing, and none can reorder it. *)
  let transcript = observe renderer ~seed ~selection in
  let on_event event =
    transcript event;
    on_event event
  in
  github_start ~github suite;
  match Runner.execute ~on_event ~config ~suite tests with
  | Error error ->
      github_end ~github;
      prerr_endline (Runner.startup_message error);
      Error error
  | Ok outcome when config.Run.list_only ->
      (* Nothing ran, so there is nothing to project: [Runner.execute]
         applied the startup checks and the selection and stopped. The
         caller prints the listing it asked for. *)
      Ok (outcome, [])
  | Ok outcome ->
      (* Release failures and a strict stale-baseline verdict ride with the
         results: both are part of the run's verdict (both set the exit
         code), so every sink must see them. *)
      let results =
        results_with_releases outcome
        @ stale_baseline_results ~invocation outcome
      in
      let coverage_data = snapshot_coverage outcome.Runner.run in
      Render.finish renderer
        ?coverage:(coverage_summary ~coverage_mode outcome.Runner.run)
        ~results ~duration:outcome.Runner.duration ();
      coverage_report renderer ~coverage_mode outcome.Runner.run coverage_data;
      report_snapshots ~out:Format.std_formatter ~output ~invocation outcome;
      github_end ~github;
      (* After [github_end], deliberately: an ::error:: block written
         inside the ::group:: envelope folds away with the transcript,
         and the annotations are the part a reviewer must see without
         unfolding anything. *)
      github_annotations ~github ~invocation results;
      Format.pp_print_flush Format.std_formatter ();
      Format.pp_print_flush Format.err_formatter ();
      Ok (outcome, results)
