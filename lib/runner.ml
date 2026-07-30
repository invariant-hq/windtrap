(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   The sequential drive loop, SIGALRM timeout, retry loop, and last-failed
   persistence derive from windtrap v1's lib/runner.ml, rebuilt over the
   per-run record (Run), data-form brackets (Test_tree), and the typed
   failure model, with the boundary capturing body and teardown outcomes
   independently.
  ---------------------------------------------------------------------------*)

(* The exit guard. Registered once per process (registration state, like
   Run's fixture ids, is process identity, not run state);
   per-run data flows through the ambient slot. Stdlib.at_exit runs each
   registered function at most once, so an interception consumes the
   registration: the guard re-arms itself before raising. Relies on
   Stdlib.exit = do_at_exit (); sys_exit — an exception from an at_exit
   function propagates to exit's caller (pinned by the child-status
   regression test). *)
let rec exit_guard () =
  if Run.active () then begin
    at_exit exit_guard;
    raise Failure.Exit_attempt
  end

let exit_guard_installed = ref false

let install_exit_guard () =
  if not !exit_guard_installed then begin
    exit_guard_installed := true;
    at_exit exit_guard
  end

(* Property tests *)

(* The channel between a [prop] body and the classifier below: the body
   always raises its engine outcome, the Body-phase guard of the same test
   consumes it. Never installed in run state and never visible
   to user code — the raise happens after the user's law returned. *)
exception Prop_outcome of Property.outcome

let prop ?pos ?tags ?timeout ?count ?examples name gen law =
  let loc = Loc.resolve ?pos () in
  let body () =
    let frame = Run.current_frame () in
    let config = Run.config (Run.run_of_frame frame) in
    (* Case count: declaration site > --prop-count > engine default. A
       config-sourced count also rides failure payloads ([config_count]) so
       the replay hint can restate the flag — a declaration-site count
       replays by itself. *)
    let count, config_count =
      match count with
      | Some _ -> (count, None)
      | None -> (config.Run.prop_count, config.Run.prop_count)
    in
    let path = Test_tree.path_to_string (Run.path frame) in
    let outcome =
      Property.run ?loc ?count ?config_count ?examples ~root:config.Run.seed
        ~path gen (fun context value ->
          Run.with_prop_context frame context (fun () -> law value))
    in
    raise (Prop_outcome outcome)
  in
  Test_tree.test ?pos ?tags ?timeout name body

let coverage_failure ?loc (stats : Property.stats) =
  let unsatisfied =
    List.filter (fun c -> not c.Property.satisfied) stats.Property.coverage
  in
  let describe c =
    Pp.str "%s %.1f%% < %.1f%% (%d hits)" c.Property.label c.Property.actual
      c.Property.required c.Property.hits
  in
  Failure.message ?loc
    ("coverage requirement not met: "
    ^ String.concat "; " (List.map describe unsatisfied))

let gave_up_failure ?loc (stats : Property.stats) =
  Failure.message ?loc
    (Pp.str
       "property gave up: %d discards exhausted the generation budget (%d \
        cases passed)"
       stats.Property.discards stats.Property.cases)

(* The per-test boundary *)

(* Root for the per-test global [Random] reseed: an arbitrary frozen
   constant, deliberately not the run's root seed — the property path never
   touches [Random], and [Random] users get a stream that depends on the
   test's path only. *)
let random_reseed_root = 0x57696e6474726170L

let with_isolated_random ~path fn =
  let saved = Random.get_state () in
  Random.init
    (Int64.to_int (Seed.derive ~root:random_reseed_root ~path ~index:0));
  Fun.protect ~finally:(fun () -> Random.set_state saved) fn

(* One SIGALRM window arming [limit] seconds around setup + body + teardown.
   This hand-rolls the cleanup instead of Fun.protect: the alarm can expire
   exactly as the scope exits and be delivered at a poll point inside the
   cleanup itself, and that late [Timeout] must be absorbed — never surface
   as [Finally_raised] or leak into caller code. The [armed] flag inertizes
   the handler; [disarm] retries once around a delivery that interrupts it
   (the one-shot timer fires at most once). *)
let with_timeout limit fn =
  match limit with
  | None -> fn ()
  | Some _ when Sys.win32 -> fn () (* documented no-op *)
  | Some limit -> (
      let armed = ref true in
      let previous_handler =
        Sys.signal Sys.sigalrm
          (Sys.Signal_handle
             (fun _ -> if !armed then raise (Failure.Timeout limit)))
      in
      let set_timer seconds =
        ignore
          (Unix.setitimer Unix.ITIMER_REAL
             { Unix.it_value = seconds; it_interval = 0. })
      in
      let rec disarm () =
        match
          armed := false;
          set_timer 0.;
          Sys.set_signal Sys.sigalrm previous_handler
        with
        | () -> ()
        | exception Failure.Timeout _ -> disarm ()
      in
      set_timer limit;
      match fn () with
      | value ->
          disarm ();
          value
      | exception exn ->
          let backtrace = Printexc.get_raw_backtrace () in
          disarm ();
          Printexc.raise_with_backtrace exn backtrace)

let timeout_failure ?loc limit =
  Failure.message ?loc (Pp.str "timed out after %gs" limit)

(* Expected failures *)

(* Whether a raw attempt outcome counts as failed for retries, --bail, the
   exit code, and the last-failed store, under the test's expectation: an
   expected failure does not count; an unexpected pass does; skips never
   count. Defined over the outcome as classified — the synthesized xfail-pass
   failure below is applied only after this decision. *)
let counts_failed ~(xfail : Test_tree.xfail option) (outcome : Failure.outcome)
    =
  match (outcome, xfail) with
  | Failure.Fail _, None -> true
  | Failure.Fail _, Some _ -> false
  | Failure.Pass, Some _ -> true
  | Failure.Pass, None | Failure.Skip _, _ -> false

(* The recorded failure of an [xfail] test that passed: a self-describing
   Message entry, so today's renderers already explain the red result. *)
let xpass_failure (case : Test_tree.case) =
  let reason =
    match case.Test_tree.xfail with
    | Some { Test_tree.reason = Some reason } -> " (" ^ reason ^ ")"
    | Some { Test_tree.reason = None } | None -> ""
  in
  Failure.message ?loc:case.Test_tree.loc
    (Pp.str "expected to fail%s, but the test passed" reason)

(* One attempt of one test: fresh frame installed by the caller. Returns the
   classified outcome and, for property tests, the engine's stats. Fatal
   exceptions propagate. *)
let run_attempt run frame (case : Test_tree.case) ~limit ~groups ~test_name =
  let prop_stats = ref None in
  let skipped = ref None in
  let phase = ref Failure.Body in
  let record_failure ph failure =
    Run.add_failure frame (Failure.with_phase ph failure)
  in
  let record_prop_outcome ph = function
    | Property.Pass stats -> prop_stats := Some stats
    | Property.Fail { failure; stats } ->
        prop_stats := Some stats;
        record_failure ph failure
    | Property.Coverage_failed stats ->
        prop_stats := Some stats;
        record_failure ph (coverage_failure ?loc:case.Test_tree.loc stats)
    | Property.Gave_up stats ->
        prop_stats := Some stats;
        record_failure ph (gave_up_failure ?loc:case.Test_tree.loc stats)
  in
  let classify ph exn backtrace =
    match exn with
    | Prop_outcome outcome -> record_prop_outcome ph outcome
    | Failure.Check_failure failure -> record_failure ph failure
    | Failure.Skip_test reason -> if !skipped = None then skipped := Some reason
    | Failure.Timeout limit ->
        record_failure ph (timeout_failure ?loc:case.Test_tree.loc limit)
    | Failure.Exit_attempt ->
        record_failure ph
          (Failure.message ?loc:case.Test_tree.loc
             "the test called exit — intercepted; a test must return or raise, \
              never exit the process")
    | exn ->
        record_failure ph
          (Failure.raised ?loc:case.Test_tree.loc
             ~actual:(Printexc.to_string exn)
             ~backtrace:(Printexc.raw_backtrace_to_string backtrace)
             ())
  in
  (* Run one phase, classifying everything non-fatal it raises. [Loc.delimit]
     bounds location capture: a tail-called assertion whose own frame is gone
     yields no location here rather than the runner's caller. *)
  let guard : type r. Failure.phase -> (unit -> r) -> r option =
   fun ph fn ->
    phase := ph;
    match Loc.delimit fn with
    | value -> Some value
    | exception exn when not (Failure.is_fatal exn) ->
        let backtrace = Printexc.get_raw_backtrace () in
        classify ph exn backtrace;
        None
  in
  let phases () =
    match case.Test_tree.body with
    | Test_tree.Body fn -> ignore (guard Failure.Body fn)
    | Test_tree.Bracket { setup; body; teardown } -> (
        match guard Failure.Setup setup with
        | None -> () (* teardown runs iff setup succeeded *)
        | Some resource ->
            (* Body and teardown outcomes are captured independently — one
               failure entry per phase, neither masking the other; teardown
               runs on every body outcome, skip and timeout included. Never
               Fun.protect at this boundary. *)
            ignore (guard Failure.Body (fun () -> body resource));
            ignore (guard Failure.Teardown (fun () -> teardown resource)))
  in
  let boundary () =
    with_isolated_random ~path:(Test_tree.path_to_string case.Test_tree.path)
      (fun () ->
        with_timeout limit (fun () ->
            match phases () with
            | () -> ()
            | exception Failure.Timeout limit ->
                (* The alarm fired between two phase guards. *)
                record_failure !phase
                  (timeout_failure ?loc:case.Test_tree.loc limit)))
  in
  (* Scratch removal runs after the attempt, outside the
     timeout window and the capture redirection, on every path where the
     runner regains control — only a fatal exception escapes the frame, and
     it too passes through the cleanup. [remove_temp] never
     raises. *)
  (match
     Run.with_frame frame (fun () ->
         match
           Capture.with_capture (Run.capture run) ~groups ~test_name boundary
         with
         | () -> ()
         | exception exn when not (Failure.is_fatal exn) ->
             (* Capture setup or restore failed (e.g. the log file could not
                be created): a failure of this test, not of the run. *)
             let backtrace = Printexc.get_raw_backtrace () in
             classify Failure.Body exn backtrace)
   with
  | () -> Run.remove_temp frame
  | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      Run.remove_temp frame;
      Printexc.raise_with_backtrace exn backtrace);
  let outcome =
    match Run.failures frame with
    | [] -> (
        match !skipped with
        | Some reason -> Failure.Skip reason
        | None -> Failure.Pass)
    | failures -> Failure.Fail failures
  in
  (outcome, !prop_stats)

(* A failing test's report carries its bounded captured output — the
   final attempt's, attached to the first failure entry. *)
let attach_tail capture outcome =
  match outcome with
  | Failure.Fail (first :: rest) -> (
      match Capture.output_tail capture with
      | Some tail -> Failure.Fail (Failure.with_output_tail tail first :: rest)
      | None -> outcome)
  | outcome -> outcome

let split_last path =
  match List.rev path with
  | [] -> ([], "unnamed") (* Test_tree.case paths are never empty *)
  | name :: rev_groups -> (List.rev rev_groups, name)

(* Events *)

type event =
  | Run_started of { run : Run.t; suite : string; total : int; selected : int }
  | Test_started of { path : string list }
  | Test_finished of Run.result
  | Fixture_release of { name : string }

(* Runs one test to completion (retries included), records its result, and
   returns it with whether it counted as failed (see [counts_failed]) — the
   caller drives --bail, the exit code, and the last-failed store from the
   flag, never from the recorded outcome alone. *)
let run_case ~on_event run (case : Test_tree.case) =
  on_event (Test_started { path = case.Test_tree.path });
  let config = Run.config run in
  let limit =
    match case.Test_tree.timeout with
    | Some _ as declared -> declared
    | None -> config.Run.timeout
  in
  let groups, test_name = split_last case.Test_tree.path in
  let total_attempts = case.Test_tree.retries + 1 in
  let rec attempt number spent =
    let frame =
      Run.frame run ~path:case.Test_tree.path ~file:case.Test_tree.file
        ~loc:case.Test_tree.loc
    in
    let start = Clock.counter () in
    let outcome, prop_stats =
      run_attempt run frame case ~limit ~groups ~test_name
    in
    let duration = spent +. Clock.count_s start in
    let failed = counts_failed ~xfail:case.Test_tree.xfail outcome in
    if failed && number < total_attempts then attempt (number + 1) duration
    else
      let outcome =
        (* An xfail test that passed is recorded as a failure with a
           self-describing message; an xfail test that failed keeps its real
           failures (and [failed] above already excused them). *)
        match (outcome, case.Test_tree.xfail) with
        | Failure.Pass, Some _ -> Failure.Fail [ xpass_failure case ]
        | outcome, _ -> outcome
      in
      let outcome = attach_tail (Run.capture run) outcome in
      let result =
        {
          Run.path = case.Test_tree.path;
          outcome;
          (* The three rendering facts computed here and nowhere else (the
             record is the contract): whether the result counted as failed,
             the expectation annotation, and the slow-tag decision bit. *)
          counted = failed;
          xfail = case.Test_tree.xfail;
          slow_tagged = Tag.mem Tag.slow case.Test_tree.tags;
          duration;
          attempts = number;
          prop_stats;
          srandom_root =
            (* The recorded attempt's draw record: [Some root] exactly when
               the test called [srandom], so its failure block can print the
               replay line. *)
            (if Run.srandom_used frame then Some config.Run.seed else None);
        }
      in
      Run.record run result;
      on_event (Test_finished result);
      (result, failed)
  in
  attempt 1 0.

(* Selection *)

(* Frozen root for --shard bucketing: buckets must be a pure
   function of the test's path — never of the run's seed — so they are
   stable across runs, machines, and suite composition. Frozen with
   Seed.derive; changing either silently repartitions every sharded CI
   matrix. *)
let shard_root = 0x77696e6473687264L (* "windshrd" *)

let shard_bucket ~shards path =
  Int64.to_int
    (Int64.unsigned_rem
       (Seed.derive ~root:shard_root ~path ~index:0)
       (Int64.of_int shards))

let selection_predicate (config : Run.config) =
  let require p tag = Tag.require tag p and drop p tag = Tag.drop tag p in
  let predicate =
    List.fold_left require Tag.default_predicate config.Run.tags
  in
  let predicate = List.fold_left drop predicate config.Run.exclude_tags in
  if config.Run.quick then Tag.drop Tag.slow predicate else predicate

let case_selected (config : Run.config) ~predicate ~allowlist ~focus_active
    (case : Test_tree.case) =
  let path = Test_tree.path_to_string case.Test_tree.path in
  let contains pattern = Text.contains_substring ~pattern path in
  (match config.Run.filter with None -> true | Some p -> contains p)
  && (match config.Run.exclude with None -> true | Some p -> not (contains p))
  && Tag.accepts predicate case.Test_tree.tags
  && (match allowlist with
    | None -> true
    | Some entries -> List.mem path entries)
  && (match config.Run.shard with
    | None -> true
    | Some (k, shards) -> shard_bucket ~shards path = k - 1)
  && ((not focus_active) || case.Test_tree.focused)

let duplicate_paths paths =
  let seen = Hashtbl.create 64 in
  List.filter
    (fun path ->
      let dup = Hashtbl.mem seen path in
      Hashtbl.replace seen path ();
      dup)
    paths
  |> List.sort_uniq String.compare

(* The last-failed store *)

(* The format is explicitly unstable: a magic first
   line, then one String.escaped test path per line. Unrecognized content
   reads as empty; I/O errors are swallowed — the store only feeds
   [--failed], it must never fail a run. *)
let store_magic = "windtrap-last-failed 1"

let store_path (config : Run.config) ~suite =
  Filename.concat
    (Filename.concat config.Run.log_dir (Path_ops.sanitize_component suite))
    ".last-failed"

let read_store path =
  match open_in_bin path with
  | exception Sys_error _ -> []
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          match input_line ic with
          | exception End_of_file -> []
          | magic when not (String.equal magic store_magic) -> []
          | _magic ->
              let seen = Hashtbl.create 16 in
              let rec lines acc =
                match input_line ic with
                | exception End_of_file -> List.rev acc
                | line -> (
                    match Scanf.unescaped line with
                    | "" -> lines acc
                    | entry when Hashtbl.mem seen entry -> lines acc
                    | entry ->
                        Hashtbl.replace seen entry ();
                        lines (entry :: acc)
                    | exception _ -> lines acc)
              in
              lines [])

let write_store path entries =
  let buffer = Buffer.create 256 in
  Buffer.add_string buffer store_magic;
  Buffer.add_char buffer '\n';
  List.iter
    (fun entry ->
      Buffer.add_string buffer (String.escaped entry);
      Buffer.add_char buffer '\n')
    entries;
  match
    Path_ops.mkdir_p (Filename.dirname path);
    Atomic_file.write ~path (Buffer.contents buffer)
  with
  | () -> ()
  | exception Sys_error _ -> ()
  | exception Unix.Unix_error _ -> ()

(* Startup errors *)

type startup_error =
  | Duplicate_paths of string list
  | Focused_in_ci of ([ `Ftest | `Fgroup ] * Loc.t option) list
  | Update_refused_in_ci
  | No_recorded_failures

let startup_exit_code = function
  | No_recorded_failures -> 2
  | Duplicate_paths _ | Focused_in_ci _ | Update_refused_in_ci -> 1

let startup_message = function
  | Duplicate_paths paths ->
      Pp.str "duplicate test paths:\n  %s\nEvery full test path must be unique."
        (String.concat "\n  " paths)
  | Focused_in_ci sites ->
      let site (kind, loc) =
        let name = match kind with `Ftest -> "ftest" | `Fgroup -> "fgroup" in
        match loc with
        | Some loc -> Pp.str "%s at %s" name (Loc.to_string loc)
        | None -> name
      in
      Pp.str
        "focused tests committed (%s); remove ftest/fgroup or set \
         WINDTRAP_ALLOW_FOCUS=1 to run them under CI"
        (String.concat ", " (List.map site sites))
  | Update_refused_in_ci ->
      "snapshot update refused: CI is set. Set WINDTRAP_UPDATE=force to update \
       baselines on a CI machine."
  | No_recorded_failures -> "no recorded failures match the current suite"

(* Outcomes *)

type outcome = {
  run : Run.t;
  selected : Test_tree.case list;
  total : int;
  focus_active : bool;
  bailed : bool;
  failed_paths : string list;
  release_failures : Failure.t list;
  orphans : string list;
  pruned : (string list, Snapshot.prune_refusal) result option;
  duration : float;
  exit_code : int;
}

let release ~on_event run =
  Run.release_fixtures run ~announce:(fun name ->
      on_event (Fixture_release { name }))

let execute ?(on_event = fun _ -> ()) ~config ~suite tests =
  install_exit_guard ();
  if Run.active () then
    invalid_arg
      "windtrap: run is already active — a test body cannot start another run";
  (* The CLI layer validates every layer it resolves; only a hand-built
     configuration can carry a malformed shard, and it must fail loudly
     before selection divides by N. *)
  (match config.Run.shard with
  | Some (k, n) when k < 1 || n < k ->
      invalid_arg "windtrap: shard must be K/N with 1 <= K <= N"
  | Some _ | None -> ());
  let started = Clock.counter () in
  let cases = Test_tree.flatten tests in
  let total = List.length cases in
  let paths =
    List.map (fun case -> Test_tree.path_to_string case.Test_tree.path) cases
  in
  let focus_active = Test_tree.has_focus tests in
  let in_ci = Env.in_ci () in
  match duplicate_paths paths with
  | _ :: _ as duplicates -> Error (Duplicate_paths duplicates)
  | [] -> (
      if focus_active && in_ci && not config.Run.allow_focus then
        Error (Focused_in_ci (Test_tree.focus_sites tests))
      else
        match Snapshot.resolve_mode ~ci:in_ci config.Run.update with
        | Snapshot.Refused_in_ci -> Error Update_refused_in_ci
        | Snapshot.Mode mode -> (
            let allowlist =
              if config.Run.failed_only then
                let recorded = read_store (store_path config ~suite) in
                Some (List.filter (fun p -> List.mem p paths) recorded)
              else None
            in
            match allowlist with
            | Some [] -> Error No_recorded_failures
            | _ ->
                let predicate = selection_predicate config in
                let selected =
                  List.filter
                    (case_selected config ~predicate ~allowlist ~focus_active)
                    cases
                in
                if config.Run.list_only then
                  Ok
                    {
                      run =
                        Run.create config ~capture:Capture.disabled
                          ~snapshots:(Snapshot.create ~mode ());
                      selected;
                      total;
                      focus_active;
                      bailed = false;
                      failed_paths = [];
                      release_failures = [];
                      orphans = [];
                      pruned = None;
                      duration = Clock.count_s started;
                      exit_code = 0;
                    }
                else
                  let capture =
                    if config.Run.stream then Capture.disabled
                    else Capture.create ~log_dir:config.Run.log_dir ~suite ()
                  in
                  let snapshots = Snapshot.create ~mode () in
                  let run = Run.create config ~capture ~snapshots in
                  (* The executing span: everything from the first event to
                     the completed outcome runs with the slot marked, so the
                     exit guard covers fixture release, observers, and store
                     maintenance — not only test attempts. On the fatal path
                     the protect empties the slot before the exception leaves
                     [execute], so the guard is inert during fatal
                     termination. *)
                  Run.with_active run @@ fun () ->
                  on_event
                    (Run_started
                       { run; suite; total; selected = List.length selected });
                  let bailed = ref false in
                  (* Effective failures (see [counts_failed]): what --bail,
                     the exit code, and the store react to. Expected [xfail]
                     failures are recorded but never accumulate here. *)
                  let rev_failed = ref [] in
                  (try
                     List.iter
                       (fun case ->
                         if not !bailed then begin
                           let _result, failed = run_case ~on_event run case in
                           if failed then
                             rev_failed :=
                               Test_tree.path_to_string case.Test_tree.path
                               :: !rev_failed;
                           match config.Run.bail with
                           | Some limit when List.length !rev_failed >= limit ->
                               bailed := true
                           | Some _ | None -> ()
                         end)
                       selected
                   with exn ->
                     (* Release on every path where the runner regains
                        control. Test-level exceptions were classified inside
                        the boundary, so only a fatal exception or a raising
                        [on_event] observer reaches here: release best effort
                        — announcements swallowed too, a teardown must not be
                        lost to an observer that keeps raising — then the
                        exception wins. *)
                     let backtrace = Printexc.get_raw_backtrace () in
                     let announce name =
                       try on_event (Fixture_release { name }) with _ -> ()
                     in
                     (try ignore (Run.release_fixtures run ~announce)
                      with _ -> ());
                     Printexc.raise_with_backtrace exn backtrace);
                  (* Releases run after the last test, outside any per-test
                     timeout, including under --bail. *)
                  let release_failures = release ~on_event run in
                  Capture.link_latest capture;
                  let results = Run.results run in
                  let executed =
                    List.map
                      (fun r -> Test_tree.path_to_string r.Run.path)
                      results
                  in
                  let failed_paths = List.rev !rev_failed in
                  (* Recorded [Fail] outcomes, expected or not: snapshot
                     orphan reporting and --prune gate on them — an xfail
                     body did not complete, so its snapshots may be stale. *)
                  let raw_failed_count =
                    List.length
                      (List.filter
                         (fun r ->
                           match r.Run.outcome with
                           | Failure.Fail _ -> true
                           | Failure.Pass | Failure.Skip _ -> false)
                         results)
                  in
                  let skipped_count =
                    List.length
                      (List.filter
                         (fun r ->
                           match r.Run.outcome with
                           | Failure.Skip _ -> true
                           | Failure.Pass | Failure.Fail _ -> false)
                         results)
                  in
                  (* A full run executed the entire declared suite: only such
                     a run may drop store entries for tests that no longer
                     exist, report orphans, or prune. *)
                  let full = (not !bailed) && List.length results = total in
                  let survivors =
                    if full then []
                    else
                      List.filter
                        (fun p -> not (List.mem p executed))
                        (read_store (store_path config ~suite))
                  in
                  write_store (store_path config ~suite)
                    (failed_paths @ survivors);
                  let focused_count =
                    List.length
                      (List.filter (fun case -> case.Test_tree.focused) cases)
                  in
                  let clean =
                    full && skipped_count = 0 && raw_failed_count = 0
                    && focused_count = 0
                  in
                  let orphans =
                    if clean then Snapshot.orphans snapshots else []
                  in
                  let pruned =
                    if config.Run.prune then
                      Some
                        (Snapshot.prune snapshots ~filtered:(not full)
                           ~skipped:skipped_count ~failed:raw_failed_count
                           ~focused:focused_count)
                    else None
                  in
                  let any_failure =
                    failed_paths <> [] || release_failures <> []
                  in
                  let exit_code =
                    if any_failure then 1 else if results = [] then 2 else 0
                  in
                  Ok
                    {
                      run;
                      selected;
                      total;
                      focus_active;
                      bailed = !bailed;
                      failed_paths;
                      release_failures;
                      orphans;
                      pruned;
                      duration = Clock.count_s started;
                      exit_code;
                    }))
