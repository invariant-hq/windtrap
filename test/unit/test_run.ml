(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Run, every run made by [execute]. First the configuration
   defaults and the run record as a body and an observer see it: the
   ambient slot (outside-run errors, isolation between sequential runs),
   the location fallback, baseline checkpoints, the fixture lifecycle
   (sharing, cached failures and skips, raising and fatal releases),
   subtests, temporary paths and the property context of a stateful
   command. Then the executor: the per-test boundary matrix (body x
   teardown x timeout), the scoped boundary, classification, retries,
   capture-tail attachment, the global Random reseed, selection (filters,
   tags, focus, sharding, the CI guards), exit codes including the
   all-skipped ratification, expected failures, subtest and scratch-path
   cleanup through the boundary, fixture release under bail and on
   release failure, events, the property wiring, the baseline guard, the
   gating of corrections, and the last-failed store round trip. Plain
   executable: [execute] refuses to nest inside an active run, so this
   suite cannot host its own assertions under the windtrap runner. *)

open Windtrap
open Windtrap.Private
open Harness

let () = init "run"

exception Boom
exception No_db

(* Configuration *)

let () =
  let config = Run.default_config () in
  check "default config: root seed is a valid token"
    (String.length (Seed.to_string config.Run.seed) = 19);
  check "default config: no filters"
    (config.Run.filter = [] && config.Run.exclude = []);
  check "default config: no tags"
    (config.Run.tags = [] && config.Run.exclude_tags = []);
  check "default config: flags off"
    ((not config.Run.failed_only)
    && (not config.Run.stream) && not config.Run.allow_focus);
  check "default config: baselines are checked, not written"
    (config.Run.baseline = Baseline.Check);
  check "default config: no limits"
    ((not config.Run.bail) && config.Run.timeout = None
    && config.Run.prop_count = None
    && config.Run.shard = None);
  check "default config: log dir is set" (config.Run.log_dir <> "");
  check "default config: not a mutation run"
    (config.Run.mutation = Run.No_mutation)

(* [for_subset] is the loop's child configuration: every path-selecting
   knob cleared, the tag knobs and the seed kept, and the child no
   mutation run of its own — its parent is the loop, and it arms what it
   is handed. A knob this forgets gives the child a selection its
   parent's tree already applied. *)
let () =
  let parent =
    {
      (Run.default_config ()) with
      Run.seed = 0x5eedL;
      filter = [ "f" ];
      exclude = [ "e" ];
      shard = Some (1, 2);
      failed_only = true;
      tags = [ "t" ];
      exclude_tags = [ "x" ];
      stream = true;
      baseline = Baseline.Update;
      junit = Some "out.xml";
      mutation = Run.Loop [ "lib/" ];
    }
  in
  let child = Run.for_subset parent ~log_dir:"/tmp/child" ~bail:true in
  check "for_subset: path selection cleared"
    (child.Run.filter = [] && child.Run.exclude = [] && child.Run.shard = None
   && not child.Run.failed_only);
  check "for_subset: tags and seed kept"
    (child.Run.tags = [ "t" ]
    && child.Run.exclude_tags = [ "x" ]
    && child.Run.seed = 0x5eedL);
  check "for_subset: read-only, silent, unreported"
    (child.Run.baseline = Baseline.Check
    && (not child.Run.stream) && child.Run.junit = None);
  check "for_subset: focus allowed, the caller's log dir and bail"
    (child.Run.allow_focus && child.Run.log_dir = "/tmp/child" && child.Run.bail);
  check "for_subset: the child is no mutation run"
    (child.Run.mutation = Run.No_mutation)

(* ------------------------------------------------------------------ *)
(* Harness *)
(* ------------------------------------------------------------------ *)

let with_temp_root f = with_temp_root ~prefix:"windtrap-runner-" f

let base_config ~log_dir () =
  { (Run.default_config ()) with Run.seed = 0x5eedL; log_dir }

let expect_run name ?on_event ~config ?(suite = "suite") tests f =
  match Run.execute ?on_event config ~suite tests with
  | Ok outcome -> f outcome
  | Error error ->
      check name false;
      Printf.printf "  startup error: %s\n%!" (Run.startup_message error)

let expect_startup_error name ~config ?(suite = "suite") tests pred =
  match Run.execute config ~suite tests with
  | Ok _ -> check (name ^ " (run was not refused)") false
  | Error error -> check name (pred error)

let result_of outcome path =
  List.find_opt (fun r -> r.Run.path = path) (Run.results outcome.Run.run)

let outcome_of outcome path =
  match result_of outcome path with
  | Some r -> Some r.Run.outcome
  | None -> None

let failure_list = function
  | Some (Failure.Fail fs) -> fs
  | Some Failure.Pass | Some (Failure.Skip _) | None -> []

let phases_of fs = List.map (fun f -> f.Failure.phase) fs

(* The words of a failure the library words itself: a message, or a
   timeout as its block reads. *)
let message_of (f : Failure.t) =
  match f.Failure.kind with
  | Failure.Message m -> m.Failure.kept
  | Failure.Timeout _ -> Report_sections.headline f
  | _ -> "<not a message>"

(* Whether [loc] is the site of [pos], a [__POS__] of this file. *)
let at_pos (file, line, _, _) = function
  | Some (loc : Loc.t) ->
      Filename.basename loc.Loc.file = Filename.basename file
      && loc.Loc.line = line
  | None -> false

(* The displayed label: the sub-case components joined with the user's
   annotation — the derivation renderers share ([Report.labeled_msg]).
   Recording keeps [msg] purely the user's; the label is data. *)
let msg_of (f : Failure.t) =
  Option.value (Report.labeled_msg f) ~default:"<none>"

(* ------------------------------------------------------------------ *)
(* The run record and the running test *)
(* ------------------------------------------------------------------ *)

let () =
  expect_invalid_arg "current_frame outside a run raises" (fun () ->
      Run.current_frame ());
  expect_invalid_arg "current outside a run raises" (fun () -> Run.current ());
  expect_invalid_arg "current_test outside a run raises" (fun () ->
      Run.current_test ())

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let inside = ref None in
  let tests =
    [
      Test_tree.group "g"
        [
          Test_tree.test "t" (fun () ->
              inside :=
                Some
                  ( Run.current (),
                    Run.current_test (),
                    Run.prop_context (Run.current_frame ()) ));
        ];
    ]
  in
  (* The observer runs inside the active run, and between attempts: no
     frame is current while it runs. *)
  let observed = ref [] in
  let on_event (_ : Run.event) =
    let frame_refused =
      match Run.current_frame () with
      | _ -> false
      | exception Invalid_argument _ -> true
    in
    observed := (Run.active (), frame_refused) :: !observed
  in
  expect_run "record suite runs" ~on_event ~config tests @@ fun outcome ->
  check "the record keeps the config" (Run.config outcome.Run.run == config);
  (match !inside with
  | Some (run, path, context) ->
      check "a body's run is the record that execute returns"
        (run == outcome.Run.run);
      check "current_test is the executing test's full path"
        (path = [ "g"; "t" ]);
      check "a plain test has no property context" (context = None)
  | None -> check "the body ran" false);
  check "the observer runs in the active run, with no frame current"
    (!observed <> [] && List.for_all (fun (a, r) -> a && r) !observed);
  check "active is false after execute returns" (not (Run.active ()));
  expect_invalid_arg "no frame outlives the run" (fun () ->
      Run.current_frame ())

let () =
  (* Sequential runs are isolated: each body sees its own run, and the
     slot is empty between them. *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let seen = ref [] in
  let tests =
    [ Test_tree.test "t" (fun () -> seen := Run.current () :: !seen) ]
  in
  expect_run "first isolated run" ~config tests @@ fun first ->
  expect_invalid_arg "the slot is empty between runs" (fun () -> Run.current ());
  expect_run "second isolated run" ~config tests @@ fun second ->
  check "each run's body sees its own record"
    (match !seen with
    | [ b; a ] -> a == first.Run.run && b == second.Run.run && a != b
    | _ -> false)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  (match
     Run.execute
       ~on_event:(fun _ -> raise Boom)
       config ~suite:"suite"
       [ Test_tree.test "t" ignore ]
   with
  | _ -> check "an observer's exception leaves execute" false
  | exception Boom -> check "an observer's exception leaves execute" true);
  check "the slot is emptied after a run an exception ended"
    (not (Run.active ()))

(* Location fallback. A tail-position failure and a given location are
   pinned with the boundary (D4); here, a property's. *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let pos = __POS__ in
  let tests =
    [
      Run.prop ~__POS__:pos ~count:1 "prop" (Gen.constant 0) (fun _ ->
          raise
            (Failure.Check_failure
               (Failure.equality ~expected:"0" ~actual:"1" ())));
    ]
  in
  expect_run "location suite runs" ~config tests @@ fun outcome ->
  match failure_list (outcome_of outcome [ "prop" ]) with
  | [ f ] ->
      check "a property failure takes the declaration site"
        (at_pos pos f.Failure.loc);
      check "its inner failure is left as it is"
        (match f.Failure.kind with
        | Failure.Property { inner = Some i; _ } -> i.Failure.loc = None
        | _ -> false)
  | _ -> check "one property failure" false

(* Baseline checkpoints: a mismatch is recorded and the call returns, so a
   body with two stale expectations reports both; only an unprovable path
   raises. *)
let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let literal line =
    Baseline.Literal
      { pos = ("t.ml", line, 2, 20); value = "old"; exact = true }
  in
  let reached = ref 0 and after_unprovable = ref false in
  let tests =
    [
      Test_tree.test "two" (fun () ->
          Run.check_baseline (literal 1) "new";
          incr reached;
          Run.check_baseline (literal 2) "new";
          incr reached);
      Test_tree.test "unprovable" (fun () ->
          Run.check_baseline
            (Baseline.Literal
               { pos = ("../outside.ml", 1, 0, 0); value = "old"; exact = true })
            "new";
          after_unprovable := true);
    ]
  in
  expect_run "checkpoint suite runs" ~config tests @@ fun outcome ->
  check_int "both checkpoints ran" ~expected:2 ~actual:!reached;
  let fs = failure_list (outcome_of outcome [ "two" ]) in
  check "both mismatches are recorded"
    (match fs with
    | [
     { Failure.kind = Failure.Baseline { state = Failure.Mismatch _; _ }; _ };
     { Failure.kind = Failure.Baseline { state = Failure.Mismatch _; _ }; _ };
    ] ->
        true
    | _ -> false);
  check "a recorded checkpoint carries no subtest label"
    (List.for_all (fun (f : Failure.t) -> f.Failure.subtest = []) fs);
  check "an unprovable path raises out of the body"
    ((not !after_unprovable)
    &&
    match failure_list (outcome_of outcome [ "unprovable" ]) with
    | [
     {
       Failure.kind = Failure.Baseline { state = Failure.Unresolvable _; _ };
       _;
     };
    ] ->
        true
    | _ -> false)

(* Fixtures *)

let () =
  let acquisitions = ref 0 in
  let accessor =
    Run.fixture (fun () ->
        incr acquisitions;
        ref 41)
  in
  check_int "creating a fixture accessor acquires nothing" ~expected:0
    ~actual:!acquisitions;
  expect_invalid_arg "fixture accessor outside a run raises" (fun () ->
      accessor ());
  check_int "the outside-run error does not acquire" ~expected:0
    ~actual:!acquisitions;
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let got = ref [] in
  let use () = got := accessor () :: !got in
  expect_run "fixture sharing suite runs" ~config
    [ Test_tree.test "first" use; Test_tree.test "second" use ]
  @@ fun _ ->
  check_int "first use acquires, and later uses in the run do not" ~expected:1
    ~actual:!acquisitions;
  check "later uses in the run share the cached resource"
    (match !got with [ b; a ] -> a == b | _ -> false);
  (* Per-run cache: a later run re-acquires a fresh resource. *)
  expect_run "fixture next run" ~config [ Test_tree.test "third" use ]
  @@ fun _ ->
  check_int "a later run re-acquires" ~expected:2 ~actual:!acquisitions;
  check "the later run gets a fresh resource"
    (match !got with [ c; b; _ ] -> not (c == b) | _ -> false)

let () =
  (* Failed acquisition: the exception fails the acquiring test and is
     cached, later uses re-raise it without re-acquiring, and the release
     skips the fixture. *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let attempts = ref 0 in
  let broken =
    Run.fixture ~teardown:ignore (fun () ->
        incr attempts;
        raise No_db)
  in
  let announced = ref false in
  let on_event = function
    | Run.Fixture_release _ -> announced := true
    | Run.Run_started _ | Run.Test_started _ | Run.Test_finished _
    | Run.Interrupted _ ->
        ()
  in
  let raised outcome path =
    match failure_list (outcome_of outcome path) with
    | [
     {
       Failure.kind =
         Failure.Raise { actual = Some { Failure.kept = actual; _ }; _ };
       _;
     };
    ] ->
        contains "No_db" actual
    | _ -> false
  in
  expect_run "broken-fixture suite runs" ~on_event ~config
    [ Test_tree.test "first" broken; Test_tree.test "later" broken ]
  @@ fun outcome ->
  check "a failed acquisition fails the acquiring test"
    (raised outcome [ "first" ]);
  check "later uses re-raise the cached error" (raised outcome [ "later" ]);
  check_int "the cached error prevents re-acquisition" ~expected:1
    ~actual:!attempts;
  check "a failed fixture is never announced or released" (not !announced);
  expect_run "broken-fixture next run" ~config [ Test_tree.test "again" broken ]
  @@ fun _ ->
  check_int "a later run retries a failed acquisition" ~expected:2
    ~actual:!attempts

let () =
  (* A skipping acquisition skips the acquiring test and is cached: later
     uses skip with its reason, and a later run re-attempts it. *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let attempts = ref 0 in
  let unavailable =
    Run.fixture ~teardown:ignore (fun () ->
        incr attempts;
        Check.skip ~reason:"no gpu" ())
  in
  expect_run "skipping-fixture suite runs" ~config
    [ Test_tree.test "first" unavailable; Test_tree.test "later" unavailable ]
  @@ fun outcome ->
  let skipped path =
    outcome_of outcome path = Some (Failure.Skip (Some "no gpu"))
  in
  check "a skipping acquisition skips the acquiring test" (skipped [ "first" ]);
  check "later uses skip with the cached reason" (skipped [ "later" ]);
  check_int "the cached skip prevents re-acquisition" ~expected:1
    ~actual:!attempts;
  expect_run "skipping-fixture next run" ~config
    [ Test_tree.test "again" unavailable ]
  @@ fun _ ->
  check_int "a later run re-attempts a skipped acquisition" ~expected:2
    ~actual:!attempts

let () =
  (* A raising teardown becomes a Release-phase failure and does not stop
     the remaining releases; several are all returned, in release order. *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let released = ref [] in
  let fx_ok =
    Run.fixture ~teardown:(fun v -> released := v :: !released) (fun () -> "ok")
  in
  let fx_plain = Run.fixture (fun () -> "plain") in
  let fx_first =
    Run.fixture ~teardown:(fun _ -> failwith "first") (fun () -> "first")
  in
  let fx_second =
    Run.fixture ~teardown:(fun _ -> failwith "second") (fun () -> "second")
  in
  let announced = ref [] in
  let on_event = function
    | Run.Fixture_release { name } -> announced := name :: !announced
    | Run.Run_started _ | Run.Test_started _ | Run.Test_finished _
    | Run.Interrupted _ ->
        ()
  in
  let tests =
    [
      Test_tree.test "acquires" (fun () ->
          List.iter
            (fun fx -> ignore (fx ()))
            [ fx_ok; fx_plain; fx_first; fx_second ]);
    ]
  in
  expect_run "raising-release suite runs" ~on_event ~config tests
  @@ fun outcome ->
  check_int "only fixtures with a teardown are announced" ~expected:3
    ~actual:(List.length !announced);
  check "releases continue past a raising teardown" (!released = [ "ok" ]);
  match outcome.Run.release_failures with
  | [ second; first ] ->
      check "every raising teardown is reported, in release order"
        (contains "second" (message_of second)
        && contains "first" (message_of first));
      check_string "the failure names the fixture and the exception"
        ~expected:
          ((match List.rev !announced with
             | name :: _ -> name
             | [] -> "<nothing announced>")
          ^ ": release raised "
          ^ Printexc.to_string (Failure "second"))
        ~actual:(message_of second);
      check "the release failure carries the declaration site"
        (match second.Failure.loc with
        | Some loc -> Filename.basename loc.Loc.file = "test_run.ml"
        | None -> false)
  | _ -> check "two release failures" false

let () =
  (* A fatal exception in a teardown leaves [execute] at once: the
     remaining releases are abandoned. *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let released = ref [] in
  let fx_ok =
    Run.fixture ~teardown:(fun v -> released := v :: !released) (fun () -> "ok")
  in
  let fx_fatal =
    Run.fixture ~teardown:(fun _ -> raise Out_of_memory) (fun () -> "fatal")
  in
  let tests =
    [
      Test_tree.test "acquires" (fun () ->
          ignore (fx_ok ());
          ignore (fx_fatal ()));
    ]
  in
  (match Run.execute config ~suite:"suite" tests with
  | _ -> check "a fatal teardown leaves execute" false
  | exception Out_of_memory -> check "a fatal teardown leaves execute" true);
  check "the fatal abandons the remaining releases" (!released = []);
  check "the slot is emptied on the fatal path" (not (Run.active ()))

(* subtest *)

let () =
  expect_invalid_arg "subtest outside a run raises" (fun () ->
      Run.subtest "s" (fun () -> ()))

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let pos = __POS__ in
  let sibling_ran = ref false in
  let control = ref [] in
  let tests =
    [
      Test_tree.test ~__POS__:pos "throws" (fun () ->
          Run.subtest "throws" (fun () -> raise Boom);
          sibling_ran := true);
      Test_tree.test "test" (fun () ->
          Run.subtest "outer" (fun () ->
              Run.subtest "inner" (fun () -> Check.is_true ~msg:"ctx" false));
          Run.subtest "outer" (fun () -> Check.fail "at-outer-level"));
      (* Skip and fatal exceptions abort the whole test: they propagate out
         of the subtest with its label popped. *)
      Test_tree.test "control" (fun () ->
          (match
             Run.subtest "skips" (fun () -> Check.skip ~reason:"later" ())
           with
          | () -> ()
          | exception Failure.Control (`Skip (Some "later")) ->
              control := "skip" :: !control);
          (match Run.subtest "fatal" (fun () -> raise Out_of_memory) with
          | () -> ()
          | exception Out_of_memory -> control := "fatal" :: !control);
          Run.subtest "clean" (fun () -> Check.fail "x"));
    ]
  in
  expect_run "subtest record suite runs" ~config tests @@ fun outcome ->
  (* A non-fatal exception is the sub-case's failure: a labeled Raise
     entry, and the siblings continue. *)
  (match failure_list (outcome_of outcome [ "throws" ]) with
  | [ f ] ->
      check "an exception in a subtest is a labeled Raise failure"
        (msg_of f = "throws › throws"
        &&
        match f.Failure.kind with
        | Failure.Raise { actual = Some { Failure.kept = actual; _ }; _ } ->
            contains "Boom" actual
        | _ -> false);
      (* No verb raised it: the declaration is its site. *)
      check "the subtest exception names the declaration as its own site"
        (at_pos pos f.Failure.loc)
  | _ -> check "one subtest exception recorded" false);
  check "siblings continue after a throwing subtest" !sibling_ran;
  (match failure_list (outcome_of outcome [ "test" ]) with
  | [ nested; outer ] ->
      check "the label is data: the test, then the open subtests"
        (nested.Failure.subtest = [ "test"; "outer"; "inner" ]);
      check "and never in msg"
        (Option.map (fun (m : Failure.text) -> m.kept) nested.Failure.msg
        = Some "ctx");
      check "after the nested subtest returns, its label is popped"
        (msg_of outer = "test › outer")
  | _ -> check "two nested subtest failures" false);
  check "skip and fatal propagate out of a subtest"
    (!control = [ "fatal"; "skip" ]);
  match failure_list (outcome_of outcome [ "control" ]) with
  | [ f ] ->
      check "control exceptions record nothing, and the stack is restored"
        (msg_of f = "control › clean")
  | _ -> check "one failure after the control exceptions" false

(* temp_dir / temp_file *)

let () =
  expect_invalid_arg "temp_dir outside a run raises" (fun () -> Run.temp_dir ());
  expect_invalid_arg "temp_file outside a run raises" (fun () ->
      Run.temp_file ())

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let private_to_owner path = (Unix.stat path).Unix.st_perm land 0o077 = 0 in
  let tests =
    [
      Test_tree.test "creates" (fun () ->
          let d1 = Run.temp_dir () in
          let d2 = Run.temp_dir ~prefix:"repo" () in
          let f1 = Run.temp_file () in
          let f2 = Run.temp_file ~suffix:".json" () in
          check "temp_dir creates a fresh empty directory"
            (Sys.is_directory d1 && Sys.readdir d1 = [||]);
          check "each temp_dir call is a new directory"
            (d1 <> d2 && Sys.is_directory d2);
          check "the prefix names the directory basename"
            (String.starts_with ~prefix:"repo" (Filename.basename d2));
          check "temp_file creates an empty file"
            (Sys.file_exists f1 && (not (Sys.is_directory f1)) && f1 <> f2);
          check "the suffix is appended to the file name"
            (Filename.check_suffix f2 ".json");
          check "the file basename is unchanged"
            (String.starts_with ~prefix:"file" (Filename.basename f1));
          check "paths share one per-attempt scratch directory"
            (Filename.dirname d1 = Filename.dirname f1
            && Filename.dirname d1 = Filename.dirname d2));
      Test_tree.test "hostile" (fun () ->
          (* A hostile prefix or suffix cannot escape the scratch
             directory. *)
          let d = Run.temp_dir ~prefix:"../evil" () in
          let f = Run.temp_file ~suffix:"/evil" () in
          let plain = Run.temp_dir () in
          check "a hostile prefix is sanitized into the scratch directory"
            (Filename.dirname d = Filename.dirname plain);
          check "a hostile suffix is sanitized into the scratch directory"
            (Filename.dirname f = Filename.dirname plain));
      Test_tree.test "private" (fun () ->
          (* The permission contract is a security property: no group or
             other access anywhere in the scratch tree (0o700 directories,
             0o600 files, modulo a umask that can only tighten them). *)
          let d = Run.temp_dir () in
          let f = Run.temp_file () in
          check "temp_dir grants no group/other access" (private_to_owner d);
          check "temp_file grants no group/other access" (private_to_owner f);
          check "the scratch root grants no group/other access"
            (private_to_owner (Filename.dirname d)));
    ]
  in
  expect_run "scratch suite runs" ~config tests @@ fun outcome ->
  check "the scratch tests pass" (outcome.Run.exit_code = 0)

(* Property context *)

let () =
  (* collect, classify and cover read the context of the running frame, so
     they work from the body of a stateful command, which the property's
     law runs. *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let labelled ~unreachable =
    [
      Stateful.call "tick"
        ~next:(fun model -> model + 1)
        (fun model () ->
          Windtrap.collect "ticked";
          Windtrap.classify "past three" (model > 3);
          Windtrap.cover "reached five"
            (if unreachable then model >= 500 else model >= 5));
    ]
  in
  let stateful ~unreachable name =
    Stateful.stateful ~count:10 ~steps:12 name ~model:0
      ~scope:(fun run -> run ())
      (labelled ~unreachable)
  in
  let tests =
    [
      stateful ~unreachable:false "labels";
      stateful ~unreachable:true "labels-unreachable";
    ]
  in
  expect_run "stateful labels suite runs" ~config tests @@ fun outcome ->
  (match result_of outcome [ "labels" ] with
  | Some { Run.outcome = Failure.Pass; prop_stats = Some stats; _ } ->
      check "collect from a command body marks every case"
        (List.assoc_opt "ticked" stats.Property.collected = Some 10);
      check "classify from a command body marks every case"
        (List.assoc_opt "past three" stats.Property.collected = Some 10);
      check "cover from a command body is satisfied"
        (match stats.Property.coverage with
        | [ status ] ->
            status.Property.label = "reached five" && status.Property.satisfied
        | _ -> false)
  | _ -> check "labels passed with statistics" false);
  (* And a requirement registered from a command body can fail the run. *)
  match result_of outcome [ "labels-unreachable" ] with
  | Some { Run.outcome = Failure.Fail _; prop_stats = Some stats; _ } ->
      check "an unreachable requirement is reported unsatisfied"
        (match stats.Property.coverage with
        | [ status ] -> not status.Property.satisfied
        | _ -> false)
  | _ -> check "labels-unreachable failed with statistics" false

(* ------------------------------------------------------------------ *)
(* The executor *)
(* ------------------------------------------------------------------ *)

let busy_forever () =
  let n = ref 1 in
  while !n > 0 do
    incr n;
    if !n > 1_000_000 then n := 1
  done

(* Boundary matrix: body x teardown *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let td_runs = ref [] in
  let bracket ~name ~body ~teardown_fails =
    Test_tree.bracket
      ~setup:(fun () -> name)
      ~teardown:(fun tag ->
        td_runs := tag :: !td_runs;
        if teardown_fails then Check.fail (tag ^ "-td-boom"))
      name body
  in
  let tests =
    [
      bracket ~name:"ok-ok" ~body:(fun _ -> ()) ~teardown_fails:false;
      bracket ~name:"fail-ok"
        ~body:(fun _ -> Check.fail "body-boom")
        ~teardown_fails:false;
      bracket ~name:"ok-fail" ~body:(fun _ -> ()) ~teardown_fails:true;
      bracket ~name:"fail-fail"
        ~body:(fun _ -> Check.fail "body-boom")
        ~teardown_fails:true;
      bracket ~name:"skip-ok"
        ~body:(fun _ -> Check.skip ~reason:"later" ())
        ~teardown_fails:false;
      bracket ~name:"skip-fail"
        ~body:(fun _ -> Check.skip ())
        ~teardown_fails:true;
      Test_tree.bracket
        ~setup:(fun () -> raise Boom)
        ~teardown:(fun () -> td_runs := "setup-fail" :: !td_runs)
        "setup-fail"
        (fun () -> td_runs := "setup-fail-body" :: !td_runs);
      Test_tree.bracket
        ~setup:(fun () -> Check.skip ~reason:"no env" ())
        ~teardown:(fun () -> td_runs := "setup-skip" :: !td_runs)
        "setup-skip"
        (fun () -> ());
      Test_tree.test "uncaught" (fun () -> raise Boom);
    ]
  in
  expect_run "boundary matrix runs" ~config tests @@ fun outcome ->
  check "ok-ok passes" (outcome_of outcome [ "ok-ok" ] = Some Failure.Pass);
  let fs = failure_list (outcome_of outcome [ "fail-ok" ]) in
  check "fail-ok: one Body failure" (phases_of fs = [ Failure.Body ]);
  let fs = failure_list (outcome_of outcome [ "ok-fail" ]) in
  check "ok-fail: one Teardown failure" (phases_of fs = [ Failure.Teardown ]);
  let fs = failure_list (outcome_of outcome [ "fail-fail" ]) in
  check "fail-fail: body and teardown entries, in order"
    (phases_of fs = [ Failure.Body; Failure.Teardown ]);
  check "skip-ok skips with its reason"
    (outcome_of outcome [ "skip-ok" ] = Some (Failure.Skip (Some "later")));
  let fs = failure_list (outcome_of outcome [ "skip-fail" ]) in
  check "skip-fail: the teardown failure wins over the skip"
    (phases_of fs = [ Failure.Teardown ]);
  let fs = failure_list (outcome_of outcome [ "setup-fail" ]) in
  check "setup-fail: one Setup failure" (phases_of fs = [ Failure.Setup ]);
  check "setup-fail: neither body nor teardown ran"
    ((not (List.mem "setup-fail" !td_runs))
    && not (List.mem "setup-fail-body" !td_runs));
  check "setup-skip: skips and the teardown did not run"
    (outcome_of outcome [ "setup-skip" ] = Some (Failure.Skip (Some "no env"))
    && not (List.mem "setup-skip" !td_runs));
  check "teardown ran for every completed setup, skip and failure included"
    (List.for_all
       (fun tag -> List.mem tag !td_runs)
       [ "ok-ok"; "fail-ok"; "ok-fail"; "fail-fail"; "skip-ok"; "skip-fail" ]);
  (let fs = failure_list (outcome_of outcome [ "uncaught" ]) in
   match fs with
   | [
    {
      Failure.kind =
        Failure.Raise { actual = Some { Failure.kept = actual; _ }; _ };
      _;
    };
   ] ->
       check "uncaught exception is a Raise failure naming it"
         (contains "Boom" actual)
   | _ -> check "uncaught exception is a Raise failure" false);
  check "a failing suite exits 1" (outcome.Run.exit_code = 1)

(* Scoped boundary: the scope owns cleanup, the runner owns attribution *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let log = ref [] in
  let mark step = log := step :: !log in
  let ran step = List.mem step !log in
  (* The canonical scoper: acquire, call back, reclaim on both paths. The
     resource it supplies is the test's own name. *)
  let protecting name fn =
    mark (name ^ ":acquire");
    Fun.protect
      ~finally:(fun () -> mark (name ^ ":release"))
      (fun () -> fn name)
  in
  let scoped_test name scope body = Test_tree.scoped scope name body in
  let tests =
    [
      scoped_test "pass" (protecting "pass") (fun r ->
          mark "pass:body";
          equal string "pass" r);
      scoped_test "body fails" (protecting "body fails") (fun _ ->
          Check.fail "body-boom");
      scoped_test "body skips" (protecting "body skips") (fun _ ->
          Check.skip ~reason:"later" ());
      (* A scope that never calls back: the body did not run, so the test
         cannot be green. *)
      scoped_test "never calls back"
        (fun _ -> mark "never calls back:acquire")
        (fun () -> mark "never calls back:body");
      scoped_test "acquire fails"
        (fun _ -> raise Boom)
        (fun () -> mark "acquire fails:body");
      scoped_test "release fails"
        (fun fn ->
          fn ();
          Check.fail "release-boom")
        (fun () -> mark "release fails:body");
      (* A release failure that replaces the body's exception: two entries,
         as under bracket. *)
      scoped_test "both fail"
        (fun fn -> try fn () with _ -> Check.fail "release-boom")
        (fun () -> Check.fail "body-boom");
      (* A scope that swallows the body's failure must not make it green. *)
      scoped_test "swallowed"
        (fun fn -> try fn () with _ -> ())
        (fun () -> Check.fail "body-boom");
      (* Skipping instead of calling back is a skip, not a missing body. *)
      scoped_test "scope skips"
        (fun _ -> Check.skip ~reason:"no device" ())
        (fun () -> mark "scope skips:body");
      scoped_test "calls back twice"
        (fun fn ->
          fn ();
          fn ())
        (fun () -> mark "calls back twice:body");
    ]
  in
  expect_run "scoped boundary matrix runs" ~config tests @@ fun outcome ->
  check "pass: the test passed"
    (outcome_of outcome [ "pass" ] = Some Failure.Pass);
  check "pass: acquire, body and release ran in that order"
    (List.filter (fun s -> String.starts_with ~prefix:"pass:" s) (List.rev !log)
    = [ "pass:acquire"; "pass:body"; "pass:release" ]);
  (let fs = failure_list (outcome_of outcome [ "body fails" ]) in
   check "body fails: one Body failure" (phases_of fs = [ Failure.Body ]);
   check "body fails: the exception reached the scope, which released"
     (ran "body fails:release"));
  check "body skips: the skip reaches the outcome"
    (outcome_of outcome [ "body skips" ] = Some (Failure.Skip (Some "later")));
  check "body skips: the scope released on the skip path"
    (ran "body skips:release");
  (let fs = failure_list (outcome_of outcome [ "never calls back" ]) in
   match fs with
   | [ f ] ->
       check "never calls back: a Setup-phase failure"
         (f.Failure.phase = Failure.Setup);
       check "never calls back: the message names what went wrong"
         (contains
            "the scope returned without running the test body; a scope must \
             call its callback exactly once"
            (message_of f))
   | _ -> check "never calls back: exactly one failure" false);
  check "never calls back: the body did not run"
    (not (ran "never calls back:body"));
  (let fs = failure_list (outcome_of outcome [ "acquire fails" ]) in
   check "acquire fails: one Setup failure" (phases_of fs = [ Failure.Setup ]));
  check "acquire fails: the body did not run" (not (ran "acquire fails:body"));
  (let fs = failure_list (outcome_of outcome [ "release fails" ]) in
   check "release fails: one Teardown failure"
     (phases_of fs = [ Failure.Teardown ]));
  check "release fails: the body did run" (ran "release fails:body");
  (let fs = failure_list (outcome_of outcome [ "both fail" ]) in
   check "both fail: body and release entries, in order"
     (phases_of fs = [ Failure.Body; Failure.Teardown ]));
  (let fs = failure_list (outcome_of outcome [ "swallowed" ]) in
   check "swallowed: a scope that eats the failure cannot make it green"
     (phases_of fs = [ Failure.Body ]));
  check "scope skips: the skip reaches the outcome"
    (outcome_of outcome [ "scope skips" ]
    = Some (Failure.Skip (Some "no device")));
  check "scope skips: no missing-body failure was invented"
    (not (ran "scope skips:body"));
  (let fs = failure_list (outcome_of outcome [ "calls back twice" ]) in
   match fs with
   | [ f ] ->
       check "calls back twice: the message states the contract"
         (contains
            "the scope called its callback 2 times and the test body ran on \
             the first call only; a scope must call it exactly once"
            (message_of f))
   | _ -> check "calls back twice: exactly one failure" false);
  check_int "calls back twice: the body ran once" ~expected:1
    ~actual:
      (List.length (List.filter (fun s -> s = "calls back twice:body") !log))

(* Backtrace recording *)

(* [@inline never] so the raise site stays a frame of its own: an inlined
   helper leaves no slot to name. *)
let[@inline never] raise_from_helper () = raise Boom

let () =
  (* [execute] turns backtrace recording on for the run. The runtime records
     nothing unless asked, and nothing tells a user to set OCAMLRUNPARAM=b,
     so without that call an uncaught exception's report is the constructor
     and the test's declaration line — never the raise site. Recording is
     switched OFF first: whatever left it on (the harness, a previous run)
     must not be what makes this pass. *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let restore = Printexc.backtrace_status () in
  Printexc.record_backtrace false;
  let tests =
    [ Test_tree.test "deep raise" (fun () -> raise_from_helper ()) ]
  in
  expect_run "backtrace suite runs" ~config tests (fun outcome ->
      check "the run turned recording on" (Printexc.backtrace_status ());
      match failure_list (outcome_of outcome [ "deep raise" ]) with
      | [ { Failure.kind = Failure.Raise { backtrace; _ }; _ } ] -> (
          match backtrace with
          | Some { Failure.kept = bt; _ } ->
              check "the Raise payload carries a non-empty backtrace"
                (String.trim bt <> "");
              check "the backtrace names the function that raised"
                (contains "raise_from_helper" bt)
          | None -> check "the Raise payload carries a backtrace" false)
      | _ -> check "deep raise: one Raise failure" false);
  Printexc.record_backtrace restore

(* A fatal exception skips the derived bracket's teardown and escapes the
   run, where any other body exception runs it and is recorded. *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let torn_down = ref false in
  let tests =
    [
      Test_tree.bracket
        ~setup:(fun () -> ())
        ~teardown:(fun () -> torn_down := true)
        "fatal"
        (fun () -> raise Sys.Break);
    ]
  in
  check "a fatal exception escapes the run"
    (match Run.execute config ~suite:"suite" tests with
    | exception Sys.Break -> true
    | Ok _ | Error _ -> false);
  check "the teardown did not run on the fatal path" (not !torn_down)

(* Timeouts (Unix only) *)

let () =
  if Sys.win32 then skip_scenario ~reason:"POSIX only" __POS__
  else
    with_temp_root @@ fun root ->
    let config = base_config ~log_dir:root () in
    let td_after_timeout = ref false in
    let body_times_out =
      Test_tree.test ~timeout:0.2 "body-times-out" busy_forever
    in
    let declared =
      match Test_tree.flatten [ body_times_out ] with
      | [ case ] -> case.Test_tree.loc
      | _ -> None
    in
    let tests =
      [
        body_times_out;
        Test_tree.bracket ~timeout:0.3
          ~setup:(fun () -> ())
          ~teardown:(fun () -> busy_forever ())
          "teardown-times-out"
          (fun () -> Check.fail "body-boom");
        Test_tree.bracket ~timeout:0.2
          ~setup:(fun () -> ())
          ~teardown:(fun () -> td_after_timeout := true)
          "teardown-after-body-timeout"
          (fun () -> busy_forever ());
      ]
    in
    expect_run "timeout suite runs" ~config tests @@ fun outcome ->
    (let fs = failure_list (outcome_of outcome [ "body-times-out" ]) in
     match fs with
     | [ f ] ->
         check "body timeout: one Body failure" (f.Failure.phase = Failure.Body);
         check "body timeout: message says timed out"
           (contains "timed out" (message_of f));
         (* The declaration is a timeout's natural site. *)
         check "body timeout: located at the declaration"
           (declared <> None && f.Failure.loc = declared)
     | _ -> check "body timeout: one failure" false);
    (let fs = failure_list (outcome_of outcome [ "teardown-times-out" ]) in
     match fs with
     | [ body_f; td_f ] ->
         check "teardown timeout: body failure kept alongside"
           (body_f.Failure.phase = Failure.Body);
         check "teardown timeout: Teardown-phase failure"
           (td_f.Failure.phase = Failure.Teardown);
         check "teardown timeout: message says timed out"
           (contains "timed out" (message_of td_f))
     | _ -> check "teardown timeout: two failures" false);
    let fs =
      failure_list (outcome_of outcome [ "teardown-after-body-timeout" ])
    in
    check "body timeout: teardown still ran" !td_after_timeout;
    check "body timeout: only the body entry" (phases_of fs = [ Failure.Body ])

let () =
  if Sys.win32 then skip_scenario ~reason:"POSIX only" __POS__
  else
    with_temp_root @@ fun root ->
    let config =
      { (base_config ~log_dir:root ()) with Run.timeout = Some 0.2 }
    in
    let tests = [ Test_tree.test "slowpoke" busy_forever ] in
    expect_run "config default timeout applies" ~config tests @@ fun outcome ->
    let fs = failure_list (outcome_of outcome [ "slowpoke" ]) in
    check "default timeout fails the test"
      (match fs with
      | [ f ] -> contains "timed out" (message_of f)
      | _ -> false)

let () =
  if Sys.win32 then skip_scenario ~reason:"POSIX only" __POS__
  else
    with_temp_root @@ fun root ->
    let config = base_config ~log_dir:root () in
    let ran = ref [] in
    let tests =
      [
        Test_tree.bracket ~timeout:0.2
          ~setup:(fun () -> busy_forever ())
          ~teardown:(fun () -> ran := "setup-times-out-td" :: !ran)
          "setup-times-out"
          (fun () -> ran := "setup-times-out-body" :: !ran);
      ]
    in
    expect_run "setup-timeout suite runs" ~config tests @@ fun outcome ->
    (match failure_list (outcome_of outcome [ "setup-times-out" ]) with
    | [ f ] ->
        check "setup timeout is a Setup-phase failure"
          (f.Failure.phase = Failure.Setup);
        check "setup timeout message says timed out"
          (contains "timed out" (message_of f))
    | _ -> check "setup timeout: exactly one failure" false);
    check "setup timeout: neither body nor teardown ran" (!ran = [])

let () =
  (* Fixture releases run outside per-test timeouts: a release slower than
     the tightest test timeout must complete untimed. *)
  if Sys.win32 then skip_scenario ~reason:"POSIX only" __POS__
  else
    with_temp_root @@ fun root ->
    let config = base_config ~log_dir:root () in
    let release_done = ref false in
    let fx =
      Run.fixture
        ~teardown:(fun _ ->
          Unix.sleepf 0.25;
          release_done := true)
        (fun () -> ())
    in
    let tests =
      [ Test_tree.test ~timeout:0.1 "tight" (fun () -> ignore (fx ())) ]
    in
    expect_run "slow-release suite runs" ~config tests @@ fun outcome ->
    check "the test passed within its window"
      (outcome_of outcome [ "tight" ] = Some Failure.Pass);
    check "the slow release completed, untimed and unfailed"
      (!release_done && outcome.Run.release_failures = []);
    check "the run stayed green" (outcome.Run.exit_code = 0)

let () =
  (* The exit guard belongs to the process that armed it. A forked child
     inherits Run.active and the at_exit registration, so without the pid
     check the child's [exit] was intercepted: it returned into the runner,
     ran every remaining test, printed a second report, and exited with the
     run's code instead of its own. *)
  if Sys.win32 then skip_scenario ~reason:"POSIX only" __POS__
  else
    with_temp_root @@ fun root ->
    let config = base_config ~log_dir:root () in
    let child_status = ref (-1) in
    let tests =
      [
        Test_tree.test "forks a child that exits 3" (fun () ->
            match Unix.fork () with
            | 0 -> exit 3
            | pid -> (
                let _, status = Unix.waitpid [] pid in
                child_status :=
                  match status with Unix.WEXITED c -> c | _ -> -1));
      ]
    in
    expect_run "a forked child exits on its own terms" ~config tests
    @@ fun outcome ->
    check "the child's exit code reached the parent" (!child_status = 3);
    check "the test passed" (outcome.Run.exit_code = 0)

let () =
  (* The timer is one-shot and [Failure.Timeout] is not fatal, so the body's
     phase guard absorbs the alarm and [phases] goes on to teardown. Before
     the window was re-armed there, a teardown that blocked after a body
     timeout ran unbounded: the whole run hung with no output. Both phases
     must time out, and both must be reported. *)
  if Sys.win32 then skip_scenario ~reason:"POSIX only" __POS__
  else
    with_temp_root @@ fun root ->
    let config = base_config ~log_dir:root () in
    let spin s =
      let t0 = Unix.gettimeofday () in
      while Unix.gettimeofday () -. t0 < s do
        ignore (Sys.opaque_identity 1)
      done
    in
    let tests =
      [
        Test_tree.bracket ~timeout:0.2 "body then teardown"
          ~setup:(fun () -> ())
          ~teardown:(fun () -> spin 30.)
          (fun () -> spin 30.);
      ]
    in
    let started = Unix.gettimeofday () in
    expect_run "a body timeout still bounds teardown" ~config tests
    @@ fun outcome ->
    let elapsed = Unix.gettimeofday () -. started in
    (* Two windows of 0.2s (0.4s measured), not a minute of spinning; the
       bound leaves ten times the measure to a loaded host. *)
    check "the run finished promptly" (elapsed < 5.0);
    match outcome_of outcome [ "body then teardown" ] with
    | Some (Failure.Fail failures) ->
        let phases =
          List.map (fun (f : Failure.t) -> f.Failure.phase) failures
        in
        check "the body timed out" (List.mem Failure.Body phases);
        check "the teardown timed out too, rather than running unbounded"
          (List.mem Failure.Teardown phases)
    | _ -> check "the test failed" false

let () =
  (* A scope is one call, so the limit covers acquire, body and release
     together. The runner re-arms the window as the body leaves the
     callback: without that, a scope that blocks while reclaiming after a
     body timeout would run unbounded, exactly as a bracket teardown
     would. *)
  if Sys.win32 then skip_scenario ~reason:"POSIX only" __POS__
  else
    with_temp_root @@ fun root ->
    let config = base_config ~log_dir:root () in
    let spin s =
      let t0 = Unix.gettimeofday () in
      while Unix.gettimeofday () -. t0 < s do
        ignore (Sys.opaque_identity 1)
      done
    in
    let released = ref false in
    let tests =
      [
        Test_tree.scoped
          (fun fn ->
            Fun.protect ~finally:(fun () -> released := true) (fun () -> fn ()))
          ~timeout:0.2 "body times out"
          (fun () -> spin 30.);
        Test_tree.scoped
          (fun fn ->
            fn ();
            spin 30.)
          ~timeout:0.2 "release times out"
          (fun () -> ());
      ]
    in
    let started = Unix.gettimeofday () in
    expect_run "scoped timeout suite runs" ~config tests @@ fun outcome ->
    let elapsed = Unix.gettimeofday () -. started in
    (* Two windows of 0.2s (0.4s measured), not a minute of spinning; the
       bound leaves ten times the measure to a loaded host. *)
    check "the run finished promptly" (elapsed < 5.0);
    (match failure_list (outcome_of outcome [ "body times out" ]) with
    | [ f ] ->
        check "body times out: one Body timeout"
          (f.Failure.phase = Failure.Body && contains "timed out" (message_of f))
    | _ -> check "body times out: exactly one failure" false);
    check "body times out: the scope still got to reclaim" !released;
    match failure_list (outcome_of outcome [ "release times out" ]) with
    | [ f ] ->
        check "release times out: one Teardown timeout"
          (f.Failure.phase = Failure.Teardown
          && contains "timed out" (message_of f))
    | _ -> check "release times out: exactly one failure" false

let () =
  (* D2: a timeout expiring during the shrink search ends the search at the
     last accepted node and reports the counterexample in hand, marked —
     the budget bounds wall time without erasing what the engine found. *)
  if Sys.win32 then skip_scenario ~reason:"POSIX only" __POS__
  else
    with_temp_root @@ fun root ->
    let config =
      { (base_config ~log_dir:root ()) with Run.timeout = Some 0.2 }
    in
    let tests =
      [
        (* Failing above a threshold keeps the descent walking the halving
           chain (the dest-first candidate passes and is rejected), so the
           search is still alive when the alarm fires. The first case fails
           at once and every candidate takes half a second: the whole
           descent, some twenty candidates, would take ten. *)
        (let calls = ref 0 in
         Run.prop "slow-shrink" (Gen.int_range 0 1000) (fun n ->
             incr calls;
             if !calls > 1 then Unix.sleepf 0.5;
             Check.is_true (n < 1)));
      ]
    in
    let started = Unix.gettimeofday () in
    expect_run "mid-shrink timeout suite runs" ~config tests @@ fun outcome ->
    let wall = Unix.gettimeofday () -. started in
    (match failure_list (outcome_of outcome [ "slow-shrink" ]) with
    | [ { Failure.kind = Failure.Property { shrink_end; _ }; _ } ] ->
        check "prop timeout mid-shrink reports the marked counterexample"
          (shrink_end = Failure.Timed_out 0.2)
    | _ -> check "mid-shrink timeout: one Property failure" false);
    (* 0.2s measured; the bound leaves ten times that to a loaded host. *)
    check "the whole-test budget bounds the wall time" (wall < 2.0);
    check "a timed-out-mid-shrink prop is an ordinary failed test"
      (outcome.Run.exit_code = 1)

let () =
  if Sys.win32 then skip_scenario ~reason:"POSIX only" __POS__
  else
    with_temp_root @@ fun root ->
    let config =
      { (base_config ~log_dir:root ()) with Run.timeout = Some 0.2 }
    in
    let tests = [ Run.prop "slow-pass" Gen.int (fun _ -> Unix.sleepf 0.05) ] in
    expect_run "pre-failure timeout suite runs" ~config tests @@ fun outcome ->
    (* The engine alone knows which case the limit cut, and how many passed
       before it: the timeout carries both, and the seed a replay needs. *)
    match failure_list (outcome_of outcome [ "slow-pass" ]) with
    | [
     {
       Failure.kind =
         Failure.Timeout
           {
             limit;
             case = Some { case_index; examples = false; passed; root; count };
           };
       _;
     };
    ] ->
        check "prop timeout before any failure names the limit" (limit = 0.2);
        check "and the case it cut, after the cases that passed"
          (case_index > 0 && passed = case_index);
        check "with the run's seed and no configured count"
          (root = config.Run.seed && count = None)
    | _ -> check "pre-failure timeout: one timeout in a case" false

(* The per-test budget is declarable at the site — [prop ~timeout] and
   [cases ~timeout]/[~retries] — not only through the global [--timeout]:
   both routes converge on the same Test_tree fields, so the memo semantics
   pinned above (pre-failure timeout, mid-shrink marking) hold unchanged. *)
let () =
  if Sys.win32 then skip_scenario ~reason:"POSIX only" __POS__
  else
    with_temp_root @@ fun root ->
    let config = base_config ~log_dir:root () in
    let attempts = ref 0 in
    let tests =
      [
        Run.prop ~timeout:0.2 "budgeted-prop" Gen.int (fun _ ->
            Unix.sleepf 0.05);
        Test_tree.cases ~timeout:0.2 "table"
          ~name:(function `Slow -> "slow" | `Fast -> "fast")
          [ `Slow; `Fast ]
          (fun input -> if input = `Slow then busy_forever ());
        Test_tree.cases ~retries:2
          ~name:(fun () -> "row")
          "flaky-table" [ () ]
          (fun () ->
            incr attempts;
            if !attempts < 3 then Check.fail "not yet");
      ]
    in
    expect_run "declared-budget suite runs" ~config tests @@ fun outcome ->
    (match failure_list (outcome_of outcome [ "budgeted-prop" ]) with
    | [ f ] ->
        check "prop ~timeout bounds the whole property"
          (contains "timed out" (message_of f))
    | _ -> check "budgeted-prop: one failure" false);
    (match failure_list (outcome_of outcome [ "table"; "slow" ]) with
    | [ f ] ->
        check "a cases child overrunning its ~timeout times out"
          (contains "timed out" (message_of f))
    | _ -> check "table › slow: one failure" false);
    check "the sibling gets its own full budget"
      (outcome_of outcome [ "table"; "fast" ] = Some Failure.Pass);
    match result_of outcome [ "flaky-table"; "row" ] with
    | Some r ->
        check "cases ~retries gives each child the extra attempts"
          (r.Run.outcome = Failure.Pass && r.Run.attempts = 3)
    | None -> check "flaky-table row recorded" false

(* Retries *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let flaky_calls = ref 0 and hopeless_calls = ref 0 and skip_calls = ref 0 in
  let tests =
    [
      Test_tree.test ~retries:2 "flaky" (fun () ->
          incr flaky_calls;
          if !flaky_calls < 3 then Check.fail "not yet");
      Test_tree.test ~retries:1 "hopeless" (fun () ->
          incr hopeless_calls;
          Check.fail (Printf.sprintf "attempt-%d" !hopeless_calls));
      Test_tree.test ~retries:2 "skips" (fun () ->
          incr skip_calls;
          Check.skip ());
    ]
  in
  expect_run "retries suite runs" ~config tests @@ fun outcome ->
  (match result_of outcome [ "flaky" ] with
  | Some r ->
      check "flaky passes after retries" (r.Run.outcome = Failure.Pass);
      check_int "flaky used three attempts" ~expected:3 ~actual:r.Run.attempts
  | None -> check "flaky recorded" false);
  (match result_of outcome [ "hopeless" ] with
  | Some r ->
      check_int "hopeless used both attempts" ~expected:2 ~actual:r.Run.attempts;
      let fs = failure_list (Some r.Run.outcome) in
      check "hopeless reports the final attempt's failure"
        (match fs with [ f ] -> message_of f = "attempt-2" | _ -> false)
  | None -> check "hopeless recorded" false);
  match result_of outcome [ "skips" ] with
  | Some r ->
      check "skip is not retried"
        (r.Run.outcome = Failure.Skip None && !skip_calls = 1);
      check_int "skip used one attempt" ~expected:1 ~actual:r.Run.attempts
  | None -> check "skips recorded" false

(* Capture wiring: tails, per-attempt truncation, stream *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let attempt = ref 0 in
  let tests =
    [
      Test_tree.bracket
        ~setup:(fun () -> ())
        ~teardown:(fun () -> Check.fail "td-boom")
        "tailed"
        (fun () ->
          print_string "tail-marker\n";
          Check.fail "body-boom");
      Test_tree.test ~retries:1 "retried-output" (fun () ->
          incr attempt;
          Printf.printf "attempt-%d\n" !attempt;
          Check.fail "always");
    ]
  in
  expect_run "capture suite runs" ~config tests @@ fun outcome ->
  (match failure_list (outcome_of outcome [ "tailed" ]) with
  | [ body_f; td_f ] ->
      (match body_f.Failure.output_tail with
      | Some tail ->
          check "the tail carries the captured output"
            (contains "tail-marker" tail.Failure.text);
          check "the tail names the full log" (tail.Failure.log_path <> None)
      | None -> check "first failure carries the output tail" false);
      check "the tail is attached once, to the first failure"
        (td_f.Failure.output_tail = None)
  | _ -> check "tailed: two failures" false);
  match failure_list (outcome_of outcome [ "retried-output" ]) with
  | [ f ] -> (
      match f.Failure.output_tail with
      | Some tail ->
          check "the report shows the final attempt's output"
            (contains "attempt-2" tail.Failure.text
            && not (contains "attempt-1" tail.Failure.text))
      | None -> check "retried failure carries a tail" false)
  | _ -> check "retried-output: one failure" false

let () =
  with_temp_root @@ fun root ->
  let config = { (base_config ~log_dir:root ()) with Run.stream = true } in
  let tests = [ Test_tree.test "quiet-fail" (fun () -> Check.fail "boom") ] in
  expect_run "stream suite runs" ~config tests @@ fun outcome ->
  match failure_list (outcome_of outcome [ "quiet-fail" ]) with
  | [ f ] ->
      check "no output tail under --stream (capture disabled)"
        (f.Failure.output_tail = None)
  | _ -> check "stream: one failure" false

(* Global Random reseed *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let draw () = (Random.bits (), Random.bits (), Random.bits ()) in
  let first = ref (0, 0, 0) and second = ref (0, 0, 0) in
  let tests =
    [
      Test_tree.test "rand-a" (fun () -> first := draw ());
      Test_tree.test "rand-b" (fun () -> second := draw ());
    ]
  in
  expect_run "random suite runs (1st)" ~config tests @@ fun _ ->
  let first_run = (!first, !second) in
  expect_run "random suite runs (2nd)" ~config tests @@ fun _ ->
  check "a test's Random stream is a function of its path"
    (first_run = (!first, !second));
  check "different paths get different Random streams" (!first <> !second)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests = [ Test_tree.test "drains" (fun () -> ignore (Random.bits ())) ] in
  Random.init 999;
  let expected = Random.int 1_000_000 in
  Random.init 999;
  expect_run "random-restore suite runs" ~config tests @@ fun _ ->
  check "the ambient Random state is restored after the run"
    (Random.int 1_000_000 = expected)

(* Selection *)

(* The paths the exit code and the last-failed store react to: test rows
   the runner counted as failed, in execution order. Derived from the
   recorded rows rather than read off the outcome — no product consumer
   asks for the list, so the runner keeps it as a local. *)
let failed_paths outcome =
  List.filter_map
    (fun (r : Run.result) ->
      if r.Run.counted then Some (Test_tree.path_to_string r.Run.path) else None)
    (Run.results outcome.Run.run)

let ran_names outcome =
  List.map (fun r -> String.concat "/" r.Run.path) (Run.results outcome.Run.run)

let () =
  with_temp_root @@ fun root ->
  let named name = Test_tree.test name (fun () -> ()) in
  let suite =
    [
      Test_tree.group "math" [ named "add"; named "sub" ];
      Test_tree.group "text" [ named "trim" ];
    ]
  in
  let config filter exclude =
    { (base_config ~log_dir:root ()) with Run.filter; exclude }
  in
  expect_run "filter selects a subtree" ~config:(config [ "math" ] []) suite
  @@ fun outcome ->
  check "only matching tests ran"
    (ran_names outcome = [ "math/add"; "math/sub" ]);
  check_int "selected mirrors the run" ~expected:2
    ~actual:(List.length outcome.Run.selected);
  check_int "total counts the whole suite" ~expected:3 ~actual:outcome.Run.total;
  expect_run "exclude drops matches" ~config:(config [] [ "math" ]) suite
  @@ fun outcome ->
  check "only non-excluded tests ran" (ran_names outcome = [ "text/trim" ]);
  expect_run "filter and exclude compose"
    ~config:(config [ "math" ] [ "sub" ])
    suite
  @@ fun outcome ->
  check "compose" (ran_names outcome = [ "math/add" ]);
  expect_run "a test that contains either filter pattern runs"
    ~config:(config [ "sub"; "trim" ] [])
    suite
  @@ fun outcome ->
  check "either pattern keeps" (ran_names outcome = [ "math/sub"; "text/trim" ]);
  expect_run "a test that contains either exclusion pattern is dropped"
    ~config:(config [] [ "add"; "trim" ])
    suite
  @@ fun outcome ->
  check "either pattern drops" (ran_names outcome = [ "math/sub" ])

let () =
  with_temp_root @@ fun root ->
  let suite =
    [
      Test_tree.test "plain" (fun () -> ());
      Test_tree.test ~tags:[ "db" ] "tagged" (fun () -> ());
      Test_tree.slow "molasses" (fun () -> ());
    ]
  in
  let config ?(tags = []) ?(exclude_tags = []) () =
    { (base_config ~log_dir:root ()) with Run.tags; exclude_tags }
  in
  expect_run "default tag predicate" ~config:(config ()) suite @@ fun outcome ->
  check "no tag flag selects the whole suite"
    (ran_names outcome = [ "plain"; "tagged"; "molasses" ]);
  expect_run "--tag requires" ~config:(config ~tags:[ "db" ] ()) suite
  @@ fun outcome ->
  check "--tag" (ran_names outcome = [ "tagged" ]);
  expect_run "--exclude-tag drops"
    ~config:(config ~exclude_tags:[ "db" ] ())
    suite
  @@ fun outcome ->
  check "--exclude-tag" (ran_names outcome = [ "plain"; "molasses" ]);
  expect_run "--exclude-tag slow drops slow"
    ~config:(config ~exclude_tags:[ "slow" ] ())
    suite
  @@ fun outcome ->
  check "--exclude-tag slow" (ran_names outcome = [ "plain"; "tagged" ])

let () =
  clear_env ();
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite =
    [
      Test_tree.test "unfocused" (fun () -> ());
      Test_tree.focus (Test_tree.test "starred" (fun () -> ()));
    ]
  in
  expect_run "focus narrows outside CI" ~config suite @@ fun outcome ->
  check "only the focused test ran" (ran_names outcome = [ "starred" ]);
  check "focus_active is reported" outcome.Run.focus_active;
  check "a focused run of passing tests exits 0" (outcome.Run.exit_code = 0)

(* The CI focus guard *)

let () =
  Fun.protect ~finally:clear_env @@ fun () ->
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite = [ Test_tree.focus (Test_tree.test "starred" (fun () -> ())) ] in
  Unix.putenv "CI" "true";
  expect_startup_error "focused tests are refused under CI" ~config suite
    (function
    | Run.Focused_in_ci [ _ ] -> true
    | _ -> false);
  (match Run.execute config ~suite:"suite" suite with
  | Error error ->
      check "the focus refusal exits 1" (Run.startup_exit_code error = 1);
      check "the focus message names the remedy"
        (contains "remove focus" (Run.startup_message error))
  | Ok _ -> check "focus refusal expected" false);
  (* [allow_focus] has no flag and no mirror: a forked mutation child is
     its only setter, through [Run.for_subset]. *)
  let config = { config with Run.allow_focus = true } in
  expect_run "a subset run lifts the guard" ~config suite @@ fun outcome ->
  check "focused test ran" (ran_names outcome = [ "starred" ])

let () =
  Fun.protect ~finally:clear_env @@ fun () ->
  (* CI falsy spellings do not arm the guard (Env: set-and-not-falsy). *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite = [ Test_tree.focus (Test_tree.test "starred" (fun () -> ())) ] in
  List.iter
    (fun value ->
      Unix.putenv "CI" value;
      expect_run
        (Printf.sprintf "CI=%S does not trigger the focus guard" value)
        ~config suite
      @@ fun outcome ->
      check "focused test ran" (ran_names outcome = [ "starred" ]))
    [ ""; "false"; "0" ]

(* Exit codes *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  expect_run "all-pass exits 0" ~config [ Test_tree.test "ok" (fun () -> ()) ]
  @@ fun outcome ->
  check "exit 0" (outcome.Run.exit_code = 0);
  expect_run "any failure exits 1" ~config
    [
      Test_tree.test "ok" (fun () -> ());
      Test_tree.test "bad" (fun () -> Check.fail "boom");
    ]
  @@ fun outcome ->
  check "exit 1" (outcome.Run.exit_code = 1);
  expect_run "a filter matching nothing exits 2"
    ~config:{ config with Run.filter = [ "zzz-nothing" ] }
    [ Test_tree.test "ok" (fun () -> ()) ]
  @@ fun outcome ->
  check "exit 2, nothing recorded"
    (outcome.Run.exit_code = 2 && Run.results outcome.Run.run = []);
  expect_run "an empty suite exits 2" ~config [] @@ fun outcome ->
  check "empty suite" (outcome.Run.exit_code = 2);
  expect_run "a nonempty selection of skips exits 0" ~config
    [
      Test_tree.test "skip-a" (fun () -> Check.skip ());
      Test_tree.test "skip-b" (fun () -> Check.skip ~reason:"no net" ());
    ]
  @@ fun outcome -> check "all-skipped ratification" (outcome.Run.exit_code = 0)

(* Expected failures *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [
      Test_tree.xfail ~reason:"issue #42"
        (Test_tree.test "known-bug" (fun () -> Check.fail "still broken"));
      Test_tree.test "healthy" (fun () -> ());
    ]
  in
  expect_run "xfail suite runs" ~config tests @@ fun outcome ->
  (match failure_list (outcome_of outcome [ "known-bug" ]) with
  | [ f ] ->
      check "the expected failure keeps its real payload"
        (message_of f = "still broken")
  | _ -> check "expected failure recorded with its failures" false);
  check "an expected failure does not fail the run" (outcome.Run.exit_code = 0);
  check "an expected failure is not a failed path" (failed_paths outcome = []);
  check "the flattened case carries the annotation for renderers"
    (match
       List.find_opt
         (fun (c : Test_tree.case) -> c.Test_tree.path = [ "known-bug" ])
         outcome.Run.selected
     with
    | Some c -> c.Test_tree.xfail = Some { Test_tree.reason = Some "issue #42" }
    | None -> false)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let pos = ("test/fake_decl.ml", 12, 2, 30) in
  let tests =
    [
      Test_tree.xfail ~reason:"issue #42"
        (Test_tree.test ~__POS__:pos "fixed" (fun () -> ()));
    ]
  in
  expect_run "xpass suite runs" ~config tests @@ fun outcome ->
  (match failure_list (outcome_of outcome [ "fixed" ]) with
  | [ f ] ->
      check "an unexpected pass fails loudly, naming the reason"
        (contains "expected to fail" (message_of f)
        && contains "issue #42" (message_of f));
      check "an unexpected pass is located at the declaration"
        (f.Failure.loc = Some (Loc.of_pos pos))
  | _ -> check "unexpected pass records one message failure" false);
  check "an unexpected pass fails the run"
    (outcome.Run.exit_code = 1 && failed_paths outcome = [ "fixed" ])

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [
      Test_tree.xfail (Test_tree.test "undecided" (fun () -> Check.skip ()));
      Test_tree.xfail
        (Run.prop "known-bad-law" Gen.int (fun n -> Check.is_true (n = n + 1)));
    ]
  in
  expect_run "xfail-edge suite runs" ~config tests @@ fun outcome ->
  check "a skip is unaffected by xfail"
    (outcome_of outcome [ "undecided" ] = Some (Failure.Skip None));
  check "xfail composes with properties"
    (match failure_list (outcome_of outcome [ "known-bad-law" ]) with
    | [ { Failure.kind = Failure.Property _; _ } ] -> true
    | _ -> false);
  check "an expected property failure keeps the run green"
    (outcome.Run.exit_code = 0 && failed_paths outcome = [])

let () =
  (* The expectation inverts the whole outcome, phases included: an xfail
     bracket whose body passes but whose teardown fails is an expected
     failure, while one that passes everywhere is an unexpected pass. *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let xf_bracket ?reason name ~teardown_fails =
    Test_tree.xfail ?reason
      (Test_tree.bracket
         ~setup:(fun () -> ())
         ~teardown:(fun () -> if teardown_fails then Check.fail "leak")
         name
         (fun () -> ()))
  in
  let tests =
    [
      xf_bracket ~reason:"leaky teardown" "xf-teardown" ~teardown_fails:true;
      xf_bracket "xf-clean" ~teardown_fails:false;
    ]
  in
  expect_run "xfail-bracket suite runs" ~config tests @@ fun outcome ->
  check "a teardown failure is excused by the expectation"
    (match failure_list (outcome_of outcome [ "xf-teardown" ]) with
    | [ f ] -> f.Failure.phase = Failure.Teardown
    | _ -> false);
  check "the clean bracket is an unexpected pass"
    (match failure_list (outcome_of outcome [ "xf-clean" ]) with
    | [ f ] -> contains "expected to fail" (message_of f)
    | _ -> false);
  check "only the unexpected pass counts as failed"
    (failed_paths outcome = [ "xf-clean" ] && outcome.Run.exit_code = 1)

let () =
  (* Retries invert with the expectation: an expected failure is final on
     the first attempt; an unexpected pass keeps retrying, hoping to fail. *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [
      Test_tree.xfail
        (Test_tree.test ~retries:2 "keeps-failing" (fun () ->
             Check.fail "expected"));
      Test_tree.xfail (Test_tree.test ~retries:2 "keeps-passing" (fun () -> ()));
    ]
  in
  expect_run "xfail-retry suite runs" ~config tests @@ fun outcome ->
  (match result_of outcome [ "keeps-failing" ] with
  | Some r ->
      check_int "an expected failure is not retried" ~expected:1
        ~actual:r.Run.attempts
  | None -> check "keeps-failing recorded" false);
  (match result_of outcome [ "keeps-passing" ] with
  | Some r ->
      check_int "an unexpected pass uses every attempt" ~expected:3
        ~actual:r.Run.attempts
  | None -> check "keeps-passing recorded" false);
  check "only the unexpected pass counts as failed"
    (failed_paths outcome = [ "keeps-passing" ])

let () =
  (* -x stops at the first counted failure: an expected one does not stop
     the run. *)
  with_temp_root @@ fun root ->
  let config = { (base_config ~log_dir:root ()) with Run.bail = true } in
  let tests =
    [
      Test_tree.xfail (Test_tree.test "excused" (fun () -> Check.fail "known"));
      Test_tree.test "ok" (fun () -> ());
      Test_tree.test "boom" (fun () -> Check.fail "real");
      Test_tree.test "after" (fun () -> ());
    ]
  in
  expect_run "xfail-bail suite runs" ~config tests @@ fun outcome ->
  check "an expected failure does not stop the run"
    (ran_names outcome = [ "excused"; "ok"; "boom" ])

let () =
  (* The last-failed store: expected failures never enter it; unexpected
     passes do. *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let rerun = { config with Run.failed_only = true } in
  let tests =
    [
      Test_tree.xfail (Test_tree.test "xf" (fun () -> Check.fail "known"));
      Test_tree.test "real" (fun () -> Check.fail "boom");
    ]
  in
  expect_run "xfail-store: first run" ~config tests @@ fun outcome ->
  check "only the real failure counted" (failed_paths outcome = [ "real" ]);
  expect_run "--failed skips expected failures" ~config:rerun tests
  @@ fun outcome ->
  check "--failed reruns only the real failure" (ran_names outcome = [ "real" ])

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let rerun = { config with Run.failed_only = true } in
  let tests = [ Test_tree.xfail (Test_tree.test "xp" (fun () -> ())) ] in
  expect_run "xpass-store: first run" ~config tests @@ fun outcome ->
  check "the unexpected pass failed the run" (outcome.Run.exit_code = 1);
  expect_run "--failed reruns an unexpected pass" ~config:rerun tests
  @@ fun outcome -> check "xp reran" (ran_names outcome = [ "xp" ])

(* Sharding *)

let shard_names = [ "t-one"; "t-two"; "t-three"; "t-four"; "t-five" ]

let shard_suite () =
  List.map (fun n -> Test_tree.test n (fun () -> ())) shard_names

let () =
  with_temp_root @@ fun root ->
  let config shard = { (base_config ~log_dir:root ()) with Run.shard } in
  expect_run "1/1 sharding selects everything"
    ~config:(config (Some (1, 1)))
    (shard_suite ())
  @@ fun outcome -> check "1/1 runs all" (ran_names outcome = shard_names)

let () =
  with_temp_root @@ fun root ->
  let config shard = { (base_config ~log_dir:root ()) with Run.shard } in
  let bucket k =
    let seen = ref [] in
    expect_run
      (Printf.sprintf "shard %d/3 runs" k)
      ~config:(config (Some (k, 3)))
      (shard_suite ())
      (fun outcome -> seen := ran_names outcome);
    !seen
  in
  let buckets = List.map bucket [ 1; 2; 3 ] in
  let all = List.concat buckets in
  check "the shard buckets partition the suite"
    (List.sort compare all = List.sort compare shard_names);
  check "shard buckets preserve declaration order within a bucket"
    (List.for_all
       (fun bucket ->
         List.filter (fun n -> List.mem n bucket) shard_names = bucket)
       buckets);
  check "shard buckets are stable across runs" (bucket 1 = List.nth buckets 0);
  (* The mapping itself is frozen, stable across machines and windtrap
     versions: this golden assignment changes only with that promise. *)
  let golden =
    [ [ "t-two"; "t-four"; "t-five" ]; []; [ "t-one"; "t-three" ] ]
  in
  check "the frozen hash pins the exact bucket assignment" (buckets = golden);
  if buckets <> golden then
    List.iteri
      (fun i bucket ->
        Printf.printf "  bucket %d: [%s]\n%!" (i + 1)
          (String.concat "; " bucket))
      buckets

let () =
  (* Sharding composes with filters: the bucket applies to the filtered
     set, and an empty shard exits 2 like any empty selection. *)
  with_temp_root @@ fun root ->
  let config k =
    {
      (base_config ~log_dir:root ()) with
      Run.shard = Some (k, 3);
      filter = [ "t-one" ];
    }
  in
  let outcomes =
    List.map
      (fun k ->
        let seen = ref ([], -1) in
        expect_run (Printf.sprintf "filtered shard %d/3 runs" k)
          ~config:(config k) (shard_suite ()) (fun outcome ->
            seen := (ran_names outcome, outcome.Run.exit_code));
        !seen)
      [ 1; 2; 3 ]
  in
  check "exactly one bucket holds the filtered test"
    (List.length (List.filter (fun (ran, _) -> ran = [ "t-one" ]) outcomes) = 1);
  check "the other buckets are empty and exit 2 (nothing ran)"
    (List.length
       (List.filter (fun (ran, code) -> ran = [] && code = 2) outcomes)
    = 2)

let () =
  (* Sharding composes with focus: the bucket applies to the focused set, so
     exactly one bucket runs the focused test — alone — and the others are
     empty selections. *)
  clear_env ();
  with_temp_root @@ fun root ->
  let suite =
    [
      Test_tree.test "plain-one" (fun () -> ());
      Test_tree.focus (Test_tree.test "starred" (fun () -> ()));
      Test_tree.test "plain-two" (fun () -> ());
    ]
  in
  let outcomes =
    List.map
      (fun k ->
        let seen = ref ([], -1) in
        expect_run
          (Printf.sprintf "focused shard %d/3 runs" k)
          ~config:
            { (base_config ~log_dir:root ()) with Run.shard = Some (k, 3) }
          suite
          (fun outcome -> seen := (ran_names outcome, outcome.Run.exit_code));
        !seen)
      [ 1; 2; 3 ]
  in
  check "exactly one bucket runs the focused test, alone"
    (List.length (List.filter (fun (ran, _) -> ran = [ "starred" ]) outcomes)
    = 1);
  check "the other buckets run nothing and exit 2 (nothing ran)"
    (List.length
       (List.filter (fun (ran, code) -> ran = [] && code = 2) outcomes)
    = 2)

let () =
  with_temp_root @@ fun root ->
  let config =
    { (base_config ~log_dir:root ()) with Run.shard = Some (2, 1) }
  in
  match
    Run.execute config ~suite:"suite" [ Test_tree.test "t" (fun () -> ()) ]
  with
  | exception Invalid_argument _ ->
      check "a malformed hand-built shard fails loudly" true
  | Ok _ | Error _ -> check "a malformed hand-built shard fails loudly" false

(* Test-body operations through the boundary *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let seen_path = ref [] in
  let scratch = ref [] in
  let tests =
    [
      Test_tree.group "grp"
        [
          Test_tree.test "identity" (fun () -> seen_path := Run.current_test ());
        ];
      Test_tree.test "passing-scratch" (fun () ->
          let dir = Run.temp_dir () in
          let file = Run.temp_file ~suffix:".log" () in
          let oc = open_out (Filename.concat dir "data") in
          output_string oc "x";
          close_out oc;
          scratch := dir :: file :: !scratch);
      Test_tree.test "failing-scratch" (fun () ->
          scratch := Run.temp_dir () :: !scratch;
          Check.fail "boom");
      Test_tree.test "skipping-scratch" (fun () ->
          scratch := Run.temp_dir () :: !scratch;
          Check.skip ());
    ]
  in
  expect_run "body-ops suite runs" ~config tests @@ fun outcome ->
  check "current_test sees the full path inside the runner"
    (!seen_path = [ "grp"; "identity" ]);
  check_int "scratch paths were created" ~expected:4
    ~actual:(List.length !scratch);
  check "scratch paths are removed on pass, failure, and skip alike"
    (List.for_all (fun p -> not (Sys.file_exists p)) !scratch);
  check "scratch cleanup does not alter outcomes"
    (outcome.Run.exit_code = 1 && failed_paths outcome = [ "failing-scratch" ])

let () =
  (* Scratch removal on the boundary's worst paths (guarantee 8):
     scratch created in the body and in a raising teardown of the very test
     that trips -x is still removed. *)
  with_temp_root @@ fun root ->
  let config = { (base_config ~log_dir:root ()) with Run.bail = true } in
  let scratch = ref [] in
  let note path = scratch := path :: !scratch in
  let tests =
    [
      Test_tree.bracket
        ~setup:(fun () -> ())
        ~teardown:(fun () ->
          note (Run.temp_dir ~prefix:"td" ());
          Check.fail "td-boom")
        "teardown-scratch"
        (fun () -> note (Run.temp_dir ()));
      Test_tree.test "never-runs" (fun () -> ());
    ]
  in
  expect_run "teardown-scratch suite runs" ~config tests @@ fun outcome ->
  check "the failing teardown tripped the bail"
    (ran_names outcome = [ "teardown-scratch" ]);
  check_int "scratch was created in body and teardown alike" ~expected:2
    ~actual:(List.length !scratch);
  check "scratch is removed when the teardown raises, under -x too"
    (List.for_all (fun p -> not (Sys.file_exists p)) !scratch)

let () =
  (* Retries never collide in the scratch: each attempt gets a fresh
     directory, and the previous attempt's is gone before the retry runs. *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let dirs = ref [] in
  let stale_seen = ref false in
  let tests =
    [
      Test_tree.test ~retries:2 "scratch-retry" (fun () ->
          if List.exists Sys.file_exists !dirs then stale_seen := true;
          dirs := Run.temp_dir () :: !dirs;
          if List.length !dirs < 3 then Check.fail "again");
    ]
  in
  expect_run "scratch-retry suite runs" ~config tests @@ fun outcome ->
  check "the test passed on its final attempt"
    (outcome_of outcome [ "scratch-retry" ] = Some Failure.Pass);
  check "each attempt got a distinct scratch directory"
    (List.length (List.sort_uniq compare !dirs) = 3);
  check "no attempt saw a previous attempt's scratch" (not !stale_seen);
  check "the final attempt's scratch is removed too"
    (List.for_all (fun d -> not (Sys.file_exists d)) !dirs)

(* Test-scoped environment and working directory (setenv, chdir)

   Both are process-global, so the runner's restoration is what makes them
   test-scoped: what the attempt bound is put back at the attempt boundary,
   outside the timeout window, on every outcome. The variables below are
   windtrap's own namespace so a failing meta-run leaks nothing a later
   suite reads. *)

let unset_var = "WINDTRAP_TEST_SCOPED_UNSET"
let bound_var = "WINDTRAP_TEST_SCOPED_BOUND"
let twice_var = "WINDTRAP_TEST_SCOPED_TWICE"
let drop_var = "WINDTRAP_TEST_SCOPED_DROP"

(* The blocks below bind the four variables outside any run; this unbinds
   them when a block ends, however it ends. *)
let unbind_scoped () =
  List.iter
    (fun var -> Os.setenv var None)
    [ unset_var; bound_var; twice_var; drop_var ]

let () =
  Fun.protect ~finally:unbind_scoped @@ fun () ->
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  (* Four shapes of prior state: never bound, bound, bound and rebound by
     the test itself, and bound then unbound by the test. *)
  Os.setenv unset_var None;
  Os.setenv bound_var (Some "before");
  Os.setenv twice_var (Some "before");
  Os.setenv drop_var (Some "before");
  let seen = ref [] in
  let note name = seen := (name, Sys.getenv_opt name) :: !seen in
  let tests =
    [
      Test_tree.test "binds" (fun () ->
          Run.setenv unset_var (Some "inside");
          Run.setenv bound_var (Some "inside");
          Run.setenv twice_var (Some "first");
          Run.setenv twice_var (Some "second");
          Run.setenv drop_var None;
          List.iter note [ unset_var; bound_var; twice_var; drop_var ]);
    ]
  in
  expect_run "setenv suite runs" ~config tests @@ fun outcome ->
  check "the suite passed" (outcome.Run.exit_code = 0);
  check "setenv binds for the rest of the test"
    (List.sort compare !seen
    = List.sort compare
        [
          (unset_var, Some "inside");
          (bound_var, Some "inside");
          (twice_var, Some "second");
          (drop_var, None);
        ]);
  check "a variable the test found unbound is unbound again — not empty"
    (Sys.getenv_opt unset_var = None);
  check "a variable the test found bound is restored to its prior value"
    (Sys.getenv_opt bound_var = Some "before");
  check "two setenvs of one name restore what the first one found"
    (Sys.getenv_opt twice_var = Some "before");
  check "a variable the test unbound is bound again"
    (Sys.getenv_opt drop_var = Some "before")

let () =
  (* A rejected name records nothing: [Os.setenv] validates before the restore
     entry is made, so the documented [Invalid_argument] is the whole story —
     no entry survives to replay the same rejection at the boundary as a
     restoration failure about a change that never happened. *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [
      Test_tree.test "rejects" (fun () ->
          (match Run.setenv "" (Some "x") with
          | () -> Check.fail "an empty name must be rejected"
          | exception Invalid_argument _ -> ());
          match Run.setenv "BAD=NAME" (Some "x") with
          | () -> Check.fail "a name containing '=' must be rejected"
          | exception Invalid_argument _ -> ());
    ]
  in
  expect_run "setenv rejection suite runs" ~config tests @@ fun outcome ->
  check "a handled rejection is the whole story — the test passes"
    (outcome.Run.exit_code = 0)

let () =
  (* Restoration is not the pass path's privilege: it happens on failure,
     on skip, and on a timeout that cut the body short. *)
  if Sys.win32 then skip_scenario ~reason:"POSIX only" __POS__
  else
    Fun.protect ~finally:unbind_scoped @@ fun () ->
    with_temp_root @@ fun root ->
    let config = base_config ~log_dir:root () in
    Os.setenv bound_var (Some "before");
    let after = ref [] in
    let tests =
      [
        Test_tree.test "fails" (fun () ->
            Run.setenv bound_var (Some "failing");
            Check.fail "boom");
        Test_tree.test "skips" (fun () ->
            Run.setenv bound_var (Some "skipping");
            Check.skip ());
        Test_tree.test ~timeout:0.2 "times out" (fun () ->
            Run.setenv bound_var (Some "hanging");
            busy_forever ());
      ]
    in
    let on_event = function
      | Run.Test_finished _ -> after := Sys.getenv_opt bound_var :: !after
      | Run.Run_started _ | Run.Test_started _ | Run.Fixture_release _
      | Run.Interrupted _ ->
          ()
    in
    expect_run "setenv outcomes suite runs" ~on_event ~config tests
    @@ fun outcome ->
    check "the timing-out test is a failure"
      (failed_paths outcome = [ "fails"; "times out" ]);
    check "the binding is restored after failure, skip, and timeout alike"
      (!after = [ Some "before"; Some "before"; Some "before" ])

(* A directory that no longer exists is exactly the state a missing
   restoration leaves behind, so reading the working directory must not
   itself be fatal here: a regression has to read as a failed check, not as
   a fatal error that takes the rest of the suite with it. *)
let cwd_opt () = try Some (Sys.getcwd ()) with Sys_error _ -> None
let go_home home = try Unix.chdir home with Unix.Unix_error _ -> ()

let () =
  (* chdir is per attempt like the scratch paths: every retry captures and
     restores its own directory, so each attempt starts where the first
     one did — and not in the deleted scratch of the attempt before. *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let home = Sys.getcwd () in
  let at_entry = ref [] in
  let inside = ref [] in
  let between = ref [] in
  let tests =
    [
      Test_tree.test ~retries:2 "chdir-retry" (fun () ->
          at_entry := cwd_opt () :: !at_entry;
          Run.chdir (Run.temp_dir ());
          inside := cwd_opt () :: !inside;
          if List.length !inside < 3 then Check.fail "again");
    ]
  in
  let on_event = function
    | Run.Test_started _ | Run.Test_finished _ ->
        between := cwd_opt () :: !between
    | Run.Run_started _ | Run.Fixture_release _ | Run.Interrupted _ -> ()
  in
  expect_run "chdir suite runs" ~on_event ~config tests @@ fun outcome ->
  go_home home;
  check "the test passed on its final attempt"
    (outcome_of outcome [ "chdir-retry" ] = Some Failure.Pass);
  check_int "every attempt ran the body" ~expected:3
    ~actual:(List.length !inside);
  check "chdir took effect inside the test"
    (List.for_all (fun d -> d <> None && d <> Some home) !inside);
  check "every attempt starts in the directory the first one did"
    (List.length !at_entry = 3
    && List.for_all (fun d -> d = Some home) !at_entry);
  check "the directory is restored before the runner moves on"
    (!between <> [] && List.for_all (fun d -> d = Some home) !between);
  check "the run ends where it started" (cwd_opt () = Some home)

let () =
  (* A restoration that cannot happen is stated, not swallowed: unlike a
     leaked scratch directory, a process left in the wrong place breaks
     every test after it. The prior directory here is one the test itself
     removes, so the runner's chdir back has nowhere to go. *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let home = Sys.getcwd () in
  let gone = Filename.concat root "gone" in
  Unix.mkdir gone 0o700;
  let site = ref None in
  let tests =
    [
      Test_tree.test "chdir-restore-fails" (fun () ->
          (* Enter [gone] without telling the runner, so it becomes the
             directory the first [chdir] below captures. Both bindings sit
             on one line so the captured line number is known. *)
          Unix.chdir gone;
          let p = __POS__ and () = Run.chdir root in
          site := Some p;
          Unix.rmdir gone);
    ]
  in
  expect_run "chdir-restore suite runs" ~config tests @@ fun outcome ->
  go_home home;
  check "an unrestorable directory fails the test"
    (failed_paths outcome = [ "chdir-restore-fails" ]);
  match failure_list (outcome_of outcome [ "chdir-restore-fails" ]) with
  | [ f ] ->
      check "the restoration failure is attributed to cleanup"
        (f.Failure.phase = Failure.Teardown);
      check "it names the directory it could not return to"
        (contains "working directory" (message_of f)
        && contains gone (message_of f));
      (* The boundary that discovers the problem is nobody's code, so the
         report points at the [chdir] the test made. *)
      check "it is located at the change that could not be undone"
        (match (f.Failure.loc, !site) with
        | Some loc, Some (file, line, _, _) ->
            Filename.basename loc.Loc.file = Filename.basename file
            && loc.Loc.line = line
        | _ -> false)
  | fs ->
      check_int "chdir restore failure entries" ~expected:1
        ~actual:(List.length fs)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [
      Test_tree.test "layouts" (fun () ->
          Run.subtest "row-major" (fun () -> Check.fail "bad shape");
          Run.subtest "col-major" (fun () -> ());
          Run.subtest "strided" (fun () -> Check.fail "bad stride"));
    ]
  in
  expect_run "subtest suite runs" ~config tests @@ fun outcome ->
  (match failure_list (outcome_of outcome [ "layouts" ]) with
  | [ a; b ] ->
      check "each failing subtest is one labeled entry, in order"
        (Report.labeled_msg a = Some "layouts › row-major"
        && Report.labeled_msg b = Some "layouts › strided")
  | _ -> check "two subtest failures recorded" false);
  check "subtest failures fail the test"
    (outcome.Run.exit_code = 1 && failed_paths outcome = [ "layouts" ])

let () =
  (* Subtest failures reset per attempt: a retry that stops failing
     passes. *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let attempts = ref 0 in
  let tests =
    [
      Test_tree.test ~retries:1 "flaky-subcase" (fun () ->
          incr attempts;
          let failing = !attempts = 1 in
          Run.subtest "sub" (fun () ->
              if failing then Check.fail "first attempt only"));
    ]
  in
  expect_run "subtest-retry suite runs" ~config tests @@ fun outcome ->
  match result_of outcome [ "flaky-subcase" ] with
  | Some r ->
      check "subtest failures reset per attempt"
        (r.Run.outcome = Failure.Pass && r.Run.attempts = 2)
  | None -> check "flaky-subcase recorded" false

let () =
  (* Subtests compose with brackets: a failing sub-case in the body cannot
     prevent the teardown, and a teardown failure lands after the labeled
     entries, phase-classified. *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let torn_down = ref false in
  let tests =
    [
      Test_tree.bracket
        ~setup:(fun () -> 7)
        ~teardown:(fun _ ->
          torn_down := true;
          Check.fail "td-boom")
        "bracketed"
        (fun resource ->
          Run.subtest "uses-resource" (fun () -> Check.is_true (resource <> 7));
          Run.subtest "fine" (fun () -> ()));
    ]
  in
  expect_run "bracket-subtest suite runs" ~config tests @@ fun outcome ->
  check "the teardown ran after a failing subtest" !torn_down;
  match failure_list (outcome_of outcome [ "bracketed" ]) with
  | [ sub; td ] ->
      check "the subtest entry is labeled and precedes the teardown's"
        (Report.labeled_msg sub = Some "bracketed › uses-resource"
        && td.Failure.phase = Failure.Teardown)
  | _ -> check "bracket-subtest: two entries" false

let () =
  (* Inside a property body a subtest failure bypasses the engine: every
     case completes unshrunk (the engine sees no failure) and the test fails
     with the labeled entries, not a Property counterexample. *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [
      Run.prop ~count:5 "prop-sub" Gen.int (fun _ ->
          Run.subtest "law-half" (fun () -> Check.fail "nope"));
    ]
  in
  expect_run "prop-subtest suite runs" ~config tests @@ fun outcome ->
  match result_of outcome [ "prop-sub" ] with
  | Some r -> (
      (match r.Run.prop_stats with
      | Some stats ->
          check_int "every case completed: the engine saw no failure"
            ~expected:5 ~actual:stats.Property.cases
      | None -> check "prop-sub records stats" false);
      match failure_list (Some r.Run.outcome) with
      | [] -> check "prop-sub failed with subtest entries" false
      | entries ->
          check_int "one labeled entry per failing case" ~expected:5
            ~actual:(List.length entries);
          check "the entries are labeled subtest failures, not Property"
            (List.for_all
               (fun f ->
                 Report.labeled_msg f = Some "prop-sub › law-half"
                 &&
                 match f.Failure.kind with
                 | Failure.Property _ -> false
                 | _ -> true)
               entries))
  | None -> check "prop-sub recorded" false

(* Fixture acquisition skips *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let device =
    Run.fixture
      ~teardown:(fun _ -> check "a skipped fixture must never release" false)
      (fun () -> Check.skip ~reason:"no metal device" ())
  in
  let announced = ref false in
  let on_event = function
    | Run.Fixture_release _ -> announced := true
    | _ -> ()
  in
  let tests =
    [
      Test_tree.test "first-gpu" (fun () -> ignore (device ()));
      Test_tree.test "second-gpu" (fun () -> ignore (device ()));
    ]
  in
  expect_run "fixture-skip suite runs" ~on_event ~config tests @@ fun outcome ->
  check "a skipped fixture is never announced for release" (not !announced);
  check "an unavailable optional resource does not turn the run red"
    (outcome.Run.exit_code = 0 && outcome.Run.release_failures = [])

(* Duplicate paths *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite =
    [
      Test_tree.group "g" [ Test_tree.test "same" (fun () -> ()) ];
      Test_tree.group "g" [ Test_tree.test "same" (fun () -> ()) ];
    ]
  in
  expect_startup_error "duplicate paths are a startup error" ~config suite
    (function
    | Run.Duplicate_paths [ path ] -> contains "same" path
    | _ -> false);
  match Run.execute config ~suite:"suite" suite with
  | Error error ->
      check "duplicates exit 1" (Run.startup_exit_code error = 1);
      check "the message lists the path"
        (contains "same" (Run.startup_message error));
      (* For [windtrap:], which anchors the first line: a path per line,
         then the rule. *)
      check_string "the paths are listed, one per line, then the rule"
        ~expected:
          "duplicate test paths:\n\
          \  a\n\
          \  b \u{203a} c\n\
           Every full test path must be unique."
        ~actual:
          (Run.startup_message (Run.Duplicate_paths [ "a"; "b \u{203a} c" ]));
      check_string "an empty --failed store keeps its sentence"
        ~expected:"no recorded failures match the current suite"
        ~actual:(Run.startup_message Run.No_recorded_failures)
  | Ok _ -> check "duplicate refusal expected" false

let () =
  (* [cases ?name] applies the renderer at declaration time: two inputs
     rendering to the same name collide as duplicate paths. *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite =
    [ Test_tree.cases ~name:string_of_int "c" [ 1; 2; 1 ] (fun _ -> ()) ]
  in
  expect_startup_error "cases name collisions are duplicate paths" ~config suite
    (function
    | Run.Duplicate_paths [ path ] -> contains "1" path
    | _ -> false)

(* Fixtures: release order, bail, release failures *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let order = ref [] in
  let fx_a =
    Run.fixture ~teardown:(fun _ -> order := "a" :: !order) (fun () -> "a")
  and fx_b =
    Run.fixture ~teardown:(fun _ -> order := "b" :: !order) (fun () -> "b")
  in
  let events = ref [] in
  let on_event e = events := e :: !events in
  let tests =
    [
      Test_tree.test "uses-both" (fun () ->
          ignore (fx_a ());
          ignore (fx_b ()));
      Test_tree.test "second" (fun () -> ());
    ]
  in
  expect_run "fixture suite runs" ~on_event ~config tests @@ fun outcome ->
  check "fixtures release in reverse acquisition order"
    (List.rev !order = [ "b"; "a" ]);
  check "no release failures" (outcome.Run.release_failures = []);
  let events = List.rev !events in
  let is_finish = function Run.Test_finished _ -> true | _ -> false in
  let is_release = function Run.Fixture_release _ -> true | _ -> false in
  let last_finish =
    List.fold_left
      (fun (i, last) e -> (i + 1, if is_finish e then i else last))
      (0, -1) events
    |> snd
  and first_release =
    let rec find i = function
      | [] -> -1
      | e :: _ when is_release e -> i
      | _ :: rest -> find (i + 1) rest
    in
    find 0 events
  in
  check "two release announcements"
    (List.length (List.filter is_release events) = 2);
  check "releases are announced after the last test"
    (first_release > last_finish)

let () =
  with_temp_root @@ fun root ->
  let config = { (base_config ~log_dir:root ()) with Run.bail = true } in
  let released = ref false in
  let fx = Run.fixture ~teardown:(fun _ -> released := true) (fun () -> ()) in
  let tests =
    [
      Test_tree.test "first-fails" (fun () ->
          ignore (fx ());
          Check.fail "boom");
      Test_tree.test "never-runs" (fun () -> ());
      Test_tree.test "never-runs-either" (fun () -> ());
    ]
  in
  expect_run "bail suite runs" ~config tests @@ fun outcome ->
  check "bail stops at the first failure" (ran_names outcome = [ "first-fails" ]);
  check "fixtures release under bail" !released;
  check "bailed failing run exits 1" (outcome.Run.exit_code = 1)

let () =
  (* A raising [on_event] observer aborts the run, but acquired fixtures
     still release (guarantee 8). *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let released = ref false in
  let fx = Run.fixture ~teardown:(fun _ -> released := true) (fun () -> ()) in
  let on_event = function
    | Run.Test_started { path = [ "second" ] } -> raise Boom
    | _ -> ()
  in
  let tests =
    [
      Test_tree.test "first" (fun () -> ignore (fx ()));
      Test_tree.test "second" (fun () -> ());
    ]
  in
  (match Run.execute ~on_event config ~suite:"suite" tests with
  | exception Boom -> check "the observer's exception aborts the run" true
  | Ok _ | Error _ -> check "the observer's exception aborts the run" false);
  check "fixtures release when an observer kills the run" !released

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let fx = Run.fixture ~teardown:(fun _ -> raise Boom) (fun () -> ()) in
  let tests = [ Test_tree.test "acquires" (fun () -> ignore (fx ())) ] in
  expect_run "release-failure suite runs" ~config tests @@ fun outcome ->
  (match outcome.Run.release_failures with
  | [ f ] ->
      check "a failing release is a Release-phase failure"
        (f.Failure.phase = Failure.Release)
  | _ -> check "one release failure" false);
  check "a release failure exits 1, tests all green"
    (outcome.Run.exit_code = 1
    && outcome_of outcome [ "acquires" ] = Some Failure.Pass)

(* Events and list-only *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let events = ref [] in
  let on_event e = events := e :: !events in
  let tests =
    [ Test_tree.test "one" (fun () -> ()); Test_tree.test "two" (fun () -> ()) ]
  in
  expect_run "event suite runs" ~on_event ~config tests @@ fun _ ->
  (match List.rev !events with
  | [
   Run.Run_started
     { suite = "suite"; total = 2; selected = 2; properties = false };
   Run.Test_started { path = [ "one" ] };
   Run.Test_finished r1;
   Run.Test_started { path = [ "two" ] };
   Run.Test_finished r2;
  ] ->
      check "events stream in execution order"
        (r1.Run.path = [ "one" ] && r2.Run.path = [ "two" ])
  | _ -> check "events stream in execution order" false);
  events := [];
  match Run.list_selection config ~suite:"suite" tests with
  | Error _ -> check "--list is the selection, and runs nothing" false
  | Ok paths ->
      check "--list is the selection, and runs nothing"
        (paths = [ "one"; "two" ] && !events = [])

let () =
  (* Whether the seed decides anything is known before the first result:
     [Run_started] says whether a selected test is a property, from the
     selection and not from the declarations. *)
  with_temp_root @@ fun root ->
  let tests =
    [
      Test_tree.test "plain" (fun () -> ());
      Test_tree.test ~tags:[ Test_tree.Tag.prop ] "law" (fun () -> ());
    ]
  in
  let started ~filter =
    let seen = ref None in
    let on_event = function
      | Run.Run_started { properties; selected; _ } ->
          seen := Some (properties, selected)
      | Run.Test_started _ | Run.Test_finished _ | Run.Fixture_release _
      | Run.Interrupted _ ->
          ()
    in
    let config = { (base_config ~log_dir:root ()) with Run.filter } in
    expect_run "property selection suite runs" ~on_event ~config tests (fun _ ->
        ());
    !seen
  in
  check "a selection holding a property says so"
    (started ~filter:[] = Some (true, 2));
  check "a selected property alone says so"
    (started ~filter:[ "law" ] = Some (true, 1));
  check "a selection that leaves the property out does not"
    (started ~filter:[ "plain" ] = Some (false, 1))

(* Property wiring *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let ctx_seen = ref false in
  let tests =
    [
      Run.prop "always-holds" Gen.int (fun _ ->
          ctx_seen :=
            !ctx_seen || Run.prop_context (Run.current_frame ()) <> None);
      Run.prop ~count:3 "tiny" Gen.int (fun _ -> ());
      Run.prop "never-holds" Gen.int (fun n -> Check.is_true (n = n + 1));
    ]
  in
  expect_run "property suite runs" ~config tests @@ fun outcome ->
  (match result_of outcome [ "always-holds" ] with
  | Some r ->
      check "a passing property passes" (r.Run.outcome = Failure.Pass);
      (match r.Run.prop_stats with
      | Some stats ->
          check_int "default case count is 100" ~expected:100
            ~actual:stats.Property.cases
      | None -> check "passing property records stats" false);
      check "the engine context is installed while the law runs" !ctx_seen
  | None -> check "always-holds recorded" false);
  (match result_of outcome [ "tiny" ] with
  | Some { Run.prop_stats = Some stats; _ } ->
      check_int "~count beats the default" ~expected:3
        ~actual:stats.Property.cases
  | _ -> check "tiny recorded with stats" false);
  match result_of outcome [ "never-holds" ] with
  | Some r -> (
      check "a failing property records stats" (r.Run.prop_stats <> None);
      match failure_list (Some r.Run.outcome) with
      | [ { Failure.kind = Failure.Property { root; examples; _ }; _ } ] ->
          check "the failure carries the run's root seed"
            (root = 0x5eedL && not examples)
      | _ -> check "failing property yields a Property failure" false)
  | None -> check "never-holds recorded" false

let () =
  with_temp_root @@ fun root ->
  let config =
    { (base_config ~log_dir:root ()) with Run.prop_count = Some 5 }
  in
  let tests =
    [
      Run.prop "counted" Gen.int (fun _ -> ());
      Run.prop ~count:2 "declared" Gen.int (fun _ -> ());
      Run.prop "counted-fails" Gen.int (fun _ -> Check.fail "no");
      Run.prop ~count:2 "declared-fails" Gen.int (fun _ -> Check.fail "no");
    ]
  in
  expect_run "prop-count suite runs" ~config tests @@ fun outcome ->
  (match result_of outcome [ "counted" ] with
  | Some { Run.prop_stats = Some stats; _ } ->
      check_int "--prop-count applies when undeclared" ~expected:5
        ~actual:stats.Property.cases
  | _ -> check "counted recorded" false);
  (match result_of outcome [ "declared" ] with
  | Some { Run.prop_stats = Some stats; _ } ->
      check_int "a declared ~count beats --prop-count" ~expected:2
        ~actual:stats.Property.cases
  | _ -> check "declared recorded" false);
  (match failure_list (outcome_of outcome [ "counted-fails" ]) with
  | [ { Failure.kind = Failure.Property { count; _ }; _ } ] ->
      check "a config-sourced count rides the failure payload" (count = Some 5)
  | _ -> check "counted-fails yields a Property failure" false);
  match failure_list (outcome_of outcome [ "declared-fails" ]) with
  | [ { Failure.kind = Failure.Property { count; _ }; _ } ] ->
      check "a declared ~count never rides the payload (it replays by itself)"
        (count = None)
  | _ -> check "declared-fails yields a Property failure" false

(* A declared [?summary] reaches the failure the run records: what
   [Stateful.stateful] relies on for its program's head line. *)
let () =
  with_temp_root @@ fun root ->
  let tests =
    [
      Run.prop "summarized"
        ~summary:(fun value -> Some (Pp.str "n=%d" value))
        (Gen.int_range 0 1000)
        (fun value -> Check.is_true (value < 10));
      Run.prop "plain" (Gen.int_range 0 1000) (fun value ->
          Check.is_true (value < 10));
    ]
  in
  expect_run "a summarized property"
    ~config:(base_config ~log_dir:root ())
    tests
  @@ fun outcome ->
  (match failure_list (outcome_of outcome [ "summarized" ]) with
  | [ { Failure.kind = Failure.Property { summary; _ }; _ } ] ->
      check "a declared ?summary rides the failure, of the shrunk value"
        (Option.map (fun (s : Failure.text) -> s.kept) summary = Some "n=10")
  | _ -> check "summarized yields a Property failure" false);
  match failure_list (outcome_of outcome [ "plain" ]) with
  | [ { Failure.kind = Failure.Property { summary; _ }; _ } ] ->
      check "a property declared without one records none" (summary = None)
  | _ -> check "plain yields a Property failure" false

(* The replay-hint contract behind the payload count: a case beyond the
   default count is reachable on replay only when the failing run's
   config-sourced count is restated. The body fails on its 500th call, so
   under [--prop-count 1000] the failure lands at case 499; replaying the
   root with the payload count reproduces it, while the same root under
   the default count never reaches the case — the pre-payload hint's lie. *)
let () =
  with_temp_root @@ fun root ->
  let calls = ref 0 in
  let tests =
    [
      Run.prop "late" Gen.int (fun _ ->
          incr calls;
          if !calls >= 500 then Check.fail "late failure");
    ]
  in
  let run_with name ~prop_count f =
    calls := 0;
    let config = { (base_config ~log_dir:root ()) with Run.prop_count } in
    expect_run name ~config tests f
  in
  run_with "late-failure run" ~prop_count:(Some 1000) @@ fun outcome ->
  (match failure_list (outcome_of outcome [ "late" ]) with
  | [ { Failure.kind = Failure.Property { case_index; count; root; _ }; _ } ] ->
      check "the late case fails beyond the default count"
        (case_index = 499 && count = Some 1000 && root = 0x5eedL)
  | _ -> check "late yields a Property failure" false);
  run_with "replay with the payload count" ~prop_count:(Some 1000)
  @@ fun replay ->
  (match failure_list (outcome_of replay [ "late" ]) with
  | [ { Failure.kind = Failure.Property { case_index; _ }; _ } ] ->
      check "seed plus payload count reproduces the failing case"
        (case_index = 499)
  | _ -> check "replay with the count reproduces a Property failure" false);
  run_with "replay without the count" ~prop_count:None @@ fun no_count ->
  check "the same seed under the default count never reaches the case"
    (outcome_of no_count [ "late" ] = Some Failure.Pass)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [ Run.prop "shrinks" Gen.int (fun n -> Check.is_true (n = n + 1)) ]
  in
  let rendered outcome =
    match failure_list (outcome_of outcome [ "shrinks" ]) with
    | [ { Failure.kind = Failure.Property { rendered; case_index; _ }; _ } ] ->
        Some (rendered.Failure.kept, case_index)
    | _ -> None
  in
  expect_run "prop determinism (1st)" ~config tests @@ fun first ->
  expect_run "prop determinism (2nd)" ~config tests @@ fun second ->
  check "the same root seed reproduces the same counterexample"
    (rendered first <> None && rendered first = rendered second)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [
      Run.prop ~examples:[ 0 ] "bad-example" Gen.int (fun n ->
          Check.is_true (n <> 0));
      Run.prop ~count:5 ~examples:[ 1; 2 ] "counted-examples" Gen.int (fun _ ->
          ());
      Run.prop ~count:5 "gives-up"
        Gen.(such_that (fun _ -> false) int)
        (fun _ -> ());
      Run.prop ~count:5 "under-covered" Gen.int (fun _ ->
          let ctx =
            match Run.prop_context (Run.current_frame ()) with
            | Some ctx -> ctx
            | None -> Check.fail "no property context"
          in
          Property.cover ctx "never" false);
    ]
  in
  expect_run "prop-edge suite runs" ~config tests @@ fun outcome ->
  (match failure_list (outcome_of outcome [ "bad-example" ]) with
  | [ { Failure.kind = Failure.Property { examples; shrink_steps; _ }; _ } ] ->
      check "a failing example reports examples=true, unshrunk"
        (examples && shrink_steps = 0)
  | _ -> check "bad-example: one Property failure" false);
  (match result_of outcome [ "counted-examples" ] with
  | Some { Run.prop_stats = Some stats; outcome = Failure.Pass; _ } ->
      check_int "examples count toward the passing cases" ~expected:7
        ~actual:stats.Property.cases
  | _ -> check "counted-examples passes with stats" false);
  (match failure_list (outcome_of outcome [ "gives-up" ]) with
  | [ f ] ->
      check "an exhausted budget fails with the discard count"
        (contains "gave up" (message_of f))
  | _ -> check "gives-up: one failure" false);
  match failure_list (outcome_of outcome [ "under-covered" ]) with
  | [ f ] ->
      check "an uncovered label is named, with the cases it went unmarked in"
        (contains "never covered" (message_of f)
        && contains {|"never"|} (message_of f)
        && contains "5 passing cases" (message_of f))
  | _ -> check "under-covered: one failure" false

(* Nested runs *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [
      Test_tree.test "starts-a-run" (fun () ->
          ignore (Run.execute config ~suite:"inner" []));
    ]
  in
  expect_run "nested-run suite runs" ~config tests @@ fun outcome ->
  match failure_list (outcome_of outcome [ "starts-a-run" ]) with
  | [
   {
     Failure.kind =
       Failure.Raise { actual = Some { Failure.kept = actual; _ }; _ };
     _;
   };
  ] ->
      check "a nested run fails the calling test"
        (contains "already executing" actual)
  | _ -> check "a nested run fails the calling test" false

(* The baseline CI guard *)

let () =
  Fun.protect ~finally:clear_env @@ fun () ->
  with_temp_root @@ fun root ->
  let config =
    { (base_config ~log_dir:root ()) with Run.baseline = Baseline.Update }
  in
  let suite = [ Test_tree.test "ok" (fun () -> ()) ] in
  Unix.putenv "CI" "true";
  expect_startup_error "-u is refused under CI" ~config suite (function
    | Run.Update_refused_in_ci -> true
    | _ -> false);
  let message = Run.startup_message Run.Update_refused_in_ci in
  check "the refusal names the CI-safe acceptance"
    (contains "--corrected" message && contains "dune promote" message);
  check_string "and says why, in sentences"
    ~expected:
      "baseline update refused: CI is set. -u rewrites baselines in place, \
       which is a developer's edit; under CI run with --corrected and accept \
       with dune promote."
    ~actual:message;
  let corrected = { config with Run.baseline = Baseline.Corrected } in
  expect_run "--corrected proceeds under CI" ~config:corrected suite
  @@ fun outcome -> check "corrected run is green" (outcome.Run.exit_code = 0)

(* Corrections

   The registry the runner builds resolves paths under
   [Os.project_root], so a throwaway root goes in
   WINDTRAP_PROJECT_ROOT. The runner's own directory is dune's build tree
   of another root, so nothing here is a build action: corrections land
   beside the files themselves. *)

let read_file path = In_channel.with_open_bin path In_channel.input_all
let baseline root = Filename.concat root "src/help.expected"

let () =
  Fun.protect ~finally:clear_env @@ fun () ->
  clear_env ();
  with_temp_root @@ fun root ->
  Unix.putenv "WINDTRAP_PROJECT_ROOT" root;
  let base = base_config ~log_dir:(Filename.concat root "_logs") () in
  let suite =
    [
      Test_tree.test "t1" (fun () ->
          Windtrap.expect_file "hello\n" "src/help.expected");
      Test_tree.test "t2" (fun () -> ());
    ]
  in
  (* Check mode: the mismatch fails the run and writes nothing. *)
  expect_run "check mode fails on a missing baseline" ~config:base suite
  @@ fun outcome ->
  check "the run fails"
    (outcome.Run.exit_code = 1 && failed_paths outcome = [ "t1" ]);
  check "nothing is written"
    ((not (Sys.file_exists (baseline root)))
    && Baseline.writes (Run.baselines outcome.Run.run) = []);
  (* Corrected mode: the test still fails, the correction lands beside the
     file, and the exit code is left to the diff that follows. *)
  let corrected = { base with Run.baseline = Baseline.Corrected } in
  expect_run "corrected mode leaves the exit code to the diff" ~config:corrected
    suite
  @@ fun outcome ->
  check "the test row counts as failed" (failed_paths outcome = [ "t1" ]);
  check "but the run exits 0" (outcome.Run.exit_code = 0);
  check "the .corrected is written beside the file"
    (Sys.file_exists (baseline root ^ ".corrected")
    && read_file (baseline root ^ ".corrected") = "hello\n");
  check "and the file itself is not" (not (Sys.file_exists (baseline root)));
  (match Baseline.writes (Run.baselines outcome.Run.run) with
  | [ Baseline.Written { path; literals = 0 } ] ->
      check "one write reported" (path = baseline root ^ ".corrected")
  | _ -> check "one write reported" false);
  (* Update mode: accepted silently, in place, green. *)
  let update = { base with Run.baseline = Baseline.Update } in
  expect_run "update mode accepts in place" ~config:update suite
  @@ fun outcome ->
  check "green" (outcome.Run.exit_code = 0 && failed_paths outcome = []);
  check "the file holds the content"
    (Sys.file_exists (baseline root) && read_file (baseline root) = "hello\n");
  check "the write is reported"
    (Baseline.writes (Run.baselines outcome.Run.run)
    = [ Baseline.Written { path = baseline root; literals = 0 } ]);
  expect_run "the accepted baseline matches from then on" ~config:base suite
  @@ fun outcome ->
  check "green" (outcome.Run.exit_code = 0 && failed_paths outcome = [])

(* Gating: a correction never blesses output produced beside another
   failure, an unresolvable path is not a correction, and a key checked
   with two contents in one run is a real failure. *)
let () =
  Fun.protect ~finally:clear_env @@ fun () ->
  clear_env ();
  with_temp_root @@ fun root ->
  Unix.putenv "WINDTRAP_PROJECT_ROOT" root;
  let corrected =
    {
      (base_config ~log_dir:(Filename.concat root "_logs") ()) with
      Run.baseline = Baseline.Corrected;
    }
  in
  let dirty =
    [
      Test_tree.test "dirty" (fun () ->
          Run.subtest "expectation" (fun () ->
              Windtrap.expect_file "x\n" "src/a.expected");
          Check.fail "boom");
    ]
  in
  expect_run "a correction beside another failure is dropped" ~config:corrected
    dirty
  @@ fun outcome ->
  check "the run fails" (outcome.Run.exit_code = 1);
  check "nothing is written"
    ((not (Sys.file_exists (Filename.concat root "src/a.expected.corrected")))
    && Baseline.writes (Run.baselines outcome.Run.run) = []);
  let escaping =
    [
      Test_tree.test "escapes" (fun () ->
          Windtrap.expect_file "x\n" "../outside.expected");
    ]
  in
  expect_run "an unresolvable baseline is not a correction" ~config:corrected
    escaping
  @@ fun outcome ->
  check "the run fails" (outcome.Run.exit_code = 1);
  let divergent =
    [
      Test_tree.test "a" (fun () ->
          Windtrap.expect_file "one\n" "src/d.expected");
      Test_tree.test "b" (fun () ->
          Windtrap.expect_file "two\n" "src/d.expected");
    ]
  in
  expect_run "two contents for one baseline" ~config:corrected divergent
  @@ fun outcome ->
  check "the first is a correction, the second a failure"
    (outcome.Run.exit_code = 1 && failed_paths outcome = [ "a"; "b" ]);
  check "the first content is what is written"
    (read_file (Filename.concat root "src/d.expected.corrected") = "one\n")

(* A kept correction ends a test's attempts whatever its retries: the next
   attempt's check would agree with the recorded text, and a deterministic
   test would pass as flaky, or fail beside a correction already kept.
   Where nothing is kept (plain checking, a correction dropped beside an
   assertion) the declared retries run. The literal's source is a real
   file under the root, so the corrections are written too. *)
let () =
  Fun.protect ~finally:clear_env @@ fun () ->
  clear_env ();
  with_temp_root @@ fun root ->
  Unix.putenv "WINDTRAP_PROJECT_ROOT" root;
  let base = base_config ~log_dir:(Filename.concat root "_logs") () in
  let corrected = { base with Run.baseline = Baseline.Corrected } in
  let update = { base with Run.baseline = Baseline.Update } in
  let source = Filename.concat root "t.ml" in
  let corrected_file = source ^ ".corrected" in
  let text literal =
    "let () =\n  expect \"new\" (__POS_OF__ {| " ^ literal ^ " |})\n"
  in
  let bodies = ref 0 in
  (* A fresh stale source, and a test whose attempt number [n] also fails
     an assertion when [also_fails n]. *)
  let suite ~retries ~also_fails =
    bodies := 0;
    if Sys.file_exists corrected_file then Sys.remove corrected_file;
    Out_channel.with_open_bin source (fun oc -> output_string oc (text "old"));
    [
      Test_tree.test ~retries "stale" (fun () ->
          incr bodies;
          Windtrap.expect "new" (("t.ml", 2, 15, 37), " old ");
          if also_fails !bodies then Check.fail "boom");
    ]
  in
  let never _ = false in
  let attempts outcome =
    match result_of outcome [ "stale" ] with
    | Some r -> r.Run.attempts
    | None -> 0
  in
  let only_the_mismatch outcome =
    match failure_list (outcome_of outcome [ "stale" ]) with
    | [ { Failure.kind = Failure.Baseline { withheld = None; _ }; _ } ] -> true
    | _ -> false
  in
  let writes outcome = Baseline.writes (Run.baselines outcome.Run.run) in
  (* Without retries is the reference: with them the row, the exit code and
     the write are the same. *)
  List.iter
    (fun retries ->
      let name = Printf.sprintf "corrected, ~retries:%d" retries in
      expect_run name ~config:corrected (suite ~retries ~also_fails:never)
      @@ fun outcome ->
      check_int (name ^ ": the body runs once") ~expected:1 ~actual:!bodies;
      check_int
        (name ^ ": one attempt is recorded, so the row is not flaky")
        ~expected:1 ~actual:(attempts outcome);
      check
        (name ^ ": the row is the stale literal's failure, acceptance offered")
        (only_the_mismatch outcome && failed_paths outcome = [ "stale" ]);
      check_int
        (name ^ ": the exit code is left to the diff")
        ~expected:0 ~actual:outcome.Run.exit_code;
      check
        (name ^ ": the correction is written once")
        (writes outcome
         = [ Baseline.Written { path = corrected_file; literals = 1 } ]
        && read_file corrected_file = text "new"
        && read_file source = text "old"))
    [ 0; 2 ];
  ( expect_run "update, ~retries:2" ~config:update
      (suite ~retries:2 ~also_fails:never)
  @@ fun outcome ->
    check_int "update: the body runs once" ~expected:1 ~actual:!bodies;
    check "update: one attempt, passed, green"
      (attempts outcome = 1
      && outcome_of outcome [ "stale" ] = Some Failure.Pass
      && outcome.Run.exit_code = 0);
    check "update: the literal is accepted in place, once"
      (writes outcome = [ Baseline.Written { path = source; literals = 1 } ]
      && read_file source = text "new") );
  ( expect_run "check, ~retries:2" ~config:base
      (suite ~retries:2 ~also_fails:never)
  @@ fun outcome ->
    check_int "check: nothing is recorded, so every declared attempt runs"
      ~expected:3 ~actual:!bodies;
    check "check: three attempts, failed, nothing written"
      (attempts outcome = 3
      && only_the_mismatch outcome && outcome.Run.exit_code = 1
      && writes outcome = []
      && read_file source = text "old") );
  ( expect_run "corrected, an assertion fails beside the literal twice"
      ~config:corrected
      (suite ~retries:2 ~also_fails:(fun n -> n < 3))
  @@ fun outcome ->
    check_int "a dropped correction does not end the attempts" ~expected:3
      ~actual:!bodies;
    check "the third attempt's correction is the one kept"
      (attempts outcome = 3
      && only_the_mismatch outcome && outcome.Run.exit_code = 0
      && writes outcome
         = [ Baseline.Written { path = corrected_file; literals = 1 } ]) );
  (* The attempt that would fail an assertion beside a correction already
     kept never runs: the test ends as it does without retries. *)
  expect_run "corrected, a second attempt would fail an assertion"
    ~config:corrected
    (suite ~retries:1 ~also_fails:(fun n -> n = 2))
  @@ fun outcome ->
  check_int "the attempt after a kept correction never runs" ~expected:1
    ~actual:!bodies;
  check "so no correction is written beside a failure outside the expectations"
    (attempts outcome = 1
    && only_the_mismatch outcome && outcome.Run.exit_code = 0
    && writes outcome
       = [ Baseline.Written { path = corrected_file; literals = 1 } ])

(* An expected failure is a failure: an xfail test's stale baseline is the
   mismatch the annotation expects, so its attempt checks read-only in
   every mode — reported, excused, never corrected, never accepted — and
   a test that skipped after a check records nothing either. *)
let () =
  Fun.protect ~finally:clear_env @@ fun () ->
  clear_env ();
  with_temp_root @@ fun root ->
  Unix.putenv "WINDTRAP_PROJECT_ROOT" root;
  let base = base_config ~log_dir:(Filename.concat root "_logs") () in
  let known =
    Test_tree.xfail ~reason:"issue #42"
      (Test_tree.test "known" (fun () ->
           Windtrap.expect_file "buggy\n" "src/known.expected"))
  in
  (* Reachable only where the check does not raise: under Update. *)
  let undecided =
    Test_tree.test "undecided" (fun () ->
        Windtrap.expect_file "partial\n" "src/undecided.expected";
        Check.skip ~reason:"not here" ())
  in
  let on_disk name =
    Sys.file_exists (Filename.concat root ("src/" ^ name ^ ".expected"))
    || Sys.file_exists
         (Filename.concat root ("src/" ^ name ^ ".expected.corrected"))
  in
  let excused outcome =
    outcome.Run.exit_code = 0
    && failed_paths outcome = []
    && Baseline.writes (Run.baselines outcome.Run.run) = []
    && not (on_disk "known")
  in
  let corrected = { base with Run.baseline = Baseline.Corrected } in
  expect_run "corrected: the xfail mismatch is excused, not corrected"
    ~config:corrected [ known ]
  @@ fun outcome ->
  check "green, nothing written" (excused outcome);
  check "the mismatch is recorded, excused"
    (match result_of outcome [ "known" ] with
    | Some
        {
          Run.outcome =
            Failure.Fail [ { Failure.kind = Failure.Baseline _; _ } ];
          counted = false;
          _;
        } ->
        true
    | _ -> false);
  let update = { base with Run.baseline = Baseline.Update } in
  expect_run "update: the xfail mismatch is excused, not accepted"
    ~config:update [ known; undecided ]
  @@ fun outcome ->
  check "green, nothing written" (excused outcome);
  check "the skipped test's correction is dropped"
    ((not (on_disk "undecided"))
    && List.exists
         (fun (r : Run.result) ->
           r.Run.path = [ "undecided" ]
           && match r.Run.outcome with Failure.Skip _ -> true | _ -> false)
         (Run.results outcome.Run.run))

(* A release failure beside a corrected test still fails the run, and the
   test's own correction is still written. *)
let () =
  Fun.protect ~finally:clear_env @@ fun () ->
  clear_env ();
  with_temp_root @@ fun root ->
  Unix.putenv "WINDTRAP_PROJECT_ROOT" root;
  let config =
    {
      (base_config ~log_dir:(Filename.concat root "_logs") ()) with
      Run.baseline = Baseline.Corrected;
    }
  in
  let fx =
    Run.fixture ~teardown:(fun _ -> Check.fail "release-boom") (fun () -> ())
  in
  let suite =
    [
      Test_tree.test "t1" (fun () ->
          Windtrap.expect_file "hello\n" "src/help.expected");
      Test_tree.test "t3" (fun () -> ignore (fx ()));
    ]
  in
  expect_run "release failure beside a correction" ~config suite
  @@ fun outcome ->
  check "the release failure still fails the run"
    (outcome.Run.exit_code = 1 && List.length outcome.Run.release_failures = 1);
  check "the correction is still written"
    (Sys.file_exists (baseline root ^ ".corrected"))

(* The last-failed store *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let rerun = { config with Run.failed_only = true } in
  let fixed = ref false in
  let tests =
    [
      Test_tree.test "steady" (fun () -> ());
      Test_tree.test "shaky" (fun () -> if not !fixed then Check.fail "boom");
    ]
  in
  expect_run "store round trip: first run" ~config tests @@ fun outcome ->
  check "first run fails" (outcome.Run.exit_code = 1);
  expect_run "--failed reruns the recorded failure" ~config:rerun tests
  @@ fun outcome ->
  check "--failed selects only the failure"
    (ran_names outcome = [ "shaky" ] && outcome.Run.exit_code = 1);
  fixed := true;
  expect_run "--failed clears on pass" ~config:rerun tests @@ fun outcome ->
  check "the fixed test passes"
    (ran_names outcome = [ "shaky" ] && outcome.Run.exit_code = 0);
  expect_startup_error "--failed with an empty store is refused" ~config:rerun
    tests (function
    | Run.No_recorded_failures -> true
    | _ -> false);
  check "the empty-store refusal exits 2"
    (Run.startup_exit_code Run.No_recorded_failures = 2)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  expect_startup_error "--failed with no store at all is refused"
    ~config:{ config with Run.failed_only = true }
    [ Test_tree.test "any" (fun () -> ()) ]
    (function Run.No_recorded_failures -> true | _ -> false)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let t1_fixed = ref false in
  let tests =
    [
      Test_tree.test "t1" (fun () -> if not !t1_fixed then Check.fail "boom");
      Test_tree.test "t2" (fun () -> Check.fail "boom");
    ]
  in
  expect_run "survivors: full failing run" ~config tests @@ fun _ ->
  t1_fixed := true;
  expect_run "survivors: filtered rerun of t1"
    ~config:{ config with Run.filter = [ "t1" ] }
    tests
  @@ fun outcome ->
  check "only t1 reran and passed"
    (ran_names outcome = [ "t1" ] && outcome.Run.exit_code = 0);
  expect_run "survivors: --failed keeps the unreached failure"
    ~config:{ config with Run.failed_only = true }
    tests
  @@ fun outcome ->
  check "t2's entry survived the filtered run" (ran_names outcome = [ "t2" ]);
  expect_run "--failed composes with a disjoint filter"
    ~config:{ config with Run.failed_only = true; filter = [ "t1" ] }
    tests
  @@ fun outcome ->
  check "a nonempty allowlist the filter rejects runs nothing, exit 2"
    (ran_names outcome = [] && outcome.Run.exit_code = 2)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  expect_run "dead entries: record a failure" ~config
    [ Test_tree.test "old-test" (fun () -> Check.fail "boom") ]
  @@ fun _ ->
  expect_run "dead entries: a full run of a new suite drops them" ~config
    [ Test_tree.test "new-test" (fun () -> ()) ]
  @@ fun outcome ->
  check "the replacement run is green" (outcome.Run.exit_code = 0);
  (* The old test declared again: had its entry survived, [--failed] would
     select it. *)
  expect_startup_error "dead entries were dropped, not merely unmatched"
    ~config:{ config with Run.failed_only = true }
    [ Test_tree.test "old-test" (fun () -> Check.fail "boom") ]
    (function Run.No_recorded_failures -> true | _ -> false)

(* The exit guard *)

(* The frozen interception text. *)
let exit_message =
  "the test called exit and was intercepted; a test must return or raise, \
   never exit the process"

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let after_ran = ref false in
  let tests =
    [
      Test_tree.test "before" (fun () -> ());
      Test_tree.test "bomb" (fun () -> Stdlib.exit 0);
      (* The code is unobservable: a guard that caught 0 alone fails here. *)
      Test_tree.test "bomb7" (fun () -> Stdlib.exit 7);
      Test_tree.test "after" (fun () ->
          after_ran := true;
          Check.fail "genuine");
    ]
  in
  expect_run "exit-guard suite runs" ~config tests @@ fun outcome ->
  check_int "exit in body is intercepted and every test still runs" ~expected:4
    ~actual:(List.length (Run.results outcome.Run.run));
  check "the test after the bomb executed" !after_ran;
  (match failure_list (outcome_of outcome [ "bomb" ]) with
  | [ f ] ->
      check "the interception is a Body-phase failure"
        (f.Failure.phase = Failure.Body);
      check_string "the interception message is frozen" ~expected:exit_message
        ~actual:(message_of f)
  | _ -> check "bomb: exactly one failure" false);
  (match failure_list (outcome_of outcome [ "bomb7" ]) with
  | [ f ] ->
      check_string "exit 7 intercepts identically" ~expected:exit_message
        ~actual:(message_of f)
  | _ -> check "bomb7: exactly one failure" false);
  check "the bombs and the genuine failure all counted"
    (failed_paths outcome = [ "bomb"; "bomb7"; "after" ]);
  check "the run exits through its own path with code 1"
    (outcome.Run.exit_code = 1);
  check "the slot is inactive after execute returns" (not (Run.active ()))

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [
      Test_tree.bracket
        ~setup:(fun () -> Stdlib.exit 0)
        ~teardown:(fun () -> ())
        "setup-bomb"
        (fun () -> ());
      Test_tree.bracket
        ~setup:(fun () -> ())
        ~teardown:(fun () -> Stdlib.exit 0)
        "teardown-bomb"
        (fun () -> ());
    ]
  in
  expect_run "exit-phase suite runs" ~config tests @@ fun outcome ->
  check "exit in setup is a Setup-phase failure"
    (phases_of (failure_list (outcome_of outcome [ "setup-bomb" ]))
    = [ Failure.Setup ]);
  check "exit in teardown is a Teardown-phase failure"
    (phases_of (failure_list (outcome_of outcome [ "teardown-bomb" ]))
    = [ Failure.Teardown ])

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  expect_run "exit-retry suite runs" ~config
    [ Test_tree.test ~retries:2 "retry-bomb" (fun () -> Stdlib.exit 0) ]
  @@ fun outcome ->
  match result_of outcome [ "retry-bomb" ] with
  | Some r ->
      check_int "exit is intercepted on every retry attempt" ~expected:3
        ~actual:r.Run.attempts
  | None -> check "retry-bomb recorded" false

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [
      Run.prop "law-bomb" (Gen.int_range 0 1000) (fun n ->
          if n > 10 then Stdlib.exit 0);
    ]
  in
  expect_run "exit-prop suite runs" ~config tests @@ fun outcome ->
  match result_of outcome [ "law-bomb" ] with
  | Some r ->
      check "exit in a property law is the intercepted exit, not a case"
        (match r.Run.outcome with
        | Failure.Fail
            [ { Failure.kind = Failure.Message { Failure.kept = text; _ }; _ } ]
          ->
            contains "the test called exit and was intercepted" text
        | _ -> false)
  | None -> check "law-bomb recorded" false

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let fx = Run.fixture ~teardown:(fun _ -> Stdlib.exit 0) (fun () -> ()) in
  expect_run "exit-release suite runs" ~config
    [ Test_tree.test "uses" (fun () -> ignore (fx ())) ]
  @@ fun outcome ->
  (match outcome.Run.release_failures with
  | [ f ] ->
      check "exit during fixture release is a Release-phase failure"
        (f.Failure.phase = Failure.Release);
      check "the release failure renders the registered printer"
        (contains
           "release raised Exit_attempt (code under test called exit; \
            intercepted by windtrap)"
           (message_of f))
  | _ -> check "exit-release: exactly one release failure" false);
  check "exit-release: run exit code is 1" (outcome.Run.exit_code = 1)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  expect_run "exit-xfail suite runs" ~config
    [
      Test_tree.xfail ~reason:"known bomb"
        (Test_tree.test "xbomb" (fun () -> Stdlib.exit 0));
    ]
  @@ fun outcome ->
  check "xfail excuses an exit-bombed test"
    (match outcome_of outcome [ "xbomb" ] with
    | Some (Failure.Fail _) -> true
    | _ -> false);
  check "the excused bomb is absent from failed_paths"
    (failed_paths outcome = []);
  check "the excused bomb leaves the run green" (outcome.Run.exit_code = 0)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let sibling_ran = ref false in
  let tests =
    [
      Test_tree.test "sub-bomb" (fun () ->
          Run.subtest "bomb" (fun () -> Stdlib.exit 0);
          Run.subtest "sibling" (fun () -> sibling_ran := true));
    ]
  in
  expect_run "exit-subtest suite runs" ~config tests @@ fun outcome ->
  check "an exit in a subtest ends the test" (not !sibling_ran);
  match failure_list (outcome_of outcome [ "sub-bomb" ]) with
  | [ f ] ->
      check "the failure is the test's interception, unlabelled"
        (f.Failure.subtest = []
        &&
        match f.Failure.kind with
        | Failure.Message { Failure.kept = text; _ } ->
            contains "the test called exit and was intercepted" text
        | _ -> false)
  | _ -> check "exit-subtest: exactly one failure" false

(* Location capture at the boundary (D4) *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let pos = ("test/fake_decl.ml", 21, 2, 30) in
  let given = ("test/fake_site.ml", 40, 4, 20) in
  let tests =
    [
      (* The check sits in tail position: its caller's frame is gone at
         raise time, so capture must stop at the runner's delimiter and the
         recording falls back to the declaration — never the line that
         called [execute]. *)
      Test_tree.test ~__POS__:pos "tail" (fun () ->
          Check.equal Testable.int 1 2);
      (* The same check, its own line handed over: [?__POS__] always wins. *)
      Test_tree.test ~__POS__:pos "given" (fun () ->
          Check.equal ~__POS__:given Testable.int 1 2);
      (* Not in tail position: the body's frame is live, so capture finds
         this file's line — no fallback, nothing to hint. *)
      Test_tree.test ~__POS__:pos "captured" (fun () ->
          Check.equal Testable.int 1 2;
          ());
      Test_tree.test ~__POS__:pos "raises" (fun () -> raise Boom);
    ]
  in
  expect_run "tail-loc suite runs" ~config tests @@ fun outcome ->
  (match failure_list (outcome_of outcome [ "tail" ]) with
  | [ f ] ->
      check "a tail-position check failure is attributed to the declaration"
        (f.Failure.loc = Some (Loc.of_pos pos))
  | _ -> check "tail-loc: exactly one failure" false);
  (match failure_list (outcome_of outcome [ "given" ]) with
  | [ f ] ->
      check "a given ?__POS__ is the location"
        (f.Failure.loc = Some (Loc.of_pos given))
  | _ -> check "given-loc: exactly one failure" false);
  (match failure_list (outcome_of outcome [ "captured" ]) with
  | [ f ] ->
      check "a captured location is this file's"
        (match f.Failure.loc with
        | Some l -> Filename.basename l.Loc.file = "test_run.ml"
        | None -> false)
  | _ -> check "captured-loc: exactly one failure" false);
  match failure_list (outcome_of outcome [ "raises" ]) with
  | [ f ] ->
      (* An uncaught exception names the declaration as its own site. *)
      check "an uncaught exception is located at the declaration"
        (f.Failure.loc = Some (Loc.of_pos pos))
  | _ -> check "raises-loc: exactly one failure" false

(* ------------------------------------------------------------------ *)
(* The rest of the contract, statement by statement *)
(* ------------------------------------------------------------------ *)

let gen = Windtrap.Gen.int_range 0 100

(* Standard output and standard error go to [file] for the extent of [fn]. *)
let with_output_to file fn =
  let flush_all () =
    Format.pp_print_flush Format.std_formatter ();
    Format.pp_print_flush Format.err_formatter ();
    flush stdout;
    flush stderr
  in
  flush_all ();
  let fd =
    Unix.openfile file [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
  in
  let saved_out = Unix.dup Unix.stdout and saved_err = Unix.dup Unix.stderr in
  Unix.dup2 fd Unix.stdout;
  Unix.dup2 fd Unix.stderr;
  Unix.close fd;
  Fun.protect
    ~finally:(fun () ->
      flush_all ();
      Unix.dup2 saved_out Unix.stdout;
      Unix.dup2 saved_err Unix.stderr;
      Unix.close saved_out;
      Unix.close saved_err)
    fn

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:(Filename.concat root "logs") () in
  let tests =
    [
      Test_tree.test "passes" ignore;
      Test_tree.test "fails" (fun () -> Check.fail "x");
      Test_tree.test "skips" (fun () -> Check.skip ());
    ]
  in
  let printed = Filename.concat root "printed" in
  with_output_to printed (fun () ->
      ignore (Run.execute config ~suite:"quiet" tests));
  check_string "execute prints nothing" ~expected:"" ~actual:(read_file printed)

let () =
  let a = Run.default_config () and b = Run.default_config () in
  check "default config: colour auto, one second, the mirrors' spelling"
    (a.Run.color = Os.Auto && a.Run.slow_threshold = 1.
    && a.Run.invocation = `Mirrors
    && (not a.Run.verbose) && not a.Run.github);
  check_string "default config: the log dir is Os.default_log_dir ()"
    ~expected:(Os.default_log_dir ()) ~actual:a.Run.log_dir;
  check "default config: every call draws a seed" (a.Run.seed <> b.Run.seed)

let () =
  let parent =
    {
      (Run.default_config ()) with
      Run.timeout = Some 3.;
      prop_count = Some 7;
      color = Os.Never;
      verbose = true;
      slow_threshold = 2.;
      github = true;
      invocation = `Exe "suite.exe";
    }
  in
  let child = Run.for_subset parent ~log_dir:"/tmp/child" ~bail:false in
  check "for_subset: every other field is the parent's"
    (child.Run.timeout = Some 3.
    && child.Run.prop_count = Some 7
    && child.Run.color = Os.Never && child.Run.verbose
    && child.Run.slow_threshold = 2.
    && child.Run.github
    && child.Run.invocation = `Exe "suite.exe")

(* The fields only a report reads change nothing a run decides. *)
let () =
  with_temp_root @@ fun root ->
  let tests =
    [
      Test_tree.test "passes" ignore;
      Test_tree.test "fails" (fun () -> Check.fail "x");
      Test_tree.test "skips" (fun () -> Check.skip ());
      Test_tree.xfail (Test_tree.test "expected" (fun () -> Check.fail "y"));
      Test_tree.slow "slow" ignore;
    ]
  in
  let decided config =
    match Run.execute config ~suite:"s" tests with
    | Error _ -> None
    | Ok o ->
        Some
          ( o.Run.exit_code,
            List.map
              (fun (r : Run.result) ->
                ( r.Run.path,
                  r.Run.counted,
                  r.Run.attempts,
                  match r.Run.outcome with
                  | Failure.Pass -> "pass"
                  | Failure.Fail _ -> "fail"
                  | Failure.Skip _ -> "skip" ))
              (Run.results o.Run.run) )
  in
  let plain = base_config ~log_dir:(Filename.concat root "a") () in
  let dressed =
    {
      (base_config ~log_dir:(Filename.concat root "b") ()) with
      Run.color = Os.Always;
      slow_threshold = 0.;
      verbose = true;
      junit = Some (Filename.concat root "junit.xml");
      github = true;
      invocation = `Exe "suite.exe";
    }
  in
  check "colour, threshold, -v, JUnit, GitHub and invocation decide nothing"
    (decided plain <> None && decided plain = decided dressed)

(* A timeout is about the test that was running, never about what it was
   doing: a fixture that times out while acquiring caches nothing, and a
   finally that the timeout cut is a timeout of the teardown, not a
   [Fun.Finally_raised]. *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let acquisitions = ref 0 in
  let slow_first =
    Run.fixture (fun () ->
        incr acquisitions;
        if !acquisitions = 1 then busy_forever ();
        "acquired")
  in
  let tests =
    [
      Test_tree.test ~timeout:0.02 "acquires past its limit" (fun () ->
          ignore (slow_first ()));
      Test_tree.test "acquires again" (fun () ->
          Check.equal Testable.string "acquired" (slow_first ()));
      Test_tree.scoped
        (fun k -> Fun.protect ~finally:busy_forever (fun () -> k ()))
        ~timeout:0.02 "a finally cut by the timeout"
        (fun () -> ());
    ]
  in
  expect_run "timeout control suite runs" ~config tests @@ fun outcome ->
  check "a timeout while acquiring is not cached"
    (outcome_of outcome [ "acquires again" ] = Some Failure.Pass);
  check_int "the later call acquires again" ~expected:2 ~actual:!acquisitions;
  match
    failure_list (outcome_of outcome [ "a finally cut by the timeout" ])
  with
  | [ f ] ->
      check "a finally cut by the timeout is a teardown timeout"
        (f.Failure.phase = Failure.Teardown
        && contains "timed out after" (message_of f))
  | _ -> check "one failure for the cut finally" false

(* A stack overflow is the failure of the recursion that raised it: the
   test fails and the run goes on. *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [
      Test_tree.test "overflows" (fun () -> raise Stack_overflow);
      Test_tree.test "runs after" ignore;
    ]
  in
  expect_run "stack overflow suite runs" ~config tests @@ fun outcome ->
  (match failure_list (outcome_of outcome [ "overflows" ]) with
  | [
   {
     Failure.kind =
       Failure.Raise { actual = Some { Failure.kept = actual; _ }; _ };
     _;
   };
  ] ->
      check "a stack overflow is an uncaught exception of its test"
        (actual = "Stack overflow")
  | _ -> check "one failure for the overflow" false);
  check "the next test runs"
    (outcome_of outcome [ "runs after" ] = Some Failure.Pass)

(* subtest: what passes through, what it labels, where *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [
      Test_tree.test ~timeout:0.02 "times out" (fun () ->
          Run.subtest "spins" busy_forever);
      Run.prop ~count:5 "discards in a subtest" gen (fun _ ->
          Run.subtest "assumes" (fun () -> Windtrap.assume false));
    ]
  in
  expect_run "subtest control suite runs" ~config tests @@ fun outcome ->
  (match failure_list (outcome_of outcome [ "times out" ]) with
  | [ f ] ->
      check "a timeout passes through subtest, unlabelled"
        (f.Failure.subtest = [] && contains "timed out" (message_of f))
  | _ -> check "one timeout failure" false);
  match failure_list (outcome_of outcome [ "discards in a subtest" ]) with
  | [ f ] ->
      check "an assume inside a subtest inside a law discards the case"
        (f.Failure.subtest = [] && contains "property gave up" (message_of f))
  | _ -> check "the property gave up" false

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [
      Test_tree.bracket
        ~setup:(fun () -> Run.subtest "in setup" (fun () -> Check.fail "s"))
        ~teardown:(fun () ->
          Run.subtest "in teardown" (fun () -> Check.fail "t"))
        "phases" ignore;
    ]
  in
  expect_run "subtest phase suite runs" ~config tests @@ fun outcome ->
  let fs = failure_list (outcome_of outcome [ "phases" ]) in
  check_int "two subtest failures" ~expected:2 ~actual:(List.length fs);
  check "a subtest failure stays Body, in a setup and a teardown"
    (List.for_all (fun f -> f.Failure.phase = Failure.Body) fs)

(* check_baseline *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let subject =
    Baseline.Literal { pos = ("test/x.ml", 3, 0, 0); value = "a"; exact = true }
  in
  let given = { Loc.file = "test/x.ml"; line = 3; column = 0 } in
  let pos = __POS__ in
  let tests =
    [
      Test_tree.test ~__POS__:pos "t" (fun () ->
          Run.check_baseline ~loc:given subject "b";
          Run.check_baseline subject "b";
          Run.subtest "s" (fun () -> Run.check_baseline ~loc:given subject "b"));
    ]
  in
  expect_run "check_baseline location suite runs" ~config tests
  @@ fun outcome ->
  match failure_list (outcome_of outcome [ "t" ]) with
  | [ located; unlocated; labelled ] ->
      check "check_baseline's loc is the failure's"
        (located.Failure.loc = Some given);
      check "without one, the declaration" (at_pos pos unlocated.Failure.loc);
      check "inside a subtest, labelled as a subtest labels one"
        (labelled.Failure.subtest = [ "t"; "s" ])
  | _ -> check "three recorded mismatches" false

let () =
  (* The registry resolves a file against the directory the run started
     in, under the project root: both are a throwaway root here. *)
  Fun.protect ~finally:clear_env @@ fun () ->
  clear_env ();
  with_temp_root @@ fun root ->
  Unix.mkdir (Filename.concat root "dir.expected") 0o700;
  Unix.putenv "WINDTRAP_PROJECT_ROOT" root;
  let config = base_config ~log_dir:(Filename.concat root "_logs") () in
  let raised = ref false in
  let tests =
    [
      Test_tree.test "t" (fun () ->
          match Run.check_baseline (Baseline.File "dir.expected") "x" with
          | () -> ()
          | exception Sys_error _ -> raised := true);
    ]
  in
  let home = Sys.getcwd () in
  Unix.chdir root;
  Fun.protect ~finally:(fun () -> Unix.chdir home) @@ fun () ->
  expect_run "check_baseline Sys_error suite runs" ~config tests @@ fun _ ->
  check "check_baseline raises Sys_error on a file it cannot read" !raised

(* Temporary paths, remove_tree and chdir *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let saved = Filename.get_temp_dir_name () in
  let unmade = ref false and name = ref "" in
  let tests =
    [
      Test_tree.test "unmade" (fun () ->
          Fun.protect
            ~finally:(fun () -> Filename.set_temp_dir_name saved)
            (fun () ->
              Filename.set_temp_dir_name "/nonexistent/windtrap-temp";
              unmade :=
                match Run.temp_dir () with
                | _ -> false
                | exception Unix.Unix_error _ -> true));
      Test_tree.test "unsuffixed" (fun () ->
          name := Filename.basename (Run.temp_file ()));
    ]
  in
  expect_run "temporary path suite runs" ~config tests @@ fun _ ->
  check "temp_dir raises Unix_error when no directory can be made" !unmade;
  let digits s = s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s in
  check "an empty suffix adds nothing to the name"
    (String.starts_with ~prefix:"file-" !name
    && digits (String.sub !name 5 (String.length !name - 5)))

let () =
  if Sys.win32 then skip_scenario ~reason:"POSIX only" __POS__
  else
    with_temp_root @@ fun root ->
    let kept = Filename.concat root "kept" in
    Unix.mkdir kept 0o700;
    close_out (open_out (Filename.concat kept "file"));
    let tree = Filename.concat root "tree" in
    Unix.mkdir tree 0o700;
    Unix.symlink kept (Filename.concat tree "link");
    Run.remove_tree tree;
    check "remove_tree removes a link, never what it points to"
      ((not (Sys.file_exists tree))
      && Sys.file_exists (Filename.concat kept "file"));
    Run.remove_tree (Filename.concat root "missing");
    check "a missing path, and no running test, raise nothing" true;
    if Unix.geteuid () <> 0 then begin
      let locked = Filename.concat root "locked" in
      Unix.mkdir locked 0o700;
      close_out (open_out (Filename.concat locked "stuck"));
      Unix.chmod locked 0o500;
      Run.remove_tree locked;
      Unix.chmod locked 0o700;
      check "an error on the way is ignored"
        (Sys.file_exists (Filename.concat locked "stuck"))
    end

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let home = Sys.getcwd () in
  let unenterable = ref false and unreadable_cwd = ref None in
  let tests =
    [
      Test_tree.test "unenterable" (fun () ->
          unenterable :=
            match Run.chdir "/nonexistent/windtrap-dir" with
            | () -> false
            | exception Unix.Unix_error _ -> true);
      Test_tree.test "unreadable cwd" (fun () ->
          if not Sys.win32 then begin
            (* Leave the process in a directory that no longer exists,
               without telling the runner. *)
            let gone = Filename.concat root "gone" in
            Unix.mkdir gone 0o700;
            Unix.chdir gone;
            Unix.rmdir gone;
            let unreadable =
              match Sys.getcwd () with
              | _ -> false
              | exception Sys_error _ -> true
            in
            let raised =
              match Run.chdir root with
              | () -> false
              | exception Sys_error _ -> true
            in
            Unix.chdir home;
            unreadable_cwd := Some (unreadable, raised)
          end);
    ]
  in
  expect_run "chdir error suite runs" ~config tests @@ fun _ ->
  check "chdir raises Unix_error for a directory it cannot enter" !unenterable;
  match !unreadable_cwd with
  | None -> skip_scenario ~reason:"POSIX only" __POS__
  | Some (true, raised) ->
      check "the first chdir raises Sys_error when the cwd cannot be read"
        raised
  | Some (false, _) -> skip_scenario ~reason:"a removed cwd reads here" __POS__

(* Fixtures and results *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let fx, line = (Run.fixture ~teardown:ignore ignore, __LINE__) in
  let names = ref [] in
  let on_event = function
    | Run.Fixture_release { name } -> names := name :: !names
    | Run.Run_started _ | Run.Test_started _ | Run.Test_finished _
    | Run.Interrupted _ ->
        ()
  in
  expect_run "fixture naming suite runs" ~on_event ~config
    [ Test_tree.test "acquires" fx ]
  @@ fun _ ->
  check_string "a fixture is named after the site of its application"
    ~expected:(Printf.sprintf "fixture (test/unit/test_run.ml:%d)" line)
    ~actual:(String.concat "," !names)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [
      Test_tree.slow "own" ignore;
      Test_tree.group ~tags:[ Test_tree.Tag.slow ] "g"
        [ Test_tree.test "inherited" ignore ];
      Test_tree.test "plain" ignore;
    ]
  in
  expect_run "slow-tag suite runs" ~config tests @@ fun outcome ->
  let tagged path =
    match result_of outcome path with
    | Some r -> r.Run.slow_tagged
    | None -> false
  in
  check "slow_tagged: the test's own tag, a group's, and not otherwise"
    (tagged [ "own" ] && tagged [ "g"; "inherited" ] && not (tagged [ "plain" ]))

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let attempts = ref 0 in
  let tests =
    [
      Test_tree.test ~retries:2 "twice failed" (fun () ->
          incr attempts;
          Unix.sleepf 0.02;
          if !attempts < 3 then Check.fail "not yet");
    ]
  in
  expect_run "duration suite runs" ~config tests @@ fun outcome ->
  (match result_of outcome [ "twice failed" ] with
  | Some r ->
      check_int "three attempts" ~expected:3 ~actual:r.Run.attempts;
      check "the duration sums them" (r.Run.duration >= 0.06)
  | None -> check "a row" false);
  check "the run's duration covers its tests" (outcome.Run.duration >= 0.06)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [
      Run.prop "skips" gen (fun _ -> Check.skip ());
      Run.prop ~timeout:0.02 "times out" gen (fun _ -> busy_forever ());
    ]
  in
  expect_run "prop_stats suite runs" ~config tests @@ fun outcome ->
  let stats path =
    match result_of outcome path with
    | Some r -> r.Run.prop_stats
    | None -> None
  in
  check "a property ended by a skip has no stats" (stats [ "skips" ] = None);
  check "one ended by a timeout has the stats of the cases before it"
    (Option.map (fun (s : Property.stats) -> s.cases) (stats [ "times out" ])
    = Some 0);
  check "the law's skip skips the test"
    (outcome_of outcome [ "skips" ] = Some (Failure.Skip None))

let () =
  match Test_tree.flatten [ Run.prop "p" gen ignore ] with
  | [ c ] ->
      check "Run.prop adds no tag"
        (not (Test_tree.Tag.mem Test_tree.Tag.prop c.Test_tree.tags))
  | _ -> check "one case" false

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let pos = ("test/test_props.ml", 12, 2, 0) in
  let tests =
    [
      Run.prop ~__POS__:pos "gives up" gen (fun _ -> Windtrap.assume false);
      Run.prop ~__POS__:pos "misses a label" gen (fun _ ->
          Windtrap.cover "never" false);
      Run.prop ~count:(-1) "negative count" gen ignore;
      Run.prop ~max_discard:(-1) "negative max_discard" gen ignore;
    ]
  in
  expect_run "property outcomes suite runs" ~config tests @@ fun outcome ->
  let at_declaration path =
    match failure_list (outcome_of outcome path) with
    | [ f ] -> f.Failure.loc = Some (Loc.of_pos pos)
    | _ -> false
  in
  check "a property that gave up is located at its declaration"
    (at_declaration [ "gives up" ]);
  check "one that missed a label too" (at_declaration [ "misses a label" ]);
  check "a negative count fails the test from inside its body"
    (failed_paths outcome
    = [ "gives up"; "misses a label"; "negative count"; "negative max_discard" ]
    )

(* Startup errors *)

let () =
  let tests =
    List.map (fun n -> Test_tree.test n ignore) [ "b"; "b"; "b"; "a"; "a" ]
  in
  expect_startup_error "duplicates are sorted and given once"
    ~config:(base_config ~log_dir:"/tmp/unused" ()) tests (function
    | Run.Duplicate_paths [ "a"; "b" ] -> true
    | _ -> false)

let () =
  Fun.protect ~finally:clear_env @@ fun () ->
  clear_env ();
  Unix.putenv "CI" "true";
  let site n = ("test/f.ml", n, 0, 0) in
  let tests =
    [
      Test_tree.focus
        (Test_tree.group ~__POS__:(site 1) "g"
           [ Test_tree.focus (Test_tree.test ~__POS__:(site 2) "x" ignore) ]);
      Test_tree.test "y" ignore;
      Test_tree.focus (Test_tree.test ~__POS__:(site 3) "z" ignore);
    ]
  in
  let config =
    { (base_config ~log_dir:"/tmp/unused" ()) with Run.filter = [ "y" ] }
  in
  expect_startup_error "the focus sites of the declared tree, in order" ~config
    tests (function
    | Run.Focused_in_ci sites ->
        List.map (Option.map (fun (l : Loc.t) -> l.Loc.line)) sites
        = [ Some 1; Some 2; Some 3 ]
    | _ -> false)

let () =
  Fun.protect ~finally:clear_env @@ fun () ->
  clear_env ();
  Unix.putenv "CI" "true";
  let focused = Test_tree.focus (Test_tree.test "f" ignore) in
  let config = base_config ~log_dir:"/tmp/unused" () in
  expect_startup_error "duplicates are checked before focus" ~config
    [ focused; Test_tree.test "f" ignore ]
    (function Run.Duplicate_paths _ -> true | _ -> false);
  expect_startup_error "focus before -u under CI"
    ~config:{ config with Run.baseline = Baseline.Update } [ focused ] (function
    | Run.Focused_in_ci _ -> true
    | _ -> false);
  expect_startup_error "-u under CI before --failed"
    ~config:{ config with Run.baseline = Baseline.Update; failed_only = true }
    [ Test_tree.test "t" ignore ]
    (function Run.Update_refused_in_ci -> true | _ -> false)

(* The outcome *)

let () =
  Fun.protect ~finally:clear_env @@ fun () ->
  clear_env ();
  with_temp_root @@ fun root ->
  Unix.putenv "WINDTRAP_PROJECT_ROOT" root;
  close_out (open_out (Filename.concat root "blocker"));
  let config =
    {
      (base_config ~log_dir:(Filename.concat root "_logs") ()) with
      Run.baseline = Baseline.Update;
    }
  in
  expect_run "unwritable correction suite runs" ~config
    [
      Test_tree.test "accepts" (fun () ->
          Run.check_baseline (Baseline.File "blocker/x.expected") "v");
    ]
  @@ fun outcome ->
  check "every test passed" (failed_paths outcome = []);
  check_int "a correction that could not be written exits 1" ~expected:1
    ~actual:outcome.Run.exit_code

let () =
  Fun.protect ~finally:clear_env @@ fun () ->
  clear_env ();
  with_temp_root @@ fun root ->
  Unix.putenv "WINDTRAP_PROJECT_ROOT" root;
  let config =
    {
      (base_config ~log_dir:(Filename.concat root "_logs") ()) with
      Run.baseline = Baseline.Update;
    }
  in
  expect_run "CI set by a test suite runs" ~config
    [
      Test_tree.test "sets CI" (fun () ->
          Unix.putenv "CI" "true";
          Run.check_baseline (Baseline.File "late.expected") "v");
    ]
  @@ fun outcome ->
  check "CI is read once, at startup: the correction is written"
    (outcome.Run.exit_code = 0
    && Sys.file_exists (Filename.concat root "late.expected")
    && read_file (Filename.concat root "late.expected") = "v\n")

(* What an exception out of execute leaves behind *)

let () =
  Fun.protect ~finally:clear_env @@ fun () ->
  clear_env ();
  with_temp_root @@ fun root ->
  Unix.putenv "WINDTRAP_PROJECT_ROOT" root;
  let config =
    {
      (base_config ~log_dir:(Filename.concat root "_logs") ()) with
      Run.baseline = Baseline.Corrected;
    }
  in
  let released = ref false in
  let fx = Run.fixture ~teardown:(fun () -> released := true) ignore in
  let tests =
    [
      Test_tree.test "corrects" (fun () ->
          fx ();
          Run.check_baseline (Baseline.File "c.expected") "v");
    ]
  in
  let store = Filename.concat root "_logs/suite/.last-failed" in
  let corrected = Filename.concat root "c.expected.corrected" in
  (match
     Run.execute
       ~on_event:(function Run.Test_finished _ -> raise Boom | _ -> ())
       config ~suite:"suite" tests
   with
  | _ -> check "an observer's exception leaves execute" false
  | exception Boom -> ());
  check "after an observer's exception: fixtures released first" !released;
  check "no store, no correction"
    ((not (Sys.file_exists store)) && not (Sys.file_exists corrected));
  released := false;
  let second = ref false in
  let fy = Run.fixture ~teardown:(fun () -> second := true) ignore in
  (match
     Run.execute
       ~on_event:(function Run.Fixture_release _ -> raise Boom | _ -> ())
       config ~suite:"suite"
       [
         Test_tree.test "acquires" (fun () ->
             fx ();
             fy ());
       ]
   with
  | _ -> check "an exception on a release event leaves execute" false
  | exception Boom -> ());
  check "the announced fixture and the rest stay unreleased"
    ((not !released) && not !second)

let () =
  Fun.protect ~finally:clear_env @@ fun () ->
  clear_env ();
  with_temp_root @@ fun root ->
  Unix.putenv "WINDTRAP_PROJECT_ROOT" root;
  let config =
    {
      (base_config ~log_dir:(Filename.concat root "_logs") ()) with
      Run.baseline = Baseline.Corrected;
    }
  in
  let released = ref false in
  let fx = Run.fixture ~teardown:(fun () -> released := true) ignore in
  let tests =
    [
      Test_tree.test "corrects" (fun () ->
          fx ();
          Run.check_baseline (Baseline.File "c.expected") "v");
      Test_tree.test "runs out of memory" (fun () -> raise Out_of_memory);
    ]
  in
  (match Run.execute config ~suite:"suite" tests with
  | _ -> check "a fatal exception leaves execute" false
  | exception Out_of_memory -> ());
  check "after a fatal exception: fixtures released" !released;
  check "no store, no correction"
    ((not (Sys.file_exists (Filename.concat root "_logs/suite/.last-failed")))
    && not (Sys.file_exists (Filename.concat root "c.expected.corrected")))

let () =
  with_temp_root @@ fun root ->
  let config =
    {
      (base_config ~log_dir:(Filename.concat root "_logs") ()) with
      Run.stream = false;
    }
  in
  let called = ref false in
  let observer = function
    | Run.Test_finished _ -> exit 3
    | Run.Run_started _ | Run.Test_started _ | Run.Fixture_release _
    | Run.Interrupted _ ->
        called := true
  in
  match
    Run.execute ~on_event:observer config ~suite:"suite"
      [ Test_tree.test "t" ignore ]
  with
  | _ -> check "exit in an observer leaves execute" false
  | exception Failure.Control `Exit ->
      check "exit in an observer is that observer's exception" !called

(* list_selection *)

let () =
  with_temp_root @@ fun root ->
  let logs = Filename.concat root "_logs" in
  let config = base_config ~log_dir:logs () in
  (match
     Run.list_selection config ~suite:"suite" [ Test_tree.test "t" ignore ]
   with
  | Ok [ "t" ] -> ()
  | _ -> check "the selection" false);
  check "list_selection makes no log directory" (not (Sys.file_exists logs));
  let store = Filename.concat logs "suite/.last-failed" in
  Os.mkdir_p (Filename.dirname store);
  let before = "windtrap-last-failed 1\nt\n" in
  Out_channel.with_open_bin store (fun oc -> output_string oc before);
  ignore
    (Run.list_selection
       { config with Run.failed_only = true }
       ~suite:"suite"
       [ Test_tree.test "t" ignore ]);
  check_string "and rewrites no store" ~expected:before
    ~actual:(read_file store);
  match
    Run.list_selection config ~suite:"suite"
      [ Test_tree.test "d" ignore; Test_tree.test "d" ignore ]
  with
  | Error (Run.Duplicate_paths _) -> ()
  | _ -> check "list_selection refuses what execute refuses" false

(* Attempts *)

let () =
  with_temp_root @@ fun root ->
  let file = Filename.concat root "a file" in
  close_out (open_out file);
  let config = base_config ~log_dir:file () in
  expect_run "capture setup failure suite runs" ~config
    [ Test_tree.test "first" ignore; Test_tree.test "second" ignore ]
  @@ fun outcome ->
  (match failure_list (outcome_of outcome [ "first" ]) with
  | [ { Failure.kind = Failure.Raise _; _ } ] -> ()
  | _ -> check "a capture that cannot be set up is a Raise failure" false);
  check "and the run goes on to the next test"
    (result_of outcome [ "second" ] <> None)

let () =
  if Sys.win32 then skip_scenario ~reason:"POSIX only" __POS__
  else
    with_temp_root @@ fun root ->
    let config = base_config ~log_dir:root () in
    let mine (_ : int) = () in
    let before = Sys.signal Sys.sigalrm (Sys.Signal_handle mine) in
    expect_run "SIGALRM suite runs" ~config
      [ Test_tree.test ~timeout:5. "limited" ignore ]
    @@ fun _ ->
    check "the previous SIGALRM handler is put back"
      (match Sys.signal Sys.sigalrm before with
      | Sys.Signal_handle f -> f == mine
      | Sys.Signal_default | Sys.Signal_ignore -> false)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let pos = ("test/test_decl.ml", 7, 0, 0) in
  let tests =
    [
      Test_tree.bracket ~setup:ignore
        ~teardown:(fun () -> Check.skip ~reason:"second" ())
        "skips twice"
        (fun () -> Check.skip ~reason:"first" ());
      Test_tree.test ~__POS__:pos ~timeout:0.02 "times out" busy_forever;
      Test_tree.scoped ~__POS__:pos
        (fun k ->
          k ();
          k ())
        "calls back twice" ignore;
    ]
  in
  expect_run "runner-made failures suite runs" ~config tests @@ fun outcome ->
  check "the first skip reason wins"
    (outcome_of outcome [ "skips twice" ] = Some (Failure.Skip (Some "first")));
  let located path =
    match failure_list (outcome_of outcome path) with
    | [ f ] -> f.Failure.loc = Some (Loc.of_pos pos)
    | _ -> false
  in
  check "a timeout is located at the declaration" (located [ "times out" ]);
  (match failure_list (outcome_of outcome [ "times out" ]) with
  | [ f ] ->
      check_string "and reads timed out after <limit>s"
        ~expected:"timed out after 0.02s" ~actual:(message_of f)
  | _ -> ());
  check "a misused scope too" (located [ "calls back twice" ])

(* Corrections: the marks, and when they are written *)

let () =
  Fun.protect ~finally:clear_env @@ fun () ->
  clear_env ();
  with_temp_root @@ fun root ->
  Unix.putenv "WINDTRAP_PROJECT_ROOT" root;
  let config =
    {
      (base_config ~log_dir:(Filename.concat root "_logs") ()) with
      Run.baseline = Baseline.Corrected;
    }
  in
  let written_at_release = ref None in
  let fx =
    Run.fixture
      ~teardown:(fun () ->
        written_at_release :=
          Some
            (Sys.file_exists (Filename.concat root "kept.expected.corrected")))
      ignore
  in
  let tests =
    [
      Test_tree.test "skipped" (fun () ->
          Run.check_baseline (Baseline.File "s.expected") "v";
          Check.skip ());
      Test_tree.test "failed outside" (fun () ->
          Run.check_baseline (Baseline.File "f.expected") "v";
          Check.fail "other");
      Test_tree.test "kept" (fun () ->
          fx ();
          Run.check_baseline (Baseline.File "kept.expected") "v");
    ]
  in
  expect_run "withheld marks suite runs" ~config tests @@ fun outcome ->
  let mark path =
    List.find_map
      (fun (f : Failure.t) ->
        match f.Failure.kind with
        | Failure.Baseline { withheld; _ } -> Some withheld
        | _ -> None)
      (failure_list (outcome_of outcome path))
  in
  check "a skip alone marks Skipped"
    (mark [ "skipped" ] = Some (Some Failure.Skipped));
  check "another failure marks Failed_outside"
    (mark [ "failed outside" ] = Some (Some Failure.Failed_outside));
  check "the corrections are written after the release"
    (!written_at_release = Some false
    && Sys.file_exists (Filename.concat root "kept.expected.corrected"))

(* The store's file and its reading *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  expect_run "store path suite runs" ~config ~suite:"lib/a.ml"
    [ Test_tree.test "t" (fun () -> Check.fail "x") ]
  @@ fun _ ->
  check "the store is <log_dir>/<sanitized suite>/.last-failed"
    (Sys.file_exists
       (Filename.concat
          (Filename.concat root (Os.sanitize_component "lib/a.ml"))
          ".last-failed"))

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let store = Filename.concat root "suite/.last-failed" in
  Os.mkdir_p (Filename.dirname store);
  Out_channel.with_open_bin store (fun oc -> output_string oc "t\n");
  expect_startup_error "a store without its first line reads as empty"
    ~config:{ config with Run.failed_only = true }
    [ Test_tree.test "t" ignore ]
    (function Run.No_recorded_failures -> true | _ -> false);
  Sys.remove store;
  Unix.mkdir store 0o700;
  expect_run "a store that cannot be written is ignored" ~config
    [ Test_tree.test "t" (fun () -> Check.fail "x") ]
  @@ fun outcome ->
  check_int "the run ends as it would" ~expected:1 ~actual:outcome.Run.exit_code

(* Summary *)

let () = finish ()
