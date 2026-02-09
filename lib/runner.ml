(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ───── Types ───── *)

type result = { passed : int; failed : int; skipped : int }

let empty_result = { passed = 0; failed = 0; skipped = 0 }

let combine a b =
  {
    passed = a.passed + b.passed;
    failed = a.failed + b.failed;
    skipped = a.skipped + b.skipped;
  }

(* ───── Path Utilities ───── *)

let build_path segments = String.concat " › " (List.rev segments)

let collect_paths test =
  Test.fold
    (fun (segments, paths) visit ->
      match visit with
      | Test.Case { name; _ } ->
          (segments, build_path (name :: segments) :: paths)
      | Test.Enter_group { name; _ } -> (name :: segments, paths)
      | Test.Leave_group _ -> (
          match segments with [] -> ([], paths) | _ :: rest -> (rest, paths)))
    ([], []) test
  |> snd

let find_duplicates paths =
  let seen = Hashtbl.create (List.length paths) in
  List.filter
    (fun path ->
      if Hashtbl.mem seen path then true
      else (
        Hashtbl.add seen path ();
        false))
    paths

let list_tests _root_name tests =
  let paths = List.concat_map collect_paths tests in
  List.iter (Pp.pr "%s@.") (List.sort String.compare paths)

let check_duplicates _root_name tests =
  match
    List.concat_map collect_paths tests
    |> find_duplicates
    |> List.sort_uniq String.compare
  with
  | [] -> ()
  | dups ->
      List.iter
        (Pp.epr "%a Duplicate test name: %s@." (Pp.styled `Red Pp.string)
           "[ERROR]")
        dups;
      Pp.epr "@.Cannot run tests with duplicate names.@.";
      exit 1

(* ───── Test Execution ───── *)

let with_timeout timeout_opt fn =
  match timeout_opt with
  | None -> fn ()
  | Some timeout ->
      if Sys.win32 then
        (* SIGALRM not available on Windows, run without timeout *)
        fn ()
      else
        let timeout_secs = max 1 (int_of_float (Float.ceil timeout)) in
        let old_handler =
          Sys.signal Sys.sigalrm
            (Sys.Signal_handle (fun _ -> raise (Failure.Timeout timeout)))
        in
        let old_alarm = Unix.alarm timeout_secs in
        Fun.protect
          ~finally:(fun () ->
            ignore (Unix.alarm old_alarm);
            Sys.set_signal Sys.sigalrm old_handler)
          fn

(* Fixed seed so every test sees the same Random state, making failures
   reproducible without requiring the caller to pass a seed. *)
let deterministic_seed = 137

let with_isolated_random_state fn =
  let saved = Random.get_state () in
  Random.init deterministic_seed;
  Fun.protect ~finally:(fun () -> Random.set_state saved) fn

type attempt_result =
  | Attempt_pass of float
  | Attempt_skip of string option
  | Attempt_fail of float * Failure.t
  | Attempt_fatal of exn

let run_single_attempt ~log_trap ~groups ~test_pos ~timeout ~test_name fn =
  let start = Clock.counter () in
  Log_trap.reset_for_test log_trap;
  Expect.set_trap log_trap;
  let wrapped_fn () =
    with_isolated_random_state (fun () ->
        Fun.protect
          ~finally:(fun () -> Expect.clear_trap ())
          (fun () -> Log_trap.with_capture log_trap ~groups ~test_name fn))
  in
  match with_timeout timeout wrapped_fn with
  | () -> Attempt_pass (Clock.count_s start)
  | exception Failure.Skip_test reason -> Attempt_skip reason
  | exception Failure.Timeout t ->
      let time = Clock.count_s start in
      let failure =
        Failure.make ?pos:test_pos (Pp.str "Test timed out after %.1fs" t)
      in
      Attempt_fail (time, failure)
  | exception Failure.Check_failure failure ->
      Attempt_fail (Clock.count_s start, failure)
  | exception ((Sys.Break | Out_of_memory | Stack_overflow) as e) ->
      Attempt_fatal e
  | exception exn ->
      let bt = Printexc.get_raw_backtrace () in
      let time = Clock.count_s start in
      let bt_str = Printexc.raw_backtrace_to_string bt in
      let msg =
        if String.length bt_str > 0 then
          Pp.str "Unexpected exception: %s@.@.Backtrace:@.%s"
            (Printexc.to_string exn) bt_str
        else Pp.str "Unexpected exception: %s" (Printexc.to_string exn)
      in
      Attempt_fail (time, Failure.make ?pos:test_pos msg)

let run_single_test ~progress ~log_trap ~groups ~test_pos ~timeout ~retries
    ~test_name path fn =
  Progress.report_start progress ~path ~groups;
  let output_file =
    Some (Log_trap.test_output_path log_trap ~groups ~test_name)
  in
  let rec attempt remaining attempt_num =
    match
      run_single_attempt ~log_trap ~groups ~test_pos ~timeout ~test_name fn
    with
    | Attempt_pass time ->
        Progress.report_result progress ~path ~groups
          (Pass { time; attempts = attempt_num });
        { passed = 1; failed = 0; skipped = 0 }
    | Attempt_skip reason ->
        Progress.report_result progress ~path ~groups (Skip { reason });
        { passed = 0; failed = 0; skipped = 1 }
    | Attempt_fail (time, failure) when remaining > 0 ->
        attempt (remaining - 1) (attempt_num + 1)
    | Attempt_fail (time, failure) ->
        Progress.report_result progress ~path ~groups
          (Fail { time; failure; output_file; attempts = attempt_num });
        { passed = 0; failed = 1; skipped = 0 }
    | Attempt_fatal exn -> raise exn
  in
  attempt retries 1

(* ───── Tree Traversal ───── *)

type hooks = {
  before_each : (unit -> unit) option;
  after_each : (unit -> unit) option;
}

type run_state = {
  path_segments : string list;
  tag_stack : Tag.t list;
  focus_stack : bool list;
  hooks_stack : hooks list;
  result : result;
  failure_count : int;
}

(* Merge inherited group tags with the test's own tags. Tags from outer groups
   are applied first so inner groups and the test itself can override. *)
let effective_tags tag_stack tags =
  let ordered = List.rev tag_stack @ [ tags ] in
  List.fold_left Tag.merge Tag.empty ordered

let run_test ~progress ~log_trap ~filter ~bail ~default_timeout ~focus_active
    test =
  let initial =
    {
      path_segments = [];
      tag_stack = [];
      focus_stack = [];
      hooks_stack = [];
      result = empty_result;
      failure_count = 0;
    }
  in
  let bailed state =
    match bail with Some n -> state.failure_count >= n | None -> false
  in
  let in_focused_scope state = List.exists Fun.id state.focus_stack in
  let final_state =
    Test.fold
      (fun state visit ->
        if bailed state then state
        else
          match visit with
          | Test.Case { name; fn; tags; pos; timeout; retries; focused } ->
              let path = build_path (name :: state.path_segments) in
              let groups = List.rev state.path_segments in
              let eff_tags = effective_tags state.tag_stack tags in
              let timeout =
                match timeout with Some _ -> timeout | None -> default_timeout
              in
              let focus_skip =
                focus_active && (not focused) && not (in_focused_scope state)
              in
              let with_hooks fn =
                let run_before_hooks () =
                  List.iter
                    (fun h -> Option.iter (fun f -> f ()) h.before_each)
                    (List.rev state.hooks_stack)
                in
                let run_after_hooks () =
                  List.iter
                    (fun h -> Option.iter (fun f -> f ()) h.after_each)
                    state.hooks_stack
                in
                fun () ->
                  run_before_hooks ();
                  Fun.protect ~finally:run_after_hooks fn
              in
              let test_result =
                if focus_skip then begin
                  Progress.report_result progress ~path ~groups
                    (Skip { reason = None });
                  { passed = 0; failed = 0; skipped = 1 }
                end
                else
                  match filter ~path eff_tags with
                  | `Run ->
                      run_single_test ~progress ~log_trap ~groups ~test_pos:pos
                        ~timeout ~retries ~test_name:name path (with_hooks fn)
                  | `Skip ->
                      Progress.report_result progress ~path ~groups
                        (Skip { reason = None });
                      { passed = 0; failed = 0; skipped = 1 }
              in
              {
                state with
                result = combine state.result test_result;
                failure_count = state.failure_count + test_result.failed;
              }
          | Test.Enter_group
              { name; tags; setup; before_each; after_each; focused; _ } ->
              Option.iter (fun f -> f ()) setup;
              {
                state with
                path_segments = name :: state.path_segments;
                tag_stack = tags :: state.tag_stack;
                focus_stack = focused :: state.focus_stack;
                hooks_stack = { before_each; after_each } :: state.hooks_stack;
              }
          | Test.Leave_group { teardown; _ } -> (
              Option.iter (fun f -> f ()) teardown;
              match
                ( state.path_segments,
                  state.tag_stack,
                  state.focus_stack,
                  state.hooks_stack )
              with
              | [], _, _, _ | _, [], _, _ | _, _, [], _ | _, _, _, [] -> state
              | _ :: segs, _ :: tags, _ :: focus, _ :: hooks ->
                  {
                    state with
                    path_segments = segs;
                    tag_stack = tags;
                    focus_stack = focus;
                    hooks_stack = hooks;
                  }))
      initial test
  in
  (final_state.result, bailed final_state)

let run_tests ~progress ~log_trap ~filter ~bail ~default_timeout ~focus_active
    tests =
  let rec loop acc = function
    | [] -> acc
    | test :: rest ->
        let result, stopped =
          run_test ~progress ~log_trap ~filter ~bail ~default_timeout
            ~focus_active test
        in
        let acc = combine acc result in
        if stopped then acc else loop acc rest
  in
  loop empty_result tests

(* ───── Configuration ───── *)

type filter = path:string -> Tag.t -> [ `Run | `Skip ]

type config = {
  filter : filter;
  progress_mode : Progress.mode;
  log_dir : string;
  capture : bool;
  snapshot_config : Snapshot.Config.t;
  bail : int option;
  junit_file : string option;
  seed : int option;
  default_timeout : float option;
  prop_count : int option;
}

let default_config () =
  {
    filter = (fun ~path:_ _ -> `Run);
    progress_mode = Verbose;
    log_dir = Path_ops.default_log_dir ();
    capture = true;
    snapshot_config = Snapshot.Config.default ();
    bail = None;
    junit_file = None;
    seed = None;
    default_timeout = None;
    prop_count = None;
  }

(* ───── Filter Construction ───── *)

let make_filter ~quick ~filter_pattern ~exclude_pattern ~required_tags
    ~dropped_tags =
  let speed_filter t =
    if quick then
      match Tag.get_speed t with Tag.Quick -> `Run | Tag.Slow -> `Skip
    else `Run
  in
  let predicate =
    let p = Tag.initial_predicate in
    let p =
      List.fold_left (fun p tag -> Tag.require_tag tag p) p required_tags
    in
    List.fold_left (fun p tag -> Tag.drop_tag tag p) p dropped_tags
  in
  let predicate_filter = Tag.filter_predicate predicate in
  let pattern_filter ~path =
    match filter_pattern with
    | None -> `Run
    | Some pat ->
        if Text.contains_substring ~pattern:pat path then `Run else `Skip
  in
  let exclude_filter ~path =
    match exclude_pattern with
    | None -> `Run
    | Some pat ->
        if Text.contains_substring ~pattern:pat path then `Skip else `Run
  in
  fun ~path t ->
    match
      ( speed_filter t,
        predicate_filter t,
        pattern_filter ~path,
        exclude_filter ~path )
    with
    | `Run, `Run, `Run, `Run -> `Run
    | _ -> `Skip

(* ───── Last Failed Persistence ───── *)

let last_failed_path log_dir = Filename.concat log_dir ".last-failed"

let write_last_failed log_dir paths =
  let file = last_failed_path log_dir in
  match paths with
  | [] -> ( try Sys.remove file with Sys_error _ -> ())
  | _ ->
      let oc = open_out file in
      Fun.protect
        ~finally:(fun () -> close_out oc)
        (fun () -> List.iter (fun p -> output_string oc (p ^ "\n")) paths)

let read_last_failed log_dir =
  let file = last_failed_path log_dir in
  if Sys.file_exists file then
    let ic = open_in file in
    Fun.protect
      ~finally:(fun () -> close_in ic)
      (fun () ->
        let lines = ref [] in
        (try
           while true do
             lines := input_line ic :: !lines
           done
         with End_of_file -> ());
        List.rev !lines |> List.filter (fun s -> String.length s > 0))
  else []

(* ───── Entry Point ───── *)

let count_tests tests =
  List.concat_map collect_paths tests |> List.length

let run ?(config = default_config ()) root_name tests =
  check_duplicates root_name tests;
  Snapshot.set_config config.snapshot_config;
  Windtrap_prop.Prop.set_default_seed config.seed;
  Windtrap_prop.Prop.set_default_count config.prop_count;
  let coverage_dir =
    Filename.concat (Filename.dirname config.log_dir) "_coverage"
  in
  Windtrap_coverage.set_output_prefix (Filename.concat coverage_dir "windtrap");
  let focus_active = Test.has_focused tests in
  let run_id = Run_id.generate () in
  let log_trap =
    Log_trap.create ~root:config.log_dir ~suite_name:root_name ~run_id
      ~enabled:config.capture
  in
  let total_tests = count_tests tests in
  let progress = Progress.create ~mode:config.progress_mode ~total_tests in
  Option.iter (Progress.set_junit_file progress) config.junit_file;
  Progress.print_header progress ~name:root_name ~run_id;
  let result =
    run_tests ~progress ~log_trap ~filter:config.filter ~bail:config.bail
      ~default_timeout:config.default_timeout ~focus_active tests
  in
  Progress.finish progress;
  Progress.print_summary progress ~passed:result.passed ~failed:result.failed
    ~skipped:result.skipped
    ~time:(Progress.total_time progress)
    ();
  (match config.progress_mode with
  | Progress.Tap | Progress.Junit -> ()
  | Progress.Verbose | Progress.Compact ->
      Log_trap.setup_symlinks log_trap;
      Log_trap.print_location log_trap);
  write_last_failed config.log_dir (Progress.failed_paths progress);
  if focus_active then
    Pp.epr
      "@.%a Focused tests detected — some tests were skipped. Remove \
       ftest/fgroup before committing.@."
      (Pp.styled `Yellow Pp.string)
      "[WARNING]";
  result
