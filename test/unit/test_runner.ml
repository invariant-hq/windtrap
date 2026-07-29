(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Runner: the per-test boundary matrix (body x teardown x
   timeout), classification, retries, capture-tail attachment, the global
   Random reseed, selection (filters, tags, quick, focus, sharding, the CI
   guards), exit codes including the all-skipped ratification, expected
   failures (xfail), subtest and scratch-path cleanup through the boundary,
   fixture release under bail and on release failure (acquisition skips
   included), events, the property wiring, snapshot guard/prune plumbing,
   and the last-failed store round trip. Plain executable: [execute]
   refuses to nest inside an active run, so this suite cannot host its
   own assertions under the windtrap runner. *)

open Windtrap
open Windtrap.Private
open Harness

let () = init "runner"

exception Boom

let with_temp_root f = with_temp_root ~prefix:"windtrap-runner-" f

let base_config ~log_dir () =
  { (Run.default_config ()) with Run.seed = 0x5eedL; log_dir }

let expect_run name ?on_event ~config ?(suite = "suite") tests f =
  match Runner.execute ?on_event ~config ~suite tests with
  | Ok outcome -> f outcome
  | Error error ->
      check name false;
      Printf.printf "  startup error: %s\n%!" (Runner.startup_message error)

let expect_startup_error name ~config ?(suite = "suite") tests pred =
  match Runner.execute ~config ~suite tests with
  | Ok _ -> check (name ^ " (run was not refused)") false
  | Error error -> check name (pred error)

let result_of outcome path =
  List.find_opt (fun r -> r.Run.path = path) (Run.results outcome.Runner.run)

let outcome_of outcome path =
  match result_of outcome path with
  | Some r -> Some r.Run.outcome
  | None -> None

let failure_list = function
  | Some (Failure.Fail fs) -> fs
  | Some Failure.Pass | Some (Failure.Skip _) | None -> []

let phases_of fs = List.map (fun f -> f.Failure.phase) fs

let message_of (f : Failure.t) =
  match f.Failure.kind with Failure.Message m -> m | _ -> "<not a message>"

let busy_forever () =
  let n = ref 1 in
  while !n > 0 do
    incr n;
    if !n > 1_000_000 then n := 1
  done

(* ───── Boundary matrix: body x teardown ───── *)

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
   | [ { Failure.kind = Failure.Raise { actual = Some actual; _ }; _ } ] ->
       check "uncaught exception is a Raise failure naming it"
         (contains "Boom" actual)
   | _ -> check "uncaught exception is a Raise failure" false);
  check "a failing suite exits 1" (outcome.Runner.exit_code = 1)

(* ───── Timeouts (Unix only) ───── *)

let () =
  if not Sys.win32 then (
    with_temp_root @@ fun root ->
    let config = base_config ~log_dir:root () in
    let td_after_timeout = ref false in
    let tests =
      [
        Test_tree.test ~timeout:0.2 "body-times-out" busy_forever;
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
           (contains "timed out" (message_of f))
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
    check "body timeout: only the body entry" (phases_of fs = [ Failure.Body ]))

let () =
  if not Sys.win32 then
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
  if not Sys.win32 then (
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
    check "setup timeout: neither body nor teardown ran" (!ran = []))

let () =
  (* Fixture releases run outside per-test timeouts: a release slower than
     the tightest test timeout must complete untimed (RFC "Resources"). *)
  if not Sys.win32 then (
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
      (!release_done && outcome.Runner.release_failures = []);
    check "the run stayed green" (outcome.Runner.exit_code = 0))

let () =
  (* D2: a timeout expiring during the shrink search ends the search at the
     last accepted node and reports the counterexample in hand, marked —
     the budget bounds wall time without erasing what the engine found. *)
  if not Sys.win32 then (
    with_temp_root @@ fun root ->
    let config =
      { (base_config ~log_dir:root ()) with Run.timeout = Some 0.2 }
    in
    let tests =
      [
        (* Failing above a threshold keeps the descent walking the halving
           chain (the dest-first candidate passes and is rejected), so the
           search is still alive when the alarm fires. *)
        Runner.prop "slow-shrink" (Gen.int_range 0 1000) (fun n ->
            Unix.sleepf 0.04;
            Check.is_true (n < 1));
      ]
    in
    let started = Unix.gettimeofday () in
    expect_run "mid-shrink timeout suite runs" ~config tests @@ fun outcome ->
    let wall = Unix.gettimeofday () -. started in
    (match failure_list (outcome_of outcome [ "slow-shrink" ]) with
    | [ { Failure.kind = Failure.Property { timed_out; _ }; _ } ] ->
        check "prop timeout mid-shrink reports the marked counterexample"
          (timed_out = Some 0.2)
    | _ -> check "mid-shrink timeout: one Property failure" false);
    check "the whole-test budget bounds the wall time" (wall < 0.8);
    check "a timed-out-mid-shrink prop is an ordinary failed test"
      (outcome.Runner.exit_code = 1))

let () =
  if not Sys.win32 then
    with_temp_root @@ fun root ->
    let config =
      { (base_config ~log_dir:root ()) with Run.timeout = Some 0.2 }
    in
    let tests =
      [ Runner.prop "slow-pass" Gen.int (fun _ -> Unix.sleepf 0.05) ]
    in
    expect_run "pre-failure timeout suite runs" ~config tests @@ fun outcome ->
    match failure_list (outcome_of outcome [ "slow-pass" ]) with
    | [ f ] ->
        check "prop timeout before any failure is a plain timeout"
          (contains "timed out" (message_of f))
    | _ -> check "pre-failure timeout: one failure" false

(* The per-test budget is declarable at the site — [prop ~timeout] and
   [cases ~timeout]/[~retries] — not only through the global [--timeout]:
   both routes converge on the same Test_tree fields, so the memo semantics
   pinned above (pre-failure timeout, mid-shrink marking) hold unchanged. *)
let () =
  if not Sys.win32 then (
    with_temp_root @@ fun root ->
    let config = base_config ~log_dir:root () in
    let attempts = ref 0 in
    let tests =
      [
        Runner.prop ~timeout:0.2 "budgeted-prop" Gen.int (fun _ ->
            Unix.sleepf 0.05);
        Test_tree.cases ~timeout:0.2 "table"
          ~name:(function `Slow -> "slow" | `Fast -> "fast")
          [ `Slow; `Fast ]
          (fun input -> if input = `Slow then busy_forever ());
        Test_tree.cases ~retries:2 "flaky-table" [ () ] (fun () ->
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
    match result_of outcome [ "flaky-table"; "flaky-table.0" ] with
    | Some r ->
        check "cases ~retries gives each child the extra attempts"
          (r.Run.outcome = Failure.Pass && r.Run.attempts = 3)
    | None -> check "flaky-table.0 recorded" false)

(* ───── Retries ───── *)

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

(* ───── Capture wiring: tails, per-attempt truncation, stream ───── *)

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

(* ───── Global Random reseed ───── *)

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

(* ───── Selection ───── *)

let ran_names outcome =
  List.map
    (fun r -> String.concat "/" r.Run.path)
    (Run.results outcome.Runner.run)

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
  expect_run "filter selects a subtree"
    ~config:(config (Some "math") None)
    suite
  @@ fun outcome ->
  check "only matching tests ran"
    (ran_names outcome = [ "math/add"; "math/sub" ]);
  check_int "selected mirrors the run" ~expected:2
    ~actual:(List.length outcome.Runner.selected);
  check_int "total counts the whole suite" ~expected:3
    ~actual:outcome.Runner.total;
  expect_run "exclude drops matches" ~config:(config None (Some "math")) suite
  @@ fun outcome ->
  check "only non-excluded tests ran" (ran_names outcome = [ "text/trim" ]);
  expect_run "filter and exclude compose"
    ~config:(config (Some "math") (Some "sub"))
    suite
  @@ fun outcome -> check "compose" (ran_names outcome = [ "math/add" ])

let () =
  with_temp_root @@ fun root ->
  let suite =
    [
      Test_tree.test "plain" (fun () -> ());
      Test_tree.test ~tags:[ "db" ] "tagged" (fun () -> ());
      Test_tree.test ~tags:[ Tag.disabled ] "off" (fun () -> ());
      Test_tree.slow "molasses" (fun () -> ());
    ]
  in
  let config ?(tags = []) ?(exclude_tags = []) ?(quick = false) () =
    { (base_config ~log_dir:root ()) with Run.tags; exclude_tags; quick }
  in
  expect_run "default tag predicate" ~config:(config ()) suite @@ fun outcome ->
  check "disabled is dropped by default, slow runs"
    (ran_names outcome = [ "plain"; "tagged"; "molasses" ]);
  expect_run "--tag requires" ~config:(config ~tags:[ "db" ] ()) suite
  @@ fun outcome ->
  check "--tag" (ran_names outcome = [ "tagged" ]);
  expect_run "--exclude-tag drops"
    ~config:(config ~exclude_tags:[ "db" ] ())
    suite
  @@ fun outcome ->
  check "--exclude-tag" (ran_names outcome = [ "plain"; "molasses" ]);
  expect_run "-q drops slow" ~config:(config ~quick:true ()) suite
  @@ fun outcome ->
  check "-q" (ran_names outcome = [ "plain"; "tagged" ]);
  expect_run "--tag disabled re-enables"
    ~config:(config ~tags:[ Tag.disabled ] ())
    suite
  @@ fun outcome -> check "--tag disabled" (ran_names outcome = [ "off" ])

let () =
  clear_env ();
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite =
    [
      Test_tree.test "unfocused" (fun () -> ());
      Test_tree.ftest "starred" (fun () -> ());
    ]
  in
  expect_run "focus narrows outside CI" ~config suite @@ fun outcome ->
  check "only the focused test ran" (ran_names outcome = [ "starred" ]);
  check "focus_active is reported" outcome.Runner.focus_active;
  check "a focused run of passing tests exits 0" (outcome.Runner.exit_code = 0)

(* ───── The CI focus guard ───── *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite = [ Test_tree.ftest "starred" (fun () -> ()) ] in
  Unix.putenv "CI" "true";
  expect_startup_error "focused tests are refused under CI" ~config suite
    (function
    | Runner.Focused_in_ci [ (`Ftest, _) ] -> true
    | _ -> false);
  (match Runner.execute ~config ~suite:"suite" suite with
  | Error error ->
      check "the focus refusal exits 1" (Runner.startup_exit_code error = 1);
      check "the focus message names the override"
        (contains "WINDTRAP_ALLOW_FOCUS" (Runner.startup_message error))
  | Ok _ -> check "focus refusal expected" false);
  let config = { config with Run.allow_focus = true } in
  expect_run "WINDTRAP_ALLOW_FOCUS lifts the guard" ~config suite
  @@ fun outcome ->
  check "focused test ran" (ran_names outcome = [ "starred" ]);
  clear_env ()

let () =
  (* CI falsy spellings do not arm the guard (Env: set-and-not-falsy). *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite = [ Test_tree.ftest "starred" (fun () -> ()) ] in
  List.iter
    (fun value ->
      Unix.putenv "CI" value;
      expect_run
        (Printf.sprintf "CI=%S does not trigger the focus guard" value)
        ~config suite
      @@ fun outcome ->
      check "focused test ran" (ran_names outcome = [ "starred" ]))
    [ ""; "false"; "0" ];
  clear_env ()

(* ───── Exit codes ───── *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  expect_run "all-pass exits 0" ~config [ Test_tree.test "ok" (fun () -> ()) ]
  @@ fun outcome ->
  check "exit 0" (outcome.Runner.exit_code = 0);
  expect_run "any failure exits 1" ~config
    [
      Test_tree.test "ok" (fun () -> ());
      Test_tree.test "bad" (fun () -> Check.fail "boom");
    ]
  @@ fun outcome ->
  check "exit 1" (outcome.Runner.exit_code = 1);
  expect_run "a filter matching nothing exits 2"
    ~config:{ config with Run.filter = Some "zzz-nothing" }
    [ Test_tree.test "ok" (fun () -> ()) ]
  @@ fun outcome ->
  check "exit 2, nothing recorded"
    (outcome.Runner.exit_code = 2 && Run.results outcome.Runner.run = []);
  expect_run "an empty suite exits 2" ~config [] @@ fun outcome ->
  check "empty suite" (outcome.Runner.exit_code = 2);
  expect_run "a nonempty selection of skips exits 0" ~config
    [
      Test_tree.test "skip-a" (fun () -> Check.skip ());
      Test_tree.test "skip-b" (fun () -> Check.skip ~reason:"no net" ());
    ]
  @@ fun outcome ->
  check "all-skipped ratification" (outcome.Runner.exit_code = 0)

(* ───── Expected failures (amendment B12) ───── *)

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
  check "an expected failure does not fail the run"
    (outcome.Runner.exit_code = 0);
  check "an expected failure is not a failed path"
    (outcome.Runner.failed_paths = []);
  check "the flattened case carries the annotation for renderers"
    (match
       List.find_opt
         (fun (c : Test_tree.case) -> c.Test_tree.path = [ "known-bug" ])
         outcome.Runner.selected
     with
    | Some c -> c.Test_tree.xfail = Some { Test_tree.reason = Some "issue #42" }
    | None -> false)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [
      Test_tree.xfail ~reason:"issue #42"
        (Test_tree.test "fixed" (fun () -> ()));
    ]
  in
  expect_run "xpass suite runs" ~config tests @@ fun outcome ->
  (match failure_list (outcome_of outcome [ "fixed" ]) with
  | [ f ] ->
      check "an unexpected pass fails loudly, naming the reason"
        (contains "expected to fail" (message_of f)
        && contains "issue #42" (message_of f))
  | _ -> check "unexpected pass records one message failure" false);
  check "an unexpected pass fails the run"
    (outcome.Runner.exit_code = 1 && outcome.Runner.failed_paths = [ "fixed" ])

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [
      Test_tree.xfail (Test_tree.test "undecided" (fun () -> Check.skip ()));
      Test_tree.xfail
        (Runner.prop "known-bad-law" Gen.int (fun n ->
             Check.is_true (n = n + 1)));
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
    (outcome.Runner.exit_code = 0 && outcome.Runner.failed_paths = [])

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
    (outcome.Runner.failed_paths = [ "xf-clean" ]
    && outcome.Runner.exit_code = 1)

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
    (outcome.Runner.failed_paths = [ "keeps-passing" ])

let () =
  (* --bail counts effective failures: expected ones do not consume the
     budget. *)
  with_temp_root @@ fun root ->
  let config = { (base_config ~log_dir:root ()) with Run.bail = Some 1 } in
  let tests =
    [
      Test_tree.xfail (Test_tree.test "excused" (fun () -> Check.fail "known"));
      Test_tree.test "ok" (fun () -> ());
      Test_tree.test "boom" (fun () -> Check.fail "real");
      Test_tree.test "after" (fun () -> ());
    ]
  in
  expect_run "xfail-bail suite runs" ~config tests @@ fun outcome ->
  check "an expected failure does not consume the bail budget"
    (ran_names outcome = [ "excused"; "ok"; "boom" ] && outcome.Runner.bailed)

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
  check "only the real failure counted"
    (outcome.Runner.failed_paths = [ "real" ]);
  expect_run "--failed skips expected failures" ~config:rerun tests
  @@ fun outcome ->
  check "--failed reruns only the real failure" (ran_names outcome = [ "real" ])

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let rerun = { config with Run.failed_only = true } in
  let tests = [ Test_tree.xfail (Test_tree.test "xp" (fun () -> ())) ] in
  expect_run "xpass-store: first run" ~config tests @@ fun outcome ->
  check "the unexpected pass failed the run" (outcome.Runner.exit_code = 1);
  expect_run "--failed reruns an unexpected pass" ~config:rerun tests
  @@ fun outcome -> check "xp reran" (ran_names outcome = [ "xp" ])

(* ───── Sharding (amendment B14) ───── *)

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
  (* The mapping itself is frozen (amendment B14: stable across machines and
     windtrap versions): this golden assignment may only change by amending
     the RFC. *)
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
      filter = Some "t-one";
    }
  in
  let outcomes =
    List.map
      (fun k ->
        let seen = ref ([], -1) in
        expect_run (Printf.sprintf "filtered shard %d/3 runs" k)
          ~config:(config k) (shard_suite ()) (fun outcome ->
            seen := (ran_names outcome, outcome.Runner.exit_code));
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
      Test_tree.ftest "starred" (fun () -> ());
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
          (fun outcome -> seen := (ran_names outcome, outcome.Runner.exit_code));
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
    Runner.execute ~config ~suite:"suite" [ Test_tree.test "t" (fun () -> ()) ]
  with
  | exception Invalid_argument _ ->
      check "a malformed hand-built shard fails loudly" true
  | Ok _ | Error _ -> check "a malformed hand-built shard fails loudly" false

(* ───── Test-body operations through the boundary (B9, B10, B13) ───── *)

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
    (outcome.Runner.exit_code = 1
    && outcome.Runner.failed_paths = [ "failing-scratch" ])

let () =
  (* Scratch removal on the boundary's worst paths (amendment B9, Law 8):
     scratch created in the body and in a raising teardown of the very test
     that trips --bail is still removed. *)
  with_temp_root @@ fun root ->
  let config = { (base_config ~log_dir:root ()) with Run.bail = Some 1 } in
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
    (ran_names outcome = [ "teardown-scratch" ] && outcome.Runner.bailed);
  check_int "scratch was created in body and teardown alike" ~expected:2
    ~actual:(List.length !scratch);
  check "scratch is removed when the teardown raises, under --bail too"
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

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let draw = ref 0 in
  let stable =
    Test_tree.test "stable" (fun () ->
        draw := Random.State.bits (Run.srandom ()))
  in
  expect_run "srandom run A" ~config [ stable ] @@ fun _ ->
  let first = !draw in
  expect_run "srandom run B (recomposed suite)" ~config
    [ Test_tree.test "other" (fun () -> ()); stable ]
  @@ fun _ ->
  check "srandom is stable across suite composition" (!draw = first);
  let reseeded = { config with Run.seed = 0xdeadL } in
  expect_run "srandom run C (new root)" ~config:reseeded [ stable ] @@ fun _ ->
  check "srandom re-keys with the root seed" (!draw <> first)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let sibling = ref false in
  let tests =
    [
      Test_tree.test "layouts" (fun () ->
          Run.subtest "row-major" (fun () -> Check.fail "bad shape");
          Run.subtest "col-major" (fun () -> ());
          Run.subtest "strided" (fun () -> Check.fail "bad stride");
          sibling := true);
    ]
  in
  expect_run "subtest suite runs" ~config tests @@ fun outcome ->
  check "subtest siblings continue inside the runner" !sibling;
  (match failure_list (outcome_of outcome [ "layouts" ]) with
  | [ a; b ] ->
      check "each failing subtest is one labeled entry, in order"
        (a.Failure.msg = Some "layouts › row-major"
        && b.Failure.msg = Some "layouts › strided")
  | _ -> check "two subtest failures recorded" false);
  check "subtest failures fail the test"
    (outcome.Runner.exit_code = 1 && outcome.Runner.failed_paths = [ "layouts" ])

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
        (sub.Failure.msg = Some "bracketed › uses-resource"
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
      Runner.prop ~count:5 "prop-sub" Gen.int (fun _ ->
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
                 f.Failure.msg = Some "prop-sub › law-half"
                 &&
                 match f.Failure.kind with
                 | Failure.Property _ -> false
                 | _ -> true)
               entries))
  | None -> check "prop-sub recorded" false

(* ───── Fixture acquisition skips (amendment C1) ───── *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let acquisitions = ref 0 in
  let device =
    Run.fixture
      ~teardown:(fun _ -> check "a skipped fixture must never release" false)
      (fun () ->
        incr acquisitions;
        Check.skip ~reason:"no metal device" ())
  in
  let announced = ref false in
  let on_event = function
    | Runner.Fixture_release _ -> announced := true
    | _ -> ()
  in
  let tests =
    [
      Test_tree.test "first-gpu" (fun () -> ignore (device ()));
      Test_tree.test "second-gpu" (fun () -> ignore (device ()));
    ]
  in
  expect_run "fixture-skip suite runs" ~on_event ~config tests @@ fun outcome ->
  check "the acquiring test skips with the fixture's reason"
    (outcome_of outcome [ "first-gpu" ]
    = Some (Failure.Skip (Some "no metal device")));
  check "later users skip with the same cached reason"
    (outcome_of outcome [ "second-gpu" ]
    = Some (Failure.Skip (Some "no metal device")));
  check_int "acquisition was attempted once" ~expected:1 ~actual:!acquisitions;
  check "a skipped fixture is never announced for release" (not !announced);
  check "an unavailable optional resource does not turn the run red"
    (outcome.Runner.exit_code = 0 && outcome.Runner.release_failures = [])

(* ───── Duplicate paths ───── *)

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
    | Runner.Duplicate_paths [ path ] -> contains "same" path
    | _ -> false);
  match Runner.execute ~config ~suite:"suite" suite with
  | Error error ->
      check "duplicates exit 1" (Runner.startup_exit_code error = 1);
      check "the message lists the path"
        (contains "same" (Runner.startup_message error))
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
    | Runner.Duplicate_paths [ path ] -> contains "1" path
    | _ -> false)

(* ───── Fixtures: release order, bail, release failures ───── *)

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
  check "no release failures" (outcome.Runner.release_failures = []);
  let events = List.rev !events in
  let is_finish = function Runner.Test_finished _ -> true | _ -> false in
  let is_release = function Runner.Fixture_release _ -> true | _ -> false in
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
  let config = { (base_config ~log_dir:root ()) with Run.bail = Some 1 } in
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
  check "bail stops after the limit"
    (ran_names outcome = [ "first-fails" ] && outcome.Runner.bailed);
  check "fixtures release under bail" !released;
  check "bailed failing run exits 1" (outcome.Runner.exit_code = 1)

let () =
  with_temp_root @@ fun root ->
  let config = { (base_config ~log_dir:root ()) with Run.bail = Some 2 } in
  let boom name = Test_tree.test name (fun () -> Check.fail "boom") in
  let tests =
    [ boom "f1"; Test_tree.test "ok" (fun () -> ()); boom "f2"; boom "f3" ]
  in
  expect_run "--bail 2 suite runs" ~config tests @@ fun outcome ->
  check "--bail 2 stops after the second failure, passes in between kept"
    (ran_names outcome = [ "f1"; "ok"; "f2" ] && outcome.Runner.bailed)

let () =
  (* A raising [on_event] observer aborts the run, but acquired fixtures
     still release (RFC Law 8). *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let released = ref false in
  let fx = Run.fixture ~teardown:(fun _ -> released := true) (fun () -> ()) in
  let on_event = function
    | Runner.Test_started { path = [ "second" ] } -> raise Boom
    | _ -> ()
  in
  let tests =
    [
      Test_tree.test "first" (fun () -> ignore (fx ()));
      Test_tree.test "second" (fun () -> ());
    ]
  in
  (match Runner.execute ~on_event ~config ~suite:"suite" tests with
  | exception Boom -> check "the observer's exception aborts the run" true
  | Ok _ | Error _ -> check "the observer's exception aborts the run" false);
  check "fixtures release when an observer kills the run" !released

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let fx = Run.fixture ~teardown:(fun _ -> raise Boom) (fun () -> ()) in
  let tests = [ Test_tree.test "acquires" (fun () -> ignore (fx ())) ] in
  expect_run "release-failure suite runs" ~config tests @@ fun outcome ->
  (match outcome.Runner.release_failures with
  | [ f ] ->
      check "a failing release is a Release-phase failure"
        (f.Failure.phase = Failure.Release)
  | _ -> check "one release failure" false);
  check "a release failure exits 1, tests all green"
    (outcome.Runner.exit_code = 1
    && outcome_of outcome [ "acquires" ] = Some Failure.Pass)

(* ───── Events and list-only ───── *)

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
   Runner.Run_started { suite = "suite"; total = 2; selected = 2; _ };
   Runner.Test_started { path = [ "one" ] };
   Runner.Test_finished r1;
   Runner.Test_started { path = [ "two" ] };
   Runner.Test_finished r2;
  ] ->
      check "events stream in execution order"
        (r1.Run.path = [ "one" ] && r2.Run.path = [ "two" ])
  | _ -> check "events stream in execution order" false);
  events := [];
  let config = { config with Run.list_only = true } in
  expect_run "list-only run" ~on_event ~config tests @@ fun outcome ->
  check "list-only: nothing executed, selection listed, exit 0"
    (Run.results outcome.Runner.run = []
    && List.length outcome.Runner.selected = 2
    && outcome.Runner.exit_code = 0);
  check "list-only: no events" (!events = [])

(* ───── Property wiring ───── *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let ctx_seen = ref false in
  let tests =
    [
      Runner.prop "always-holds" Gen.int (fun _ ->
          ctx_seen :=
            !ctx_seen || Run.prop_context (Run.current_frame ()) <> None);
      Runner.prop ~count:3 "tiny" Gen.int (fun _ -> ());
      Runner.prop "never-holds" Gen.int (fun n -> Check.is_true (n = n + 1));
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
      Runner.prop "counted" Gen.int (fun _ -> ());
      Runner.prop ~count:2 "declared" Gen.int (fun _ -> ());
      Runner.prop "counted-fails" Gen.int (fun _ -> Check.fail "no");
      Runner.prop ~count:2 "declared-fails" Gen.int (fun _ -> Check.fail "no");
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
      Runner.prop "late" Gen.int (fun _ ->
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
    [ Runner.prop "shrinks" Gen.int (fun n -> Check.is_true (n = n + 1)) ]
  in
  let rendered outcome =
    match failure_list (outcome_of outcome [ "shrinks" ]) with
    | [ { Failure.kind = Failure.Property { rendered; case_index; _ }; _ } ] ->
        Some (rendered, case_index)
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
      Runner.prop ~examples:[ 0 ] "bad-example" Gen.int (fun n ->
          Check.is_true (n <> 0));
      Runner.prop ~count:5 ~examples:[ 1; 2 ] "counted-examples" Gen.int
        (fun _ -> ());
      Runner.prop ~count:5 "gives-up"
        Gen.(such_that (fun _ -> false) int)
        (fun _ -> ());
      Runner.prop ~count:5 "under-covered" Gen.int (fun _ ->
          let ctx =
            match Run.prop_context (Run.current_frame ()) with
            | Some ctx -> ctx
            | None -> Check.fail "no property context"
          in
          Property.cover ctx ~label:"never" ~at_least:99.0 false);
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
      check "an unsatisfied cover threshold names its label"
        (contains "coverage requirement not met" (message_of f)
        && contains "never" (message_of f))
  | _ -> check "under-covered: one failure" false

(* ───── Nested runs ───── *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [
      Test_tree.test "starts-a-run" (fun () ->
          ignore (Runner.execute ~config ~suite:"inner" []));
    ]
  in
  expect_run "nested-run suite runs" ~config tests @@ fun outcome ->
  match failure_list (outcome_of outcome [ "starts-a-run" ]) with
  | [ { Failure.kind = Failure.Raise { actual = Some actual; _ }; _ } ] ->
      check "a nested run fails the calling test"
        (contains "already active" actual)
  | _ -> check "a nested run fails the calling test" false

(* ───── Snapshot guard and prune plumbing ───── *)

let () =
  with_temp_root @@ fun root ->
  let config =
    { (base_config ~log_dir:root ()) with Run.update = Env.Update }
  in
  let suite = [ Test_tree.test "ok" (fun () -> ()) ] in
  Unix.putenv "CI" "true";
  expect_startup_error "update mode is refused under CI" ~config suite (function
    | Runner.Update_refused_in_ci -> true
    | _ -> false);
  check "the refusal message names the override"
    (contains "force" (Runner.startup_message Runner.Update_refused_in_ci));
  let forced = { config with Run.update = Env.Force_update } in
  expect_run "force update proceeds under CI" ~config:forced suite
  @@ fun outcome ->
  check "forced run is green" (outcome.Runner.exit_code = 0);
  clear_env ()

let () =
  clear_env ();
  with_temp_root @@ fun root ->
  let base = base_config ~log_dir:root () in
  let suite =
    [ Test_tree.test "t1" (fun () -> ()); Test_tree.test "t2" (fun () -> ()) ]
  in
  expect_run "no prune request means no prune" ~config:base suite
  @@ fun outcome ->
  check "pruned is None without --prune" (outcome.Runner.pruned = None);
  check "orphans of a snapshot-less run are empty" (outcome.Runner.orphans = []);
  let config =
    { base with Run.update = Env.Update; prune = true; filter = Some "t1" }
  in
  expect_run "prune refused on a filtered run" ~config suite @@ fun outcome ->
  (match outcome.Runner.pruned with
  | Some (Error refusal) ->
      check "the refusal reports the filter"
        (refusal.Snapshot.filtered && not refusal.Snapshot.not_update_run)
  | _ -> check "filtered prune is refused" false);
  let config = { base with Run.prune = true } in
  expect_run "prune refused without update mode" ~config suite @@ fun outcome ->
  match outcome.Runner.pruned with
  | Some (Error refusal) ->
      check "the refusal reports the missing update mode"
        (refusal.Snapshot.not_update_run && not refusal.Snapshot.filtered)
  | _ -> check "check-mode prune is refused" false

(* ───── The last-failed store ───── *)

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
  check "first run fails" (outcome.Runner.exit_code = 1);
  expect_run "--failed reruns the recorded failure" ~config:rerun tests
  @@ fun outcome ->
  check "--failed selects only the failure"
    (ran_names outcome = [ "shaky" ] && outcome.Runner.exit_code = 1);
  fixed := true;
  expect_run "--failed clears on pass" ~config:rerun tests @@ fun outcome ->
  check "the fixed test passes"
    (ran_names outcome = [ "shaky" ] && outcome.Runner.exit_code = 0);
  expect_startup_error "--failed with an empty store is refused" ~config:rerun
    tests (function
    | Runner.No_recorded_failures -> true
    | _ -> false);
  check "the empty-store refusal exits 2"
    (Runner.startup_exit_code Runner.No_recorded_failures = 2)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  expect_startup_error "--failed with no store at all is refused"
    ~config:{ config with Run.failed_only = true }
    [ Test_tree.test "any" (fun () -> ()) ]
    (function Runner.No_recorded_failures -> true | _ -> false)

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
    ~config:{ config with Run.filter = Some "t1" }
    tests
  @@ fun outcome ->
  check "only t1 reran and passed"
    (ran_names outcome = [ "t1" ] && outcome.Runner.exit_code = 0);
  expect_run "survivors: --failed keeps the unreached failure"
    ~config:{ config with Run.failed_only = true }
    tests
  @@ fun outcome ->
  check "t2's entry survived the filtered run" (ran_names outcome = [ "t2" ]);
  expect_run "--failed composes with a disjoint filter"
    ~config:{ config with Run.failed_only = true; filter = Some "t1" }
    tests
  @@ fun outcome ->
  check "a nonempty allowlist the filter rejects runs nothing, exit 2"
    (ran_names outcome = [] && outcome.Runner.exit_code = 2)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  expect_run "dead entries: record a failure" ~config
    [ Test_tree.test "old-test" (fun () -> Check.fail "boom") ]
  @@ fun _ ->
  expect_run "dead entries: a full run of a new suite drops them" ~config
    [ Test_tree.test "new-test" (fun () -> ()) ]
  @@ fun outcome ->
  check "the replacement run is green" (outcome.Runner.exit_code = 0);
  expect_startup_error "dead entries no longer match"
    ~config:{ config with Run.failed_only = true }
    [ Test_tree.test "new-test" (fun () -> ()) ]
    (function Runner.No_recorded_failures -> true | _ -> false)

(* ───── The exit guard (D1) ───── *)

(* The frozen interception text (runner.mli, classification). *)
let exit_message =
  "the test called exit — intercepted; a test must return or raise, never exit \
   the process"

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let after_ran = ref false in
  let tests =
    [
      Test_tree.test "before" (fun () -> ());
      Test_tree.test "bomb" (fun () -> Stdlib.exit 0);
      Test_tree.test "after" (fun () ->
          after_ran := true;
          Check.fail "genuine");
    ]
  in
  expect_run "exit-guard suite runs" ~config tests @@ fun outcome ->
  check_int "exit in body is intercepted and every test still runs" ~expected:3
    ~actual:(List.length (Run.results outcome.Runner.run));
  check "the test after the bomb executed" !after_ran;
  (match failure_list (outcome_of outcome [ "bomb" ]) with
  | [ f ] ->
      check "the interception is a Body-phase failure"
        (f.Failure.phase = Failure.Body);
      check_string "the interception message is frozen" ~expected:exit_message
        ~actual:(message_of f)
  | _ -> check "bomb: exactly one failure" false);
  check "the bomb and the genuine failure both counted"
    (outcome.Runner.failed_paths = [ "bomb"; "after" ]);
  check "the run exits through its own path with code 1"
    (outcome.Runner.exit_code = 1);
  check "the slot is inactive after execute returns" (not (Run.active ()))

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  expect_run "exit-7 suite runs" ~config
    [ Test_tree.test "bomb7" (fun () -> Stdlib.exit 7) ]
  @@ fun outcome ->
  check "exit 7 intercepts identically (the code is unobservable)"
    (match failure_list (outcome_of outcome [ "bomb7" ]) with
    | [ f ] -> message_of f = exit_message
    | _ -> false);
  check "exit 7: run exit code is 1" (outcome.Runner.exit_code = 1)

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
      Runner.prop "law-bomb" (Gen.int_range 0 1000) (fun n ->
          if n > 10 then Stdlib.exit 0);
    ]
  in
  expect_run "exit-prop suite runs" ~config tests @@ fun outcome ->
  match result_of outcome [ "law-bomb" ] with
  | Some r ->
      check "exit in a property law shrinks to a counterexample"
        (match r.Run.outcome with
        | Failure.Fail [ { Failure.kind = Failure.Property _; _ } ] -> true
        | _ -> false);
      check "the engine's stats are recorded" (r.Run.prop_stats <> None)
  | None -> check "law-bomb recorded" false

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let fx = Run.fixture ~teardown:(fun _ -> Stdlib.exit 0) (fun () -> ()) in
  expect_run "exit-release suite runs" ~config
    [ Test_tree.test "uses" (fun () -> ignore (fx ())) ]
  @@ fun outcome ->
  (match outcome.Runner.release_failures with
  | [ f ] ->
      check "exit during fixture release is a Release-phase failure"
        (f.Failure.phase = Failure.Release);
      check "the release failure renders the registered printer"
        (contains
           "release raised Exit_attempt (code under test called exit; \
            intercepted by windtrap)"
           (message_of f))
  | _ -> check "exit-release: exactly one release failure" false);
  check "exit-release: run exit code is 1" (outcome.Runner.exit_code = 1)

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
    (outcome.Runner.failed_paths = []);
  check "the excused bomb leaves the run green" (outcome.Runner.exit_code = 0)

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
  check "siblings run after an exit-bombed subtest" !sibling_ran;
  match failure_list (outcome_of outcome [ "sub-bomb" ]) with
  | [ f ] ->
      check "the sub-case failure renders the interception"
        (match f.Failure.kind with
        | Failure.Raise { actual = Some text; _ } ->
            contains "Exit_attempt" text
        | _ -> false)
  | _ -> check "exit-subtest: exactly one failure" false

(* ───── Location capture at the boundary (D4) ───── *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let pos = ("test/fake_decl.ml", 21, 2, 30) in
  let tests =
    [
      (* The check sits in tail position: its caller's frame is gone at
         raise time, so capture must stop at the runner's delimiter and the
         recording falls back to the declaration — never the line that
         called [execute]. *)
      Test_tree.test ~pos "tail" (fun () -> Check.equal Testable.int 1 2);
    ]
  in
  expect_run "tail-loc suite runs" ~config tests @@ fun outcome ->
  match failure_list (outcome_of outcome [ "tail" ]) with
  | [ f ] ->
      check "a tail-position check failure is attributed to the declaration"
        (f.Failure.loc = Some (Loc.of_pos pos))
  | _ -> check "tail-loc: exactly one failure" false

(* ───── srandom recording (D5 §6) ───── *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [
      Test_tree.test "draws and fails" (fun () ->
          ignore (Random.State.bits (Run.srandom ()));
          Check.fail "boom");
      Test_tree.test "draws and passes" (fun () ->
          ignore (Random.State.bits (Run.srandom ())));
      Test_tree.test "never draws" (fun () -> Check.fail "boom");
    ]
  in
  expect_run "srandom recording" ~config tests @@ fun outcome ->
  let root_of path =
    Option.bind (result_of outcome path) (fun r -> r.Run.srandom_root)
  in
  check "a failing test that drew records the run's root seed"
    (root_of [ "draws and fails" ] = Some config.Run.seed);
  check "a passing test that drew records it too (per-result data)"
    (root_of [ "draws and passes" ] = Some config.Run.seed);
  check "a test that never drew records nothing"
    (root_of [ "never draws" ] = None)

let () =
  (* The flag is per attempt and the result records the final attempt's:
     a retried test that draws on every attempt still carries the root. *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let attempts = ref 0 in
  let tests =
    [
      Test_tree.test ~retries:1 "flaky draw" (fun () ->
          incr attempts;
          ignore (Random.State.bits (Run.srandom ()));
          if !attempts = 1 then Check.fail "first attempt");
    ]
  in
  expect_run "srandom on retries" ~config tests @@ fun outcome ->
  match result_of outcome [ "flaky draw" ] with
  | Some r ->
      check "the final attempt's draw is recorded across retries"
        (r.Run.attempts = 2 && r.Run.srandom_root = Some config.Run.seed)
  | None -> check "flaky draw result present" false

(* ───── Summary ───── *)

let () = finish ()
