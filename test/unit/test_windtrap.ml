(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* End-to-end tests of the Windtrap facade: suites declared with the public
   surface (verbs, testables, prop, expect, output, fixture) executed
   in-process through Run.execute under a synthetic config, asserting on
   typed outcomes. Plain executable: [run] and [execute] both refuse to
   nest inside an active run, so a windtrap suite could drive neither.
   The facade's [run] returns its exit code, so its command-line paths
   are driven in this process too ([run_in_process] below); only the
   signal scenarios, which need a process to signal, re-exec this
   executable as a child. The transcripts of a real process (a test that
   calls [exit], the replay line's spelling, a failing release) are
   test/cli's. *)

open Windtrap
open Windtrap.Private
module Tag = Test_tree.Tag
open Harness

let () = init "facade"

(* The signal child: re-exec'd to run a suite whose third test says it is
   ready and then waits, for the parent to signal; or, in the [between]
   mode, a suite whose observer signals the process from the executor's
   own code; or, in the [releasing] mode, a suite whose last-acquired
   fixture says it is ready from its release and then waits. The three
   dispositions are reset first: what the parent of this test was started
   with is not the subject, and the [ignored-hup] mode states its own. *)
let () =
  match Array.to_list Sys.argv with
  | [ _; "--signal-child"; root; mode ] ->
      clear_env ();
      List.iter
        (fun signal -> Sys.set_signal signal Sys.Signal_default)
        [ Sys.sigint; Sys.sigterm; Sys.sighup ];
      if mode = "ignored-hup" then Sys.set_signal Sys.sighup Sys.Signal_ignore;
      let waits () =
        print_string "captured, never shown\n";
        ignore (temp_dir ());
        close_out (open_out (Filename.concat root "ready"));
        Unix.sleepf 60.
      in
      let touch name () = close_out (open_out (Filename.concat root name)) in
      let first = fixture ~teardown:(touch "first released") ignore in
      let second = fixture ~teardown:(touch "second released") ignore in
      let hangs =
        fixture
          ~teardown:(fun () ->
            touch "ready" ();
            Unix.sleepf 60.)
          ignore
      in
      (* [leftovers]: what an interrupted run leaves as it was. The first
         test records a correction, the second fails, and the third sets a
         variable inside a bracket and waits. The fixture's release reads
         the variable. [second]: a release that hangs after the first
         signal, for the second to end. *)
      if mode = "leftovers" then begin
        Unix.putenv "WINDTRAP_PROJECT_ROOT" root;
        let reads_env =
          fixture
            ~teardown:(fun () ->
              Out_channel.with_open_bin (Filename.concat root "env at release")
                (fun oc ->
                  output_string oc
                    (Option.value ~default:"<unset>"
                       (Sys.getenv_opt "WINDTRAP_LEFT"))))
            ignore
        in
        exit
        @@ Windtrap.run
             ~argv:
               [|
                 "signal-child";
                 "-o";
                 Filename.concat root "logs";
                 "--color";
                 "never";
                 "--corrected";
               |]
             "signals"
             [
               test "corrects" (fun () ->
                   reads_env ();
                   expect_file "new\n" "c.expected");
               test "fails" (fun () -> equal int 1 2);
               bracket ~setup:ignore ~teardown:(touch "teardown ran") "waits"
                 (fun () ->
                   setenv "WINDTRAP_LEFT" (Some "set by the test");
                   waits ());
             ]
      end;
      if mode = "second" then begin
        let lingers =
          fixture
            ~teardown:(fun () ->
              touch "releasing" ();
              Unix.sleepf 60.)
            ignore
        in
        exit
        @@ Windtrap.run
             ~argv:[| "signal-child"; "-o"; root; "--color"; "never" |]
             "signals"
             [
               test "holds and waits" (fun () ->
                   lingers ();
                   waits ());
             ]
      end;
      let tests =
        if mode = "releasing" then
          [
            test "acquires" (fun () ->
                first ();
                second ();
                hangs ());
          ]
        else
          [
            test "passes" (fun () -> is_true true);
            test "fails" (fun () -> equal int 1 2);
            group "deep" [ test "waits" waits ];
            test "never reached" (fun () -> is_true true);
          ]
      in
      if mode = "between" then begin
        let config =
          {
            (Run.default_config ()) with
            Run.log_dir = root;
            color = Os.Never;
            exclude = Some "fails";
          }
        in
        (* It raises on the Interrupted event, which the runner ignores. *)
        let signal_self = function
          | Run.Test_finished _ -> Unix.kill (Unix.getpid ()) Sys.sigterm
          | Run.Interrupted _ -> raise Exit
          | Run.Run_started _ | Run.Test_started _ | Run.Fixture_release _ -> ()
        in
        ignore (Report.run ~on_event:signal_self ~suite:"signals" config tests);
        exit 3
      end
      else
        exit
        @@ Windtrap.run
             ~argv:[| "signal-child"; "-o"; root; "--color"; "never" |]
             "signals" tests
  | _ -> ()

(* Standard output and standard error redirected at the descriptor level
   to two files under [root] for the extent of [fn]: what an in-process
   [run] prints is read back rather than mixed into this harness's own
   transcript. The library's capture juggles the same two descriptors
   around every test and restores what it saved, which is these files. *)
let with_redirected_output root fn =
  let flush_all () =
    Format.pp_print_flush Format.std_formatter ();
    Format.pp_print_flush Format.err_formatter ();
    flush stdout;
    flush stderr
  in
  let redirect name fd =
    let path = Filename.concat root name in
    let file =
      Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
    in
    let saved = Unix.dup ~cloexec:true fd in
    Unix.dup2 file fd;
    Unix.close file;
    (path, saved)
  in
  flush_all ();
  let out, saved_out = redirect "run.stdout" Unix.stdout in
  let err, saved_err = redirect "run.stderr" Unix.stderr in
  let restore () =
    flush_all ();
    Unix.dup2 saved_out Unix.stdout;
    Unix.dup2 saved_err Unix.stderr;
    Unix.close saved_out;
    Unix.close saved_err
  in
  let result = Fun.protect ~finally:restore fn in
  let contents path = In_channel.with_open_bin path In_channel.input_all in
  (result, contents out, contents err)

(* The facade's [run] in this process, under [argv] after the fixed
   prefix every run here takes: the code it returned, its standard
   output and its standard error. [root] is the capture-log root and the
   home of the two redirected files. *)
let run_in_process ?(argv = []) root suite tests =
  with_redirected_output root (fun () ->
      Windtrap.run
        ~argv:
          (Array.of_list
             (suite :: "-o" :: root :: "--color" :: "never" :: argv))
        suite tests)

let with_temp_root f = with_temp_root ~prefix:"windtrap-facade-" f

let base_config ~log_dir () =
  { (Run.default_config ()) with Run.seed = 0x5eedL; log_dir }

let expect_run name ?on_event ~config ?(suite = "suite") tests f =
  match Run.execute ?on_event config ~suite tests with
  | Ok outcome -> f outcome
  | Error error ->
      check name false;
      Printf.printf "  startup error: %s\n%!" (Run.startup_message error)

let result_of outcome path =
  List.find_opt (fun r -> r.Run.path = path) (Run.results outcome.Run.run)

(* The paths that counted as failed, in execution order — what the exit
   code and the last-failed store react to. *)
let failed_paths outcome =
  List.filter_map
    (fun (r : Run.result) ->
      if r.Run.counted then Some (Test_tree.path_to_string r.Run.path) else None)
    (Run.results outcome.Run.run)

let outcome_of outcome path =
  match result_of outcome path with
  | Some r -> Some r.Run.outcome
  | None -> None

let failure_list = function
  | Some (Failure.Fail fs) -> fs
  | Some Failure.Pass | Some (Failure.Skip _) | None -> []

(* A working directory that no longer exists is what a missing [chdir]
   restoration leaves behind, so reading it must not be fatal: the
   regression has to read as a failed check rather than take the suite
   down with it. *)
let cwd_or_gone () = try Sys.getcwd () with Sys_error _ -> "<gone>"

(* Ambient operations outside a run *)

let probe_fixture = fixture (fun () -> ())

let () =
  let outside label thunk =
    match thunk () with
    | () -> check (label ^ " raises outside a run") false
    | exception Invalid_argument message ->
        check
          (label ^ " raises the outside-run error")
          (contains "no test is running" message)
    | exception _ -> check (label ^ " raises Invalid_argument") false
  in
  outside "output ()" (fun () -> ignore (output ()));
  outside "expect_file" (fun () -> expect_file "x" "name.expected");
  outside "collect" (fun () -> collect "label");
  outside "fixture accessor" (fun () -> probe_fixture ());
  outside "current_test" (fun () -> ignore (current_test ()));
  outside "temp_dir" (fun () -> ignore (temp_dir ()));
  outside "temp_file" (fun () -> ignore (temp_file ()));
  (* Both read the frame before touching the process: an ambient operation
     with no runner to undo it must not half-happen. *)
  let home = Sys.getcwd () in
  outside "setenv" (fun () -> setenv "WINDTRAP_TEST_OUTSIDE" (Some "x"));
  outside "chdir" (fun () -> chdir (Filename.get_temp_dir_name ()));
  check "setenv and chdir outside a run change nothing"
    (Sys.getenv_opt "WINDTRAP_TEST_OUTSIDE" = None && Sys.getcwd () = home);
  (* [subtest] reads the frame before running its body: the body must not
     execute outside a run. *)
  let body_ran = ref false in
  outside "subtest" (fun () -> subtest "sub" (fun () -> body_ran := true));
  check "subtest outside a run never runs its body" (not !body_ran)

(* The raising verbs need no ambient state. *)
let () =
  (match equal int 1 2 with
  | () -> check "equal outside a run raises" false
  | exception Failure.Check_failure failure ->
      check "equal outside a run raises Check_failure"
        (match failure.Failure.kind with
        | Failure.Equality _ -> true
        | _ -> false));
  match skip ~reason:"why" () with
  | _ -> check "skip raises Skip_test" false
  | exception Failure.Control (`Skip reason) ->
      check "uncaught skip renders readably"
        (contains "why" (Printexc.to_string (Failure.Control (`Skip reason))))

(* Declaration surface *)

let () =
  let tree =
    [
      test "plain" (fun () -> ());
      group "outer" [ group "inner" [ test "nested" (fun () -> ()) ] ];
      slow "big" (fun () -> ());
      cases "squares" ~name:string_of_int [ 1; 2 ] (fun _ -> ());
      prop "law" Gen.int (fun _ -> ());
      bracket
        ~setup:(fun () -> ())
        ~teardown:(fun () -> ())
        "bracketed"
        (fun () -> ());
      scoped (fun fn -> fn ()) "in a scope" (fun () -> ());
    ]
  in
  let cases_flat = Test_tree.flatten tree in
  let paths =
    List.map (fun c -> Test_tree.path_to_string c.Test_tree.path) cases_flat
  in
  check "flatten yields depth-first paths"
    (paths
    = [
        "plain";
        "outer › inner › nested";
        "big";
        "squares › 1";
        "squares › 2";
        "law";
        "bracketed";
        "in a scope";
      ]);
  let tags_of path =
    match
      List.find_opt
        (fun c -> Test_tree.path_to_string c.Test_tree.path = path)
        cases_flat
    with
    | Some c -> c.Test_tree.tags
    | None -> Tag.empty
  in
  check "slow pre-applies the slow tag" (Tag.mem Tag.slow (tags_of "big"));
  check "facade prop pre-applies the prop tag" (Tag.mem "prop" (tags_of "law"))

(* The verbs, end to end *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite =
    [
      test "equal" (fun () -> equal (list int) [ 1; 2 ] [ 1; 2 ]);
      test "not_equal" (fun () -> not_equal string "a" "b");
      test "bools" (fun () ->
          is_true true;
          is_false false);
      test "require_some" (fun () -> equal int 1 (require_some (Some 1)));
      test "require_ok" (fun () -> equal int 2 (require_ok (Ok 2)));
      test "require_error" (fun () ->
          equal string "e" (require_error (Error "e")));
      test "raises" (fun () -> raises Exit (fun () -> raise Exit));
      test "raises_match" (fun () ->
          raises_match
            (function Stdlib.Failure _ -> true | _ -> false)
            (fun () -> failwith "boom"));
      test "composites" (fun () ->
          equal
            (option (pair (float 1e-9) (slist int compare)))
            (Some (1., [ 1; 2 ]))
            (Some (1., [ 2; 1 ]));
          equal (Testable.contramap String.length int) "abc" "xyz";
          equal pass 1 2);
    ]
  in
  expect_run "verbs all pass" ~config suite @@ fun outcome ->
  check_int "verbs: exit code" ~expected:0 ~actual:outcome.Run.exit_code;
  check "verbs: every outcome is Pass"
    (List.for_all
       (fun r -> r.Run.outcome = Failure.Pass)
       (Run.results outcome.Run.run))

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite =
    [
      test "wrong" (fun () -> equal int 5 7);
      test "unwrap" (fun () ->
          ignore (require_ok ~pp:Format.pp_print_string (Error "bad parse")));
      test "wrong exn" (fun () -> raises Exit (fun () -> failwith "other"));
      test "skipped" (fun () -> skip ~reason:"not today" ());
    ]
  in
  expect_run "failing verbs" ~config suite @@ fun outcome ->
  check_int "failing verbs: exit code" ~expected:1 ~actual:outcome.Run.exit_code;
  (match failure_list (outcome_of outcome [ "wrong" ]) with
  | [ { Failure.kind = Failure.Equality { expected; actual; not_ }; _ } ] ->
      check "equal failure renders expected then actual"
        (expected = "5" && actual = "7" && not not_)
  | _ -> check "equal failure carries an Equality payload" false);
  (match failure_list (outcome_of outcome [ "unwrap" ]) with
  | [ { Failure.kind = Failure.Equality { actual; _ }; _ } ] ->
      check "require_ok renders the error side via pp"
        (contains "bad parse" actual)
  | _ -> check "require_ok failure carries an Equality payload" false);
  (match failure_list (outcome_of outcome [ "wrong exn" ]) with
  | [ { Failure.kind = Failure.Raise { expected; actual; _ }; _ } ] ->
      check "raises failure records both exceptions"
        (expected <> None && actual <> None)
  | _ -> check "raises failure carries a Raise payload" false);
  check "skip is not a failure"
    (outcome_of outcome [ "skipped" ] = Some (Failure.Skip (Some "not today")))

(* A nonempty selection whose every test skipped exits 0 (guarantee 9). *)
let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite = [ test "s" (fun () -> skip ()) ] in
  expect_run "all-skipped run" ~config suite @@ fun outcome ->
  check_int "all-skipped run exits 0" ~expected:0 ~actual:outcome.Run.exit_code

(* Scopes, brackets and fixtures *)

let () =
  (* The facade export, end to end: a scoper of the shape most OCaml
     resources come in, partially applied into a constructor exactly as the
     interface advertises. *)
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let released = ref [] in
  let with_conn =
    scoped (fun fn ->
        Fun.protect
          ~finally:(fun () -> released := "conn" :: !released)
          (fun () -> fn "conn"))
  in
  let suite =
    [
      with_conn "clean" (fun conn -> equal string "conn" conn);
      with_conn ~tags:[ "net" ] "the body fails" (fun _ -> fail "body-boom");
    ]
  in
  expect_run "scopes" ~config suite @@ fun outcome ->
  check "the scope reclaimed on both the passing and the failing path"
    (!released = [ "conn"; "conn" ]);
  check "a scoped test passes when its body does"
    (outcome_of outcome [ "clean" ] = Some Failure.Pass);
  check "the partially applied constructor still takes ~tags"
    (List.exists
       (fun (c : Test_tree.case) ->
         c.Test_tree.path = [ "the body fails" ]
         && Tag.mem "net" c.Test_tree.tags)
       (Test_tree.flatten suite));
  match failure_list (outcome_of outcome [ "the body fails" ]) with
  | [ f ] ->
      check "the body's failure is the test's only failure"
        (f.Failure.phase = Failure.Body)
  | fs ->
      check_int "scoped failure entries" ~expected:1 ~actual:(List.length fs)

(* Brackets and fixtures *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let teardowns = ref [] in
  let suite =
    [
      bracket
        ~setup:(fun () -> "db")
        ~teardown:(fun tag -> teardowns := tag :: !teardowns)
        "clean"
        (fun tag -> equal string "db" tag);
      bracket
        ~setup:(fun () -> "res")
        ~teardown:(fun _ -> fail "td-boom")
        "both fail"
        (fun _ -> fail "body-boom");
    ]
  in
  expect_run "brackets" ~config suite @@ fun outcome ->
  check "bracket teardown ran" (!teardowns = [ "db" ]);
  match failure_list (outcome_of outcome [ "both fail" ]) with
  | [ a; b ] ->
      check "body and teardown failures are two entries"
        (a.Failure.phase = Failure.Body && b.Failure.phase = Failure.Teardown)
  | fs ->
      check_int "bracket failure entries" ~expected:2 ~actual:(List.length fs)

let shared_calls = ref 0
let shared_teardowns = ref 0

let shared =
  fixture
    ~teardown:(fun _ -> incr shared_teardowns)
    (fun () ->
      incr shared_calls;
      !shared_calls)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let releases = ref [] in
  let on_event = function
    | Run.Fixture_release { name } -> releases := name :: !releases
    | _ -> ()
  in
  let suite =
    [
      test "first use" (fun () -> equal int 1 (shared ()));
      test "second use" (fun () -> equal int 1 (shared ()));
    ]
  in
  expect_run "fixture sharing" ~on_event ~config suite @@ fun outcome ->
  check_int "fixture: acquired once" ~expected:1 ~actual:!shared_calls;
  check_int "fixture: released once" ~expected:1 ~actual:!shared_teardowns;
  check_int "fixture: exit code" ~expected:0 ~actual:outcome.Run.exit_code;
  check "fixture: release announced by name"
    (match !releases with [ name ] -> contains "fixture" name | _ -> false);
  (* A later run in the same process re-acquires: the cache is per run. *)
  expect_run "fixture re-acquisition" ~config
    [ test "again" (fun () -> equal int 2 (shared ())) ]
  @@ fun outcome2 ->
  check_int "fixture: re-acquired on the next run" ~expected:2
    ~actual:!shared_calls;
  check_int "fixture: second run exit code" ~expected:0
    ~actual:outcome2.Run.exit_code

let failing_release =
  fixture ~teardown:(fun _ -> fail "release-boom") (fun () -> ())

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite = [ test "touch" (fun () -> failing_release ()) ] in
  expect_run "release failure" ~config suite @@ fun outcome ->
  check "release failure is a Release-phase entry"
    (match outcome.Run.release_failures with
    | [ f ] -> f.Failure.phase = Failure.Release
    | _ -> false);
  check_int "release failure exits 1" ~expected:1 ~actual:outcome.Run.exit_code

(* Properties through the facade *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite =
    [
      prop "holds" ~count:25 Gen.small_int (fun n -> equal int n n);
      prop "labelled" ~count:25 Gen.small_int (fun n ->
          collect (if n mod 2 = 0 then "even" else "odd");
          classify "small" (abs n < 100);
          cover "any" true);
      prop "assumes" ~count:10 Gen.small_int (fun n ->
          assume (n mod 2 = 0);
          equal int 0 (n mod 2));
      prop "fails" ~count:50 Gen.int (fun n -> is_true (n = n + 1));
    ]
  in
  expect_run "properties" ~config suite @@ fun outcome ->
  check "prop pass records stats"
    (match result_of outcome [ "holds" ] with
    | Some { Run.prop_stats = Some stats; _ } -> stats.Property.cases = 25
    | _ -> false);
  check "collect/classify/cover reach the engine"
    (match result_of outcome [ "labelled" ] with
    | Some { Run.prop_stats = Some stats; _ } ->
        List.mem_assoc "small" stats.Property.collected
        && (List.mem_assoc "even" stats.Property.collected
           || List.mem_assoc "odd" stats.Property.collected)
        && List.exists
             (fun c -> c.Property.label = "any" && c.Property.satisfied)
             stats.Property.coverage
    | _ -> false);
  check "assume discards are counted"
    (match result_of outcome [ "assumes" ] with
    | Some { Run.prop_stats = Some stats; _ } ->
        stats.Property.cases = 10 && stats.Property.discards > 0
    | _ -> false);
  match failure_list (outcome_of outcome [ "fails" ]) with
  | [ { Failure.kind = Failure.Property { root; inner; _ }; _ } ] -> (
      check "prop failure carries the run's root seed" (root = 0x5eedL);
      match inner with
      | Some inner ->
          check "prop inner failure is the assertion's"
            (match inner.Failure.kind with
            | Failure.Equality _ -> true
            | _ -> false)
      | None -> check "prop failure carries an inner failure" false)
  | _ -> check "prop failure carries a Property payload" false

(* Properties nest freely under groups: the pre-applied tag and the stats
   survive at the full path. *)
let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite =
    [
      group "outer"
        [ prop "law" ~count:5 Gen.small_int (fun n -> equal int n n) ];
    ]
  in
  expect_run "nested prop" ~config suite @@ fun outcome ->
  check_int "nested prop: exit code" ~expected:0 ~actual:outcome.Run.exit_code;
  check "nested prop records stats at its full path"
    (match result_of outcome [ "outer"; "law" ] with
    | Some { Run.prop_stats = Some stats; _ } -> stats.Property.cases = 5
    | _ -> false);
  check "nested prop keeps the prop tag"
    (List.exists
       (fun case ->
         case.Test_tree.path = [ "outer"; "law" ]
         && Tag.mem "prop" case.Test_tree.tags)
       outcome.Run.selected)

(* collect/classify/cover outside a property error out. *)
let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite = [ test "stray collect" (fun () -> collect "label") ] in
  expect_run "ambient collect misuse" ~config suite @@ fun outcome ->
  match failure_list (outcome_of outcome [ "stray collect" ]) with
  | [ { Failure.kind = Failure.Raise { actual = Some rendered; _ }; _ } ] ->
      check "collect outside a property names the misuse"
        (contains "collect" rendered && contains "property" rendered)
  | _ -> check "collect outside a property fails the test" false

(* A discard outside a property has no owner: the test fails with a
   message that names the two verbs. *)
let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite =
    [
      test "stray assume" (fun () -> assume false);
      test "stray reject" (fun () -> reject ());
    ]
  in
  expect_run "discard outside a property" ~config suite @@ fun outcome ->
  List.iter
    (fun name ->
      match failure_list (outcome_of outcome [ name ]) with
      | [ { Failure.kind = Failure.Message text; _ } ] ->
          check
            (name ^ " fails with the message")
            (text = "assume or reject was called outside a property")
      | _ -> check (name ^ " fails with one message") false)
    [ "stray assume"; "stray reject" ]

(* Captured output *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite =
    [
      test "reads captured bytes" (fun () ->
          print_string "hello\n";
          equal string "hello\n" (output ());
          equal string "" (output ());
          print_string "more";
          equal string "more" (output ()));
    ]
  in
  expect_run "output ()" ~config suite @@ fun outcome ->
  check_int "output (): exit code" ~expected:0 ~actual:outcome.Run.exit_code

let () =
  with_temp_root @@ fun root ->
  let config = { (base_config ~log_dir:root ()) with Run.stream = true } in
  let suite = [ test "streams" (fun () -> ignore (output ())) ] in
  expect_run "output () under --stream" ~config suite @@ fun outcome ->
  match failure_list (outcome_of outcome [ "streams" ]) with
  | [ { Failure.kind = Failure.Message message; _ } ] ->
      check "output () under --stream fails with the capture hint"
        (contains "--stream" message)
  | _ -> check "output () under --stream fails the test" false

(* Baselines through the facade *)

(* [Run.execute] resolves the project root before any test runs, so no
   test can bind it through [setenv]: the scenario binds it around its
   runs, and puts back what the variable held, bound or not. *)
let with_project_root root f =
  let prior = Sys.getenv_opt "WINDTRAP_PROJECT_ROOT" in
  Os.setenv "WINDTRAP_PROJECT_ROOT" (Some root);
  Fun.protect ~finally:(fun () -> Os.setenv "WINDTRAP_PROJECT_ROOT" prior) f

let read_file path = In_channel.with_open_bin path In_channel.input_all

let () =
  with_temp_root @@ fun root ->
  with_project_root root @@ fun () ->
  let config = base_config ~log_dir:(Filename.concat root "logs") () in
  let file_test =
    test "greets" (fun () -> expect_file "hello\n" "src/greeting.expected")
  in
  let path = Filename.concat root "src/greeting.expected" in
  (* 1. Check mode, no baseline: read-only failure carrying the proposal. *)
  ( expect_run "file baseline missing" ~config [ file_test ] @@ fun outcome ->
    match failure_list (outcome_of outcome [ "greets" ]) with
    | [
     {
       Failure.kind =
         Failure.Baseline
           { baseline = Failure.File p; state = Failure.Missing { proposed } };
       _;
     };
    ] ->
        check "a missing baseline proposes the canonical content"
          (p = "src/greeting.expected" && proposed = "hello\n");
        check "a missing baseline writes nothing" (not (Sys.file_exists path))
    | _ -> check "a missing baseline fails with a Baseline payload" false );
  (* 2. Update mode accepts, reports the write, and exits 0. *)
  let update_config = { config with Run.baseline = Baseline.Update } in
  ( expect_run "file baseline acceptance" ~config:update_config [ file_test ]
  @@ fun outcome ->
    check_int "update run exits 0" ~expected:0 ~actual:outcome.Run.exit_code;
    match Baseline.writes (Run.baselines outcome.Run.run) with
    | [ Baseline.Written { path = written; literals = 0 } ] ->
        check "acceptance wrote the file"
          (written = path && read_file path = "hello\n")
    | _ -> check "acceptance recorded one write" false );
  (* 3. Check mode now passes; a changed actual mismatches. *)
  ( expect_run "file baseline green" ~config [ file_test ] @@ fun outcome ->
    check_int "the baseline matches its committed file" ~expected:0
      ~actual:outcome.Run.exit_code );
  expect_run "file baseline mismatch" ~config
    [
      test "greets" (fun () -> expect_file "goodbye\n" "src/greeting.expected");
    ]
  @@ fun outcome ->
  match failure_list (outcome_of outcome [ "greets" ]) with
  | [
   {
     Failure.kind =
       Failure.Baseline { state = Failure.Mismatch { expected; actual }; _ };
     _;
   };
  ] ->
      check "mismatch carries both canonical texts"
        (expected = "hello\n" && actual = "goodbye\n")
  | _ -> check "mismatch fails with a Mismatch payload" false

(* Literals through the facade: the literal is last, compared flexibly by
   [expect] and byte for byte by [expect_exact]; a mismatch sits at the
   literal's own position. *)
let () =
  with_temp_root @@ fun root ->
  with_project_root root @@ fun () ->
  let config = base_config ~log_dir:(Filename.concat root "logs") () in
  (* The stale literal's own position, as the verb receives it. *)
  let stale_site = ref None in
  let suite =
    [
      test "flexible" (fun () ->
          expect "a\n  b\n"
          @@ __POS_OF__ {|
            a
              b
          |});
      test "exact" (fun () -> expect_exact "a\n" @@ __POS_OF__ "a\n");
      test "stale" (fun () ->
          let ((pos, _) as literal) = __POS_OF__ {| old |} in
          stale_site := Some pos;
          expect "new" literal);
      test "stale exact" (fun () -> expect_exact "new" @@ __POS_OF__ "new ");
    ]
  in
  expect_run "literal baselines" ~config suite @@ fun outcome ->
  check "flexible and exact match; the stale literals fail"
    (failed_paths outcome = [ "stale"; "stale exact" ]);
  (* The failure says which verb read the literal: the block's first fact
     line is [expect: mismatch] or [expect_exact: mismatch]. *)
  (match failure_list (outcome_of outcome [ "stale exact" ]) with
  | [
   {
     Failure.kind =
       Failure.Baseline
         {
           baseline = Failure.Literal { exact = true };
           state = Failure.Mismatch { expected = "new "; actual = "new" };
         };
     _;
   };
  ] ->
      ()
  | _ -> check "a stale exact literal fails with an exact Literal payload" false);
  match failure_list (outcome_of outcome [ "stale" ]) with
  | [
   ({
      Failure.kind =
        Failure.Baseline
          {
            baseline = Failure.Literal { exact = false };
            state = Failure.Mismatch { expected; actual };
          };
      _;
    } as f);
  ] ->
      check "the mismatch carries the normalized forms"
        (expected = "old" && actual = "new");
      check "the failure sits at the literal's position"
        (match (f.Failure.loc, !stale_site) with
        | Some loc, Some (file, line, _, _) ->
            String.ends_with ~suffix:"test_windtrap.ml" loc.Loc.file
            && String.ends_with ~suffix:"test_windtrap.ml" file
            && loc.Loc.line = line
        | _ -> false)
  | _ -> check "a stale literal fails with a Literal payload" false

(* A stale expectation beside another failure: the run keeps no
   correction in any mode, and says so on the baseline failure, literal
   or file, so that no report offers an acceptance that would rewrite or
   promote nothing. *)
let () =
  with_temp_root @@ fun root ->
  with_project_root root @@ fun () ->
  let base = base_config ~log_dir:(Filename.concat root "logs") () in
  (* The literal's source is a real file under the root, which a
     correcting check reads. *)
  Out_channel.with_open_bin (Filename.concat root "t.ml") (fun oc ->
      output_string oc "let () =\n  expect \"new\" (__POS_OF__ {| old |})\n");
  let suite =
    [
      test "masked literal" (fun () ->
          expect "new" (("t.ml", 2, 15, 0), " old ");
          fail "boom");
      test "masked file" (fun () ->
          expect_file "new\n" "src/masked.expected";
          fail "boom");
      test "skipped" (fun () ->
          expect_file "new\n" "src/skipped.expected";
          skip ());
      test "clean" (fun () -> expect_file "new\n" "src/clean.expected");
    ]
  in
  let withheld outcome path =
    List.filter_map
      (fun (f : Failure.t) ->
        match f.Failure.kind with
        | Failure.Baseline { withheld; _ } -> Some withheld
        | _ -> None)
      (failure_list (outcome_of outcome [ path ]))
  in
  let written name =
    List.exists Sys.file_exists
      [
        Filename.concat root ("src/" ^ name ^ ".expected");
        Filename.concat root ("src/" ^ name ^ ".expected.corrected");
      ]
  in
  let outside = [ Some Failure.Failed_outside ] in
  List.iter
    (fun (mode, baseline) ->
      expect_run
        ("withheld corrections, " ^ mode)
        ~config:{ base with Run.baseline } suite
      @@ fun outcome ->
      check
        (mode ^ ": a literal beside another failure is withheld")
        (withheld outcome "masked literal" = outside);
      check
        (mode ^ ": a file beside another failure is withheld")
        (withheld outcome "masked file" = outside);
      check
        (mode ^ ": a skip after the expectation withholds it too")
        (withheld outcome "skipped" = [ Some Failure.Skipped ]);
      check
        (mode ^ ": an otherwise clean test keeps its correction")
        (withheld outcome "clean" = [ None ]);
      check
        (mode ^ ": nothing is written for a withheld correction")
        (not (written "masked" || written "skipped")))
    [ ("check", Baseline.Check); ("corrected", Baseline.Corrected) ];
  (* Under [-u] a stale expectation is no failure: the masked tests fail on
     their assertion alone, offer no acceptance, and rewrite nothing. *)
  expect_run "withheld corrections, update"
    ~config:{ base with Run.baseline = Baseline.Update }
    suite
  @@ fun outcome ->
  check "update: the masked tests hold no baseline failure to accept"
    (withheld outcome "masked literal" = []
    && withheld outcome "masked file" = []);
  check "update: they still fail"
    (List.for_all
       (fun path ->
         List.length (failure_list (outcome_of outcome [ path ])) = 1)
       [ "masked literal"; "masked file" ]);
  check "update: nothing is rewritten for them"
    (not (written "masked" || written "skipped"));
  check "update: the clean test's file is" (written "clean")

(* A checkpoint, not an assertion: a mismatch is recorded and the call
   returns, so a body with two stale literals reports both, and a
   correcting run records both corrections in one pass — the source is
   a real file under the root, so the pass also writes them. *)
let () =
  with_temp_root @@ fun root ->
  with_project_root root @@ fun () ->
  let config =
    {
      (base_config ~log_dir:(Filename.concat root "logs") ()) with
      Run.baseline = Baseline.Corrected;
    }
  in
  let source =
    "let () =\n\
    \  expect \"x\" (__POS_OF__ {| old1 |});\n\
    \  expect \"y\" (__POS_OF__ {| old2 |})\n"
  in
  let path = Filename.concat root "t.ml" in
  Out_channel.with_open_bin path (fun oc -> output_string oc source);
  let reached_second = ref false in
  let suite =
    [
      test "two stale" (fun () ->
          expect "x" (("t.ml", 2, 13, 36), " old1 ");
          reached_second := true;
          expect "y" (("t.ml", 3, 13, 36), " old2 "));
    ]
  in
  expect_run "two stale literals" ~config suite @@ fun outcome ->
  check "the body continued past the first mismatch" !reached_second;
  check_int "both mismatches are the test's failures" ~expected:2
    ~actual:(List.length (failure_list (outcome_of outcome [ "two stale" ])));
  check_int "both are recorded corrections: the exit code is left alone"
    ~expected:0 ~actual:outcome.Run.exit_code;
  match Baseline.writes (Run.baselines outcome.Run.run) with
  | [ Baseline.Written { path = written; literals = 2 } ] ->
      check "one corrected file holds both literals"
        (written = path ^ ".corrected"
        && read_file written
           = "let () =\n\
             \  expect \"x\" (__POS_OF__ {| x |});\n\
             \  expect \"y\" (__POS_OF__ {| y |})\n")
  | _ -> check "one run corrects both literals" false

(* A test leaves the exit code to dune's [diff?] only when each of its
   failures carries a kept correction. Under [-u] an accepted expectation
   raises nothing, so a failure beside it that carries none still fails
   the run: a literal the source refuses, a path outside the root. Under
   [--corrected] a second text for a baseline carries none either. *)
let () =
  with_temp_root @@ fun root ->
  with_project_root root @@ fun () ->
  let base = base_config ~log_dir:(Filename.concat root "logs") () in
  let update = { base with Run.baseline = Baseline.Update } in
  Out_channel.with_open_bin (Filename.concat root "t.ml") (fun oc ->
      output_string oc
        "let () =\n\
        \  expect \"x\" (__POS_OF__ {| old1 |});\n\
        \  expect \"y\" (__POS_OF__ {| edited |})\n");
  let refused =
    test "refused beside accepted" (fun () ->
        expect "x" (("t.ml", 2, 13, 36), " old1 ");
        expect "y" (("t.ml", 3, 13, 36), " old2 "))
  in
  expect_run "a refused literal beside an accepted one" ~config:update
    [ refused ]
  @@ fun outcome ->
  (match failure_list (outcome_of outcome [ "refused beside accepted" ]) with
  | [
   {
     Failure.kind =
       Failure.Baseline { withheld = Some (Failure.Refused { line = 3; _ }); _ };
     _;
   };
  ] ->
      check "update: the refused literal is the test's one failure" true
  | _ -> check "update: the refused literal is the test's one failure" false);
  check_int "update: a refused literal beside an accepted one exits 1"
    ~expected:1 ~actual:outcome.Run.exit_code;
  let outside =
    test "outside beside accepted" (fun () ->
        expect_file "a\n" "a.expected";
        expect_file "b\n" "../outside.expected")
  in
  expect_run "an out-of-root file beside an accepted one" ~config:update
    [ outside ]
  @@ fun outcome ->
  check "update: the file under the root is accepted"
    (read_file (Filename.concat root "a.expected") = "a\n");
  check_int "update: an out-of-root file beside an accepted one exits 1"
    ~expected:1 ~actual:outcome.Run.exit_code;
  let conflict =
    test "two texts" (fun () ->
        expect_file "one\n" "c.expected";
        expect_file "two\n" "c.expected")
  in
  expect_run "a second text for one baseline"
    ~config:{ base with Run.baseline = Baseline.Corrected }
    [ conflict ]
  @@ fun outcome ->
  (match failure_list (outcome_of outcome [ "two texts" ]) with
  | [
   { Failure.kind = Failure.Baseline { withheld = None; _ }; _ };
   {
     Failure.kind = Failure.Baseline { withheld = Some Failure.Conflict; _ };
     _;
   };
  ] ->
      check "corrected: the second text is marked a conflict" true
  | _ -> check "corrected: the second text is marked a conflict" false);
  check_int "corrected: a conflict beside a kept correction exits 1" ~expected:1
    ~actual:outcome.Run.exit_code

(* A baseline check inside a bracket body reaches the run's registry like
   any ambient operation. *)
let () =
  with_temp_root @@ fun root ->
  with_project_root root @@ fun () ->
  let config =
    {
      (base_config ~log_dir:(Filename.concat root "logs") ()) with
      Run.baseline = Baseline.Update;
    }
  in
  let suite =
    [
      bracket
        ~setup:(fun () -> ())
        ~teardown:(fun () -> ())
        "bracketed"
        (fun () -> expect_file "content\n" "src/bracketed.expected");
    ]
  in
  expect_run "baseline in bracket" ~config suite @@ fun outcome ->
  check_int "baseline in bracket: exit code" ~expected:0
    ~actual:outcome.Run.exit_code;
  match Baseline.writes (Run.baselines outcome.Run.run) with
  | [ Baseline.Written { path; literals = 0 } ] ->
      check "the bracket's baseline is accepted under the root"
        (path = Filename.concat root "src/bracketed.expected")
  | _ -> check "bracket baseline accepted a file" false

(* The assertion verbs, body operations and xfail through the facade

   Deep semantics live in test/check and test/structure; this block only
   proves the facade wiring: the new verbs raise their typed claims, the
   body operations dispatch through the ambient slot, and xfail inverts
   what counts as failed. [Windtrap.contains] is qualified because this
   file's local [contains] helper shadows it. *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let observed_path = ref [] in
  let scratch = ref "" in
  let home = Sys.getcwd () in
  let facade_env = ref (Some "unset") in
  let facade_cwd = ref "" in
  let suite =
    [
      test "satisfies" (fun () -> satisfies int (fun n -> n > 0) 0);
      test "contains" (fun () -> Windtrap.contains ~sub:"needle" "a haystack");
      test "require_match" (fun () ->
          ignore (require_match (function 0 -> Some "zero" | _ -> None) 42));
      test "exn predicate" (fun () ->
          raises_match (Exn.invalid_arg ~substring:"boom") (fun () ->
              invalid_arg "kaboom: boom indeed"));
      test "subtests" (fun () ->
          subtest "first" (fun () -> equal int 1 2);
          subtest "second" (fun () -> equal int 3 4));
      test "body operations" (fun () ->
          observed_path := current_test ();
          scratch := temp_dir ();
          is_true (Sys.is_directory !scratch));
      test "scoped process state" (fun () ->
          setenv "WINDTRAP_TEST_FACADE" (Some "inside");
          chdir (temp_dir ());
          facade_env := Sys.getenv_opt "WINDTRAP_TEST_FACADE";
          facade_cwd := cwd_or_gone ());
      xfail ~reason:"known" (test "expected failure" (fun () -> fail "boom"));
      xfail (test "unexpected pass" (fun () -> ()));
    ]
  in
  expect_run "facade wiring" ~config suite @@ fun outcome ->
  (match failure_list (outcome_of outcome [ "satisfies" ]) with
  | [
   {
     Failure.kind =
       Failure.Equality
         { expected = claim; actual = value; diffable = false; _ };
     _;
   };
  ] ->
      check "satisfies renders the rejected value" (value = "0");
      (* The claim sentence is what tells the two predicate verbs apart. *)
      check "satisfies names the predicate claim"
        (claim = "value satisfying the predicate")
  | _ -> check "satisfies carries an undiffable Equality payload" false);
  (match failure_list (outcome_of outcome [ "contains" ]) with
  | [ { Failure.kind = Failure.Containment { needle; _ }; _ } ] ->
      check "contains carries the needle" (needle = "needle")
  | _ -> check "contains carries a Containment payload" false);
  (match failure_list (outcome_of outcome [ "require_match" ]) with
  | [
   {
     Failure.kind = Failure.Equality { expected = claim; diffable = false; _ };
     _;
   };
  ] ->
      check "require_match names the match claim" (claim = "a match")
  | _ -> check "require_match carries an undiffable Equality payload" false);
  check "Exn predicates satisfy raises_match"
    (outcome_of outcome [ "exn predicate" ] = Some Failure.Pass);
  (match failure_list (outcome_of outcome [ "subtests" ]) with
  | [ a; b ] ->
      check "a failing subtest lets its sibling run, labeled parent › name"
        (match (Report.labeled_msg a, Report.labeled_msg b) with
        | Some ma, Some mb ->
            contains "subtests › first" ma && contains "subtests › second" mb
        | _ -> false)
  | fs ->
      check_int "subtest failure entries" ~expected:2 ~actual:(List.length fs));
  check "current_test is the executing test's path"
    (!observed_path = [ "body operations" ]);
  check "temp_dir was removed after its test"
    (!scratch <> "" && not (Sys.file_exists !scratch));
  check "setenv bound inside the test and was undone after it"
    (!facade_env = Some "inside" && Sys.getenv_opt "WINDTRAP_TEST_FACADE" = None);
  check "chdir took effect inside the test and was undone after it"
    (!facade_cwd <> "" && !facade_cwd <> home && cwd_or_gone () = home);
  check "an expected failure does not count as failed"
    (not (List.mem "expected failure" (failed_paths outcome)));
  check "an unexpected pass counts as failed"
    (List.mem "unexpected pass" (failed_paths outcome));
  check_int "facade wiring exit code" ~expected:1 ~actual:outcome.Run.exit_code

(* Their edges

   The corners the happy paths above do not reach: empty needles, a raising
   extractor, xfail composed with cases and with slow selection, and
   scratch paths in teardown phases. *)

(* Empty needles: contained in every string for [contains], so
   [not_contains ~sub:""] always fails. *)
let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite =
    [
      test "empty needle contained" (fun () ->
          Windtrap.contains ~sub:"" "";
          Windtrap.contains ~sub:"" "anything");
      test "empty needle not_contains" (fun () -> not_contains ~sub:"" "x");
    ]
  in
  expect_run "empty needles" ~config suite @@ fun outcome ->
  check "the empty needle is contained in every string"
    (outcome_of outcome [ "empty needle contained" ] = Some Failure.Pass);
  match failure_list (outcome_of outcome [ "empty needle not_contains" ]) with
  | [ { Failure.kind = Failure.Containment _; _ } ] ->
      check "not_contains with an empty needle always fails" true
  | _ -> check "not_contains with an empty needle always fails" false

(* A raising extractor propagates unchanged out of [require_match]: the
   test fails with the extractor's exception, not a match failure. *)
let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite =
    [
      test "raising extractor" (fun () ->
          ignore (require_match (fun _ -> raise Not_found) 42));
    ]
  in
  expect_run "raising extractor" ~config suite @@ fun outcome ->
  match failure_list (outcome_of outcome [ "raising extractor" ]) with
  | [ { Failure.kind = Failure.Raise { actual = Some rendered; _ }; _ } ] ->
      check "require_match propagates the extractor's exception"
        (contains "Not_found" rendered)
  | _ -> check "require_match propagates the extractor's exception" false

(* xfail through a [cases] group marks every child; the inversion is per
   test — a passing child is an unexpected pass and counts as failed. *)
let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite =
    [
      xfail ~reason:"issue #7"
        (cases "mixed" ~name:string_of_int [ 1; 2; 3 ] (fun n ->
             if n = 2 then fail "boom"));
    ]
  in
  expect_run "xfail over cases" ~config suite @@ fun outcome ->
  check "the failing child is excused"
    (not (List.mem "mixed › 2" (failed_paths outcome)));
  check "each passing child is an unexpected pass"
    (List.mem "mixed › 1" (failed_paths outcome)
    && List.mem "mixed › 3" (failed_paths outcome));
  check_int "xfail over cases exit code" ~expected:1
    ~actual:outcome.Run.exit_code

(* xfail composes with [slow]: the tag survives the annotation, so
   [--exclude-tag slow] deselects the test before the inversion could
   apply. *)
let () =
  with_temp_root @@ fun root ->
  let config =
    { (base_config ~log_dir:root ()) with Run.exclude_tags = [ "slow" ] }
  in
  let suite =
    [
      test "fast" (fun () -> ());
      xfail (slow "sluggish" (fun () -> fail "known"));
    ]
  in
  expect_run "xfail over an excluded slow tag" ~config suite @@ fun outcome ->
  check "--exclude-tag slow drops an xfail-marked slow test"
    (outcome_of outcome [ "sluggish" ] = None);
  check_int "xfail over an excluded slow tag: exit code" ~expected:0
    ~actual:outcome.Run.exit_code

(* Scratch paths work in every phase of a test attempt — a bracket
   teardown included — and are removed with the attempt. *)
let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let teardown_scratch = ref "" in
  let suite =
    [
      bracket
        ~setup:(fun () -> ())
        ~teardown:(fun () ->
          let dir = temp_dir () in
          teardown_scratch := dir;
          is_true (Sys.is_directory dir))
        "teardown scratch"
        (fun () -> ());
    ]
  in
  expect_run "temp_dir in bracket teardown" ~config suite @@ fun outcome ->
  check_int "temp_dir works in a bracket teardown" ~expected:0
    ~actual:outcome.Run.exit_code;
  check "teardown scratch is removed with the attempt"
    (!teardown_scratch <> "" && not (Sys.file_exists !teardown_scratch))

(* A fixture release runs outside any test attempt: [temp_dir] there hits
   the ambient guard, surfacing as a Release-phase failure — the run fails
   loudly instead of leaking or crashing. *)
let release_wants_scratch =
  fixture ~teardown:(fun () -> ignore (temp_dir ())) (fun () -> ())

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite = [ test "touch" (fun () -> release_wants_scratch ()) ] in
  expect_run "temp_dir in fixture release" ~config suite @@ fun outcome ->
  check "temp_dir in a fixture release is a Release-phase failure"
    (match outcome.Run.release_failures with
    | [ f ] -> f.Failure.phase = Failure.Release
    | _ -> false);
  check_int "temp_dir in fixture release exits 1" ~expected:1
    ~actual:outcome.Run.exit_code

(* [run] returns the exit code (design review 3.2) *)

let () =
  (* Every path out of [run] is a returned code, never an exit: the four
     informational and refusal pages, then the verdicts — and a second
     suite runs in the same process after the first, which is what a
     returned code is for. *)
  with_temp_root @@ fun root ->
  let pass = test "passes" (fun () -> is_true true) in
  let boom = test "boom" (fun () -> equal int 1 2) in
  let code, out, err = run_in_process ~argv:[ "--help" ] root "codes" [] in
  check_int "--help returns 0" ~expected:0 ~actual:code;
  check_contains "--help prints the page on stdout, flushed"
    ~sub:"usage: codes [OPTIONS] [PATTERN]" out;
  check_string "--help prints nothing on stderr" ~expected:"" ~actual:err;
  let code, out, _ = run_in_process ~argv:[ "--version" ] root "codes" [] in
  check_int "--version returns 0" ~expected:0 ~actual:code;
  check_contains "--version prints its line, flushed" ~sub:"windtrap " out;
  let code, out, err =
    run_in_process ~argv:[ "--nosuchflag" ] root "codes" []
  in
  check_int "a parse error returns 2" ~expected:2 ~actual:code;
  check_string "a parse error prints nothing on stdout" ~expected:"" ~actual:out;
  check_contains "a parse error names the option on stderr"
    ~sub:"unknown option '--nosuchflag'" err;
  let code, out, err =
    run_in_process ~argv:[ "--timeout"; "x" ] root "codes" []
  in
  check_int "a resolution error returns 2" ~expected:2 ~actual:code;
  check_string "a resolution error prints nothing on stdout" ~expected:""
    ~actual:out;
  check_contains "a resolution error names the value on stderr"
    ~sub:"invalid value 'x' for" err;
  let code, out, err = run_in_process root "codes" [ pass ] in
  check_int "a green suite returns 0" ~expected:0 ~actual:code;
  check_contains "and its transcript is complete when run returns"
    ~sub:"codes: 1 passed in " out;
  check_string "a green suite prints nothing on stderr" ~expected:"" ~actual:err;
  let code, out, _ = run_in_process root "codes" [ pass; boom ] in
  check_int "a failing suite returns 1" ~expected:1 ~actual:code;
  check_contains "with the failure block in its transcript" ~sub:"  FAIL  boom"
    out;
  let code, out, _ = run_in_process root "second" [ pass ] in
  check_int "a second suite in the same process returns its own code"
    ~expected:0 ~actual:code;
  check_contains "and prints its own transcript" ~sub:"second: 1 passed in " out

(* A list-only run prints the selection and nothing else *)

let () =
  (* A list run selects and stops — the facade answers it from
     [Run.list_selection], before the drive spine — so the whole
     transcript must be the paths and nothing else: no header, no
     summary line. *)
  with_temp_root @@ fun root ->
  let suite =
    [
      group "outer" [ test "picked" (fun () -> is_true true) ];
      test "other" (fun () -> is_true true);
    ]
  in
  let code, out, err = run_in_process ~argv:[ "-l" ] root "listsuite" suite in
  check_int "a list run returns 0" ~expected:0 ~actual:code;
  check_string "the transcript is the selection, in declaration order"
    ~expected:"outer \u{203a} picked\nother\n" ~actual:out;
  check_string "a list run prints nothing on stderr" ~expected:"" ~actual:err;
  (* A path is one line of the listing whatever its name holds. *)
  let _, out, _ =
    run_in_process ~argv:[ "-l" ] root "listcontrol"
      [ test "first\nhalf" (fun () -> is_true true) ]
  in
  check_string "a listed path escapes its control bytes"
    ~expected:"first\\x0ahalf\n" ~actual:out;
  (* And over an empty selection: a listing that answered a mistyped
     filter with silence is the dead end the empty run's own [list:] hint
     leads to. The sentence is windtrap's own, so it goes to stderr, and
     stdout stays what a reader of paths expects: empty. *)
  let code, out, err =
    run_in_process ~argv:[ "-l"; "-f"; "zzznope" ] root "listsuite" suite
  in
  check_int "an empty listing still returns 0" ~expected:0 ~actual:code;
  check_string "an empty listing prints nothing on stdout" ~expected:""
    ~actual:out;
  check_string "an empty listing says why it is empty, on stderr"
    ~expected:
      "windtrap: no tests ran: filter \"zzznope\" matched none of 2 tests.\n"
    ~actual:err

(* An empty selection says why it is empty *)

let () =
  (* The description is the library runner's own header policy — the
     inline runner passes [None] — so it reaches the renderer through the
     driver's call and nowhere else. The pin is the whole transcript: the
     sentence naming the filter and the denominator, and the [list:] hint
     under it, the one line allowed after an outcome, spelled with the
     launcher as typed. *)
  with_temp_root @@ fun root ->
  let code, out, err =
    run_in_process ~argv:[ "-f"; "zzznope" ] root "emptysuite"
      [
        test "picked" (fun () -> is_true true);
        test "other" (fun () -> is_true true);
      ]
  in
  check_int "an empty selection returns 2" ~expected:2 ~actual:code;
  check_string "the summary names the filter, the count, and the way out"
    ~expected:
      "emptysuite: no tests ran: filter \"zzznope\" matched none of 2 tests.\n\
       list: emptysuite -l\n"
    ~actual:out;
  check_string "an empty selection prints nothing on stderr" ~expected:""
    ~actual:err

(* Under --corrected — a build action's run — a selection that runs none
   of the suite's tests exits 0 rather than 2, still saying why; without
   the flag it exits 2 as before, a usage error exits 2 either way, and a
   suite that declares no tests keeps its 2. *)
let () =
  with_temp_root @@ fun root ->
  let suite = [ test "passes" (fun () -> is_true true) ] in
  let code, out, _ =
    run_in_process ~argv:[ "-f"; "zzznope" ] root "emptied" suite
  in
  check_int "an emptied selection exits 2 by default" ~expected:2 ~actual:code;
  check_string "and says why, then how to list what there is"
    ~expected:
      "emptied: no tests ran: filter \"zzznope\" matched none of 1 test.\n\
       list: emptied -l\n"
    ~actual:out;
  let code, out, _ =
    run_in_process ~argv:[ "-f"; "zzznope"; "--corrected" ] root "emptied" suite
  in
  check_int "under --corrected an emptied selection exits 0" ~expected:0
    ~actual:code;
  check_string
    "and still says why, then names the flag: a build action has no launcher \
     to restate"
    ~expected:
      "emptied: no tests ran: filter \"zzznope\" matched none of 1 test.\n\
       (list the suite's tests with -l)\n"
    ~actual:out;
  let code, _, err =
    run_in_process ~argv:[ "--corrected"; "--nosuchflag" ] root "emptied" suite
  in
  check_int "a usage error under --corrected still exits 2" ~expected:2
    ~actual:code;
  check_contains "and names the option" ~sub:"unknown option '--nosuchflag'" err;
  let code, _, _ = run_in_process ~argv:[ "--corrected" ] root "emptied" [] in
  check_int "a suite that declares no tests exits 2 under --corrected too"
    ~expected:2 ~actual:code

(* The promotion warning *)

let () =
  (* dune promotes a correction only from an action that exits 0, so a
     [--corrected] run that wrote one and returns 1 says so on stderr, once,
     whatever a block's [accept:] line offered; no other run does. Every
     scenario has its own baseline file: [-u] leaves one behind. *)
  with_temp_root @@ fun root ->
  with_project_root root @@ fun () ->
  let stale name =
    let file = name ^ ".expected" in
    Out_channel.with_open_bin (Filename.concat root file) (fun oc ->
        output_string oc "old\n");
    test name (fun () -> expect_file "new\n" file)
  in
  let boom = test "boom" (fun () -> equal int 1 2) in
  let warning ~corrections =
    "windtrap: warning: dune registers a correction for promotion only when \
     the run that wrote it exits 0, so the failures above withhold the "
    ^ corrections
    ^ " written here. Fix the failures, rerun, then 'dune promote'.\n"
  in
  let run argv suite = run_in_process ~argv root "promotion" suite in
  let last_line out =
    match List.rev (String.split_on_char '\n' (String.trim out)) with
    | last :: _ -> last
    | [] -> ""
  in
  let code, out, err = run [ "--corrected" ] [ stale "one"; boom ] in
  check_int "a correction beside another test's failure: the run returns 1"
    ~expected:1 ~actual:code;
  check_contains "its block offered the promotion"
    ~sub:"    accept: dune promote one.expected\n" out;
  check_contains "the summary is still the last line of the report"
    ~sub:"2 failed, 1 correction written in " (last_line out);
  check_string "and stderr is the one warning"
    ~expected:(warning ~corrections:"correction")
    ~actual:err;
  let code, _, err =
    run [ "--corrected" ] [ stale "two-a"; stale "two-b"; boom ]
  in
  check_int "two corrections beside a failure: the run returns 1" ~expected:1
    ~actual:code;
  check_string "the warning agrees in number"
    ~expected:(warning ~corrections:"corrections")
    ~actual:err;
  let code, _, err = run [ "--corrected" ] [ stale "alone" ] in
  check_int "corrections alone leave the exit code to the diff" ~expected:0
    ~actual:code;
  check_string "so dune will promote them, and nothing is said" ~expected:""
    ~actual:err;
  let masked =
    test "masked" (fun () ->
        expect_file "new\n" "masked.expected";
        equal int 1 2)
  in
  let code, _, err = run [ "--corrected" ] [ masked; boom ] in
  check_int "failures and no correction written: the run returns 1" ~expected:1
    ~actual:code;
  check_string "nothing was written, so nothing is said" ~expected:""
    ~actual:err;
  let code, _, err = run [] [ stale "checked"; boom ] in
  check_int "plain checking returns 1" ~expected:1 ~actual:code;
  check_string "and says nothing: it writes no correction" ~expected:""
    ~actual:err;
  let code, out, err = run [ "-u" ] [ stale "accepted"; boom ] in
  check_int "-u beside a failure returns 1" ~expected:1 ~actual:code;
  check_contains "having accepted in place" ~sub:"1 correction accepted in " out;
  check_string "which no exit code undoes, so nothing is said" ~expected:""
    ~actual:err

(* Signals *)

(* INT, TERM and HUP end a run on what it knows: one [windtrap:] line
   naming the stopped test, the summary with what did not run, and a death
   by the same signal, so the parent sees the signal and not a code. *)

let signal_child root mode signal_it =
  let file name =
    Unix.openfile
      (Filename.concat root name)
      [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
      0o600
  in
  let out = file "out" and err = file "err" in
  let pid =
    Unix.create_process Sys.executable_name
      [| Sys.executable_name; "--signal-child"; root; mode |]
      Unix.stdin out err
  in
  Unix.close out;
  Unix.close err;
  signal_it pid;
  let status = snd (Unix.waitpid [] pid) in
  ( status,
    read_file (Filename.concat root "out"),
    read_file (Filename.concat root "err") )

(* Signals [pid] once its third test is waiting; a child that never gets
   there is killed, and its status fails the checks below. *)
let once_waiting root signal pid =
  let ready = Filename.concat root "ready" in
  let rec await tries =
    if Sys.file_exists ready then Unix.kill pid signal
    else if tries = 0 then Unix.kill pid Sys.sigkill
    else begin
      Unix.sleepf 0.01;
      await (tries - 1)
    end
  in
  await 2000

let () =
  if Sys.win32 then skip_scenario ~reason:"POSIX only" __POS__
  else (
    List.iter
      (fun (name, signal) ->
        with_temp_root @@ fun root ->
        let status, out, err =
          signal_child root "waiting" (once_waiting root signal)
        in
        check
          (name ^ ": the run dies by the signal it got")
          (status = Unix.WSIGNALED signal);
        check_string
          (name ^ ": stderr is the one line naming the stopped test")
          ~expected:"windtrap: interrupted in deep \u{203a} waits\n" ~actual:err;
        check_contains
          (name ^ ": the failure so far was already committed, under its rule")
          ~sub:
            "signals: 4 tests\n\
             ──────────────────────── failures ────────────────────────\n\
            \  FAIL  fails\n"
          out;
        check_contains
          (name ^ ": the closing rule still prints, before the summary")
          ~sub:
            "    actual    2\n\
             ──────────────────────────────────────────────────────────\n\n\
             1 passed, 1 failed, 2 not run in "
          out;
        let last =
          List.nth
            (String.split_on_char '\n' out)
            (List.length (String.split_on_char '\n' out) - 2)
        in
        check
          (name ^ ": the summary is the last line and counts what did not run: "
         ^ last)
          (String.starts_with ~prefix:"1 passed, 1 failed, 2 not run in " last
          && String.ends_with ~suffix:"." last);
        check
          (name ^ ": the stopped test's bytes stay in its log")
          ((not (contains "captured, never shown" out))
          && contains "captured, never shown"
               (read_file
                  (List.fold_left Filename.concat root
                     [ "signals"; "deep"; "waits.output" ])));
        check
          (name ^ ": the stopped attempt's scratch directory is removed")
          (not
             (Array.exists
                (String.starts_with ~prefix:"windtrap-")
                (Sys.readdir root))))
      [ ("INT", Sys.sigint); ("TERM", Sys.sigterm); ("HUP", Sys.sighup) ];
    (* A signal that arrives in the executor's own code, here an observer,
       is honoured before the next test starts; the observer's exception on
       the Interrupted event is ignored. *)
    ( with_temp_root @@ fun root ->
      let status, out, err = signal_child root "between" ignore in
      check "between tests: the run dies by the signal"
        (status = Unix.WSIGNALED Sys.sigterm);
      check_string "between tests: stderr says so"
        ~expected:"windtrap: interrupted between tests\n" ~actual:err;
      check
        ("between tests: one line, the test that finished counted: " ^ out)
        (String.starts_with ~prefix:"signals: 1 passed, 2 not run in " out
        && List.length (String.split_on_char '\n' out) = 2) );
    (* A signal that stops a fixture's release names it, and the fixtures
       still held are released: the one in flight is not re-entered. *)
    ( with_temp_root @@ fun root ->
      let status, out, err =
        signal_child root "releasing" (once_waiting root Sys.sigterm)
      in
      check "in a release: the run dies by the signal"
        (status = Unix.WSIGNALED Sys.sigterm);
      check
        ("in a release: stderr names the fixture: " ^ err)
        (String.starts_with
           ~prefix:"windtrap: interrupted while releasing fixture (" err
        && List.length (String.split_on_char '\n' err) = 2);
      check
        ("in a release: every test had finished: " ^ out)
        (String.starts_with ~prefix:"signals: 1 passed in " out);
      check "in a release: the fixtures still held are released"
        (Sys.file_exists (Filename.concat root "first released")
        && Sys.file_exists (Filename.concat root "second released")) );
    (* A signal the process was started ignoring stays ignored. *)
    with_temp_root @@ fun root ->
    let survived = ref false in
    let status, _, err =
      signal_child root "ignored-hup" (fun pid ->
          once_waiting root Sys.sighup pid;
          Unix.sleepf 0.2;
          survived := fst (Unix.waitpid [ Unix.WNOHANG ] pid) = 0;
          Unix.kill pid Sys.sigterm)
    in
    check "an ignored SIGHUP stays ignored" !survived;
    check "and the run then dies by the SIGTERM that followed"
      (status = Unix.WSIGNALED Sys.sigterm);
    check_string "having said so once"
      ~expected:"windtrap: interrupted in deep \u{203a} waits\n" ~actual:err)

(* What a signal leaves: the interrupted test's teardown does not run, no
   correction is written, the store is not updated, and the test's
   binding is still there when the fixtures are released. *)
let () =
  if Sys.win32 then skip_scenario ~reason:"POSIX only" __POS__
  else
    with_temp_root @@ fun root ->
    let status, _, _ =
      signal_child root "leftovers" (once_waiting root Sys.sigterm)
    in
    check "leftovers: the run dies by the signal"
      (status = Unix.WSIGNALED Sys.sigterm);
    check "the interrupted test's teardown does not run"
      (not (Sys.file_exists (Filename.concat root "teardown ran")));
    check "no correction is written"
      (not (Sys.file_exists (Filename.concat root "c.expected.corrected")));
    check "the store is not updated"
      (not
         (Sys.file_exists
            (List.fold_left Filename.concat root
               [ "logs"; "signals"; ".last-failed" ])));
    check_string "and what setenv changed stays" ~expected:"set by the test"
      ~actual:
        (if Sys.file_exists (Filename.concat root "env at release") then
           read_file (Filename.concat root "env at release")
         else "<no release>")

(* The first signal restores the default dispositions: a second one, while
   a release hangs, kills at once. *)
let () =
  if Sys.win32 then skip_scenario ~reason:"POSIX only" __POS__
  else
    with_temp_root @@ fun root ->
    let status, _, _ =
      signal_child root "second" (fun pid ->
          once_waiting root Sys.sigterm pid;
          let releasing = Filename.concat root "releasing" in
          let rec await tries =
            if Sys.file_exists releasing then Unix.kill pid Sys.sigint
            else if tries = 0 then Unix.kill pid Sys.sigkill
            else begin
              Unix.sleepf 0.01;
              await (tries - 1)
            end
          in
          await 2000)
    in
    check "a second signal kills at once" (status = Unix.WSIGNALED Sys.sigint)

(* A process a test forks inherits the handlers and not the run: killed, it
   dies silently, as it did before a run handled signals, and the run goes
   on. *)
let () =
  if Sys.win32 then skip_scenario ~reason:"POSIX only" __POS__
  else
    with_temp_root @@ fun root ->
    let reaped = ref None in
    let code, out, err =
      run_in_process root "forks"
        [
          test "kills the process it forked" (fun () ->
              match Unix.fork () with
              | 0 ->
                  Unix.sleepf 60.;
                  Unix._exit 0
              | pid ->
                  Unix.sleepf 0.05;
                  Unix.kill pid Sys.sigterm;
                  reaped := Some (snd (Unix.waitpid [] pid)));
        ]
    in
    check "the forked process died by the signal"
      (!reaped = Some (Unix.WSIGNALED Sys.sigterm));
    check_int "and the run went on to pass" ~expected:0 ~actual:code;
    check_string "saying nothing on stderr" ~expected:"" ~actual:err;
    check
      ("and its one line on stdout: " ^ out)
      (String.starts_with ~prefix:"forks: 1 passed in " out
      && List.length (String.split_on_char '\n' out) = 2)

(* The handlers are the run's: what was installed before it is back after. *)
let () =
  if Sys.win32 then skip_scenario ~reason:"POSIX only" __POS__
  else
    with_temp_root @@ fun root ->
    let mine (_ : int) = () in
    let signals = [ Sys.sigint; Sys.sigterm; Sys.sighup ] in
    let before =
      List.map (fun s -> Sys.signal s (Sys.Signal_handle mine)) signals
    in
    let code, _, _ =
      run_in_process root "handlers" [ test "passes" (fun () -> is_true true) ]
    in
    check_int "the run passes" ~expected:0 ~actual:code;
    List.iter2
      (fun s previous ->
        check "a run leaves the handler it found"
          (match Sys.signal s previous with
          | Sys.Signal_handle f -> f == mine
          | Sys.Signal_default | Sys.Signal_ignore -> false))
      signals before

(* Where a helper's failure is located. The lines are found in this file's
   copy beside the executable, by the markers in their comments. *)

let source_line marker =
  let text =
    read_file
      (Filename.concat
         (Filename.dirname Sys.executable_name)
         "test_windtrap.ml")
  in
  let rec find n = function
    | [] -> 0
    | line :: rest -> if contains marker line then n else find (n + 1) rest
  in
  find 1 (String.split_on_char '\n' text)

let[@inline never] own_line_helper x =
  is_true x (* helper: own line *);
  ()

let[@inline never] tail_helper x = is_true x
let[@inline never] pos_helper ?__POS__ x = is_true ?__POS__ x

let[@inline never] calls_tail_helper () =
  tail_helper false (* helper: caller *);
  ()

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let tests =
    [
      test "own line" (fun () -> own_line_helper false);
      test "tail" calls_tail_helper;
      test "passed on" (fun () ->
          pos_helper ~__POS__:("given.ml", 7, 0, 0) false;
          ());
    ]
  in
  expect_run "helper location suite runs" ~config tests @@ fun outcome ->
  let line path =
    match failure_list (outcome_of outcome path) with
    | [ { Failure.loc = Some l; _ } ] -> (l.Loc.file, l.Loc.line)
    | _ -> ("<none>", 0)
  in
  check_int "a helper that wraps a verb reports its own line"
    ~expected:(source_line ("(* helper: " ^ "own line *)"))
    ~actual:(snd (line [ "own line" ]));
  check_int "a wrapped call in tail position reports the caller's line"
    ~expected:(source_line ("(* helper: " ^ "caller *)"))
    ~actual:(snd (line [ "tail" ]));
  check "a helper that passes ?__POS__ on reports the caller's location"
    (line [ "passed on" ] = ("given.ml", 7))

(* The witness's printer and equality, as the verbs use them *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let raising =
    Testable.make ~pp:(fun _ _ -> failwith "pp boom") ~equal:( = )
  in
  expect_run "raising printer suite runs" ~config
    [ test "prints" (fun () -> equal raising 1 2) ]
  @@ fun outcome ->
  match failure_list (outcome_of outcome [ "prints" ]) with
  | [ f ] ->
      let block =
        Pp.str "%a" (fun ppf f -> Report.pp_failure ~ansi:false ppf f) f
      in
      check_contains "a raising printer replaces the failure"
        ~sub:"uncaught exception:" block;
      check_contains "with that exception" ~sub:"pp boom" block;
      check "and no value" (not (contains "expected" block))
  | _ -> check "one failure" false

let () =
  let pairs = ref [] in
  let recording =
    Testable.make ~pp:Format.pp_print_int ~equal:(fun a b ->
        pairs := (a, b) :: !pairs;
        true)
  in
  equal recording 1 2;
  check "the equality takes the expected value first" (!pairs = [ (1, 2) ]);
  pairs := [];
  mem recording 1 [ 2; 3 ];
  check "mem passes x as the expected value"
    (List.for_all (fun (a, _) -> a = 1) !pairs && !pairs <> [])

(* A tag named by both flags is excluded, in either order: it is dropped,
   and no longer required. *)

let () =
  with_temp_root @@ fun root ->
  let tests = [ test ~tags:[ "x" ] "tagged" ignore; test "untagged" ignore ] in
  List.iter
    (fun argv ->
      let _, listed, _ =
        run_in_process ~argv:("-l" :: argv) root "both" tests
      in
      check_string
        ("a tag named by both flags is excluded: " ^ String.concat " " argv)
        ~expected:"untagged\n" ~actual:listed)
    [
      [ "--tag"; "x"; "--exclude-tag"; "x" ];
      [ "--exclude-tag"; "x"; "--tag"; "x" ];
    ]

(* cases evaluates at declaration, outside any test *)

let () =
  expect_invalid_arg "temp_dir in a cases name raises at declaration" (fun () ->
      cases
        ~name:(fun _ ->
          ignore (temp_dir ());
          "x")
        "c" [ 1 ] ignore);
  expect_invalid_arg "setenv too" (fun () ->
      cases
        ~name:(fun _ ->
          setenv "WINDTRAP_TEST_CASES" (Some "x");
          "x")
        "c" [ 1 ] ignore)

(* A fixture's failed create, raised again with its first backtrace *)

let[@inline never] failing_create () =
  ignore (failwith "no database");
  ()

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let db = fixture failing_create in
  expect_run "failed fixture suite runs" ~config
    [ test "first" (fun () -> db ()); test "second" (fun () -> db ()) ]
  @@ fun outcome ->
  match failure_list (outcome_of outcome [ "second" ]) with
  | [ { Failure.kind = Failure.Raise { backtrace = Some bt; _ }; _ } ] ->
      check_contains "the second call carries the first create's backtrace"
        ~sub:"failing_create" bt
  | _ -> check "a Raise failure with a backtrace" false

(* Retries of a property replay the same cases *)

let thirds l =
  let n = List.length l / 3 in
  let rec take k l =
    if k = 0 then [] else List.hd l :: take (k - 1) (List.tl l)
  in
  let rec drop k l = if k = 0 then l else drop (k - 1) (List.tl l) in
  (take n l, take n (drop n l), drop (2 * n) l)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let drawn = ref [] and called = ref [] in
  let tests =
    [
      group ~retries:2 "g"
        [
          prop ~count:5 "p" (Gen.int_range 0 1000) (fun x ->
              drawn := x :: !drawn;
              is_true (x < 0));
          stateful ~count:3 ~steps:5 "s" ~model:0
            ~scope:(fun k -> k (ref 0))
            [
              command "add" (Gen.int_range 0 9) ~next:( + ) (fun _ x r ->
                  called := x :: !called;
                  r := !r + x;
                  is_true (!r < 5));
            ];
        ];
    ]
  in
  expect_run "retried properties suite runs" ~config tests @@ fun outcome ->
  let attempts path =
    match result_of outcome path with Some r -> r.Run.attempts | None -> 0
  in
  check_int "a prop inherits the group's retries" ~expected:3
    ~actual:(attempts [ "g"; "p" ]);
  check_int "so does a stateful test" ~expected:3
    ~actual:(attempts [ "g"; "s" ]);
  let a, b, c = thirds (List.rev !drawn) in
  check "every retry of a prop replays the same cases"
    (a <> [] && a = b && b = c);
  let a, b, c = thirds (List.rev !called) in
  check "every retry of a stateful test replays the same programs"
    (a <> [] && a = b && b = c)

(* Gen's documented frequencies and bound *)

let () =
  let module Engine = Gen_engine in
  let draws gen n =
    let rec go state k acc =
      if k = 0 then List.rev acc
      else
        let tree, state = Engine.run gen state in
        go state (k - 1) (Engine.Shrink_tree.root tree :: acc)
    in
    go (Seed.make 0x5eedL) n []
  in
  let share p l =
    float_of_int (List.length (List.filter p l)) /. float_of_int (List.length l)
  in
  let none = share Option.is_none (draws (Gen.option Gen.int) 10_000) in
  check
    ("Gen.option gives None with probability 0.15: " ^ string_of_float none)
    (none > 0.13 && none < 0.17);
  let ok = share Result.is_ok (draws (Gen.result Gen.int Gen.int) 10_000) in
  check
    ("Gen.result gives Ok with probability 0.75: " ^ string_of_float ok)
    (ok > 0.73 && ok < 0.77);
  let tries = ref 0 in
  let never =
    Gen.such_that
      (fun _ ->
        incr tries;
        false)
      Gen.int
  in
  (match Engine.sample never (Seed.make 0x5eedL) with
  | _ -> check "such_that gives up" false
  | exception Failure.Control `Discard -> ());
  check_int "such_that tries at most 100 draws" ~expected:100 ~actual:!tries

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let branch tag =
    Gen.with_pp (fun ppf -> Format.fprintf ppf "%s:%d" tag) Gen.int
  in
  expect_run "examples printer suite runs" ~config
    [
      prop ~count:0 ~examples:[ 5 ] "example"
        (Gen.one_of [ branch "first"; branch "second" ])
        (fun _ -> is_true false);
    ]
  @@ fun outcome ->
  match failure_list (outcome_of outcome [ "example" ]) with
  | [ f ] ->
      check_contains "an example under one_of prints with the first branch's"
        ~sub:"first:5"
        (Pp.str "%a" (fun ppf f -> Report.pp_failure ~ansi:false ppf f) f)
  | _ -> check "one failure" false

(* Corrections that fail the run *)

let () =
  Fun.protect ~finally:clear_env @@ fun () ->
  clear_env ();
  with_temp_root @@ fun root ->
  Unix.putenv "WINDTRAP_PROJECT_ROOT" root;
  close_out (open_out (Filename.concat root "blocker"));
  let code, _, _ =
    run_in_process ~argv:[ "-u" ] root "unwritable"
      [ test "accepts" (fun () -> expect_file "v" "blocker/x.expected") ]
  in
  check_int "a correction that cannot be written fails the run" ~expected:1
    ~actual:code;
  Out_channel.with_open_bin (Filename.concat root "t.ml") (fun oc ->
      output_string oc "let () =\n  expect x @@ __POS_OF__ {| edited |}\n");
  let code, _, _ =
    run_in_process ~argv:[ "-u" ] root "drifted"
      [ test "accepts" (fun () -> expect "new" (("t.ml", 2, 14, 0), " old ")) ]
  in
  check_int "a correction refused because the source changed fails the run"
    ~expected:1 ~actual:code

let () =
  Fun.protect ~finally:clear_env @@ fun () ->
  clear_env ();
  with_temp_root @@ fun root ->
  Unix.putenv "WINDTRAP_PROJECT_ROOT" root;
  Unix.mkdir (Filename.concat root "dir.expected") 0o700;
  Out_channel.with_open_bin (Filename.concat root "rel.expected") (fun oc ->
      output_string oc "v\n");
  let config = base_config ~log_dir:(Filename.concat root "_logs") () in
  let tests =
    [
      test "unreadable" (fun () ->
          raises_match Check.Exn.sys_error (fun () ->
              expect_file "x" "dir.expected"));
      test "moved" (fun () ->
          chdir (temp_dir ());
          expect_file "v" "rel.expected");
    ]
  in
  expect_run "expect_file edges suite runs" ~config tests @@ fun outcome ->
  check "expect_file raises Sys_error on a file it cannot read"
    (outcome_of outcome [ "unreadable" ] = Some Failure.Pass);
  check "a relative expect_file path does not follow chdir"
    (outcome_of outcome [ "moved" ] = Some Failure.Pass)

(* current_test and subtest *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let seen = ref [] in
  let tests =
    [
      test ~retries:1 "t" (fun () ->
          seen := current_test () :: !seen;
          subtest "s" (fun () -> seen := current_test () :: !seen);
          if List.length !seen = 2 then fail "first attempt");
      prop ~count:20 "law" (Gen.int_range 0 1000) (fun x ->
          subtest "s" (fun () -> is_true (x < 10)));
    ]
  in
  expect_run "current_test and subtest suite runs" ~config tests
  @@ fun outcome ->
  check "current_test is the same in every attempt and in a subtest"
    (List.length !seen = 4 && List.for_all (fun p -> p = [ "t" ]) !seen);
  let fs = failure_list (outcome_of outcome [ "law" ]) in
  check "a subtest failure inside a law fails the test unshrunk"
    (fs <> []
    && List.for_all
         (fun (f : Failure.t) ->
           f.Failure.subtest = [ "law"; "s" ]
           &&
           match f.Failure.kind with
           | Failure.Property _ -> false
           | _ -> true)
         fs)

(* The last failed tests under --corrected and -x *)

let () =
  Fun.protect ~finally:clear_env @@ fun () ->
  clear_env ();
  with_temp_root @@ fun root ->
  Unix.putenv "WINDTRAP_PROJECT_ROOT" root;
  let tests =
    [
      test "corrects" (fun () -> expect_file "new\n" "c.expected");
      test "later" ignore;
    ]
  in
  let _, out, _ =
    run_in_process ~argv:[ "--corrected"; "-x" ] root "kept" tests
  in
  check_contains "a test of kept corrections still stops -x" ~sub:"1 not run"
    out;
  let _, listed, _ =
    run_in_process ~argv:[ "-l"; "--failed" ] root "kept" tests
  in
  check_string "and enters the last failed tests" ~expected:"corrects\n"
    ~actual:listed

let () =
  with_temp_root @@ fun root ->
  let fails name = test name (fun () -> equal int 1 2) in
  let tests = [ fails "a"; fails "b" ] in
  ignore (run_in_process root "bailed" tests);
  ignore (run_in_process ~argv:[ "-x" ] root "bailed" tests);
  let _, listed, _ =
    run_in_process ~argv:[ "-l"; "--failed" ] root "bailed" tests
  in
  check_string "a test not executed after -x keeps its entry" ~expected:"a\nb\n"
    ~actual:listed

(* What prints changes no outcome and no exit code *)

let () =
  with_temp_root @@ fun root ->
  let tests =
    [ test "passes" ignore; test "fails" (fun () -> equal int 1 2) ]
  in
  let code argv =
    let c, _, _ = run_in_process ~argv root "printing" tests in
    c
  in
  let plain = code [] in
  check_int "a failing run" ~expected:1 ~actual:plain;
  List.iter
    (fun argv ->
      check_int
        ("the exit code under " ^ String.concat " " argv)
        ~expected:plain ~actual:(code argv))
    [
      [ "-v" ];
      [ "--color"; "always" ];
      [ "--slow-threshold"; "0" ];
      [ "-s" ];
      [ "--junit"; Filename.concat root "junit.xml" ];
    ]

(* A stateful test through the runner: its summary is the program's, and a
   count of 0 draws no case and opens no system. The engine's outcome is
   internal to [Run]; the result is what a run records of it. *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let scopes = ref 0 in
  let counting run =
    incr scopes;
    run ()
  in
  let tick = [ call "tick" ~next:succ (fun model () -> is_true (model < 2)) ] in
  let suite =
    [
      stateful ~count:3 ~steps:3 "summary" ~model:0
        ~scope:(fun run -> run ())
        tick;
      stateful ~count:0 "none" ~model:0 ~scope:counting tick;
    ]
  in
  expect_run "stateful through the runner" ~config suite @@ fun outcome ->
  (match failure_list (outcome_of outcome [ "summary" ]) with
  | [ { Failure.kind = Failure.Property { summary; _ }; _ } ] ->
      check "a stateful failure's summary is the program's summary"
        (summary = Some "3 calls, last: tick")
  | _ -> check "a stateful failure carries a Property payload" false);
  check "~count:0 draws no case"
    (match result_of outcome [ "none" ] with
    | Some { Run.prop_stats = Some stats; _ } -> stats.Property.cases = 0
    | _ -> false);
  check_int "and opens no system" ~expected:0 ~actual:!scopes

(* Summary *)

let () = finish ()
