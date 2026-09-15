(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* End-to-end tests of the Windtrap facade: suites declared with the public
   surface (verbs, testables, prop, expect, output, fixture) executed
   in-process through Runner.execute under a synthetic config, asserting on
   typed outcomes. Plain executable: [run] and [execute] both refuse to
   nest inside an active run, so a windtrap suite could drive neither.
   The facade's [run] returns its exit code, so its command-line paths
   are driven in this process too ([run_in_process] below); only what
   needs a process of its own — an environment of its own, a real exit
   — is re-exec'd as a child. *)

open Windtrap
open Windtrap.Private
open Harness

let () = init "facade"

(* The exit-guard child (D1): re-exec'd with a marker to run the facade's
   [run] on a suite whose second test calls [Stdlib.exit 0], and to exit
   with the code [run] returns — the guard's whole subject is which of
   the two exits ends the process. The parent below asserts on the
   child's status and transcript; never returns for a child invocation. *)
let () =
  match Array.to_list Sys.argv with
  | [ _; "--exit-guard-child"; log_dir ] ->
      clear_env ();
      exit
      @@ Windtrap.run
           ~argv:[| "exit-guard-child"; "-o"; log_dir; "--color"; "never" |]
           "exitguard"
           [
             test "before" (fun () -> is_true true);
             test "bomb" (fun () -> Stdlib.exit 0);
             test "after" (fun () -> equal int 1 2);
           ]
  | _ -> ()

(* The invocation child (D5 §1): re-exec'd to run the facade's [run] on a
   failing suite with a controlled argv0 and INSIDE_DUNE, so the parent
   can assert on the rerun hint's spelling — the invocation is computed at
   startup from exactly these two inputs. *)
let () =
  match Array.to_list Sys.argv with
  | [ _; "--invocation-child"; log_dir; mode ] ->
      clear_env ();
      let argv0 =
        match mode with
        | "standalone" -> "./_build/default/qa/x/t.exe"
        | "dune" ->
            (* dune runs test actions with argv0 [./t.exe] and cwd = the
               test's build directory: the concatenation carries a [/./]
               unless the spelling is normalized (render/F-3). *)
            Unix.putenv "INSIDE_DUNE" "1";
            "./t.exe"
        | "mirrors" -> ""
        | _ -> assert false
      in
      let argv =
        if argv0 = "" then [||]
        else [| argv0; "-o"; log_dir; "--color"; "never" |]
      in
      (* A property, so the transcript carries a replay line: that is the
         surviving hint the invocation spelling reaches. *)
      exit
      @@ Windtrap.run ~argv "invsuite"
           [ prop "boom" Gen.int (fun _ -> equal int 1 2) ]
  | _ -> ()

(* The xpass-collision child (F4): re-exec'd to run the facade's [run] on
   an xfail test whose real failure message equals the runner's synthesized
   unexpected-pass string. Classification is record-driven ([Run.result]'s
   [counted] bit), so the stream must render the failure as excused —
   agreeing with the exit code and the "1 expected failure" summary — never
   reconstruct the decision by matching the message. *)
let () =
  match Array.to_list Sys.argv with
  | [ _; "--xpass-collide-child"; log_dir; level ] ->
      clear_env ();
      let argv =
        Array.of_list
          ([ "collide-child"; "-o"; log_dir; "--color"; "never" ]
          @ if level = "verbose" then [ "--verbose" ] else [])
      in
      exit
      @@ Windtrap.run ~argv "collide"
           [
             xfail
               (test "collide" (fun () ->
                    fail "expected to fail, but the test passed"));
           ]
  | _ -> ()

(* The release-failure child (runner, "fixture releases"): re-exec'd to run
   the facade's [run] on a suite that touches a fixture whose teardown
   raises. Releases run after the last test, and the runner records each
   failure as a result row the moment it happens — the one recorded list
   every sink (renderer, JUnit) projects. The one test passes, so a runner
   that dropped the row would print a clean transcript and count zero JUnit
   failures while still exiting 1: exactly the defect (Law 8). *)
let leaky_release =
  fixture ~teardown:(fun () -> failwith "release-boom") (fun () -> ())

let () =
  match Array.to_list Sys.argv with
  | [ _; "--release-failure-child"; log_dir; junit ] ->
      clear_env ();
      exit
      @@ Windtrap.run
           ~argv:
             [|
               "release-child";
               "-o";
               log_dir;
               "--color";
               "never";
               "--junit";
               junit;
             |]
           "releasesuite"
           [ test "touches the fixture" (fun () -> leaky_release ()) ]
  | _ -> ()

(* Re-exec this executable with [args], returning its exit status and its
   standard output — plus its standard error when [merge_stderr]. *)
let spawn_child ?(merge_stderr = false) args =
  let out_read, out_write = Unix.pipe () in
  let child_stderr = if merge_stderr then out_write else Unix.stderr in
  let pid =
    Unix.create_process Sys.executable_name
      (Array.of_list (Sys.executable_name :: args))
      Unix.stdin out_write child_stderr
  in
  Unix.close out_write;
  let buffer = Buffer.create 1024 in
  let chunk = Bytes.create 4096 in
  let rec drain () =
    let n = Unix.read out_read chunk 0 (Bytes.length chunk) in
    if n > 0 then begin
      Buffer.add_subbytes buffer chunk 0 n;
      drain ()
    end
  in
  drain ();
  Unix.close out_read;
  let status = snd (Unix.waitpid [] pid) in
  (status, Buffer.contents buffer)

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
  Windtrap.Private.Pp.styled_string ~ansi:false `Red "" |> ignore;
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
  match Runner.execute ?on_event ~config ~suite tests with
  | Ok outcome -> f outcome
  | Error error ->
      check name false;
      Printf.printf "  startup error: %s\n%!" (Runner.startup_message error)

let result_of outcome path =
  List.find_opt (fun r -> r.Run.path = path) (Run.results outcome.Runner.run)

(* The end-of-run fixture-release rows the runner records beside the test
   rows (one result model), identified by their subject. *)
let release_rows outcome =
  List.filter
    (fun (r : Run.result) -> r.Run.subject = Run.Fixture_release)
    (Run.results outcome.Runner.run)

(* The paths that counted as failed, in execution order — what the exit
   code and the last-failed store react to. *)
let failed_paths outcome =
  List.filter_map
    (fun (r : Run.result) ->
      if r.Run.subject = Run.Test && r.Run.counted then
        Some (Test_tree.path_to_string r.Run.path)
      else None)
    (Run.results outcome.Runner.run)

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

(* The raising verbs need no ambient state; escaping without a run they
   print the failure's headline, not an opaque constructor. *)
let () =
  (match equal int 1 2 with
  | () -> check "equal outside a run raises" false
  | exception Failure.Check_failure failure ->
      check "equal outside a run raises Check_failure"
        (match failure.Failure.kind with
        | Failure.Equality _ -> true
        | _ -> false);
      let rendered = Printexc.to_string (Failure.Check_failure failure) in
      check "uncaught assertion failures render readably"
        (contains "expected" rendered && contains "windtrap" rendered));
  match skip ~reason:"why" () with
  | _ -> check "skip raises Skip_test" false
  | exception Failure.Skip_test reason ->
      check "uncaught skip renders readably"
        (contains "why" (Printexc.to_string (Failure.Skip_test reason)))

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
  check_int "verbs: exit code" ~expected:0 ~actual:outcome.Runner.exit_code;
  check "verbs: every outcome is Pass"
    (List.for_all
       (fun r -> r.Run.outcome = Failure.Pass)
       (Run.results outcome.Runner.run))

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
  check_int "failing verbs: exit code" ~expected:1
    ~actual:outcome.Runner.exit_code;
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

(* A nonempty selection whose every test skipped exits 0 (Law 11). *)
let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite = [ test "s" (fun () -> skip ()) ] in
  expect_run "all-skipped run" ~config suite @@ fun outcome ->
  check_int "all-skipped run exits 0" ~expected:0
    ~actual:outcome.Runner.exit_code

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
    | Runner.Fixture_release { name } -> releases := name :: !releases
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
  check_int "fixture: exit code" ~expected:0 ~actual:outcome.Runner.exit_code;
  check "fixture: release announced by name"
    (match !releases with [ name ] -> contains "fixture" name | _ -> false);
  (* A later run in the same process re-acquires: the cache is per run. *)
  expect_run "fixture re-acquisition" ~config
    [ test "again" (fun () -> equal int 2 (shared ())) ]
  @@ fun outcome2 ->
  check_int "fixture: re-acquired on the next run" ~expected:2
    ~actual:!shared_calls;
  check_int "fixture: second run exit code" ~expected:0
    ~actual:outcome2.Runner.exit_code

let failing_release =
  fixture ~teardown:(fun _ -> fail "release-boom") (fun () -> ())

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite = [ test "touch" (fun () -> failing_release ()) ] in
  expect_run "release failure" ~config suite @@ fun outcome ->
  check "release failure is a Release-phase entry"
    (match release_rows outcome with
    | [ { Run.outcome = Failure.Fail [ f ]; _ } ] ->
        f.Failure.phase = Failure.Release
    | _ -> false);
  check_int "release failure exits 1" ~expected:1
    ~actual:outcome.Runner.exit_code

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
  check_int "nested prop: exit code" ~expected:0
    ~actual:outcome.Runner.exit_code;
  check "nested prop records stats at its full path"
    (match result_of outcome [ "outer"; "law" ] with
    | Some { Run.prop_stats = Some stats; _ } -> stats.Property.cases = 5
    | _ -> false);
  check "nested prop keeps the prop tag"
    (List.exists
       (fun case ->
         case.Test_tree.path = [ "outer"; "law" ]
         && Tag.mem "prop" case.Test_tree.tags)
       outcome.Runner.selected)

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
  check_int "output (): exit code" ~expected:0 ~actual:outcome.Runner.exit_code

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

let with_project_root root f =
  Unix.putenv "WINDTRAP_PROJECT_ROOT" root;
  Fun.protect ~finally:(fun () -> Unix.putenv "WINDTRAP_PROJECT_ROOT" "") f

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
    check_int "update run exits 0" ~expected:0 ~actual:outcome.Runner.exit_code;
    match Baseline.writes (Run.baselines outcome.Runner.run) with
    | [ { Baseline.path = written; literals = 0 } ] ->
        check "acceptance wrote the file"
          (written = path && read_file path = "hello\n")
    | _ -> check "acceptance recorded one write" false );
  (* 3. Check mode now passes; a changed actual mismatches. *)
  ( expect_run "file baseline green" ~config [ file_test ] @@ fun outcome ->
    check_int "the baseline matches its committed file" ~expected:0
      ~actual:outcome.Runner.exit_code );
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
  let suite =
    [
      test "flexible" (fun () ->
          expect "a\n  b\n"
          @@ __POS_OF__ {|
            a
              b
          |});
      test "exact" (fun () -> expect_exact "a\n" @@ __POS_OF__ "a\n");
      test "stale" (fun () -> expect "new" @@ __POS_OF__ {| old |});
    ]
  in
  expect_run "literal baselines" ~config suite @@ fun outcome ->
  check "flexible and exact match; the stale literal fails"
    (failed_paths outcome = [ "stale" ]);
  match failure_list (outcome_of outcome [ "stale" ]) with
  | [
   ({
      Failure.kind =
        Failure.Baseline
          {
            baseline = Failure.Literal;
            state = Failure.Mismatch { expected; actual };
          };
      _;
    } as f);
  ] ->
      check "the mismatch carries the normalized forms"
        (expected = "old" && actual = "new");
      check "the failure sits at the literal's position"
        (match f.Failure.loc with
        | Some loc -> String.ends_with ~suffix:"test_windtrap.ml" loc.Loc.file
        | None -> false)
  | _ -> check "a stale literal fails with a Literal payload" false

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
    ~actual:outcome.Runner.exit_code;
  match Baseline.writes (Run.baselines outcome.Runner.run) with
  | [ { Baseline.path; literals = 0 } ] ->
      check "the bracket's baseline is accepted under the root"
        (path = Filename.concat root "src/bracketed.expected")
  | _ -> check "bracket baseline accepted a file" false

(* Nested runs *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite = [ test "nests" (fun () -> ignore (run "inner" [])) ] in
  expect_run "nested run" ~config suite @@ fun outcome ->
  match failure_list (outcome_of outcome [ "nests" ]) with
  | [ { Failure.kind = Failure.Raise { actual = Some rendered; _ }; _ } ] ->
      check "run inside a test body fails that test"
        (contains "already active" rendered)
  | _ -> check "run inside a test body fails with a Raise payload" false

(* The B-package through the facade

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
  expect_run "b-package" ~config suite @@ fun outcome ->
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
        (match (Render.labeled_msg a, Render.labeled_msg b) with
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
  check_int "b-package exit code" ~expected:1 ~actual:outcome.Runner.exit_code

(* B-package edges

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
    ~actual:outcome.Runner.exit_code

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
    ~actual:outcome.Runner.exit_code

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
    ~actual:outcome.Runner.exit_code;
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
    (match release_rows outcome with
    | [ { Run.outcome = Failure.Fail [ f ]; _ } ] ->
        f.Failure.phase = Failure.Release
    | _ -> false);
  check_int "temp_dir in fixture release exits 1" ~expected:1
    ~actual:outcome.Runner.exit_code

(* The exit guard, process level (D1) *)

(* The one implementation-behavior dependency of the exit guard: an
   exception raised by an [at_exit] function propagates out of
   [Stdlib.exit] to its caller. If a future stdlib swallowed it, the child
   below would exit 0 with a truncated transcript — this test flips
   loudly. *)
let () =
  if not Sys.win32 then (
    with_temp_root @@ fun root ->
    let status, transcript = spawn_child [ "--exit-guard-child"; root ] in
    check "an exit-bombed suite exits through windtrap's own path with 1"
      (status = Unix.WEXITED 1);
    check_contains "the transcript names the interception"
      ~sub:"the test called exit" transcript;
    check_contains "the test after the bomb still ran and failed"
      ~sub:"2 failed" transcript)

(* The run-entry guard reads the widened slot (D1) *)

let () =
  (* [run] refuses whenever the ambient slot is occupied — [Run.active],
     not just an executing frame — so a fixture release or an observer
     starting a nested run is refused like a test body would be. *)
  with_temp_root @@ fun root ->
  let run_record =
    Run.create
      (base_config ~log_dir:root ())
      ~capture:Capture.disabled
      ~baselines:(Baseline.create ~mode:Baseline.Check ())
  in
  match
    Run.with_active run_record (fun () -> Windtrap.run ~argv:[| "x" |] "s" [])
  with
  | _ -> check "run inside an active run is refused" false
  | exception Invalid_argument message ->
      check "run inside an active run raises the already-active error"
        (contains "already active" message)
  | exception _ ->
      check "run inside an active run raises Invalid_argument" false

(* The startup-computed invocation, process level (D5 §1) *)

let () =
  if not Sys.win32 then (
    let spawn_invocation_child mode =
      with_temp_root @@ fun root ->
      let status, transcript =
        spawn_child [ "--invocation-child"; root; mode ]
      in
      check (mode ^ " child fails its one test") (status = Unix.WEXITED 1);
      transcript
    in
    let standalone = spawn_invocation_child "standalone" in
    check "standalone: the hint keeps argv0 verbatim"
      (contains "replay: ./_build/default/qa/x/t.exe --seed " standalone);
    let dune = spawn_invocation_child "dune" in
    check "under dune: the hint is a dune exec spelling"
      (contains "replay: dune exec " dune && contains " -- --seed " dune);
    check "under dune: the spelling carries no /./ (render/F-3)"
      (not (contains "/./" dune));
    let mirrors = spawn_invocation_child "mirrors" in
    check "empty argv: the hint falls back to the environment mirrors"
      (contains "replay: WINDTRAP_SEED=" mirrors
      && not (contains "dune exec" mirrors)))

(* The xpass-string collision stays excused, process level (F4) *)

let () =
  if not Sys.win32 then (
    let spawn_collide_child level =
      with_temp_root @@ fun root ->
      let status, transcript =
        spawn_child [ "--xpass-collide-child"; root; level ]
      in
      check
        ("collide " ^ level ^ " child exits 0 (the failure was expected)")
        (status = Unix.WEXITED 0);
      transcript
    in
    (* Compact: the excused failure is not noteworthy, so the transcript is
       the one named summary line — no flushed header, no loud F glyph. *)
    let compact = spawn_collide_child "compact" in
    check "collide compact: summary counts one expected failure"
      (contains "collide: 1 expected failure in " compact);
    check "collide compact: no header flush" (not (contains "1 test" compact));
    check "collide compact: no loud F on the stream"
      (not (contains "F" compact));
    (* Verbose streams a line per test: the collision must render XFAIL.
       The FAIL probe keeps the tag's two-space gutter so it cannot match
       inside the XFAIL tag itself. *)
    let verbose = spawn_collide_child "verbose" in
    check "collide verbose: the stream line is XFAIL"
      (contains "  XFAIL  collide" verbose);
    check "collide verbose: no loud FAIL line"
      (not (contains "  FAIL  collide" verbose));
    check "collide verbose: summary counts one expected failure"
      (contains "1 expected failure in " verbose))

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
     [Runner.list_selection], before the drive spine — so the whole
     transcript must be the paths and nothing else: no header, no glyph
     row, no summary line. *)
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
  (* And over an empty selection: a listing that answered a mistyped
     filter with silence is the dead end the empty-selection line's own
     "(list the suite's tests with -l)" hint leads to. *)
  let code, out, err =
    run_in_process ~argv:[ "-l"; "-f"; "zzznope" ] root "listsuite" suite
  in
  check_int "an empty listing still returns 0" ~expected:0 ~actual:code;
  check_string "an empty listing says why it is empty"
    ~expected:"no tests ran: filter \"zzznope\" matched none of 2 tests.\n"
    ~actual:out;
  check_string "an empty listing prints nothing on stderr" ~expected:""
    ~actual:err

(* An empty selection says why it is empty *)

let () =
  (* The description is the library runner's own header policy — the
     inline runner passes [None] — so it reaches the renderer through the
     driver's call and nowhere else. The pin is the whole transcript: the
     sentence naming the filter and the denominator, and the [-l] hint
     under it. *)
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
       (list the suite's tests with -l)\n"
    ~actual:out;
  check_string "an empty selection prints nothing on stderr" ~expected:""
    ~actual:err

(* Release failures reach every sink, process level *)

let () =
  if not Sys.win32 then (
    with_temp_root @@ fun root ->
    let junit = Filename.concat root "junit.xml" in
    let status, transcript =
      spawn_child [ "--release-failure-child"; root; junit ]
    in
    (* The runner's own verdict is not the guard here: the exit code is 1
       whether or not the failure was projected. What the projection buys
       is that the reader is told — so assert the transcript and JUnit,
       not just the code. *)
    check "a failing fixture release exits 1" (status = Unix.WEXITED 1);
    check_contains "the transcript carries a failure block for the release"
      ~sub:"fixture release" transcript;
    check_contains "the release failure names its cause" ~sub:"release-boom"
      transcript;
    check_contains "the summary counts the release failure" ~sub:"1 failed"
      transcript;
    check "the one real test is still reported as passing"
      (contains "1 passed" transcript);
    (* Raises if the child never wrote the file — a silently absent JUnit
       report would let the two checks below pass vacuously. *)
    let xml = In_channel.with_open_bin junit In_channel.input_all in
    check_contains "JUnit counts the release failure" ~sub:"failures=\"1\"" xml;
    check_contains "the JUnit case is the release's own path"
      ~sub:"fixture release" xml)

(* The focus warning (testing/T3) *)

let () =
  (* A passing two-test suite, focused or not, outside CI ([init] unset
     CI). The mli-promised warning — windtrap.mli: "outside CI a
     successful focused run prints a warning" — goes to stderr, and is
     absent without focus. *)
  with_temp_root @@ fun root ->
  let pass name = test name (fun () -> is_true true) in
  let run_focus mode suite =
    let code, _, err = run_in_process root "focussuite" suite in
    check_int
      (mode ^ " run returns 0 (focus narrows, never fails)")
      ~expected:0 ~actual:code;
    err
  in
  let focused =
    run_focus "focused"
      [ focus (test "picked" (fun () -> is_true true)); pass "other" ]
  in
  check_contains "outside CI a successful focused run warns"
    ~sub:"warning: focus is active — 1 of 2 tests ran" focused;
  check_contains "the warning tells the committer what to do"
    ~sub:"remove the focus before committing" focused;
  let plain = run_focus "plain" [ pass "picked"; pass "other" ] in
  check "no focus, no warning" (not (contains "focus is active" plain))

(* Summary *)

let () = finish ()
