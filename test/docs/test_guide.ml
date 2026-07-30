(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The RFC guide's failing-path walkthroughs, recreated as passing tests:
   each failing example runs in-process through Runner.execute and the test
   asserts on the failure's rendering — the printed diff, counterexample,
   replay command, and acceptance command the guide shows. (Failing examples
   cannot live under examples/: runtest must stay green.) *)

open Windtrap
open Windtrap.Private

(* The shared meta harness (test/unit/harness.ml): checks, environment
   hygiene, temp roots, and the tree-wide summary dialect — one output
   dialect, one implementation. *)
open Harness

let () = init "guide"
let with_temp_root f = with_temp_root ~prefix:"windtrap-guide-" f
let root_seed = 0x7be1d2c904aa31f5L

let base_config ~log_dir () =
  { (Run.default_config ()) with Run.seed = root_seed; log_dir }

let first_failure outcome path =
  let result =
    List.find_opt (fun r -> r.Run.path = path) (Run.results outcome.Runner.run)
  in
  match result with
  | Some { Run.outcome = Failure.Fail (f :: _); _ } -> Some f
  | _ -> None

let render ?filter failure =
  Format.asprintf "%a"
    (fun ppf f -> Render.pp_failure ~ansi:false ?filter ppf f)
    failure

(* "A failing assertion": the sessions diff *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite =
    [
      test "sessions after login" (fun () ->
          equal
            (list (pair string (list int)))
            [ ("alice", [ 1; 2; 3 ]); ("bob", [ 4 ]) ]
            [ ("alice", [ 1; 2; 3 ]); ("bob", [ 4; 5 ]); ("carol", []) ]);
    ]
  in
  match Runner.execute ~config ~suite:"users" suite with
  | Error _ -> check "sessions example executes" false
  | Ok outcome -> (
      match first_failure outcome [ "sessions after login" ] with
      | None -> check "sessions example fails" false
      | Some failure ->
          let report = render failure in
          check_contains "diff shows the expected side"
            ~sub:{|[("alice", [1; 2; 3]); ("bob", [4])]|} report;
          check_contains "diff shows the actual side" ~sub:{|("carol", [])|}
            report;
          check_contains "report labels the expected line" ~sub:"expected"
            report;
          check_contains "report labels the actual line" ~sub:"actual" report)

(* "A failing property": the shape counterexample *)

type shape = Circle of float | Rect of float * float

let pp_shape ppf = function
  | Circle r -> Format.fprintf ppf "Circle %g" r
  | Rect (w, h) -> Format.fprintf ppf "Rect (%g, %g)" w h

let gen_shape =
  Gen.(
    one_of
      [
        map (fun r -> Circle r) (float_range 0. 100.);
        map
          (fun (w, h) -> Rect (w, h))
          (pair (float_range 0. 100.) (float_range 0. 100.));
      ])
  |> Gen.with_pp pp_shape

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite =
    [
      (* A law wrong for every Rect, so the run fails deterministically and
         shrinking drives the counterexample toward the minimal Rect. *)
      prop "area non-negative" gen_shape (fun s ->
          match s with Rect _ -> is_true false | Circle _ -> ());
    ]
  in
  match Runner.execute ~config ~suite:"geo" suite with
  | Error _ -> check "shape example executes" false
  | Ok outcome -> (
      match first_failure outcome [ "area non-negative" ] with
      | None -> check "shape property fails" false
      | Some failure ->
          (match failure.Failure.kind with
          | Failure.Property { rendered; root; inner; _ } ->
              check_contains "counterexample renders with the attached pp"
                ~sub:"Rect" rendered;
              check "replay data carries the run's root seed" (root = root_seed);
              check "the body's assertion is the inner failure" (inner <> None)
          | _ -> check "shape failure carries a Property payload" false);
          let report = render ~filter:"area non-negative" failure in
          check_contains "report shows the counterexample block"
            ~sub:"counterexample" report;
          check_contains "replay line prints the root token"
            ~sub:("WINDTRAP_SEED=" ^ Seed.to_string root_seed)
            report;
          check_contains "replay line prints the filter"
            ~sub:"WINDTRAP_FILTER='area non-negative'" report;
          check_contains "replay line completes with dune runtest"
            ~sub:"dune runtest" report)

(* "Snapshots": no baseline, proposal and acceptance command *)

let () =
  with_temp_root @@ fun root ->
  Unix.putenv "WINDTRAP_PROJECT_ROOT" root;
  Fun.protect ~finally:(fun () -> Unix.putenv "WINDTRAP_PROJECT_ROOT" "")
  @@ fun () ->
  let config = base_config ~log_dir:(Filename.concat root "logs") () in
  let suite =
    [
      test "cli help" (fun () ->
          snapshot "help" "Usage: mytool [OPTIONS] COMMAND\n");
    ]
  in
  match Runner.execute ~config ~suite:"cli" suite with
  | Error _ -> check "snapshot example executes" false
  | Ok outcome -> (
      match first_failure outcome [ "cli help" ] with
      | None -> check "missing baseline fails" false
      | Some failure ->
          let report = render failure in
          check_contains "report names the missing baseline" ~sub:"no baseline"
            report;
          check_contains "report shows the proposed content"
            ~sub:"Usage: mytool" report;
          check_contains "report prints the acceptance command"
            ~sub:"WINDTRAP_UPDATE=1" report;
          check_contains "headline names the snapshot" ~sub:{|snapshot "help"|}
            (Render.headline failure))

(* "The runner": the transcript and the rerun hint *)

let () =
  with_temp_root @@ fun root ->
  let config = base_config ~log_dir:root () in
  let suite =
    [
      test "addition" (fun () -> equal int 4 (2 + 2));
      test "sessions after login" (fun () ->
          print_endline "[debug] carol: ghost session from pool reuse";
          equal int 2 3);
    ]
  in
  let buffer = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buffer in
  (* The guide's transcript walkthrough shows the line-per-test level. *)
  let renderer =
    Render.create ~out:ppf ~ansi:false ~mode:`Verbose
      ~invocation:(`Exe "dune exec test/main.exe --") ()
  in
  let on_event = function
    | Runner.Run_started { suite; selected; _ } ->
        Render.header renderer ~suite ~tests:selected ~seed:None
    | Runner.Test_started { path } -> Render.begin_test renderer ~path
    | Runner.Test_finished result -> Render.result renderer result
    | Runner.Fixture_release _ -> ()
  in
  match Runner.execute ~on_event ~config ~suite:"mylib" suite with
  | Error _ -> check "runner transcript example executes" false
  | Ok outcome ->
      Render.finish renderer
        ~results:(Run.results outcome.Runner.run)
        ~duration:outcome.Runner.duration ();
      Format.pp_print_flush ppf ();
      let transcript = Buffer.contents buffer in
      check_contains "header counts the suite" ~sub:"mylib: 2 tests" transcript;
      check_contains "one PASS line per passing test" ~sub:"PASS  addition"
        transcript;
      check_contains "one FAIL line per failing test"
        ~sub:"FAIL  sessions after login" transcript;
      check_contains "failures are re-printed in full" ~sub:"expected"
        transcript;
      check_contains "captured output appears in the failure report"
        ~sub:"ghost session" transcript;
      check_contains "summary counts both" ~sub:"1 passed, 1 failed" transcript;
      check_contains "the rerun hint prints the exact command"
        ~sub:"rerun failures only: dune exec test/main.exe -- --failed"
        transcript;
      check "the run exits 1" (outcome.Runner.exit_code = 1)

(* Summary *)

let () = finish ()
