(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The suite the sessions in this directory run. The default declaration
   is five tests, one of them failing and one reading a file baseline, so
   that a session selects the run it needs from the command line. Each
   other declaration, selected by FACADE_FIXTURE, stands alone because it
   is a property of the suite rather than of a flag: a duplicate path, a
   focus, a test that calls [exit], output that bypasses the capture, a
   failing property. *)

open Windtrap
module Os = Windtrap.Private.Os
module Report = Windtrap.Private.Report
module Run = Windtrap.Private.Run

let default =
  [
    group "math"
      [
        test "adds" (fun () -> equal int 4 (2 + 2));
        test "subtracts" (fun () -> equal int 0 (2 - 2));
      ];
    (* The one failing test: scenarios select it by name to fail a run and
       exclude it by name to pass one. *)
    test "boom" (fun () -> equal ~msg:"deliberate" int 1 2);
    slow "crawls" (fun () -> is_true true);
    (* The baseline the sessions plant, at a path both files agree on. *)
    test "greeting" (fun () ->
        expect_file "hello from the fixture\n" "test/cram/run/greeting.expected");
  ]

let focused =
  [
    focus (test "focused" (fun () -> is_true true));
    test "unfocused" (fun () -> is_true true);
  ]

let duplicate =
  [
    group "dup" [ test "twice" (fun () -> is_true true) ];
    group "dup" [ test "twice" (fun () -> is_true true) ];
  ]

(* Prints, then fails: the one test whose report carries a captured tail
   and the full log's path. *)
let noisy =
  [
    test "noisy" (fun () ->
        print_string "hello from noisy\n";
        equal ~msg:"deliberate" int 1 2);
  ]

(* Output that does not go through the report's formatter: left unflushed
   in the [stdout] channel, written straight to descriptor 1, and written
   by a subprocess through its own C stdio. *)
let streamed =
  [
    test "channel" (fun () -> print_string "through the stdout channel\n");
    test "descriptor" (fun () ->
        let line = "through descriptor 1\n" in
        ignore (Unix.write_substring Unix.stdout line 0 (String.length line)));
    test "subprocess" (fun () ->
        let pid =
          Unix.create_process "echo"
            [| "echo"; "through a subprocess" |]
            Unix.stdin Unix.stdout Unix.stderr
        in
        ignore (Unix.waitpid [] pid));
    test "fails" (fun () ->
        print_string "before the failure\n";
        equal ~msg:"deliberate" int 1 2);
  ]

(* A stale baseline beside a failing assertion: the run keeps no
   correction, whatever it was asked to do with one. *)
let masked =
  [
    test "masked" (fun () ->
        expect_file "fresh from the fixture\n" "test/cram/run/masked.expected";
        equal ~msg:"deliberate" int 1 2);
  ]

(* The second test calls [exit], which must not end the run: the third
   still runs, and fails. *)
let exits =
  [
    test "before" (fun () -> is_true true);
    test "bomb" (fun () -> Stdlib.exit 0);
    test "after" (fun () -> equal ~msg:"deliberate" int 1 2);
  ]

(* A property that fails on its first case: its report ends on the replay
   line, the command a report spells for the way the run was started. *)
let property = [ prop "boom" Gen.int (fun _ -> equal int 1 2) ]

(* Two properties that fail on a case the seed picks, beside a test that
   passes: one replay line reruns the two, each on its own case. *)
let properties =
  [
    test "passes" (fun () -> is_true true);
    prop "even" Gen.int (fun n -> is_true (n mod 2 = 0));
    prop "small" Gen.int (fun n -> is_true (abs n < 1000));
  ]

(* A literal that holds, a stale literal and a stale file baseline, beside
   a failing property: the one accept: line of the report, pasted, rewrites
   the two stale baselines and no other. *)
let accepts =
  [
    test "holds" (fun () -> expect "same" @@ __POS_OF__ "same");
    test "stale literal" (fun () -> expect "fresh" @@ __POS_OF__ "stale");
    test "stale file" (fun () ->
        expect_file "fresh from the fixture\n" "test/cram/run/accepts.expected");
    prop "small" Gen.int (fun n -> is_true (abs n < 1000));
  ]

(* Signals *)

(* The declarations signals.t stops with a signal. A test or a release says
   it is ready by creating a file in the working directory, then waits for
   the signal; the signal comes from outside, from send_signal.exe, except
   between tests. *)

let touch name = close_out (open_out name)

let waits () =
  print_string "captured, never shown\n";
  ignore (temp_dir ());
  touch "ready";
  Unix.sleepf 60.

let waiting =
  [
    test "passes" (fun () -> is_true true);
    test "fails" (fun () -> equal ~msg:"deliberate" int 1 2);
    group "deep" [ test "waits" waits ];
    test "never reached" (fun () -> is_true true);
  ]

(* A signal between two tests can only be sent by the process itself, from
   an observer of the run; the observer raises on the event that says the
   run was interrupted, and the runner ignores that. *)
let between () =
  let signal_self = function
    | Run.Test_finished _ -> Unix.kill (Unix.getpid ()) Sys.sigterm
    | Run.Interrupted _ -> raise Exit
    | Run.Run_started _ | Run.Test_started _ | Run.Shrinking _ | Run.Shrunk _
    | Run.Fixture_release _ ->
        ()
  in
  let config =
    {
      (Run.default_config ()) with
      log_dir = "logs";
      color = Os.Never;
      exclude = [ "fails" ];
    }
  in
  ignore (Report.run ~on_event:signal_self ~suite:"fixture" config waiting);
  3

(* Released latest first: the third fixture waits in its release, the two
   others are still held. *)
let releasing =
  let released name = fixture ~teardown:(fun () -> touch name) ignore in
  let first = released "first released"
  and second = released "second released" in
  let hangs =
    fixture
      ~teardown:(fun () ->
        touch "ready";
        Unix.sleepf 60.)
      ignore
  in
  [
    test "acquires" (fun () ->
        first ();
        second ();
        hangs ());
  ]

(* The first test records a correction and the second fails; the third binds
   a variable inside a bracket and waits. The fixture's release writes what
   it reads of the variable. *)
let leaving =
  let reads_env =
    fixture
      ~teardown:(fun () ->
        Out_channel.with_open_bin "env at release" (fun oc ->
            output_string oc
              (Option.value ~default:"<unset>" (Sys.getenv_opt "WINDTRAP_LEFT"))))
      ignore
  in
  [
    test "corrects" (fun () ->
        reads_env ();
        expect_file "new\n" "c.expected");
    test "fails" (fun () -> equal ~msg:"deliberate" int 1 2);
    bracket ~setup:ignore
      ~teardown:(fun () -> touch "teardown ran")
      "waits"
      (fun () ->
        setenv "WINDTRAP_LEFT" (Some "set by the test");
        waits ());
  ]

(* A release that waits after the first signal, for the second. *)
let lingering =
  let lingers =
    fixture
      ~teardown:(fun () ->
        touch "releasing";
        Unix.sleepf 60.)
      ignore
  in
  [
    test "holds and waits" (fun () ->
        lingers ();
        waits ());
  ]

(* A process the test forks inherits the runner's handlers, and a signal
   must kill it rather than run the parent's report in the child. *)
let forking =
  let status = function
    | Unix.WSIGNALED s when s = Sys.sigterm -> "killed by SIGTERM"
    | Unix.WSIGNALED s -> "killed by signal " ^ string_of_int s
    | Unix.WEXITED code -> "exited " ^ string_of_int code
    | Unix.WSTOPPED _ -> "stopped"
  in
  [
    test "kills the process it forked" (fun () ->
        match Unix.fork () with
        | 0 ->
            Unix.sleepf 60.;
            Unix._exit 0
        | pid ->
            Unix.sleepf 0.05;
            Unix.kill pid Sys.sigterm;
            equal string "killed by SIGTERM"
              (status (snd (Unix.waitpid [] pid))));
  ]

(* Whether the handlers this process installed are its own again once [run]
   returns. *)
let handlers () =
  let mine (_ : int) = () in
  let signals = [ Sys.sigint; Sys.sigterm; Sys.sighup ] in
  let before =
    List.map (fun s -> Sys.signal s (Sys.Signal_handle mine)) signals
  in
  let code = run "fixture" [ test "passes" (fun () -> is_true true) ] in
  let kept s previous =
    match Sys.signal s previous with
    | Sys.Signal_handle f when f == mine -> "kept"
    | Sys.Signal_handle _ | Sys.Signal_default | Sys.Signal_ignore -> "lost"
  in
  print_endline
    ("handlers: " ^ String.concat " " (List.map2 kept signals before));
  code

let declared = function
  | Some "focus" -> focused
  | Some "duplicate" -> duplicate
  | Some "noisy" -> noisy
  | Some "stream" -> streamed
  | Some "masked" -> masked
  | Some "exits" -> exits
  | Some "property" -> property
  | Some "properties" -> properties
  | Some "accepts" -> accepts
  | Some "waiting" -> waiting
  | Some "releasing" -> releasing
  | Some "leaving" -> leaving
  | Some "lingering" -> lingering
  | Some "forking" -> forking
  | Some ("" | "default") | None -> default
  | Some other -> invalid_arg ("suite_main: unknown FACADE_FIXTURE " ^ other)

let () =
  exit
    (match Sys.getenv_opt "FACADE_FIXTURE" with
    | Some "between" -> between ()
    | Some "handlers" -> handlers ()
    | name -> run "fixture" (declared name))
