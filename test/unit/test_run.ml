(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for Run: the resolved-config defaults, run-record accessors, frame
   bookkeeping, the single ambient slot (set/restore, outside-run error,
   isolation between sequential runs), the test-body operations
   (current_test, srandom, subtest, temp_dir/temp_file with remove_temp),
   the fixture lifecycle over a scripted fake run (first-use acquisition,
   per-run caching, cached acquisition errors and skips, reverse-order
   announced release, drain, re-acquisition across runs), results
   accumulation, and the coverage seam. Plain executable — the ambient
   slot under test is the one a hosting runner would occupy; "runs" here
   are hand-built records and frames. *)

open Windtrap
open Windtrap.Private
open Harness

let () = init "run"

exception Boom
exception No_db

(* A scripted fake run: disabled capture (no filesystem), a check-mode
   snapshot registry, and a fixed seed. *)
let make_run () =
  let config = { (Run.default_config ()) with Run.seed = 0x5eedL } in
  Run.create config ~capture:Capture.disabled
    ~snapshots:(Snapshot.create ~mode:Snapshot.Check ())

(* Run [fn] as one scripted test attempt of [run]. *)
let in_test ?(path = [ "suite"; "t" ]) ?file run fn =
  let frame = Run.frame run ~path ~file ~loc:None in
  Run.with_frame frame (fun () -> fn frame)

(* Configuration *)

let () =
  let config = Run.default_config () in
  check "default config: root seed is a valid token"
    (String.length (Seed.to_string config.Run.seed) = 19);
  check "default config: no filters"
    (config.Run.filter = None && config.Run.exclude = None);
  check "default config: no tags"
    (config.Run.tags = [] && config.Run.exclude_tags = []);
  check "default config: flags off"
    ((not config.Run.quick)
    && (not config.Run.failed_only)
    && (not config.Run.list_only) && (not config.Run.stream)
    && (not config.Run.prune) && not config.Run.allow_focus);
  check "default config: no update request" (config.Run.update = Env.No_update);
  check "default config: color auto" (config.Run.color = Env.Auto);
  check "default config: no overrides"
    (config.Run.bail = None && config.Run.timeout = None
    && config.Run.prop_count = None
    && config.Run.junit = None && config.Run.columns = None
    && config.Run.tail_errors = None
    && config.Run.shard = None);
  check "default config: log dir is set" (config.Run.log_dir <> "")

let () =
  let config = { (Run.default_config ()) with Run.seed = 0xabcL } in
  let snapshots = Snapshot.create ~mode:Snapshot.Check () in
  let run = Run.create config ~capture:Capture.disabled ~snapshots in
  check "create keeps the config" (Run.config run == config);
  check "create keeps the capture state" (Run.capture run == Capture.disabled);
  check "create keeps the snapshot registry" (Run.snapshots run == snapshots)

(* Frames *)

let () =
  let run = make_run () in
  let frame =
    Run.frame run ~path:[ "g"; "t" ] ~file:(Some "test/test_g.ml") ~loc:None
  in
  check "frame records its path" (Run.path frame = [ "g"; "t" ]);
  check "frame records its declaration file"
    (Run.file frame = Some "test/test_g.ml");
  check "frame belongs to its run" (Run.run_of_frame frame == run);
  check "frame starts with no failures" (Run.failures frame = []);
  check "frame starts with no property context" (Run.prop_context frame = None);
  let f1 = Failure.message "first" in
  let f2 = Failure.message "second" |> Failure.with_phase Failure.Teardown in
  Run.add_failure frame f1;
  Run.add_failure frame f2;
  match Run.failures frame with
  | [ a; b ] ->
      check "failures keep insertion order (body before teardown)"
        (a == f1 && b == f2);
      check "phases survive collection"
        (a.Failure.phase = Failure.Body && b.Failure.phase = Failure.Teardown)
  | _ -> check "failures shape" false

(* Location fallback (D4) *)

let declared = { Loc.file = "test/test_g.ml"; line = 4; column = 2 }
let elsewhere = { Loc.file = "test/helper.ml"; line = 9; column = 0 }

let () =
  let run = make_run () in
  let frame =
    Run.frame run ~path:[ "t" ] ~file:(Some "test/test_g.ml")
      ~loc:(Some declared)
  in
  check "frame records its declaration location" (Run.loc frame = Some declared);
  (* A failure without a location — its failing call sat in tail position —
     is attributed to the declaration. *)
  Run.add_failure frame (Failure.message "tail failure");
  (* A failure with its own location keeps it. *)
  Run.add_failure frame (Failure.message ~loc:elsewhere "located failure");
  (* A property failure's nested inner is left untouched. *)
  Run.add_failure frame
    (Failure.property
       ~inner:(Failure.equality ~expected:"0" ~actual:"1" ())
       ~rendered:"1" ~case_index:0 ~shrink_steps:0 ~root:1L ~examples:false ());
  (match Run.failures frame with
  | [ tail; located; prop ] ->
      check "add_failure falls back to the frame's declaration location"
        (tail.Failure.loc = Some declared);
      check "add_failure keeps an explicit location"
        (located.Failure.loc = Some elsewhere);
      check "the fallback fills the property failure's own location"
        (prop.Failure.loc = Some declared);
      check "a property failure's inner location is left untouched"
        (match prop.Failure.kind with
        | Failure.Property { inner = Some i; _ } -> i.Failure.loc = None
        | _ -> false)
  | _ -> check "location fallback failures shape" false);
  let bare = Run.frame run ~path:[ "t" ] ~file:None ~loc:None in
  Run.add_failure bare (Failure.message "nowhere");
  match Run.failures bare with
  | [ f ] ->
      check "a frame without a declaration location records None"
        (f.Failure.loc = None)
  | _ -> check "bare frame failure shape" false

(* The ambient slot *)

let () =
  (* The widened slot (exit guard, D1): [active] answers for the whole
     executing span, frames overlay it, and the frame-reading operations
     still refuse between attempts. *)
  let run = make_run () in
  check "active is false outside any run" (not (Run.active ()));
  Run.with_active run (fun () ->
      check "active is true inside with_active" (Run.active ());
      check "no frame is current between attempts (current_opt)"
        (Run.current_opt () = None);
      expect_invalid_arg "current_frame between attempts raises" (fun () ->
          Run.current_frame ());
      in_test run (fun frame ->
          check "a frame overlays the executing run"
            (Run.active () && Run.current_frame () == frame));
      check "the run stays active after the attempt"
        (Run.active () && Run.current_opt () = None));
  check "active is false after with_active returns" (not (Run.active ()));
  (match Run.with_active run (fun () -> raise Boom) with
  | () -> check "with_active propagates exceptions" false
  | exception Boom -> check "with_active propagates exceptions" true);
  check "the slot is emptied after a raising span" (not (Run.active ()));
  check_int "with_active returns the body's value" ~expected:9
    ~actual:(Run.with_active run (fun () -> 9))

let () =
  expect_invalid_arg "current_frame outside a run raises" (fun () ->
      Run.current_frame ());
  expect_invalid_arg "current outside a run raises" (fun () -> Run.current ());
  check "current_opt outside a run is None" (Run.current_opt () = None)

let () =
  let run = make_run () in
  let seen = ref None in
  in_test run (fun frame ->
      seen := Some (Run.current_frame () == frame && Run.current () == run));
  check "the slot exposes the executing frame and its run" (!seen = Some true);
  check "the slot is cleared after the attempt" (Run.current_opt () = None)

let () =
  let run = make_run () in
  check_int "with_frame returns the body's value" ~expected:7
    ~actual:(in_test run (fun _ -> 7));
  (match in_test run (fun _ -> raise Boom) with
  | () -> check "with_frame propagates exceptions" false
  | exception Boom -> check "with_frame propagates exceptions" true);
  check "the slot is cleared after a raising attempt" (Run.current_opt () = None)

let () =
  (* Nesting restores the previous frame — with_frame is save/restore, not
     assign/clear. *)
  let run = make_run () in
  let outer = Run.frame run ~path:[ "outer" ] ~file:None ~loc:None in
  let inner = Run.frame run ~path:[ "inner" ] ~file:None ~loc:None in
  Run.with_frame outer (fun () ->
      Run.with_frame inner (fun () ->
          check "inner frame is current inside" (Run.current_frame () == inner));
      check "outer frame is restored after inner" (Run.current_frame () == outer))

let () =
  (* Sequential runs are isolated: each run's frames expose that run and
     nothing leaks across. *)
  let run_a = make_run () in
  let run_b = make_run () in
  in_test run_a (fun _ ->
      check "first run sees itself" (Run.current () == run_a));
  check "slot empty between runs" (Run.current_opt () = None);
  in_test run_b (fun _ ->
      check "second run sees itself, not the first" (Run.current () == run_b));
  check "slot empty after both runs" (Run.current_opt () = None)

(* Property context *)

(* A real [Property.context], captured from a one-case engine run. *)
let get_prop_context () =
  let captured = ref None in
  (match
     Property.run ~root:1L ~path:"ctx" ~count:1 (Gen.constant ()) (fun ctx () ->
         captured := Some ctx)
   with
  | Property.Pass _ -> ()
  | _ -> failwith "property context scaffolding run failed");
  Option.get !captured

let () =
  let run = make_run () in
  let ctx = get_prop_context () in
  in_test run (fun frame ->
      Run.with_prop_context frame ctx (fun () ->
          check "property context is exposed while the body runs"
            (match Run.prop_context frame with
            | Some c -> c == ctx
            | None -> false));
      check "property context is restored afterwards"
        (Run.prop_context frame = None);
      (match Run.with_prop_context frame ctx (fun () -> raise Boom) with
      | () -> check "with_prop_context propagates exceptions" false
      | exception Boom -> check "with_prop_context propagates exceptions" true);
      check "property context is restored after a raise"
        (Run.prop_context frame = None))

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
  let run = make_run () in
  let first = in_test run (fun _ -> accessor ()) in
  check_int "first use acquires" ~expected:1 ~actual:!acquisitions;
  let second = in_test run ~path:[ "suite"; "other" ] (fun _ -> accessor ()) in
  check "later uses in the run share the cached resource" (first == second);
  check_int "later uses do not re-acquire" ~expected:1 ~actual:!acquisitions;
  (* Per-run cache: a later run re-acquires a fresh resource. *)
  let next_run = make_run () in
  let third = in_test next_run (fun _ -> accessor ()) in
  check_int "a later run re-acquires" ~expected:2 ~actual:!acquisitions;
  check "the later run gets a fresh resource" (not (third == first))

let () =
  (* Reverse-order announced release, with teardown receiving the exact
     acquired value. *)
  let log = ref [] in
  let mark entry = log := entry :: !log in
  let fx_a =
    Run.fixture ~teardown:(fun v -> mark ("release " ^ v)) (fun () -> "a")
  in
  let fx_b =
    Run.fixture ~teardown:(fun v -> mark ("release " ^ v)) (fun () -> "b")
  in
  let fx_plain = Run.fixture (fun () -> "no-teardown") in
  let run = make_run () in
  in_test run (fun _ ->
      ignore (fx_a ());
      ignore (fx_plain ());
      ignore (fx_b ()));
  let announced = ref [] in
  let release_failures =
    Run.release_fixtures run ~announce:(fun name ->
        announced := name :: !announced;
        mark ("announce " ^ name))
  in
  check "clean releases report no failures" (release_failures = []);
  check_int "only fixtures with a teardown are announced" ~expected:2
    ~actual:(List.length !announced);
  check "announcements name the declaration site"
    (List.for_all (fun name -> contains "fixture" name) !announced);
  (match List.rev !log with
  | [ a1; r1; a2; r2 ] ->
      check "release order is reverse acquisition order"
        (r1 = "release b" && r2 = "release a");
      check "each fixture is announced before its own teardown runs"
        (contains "announce" a1 && contains "announce" a2)
  | _ -> check "release log shape" false);
  check "release drains the registry: a second call releases nothing"
    (Run.release_fixtures run ~announce:(fun _ -> mark "extra") = []
    && not (List.exists (fun e -> e = "extra") !log))

let () =
  (* Failed acquisition: the exception propagates, is cached, and later uses
     re-raise it without re-acquiring; release skips the fixture. *)
  let attempts = ref 0 in
  let broken =
    Run.fixture ~teardown:ignore (fun () ->
        incr attempts;
        raise No_db)
  in
  let run = make_run () in
  in_test run (fun _ ->
      match broken () with
      | _ -> check "failed acquisition raises at first use" false
      | exception No_db -> check "failed acquisition raises at first use" true);
  check_int "acquisition ran once" ~expected:1 ~actual:!attempts;
  in_test run ~path:[ "suite"; "later" ] (fun _ ->
      match broken () with
      | _ -> check "later uses re-raise the cached error" false
      | exception No_db -> check "later uses re-raise the cached error" true);
  check_int "the cached error prevents re-acquisition" ~expected:1
    ~actual:!attempts;
  check "a failed fixture is never announced or released"
    (Run.release_fixtures run ~announce:(fun _ -> check "no announce" false)
    = []);
  (* A fresh run retries the acquisition (the error cache is per run). *)
  let next_run = make_run () in
  in_test next_run (fun _ ->
      match broken () with () -> () | exception No_db -> ());
  check_int "a later run retries a failed acquisition" ~expected:2
    ~actual:!attempts

let () =
  (* A raising teardown becomes a Release-phase failure and does not stop
     the remaining releases. *)
  let released = ref [] in
  let fx_ok =
    Run.fixture ~teardown:(fun v -> released := v :: !released) (fun () -> "ok")
  in
  let fx_bad =
    Run.fixture ~teardown:(fun _ -> failwith "leak") (fun () -> "bad")
  in
  let run = make_run () in
  in_test run (fun _ ->
      ignore (fx_ok ());
      ignore (fx_bad ()));
  (match Run.release_fixtures run ~announce:ignore with
  | [ failure ] ->
      check "a raising teardown yields a Release-phase failure"
        (failure.Failure.phase = Failure.Release);
      (match failure.Failure.kind with
      | Failure.Message message ->
          check "the failure names the fixture and the exception"
            (contains "fixture" message
            && contains "release raised" message
            && contains "leak" message)
      | _ -> check "release failure kind" false);
      check "the release failure carries the declaration site"
        (match failure.Failure.loc with
        | Some loc -> Filename.basename loc.Loc.file = "test_run.ml"
        | None -> false)
  | _ -> check "release failure shape" false);
  check "releases continue past a raising teardown" (!released = [ "ok" ])

let () =
  (* Several raising teardowns: every failure is returned, in release
     (reverse acquisition) order. *)
  let fx_first = Run.fixture ~teardown:(fun _ -> failwith "first") ignore in
  let fx_second = Run.fixture ~teardown:(fun _ -> failwith "second") ignore in
  let run = make_run () in
  in_test run (fun _ ->
      fx_first ();
      fx_second ());
  match Run.release_fixtures run ~announce:ignore with
  | [ a; b ] ->
      let text failure =
        match failure.Failure.kind with Failure.Message m -> m | _ -> ""
      in
      check "all raising teardowns are reported, in release order"
        (contains "second" (text a) && contains "first" (text b))
  | _ -> check "multiple release failures shape" false

let () =
  (* A fatal exception in a teardown is re-raised immediately: the
     remaining releases are abandoned and the registry stays drained. *)
  let released = ref [] in
  let fx_ok =
    Run.fixture ~teardown:(fun v -> released := v :: !released) (fun () -> "ok")
  in
  let fx_fatal =
    Run.fixture ~teardown:(fun _ -> raise Stack_overflow) (fun () -> "fatal")
  in
  let run = make_run () in
  in_test run (fun _ ->
      ignore (fx_ok ());
      ignore (fx_fatal ()));
  (match Run.release_fixtures run ~announce:ignore with
  | _ -> check "a fatal teardown re-raises out of release_fixtures" false
  | exception Stack_overflow ->
      check "a fatal teardown re-raises out of release_fixtures" true);
  check "the fatal abandons the remaining releases" (!released = []);
  check "the registry is drained even after a fatal"
    (Run.release_fixtures run ~announce:ignore = [] && !released = [])

(* current_test (B9) *)

let () =
  expect_invalid_arg "current_test outside a run raises" (fun () ->
      Run.current_test ())

let () =
  let run = make_run () in
  let seen = ref [] in
  in_test run ~path:[ "users"; "sessions"; "login" ] (fun _ ->
      seen := Run.current_test ());
  check "current_test is the executing test's full path"
    (!seen = [ "users"; "sessions"; "login" ])

(* srandom (B10) *)

let () =
  expect_invalid_arg "srandom outside a run raises" (fun () -> Run.srandom ())

let draw_ints run ~path =
  in_test run ~path (fun _ ->
      let state = Run.srandom () in
      List.init 4 (fun _ -> Random.State.bits state))

let () =
  let run = make_run () in
  let first = draw_ints run ~path:[ "g"; "t" ] in
  let again = draw_ints run ~path:[ "g"; "t" ] in
  check "srandom is a pure function of (root, path): attempts repeat it"
    (first = again);
  let other = draw_ints run ~path:[ "g"; "other" ] in
  check "a different path gets a different stream" (first <> other);
  let other_root =
    let config = { (Run.default_config ()) with Run.seed = 0xdeadL } in
    let run =
      Run.create config ~capture:Capture.disabled
        ~snapshots:(Snapshot.create ~mode:Snapshot.Check ())
    in
    draw_ints run ~path:[ "g"; "t" ]
  in
  check "a different root seed gets a different stream" (first <> other_root)

let () =
  let run = make_run () in
  in_test run (fun _ ->
      let a = Run.srandom () in
      let b = Run.srandom () in
      check "every call in a test returns an identically seeded state"
        (List.init 4 (fun _ -> Random.State.bits a)
        = List.init 4 (fun _ -> Random.State.bits b)))

let () =
  (* srandom never touches the ambient global Random state. *)
  let run = make_run () in
  Random.init 4242;
  let expected = Random.int 1_000_000 in
  Random.init 4242;
  in_test run (fun _ -> ignore (Random.State.bits (Run.srandom ())));
  check "srandom leaves the global Random state alone"
    (Random.int 1_000_000 = expected)

(* subtest (B13) *)

let () =
  expect_invalid_arg "subtest outside a run raises" (fun () ->
      Run.subtest "s" (fun () -> ()))

let msg_of (f : Failure.t) = Option.value f.Failure.msg ~default:"<none>"

let () =
  let run = make_run () in
  let after_failing = ref false in
  in_test run ~path:[ "suite"; "t" ] (fun frame ->
      Run.subtest "passing" (fun () -> ());
      Run.subtest "failing" (fun () -> Check.fail "boom");
      after_failing := true;
      match Run.failures frame with
      | [ failure ] ->
          check "a failing subtest appends one labeled failure"
            (msg_of failure = "t › failing");
          check "the failure keeps its payload"
            (match failure.Failure.kind with
            | Failure.Message m -> m = "boom"
            | _ -> false)
      | _ -> check "one subtest failure recorded" false);
  check "siblings continue after a failing subtest" !after_failing

let () =
  (* Labels compose through nesting, and a user ~msg is kept after the
     label. *)
  let run = make_run () in
  in_test run ~path:[ "t" ] (fun frame ->
      Run.subtest "outer" (fun () ->
          Run.subtest "inner" (fun () -> Check.is_true ~msg:"ctx" false));
      Run.subtest "outer" (fun () -> Check.fail "at-outer-level");
      match Run.failures frame with
      | [ nested; outer ] ->
          check "nested labels compose innermost-last"
            (msg_of nested = "t › outer › inner: ctx");
          check "after the nested subtest returns, its label is popped"
            (msg_of outer = "t › outer")
      | _ -> check "nested subtest failures shape" false)

let () =
  (* A non-fatal exception is the sub-case's failure: recorded as a Raise
     entry, labeled, and siblings continue. *)
  let run = make_run () in
  let sibling_ran = ref false in
  in_test run ~path:[ "t" ] (fun frame ->
      Run.subtest "throws" (fun () -> raise Boom);
      sibling_ran := true;
      match Run.failures frame with
      | [ failure ] ->
          check "an exception in a subtest is a labeled Raise failure"
            (msg_of failure = "t › throws"
            &&
            match failure.Failure.kind with
            | Failure.Raise { actual = Some actual; _ } ->
                contains "Boom" actual
            | _ -> false)
      | _ -> check "subtest exception recorded" false);
  check "siblings continue after a throwing subtest" !sibling_ran

let () =
  (* Skip and fatal exceptions abort the whole test: they propagate out of
     the subtest with the label popped. *)
  let run = make_run () in
  in_test run ~path:[ "t" ] (fun frame ->
      (match Run.subtest "skips" (fun () -> Check.skip ~reason:"later" ()) with
      | () -> check "skip propagates out of a subtest" false
      | exception Failure.Skip_test (Some "later") ->
          check "skip propagates out of a subtest" true
      | exception _ -> check "skip propagates out of a subtest" false);
      (match Run.subtest "fatal" (fun () -> raise Stack_overflow) with
      | () -> check "a fatal exception propagates out of a subtest" false
      | exception Stack_overflow ->
          check "a fatal exception propagates out of a subtest" true
      | exception _ ->
          check "a fatal exception propagates out of a subtest" false);
      check "control exceptions record no subtest failure"
        (Run.failures frame = []);
      (* The stack was popped on the control paths: a later failure carries
         only its own label. *)
      Run.subtest "clean" (fun () -> Check.fail "x");
      match Run.failures frame with
      | [ failure ] ->
          check "the subtest stack is restored after control exceptions"
            (msg_of failure = "t › clean")
      | _ -> check "post-control subtest failure shape" false)

(* temp_dir / temp_file (B9) *)

let () =
  expect_invalid_arg "temp_dir outside a run raises" (fun () -> Run.temp_dir ());
  expect_invalid_arg "temp_file outside a run raises" (fun () ->
      Run.temp_file ())

let () =
  let run = make_run () in
  let frame = Run.frame run ~path:[ "t" ] ~file:None ~loc:None in
  Run.with_frame frame (fun () ->
      let d1 = Run.temp_dir () in
      let d2 = Run.temp_dir ~prefix:"repo" () in
      let f1 = Run.temp_file () in
      let f2 = Run.temp_file ~suffix:".json" () in
      check "temp_dir creates a fresh empty directory"
        (Sys.is_directory d1 && Sys.readdir d1 = [||]);
      check "each temp_dir call is a new directory"
        (d1 <> d2 && Sys.is_directory d2);
      check "the prefix names the directory basename"
        (String.length (Filename.basename d2) >= 4
        && String.sub (Filename.basename d2) 0 4 = "repo");
      check "temp_file creates an empty file"
        (Sys.file_exists f1 && (not (Sys.is_directory f1)) && f1 <> f2);
      check "the suffix is appended to the file name"
        (Filename.check_suffix f2 ".json");
      check "paths share one per-attempt scratch directory"
        (Filename.dirname d1 = Filename.dirname f1
        && Filename.dirname d1 = Filename.dirname d2);
      (* Populate the scratch tree; the dedicated removal test below proves
         recursive cleanup. *)
      let oc = open_out (Filename.concat d1 "nested.txt") in
      output_string oc "scratch";
      close_out oc);
  Run.remove_temp frame

let () =
  let run = make_run () in
  let frame = Run.frame run ~path:[ "t" ] ~file:None ~loc:None in
  let paths =
    Run.with_frame frame (fun () ->
        let d = Run.temp_dir () in
        let f = Run.temp_file () in
        let oc = open_out (Filename.concat d "deep.txt") in
        output_string oc "x";
        close_out oc;
        [ d; f; Filename.concat d "deep.txt" ])
  in
  check "scratch paths exist until removal" (List.for_all Sys.file_exists paths);
  Run.remove_temp frame;
  check "remove_temp removes every scratch path, recursively"
    (List.for_all (fun p -> not (Sys.file_exists p)) paths);
  Run.remove_temp frame;
  check "remove_temp is idempotent" true

let () =
  (* Exception safety: paths created before a raising body are still
     removable — the runner calls remove_temp on every path it controls. *)
  let run = make_run () in
  let frame = Run.frame run ~path:[ "t" ] ~file:None ~loc:None in
  let created = ref "" in
  (match
     Run.with_frame frame (fun () ->
         created := Run.temp_dir ();
         raise Boom)
   with
  | () -> check "raising body propagates" false
  | exception Boom -> check "raising body propagates" true);
  check "the scratch dir survives the raise (removal is the runner's)"
    (Sys.file_exists !created);
  Run.remove_temp frame;
  check "remove_temp cleans up after a raising body"
    (not (Sys.file_exists !created))

let () =
  (* A hostile prefix/suffix cannot escape the scratch directory. *)
  let run = make_run () in
  let frame = Run.frame run ~path:[ "t" ] ~file:None ~loc:None in
  Run.with_frame frame (fun () ->
      let d = Run.temp_dir ~prefix:"../evil" () in
      let f = Run.temp_file ~suffix:"/evil" () in
      let plain = Run.temp_dir () in
      check "a hostile prefix is sanitized into the scratch directory"
        (Filename.dirname d = Filename.dirname plain
        && not (String.contains (Filename.basename d) '/'));
      check "a hostile suffix is sanitized into the scratch directory"
        (Filename.dirname f = Filename.dirname plain));
  Run.remove_temp frame

let () =
  (* The documented permission contract is a security property: no group or
     other access anywhere in the scratch tree (0o700 directories, 0o600
     files, modulo the caller's umask which can only tighten them). *)
  let run = make_run () in
  let frame = Run.frame run ~path:[ "t" ] ~file:None ~loc:None in
  Run.with_frame frame (fun () ->
      let d = Run.temp_dir () in
      let f = Run.temp_file () in
      let private_to_owner path =
        (Unix.stat path).Unix.st_perm land 0o077 = 0
      in
      check "temp_dir grants no group/other access" (private_to_owner d);
      check "temp_file grants no group/other access" (private_to_owner f);
      check "the scratch root grants no group/other access"
        (private_to_owner (Filename.dirname d)));
  Run.remove_temp frame

let () =
  (* Fresh frames get fresh scratch directories: retries never see a
     previous attempt's files. *)
  let run = make_run () in
  let first = Run.frame run ~path:[ "t" ] ~file:None ~loc:None in
  let d1 = Run.with_frame first (fun () -> Run.temp_dir ()) in
  Run.remove_temp first;
  let second = Run.frame run ~path:[ "t" ] ~file:None ~loc:None in
  let d2 = Run.with_frame second (fun () -> Run.temp_dir ()) in
  check "a later attempt gets a fresh scratch directory" (d1 <> d2);
  Run.remove_temp second

(* Fixture acquisition skips (amendment C1) *)

let () =
  let attempts = ref 0 in
  let torn_down = ref false in
  let unavailable =
    Run.fixture
      ~teardown:(fun _ -> torn_down := true)
      (fun () ->
        incr attempts;
        Check.skip ~reason:"no gpu" ())
  in
  let run = make_run () in
  in_test run (fun _ ->
      match unavailable () with
      | _ -> check "a skipping acquisition skips the acquiring test" false
      | exception Failure.Skip_test (Some "no gpu") ->
          check "a skipping acquisition skips the acquiring test" true
      | exception _ ->
          check "a skipping acquisition skips the acquiring test" false);
  check_int "acquisition ran once" ~expected:1 ~actual:!attempts;
  in_test run ~path:[ "suite"; "later" ] (fun _ ->
      match unavailable () with
      | _ -> check "later uses skip with the cached reason" false
      | exception Failure.Skip_test (Some "no gpu") ->
          check "later uses skip with the cached reason" true
      | exception _ -> check "later uses skip with the cached reason" false);
  check_int "the cached skip prevents re-acquisition" ~expected:1
    ~actual:!attempts;
  check "a skipped fixture is never announced or released"
    (Run.release_fixtures run ~announce:(fun _ -> check "no announce" false)
     = []
    && not !torn_down);
  (* The skip cache is per run: a fresh run re-attempts the acquisition. *)
  let next_run = make_run () in
  in_test next_run (fun _ ->
      match unavailable () with _ -> () | exception Failure.Skip_test _ -> ());
  check_int "a later run re-attempts a skipped acquisition" ~expected:2
    ~actual:!attempts

(* Results *)

let () =
  let run = make_run () in
  check "no results initially" (Run.results run = []);
  let r1 =
    {
      Run.path = [ "a" ];
      outcome = Failure.Pass;
      counted = false;
      xfail = None;
      slow_tagged = false;
      duration = 0.25;
      attempts = 1;
      prop_stats = None;
      srandom_root = None;
    }
  in
  let r2 =
    {
      Run.path = [ "g"; "b" ];
      outcome = Failure.Fail [ Failure.message "boom" ];
      counted = true;
      xfail = None;
      slow_tagged = false;
      duration = 1.5;
      attempts = 3;
      prop_stats = None;
      srandom_root = None;
    }
  in
  Run.record run r1;
  Run.record run r2;
  match Run.results run with
  | [ a; b ] ->
      check "results keep execution order" (a == r1 && b == r2);
      check "result fields round-trip"
        (b.Run.path = [ "g"; "b" ] && b.Run.attempts = 3 && b.Run.duration = 1.5)
  | _ -> check "results shape" false

(* Coverage seam *)

let () =
  let run = make_run () in
  check "no coverage snapshot by default" (Run.coverage run = None);
  Run.set_coverage run { Run.visited = 312; total = 358; siblings = true };
  match Run.coverage run with
  | Some summary ->
      check "the coverage snapshot is exposed to renderers"
        (summary.Run.visited = 312 && summary.Run.total = 358
       && summary.Run.siblings)
  | None -> check "coverage snapshot recorded" false

(* Summary *)

let () = finish ()
