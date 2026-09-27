(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [Run.execute] refuses to start while a run is active, and every test body
   runs inside this suite's own run. The runs the tests judge are therefore
   recorded as the module initialises, before the [run] that ends the file. *)

open Windtrap
module Baseline = Windtrap.Private.Baseline
module Failure = Windtrap.Private.Failure
module Loc = Windtrap.Private.Loc
module Os = Windtrap.Private.Os
module Property = Windtrap.Private.Property
module Report_junit = Windtrap.Private.Report_junit
module Run = Windtrap.Private.Run
module Seed = Windtrap.Private.Seed
module Test_tree = Windtrap.Private.Test_tree
module Scratch = Windtrap_test_support.Scratch

let strf = Printf.sprintf

exception Boom
exception No_db

(* Projections *)

(* How [fn] ended: [None] when it returned, else what it raised. A test hands
   the exception to [raises] again with [replay]. *)
let escape fn = match fn () with _ -> None | exception e -> Some e
let replay escaped = Option.iter raise escaped
let exn = Testable.contramap Failure.exn_to_string string
let path_string = Test_tree.path_to_string

let phase = function
  | Failure.Setup -> "setup"
  | Failure.Body -> "body"
  | Failure.Teardown -> "teardown"
  | Failure.Release -> "release"

let withheld = function
  | None -> ""
  | Some Failure.Failed_outside -> ", withheld: failed outside"
  | Some Failure.Skipped -> ", withheld: skipped"
  | Some (Failure.Refused _) -> ", withheld: refused"
  | Some Failure.Conflict -> ", withheld: conflict"

(* A failure as the runner's claims read it: its phase, the kind of failure
   the runner made of it, and its subtest label. The words of a message are
   the runner's own and are judged as baselines. *)
let line (f : Failure.t) =
  let kind =
    match f.kind with
    | Failure.Message _ -> "message"
    | Failure.Timeout { limit; case = None } -> strf "timeout %gs" limit
    | Failure.Timeout { limit; case = Some _ } ->
        strf "timeout %gs in a case" limit
    | Failure.Raise { actual = Some actual; _ } -> "raise " ^ actual.kept
    | Failure.Raise { actual = None; _ } -> "raise"
    | Failure.Baseline { state = Failure.Missing _; withheld = w; _ } ->
        "missing baseline" ^ withheld w
    | Failure.Baseline { state = Failure.Mismatch _; withheld = w; _ } ->
        "baseline mismatch" ^ withheld w
    | Failure.Baseline { state = Failure.Unresolvable _; _ } ->
        "unresolvable baseline"
    | Failure.Property _ -> "property"
    | Failure.Law _ -> "law"
    | Failure.Equality _ -> "equality"
    | Failure.Containment _ -> "containment"
  in
  let label =
    match f.subtest with [] -> "" | label -> " in " ^ String.concat "/" label
  in
  phase f.phase ^ " " ^ kind ^ label

let lines r path = List.map line (Recorded.failures r path)
let loc (f : Failure.t) = Option.map Loc.to_string f.loc
let locs r path = List.map loc (Recorded.failures r path)
let only = function [ x ] -> Some x | _ -> None
let failure r path = require_match only (Recorded.failures r path)

let message =
  require_match (fun (f : Failure.t) ->
      match f.kind with Failure.Message m -> Some m.kept | _ -> None)

let rows_of r = Run.results (Recorded.outcome r).run

let row_at r path =
  let at (row : Run.result) = List.equal String.equal row.path path in
  require_some (List.find_opt at (rows_of r))

let attempts_used r path = (row_at r path).attempts

let counted r =
  List.filter_map
    (fun (row : Run.result) ->
      if row.counted then Some (path_string row.path) else None)
    (rows_of r)

let stats r path = (row_at r path).prop_stats

let cases_run r path =
  Option.map (fun (s : Property.stats) -> s.cases) (stats r path)

let contents path =
  if Sys.file_exists path then
    Some (In_channel.with_open_bin path In_channel.input_all)
  else None

let touch path = close_out (open_out path)

(* Windows enforces no timeout ([Run.with_timeout] arms none there), so a
   body that only its limit ends skips there, and so does each test that
   reads what such a body recorded. *)
let needs_timeouts () =
  if Sys.win32 then skip ~reason:"no timeout is enforced on Windows" ()

let busy_forever () =
  needs_timeouts ();
  let n = ref 1 in
  while !n > 0 do
    incr n;
    if !n > 1_000_000 then n := 1
  done

let spin seconds =
  needs_timeouts ();
  let t0 = Unix.gettimeofday () in
  while Unix.gettimeofday () -. t0 < seconds do
    ignore (Sys.opaque_identity 1)
  done

let gen = Gen.int_range 0 100

(* Configuration *)

let mode_name = function
  | Baseline.Check -> "check"
  | Baseline.Corrected -> "corrected"
  | Baseline.Update -> "update"

let color_name = function
  | Os.Auto -> "auto"
  | Os.Always -> "always"
  | Os.Never -> "never"

let mutation_name = function
  | Run.No_mutation -> "none"
  | Run.Loop prefixes -> "loop " ^ String.concat "," prefixes
  | Run.Armed id -> "armed " ^ id

let broadcast_name (b : Run.broadcast) =
  match (b.selection, b.mutate) with
  | false, false -> "none"
  | true, false -> "selection"
  | false, true -> "mutate"
  | true, true -> "selection, mutate"

(* In the order of the record, which an expected list follows. *)
let fields (c : Run.config) =
  let opt f = function None -> "none" | Some v -> f v in
  let strings l = "[" ^ String.concat "; " l ^ "]" in
  [
    ("seed", Seed.to_string c.seed);
    ("filter", strings c.filter);
    ("exclude", strings c.exclude);
    ("tags", strings c.tags);
    ("exclude_tags", strings c.exclude_tags);
    ("shard", opt (fun (k, n) -> strf "%d/%d" k n) c.shard);
    ("failed_only", string_of_bool c.failed_only);
    ("bail", string_of_bool c.bail);
    ("stream", string_of_bool c.stream);
    ("baseline", mode_name c.baseline);
    ("timeout", opt string_of_float c.timeout);
    ("prop_count", opt string_of_int c.prop_count);
    ("log_dir", c.log_dir);
    ("allow_focus", string_of_bool c.allow_focus);
    ("color", color_name c.color);
    ("slow_threshold", string_of_float c.slow_threshold);
    ("verbose", string_of_bool c.verbose);
    ("junit", opt Fun.id c.junit);
    ("mutation", mutation_name c.mutation);
    ("github", string_of_bool c.github);
    ( "invocation",
      match c.invocation with `Exe cmd -> "exe " ^ cmd | `Mirrors -> "mirrors"
    );
    ("broadcast", broadcast_name c.broadcast);
  ]

let project names c =
  List.filter (fun (name, _) -> List.mem name names) (fields c)

let settings = list (pair string string)

let given_no_flag =
  [
    ("filter", "[]");
    ("exclude", "[]");
    ("tags", "[]");
    ("exclude_tags", "[]");
    ("shard", "none");
    ("failed_only", "false");
    ("bail", "false");
    ("stream", "false");
    ("baseline", "check");
    ("timeout", "none");
    ("prop_count", "none");
    ("allow_focus", "false");
    ("color", "auto");
    ("slow_threshold", "1.");
    ("verbose", "false");
    ("junit", "none");
    ("mutation", "none");
    ("github", "false");
    ("invocation", "mirrors");
    ("broadcast", "none");
  ]

let parent : Run.config =
  {
    seed = 0x5eedL;
    filter = [ "f" ];
    exclude = [ "e" ];
    tags = [ "t" ];
    exclude_tags = [ "x" ];
    shard = Some (1, 2);
    failed_only = true;
    bail = false;
    stream = true;
    baseline = Baseline.Update;
    timeout = Some 3.;
    prop_count = Some 7;
    log_dir = "parent";
    allow_focus = false;
    color = Os.Never;
    slow_threshold = 2.;
    verbose = true;
    junit = Some "out.xml";
    mutation = Run.Loop [ "lib/" ];
    github = true;
    invocation = `Exe "suite.exe";
    broadcast = { selection = true; mutate = true };
  }

let subset = Run.for_subset parent ~log_dir:"child" ~bail:true

let subset_rules =
  [
    ( "clears the selection",
      [
        ("filter", "[]");
        ("exclude", "[]");
        ("shard", "none");
        ("failed_only", "false");
      ] );
    ( "keeps the tags and the seed",
      [
        ("seed", Seed.to_string parent.seed);
        ("tags", "[t]");
        ("exclude_tags", "[x]");
      ] );
    ( "checks, allows focus and neither reports nor broadcasts",
      [
        ("baseline", "check");
        ("allow_focus", "true");
        ("junit", "none");
        ("broadcast", "none");
      ] );
    ( "keeps the mutation, which its tests may read",
      [ ("mutation", "loop lib/") ] );
    ( "captures into the log directory it is given",
      [ ("stream", "false"); ("log_dir", "child") ] );
    ( "bails as told and keeps every other field",
      [
        ("bail", "true");
        ("timeout", "3.");
        ("prop_count", "7");
        ("color", "never");
        ("slow_threshold", "2.");
        ("verbose", "true");
        ("github", "true");
        ("invocation", "exe suite.exe");
      ] );
  ]

let is_the_configuration_of_no_flag () =
  let given = Run.default_config () in
  equal settings given_no_flag (project (List.map fst given_no_flag) given)

let configuration =
  group "Configuration"
    [
      test "default_config is the configuration of a run given no flag"
        is_the_configuration_of_no_flag;
      test "default_config takes the log directory from Os.default_log_dir"
        (fun () ->
          equal string (Os.default_log_dir ()) (Run.default_config ()).log_dir);
      test "every default_config draws its own seed" (fun () ->
          let seed () = Seed.to_string (Run.default_config ()).seed in
          not_equal string (seed ()) (seed ()));
      cases "for_subset" ~name:fst subset_rules (fun (_, expected) ->
          equal settings expected (project (List.map fst expected) subset));
    ]

(* Run records and the ambient slot *)

let ambient, in_body, observed =
  let in_body = ref None and observed = ref [] in
  let observe (_ : Run.event) =
    let frame =
      match Run.current_frame () with
      | _ -> "a frame"
      | exception Invalid_argument _ -> "no frame"
    in
    observed := strf "active: %b, %s" (Run.active ()) frame :: !observed
  in
  let body () =
    in_body := Some (Run.current (), Run.current_test (), Run.prop_context ())
  in
  let r = Recorded.execute ~on_event:observe [ group "g" [ test "t" body ] ] in
  (r, !in_body, List.rev !observed)

let active_after_return = Run.active ()
let frame_after_return = escape Run.current_frame

let isolated, between, seen_logs =
  let seen = ref [] in
  let note () = seen := (Run.config (Run.current ())).log_dir :: !seen in
  let first = Recorded.execute [ test "t" note ] in
  let between = escape Run.current in
  let second = Recorded.execute [ test "t" note ] in
  ([ first; second ], between, List.rev !seen)

let observer_raised =
  Recorded.escaped
    (Recorded.execute ~on_event:(fun _ -> raise Boom) [ test "t" ignore ])

let active_after_observer = Run.active ()

let nested =
  let starts () =
    ignore (Run.execute (Run.default_config ()) ~suite:"inner" [])
  in
  Recorded.execute [ test "starts a run" starts ]

let outside_a_run =
  [
    ("current_frame", escape Run.current_frame); ("current", escape Run.current);
  ]

let the_record_keeps_its_configuration () =
  let config = Run.config (Recorded.outcome ambient).run in
  equal settings
    [
      ("seed", Seed.to_string Recorded.seed);
      ("log_dir", Recorded.log_dir ambient);
    ]
    (project [ "seed"; "log_dir" ] config)

let a_body_sees_the_record_execute_returns () =
  let run, _, _ = require_some in_body in
  equal (list string)
    (Recorded.executed ambient)
    (List.map
       (fun (row : Run.result) -> path_string row.path)
       (Run.results run))

let run_records =
  group "Run records"
    [
      test "config is the configuration the run was created with"
        the_record_keeps_its_configuration;
      test "a test body reads the record that execute returns"
        a_body_sees_the_record_execute_returns;
      test "each run's body reads the record of its own run" (fun () ->
          equal (list string) (List.map Recorded.log_dir isolated) seen_logs);
    ]

let a_nested_run_fails_its_caller () =
  equal (list string)
    [
      "body raise "
      ^ Failure.exn_to_string (Invalid_argument Run.active_run_error);
    ]
    (lines nested [ "starts a run" ])

let ambient_slot =
  group "The ambient slot"
    [
      cases "raises Invalid_argument outside a test" ~name:fst outside_a_run
        (fun (_, raised) ->
          raises_match Exn.invalid_arg (fun () -> replay raised));
      test "an observer runs in the active run, with no frame current"
        (fun () ->
          equal (list string)
            (List.init 3 (fun _ -> "active: true, no frame"))
            observed);
      test "no run is active after execute returns" (fun () ->
          is_false active_after_return);
      test "no frame outlives its run" (fun () ->
          raises_match Exn.invalid_arg (fun () -> replay frame_after_return));
      test "the slot is empty between two runs" (fun () ->
          raises_match Exn.invalid_arg (fun () -> replay between));
      test "the slot is emptied when an exception ends the run" (fun () ->
          is_false active_after_observer);
      test "a run started inside a run fails the test that started it"
        a_nested_run_fails_its_caller;
    ]

(* Frames *)

(* The reference labels each call by its drawn argument. *)
let stateful_labels =
  let labels name ~reach =
    let tick x =
      collect "ticked";
      classify "past three" (x > 3);
      cover "reached" (x >= reach)
    in
    stateful ~count:10 ~steps:12 name
      [ command "tick" (Gen.int_range 0 9 @-> returns unit) tick ignore ]
  in
  Recorded.execute [ labels "labels" ~reach:5; labels "unreachable" ~reach:500 ]

(* Each case makes three values, so each place labels it several times. *)
let stateful_places =
  let r =
    abstract "r"
      ~invariant:(fun () _ -> collect "invariant")
      ~release:(fun _ -> collect "release")
  in
  let pre () =
    collect "pre";
    true
  in
  let system () =
    collect "system";
    ref ()
  in
  Recorded.execute
    [
      stateful ~count:10 ~steps:3 "places"
        [ command "open" ~pre (Gen.unit @-> makes r) ignore system ];
    ]

let labels_counted r path =
  let s = require_some (stats r path) in
  let demand (c : Property.cover_status) = (c.label, c.satisfied) in
  (s.collected, List.map demand s.coverage)

(* The labels of a law, muted, restored and counted. *)
let muted_labels =
  let marked () =
    collect "x";
    cover "never" false
  in
  Recorded.execute
    [
      Run.prop ~count:3 "muted" gen (fun _ -> Run.without_labels marked);
      Run.prop ~count:3 "counted" gen (fun _ -> marked ());
      Run.prop ~count:3 "restored" gen (fun _ ->
          Run.without_labels ignore;
          collect "after");
    ]

let law_contexts =
  let seen = ref [] in
  let law _ = seen := Option.is_some (Run.prop_context ()) :: !seen in
  ignore (Recorded.execute [ Run.prop ~count:5 "reads its frame" gen law ]);
  List.sort_uniq Bool.compare !seen

let a_command_labels_its_case () =
  equal
    (pair (list (pair string int)) (list (pair string bool)))
    ( [ ("past three", 10); ("reached", 10); ("ticked", 10) ],
      [ ("reached", true) ] )
    (labels_counted stateful_labels [ "labels" ])

let an_unmet_demand_fails () =
  equal
    (list (pair string bool))
    [ ("reached", false) ]
    (snd (labels_counted stateful_labels [ "unreachable" ]))

let every_place_labels_its_case () =
  equal
    (list (pair string int))
    [ ("invariant", 10); ("pre", 10); ("release", 10); ("system", 10) ]
    (fst (labels_counted stateful_places [ "places" ]))

let frames =
  group "Frames"
    [
      test "a plain test's frame has no property context" (fun () ->
          let _, _, context = require_some in_body in
          is_none context);
      test "a law runs with the property context on its frame" (fun () ->
          equal (list bool) [ true ] law_contexts);
      test "a stateful reference labels its program's case once"
        a_command_labels_its_case;
      test "a demand from a stateful reference that no case meets fails"
        an_unmet_demand_fails;
      test
        "a stateful system function, pre, invariant and release label their \
         program's case"
        every_place_labels_its_case;
      test "without_labels marks and demands nothing" (fun () ->
          equal
            (pair (list (pair string int)) (list (pair string bool)))
            ([], [])
            (labels_counted muted_labels [ "muted" ]);
          equal string "fail body" (Recorded.row muted_labels [ "counted" ]));
      test "without_labels gives the law its context back" (fun () ->
          equal
            (list (pair string int))
            [ ("after", 3) ]
            (fst (labels_counted muted_labels [ "restored" ])));
    ]

(* Refused callers *)

let other_domain_error =
  "a function that reads the running test was called from a domain other than \
   the one running the tests; hand the result back to the test's domain"

let reads () = ignore (Run.current_test ())

let refused_operations =
  let accessor = Run.fixture ignore in
  [
    ("current_test", reads);
    ("subtest", fun () -> Run.subtest "s" ignore);
    ("output", fun () -> ignore (output ()));
    ( "check_baseline",
      fun () -> Run.check_baseline (Baseline.File "x.expected") "x" );
    ("temp_dir", fun () -> ignore (Run.temp_dir ()));
    ("temp_file", fun () -> ignore (Run.temp_file ()));
    ("setenv", fun () -> Run.setenv "WINDTRAP_TEST_REFUSED" (Some "x"));
    ("chdir", fun () -> Run.chdir ".");
    ("a fixture's accessor", accessor);
    ("a label", fun () -> collect "x");
  ]

let swallowing () = try reads () with Failure.Check_failure _ -> ()

(* The site of the refusal that another domain gets. *)
let refusal_site () =
  let refused () =
    match reads () with
    | () -> None
    | exception Failure.Check_failure f -> Option.map Loc.to_string f.loc
  in
  starts_with ~affix:"test/unit/test_run.ml:"
    (require_some (Domain.join (Domain.spawn refused)))

let spawning =
  List.map
    (fun (name, operation) ->
      (name, fun () -> Domain.join (Domain.spawn operation)))
    refused_operations
  @ [
      ("swallowed", fun () -> Domain.join (Domain.spawn swallowing));
      ("located", refusal_site);
      ( "untouched",
        fun () ->
          ignore (Domain.join (Domain.spawn (fun () -> Sys.opaque_identity 1)))
      );
      ("on the run's domain", reads);
    ]

(* The tests that spawn a domain run in a forked child, which hands back
   each test's row, then its failures, one line per test. The child's
   scratch directory is removed before it leaves by [_exit]. *)
let from_domains =
  Windtrap_test_support.Child.forked (fun () ->
      let r =
        Recorded.execute (List.map (fun (n, body) -> test n body) spawning)
      in
      let failed (f : Failure.t) =
        match f.kind with
        | Failure.Message m -> line f ^ ": " ^ m.kept
        | _ -> line f
      in
      let row (name, _) =
        String.concat "; "
          ((name ^ " -> " ^ Recorded.row r [ name ])
          :: List.map failed (Recorded.failures r [ name ]))
      in
      let rows = List.map row spawning in
      Scratch.remove_tree (Filename.dirname (Recorded.log_dir r));
      String.concat "\n" rows)

let from_domain name =
  match from_domains with
  | None -> skip ~reason:"POSIX only: the domains run in a forked child" ()
  | Some text ->
      let prefix = name ^ " -> " in
      require_some ~msg:text
        (List.find_opt
           (String.starts_with ~prefix)
           (String.split_on_char '\n' text))

let refused_by_domain = "fail body; body message: " ^ other_domain_error

let refused_callers =
  group "Refused callers"
    [
      cases
        "an operation from another domain raises there, and fails the test \
         when the join raises it again"
        ~name:fst refused_operations (fun (name, _) ->
          equal string (name ^ " -> " ^ refused_by_domain) (from_domain name));
      test "the refusal is located at the call" (fun () ->
          equal string "located -> pass" (from_domain "located"));
      test "a domain that swallows its refusal leaves its test passing"
        (fun () -> equal string "swallowed -> pass" (from_domain "swallowed"));
      test "a domain that reads nothing leaves its test passing" (fun () ->
          equal string "untouched -> pass" (from_domain "untouched"));
      test "an operation on the run's domain is not refused" (fun () ->
          equal string "on the run's domain -> pass"
            (from_domain "on the run's domain"));
    ]

(* The running test *)

let untouched_by_outside_calls =
  let acquisitions = ref 0 in
  let accessor = Run.fixture (fun () -> incr acquisitions) in
  let made = !acquisitions in
  let refused = escape accessor in
  (made, refused, !acquisitions)

let body_operations_outside_a_test =
  let accessor = Run.fixture ignore in
  [
    ("current_test", escape Run.current_test);
    ("subtest", escape (fun () -> Run.subtest "s" ignore));
    ( "check_baseline",
      escape (fun () -> Run.check_baseline (Baseline.File "x.expected") "x") );
    ("temp_dir", escape (fun () -> Run.temp_dir ()));
    ("temp_file", escape (fun () -> Run.temp_file ()));
    ("setenv", escape (fun () -> Run.setenv "WINDTRAP_TEST_OUTSIDE" (Some "x")));
    ("chdir", escape (fun () -> Run.chdir "."));
    ("a fixture's accessor", escape accessor);
  ]

let subtest_pos = ("test/subtest_decl.ml", 10, 0, 0)

let subtests, resumed, passed_through, bracket_released =
  let resumed = ref [] and passed = ref [] and released = ref false in
  let flaky = ref 0 in
  let catch name fn =
    match fn () with
    | () -> passed := (name ^ " returned") :: !passed
    | exception Failure.Control (`Skip _) ->
        passed := (name ^ " passed through") :: !passed
    | exception Out_of_memory -> passed := (name ^ " passed through") :: !passed
  in
  let r =
    Recorded.execute
      [
        test ~__POS__:subtest_pos "throws" (fun () ->
            Run.subtest "throws" (fun () -> raise Boom);
            resumed := "throws" :: !resumed);
        test "nested" (fun () ->
            Run.subtest "outer" (fun () ->
                Run.subtest "inner" (fun () -> equal ~msg:"ctx" int 1 2));
            Run.subtest "outer" (fun () -> fail "at the outer level"));
        test "controls" (fun () ->
            catch "a skip" (fun () ->
                Run.subtest "skips" (fun () -> skip ~reason:"later" ()));
            catch "a fatal exception" (fun () ->
                Run.subtest "fatal" (fun () -> raise Out_of_memory));
            Run.subtest "clean" (fun () -> fail "x"));
        test "layouts" (fun () ->
            Run.subtest "row-major" (fun () -> fail "bad shape");
            Run.subtest "col-major" ignore;
            Run.subtest "strided" (fun () -> fail "bad stride"));
        test ~retries:1 "flaky" (fun () ->
            incr flaky;
            let failing = !flaky = 1 in
            Run.subtest "sub" (fun () -> if failing then fail "first attempt"));
        bracket "bracketed"
          ~setup:(fun () -> 7)
          ~teardown:(fun _ ->
            released := true;
            fail "teardown")
          (fun resource ->
            Run.subtest "uses" (fun () -> equal int 0 resource);
            Run.subtest "fine" ignore);
        prop "law" gen (fun x ->
            Run.subtest "half" (fun () -> if x > 10 then fail "nope"));
        prop "law raises" gen (fun x ->
            Run.subtest "raises" (fun () -> if x > 10 then raise Boom));
        prop "nested law" gen (fun x ->
            Run.subtest "outer" (fun () ->
                Run.subtest "inner" (fun () -> if x > 10 then fail "deep")));
        stateful "stateful"
          [
            command "check"
              (Gen.int_range 0 20 @-> returns unit)
              ignore
              (fun x ->
                Run.subtest "small" (fun () -> if x > 10 then fail "big"));
          ];
        test ~timeout:0.02 "times out" (fun () ->
            Run.subtest "spins" busy_forever);
        prop ~count:5 "discards" gen (fun _ ->
            Run.subtest "assumes" (fun () -> assume false));
        bracket "phases"
          ~setup:(fun () -> Run.subtest "in setup" (fun () -> fail "s"))
          ~teardown:(fun () -> Run.subtest "in teardown" (fun () -> fail "t"))
          ignore;
      ]
  in
  (r, List.rev !resumed, List.rev !passed, !released)

let checkpoints, checked =
  let checked = ref [] in
  let note step = checked := step :: !checked in
  let literal line =
    Baseline.Literal
      { pos = ("t.ml", line, 2, 20); value = "old"; exact = true }
  in
  let outside =
    Baseline.Literal
      { pos = ("../outside.ml", 1, 0, 0); value = "old"; exact = true }
  in
  let subject =
    Baseline.Literal { pos = ("test/x.ml", 3, 0, 0); value = "a"; exact = true }
  in
  let given = { Loc.file = "test/x.ml"; line = 3; column = 0 } in
  let r =
    Recorded.execute
      [
        test "two" (fun () ->
            Run.check_baseline (literal 1) "new";
            note "first";
            Run.check_baseline (literal 2) "new";
            note "second");
        test "unprovable" (fun () ->
            Run.check_baseline outside "new";
            note "after the unprovable one");
        test ~__POS__:("test/located_decl.ml", 5, 0, 0) "located" (fun () ->
            Run.check_baseline ~loc:given subject "b";
            Run.check_baseline subject "b";
            Run.subtest "s" (fun () ->
                Run.check_baseline ~loc:given subject "b"));
      ]
  in
  (r, List.rev !checked)

(* The registry resolves a file under the project root, from the directory the
   run started in. *)
let unreadable_baseline =
  let root = Scratch.dir "windtrap-unreadable-" in
  Unix.mkdir (Filename.concat root "dir.expected") 0o700;
  let raised = ref None in
  let reads () =
    raised :=
      escape (fun () -> Run.check_baseline (Baseline.File "dir.expected") "x")
  in
  let home = Sys.getcwd () in
  Unix.chdir root;
  Fun.protect
    ~finally:(fun () -> Unix.chdir home)
    (fun () ->
      ignore
        (Recorded.execute
           ~env:[ ("WINDTRAP_PROJECT_ROOT", root) ]
           [ test "reads a directory" reads ]));
  !raised

let the_labels_of_nested_subtests () =
  equal (list string)
    [ "body equality in nested/outer/inner"; "body message in nested/outer" ]
    (lines subtests [ "nested" ])

let the_label_never_enters_msg () =
  let msg (f : Failure.t) =
    Option.map (fun (m : Failure.text) -> m.kept) f.msg
  in
  equal
    (list (option string))
    [ Some "ctx"; None ]
    (List.map msg (Recorded.failures subtests [ "nested" ]))

(* The shrunk counterexample of a law that failed, and the failure of the law
   on it. *)
let shrunk r path =
  require_match
    (fun (f : Failure.t) ->
      match f.kind with
      | Failure.Property { rendered; inner = Some inner; _ } ->
          Some (rendered.kept, line inner)
      | _ -> None)
    (failure r path)

let a_subtest_in_a_law_fails_the_case () =
  equal (list string) [ "body property" ] (lines subtests [ "law" ]);
  equal (pair string string)
    ("11", "body message in law/half")
    (shrunk subtests [ "law" ])

let a_subtest_exception_is_at_the_declaration () =
  equal
    (list (option string))
    [ Some "test/subtest_decl.ml:10" ]
    (locs subtests [ "throws" ])

let each_failing_subtest_is_one_entry () =
  equal (list string)
    [ "body message in layouts/row-major"; "body message in layouts/strided" ]
    (lines subtests [ "layouts" ]);
  mem string "layouts" (counted subtests)

let a_teardown_follows_a_failing_subtest () =
  is_true bracket_released;
  equal (list string)
    [ "body equality in bracketed/uses"; "teardown message" ]
    (lines subtests [ "bracketed" ])

let a_subtest_failure_stays_in_the_body () =
  equal (list string)
    [ "body message in phases/in setup"; "body message in phases/in teardown" ]
    (lines subtests [ "phases" ])

let an_unresolvable_path_raises () =
  equal (list string)
    [ "body unresolvable baseline" ]
    (lines checkpoints [ "unprovable" ]);
  equal (list string) [ "first"; "second" ] checked

let check_baseline_locates_its_failure () =
  equal
    (list (option string))
    [ Some "test/x.ml:3"; Some "test/located_decl.ml:5"; Some "test/x.ml:3" ]
    (locs checkpoints [ "located" ])

let check_baseline_labels_in_a_subtest () =
  equal (list string)
    [
      "body baseline mismatch";
      "body baseline mismatch";
      "body baseline mismatch in located/s";
    ]
    (lines checkpoints [ "located" ])

let the_running_test =
  group "The running test"
    [
      cases "raises Invalid_argument outside a test" ~name:fst
        body_operations_outside_a_test (fun (_, raised) ->
          raises_match Exn.invalid_arg (fun () -> replay raised));
      test "current_test is the path of the running test" (fun () ->
          let _, path, _ = require_some in_body in
          equal (list string) [ "g"; "t" ] path);
      test "an exception in a subtest is a raise failure labelled with it"
        (fun () ->
          equal (list string)
            [ "body raise Test_run.Boom in throws/throws" ]
            (lines subtests [ "throws" ]));
      test "an exception in a subtest is located at the test's declaration"
        a_subtest_exception_is_at_the_declaration;
      test "the test goes on after a failing subtest" (fun () ->
          equal (list string) [ "throws" ] resumed);
      test "a label is the test, then the open subtests, outermost first"
        the_labels_of_nested_subtests;
      test "a label never enters the failure's msg" the_label_never_enters_msg;
      test "a skip and a fatal exception pass through a subtest" (fun () ->
          equal (list string)
            [ "a skip passed through"; "a fatal exception passed through" ]
            passed_through);
      test "a subtest that a control left leaves no label open" (fun () ->
          equal (list string)
            [ "body message in controls/clean" ]
            (lines subtests [ "controls" ]));
      test "each failing subtest is one entry, in order, and fails the test"
        each_failing_subtest_is_one_entry;
      test "the failures of subtests end with their attempt" (fun () ->
          equal (pair string int) ("pass", 2)
            ( Recorded.row subtests [ "flaky" ],
              attempts_used subtests [ "flaky" ] ));
      test "a teardown runs after a failing subtest and fails after it"
        a_teardown_follows_a_failing_subtest;
      test "a subtest failure in a law fails the case, which shrinks"
        a_subtest_in_a_law_fails_the_case;
      test "an exception in a subtest in a law fails the case, labelled"
        (fun () ->
          equal (pair string string)
            ("11", "body raise Test_run.Boom in law raises/raises")
            (shrunk subtests [ "law raises" ]));
      test "an outer subtest keeps the label that an inner one raised with"
        (fun () ->
          equal (pair string string)
            ("11", "body message in nested law/outer/inner")
            (shrunk subtests [ "nested law" ]));
      test "a subtest failure in a stateful function fails the program"
        (fun () ->
          equal (list string) [ "body property" ]
            (lines subtests [ "stateful" ]);
          equal (pair string string)
            (" #  call\n 1  check 11", "body message in stateful/small")
            (shrunk subtests [ "stateful" ]));
      test "a timeout passes through a subtest, unlabelled" (fun () ->
          needs_timeouts ();
          equal (list string) [ "body timeout 0.02s" ]
            (lines subtests [ "times out" ]));
      test "an assume in a subtest in a law discards the case" (fun () ->
          equal (list string) [ "body message" ] (lines subtests [ "discards" ]);
          contains ~sub:"property gave up"
            (message (failure subtests [ "discards" ])));
      test "a subtest failure stays in the body, in a setup and a teardown"
        a_subtest_failure_stays_in_the_body;
      test "check_baseline records a mismatch and returns" (fun () ->
          equal (list string)
            [ "body baseline mismatch"; "body baseline mismatch" ]
            (lines checkpoints [ "two" ]));
      test "check_baseline raises an unresolvable path out of the body"
        an_unresolvable_path_raises;
      test "check_baseline's loc locates the failure, else the declaration"
        check_baseline_locates_its_failure;
      test "check_baseline in a subtest labels its failure as subtest does"
        check_baseline_labels_in_a_subtest;
      test "check_baseline raises Sys_error on a file it cannot read" (fun () ->
          raises_match Exn.sys_error (fun () -> replay unreadable_baseline));
    ]

(* Temporary paths *)

let describe path =
  let st = Unix.stat path in
  let mode = st.st_perm land 0o777 in
  match st.st_kind with
  | Unix.S_DIR ->
      strf "directory %o%s" mode
        (if Sys.readdir path = [||] then ", empty" else "")
  | Unix.S_REG -> strf "file %o, %d bytes" mode st.st_size
  | Unix.S_CHR | Unix.S_BLK | Unix.S_LNK | Unix.S_FIFO | Unix.S_SOCK -> "other"

type made = {
  paths : (string * string) list; (* each path the test made, by role *)
  described : (string * string) list; (* each path described while it existed *)
  root : string; (* the description of their directory *)
}

type scratch = {
  run : Recorded.execution;
  made : made option; (* [None] when the creating test did not finish *)
  unmade : exn option; (* what temp_dir raised with no temporary directory *)
  leftovers : string list; (* the paths of every test that outlived the run *)
  retries : string list; (* the directory of each attempt of the retried test *)
  stale : string list; (* earlier attempts' directories that a retry found *)
}

let scratch =
  let made = ref None
  and all = ref []
  and retries = ref []
  and stale = ref [] in
  let unmade_by = ref None in
  let keep path = all := path :: !all in
  let unmade () =
    let saved = Filename.get_temp_dir_name () in
    Filename.set_temp_dir_name "/nonexistent/windtrap-temp";
    Fun.protect
      ~finally:(fun () -> Filename.set_temp_dir_name saved)
      (fun () -> escape (fun () -> Run.temp_dir ~prefix:"elsewhere" ()))
  in
  let creates () =
    let paths =
      [
        ("dir", Run.temp_dir ());
        ("repo", Run.temp_dir ~prefix:"repo" ());
        ("file", Run.temp_file ());
        ("json", Run.temp_file ~suffix:".json" ());
        ("hostile dir", Run.temp_dir ~prefix:"../evil" ());
        ("hostile file", Run.temp_file ~suffix:"/evil" ());
      ]
    in
    List.iter (fun (_, path) -> keep path) paths;
    let described =
      List.map (fun (role, path) -> (role, describe path)) paths
    in
    let root = describe (Filename.dirname (List.assoc "dir" paths)) in
    made := Some { paths; described; root }
  in
  let retried () =
    stale := List.filter Sys.file_exists !retries @ !stale;
    let dir = Run.temp_dir () in
    keep dir;
    retries := dir :: !retries;
    if List.length !retries < 3 then fail "again"
  in
  let run =
    Recorded.execute
      [
        test "creates" creates;
        test "cannot make its directory" (fun () -> unmade_by := unmade ());
        test "fails" (fun () ->
            keep (Run.temp_dir ());
            fail "boom");
        test "skips" (fun () ->
            keep (Run.temp_dir ());
            skip ());
        test "locks its directory" (fun () ->
            let dir = Run.temp_dir () in
            keep dir;
            touch (Filename.concat dir "file");
            Unix.chmod dir 0o000;
            Unix.chmod (Filename.dirname dir) 0o500);
        bracket "tears down" ~setup:ignore
          ~teardown:(fun () ->
            keep (Run.temp_dir ~prefix:"td" ());
            fail "teardown")
          (fun () -> keep (Run.temp_dir ()));
        test ~retries:2 "retried" retried;
      ]
  in
  {
    run;
    made = !made;
    unmade = !unmade_by;
    leftovers = List.filter Sys.file_exists !all;
    retries = List.rev !retries;
    stale = !stale;
  }

let scratch_under_bail, bail_scratch =
  let made = ref [] in
  let keep path = made := path :: !made in
  let r =
    Recorded.execute
      ~config:(fun c -> { c with bail = true })
      [
        bracket "tears down" ~setup:ignore
          ~teardown:(fun () ->
            keep (Run.temp_dir ~prefix:"td" ());
            fail "teardown")
          (fun () -> keep (Run.temp_dir ()));
        test "never runs" ignore;
      ]
  in
  (r, (List.length !made, List.filter Sys.file_exists !made))

let scratch_made () = require_some scratch.made

let scratch_name role =
  Filename.basename (List.assoc role (scratch_made ()).paths)

let removes_a_link_and_never_its_target () =
  if Sys.win32 then skip ~reason:"no symbolic links" ();
  let root = temp_dir () in
  let kept = Filename.concat root "kept"
  and tree = Filename.concat root "tree" in
  Unix.mkdir kept 0o700;
  touch (Filename.concat kept "file");
  Unix.mkdir tree 0o700;
  Unix.symlink kept (Filename.concat tree "link");
  Run.remove_tree tree;
  is_false (Sys.file_exists tree);
  equal (list string) [ "file" ] (Array.to_list (Sys.readdir kept))

let removes_what_the_test_locked () =
  if Sys.win32 then skip ~reason:"POSIX permissions" ();
  let tree = Filename.concat (temp_dir ()) "tree" in
  let unreadable = Filename.concat tree "unreadable" in
  Unix.mkdir tree 0o700;
  Unix.mkdir unreadable 0o700;
  touch (Filename.concat unreadable "file");
  Unix.chmod unreadable 0o000;
  Unix.chmod tree 0o500;
  Run.remove_tree tree;
  is_false (Sys.file_exists tree)

(* [remove_tree] never changes the mode of its argument's parent, and a
   read-only parent refuses the removal of the directory itself: its entries
   go, the last step fails, and the failure is ignored. *)
let ignores_an_error_on_the_way () =
  if Sys.win32 then skip ~reason:"Windows has no directory modes" ();
  if Unix.geteuid () = 0 then skip ~reason:"root removes any entry" ();
  let parent = Filename.concat (temp_dir ()) "parent" in
  let child = Filename.concat parent "child" in
  Unix.mkdir parent 0o700;
  Unix.mkdir child 0o700;
  touch (Filename.concat child "entry");
  Unix.chmod parent 0o500;
  Run.remove_tree child;
  equal (list string) [] (Array.to_list (Sys.readdir child))

let the_empty_suffix_adds_nothing () =
  let name = scratch_name "file" in
  starts_with ~affix:"file-" name;
  is_some (int_of_string_opt (String.sub name 5 (String.length name - 5)))

let every_path_is_in_one_directory () =
  let dirs =
    List.map (fun (_, p) -> Filename.dirname p) (scratch_made ()).paths
  in
  equal (list string) (List.map (fun _ -> List.hd dirs) dirs) dirs

let makes_empty_private_paths () =
  equal settings
    [
      ("dir", "directory 700, empty");
      ("repo", "directory 700, empty");
      ("file", "file 600, 0 bytes");
      ("json", "file 600, 0 bytes");
    ]
    (List.filter
       (fun (role, _) -> List.mem role [ "dir"; "repo"; "file"; "json" ])
       (scratch_made ()).described)

let a_retry_gets_its_own_directory () =
  equal int 3 (List.length (List.sort_uniq String.compare scratch.retries));
  equal (list string) [] scratch.stale

let the_removal_changes_no_row () =
  equal (list string)
    [
      "creates: pass";
      "cannot make its directory: pass";
      "fails: fail body";
      "skips: skip";
      "locks its directory: pass";
      "tears down: fail teardown";
      "retried: pass";
    ]
    (List.map
       (fun path -> path ^ ": " ^ Recorded.row scratch.run [ path ])
       (Recorded.executed scratch.run))

let temporary_paths =
  group "Temporary paths"
    [
      test "temp_dir and temp_file make empty paths of mode 0o700 and 0o600"
        (fun () ->
          if Sys.win32 then skip ~reason:"POSIX only" ();
          makes_empty_private_paths ());
      test "temp_dir makes a new directory at every call" (fun () ->
          not_equal string (scratch_name "dir") (scratch_name "repo"));
      test "temp_dir's prefix starts the directory's name" (fun () ->
          starts_with ~affix:"repo" (scratch_name "repo"));
      test "temp_file's suffix ends the file's name" (fun () ->
          ends_with ~affix:".json" (scratch_name "json"));
      test "temp_file's empty suffix adds nothing to the name"
        the_empty_suffix_adds_nothing;
      test "an attempt's paths share one directory, hostile names included"
        every_path_is_in_one_directory;
      test "the directory of an attempt has mode 0o700" (fun () ->
          if Sys.win32 then skip ~reason:"POSIX only" ();
          starts_with ~affix:"directory 700" (scratch_made ()).root);
      test "temp_dir raises Unix_error when no directory can be made" (fun () ->
          raises_match
            (function Unix.Unix_error _ -> true | _ -> false)
            (fun () -> replay scratch.unmade));
      test "the directory is removed when the attempt ends, however it ends"
        (fun () -> equal (list string) [] scratch.leftovers);
      test "the removal changes no row" the_removal_changes_no_row;
      test "a retry gets a directory of its own" a_retry_gets_its_own_directory;
      test "a raising teardown's directory is removed under bail too" (fun () ->
          equal (list string) [ "tears down" ]
            (Recorded.executed scratch_under_bail);
          equal (pair int (list string)) (2, []) bail_scratch);
      test "remove_tree removes a symbolic link and never its target"
        removes_a_link_and_never_its_target;
      test "remove_tree of a missing path raises nothing" (fun () ->
          Run.remove_tree (Filename.concat (temp_dir ()) "missing"));
      test "remove_tree removes directories the test made unreadable"
        removes_what_the_test_locked;
      test "remove_tree ignores an error on the way" ignores_an_error_on_the_way;
    ]

(* Process state *)

let unset_var = "WINDTRAP_TEST_UNSET"
let bound_var = "WINDTRAP_TEST_BOUND"
let twice_var = "WINDTRAP_TEST_TWICE"
let dropped_var = "WINDTRAP_TEST_DROPPED"
let scoped_vars = [ unset_var; bound_var; twice_var; dropped_var ]
let bindings () = List.map (fun name -> (name, Sys.getenv_opt name)) scoped_vars

(* The bindings are read by the observer as each test finishes: the recorded
   environment is gone once [Recorded.execute] returns. *)
let environment, bound_inside, bound_after, rejections =
  let inside = ref [] and after = ref [] and rejections = ref [] in
  let on_event = function
    | Run.Test_finished row ->
        after := (path_string row.path, bindings ()) :: !after
    | Run.Run_started _ | Run.Test_started _ | Run.Fixture_release _
    | Run.Interrupted _ ->
        ()
  in
  let binds () =
    Run.setenv unset_var (Some "inside");
    Run.setenv bound_var (Some "inside");
    Run.setenv twice_var (Some "first");
    Run.setenv twice_var (Some "second");
    Run.setenv dropped_var None;
    inside := bindings ()
  in
  let rejects () =
    rejections :=
      [
        escape (fun () -> Run.setenv "" (Some "x"));
        escape (fun () -> Run.setenv "BAD=NAME" (Some "x"));
      ]
  in
  let r =
    Recorded.execute ~on_event
      ~env:
        [
          (bound_var, "before"); (twice_var, "before"); (dropped_var, "before");
        ]
      [
        test "binds" binds;
        test "rejects" rejects;
        test "fails" (fun () ->
            Run.setenv bound_var (Some "failing");
            fail "boom");
        test "skips" (fun () ->
            Run.setenv bound_var (Some "skipping");
            skip ());
        test ~timeout:0.02 "times out" (fun () ->
            Run.setenv bound_var (Some "hanging");
            busy_forever ());
      ]
  in
  (r, !inside, List.rev !after, !rejections)

let after_test name var = List.assoc var (List.assoc name bound_after)

let restorations =
  [
    ("found unset", unset_var, None);
    ("found set", bound_var, Some "before");
    ("set twice", twice_var, Some "before");
    ("unset by the test", dropped_var, Some "before");
  ]

let cwd () = try Sys.getcwd () with Sys_error _ -> "<unreadable>"

type moved = {
  home : string;
  entered : string list; (* the directory each attempt started in *)
  targets : string list; (* the directory each attempt moved to, resolved *)
  inside : string list; (* the directory each attempt was in after it moved *)
  between : string list; (* the directory the observer found *)
  final : string;
}

let moved =
  let home = Sys.getcwd () in
  let entered = ref [] and targets = ref [] and inside = ref [] in
  let between = ref [] in
  let moves () =
    entered := cwd () :: !entered;
    let dir = Run.temp_dir () in
    targets := Unix.realpath dir :: !targets;
    Run.chdir dir;
    inside := Unix.realpath (cwd ()) :: !inside;
    if List.length !inside < 3 then fail "again"
  in
  let on_event = function
    | Run.Test_started _ | Run.Test_finished _ -> between := cwd () :: !between
    | Run.Run_started _ | Run.Fixture_release _ | Run.Interrupted _ -> ()
  in
  ignore (Recorded.execute ~on_event [ test ~retries:2 "moves" moves ]);
  let final = cwd () in
  (try Unix.chdir home with Unix.Unix_error _ -> ());
  {
    home;
    entered = List.rev !entered;
    targets = List.rev !targets;
    inside = List.rev !inside;
    between = List.rev !between;
    final;
  }

let stranded, gone, stranded_site =
  let root = Scratch.dir "windtrap-stranded-" in
  let gone = Filename.concat root "gone" in
  Unix.mkdir gone 0o700;
  let home = Sys.getcwd () in
  let site = ref None in
  let strands () =
    Unix.chdir gone;
    let pos = __POS__ and () = Run.chdir root in
    site := Some pos;
    Unix.rmdir gone
  in
  let r = Recorded.execute [ test "strands" strands ] in
  (try Unix.chdir home with Unix.Unix_error _ -> ());
  (r, gone, !site)

let no_removed_cwd = "Windows cannot remove a process's working directory"

let unenterable, unreadable =
  let unenterable = ref None and unreadable = ref None in
  let home = Sys.getcwd () in
  let root = Scratch.dir "windtrap-cwd-" in
  let reads_a_removed_directory () =
    if Sys.win32 then skip ~reason:no_removed_cwd ();
    let gone = Filename.concat root "gone" in
    Unix.mkdir gone 0o700;
    Unix.chdir gone;
    Unix.rmdir gone;
    let readable =
      match Sys.getcwd () with _ -> true | exception Sys_error _ -> false
    in
    let raised = escape (fun () -> Run.chdir root) in
    Unix.chdir home;
    unreadable := Some (readable, raised)
  in
  let enters () =
    unenterable := escape (fun () -> Run.chdir "/nonexistent/windtrap-dir")
  in
  ignore
    (Recorded.execute
       [
         test "unenterable" enters; test "unreadable" reads_a_removed_directory;
       ]);
  (!unenterable, !unreadable)

let setenv_binds_for_the_rest_of_the_test () =
  equal
    (list (pair string (option string)))
    [
      (unset_var, Some "inside");
      (bound_var, Some "inside");
      (twice_var, Some "second");
      (dropped_var, None);
    ]
    bound_inside

let a_rejected_name_records_nothing () =
  List.iter
    (fun raised -> raises_match Exn.invalid_arg (fun () -> replay raised))
    rejections;
  equal string "pass" (Recorded.row environment [ "rejects" ])

let the_first_chdir_reads_the_working_directory () =
  if Sys.win32 then skip ~reason:no_removed_cwd ();
  let readable, raised = require_some unreadable in
  if readable then skip ~reason:"a removed directory reads here" ();
  raises_match Exn.sys_error (fun () -> replay raised)

let the_restoration_failure_is_at_the_chdir () =
  equal
    (list (option string))
    [ Some (Loc.to_string (Loc.of_pos (require_some stranded_site))) ]
    (locs stranded [ "strands" ])

let process_state =
  group "Process state"
    [
      test "setenv binds for the rest of the test"
        setenv_binds_for_the_rest_of_the_test;
      cases "the attempt's end restores a variable"
        ~name:(fun (shape, _, _) -> shape)
        restorations
        (fun (_, var, expected) ->
          equal (option string) expected (after_test "binds" var));
      test "a name that setenv rejects raises and records nothing"
        a_rejected_name_records_nothing;
      cases "a binding is restored after a test that" ~name:Fun.id
        [ "fails"; "skips"; "times out" ] (fun name ->
          if name = "times out" then needs_timeouts ();
          equal (option string) (Some "before") (after_test name bound_var));
      test "chdir moves the process for the rest of the attempt" (fun () ->
          equal (list string) moved.targets moved.inside);
      test "every attempt starts in the directory of the first" (fun () ->
          equal (list string)
            [ moved.home; moved.home; moved.home ]
            moved.entered);
      test "the working directory is restored before the runner moves on"
        (fun () -> equal (list string) [ moved.home; moved.home ] moved.between);
      test "the run ends in the directory it started in" (fun () ->
          equal string moved.home moved.final);
      test "a directory that cannot be restored fails the teardown" (fun () ->
          equal (list string) [ "teardown message" ]
            (lines stranded [ "strands" ]));
      test "the restoration failure names the directory" (fun () ->
          contains ~sub:gone (message (failure stranded [ "strands" ])));
      test "the restoration failure is located at the chdir"
        the_restoration_failure_is_at_the_chdir;
      test "chdir raises Unix_error for a directory it cannot enter" (fun () ->
          raises_match
            (function Unix.Unix_error _ -> true | _ -> false)
            (fun () -> replay unenterable));
      test "the first chdir raises Sys_error when the directory cannot be read"
        the_first_chdir_reads_the_working_directory;
    ]

(* Fixtures *)

let on_release note = function
  | Run.Fixture_release { name } -> note name
  | Run.Run_started _ | Run.Test_started _ | Run.Test_finished _
  | Run.Interrupted _ ->
      ()

let shared_values, next_run_values =
  let acquisitions = ref 0 and seen = ref [] in
  let accessor =
    Run.fixture (fun () ->
        incr acquisitions;
        !acquisitions)
  in
  let use () = seen := accessor () :: !seen in
  ignore (Recorded.execute [ test "first" use; test "second" use ]);
  let first = List.rev !seen in
  seen := [];
  ignore (Recorded.execute [ test "third" use ]);
  (first, List.rev !seen)

let broken, broken_acquisitions, broken_announced =
  let attempts = ref 0 and announced = ref [] in
  let accessor =
    Run.fixture ~teardown:ignore (fun () ->
        incr attempts;
        raise No_db)
  in
  let on_event = on_release (fun name -> announced := name :: !announced) in
  let r =
    Recorded.execute ~on_event [ test "first" accessor; test "later" accessor ]
  in
  let in_first_run = !attempts in
  ignore (Recorded.execute ~on_event [ test "again" accessor ]);
  (r, [ in_first_run; !attempts ], !announced)

let unavailable, unavailable_acquisitions, unavailable_released =
  let attempts = ref 0 and released = ref [] in
  let accessor =
    Run.fixture
      ~teardown:(fun () -> released := "teardown" :: !released)
      (fun () ->
        incr attempts;
        skip ~reason:"no gpu" ())
  in
  let on_event = on_release (fun name -> released := name :: !released) in
  let r =
    Recorded.execute ~on_event [ test "first" accessor; test "later" accessor ]
  in
  let in_first_run = !attempts in
  ignore (Recorded.execute ~on_event [ test "again" accessor ]);
  (r, [ in_first_run; !attempts ], !released)

let cut_acquisition, cut_acquisitions =
  let acquisitions = ref 0 in
  let slow_first =
    Run.fixture (fun () ->
        incr acquisitions;
        if !acquisitions = 1 then busy_forever ();
        "acquired")
  in
  let r =
    Recorded.execute
      [
        test ~timeout:0.02 "acquires past its limit" (fun () ->
            ignore (slow_first ()));
        test "acquires again" (fun () ->
            equal string "acquired" (slow_first ()));
      ]
  in
  (r, !acquisitions)

let fixture_names, fixture_line =
  let accessor, line = (Run.fixture ~teardown:ignore ignore, __LINE__) in
  let names = ref [] in
  let on_event = on_release (fun name -> names := name :: !names) in
  ignore (Recorded.execute ~on_event [ test "acquires" accessor ]);
  (List.rev !names, line)

let making_an_accessor_acquires_nothing () =
  let made, _, _ = untouched_by_outside_calls in
  equal int 0 made

let a_call_outside_a_test_acquires_nothing () =
  let _, refused, acquisitions = untouched_by_outside_calls in
  raises_match Exn.invalid_arg (fun () -> replay refused);
  equal int 0 acquisitions

let a_skipped_acquisition_fails_nothing () =
  equal (list string) [] unavailable_released;
  equal int 0 (Recorded.exit_code unavailable)

let a_failed_acquisition_fails_its_callers () =
  equal
    (list (list string))
    [ [ "body raise Test_run.No_db" ]; [ "body raise Test_run.No_db" ] ]
    [ lines broken [ "first" ]; lines broken [ "later" ] ]

let a_skipped_acquisition_skips_its_callers () =
  equal (list string)
    [ "skip no gpu"; "skip no gpu" ]
    [
      Recorded.row unavailable [ "first" ]; Recorded.row unavailable [ "later" ];
    ]

let fixtures =
  group "Fixtures"
    [
      test "making an accessor acquires nothing"
        making_an_accessor_acquires_nothing;
      test "an accessor called outside a test raises and acquires nothing"
        a_call_outside_a_test_acquires_nothing;
      test "the first call acquires, and later calls in the run share it"
        (fun () -> equal (list int) [ 1; 1 ] shared_values);
      test "a later run acquires again" (fun () ->
          equal (list int) [ 2 ] next_run_values);
      test "a failed acquisition fails every test that calls the accessor"
        a_failed_acquisition_fails_its_callers;
      test "a failed acquisition is kept for the run and tried in the next"
        (fun () -> equal (list int) [ 1; 2 ] broken_acquisitions);
      test "a failed acquisition is never released" (fun () ->
          equal (list string) [] broken_announced);
      test "a skipped acquisition skips every test that calls the accessor"
        a_skipped_acquisition_skips_its_callers;
      test "a skipped acquisition is kept for the run and tried in the next"
        (fun () -> equal (list int) [ 1; 2 ] unavailable_acquisitions);
      test "a skipped acquisition is never released and fails nothing"
        a_skipped_acquisition_fails_nothing;
      test "a timeout while acquiring is not kept, and the next call acquires"
        (fun () ->
          needs_timeouts ();
          equal (pair string int) ("pass", 2)
            (Recorded.row cut_acquisition [ "acquires again" ], cut_acquisitions));
      test "a fixture is named after the site where fixture was applied"
        (fun () ->
          equal (list string)
            [ strf "fixture (test/unit/test_run.ml:%d)" fixture_line ]
            fixture_names);
    ]

(* Results *)

let row_facts =
  let attempts = ref 0 in
  Recorded.execute
    [
      slow "own" ignore;
      group ~tags:[ Test_tree.Tag.slow ] "g" [ test "inherited" ignore ];
      test "plain" ignore;
      test ~retries:2 "twice failed" (fun () ->
          incr attempts;
          Unix.sleepf 0.02;
          if !attempts < 3 then fail "not yet");
      Run.prop "skips" gen (fun _ -> skip ());
      Run.prop ~timeout:0.02 "times out in a case" gen (fun _ ->
          busy_forever ());
    ]

let xpass_pos = ("test/xfail_decl.ml", 12, 2, 30)

let known_bug =
  xfail ~reason:"issue #42" (test "known bug" (fun () -> fail "still broken"))

let fixed = xfail ~reason:"issue #42" (test ~__POS__:xpass_pos "fixed" ignore)
let undecided = xfail (test "undecided" (fun () -> skip ()))

let known_bad_law =
  xfail (prop "known bad law" Gen.int (fun n -> equal int n (n + 1)))

let leaky =
  xfail ~reason:"leaky teardown"
    (bracket "leaky" ~setup:ignore ~teardown:(fun () -> fail "leak") ignore)

let clean = xfail (bracket "clean" ~setup:ignore ~teardown:ignore ignore)

let keeps_failing =
  xfail (test ~retries:2 "keeps failing" (fun () -> fail "expected"))

let keeps_passing = xfail (test ~retries:2 "keeps passing" ignore)

let xfails =
  Recorded.execute
    [
      known_bug;
      fixed;
      undecided;
      known_bad_law;
      leaky;
      clean;
      keeps_failing;
      keeps_passing;
      test "healthy" ignore;
    ]

let expected_failures_only =
  Recorded.execute
    [
      known_bug;
      undecided;
      known_bad_law;
      leaky;
      keeps_failing;
      test "healthy" ignore;
    ]

let slow_tags () =
  let about = [ "own"; "g › inherited"; "plain" ] in
  let tagged (row : Run.result) = (path_string row.path, row.slow_tagged) in
  equal
    (list (pair string bool))
    [ ("own", true); ("g › inherited", true); ("plain", false) ]
    (List.filter
       (fun (path, _) -> List.mem path about)
       (List.map tagged (rows_of row_facts)))

let durations_sum_the_attempts () =
  at_least float_exact ~than:0.06 (row_at row_facts [ "twice failed" ]).duration;
  at_least float_exact ~than:0.06 (Recorded.outcome row_facts).duration

let an_expected_failure_keeps_its_failures () =
  equal string "xfail body" (Recorded.row xfails [ "known bug" ]);
  equal string "still broken" (message (failure xfails [ "known bug" ]))

let an_unexpected_pass_fails_at_its_declaration () =
  equal (list string) [ "body message" ] (lines xfails [ "fixed" ]);
  equal
    (list (option string))
    [ Some "test/xfail_decl.ml:12" ]
    (locs xfails [ "fixed" ])

let the_row_carries_its_annotation () =
  let reason path =
    Option.map
      (fun (x : Test_tree.xfail) -> x.reason)
      (row_at xfails path).xfail
  in
  equal
    (list (option (option string)))
    [ Some (Some "issue #42"); Some None; None ]
    [ reason [ "known bug" ]; reason [ "undecided" ]; reason [ "healthy" ] ]

let xfail_rows =
  [
    ( "an expected property failure keeps its property failure",
      "known bad law",
      "xfail body" );
    ( "an xfail test whose teardown fails is an expected failure",
      "leaky",
      "xfail teardown" );
    ( "an xfail test that passes everywhere is an unexpected pass",
      "clean",
      "fail body" );
    ("a skip stays a skip under xfail", "undecided", "skip");
  ]

let results =
  group "Results"
    [
      test "slow_tagged is the test's own slow tag or a group's" slow_tags;
      test "duration sums the attempts, and the run's covers its tests"
        durations_sum_the_attempts;
      test "a property that a skip ended has no statistics" (fun () ->
          equal (option int) None (cases_run row_facts [ "skips" ]));
      test "a skip in a law skips the test" (fun () ->
          equal string "skip" (Recorded.row row_facts [ "skips" ]));
      test "a property that a timeout ended in a case has the cases before it"
        (fun () ->
          needs_timeouts ();
          equal (option int) (Some 0)
            (cases_run row_facts [ "times out in a case" ]));
      test "an expected failure keeps its failures and does not count"
        an_expected_failure_keeps_its_failures;
      test "an unexpected pass fails with a message at its declaration"
        an_unexpected_pass_fails_at_its_declaration;
      test "the message of an unexpected pass gives the reason" (fun () ->
          expect (message (failure xfails [ "fixed" ]))
          @@ __POS_OF__ {| expected to fail (issue #42), but the test passed |});
      cases "the rows of expected failures"
        ~name:(fun (claim, _, _) -> claim)
        xfail_rows
        (fun (_, path, row) -> equal string row (Recorded.row xfails [ path ]));
      test "only the unexpected passes count as failed" (fun () ->
          equal (list string)
            [ "fixed"; "clean"; "keeps passing" ]
            (counted xfails));
      test "the row carries its xfail annotation" the_row_carries_its_annotation;
    ]

(* Properties *)

let prop_pos = ("test/prop_decl.ml", 21, 2, 0)

let fails_in_tail_position _ =
  raise (Failure.Check_failure (Failure.equality ~expected:"0" ~actual:"1" ()))

let props =
  Recorded.execute
    [
      Run.prop "holds" Gen.int ignore;
      Run.prop ~count:3 "declares a count" Gen.int ignore;
      Run.prop "never holds" Gen.int (fun n -> equal int n (n + 1));
      Run.prop ~examples:[ 0 ] "fails on an example" Gen.int (fun n ->
          not_equal int 0 n);
      Run.prop ~__POS__:prop_pos "gives up" gen (fun _ -> assume false);
      Run.prop ~__POS__:prop_pos "misses a label" gen (fun _ ->
          cover "never" false);
      Run.prop ~count:(-1) "negative count" gen ignore;
      Run.prop ~max_discard:(-1) "negative max_discard" gen ignore;
      Run.prop "summarized"
        ~summary:(fun value -> Some (strf "n=%d" value))
        (Gen.int_range 0 1000)
        (fun value -> less int ~than:10 value);
      Run.prop "unsummarized" (Gen.int_range 0 1000) (fun value ->
          less int ~than:10 value);
      Run.prop ~__POS__:prop_pos ~count:1 "fails in tail position"
        (Gen.constant 0) fails_in_tail_position;
    ]

let prop_counted =
  Recorded.execute
    ~config:(fun c -> { c with prop_count = Some 5 })
    [
      Run.prop "counted" Gen.int ignore;
      Run.prop ~count:2 "declared" Gen.int ignore;
      Run.prop "counted fails" Gen.int (fun _ -> fail "no");
      Run.prop ~count:2 "declared fails" Gen.int (fun _ -> fail "no");
    ]

let twice_drawn =
  let tests = [ Run.prop "shrinks" Gen.int (fun n -> equal int n (n + 1)) ] in
  [ Recorded.execute tests; Recorded.execute tests ]

(* A law that returns can outlive its limit and hand the signal to the engine
   between two cases, so each law below blocks where the limit must cut it. *)
let timed_props, timed_wall =
  let calls = ref 0 and cases = ref 0 in
  let mid_shrink n =
    incr calls;
    if !calls > 1 then busy_forever ();
    less int ~than:1 n
  in
  let passes_three _ =
    incr cases;
    if !cases > 3 then busy_forever ()
  in
  let started = Unix.gettimeofday () in
  let r =
    Recorded.execute
      ~config:(fun c -> { c with timeout = Some 0.1 })
      [
        Run.prop "cut while shrinking" (Gen.int_range 0 1000) mid_shrink;
        Run.prop "cut before a failure" Gen.int passes_three;
        Run.prop ~timeout:0.05 "declares a limit" Gen.int (fun _ ->
            busy_forever ());
      ]
  in
  (r, Unix.gettimeofday () -. started)

let seed_and_count r path =
  require_match
    (fun (f : Failure.t) ->
      match f.kind with
      | Failure.Property p -> Some (Seed.to_string p.root, p.count)
      | _ -> None)
    (failure r path)

let summary_of r path =
  require_match
    (fun (f : Failure.t) ->
      match f.kind with
      | Failure.Property p ->
          Some (Option.map (fun (s : Failure.text) -> s.kept) p.summary)
      | _ -> None)
    (failure r path)

let counterexample r path =
  require_match
    (fun (f : Failure.t) ->
      match f.kind with
      | Failure.Property p -> Some (p.rendered.kept, p.case_index)
      | _ -> None)
    (failure r path)

let example_facts r path =
  require_match
    (fun (f : Failure.t) ->
      match f.kind with
      | Failure.Property p -> Some (p.examples, p.shrink_steps)
      | _ -> None)
    (failure r path)

let inner_loc r path =
  require_match
    (fun (f : Failure.t) ->
      match f.kind with
      | Failure.Property { inner = Some inner; _ } -> Some (loc inner)
      | _ -> None)
    (failure r path)

let shrink_end r path =
  require_match
    (fun (f : Failure.t) ->
      match f.kind with
      | Failure.Property { shrink_end = Failure.Timed_out limit; _ } ->
          Some (strf "timed out after %gs" limit)
      | Failure.Property { shrink_end = Failure.Converged; _ } ->
          Some "converged"
      | Failure.Property { shrink_end = Failure.Budget_spent; _ } ->
          Some "budget spent"
      | Failure.Property { shrink_end = Failure.Candidate_raised _; _ } ->
          Some "candidate raised"
      | _ -> None)
    (failure r path)

let timed_case r path =
  require_match
    (fun (f : Failure.t) ->
      match f.kind with
      | Failure.Timeout { limit; case = Some c } ->
          Some
            ( strf "limit %gs, example %b, root %s, count %s" limit c.examples
                (Seed.to_string c.root)
                (Option.fold ~none:"none" ~some:string_of_int c.count),
              c.case_index,
              c.passed )
      | _ -> None)
    (failure r path)

let a_limit_cut_before_a_failure_names_the_case () =
  needs_timeouts ();
  let facts, case_index, passed =
    timed_case timed_props [ "cut before a failure" ]
  in
  equal string
    (strf "limit 0.1s, example false, root %s, count none"
       (Seed.to_string Recorded.seed))
    facts;
  equal (pair int int) (3, 3) (case_index, passed)

let a_failing_law_adds_the_engine_failure () =
  equal (list string) [ "body property" ] (lines props [ "never holds" ]);
  equal
    (pair string (option int))
    (Seed.to_string Recorded.seed, None)
    (seed_and_count props [ "never holds" ]);
  is_some (cases_run props [ "never holds" ])

let gave_up_and_missed_at_the_declaration () =
  equal
    (list (list (option string)))
    [ [ Some "test/prop_decl.ml:21" ]; [ Some "test/prop_decl.ml:21" ] ]
    [ locs props [ "gives up" ]; locs props [ "misses a label" ] ]

let the_same_seed_draws_the_same_counterexample () =
  match twice_drawn with
  | [ first; second ] ->
      equal (pair string int)
        (counterexample first [ "shrinks" ])
        (counterexample second [ "shrinks" ])
  | _ -> fail "two runs were recorded"

let run_prop_adds_no_tag () =
  let case =
    require_match only (Test_tree.flatten [ Run.prop "p" gen ignore ])
  in
  is_false (Test_tree.Tag.mem Test_tree.Tag.prop case.tags)

let a_holding_law_runs_the_default_count () =
  equal
    (pair string (option int))
    ("pass", Some 100)
    (Recorded.row props [ "holds" ], cases_run props [ "holds" ])

let the_engine_knows_where_its_count_came_from () =
  equal
    (list (option int))
    [ Some 5; None ]
    [
      snd (seed_and_count prop_counted [ "counted fails" ]);
      snd (seed_and_count prop_counted [ "declared fails" ]);
    ]

let a_property_failure_is_at_the_declaration () =
  equal
    (pair (list (option string)) (option string))
    ([ Some "test/prop_decl.ml:21" ], None)
    ( locs props [ "fails in tail position" ],
      inner_loc props [ "fails in tail position" ] )

let a_summary_rides_the_failure () =
  equal
    (list (option string))
    [ Some "n=10"; None ]
    [ summary_of props [ "summarized" ]; summary_of props [ "unsummarized" ] ]

let a_negative_count_fails_the_body () =
  equal (list string)
    [ "fail body"; "fail body" ]
    [
      Recorded.row props [ "negative count" ];
      Recorded.row props [ "negative max_discard" ];
    ]

let a_limit_while_shrinking_keeps_the_counterexample () =
  needs_timeouts ();
  equal (pair string string)
    ("fail body", "timed out after 0.1s")
    ( Recorded.row timed_props [ "cut while shrinking" ],
      shrink_end timed_props [ "cut while shrinking" ] )

let properties =
  group "Properties"
    [
      test "a law that holds passes over the engine's default count"
        a_holding_law_runs_the_default_count;
      test "a declared count is the number of cases" (fun () ->
          equal (option int) (Some 3) (cases_run props [ "declares a count" ]));
      test "the configuration's count applies when none is declared" (fun () ->
          equal (option int) (Some 5) (cases_run prop_counted [ "counted" ]));
      test "a declared count wins over the configuration's" (fun () ->
          equal (option int) (Some 2) (cases_run prop_counted [ "declared" ]));
      test "the engine is told whether its count came from the configuration"
        the_engine_knows_where_its_count_came_from;
      test "a failing law adds the engine's failure, drawn from the run's seed"
        a_failing_law_adds_the_engine_failure;
      test "the examples reach the engine" (fun () ->
          equal (pair bool int) (true, 0)
            (example_facts props [ "fails on an example" ]));
      test "the same seed draws the same counterexample"
        the_same_seed_draws_the_same_counterexample;
      test "a property failure is at the declaration, its inner one as it was"
        a_property_failure_is_at_the_declaration;
      test "a declared summary rides the failure, of the shrunk value"
        a_summary_rides_the_failure;
      test "a property that gave up or missed a label fails at its declaration"
        gave_up_and_missed_at_the_declaration;
      test "a property that gave up says how many cases it discarded" (fun () ->
          expect (message (failure props [ "gives up" ]))
          @@ __POS_OF__
               {| property gave up: 201 discards exhausted the generation budget (0 cases passed) |});
      test "a property that missed a label names it" (fun () ->
          expect (message (failure props [ "misses a label" ]))
          @@ __POS_OF__ {| never covered: "never" (over 100 passing cases) |});
      test "a negative count or max_discard fails the test from its body"
        a_negative_count_fails_the_body;
      test "a limit that expires while shrinking keeps the counterexample"
        a_limit_while_shrinking_keeps_the_counterexample;
      test "a limit that expires before a failure names the case it cut"
        a_limit_cut_before_a_failure_names_the_case;
      test "a declared limit covers the whole property" (fun () ->
          needs_timeouts ();
          equal (list string)
            [ "body timeout 0.05s in a case" ]
            (lines timed_props [ "declares a limit" ]));
      test "the limit bounds the wall time of a property" (fun () ->
          needs_timeouts ();
          less float_exact ~than:2.0 timed_wall);
      test "Run.prop adds no tag" run_prop_adds_no_tag;
    ]

(* Events *)

let event_line = function
  | Run.Run_started { suite; total; selected; properties } ->
      strf "run started: %s, %d of %d%s" suite selected total
        (if properties then ", properties" else "")
  | Run.Test_started { path } -> "started " ^ path_string path
  | Run.Test_finished row -> "finished " ^ path_string row.path
  | Run.Fixture_release _ -> "release"
  | Run.Interrupted _ -> "interrupted"

let event_log, release_order =
  let events = ref [] and order = ref [] in
  let fx name =
    Run.fixture ~teardown:(fun () -> order := name :: !order) ignore
  in
  let fx_a = fx "a" and fx_b = fx "b" in
  let r =
    Recorded.execute
      ~on_event:(fun e -> events := event_line e :: !events)
      [
        test "uses both" (fun () ->
            fx_a ();
            fx_b ());
        test "second" ignore;
      ]
  in
  ignore r;
  (List.rev !events, List.rev !order)

let property_flags =
  let suite =
    [ test "plain" ignore; test ~tags:[ Test_tree.Tag.prop ] "law" ignore ]
  in
  let started filter =
    let seen = ref "not started" in
    let on_event = function
      | Run.Run_started { properties; selected; _ } ->
          seen := strf "%d selected, properties: %b" selected properties
      | Run.Test_started _ | Run.Test_finished _ | Run.Fixture_release _
      | Run.Interrupted _ ->
          ()
    in
    ignore
      (Recorded.execute ~on_event ~config:(fun c -> { c with filter }) suite);
    !seen
  in
  List.map started [ []; [ "law" ]; [ "plain" ] ]

let observer_exit, observer_called =
  let called = ref false in
  let observer = function
    | Run.Test_finished _ -> exit 3
    | Run.Run_started _ | Run.Test_started _ | Run.Fixture_release _
    | Run.Interrupted _ ->
        called := true
  in
  let raised =
    Recorded.escaped (Recorded.execute ~on_event:observer [ test "t" ignore ])
  in
  (raised, !called)

let events_come_in_order () =
  equal (list string)
    [
      "run started: suite, 2 of 2";
      "started uses both";
      "finished uses both";
      "started second";
      "finished second";
      "release";
      "release";
    ]
    event_log

let run_started_says_whether_properties_run () =
  equal (list string)
    [
      "2 selected, properties: true";
      "1 selected, properties: true";
      "1 selected, properties: false";
    ]
    property_flags

let events =
  group "Events"
    [
      test "events come in the order of execution, releases after the last"
        events_come_in_order;
      test "Run_started says whether a selected test is a property"
        run_started_says_whether_properties_run;
      test "an exit in an observer is that observer's exception" (fun () ->
          is_true observer_called;
          equal (option exn) (Some (Failure.Control `Exit)) observer_exit);
    ]

(* Startup errors *)

let refusal = function
  | Run.Duplicate_paths paths -> "duplicate paths: " ^ String.concat ", " paths
  | Run.Focused_in_ci sites ->
      let site = function Some l -> Loc.to_string l | None -> "unknown" in
      "focused in CI: " ^ String.concat ", " (List.map site sites)
  | Run.Update_refused_in_ci -> "update refused in CI"
  | Run.No_recorded_failures -> "no recorded failures"

let refused r = refusal (require_error (Recorded.returned r))
let in_ci = [ ("CI", "true") ]
let focus_pos line = ("test/focus_decl.ml", line, 0, 0)

let focused_suite =
  [
    focus
      (group ~__POS__:(focus_pos 1) "g"
         [ focus (test ~__POS__:(focus_pos 2) "x" ignore) ]);
    test "y" ignore;
    focus (test ~__POS__:(focus_pos 3) "z" ignore);
  ]

let refusals =
  let update c = { c with Run.baseline = Baseline.Update } in
  let focused = focus (test ~__POS__:(focus_pos 4) "f" ignore) in
  [
    ( "two tests of one path, and names each path once, sorted",
      "duplicate paths: a, g › same",
      Recorded.execute
        [
          group "g" [ test "same" ignore ];
          group "g" [ test "same" ignore ];
          test "a" ignore;
          test "a" ignore;
          test "a" ignore;
        ] );
    ( "a focus under CI, whatever the selection",
      "focused in CI: test/focus_decl.ml:1, test/focus_decl.ml:2, \
       test/focus_decl.ml:3",
      Recorded.execute ~env:in_ci
        ~config:(fun c -> { c with filter = [ "y" ] })
        focused_suite );
    ( "-u under CI",
      "update refused in CI",
      Recorded.execute ~env:in_ci ~config:update [ test "ok" ignore ] );
    ( "duplicates are checked before a focus",
      "duplicate paths: f",
      Recorded.execute ~env:in_ci [ focused; test "f" ignore ] );
    ( "a focus is checked before -u",
      "focused in CI: test/focus_decl.ml:4",
      Recorded.execute ~env:in_ci ~config:update [ focused ] );
    ( "-u is checked before --failed",
      "update refused in CI",
      Recorded.execute ~env:in_ci
        ~config:(fun c -> { (update c) with failed_only = true })
        [ test "t" ignore ] );
  ]

let lifted =
  [
    ( "a focus under CI, with allow_focus",
      Recorded.execute ~env:in_ci
        ~config:(fun c -> { c with allow_focus = true })
        focused_suite );
    ( "--corrected under CI",
      Recorded.execute ~env:in_ci
        ~config:(fun c -> { c with baseline = Baseline.Corrected })
        [ test "ok" ignore ] );
  ]

let exit_codes_of_refusals =
  [
    ("duplicates", Run.Duplicate_paths [ "a" ], 1);
    ("a focus under CI", Run.Focused_in_ci [], 1);
    ("-u under CI", Run.Update_refused_in_ci, 1);
    ("an empty store", Run.No_recorded_failures, 2);
  ]

let the_message_of_duplicates () =
  expect (Run.startup_message (Run.Duplicate_paths [ "a"; "b \u{203a} c" ]))
  @@ __POS_OF__
       {|
       duplicate test paths:
         a
         b › c
       Every full test path must be unique.
       |}

let the_message_of_a_focus () =
  expect
    (Run.startup_message
       (Run.Focused_in_ci [ Some (Loc.of_pos (focus_pos 1)); None ]))
  @@ __POS_OF__
       {| focused tests committed (focus at test/focus_decl.ml:1, focus); remove focus to run under CI |}

let startup_errors =
  group "Startup errors"
    [
      cases "refuses"
        ~name:(fun (claim, _, _) -> claim)
        refusals
        (fun (_, expected, r) -> equal string expected (refused r));
      cases "runs" ~name:fst lifted (fun (_, r) ->
          equal int 0 (Recorded.exit_code r));
      cases "the exit code of a refusal of"
        ~name:(fun (n, _, _) -> n)
        exit_codes_of_refusals
        (fun (_, error, code) -> equal int code (Run.startup_exit_code error));
      test "the message of duplicates lists a path per line, then the rule"
        the_message_of_duplicates;
      test "the message of a focus under CI names each site and the remedy"
        the_message_of_a_focus;
      test "the message of -u under CI names the way to accept under CI"
        (fun () ->
          expect (Run.startup_message Run.Update_refused_in_ci)
          @@ __POS_OF__
               {| baseline update refused: CI is set. -u rewrites baselines in place, which is a developer's edit; under CI run with --corrected and accept with dune promote. |});
      test "the message of an empty store" (fun () ->
          expect (Run.startup_message Run.No_recorded_failures)
          @@ __POS_OF__ {| no recorded failures match the current suite |});
    ]

(* Selection *)

let math_and_text =
  [
    group "math" [ test "add" ignore; test "sub" ignore ];
    group "text" [ test "trim" ignore ];
  ]

let filtered =
  List.map
    (fun (claim, filter, exclude, expected) ->
      ( claim,
        expected,
        Recorded.execute
          ~config:(fun c -> { c with filter; exclude })
          math_and_text ))
    [
      ( "a filter keeps the tests whose path contains it",
        [ "math" ],
        [],
        [ "math › add"; "math › sub" ] );
      ( "an exclusion drops the tests whose path contains it",
        [],
        [ "math" ],
        [ "text › trim" ] );
      ( "a filter and an exclusion intersect",
        [ "math" ],
        [ "sub" ],
        [ "math › add" ] );
      ( "a test that contains one of the filters runs",
        [ "sub"; "trim" ],
        [],
        [ "math › sub"; "text › trim" ] );
      ( "a test that contains one of the exclusions is dropped",
        [],
        [ "add"; "trim" ],
        [ "math › sub" ] );
    ]

let tagged_suite =
  [
    test "plain" ignore;
    test ~tags:[ "db" ] "tagged" ignore;
    slow "molasses" ignore;
  ]

let tag_selected =
  List.map
    (fun (claim, tags, exclude_tags, expected) ->
      ( claim,
        expected,
        Recorded.execute
          ~config:(fun c -> { c with tags; exclude_tags })
          tagged_suite ))
    [
      ("no tag keeps every test", [], [], [ "plain"; "tagged"; "molasses" ]);
      ("a tag keeps the tests that carry it", [ "db" ], [], [ "tagged" ]);
      ("an excluded tag drops its tests", [], [ "db" ], [ "plain"; "molasses" ]);
      ( "excluding slow drops the slow tests",
        [],
        [ "slow" ],
        [ "plain"; "tagged" ] );
    ]

let allowed =
  Recorded.execute
    ~allowlist:[ "math › add"; "text › trim" ]
    ~config:(fun c -> { c with filter = [ "math" ] })
    math_and_text

let focused =
  Recorded.execute [ test "unfocused" ignore; focus (test "starred" ignore) ]

let shard_names = [ "t-one"; "t-two"; "t-three"; "t-four"; "t-five" ]
let shard_suite = List.map (fun name -> test name ignore) shard_names

let sharded ?(filter = []) ?(suite = shard_suite) (k, n) =
  Recorded.execute
    ~config:(fun c -> { c with shard = Some (k, n); filter })
    suite

let bucket_runs = List.map (fun k -> sharded (k, 3)) [ 1; 2; 3 ]
let bucket_again = sharded (1, 3)
let whole = sharded (1, 1)
let buckets () = List.map Recorded.executed bucket_runs

let bucket_line r =
  let ran =
    match Recorded.executed r with
    | [] -> "nothing"
    | paths -> String.concat ", " paths
  in
  strf "%s, exit %d" ran (Recorded.exit_code r)

let filtered_buckets =
  List.map (fun k -> sharded ~filter:[ "t-one" ] (k, 3)) [ 1; 2; 3 ]

let focused_buckets =
  let suite =
    [
      test "plain one" ignore;
      focus (test "starred" ignore);
      test "plain two" ignore;
    ]
  in
  List.map (fun k -> sharded ~suite (k, 3)) [ 1; 2; 3 ]

let malformed_shard =
  Recorded.escaped
    (Recorded.execute
       ~config:(fun c -> { c with shard = Some (2, 1) })
       [ test "t" ignore ])

let a_bucket_keeps_declaration_order () =
  let in_order bucket = List.filter (fun n -> List.mem n bucket) shard_names in
  let buckets = buckets () in
  equal (list (list string)) (List.map in_order buckets) buckets

let the_selection_and_the_suite_are_counted () =
  let _, _, r = List.hd filtered in
  let outcome = Recorded.outcome r in
  equal (pair int int) (2, 3) (List.length outcome.selected, outcome.total)

let a_focus_narrows_the_selection () =
  equal (list string) [ "starred" ] (Recorded.executed focused);
  is_true (Recorded.outcome focused).focus_active;
  equal int 0 (Recorded.exit_code focused)

let buckets_text () =
  String.concat "\n"
    (List.mapi
       (fun i b -> strf "%d/3: %s" (i + 1) (String.concat ", " b))
       (buckets ()))

let the_buckets_are_frozen () =
  expect (buckets_text ())
  @@ __POS_OF__
       {|
            1/3: t-two, t-four, t-five
            2/3:
            3/3: t-one, t-three
            |}

let a_shard_buckets_the_filtered_tests () =
  equal
    (slist string String.compare)
    [ "t-one, exit 0"; "nothing, exit 2"; "nothing, exit 2" ]
    (List.map bucket_line filtered_buckets)

let a_shard_buckets_the_focused_tests () =
  equal
    (slist string String.compare)
    [ "starred, exit 0"; "nothing, exit 2"; "nothing, exit 2" ]
    (List.map bucket_line focused_buckets)

let selection =
  group "Selection"
    [
      cases "filters"
        ~name:(fun (claim, _, _) -> claim)
        filtered
        (fun (_, expected, r) ->
          equal (list string) expected (Recorded.executed r));
      test "the outcome counts the selection and the declared tests"
        the_selection_and_the_suite_are_counted;
      cases "tags"
        ~name:(fun (claim, _, _) -> claim)
        tag_selected
        (fun (_, expected, r) ->
          equal (list string) expected (Recorded.executed r));
      test "an allowlist narrows the selection within the other layers"
        (fun () ->
          equal (list string) [ "math › add" ] (Recorded.executed allowed));
      test "a focus narrows the selection to the focused tests"
        a_focus_narrows_the_selection;
      test "1/1 keeps every test" (fun () ->
          equal (list string) shard_names (Recorded.executed whole));
      test "the buckets partition the selection" (fun () ->
          equal
            (slist string String.compare)
            shard_names
            (List.concat (buckets ())));
      test "a bucket keeps the declaration order"
        a_bucket_keeps_declaration_order;
      test "a bucket is the same in every run" (fun () ->
          equal (list string)
            (Recorded.executed (List.hd bucket_runs))
            (Recorded.executed bucket_again));
      test "the bucket of a path is frozen" the_buckets_are_frozen;
      test "a shard buckets the filtered tests"
        a_shard_buckets_the_filtered_tests;
      test "a shard buckets the focused tests" a_shard_buckets_the_focused_tests;
      test "a shard outside 1 <= K <= N raises Invalid_argument" (fun () ->
          raises_match Exn.invalid_arg (fun () -> replay malformed_shard));
    ]

(* Attempts *)

let boundary, released =
  let log = ref [] in
  let note name () = log := name :: !log in
  let cell name ~body ~teardown =
    bracket name
      ~setup:(fun () -> name)
      ~teardown:(fun name ->
        note name ();
        teardown ())
      (fun _ -> body ())
  in
  let failing phase () = fail phase in
  let r =
    Recorded.execute
      [
        cell "ok-ok" ~body:ignore ~teardown:ignore;
        cell "fail-ok" ~body:(failing "body") ~teardown:ignore;
        cell "ok-fail" ~body:ignore ~teardown:(failing "teardown");
        cell "fail-fail" ~body:(failing "body") ~teardown:(failing "teardown");
        cell "skip-ok"
          ~body:(fun () -> skip ~reason:"later" ())
          ~teardown:ignore;
        cell "skip-fail"
          ~body:(fun () -> skip ())
          ~teardown:(failing "teardown");
        bracket "setup-fail"
          ~setup:(fun () -> raise Boom)
          ~teardown:(note "setup-fail") (note "setup-fail body");
        bracket "setup-skip"
          ~setup:(fun () -> skip ~reason:"no env" ())
          ~teardown:(note "setup-skip") ignore;
        test "uncaught" (fun () -> raise Boom);
      ]
  in
  (r, List.rev !log)

let boundary_rows =
  [
    ("ok-ok", "pass");
    ("fail-ok", "fail body");
    ("ok-fail", "fail teardown");
    ("fail-fail", "fail body, teardown");
    ("skip-ok", "skip later");
    ("skip-fail", "fail teardown");
    ("setup-fail", "fail setup");
    ("setup-skip", "skip no env");
    ("uncaught", "fail body");
  ]

let limit_pos = ("test/limit_decl.ml", 7, 0, 0)

let limits, limit_log =
  let log = ref [] in
  let note step () = log := step :: !log in
  let r =
    Recorded.execute
      [
        test ~__POS__:limit_pos ~timeout:0.02 "body" busy_forever;
        bracket ~timeout:0.05 "teardown" ~setup:ignore ~teardown:busy_forever
          (fun () -> fail "body");
        bracket ~timeout:0.02 "body, then a teardown" ~setup:ignore
          ~teardown:(note "teardown after a body timeout")
          busy_forever;
        bracket ~timeout:0.02 "setup" ~setup:busy_forever
          ~teardown:(note "teardown after a setup timeout")
          (note "body after a setup timeout");
        cases ~timeout:0.05 "table" ~name:Fun.id [ "slow"; "fast" ]
          (fun input -> if input = "slow" then busy_forever ());
      ]
  in
  (r, List.rev !log)

let configured_limit =
  Recorded.execute
    ~config:(fun c -> { c with timeout = Some 0.02 })
    [ test "slowpoke" busy_forever ]

let both_phases_spin =
  Recorded.execute
    [
      bracket ~timeout:0.02 "spins" ~setup:ignore
        ~teardown:(fun () -> spin 30.)
        (fun () -> spin 30.);
    ]

let draws, draws_again =
  let draw () = (Random.bits (), Random.bits (), Random.bits ()) in
  let seen = ref [] in
  let tests =
    [
      test "a" (fun () -> seen := draw () :: !seen);
      test "b" (fun () -> seen := draw () :: !seen);
    ]
  in
  ignore (Recorded.execute tests);
  let first = List.rev !seen in
  seen := [];
  ignore (Recorded.execute tests);
  (first, List.rev !seen)

let random_restored =
  Random.init 999;
  let expected = Random.int 1_000_000 in
  Random.init 999;
  ignore (Recorded.execute [ test "draws" (fun () -> ignore (Random.bits ())) ]);
  (expected, Random.int 1_000_000)

let tails =
  let attempt = ref 0 in
  Recorded.execute
    [
      bracket "tailed" ~setup:ignore
        ~teardown:(fun () -> fail "teardown")
        (fun () ->
          print_string "tail-marker\n";
          fail "body");
      test ~retries:1 "retried" (fun () ->
          incr attempt;
          Printf.printf "attempt-%d\n" !attempt;
          fail "always");
    ]

let streamed =
  Recorded.execute
    ~config:(fun c -> { c with stream = true })
    [ test "fails quietly" (fun () -> fail "boom") ]

let silent =
  Recorded.execute
    [
      test "prints nothing" (fun () -> fail "quiet");
      test "reads what it printed" (fun () ->
          print_string "seen\n";
          ignore (output ());
          fail "after reading");
    ]

let uncapturable =
  let root = Scratch.dir "windtrap-uncapturable-" in
  let file = Filename.concat root "a file" in
  touch file;
  Recorded.execute
    ~config:(fun c -> { c with log_dir = file })
    [ test "first" ignore; test "second" ignore ]

let decl_pos = ("test/fake_decl.ml", 21, 2, 30)
let given_pos = ("test/fake_site.ml", 40, 4, 20)

let located =
  Recorded.execute
    [
      test ~__POS__:decl_pos "tail" (fun () -> equal int 1 2);
      test ~__POS__:decl_pos "given" (fun () ->
          equal ~__POS__:given_pos int 1 2);
      test ~__POS__:decl_pos "captured" (fun () ->
          equal int 1 2;
          ());
      test ~__POS__:decl_pos "raises" (fun () -> raise Boom);
      test ~__POS__:decl_pos ~timeout:0.02 "times out" busy_forever;
      scoped ~__POS__:decl_pos
        (fun k ->
          k ();
          k ())
        "calls back twice" ignore;
      test "overflows" (fun () -> raise Stack_overflow);
      test "runs after" ignore;
      test "assumes" (fun () -> assume false);
      bracket "skips twice" ~setup:ignore
        ~teardown:(fun () -> skip ~reason:"second" ())
        (fun () -> skip ~reason:"first" ());
    ]

let alarm_handler_restored =
  if Sys.win32 then None
  else
    let mine (_ : int) = () in
    let before = Sys.signal Sys.sigalrm (Sys.Signal_handle mine) in
    ignore (Recorded.execute [ test ~timeout:5. "limited" ignore ]);
    match Sys.signal Sys.sigalrm before with
    | Sys.Signal_handle handler -> Some (handler == mine)
    | Sys.Signal_default | Sys.Signal_ignore -> Some false

let tail (f : Failure.t) =
  Option.map (fun (t : Failure.tail) -> t.text) f.output_tail

let the_first_failure_carries_the_tail () =
  let failures = Recorded.failures tails [ "tailed" ] in
  equal
    (list (option string))
    [ Some "tail-marker\n"; None ]
    (List.map tail failures);
  is_some
    (Option.bind (List.hd failures).output_tail (fun (t : Failure.tail) ->
         t.log_path))

let each_phase_fails_on_its_own_limit () =
  needs_timeouts ();
  equal
    (list (pair string (list string)))
    [
      ("body", [ "body timeout 0.02s" ]);
      ("teardown", [ "body message"; "teardown timeout 0.05s" ]);
      ("body, then a teardown", [ "body timeout 0.02s" ]);
      ("setup", [ "setup timeout 0.02s" ]);
    ]
    (List.map
       (fun name -> (name, lines limits [ name ]))
       [ "body"; "teardown"; "body, then a teardown"; "setup" ])

let each_test_has_its_own_limit () =
  needs_timeouts ();
  equal (list string) [ "body timeout 0.05s" ]
    (lines limits [ "table"; "slow" ]);
  equal string "pass" (Recorded.row limits [ "table"; "fast" ])

let a_body_timeout_bounds_the_teardown_too () =
  needs_timeouts ();
  equal (list string)
    [ "body timeout 0.02s"; "teardown timeout 0.02s" ]
    (lines both_phases_spin [ "spins" ]);
  less float_exact ~than:5.0 (Recorded.outcome both_phases_spin).duration

let the_random_state_is_a_function_of_the_path () =
  let draw = triple int int int in
  equal (list draw) draws draws_again;
  match draws with [ a; b ] -> not_equal draw a b | _ -> fail "two tests drew"

let where_failures_are_located =
  [
    ( "from tail position, at the declaration",
      "tail",
      Some "test/fake_decl.ml:21" );
    ("with a given position, at it", "given", Some "test/fake_site.ml:40");
    ( "an uncaught exception, at the declaration",
      "raises",
      Some "test/fake_decl.ml:21" );
    ("a timeout, at the declaration", "times out", Some "test/fake_decl.ml:21");
    ( "a misused scope, at the declaration",
      "calls back twice",
      Some "test/fake_decl.ml:21" );
  ]

let a_failure_away_from_tail_position_is_at_its_line () =
  let at = require_some (loc (failure located [ "captured" ])) in
  starts_with ~affix:"test/unit/test_run.ml:" at

let teardowns_follow_returned_setups () =
  equal (list string)
    [ "ok-ok"; "fail-ok"; "ok-fail"; "fail-fail"; "skip-ok"; "skip-fail" ]
    released

let a_retried_tail_is_the_last_attempt () =
  equal
    (list (option string))
    [ Some "attempt-2\n" ]
    (List.map tail (Recorded.failures tails [ "retried" ]))

let no_tail_under_stream () =
  equal
    (list (option string))
    [ None ]
    (List.map tail (Recorded.failures streamed [ "fails quietly" ]))

let no_tail_without_unread_output () =
  equal
    (list (option string))
    [ None; None ]
    (List.concat_map
       (fun path -> List.map tail (Recorded.failures silent [ path ]))
       [ "prints nothing"; "reads what it printed" ]);
  let junit = Filename.concat (Scratch.dir "windtrap-silent-") "junit.xml" in
  Report_junit.write ~invocation:`Mirrors ~suite:"suite" ~duration:0.
    ~results:(rows_of silent) ~release_failures:[] junit;
  not_contains ~sub:"system-out" (require_some (contents junit))

let an_uncapturable_test_fails_alone () =
  starts_with ~affix:"body raise" (line (failure uncapturable [ "first" ]));
  equal (list string) [ "first"; "second" ] (Recorded.executed uncapturable)

let a_stack_overflow_fails_its_test () =
  equal (list string)
    [ "body raise Stack overflow" ]
    (lines located [ "overflows" ]);
  equal string "pass" (Recorded.row located [ "runs after" ])

let attempts =
  group "Attempts"
    [
      cases "a test ends with the phases of its failures" ~name:fst
        boundary_rows (fun (name, row) ->
          equal string row (Recorded.row boundary [ name ]));
      test "a teardown runs after every setup that returned, and only then"
        teardowns_follow_returned_setups;
      test "an uncaught exception is a raise failure that names it" (fun () ->
          equal (list string)
            [ "body raise Test_run.Boom" ]
            (lines boundary [ "uncaught" ]));
      test "a timeout is a failure of the phase it interrupted"
        each_phase_fails_on_its_own_limit;
      test "a body that timed out still has its teardown run" (fun () ->
          needs_timeouts ();
          equal (list string) [ "teardown after a body timeout" ] limit_log);
      test "a teardown after a body timeout gets a limit of its own"
        a_body_timeout_bounds_the_teardown_too;
      test "the limit of a test is its own" each_test_has_its_own_limit;
      test "the configuration's limit applies when none is declared" (fun () ->
          needs_timeouts ();
          equal (list string) [ "body timeout 0.02s" ]
            (lines configured_limit [ "slowpoke" ]));
      test "the Random state of a test is a function of its path"
        the_random_state_is_a_function_of_the_path;
      test "the Random state is restored after the run" (fun () ->
          equal int (fst random_restored) (snd random_restored));
      test "the first failure carries the tail of the output, and names the log"
        the_first_failure_carries_the_tail;
      test "a retried test's tail is its last attempt's"
        a_retried_tail_is_the_last_attempt;
      test "no failure carries a tail under stream" no_tail_under_stream;
      test
        "a failure with no unread output carries no tail, and its JUnit \
         testcase no system-out"
        no_tail_without_unread_output;
      test "a capture that cannot be set up fails the body, and the run goes on"
        an_uncapturable_test_fails_alone;
      cases "a failure without a location is located"
        ~name:(fun (claim, _, _) -> claim)
        where_failures_are_located
        (fun (_, path, expected) ->
          if path = "times out" then needs_timeouts ();
          equal (list (option string)) [ expected ] (locs located [ path ]));
      test "a failure away from tail position is located at its own line"
        a_failure_away_from_tail_position_is_at_its_line;
      test "a stack overflow fails its test, and the run goes on"
        a_stack_overflow_fails_its_test;
      test "an assume outside a property is the message the interface states"
        (fun () ->
          equal string "assume or reject was called outside a property"
            (message (failure located [ "assumes" ])));
      test "the first skip reason wins" (fun () ->
          equal string "skip first" (Recorded.row located [ "skips twice" ]));
      test "the previous handler of SIGALRM is put back" (fun () ->
          match alarm_handler_restored with
          | None -> skip ~reason:"no SIGALRM on Windows" ()
          | Some restored -> is_true restored);
    ]

(* Scoped tests *)

let scopes, scope_log =
  let log = ref [] in
  let mark step = log := step :: !log in
  let protecting name fn =
    mark (name ^ ": acquire");
    Fun.protect
      ~finally:(fun () -> mark (name ^ ": release"))
      (fun () -> fn name)
  in
  let r =
    Recorded.execute
      [
        scoped (protecting "pass") "pass" (fun resource ->
            mark "pass: body";
            equal string "pass" resource);
        scoped (protecting "body fails") "body fails" (fun _ -> fail "body");
        scoped (protecting "body skips") "body skips" (fun _ ->
            skip ~reason:"later" ());
        scoped
          (fun _ -> mark "never calls back: acquire")
          "never calls back"
          (fun () -> mark "never calls back: body");
        scoped
          (fun _ -> raise Boom)
          "acquire fails"
          (fun () -> mark "acquire fails: body");
        scoped
          (fun fn ->
            fn ();
            fail "release")
          "release fails"
          (fun () -> mark "release fails: body");
        scoped
          (fun fn -> try fn () with _ -> fail "release")
          "both fail"
          (fun () -> fail "body");
        scoped
          (fun fn -> try fn () with _ -> ())
          "swallowed"
          (fun () -> fail "body");
        scoped
          (fun _ -> skip ~reason:"no device" ())
          "scope skips"
          (fun () -> mark "scope skips: body");
        scoped
          (fun fn ->
            fn ();
            fn ())
          "calls back twice"
          (fun () -> mark "calls back twice: body");
        scoped
          (fun fn -> Fun.protect ~finally:busy_forever fn)
          ~timeout:0.02 "a finally cut by the limit" ignore;
      ]
  in
  (r, List.rev !log)

let scoped_limits, reclaimed =
  let reclaimed = ref false in
  let r =
    Recorded.execute
      [
        scoped
          (fun fn -> Fun.protect ~finally:(fun () -> reclaimed := true) fn)
          ~timeout:0.02 "body times out"
          (fun () -> spin 30.);
        scoped
          (fun fn ->
            fn ();
            spin 30.)
          ~timeout:0.02 "release times out" ignore;
      ]
  in
  (r, !reclaimed)

let scope_rows =
  [
    ("pass", "pass");
    ("body fails", "fail body");
    ("body skips", "skip later");
    ("never calls back", "fail setup");
    ("acquire fails", "fail setup");
    ("release fails", "fail teardown");
    ("both fail", "fail body, teardown");
    ("swallowed", "fail body");
    ("scope skips", "skip no device");
    ("calls back twice", "fail body");
    ("a finally cut by the limit", "fail teardown");
  ]

let steps_ending_with suffix =
  List.filter (fun step -> String.ends_with ~suffix step) scope_log

let the_limit_is_armed_again () =
  needs_timeouts ();
  equal (list string)
    [ "teardown timeout 0.02s" ]
    (lines scoped_limits [ "release times out" ]);
  less float_exact ~than:5.0 (Recorded.outcome scoped_limits).duration

let scoped_tests =
  group "Scoped tests"
    [
      cases "the phase of a failure is how far the callback got" ~name:fst
        scope_rows (fun (name, row) ->
          if name = "a finally cut by the limit" then needs_timeouts ();
          equal string row (Recorded.row scopes [ name ]));
      test "a scope acquires, runs the body, then releases" (fun () ->
          equal (list string)
            [ "pass: acquire"; "pass: body"; "pass: release" ]
            (List.filter (String.starts_with ~prefix:"pass:") scope_log));
      test "a scope releases after a body that failed or skipped" (fun () ->
          equal (list string)
            [ "pass: release"; "body fails: release"; "body skips: release" ]
            (steps_ending_with ": release"));
      test "a body runs once, and only when the scope calls back" (fun () ->
          equal (list string)
            [ "pass: body"; "release fails: body"; "calls back twice: body" ]
            (steps_ending_with ": body"));
      test "a scope that never calls back fails with a message" (fun () ->
          expect (message (failure scopes [ "never calls back" ]))
          @@ __POS_OF__
               {| the scope returned without running the test body; a scope must call its callback exactly once |});
      test "a second call of the callback fails with the number of calls"
        (fun () ->
          expect (message (failure scopes [ "calls back twice" ]))
          @@ __POS_OF__
               {| the scope called its callback 2 times and the test body ran on the first call only; a scope must call it exactly once |});
      test "a finally that the limit cut is a timeout of the teardown"
        (fun () ->
          needs_timeouts ();
          equal (list string)
            [ "teardown timeout 0.02s" ]
            (lines scopes [ "a finally cut by the limit" ]));
      test "a body that timed out is reclaimed by its scope" (fun () ->
          needs_timeouts ();
          equal (list string) [ "body timeout 0.02s" ]
            (lines scoped_limits [ "body times out" ]);
          is_true reclaimed);
      test "the limit is armed again when the body leaves the callback"
        the_limit_is_armed_again;
    ]

(* Retries *)

let retried, skip_calls =
  let flaky = ref 0 and hopeless = ref 0 and skips = ref 0 in
  let r =
    Recorded.execute
      [
        test ~retries:2 "flaky" (fun () ->
            incr flaky;
            if !flaky < 3 then fail "not yet");
        test ~retries:1 "hopeless" (fun () ->
            incr hopeless;
            fail (strf "attempt %d" !hopeless));
        test ~retries:2 "skips" (fun () ->
            incr skips;
            skip ());
        cases ~retries:2 ~name:Fun.id "flaky table" [ "row" ]
          (let calls = ref 0 in
           fun _ ->
             incr calls;
             if !calls < 3 then fail "not yet");
      ]
  in
  (r, !skips)

let retry_rows =
  [
    ("a test that passes on a retry", retried, [ "flaky" ], "pass", 3);
    ("a test that never passes", retried, [ "hopeless" ], "fail body", 2);
    ("a test that skips", retried, [ "skips" ], "skip", 1);
    ("a child of cases", retried, [ "flaky table"; "row" ], "pass", 3);
    ("an expected failure", xfails, [ "keeps failing" ], "xfail body", 1);
    ("an unexpected pass", xfails, [ "keeps passing" ], "fail body", 3);
  ]

let retries =
  group "Retries"
    [
      cases "the row and the attempts of"
        ~name:(fun (claim, _, _, _, _) -> claim)
        retry_rows
        (fun (_, r, path, row, used) ->
          equal (pair string int) (row, used)
            (Recorded.row r path, attempts_used r path));
      test "the row keeps the failures of the last attempt" (fun () ->
          equal string "attempt 2" (message (failure retried [ "hopeless" ])));
      test "a skip is never retried" (fun () -> equal int 1 skip_calls);
    ]

(* Corrections *)

let correcting ~root baseline tests =
  Recorded.execute
    ~env:[ ("WINDTRAP_PROJECT_ROOT", root) ]
    ~config:(fun c -> { c with baseline })
    tests

(* Separators compared as '/': on Windows the root is native and a baseline
   path is spelled with '/'. *)
let relative root path =
  let root = Windtrap_test_support.slashed root
  and path = Windtrap_test_support.slashed path in
  let prefix = root ^ "/" in
  if String.starts_with ~prefix path then
    String.sub path (String.length prefix)
      (String.length path - String.length prefix)
  else path

let writes root r =
  List.map
    (function
      | Baseline.Written { path; literals } ->
          strf "wrote %s, %d literals" (relative root path) literals
      | Baseline.Refused { path; reason } ->
          strf "refused %s: %s" (relative root path) reason)
    (Baseline.writes (Run.baselines (Recorded.outcome r).run))

let present = function None -> "absent" | Some text -> text

(* A baseline file on disk whose content no test produces, so that a
   correcting run records a correction for it: a missing file gets none. *)
let stale root file =
  let path = Filename.concat root file in
  Os.mkdir_p (Filename.dirname path);
  Out_channel.with_open_bin path (fun oc -> output_string oc "stale\n")

let help_runs =
  let root = Scratch.dir "windtrap-corrections-" in
  let file = Filename.concat root "src/help.expected" in
  let suite =
    [
      test "t1" (fun () -> expect_file "hello\n" "src/help.expected");
      test "t2" ignore;
    ]
  in
  let one baseline =
    if Sys.file_exists (file ^ ".corrected") then
      Sys.remove (file ^ ".corrected");
    let r = correcting ~root baseline suite in
    let on_disk = present (contents file)
    and correction = present (contents (file ^ ".corrected")) in
    fun () ->
      [
        strf "exit %d" (Recorded.exit_code r);
        "t1: " ^ Recorded.row r [ "t1" ];
        "file: " ^ on_disk;
        "correction: " ^ correction;
      ]
      @ writes root r
  in
  let check = one Baseline.Check in
  let missing = one Baseline.Corrected in
  let corrected =
    stale root "src/help.expected";
    one Baseline.Corrected
  in
  let update = one Baseline.Update in
  let again = one Baseline.Check in
  [ check; missing; corrected; update; again ]

let help_rows =
  [
    ( "under Check a mismatch fails the run and writes nothing",
      [ "exit 1"; "t1: fail body"; "file: absent"; "correction: absent" ] );
    ( "under Corrected a missing file fails the run and writes nothing",
      [ "exit 1"; "t1: fail body"; "file: absent"; "correction: absent" ] );
    ( "under Corrected the correction lands beside the file, the diff decides",
      [
        "exit 0";
        "t1: fail body";
        "file: stale\n";
        "correction: hello\n";
        "wrote src/help.expected.corrected, 0 literals";
      ] );
    ( "under Update the baseline is accepted in place",
      [
        "exit 0";
        "t1: pass";
        "file: hello\n";
        "correction: absent";
        "wrote src/help.expected, 0 literals";
      ] );
    ( "an accepted baseline matches from then on",
      [ "exit 0"; "t1: pass"; "file: hello\n"; "correction: absent" ] );
  ]

let gated =
  let root = Scratch.dir "windtrap-gated-" in
  let under name = Filename.concat root name in
  let summary r names =
    let corrections =
      List.map
        (fun name -> "correction: " ^ present (contents (under name)))
        names
    in
    fun () ->
      [
        strf "exit %d" (Recorded.exit_code r);
        "counted: " ^ String.concat ", " (counted r);
      ]
      @ writes root r @ corrections
  in
  stale root "src/a.expected";
  stale root "src/d.expected";
  let dirty =
    correcting ~root Baseline.Corrected
      [
        test "dirty" (fun () ->
            Run.subtest "expectation" (fun () ->
                expect_file "x\n" "src/a.expected");
            fail "boom");
      ]
  in
  let dirty = summary dirty [ "src/a.expected.corrected" ] in
  let escaping =
    correcting ~root Baseline.Corrected
      [ test "escapes" (fun () -> expect_file "x\n" "../outside.expected") ]
  in
  let escaping = summary escaping [] in
  let divergent =
    correcting ~root Baseline.Corrected
      [
        test "a" (fun () -> expect_file "one\n" "src/d.expected");
        test "b" (fun () -> expect_file "two\n" "src/d.expected");
      ]
  in
  let divergent = summary divergent [ "src/d.expected.corrected" ] in
  [
    ( "a correction beside another failure is dropped",
      [ "exit 1"; "counted: dirty"; "correction: absent" ],
      dirty );
    ( "an unresolvable path is no correction",
      [ "exit 1"; "counted: escapes" ],
      escaping );
    ( "a second content for one baseline fails, the first is written",
      [
        "exit 1";
        "counted: a, b";
        "wrote src/d.expected.corrected, 0 literals";
        "correction: one\n";
      ],
      divergent );
  ]

(* A literal whose source lies under the project root, so that its correction
   is written too. *)
let stale_runs =
  let root = Scratch.dir "windtrap-stale-" in
  let source = Filename.concat root "t.ml" in
  let corrected = source ^ ".corrected" in
  let text literal =
    "let () =\n  expect \"new\" (__POS_OF__ {| " ^ literal ^ " |})\n"
  in
  let which content =
    let is literal = Option.equal String.equal content (Some (text literal)) in
    if is "old" then "old" else if is "new" then "new" else present content
  in
  let one baseline ~retries ~also_fails =
    let bodies = ref 0 in
    if Sys.file_exists corrected then Sys.remove corrected;
    Out_channel.with_open_bin source (fun oc -> output_string oc (text "old"));
    let stale () =
      incr bodies;
      expect "new" (("t.ml", 2, 15, 37), " old ");
      if also_fails !bodies then fail "boom"
    in
    let r = correcting ~root baseline [ test ~retries "stale" stale ] in
    let bodies = !bodies
    and in_source = which (contents source)
    and correction = which (contents corrected) in
    fun () ->
      [
        strf "%d bodies, %d attempts" bodies (attempts_used r [ "stale" ]);
        "row: " ^ Recorded.row r [ "stale" ];
        "failures: " ^ String.concat ", " (lines r [ "stale" ]);
        strf "exit %d" (Recorded.exit_code r);
        "source: " ^ in_source;
        "correction: " ^ correction;
      ]
      @ writes root r
  in
  let never _ = false in
  [
    one Baseline.Corrected ~retries:0 ~also_fails:never;
    one Baseline.Corrected ~retries:2 ~also_fails:never;
    one Baseline.Update ~retries:2 ~also_fails:never;
    one Baseline.Check ~retries:2 ~also_fails:never;
    one Baseline.Corrected ~retries:2 ~also_fails:(fun n -> n < 3);
    one Baseline.Corrected ~retries:1 ~also_fails:(fun n -> n = 2);
  ]

let kept_once =
  [
    "1 bodies, 1 attempts";
    "row: fail body";
    "failures: body baseline mismatch";
    "exit 0";
    "source: old";
    "correction: new";
    "wrote t.ml.corrected, 1 literals";
  ]

let stale_rows =
  [
    ("a kept correction ends the attempts, without retries", kept_once);
    ("a kept correction ends the attempts, with retries", kept_once);
    ( "an accepted literal ends the attempts",
      [
        "1 bodies, 1 attempts";
        "row: pass";
        "failures: ";
        "exit 0";
        "source: new";
        "correction: absent";
        "wrote t.ml, 1 literals";
      ] );
    ( "a check keeps nothing, so every attempt runs",
      [
        "3 bodies, 3 attempts";
        "row: fail body";
        "failures: body baseline mismatch";
        "exit 1";
        "source: old";
        "correction: absent";
      ] );
    ( "a dropped correction does not end the attempts",
      [
        "3 bodies, 3 attempts";
        "row: fail body";
        "failures: body baseline mismatch";
        "exit 0";
        "source: old";
        "correction: new";
        "wrote t.ml.corrected, 1 literals";
      ] );
    ("the attempt after a kept correction never runs", kept_once);
  ]

let excused =
  let root = Scratch.dir "windtrap-excused-" in
  let on_disk name =
    let file = Filename.concat root ("src/" ^ name ^ ".expected") in
    Sys.file_exists file || Sys.file_exists (file ^ ".corrected")
  in
  let known =
    xfail ~reason:"issue #42"
      (test "known" (fun () -> expect_file "buggy\n" "src/known.expected"))
  in
  let undecided =
    test "undecided" (fun () ->
        expect_file "partial\n" "src/undecided.expected";
        skip ~reason:"not here" ())
  in
  let summary r =
    let written = on_disk "known" || on_disk "undecided" in
    fun () ->
      [
        strf "exit %d" (Recorded.exit_code r);
        "counted: " ^ String.concat ", " (counted r);
        "known: " ^ Recorded.row r [ "known" ] ^ ", "
        ^ String.concat ", " (lines r [ "known" ]);
        strf "on disk: %b" written;
      ]
      @ writes root r
  in
  let corrected = summary (correcting ~root Baseline.Corrected [ known ]) in
  let updated = correcting ~root Baseline.Update [ known; undecided ] in
  (corrected, summary updated, updated)

let release_beside_a_correction =
  let root = Scratch.dir "windtrap-beside-" in
  stale root "src/help.expected";
  let fx = Run.fixture ~teardown:(fun () -> fail "release") ignore in
  let r =
    correcting ~root Baseline.Corrected
      [
        test "corrects" (fun () -> expect_file "hello\n" "src/help.expected");
        test "acquires" fx;
      ]
  in
  fun () ->
    [
      strf "exit %d" (Recorded.exit_code r);
      strf "%d release failures"
        (List.length (Recorded.outcome r).release_failures);
    ]
    @ writes root r

let marks, written_at_release, written_after =
  let root = Scratch.dir "windtrap-marks-" in
  stale root "kept.expected";
  let kept = Filename.concat root "kept.expected.corrected" in
  let at_release = ref None in
  let fx =
    Run.fixture
      ~teardown:(fun () -> at_release := Some (Sys.file_exists kept))
      ignore
  in
  let r =
    correcting ~root Baseline.Corrected
      [
        test "skipped" (fun () ->
            Run.check_baseline (Baseline.File "s.expected") "v";
            skip ());
        test "failed outside" (fun () ->
            Run.check_baseline (Baseline.File "f.expected") "v";
            fail "other");
        test "kept" (fun () ->
            fx ();
            Run.check_baseline (Baseline.File "kept.expected") "v");
      ]
  in
  (r, !at_release, Sys.file_exists kept)

let unwritable =
  let root = Scratch.dir "windtrap-unwritable-" in
  touch (Filename.concat root "blocker");
  correcting ~root Baseline.Update
    [
      test "accepts" (fun () ->
          Run.check_baseline (Baseline.File "blocker/x.expected") "v");
    ]

let ci_read_once =
  let root = Scratch.dir "windtrap-ci-once-" in
  let r =
    correcting ~root Baseline.Update
      [
        test "sets CI" (fun () ->
            Unix.putenv "CI" "true";
            Run.check_baseline (Baseline.File "late.expected") "v");
      ]
  in
  (r, contents (Filename.concat root "late.expected"))

let corrections_are_written_after_the_release () =
  equal
    (pair (option bool) bool)
    (Some false, true)
    (written_at_release, written_after)

let an_xfail_test_checks_without_correcting () =
  let corrected, _, _ = excused in
  equal (list string)
    [
      "exit 0";
      "counted: ";
      "known: xfail body, body missing baseline";
      "on disk: false";
    ]
    (corrected ())

let an_xfail_test_accepts_nothing () =
  let _, updated, undecided = excused in
  equal (list string)
    [
      "exit 0";
      "counted: ";
      "known: xfail body, body missing baseline";
      "on disk: false";
    ]
    (updated ());
  equal string "skip not here" (Recorded.row undecided [ "undecided" ])

let a_failed_release_beside_a_correction () =
  equal (list string)
    [
      "exit 1";
      "1 release failures";
      "wrote src/help.expected.corrected, 0 literals";
    ]
    (release_beside_a_correction ())

let a_withheld_correction_says_why () =
  equal (list string)
    [
      "body missing baseline, withheld: skipped";
      "body missing baseline, withheld: failed outside";
      "body message";
    ]
    (lines marks [ "skipped" ] @ lines marks [ "failed outside" ])

let an_unwritable_correction_fails_the_run () =
  equal
    (pair (list string) int)
    ([], 1)
    (counted unwritable, Recorded.exit_code unwritable)

let corrections =
  group "Corrections"
    [
      cases "a stale file"
        ~name:(fun (claim, _, _) -> claim)
        (List.map2
           (fun (claim, expected) actual -> (claim, expected, actual))
           help_rows help_runs)
        (fun (_, expected, actual) -> equal (list string) expected (actual ()));
      cases "gating"
        ~name:(fun (claim, _, _) -> claim)
        gated
        (fun (_, expected, actual) -> equal (list string) expected (actual ()));
      cases "a stale literal"
        ~name:(fun (claim, _, _) -> claim)
        (List.map2
           (fun (claim, expected) actual -> (claim, expected, actual))
           stale_rows stale_runs)
        (fun (_, expected, actual) -> equal (list string) expected (actual ()));
      test "an xfail test checks without correcting"
        an_xfail_test_checks_without_correcting;
      test "an xfail test accepts nothing, and a skip drops its correction"
        an_xfail_test_accepts_nothing;
      test "a failed release fails the run beside a written correction"
        a_failed_release_beside_a_correction;
      test "a withheld correction says why" a_withheld_correction_says_why;
      test "the corrections are written after the release"
        corrections_are_written_after_the_release;
      test "a correction that cannot be written fails the run, not a test"
        an_unwritable_correction_fails_the_run;
      test "CI is read once, at startup" (fun () ->
          let r, accepted = ci_read_once in
          equal
            (pair int (option string))
            (0, Some "v\n")
            (Recorded.exit_code r, accepted));
    ]

(* Fixture release *)

let releases, release_announced, release_released =
  let released = ref [] and announced = ref [] in
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
  let on_event = on_release (fun name -> announced := name :: !announced) in
  let acquires () =
    List.iter
      (fun fx -> ignore (fx ()))
      [ fx_ok; fx_plain; fx_first; fx_second ]
  in
  let r = Recorded.execute ~on_event [ test "acquires" acquires ] in
  (r, List.rev !announced, !released)

let untimed_release =
  let finished = ref false in
  let fx =
    Run.fixture
      ~teardown:(fun () ->
        Unix.sleepf 0.1;
        finished := true)
      ignore
  in
  let r = Recorded.execute [ test ~timeout:0.02 "tight" fx ] in
  let finished = !finished in
  fun () ->
    [
      "tight: " ^ Recorded.row r [ "tight" ];
      strf "finished: %b" finished;
      strf "%d release failures"
        (List.length (Recorded.outcome r).release_failures);
      strf "exit %d" (Recorded.exit_code r);
    ]

let bailed, bail_released =
  let released = ref false in
  let fx = Run.fixture ~teardown:(fun () -> released := true) ignore in
  let r =
    Recorded.execute
      ~config:(fun c -> { c with bail = true })
      [
        test "first fails" (fun () ->
            fx ();
            fail "boom");
        test "never runs" ignore;
        test "never runs either" ignore;
      ]
  in
  (r, !released)

let release_failed =
  let fx = Run.fixture ~teardown:(fun () -> raise Boom) ignore in
  Recorded.execute [ test "acquires" fx ]

let exit_in_release =
  let fx = Run.fixture ~teardown:(fun () -> exit 0) ignore in
  Recorded.execute [ test "uses" fx ]

let release_messages r = List.map message (Recorded.outcome r).release_failures

let site_of name =
  let prefix = "fixture (" in
  if String.starts_with ~prefix name then
    Some
      (String.sub name (String.length prefix)
         (String.length name - String.length prefix - 1))
  else None

let every_raising_release_is_a_failure_in_order () =
  match release_announced with
  | [ second; first; _ok ] ->
      equal (list string)
        [
          second ^ ": release raised "
          ^ Failure.exn_to_string (Stdlib.Failure "second");
          first ^ ": release raised "
          ^ Failure.exn_to_string (Stdlib.Failure "first");
        ]
        (release_messages releases)
  | announced -> failf "three releases, got %d" (List.length announced)

let a_release_failure_is_at_the_fixture_site () =
  equal
    (list (option string))
    (List.map site_of (List.filteri (fun i _ -> i < 2) release_announced))
    (List.map loc (Recorded.outcome releases).release_failures)

let a_failed_release_fails_the_run () =
  equal (list string) [ "release message" ]
    (List.map line (Recorded.outcome release_failed).release_failures);
  equal (pair string int) ("pass", 1)
    ( Recorded.row release_failed [ "acquires" ],
      Recorded.exit_code release_failed )

let an_exit_in_a_release () =
  equal (list string) [ "release message" ]
    (List.map line (Recorded.outcome exit_in_release).release_failures);
  contains
    ~sub:
      "release raised Exit_attempt (code under test called exit; intercepted \
       by windtrap)"
    (List.hd (release_messages exit_in_release))

let release =
  group "Fixture release"
    [
      test "only a fixture with a teardown is released" (fun () ->
          equal int 3 (List.length release_announced));
      test "a release that raises does not stop the others" (fun () ->
          equal (list string) [ "ok" ] release_released);
      test "each raising release is a release failure, in release order"
        every_raising_release_is_a_failure_in_order;
      test "a release failure is located at the site of its fixture"
        a_release_failure_is_at_the_fixture_site;
      test "the fixtures are released latest first" (fun () ->
          equal (list string) [ "b"; "a" ] release_order);
      test "no limit covers a release" (fun () ->
          equal (list string)
            [ "tight: pass"; "finished: true"; "0 release failures"; "exit 0" ]
            (untimed_release ()));
      test "the fixtures are released under bail" (fun () ->
          is_true bail_released);
      test "a failed release fails the run, and no test"
        a_failed_release_fails_the_run;
      test "an exit in a release is the failure of that release"
        an_exit_in_a_release;
    ]

(* The last-failed store *)

let ran r =
  match Recorded.returned r with
  | Ok (outcome : Run.outcome) ->
      strf "exit %d: %s" outcome.exit_code
        (String.concat ", " (Recorded.executed r))
  | Error error -> "refused: " ^ refusal error

let with_store dir ?(filter = []) ?(failed_only = false) tests =
  Recorded.execute
    ~config:(fun c -> { c with log_dir = dir; filter; failed_only })
    tests

let round_trip =
  let dir = Scratch.dir "windtrap-store-" in
  let fixed = ref false in
  let suite =
    [
      test "steady" ignore;
      test "shaky" (fun () -> if not !fixed then fail "boom");
    ]
  in
  let first = with_store dir suite in
  let rerun = with_store dir ~failed_only:true suite in
  fixed := true;
  let fixed_run = with_store dir ~failed_only:true suite in
  let emptied = with_store dir ~failed_only:true suite in
  [ first; rerun; fixed_run; emptied ]

let survivors =
  let dir = Scratch.dir "windtrap-survivors-" in
  let t1_fixed = ref false in
  let suite =
    [
      test "t1" (fun () -> if not !t1_fixed then fail "boom");
      test "t2" (fun () -> fail "boom");
    ]
  in
  let first = with_store dir suite in
  t1_fixed := true;
  let filtered = with_store dir ~filter:[ "t1" ] suite in
  let rerun = with_store dir ~failed_only:true suite in
  let disjoint = with_store dir ~failed_only:true ~filter:[ "t1" ] suite in
  [ first; filtered; rerun; disjoint ]

let dead_entries =
  let dir = Scratch.dir "windtrap-dead-" in
  let old = [ test "old" (fun () -> fail "boom") ] in
  let first = with_store dir old in
  let replaced = with_store dir [ test "new" ignore ] in
  let rerun = with_store dir ~failed_only:true old in
  [ first; replaced; rerun ]

let expected_in_store =
  let dir = Scratch.dir "windtrap-xfail-store-" in
  let suite =
    [
      xfail (test "xf" (fun () -> fail "known"));
      test "real" (fun () -> fail "boom");
    ]
  in
  let first = with_store dir suite in
  [ first; with_store dir ~failed_only:true suite ]

let unexpected_in_store =
  let dir = Scratch.dir "windtrap-xpass-store-" in
  let suite = [ xfail (test "xp" ignore) ] in
  let first = with_store dir suite in
  [ first; with_store dir ~failed_only:true suite ]

let no_store =
  with_store
    (Scratch.dir "windtrap-no-store-")
    ~failed_only:true
    [ test "any" ignore ]

let sanitized_store =
  Recorded.execute ~suite:"lib/a.ml" [ test "t" (fun () -> fail "x") ]

let the_store_is_under_the_sanitized_suite () =
  let suite_dir =
    Filename.concat
      (Recorded.log_dir sanitized_store)
      (Os.sanitize_component "lib/a.ml")
  in
  is_some (contents (Filename.concat suite_dir ".last-failed"))

let unrecognised_store =
  let dir = Scratch.dir "windtrap-unrecognised-" in
  let store = Filename.concat dir "suite/.last-failed" in
  Os.mkdir_p (Filename.dirname store);
  Out_channel.with_open_bin store (fun oc -> output_string oc "t\n");
  let suite = [ test "t" (fun () -> fail "x") ] in
  let unrecognised = with_store dir ~failed_only:true suite in
  Sys.remove store;
  Unix.mkdir store 0o700;
  [ unrecognised; with_store dir suite ]

let unreadable_store =
  let dir = Scratch.dir "windtrap-unreadable-store-" in
  Os.mkdir_p (Filename.concat dir "suite/.last-failed");
  let suite = [ test "a" ignore; test "b" ignore ] in
  [
    with_store dir ~filter:[ "a" ] suite; with_store dir ~failed_only:true suite;
  ]

let a_store_that_cannot_be_read_reads_as_empty () =
  if Sys.win32 then skip ~reason:"a directory does not open as a file here" ();
  equal (list string)
    [ "exit 0: a"; "refused: no recorded failures" ]
    (List.map ran unreadable_store)

let failed_runs_the_recorded_failures () =
  equal (list string)
    [
      "exit 1: steady, shaky";
      "exit 1: shaky";
      "exit 0: shaky";
      "refused: no recorded failures";
    ]
    (List.map ran round_trip)

let store =
  group "The last-failed store"
    [
      test "--failed runs the recorded failures until they pass"
        failed_runs_the_recorded_failures;
      test "the entry of a test the run did not execute survives" (fun () ->
          equal (list string)
            [ "exit 1: t1, t2"; "exit 0: t1"; "exit 1: t2"; "exit 2: " ]
            (List.map ran survivors));
      test "a run of the whole suite drops the paths that no longer exist"
        (fun () ->
          equal (list string)
            [ "exit 1: old"; "exit 0: new"; "refused: no recorded failures" ]
            (List.map ran dead_entries));
      test "an expected failure is never recorded" (fun () ->
          equal (list string)
            [ "exit 1: xf, real"; "exit 1: real" ]
            (List.map ran expected_in_store));
      test "an unexpected pass is recorded" (fun () ->
          equal (list string)
            [ "exit 1: xp"; "exit 1: xp" ]
            (List.map ran unexpected_in_store));
      test "--failed without a store is refused" (fun () ->
          equal string "refused: no recorded failures" (ran no_store));
      test "the store is <log_dir>/<sanitized suite>/.last-failed"
        the_store_is_under_the_sanitized_suite;
      test
        "a store that is not recognised reads as empty, one not written is \
         ignored" (fun () ->
          equal (list string)
            [ "refused: no recorded failures"; "exit 1: t" ]
            (List.map ran unrecognised_store));
      test "a store that cannot be read reads as empty"
        a_store_that_cannot_be_read_reads_as_empty;
    ]

(* Exits and backtraces *)

let exited, sibling_ran, after_ran =
  let sibling = ref false and after = ref false in
  let r =
    Recorded.execute
      [
        test "before" ignore;
        test "exits" (fun () -> exit 0);
        test "exits 7" (fun () -> exit 7);
        test "after" (fun () ->
            after := true;
            fail "genuine");
        bracket "setup exits" ~setup:(fun () -> exit 0) ~teardown:ignore ignore;
        bracket "teardown exits" ~setup:ignore
          ~teardown:(fun () -> exit 0)
          ignore;
        test ~retries:2 "exits on every attempt" (fun () -> exit 0);
        prop "law exits" (Gen.int_range 0 1000) (fun n -> if n > 10 then exit 0);
        xfail ~reason:"known" (test "expected to exit" (fun () -> exit 0));
        test "subtest exits" (fun () ->
            Run.subtest "exits" (fun () -> exit 0);
            Run.subtest "sibling" (fun () -> sibling := true));
      ]
  in
  (r, !sibling, !after)

let forked =
  if Sys.win32 then None
  else
    let status = ref "not forked" in
    let forks () =
      match Unix.fork () with
      | 0 -> exit 3
      | pid -> (
          match Unix.waitpid [] pid with
          | _, Unix.WEXITED code -> status := strf "exited %d" code
          | _, Unix.WSIGNALED s -> status := strf "killed by %d" s
          | _, Unix.WSTOPPED s -> status := strf "stopped by %d" s)
    in
    let r = Recorded.execute [ test "forks" forks ] in
    Some (r, !status)

let[@inline never] raise_from_helper () = raise Boom

let recording_on, deep_raise =
  Printexc.record_backtrace false;
  let r =
    Recorded.execute
      [
        test "deep raise" raise_from_helper;
        prop "deep raise in a law" gen (fun _ -> raise_from_helper ());
      ]
  in
  (Printexc.backtrace_status (), r)

let backtrace_of r path =
  require_match
    (fun (f : Failure.t) ->
      match f.kind with
      | Failure.Raise { backtrace = Some bt; _ } -> Some bt.kept
      | Failure.Property
          {
            inner = Some { kind = Failure.Raise { backtrace = Some bt; _ }; _ };
            _;
          } ->
          Some bt.kept
      | _ -> None)
    (failure r path)

let exit_phases =
  [
    ("the body", "exits", "fail body");
    ("a setup", "setup exits", "fail setup");
    ("a teardown", "teardown exits", "fail teardown");
    ("a law", "law exits", "fail body");
    ("an xfail test", "expected to exit", "xfail body");
  ]

let an_exit_is_intercepted_and_the_run_goes_on () =
  equal (list string)
    [
      "before";
      "exits";
      "exits 7";
      "after";
      "setup exits";
      "teardown exits";
      "exits on every attempt";
      "law exits";
      "expected to exit";
      "subtest exits";
    ]
    (Recorded.executed exited);
  is_true after_ran

let every_exit_code_is_intercepted_alike () =
  equal (list string)
    (List.map message (Recorded.failures exited [ "exits" ]))
    (List.map message (Recorded.failures exited [ "exits 7" ]))

let the_intercepted_exits_count () =
  equal
    (pair (list string) int)
    ( [
        "exits";
        "exits 7";
        "after";
        "setup exits";
        "teardown exits";
        "exits on every attempt";
        "law exits";
        "subtest exits";
      ],
      1 )
    (counted exited, Recorded.exit_code exited)

let exits =
  group "Exits and backtraces"
    [
      test "an exit in a test is intercepted, and the run goes on"
        an_exit_is_intercepted_and_the_run_goes_on;
      cases "an exit is a failure of the phase that called it, in"
        ~name:(fun (claim, _, _) -> claim)
        exit_phases
        (fun (_, path, row) -> equal string row (Recorded.row exited [ path ]));
      test "the interception says what the test did" (fun () ->
          expect (message (failure exited [ "exits" ]))
          @@ __POS_OF__
               {| the test called exit and was intercepted; a test must return or raise, never exit the process |});
      test "every exit code is intercepted alike"
        every_exit_code_is_intercepted_alike;
      test "an exit is intercepted on every attempt" (fun () ->
          equal int 3 (attempts_used exited [ "exits on every attempt" ]));
      test "an exit in a law is the intercepted exit and not a case" (fun () ->
          equal (list string) [ "body message" ] (lines exited [ "law exits" ]));
      test "an exit in a subtest ends the test, unlabelled" (fun () ->
          equal (list string) [ "body message" ]
            (lines exited [ "subtest exits" ]);
          is_false sibling_ran);
      test "the intercepted exits count as failures" the_intercepted_exits_count;
      test "a forked child's exit ends the child" (fun () ->
          match forked with
          | None -> skip ~reason:"no fork on Windows" ()
          | Some (r, status) ->
              equal (pair string string) ("pass", "exited 3")
                (Recorded.row r [ "forks" ], status));
      test "execute turns the recording of backtraces on" (fun () ->
          is_true recording_on);
      test "an uncaught exception carries the backtrace of its raise" (fun () ->
          contains ~sub:"raise_from_helper"
            (backtrace_of deep_raise [ "deep raise" ]));
      test "a law's uncaught exception ends its backtrace on the law's frames"
        (fun () ->
          let backtrace = backtrace_of deep_raise [ "deep raise in a law" ] in
          contains ~sub:"raise_from_helper" backtrace;
          not_contains ~sub:"Fun.protect" backtrace);
    ]

(* Executing *)

let printed =
  Recorded.execute
    [
      test "passes" ignore;
      test "fails" (fun () -> fail "x");
      test "skips" (fun () -> skip ());
    ]

let decisions r =
  strf "exit %d" (Recorded.exit_code r)
  :: List.map
       (fun (row : Run.result) ->
         strf "%s: %s, %d attempts" (path_string row.path)
           (Recorded.row r row.path) row.attempts)
       (rows_of r)

let undecided_by_the_report =
  let tests =
    [
      test "passes" ignore;
      test "fails" (fun () -> fail "x");
      test "skips" (fun () -> skip ());
      xfail (test "expected" (fun () -> fail "y"));
      slow "slow" ignore;
    ]
  in
  let dressed (c : Run.config) =
    {
      c with
      color = Os.Always;
      slow_threshold = 0.;
      verbose = true;
      junit = Some (Filename.concat (Scratch.dir "windtrap-junit-") "junit.xml");
      github = true;
      invocation = `Exe "suite.exe";
    }
  in
  (Recorded.execute tests, Recorded.execute ~config:dressed tests)

let exit_code_runs =
  [
    ("0 when every test passed", 0, Recorded.execute [ test "ok" ignore ]);
    ( "1 when a test failed",
      1,
      Recorded.execute [ test "ok" ignore; test "bad" (fun () -> fail "boom") ]
    );
    ( "2 when the selection is empty",
      2,
      Recorded.execute
        ~config:(fun c -> { c with filter = [ "zzz-nothing" ] })
        [ test "ok" ignore ] );
    ("2 for an empty suite", 2, Recorded.execute []);
    ( "0 when every selected test skipped",
      0,
      Recorded.execute
        [
          test "skip a" (fun () -> skip ());
          test "skip b" (fun () -> skip ~reason:"no net" ());
        ] );
    ("0 when the only failures were expected", 0, expected_failures_only);
    ("1 for an unexpected pass", 1, xfails);
    ("1 when a release failed", 1, release_failed);
    ("1 when a correction could not be written", 1, unwritable);
    ("1 under bail after a failure", 1, bailed);
  ]

let aftermath ~raised ~released ~store ~correction =
  [
    (match raised with
    | None -> "returned"
    | Some e -> "raised " ^ Failure.exn_to_string e);
    (if released then "fixtures released" else "fixtures held");
    (if Sys.file_exists store then "store written" else "no store");
    (if Sys.file_exists correction then "correction written"
     else "no correction");
  ]

let ended_by_exception ~on_event ~tests =
  let root = Scratch.dir "windtrap-ended-" in
  let released = ref false in
  let fx = Run.fixture ~teardown:(fun () -> released := true) ignore in
  let r =
    Recorded.execute ~on_event
      ~env:[ ("WINDTRAP_PROJECT_ROOT", root) ]
      ~config:(fun c -> { c with baseline = Baseline.Corrected })
      (tests fx)
  in
  aftermath ~raised:(Recorded.escaped r) ~released:!released
    ~store:(Filename.concat (Recorded.log_dir r) "suite/.last-failed")
    ~correction:(Filename.concat root "c.expected.corrected")

let corrects fx =
  test "corrects" (fun () ->
      fx ();
      Run.check_baseline (Baseline.File "c.expected") "v")

let observer_ended =
  ended_by_exception
    ~on_event:(function
      | Run.Test_finished _ -> raise Boom
      | Run.Run_started _ | Run.Test_started _ | Run.Fixture_release _
      | Run.Interrupted _ ->
          ())
    ~tests:(fun fx -> [ corrects fx ])

let fatal_ended =
  ended_by_exception ~on_event:ignore ~tests:(fun fx ->
      [ corrects fx; test "runs out of memory" (fun () -> raise Out_of_memory) ])

let release_event_ended =
  let second = ref false in
  let fy = Run.fixture ~teardown:(fun () -> second := true) ignore in
  let left =
    ended_by_exception
      ~on_event:(function
        | Run.Fixture_release _ -> raise Boom
        | Run.Run_started _ | Run.Test_started _ | Run.Test_finished _
        | Run.Interrupted _ ->
            ())
      ~tests:(fun fx ->
        [
          test "acquires" (fun () ->
              fx ();
              fy ());
        ])
  in
  (left, !second)

let fatal_release, fatal_released, active_after_fatal =
  let released = ref [] in
  let fx_ok =
    Run.fixture ~teardown:(fun v -> released := v :: !released) (fun () -> "ok")
  in
  let fx_fatal =
    Run.fixture ~teardown:(fun _ -> raise Out_of_memory) (fun () -> "fatal")
  in
  let r =
    Recorded.execute
      [
        test "acquires" (fun () ->
            ignore (fx_ok ());
            ignore (fx_fatal ()));
      ]
  in
  (Recorded.escaped r, !released, Run.active ())

let broken_by_break, break_torn_down =
  let torn = ref false in
  let r =
    Recorded.execute
      [
        bracket "breaks" ~setup:ignore
          ~teardown:(fun () -> torn := true)
          (fun () -> raise Sys.Break);
      ]
  in
  (Recorded.escaped r, !torn)

let bail_past_xfail =
  Recorded.execute
    ~config:(fun c -> { c with bail = true })
    [
      xfail (test "excused" (fun () -> fail "known"));
      test "ok" ignore;
      test "boom" (fun () -> fail "real");
      test "after" ignore;
    ]

(* [list_selection] takes no observer, so a body that ran would note it. *)
let listed, listed_ran =
  let ran = ref [] in
  let note name () = ran := name :: !ran in
  let listed =
    Recorded.list_selection [ test "one" (note "one"); test "two" (note "two") ]
  in
  (listed, !ran)

let stored = "windtrap-last-failed 1\none\n"

(* The store is written before the call, so the log directory is not the
   recorded one, which does not exist until the call. *)
let listed_from_store =
  let dir = Scratch.dir "windtrap-listing-" in
  let store = Filename.concat dir "suite/.last-failed" in
  Os.mkdir_p (Filename.dirname store);
  Out_channel.with_open_bin store (fun oc -> output_string oc stored);
  let r =
    Recorded.list_selection
      ~config:(fun c -> { c with log_dir = dir; failed_only = true })
      [ test "one" ignore; test "two" ignore ]
  in
  (r, store)

let listed_duplicates =
  Recorded.list_selection [ test "d" ignore; test "d" ignore ]

let the_exceptions_that_leave_execute =
  [
    ("an observer's", observer_raised, Some Boom);
    ("a fatal one of a body", broken_by_break, Some Sys.Break);
    ("a fatal one of a release", fatal_release, Some Out_of_memory);
  ]

let an_observer_exception_releases_first () =
  equal (list string)
    [ "raised Test_run.Boom"; "fixtures released"; "no store"; "no correction" ]
    observer_ended

let a_fatal_exception_releases_first () =
  equal (list string)
    [ "raised Out of memory"; "fixtures released"; "no store"; "no correction" ]
    fatal_ended

let an_exception_on_a_release_event () =
  let left, second = release_event_ended in
  equal (list string)
    [ "raised Test_run.Boom"; "fixtures held"; "no store"; "no correction" ]
    left;
  is_false second

let stopped_run =
  Recorded.execute [ test "stops" (fun () -> Run.stop ()); test "later" ignore ]

let stopped r = Run.stopped (Recorded.outcome r).run

let executing =
  group "Executing"
    ([
       test "execute prints nothing" (fun () ->
           equal (pair string string) ("", "")
             (Recorded.out printed, Recorded.err printed));
       test "the fields only a report reads decide nothing" (fun () ->
           let plain, dressed = undecided_by_the_report in
           equal (list string) (decisions plain) (decisions dressed));
       cases "the exit code is"
         ~name:(fun (claim, _, _) -> claim)
         exit_code_runs
         (fun (_, code, r) -> equal int code (Recorded.exit_code r));
       test "an empty selection records no row" (fun () ->
           let _, _, r = List.nth exit_code_runs 2 in
           equal (list string) [] (Recorded.executed r));
       cases "an exception leaves execute:"
         ~name:(fun (claim, _, _) -> claim)
         the_exceptions_that_leave_execute
         (fun (_, raised, expected) -> equal (option exn) expected raised);
       test "an observer's exception releases the fixtures first"
         an_observer_exception_releases_first;
       test "a fatal exception releases the fixtures first"
         a_fatal_exception_releases_first;
       test "an exception on a release event leaves every fixture unreleased"
         an_exception_on_a_release_event;
       test "a fatal release leaves the others unreleased" (fun () ->
           equal (list string) [] fatal_released;
           is_false active_after_fatal);
       test "a fatal body skips the teardown of its bracket" (fun () ->
           is_false break_torn_down);
       test "under bail the run stops after the first counted failure"
         (fun () ->
           equal (list string) [ "first fails" ] (Recorded.executed bailed));
       test "stop ends the run after the running test, and says which"
         (fun () ->
           equal (list string) [ "stops" ] (Recorded.executed stopped_run);
           equal (option (list string)) (Some [ "stops" ]) (stopped stopped_run);
           equal (option (list string)) None (stopped bailed));
       test "an expected failure does not stop a run under bail" (fun () ->
           equal (list string)
             [ "excused"; "ok"; "boom" ]
             (Recorded.executed bail_past_xfail));
       test "list_selection is what execute would run, and runs nothing"
         (fun () ->
           equal (list string) [ "one"; "two" ]
             (require_ok (Recorded.returned listed));
           equal (list string) [] listed_ran);
       test "list_selection makes no log directory" (fun () ->
           is_false (Sys.file_exists (Recorded.log_dir listed)));
       test "list_selection reads the store and rewrites none" (fun () ->
           let r, store = listed_from_store in
           equal (list string) [ "one" ] (require_ok (Recorded.returned r));
           equal (option string) (Some stored) (contents store));
       test "list_selection refuses what execute refuses" (fun () ->
           equal string "duplicate paths: d"
             (refusal (require_error (Recorded.returned listed_duplicates))));
     ]
    @ [
        selection;
        attempts;
        scoped_tests;
        retries;
        corrections;
        release;
        store;
        exits;
      ])

let () =
  exit
    (run "run"
       [
         configuration;
         run_records;
         frames;
         refused_callers;
         ambient_slot;
         the_running_test;
         temporary_paths;
         process_state;
         fixtures;
         results;
         properties;
         events;
         startup_errors;
         executing;
       ])
