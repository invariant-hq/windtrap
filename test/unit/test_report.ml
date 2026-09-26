(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Baseline = Windtrap.Private.Baseline
module Failure = Windtrap.Private.Failure
module Loc = Windtrap.Private.Loc
module Os = Windtrap.Private.Os
module Property = Windtrap.Private.Property
module Report = Windtrap.Private.Report
module Run = Windtrap.Private.Run
module Sections = Windtrap.Private.Report_sections
module Test_tree = Windtrap.Private.Test_tree
module Text = Windtrap.Private.Text
module Fixtures = Render_fixtures

let strf = Printf.sprintf
let seed = "s1:7be1d2c904aa31f5"
let exe = `Exe "./t.exe"
let armed = "lib/calc.ml:9:12:add"
let lines s = String.split_on_char '\n' s

let failures_rule = "──────────────────────── failures ────────────────────────"

let closing_rule = "──────────────────────────────────────────────────────────"

let config ?(verbose = false) ?(slow_threshold = 1.0) ?(invocation = `Mirrors)
    ?armed () =
  {
    (Run.default_config ()) with
    Run.verbose;
    slow_threshold;
    invocation;
    mutation =
      (match armed with Some id -> Run.Armed id | None -> Run.No_mutation);
  }

let rendered ?(ansi = false) ?terminal ?(config = config ()) calls =
  let b = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer b in
  calls (Report.create ~out:ppf ~ansi ?terminal config);
  Format.pp_print_flush ppf ();
  Buffer.contents b

(* What each step committed, read without flushing the formatter, so that a
   step shows only the lines it flushed. *)
let timeline ?(ansi = false) ?terminal ?(config = config ()) steps =
  let b = Buffer.create 256 in
  let r =
    Report.create ~out:(Format.formatter_of_buffer b) ~ansi ?terminal config
  in
  let step (label, call) =
    let before = Buffer.length b in
    call r;
    let wrote = Buffer.sub b before (Buffer.length b - before) in
    let wrote =
      if wrote = "" || String.ends_with ~suffix:"\n" wrote then wrote
      else wrote ^ " [no newline]\n"
    in
    "--- " ^ label ^ "\n" ^ wrote
  in
  String.concat "" (List.map step steps)

let result ?attempts ?duration ?prop_stats ?slow_tagged ?xfail path outcome =
  Fixtures.result ?attempts ?duration ?prop_stats ?slow_tagged ?xfail path
    outcome

let pass ?duration ?attempts name =
  result ?duration ?attempts [ name ] Failure.Pass

let failed ?duration ?attempts name msg =
  result ?duration ?attempts [ name ] (Failure.Fail [ Failure.message msg ])

let run_through ?(tests = None) ?(suite = "s") ?(results = [])
    ?(release_failures = []) ?baselines ?(duration = 0.1) r =
  (match tests with
  | Some tests -> Report.header r ~suite ~tests ~seed:None ()
  | None -> ());
  List.iter (Report.result r) results;
  Report.finish r ~release_failures ~results ?baselines ~duration ()

let finished ?ansi ?config ?tests ?suite ?release_failures ?baselines ?duration
    results =
  rendered ?ansi ?config
    (run_through ~tests ?suite ~results ?release_failures ?baselines ?duration)

let last_line s =
  match List.rev (lines s) with
  | "" :: last :: _ -> last
  | _ -> "\u{ab}the output does not end on a newline\u{bb}"

let gallery name entries =
  Gallery.check ("test/unit/expected/test_report/" ^ name ^ ".expected") entries

(* The renderer *)

let refused_thresholds =
  [ ("a negative one", -1.0); ("nan", Float.nan); ("infinity", Float.infinity) ]

let refused_threshold (_, slow_threshold) =
  raises_match Exn.invalid_arg (fun () ->
      Report.create
        ~out:(Format.formatter_of_buffer (Buffer.create 8))
        ~ansi:false
        (config ~slow_threshold ()))

let terminal_block color =
  let r = Report.terminal { (config ~verbose:true ()) with Run.color } in
  Report.header r ~suite:"s" ~tests:1 ~seed:None ();
  Report.begin_test r ~path:[ "bad" ];
  Report.result r (failed "bad" "b");
  output ()

let terminal_styles () =
  contains ~sub:"\027[31mFAIL" (terminal_block Os.Always);
  let plain = terminal_block Os.Never in
  contains ~sub:"  FAIL  bad" plain;
  not_contains ~sub:"\027" plain

let renderer =
  group "The renderer"
    [
      cases
        "create refuses a slow threshold that is not finite and non-negative"
        ~name:fst refused_thresholds refused_threshold;
      test "terminal styles standard output as --color says, with no live line"
        terminal_styles;
    ]

(* The transcript *)

let golden = `Exe "dune exec test/main.exe --"

let transcript ?ansi ?(verbose = false) () =
  let tests = Fixtures.results in
  rendered ?ansi ~config:(config ~verbose ~invocation:golden ()) (fun r ->
      Report.header r ~suite:"mylib" ~tests:(List.length tests)
        ~seed:(Some Fixtures.root) ();
      List.iter
        (fun (res : Run.result) ->
          Report.begin_test r ~path:res.path;
          Report.result r res)
        tests;
      Report.finish r ~results:tests
        ~release_failures:[ Fixtures.release_failure ]
        ~duration:Fixtures.duration ())

let baseline name = "test/unit/expected/test_report/" ^ name ^ ".expected"

let plain_golden ~verbose name () =
  let t = transcript ~verbose () in
  not_contains ~sub:"\027" t;
  expect_file t (baseline name)

let coloured_golden ~verbose name () =
  let t = transcript ~ansi:true ~verbose () in
  contains ~sub:"\027[" t;
  expect_file (Gallery.marked t) (baseline name)

let header_rows =
  [
    ("under -v, one test", (true, 1, "s: 1 test\n"));
    ("under -v, no test", (true, 0, "s: 0 tests\n"));
    ("a compact run prints nothing at the start", (false, 1, ""));
  ]

let header_row (_, (verbose, tests, expected)) =
  equal string expected
    (rendered ~config:(config ~verbose ()) (fun r ->
         Report.header r ~suite:"s" ~tests ~seed:None ()))

(* The word after [prefix] on the first line that starts with it. *)
let word_after prefix lines =
  List.find_map
    (fun line ->
      if not (String.starts_with ~prefix line) then None
      else
        let rest =
          String.sub line (String.length prefix)
            (String.length line - String.length prefix)
        in
        Some
          (List.hd
             (String.split_on_char ')'
                (List.hd (String.split_on_char ' ' rest)))))
    lines

let seed_restated () =
  let t = lines (transcript ~verbose:true ()) in
  let header = word_after "mylib: 12 tests (seed " t in
  let replay = word_after "replay: dune exec test/main.exe -- --seed " t in
  equal (option string) (Some seed) header;
  equal (option string) header replay

let duration_rows =
  [
    ("0.02ms", (2e-05, "0.0ms"));
    ("0.46ms", (0.00046, "0.5ms"));
    ("9.94ms", (0.00994, "9.9ms"));
    ("9.95ms", (0.00995, "10ms"));
    ("9.96ms", (0.00996, "10ms"));
    ("60ms", (0.06, "60ms"));
    ("999.4ms", (0.9994, "999ms"));
    ("999.5ms", (0.9995, "1.0s"));
    ("999.6ms", (0.9996, "1.0s"));
    ("6.5s", (6.5, "6.5s"));
    ("119.6s", (119.6, "119.6s"));
    ("5400s", (5400.0, "5400.0s"));
    ("zero", (0., "0.0ms"));
  ]

let summary_in duration = last_line (finished ~duration [ pass "t" ])

let duration_row (_, (duration, form)) =
  equal string
    (strf "  PASS  t%s%s" (String.make 42 ' ') form)
    (List.hd
       (lines
          (rendered ~config:(config ~verbose:true ()) (fun r ->
               Report.result r (pass ~duration "t")))));
  equal string ("1 passed in " ^ form ^ ".") (summary_in duration)

let unit_after_rounding () =
  let around first last =
    List.init (last - first + 1) (fun i -> float_of_int (first + i) /. 1e6)
  in
  let wrong = [ "1 passed in 10.0ms."; "1 passed in 1000ms." ] in
  equal (list string) []
    (List.filter
       (fun s -> List.mem s wrong)
       (List.map summary_in (around 9_900 10_050 @ around 999_000 1_000_600)))

let off_live_rows =
  (* What [begin_test] wrote, after the header. *)
  let draw ?(verbose = false) ?(stream = false) ~ansi ~terminal () =
    let b = Buffer.create 64 in
    let ppf = Format.formatter_of_buffer b in
    let r =
      Report.create ~out:ppf ~ansi ~terminal
        { (config ~verbose ()) with Run.stream }
    in
    Report.header r ~suite:"s" ~tests:2 ~seed:None ();
    Format.pp_print_flush ppf ();
    Buffer.clear b;
    Report.begin_test r ~path:[ "math"; "addition" ];
    Format.pp_print_flush ppf ();
    Buffer.contents b
  in
  [
    ( "without colour, under -v",
      fun () -> draw ~verbose:true ~ansi:false ~terminal:true () );
    ("without colour", fun () -> draw ~ansi:false ~terminal:true ());
    ("off a terminal", fun () -> draw ~ansi:true ~terminal:false ());
    ("under --stream", fun () -> draw ~stream:true ~ansi:true ~terminal:true ());
  ]

let live_line_cut () =
  let long = String.make 200 'n' in
  let drawn =
    rendered ~ansi:true ~terminal:true (fun r ->
        Report.header r ~suite:"s" ~tests:1 ~seed:None ();
        Report.begin_test r ~path:[ long ])
  in
  let line =
    Gallery.unstyled (String.concat "" (String.split_on_char '\r' drawn))
  in
  at_most int ~than:80 (Text.length_utf8 line);
  not_contains ~sub:long line

let label_stats collected =
  { Property.cases = 100; discards = 0; collected; coverage = [] }

let silent_rows =
  [
    ("a pass", pass "t");
    ("a skip", result [ "t" ] (Failure.Skip None));
    ( "a passing property with labels",
      result ~prop_stats:(label_stats [ ("even", 46) ]) [ "t" ] Failure.Pass );
    ("an excused failure", Fixtures.excused_result);
  ]

let silent (_, r) =
  equal string ""
    (rendered (fun rd ->
         Report.header rd ~suite:"s" ~tests:1 ~seed:None ();
         Report.result rd r))

let observe = Report.observe ~seed:Fixtures.root ~selection:None

let started ?(total = 4) ?(properties = false) () =
  Run.Run_started { suite = "s"; total; selected = total; properties }

let release =
  Failure.with_phase Failure.Release
    (Failure.message
       ~loc:(Fixtures.loc "test/t.ml" 4)
       "db: release raised Exit")

let committed_results =
  [ pass "ok"; failed "first" "boom"; failed "second" "boom" ]

let commits ~verbose =
  timeline ~config:(config ~verbose ())
    [
      ("Run_started, four tests", fun r -> observe r (started ()));
      ( "Test_started ok",
        fun r -> observe r (Run.Test_started { path = [ "ok" ] }) );
      ( "Test_finished ok, a pass",
        fun r -> observe r (Run.Test_finished (pass "ok")) );
      ( "Test_started first",
        fun r -> observe r (Run.Test_started { path = [ "first" ] }) );
      ( "Test_finished first, a failure",
        fun r -> observe r (Run.Test_finished (failed "first" "boom")) );
      ( "Test_finished second, a failure",
        fun r -> observe r (Run.Test_finished (failed "second" "boom")) );
      ( "finish, a fixture release failed",
        fun r ->
          Report.finish r ~results:committed_results
            ~release_failures:[ release ] ~duration:0.0042 () );
    ]

let two_results = [ pass "a"; pass "b" ]

let timeline_entries () =
  [
    ( "a compact run commits a block when its test finishes",
      commits ~verbose:false );
    ( "under -v every result commits its row, a failure its block under it",
      commits ~verbose:true );
    ( "under -v the live line names the test and its position",
      timeline ~ansi:true ~terminal:true ~config:(config ~verbose:true ())
        [
          ( "header",
            fun r -> Report.header r ~suite:"mylib" ~tests:2 ~seed:None () );
          ( "begin_test",
            fun r -> Report.begin_test r ~path:[ "math"; "addition" ] );
          ( "result",
            fun r ->
              Report.result r
                (Fixtures.result [ "math"; "addition" ] Failure.Pass) );
        ] );
    ( "a compact run's live line draws from column zero and never brings the \
       header out",
      timeline ~ansi:true ~terminal:true
        [
          ( "header",
            fun r -> Report.header r ~suite:"mylib" ~tests:2 ~seed:None () );
          ( "begin_test",
            fun r -> Report.begin_test r ~path:[ "math"; "addition" ] );
          ( "a pass",
            fun r ->
              Report.result r
                (Fixtures.result [ "math"; "addition" ] Failure.Pass) );
          ( "begin_test",
            fun r ->
              Report.begin_test r ~path:[ "users"; "sessions after login" ] );
        ] );
    ( "the live line is erased before a block and redrawn under it",
      timeline ~ansi:true ~terminal:true
        [
          ( "header",
            fun r -> Report.header r ~suite:"mylib" ~tests:2 ~seed:None () );
          ("begin_test", fun r -> Report.begin_test r ~path:[ "bad" ]);
          ("a failure", fun r -> Report.result r (failed "bad" "b"));
          ( "begin_test",
            fun r -> Report.begin_test r ~path:[ "math"; "addition" ] );
          ( "a pass",
            fun r ->
              Report.result r
                (Fixtures.result [ "math"; "addition" ] Failure.Pass) );
        ] );
    ( "a live name's control bytes print escaped",
      timeline ~ansi:true ~terminal:true
        [
          ( "header",
            fun r -> Report.header r ~suite:"vnames" ~tests:2 ~seed:None () );
          ("begin_test", fun r -> Report.begin_test r ~path:[ "first\nhalf" ]);
        ] );
    ( "a compact run's notice is the live line, erased before the summary",
      timeline ~ansi:true ~terminal:true
        [
          ("header", fun r -> Report.header r ~suite:"s" ~tests:2 ~seed:None ());
          ("a pass", fun r -> Report.result r (pass "a"));
          ("begin_test", fun r -> Report.begin_test r ~path:[ "b" ]);
          ("note", fun r -> Report.note r "releasing db");
          ( "finish",
            fun r ->
              Report.finish r ~release_failures:[]
                ~results:[ pass "a" ]
                ~duration:0.01 () );
        ] );
    ( "under -v every event has its line",
      timeline ~config:(config ~verbose:true ())
        [
          ("Run_started", fun r -> observe r (started ~total:1 ()));
          ( "Test_started",
            fun r -> observe r (Run.Test_started { path = [ "t" ] }) );
          ("Test_finished", fun r -> observe r (Run.Test_finished (pass "t")));
          ( "Fixture_release",
            fun r -> observe r (Run.Fixture_release { name = "db" }) );
        ] );
  ]

let row_entries () =
  let verbose ?ansi ?invocation ?armed calls =
    rendered ?ansi ~config:(config ~verbose:true ?invocation ?armed ()) calls
  in
  let one ?ansi ?invocation ?armed r =
    verbose ?ansi ?invocation ?armed (fun rd -> Report.result rd r)
  in
  let excused_property =
    {
      Fixtures.excused_result with
      Run.outcome =
        Failure.Fail
          [
            Failure.property
              ~loc:(Fixtures.loc "test/test_carry.ml" 5)
              ~inner:(Failure.message "carry lost")
              ~rendered:"(1, 2)" ~case_index:3 ~shrink_steps:0
              ~root:Fixtures.root ~examples:false ();
          ];
    }
  in
  [
    ( "a failure's row is its block's title: its path bold, then the armed \
       qualifier",
      one ~ansi:true ~invocation:exe ~armed
        (result [ "first" ] (Failure.Fail [ Fixtures.prop_failure ])) );
    ( "a missing baseline qualifies the row after its duration",
      one (result [ "help" ] (Failure.Fail [ Fixtures.snap_missing ])) );
    ( "and shares the parenthesis with the armed qualifier",
      one ~armed (result [ "help" ] (Failure.Fail [ Fixtures.snap_missing ])) );
    ( "an expected failure: XFAIL, the duration, then the reason",
      one Fixtures.excused_result );
    ( "an expected failure without a reason",
      one
        {
          Fixtures.excused_result with
          Run.xfail = Some { Test_tree.reason = None };
        } );
    ( "an expected failure's block under its row, with no replay",
      one ~invocation:exe excused_property );
    ( "and styled, each line faint past its indent",
      one ~ansi:true excused_property );
    ( "an expected failure whose message is the unexpected pass's stays excused",
      one
        (result [ "collide" ]
           ~xfail:{ Test_tree.reason = None }
           (Failure.Fail
              [ Failure.message "expected to fail, but the test passed" ])) );
    ("an unexpected pass is a FAIL", one Fixtures.xpass_result);
    ( "an xfail annotation on a pass changes nothing",
      one (result [ "t" ] ~xfail:Fixtures.xfail_reason Failure.Pass) );
    ( "a passing property prints its label table",
      one
        (result
           ~prop_stats:(label_stats [ ("even", 46) ])
           ~duration:0.0012 [ "labels visible" ] Failure.Pass) );
    ( "and none without collected labels",
      one (result ~prop_stats:(label_stats []) [ "no labels" ] Failure.Pass) );
    ( "an expected failure's table is in its block",
      one
        {
          Fixtures.excused_result with
          Run.prop_stats = Some (label_stats [ ("even", 46) ]);
        } );
    ( "a flaky pass names its attempts",
      one
        (result ~attempts:2 [ "network"; "fetches the manifest" ] Failure.Pass)
    );
    ("a name's control bytes print escaped", one (failed "first\nhalf" "b"));
    ( "a skip's reason and an expected failure's stay in their rows, escaped",
      verbose (fun r ->
          Report.header r ~suite:"s" ~tests:2 ~seed:None ();
          Report.result r (result [ "skipped" ] (Failure.Skip (Some "no\ndb")));
          Report.result r
            (result [ "excused" ]
               ~xfail:{ Test_tree.reason = Some "issue\t42" }
               (Failure.Fail [ Fixtures.eq_failure ]))) );
    ( "a header's control bytes print escaped",
      verbose (fun r -> Report.header r ~suite:"a\x07b" ~tests:1 ~seed:None ())
    );
    ( "a note's control bytes print escaped",
      verbose (fun r -> Report.note r "releasing d\nb") );
  ]

let note_rows =
  [
    ( "a green compact run stays one line",
      fun () ->
        ( rendered (fun r ->
              Report.header r ~suite:"s" ~tests:2 ~seed:None ();
              List.iter (Report.result r) two_results;
              Report.note r "releasing db";
              Report.finish r ~release_failures:[] ~results:two_results
                ~duration:0.01 ()),
          "s: 2 passed in 10ms.\n" ) );
    ( "under -v the plain line",
      fun () ->
        ( rendered ~config:(config ~verbose:true ()) (fun r ->
              Report.note r "releasing db"),
          "releasing db\n" ) );
    ( "a compact streamed run shows none",
      fun () ->
        ( rendered ~ansi:true ~terminal:true
            ~config:{ (config ()) with Run.stream = true }
            (fun r -> Report.note r "releasing db"),
          "" ) );
  ]

let noteworthy_notice () =
  let results = [ pass "a"; failed "b" "boom" ] in
  let t =
    rendered (fun r ->
        Report.header r ~suite:"s" ~tests:2 ~seed:None ();
        Report.result r (List.nth results 0);
        Report.note r "releasing db";
        Report.result r (List.nth results 1);
        Report.finish r ~release_failures:[] ~results ~duration:0.01 ())
  in
  not_contains ~sub:"releasing" t;
  starts_with ~affix:("s: 2 tests\n" ^ failures_rule ^ "\n  FAIL  b\n") t

(* The observer *)

let seed_rows =
  let header ~properties () =
    rendered ~config:(config ~verbose:true ()) (fun r ->
        observe r (started ~total:2 ~properties ()))
  in
  let green ~properties () =
    rendered (fun r ->
        observe r (started ~total:2 ~properties ());
        List.iter (fun res -> observe r (Run.Test_finished res)) two_results;
        Report.finish r ~release_failures:[] ~results:two_results ~duration:0.06
          ())
  in
  [
    ( "a header over a property prints the seed",
      (header ~properties:true, "s: 2 tests (seed " ^ seed ^ ")\n") );
    ( "a header over no property prints none",
      (header ~properties:false, "s: 2 tests\n") );
    ( "a green property run ends on its seed",
      (green ~properties:true, "s: 2 passed in 60ms (seed " ^ seed ^ ").\n") );
    ( "a green run without a property carries none",
      (green ~properties:false, "s: 2 passed in 60ms.\n") );
  ]

let observe_interrupted () =
  let t =
    rendered (fun r ->
        observe r (started ~total:2 ());
        observe r
          (Run.Interrupted
             {
               running = Some [ "g"; "t" ];
               releasing = None;
               results = [];
               duration = 0.5;
             }))
  in
  equal string "s: 2 not run in 500ms.\n" t;
  equal string "windtrap: interrupted in g \u{203a} t\n" (output ())

let observe_raises_nothing () =
  let fail = failed "x" "m" in
  ignore
    (rendered ~ansi:true ~terminal:true (fun r ->
         observe r (Run.Test_finished fail);
         observe r (Run.Fixture_release { name = "" });
         observe r (Run.Test_started { path = [] });
         observe r
           (Run.Run_started
              { suite = ""; total = 0; selected = 5; properties = true });
         observe r (Run.Test_finished fail);
         observe r
           (Run.Interrupted
              {
                running = None;
                releasing = Some "\027";
                results = [ fail; fail ];
                duration = Float.nan;
              })))

(* --stream *)

let streamed ?(verbose = false) results =
  rendered
    ~config:{ (config ~verbose ()) with Run.stream = true }
    (fun r ->
      Report.header r ~suite:"s" ~tests:(List.length results) ~seed:None ();
      List.iter (Report.result r) results;
      Report.finish r ~release_failures:[] ~results ~duration:0.5 ())

let stream_rows =
  [
    ( "a green run is its summary line",
      ([ pass "ok" ], false, "s: 1 passed in 500ms.\n") );
    ( "a run's failures are the compact section",
      ( [ pass "ok"; failed "bad" "b" ],
        false,
        "s: 2 tests\n" ^ failures_rule ^ "\n  FAIL  bad\n    b\n" ^ closing_rule
        ^ "\n\n1 passed, 1 failed in 500ms.\n" ) );
    ( "under -v the rows are -v's",
      ( [ pass "ok" ],
        true,
        "s: 1 test\n\
        \  PASS  ok                                         0.2ms\n\
         1 passed in 500ms.\n" ) );
  ]

let stream_drains () =
  let r =
    Report.create ~out:Format.std_formatter ~ansi:false
      { (config ~verbose:true ()) with Run.stream = true }
  in
  let first_byte write =
    Printf.eprintf "E";
    write ();
    String.sub (output ()) 0 1
  in
  equal (list string) [ "E"; "E"; "E" ]
    [
      first_byte (fun () -> Report.note r "a notice");
      first_byte (fun () -> Report.result r (pass ~duration:0.1 "t"));
      first_byte (fun () ->
          Report.finish r ~release_failures:[]
            ~results:[ pass "t" ]
            ~duration:0.5 ());
    ]

(* The selection *)

let base = Run.default_config ()

let selection_rows =
  [
    ("nothing narrows a default run", (false, base, None));
    ( "every part, in order, the last joined with and",
      ( false,
        {
          base with
          Run.filter = [ "pars er" ];
          exclude = [ "it's" ];
          tags = [ "a"; "b c" ];
          exclude_tags = [ "d" ];
          failed_only = true;
          shard = Some (1, 3);
        },
        Some
          "filter \"pars er\", exclusion \"it's\", tag \"a\", \"b c\", \
           excluded tag \"d\", --failed and shard 1/3" ) );
    ( "a control byte is escaped, so the line stays one",
      (false, { base with Run.filter = [ "a\nb" ] }, Some "filter \"a\\nb\"") );
    ( "a double quote and a backslash are escaped",
      ( false,
        { base with Run.filter = [ {|a"b\c|} ] },
        Some {|filter "a\"b\\c"|} ) );
    ( "several patterns, joined with or",
      ( false,
        { base with Run.filter = [ "a"; "b" ]; exclude = [ "c"; "d" ] },
        Some {|filter "a" or "b" and exclusion "c" or "d"|} ) );
    ( "a focus is named first",
      (true, { base with Run.filter = [ "a" ] }, Some {|focus and filter "a"|})
    );
    ("a focus alone is a selection", (true, base, Some "focus"));
  ]

let reason_rows =
  [
    ( "a suite that declares none",
      (0, Some {|filter "a"|}, Some "the suite declares none") );
    ( "a selection, every pattern named",
      ( 5,
        Some {|filter "a" or "b"|},
        Some {|filter "a" or "b" matched none of 5 tests|} ) );
    ( "one declared test",
      (1, Some {|tag "slow"|}, Some {|tag "slow" matched none of 1 test|}) );
    ("nothing narrowed", (5, None, None));
  ]

(* The end of the run *)

let empty_rows =
  let empty ?(invocation = `Mirrors) ?header () () =
    rendered ~config:(config ~invocation ()) (fun r ->
        Option.iter
          (fun (declared, selection) ->
            Report.header r ~suite:"mylib" ~tests:0 ~declared ?selection
              ~seed:None ())
          header;
        Report.finish r ~release_failures:[] ~results:[] ~duration:0.01 ())
  in
  let parsr = (48, Some {|filter "parsr"|}) in
  [
    ("without a header, the fact alone", (empty (), "no tests ran.\n"));
    ( "a suite that declares none names itself as the cause",
      ( empty ~header:(0, None) (),
        "mylib: no tests ran: the suite declares none.\n" ) );
    ( "a selection names itself, the total and how to list",
      ( empty ~invocation:exe ~header:parsr (),
        "mylib: no tests ran: filter \"parsr\" matched none of 48 tests.\n\
         list: ./t.exe -l\n" ) );
    ( "a build action names the flag",
      ( empty ~header:parsr (),
        "mylib: no tests ran: filter \"parsr\" matched none of 48 tests.\n\
         (list the suite's tests with -l)\n" ) );
    ( "a suite that declares none has nothing to list",
      ( empty ~invocation:exe ~header:(0, None) (),
        "mylib: no tests ran: the suite declares none.\n" ) );
  ]

(* A file on disk that no test produces: under Corrected a missing file
   records no correction. *)
let stale_file root path =
  Out_channel.with_open_bin (Filename.concat root path) (fun oc ->
      Out_channel.output_string oc "stale\n")

let baselines_written ?(mode = Baseline.Corrected) ?(refuse = false) files =
  let root = temp_dir () in
  if mode = Baseline.Corrected then
    List.iter
      (function
        | Baseline.File path, _ -> stale_file root path
        | (Baseline.Literal _ | Trailing _), _ -> ())
      files;
  let b = Baseline.create ~root ~cwd:root ~mode () in
  List.iter
    (fun (baseline, text) ->
      try Baseline.check b baseline text with Failure.Check_failure _ -> ())
    files;
  ignore (Baseline.settle b ~keep:true);
  if refuse then Os.mkdir_p (Filename.concat root "help.expected");
  Baseline.write b;
  (root, b)

let help = (Baseline.File "help.expected", "hello\n")

let summary_rows =
  let bad = failed "bad" "b" in
  let excused =
    [
      Fixtures.excused_result;
      { Fixtures.excused_result with Run.path = [ "also excused" ] };
    ]
  in
  [
    ( "a stopped run counts what it never reached",
      fun () ->
        ( finished ~tests:5 ~duration:0.0004 [ bad ],
          "1 failed, 4 not run in 0.4ms." ) );
    ( "a run that reached every test omits the term",
      fun () ->
        (finished ~tests:1 ~duration:0.0004 [ bad ], "1 failed in 0.4ms.") );
    ( "every term, in order",
      fun () ->
        let _, baselines = baselines_written [ help ] in
        ( finished ~tests:7 ~duration:6.5 ~baselines
            [
              pass "ok";
              pass ~attempts:2 "flaky";
              result [ "skipped" ] (Failure.Skip None);
              Fixtures.excused_result;
              result [ "sub" ]
                (Failure.Fail [ Fixtures.subtest_failure "shape [0]" ]);
            ],
          "2 passed (1 flaky), 1 skipped, 1 expected failure, 1 failed (1 \
           subtest failure), 2 not run, 1 correction written in 6.5s." ) );
    ( "zero terms are omitted, passed included, and a term is plural",
      fun () ->
        ( finished ~tests:2 ~duration:0.001 excused,
          "s: 2 expected failures in 1.0ms." ) );
    ( "a green compact run names its suite",
      fun () ->
        ( finished ~suite:"mylib" ~tests:1 ~duration:1.2 [ pass "a" ],
          "mylib: 1 passed in 1.2s." ) );
    ( "a green compact run carries the seed its header held",
      fun () ->
        ( rendered (fun r ->
              Report.header r ~suite:"mylib" ~tests:1 ~seed:(Some Fixtures.root)
                ();
              Report.result r (pass "a");
              Report.finish r ~release_failures:[]
                ~results:[ pass "a" ]
                ~duration:1.2 ()),
          "mylib: 1 passed in 1.2s (seed " ^ seed ^ ")." ) );
    ( "a suite's name keeps its tab",
      fun () ->
        ( finished ~suite:"my\tsuite" ~tests:1 [ pass "t" ],
          "my\tsuite: 1 passed in 100ms." ) );
    ( "a green run keeps its skips and expected failures on the line",
      fun () ->
        ( finished ~suite:"mylib" ~tests:3 ~duration:0.2
            [
              pass "a";
              result [ "s" ] (Failure.Skip None);
              Fixtures.excused_result;
            ],
          "mylib: 1 passed, 1 skipped, 1 expected failure in 200ms." ) );
    ( "an expected failure beside a failure",
      fun () ->
        ( finished
            ~config:(config ~invocation:(`Exe "exe") ())
            ~duration:0.2
            [ pass "ok"; Fixtures.excused_result; failed "bad" "boom" ],
          "1 passed, 1 expected failure, 1 failed in 200ms." ) );
    ( "subtest failures are counted",
      fun () ->
        ( finished [ Fixtures.subtest_result ],
          "1 failed (2 subtest failures) in 100ms." ) );
    ( "one subtest failure is singular",
      fun () ->
        ( finished
            [
              result [ "backend"; "contract" ]
                (Failure.Fail [ Fixtures.subtest_failure "shape [0]" ]);
            ],
          "1 failed (1 subtest failure) in 100ms." ) );
    ( "a message spelling a subtest's label is no subtest failure",
      fun () ->
        ( finished
            [
              result [ "backend"; "contract" ]
                (Failure.Fail
                   [
                     {
                       (Failure.message "boom") with
                       Failure.msg =
                         Some (Failure.text "contract \u{203a} shape [0]");
                     };
                   ]);
            ],
          "1 failed in 100ms." ) );
    ( "an excused failure whose message is the unexpected pass's is counted as \
       expected",
      fun () ->
        ( finished ~tests:1
            [
              result [ "collide" ]
                ~xfail:{ Test_tree.reason = None }
                (Failure.Fail
                   [ Failure.message "expected to fail, but the test passed" ]);
            ],
          "s: 1 expected failure in 100ms." ) );
    ( "a failed fixture release counts as failed",
      fun () ->
        ( finished ~release_failures:[ release ] ~duration:0.0042
            committed_results,
          "1 passed, 3 failed in 4.2ms." ) );
  ]

let slow_rows t =
  let rec after = function
    | [] -> []
    | l :: rest when String.starts_with ~prefix:"slow tests (" l ->
        l :: rows rest
    | _ :: rest -> after rest
  and rows = function
    | l :: rest when String.starts_with ~prefix:"  " l -> l :: rows rest
    | _ -> []
  in
  after (lines t)

let slow_section_rows =
  let slow_fail = failed ~duration:2.0 "boom" "b" in
  [
    ( "an untagged pass over the threshold",
      ( 1.0,
        [ pass ~duration:1.2 "t" ],
        [ "slow tests (1, over 1s):"; "  1.2s  t" ] ) );
    ( "the threshold is inclusive",
      ( 1.0,
        [ pass ~duration:1.0 "t" ],
        [ "slow tests (1, over 1s):"; "  1.0s  t" ] ) );
    ( "a slow-tagged test is exempt",
      (1.0, [ result ~slow_tagged:true ~duration:1.2 [ "t" ] Failure.Pass ], [])
    );
    ( "a skip is exempt",
      (1.0, [ result ~duration:2.0 [ "t" ] (Failure.Skip None) ], []) );
    ( "an excused failure under the threshold is not listed",
      (1.0, [ Fixtures.excused_result ], []) );
    ( "an excused failure over it is",
      ( 1.0,
        [ { Fixtures.excused_result with Run.duration = 2.0 } ],
        [ "slow tests (1, over 1s):"; "  2.0s  known \u{203a} broken carry" ] )
    );
    ( "a retried test on its attempts summed",
      ( 1.0,
        [ pass ~duration:1.2 ~attempts:3 "flaky" ],
        [ "slow tests (1, over 1s):"; "  1.2s  flaky" ] ) );
    ( "a failure is listed too",
      (1.0, [ slow_fail ], [ "slow tests (1, over 1s):"; "  2.0s  boom" ]) );
    ( "a threshold of 0 disables the section",
      (0.0, [ pass ~duration:5.0 "t" ], []) );
    ( "slowest first",
      ( 1.0,
        [
          pass ~duration:2.5 "big sort";
          pass ~duration:3.0 "hash";
          pass ~duration:1.5 "slow one";
        ],
        [
          "slow tests (3, over 1s):";
          "  3.0s  hash";
          "  2.5s  big sort";
          "  1.5s  slow one";
        ] ) );
    ( "a threshold prints as given: 0.01",
      ( 0.01,
        [ pass ~duration:1.2 "t" ],
        [ "slow tests (1, over 0.01s):"; "  1.2s  t" ] ) );
    ( "a threshold prints as given: 0.5",
      ( 0.5,
        [ pass ~duration:1.2 "t" ],
        [ "slow tests (1, over 0.5s):"; "  1.2s  t" ] ) );
    ( "a threshold prints as given, never as an exponent",
      ( 1e-9,
        [ pass ~duration:1.2 "t" ],
        [ "slow tests (1, over 0.000000001s):"; "  1.2s  t" ] ) );
    ( "a name's control bytes print escaped",
      ( 1.0,
        [ pass ~duration:1.5 "sl\now" ],
        [ "slow tests (1, over 1s):"; "  1.5s  sl\\x0aow" ] ) );
  ]

let slow_section (_, (slow_threshold, results, expected)) =
  equal (list string) expected
    (slow_rows
       (finished
          ~config:(config ~slow_threshold ())
          ~tests:(List.length results) results))

let flaky =
  result ~attempts:2 [ "network"; "fetches the manifest" ] Failure.Pass

let numbered first last =
  String.concat "\n"
    (List.init (last - first + 1) (fun i -> strf "l%d" (first + i)))

let tail_block ?ansi tail =
  finished ?ansi ~duration:0.01
    [
      result [ "t" ]
        (Failure.Fail [ Failure.with_output_tail tail (Failure.message "boom") ]);
    ]

let ok = result [ "g"; "ok" ] Failure.Pass

(* A run a signal stops, then what it said on standard error. *)
let stopped ?(verbose = false) calls =
  let t =
    rendered ~config:(config ~verbose ()) (fun r ->
        Report.header r ~suite:"s" ~tests:4 ~seed:None ();
        calls r)
  in
  t ^ "(standard error) " ^ output ()

let endings_entries () =
  let tail_and_commands =
    result [ "cli"; "both" ]
      (Failure.Fail
         [
           Fixtures.snap_mismatch;
           Failure.with_output_tail
             (Failure.tail "log line\n")
             Fixtures.prop_failure;
         ])
  in
  let withheld =
    result [ "cli"; "both" ]
      (Failure.Fail
         [
           Failure.with_output_tail
             (Failure.tail "log line\n")
             (Failure.message "boom");
           Failure.with_withheld Failure.Failed_outside Fixtures.snap_mismatch;
         ])
  in
  [
    ( "an untagged pass over the threshold makes a compact run print its \
       header and the section",
      finished ~tests:1 ~duration:1.2 [ pass ~duration:1.2 "t" ] );
    ( "a slow failure has its block and its row",
      finished ~tests:1 ~duration:2.0 [ failed ~duration:2.0 "boom" "b" ] );
    ( "under -v the section sits between the rows and the summary",
      finished ~config:(config ~verbose:true ()) ~tests:1 ~duration:1.5
        [ pass ~duration:1.5 "t" ] );
    ( "a flaky pass: the header, its section, the summary's term",
      finished ~tests:2 ~duration:0.3 [ pass "steady"; flaky ] );
    ( "the flaky section follows the failures and the slow section",
      finished ~duration:2.0
        [ failed "bad" "b"; pass ~duration:1.5 "slow one"; flaky ] );
    ( "a failure on every attempt is no flake, and its title counts its attempts",
      finished ~duration:0.1
        [ failed ~attempts:3 "hopeless" "still"; pass "steady" ] );
    ( "under -v the section follows the rows, one blank line after them",
      finished ~config:(config ~verbose:true ()) ~tests:1 ~duration:0.3
        [ flaky ] );
    ( "under colour the flaky section and term are yellow",
      finished ~ansi:true ~duration:0.3 [ flaky ] );
    ( "under colour the summary's expected failures are faint",
      finished ~ansi:true ~duration:0.1 [ Fixtures.excused_result ] );
    ( "a release block ends on its facts",
      finished
        ~config:(config ~invocation:exe ())
        ~release_failures:[ release ] ~duration:0.001 [] );
    ( "an expected failure leaves the failures section its one block",
      finished
        ~config:(config ~invocation:(`Exe "exe") ())
        ~duration:0.2
        [ pass "ok"; Fixtures.excused_result; failed "bad" "boom" ] );
    ( "all failures excused: no section, no rule",
      finished
        ~config:(config ~invocation:(`Exe "exe") ())
        ~duration:0.2
        [ pass "ok"; Fixtures.excused_result ] );
    ( "an unexpected pass is a failure's block",
      finished [ Fixtures.xpass_result ] );
    ( "a label table: every label, the covered ones too",
      finished ~duration:0.01
        [
          result
            ~prop_stats:
              {
                Property.cases = 100;
                discards = 3;
                collected = [ ("empty", 36); ("nonempty", 64) ];
                coverage =
                  [
                    {
                      Property.label = "collision";
                      hits = 0;
                      satisfied = false;
                    };
                    { Property.label = "singleton"; hits = 9; satisfied = true };
                  ];
              }
            [ "p" ]
            (Failure.Fail [ Failure.message "coverage unsatisfied" ]);
        ] );
    ( "a lone label is not restated",
      finished ~duration:0.01
        [
          result
            ~prop_stats:
              {
                Property.cases = 100;
                discards = 3;
                collected = [ ("empty", 36); ("nonempty", 64) ];
                coverage =
                  [
                    {
                      Property.label = "collision";
                      hits = 0;
                      satisfied = false;
                    };
                  ];
              }
            [ "p" ]
            (Failure.Fail [ Failure.message "coverage unsatisfied" ]);
        ] );
    ( "subtest entries: each names its subtest, a blank line between entries",
      finished [ Fixtures.subtest_result ] );
    ( "the tail closes the block, and the commands sit on the summary",
      finished ~config:(config ~invocation:exe ()) [ tail_and_commands ] );
    ( "a withheld correction: the tail, the reason, no command",
      finished ~config:(config ~invocation:exe ()) [ withheld ] );
    ( "an armed run's titles say so, beside the attempts",
      finished
        ~config:(config ~invocation:exe ~armed ())
        [
          failed "sub \u{203a} subtracts" "b";
          failed ~attempts:2 "sub \u{203a} retried" "b";
        ] );
    ( "a name's control bytes print escaped in its title",
      finished [ failed "first\nhalf" "b" ] );
    ( "a ?msg keeps its lines inside the block",
      rendered (fun r ->
          Report.header r ~suite:"s" ~tests:1 ~seed:None ();
          Report.result r
            (result [ "t" ]
               (Failure.Fail
                  [
                    Failure.equality ~msg:"first\nsecond\x07" ~expected:"1"
                      ~actual:"2" ();
                  ]))) );
    ( "a tail of twelve lines shows its last ten",
      tail_block (Failure.tail (numbered 1 12 ^ "\n")) );
    ( "a whole tail names its log at the heading's column",
      tail_block (Failure.tail ~log_path:"log.output" "only\n") );
    ( "and under colour the heading and the log are faint, the lines plain",
      tail_block ~ansi:true (Failure.tail ~log_path:"log.output" "only\n") );
    ( "a whole tail of several lines counts them",
      tail_block (Failure.tail "a\nb\nc\n") );
    ( "a tail the capture cut counts the bytes left out",
      tail_block (Failure.tail ~omitted_bytes:512 "kept\n") );
    ( "at the cap, the count adds the lines dropped",
      tail_block (Failure.tail ~omitted_bytes:9000 (numbered 1 12 ^ "\n")) );
    ( "a dropped blank line counts its newline",
      tail_block (Failure.tail ~omitted_bytes:1 ("\n" ^ numbered 2 11)) );
    ( "a trailing blank line is the last line shown",
      tail_block (Failure.tail ~omitted_bytes:1 (numbered 1 11 ^ "\n\n")) );
    ( "a captured tail's control bytes print escaped",
      finished ~duration:0.01
        [
          result [ "t" ]
            (Failure.Fail
               [
                 Failure.with_output_tail
                   (Failure.tail ~log_path:"log" "\027[31mred\027[0m captured\n")
                   (Failure.message "boom");
               ]);
        ] );
    ( "nothing written: a green run stays one line",
      rendered (fun r ->
          run_through ~tests:(Some 1)
            ~results:[ pass "t" ]
            ~baselines:(Baseline.create ~mode:Baseline.Check ())
            ~duration:0.002 r) );
    ( "a stopped run's summary counts what it did not run, stderr names the test",
      stopped (fun r ->
          Report.result r ok;
          Report.interrupted r
            ~running:(Some [ "g"; "sleeps\n" ])
            ~results:[ ok ] ~duration:0.5 ()) );
    ( "under -v the rows stay above the summary, stderr says between tests",
      stopped ~verbose:true (fun r ->
          Report.result r ok;
          Report.interrupted r ~running:None ~results:[ ok ] ~duration:0.5 ())
    );
    ( "stopped before its first result, while releasing a fixture",
      stopped (fun r ->
          Report.interrupted r ~releasing:"fixture (db.ml:3)\027[31m"
            ~running:None ~results:[] ~duration:0.5 ()) );
  ]

let write_source root =
  let path = Filename.concat root "t.ml" in
  Os.mkdir_p root;
  Out_channel.with_open_bin path (fun oc ->
      Out_channel.output_string oc "let () = expect x @@ __POS_OF__ {| a |}\n");
  path

let corrections_of ?invocation baselines =
  rendered
    ?config:(Option.map (fun invocation -> config ~invocation ()) invocation)
    (fun r ->
      run_through ~tests:(Some 1)
        ~results:[ pass "t" ]
        ~baselines ~duration:0.002 r)

let literal =
  Baseline.Literal { pos = ("t.ml", 1, 21, 0); value = " a "; exact = false }

let corrections_written () =
  let root = temp_dir () in
  let source = write_source root in
  stale_file root "help.expected";
  let b = Baseline.create ~root ~cwd:root ~mode:Baseline.Corrected () in
  (try Baseline.check b literal "b" with Failure.Check_failure _ -> ());
  (try Baseline.check b (fst help) (snd help)
   with Failure.Check_failure _ -> ());
  ignore (Baseline.settle b ~keep:true);
  Baseline.write b;
  let display name = Os.display_path (Filename.concat root name) in
  equal (list string)
    [
      "corrections (2):";
      "  wrote " ^ display "help.expected.corrected";
      "  wrote " ^ Os.display_path (source ^ ".corrected") ^ " (1 expectation)";
      "";
      "1 passed, 2 corrections written in 2.0ms.";
    ]
    (List.tl (List.rev (List.tl (List.rev (lines (corrections_of b))))));
  equal text
    (corrections_of ~invocation:`Mirrors b)
    (corrections_of ~invocation:exe b)

let accepted_literal () =
  let root = temp_dir () in
  let source = write_source root in
  let b = Baseline.create ~root ~cwd:root ~mode:Baseline.Update () in
  Baseline.check b literal "b";
  ignore (Baseline.settle b ~keep:true);
  Baseline.write b;
  equal string
    ("  accepted " ^ Os.display_path source
   ^ " (1 expectation; rebuild before the tests see it)")
    (List.nth (lines (corrections_of b)) 2)

let accepted_file () =
  let root, b = baselines_written ~mode:Baseline.Update [ help ] in
  equal string
    ("  accepted " ^ Os.display_path (Filename.concat root "help.expected"))
    (List.nth (lines (corrections_of b)) 2)

let refused_file () =
  let root, b =
    baselines_written ~mode:Baseline.Update ~refuse:true
      [ (Baseline.File "a.expected", "a\n"); help ]
  in
  let display name = Os.display_path (Filename.concat root name) in
  let t = lines (corrections_of b) in
  let head = "  could not write " ^ display "help.expected" ^ ": " in
  let refused = List.nth t 3 in
  equal string ("  accepted " ^ display "a.expected") (List.nth t 2);
  starts_with ~affix:head refused;
  not_contains ~sub:"help.expected"
    (String.sub refused (String.length head)
       (String.length refused - String.length head));
  equal string "1 passed, 1 correction accepted, 1 not written in 2.0ms."
    (last_line (corrections_of b))

(* The commands that end a report *)

let ending ?(config = config ~invocation:exe ()) ?(interrupted = false) results
    =
  rendered ~config (fun r ->
      if interrupted then
        Report.interrupted r ~running:None ~results ~duration:0.1 ()
      else Report.finish r ~results ~release_failures:[] ~duration:0.1 ())

let failing ?xfail name failures =
  result ?xfail [ name ] (Failure.Fail failures)

let two_props =
  [
    failing "even" [ Fixtures.prop_failure ];
    failing "small" [ Fixtures.prop_failure ];
  ]

let stale =
  [
    failing "cli" [ Fixtures.snap_mismatch ];
    failing "geo" [ Fixtures.prop_failure; Fixtures.snap_missing ];
  ]

let count ~sub s =
  List.length (List.filter (String.starts_with ~prefix:sub) (lines s))

let commands_on_summary () =
  let t = ending stale in
  equal (list string)
    [
      closing_rule;
      "";
      "accept: ./t.exe -u";
      "replay: ./t.exe --seed " ^ seed;
      "2 failed in 100ms.";
      "";
    ]
    (List.filteri (fun i _ -> i >= List.length (lines t) - 6) (lines t))

let one_of_each () =
  equal (pair int int) (1, 1)
    ( count ~sub:"accept:" (ending stale),
      count ~sub:"replay:" (ending two_props) )

let bail_accepts_its_test () =
  let bail =
    { (config ~invocation:exe ()) with Run.bail = true; filter = [ "geo" ] }
  in
  equal (list string)
    [ "accept: ./t.exe -u -f 'geo \u{203a} area'" ]
    (List.filter
       (String.starts_with ~prefix:"accept:")
       (lines
          (ending ~config:bail
             [ failing "geo \u{203a} area" [ Fixtures.snap_mismatch ] ])));
  equal (list string)
    [ "replay: ./t.exe --seed " ^ seed ^ " -f 'geo'" ]
    (List.filter
       (String.starts_with ~prefix:"replay:")
       (lines
          (ending ~config:bail
             [ failing "geo \u{203a} area" [ Fixtures.prop_failure ] ])))

let bail_escapes_its_path () =
  let bail = { (config ~invocation:exe ()) with Run.bail = true } in
  contains ~sub:"\naccept: ./t.exe -u -f $'first\\nhalf'\n"
    (ending ~config:bail [ failing "first\nhalf" [ Fixtures.snap_mismatch ] ])

let armed_commands () =
  let t =
    ending ~config:(config ~invocation:exe ~armed ()) (two_props @ stale)
  in
  contains
    ~sub:("\nreplay: ./t.exe --arm " ^ armed ^ " --seed " ^ seed ^ "\n")
    t;
  not_contains ~sub:"accept:" t

let mirrors_commands () =
  let t = ending ~config:{ (config ()) with Run.filter = [ "geo" ] } stale in
  contains
    ~sub:
      ("\nreplay: WINDTRAP_SEED=" ^ seed
     ^ " WINDTRAP_FILTER='geo' dune runtest\n")
    t;
  equal (pair int int) (2, 0)
    (count ~sub:"    accept: " t, count ~sub:"accept:" t)

let nothing_to_run_again_rows =
  [
    ( "an example or a plain failure drew nothing",
      ( false,
        [
          failing "ex"
            [
              Failure.property ~rendered:"0" ~case_index:0 ~shrink_steps:0
                ~root:Fixtures.root ~examples:true ();
            ];
          failing "plain" [ Failure.message "b" ];
        ] ) );
    ( "a failure with no kept correction accepts nothing",
      ( false,
        [
          failing "plain" [ Failure.message "b" ];
          failing "outside"
            [
              Failure.with_withheld Failure.Failed_outside
                Fixtures.snap_mismatch;
              Failure.message "b";
            ];
        ] ) );
    ( "an expected failure is neither replayed nor accepted",
      ( false,
        [
          failing ~xfail:Fixtures.xfail_reason "known"
            [ Fixtures.prop_failure; Fixtures.snap_mismatch ];
        ] ) );
    ("a signal kept tests from running", (true, two_props @ stale));
  ]

let nothing_to_run_again (_, (interrupted, results)) =
  equal (list string) []
    (List.filter
       (fun l ->
         String.starts_with ~prefix:"accept:" l
         || String.starts_with ~prefix:"replay:" l)
       (lines (ending ~interrupted results)))

let interrupted_says () =
  ignore (ending ~interrupted:true two_props);
  equal string "windtrap: interrupted between tests\n" (output ())

let transcript_group =
  group "The transcript"
    [
      test "the compact transcript prints as its baseline, unstyled"
        (plain_golden ~verbose:false "compact");
      test "the verbose transcript prints as its baseline, unstyled"
        (plain_golden ~verbose:true "verbose");
      test "the coloured compact transcript prints as its baseline"
        (coloured_golden ~verbose:false "compact-ansi");
      test "the coloured verbose transcript prints as its baseline"
        (coloured_golden ~verbose:true "verbose-ansi");
      test "the transcripts of each step print as their gallery" (fun () ->
          gallery "timelines" (timeline_entries ()));
      test "the verbose rows print as their gallery" (fun () ->
          gallery "rows" (row_entries ()));
      cases "header" ~name:fst header_rows header_row;
      test "the replay line carries the seed the header printed" seed_restated;
      cases "a duration prints in one form in a row and in the summary"
        ~name:fst duration_rows duration_row;
      test "a duration's unit is chosen after rounding" unit_after_rounding;
      cases "the live line is off" ~name:fst off_live_rows (fun (_, draw) ->
          equal string "" (draw ()));
      test "the live line is cut to 80 columns" live_line_cut;
      cases
        "a compact run prints nothing of a result that is no counted failure"
        ~name:fst silent_rows silent;
      cases "note" ~name:fst note_rows (fun (_, run) ->
          let t, expected = run () in
          equal string expected t);
      test "a compact transcript never carries a notice" noteworthy_notice;
      cases "observe prints the seed iff a selected test is a property"
        ~name:fst seed_rows (fun (_, (t, expected)) ->
          equal string expected (t ()));
      test "observe maps Interrupted to interrupted" observe_interrupted;
      test "observe raises nothing of its own" observe_raises_nothing;
      cases "--stream changes no line of the transcript" ~name:fst stream_rows
        (fun (_, (results, verbose, expected)) ->
          equal text expected (streamed ~verbose results));
      test "under --stream a test's bytes come before the report's"
        stream_drains;
      group "The selection"
        [
          cases "selection_description" ~name:fst selection_rows
            (fun (_, (focused, config, expected)) ->
              equal (option string) expected
                (Report.selection_description ~focused config));
          cases "empty_selection_reason" ~name:fst reason_rows
            (fun (_, (declared, selection, expected)) ->
              equal (option string) expected
                (Report.empty_selection_reason ~declared ~selection));
        ];
      group "The end of the run"
        [
          test "the endings print as their gallery" (fun () ->
              gallery "endings" (endings_entries ()));
          cases "an empty run says why" ~name:fst empty_rows
            (fun (_, (t, expected)) -> equal text expected (t ()));
          cases "the summary is the last line" ~name:fst summary_rows
            (fun (_, run) ->
              let t, expected = run () in
              equal string expected (last_line t));
          cases "the slow section lists the results at or over the threshold"
            ~name:fst slow_section_rows slow_section;
          test "corrections: one written row per file, in path order"
            corrections_written;
          test "corrections: an accepted file's row" accepted_file;
          test "corrections: an accepted literal asks for a rebuild"
            accepted_literal;
          test "corrections: a file not written is a row that says why"
            refused_file;
          test "accept then replay sit right above the summary"
            commands_on_summary;
          test "one accept and one replay for the whole report" one_of_each;
          test "-x accepts the test it stopped on and replays the selection"
            bail_accepts_its_test;
          test "a path in an accept line stays one line" bail_escapes_its_path;
          test "an armed run arms its replay and accepts nothing" armed_commands;
          test "under a build action each block promotes its own file"
            mirrors_commands;
          cases "no accept and no replay" ~name:fst nothing_to_run_again_rows
            nothing_to_run_again;
          test "an interrupted run says on stderr what it stopped"
            interrupted_says;
        ];
    ]

(* The GitHub Actions envelope *)

(* A row's annotation is made in its test. *)
let annotation ?invocation ?armed ?(path = [ "t" ]) f () =
  Report.annotation ?invocation ?armed ~path f

let annotations ?invocation ?armed ?(release_failures = []) results () =
  Report.annotations ?invocation ?armed ~release_failures results

let located file line f =
  { f with Failure.loc = Some { Loc.file; line; column = 0 } }

let qa = `Exe "dune exec qa/x/t.exe --"

let annotation_rows =
  [
    ( "a percent is encoded first, a CR shown as the block shows it",
      ( annotation (Failure.message "50% done\r\nnext: a,b"),
        "50%25 done\\x0d%0A    next" ) );
    ( "colons and commas in the message are data",
      (annotation (Failure.message "50% done\r\nnext: a,b"), "next: a,b") );
    ( "the file property encodes its delimiters",
      ( annotation ~path:[ "suite: a,b"; "case" ]
          (located "dir,x:y/test.ml" 7 (Failure.message "boom")),
        "file=dir%2Cx%3Ay/test.ml,line=7," ) );
    ( "the title encodes its delimiters",
      ( annotation ~path:[ "suite: a,b"; "case" ] (Failure.message "boom"),
        "title=Test failure%3A suite%3A a%2Cb \u{203a} case::" ) );
    ( "the title spells control bytes as the block's title does",
      ( annotation ~path:[ "a\001b\nc" ] (Failure.message "boom"),
        "title=Test failure%3A a\\x01b\\x0ac::" ) );
    ( "no location, no file property",
      (annotation (Failure.message "boom"), "::error title=Test failure%3A t::")
    );
    ( "a failure at the declaration annotates that line",
      ( annotation (located "test/test_users.ml" 88 Fixtures.eq_failure),
        "file=test/test_users.ml,line=88," ) );
    ( "and its message opens on the bare location",
      ( annotation (located "test/test_users.ml" 88 Fixtures.eq_failure),
        "::    test/test_users.ml:88%0A    expected  [(\"alice\"" ) );
    ( "a property's annotation carries its test's replay",
      ( annotation ~path:[ "geo"; "area non-negative" ] Fixtures.prop_failure,
        "replay: WINDTRAP_SEED=" ^ seed
        ^ " WINDTRAP_FILTER='geo \u{203a} area non-negative' dune runtest" ) );
    ( "and its counterexample",
      ( annotation ~path:[ "geo"; "area non-negative" ] Fixtures.prop_failure,
        "counterexample (case 12, shrunk 4 steps): Rect (2, 0)" ) );
    ( "a launcher spells the replay",
      ( annotation ~invocation:qa
          ~path:[ "geo"; "area non-negative" ]
          Fixtures.prop_failure,
        "%0A    replay: dune exec qa/x/t.exe -- --seed " ^ seed
        ^ " -f 'geo \u{203a} area non-negative'" ) );
    ( "a compared value keeps its bytes, escaped",
      ( annotation
          (Failure.equality ~expected:"\027[32mgreen\027[0m" ~actual:"plain" ()),
        {|\x1b[32mgreen\x1b[0m|} ) );
    ( "annotations pass the launcher to accept",
      ( annotations ~invocation:qa
          [
            result [ "cli"; "cli help" ]
              (Failure.Fail [ Fixtures.snap_missing ]);
          ],
        "%0A    accept: dune exec qa/x/t.exe -- -u -f 'cli \u{203a} cli help'\n"
      ) );
    ( "and the armed mutant to replay",
      ( annotations ~invocation:qa ~armed
          [ result [ "geo" ] (Failure.Fail [ Fixtures.prop_failure ]) ],
        "%0A    replay: dune exec qa/x/t.exe -- --arm " ^ armed ^ " --seed "
        ^ seed ^ " -f 'geo'\n" ) );
    ( "a subtest's annotation is titled by its test",
      ( annotations [ Fixtures.subtest_result ],
        "title=Test failure%3A backend \u{203a} contract::" ) );
    ( "and points into its body",
      ( annotations [ Fixtures.subtest_result ],
        "file=test/test_backend.ml,line=40," ) );
    ( "its subtest line follows the entry's location",
      ( annotations [ Fixtures.subtest_result ],
        "::    test/test_backend.ml:40%0A    subtest   shape [0]%0A" ) );
    ( "an unexpected pass annotates",
      ( annotations [ Fixtures.xpass_result ],
        "title=Test failure%3A known \u{203a} fixed already::" ) );
    ( "a failed test annotates beside an excused one",
      ( annotations [ Fixtures.excused_result; failed "bad" "boom" ],
        "title=Test failure%3A bad::" ) );
    ( "every failing test is named",
      ( annotations
          ~release_failures:[ Fixtures.release_failure ]
          Fixtures.results,
        "title=Test failure%3A db \u{203a} insert::" ) );
  ]

let annotation_row (_, (a, sub)) = contains ~sub (a ())

let annotation_absent_rows =
  [
    ( "no location, no file property",
      (annotation (Failure.message "boom"), "file=") );
    ( "nothing about ~__POS__ or a declaration",
      ( annotation (located "test/test_users.ml" 88 Fixtures.eq_failure),
        "declared" ) );
    ( "a launcher's replay has no mirror",
      ( annotation ~invocation:qa
          ~path:[ "geo"; "area non-negative" ]
          Fixtures.prop_failure,
        "WINDTRAP_SEED" ) );
    ( "no escape reaches an annotation",
      ( annotation
          (Failure.equality ~expected:"\027[32mgreen\027[0m" ~actual:"plain" ()),
        "\027" ) );
    ( "an excused test is not annotated",
      ( annotations [ Fixtures.excused_result; failed "bad" "boom" ],
        "broken carry" ) );
    ( "a withheld correction's annotation offers no acceptance",
      ( annotations
          [
            result [ "cli"; "both" ]
              (Failure.Fail
                 [
                   Failure.with_output_tail
                     (Failure.tail "log line\n")
                     (Failure.message "boom");
                   Failure.with_withheld Failure.Failed_outside
                     Fixtures.snap_mismatch;
                 ]);
          ],
        "accept:" ) );
  ]

let annotation_absent (_, (a, sub)) = not_contains ~sub (a ())

let command_lines_rows =
  [
    ( "an annotation is one line",
      (annotation (Failure.message "50% done\r\nnext: a,b"), 1) );
    ( "a command per failure entry, the teardown pair two, the release one",
      ( annotations
          ~release_failures:[ Fixtures.release_failure ]
          Fixtures.results,
        8 ) );
    ("a failure per subtest entry", (annotations [ Fixtures.subtest_result ], 3));
    ( "an excused failure has none",
      (annotations [ Fixtures.excused_result; failed "bad" "boom" ], 1) );
  ]

let command_lines (_, (a, n)) =
  let a = a () in
  equal int n (List.length (String.split_on_char '\n' a) - 1);
  equal int n
    (List.length
       (List.filter (String.starts_with ~prefix:"::error ") (lines a)))

let empty_annotation_rows =
  [
    ("no failure", [ pass "ok"; result [ "s" ] (Failure.Skip None) ]);
    ("no result", []);
    ("every failure excused", [ Fixtures.excused_result ]);
  ]

let withheld_annotation () =
  let a =
    annotations
      [
        result [ "cli"; "both" ]
          (Failure.Fail
             [
               Failure.with_output_tail
                 (Failure.tail "log line\n")
                 (Failure.message "boom");
               Failure.with_withheld Failure.Failed_outside
                 Fixtures.snap_mismatch;
             ]);
      ]
      ()
  in
  ends_with
    ~affix:
      "%0A    no correction was kept: the test also failed outside its \
       expectations; fix that failure and rerun\n"
    a

let envelope_composed () =
  let bad = failed "bad" "boom" in
  let r = Report.create ~out:Format.std_formatter ~ansi:false (config ()) in
  print_string (Report.group_start "mylib");
  observe r
    (Run.Run_started
       { suite = "mylib"; total = 1; selected = 1; properties = false });
  observe r (Run.Test_finished bad);
  Report.finish r ~release_failures:[] ~results:[ bad ] ~duration:0.01
    ~before_summary:(fun () ->
      print_string Report.group_end;
      print_string
        (Report.annotations ~release_failures:[] ~invocation:`Mirrors [ bad ]))
    ();
  equal text
    ("::group::mylib\nmylib: 1 test\n" ^ failures_rule
   ^ "\n  FAIL  bad\n    boom\n" ^ closing_rule
   ^ "\n\
      ::endgroup::\n\
      ::error title=Test failure%3A bad::    boom\n\n\
      1 failed in 10ms.\n")
    (output ())

(* The hook writes past the formatter, as [print_string] does: what the
   renderer committed is flushed before it runs. *)
let folded results =
  let b = Buffer.create 256 in
  let ppf = Format.formatter_of_buffer b in
  let r = Report.create ~out:ppf ~ansi:false (config ()) in
  Report.header r ~suite:"mylib" ~tests:(List.length results) ~seed:None ();
  List.iter (Report.result r) results;
  Report.finish r ~release_failures:[] ~results ~duration:2.0
    ~before_summary:(fun () -> Buffer.add_string b "::endgroup::\n")
    ();
  Format.pp_print_flush ppf ();
  Buffer.contents b

let fold_rows =
  [
    ( "the close follows the last section, the blank line the close",
      ( [ failed "bad" "boom"; pass ~duration:1.5 "slow one" ],
        "mylib: 2 tests\n" ^ failures_rule ^ "\n  FAIL  bad\n    boom\n"
        ^ closing_rule
        ^ "\n\n\
           slow tests (1, over 1s):\n\
          \  1.5s  slow one\n\
           ::endgroup::\n\n\
           1 passed, 1 failed in 2.0s.\n" ) );
    ( "a green run: the close, then the one line",
      ([ pass "ok" ], "::endgroup::\nmylib: 1 passed in 2.0s.\n") );
  ]

let group_rows =
  [
    ("group_start", (Report.group_start "mylib", "::group::mylib\n"));
    ("group_end", (Report.group_end, "::endgroup::\n"));
    ( "a group's name has its newline encoded",
      (Report.group_start "a\nb", "::group::a%0Ab\n") );
  ]

let github =
  group "The GitHub Actions envelope"
    [
      cases "the fold's commands" ~name:fst group_rows
        (fun (_, (c, expected)) -> equal string expected c);
      test "an annotation prints as its baseline" (fun () ->
          expect_file
            (annotation
               ~path:[ "users"; "sessions after login" ]
               Fixtures.eq_failure ())
            (baseline "annotation"));
      cases "an annotation holds" ~name:fst annotation_rows annotation_row;
      cases "an annotation leaves out" ~name:fst annotation_absent_rows
        annotation_absent;
      cases "one command line per counted failure entry" ~name:fst
        command_lines_rows command_lines;
      cases "no annotation" ~name:fst empty_annotation_rows (fun (_, results) ->
          equal string "" (annotations results ()));
      test "an annotation with no command ends on its facts" (fun () ->
          equal string "::error title=Test failure%3A bad::    boom\n"
            (annotations ~invocation:qa ~armed [ failed "bad" "boom" ] ()));
      test "a withheld correction's annotation ends on the reason"
        withheld_annotation;
      test "the fold closes before the annotations, the summary last"
        envelope_composed;
      cases "the fold closes after the last section" ~name:fst fold_rows
        (fun (_, (results, expected)) -> equal text expected (folded results));
    ]

(* Mutation lines *)

let verdict_rows =
  [
    ( "an announcement",
      ( (fun r ->
          Report.mutation_armed r ~id:"lib/calc.ml:9:12:add" ~before:"a + b"
            ~after:"a - b"),
        "mutant lib/calc.ml:9:12:add armed: a + b \u{2192} a - b\n" ) );
    ("killed", ((fun r -> Report.mutation_killed r), "mutant killed.\n"));
    ( "survived, evaluated once",
      ( (fun r -> Report.mutation_survived r ~hits:1 ~xfail_failed:false),
        "mutant survived: the armed site was evaluated 1 time and no test \
         failed.\n" ) );
    ( "survived, evaluated twice",
      ( (fun r -> Report.mutation_survived r ~hits:2 ~xfail_failed:false),
        "mutant survived: the armed site was evaluated 2 times and no test \
         failed.\n" ) );
    ( "survived, and an xfail test passed",
      ( (fun r -> Report.mutation_survived r ~hits:2 ~xfail_failed:true),
        "mutant survived: the site was evaluated 2 times and only xfail tests \
         failed.\n" ) );
    ( "not evaluated",
      ( (fun r -> Report.mutation_not_evaluated r),
        "mutant not evaluated: no selected test ran the site.\n" ) );
    ( "not reached, only xfail tests ran the site",
      ( (fun r -> Report.mutation_not_reached r),
        "mutant not reached: only xfail tests ran the site.\n" ) );
  ]

let unflushed (_, (write, expected)) =
  let b = Buffer.create 64 in
  let ppf = Format.formatter_of_buffer b in
  write (Report.create ~out:ppf ~ansi:false (config ~armed:"x" ()));
  let before = Buffer.contents b in
  Format.pp_print_flush ppf ();
  equal (pair string string) ("", expected) (before, Buffer.contents b)

let loop_invocation =
  `Exe "dune exec --instrument-with ppx_windtrap.mutate test/test_calc.exe --"

let source_of texts =
  let last = List.fold_left (fun n (line, _) -> max n line) 0 texts in
  String.concat "\n"
    (List.init last (fun i ->
         Option.value ~default:"" (List.assoc_opt (i + 1) texts)))
  ^ "\n"

let calc_source =
  source_of
    [ (13, "  | Sub -> a - b"); (21, "let sign n = if n > 0 then 1 else 0") ]

let survivor id line before after witnesses =
  {
    Sections.mutant =
      { Sections.id; line; before; after; source = Some calc_source };
    witnesses =
      List.map
        (fun (test, line) ->
          {
            Sections.test;
            loc = Some { Loc.file = "test/test_calc.ml"; line; column = 0 };
            exe = None;
          })
        witnesses;
  }

let add_survivor =
  survivor "lib/calc.ml:13:11:add" 13 "a - b" "a + b"
    [ ("subtraction \u{203a} stays positive", 19) ]

let ge_survivor =
  survivor "lib/calc.ml:21:16:ge" 21 "n > 0" "n >= 0"
    [ ("sign of a negative", 31); ("sign of a positive", 30) ]

let loop_report =
  {
    Sections.survivors = [ add_survivor; ge_survivor ];
    not_evaluated = [];
    unreached = [ ("lib/calc.ml", 40); ("lib/calc.ml", 41) ];
    outside_tests = [];
    killed = 3;
    not_tested = 0;
    scope = Sections.Suite;
  }

let loop_steps (m : Sections.mutation) =
  let total = List.length m.survivors + m.killed in
  List.concat
    (List.mapi
       (fun i (s : Sections.survivor) ->
         [
           ( strf "mutation_testing %d" (i + 1),
             fun r ->
               Report.mutation_testing r ~index:(i + 1) ~total ~id:s.mutant.id
           );
           ("mutation_survivor", fun r -> Report.mutation_survivor r s);
         ])
       m.survivors)
  @ [ ("mutation_finish", fun r -> Report.mutation_finish r m) ]

let loop ?(ansi = false) ?terminal m =
  timeline ~ansi ?terminal
    ~config:(config ~invocation:loop_invocation ())
    (loop_steps m)

let mutation_entries () =
  [
    ( "a loop commits each survivor's block when its child ends",
      loop loop_report );
    ("and styled", loop ~ansi:true loop_report);
    ( "the live line names the mutant, and is erased before a block",
      loop ~ansi:true ~terminal:true
        {
          loop_report with
          Sections.survivors = [ add_survivor ];
          unreached = [];
        } );
    ( "a killed mutant leaves nothing, the next line draws over it",
      timeline ~ansi:true ~terminal:true
        [
          ( "mutation_testing 1",
            fun r ->
              Report.mutation_testing r ~index:1 ~total:2
                ~id:"lib/calc.ml:9:3:sub" );
          ( "mutation_testing 2",
            fun r ->
              Report.mutation_testing r ~index:2 ~total:2
                ~id:"lib/calc.ml:13:11:add" );
        ] );
    ( "every mutant killed: the outcome alone",
      loop ~ansi:true
        { loop_report with Sections.survivors = []; unreached = [] } );
    ( "an interrupted loop closes over the children that ended",
      rendered ~config:(config ~invocation:loop_invocation ()) (fun r ->
          Report.mutation_finish r
            {
              loop_report with
              Sections.survivors = [ add_survivor ];
              killed = 0;
              not_tested = 2;
            }) );
    ( "stopped before a child ended",
      rendered (fun r ->
          Report.mutation_finish r
            {
              loop_report with
              Sections.survivors = [];
              unreached = [];
              killed = 0;
              not_tested = 5;
            }) );
  ]

let live_off_rows =
  [ ("off a terminal", (true, false)); ("without colour", (false, true)) ]

let refused_after_live () =
  let r =
    Report.create ~out:Format.std_formatter ~ansi:true ~terminal:true
      (config ())
  in
  Report.mutation_testing r ~index:1 ~total:2 ~id:"lib/a.ml:1:0:add";
  Report.mutation_refused r "no mutant";
  equal text
    "\r\027[2K\027[2m  [1/2] lib/a.ml:1:0:add\u{2026}\027[0m\r\027[2Kwindtrap: \
     no mutant\n"
    (output ())

let interrupted_rows =
  [
    ( "a child",
      ( Some "lib/calc.ml:9:12:add",
        "windtrap: interrupted while testing lib/calc.ml:9:12:add\n" ) );
    ( "the determinism probe",
      (None, "windtrap: interrupted during the determinism probe\n") );
  ]

let interrupted_loop (_, (testing, expected)) =
  let t =
    rendered (fun r ->
        Report.mutation_interrupted r ~testing
          { loop_report with Sections.survivors = []; unreached = [] })
  in
  equal (pair string string)
    (expected, "mutants: 3 reached by this suite, 3 killed\n")
    (output (), t)

let mutation =
  group "Mutation lines"
    [
      cases "an armed run's announcement and verdicts" ~name:fst verdict_rows
        unflushed;
      test "a loop's lines print as their gallery" (fun () ->
          gallery "mutation" (mutation_entries ()));
      cases "the loop's live line is off" ~name:fst live_off_rows
        (fun (_, (ansi, terminal)) ->
          equal string ""
            (rendered ~ansi ~terminal (fun r ->
                 Report.mutation_testing r ~index:1 ~total:2
                   ~id:"lib/calc.ml:9:3:sub")));
      test
        "mutation_refused erases the live line and flushes before its message"
        refused_after_live;
      cases "mutation_interrupted says what it stopped, then closes" ~name:fst
        interrupted_rows interrupted_loop;
    ]

(* Failure projections *)

let projections () =
  let fs =
    [
      Fixtures.prop_failure;
      Fixtures.subtest_failure "shape [0]";
      Fixtures.snap_missing;
    ]
  in
  let entry pp f =
    Format.asprintf "%a"
      (pp ~ansi:false ?terminal:None ?excerpt:None ?hints:None ?filter:None
         ?invocation:None ?armed:None)
      f
  in
  equal (list string)
    (List.map Sections.headline fs)
    (List.map Report.headline fs);
  equal (list bool)
    (List.map Sections.is_subtest_failure fs)
    (List.map Report.is_subtest_failure fs);
  equal
    (list (option string))
    (List.map Sections.labeled_msg fs)
    (List.map Report.labeled_msg fs);
  equal (list string)
    (List.map (entry Sections.pp_failure) fs)
    (List.map (entry Report.pp_failure) fs)

let () =
  exit
    (run "report"
       [
         renderer;
         transcript_group;
         github;
         mutation;
         group "Failure projections"
           [ test "they are Report_sections'" projections ];
       ])
