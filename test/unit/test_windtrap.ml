(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* [run] and [Run.execute] refuse to start while a run is active, and every
   test body runs inside this suite's own run. The runs the tests judge are
   therefore recorded as the module initialises, before the [run] that ends
   the file. *)

open Windtrap
module Baseline = Windtrap.Private.Baseline
module Failure = Windtrap.Private.Failure
module Loc = Windtrap.Private.Loc
module Os = Windtrap.Private.Os
module Property = Windtrap.Private.Property
module Report = Windtrap.Private.Report
module Run = Windtrap.Private.Run
module Test_tree = Windtrap.Private.Test_tree
module Scratch = Windtrap_test_support.Scratch

let strf = Printf.sprintf
let touch path = close_out (open_out path)
let read path = In_channel.with_open_bin path In_channel.input_all

let write path text =
  Out_channel.with_open_bin path (fun oc -> output_string oc text)

let contents path = if Sys.file_exists path then Some (read path) else None

(* How [fn] ended: [None] when it returned, else what it raised. A test hands
   the exception to [raises] again with [replay]. *)
let escape fn = match fn () with _ -> None | exception e -> Some e
let replay escaped = Option.iter raise escaped
let only = function [ x ] -> Some x | _ -> None
let rows_of r = Run.results (Recorded.outcome r).run

let row_at r path =
  let at (row : Run.result) = List.equal String.equal row.path path in
  require_some (List.find_opt at (rows_of r))

let counted r =
  List.filter_map
    (fun (row : Run.result) ->
      if row.counted then Some (Test_tree.path_to_string row.path) else None)
    (rows_of r)

let failure r path = require_match only (Recorded.failures r path)

let raised =
  require_match (fun (f : Failure.t) ->
      match f.kind with
      | Failure.Raise { actual = Some actual; _ } -> Some actual.kept
      | _ -> None)

let message =
  require_match (fun (f : Failure.t) ->
      match f.kind with Failure.Message m -> Some m.kept | _ -> None)

let stats r path = require_some (row_at r path).prop_stats

(* The last line of [s], which ends with a newline, or [""]. *)
let last_line s =
  match List.rev (String.split_on_char '\n' s) with
  | "" :: last :: _ -> last
  | _ -> ""

(* [s] is one line, ended by its only newline. *)
let one_line ?__POS__ s =
  equal ?__POS__ (option int)
    (Some (String.length s - 1))
    (String.index_opt s '\n')

(* Runs of the facade *)

(* A call of [run] states its environment as [Recorded] does: every
   [WINDTRAP_*] variable, [CI], [GITHUB_ACTIONS], [INSIDE_DUNE], [NO_COLOR] and
   [TERM] unset, then [env], all put back when the call ends. Its two output
   streams go to files, descriptors included. *)

type ran = { code : int; out : string; err : string }

let stated_names env =
  let windtrap binding =
    match String.index_opt binding '=' with
    | Some i when String.starts_with ~prefix:"WINDTRAP_" binding ->
        Some (String.sub binding 0 i)
    | Some _ | None -> None
  in
  List.sort_uniq String.compare
    ([ "CI"; "GITHUB_ACTIONS"; "INSIDE_DUNE"; "NO_COLOR"; "TERM" ]
    @ List.filter_map windtrap (Array.to_list (Unix.environment ()))
    @ List.map fst env)

let with_environment env fn =
  let saved = List.map (fun n -> (n, Sys.getenv_opt n)) (stated_names env) in
  List.iter (fun (name, _) -> Os.setenv name None) saved;
  List.iter (fun (name, v) -> Os.setenv name (Some v)) env;
  Fun.protect
    ~finally:(fun () -> List.iter (fun (n, v) -> Os.setenv n v) saved)
    fn

let flush_all () =
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  flush_all ()

let redirect path fd =
  let file = Unix.openfile path [ Unix.O_WRONLY; O_CREAT; O_TRUNC ] 0o600 in
  Unix.dup2 file fd;
  Unix.close file

(* The runner's capture moves the same two descriptors around every test and
   restores what it saved, which is these files. *)
let with_output_in dir fn =
  let out = Filename.concat dir "stdout"
  and err = Filename.concat dir "stderr" in
  flush_all ();
  let saved_out = Unix.dup ~cloexec:true Unix.stdout in
  let saved_err = Unix.dup ~cloexec:true Unix.stderr in
  redirect out Unix.stdout;
  redirect err Unix.stderr;
  let restore () =
    flush_all ();
    Unix.dup2 saved_out Unix.stdout;
    Unix.dup2 saved_err Unix.stderr;
    Unix.close saved_out;
    Unix.close saved_err
  in
  let code = Fun.protect ~finally:restore fn in
  { code; out = read out; err = read err }

let run_argv ?(env = []) argv suite tests =
  let dir = Scratch.dir "windtrap-facade-" in
  with_environment env @@ fun () ->
  with_output_in dir (fun () -> run ~argv suite tests)

let logs () = Filename.concat (Scratch.dir "windtrap-logs-") "logs"

(* Every run here names its log directory and turns styling off. *)
let facade ?env ?(logs = logs ()) ?(flags = []) suite tests =
  let argv = suite :: "-o" :: logs :: "--color" :: "never" :: flags in
  run_argv ?env (Array.of_list argv) suite tests

let passes = test "passes" ignore
let boom = test "boom" (fun () -> equal int 1 2)

(* Types *)

(* The lines are found in this file's copy beside the executable, by the
   markers in their comments. *)
let this_file = Filename.basename __FILE__

let source_line marker =
  let source =
    Filename.concat (Filename.dirname Sys.executable_name) this_file
  in
  let lines = String.split_on_char '\n' (read source) in
  let rec find n = function
    | [] -> 0
    | line :: rest ->
        if Windtrap.Private.Text.contains_substring ~pattern:marker line then n
        else find (n + 1) rest
  in
  find 1 lines

let[@inline never] own_line_helper x =
  is_true x (* helper: own line *);
  ()

let[@inline never] tail_helper x = is_true x
let[@inline never] pos_helper ?__POS__ x = is_true ?__POS__ x

let[@inline never] calls_tail_helper () =
  tail_helper false (* helper: caller *);
  ()

let helpers =
  Recorded.execute
    [
      test "own line" (fun () -> own_line_helper false);
      test "tail" calls_tail_helper;
      test "passed on" (fun () ->
          pos_helper ~__POS__:("given.ml", 7, 0, 0) false;
          ());
    ]

let reported_at path =
  let loc = require_some (failure helpers path).loc in
  (Filename.basename loc.Loc.file, loc.Loc.line)

let types =
  group "Types"
    [
      test "a helper that wraps a verb reports a line of its own" (fun () ->
          equal (pair string int)
            (this_file, source_line ("(* helper: " ^ "own line *)"))
            (reported_at [ "own line" ]));
      test "a helper's verb in tail position reports its caller's line"
        (fun () ->
          equal (pair string int)
            (this_file, source_line ("(* helper: " ^ "caller *)"))
            (reported_at [ "tail" ]));
      test "a helper that passes ?__POS__ on reports its caller's location"
        (fun () ->
          equal (pair string int) ("given.ml", 7) (reported_at [ "passed on" ]));
    ]

(* Declaring tests *)

let tagged_prop () =
  let tests =
    [
      test "plain" ignore;
      prop "law" Gen.int ignore;
      group "outer" [ prop "law" Gen.int ignore ];
    ]
  in
  let carries (c : Test_tree.case) =
    Test_tree.Tag.mem Test_tree.Tag.prop c.tags
  in
  equal (list string) [ "law"; "outer › law" ]
    (List.filter_map
       (fun (c : Test_tree.case) ->
         if carries c then Some (Test_tree.path_to_string c.path) else None)
       (Test_tree.flatten tests))

let both_flags =
  let tests = [ test ~tags:[ "x" ] "tagged" ignore; test "untagged" ignore ] in
  List.map
    (fun flags ->
      (String.concat " " flags, facade ~flags:("-l" :: flags) "both" tests))
    [
      [ "--tag"; "x"; "--exclude-tag"; "x" ];
      [ "--exclude-tag"; "x"; "--tag"; "x" ];
    ]

let retried, drawn, called =
  let drawn = ref [] and called = ref [] in
  let r =
    Recorded.execute
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
  (r, List.rev !drawn, List.rev !called)

(* The attempts of a retried test, the draws of each in turn. *)
let replays_every_attempt draws =
  let n = List.length draws / 3 in
  let attempt k = List.filteri (fun i _ -> i / n = k) draws in
  not_equal (list int) [] (attempt 0);
  equal (list (list int)) [ attempt 0; attempt 0 ] [ attempt 1; attempt 2 ]

let[@inline never] failing_create () =
  ignore (failwith "no database");
  ()

let failed_create =
  let db = fixture failing_create in
  Recorded.execute [ test "first" db; test "second" db ]

let the_second_call_has_the_first_backtrace () =
  let backtrace (f : Failure.t) =
    match f.kind with
    | Failure.Raise { backtrace = Some bt; _ } -> Some bt.kept
    | _ -> None
  in
  contains ~sub:"failing_create"
    (require_match backtrace (failure failed_create [ "second" ]))

let declaring =
  group "Declaring tests"
    [
      test "prop adds the prop tag, under a group too" tagged_prop;
      cases "a tag named by both --tag and --exclude-tag is excluded" ~name:fst
        both_flags (fun (_, r) -> equal text "untagged\n" r.out);
      test "a property and a stateful test inherit a group's retries" (fun () ->
          equal (pair int int) (3, 3)
            ( (row_at retried [ "g"; "p" ]).attempts,
              (row_at retried [ "g"; "s" ]).attempts ));
      test "every retry of a property replays the same cases" (fun () ->
          replays_every_attempt drawn);
      test "every retry of a stateful test replays the same programs" (fun () ->
          replays_every_attempt called);
      test "a fixture whose create raised raises it again with its backtrace"
        the_second_call_has_the_first_backtrace;
    ]

(* Properties *)

let labelled =
  Recorded.execute
    [
      prop "labelled" ~count:25 Gen.small_int (fun n ->
          collect (if n mod 2 = 0 then "even" else "odd");
          classify "small" (abs n < 100);
          cover "any" true);
    ]

let the_labels_reach_the_engine () =
  let s = stats labelled [ "labelled" ] in
  equal (list string)
    [ "any"; "even"; "odd"; "small" ]
    (List.map fst s.collected);
  equal
    (list (pair string bool))
    [ ("any", true) ]
    (List.map
       (fun (c : Property.cover_status) -> (c.label, c.satisfied))
       s.coverage)

let strays =
  Recorded.execute
    [
      test "collect" (fun () -> collect "label");
      test "classify" (fun () -> classify "label" false);
      test "cover" (fun () -> cover "label" false);
      test "reject" (fun () -> reject ());
    ]

let a_label_outside_a_property name =
  let text = raised (failure strays [ name ]) in
  starts_with ~affix:"Invalid_argument" text;
  contains ~sub:name text;
  contains ~sub:"property" text

let chosen_example =
  let branch tag =
    Gen.with_pp (fun ppf -> Format.fprintf ppf "%s:%d" tag) Gen.int
  in
  Recorded.execute
    [
      prop ~count:0 ~examples:[ 5 ] "example"
        (Gen.one_of [ branch "first"; branch "second" ])
        (fun _ -> is_true false);
    ]

let rendered =
  require_match (fun (f : Failure.t) ->
      match f.kind with
      | Failure.Property { rendered; _ } -> Some rendered.kept
      | _ -> None)

let properties =
  group "Properties"
    [
      test "collect, classify and cover label the cases of the running law"
        the_labels_reach_the_engine;
      cases "a label outside a property fails the test with Invalid_argument"
        ~name:Fun.id
        [ "collect"; "classify"; "cover" ]
        a_label_outside_a_property;
      test "reject outside a property fails the test with the message"
        (fun () ->
          equal string "assume or reject was called outside a property"
            (message (failure strays [ "reject" ])));
      test "an example under one_of prints with the first branch's printer"
        (fun () ->
          equal string "first:5"
            (rendered (failure chosen_example [ "example" ])));
    ]

(* Stateful tests *)

let programs, opened =
  let scopes = ref 0 in
  let counting k =
    incr scopes;
    k ()
  in
  let tick = [ call "tick" ~next:succ (fun model () -> is_true (model < 2)) ] in
  let r =
    Recorded.execute
      [
        stateful ~count:3 ~steps:3 "summary" ~model:0
          ~scope:(fun k -> k ())
          tick;
        stateful ~count:0 "none" ~model:0 ~scope:counting tick;
      ]
  in
  (r, !scopes)

let summary =
  require_match (fun (f : Failure.t) ->
      match f.kind with
      | Failure.Property { summary = Some s; _ } -> Some s.kept
      | _ -> None)

let stateful_tests =
  group "Stateful tests"
    [
      test "a failing program's summary is the failure's" (fun () ->
          equal string "3 calls, last: tick"
            (summary (failure programs [ "summary" ])));
      test "~count:0 draws no case and opens no system" (fun () ->
          equal (pair int int) (0, 0) ((stats programs [ "none" ]).cases, opened));
    ]

(* Baselines *)

let project env root = ("WINDTRAP_PROJECT_ROOT", root) :: env

let literals, stale_line =
  let root = Scratch.dir "windtrap-literals-" in
  let stale = ref 0 in
  let r =
    Recorded.execute ~env:(project [] root)
      [
        test "flexible" (fun () ->
            expect "a\n  b\n"
            @@ __POS_OF__
                 {|
                 a
                   b
               |});
        test "exact" (fun () -> expect_exact "a\n" @@ __POS_OF__ "a\n");
        test "stale" (fun () ->
            let (((_, line, _, _), _) as literal) = __POS_OF__ {| old |} in
            stale := line;
            expect "new" literal);
        test "stale exact" (fun () -> expect_exact "new" @@ __POS_OF__ "new ");
      ]
  in
  (r, !stale)

let read_as path =
  let verb (f : Failure.t) =
    match f.kind with
    | Failure.Baseline { baseline = Failure.Literal { exact }; _ } ->
        Some (if exact then "expect_exact" else "expect")
    | _ -> None
  in
  require_match verb (failure literals path)

let a_literal_is_located_at_its_position () =
  let loc = require_some (failure literals [ "stale" ]).loc in
  equal (pair string int) (this_file, stale_line)
    (Filename.basename loc.Loc.file, loc.Loc.line)

let relative_file =
  let root = Scratch.dir "windtrap-relative-" in
  write (Filename.concat root "rel.expected") "v\n";
  Recorded.execute ~env:(project [] root)
    [
      test "moved" (fun () ->
          chdir (temp_dir ());
          expect_file "v" "rel.expected");
    ]

let bracketed, bracket_root =
  let root = Scratch.dir "windtrap-bracketed-" in
  let r =
    Recorded.execute ~env:(project [] root)
      ~config:(fun c -> { c with baseline = Baseline.Update })
      [
        bracket ~setup:ignore ~teardown:ignore "bracketed" (fun () ->
            expect_file "content\n" "src/bracketed.expected");
      ]
  in
  (r, root)

let written r =
  List.map
    (function
      | Baseline.Written { path; literals } -> strf "wrote %s, %d" path literals
      | Baseline.Refused { path; reason } -> strf "refused %s: %s" path reason)
    (Baseline.writes (Run.baselines (Recorded.outcome r).run))

(* A --corrected run that wrote a correction and returns 1 warns on stderr;
   every other run says nothing there. Each run has a baseline file of its
   own, since -u leaves one behind. *)
let promotion =
  let root = Scratch.dir "windtrap-promotion-" in
  let stale name =
    let file = name ^ ".expected" in
    write (Filename.concat root file) "old\n";
    test name (fun () -> expect_file "new\n" file)
  in
  let masked =
    test "masked" (fun () ->
        expect_file "new\n" "masked.expected";
        equal int 1 2)
  in
  let run flags tests =
    facade ~env:(project [] root) ~flags "promotion" tests
  in
  [
    ( "a correction beside a failure",
      run [ "--corrected" ] [ stale "one"; boom ] );
    ( "two corrections beside a failure",
      run [ "--corrected" ] [ stale "two-a"; stale "two-b"; boom ] );
    ("corrections alone", run [ "--corrected" ] [ stale "alone" ]);
    ( "failures and no correction written",
      run [ "--corrected" ] [ masked; boom ] );
    ("plain checking", run [] [ stale "checked"; boom ]);
    ("-u beside a failure", run [ "-u" ] [ stale "accepted"; boom ]);
  ]

let promoted name = List.assoc name promotion

let one_warning () =
  expect_exact (promoted "a correction beside a failure").err
  @@ __POS_OF__
       {|windtrap: warning: dune registers a correction for promotion only when the run that wrote it exits 0, so the failures above withhold the correction written here. Fix the failures, rerun, then 'dune promote'.
|}

let the_warning_agrees_in_number () =
  let err = (promoted "two corrections beside a failure").err in
  contains ~sub:"withhold the corrections written here" err;
  one_line err

let baselines =
  group "Baselines"
    [
      test "expect compares up to whitespace, expect_exact byte for byte"
        (fun () ->
          equal (list string) [ "stale"; "stale exact" ] (counted literals));
      test "a literal's failure names the verb that read it" (fun () ->
          equal (pair string string) ("expect", "expect_exact")
            (read_as [ "stale" ], read_as [ "stale exact" ]));
      test "a literal's mismatch is located at its __POS_OF__"
        a_literal_is_located_at_its_position;
      test "a relative expect_file path does not follow chdir" (fun () ->
          equal string "pass" (Recorded.row relative_file [ "moved" ]));
      test "a baseline in a bracket's body is accepted like any other"
        (fun () ->
          equal
            (pair int (list string))
            ( 0,
              [
                strf "wrote %s/src/bracketed.expected, 0"
                  (Windtrap_test_support.slashed bracket_root);
              ] )
            (Recorded.exit_code bracketed, written bracketed));
      cases "a correction dune will not promote is the one run that warns"
        ~name:(fun (name, _, _) -> name)
        [
          ("a correction beside a failure", 1, true);
          ("two corrections beside a failure", 1, true);
          ("corrections alone", 0, false);
          ("failures and no correction written", 1, false);
          ("plain checking", 1, false);
          ("-u beside a failure", 1, false);
        ]
        (fun (name, code, warns) ->
          let r = promoted name in
          equal (pair int bool) (code, warns) (r.code, r.err <> ""));
      test "the warning is one line on stderr, whatever the report offered"
        one_warning;
      test "the warning agrees in number" the_warning_agrees_in_number;
      test "the warning leaves the summary the report's last line" (fun () ->
          let r = promoted "a correction beside a failure" in
          starts_with ~affix:"2 failed, 1 correction written in "
            (last_line r.out));
      test "-u beside a failure accepts in place" (fun () ->
          contains ~sub:"1 correction accepted in "
            (promoted "-u beside a failure").out);
    ]

(* Captured output *)

let outputs, read_back =
  let seen = ref [] in
  let note () = seen := output () :: !seen in
  let r =
    Recorded.execute
      [
        test "reads" (fun () ->
            print_string "hello\n";
            note ();
            note ();
            print_string "more";
            note ());
      ]
  in
  (r, List.rev !seen)

let streamed =
  Recorded.execute
    ~config:(fun c -> { c with stream = true })
    [ test "streams" (fun () -> ignore (output ())) ]

let captured_output =
  group "Captured output"
    [
      test "output is what the test wrote since the previous call" (fun () ->
          equal (list string) [ "hello\n"; ""; "more" ] read_back;
          equal string "pass" (Recorded.row outputs [ "reads" ]));
      test "output fails the test under --stream" (fun () ->
          contains ~sub:"--stream" (message (failure streamed [ "streams" ])));
    ]

(* The running test *)

let outside_a_test =
  [
    ("output", escape output);
    ("expect", escape (fun () -> expect "x" (__POS__, "x")));
    ("expect_exact", escape (fun () -> expect_exact "x" (__POS__, "x")));
    ("expect_file", escape (fun () -> expect_file "x" "x.expected"));
    ("collect", escape (fun () -> collect "label"));
    ("classify", escape (fun () -> classify "label" true));
    ("cover", escape (fun () -> cover "label" true));
  ]

let home = Sys.getcwd ()

(* The variable, the working directory and whether the body ran, after
   setenv, chdir and subtest were called outside a test. *)
let untouched_outside_a_test =
  let ran = ref false in
  ignore (escape (fun () -> setenv "WINDTRAP_TEST_OUTSIDE" (Some "x")));
  ignore (escape (fun () -> chdir (Filename.get_temp_dir_name ())));
  ignore (escape (fun () -> subtest "sub" (fun () -> ran := true)));
  (Sys.getenv_opt "WINDTRAP_TEST_OUTSIDE", Sys.getcwd (), !ran)

let current_paths =
  let seen = ref [] in
  let note () = seen := current_test () :: !seen in
  ignore
    (Recorded.execute
       [
         test ~retries:1 "t" (fun () ->
             note ();
             subtest "s" note;
             if List.length !seen = 2 then fail "first attempt");
       ]);
  List.rev !seen

let release_scratch =
  let wants_scratch =
    fixture ~teardown:(fun () -> ignore (temp_dir ())) ignore
  in
  Recorded.execute [ test "touch" wants_scratch ]

let phase = function
  | Failure.Setup -> "setup"
  | Failure.Body -> "body"
  | Failure.Teardown -> "teardown"
  | Failure.Release -> "release"

let an_operation_fails_a_release () =
  let failures = (Recorded.outcome release_scratch).release_failures in
  equal
    (pair (list string) int)
    ([ "release" ], 1)
    ( List.map (fun (f : Failure.t) -> phase f.phase) failures,
      Recorded.exit_code release_scratch )

let the_running_test =
  group "The running test"
    [
      cases "raises Invalid_argument outside a test" ~name:fst outside_a_test
        (fun (_, e) ->
          raises_match (Exn.invalid_arg ~substring:"no test is running")
            (fun () -> replay e));
      test "outside a test setenv, chdir and subtest change nothing" (fun () ->
          equal
            (triple (option string) string bool)
            (None, home, false) untouched_outside_a_test);
      test "current_test is the same in every attempt and in a subtest"
        (fun () ->
          equal
            (list (list string))
            [ [ "t" ]; [ "t" ]; [ "t" ]; [ "t" ] ]
            current_paths);
      test "an operation on the running test fails a fixture's release"
        an_operation_fails_a_release;
    ]

(* Running *)

let help = facade ~flags:[ "--help" ] "codes" []
let version = facade ~flags:[ "--version" ] "codes" []
let unknown_flag = facade ~flags:[ "--nosuchflag" ] "codes" []
let bad_value = facade ~flags:[ "--timeout"; "x" ] "codes" []
let green = facade "codes" [ passes ]
let red = facade "codes" [ passes; boom ]
let second = facade "second" [ passes ]

let codes =
  [
    ("--help", 0, help);
    ("--version", 0, version);
    ("a parse error", 2, unknown_flag);
    ("a resolution error", 2, bad_value);
    ("a green suite", 0, green);
    ("a failing suite", 1, red);
    ("a second suite in the same process", 0, second);
  ]

let nested_run () =
  raises_match Exn.invalid_arg (fun () -> run ~argv:[| "nested" |] "nested" [])

let usage_errors =
  [
    ("a parse error", unknown_flag, "unknown option '--nosuchflag'");
    ("a resolution error", bad_value, "invalid value 'x' for");
  ]

let listed =
  facade ~flags:[ "-l" ] "listsuite"
    [ group "outer" [ test "picked" ignore ]; test "other" ignore ]

let listed_control =
  facade ~flags:[ "-l" ] "listcontrol" [ test "first\nhalf" ignore ]

let listed_nothing =
  facade ~flags:[ "-l"; "-f"; "zzznope" ] "listsuite"
    [ group "outer" [ test "picked" ignore ]; test "other" ignore ]

let emptied_pair =
  facade ~flags:[ "-f"; "zzznope" ] "emptysuite" [ passes; boom ]

(* A selection that the mirrors alone give reaches every stanza of a project. *)
let filter_mirror = [ ("WINDTRAP_FILTER", "zzznope") ]
let typed = facade ~flags:[ "-f"; "zzznope" ] "emptied" [ passes ]

let typed_corrected =
  facade ~flags:[ "-f"; "zzznope"; "--corrected" ] "emptied" [ passes ]

let mirrored = facade ~env:filter_mirror "emptied" [ passes ]

let mirrored_corrected =
  facade ~env:filter_mirror ~flags:[ "--corrected" ] "emptied" [ passes ]

let mirrored_typed =
  facade ~env:filter_mirror ~flags:[ "-f"; "zzznope" ] "emptied" [ passes ]

let mirrored_usage =
  facade ~env:filter_mirror
    ~flags:[ "--corrected"; "--nosuchflag" ]
    "emptied" [ passes ]

let mirrored_empty = facade ~env:filter_mirror "emptied" []

(* A run whose argv.(0) is empty, and one without argv. *)
let fixed_flags logs = [ "-o"; logs; "--color"; "never" ]

let blank_usage =
  run_argv
    (Array.of_list (("" :: fixed_flags (logs ())) @ [ "--nosuchflag" ]))
    "blank" [ passes ]

let blank_typed =
  run_argv
    (Array.of_list (("" :: fixed_flags (logs ())) @ [ "-f"; "zzznope" ]))
    "emptied" [ passes ]

let blank_mirrored =
  run_argv
    ~env:
      ([ ("WINDTRAP_OUTPUT", logs ()); ("WINDTRAP_COLOR", "never") ]
      @ filter_mirror)
    [||] "emptied" [ passes ]

let selection_codes =
  [
    ("a typed filter", 2, typed);
    ("a typed filter under --corrected", 2, typed_corrected);
    ("a mirror's filter", 0, mirrored);
    ("a mirror's filter under --corrected", 0, mirrored_corrected);
    ("a typed filter beside the mirror", 2, mirrored_typed);
    ("a usage error beside the mirror", 2, mirrored_usage);
    ("a suite that declares no tests, beside the mirror", 2, mirrored_empty);
    ("a typed filter with an empty argv.(0)", 2, blank_typed);
    ("a mirror's filter with no argv", 0, blank_mirrored);
  ]

let printing =
  let logs = logs () in
  let tests = [ passes; boom ] in
  let junit = Filename.concat (Filename.dirname logs) "junit.xml" in
  List.map
    (fun (name, flags) -> (name, (facade ~logs ~flags "printing" tests).code))
    [
      ("no flag", []);
      ("-v", [ "-v" ]);
      ("--color always", [ "--color"; "always" ]);
      ("--slow-threshold 0", [ "--slow-threshold"; "0" ]);
      ("-s", [ "-s" ]);
      ("--junit", [ "--junit"; junit ]);
    ]

(* The last failed tests under --corrected and -x *)

let kept_stops, kept_entered =
  let root = Scratch.dir "windtrap-kept-" and logs = logs () in
  let tests =
    [
      test "corrects" (fun () -> expect_file "new\n" "c.expected");
      test "later" ignore;
    ]
  in
  let env = project [] root in
  let stopped = facade ~env ~logs ~flags:[ "--corrected"; "-x" ] "kept" tests in
  (stopped, facade ~env ~logs ~flags:[ "-l"; "--failed" ] "kept" tests)

let bailed_entries =
  let logs = logs () in
  let tests =
    [ test "a" (fun () -> equal int 1 2); test "b" (fun () -> equal int 1 2) ]
  in
  ignore (facade ~logs "bailed" tests);
  ignore (facade ~logs ~flags:[ "-x" ] "bailed" tests);
  facade ~logs ~flags:[ "-l"; "--failed" ] "bailed" tests

let version_line () =
  let out = version.out in
  starts_with ~affix:"windtrap " out;
  one_line out;
  not_contains ~sub:"%" out

let running =
  group "Running"
    [
      cases "run returns the exit code"
        ~name:(fun (name, _, _) -> name)
        codes
        (fun (_, code, r) -> equal int code r.code);
      test "run raises Invalid_argument while a run is executing" nested_run;
      test "--help prints the page on stdout, and nothing on stderr" (fun () ->
          contains ~sub:"usage: codes [OPTIONS] [PATTERN...]" help.out;
          equal string "" help.err);
      test "--version prints one line that names the version" version_line;
      cases "a usage error prints nothing on stdout and names the fault"
        ~name:(fun (name, _, _) -> name)
        usage_errors
        (fun (_, r, fault) ->
          equal string "" r.out;
          contains ~sub:fault r.err);
      test "the report is complete on stdout when run returns" (fun () ->
          starts_with ~affix:"codes: 1 passed in " green.out;
          equal string "" green.err);
      test "a failing suite's report holds its failure" (fun () ->
          contains ~sub:"  FAIL  boom" red.out);
      test "a second suite in the same process prints its own report" (fun () ->
          starts_with ~affix:"second: 1 passed in " second.out);
      test "-l prints the selection alone, in declaration order" (fun () ->
          equal (triple int text string)
            (0, "outer › picked\nother\n", "")
            (listed.code, listed.out, listed.err));
      test "-l prints a path on one line, its control bytes escaped" (fun () ->
          equal text "first\\x0ahalf\n" listed_control.out);
      test "an empty listing returns 0 and says why on stderr" (fun () ->
          equal (pair int text) (0, "") (listed_nothing.code, listed_nothing.out);
          expect_exact listed_nothing.err
          @@ __POS_OF__
               {|windtrap: no tests ran: filter "zzznope" matched none of 2 tests.
|});
      test "an empty selection says why, then how to list the suite" (fun () ->
          equal (pair int string) (2, "") (emptied_pair.code, emptied_pair.err);
          expect_exact emptied_pair.out
          @@ __POS_OF__
               {|emptysuite: no tests ran: filter "zzznope" matched none of 2 tests.
list: emptysuite -l
|});
      cases "the code of a selection that keeps no test"
        ~name:(fun (name, _, _) -> name)
        selection_codes
        (fun (_, code, r) -> equal int code r.code);
      test "a mirror's emptied selection still says why" (fun () ->
          equal text typed.out mirrored.out);
      test "under --corrected the way out names the flag" (fun () ->
          expect_exact mirrored_corrected.out
          @@ __POS_OF__
               {|emptied: no tests ran: filter "zzznope" matched none of 1 test.
(list the suite's tests with -l)
|});
      test "without argv.(0) the way out names the flag too" (fun () ->
          equal text mirrored_corrected.out blank_typed.out;
          equal text mirrored_corrected.out blank_mirrored.out);
      test "a usage error beside the mirror names the option" (fun () ->
          contains ~sub:"unknown option '--nosuchflag'" mirrored_usage.err);
      test "an empty argv.(0) leaves the suite's name to the usage line"
        (fun () ->
          equal int 2 blank_usage.code;
          contains ~sub:"usage: blank [OPTIONS] [PATTERN...]" blank_usage.err);
      cases "flags that change what prints change no exit code" ~name:fst
        printing (fun (_, code) -> equal int 1 code);
      test "a test of kept corrections still stops -x" (fun () ->
          contains ~sub:"1 not run" kept_stops.out);
      test "a test of kept corrections enters the last failed tests" (fun () ->
          equal text "corrects\n" kept_entered.out);
      test "a test that -x left unexecuted keeps its entry" (fun () ->
          equal text "a\nb\n" bailed_entries.out);
    ]

(* The process *)

(* A signalled run is a forked child that runs a suite and is signalled by
   this process: only a process that received a signal can show what the
   runner does with it. The children are all started first, and each is then
   signalled in turn, so their waits overlap. *)

type started = { dir : string; pid : int }
type signalled = { status : string; out : string; err : string; root : string }

let signal_name s =
  let names =
    [
      (Sys.sighup, "SIGHUP");
      (Sys.sigint, "SIGINT");
      (Sys.sigterm, "SIGTERM");
      (Sys.sigkill, "SIGKILL");
    ]
  in
  Option.value (List.assoc_opt s names) ~default:(string_of_int s)

let status = function
  | Unix.WEXITED code -> strf "exit %d" code
  | Unix.WSIGNALED s -> "killed by " ^ signal_name s
  | Unix.WSTOPPED s -> "stopped by " ^ signal_name s

let under root name = Filename.concat root name

(* The child starts from the stated environment, with the three dispositions
   at their default, and makes its temporary directories under [dir]. *)
let start ?(ignored = []) suite =
  if Sys.win32 then None
  else begin
    let dir = Scratch.dir "windtrap-signalled-" in
    flush_all ();
    match Unix.fork () with
    | 0 ->
        List.iter (fun name -> Os.setenv name None) (stated_names []);
        List.iter
          (fun s -> Sys.set_signal s Sys.Signal_default)
          [ Sys.sigint; Sys.sigterm; Sys.sighup ];
        List.iter (fun s -> Sys.set_signal s Sys.Signal_ignore) ignored;
        Filename.set_temp_dir_name dir;
        redirect (under dir "out") Unix.stdout;
        redirect (under dir "err") Unix.stderr;
        exit (suite dir)
    | pid -> Some { dir; pid }
  end

let signalled ~signal_it = function
  | None -> None
  | Some { dir; pid } ->
      signal_it dir pid;
      let _, ended = Unix.waitpid [] pid in
      Some
        {
          status = status ended;
          out = read (under dir "out");
          err = read (under dir "err");
          root = dir;
        }

let posix = function Some v -> v | None -> skip ~reason:"POSIX only" ()

(* Sends [signal] once [file] exists under [root]; a child that never gets
   there is killed. *)
let once ?(file = "ready") root signal pid =
  let rec await tries =
    if Sys.file_exists (under root file) then Unix.kill pid signal
    else if tries = 0 then Unix.kill pid Sys.sigkill
    else begin
      Unix.sleepf 0.01;
      await (tries - 1)
    end
  in
  await 2000

let child_argv root flags =
  Array.of_list
    ("signal-child" :: "-o" :: under root "logs" :: "--color" :: "never"
   :: flags)

let waits root () =
  print_string "captured, never shown\n";
  ignore (temp_dir ());
  touch (under root "ready");
  Unix.sleepf 60.

let four root =
  [
    passes;
    test "fails" (fun () -> equal int 1 2);
    group "deep" [ test "waits" (waits root) ];
    test "never reached" ignore;
  ]

let waiting =
  List.map
    (fun signal ->
      ( signal,
        start (fun root -> run ~argv:(child_argv root []) "signals" (four root))
      ))
    [ Sys.sigint; Sys.sigterm; Sys.sighup ]

(* It raises on the Interrupted event, which the runner ignores. *)
let between_tests =
  start (fun root ->
      let signal_self = function
        | Run.Test_finished _ -> Unix.kill (Unix.getpid ()) Sys.sigterm
        | Run.Interrupted _ -> raise Exit
        | Run.Run_started _ | Run.Test_started _ | Run.Fixture_release _ -> ()
      in
      let config =
        {
          (Run.default_config ()) with
          log_dir = under root "logs";
          color = Os.Never;
          exclude = [ "fails" ];
        }
      in
      ignore
        (Report.run ~on_event:signal_self ~suite:"signals" config (four root));
      3)

let in_a_release =
  start (fun root ->
      let released name =
        fixture ~teardown:(fun () -> touch (under root name)) ignore
      in
      let first = released "first released"
      and second = released "second released" in
      let hangs =
        fixture
          ~teardown:(fun () ->
            touch (under root "ready");
            Unix.sleepf 60.)
          ignore
      in
      run ~argv:(child_argv root []) "signals"
        [
          test "acquires" (fun () ->
              first ();
              second ();
              hangs ());
        ])

let ignoring_hup =
  start ~ignored:[ Sys.sighup ] (fun root ->
      run ~argv:(child_argv root []) "signals" (four root))

(* The first test records a correction and the second fails; the third
   binds a variable inside a bracket and waits. The fixture's release reads
   the variable. *)
let leaving =
  start (fun root ->
      Os.setenv "WINDTRAP_PROJECT_ROOT" (Some root);
      let reads_env =
        fixture
          ~teardown:(fun () ->
            write
              (under root "env at release")
              (Option.value ~default:"<unset>" (Sys.getenv_opt "WINDTRAP_LEFT")))
          ignore
      in
      run
        ~argv:(child_argv root [ "--corrected" ])
        "signals"
        [
          test "corrects" (fun () ->
              reads_env ();
              expect_file "new\n" "c.expected");
          test "fails" (fun () -> equal int 1 2);
          bracket ~setup:ignore
            ~teardown:(fun () -> touch (under root "teardown ran"))
            "waits"
            (fun () ->
              setenv "WINDTRAP_LEFT" (Some "set by the test");
              waits root ());
        ])

let lingering =
  start (fun root ->
      let lingers =
        fixture
          ~teardown:(fun () ->
            touch (under root "releasing");
            Unix.sleepf 60.)
          ignore
      in
      run ~argv:(child_argv root []) "signals"
        [
          test "holds and waits" (fun () ->
              lingers ();
              waits root ());
        ])

let interrupted =
  List.map
    (fun (signal, child) ->
      ( signal_name signal,
        signalled child ~signal_it:(fun root -> once root signal) ))
    waiting

let between = signalled between_tests ~signal_it:(fun _ _ -> ())

let releasing =
  signalled in_a_release ~signal_it:(fun root -> once root Sys.sigterm)

let ignored_hup, survived =
  let survived = ref false in
  let r =
    signalled ignoring_hup ~signal_it:(fun root pid ->
        once root Sys.sighup pid;
        Unix.sleepf 0.2;
        survived := fst (Unix.waitpid [ Unix.WNOHANG ] pid) = 0;
        Unix.kill pid Sys.sigterm)
  in
  (r, !survived)

let leftovers = signalled leaving ~signal_it:(fun root -> once root Sys.sigterm)

let second_signal =
  signalled lingering ~signal_it:(fun root pid ->
      once root Sys.sigterm pid;
      once ~file:"releasing" root Sys.sigint pid)

let forks, reaped =
  let reaped = ref "not reaped" in
  let kills () =
    match Unix.fork () with
    | 0 ->
        Unix.sleepf 60.;
        Unix._exit 0
    | pid ->
        Unix.sleepf 0.05;
        Unix.kill pid Sys.sigterm;
        reaped := status (snd (Unix.waitpid [] pid))
  in
  if Sys.win32 then (None, !reaped)
  else
    let r = facade "forks" [ test "kills the process it forked" kills ] in
    (Some r, !reaped)

(* Whether each of the three handlers is still this process's own after a run. *)
let handlers_kept =
  if Sys.win32 then None
  else begin
    let mine (_ : int) = () in
    let signals = [ Sys.sigint; Sys.sigterm; Sys.sighup ] in
    let before =
      List.map (fun s -> Sys.signal s (Sys.Signal_handle mine)) signals
    in
    ignore (facade "handlers" [ passes ]);
    Some
      (List.map2
         (fun s previous ->
           match Sys.signal s previous with
           | Sys.Signal_handle f -> f == mine
           | Sys.Signal_default | Sys.Signal_ignore -> false)
         signals before)
  end

let the_stopped_test_keeps_its_bytes r =
  not_contains ~sub:"captured, never shown" r.out;
  contains ~sub:"captured, never shown"
    (read
       (List.fold_left Filename.concat r.root
          [ "logs"; "signals"; "deep"; "waits.output" ]))

let temporary_directories r =
  List.filter
    (String.starts_with ~prefix:"windtrap-")
    (Array.to_list (Sys.readdir r.root))

let the_failures_so_far_come_first r =
  contains
    ~sub:
      "signals: 4 tests\n\
       ──────────────────────── failures ────────────────────────\n\
      \  FAIL  fails\n"
    r.out;
  contains
    ~sub:
      "    actual    2\n\
       ──────────────────────────────────────────────────────────\n\n\
       1 passed, 1 failed, 2 not run in "
    r.out

let names_the_fixture () =
  let r = posix releasing in
  equal string "killed by SIGTERM" r.status;
  starts_with ~affix:"windtrap: interrupted while releasing fixture (" r.err;
  one_line r.err

let what_a_signal_leaves () =
  let r = posix leftovers in
  equal string "killed by SIGTERM" r.status;
  equal (list string) []
    (List.filter
       (fun name -> Sys.file_exists (under r.root name))
       [ "teardown ran"; "c.expected.corrected"; "logs/signals/.last-failed" ])

let a_forked_process_dies_silently () =
  let r = posix forks in
  equal (triple string int string)
    ("killed by SIGTERM", 0, "")
    (reaped, r.code, r.err);
  starts_with ~affix:"forks: 1 passed in " r.out;
  one_line r.out

let signal_rows fn =
  List.map (fun (name, r) -> (name, fun () -> fn (posix r))) interrupted

let the_process =
  group "The process"
    [
      cases "a signal kills the run by the same signal" ~name:fst interrupted
        (fun (name, r) -> equal string ("killed by " ^ name) (posix r).status);
      cases "stderr is one line naming the stopped test" ~name:fst
        (signal_rows (fun r ->
             equal text "windtrap: interrupted in deep › waits\n" r.err))
        (fun (_, check) -> check ());
      cases "the failures so far are reported under their rule" ~name:fst
        (signal_rows the_failures_so_far_come_first) (fun (_, check) ->
          check ());
      cases "the summary is the last line and counts what did not run" ~name:fst
        (signal_rows (fun r ->
             let last = last_line r.out in
             starts_with ~affix:"1 passed, 1 failed, 2 not run in " last;
             ends_with ~affix:"." last))
        (fun (_, check) -> check ());
      cases "the stopped test's output stays in its log" ~name:fst
        (signal_rows the_stopped_test_keeps_its_bytes) (fun (_, check) ->
          check ());
      cases "the stopped attempt's temporary directory is removed" ~name:fst
        (signal_rows (fun r -> equal (list string) [] (temporary_directories r)))
        (fun (_, check) -> check ());
      test "a signal between tests acts before the next test" (fun () ->
          let r = posix between in
          equal (pair string text)
            ("killed by SIGTERM", "windtrap: interrupted between tests\n")
            (r.status, r.err);
          starts_with ~affix:"signals: 1 passed, 2 not run in " r.out;
          one_line r.out);
      test "a signal in a release names the fixture" names_the_fixture;
      test "a signal in a release comes after every test finished" (fun () ->
          starts_with ~affix:"signals: 1 passed in " (posix releasing).out);
      test "a signal in a release releases the fixtures still held" (fun () ->
          let r = posix releasing in
          equal (list bool) [ true; true ]
            (List.map
               (fun name -> Sys.file_exists (under r.root name))
               [ "first released"; "second released" ]));
      test "a signal the process was started ignoring stays ignored" (fun () ->
          let r = posix ignored_hup in
          is_true survived;
          equal (pair string text)
            ("killed by SIGTERM", "windtrap: interrupted in deep › waits\n")
            (r.status, r.err));
      test "a signal skips the teardown and writes no correction and no store"
        what_a_signal_leaves;
      test "what setenv changed stays for the release" (fun () ->
          let r = posix leftovers in
          equal (option string) (Some "set by the test")
            (contents (under r.root "env at release")));
      test "a second signal kills at once" (fun () ->
          equal string "killed by SIGINT" (posix second_signal).status);
      test "a process a test forked dies by a signal, and the run goes on"
        a_forked_process_dies_silently;
      test "a run puts back the handlers it found" (fun () ->
          equal (list bool) [ true; true; true ] (posix handlers_kept));
    ]

let () =
  exit
    (run "facade"
       [
         types;
         declaring;
         properties;
         stateful_tests;
         baselines;
         captured_output;
         the_running_test;
         running;
         the_process;
       ])
