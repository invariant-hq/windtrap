(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The facade's command line, end to end: suite_main.exe spawned once per
   scenario with a scrubbed environment, asserting on the exit code AND
   the bytes on each stream.

   Why a spawn and not an in-process call: [Windtrap.run] owns the
   process — every branch of it ends in [exit] — so the exit code is the
   only place half of its contract is visible (Law 11's nothing-ran 2,
   the refusals' 1, the informational 0 of --help and -l), and a test
   that could observe it from inside would be testing something else.
   The suite under the flags is test/facade/suite_main.ml, small on
   purpose so these transcripts can be pinned byte for byte; durations
   are the one thing that moves, so the scenarios that run tests assert
   fragments and the rest assert whole streams.

   A windtrap suite (see test/mutate_loop): every subject is a child
   process, so hosting the assertions under the runner nests nothing. *)

open Windtrap
module Instr = Windtrap_instr

let exe_dir = Filename.dirname Sys.executable_name
let suite_exe = Filename.concat exe_dir "suite_main.exe"

(* Scratch *)

let rec remove_tree path =
  match Unix.lstat path with
  | exception Unix.Unix_error _ -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } -> (
      Array.iter
        (fun name -> remove_tree (Filename.concat path name))
        (try Sys.readdir path with Sys_error _ -> [||]);
      try Unix.rmdir path with Unix.Unix_error _ -> ())
  | _ -> ( try Unix.unlink path with Unix.Unix_error _ -> ())

let scratch_dir =
  let dir = Filename.temp_file "windtrap_facade" "" in
  Sys.remove dir;
  Sys.mkdir dir 0o755;
  at_exit (fun () -> remove_tree dir);
  dir

let scratch name = Filename.concat scratch_dir name

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    Sys.mkdir dir 0o755
  end

let write_file path contents =
  mkdir_p (Filename.dirname path);
  let oc = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc contents)

let read_file path =
  match open_in_bin path with
  | exception Sys_error _ -> ""
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () -> really_input_string ic (in_channel_length ic))

(* The snapshot root the children are aimed at: the fixture's one
   baseline, planted where WINDTRAP_PROJECT_ROOT sends the child's
   snapshot layer. A scratch tree rather than this directory's own
   __snapshots__, so no scenario can write into the source tree and the
   path the fixture resolves is stated here rather than discovered. *)
let project_root = scratch "root"

let () =
  write_file
    (Filename.concat project_root
       "test/facade/__snapshots__/suite_main/greeting.snap")
    "hello from the fixture\n"

(* Coverage bookkeeping

   Under `dune build @cover` the fixture links an instrumented core, so
   every spawn measures lib/ — but the runtime files its dump under the
   executable's identity, one path per executable, and the last spawn's
   dump would be the only one left of twenty. Each spawn writes beside
   the others instead, into the directory the merge reads; leftovers of
   a previous run go first, so a scenario that is renamed or deleted
   cannot leave a dump behind for the report to call stale. Without a
   [_build] above us (an installed or hand-built binary) the children
   keep the runtime's own default.

   Off unless this executable itself links an instrumented core, which
   is exactly when the children have something to dump: under a plain
   [dune runtest] the redirection would name files nothing writes, and
   the sweep would delete what an earlier [@cover] run left. *)

let coverage_dir =
  if Windtrap_coverage.is_empty (Windtrap_coverage.snapshot ()) then None
  else
    match Instr.build_root ~path:Sys.executable_name with
    | None -> None
    | Some root ->
        let dir = Filename.concat root "_build/_coverage" in
        let ours name = String.starts_with ~prefix:"windtrap-facade-" name in
        (try mkdir_p dir with Sys_error _ -> ());
        Array.iter
          (fun name ->
            if ours name then
              try Sys.remove (Filename.concat dir name) with Sys_error _ -> ())
          (try Sys.readdir dir with Sys_error _ -> [||]);
        Some dir

(* Spawning *)

type run = { code : int; out : string; err : string }

let counter = ref 0

(* Nothing of the ambient environment survives except what a process
   needs to start. Every windtrap variable the runner reads would
   reshape a transcript pinned below, and the scenario's own bindings
   come first: [getenv] answers with the first match, so a default
   listed ahead of them would silently win. *)
let environment bindings =
  let inherited name =
    match Sys.getenv_opt name with
    | Some value -> [ name ^ "=" ^ value ]
    | None -> []
  in
  let bound name =
    List.exists (String.starts_with ~prefix:(name ^ "=")) bindings
  in
  let defaults =
    [
      "WINDTRAP_COLOR=never";
      "WINDTRAP_SLOW_THRESHOLD=0";
      "WINDTRAP_COVERAGE=off";
      "WINDTRAP_MUTATE=off";
      "WINDTRAP_PROJECT_ROOT=" ^ project_root;
      "FACADE_FIXTURE=default";
    ]
  in
  let kept =
    List.filter
      (fun binding ->
        match String.index_opt binding '=' with
        | None -> true
        | Some i -> not (bound (String.sub binding 0 i)))
      defaults
  in
  Array.of_list
    (List.concat_map inherited [ "PATH"; "HOME"; "TMPDIR"; "LANG"; "LC_ALL" ]
    @ bindings @ kept)

let spawn ?(env = []) args =
  incr counter;
  let n = !counter in
  let out_path = scratch (Printf.sprintf "out-%d" n)
  and err_path = scratch (Printf.sprintf "err-%d" n) in
  let dump =
    match coverage_dir with
    | None -> []
    | Some dir ->
        [
          Printf.sprintf "WINDTRAP_COVERAGE_FILE=%s"
            (Filename.concat dir
               (Printf.sprintf "windtrap-facade-%d.coverage" n));
        ]
  in
  let open_target path =
    Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
  in
  let out = open_target out_path and err = open_target err_path in
  let pid =
    Unix.create_process_env suite_exe
      (Array.of_list (suite_exe :: args))
      (environment (env @ dump))
      Unix.stdin out err
  in
  Unix.close out;
  Unix.close err;
  let code =
    match snd (Unix.waitpid [] pid) with
    | Unix.WEXITED code -> code
    | Unix.WSIGNALED signal -> 128 + signal
    | Unix.WSTOPPED _ -> 255
  in
  { code; out = read_file out_path; err = read_file err_path }

(* Assertions

   A wrong exit code is undiagnosable without the transcript that
   explains it, so the exit-code check carries both streams into its
   message rather than reporting two integers. *)

let exits ?pos ~msg expected r =
  if r.code <> expected then
    failf ?pos
      "%s: expected exit %d, got %d\n--- stdout ---\n%s--- stderr ---\n%s" msg
      expected r.code r.out r.err

let usage = "usage: suite_main.exe [OPTIONS] [PATTERN]\n"

(* The suite declares five tests; the empty-selection sentence counts
   them, and every scenario below that quotes the sentence spells the
   same number. *)
let declared = 5

(* Scenarios *)

let test_help () =
  let r = spawn [ "--help" ] in
  exits ~msg:"--help" 0 r;
  contains ~msg:"the banner names the program"
    ~sub:"suite_main.exe - windtrap test runner\n" r.out;
  contains ~msg:"the usage line is on stdout" ~sub:usage r.out;
  equal ~msg:"--help says nothing on stderr" string "" r.err

let test_version () =
  let r = spawn [ "--version" ] in
  exits ~msg:"--version" 0 r;
  (* The version is the release watermark — "dev" in a working tree, a
     number in a distribution tarball — so the shape is what is pinned. *)
  (match String.split_on_char '\n' r.out with
  | [ line; "" ] ->
      is_true ~msg:"--version names windtrap"
        (String.starts_with ~prefix:"windtrap " line);
      is_true ~msg:"--version names a version after it"
        (String.length line > String.length "windtrap ")
  | _ -> failf "--version printed %S, not one line" r.out);
  equal ~msg:"--version says nothing on stderr" string "" r.err

let test_unknown_flag () =
  let r = spawn [ "--nosuchflag" ] in
  exits ~msg:"an unknown flag" 2 r;
  equal ~msg:"the error and the usage go to stderr" string
    ("unknown option '--nosuchflag'\n" ^ usage)
    r.err;
  equal ~msg:"nothing goes to stdout" string "" r.out

let test_near_miss_flag () =
  let r = spawn [ "--bial"; "1" ] in
  exits ~msg:"a near-miss flag" 2 r;
  equal ~msg:"the error names the flag it is one slip from" string
    ("unknown option '--bial'; did you mean '--bail'?\n" ^ usage)
    r.err

let test_invalid_value () =
  let r = spawn [ "--bail"; "x" ] in
  exits ~msg:"an invalid value" 2 r;
  equal ~msg:"the error names the flag and what it expected" string
    ("invalid value 'x' for --bail: expected a positive integer\n" ^ usage)
    r.err;
  equal ~msg:"nothing goes to stdout" string "" r.out

let test_invalid_mirror_value () =
  (* Under `dune runtest` the mirrors are the command line, so a value a
     flag would refuse must be refused with the same sentence and the
     same code when it arrives through the environment — the resolution
     that finds it is the second of the facade's two error exits. *)
  let r = spawn ~env:[ "WINDTRAP_BAIL=nope" ] [] in
  exits ~msg:"an invalid mirror value" 2 r;
  equal ~msg:"the error names the variable, not a flag" string
    ("invalid value 'nope' for WINDTRAP_BAIL: expected a positive integer\n"
   ^ usage)
    r.err

let test_list_matching () =
  let r = spawn [ "-l"; "-f"; "math" ] in
  exits ~msg:"-l with a matching filter" 0 r;
  equal ~msg:"the listing is the selection, in declaration order" string
    "math \u{203a} adds\nmath \u{203a} subtracts\n" r.out;
  equal ~msg:"a listing says nothing on stderr" string "" r.err

let test_list_empty () =
  let r = spawn [ "-l"; "-f"; "zzznope" ] in
  (* 385fdde: a listing that answered a mistyped filter with silence
     would be the dead end its own "-l" hint leads to. Exit 0 — the
     listing did what it was asked. *)
  exits ~msg:"-l with a filter that matches nothing" 0 r;
  equal ~msg:"the listing says why it is empty" string
    (Printf.sprintf
       "no tests ran: filter \"zzznope\" matched none of %d tests.\n" declared)
    r.out

let test_empty_selection_exits_2 () =
  let r = spawn [ "-f"; "zzznope" ] in
  (* Law 11: a run that ran nothing is not a pass. *)
  exits ~msg:"a filter that matches nothing" 2 r;
  equal ~msg:"the summary names the filter, the count and the way out" string
    (Printf.sprintf
       "fixture: no tests ran: filter \"zzznope\" matched none of %d tests.\n\
        (list the suite's tests with -l)\n"
       declared)
    r.out

let test_failing_selection () =
  let r = spawn [ "-f"; "boom" ] in
  exits ~msg:"a failing selection" 1 r;
  contains ~msg:"the transcript names the failing test" ~sub:"boom" r.out;
  contains ~msg:"and its message" ~sub:"deliberate" r.out;
  contains ~msg:"and the summary counts it" ~sub:"1 failed" r.out

let test_passing_selection () =
  let r = spawn [ "-e"; "boom" ] in
  exits ~msg:"a passing selection" 0 r;
  contains ~msg:"the summary counts every selected test" ~sub:"4 passed" r.out;
  equal ~msg:"a green run says nothing on stderr" string "" r.err

let test_selection_description_parts () =
  let r =
    spawn
      [
        "-l";
        "-f";
        "zzznope";
        "-e";
        "yyy";
        "--tag";
        "a";
        "--tag";
        "b";
        "--exclude-tag";
        "c";
        "--shard";
        "1/3";
      ]
  in
  exits ~msg:"every narrowing flag at once" 0 r;
  equal ~msg:"the sentence names every part, in the order it lists them" string
    (Printf.sprintf
       "no tests ran: filter \"zzznope\", exclusion \"yyy\", tag \"a\", \"b\", \
        excluded tag \"c\" and shard 1/3 matched none of %d tests.\n"
       declared)
    r.out

let test_selection_description_escapes () =
  (* The sentence is meant to be read and retyped, so it escapes for a
     reader: the quote, the backslash, the three named control
     characters and the hex fallback for everything else below space. *)
  let r = spawn [ "-l"; "-f"; "a\"b\\c\nd\te\rf\001g\127h" ] in
  exits ~msg:"a filter full of control characters" 0 r;
  equal ~msg:"the sentence escapes what the reader typed" string
    (Printf.sprintf
       "no tests ran: filter \"a\\\"b\\\\c\\nd\\te\\rf\\x01g\\x7fh\" matched \
        none of %d tests.\n"
       declared)
    r.out

let test_selection_description_failed () =
  (* --failed is the one part with a prerequisite: the store the last run
     left under -o. Two spawns, and the second is where the flag joins
     the sentence. *)
  let store = scratch "store" in
  let first = spawn [ "-f"; "boom"; "-o"; store ] in
  exits ~msg:"the recording run" 1 first;
  let r = spawn [ "-l"; "-o"; store; "--failed"; "--tag"; "zzznope" ] in
  exits ~msg:"--failed narrowed to nothing" 0 r;
  equal ~msg:"the sentence names --failed by its flag" string
    (Printf.sprintf
       "no tests ran: tag \"zzznope\" and --failed matched none of %d tests.\n"
       declared)
    r.out

let test_list_startup_error () =
  (* A listing makes the startup checks a real run makes, and reports
     them the way a real run does: the message on stderr and the check's
     own exit code, not the listing's 0. *)
  let store = scratch "empty-store" in
  mkdir_p store;
  let r = spawn [ "-l"; "-o"; store; "--failed" ] in
  exits ~msg:"-l over a --failed store with nothing in it" 2 r;
  equal ~msg:"the startup error is on stderr" string
    "no recorded failures match the current suite\n" r.err;
  equal ~msg:"and no listing on stdout" string "" r.out

let test_duplicate_paths () =
  let r = spawn ~env:[ "FACADE_FIXTURE=duplicate" ] [] in
  exits ~msg:"a suite with two tests at one path" 1 r;
  equal ~msg:"the refusal names the path and the rule" string
    "duplicate test paths:\n  dup \u{203a} twice\nEvery full test path must be \
     unique.\n"
    r.err;
  equal ~msg:"nothing ran, so nothing was reported" string "" r.out

let test_focus_warns_outside_ci () =
  let r = spawn ~env:[ "FACADE_FIXTURE=focus" ] [] in
  exits ~msg:"a green focused run" 0 r;
  contains ~msg:"the warning counts what focus dropped"
    ~sub:"warning: focus is active (ftest/fgroup) — 1 of 2 tests ran" r.err;
  contains ~msg:"and says what to do about it"
    ~sub:"remove the focus before committing" r.err

let test_focus_refused_in_ci () =
  let r = spawn ~env:[ "FACADE_FIXTURE=focus"; "CI=1" ] [] in
  exits ~msg:"a focused run under CI" 1 r;
  contains ~msg:"the refusal names the focus site"
    ~sub:"focused tests committed (ftest at " r.err;
  contains ~msg:"and the remedy" ~sub:"); remove ftest/fgroup to run under CI"
    r.err

let test_junit_file () =
  let path = scratch "report.xml" in
  let r = spawn [ "-f"; "math"; "--junit"; path ] in
  exits ~msg:"a green run with --junit" 0 r;
  is_true ~msg:"the report is where --junit said" (Sys.file_exists path);
  (* The document is Render_junit's, pinned in test_render_junit.ml; what
     this scenario owns is that the driver wrote it, for this suite. *)
  contains ~msg:"the report names the suite" ~sub:"fixture" (read_file path)

let test_junit_directory () =
  let dir = scratch "reports" in
  let r = spawn [ "-f"; "math"; "--junit"; dir ] in
  exits ~msg:"a green run with --junit naming a directory" 0 r;
  is_true ~msg:"the directory form writes one report per suite"
    (Sys.file_exists (Filename.concat dir "fixture.xml"));
  contains ~msg:"the report names the suite" ~sub:"fixture"
    (read_file (Filename.concat dir "fixture.xml"))

let test_junit_unwritable_file () =
  let blocked = scratch "blocked" in
  write_file blocked "not a directory\n";
  let r = spawn [ "-f"; "math"; "--junit"; Filename.concat blocked "r.xml" ] in
  (* The report is a side product: failing to write it warns, and the
     run still exits by what the tests did. *)
  exits ~msg:"a green run whose --junit target is unwritable" 0 r;
  contains ~msg:"the warning says the report was not written"
    ~sub:"warning: could not write JUnit report: " r.err;
  contains ~msg:"the run still reported its own outcome" ~sub:"2 passed" r.out

let test_junit_unwritable_directory () =
  let blocked = scratch "blocked" in
  write_file blocked "not a directory\n";
  let r = spawn [ "-f"; "boom"; "--junit"; Filename.concat blocked "out" ] in
  exits ~msg:"a failing run whose --junit directory cannot be made" 1 r;
  contains ~msg:"the warning names the path it could not write"
    ~sub:"warning: could not write JUnit report to " r.err;
  contains ~msg:"and why" ~sub:"Not a directory" r.err

let () =
  run "facade-cli"
    [
      test "--help prints the usage banner and exits 0" test_help;
      test "--version prints one windtrap line and exits 0" test_version;
      test "an unknown flag exits 2 with the error and the usage on stderr"
        test_unknown_flag;
      test "a near-miss flag names the flag it is one slip from"
        test_near_miss_flag;
      test "an invalid value names its flag and exits 2" test_invalid_value;
      test "an invalid mirror value names its variable and exits 2"
        test_invalid_mirror_value;
      test "-l with a matching filter lists the selection and exits 0"
        test_list_matching;
      test "-l with a filter that matches nothing says so and exits 0"
        test_list_empty;
      test "a filter that matches nothing exits 2 with the way out"
        test_empty_selection_exits_2;
      test "a failing selection exits 1" test_failing_selection;
      test "a passing selection exits 0" test_passing_selection;
      test "the empty-selection sentence names every part of the selection"
        test_selection_description_parts;
      test "the empty-selection sentence escapes what the reader typed"
        test_selection_description_escapes;
      test "--failed joins the selection sentence by its flag"
        test_selection_description_failed;
      test "-l reports a startup error by the error's own exit code"
        test_list_startup_error;
      test "duplicate test paths refuse the run before it starts"
        test_duplicate_paths;
      test "a green focused run warns outside CI and still exits 0"
        test_focus_warns_outside_ci;
      test "a focused suite refuses to start under CI" test_focus_refused_in_ci;
      test "--junit writes the report where it was told" test_junit_file;
      test "--junit naming a directory writes the suite's own report there"
        test_junit_directory;
      test "an unwritable --junit file warns and leaves the exit code alone"
        test_junit_unwritable_file;
      test
        "an unmakeable --junit directory warns and leaves the exit code alone"
        test_junit_unwritable_directory;
    ]
