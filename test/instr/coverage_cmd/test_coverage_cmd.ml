(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for coverage's reporting surface: a real windtrap run over an
   instrumented-like executable, which prints no number of its own (the
   dump is the report, and the exit codes hold), the at_exit dump
   feeding the reporting command (under a build directory and, for a
   tree built without one, under _windtrap) and `windtrap coverage` end
   to end (walk-up discovery, merge across two executables, the
   orphan/stale matrix, --min matrix, --json shape, --show-uncovered,
   loud failures). A windtrap suite ([run] executes tests sequentially
   in declaration order); every subject under test is a spawned child
   process, so hosting the assertions under the windtrap runner nests
   nothing.

   The one thing not reproducible here: the freshness of the blessed
   @self-cover rule itself ((alias_rec runtest) + (universe)) is dune
   semantics; reproducing it needs a nested `dune build` inside this
   dune-run test, which would contend for the workspace lock. What this
   file covers instead is everything the rule's action does:
   discovery from a rule-like cwd, the staleness pass over the dumps the
   alias cannot see, the merge, and the --min gate. *)

open Windtrap
module C = Windtrap_runtime.Coverage
module I = Windtrap_runtime.Instr
module Child = Windtrap_test_support.Child
module Scratch = Windtrap_test_support.Scratch

(* Scratch and process helpers *)

(* Hermeticity: absolute paths throughout, so the test behaves the same
   under dune's sandbox and by hand; each test's scratch lives in its own
   temp_dir. *)
let exe_dir = Filename.dirname Sys.executable_name
let child_exe = Filename.concat exe_dir "inline_child.exe"

let windtrap_exe =
  Filename.concat exe_dir
    (Filename.concat ".."
       (Filename.concat ".." (Filename.concat ".." "bin/main.exe")))

(* [scratch name] is a path named [name] in a directory of the test's
   own; nothing exists there yet. *)
let scratch name = Filename.concat (temp_dir ()) name

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    Sys.mkdir dir 0o755
  end

let write_file path contents =
  mkdir_p (Filename.dirname path);
  Out_channel.with_open_bin path (fun oc -> output_string oc contents)

let read_file path = In_channel.with_open_bin path In_channel.input_all

(* [capture ?env ?cwd exe args] runs [exe] and returns (exit code,
   stdout, stderr). The child's environment is stated, never inherited:
   nothing of this process's survives but what a process needs to start,
   colour is off, and [env] adds the scenario's own bindings. The
   children link the core, and the tree-wide mutation run hands this
   suite WINDTRAP_MUTATE=1: a child that inherited it would run the loop
   in place of its scenario. Every inline_child run must carry
   WINDTRAP_COVERAGE_FILE so its at_exit dump lands in scratch, never in
   the real _build. *)
let capture ?(env = []) ?cwd exe args =
  let r = Child.run ?cwd ~env exe args in
  (Child.exit_code r, r.Child.out, r.Child.err)

(* The run prints no number; the dump is the report *)

(* Each child dumps into its own fresh directory, so a run never reads
   or overwrites another's data. The child links the windtrap core,
   which under --instrument-with is itself instrumented and carries
   thousands of points; the dump is deliberately left whole (it is what
   `windtrap coverage` merges), and the assertions that read it scope
   themselves to one file's report. *)
let child ?(env = []) ?(args = []) () =
  let dump = scratch "self.coverage" in
  let code, out, err =
    capture ~env:(("WINDTRAP_COVERAGE_FILE", dump) :: env) child_exe args
  in
  (code, out, err, dump)

(* A dump read back for assertions: the report of the file the test
   planted, for the same reason the run is, with the dump's identity.
   [None] when the dump does not load or does not hold the file. *)
let dump_of ?source_roots ?(only = "lib/fake.ml") path =
  match C.load path with
  | Error _ -> None
  | Ok (t, id) -> (
      match
        List.filter
          (fun (r : C.file_report) -> r.C.file = only)
          (C.file_reports ?source_roots t)
      with
      | [ r ] -> Some (r, id)
      | _ -> None)

(* Six lines of nine characters: block [i] is line [i + 1]'s text. Four
   of six blocks visited leaves lines 5-6 uncovered, the shape the
   reporting command renders from this run's dump. Written once, read by
   every run that registers it. *)
let child_source =
  "line1----\nline2----\nline3----\nline4----\nline5----\nline6----\n"

let child_src_path =
  let path = Filename.concat (Scratch.dir "windtrap-coverage-cmd") "src.ml" in
  write_file path child_source;
  path

let child_src_env =
  [
    ("CHILD_FILE", child_src_path);
    ("CHILD_TOTAL", "6");
    ("CHILD_VISITED", "4");
    ("CHILD_LINE_LEN", "10");
  ]

let dump_is_the_report =
  test "the run prints no number, and the dump is the report" @@ fun () ->
  let code, out, _, dump =
    child ~env:child_src_env ~args:[ "--color"; "never" ] ()
  in
  equal ~msg:"an instrumented child exits 0" int 0 code;
  not_contains ~msg:"the run prints no coverage line" ~sub:"coverage:" out;
  not_contains ~msg:"and draws no per-file table" ~sub:"uncovered lines" out;
  (* The at_exit dump of the same run is what carries the measurement,
     and names the executable that wrote it, so `windtrap coverage` can
     merge and vet it. *)
  (match dump_of ~only:child_src_path dump with
  | Some (r, exe) ->
      let s = r.C.summary in
      equal ~msg:"the dump holds what the run measured" (pair int int) (4, 6)
        (s.C.visited, s.C.total);
      is_true ~msg:"the dump records the child executable's identity"
        (exe
        = Some
            {
              C.exe = I.exe_identity ~exe:child_exe;
              digest = Digest.to_hex (Digest.file child_exe);
            })
  | None -> fail "the child's dump does not load");
  (* Coverage never changes outcomes or exit codes. *)
  let code, _, _, dump =
    child
      ~env:(("CHILD_FAIL", "1") :: child_src_env)
      ~args:[ "--color"; "never" ] ()
  in
  equal ~msg:"a failing instrumented run still exits 1" int 1 code;
  (match dump_of ~only:child_src_path dump with
  | Some (r, _) -> equal ~msg:"and still dumps" int 6 r.C.summary.C.total
  | None -> fail "the failing run's dump does not load");
  (* An uninstrumented child registers nothing: the file may exist
     anyway, because under `--instrument-with` the windtrap core this
     child links registers and dumps. What must be true either way is
     that the child contributed nothing to it. *)
  let code, _, _, dump =
    child ~env:[ ("CHILD_TOTAL", "0") ] ~args:[ "--color"; "never" ] ()
  in
  equal ~msg:"an uninstrumented child exits 0" int 0 code;
  is_true ~msg:"an uninstrumented run contributes nothing to the dump"
    (dump_of dump = None);
  (* The retired knobs are gone: an unknown option and an unlisted
     variable, never silently ignored ones. *)
  let code, _, err, _ = child ~args:[ "--coverage"; "report" ] () in
  equal ~msg:"--coverage is no longer an option" int 2 code;
  contains ~msg:"--coverage is reported as unknown"
    ~sub:"unknown option '--coverage'" err;
  let code, out, _, _ = child ~args:[ "--help" ] () in
  equal ~msg:"--help exits 0" int 0 code;
  not_contains ~msg:"--help lists no coverage flag" ~sub:"--coverage" out;
  contains ~msg:"--help lists the dump override" ~sub:"WINDTRAP_COVERAGE_FILE"
    out

(* A fake merged project for `windtrap coverage` *)

let foo_points =
  [|
    { C.start_ofs = 0; end_ofs = 9 };
    { C.start_ofs = 10; end_ofs = 19 };
    { C.start_ofs = 20; end_ofs = 29 };
  |]

let bar_points =
  [| { C.start_ofs = 0; end_ofs = 9 }; { C.start_ofs = 10; end_ofs = 19 } |]

(* A dump written by hand, in the grammar of the runtime's own (pinned by
   test/instr/coverage): the header that [Instr.add_header] writes, the
   file count, then for each file [len name], its point count and one
   [start end count] line per point. *)
let collection ?identity adds =
  let b = Buffer.create 256 in
  I.add_header C.format b identity;
  Printf.bprintf b "%d\n" (List.length adds);
  List.iter
    (fun (file, (points : C.point array), counts) ->
      Printf.bprintf b "%d %s\n%d\n" (String.length file) file
        (Array.length points);
      Array.iteri
        (fun i (p : C.point) ->
          Printf.bprintf b "%d %d %d\n" p.C.start_ofs p.C.end_ofs counts.(i))
        points)
    adds;
  Buffer.contents b

(* Two executables' worth of data: foo.ml visited [1;0;0] in one and
   [0;1;0] in the other (merge must add to 2/3, uncovered line 3);
   bar.ml only in the second (1/2, uncovered line 2). Total 3/5 = 60%.
   Each test that reads it plants its own copy, so what one test adds to
   the tree no other test sees. *)
let proj () =
  let root = scratch "proj" in
  write_file
    (Filename.concat root "lib/foo.ml")
    "let a = 1\nlet b = 2\nlet c = 3\n";
  write_file (Filename.concat root "lib/bar.ml") "let d = 4\nlet e = 5\n";
  let a = collection [ ("lib/foo.ml", foo_points, [| 1; 0; 0 |]) ]
  and b =
    collection
      [
        ("lib/foo.ml", foo_points, [| 0; 1; 0 |]);
        ("lib/bar.ml", bar_points, [| 1; 0 |]);
      ]
  in
  write_file (Filename.concat root "_build/_coverage/windtrap-a.coverage") a;
  write_file (Filename.concat root "_build/_coverage/windtrap-b.coverage") b;
  root

(* INSIDE_DUNE reaches the command only when the scenario sets it: `dune
   runtest` exports its own context to this suite, and a command that
   inherited it would report the real build directory's estate in place
   of the scratch project's. *)
let coverage_cmd ?cwd ?inside_dune args =
  let inside_dune =
    match inside_dune with
    | Some context -> [ ("INSIDE_DUNE", context) ]
    | None -> []
  in
  capture ~env:inside_dune ?cwd windtrap_exe ("coverage" :: args)

(* [occurs ~sub s] is [true] iff [sub] occurs in [s]: a predicate, for
   finding and counting lines, where the facade's [contains] asserts. *)
let occurs ~sub s =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0

let lines_with ~sub text =
  List.filter (occurs ~sub) (String.split_on_char '\n' text)

(* The reporting command: merge, table, walk-up *)

let reporting_command =
  test "the reporting command: merge, table, walk-up" @@ fun () ->
  let proj = proj () in
  let code, out, err = coverage_cmd ~cwd:proj [] in
  equal ~msg:"the merged report exits 0" int 0 code;
  equal ~msg:"the merged report keeps stderr empty" text "" err;
  equal
    ~msg:
      "the table under its header row, the outcome last: counts merge across \
       executables, each file reports its uncovered line"
    text
    "   cover    points   file         uncovered lines (-u shows the source)\n\
    \   50.0%    1/2      lib/bar.ml   2\n\
    \   66.7%    2/3      lib/foo.ml   3\n\
     coverage: 60.0% (3/5 points)\n"
    out;
  (* Discovery walks up from a subdirectory to the project root. *)
  let code, out, _ = coverage_cmd ~cwd:(Filename.concat proj "lib") [] in
  equal ~msg:"walk-up discovery exits 0" int 0 code;
  contains ~msg:"walk-up discovery finds the same data"
    ~sub:"coverage: 60.0% (3/5 points)" out;
  contains ~msg:"walk-up discovery still resolves sources"
    ~sub:"lib/foo.ml   3\n" out;
  (* Explicit PATH arguments replace discovery; sources then resolve
     against the current directory only. *)
  let code, out, _ =
    coverage_cmd ~cwd:(temp_dir ()) [ Filename.concat proj "_build/_coverage" ]
  in
  equal ~msg:"an explicit PATH exits 0" int 0 code;
  contains ~msg:"an explicit PATH merges the same data"
    ~sub:"coverage: 60.0% (3/5 points)" out;
  contains ~msg:"unresolvable sources are named, not silently blank"
    ~sub:"(source not found)" out;
  (* Excerpts. *)
  let code, out, _ = coverage_cmd ~cwd:proj [ "--show-uncovered" ] in
  equal ~msg:"--show-uncovered exits 0" int 0 code;
  (* The table, whose header no longer says how to see the source since
     it is shown; then each file under a heading that ends on its
     numbers, its uncovered source painted; and the outcome one blank
     line under the last file. *)
  equal ~msg:"--show-uncovered paints the uncovered source, whole" text
    "   cover    points   file         uncovered lines\n\
    \   50.0%    1/2      lib/bar.ml   2\n\
    \   66.7%    2/3      lib/foo.ml   3\n\n\
     lib/bar.ml: 50.0% (1/2)\n\n\
    \      1 \u{2502} let d = 4\n\
    \  \u{258c}   2 \u{2502} let e = 5\n\n\
     lib/foo.ml: 66.7% (2/3)\n\n\
    \      2 \u{2502} let b = 2\n\
    \  \u{258c}   3 \u{2502} let c = 3\n\n\
     coverage: 60.0% (3/5 points)\n"
    out

(* A tree built without dune: the executable is under no build
   directory, so it dumps under the working directory's _windtrap, the
   directory a Make tree may grow where it must never grow a _build,
   and the reporting command finds it there. The child is copied out of
   the build tree to get an executable path with no _build component. *)
let standalone_layout =
  test "outside any build directory: _windtrap/coverage, found by the merge"
  @@ fun () ->
  let proj = scratch "standalone" in
  let exe = Filename.concat proj "bin/child.exe" in
  write_file exe (read_file child_exe);
  Unix.chmod exe 0o755;
  write_file (Filename.concat proj child_src_path) child_source;
  let code, out, _ =
    capture ~cwd:proj ~env:child_src_env exe [ "--color"; "never" ]
  in
  equal ~msg:"the copied child exits 0" int 0 code;
  not_contains ~msg:"and prints no coverage line" ~sub:"coverage:" out;
  let estate = Filename.concat proj "_windtrap/coverage" in
  is_true ~msg:"the dump directory is <cwd>/_windtrap/coverage"
    (Sys.file_exists estate && Sys.is_directory estate);
  is_false ~msg:"and no _build was grown"
    (Sys.file_exists (Filename.concat proj "_build"));
  let code, out, err = coverage_cmd ~cwd:proj [] in
  equal ~msg:"the reporting command finds it without arguments" int 0 code;
  equal ~msg:"with no exclusion: the identity is the copy's absolute path" text
    "" err;
  (* Under --instrument-with the copied child links an instrumented core and
     dumps its points too, which widens the table's columns: pin the child's
     row, not the padding around it. *)
  (match lines_with ~sub:child_src_path out with
  | [ row ] -> contains ~msg:"and reports what the run measured" ~sub:"4/6" row
  | rows ->
      failf "%d rows name the child's source in:\n%s" (List.length rows) out);
  (* Walk-up applies to this layout too. *)
  let sub = Filename.concat proj "lib/deep" in
  mkdir_p sub;
  let code, out, _ = coverage_cmd ~cwd:sub [] in
  equal ~msg:"walk-up finds _windtrap from a subdirectory" int 0 code;
  contains ~msg:"and merges the same data" ~sub:"4/6" out

(* `dune exec windtrap -- coverage` under a private --build-dir: dune
   exports the context it built in as INSIDE_DUNE, and the estate is
   that build directory's, never the _build an ancestor scan would
   find first. The same rule the core applies to its own root. *)
let inside_dune_estate =
  test "INSIDE_DUNE names the build directory whose estate is reported"
  @@ fun () ->
  let proj = scratch "private-build-dir" in
  write_file
    (Filename.concat proj "lib/foo.ml")
    "let a = 1\nlet b = 2\nlet c = 3\n";
  let shared = collection [ ("lib/foo.ml", foo_points, [| 1; 1; 1 |]) ]
  and private_dir = collection [ ("lib/foo.ml", foo_points, [| 1; 0; 0 |]) ] in
  write_file
    (Filename.concat proj "_build/_coverage/windtrap-s.coverage")
    shared;
  write_file
    (Filename.concat proj "_build_ci/_coverage/windtrap-p.coverage")
    private_dir;
  let context = Filename.concat proj "_build_ci/default" in
  let code, out, err = coverage_cmd ~cwd:proj ~inside_dune:context [] in
  equal ~msg:"the command exits 0" int 0 code;
  equal ~msg:"and excludes nothing" text "" err;
  contains ~msg:"the private build directory's estate, not the shared one's"
    ~sub:"coverage: 33.3% (1/3 points)" out;
  (* Without it, the ancestor scan finds the shared _build first. *)
  let _, out, _ = coverage_cmd ~cwd:proj [] in
  contains ~msg:"unset, the scan reports the shared _build"
    ~sub:"coverage: 100.0% (3/3 points)" out;
  (* A boolean spelling (a harness's INSIDE_DUNE=1) names no build
     directory, and the scan runs as if it were unset. *)
  let _, out, _ = coverage_cmd ~cwd:proj ~inside_dune:"1" [] in
  contains ~msg:"a value that names no build directory is ignored"
    ~sub:"coverage: 100.0% (3/3 points)" out

(* --min matrix *)

let min_matrix =
  test "--min gates the merged percentage" @@ fun () ->
  let proj = proj () in
  let code, out, _ = coverage_cmd ~cwd:proj [ "--min"; "50" ] in
  equal ~msg:"--min below the total exits 0" int 0 code;
  ends_with
    ~msg:"--min ok prints the verdict on the outcome line, which is last"
    ~affix:"\ncoverage: 60.0% (3/5 points), minimum 50%: ok\n" out;
  not_contains ~msg:"and the gate is no line of its own" ~sub:"\nminimum" out;
  let code, out, _ = coverage_cmd ~cwd:proj [ "--min"; "60" ] in
  equal ~msg:"--min at the total exits 0" int 0 code;
  contains ~msg:"--min at the boundary is ok" ~sub:"minimum 60%: ok" out;
  let code, out, _ = coverage_cmd ~cwd:proj [ "--min"; "80" ] in
  equal ~msg:"--min above the total exits 1" int 1 code;
  ends_with ~msg:"--min failure states the measurement and its fraction, last"
    ~affix:"\ncoverage: 60.0% (3/5 points), minimum 80%: FAILED\n" out;
  let code, out, _ = coverage_cmd ~cwd:proj [ "-u"; "--min"; "80" ] in
  equal ~msg:"the source view gates alike" int 1 code;
  ends_with ~msg:"and ends on the same line, after its last file"
    ~affix:"\n\ncoverage: 60.0% (3/5 points), minimum 80%: FAILED\n" out;
  let code, _, err = coverage_cmd ~cwd:proj [ "--min"; "eleventy" ] in
  equal ~msg:"a malformed --min exits 2" int 2 code;
  contains ~msg:"a malformed --min is a usage error"
    ~sub:"invalid value 'eleventy' for --min" err;
  let code, _, err = coverage_cmd ~cwd:proj [ "--min"; "120" ] in
  equal ~msg:"an out-of-range --min exits 2" int 2 code;
  contains ~msg:"an out-of-range --min is a usage error"
    ~sub:"expected a percentage" err

(* --json *)

(* Minimal well-formedness walk: the artifact must parse as one JSON
   value with balanced structure; shape drift or a stray comma is a
   frozen-contract break, not a formatting choice. *)
let json_well_formed s =
  let n = String.length s in
  let pos = ref 0 in
  let fail = ref false in
  let peek () = if !pos < n then Some s.[!pos] else None in
  let skip_ws () =
    while !pos < n && (s.[!pos] = ' ' || s.[!pos] = '\n' || s.[!pos] = '\t') do
      incr pos
    done
  in
  let expect c = if peek () = Some c then incr pos else fail := true in
  let literal word =
    let m = String.length word in
    if !pos + m <= n && String.sub s !pos m = word then pos := !pos + m
    else fail := true
  in
  let string_lit () =
    expect '"';
    let closed = ref false in
    while (not !closed) && not !fail do
      match peek () with
      | None -> fail := true
      | Some '\\' -> pos := !pos + 2
      | Some '"' ->
          incr pos;
          closed := true
      | Some _ -> incr pos
    done
  in
  let number () =
    while
      !pos < n
      &&
      match s.[!pos] with
      | '0' .. '9' | '-' | '+' | '.' | 'e' | 'E' -> true
      | _ -> false
    do
      incr pos
    done
  in
  let rec value depth =
    if depth > 100 then fail := true
    else begin
      skip_ws ();
      match peek () with
      | Some '{' ->
          incr pos;
          skip_ws ();
          if peek () = Some '}' then incr pos
          else begin
            let more = ref true in
            while !more && not !fail do
              skip_ws ();
              string_lit ();
              skip_ws ();
              expect ':';
              value (depth + 1);
              skip_ws ();
              if peek () = Some ',' then incr pos
              else begin
                expect '}';
                more := false
              end
            done
          end
      | Some '[' ->
          incr pos;
          skip_ws ();
          if peek () = Some ']' then incr pos
          else begin
            let more = ref true in
            while !more && not !fail do
              value (depth + 1);
              skip_ws ();
              if peek () = Some ',' then incr pos
              else begin
                expect ']';
                more := false
              end
            done
          end
      | Some '"' -> string_lit ()
      | Some ('0' .. '9' | '-') -> number ()
      | Some 't' -> literal "true"
      | Some 'f' -> literal "false"
      | Some 'n' -> literal "null"
      | _ -> fail := true
    end
  in
  value 0;
  skip_ws ();
  (not !fail) && !pos = n

let json_shape =
  test "--json emits the frozen shape" @@ fun () ->
  let proj = proj () in
  let code, out, err = coverage_cmd ~cwd:proj [ "--json" ] in
  equal ~msg:"--json exits 0" int 0 code;
  equal ~msg:"--json keeps stderr empty" text "" err;
  is_true ~msg:"--json is well-formed" (json_well_formed out);
  (* The design-frozen shape: summary + files with path/visited/total/
     percentage/uncovered_lines. *)
  contains ~msg:"json: the summary object"
    ~sub:"\"summary\": { \"visited\": 3, \"total\": 5, \"percentage\": 60.00 }"
    out;
  contains ~msg:"json: files carry paths" ~sub:"\"path\": \"lib/foo.ml\"" out;
  contains ~msg:"json: per-file counts" ~sub:"\"visited\": 2, \"total\": 3" out;
  contains ~msg:"json: per-file percentage" ~sub:"\"percentage\": 66.67" out;
  contains ~msg:"json: uncovered lines" ~sub:"\"uncovered_lines\": [3]" out;
  contains ~msg:"json: bar.ml is present" ~sub:"\"path\": \"lib/bar.ml\"" out;
  (* --json --min: stdout stays a pure JSON artifact. *)
  let code, out, err = coverage_cmd ~cwd:proj [ "--json"; "--min"; "80" ] in
  equal ~msg:"--json --min still gates" int 1 code;
  is_true ~msg:"--json --min keeps stdout pure JSON" (json_well_formed out);
  equal
    ~msg:
      "--json --min moves the verdict to stderr, where it is windtrap's own \
       line, behind the anchor"
    text "windtrap: coverage: 60.0% (3/5 points), minimum 80%: FAILED\n" err;
  let code, out, err = coverage_cmd ~cwd:proj [ "--json"; "--min"; "50" ] in
  equal ~msg:"--json --min met exits 0" int 0 code;
  is_true ~msg:"and keeps stdout pure JSON" (json_well_formed out);
  equal ~msg:"with the same sentence on stderr" text
    "windtrap: coverage: 60.0% (3/5 points), minimum 50%: ok\n" err

(* --expect: exhaustiveness *)

let expectations =
  test "--expect names the sources the merge must cover" @@ fun () ->
  (* The fake project's dumps, under a tree with one source the merge
     never saw (baz.ml), a preprocessed twin of one it did (foo.pp.ml),
     a lexer source whose generated module it did (bar.mll), and a
     dot-directory to skip. *)
  let proj = proj () in
  let root = scratch "expect" in
  List.iter
    (fun name ->
      write_file
        (Filename.concat root (Filename.concat "_build/_coverage" name))
        (read_file
           (Filename.concat proj (Filename.concat "_build/_coverage" name))))
    [ "windtrap-a.coverage"; "windtrap-b.coverage" ];
  List.iter
    (fun (path, contents) -> write_file (Filename.concat root path) contents)
    [
      ("lib/foo.ml", "let a = 1\nlet b = 2\nlet c = 3\n");
      ("lib/foo.pp.ml", "let a = 1\nlet b = 2\nlet c = 3\n");
      ("lib/bar.mll", "rule token = parse eof { () }\n");
      ("lib/baz.ml", "let d = 4\n");
      ("lib/.hidden/ghost.ml", "let e = 5\n");
    ];
  let code, out, err = coverage_cmd ~cwd:root [ "--expect"; "lib" ] in
  equal ~msg:"a directory with an unseen source exits 1" int 1 code;
  contains ~msg:"the report still renders" ~sub:"coverage: 60.0%" out;
  contains
    ~msg:
      "the unseen source is named, with the reasons it can be absent, behind \
       windtrap's one anchor"
    ~sub:
      "windtrap: lib/baz.ml: expected source has no coverage data (not \
       instrumented, or linked into no test executable that ran)\n"
    err;
  not_contains ~msg:"a preprocessed twin is its source" ~sub:"foo.pp.ml" err;
  not_contains ~msg:"a lexer source is its generated module" ~sub:"bar.mll" err;
  not_contains ~msg:"dot-directories are skipped" ~sub:"ghost.ml" err;
  let code, _, err =
    coverage_cmd ~cwd:root
      [ "--expect"; "lib"; "--do-not-expect"; "lib/baz.ml" ]
  in
  equal ~msg:"--do-not-expect exempts the unseen source" int 0 code;
  equal ~msg:"and nothing is warned about" text "" err;
  let code, _, _ =
    coverage_cmd ~cwd:root [ "--expect=lib"; "--do-not-expect=lib/baz.ml" ]
  in
  equal ~msg:"--expect=PATH and --do-not-expect=PATH equal the two-word forms"
    int 0 code;
  let code, _, _ = coverage_cmd ~cwd:root [ "--expect"; "lib/foo.ml" ] in
  equal ~msg:"a single covered file passes" int 0 code;
  let code, _, err = coverage_cmd ~cwd:root [ "--expect"; "lib/nope" ] in
  equal ~msg:"a nonexistent --expect path exits 1" int 1 code;
  contains ~msg:"a nonexistent --expect path is named" ~sub:"lib/nope" err;
  let code, _, err = coverage_cmd ~cwd:root [ "--expect" ] in
  equal ~msg:"--expect without an argument exits 2" int 2 code;
  contains ~msg:"--expect without an argument says so" ~sub:"--expect" err;
  (* Under a machine format stdout stays the artifact; both gates run. *)
  let code, out, err =
    coverage_cmd ~cwd:root [ "--json"; "--expect"; "lib"; "--min"; "80" ]
  in
  equal ~msg:"--json --expect --min exits 1" int 1 code;
  is_true ~msg:"--json --expect keeps stdout pure JSON" (json_well_formed out);
  contains ~msg:"the missing source is on stderr" ~sub:"lib/baz.ml" err;
  contains ~msg:"and so is the --min verdict" ~sub:"FAILED" err

(* --lcov: the tracefile *)

let lcov_output =
  test "--lcov emits a tracefile" @@ fun () ->
  let proj = proj () in
  let code, out, err = coverage_cmd ~cwd:proj [ "--lcov" ] in
  equal ~msg:"--lcov exits 0" int 0 code;
  equal ~msg:"--lcov keeps stderr empty" text "" err;
  (* Files by name, every touched line with its hits (the merged
     counts: foo [1;1;0], bar [1;0]), then the line totals. *)
  equal ~msg:"--lcov is the frozen tracefile" string
    "TN:\n\
     SF:lib/bar.ml\n\
     DA:1,1\n\
     DA:2,0\n\
     LF:2\n\
     LH:1\n\
     end_of_record\n\
     TN:\n\
     SF:lib/foo.ml\n\
     DA:1,1\n\
     DA:2,1\n\
     DA:3,0\n\
     LF:3\n\
     LH:2\n\
     end_of_record\n"
    out;
  (* --lcov --min: stdout stays a pure tracefile. *)
  let code, out, err = coverage_cmd ~cwd:proj [ "--lcov"; "--min"; "80" ] in
  equal ~msg:"--lcov --min still gates" int 1 code;
  not_contains ~msg:"--lcov --min keeps stdout pure" ~sub:"minimum" out;
  not_contains ~msg:"the outcome line stays off a tracefile" ~sub:"coverage:"
    out;
  equal
    ~msg:
      "--lcov --min moves the verdict to stderr, the report's sentence behind \
       the anchor"
    text "windtrap: coverage: 60.0% (3/5 points), minimum 80%: FAILED\n" err;
  (* Two owners of stdout is a usage error. *)
  let code, _, err = coverage_cmd ~cwd:proj [ "--lcov"; "--json" ] in
  equal ~msg:"--lcov --json exits 2" int 2 code;
  contains ~msg:"--lcov --json names the clash" ~sub:"--lcov" err;
  (* A file whose source is missing is omitted and named, never painted. *)
  let orphan = scratch "lcov-orphan" in
  write_file
    (Filename.concat orphan "_build/_coverage/x.coverage")
    (collection [ ("lib/gone.ml", bar_points, [| 1; 0 |]) ]);
  let code, out, err = coverage_cmd ~cwd:orphan [ "--lcov" ] in
  equal ~msg:"a missing source still exits 0" int 0 code;
  not_contains ~msg:"a missing source has no record" ~sub:"SF:" out;
  equal ~msg:"a missing source is named on stderr, behind windtrap's one anchor"
    text
    "windtrap: lib/gone.ml: source not found; omitted from the lcov output\n"
    err

(* Loud failures *)

let loud_failures =
  test "failures are loud: no data, corrupt data, usage errors" @@ fun () ->
  let proj = proj () in
  (* Nothing to report. *)
  let empty = scratch "empty-root" in
  mkdir_p empty;
  let code, _, err = coverage_cmd ~cwd:empty [] in
  equal ~msg:"no .coverage files exit 1" int 1 code;
  contains ~msg:"no files: the hint names the backend, not a build tool"
    ~sub:"ppx_windtrap.coverage" err;
  not_contains ~msg:"and spells no dune command" ~sub:"dune " err;
  (* Corrupt and foreign files are rejected loudly. *)
  let corrupt = scratch "corrupt" in
  write_file
    (Filename.concat corrupt "_build/_coverage/bad.coverage")
    "not a coverage file\n";
  let code, _, err = coverage_cmd ~cwd:corrupt [] in
  equal ~msg:"a corrupt file exits 1" int 1 code;
  contains ~msg:"a corrupt file is named" ~sub:"bad.coverage" err;
  let v1 = scratch "v1" in
  write_file
    (Filename.concat v1 "_build/_coverage/old.coverage")
    "WINDTRAP-COVERAGE-1\nsome v1 payload\n";
  let code, _, err = coverage_cmd ~cwd:v1 [] in
  equal ~msg:"a v1-format file exits 1" int 1 code;
  contains ~msg:"a v1-format file is named" ~sub:"old.coverage" err;
  contains
    ~msg:"a foreign format instructs deletion (re-running cannot remove it)"
    ~sub:"delete" err;
  (* Mismatched point tables across executables. *)
  let mismatch = scratch "mismatch" in
  let one = collection [ ("lib/foo.ml", foo_points, [| 1; 0; 0 |]) ]
  and two = collection [ ("lib/foo.ml", bar_points, [| 1; 0 |]) ] in
  write_file (Filename.concat mismatch "_build/_coverage/one.coverage") one;
  write_file (Filename.concat mismatch "_build/_coverage/two.coverage") two;
  let code, _, err = coverage_cmd ~cwd:mismatch [] in
  equal ~msg:"mismatched point tables exit 1" int 1 code;
  contains ~msg:"mismatched point tables name the file" ~sub:"lib/foo.ml" err;
  contains ~msg:"the mismatch hint is a full re-run, not deletion first"
    ~sub:"from one build" err;
  (* Usage errors. *)
  let code, _, err = coverage_cmd ~cwd:proj [ "--frobnicate" ] in
  equal ~msg:"an unknown option exits 2" int 2 code;
  equal ~msg:"a usage error is the anchored sentence, then the usage line" text
    "windtrap: unknown option '--frobnicate'\n\
     usage: windtrap coverage [OPTIONS] [PATH...]\n"
    err;
  let code, out, _ = coverage_cmd ~cwd:proj [ "--help" ] in
  equal ~msg:"coverage --help exits 0" int 0 code;
  contains ~msg:"coverage --help documents --min, its sentence under it"
    ~sub:"  --min=PCT\n      Exit 1 when total coverage is below PCT.\n" out;
  contains ~msg:"a description wraps and is never cut"
    ~sub:
      "  --expect=PATH\n\
      \      Exit 1 unless every .ml/.mll/.mly under PATH (or PATH itself) has\n\
      \      coverage data; repeatable.\n"
    out;
  contains ~msg:"--lcov names only tools that read an LCOV tracefile"
    ~sub:
      "  --lcov\n\
      \      LCOV tracefile on standard output (genhtml, Codecov, Coveralls, \
       editor\n\
      \      gutters).\n"
    out;
  contains ~msg:"coverage --help opens on the name line, then the usage line"
    ~sub:
      "windtrap coverage - merge .coverage files and report\n\n\
       usage: windtrap coverage [OPTIONS] [PATH...]\n"
    out;
  contains ~msg:"and names the one variable that has no flag"
    ~sub:
      "ENVIRONMENT (no flag):\n\
      \  WINDTRAP_COLOR\n\
      \      Color output: always, never or auto.\n"
    out;
  List.iter
    (fun line ->
      at_most
        ~msg:(Printf.sprintf "coverage --help fits 80 columns: %s" line)
        int ~than:80 (String.length line))
    (String.split_on_char '\n' out);
  (* Top-level dispatch. *)
  let commands =
    "usage: windtrap <command> [OPTIONS]\n\n\
     COMMANDS:\n\
    \  coverage\n\
    \      Merge .coverage files and report; --min gates, --json exports.\n\n\
    \  mutants\n\
    \      Merge .mutants verdict files and report the project's survivors.\n\n\
     OPTIONS:\n\
    \  -h, --help\n\
    \      Print this help and exit.\n\n\
     See `windtrap <command> --help` for a subcommand's options.\n"
  in
  let code, _, err = capture windtrap_exe [] in
  equal ~msg:"no command exits 2" int 2 code;
  equal ~msg:"no command says so, then the usage line and the commands" text
    ("windtrap: no command given\n" ^ commands)
    err;
  let code, _, err = capture windtrap_exe [ "frobnicate" ] in
  equal ~msg:"an unknown command exits 2" int 2 code;
  equal
    ~msg:
      "an unknown command is named, then the usage line, the commands there \
       are and the pointer to their help"
    text
    ("windtrap: unknown command 'frobnicate'\n" ^ commands)
    err;
  let code, out, _ = capture windtrap_exe [ "--help" ] in
  equal ~msg:"windtrap --help exits 0" int 0 code;
  contains ~msg:"windtrap --help opens on the name line, then the usage line"
    ~sub:
      "windtrap - reports merged from instrumented test runs\n\n\
       usage: windtrap <command> [OPTIONS]\n"
    out;
  contains ~msg:"windtrap --help lists the subcommand" ~sub:"  coverage\n" out;
  List.iter
    (fun line ->
      at_most
        ~msg:(Printf.sprintf "windtrap --help fits 80 columns: %s" line)
        int ~than:80 (String.length line))
    (String.split_on_char '\n' out);
  equal ~msg:"windtrap --help is the name line, then the usage and commands"
    text
    ("windtrap - reports merged from instrumented test runs\n\n" ^ commands)
    out;
  List.iter
    (fun flag ->
      let code, flag_out, _ = capture windtrap_exe [ flag; "frobnicate" ] in
      equal ~msg:(flag ^ " exits 0") int 0 code;
      equal ~msg:(flag ^ " is --help, whatever follows it") text out flag_out)
    [ "-h"; "-help"; "--help" ]

(* --min boundaries *)

let min_boundaries =
  test "--min boundaries and spellings" @@ fun () ->
  let proj = proj () in
  (* The 0 and 100 rails. *)
  let code, out, _ = coverage_cmd ~cwd:proj [ "--min"; "0" ] in
  equal ~msg:"--min 0 always passes" int 0 code;
  contains ~msg:"--min 0 prints its verdict" ~sub:"minimum 0%: ok" out;
  let code, out, _ = coverage_cmd ~cwd:proj [ "--min"; "100" ] in
  equal ~msg:"--min 100 fails below full coverage" int 1 code;
  contains ~msg:"--min 100 states the shortfall"
    ~sub:"coverage: 60.0% (3/5 points), minimum 100%: FAILED\n" out;
  let full = scratch "fullproj" in
  let all =
    collection
      [
        ("lib/foo.ml", foo_points, [| 1; 1; 1 |]);
        ("lib/bar.ml", bar_points, [| 2; 1 |]);
      ]
  in
  write_file (Filename.concat full "_build/_coverage/full.coverage") all;
  let code, out, _ = coverage_cmd ~cwd:full [ "--min"; "100" ] in
  equal ~msg:"--min 100 passes at exactly 100%" int 0 code;
  contains ~msg:"full coverage meets the 100% gate" ~sub:"minimum 100%: ok" out;
  (* The --min=PCT spelling, including the empty value. *)
  let code, out, _ = coverage_cmd ~cwd:proj [ "--min=50" ] in
  equal ~msg:"--min=PCT equals the two-word form" int 0 code;
  contains ~msg:"--min=PCT prints its verdict" ~sub:"minimum 50%: ok" out;
  let code, _, err = coverage_cmd ~cwd:proj [ "--min=" ] in
  equal ~msg:"an empty --min= exits 2" int 2 code;
  contains ~msg:"an empty --min= is an invalid value, not an unknown option"
    ~sub:"invalid value '' for --min" err;
  (* The gate compares raw percentages, not their renderings: 2/3 rounds
     to the 66.7 it is gated against and still falls short. The verdict
     makes no comparative claim, so the fraction is what says why. *)
  let thirds = scratch "twothirds" in
  let two_of_three = collection [ ("lib/foo.ml", foo_points, [| 1; 1; 0 |]) ] in
  write_file (Filename.concat thirds "_build/_coverage/t.coverage") two_of_three;
  let code, out, _ = coverage_cmd ~cwd:thirds [ "--min"; "66.7" ] in
  equal ~msg:"the gate compares raw percentages" int 1 code;
  contains ~msg:"a display-equal shortfall still fails, with its fraction"
    ~sub:"coverage: 66.7% (2/3 points), minimum 66.7%: FAILED\n" out

(* Discovery and merge robustness *)

let discovery_robustness =
  test "discovery and merge robustness" @@ fun () ->
  let proj = proj () in
  (* An existing but empty _build/_coverage is "no files", loudly. *)
  let bare = scratch "bare" in
  mkdir_p (Filename.concat bare "_build/_coverage");
  let code, _, err = coverage_cmd ~cwd:bare [] in
  equal ~msg:"an empty _build/_coverage exits 1" int 1 code;
  equal
    ~msg:
      "an empty _build/_coverage prints the no-files hint, behind windtrap's \
       one anchor, the hint on its own line"
    text
    "windtrap: no .coverage files found\n\
     Instrument the library under test with ppx_windtrap.coverage and run its \
     tests first; every instrumented test executable writes its dump at exit, \
     under the build directory's _coverage or under _windtrap/coverage.\n"
    err;
  (* A truncated file is corrupt and named, never partially merged. *)
  let serialized = collection [ ("lib/foo.ml", foo_points, [| 1; 0; 0 |]) ] in
  let trunc = scratch "trunc" in
  write_file
    (Filename.concat trunc "_build/_coverage/cut.coverage")
    (String.sub serialized 0 (String.length serialized - 4));
  let code, _, err = coverage_cmd ~cwd:trunc [] in
  equal ~msg:"a truncated file exits 1" int 1 code;
  contains ~msg:"a truncated file is named" ~sub:"cut.coverage" err;
  contains ~msg:"a truncated file is called corrupt" ~sub:"corrupt" err;
  (* A dump recorded as written by the reporting binary itself gets no
     special treatment: it is judged by its identity like any other.
     Here the recorded executable does not exist under this root, so
     it is an orphan, excluded and named. *)
  let selfish = scratch "selfish" in
  write_file
    (Filename.concat selfish "_build/_coverage/self.coverage")
    (collection
       ~identity:
         {
           C.exe = I.exe_identity ~exe:windtrap_exe;
           digest = Digest.to_hex (Digest.string "some earlier build");
         }
       [ ("lib/ghost.ml", foo_points, [| 1; 1; 1 |]) ]);
  let code, out, err = coverage_cmd ~cwd:selfish [] in
  equal ~msg:"a directory holding only the reporter's own dump exits 1" int 1
    code;
  contains ~msg:"the dump is excluded like any other orphan"
    ~sub:"self.coverage" err;
  contains ~msg:"and the run says every file found was excluded, and why"
    ~sub:"windtrap: found 1 .coverage file and every one is orphaned\n" err;
  not_contains ~msg:"and nothing is merged" ~sub:"ghost.ml" out;
  (* An explicit .coverage FILE argument is honored as-is. *)
  let code, out, _ =
    coverage_cmd ~cwd:(temp_dir ())
      [ Filename.concat proj "_build/_coverage/windtrap-a.coverage" ]
  in
  equal ~msg:"an explicit file argument exits 0" int 0 code;
  contains ~msg:"an explicit file argument reports its data alone"
    ~sub:"coverage: 33.3% (1/3 points)" out;
  (* A rule-action cwd (inside _build) resolves the root by the
     topmost-_build rule (the runtime's), never the ancestor scan. *)
  mkdir_p (Filename.concat proj "_build/default/examples");
  let code, out, _ =
    coverage_cmd ~cwd:(Filename.concat proj "_build/default/examples") []
  in
  equal ~msg:"a cwd inside _build exits 0" int 0 code;
  contains ~msg:"a cwd inside _build resolves the workspace root"
    ~sub:"coverage: 60.0% (3/5 points)" out;
  contains ~msg:"sources resolve from that root too" ~sub:"lib/foo.ml   3\n" out;
  (* The sandbox trap: v1 garbage planted at _build/.sandbox/_build/_coverage
     must not capture discovery from a sandboxed action's cwd. The
     topmost _build wins. *)
  write_file
    (Filename.concat proj "_build/.sandbox/_build/_coverage/junk.coverage")
    "WINDTRAP-COVERAGE-1\nleftover\n";
  mkdir_p (Filename.concat proj "_build/.sandbox/0abc/default");
  let code, out, err =
    coverage_cmd ~cwd:(Filename.concat proj "_build/.sandbox/0abc/default") []
  in
  equal ~msg:"a sandboxed cwd escapes planted garbage" int 0 code;
  contains ~msg:"a sandboxed cwd reports the workspace data"
    ~sub:"coverage: 60.0% (3/5 points)" out;
  not_contains ~msg:"the planted v1 file is never read" ~sub:"junk.coverage" err

(* Explicit PATH arguments are a contract *)

let explicit_path_contract =
  test "explicit PATH arguments are loud when invalid" @@ fun () ->
  let proj = proj () in
  let elsewhere = temp_dir () in
  (* A nonexistent explicit path is an error naming the path and the
     reason, never a silent drop into the no-data report, whose
     instrument-your-library remedy would be wrong here. *)
  let absent = scratch "no-such-dir/absent.coverage" in
  let code, _, err = coverage_cmd ~cwd:elsewhere [ absent ] in
  equal ~msg:"a missing explicit path exits 1" int 1 code;
  contains ~msg:"a missing explicit path is named" ~sub:absent err;
  contains ~msg:"a missing explicit path states the reason"
    ~sub:"no such file or directory" err;
  not_contains ~msg:"a missing explicit path never blames instrumentation"
    ~sub:"Instrument the library" err;
  (* An existing file without the .coverage suffix (a renamed dump)
     is equally loud, whatever its content. *)
  let renamed = scratch "renamed.cov" in
  write_file renamed (collection [ ("lib/foo.ml", foo_points, [| 1; 0; 0 |]) ]);
  let code, _, err = coverage_cmd ~cwd:elsewhere [ renamed ] in
  equal ~msg:"a wrong-suffix explicit file exits 1" int 1 code;
  contains ~msg:"a wrong-suffix explicit file is named" ~sub:renamed err;
  contains ~msg:"a wrong-suffix explicit file states the reason"
    ~sub:"not a .coverage file" err;
  not_contains ~msg:"a wrong-suffix explicit file never blames instrumentation"
    ~sub:"Instrument the library" err;
  (* An invalid path beside a valid one still fails the invocation:
     explicit arguments never narrow silently. *)
  let valid = Filename.concat proj "_build/_coverage/windtrap-a.coverage" in
  let code, _, err = coverage_cmd ~cwd:elsewhere [ valid; absent ] in
  equal ~msg:"one bad path fails the whole invocation" int 1 code;
  contains ~msg:"the bad path is the one named" ~sub:absent err;
  (* Directory arguments keep the scan's tolerance: an existing
     directory holding no dumps falls through to the no-data report. *)
  let empty_dir = scratch "explicit-empty" in
  mkdir_p empty_dir;
  let code, _, err = coverage_cmd ~cwd:elsewhere [ empty_dir ] in
  equal ~msg:"an empty explicit directory exits 1" int 1 code;
  contains ~msg:"an empty explicit directory is a no-data report"
    ~sub:"no .coverage files found" err

(* The staleness pass: orphaned and outdated dumps *)

(* The holes the @cover alias cannot see: a
   dump whose executable was deleted (orphan, silently inflates the
   merge) and a dump whose executable is not the one now on disk, a
   re-run without --instrument-with (wrote nothing fresh), or a test
   action dune replayed from cache after sources reverted to an
   already-tested state (measured against the blessed alias: the dump
   stays a different build's, and plain re-runs stay cache hits, so
   only `--force` heals it). Both are detected from the recorded
   identity; staleness is a content comparison (the recorded digest
   against the executable now on disk) because dune's cache restores
   rebuilt artifacts with their original mtimes. Both are warned about
   and excluded, always: there is no override. Identity-less dumps (the
   fixtures above) are never flagged. *)

let ghost_points = [| { C.start_ofs = 0; end_ofs = 9 } |]

let plant_exe root exe contents =
  write_file (Filename.concat root (Filename.concat "_build" exe)) contents;
  { C.exe; digest = Digest.to_hex (Digest.string contents) }

let write_dump root name ~identity adds =
  write_file
    (Filename.concat root (Filename.concat "_build/_coverage" name))
    (collection ~identity adds)

let stale_root name =
  let root = scratch name in
  write_file
    (Filename.concat root "lib/foo.ml")
    "let a = 1\nlet b = 2\nlet c = 3\n";
  let identity = plant_exe root "default/test/a.exe" "the instrumented build" in
  write_dump root "a.coverage" ~identity
    [ ("lib/foo.ml", foo_points, [| 1; 1; 1 |]) ];
  root

let staleness_pass =
  test "the staleness pass: orphaned and outdated dumps" @@ fun () ->
  (* Fresh: the executable on disk is the dump's writer (full
     inclusion). *)
  let root = stale_root "stale-fresh" in
  let code, out, err = coverage_cmd ~cwd:root [] in
  equal ~msg:"a fresh identity-carrying dump exits 0" int 0 code;
  equal ~msg:"a fresh identity-carrying dump warns about nothing" text "" err;
  contains ~msg:"a fresh identity-carrying dump merges"
    ~sub:"coverage: 100.0% (3/3 points)" out;
  (* Orphan: a second dump whose executable no longer exists. *)
  let root = stale_root "stale-orphan" in
  write_dump root "gone.coverage"
    ~identity:
      {
        C.exe = "default/test/gone.exe";
        digest = Digest.to_hex (Digest.string "gone");
      }
    [ ("lib/ghost.ml", ghost_points, [| 0 |]) ];
  let code, out, err = coverage_cmd ~cwd:root [] in
  equal ~msg:"an orphaned dump still reports the live data" int 0 code;
  contains ~msg:"the orphan is excluded from the merge"
    ~sub:"coverage: 100.0% (3/3 points)" out;
  not_contains ~msg:"the orphan's files stay out of the table" ~sub:"ghost.ml"
    out;
  contains ~msg:"the orphan warning names the dump" ~sub:"gone.coverage" err;
  contains ~msg:"the orphan warning names the missing executable"
    ~sub:"default/test/gone.exe" err;
  contains ~msg:"the orphan warning says what it did" ~sub:"excluding it" err;
  (* Stale: the executable was rebuilt since the dump. Its content no
     longer matches the recorded digest (its mtime is irrelevant). *)
  let root = stale_root "stale-rebuilt" in
  ignore (plant_exe root "default/test/a.exe" "an uninstrumented rebuild");
  let code, _, err = coverage_cmd ~cwd:root [] in
  equal ~msg:"a lone stale dump exits 1 (nothing left to report)" int 1 code;
  contains ~msg:"the stale warning names the dump" ~sub:"a.coverage" err;
  contains ~msg:"the stale warning says the executable was rebuilt"
    ~sub:"rebuilt since" err;
  contains
    ~msg:
      "the remedy is an instrumented re-run that names the cached-run cause, \
       one line behind windtrap's one anchor"
    ~sub:
      "windtrap: re-run the suite instrumented (forcing the runs your build \
       tool cached), then merge again; delete the files whose executable no \
       longer exists\n"
    err;
  not_contains ~msg:"the command's own prefix is gone" ~sub:"windtrap coverage:"
    err;
  not_contains ~msg:"and spells no dune command" ~sub:"dune " err;
  contains
    ~msg:
      "excluding everything is loud: the count, what the files are, where they \
       came from and the usual cause"
    ~sub:
      "windtrap: found 1 .coverage file and every one is stale\n\
      \  They were written by executables that no longer exist or have been \
       rebuilt since.\n\
      \  The usual cause is a build without the instrumentation flag.\n\
       windtrap: re-run the suite instrumented"
    err;
  (* A build without the instrumentation excludes every dump of the
     project: three are named, the rest counted, then the summary splits
     the stale from the orphaned and the remedy prints once. *)
  let root = stale_root "stale-many" in
  ignore (plant_exe root "default/test/a.exe" "an uninstrumented rebuild");
  List.iter
    (fun name ->
      let identity =
        plant_exe root
          ("default/test/" ^ name ^ ".exe")
          "the instrumented build"
      in
      write_dump root (name ^ ".coverage") ~identity
        [ ("lib/ghost.ml", ghost_points, [| 1 |]) ];
      ignore
        (plant_exe root
           ("default/test/" ^ name ^ ".exe")
           "an uninstrumented rebuild"))
    [ "b"; "c"; "d" ];
  write_dump root "e.coverage"
    ~identity:
      {
        C.exe = "default/test/gone.exe";
        digest = Digest.to_hex (Digest.string "gone");
      }
    [ ("lib/ghost.ml", ghost_points, [| 1 |]) ];
  let code, _, err = coverage_cmd ~cwd:root [] in
  equal ~msg:"five excluded dumps and nothing else exits 1" int 1 code;
  equal ~msg:"at most three files are named" int 3
    (List.length (lines_with ~sub:"; excluding it" err));
  contains ~msg:"the first three, in path order" ~sub:"c.coverage" err;
  not_contains ~msg:"the fourth is counted, not named" ~sub:"d.coverage" err;
  contains ~msg:"the rest are one line, then the summary with its split"
    ~sub:
      "excluding it\n\
       windtrap: ... and 2 more like that\n\
       windtrap: found 5 .coverage files and every one is stale or orphaned (1 \
       orphaned)\n"
    err;
  equal ~msg:"the remedy still prints once" int 1
    (List.length (lines_with ~sub:"then merge again" err));
  (let root = stale_root "stale-few" in
   ignore (plant_exe root "default/test/a.exe" "an uninstrumented rebuild");
   let _, _, err = coverage_cmd ~cwd:root [] in
   not_contains ~msg:"three or fewer excluded files draw no count line"
     ~sub:"more like that" err);
  (* Stale beside fresh (the revert trap), measured against the blessed
     alias: reverting sources to an already-tested state makes that
     test action a dune cache hit, so its dump is never rewritten and
     stays a different (intermediate) build's. The report must keep
     gating on the fresh data, exclude the stale dump, and name the one
     remedy that always works: a plain instrumented re-run stays a
     cache hit and never heals. *)
  let root = stale_root "stale-revert" in
  let identity = plant_exe root "default/test/b.exe" "an intermediate build" in
  write_dump root "b.coverage" ~identity
    [ ("lib/ghost.ml", ghost_points, [| 1 |]) ];
  ignore (plant_exe root "default/test/b.exe" "the reverted build");
  let code, out, err = coverage_cmd ~cwd:root [] in
  equal ~msg:"a stale dump beside a fresh one exits 0" int 0 code;
  contains ~msg:"the fresh data still gates alone"
    ~sub:"coverage: 100.0% (3/3 points)" out;
  not_contains ~msg:"the stale dump's files stay out of the table"
    ~sub:"ghost.ml" out;
  contains ~msg:"the partial-exclusion warning names the dump" ~sub:"b.coverage"
    err;
  equal ~msg:"the partial-exclusion remedy is the same sentence, said once" int
    1
    (List.length (lines_with ~sub:"then merge again" err));
  (* An absolute identity resolves without a _build root. *)
  let root = stale_root "stale-abs" in
  write_dump root "abs.coverage"
    ~identity:
      {
        C.exe = scratch "no-such-exe";
        digest = Digest.to_hex (Digest.string "x");
      }
    [ ("lib/ghost.ml", ghost_points, [| 1 |]) ];
  let _, out, err = coverage_cmd ~cwd:root [] in
  contains ~msg:"a missing absolute identity is an orphan" ~sub:"no-such-exe"
    err;
  not_contains ~msg:"the absolute orphan is excluded" ~sub:"ghost.ml" out;
  (* Usage rail: the flag is gone, and an unknown flag is a usage
     error, never a silently ignored argument. *)
  let code, _, err = coverage_cmd ~cwd:root [ "--stale=include" ] in
  equal ~msg:"--stale is no longer an option" int 2 code;
  contains ~msg:"--stale is reported as unknown"
    ~sub:"unknown option '--stale=include'" err

(* Raise attribution end to end (the frozen expression-grade scope) *)

(* The one genuinely instrumented path in this test: raise_child drives
   the covcli_fixture library - instrumented by the real PPX - through a
   real windtrap run. Its raising call's out-edge can never fire, so the
   dump and the CLI report must both show 2/3 points with the call line
   uncovered: a raising path lowers the percentage. *)
let raise_child_exe = Filename.concat exe_dir "raise_child.exe"

(* The records of [only] in the dump at [path], read with the runtime's
   scanner: the dump is the child's whole process, and a report replanted
   from it must hold the fixture alone. *)
let records_of ~only path =
  let c =
    match I.start C.format ~path (read_file path) with
    | Ok c -> c
    | Error e -> failf "the dump does not start: %a" (I.pp_error C.format) e
  in
  ignore (I.read_identity c);
  let found = ref None in
  for _ = 1 to I.read_count c "files" do
    let file = I.read_name c "file" in
    let n = I.read_count c "points" in
    let points = Array.make n { C.start_ofs = 0; end_ofs = 0 } in
    let counts = Array.make n 0 in
    for i = 0 to n - 1 do
      let start_ofs = I.read_nat c "start" in
      let end_ofs = I.read_nat c "end" in
      points.(i) <- { C.start_ofs; end_ofs };
      counts.(i) <- I.read_nat c "count"
    done;
    if file = only then found := Some (file, points, counts)
  done;
  match !found with
  | Some records -> records
  | None -> failf "the dump holds no %s" only

let raise_attribution =
  test "raise attribution end to end" @@ fun () ->
  let dump = scratch "raise.coverage" in
  let fixture = "test/instr/coverage_cmd/covcli_fixture.ml" in
  let code, out, _ =
    capture
      ~env:[ ("WINDTRAP_COVERAGE_FILE", dump) ]
      raise_child_exe [ "--color"; "never" ]
  in
  equal ~msg:"the raise child exits 0" int 0 code;
  not_contains ~msg:"the run prints no number of its own" ~sub:"coverage:" out;
  (* The fixture source is a declared test dep, copied beside the
     executable, resolved absolutely so a by-hand run from anywhere in
     the checkout reads it too. *)
  let source = read_file (Filename.concat exe_dir "covcli_fixture.ml") in
  let sources = scratch "raise-sources" in
  write_file (Filename.concat sources fixture) source;
  match dump_of ~source_roots:[ sources ] ~only:fixture dump with
  | None -> fail "the raise child's dump does not load"
  | Some (r, _) ->
      equal ~msg:"exactly one point - the out-edge - is unvisited"
        (pair int int) (2, 3)
        (r.C.summary.C.visited, r.C.summary.C.total);
      equal ~msg:"one uncovered extent: the raising call" int 1
        (List.length r.C.uncovered_extents);
      equal ~msg:"the uncovered extent is the call line (line 7)" (list int)
        [ 7 ] r.C.uncovered_lines;
      (* Replant source and the fixture's records in a scratch project:
         the reporting command must attribute the unreached out-edge to
         the call line. *)
      let root = scratch "raiseproj" in
      write_file (Filename.concat root fixture) source;
      write_file
        (Filename.concat root "_build/_coverage/raise.coverage")
        (collection [ records_of ~only:fixture dump ]);
      let code, out, _ = coverage_cmd ~cwd:root [] in
      equal ~msg:"the raise report exits 0" int 0 code;
      contains ~msg:"the report totals the unreached out-edge"
        ~sub:"coverage: 66.7% (2/3 points)" out;
      contains ~msg:"the unreached out-edge is an uncovered line"
        ~sub:"covcli_fixture.ml   7\n" out;
      let code, out, _ = coverage_cmd ~cwd:root [ "--show-uncovered" ] in
      equal ~msg:"the raise excerpt exits 0" int 0 code;
      contains ~msg:"the excerpt paints the raising call" ~sub:"boom ()" out

(* Containment rails: JUnit and uninstrumented modes *)

let junit_rails =
  test "containment rails: JUnit and uninstrumented modes" @@ fun () ->
  (* JUnit ignores coverage: the XML carries no coverage data from an
     instrumented run. *)
  let junit = scratch "junit.xml" in
  let code, out, _, _ =
    child
      ~env:[ ("CHILD_VISITED", "9") ]
      ~args:[ "--junit"; junit; "--color"; "never" ]
      ()
  in
  equal ~msg:"an instrumented --junit run exits 0" int 0 code;
  not_contains ~msg:"the run prints no coverage line beside --junit"
    ~sub:"coverage:" out;
  let xml = read_file junit in
  contains ~msg:"the JUnit report is JUnit" ~sub:"<testsuites" xml;
  not_contains ~msg:"JUnit carries no coverage line" ~sub:"coverage:" xml;
  not_contains ~msg:"JUnit carries no coverage counts" ~sub:"points)" xml;
  (* An uninstrumented run renders nothing: no line, no empty table. *)
  let code, out, _, _ =
    child ~env:[ ("CHILD_TOTAL", "0") ] ~args:[ "--color"; "never" ] ()
  in
  equal ~msg:"an uninstrumented run exits 0" int 0 code;
  not_contains ~msg:"an uninstrumented run renders no coverage line"
    ~sub:"coverage:" out

(* The files, the machine formats and the flags, at their edges *)

(* A project of three files that each stand at an edge of the report:
   [lib/Z.ml] has no point, [lib/a.ml]'s source is missing, and
   [lib/s.ml]'s planted source is shorter than its points, so stale.
   [lib/Z.ml] sorts before [lib/a.ml] under String.compare. *)
let edges () =
  let root = scratch "edges" in
  write_file (Filename.concat root "lib/s.ml") "let x\n";
  write_file (Filename.concat root "lib/Z.ml") "";
  write_file
    (Filename.concat root "_build/_coverage/edges.coverage")
    (collection
       [
         ("lib/s.ml", bar_points, [| 1; 0 |]);
         ("lib/a.ml", ghost_points, [| 0 |]);
         ("lib/Z.ml", [||], [||]);
       ]);
  root

let edge_tests =
  [
    test "a dump is judged from its header before it is loaded" (fun () ->
        (* A leftover whose header is intact and whose records are
           corrupt: two claimed records, none written. *)
        let root = stale_root "judge-first" in
        let leftover ~exe ~digest =
          write_file
            (Filename.concat root "_build/_coverage/leftover.coverage")
            (Printf.sprintf "windtrap-coverage-v3\nexe %s %d %s\n2\n" digest
               (String.length exe) exe)
        in
        (* Of a gone executable: excluded as an orphan, its records never
           read, and the live dump's report stands. *)
        leftover ~exe:"default/test/gone.exe"
          ~digest:(Digest.to_hex (Digest.string "gone"));
        let code, out, err = coverage_cmd ~cwd:root [] in
        equal ~msg:"the live dump exits 0" int 0 code;
        contains ~msg:"the live dump is reported"
          ~sub:"coverage: 100.0% (3/3 points)" out;
        contains ~msg:"the corrupt orphan is excluded with its reason"
          ~sub:
            "leftover.coverage: its executable (default/test/gone.exe) no \
             longer exists; excluding it"
          err;
        not_contains ~msg:"its records are never read" ~sub:"corrupt" err;
        (* Of an earlier build of the executable on disk: excluded as
           stale. *)
        leftover ~exe:"default/test/a.exe"
          ~digest:(Digest.to_hex (Digest.string "an earlier build"));
        let code, out, err = coverage_cmd ~cwd:root [] in
        equal ~msg:"the live dump exits 0 again" int 0 code;
        contains ~msg:"the live dump is reported again"
          ~sub:"coverage: 100.0% (3/3 points)" out;
        contains ~msg:"the corrupt stale dump is excluded with its reason"
          ~sub:"leftover.coverage: not written by the executable now at" err;
        not_contains ~msg:"its records are never read either" ~sub:"corrupt" err;
        (* Of the executable on disk: a dump of this build must load, so its
           corruption ends the command. *)
        leftover ~exe:"default/test/a.exe"
          ~digest:(Digest.to_hex (Digest.string "the instrumented build"));
        let code, out, err = coverage_cmd ~cwd:root [] in
        equal ~msg:"a corrupt dump of this build exits 1" int 1 code;
        equal ~msg:"with no report" text "" out;
        contains ~msg:"the corrupt dump is named"
          ~sub:"leftover.coverage: corrupt" err;
        not_contains ~msg:"and never excluded" ~sub:"excluding it" err);
    test "the --expect walk skips _build and _opam" (fun () ->
        let root = proj () in
        write_file (Filename.concat root "lib/_build/built.ml") "let x = 1\n";
        write_file (Filename.concat root "lib/_opam/switch.ml") "let y = 2\n";
        let code, _, err = coverage_cmd ~cwd:root [ "--expect"; "lib" ] in
        equal ~msg:"every source the walk reaches is covered" int 0 code;
        equal ~msg:"and the two directories are never walked" text "" err);
    test "sources without data are named in path order" (fun () ->
        let root = proj () in
        write_file (Filename.concat root "lib/zeta.ml") "let z = 1\n";
        write_file (Filename.concat root "lib/alpha.ml") "let a = 1\n";
        let code, _, err =
          coverage_cmd ~cwd:root
            [ "--expect"; "lib/zeta.ml"; "--expect"; "lib/alpha.ml" ]
        in
        equal ~msg:"exit code" int 1 code;
        equal ~msg:"alpha before zeta, whatever the order of the flags" text
          "windtrap: lib/alpha.ml: expected source has no coverage data (not \
           instrumented, or linked into no test executable that ran)\n\
           windtrap: lib/zeta.ml: expected source has no coverage data (not \
           instrumented, or linked into no test executable that ran)\n"
          err);
    test "-u changes nothing under a machine format" (fun () ->
        let root = proj () in
        List.iter
          (fun format ->
            let _, plain, _ = coverage_cmd ~cwd:root [ format ] in
            let code, shown, err = coverage_cmd ~cwd:root [ format; "-u" ] in
            equal ~msg:(format ^ " -u exits 0") int 0 code;
            equal ~msg:(format ^ " -u keeps stderr empty") text "" err;
            equal ~msg:(format ^ " -u is the same document") text plain shown)
          [ "--json"; "--lcov" ]);
    test "--json at the edges: String.compare order, 100.00, no lines"
      (fun () ->
        let code, out, err = coverage_cmd ~cwd:(edges ()) [ "--json" ] in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"stderr" text "" err;
        equal
          ~msg:
            "Z before a; 100.00 for no point; no line for a missing or a stale \
             source"
          text
          "{ \"summary\": { \"visited\": 1, \"total\": 3, \"percentage\": \
           33.33 },\n\
          \  \"files\": [\n\
          \    { \"path\": \"lib/Z.ml\", \"visited\": 0, \"total\": 0,\n\
          \      \"percentage\": 100.00,\n\
          \      \"uncovered_lines\": [] },\n\
          \    { \"path\": \"lib/a.ml\", \"visited\": 0, \"total\": 1,\n\
          \      \"percentage\": 0.00,\n\
          \      \"uncovered_lines\": [] },\n\
          \    { \"path\": \"lib/s.ml\", \"visited\": 1, \"total\": 2,\n\
          \      \"percentage\": 50.00,\n\
          \      \"uncovered_lines\": [] } ] }\n"
          out;
        let root = scratch "no-point" in
        write_file
          (Filename.concat root "_build/_coverage/none.coverage")
          (collection [ ("lib/Z.ml", [||], [||]) ]);
        let _, out, _ = coverage_cmd ~cwd:root [ "--json" ] in
        contains ~msg:"a merge of no point is 100.00"
          ~sub:
            "{ \"summary\": { \"visited\": 0, \"total\": 0, \"percentage\": \
             100.00 },"
          out);
    test "--lcov leaves out a stale source and says why" (fun () ->
        let code, out, err = coverage_cmd ~cwd:(edges ()) [ "--lcov" ] in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"only the file with a current source has a record" text
          "TN:\nSF:lib/Z.ml\nLF:0\nLH:0\nend_of_record\n" out;
        equal ~msg:"the other two are named, each with its reason" text
          "windtrap: lib/a.ml: source not found; omitted from the lcov output\n\
           windtrap: lib/s.ml: the source changed since the run; omitted from \
           the lcov output\n"
          err);
    test "a stale source through the command: its row, and no excerpt"
      (fun () ->
        let root = edges () in
        let code, out, err = coverage_cmd ~cwd:root [] in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"stderr" text "" err;
        equal ~msg:"the stale row says so, the missing source says so" text
          "   cover    points   file       uncovered lines (-u shows the source)\n\
          \  100.0%    0/0      lib/Z.ml\n\
          \    0.0%    0/1      lib/a.ml   (source not found)\n\
          \   50.0%    1/2      lib/s.ml   stale: the source changed; re-run \
           the instrumented tests\n\
           coverage: 33.3% (1/3 points)\n"
          out;
        (* Under -u a heading and an excerpt go only to a file with
           uncovered lines and a source, which none of the three is. *)
        let _, out, _ = coverage_cmd ~cwd:root [ "-u" ] in
        not_contains ~msg:"no heading for any of them" ~sub:"lib/a.ml:" out;
        not_contains ~msg:"nor for the stale one" ~sub:"lib/s.ml:" out;
        not_contains ~msg:"nor for the one with no point" ~sub:"lib/Z.ml:" out);
    test "colour is WINDTRAP_COLOR's alone, refused as a runner refuses it"
      (fun () ->
        let root = proj () in
        let code, _, err = coverage_cmd ~cwd:root [ "--color"; "never" ] in
        equal ~msg:"there is no --color flag" int 2 code;
        contains ~msg:"it is an unknown option" ~sub:"unknown option '--color'"
          err;
        let colour args =
          capture ~cwd:root
            ~env:[ ("WINDTRAP_COLOR", "sometimes") ]
            windtrap_exe ("coverage" :: args)
        in
        let code, out, err = colour [] in
        equal ~msg:"a refused value is a usage error" int 2 code;
        equal ~msg:"nothing on stdout" text "" out;
        equal ~msg:"the runner's sentence" text
          "windtrap: invalid value 'sometimes' for WINDTRAP_COLOR: expected \
           always, never or auto\n"
          err;
        (* A document has no colour: the variable is not read, so a value
           the report would refuse changes nothing. *)
        List.iter
          (fun flag ->
            let code, out, err = colour [ flag ] in
            let plain_code, plain_out, plain_err =
              coverage_cmd ~cwd:root [ flag ]
            in
            equal
              ~msg:(flag ^ " exits as without the variable")
              int plain_code code;
            is_true ~msg:(flag ^ " prints a document") (out <> "");
            equal ~msg:(flag ^ " prints its document") text plain_out out;
            equal
              ~msg:(flag ^ " says what it says without it")
              text plain_err err)
          [ "--json"; "--lcov" ]);
    test "-h and -help print the help page" (fun () ->
        let _, help, _ = coverage_cmd [ "--help" ] in
        List.iter
          (fun flag ->
            let code, out, err = coverage_cmd [ flag ] in
            equal ~msg:(flag ^ " exits 0") int 0 code;
            equal ~msg:(flag ^ " is --help") text help out;
            equal ~msg:(flag ^ " says nothing else") text "" err)
          [ "-h"; "-help" ]);
  ]

(* Discovery at its edges: what the walk-up takes, what it cannot read,
   and the dumps that cannot be judged *)

let discovery_edge_tests =
  [
    test "the walk-up takes _build by that name only, and no sandbox" (fun () ->
        let root = scratch "walk-up" in
        write_file
          (Filename.concat root "_build_ci/_coverage/ci.coverage")
          (collection [ ("lib/foo.ml", foo_points, [| 1; 1; 1 |]) ]);
        mkdir_p (Filename.concat root "src");
        let code, _, err = coverage_cmd ~cwd:(Filename.concat root "src") [] in
        equal ~msg:"a private build directory is not walked up to" int 1 code;
        contains ~msg:"so nothing is found" ~sub:"no .coverage files found" err;
        (* An ancestor with a .sandbox component holds junk, the one above
           it the data. *)
        let root = proj () in
        let sandboxed = Filename.concat root ".sandbox/0abc" in
        write_file
          (Filename.concat sandboxed "_build/_coverage/junk.coverage")
          "not a coverage file\n";
        let code, out, err = coverage_cmd ~cwd:sandboxed [] in
        equal ~msg:"the sandboxed ancestor is passed over" int 0 code;
        equal ~msg:"its junk is never read" text "" err;
        contains ~msg:"and the project above it is reported"
          ~sub:"coverage: 60.0% (3/5 points)" out);
    test "what cannot be listed or inspected contributes nothing, silently"
      (fun () ->
        let root = proj () in
        let locked = Filename.concat root "_build/_coverage/locked" in
        write_file (Filename.concat locked "hidden.coverage") "not read\n";
        Unix.chmod locked 0o000;
        Unix.symlink
          (Filename.concat root "nowhere")
          (Filename.concat root "_build/_coverage/dangling.coverage");
        let code, out, err =
          Fun.protect
            ~finally:(fun () -> Unix.chmod locked 0o755)
            (fun () -> coverage_cmd ~cwd:root [])
        in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"no error and no warning" text "" err;
        contains ~msg:"the readable dumps are reported"
          ~sub:"coverage: 60.0% (3/5 points)" out);
    test "a dump that cannot be judged is kept" (fun () ->
        (* A relative identity in a file that lies in no build directory. *)
        let loose = scratch "loose.coverage" in
        write_file loose
          (collection
             ~identity:
               {
                 C.exe = "default/test/gone.exe";
                 digest = Digest.to_hex (Digest.string "gone");
               }
             [ ("lib/foo.ml", foo_points, [| 1; 1; 1 |]) ]);
        let code, out, err = coverage_cmd ~cwd:(temp_dir ()) [ loose ] in
        equal ~msg:"a relative identity outside a build directory: exit" int 0
          code;
        equal ~msg:"no warning" text "" err;
        contains ~msg:"and merged" ~sub:"coverage: 100.0% (3/3 points)" out;
        (* An executable that exists and cannot be read. *)
        let root = scratch "unreadable-exe" in
        let identity = plant_exe root "default/test/a.exe" "another build" in
        let exe = Filename.concat root "_build/default/test/a.exe" in
        write_file exe "yet another build";
        Unix.chmod exe 0o000;
        write_dump root "a.coverage" ~identity
          [ ("lib/foo.ml", foo_points, [| 1; 1; 1 |]) ];
        let code, out, err = coverage_cmd ~cwd:root [] in
        equal ~msg:"an unreadable executable: exit" int 0 code;
        equal ~msg:"no warning" text "" err;
        contains ~msg:"and merged" ~sub:"coverage: 100.0% (3/3 points)" out);
  ]

(* The command line, the gates and the merge at their edges *)

let usage_error message =
  "windtrap: " ^ message ^ "\nusage: windtrap coverage [OPTIONS] [PATH...]\n"

let lines text = String.split_on_char '\n' text

let command_edge_tests =
  [
    test "--min refuses a number outside 0 to 100" (fun () ->
        List.iter
          (fun value ->
            let code, out, err =
              coverage_cmd ~cwd:(proj ()) [ "--min"; value ]
            in
            equal ~msg:(value ^ ": exit code") int 2 code;
            equal ~msg:(value ^ ": stdout") text "" out;
            equal ~msg:(value ^ ": stderr") text
              (usage_error
                 (Printf.sprintf
                    "invalid value '%s' for --min: expected a percentage \
                     (0-100)"
                    value))
              err)
          [ "nan"; "inf"; "-inf"; "-1"; "100.5" ]);
    test "a flag that ends the line lacks its value" (fun () ->
        List.iter
          (fun flag ->
            let code, _, err = coverage_cmd ~cwd:(proj ()) [ "-u"; flag ] in
            equal ~msg:(flag ^ ": exit code") int 2 code;
            equal ~msg:(flag ^ ": stderr") text
              (usage_error
                 (Printf.sprintf "option '%s' requires an argument" flag))
              err)
          [ "--min"; "--expect"; "--do-not-expect" ]);
    test "--flag=value is split before any flag is read" (fun () ->
        let root = proj () in
        let code, _, err = coverage_cmd ~cwd:root [ "--min"; "--expect=lib" ] in
        equal ~msg:"--min takes the flag's own name" int 2 code;
        equal ~msg:"and refuses it" text
          (usage_error
             "invalid value '--expect' for --min: expected a percentage (0-100)")
          err;
        List.iter
          (fun arg ->
            let code, _, err = coverage_cmd ~cwd:root [ arg ] in
            equal ~msg:(arg ^ ": exit code") int 2 code;
            equal ~msg:(arg ^ ": stderr") text
              (usage_error (Printf.sprintf "unknown option '%s'" arg))
              err)
          [ "--json=1"; "--help=1"; "-" ]);
    test "the first argument that ends the parse decides" (fun () ->
        let root = proj () in
        let code, _, err =
          coverage_cmd ~cwd:root [ "--frobnicate"; "--help" ]
        in
        equal ~msg:"an unknown option before --help" int 2 code;
        equal ~msg:"is refused" text
          (usage_error "unknown option '--frobnicate'")
          err;
        let _, help, _ = coverage_cmd ~cwd:root [ "--help" ] in
        let code, out, _ =
          coverage_cmd ~cwd:root [ "--help"; "--frobnicate" ]
        in
        equal ~msg:"--help before an unknown option" int 0 code;
        equal ~msg:"prints the help page" text help out;
        let code, _, err =
          coverage_cmd ~cwd:root [ "--json"; "--lcov"; "--frobnicate" ]
        in
        equal ~msg:"the clash before an unknown option" int 2 code;
        equal ~msg:"is the error" text
          (usage_error "--json and --lcov each own standard output; pick one")
          err;
        let _, json, _ = coverage_cmd ~cwd:root [ "--json" ] in
        let code, out, err = coverage_cmd ~cwd:root [ "--json"; "--json" ] in
        equal ~msg:"--json twice" int 0 code;
        equal ~msg:"is --json" text json out;
        equal ~msg:"and says nothing" text "" err);
    test "a refused WINDTRAP_COLOR is said before the files are found"
      (fun () ->
        let code, out, err =
          capture ~cwd:(temp_dir ())
            ~env:[ ("WINDTRAP_COLOR", "sometimes") ]
            windtrap_exe
            [ "coverage"; scratch "absent.coverage" ]
        in
        equal ~msg:"exit code" int 2 code;
        equal ~msg:"stdout" text "" out;
        equal ~msg:"the colour sentence alone" text
          "windtrap: invalid value 'sometimes' for WINDTRAP_COLOR: expected \
           always, never or auto\n"
          err);
    test "--json escapes a recorded name" (fun () ->
        let root = scratch "json-escape" in
        write_file
          (Filename.concat root "_build/_coverage/names.coverage")
          (collection
             [
               ("lib/q\"b\\s\tt\r\nx\001\127\xc3\xa9.ml", ghost_points, [| 0 |]);
             ]);
        let code, out, err = coverage_cmd ~cwd:root [ "--json" ] in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"stderr" text "" err;
        equal
          ~msg:"quote, backslash, TAB, CR, LF and C0 escaped, the rest as is"
          text
          "{ \"summary\": { \"visited\": 0, \"total\": 1, \"percentage\": 0.00 \
           },\n\
          \  \"files\": [\n\
          \    { \"path\": \
           \"lib/q\\\"b\\\\s\\tt\\r\\nx\\u0001\127\xc3\xa9.ml\", \"visited\": \
           0, \"total\": 1,\n\
          \      \"percentage\": 0.00,\n\
          \      \"uncovered_lines\": [] } ] }\n"
          out);
    test "--lcov says an omission between the records around it" (fun () ->
        let root = scratch "lcov-order" in
        write_file (Filename.concat root "lib/a.ml") "let a = 1\n";
        write_file (Filename.concat root "lib/c.ml") "let c = 1\n";
        write_file
          (Filename.concat root "_build/_coverage/abc.coverage")
          (collection
             [
               ("lib/a.ml", ghost_points, [| 1 |]);
               ("lib/b.ml", ghost_points, [| 1 |]);
               ("lib/c.ml", ghost_points, [| 0 |]);
             ]);
        let code, out, _ =
          capture ~cwd:root "/bin/sh"
            [ "-c"; "exec \"$0\" coverage --lcov 2>&1"; windtrap_exe ]
        in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"the two streams, merged in order" text
          "TN:\n\
           SF:lib/a.ml\n\
           DA:1,1\n\
           LF:1\n\
           LH:1\n\
           end_of_record\n\
           windtrap: lib/b.ml: source not found; omitted from the lcov output\n\
           TN:\n\
           SF:lib/c.ml\n\
           DA:1,0\n\
           LF:1\n\
           LH:0\n\
           end_of_record\n"
          out);
    test "--do-not-expect is read only under --expect" (fun () ->
        let root = proj () in
        let code, _, err =
          coverage_cmd ~cwd:root [ "--do-not-expect"; "nope" ]
        in
        equal ~msg:"alone, a missing path is never looked at" int 0 code;
        equal ~msg:"and nothing is said" text "" err;
        let code, out, err =
          coverage_cmd ~cwd:root
            [ "--expect"; "lib"; "--do-not-expect"; "nope" ]
        in
        equal ~msg:"under --expect it must exist" int 1 code;
        contains ~msg:"after the report" ~sub:"coverage: 60.0%" out;
        equal ~msg:"the path is named" text
          "windtrap: nope: no such file or directory\n" err;
        let code, _, err =
          coverage_cmd ~cwd:root
            [
              "--do-not-expect";
              "gone3";
              "--expect";
              "gone1";
              "--expect";
              "gone2";
            ]
        in
        equal ~msg:"three missing paths" int 1 code;
        equal ~msg:"the first --expect is named, and no other" text
          "windtrap: gone1: no such file or directory\n" err);
    test "an expected source is named once, and matched by its stem" (fun () ->
        let root = scratch "stems" in
        List.iter
          (fun name -> write_file (Filename.concat root name) "let x = 1\n")
          [ "lib/baz.ml"; "lib/qux.ml"; "lib/extra.ml" ];
        write_file
          (Filename.concat root "_build/_coverage/stems.coverage")
          (collection
             [
               ("./lib//baz.pp.ml", ghost_points, [| 1 |]);
               ("lib\\qux.ml", ghost_points, [| 1 |]);
             ]);
        let code, _, err =
          coverage_cmd ~cwd:root
            [ "--expect"; "lib"; "--expect"; "lib/extra.ml" ]
        in
        equal ~msg:"exit code" int 1 code;
        equal ~msg:"extra.ml, once; baz and qux are covered" text
          "windtrap: lib/extra.ml: expected source has no coverage data (not \
           instrumented, or linked into no test executable that ran)\n"
          err);
    test "a merge that fails says the exclusions and the remedy first"
      (fun () ->
        let root = stale_root "merge-after-exclusion" in
        write_dump root "b.coverage"
          ~identity:
            {
              C.exe = "default/test/gone.exe";
              digest = Digest.to_hex (Digest.string "gone");
            }
          [ ("lib/ghost.ml", ghost_points, [| 1 |]) ];
        write_file
          (Filename.concat root "_build/_coverage/c.coverage")
          (collection [ ("lib/foo.ml", bar_points, [| 1; 0 |]) ]);
        let code, out, err = coverage_cmd ~cwd:root [] in
        equal ~msg:"exit code" int 1 code;
        equal ~msg:"no report" text "" out;
        match lines err with
        | [ warning; remedy; mismatch; "" ] ->
            contains ~msg:"the exclusion first"
              ~sub:"b.coverage: its executable" warning;
            is_true ~msg:"then the remedy"
              (String.starts_with ~prefix:"windtrap: re-run the suite" remedy);
            is_true ~msg:"then the merge's error"
              (String.starts_with
                 ~prefix:"windtrap: lib/foo.ml: coverage point tables disagree"
                 mismatch)
        | _ -> failf "three lines expected on stderr:\n%s" err);
    test "a dump that cannot be loaded is said alone" (fun () ->
        let root = stale_root "load-before-exclusion" in
        write_dump root "b.coverage"
          ~identity:
            {
              C.exe = "default/test/gone.exe";
              digest = Digest.to_hex (Digest.string "gone");
            }
          [ ("lib/ghost.ml", ghost_points, [| 1 |]) ];
        write_file
          (Filename.concat root "_build/_coverage/c.coverage")
          "not a coverage file\n";
        let code, out, err = coverage_cmd ~cwd:root [] in
        equal ~msg:"exit code" int 1 code;
        equal ~msg:"no report" text "" out;
        match lines err with
        | [ error; "" ] ->
            contains ~msg:"the unreadable dump is named" ~sub:"c.coverage" error
        | _ -> failf "one line expected on stderr:\n%s" err);
  ]

(* The suite *)

let () =
  exit
  @@ run "coverage_cmd"
       [
         dump_is_the_report;
         standalone_layout;
         inside_dune_estate;
         reporting_command;
         min_matrix;
         json_shape;
         lcov_output;
         expectations;
         loud_failures;
         min_boundaries;
         discovery_robustness;
         explicit_path_contract;
         staleness_pass;
         raise_attribution;
         junit_rails;
         group "edges" edge_tests;
         group "discovery edges" discovery_edge_tests;
         group "command edges" command_edge_tests;
       ]
