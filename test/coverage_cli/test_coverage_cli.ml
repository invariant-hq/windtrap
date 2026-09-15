(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for coverage's reporting surface: a real windtrap run over an
   instrumented-like executable, which prints no number of its own (the
   dump is the report, and Law-13's exit codes hold), the at_exit dump
   feeding the reporting command — under a build directory and, for a
   tree built without one, under _windtrap — and `windtrap coverage` end
   to end (walk-up discovery, merge across two executables, the
   orphan/stale matrix, --min matrix, --json shape, --show-uncovered,
   loud failures). A windtrap suite ([run] executes tests sequentially
   in declaration order); every subject under test is a spawned child
   process, so hosting the assertions under the windtrap runner nests
   nothing.

   The one thing not reproducible here: the E2 freshness behavior of the
   blessed @cover rule itself ((alias_rec runtest) + (universe)) is dune
   semantics — reproducing it needs a nested `dune build` inside this
   dune-run test, which would contend for the workspace lock. It was
   verified in the aggregation lab (rfc-v3/coverage-aggregation.md, E2);
   what this file covers instead is everything the rule's action does:
   discovery from a rule-like cwd, the staleness pass over the dumps the
   alias cannot see, the merge, and the --min gate. *)

open Windtrap
module C = Windtrap_runtime.Coverage
module I = Windtrap_runtime.Instr

let check name cond = is_true ~msg:name cond
let check_int name ~expected ~actual = equal ~msg:name int expected actual

let check_contains name ~needle haystack =
  contains ~msg:name ~sub:needle haystack

let check_absent name ~needle haystack =
  not_contains ~msg:name ~sub:needle haystack

(* Boolean containment for predicates; shadows the facade assertion,
   which the helpers above already captured. *)
let contains needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec loop i =
    if i + n > h then false
    else if String.sub haystack i n = needle then true
    else loop (i + 1)
  in
  loop 0

(* Scratch and process helpers *)

(* Hermeticity: absolute paths throughout, so the test behaves the same
   under dune's sandbox and by hand; scratch lives in a private temp
   directory removed at exit. *)
let exe_dir = Filename.dirname Sys.executable_name
let child_exe = Filename.concat exe_dir "inline_child.exe"

let windtrap_exe =
  Filename.concat exe_dir
    (Filename.concat ".." (Filename.concat ".." "bin/main.exe"))

(* Symlink-aware (the child's capture log trap plants a `latest`
   symlink): directories recurse, everything else — symlinks included —
   unlinks; never follows. *)
let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR ->
      Array.iter
        (fun name -> remove_tree (Filename.concat path name))
        (Sys.readdir path);
      Sys.rmdir path
  | _ -> Sys.remove path
  | exception Unix.Unix_error _ -> ()
  | exception Sys_error _ -> ()

let scratch_dir =
  let dir = Filename.temp_file "windtrap_cov_cli" "" in
  Sys.remove dir;
  Sys.mkdir dir 0o755;
  at_exit (fun () -> remove_tree dir);
  dir

let scratch path = Filename.concat scratch_dir path

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    Sys.mkdir dir 0o755
  end

let write_file path contents =
  mkdir_p (Filename.dirname path);
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc

let read_file path =
  match open_in_bin path with
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () -> really_input_string ic (in_channel_length ic))
  | exception Sys_error _ -> ""

(* [capture ~env ?cwd exe args] runs [exe] through the shell and returns
   (exit code, stdout, stderr). [env] entries are NAME=value words for
   env(1) — every inline_child run must carry WINDTRAP_COVERAGE_FILE so
   its at_exit dump lands in scratch, never in the real _build. *)
let run_counter = ref 0

let capture ?(env = []) ?cwd exe args =
  incr run_counter;
  let out = scratch (Printf.sprintf "out-%d.txt" !run_counter)
  and err = scratch (Printf.sprintf "err-%d.txt" !run_counter) in
  let command =
    String.concat " " (List.map Filename.quote (("env" :: env) @ (exe :: args)))
    ^ " > " ^ Filename.quote out ^ " 2> " ^ Filename.quote err
  in
  let command =
    match cwd with
    | None -> command
    | Some dir -> "cd " ^ Filename.quote dir ^ " && " ^ command
  in
  let code = Sys.command command in
  (code, read_file out, read_file err)

(* The run prints no number; the dump is the report *)

let dump_counter = ref 0

(* Each child dumps into its own fresh directory, so a run never reads
   or overwrites another's data. The child links the windtrap core,
   which under --instrument-with is itself instrumented and carries
   thousands of points; the dump is deliberately left whole — it is what
   `windtrap coverage` merges — and the assertions that read it scope
   themselves with [C.filter]. *)
let child ?(env = []) ?(args = []) () =
  incr dump_counter;
  let dump = scratch (Printf.sprintf "dump-%d/self.coverage" !dump_counter) in
  let code, out, err =
    capture ~env:(("WINDTRAP_COVERAGE_FILE=" ^ dump) :: env) child_exe args
  in
  (code, out, err, dump)

(* A dump read back for assertions: scoped to the file the test planted,
   for the same reason the run is. *)
let dump_of ?(only = "lib/fake.ml") path =
  match C.load path with
  | Error _ -> None
  | Ok (t, id) -> Some (C.filter (fun file -> file = only) t, id)

(* Six lines of nine characters: block [i] is line [i + 1]'s text. Four
   of six blocks visited leaves lines 5-6 uncovered — the shape the
   reporting command renders from this run's dump. *)
let child_source =
  "line1----\nline2----\nline3----\nline4----\nline5----\nline6----\n"

let child_src_path = scratch "child-src.ml"

let child_src_env =
  write_file child_src_path child_source;
  [
    "CHILD_FILE=" ^ child_src_path;
    "CHILD_TOTAL=6";
    "CHILD_VISITED=4";
    "CHILD_LINE_LEN=10";
  ]

let dump_is_the_report =
  test "the run prints no number, and the dump is the report" @@ fun () ->
  let code, out, _, dump =
    child ~env:child_src_env ~args:[ "--color"; "never" ] ()
  in
  check_int "an instrumented child exits 0" ~expected:0 ~actual:code;
  check_absent "the run prints no coverage line" ~needle:"coverage:" out;
  check_absent "and draws no per-file table" ~needle:"uncovered:" out;
  (* The at_exit dump of the same run is what carries the measurement,
     and names the executable that wrote it, so `windtrap coverage` can
     merge and vet it. *)
  (match dump_of ~only:child_src_path dump with
  | Some (t, exe) ->
      let s = C.summary t in
      check "the dump holds what the run measured"
        (s.C.visited = 4 && s.C.total = 6);
      check "the dump records the child executable's identity"
        (exe
        = Some
            {
              C.exe = I.exe_identity ~exe:child_exe;
              digest = Digest.to_hex (Digest.file child_exe);
            })
  | None -> check "the dump holds what the run measured" false);
  (* Law 13: coverage never changes outcomes or exit codes. *)
  let code, _, _, dump =
    child ~env:("CHILD_FAIL=1" :: child_src_env) ~args:[ "--color"; "never" ] ()
  in
  check_int "a failing instrumented run still exits 1" ~expected:1 ~actual:code;
  check "and still dumps"
    (match dump_of ~only:child_src_path dump with
    | Some (t, _) -> (C.summary t).C.total = 6
    | None -> false);
  (* An uninstrumented child registers nothing: the file may exist
     anyway, because under `--instrument-with` the windtrap core this
     child links registers and dumps. What must be true either way is
     that the child contributed nothing to it. *)
  let code, _, _, dump =
    child ~env:[ "CHILD_TOTAL=0" ] ~args:[ "--color"; "never" ] ()
  in
  check_int "an uninstrumented child exits 0" ~expected:0 ~actual:code;
  check "an uninstrumented run contributes nothing to the dump"
    (match dump_of dump with None -> true | Some (t, _) -> C.is_empty t);
  (* The retired knobs are gone: an unknown option and an unlisted
     variable, never silently ignored ones. *)
  let code, _, err, _ = child ~args:[ "--coverage"; "report" ] () in
  check_int "--coverage is no longer an option" ~expected:2 ~actual:code;
  check_contains "--coverage is reported as unknown"
    ~needle:"unknown option '--coverage'" err;
  let code, out, _, _ = child ~args:[ "--help" ] () in
  check_int "--help exits 0" ~expected:0 ~actual:code;
  check_absent "--help lists no coverage flag" ~needle:"--coverage" out;
  check_contains "--help lists the dump override"
    ~needle:"WINDTRAP_COVERAGE_FILE" out

(* A fake merged project for `windtrap coverage` *)

let foo_points =
  [|
    { C.start_ofs = 0; end_ofs = 9 };
    { C.start_ofs = 10; end_ofs = 19 };
    { C.start_ofs = 20; end_ofs = 29 };
  |]

let bar_points =
  [| { C.start_ofs = 0; end_ofs = 9 }; { C.start_ofs = 10; end_ofs = 19 } |]

let collection name adds =
  List.fold_left
    (fun acc (file, points, counts) ->
      match C.add acc ~file ~points ~counts with
      | Ok t -> t
      (* Built at module load, outside any test: a broken fixture is a
         loud crash, not an assertion. *)
      | Error _ -> failwith (name ^ ": fixture collection does not build"))
    C.empty adds

(* Two executables' worth of data: foo.ml visited [1;0;0] in one and
   [0;1;0] in the other (merge must add to 2/3, uncovered line 3);
   bar.ml only in the second (1/2, uncovered line 2). Total 3/5 = 60%. *)
let proj =
  let root = scratch "proj" in
  write_file
    (Filename.concat root "lib/foo.ml")
    "let a = 1\nlet b = 2\nlet c = 3\n";
  write_file (Filename.concat root "lib/bar.ml") "let d = 4\nlet e = 5\n";
  let a = collection "exe-a" [ ("lib/foo.ml", foo_points, [| 1; 0; 0 |]) ]
  and b =
    collection "exe-b"
      [
        ("lib/foo.ml", foo_points, [| 0; 1; 0 |]);
        ("lib/bar.ml", bar_points, [| 1; 0 |]);
      ]
  in
  write_file
    (Filename.concat root "_build/_coverage/windtrap-a.coverage")
    (C.to_string a);
  write_file
    (Filename.concat root "_build/_coverage/windtrap-b.coverage")
    (C.to_string b);
  root

(* INSIDE_DUNE is scrubbed unless the scenario sets it: `dune runtest`
   exports its own context to this suite, and the command would
   otherwise report the real build directory's estate in place of the
   scratch project's. *)
let coverage_cmd ?cwd ?inside_dune args =
  let inside_dune =
    match inside_dune with
    | Some context -> [ "INSIDE_DUNE=" ^ context ]
    | None -> [ "-u"; "INSIDE_DUNE" ]
  in
  capture
    ~env:(inside_dune @ [ "WINDTRAP_COLOR=never" ])
    ?cwd windtrap_exe ("coverage" :: args)

(* The reporting command: merge, table, walk-up *)

let reporting_command =
  test "the reporting command: merge, table, walk-up" @@ fun () ->
  let code, out, err = coverage_cmd ~cwd:proj [] in
  check_int "the merged report exits 0" ~expected:0 ~actual:code;
  check "the merged report keeps stderr empty" (err = "");
  check_contains "counts merge across executables"
    ~needle:"coverage: 60.0% (3/5 points)" out;
  check_contains "foo.ml adds counts from both executables" ~needle:"2/3" out;
  check_contains "foo.ml reports its uncovered line" ~needle:"uncovered: 3" out;
  check_contains "bar.ml reports from one executable alone" ~needle:"1/2" out;
  check_contains "bar.ml reports its uncovered line" ~needle:"uncovered: 2" out;
  check_absent "the table paints no excerpts by default" ~needle:"\u{258c}" out;
  (* Discovery walks up from a subdirectory to the project root. *)
  let code, out, _ = coverage_cmd ~cwd:(Filename.concat proj "lib") [] in
  check_int "walk-up discovery exits 0" ~expected:0 ~actual:code;
  check_contains "walk-up discovery finds the same data"
    ~needle:"coverage: 60.0% (3/5 points)" out;
  check_contains "walk-up discovery still resolves sources"
    ~needle:"uncovered: 3" out;
  (* Explicit PATH arguments replace discovery; sources then resolve
     against the current directory only. *)
  let code, out, _ =
    coverage_cmd ~cwd:scratch_dir [ Filename.concat proj "_build/_coverage" ]
  in
  check_int "an explicit PATH exits 0" ~expected:0 ~actual:code;
  check_contains "an explicit PATH merges the same data"
    ~needle:"coverage: 60.0% (3/5 points)" out;
  check_contains "unresolvable sources are named, not silently blank"
    ~needle:"(source not found)" out;
  (* Excerpts. *)
  let code, out, _ = coverage_cmd ~cwd:proj [ "--show-uncovered" ] in
  check_int "--show-uncovered exits 0" ~expected:0 ~actual:code;
  check_contains "--show-uncovered paints the uncovered arm" ~needle:"\u{258c}"
    out;
  check_contains "--show-uncovered shows the uncovered source"
    ~needle:"let c = 3" out

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
  check_int "the copied child exits 0" ~expected:0 ~actual:code;
  check_absent "and prints no coverage line" ~needle:"coverage:" out;
  let estate = Filename.concat proj "_windtrap/coverage" in
  check "the dump directory is <cwd>/_windtrap/coverage"
    (Sys.file_exists estate && Sys.is_directory estate);
  check "and no _build was grown"
    (not (Sys.file_exists (Filename.concat proj "_build")));
  let code, out, err = coverage_cmd ~cwd:proj [] in
  check_int "the reporting command finds it without arguments" ~expected:0
    ~actual:code;
  check "with no exclusion: the identity is the copy's absolute path" (err = "");
  check_contains "and reports what the run measured"
    ~needle:(Printf.sprintf "4/6  %s" child_src_path)
    out;
  (* Walk-up applies to this layout too. *)
  let sub = Filename.concat proj "lib/deep" in
  mkdir_p sub;
  let code, out, _ = coverage_cmd ~cwd:sub [] in
  check_int "walk-up finds _windtrap from a subdirectory" ~expected:0
    ~actual:code;
  check_contains "and merges the same data" ~needle:"4/6" out

(* `dune exec windtrap -- coverage` under a private --build-dir: dune
   exports the context it built in as INSIDE_DUNE, and the estate is
   that build directory's — never the _build an ancestor scan would
   find first. The same rule the core applies to its own root. *)
let inside_dune_estate =
  test "INSIDE_DUNE names the build directory whose estate is reported"
  @@ fun () ->
  let proj = scratch "private-build-dir" in
  write_file
    (Filename.concat proj "lib/foo.ml")
    "let a = 1\nlet b = 2\nlet c = 3\n";
  let shared =
    collection "exe-shared" [ ("lib/foo.ml", foo_points, [| 1; 1; 1 |]) ]
  and private_dir =
    collection "exe-private" [ ("lib/foo.ml", foo_points, [| 1; 0; 0 |]) ]
  in
  write_file
    (Filename.concat proj "_build/_coverage/windtrap-s.coverage")
    (C.to_string shared);
  write_file
    (Filename.concat proj "_build_ci/_coverage/windtrap-p.coverage")
    (C.to_string private_dir);
  let context = Filename.concat proj "_build_ci/default" in
  let code, out, err = coverage_cmd ~cwd:proj ~inside_dune:context [] in
  check_int "the command exits 0" ~expected:0 ~actual:code;
  check "and excludes nothing" (err = "");
  check_contains "the private build directory's estate, not the shared one's"
    ~needle:"coverage: 33.3% (1/3 points)" out;
  (* Without it, the ancestor scan finds the shared _build first. *)
  let _, out, _ = coverage_cmd ~cwd:proj [] in
  check_contains "unset, the scan reports the shared _build"
    ~needle:"coverage: 100.0% (3/3 points)" out;
  (* A boolean spelling — a harness's INSIDE_DUNE=1 — names no build
     directory, and the scan runs as if it were unset. *)
  let _, out, _ = coverage_cmd ~cwd:proj ~inside_dune:"1" [] in
  check_contains "a value that names no build directory is ignored"
    ~needle:"coverage: 100.0% (3/3 points)" out

(* --min matrix *)

let min_matrix =
  test "--min gates the merged percentage" @@ fun () ->
  let code, out, _ = coverage_cmd ~cwd:proj [ "--min"; "50" ] in
  check_int "--min below the total exits 0" ~expected:0 ~actual:code;
  check_contains "--min ok prints the verdict" ~needle:"minimum 50%: ok" out;
  let code, out, _ = coverage_cmd ~cwd:proj [ "--min"; "60" ] in
  check_int "--min at the total exits 0" ~expected:0 ~actual:code;
  check_contains "--min at the boundary is ok" ~needle:"minimum 60%: ok" out;
  let code, out, _ = coverage_cmd ~cwd:proj [ "--min"; "80" ] in
  check_int "--min above the total exits 1" ~expected:1 ~actual:code;
  check_contains "--min failure states the measurement and its fraction"
    ~needle:"minimum 80%: FAILED \u{2014} 60.0% (3/5 points)" out;
  let code, _, err = coverage_cmd ~cwd:proj [ "--min"; "eleventy" ] in
  check_int "a malformed --min exits 2" ~expected:2 ~actual:code;
  check_contains "a malformed --min is a usage error"
    ~needle:"invalid value 'eleventy' for --min" err;
  let code, _, err = coverage_cmd ~cwd:proj [ "--min"; "120" ] in
  check_int "an out-of-range --min exits 2" ~expected:2 ~actual:code;
  check_contains "an out-of-range --min is a usage error"
    ~needle:"expected a percentage" err

(* --json *)

(* Minimal well-formedness walk: the artifact must parse as one JSON
   value with balanced structure — shape drift or a stray comma is a
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
      | Some 't' -> pos := !pos + 4
      | Some 'f' -> pos := !pos + 5
      | Some 'n' -> pos := !pos + 4
      | _ -> fail := true
    end
  in
  value 0;
  skip_ws ();
  (not !fail) && !pos = n

let json_shape =
  test "--json emits the frozen shape" @@ fun () ->
  let code, out, err = coverage_cmd ~cwd:proj [ "--json" ] in
  check_int "--json exits 0" ~expected:0 ~actual:code;
  check "--json keeps stderr empty" (err = "");
  check "--json is well-formed" (json_well_formed out);
  (* The design-frozen shape: summary + files with path/visited/total/
     percentage/uncovered_lines (design 1c). *)
  check_contains "json: the summary object"
    ~needle:
      "\"summary\": { \"visited\": 3, \"total\": 5, \"percentage\": 60.00 }"
    out;
  check_contains "json: files carry paths" ~needle:"\"path\": \"lib/foo.ml\""
    out;
  check_contains "json: per-file counts" ~needle:"\"visited\": 2, \"total\": 3"
    out;
  check_contains "json: per-file percentage" ~needle:"\"percentage\": 66.67" out;
  check_contains "json: uncovered lines" ~needle:"\"uncovered_lines\": [3]" out;
  check_contains "json: bar.ml is present" ~needle:"\"path\": \"lib/bar.ml\""
    out;
  (* --json --min: stdout stays a pure JSON artifact. *)
  let code, out, err = coverage_cmd ~cwd:proj [ "--json"; "--min"; "80" ] in
  check_int "--json --min still gates" ~expected:1 ~actual:code;
  check "--json --min keeps stdout pure JSON" (json_well_formed out);
  check_contains "--json --min moves the verdict to stderr" ~needle:"FAILED" err

(* --expect: exhaustiveness *)

let expectations =
  test "--expect names the sources the merge must cover" @@ fun () ->
  (* The fake project's dumps, under a tree with one source the merge
     never saw (baz.ml), a preprocessed twin of one it did (foo.pp.ml),
     a lexer source whose generated module it did (bar.mll), and a
     dot-directory to skip. *)
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
  check_int "a directory with an unseen source exits 1" ~expected:1 ~actual:code;
  check_contains "the report still renders" ~needle:"coverage: 60.0%" out;
  check_contains "the unseen source is named" ~needle:"lib/baz.ml" err;
  check_contains "and the reasons it can be absent" ~needle:"not instrumented"
    err;
  check_absent "a preprocessed twin is its source" ~needle:"foo.pp.ml" err;
  check_absent "a lexer source is its generated module" ~needle:"bar.mll" err;
  check_absent "dot-directories are skipped" ~needle:"ghost.ml" err;
  let code, _, err =
    coverage_cmd ~cwd:root
      [ "--expect"; "lib"; "--do-not-expect"; "lib/baz.ml" ]
  in
  check_int "--do-not-expect exempts the unseen source" ~expected:0 ~actual:code;
  check "and nothing is warned about" (err = "");
  let code, _, _ = coverage_cmd ~cwd:root [ "--expect"; "lib/foo.ml" ] in
  check_int "a single covered file passes" ~expected:0 ~actual:code;
  let code, _, err = coverage_cmd ~cwd:root [ "--expect"; "lib/nope" ] in
  check_int "a nonexistent --expect path exits 1" ~expected:1 ~actual:code;
  check_contains "a nonexistent --expect path is named" ~needle:"lib/nope" err;
  let code, _, err = coverage_cmd ~cwd:root [ "--expect" ] in
  check_int "--expect without an argument exits 2" ~expected:2 ~actual:code;
  check_contains "--expect without an argument says so" ~needle:"--expect" err;
  (* Under a machine format stdout stays the artifact; both gates run. *)
  let code, out, err =
    coverage_cmd ~cwd:root [ "--json"; "--expect"; "lib"; "--min"; "80" ]
  in
  check_int "--json --expect --min exits 1" ~expected:1 ~actual:code;
  check "--json --expect keeps stdout pure JSON" (json_well_formed out);
  check_contains "the missing source is on stderr" ~needle:"lib/baz.ml" err;
  check_contains "and so is the --min verdict" ~needle:"FAILED" err

(* --lcov: the tracefile *)

let lcov_output =
  test "--lcov emits a tracefile" @@ fun () ->
  let code, out, err = coverage_cmd ~cwd:proj [ "--lcov" ] in
  check_int "--lcov exits 0" ~expected:0 ~actual:code;
  check "--lcov keeps stderr empty" (err = "");
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
  check_int "--lcov --min still gates" ~expected:1 ~actual:code;
  check_absent "--lcov --min keeps stdout pure" ~needle:"minimum" out;
  check_contains "--lcov --min moves the verdict to stderr" ~needle:"FAILED" err;
  (* Two owners of stdout is a usage error. *)
  let code, _, err = coverage_cmd ~cwd:proj [ "--lcov"; "--json" ] in
  check_int "--lcov --json exits 2" ~expected:2 ~actual:code;
  check_contains "--lcov --json names the clash" ~needle:"--lcov" err;
  (* A file whose source is missing is omitted and named, never painted. *)
  let orphan = scratch "lcov-orphan" in
  write_file
    (Filename.concat orphan "_build/_coverage/x.coverage")
    (C.to_string (collection "x" [ ("lib/gone.ml", bar_points, [| 1; 0 |]) ]));
  let code, out, err = coverage_cmd ~cwd:orphan [ "--lcov" ] in
  check_int "a missing source still exits 0" ~expected:0 ~actual:code;
  check_absent "a missing source has no record" ~needle:"SF:" out;
  check_contains "a missing source is named on stderr" ~needle:"lib/gone.ml" err

(* Loud failures *)

let loud_failures =
  test "failures are loud: no data, corrupt data, usage errors" @@ fun () ->
  (* Nothing to report. *)
  let empty = scratch "empty-root" in
  mkdir_p empty;
  let code, _, err = coverage_cmd ~cwd:empty [] in
  check_int "no .coverage files exit 1" ~expected:1 ~actual:code;
  check_contains "no files: the hint names the backend, not a build tool"
    ~needle:"ppx_windtrap.coverage" err;
  check_absent "and spells no dune command" ~needle:"dune " err;
  (* Corrupt and foreign files are rejected loudly (Law 15). *)
  let corrupt = scratch "corrupt/_build/_coverage/bad.coverage" in
  write_file corrupt "not a coverage file\n";
  let code, _, err = coverage_cmd ~cwd:(scratch "corrupt") [] in
  check_int "a corrupt file exits 1" ~expected:1 ~actual:code;
  check_contains "a corrupt file is named" ~needle:"bad.coverage" err;
  let v1 = scratch "v1/_build/_coverage/old.coverage" in
  write_file v1 "WINDTRAP-COVERAGE-1\nsome v1 payload\n";
  let code, _, err = coverage_cmd ~cwd:(scratch "v1") [] in
  check_int "a v1-format file exits 1" ~expected:1 ~actual:code;
  check_contains "a v1-format file is named" ~needle:"old.coverage" err;
  check_contains
    "a foreign format instructs deletion (re-running cannot remove it)"
    ~needle:"delete" err;
  (* Mismatched point tables across executables. *)
  let mismatch = scratch "mismatch" in
  let one =
    collection "mismatch-one" [ ("lib/foo.ml", foo_points, [| 1; 0; 0 |]) ]
  and two =
    collection "mismatch-two" [ ("lib/foo.ml", bar_points, [| 1; 0 |]) ]
  in
  write_file
    (Filename.concat mismatch "_build/_coverage/one.coverage")
    (C.to_string one);
  write_file
    (Filename.concat mismatch "_build/_coverage/two.coverage")
    (C.to_string two);
  let code, _, err = coverage_cmd ~cwd:mismatch [] in
  check_int "mismatched point tables exit 1" ~expected:1 ~actual:code;
  check_contains "mismatched point tables name the file" ~needle:"lib/foo.ml"
    err;
  check_contains "the mismatch hint is a full re-run, not deletion first"
    ~needle:"from one build" err;
  (* Usage errors. *)
  let code, _, err = coverage_cmd ~cwd:proj [ "--frobnicate" ] in
  check_int "an unknown option exits 2" ~expected:2 ~actual:code;
  check_contains "an unknown option is named"
    ~needle:"unknown option '--frobnicate'" err;
  let code, out, _ = coverage_cmd ~cwd:proj [ "--help" ] in
  check_int "coverage --help exits 0" ~expected:0 ~actual:code;
  check_contains "coverage --help documents --min" ~needle:"--min PCT" out;
  (* Top-level dispatch. *)
  let code, _, err = capture windtrap_exe [] in
  check_int "no command exits 2" ~expected:2 ~actual:code;
  check_contains "no command prints usage" ~needle:"usage: windtrap" err;
  let code, _, err = capture windtrap_exe [ "frobnicate" ] in
  check_int "an unknown command exits 2" ~expected:2 ~actual:code;
  check_contains "an unknown command is named"
    ~needle:"unknown command 'frobnicate'" err;
  let code, out, _ = capture windtrap_exe [ "--help" ] in
  check_int "windtrap --help exits 0" ~expected:0 ~actual:code;
  check_contains "windtrap --help lists the subcommand" ~needle:"coverage" out

(* --min boundaries *)

let min_boundaries =
  test "--min boundaries and spellings" @@ fun () ->
  (* The 0 and 100 rails. *)
  let code, out, _ = coverage_cmd ~cwd:proj [ "--min"; "0" ] in
  check_int "--min 0 always passes" ~expected:0 ~actual:code;
  check_contains "--min 0 prints its verdict" ~needle:"minimum 0%: ok" out;
  let code, out, _ = coverage_cmd ~cwd:proj [ "--min"; "100" ] in
  check_int "--min 100 fails below full coverage" ~expected:1 ~actual:code;
  check_contains "--min 100 states the shortfall"
    ~needle:"minimum 100%: FAILED \u{2014} 60.0% (3/5 points)" out;
  let full = scratch "fullproj" in
  let all =
    collection "full"
      [
        ("lib/foo.ml", foo_points, [| 1; 1; 1 |]);
        ("lib/bar.ml", bar_points, [| 2; 1 |]);
      ]
  in
  write_file
    (Filename.concat full "_build/_coverage/full.coverage")
    (C.to_string all);
  let code, out, _ = coverage_cmd ~cwd:full [ "--min"; "100" ] in
  check_int "--min 100 passes at exactly 100%" ~expected:0 ~actual:code;
  check_contains "full coverage meets the 100% gate" ~needle:"minimum 100%: ok"
    out;
  (* The --min=PCT spelling, including the empty value. *)
  let code, out, _ = coverage_cmd ~cwd:proj [ "--min=50" ] in
  check_int "--min=PCT equals the two-word form" ~expected:0 ~actual:code;
  check_contains "--min=PCT prints its verdict" ~needle:"minimum 50%: ok" out;
  let code, _, err = coverage_cmd ~cwd:proj [ "--min=" ] in
  check_int "an empty --min= exits 2" ~expected:2 ~actual:code;
  check_contains "an empty --min= is an invalid value, not an unknown option"
    ~needle:"invalid value '' for --min" err;
  (* The gate compares raw percentages, not their renderings: 2/3 rounds
     to the 66.7 it is gated against and still falls short. The verdict
     makes no comparative claim, so the fraction is what says why. *)
  let thirds = scratch "twothirds" in
  let two_of_three =
    collection "two-thirds" [ ("lib/foo.ml", foo_points, [| 1; 1; 0 |]) ]
  in
  write_file
    (Filename.concat thirds "_build/_coverage/t.coverage")
    (C.to_string two_of_three);
  let code, out, _ = coverage_cmd ~cwd:thirds [ "--min"; "66.7" ] in
  check_int "the gate compares raw percentages" ~expected:1 ~actual:code;
  check_contains "a display-equal shortfall still fails, with its fraction"
    ~needle:"minimum 66.7%: FAILED \u{2014} 66.7% (2/3 points)" out

(* Discovery and merge robustness *)

let discovery_robustness =
  test "discovery and merge robustness" @@ fun () ->
  (* An existing but empty _build/_coverage is "no files", loudly. *)
  let bare = scratch "bare" in
  mkdir_p (Filename.concat bare "_build/_coverage");
  let code, _, err = coverage_cmd ~cwd:bare [] in
  check_int "an empty _build/_coverage exits 1" ~expected:1 ~actual:code;
  check_contains "an empty _build/_coverage prints the no-files hint"
    ~needle:"no .coverage files found" err;
  (* A truncated file is corrupt and named, never partially merged. *)
  let serialized =
    C.to_string
      (collection "trunc" [ ("lib/foo.ml", foo_points, [| 1; 0; 0 |]) ])
  in
  let cut = scratch "trunc/_build/_coverage/cut.coverage" in
  write_file cut (String.sub serialized 0 (String.length serialized - 4));
  let code, _, err = coverage_cmd ~cwd:(scratch "trunc") [] in
  check_int "a truncated file exits 1" ~expected:1 ~actual:code;
  check_contains "a truncated file is named" ~needle:"cut.coverage" err;
  check_contains "a truncated file is called corrupt" ~needle:"corrupt" err;
  (* A dump recorded as written by the reporting binary itself gets no
     special treatment: it is judged by its identity like any other —
     here the recorded executable does not exist under this root, so
     it is an orphan, excluded and named. *)
  let selfish = scratch "selfish" in
  write_file
    (Filename.concat selfish "_build/_coverage/self.coverage")
    (C.to_string
       ~identity:
         {
           C.exe = I.exe_identity ~exe:windtrap_exe;
           digest = Digest.to_hex (Digest.string "some earlier build");
         }
       (collection "self" [ ("lib/ghost.ml", foo_points, [| 1; 1; 1 |]) ]));
  let code, out, err = coverage_cmd ~cwd:selfish [] in
  check_int "a directory holding only the reporter's own dump exits 1"
    ~expected:1 ~actual:code;
  check_contains "the dump is excluded like any other orphan"
    ~needle:"self.coverage" err;
  check_contains "and the run says everything was excluded"
    ~needle:"every .coverage file was excluded" err;
  check_absent "and nothing is merged" ~needle:"ghost.ml" out;
  (* An explicit .coverage FILE argument is honored as-is. *)
  let code, out, _ =
    coverage_cmd ~cwd:scratch_dir
      [ Filename.concat proj "_build/_coverage/windtrap-a.coverage" ]
  in
  check_int "an explicit file argument exits 0" ~expected:0 ~actual:code;
  check_contains "an explicit file argument reports its data alone"
    ~needle:"coverage: 33.3% (1/3 points)" out;
  (* A rule-action cwd — inside _build — resolves the root by the
     topmost-_build rule (the runtime's), never the ancestor scan. *)
  mkdir_p (Filename.concat proj "_build/default/examples");
  let code, out, _ =
    coverage_cmd ~cwd:(Filename.concat proj "_build/default/examples") []
  in
  check_int "a cwd inside _build exits 0" ~expected:0 ~actual:code;
  check_contains "a cwd inside _build resolves the workspace root"
    ~needle:"coverage: 60.0% (3/5 points)" out;
  check_contains "sources resolve from that root too" ~needle:"uncovered: 3" out;
  (* E2's trap: v1 garbage planted at _build/.sandbox/_build/_coverage
     must not capture discovery from a sandboxed action's cwd — the
     topmost _build wins. *)
  write_file
    (Filename.concat proj "_build/.sandbox/_build/_coverage/junk.coverage")
    "WINDTRAP-COVERAGE-1\nleftover\n";
  mkdir_p (Filename.concat proj "_build/.sandbox/0abc/default");
  let code, out, err =
    coverage_cmd ~cwd:(Filename.concat proj "_build/.sandbox/0abc/default") []
  in
  check_int "a sandboxed cwd escapes planted garbage" ~expected:0 ~actual:code;
  check_contains "a sandboxed cwd reports the workspace data"
    ~needle:"coverage: 60.0% (3/5 points)" out;
  check_absent "the planted v1 file is never read" ~needle:"junk.coverage" err

(* Explicit PATH arguments are a contract (qa-jul29 F-3) *)

let explicit_path_contract =
  test "explicit PATH arguments are loud when invalid" @@ fun () ->
  (* A nonexistent explicit path is an error naming the path and the
     reason — never a silent drop into the no-data report, whose
     instrument-your-library remedy would be wrong here. *)
  let absent = scratch "no-such-dir/absent.coverage" in
  let code, _, err = coverage_cmd ~cwd:scratch_dir [ absent ] in
  check_int "a missing explicit path exits 1" ~expected:1 ~actual:code;
  check_contains "a missing explicit path is named" ~needle:absent err;
  check_contains "a missing explicit path states the reason"
    ~needle:"no such file or directory" err;
  check_absent "a missing explicit path never blames instrumentation"
    ~needle:"Instrument the library" err;
  (* An existing file without the .coverage suffix — a renamed dump —
     is equally loud, whatever its content. *)
  let renamed = scratch "renamed.cov" in
  write_file renamed
    (C.to_string
       (collection "renamed" [ ("lib/foo.ml", foo_points, [| 1; 0; 0 |]) ]));
  let code, _, err = coverage_cmd ~cwd:scratch_dir [ renamed ] in
  check_int "a wrong-suffix explicit file exits 1" ~expected:1 ~actual:code;
  check_contains "a wrong-suffix explicit file is named" ~needle:renamed err;
  check_contains "a wrong-suffix explicit file states the reason"
    ~needle:"not a .coverage file" err;
  check_absent "a wrong-suffix explicit file never blames instrumentation"
    ~needle:"Instrument the library" err;
  (* An invalid path beside a valid one still fails the invocation:
     explicit arguments never narrow silently. *)
  let valid = Filename.concat proj "_build/_coverage/windtrap-a.coverage" in
  let code, _, err = coverage_cmd ~cwd:scratch_dir [ valid; absent ] in
  check_int "one bad path fails the whole invocation" ~expected:1 ~actual:code;
  check_contains "the bad path is the one named" ~needle:absent err;
  (* Directory arguments keep the scan's tolerance: an existing
     directory holding no dumps falls through to the no-data report. *)
  let empty_dir = scratch "explicit-empty" in
  mkdir_p empty_dir;
  let code, _, err = coverage_cmd ~cwd:scratch_dir [ empty_dir ] in
  check_int "an empty explicit directory exits 1" ~expected:1 ~actual:code;
  check_contains "an empty explicit directory is a no-data report"
    ~needle:"no .coverage files found" err

(* The staleness pass: orphaned and outdated dumps *)

(* The holes the @cover alias cannot see (aggregation design, E5): a
   dump whose executable was deleted (orphan — silently inflates the
   merge) and a dump whose executable is not the one now on disk — a
   re-run without --instrument-with (wrote nothing fresh), or a test
   action dune replayed from cache after sources reverted to an
   already-tested state (measured against the blessed alias: the dump
   stays a different build's, and plain re-runs stay cache hits, so
   only `--force` heals it). Both are detected from the recorded
   identity; staleness is a content comparison — the recorded digest
   against the executable now on disk — because dune's cache restores
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
    (C.to_string ~identity (collection name adds))

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
  (* Fresh: the executable on disk is the dump's writer — full
     inclusion. *)
  let root = stale_root "stale-fresh" in
  let code, out, err = coverage_cmd ~cwd:root [] in
  check_int "a fresh identity-carrying dump exits 0" ~expected:0 ~actual:code;
  check "a fresh identity-carrying dump warns about nothing" (err = "");
  check_contains "a fresh identity-carrying dump merges"
    ~needle:"coverage: 100.0% (3/3 points)" out;
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
  check_int "an orphaned dump still reports the live data" ~expected:0
    ~actual:code;
  check_contains "the orphan is excluded from the merge"
    ~needle:"coverage: 100.0% (3/3 points)" out;
  check_absent "the orphan's files stay out of the table" ~needle:"ghost.ml" out;
  check_contains "the orphan warning names the dump" ~needle:"gone.coverage" err;
  check_contains "the orphan warning names the missing executable"
    ~needle:"default/test/gone.exe" err;
  check_contains "the orphan warning says what it did" ~needle:"excluding it"
    err;
  (* Stale: the executable was rebuilt since the dump — its content no
     longer matches the recorded digest (its mtime is irrelevant). *)
  let root = stale_root "stale-rebuilt" in
  ignore (plant_exe root "default/test/a.exe" "an uninstrumented rebuild");
  let code, _, err = coverage_cmd ~cwd:root [] in
  check_int "a lone stale dump exits 1 (nothing left to report)" ~expected:1
    ~actual:code;
  check_contains "the stale warning names the dump" ~needle:"a.coverage" err;
  check_contains "the stale warning says the executable was rebuilt"
    ~needle:"rebuilt since" err;
  check_contains "the remedy is an instrumented re-run"
    ~needle:"re-run the suite instrumented" err;
  check_contains "the remedy names the cached-run cause" ~needle:"cached" err;
  check_absent "and spells no dune command" ~needle:"dune " err;
  check_contains "excluding everything is loud"
    ~needle:"every .coverage file was excluded" err;
  (* Stale beside fresh — the revert trap, measured against the blessed
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
  check_int "a stale dump beside a fresh one exits 0" ~expected:0 ~actual:code;
  check_contains "the fresh data still gates alone"
    ~needle:"coverage: 100.0% (3/3 points)" out;
  check_absent "the stale dump's files stay out of the table" ~needle:"ghost.ml"
    out;
  check_contains "the partial-exclusion warning names the dump"
    ~needle:"b.coverage" err;
  check_contains "the partial-exclusion remedy is the same sentence"
    ~needle:"then merge again" err;
  check_int "said once" ~expected:1
    ~actual:
      (List.length
         (List.filter
            (fun line -> contains "then merge again" line)
            (String.split_on_char '\n' err)));
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
  check_contains "a missing absolute identity is an orphan"
    ~needle:"no-such-exe" err;
  check_absent "the absolute orphan is excluded" ~needle:"ghost.ml" out;
  (* Usage rail: the flag is gone, and an unknown flag is a usage
     error, never a silently ignored argument. *)
  let code, _, err = coverage_cmd ~cwd:root [ "--stale=include" ] in
  check_int "--stale is no longer an option" ~expected:2 ~actual:code;
  check_contains "--stale is reported as unknown"
    ~needle:"unknown option '--stale=include'" err

(* Raise attribution end to end (Law 14 as amended) *)

(* The one genuinely instrumented path in this test: raise_child drives
   the covcli_fixture library - instrumented by the real PPX - through a
   real windtrap run. Its raising call's out-edge can never fire, so the
   dump and the CLI report must both show 2/3 points with the call line
   uncovered: a raising path lowers the percentage. *)
let raise_child_exe = Filename.concat exe_dir "raise_child.exe"

let raise_attribution =
  test "raise attribution end to end" @@ fun () ->
  incr dump_counter;
  let dump = scratch (Printf.sprintf "dump-%d.coverage" !dump_counter) in
  let fixture = "test/coverage_cli/covcli_fixture.ml" in
  let code, out, _ =
    capture
      ~env:[ "WINDTRAP_COVERAGE_FILE=" ^ dump ]
      raise_child_exe [ "--color"; "never" ]
  in
  check_int "the raise child exits 0" ~expected:0 ~actual:code;
  check_absent "the run prints no number of its own" ~needle:"coverage:" out;
  match dump_of ~only:fixture dump with
  | None -> check "the raise child's dump loads" false
  | Some (t, _) -> (
      let s = C.summary t in
      check "exactly one point - the out-edge - is unvisited"
        (s.C.total = 3 && s.C.visited = 2);
      match C.file_reports t with
      | [ r ] ->
          check_int "one uncovered extent: the raising call" ~expected:1
            ~actual:(List.length r.C.uncovered_extents);
          (* The fixture source is a declared test dep, copied beside the
             executable — resolved absolutely so a by-hand run from
             anywhere in the checkout reads it too. *)
          let source =
            read_file (Filename.concat exe_dir "covcli_fixture.ml")
          in
          check "the fixture source is a test dep" (source <> "");
          check "the uncovered extent is the call line (line 7)"
            (C.lines_of_extents ~source r.C.uncovered_extents = [ 7 ]);
          (* Replant source and dump in a scratch project: the reporting
             command must attribute the unreached out-edge to the call
             line. *)
          let root = scratch "raiseproj" in
          write_file (Filename.concat root r.C.file) source;
          write_file
            (Filename.concat root "_build/_coverage/raise.coverage")
            (C.to_string t);
          let code, out, _ = coverage_cmd ~cwd:root [] in
          check_int "the raise report exits 0" ~expected:0 ~actual:code;
          check_contains "the report totals the unreached out-edge"
            ~needle:"coverage: 66.7% (2/3 points)" out;
          check_contains "the unreached out-edge is an uncovered line"
            ~needle:"uncovered: 7" out;
          let code, out, _ = coverage_cmd ~cwd:root [ "--show-uncovered" ] in
          check_int "the raise excerpt exits 0" ~expected:0 ~actual:code;
          check_contains "the excerpt paints the raising call" ~needle:"boom ()"
            out
      | reports ->
          check
            (Printf.sprintf "exactly one instrumented file, got %d"
               (List.length reports))
            false)

(* Law 12/13 rails: JUnit and uninstrumented modes *)

let junit_rails =
  test "Law 12/13 rails: JUnit and uninstrumented modes" @@ fun () ->
  (* JUnit ignores coverage: the XML carries no coverage data from an
     instrumented run. *)
  let junit = scratch "junit.xml" in
  let code, out, _, _ =
    child ~env:[ "CHILD_VISITED=9" ]
      ~args:[ "--junit"; junit; "--color"; "never" ]
      ()
  in
  check_int "an instrumented --junit run exits 0" ~expected:0 ~actual:code;
  check_absent "the run prints no coverage line beside --junit"
    ~needle:"coverage:" out;
  let xml = read_file junit in
  check "the JUnit report was written" (xml <> "");
  check_contains "the JUnit report is JUnit" ~needle:"<testsuites" xml;
  check_absent "JUnit carries no coverage line" ~needle:"coverage:" xml;
  check_absent "JUnit carries no coverage counts" ~needle:"points)" xml;
  (* An uninstrumented run renders nothing — no line, no empty table. *)
  let code, out, _, _ =
    child ~env:[ "CHILD_TOTAL=0" ] ~args:[ "--color"; "never" ] ()
  in
  check_int "an uninstrumented run exits 0" ~expected:0 ~actual:code;
  check_absent "an uninstrumented run renders no coverage line"
    ~needle:"coverage:" out

(* The suite *)

let () =
  exit
  @@ run "coverage_cli"
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
       ]
