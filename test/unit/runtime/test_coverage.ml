(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Coverage = Windtrap_runtime.Coverage
module Instr = Windtrap_runtime.Instr
module Child = Windtrap_test_support.Child

let strf = Printf.sprintf
let read path = In_channel.with_open_bin path In_channel.input_all

let write path contents =
  Out_channel.with_open_bin path (fun oc -> output_string oc contents)

let rec mkdir_p dir =
  if not (Sys.file_exists dir) then begin
    mkdir_p (Filename.dirname dir);
    Sys.mkdir dir 0o755
  end

let put root file contents =
  let path = Filename.concat root file in
  mkdir_p (Filename.dirname path);
  write path contents

let pt start_ofs end_ofs = { Coverage.start_ofs; end_ofs }
let extent (p : Coverage.point) = strf "%d-%d" p.start_ofs p.end_ofs

let summary =
  Testable.make
    ~pp:(fun ppf (s : Coverage.summary) ->
      Format.fprintf ppf "%d/%d" s.visited s.total)
    ~equal:(fun (a : Coverage.summary) (b : Coverage.summary) ->
      a.visited = b.visited && a.total = b.total)

let identity =
  Testable.contramap
    (fun (i : Coverage.identity) -> (i.exe, i.digest))
    (pair string string)

(* Dumps written by hand *)

(* A dump in the grammar of the runtime's own: the header of
   [Instr.add_header], the file count, then for each file its name, its point
   count and one [start end count] line per point. *)
let dump ?identity files =
  let b = Buffer.create 128 in
  Instr.add_header Coverage.format b identity;
  Printf.bprintf b "%d\n" (List.length files);
  let add_file (file, rows) =
    Printf.bprintf b "%d %s\n%d\n" (String.length file) file (List.length rows);
    List.iter
      (fun ((p : Coverage.point), count) ->
        Printf.bprintf b "%d %d %d\n" p.start_ofs p.end_ofs count)
      rows
  in
  List.iter add_file files;
  Buffer.contents b

let load_text text =
  let path = Filename.concat (temp_dir ()) "fixture.coverage" in
  write path text;
  Coverage.load path

let collection files =
  fst (require_ok ~pp:Coverage.pp_error (load_text (dump files)))

(* What a load gave, as a row a failure prints. *)
let loaded = function
  | Ok (t, _) -> "files: " ^ String.concat ", " (Coverage.files t)
  | Error (Coverage.Data (Instr.Unknown_format _)) -> "unknown format"
  | Error (Data (Unreadable _)) -> "unreadable"
  | Error (Data (Corrupt _)) -> "corrupt"
  | Error (Point_mismatch { file }) -> "point mismatch in " ^ file

(* [line n] is the extent of the [n]th line of a source of five-byte lines,
   which [hits] writes for every file, so that the hits of a line are the count
   of its one point. *)
let line n = pt (6 * (n - 1)) ((6 * (n - 1)) + 5)

let hits t =
  let root = temp_dir () in
  let source = String.concat "" (List.init 9 (fun _ -> "xxxxx\n")) in
  List.iter (fun file -> put root file source) (Coverage.files t);
  List.map
    (fun (r : Coverage.file_report) -> (r.file, r.line_hits))
    (Coverage.file_reports ~source_roots:[ root ] t)

let hits_t = list (pair string (list (pair int int)))

(* Two files, dumped in the reverse of the order of their names. *)
let ab () =
  collection
    [
      ("lib/b.ml", [ (line 1, 1); (line 2, 0) ]); ("lib/a.ml", [ (line 1, 7) ]);
    ]

(* Points and instrumentation *)

(* The registry is global to the process, and the mutation loop runs a test
   again in forks of a process that already ran it, so a test registers under
   a name no earlier run of it used. *)
let fresh =
  let n = ref 0 in
  fun base ->
    incr n;
    strf "%s_%d.ml" base !n

let conflict_warning file =
  strf
    "windtrap: warning: %s: conflicting instrumentation tables in one \
     executable (stale build artifacts? rebuild from clean); ignoring one \
     module's coverage data\n"
    file

let differing_table_warns () =
  let file = fresh "differing" in
  Coverage.register ~file ~points:[| pt 6000 6010 |] ~counts:(Array.make 1 0);
  ignore (output ());
  Coverage.register ~file ~points:[| pt 6000 6020 |] ~counts:(Array.make 1 0);
  equal string (conflict_warning file) (output ())

let child_exe =
  Filename.concat
    (Filename.dirname Windtrap_runtime.Instr.executable)
    "coverage_child.exe"

let exe_line exe =
  let id = Instr.exe_identity ~exe in
  strf "exe %s %d %s\n" (Digest.to_hex (Digest.file exe)) (String.length id) id

(* The bytes of a dump of [lib/child.ml] with [counts], and the identity of
   [exe] when the writer could read itself back at exit. *)
let child_dump ?exe counts =
  "windtrap-coverage-v3\n"
  ^ Option.fold ~none:"" ~some:exe_line exe
  ^ strf "1\n12 lib/child.ml\n3\n0 5 %d\n6 11 %d\n12 17 %d\n" counts.(0)
      counts.(1) counts.(2)

let run_child ?cwd ?(exe = child_exe) ~dump args =
  Child.run ?cwd ~env:[ ("WINDTRAP_COVERAGE_FILE", dump) ] exe args

(* The dump that [mode] writes under the variable, from a child that exited
   0. *)
let dumped mode =
  let dump = Filename.concat (temp_dir ()) "child.coverage" in
  equal int 0 (Child.exit_code (run_child ~dump [ mode ]));
  dump

let child_conflict () =
  let dump = Filename.concat (temp_dir ()) "child.coverage" in
  let r = run_child ~dump [ "conflict" ] in
  equal int 0 (Child.exit_code r);
  equal string (conflict_warning "lib/child.ml") r.err;
  equal string (child_dump ~exe:child_exe [| 1; 0; 0 |]) (read dump)

let duplicate_counts_once () =
  let t, _ =
    require_ok ~pp:Coverage.pp_error (Coverage.load (dumped "duplicate"))
  in
  equal summary { Coverage.visited = 1; total = 3 } (Coverage.summary t)

let points_and_instrumentation =
  group "Points and instrumentation"
    [
      cases "register refuses a table no instrumenter emits" ~name:fst
        [
          ("counts of another length", ([| pt 0 1 |], Array.make 2 0));
          ("an inverted extent", ([| pt 5 3 |], [| 0 |]));
          ("a negative offset", ([| pt (-1) 3 |], [| 0 |]));
          ("a negative count", ([| pt 0 1 |], [| -1 |]));
        ]
        (fun (_, (points, counts)) ->
          raises_match Exn.invalid_arg (fun () ->
              Coverage.register ~file:(fresh "refused") ~points ~counts));
      test "visit adds one to a count, which saturates at max_int" (fun () ->
          let counts = [| 0; max_int - 1; max_int |] in
          Array.iteri (fun i _ -> Coverage.visit counts i) counts;
          equal (array int) [| 1; max_int; max_int |] counts);
      test "visit refuses an index outside the counts" (fun () ->
          raises_match Exn.invalid_arg (fun () ->
              Coverage.visit (Array.make 1 0) 1));
      test "a table that differs from the file's first is a warning"
        differing_table_warns;
      test
        "the dump holds the first of two differing tables, and the process \
         exits 0"
        child_conflict;
      test "a file registered again with an equal table adds up its counts"
        (fun () ->
          equal string
            (child_dump ~exe:child_exe [| 2; 0; 0 |])
            (read (dumped "duplicate")));
      test "a file registered again with an equal table counts its points once"
        duplicate_counts_once;
      test "a file of no point is dumped, in the order of names" (fun () ->
          equal string
            ("windtrap-coverage-v3\n" ^ exe_line child_exe
           ^ "2\n\
              12 lib/child.ml\n\
              3\n\
              0 5 1\n\
              6 11 0\n\
              12 17 0\n\
              11 lib/zero.ml\n\
              0\n")
            (read (dumped "files")));
    ]

(* Collections *)

(* A drawn collection holds some of three names, each with one of three
   tables and its counts. Two tables have one length, so that a mismatch is
   not only one of lengths. Counts near [max_int] make sums saturate. *)
let names = [ "lib/a.ml"; "lib/b.ml"; "lib/c.ml" ]

let tables =
  [|
    [ line 1; line 2; line 3 ]; [ line 1; line 2 ]; [ line 1; line 2; pt 12 16 ];
  |]

let pp_entry ppf (file, table, counts) =
  Format.fprintf ppf "%s table %d [%s]" file table
    (String.concat "; " (List.map string_of_int counts))

let pp_drawn ppf entries =
  Format.fprintf ppf "{%s}"
    (String.concat ", " (List.map (Format.asprintf "%a" pp_entry) entries))

let drawn =
  let count =
    Gen.frequency
      [
        (1, Gen.int_range 0 9);
        (1, Gen.map (fun k -> max_int - k) (Gen.int_range 0 9));
      ]
  in
  let entry =
    let open Gen in
    let* table =
      frequency [ (6, constant 0); (1, constant 1); (1, constant 2) ]
    in
    let+ counts = list ~size:(constant (List.length tables.(table))) count in
    (table, counts)
  in
  let open Gen in
  with_pp pp_drawn
    (let+ a = option entry and+ b = option entry and+ c = option entry in
     List.filter_map
       (fun (file, e) ->
         Option.map (fun (table, counts) -> (file, table, counts)) e)
       (List.combine names [ a; b; c ]))

(* The dump of a drawn collection lists its files in the reverse of the order
   of their names. *)
let of_drawn entries =
  collection
    (List.rev_map
       (fun (file, table, counts) -> (file, List.combine tables.(table) counts))
       entries)

let sat_add x y = if x > max_int - y then max_int else x + y

let drawn_hits (file, _, counts) =
  (file, List.mapi (fun i c -> (i + 1, c)) counts)

(* The spec of [merge] over drawn collections. *)
let model_merge a b =
  let differs (file, table, _) =
    List.exists (fun (f, t, _) -> String.equal f file && t <> table) a
  in
  match List.find_opt differs b with
  | Some (file, _, _) -> Error file
  | None ->
      let merged file =
        match
          ( List.find_opt (fun (f, _, _) -> String.equal f file) a,
            List.find_opt (fun (f, _, _) -> String.equal f file) b )
        with
        | Some (_, table, x), Some (_, _, y) ->
            Some (file, table, List.map2 sat_add x y)
        | (Some _ as e), None | None, (Some _ as e) -> e
        | None, None -> None
      in
      Ok (List.map drawn_hits (List.filter_map merged names))

let merged r =
  match r with
  | Ok t -> Ok (hits t)
  | Error (Coverage.Point_mismatch { file }) -> Error file
  | Error (Data _ as e) -> Error (Format.asprintf "%a" Coverage.pp_error e)

let merged_t = result hits_t string

(* The pairs of entries of one name in [a] and [b]. *)
let shared a b =
  List.concat_map
    (fun (f, t, x) ->
      List.filter_map
        (fun (g, u, y) ->
          if String.equal f g then Some ((t, x), (u, y)) else None)
        a)
    b

let merge_law (a, b) =
  let expected = model_merge a b in
  let pairs = shared a b in
  let saturates ((_, x), (_, y)) =
    List.length x = List.length y
    && List.exists2 (fun x y -> x > max_int - y) x y
  in
  cover "files both hold add up" (Result.is_ok expected && pairs <> []);
  cover "a sum saturates" (Result.is_ok expected && List.exists saturates pairs);
  cover "two names disagree"
    (List.length (List.filter (fun ((t, _), (u, _)) -> t <> u) pairs) >= 2);
  equal merged_t expected (merged (Coverage.merge (of_drawn a) (of_drawn b)))

let merge_examples =
  [
    ([ ("lib/a.ml", 1, [ 7; 0 ]) ], [ ("lib/b.ml", 1, [ 1; 0 ]) ]);
    ([ ("lib/a.ml", 1, [ 1; 0 ]) ], [ ("lib/a.ml", 1, [ 4; 2 ]) ]);
    ([ ("lib/a.ml", 1, [ max_int - 1; 0 ]) ], [ ("lib/a.ml", 1, [ 5; 0 ]) ]);
    ([ ("lib/a.ml", 1, [ 5; 0 ]) ], [ ("lib/a.ml", 1, [ 5; 0 ]) ]);
    ([ ("lib/a.ml", 0, [ 1; 1; 1 ]) ], [ ("lib/a.ml", 2, [ 1; 1; 1 ]) ]);
    ( [
        ("lib/a.ml", 0, [ 1; 1; 1 ]);
        ("lib/b.ml", 0, [ 1; 1; 1 ]);
        ("lib/c.ml", 0, [ 1; 1; 1 ]);
      ],
      [
        ("lib/a.ml", 0, [ 1; 1; 1 ]);
        ("lib/b.ml", 2, [ 1; 1; 1 ]);
        ("lib/c.ml", 1, [ 1; 1 ]);
      ] );
    ([], [ ("lib/a.ml", 1, [ 7; 0 ]); ("lib/b.ml", 1, [ 1; 0 ]) ]);
    ([ ("lib/a.ml", 1, [ 7; 0 ]); ("lib/b.ml", 1, [ 1; 0 ]) ], []);
  ]

let empty_is_an_identity entries =
  let t = of_drawn entries in
  equal merged_t (Ok (hits t)) (merged (Coverage.merge Coverage.empty t));
  equal merged_t (Ok (hits t)) (merged (Coverage.merge t Coverage.empty))

let pp_message e = Format.asprintf "%a" Coverage.pp_error e

let mismatch_message () =
  let message = pp_message (Point_mismatch { file = "lib/x.ml" }) in
  contains ~sub:"from one build" message;
  contains ~sub:"delete the coverage files" message;
  not_contains ~sub:"dune " message

let data_message () =
  let e = Instr.Unknown_format { path = "old.coverage"; header = "V1" } in
  equal string
    (Format.asprintf "%a" (Instr.pp_error Coverage.format) e)
    (pp_message (Data e));
  contains ~sub:"delete the stale coverage files" (pp_message (Data e))

let collections =
  group "Collections"
    [
      test "empty has no file" (fun () ->
          equal (list string) [] (Coverage.files Coverage.empty));
      test "files are in the order of names, whatever the order of the dump"
        (fun () ->
          equal (list string) [ "lib/a.ml"; "lib/b.ml" ]
            (Coverage.files (ab ())));
      test "a file of no point is data, not emptiness" (fun () ->
          equal (list string) [ "lib/e.ml" ]
            (Coverage.files (collection [ ("lib/e.ml", []) ])));
      prop
        "merge adds up the counts of a file both hold, saturating at max_int, \
         or names the first file of b by name whose table differs"
        ~examples:merge_examples (Gen.pair drawn drawn) merge_law;
      prop "empty is an identity of merge on either side" drawn
        empty_is_an_identity;
      test
        "a point mismatch says to run again from one build, then to delete the \
         files, and names no build tool"
        mismatch_message;
      test "a data error is Instr's message, which says to delete the files"
        data_message;
    ]

(* Dumps *)

let digest_of s = Digest.to_hex (Digest.string s)

let identity_is_read () =
  let recorded =
    { Coverage.exe = "default/test/a.exe"; digest = digest_of "exe-a" }
  in
  let t, read_back =
    require_ok ~pp:Coverage.pp_error
      (load_text (dump ~identity:recorded [ ("lib/a.ml", [ (line 1, 7) ]) ]))
  in
  equal (option identity) (Some recorded) read_back;
  equal hits_t [ ("lib/a.ml", [ (1, 7) ]) ] (hits t)

let digest_a = String.make 32 'a'

let corrupt =
  [
    ("a bare magic line", "windtrap-coverage-v3");
    ("no file body", "windtrap-coverage-v3\n1\n");
    ("a truncated file name", "windtrap-coverage-v3\n1\n99 lib/a.ml\n");
    ("an inverted extent", "windtrap-coverage-v3\n1\n8 lib/a.ml\n1\n5 3 1\n");
    ("a negative count", "windtrap-coverage-v3\n1\n8 lib/a.ml\n1\n0 5 -1\n");
    ("a negative point count", "windtrap-coverage-v3\n1\n8 lib/a.ml\n-1\n");
    ( "a point count larger than the input",
      "windtrap-coverage-v3\n1\n8 lib/a.ml\n999999999\n0 5 1\n" );
    ("bytes after the last record", "windtrap-coverage-v3\n0\nxx");
    ( "a truncated identity path",
      strf "windtrap-coverage-v3\nexe %s 99 default/a.exe\n0\n" digest_a );
    ( "an empty identity path",
      strf "windtrap-coverage-v3\nexe %s 0 \n0\n" digest_a );
    ( "an identity without a digest",
      "windtrap-coverage-v3\nexe 14 default/a.exe\n0\n" );
    ( "an identity with a short digest",
      "windtrap-coverage-v3\nexe abc123 14 default/a.exe\n0\n" );
    ("a tab before a file name", "windtrap-coverage-v3\n1\n8\tlib/a.ml\n0\n");
  ]

let foreign =
  [
    ("the v1 magic", "WINDTRAP-COVERAGE-1 1 8 lib/a.ml 1 12 1 34");
    ( "the pre-release v2 magic",
      "windtrap-coverage-v2\n1\n8 lib/a.ml\n1\n0 5 1\n" );
    ("an empty file", "");
    ("the magic followed by a digit", "windtrap-coverage-v33\n0\n");
  ]

let two_entries end_ofs =
  strf "windtrap-coverage-v3\n2\n8 lib/a.ml\n1\n0 5 1\n8 lib/a.ml\n1\n0 %d 2\n"
    end_ofs

let tabs_and_crlf () =
  let spaced = "windtrap-coverage-v3\n1\n8 lib/a.ml\n1\n0 5 7\n" in
  let crlf = "windtrap-coverage-v3\r\n1\r\n8 lib/a.ml\r\n1\r\n0\t5\t7\r\n" in
  let hits_of text =
    hits (fst (require_ok ~pp:Coverage.pp_error (load_text text)))
  in
  equal hits_t (hits_of spaced) (hits_of crlf)

let dumps_with_its_writer () =
  let dump = dumped "first" in
  equal string (child_dump ~exe:child_exe [| 1; 0; 0 |]) (read dump);
  equal (option identity)
    (Some
       {
         Coverage.exe = Instr.exe_identity ~exe:child_exe;
         digest = Digest.to_hex (Digest.file child_exe);
       })
    (snd (require_ok ~pp:Coverage.pp_error (Coverage.load dump)))

let temporaries dir =
  List.filter
    (fun name -> Filename.check_suffix name ".tmp")
    (Array.to_list (Sys.readdir dir))

let rerun_replaces () =
  let dump = Filename.concat (temp_dir ()) "child.coverage" in
  equal (list int) [ 0; 0 ]
    (List.map
       (fun mode -> Child.exit_code (run_child ~dump [ mode ]))
       [ "first"; "second" ]);
  equal string (child_dump ~exe:child_exe [| 1; 2; 0 |]) (read dump);
  equal (list string) [] (temporaries (Filename.dirname dump))

let unwritable () =
  let blocked = Filename.concat (temp_file ()) "under-a-file.coverage" in
  let r = run_child ~dump:blocked [ "first" ] in
  equal int 0 (Child.exit_code r);
  starts_with
    ~affix:("windtrap: warning: cannot write coverage file " ^ blocked ^ ": ")
    r.err

let silent () =
  let dump = Filename.concat (temp_dir ()) "child.coverage" in
  equal int 0 (Child.exit_code (run_child ~dump [ "silent" ]));
  is_false (Sys.file_exists dump)

(* A copy of the child at [path], so that the directory of its default dump is
   a scratch one. *)
let install_child path =
  mkdir_p (Filename.dirname path);
  write path (read child_exe);
  Unix.chmod path 0o755;
  path

let under_scratch_build () =
  install_child (Filename.concat (temp_dir ()) "_build/default/child.exe")

(* The variable stated empty reads as unset. *)
let run_default exe args = run_child ~exe ~dump:"" args

let dumps_of dir =
  match Sys.readdir dir with
  | exception Sys_error _ -> []
  | names ->
      List.sort String.compare
        (List.filter
           (fun name -> Filename.check_suffix name ".coverage")
           (Array.to_list names))

let merge_dumps dir =
  List.fold_left
    (fun acc name ->
      let t, _ =
        require_ok ~pp:Coverage.pp_error
          (Coverage.load (Filename.concat dir name))
      in
      require_ok ~pp:Coverage.pp_error (Coverage.merge acc t))
    Coverage.empty (dumps_of dir)

(* The first run registers two files, and still keeps one dump. *)
let runs_accumulate () =
  let exe = under_scratch_build () in
  let dir = Instr.output_dir Coverage.format ~exe in
  equal (list int) [ 0; 0 ]
    (List.map
       (fun mode -> Child.exit_code (run_default exe [ mode ]))
       [ "files"; "second" ]);
  let digest = Digest.to_hex (Digest.file exe) in
  equal (list string) [ digest; digest ]
    (List.map
       (fun name -> List.hd (String.split_on_char '-' name))
       (dumps_of dir));
  equal hits_t
    [ ("lib/child.ml", [ (1, 2); (2, 2); (3, 0) ]); ("lib/zero.ml", []) ]
    (hits (merge_dumps dir))

let rebuild_supersedes () =
  let exe = under_scratch_build () in
  let dir = Instr.output_dir Coverage.format ~exe in
  equal int 0 (Child.exit_code (run_default exe [ "first" ]));
  let older = digest_of "an older build" in
  let stale = Filename.concat dir (older ^ "-000001.coverage") in
  write stale
    (dump
       ~identity:{ exe = Instr.exe_identity ~exe; digest = older }
       [ ("lib/child.ml", [ (line 1, 1); (line 2, 0); (line 3, 0) ]) ]);
  equal int 0 (Child.exit_code (run_default exe [ "first" ]));
  is_false (Sys.file_exists stale);
  equal int 2 (List.length (dumps_of dir));
  equal (list string) [] (temporaries dir)

let destination_fixed () =
  let first = temp_dir () and later = temp_dir () in
  let r = run_child ~cwd:first ~dump:"rel.coverage" [ "moved"; later ] in
  equal int 0 (Child.exit_code r);
  equal string
    (child_dump ~exe:child_exe [| 1; 0; 0 |])
    (read (Filename.concat first "rel.coverage"));
  equal (list string) [] (Array.to_list (Sys.readdir later))

let posix_only () = if Sys.win32 then skip ~reason:"POSIX only" ()

let fork_shares_the_path () =
  posix_only ();
  let dump = Filename.concat (temp_dir ()) "fork.coverage" in
  equal int 0 (Child.exit_code (run_child ~dump [ "fork" ]));
  equal string (child_dump ~exe:child_exe [| 1; 0; 1 |]) (read dump)

let fork_keeps_a_dump () =
  posix_only ();
  let exe = under_scratch_build () in
  equal int 0 (Child.exit_code (run_default exe [ "fork" ]));
  let dir = Instr.output_dir Coverage.format ~exe in
  equal int 2 (List.length (dumps_of dir));
  equal hits_t
    [ ("lib/child.ml", [ (1, 2); (2, 1); (3, 1) ]) ]
    (hits (merge_dumps dir))

let no_removed_cwd () =
  if Sys.win32 then
    skip ~reason:"Windows cannot remove a process's working directory" ()

(* What a child wrote on standard error when it removed its directory, with
   its own copy in it, before it registered. *)
let cwd_gone ~dump =
  let dir = Filename.concat (temp_dir ()) "here" in
  ignore (install_child (Filename.concat dir "coverage_child.exe"));
  let r = run_child ~cwd:dir ~exe:"./coverage_child.exe" ~dump [ "cwd-gone" ] in
  equal int 0 (Child.exit_code r);
  is_false (Sys.file_exists dir);
  r.err

let relative_needs_cwd () =
  no_removed_cwd ();
  let err = cwd_gone ~dump:"rel.coverage" in
  starts_with
    ~affix:"windtrap: warning: cannot determine the coverage output file: " err;
  equal int 1 (List.length (String.split_on_char '\n' (String.trim err)))

let absolute_needs_none () =
  no_removed_cwd ();
  let dump = Filename.concat (temp_dir ()) "abs.coverage" in
  equal text "" (cwd_gone ~dump);
  equal string (child_dump [| 1; 0; 0 |]) (read dump)

let named_build () =
  let exe = install_child (Filename.concat (temp_dir ()) "_build_child.exe") in
  let dump = Filename.concat (temp_dir ()) "named.coverage" in
  let r = run_child ~exe ~dump [ "first" ] in
  equal int 0 (Child.exit_code r);
  equal text "" r.err;
  equal string (child_dump ~exe [| 1; 0; 0 |]) (read dump)

let dumps =
  group "Dumps"
    [
      test "a process dumps its counts at exit, with its writer's identity"
        dumps_with_its_writer;
      test "under WINDTRAP_COVERAGE_FILE a run replaces the file atomically"
        rerun_replaces;
      test "a dump that cannot be written is a warning that leaves exit 0"
        unwritable;
      test "a process that registered nothing writes no dump" silent;
      test
        "by default each run keeps one dump, named after its writer's digest, \
         and the runs add up"
        runs_accumulate;
      test "the first dump of a rebuilt executable removes its predecessors'"
        rebuild_supersedes;
      test
        "the destination is fixed at the first registration, a relative path \
         against the directory of that moment"
        destination_fixed;
      test
        "under WINDTRAP_COVERAGE_FILE a forked child and its parent share the \
         path, and the last to exit wins"
        fork_shares_the_path;
      test
        "by default a forked child keeps a dump of its own, with the counts \
         from before the fork"
        fork_keeps_a_dump;
      test
        "a relative destination without a current directory is a warning of \
         one line"
        relative_needs_cwd;
      test
        "an absolute destination needs no current directory, and the dump has \
         no identity once the executable is gone"
        absolute_needs_none;
      test "an executable named _build* below no build directory dumps silently"
        named_build;
      test
        "format is the magic line windtrap-coverage-v3, coverage for the \
         directory, the extension and the kind, and its owner's name" (fun () ->
          let f = Coverage.format in
          equal (list string)
            [
              "windtrap-coverage-v3";
              "coverage";
              "coverage";
              "coverage";
              "Windtrap_runtime.Coverage";
            ]
            [ f.magic; f.dir; f.ext; f.kind; f.who ]);
      test "load reads the identity that follows the magic line"
        identity_is_read;
      test "load reads no identity from a dump without one" (fun () ->
          is_none ~pp:(Testable.pp identity)
            (snd
               (require_ok ~pp:Coverage.pp_error
                  (load_text (dump [ ("lib/a.ml", [ (line 1, 7) ]) ])))));
      test "load reads a dump of no file as the empty collection" (fun () ->
          equal string "files: "
            (loaded (load_text "windtrap-coverage-v3\n0\n")));
      cases "load refuses another first line" ~name:fst foreign
        (fun (_, text) ->
          equal string "unknown format" (loaded (load_text text)));
      cases "load refuses truncated or invalid data" ~name:fst corrupt
        (fun (_, text) -> equal string "corrupt" (loaded (load_text text)));
      test "load refuses a missing file as unreadable" (fun () ->
          equal string "unreadable"
            (loaded
               (Coverage.load
                  (Filename.concat (temp_dir ()) "missing.coverage"))));
      test "two entries of a file with different tables are a point mismatch"
        (fun () ->
          equal string "point mismatch in lib/a.ml"
            (loaded (load_text (two_entries 6))));
      test "two entries of a file with an equal table add up their counts"
        (fun () ->
          equal hits_t
            [ ("lib/a.ml", [ (1, 3) ]) ]
            (hits
               (fst
                  (require_ok ~pp:Coverage.pp_error (load_text (two_entries 5))))));
      test "a dump separated by tabs and CR LF loads as one of spaces and LF"
        tabs_and_crlf;
      test "a file name holding whitespace keeps it" (fun () ->
          let odd = "lib/a\tb \r.ml" in
          equal (list string) [ odd ]
            (Coverage.files (collection [ (odd, []) ])));
    ]

(* Report data *)

(* Lines 1, 2 and 3 are the bytes 0-8, 9-17 and 18-28, newlines included. *)
let three_lines = "line one\nline two\nline three\n"
let source_of_whole = strf "source %S" three_lines

let source_of (r : Coverage.file_report) =
  match (r.source, r.stale) with
  | Some source, false -> strf "source %S" source
  | None, false -> "no source"
  | None, true -> "stale"
  | Some _, true -> "stale, with its source"

let ints l = String.concat " " (List.map string_of_int l)

(* A report as one row: every field a failure should show. *)
let row (r : Coverage.file_report) =
  strf "%s %d/%d, uncovered [%s], lines [%s], hits [%s], %s" r.file
    r.summary.visited r.summary.total
    (String.concat " " (List.map extent r.uncovered_extents))
    (ints r.uncovered_lines)
    (String.concat " "
       (List.map (fun (line, n) -> strf "%d:%d" line n) r.line_hits))
    (source_of r)

let pp_rows ppf reports =
  Format.pp_print_list Format.pp_print_string ppf (List.map row reports)

(* The one report of a collection of one file. *)
let only reports =
  require_match ~pp:pp_rows (function [ r ] -> Some r | _ -> None) reports

let report ?(source = three_lines) rows =
  let root = temp_dir () in
  put root "lib/f.ml" source;
  only
    (Coverage.file_reports ~source_roots:[ root ]
       (collection [ ("lib/f.ml", rows) ]))

let touched_lines =
  [
    ( "an extent over the file marks every line",
      three_lines,
      [ pt 0 28 ],
      [ 1; 2; 3 ] );
    ("an extent of one line marks it alone", three_lines, [ pt 9 17 ], [ 2 ]);
    ( "an extent that ends on a newline stays on its line",
      three_lines,
      [ pt 9 18 ],
      [ 2 ] );
    ( "an extent across a newline marks both lines",
      three_lines,
      [ pt 9 19 ],
      [ 2; 3 ] );
    ( "two extents on one line mark it once",
      three_lines,
      [ pt 0 4; pt 5 8 ],
      [ 1 ] );
    ( "an empty extent marks the line that holds it",
      three_lines,
      [ pt 9 9 ],
      [ 2 ] );
    ( "an outer extent marks the lines of the inner one",
      three_lines,
      [ pt 0 28; pt 9 17 ],
      [ 1; 2; 3 ] );
    ("no extent marks no line", three_lines, [], []);
    ( "a source without a final newline keeps its last line",
      "a\nb",
      [ pt 2 3 ],
      [ 2 ] );
    ( "an empty extent after the final newline marks the last line",
      three_lines,
      [ pt 29 29 ],
      [ 3 ] );
    ( "an empty extent at the end of a source without one marks its last line",
      "a\nb",
      [ pt 3 3 ],
      [ 2 ] );
  ]

let huge_file () =
  let n = 20_000 in
  let source =
    String.concat "" (List.init n (fun i -> strf "line %d\n" (i + 1)))
  in
  equal (list int) (List.init n succ)
    (report ~source [ (pt 0 (String.length source), 0) ]).uncovered_lines

(* Found at [roots], the source of a file whose one point needs 20 bytes. *)
let found ~roots file =
  source_of
    (only
       (Coverage.file_reports ~source_roots:roots
          (collection [ (file, [ (pt 0 20, 0) ]) ])))

let two_copies () =
  let short = temp_dir () and whole = temp_dir () in
  put short "lib/order.ml" "short\n";
  put whole "lib/order.ml" three_lines;
  (short, whole)

let first_root_wins () =
  let short, whole = two_copies () in
  equal (list string)
    [ "stale"; source_of_whole ]
    [
      found ~roots:[ short; whole ] "lib/order.ml";
      found ~roots:[ whole; short ] "lib/order.ml";
    ]

let unreadable_roots_passed () =
  let _, whole = two_copies () in
  let directory = temp_dir () in
  mkdir_p (Filename.concat directory "lib/order.ml");
  equal (list string)
    [ source_of_whole; source_of_whole ]
    [
      found ~roots:[ temp_dir (); whole ] "lib/order.ml";
      found ~roots:[ directory; whole ] "lib/order.ml";
    ]

let recorded_name_first () =
  if Sys.win32 then
    skip ~reason:"no directory holds a Windows absolute name, drive and all" ();
  let short, _ = two_copies () in
  let recorded = Filename.concat (temp_dir ()) "recorded.ml" in
  write recorded three_lines;
  put short recorded "short\n";
  equal string source_of_whole (found ~roots:[ short ] recorded)

let report_data =
  group "Report data"
    [
      test "summary counts the visited points of every file" (fun () ->
          equal summary
            { Coverage.visited = 2; total = 3 }
            (Coverage.summary (ab ())));
      test "a collection of no point sums to 0/0" (fun () ->
          equal summary
            { Coverage.visited = 0; total = 0 }
            (Coverage.summary (collection [ ("lib/e.ml", []) ])));
      cases "uncovered_lines are the lines an unvisited extent touches"
        ~name:(fun (name, _, _, _) -> name)
        touched_lines
        (fun (_, source, extents, lines) ->
          equal (list int) lines
            (report ~source (List.map (fun p -> (p, 0)) extents))
              .uncovered_lines);
      test "an extent over a huge file marks each line once, in order" huge_file;
      test "an empty source has no line" (fun () ->
          equal string
            "lib/f.ml 0/1, uncovered [0-0], lines [], hits [], source \"\""
            (row (report ~source:"" [ (pt 0 0, 0) ])));
      test
        "a visited outer point leaves its unvisited inner point uncovered, and \
         a line has the fewest visits of its points" (fun () ->
          equal string
            (strf
               "lib/f.ml 1/2, uncovered [9-17], lines [2], hits [1:1 2:0 3:1], \
                %s"
               source_of_whole)
            (row (report [ (pt 0 28, 1); (pt 9 17, 0) ])));
      test
        "reports are in the order of names, and one without its source has no \
         line" (fun () ->
          equal (list string)
            [
              "lib/a.ml 1/1, uncovered [], lines [], hits [], no source";
              "lib/b.ml 1/2, uncovered [6-11], lines [], hits [], no source";
            ]
            (List.map row
               (Coverage.file_reports ~source_roots:[ temp_dir () ] (ab ()))));
      test "a source shorter than the extents is stale, and paints no line"
        (fun () ->
          equal string
            "lib/f.ml 1/2, uncovered [20-40], lines [], hits [], stale"
            (row (report [ (pt 0 10, 1); (pt 20 40, 0) ])));
      test "an extent that ends at the last byte is not stale" (fun () ->
          equal string
            (strf
               "lib/f.ml 0/1, uncovered [0-29], lines [1 2 3], hits [1:0 2:0 \
                3:0], %s"
               source_of_whole)
            (row (report [ (pt 0 (String.length three_lines), 0) ])));
      test "the first root that holds a readable file wins, stale or not"
        first_root_wins;
      test
        "a root without the file, or with a directory by its name, is passed \
         over"
        unreadable_roots_passed;
      test "a recorded absolute name is looked up before every root"
        recorded_name_first;
    ]

let () =
  exit
    (run "coverage"
       [ points_and_instrumentation; collections; dumps; report_data ])
