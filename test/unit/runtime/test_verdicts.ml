(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Instr = Windtrap_runtime.Instr
module Mutate = Windtrap_runtime.Mutate
module Verdicts = Windtrap_runtime.Verdicts

let strf = Printf.sprintf
let read path = In_channel.with_open_bin path In_channel.input_all
let id file line col rewrite = { Mutate.file; line; col; rewrite }

let record ?(before = "b") ?(after = "a") id verdict =
  { Verdicts.id; before; after; verdict }

let collection records = List.fold_left Verdicts.add Verdicts.empty records

(* Rows. Every string is quoted, so a row tells any two values apart. *)

let quoted names = String.concat " " (List.map (strf "%S") names)

let verdict_row = function
  | Verdicts.Killed -> "killed"
  | Survived { first; others } ->
      "survived by "
      ^ String.concat ", "
          (List.map (fun t -> "[" ^ quoted t ^ "]") (first :: others))
  | Not_evaluated -> "not evaluated"
  | Outside_tests -> "outside tests"
  | Unreached -> "unreached"

let record_row (r : Verdicts.record) =
  strf "%S:%d:%d:%s %S -> %S: %s" r.id.file r.id.line r.id.col r.id.rewrite
    r.before r.after (verdict_row r.verdict)

let rows t = List.map record_row (Verdicts.records t)
let verdict = Testable.contramap verdict_row Testable.string
let records = Testable.contramap rows (list string)
let identity_row (i : Verdicts.identity) = strf "%S %s" i.exe i.digest
let identity = Testable.contramap identity_row Testable.string

let error_row = function
  | Verdicts.Unknown_format { path; header } ->
      strf "unknown format %s %S" path header
  | Unreadable { path; _ } -> "unreadable " ^ path
  | Corrupt { path; reason } -> strf "corrupt %s: %s" path reason

let load_rows = function
  | Ok (t, i) -> Ok (rows t, Option.map identity_row i)
  | Error e -> Error (error_row e)

let load_text s =
  let path = Filename.concat (temp_dir ()) "f.mutants" in
  Out_channel.with_open_bin path (fun oc -> output_string oc s);
  (path, Verdicts.load path)

let corrupt_path = function
  | Error (Verdicts.Corrupt { path; _ }) -> Some path
  | _ -> None

let saved ?identity t =
  let path = Filename.concat (temp_dir ()) "saved.mutants" in
  Verdicts.save ?identity path t;
  read path

(* Generators *)

let pp_verdict ppf v = Format.pp_print_string ppf (verdict_row v)
let pp_record ppf r = Format.pp_print_string ppf (record_row r)
let pp_records ppf t = Format.pp_print_string ppf (String.concat "\n" (rows t))
let pp_identity ppf i = Format.pp_print_string ppf (identity_row i)

(* Few names and identifiers, so that two draws often share one. *)
let small_verdict =
  let path =
    Gen.list ~size:(Gen.int_range 1 2) (Gen.of_list [ "g"; "t"; "u" ])
  in
  Gen.with_pp pp_verdict
    (Gen.frequency
       [
         (1, Gen.constant Verdicts.Killed);
         (1, Gen.constant Verdicts.Not_evaluated);
         (3, Gen.map Verdicts.survived (Gen.list ~size:(Gen.int_range 1 3) path));
         (1, Gen.constant Verdicts.Outside_tests);
         (1, Gen.constant Verdicts.Unreached);
       ])

let small_record =
  Gen.with_pp pp_record
    (Gen.map
       (fun (file, line, (before, after), verdict) ->
         record ~before ~after (id file line 0 "lt") verdict)
       (Gen.quad
          (Gen.of_list [ "lib/a.ml"; "lib/b.ml" ])
          (Gen.int_range 1 2)
          (Gen.pair (Gen.of_list [ "a"; "b" ]) (Gen.of_list [ "a"; "b" ]))
          small_verdict))

let small_collection =
  Gen.with_pp pp_records
    (Gen.map collection (Gen.list ~size:(Gen.int_range 0 6) small_record))

(* Any bytes in every name, and any identifier that load accepts. *)
let any_record =
  let name = Gen.string in
  let path = Gen.list ~size:(Gen.int_range 0 3) name in
  let verdict =
    Gen.one_of
      [
        Gen.of_list Verdicts.[ Killed; Not_evaluated; Outside_tests; Unreached ];
        Gen.map Verdicts.survived (Gen.list ~size:(Gen.int_range 1 3) path);
      ]
  in
  Gen.map
    (fun ((file, line, col, rewrite), before, after, verdict) ->
      record ~before ~after (id file line col rewrite) verdict)
    (Gen.quad
       (Gen.quad
          (Gen.map (fun s -> "f" ^ s) name)
          (Gen.map succ Gen.nat) Gen.nat
          (Gen.of_list Mutate.rewrites))
       name name verdict)

let any_identity =
  Gen.option
    (Gen.with_pp pp_identity
       (Gen.map
          (fun (exe, digest) -> { Verdicts.exe = "e" ^ exe; digest })
          (Gen.pair Gen.string
             (Gen.string_of ~size:(Gen.constant 32)
                (Gen.of_list (List.of_seq (String.to_seq "0123456789abcdef")))))))

let any_file =
  Gen.pair any_identity
    (Gen.with_pp pp_records
       (Gen.map collection (Gen.list ~size:(Gen.int_range 0 5) any_record)))

(* Verdicts *)

let rank = function
  | Verdicts.Killed -> 4
  | Not_evaluated -> 3
  | Survived _ -> 2
  | Outside_tests -> 1
  | Unreached -> 0

(* The verdict of a mutant that one executable saw as [a] and another as [b],
   as add states it. *)
let join a b =
  match (a, b) with
  | Verdicts.Survived x, Verdicts.Survived y ->
      Verdicts.survived ((x.first :: x.others) @ (y.first :: y.others))
  | _ -> if rank a >= rank b then a else b

let combined (a, b) =
  let m = id "lib/lattice.ml" 1 0 "add" in
  let t = collection [ record m a; record m b ] in
  equal verdict (join a b)
    (require_match
       (function [ r ] -> Some r.Verdicts.verdict | _ -> None)
       (Verdicts.records t))

let reaching = function
  | Verdicts.Survived { first; others } -> Some (first :: others)
  | _ -> None

let sorted_names ts =
  equal
    (list (list string))
    (List.sort_uniq compare ts)
    (require_match ~pp:pp_verdict reaching (Verdicts.survived ts))

let lattice_examples =
  let s = Verdicts.survived in
  Verdicts.
    [
      (Killed, s [ [ "a" ] ]);
      (Unreached, Killed);
      (Not_evaluated, s [ [ "a" ] ]);
      (Outside_tests, s [ [ "a" ] ]);
      (Not_evaluated, Outside_tests);
      (Unreached, Outside_tests);
      (s [ [ "b" ]; [ "a" ] ], s [ [ "c" ]; [ "b" ] ]);
    ]

let verdicts =
  group "Verdicts"
    [
      prop "survived sorts its reaching tests and drops duplicates"
        ~examples:[ [ [ "b" ]; [ "a" ]; [ "b" ] ] ]
        (Gen.list ~size:(Gen.int_range 1 4)
           (Gen.list ~size:(Gen.int_range 0 2) (Gen.of_list [ "a"; "b"; "" ])))
        sorted_names;
      test "survived raises Invalid_argument on no reaching test" (fun () ->
          raises
            (Invalid_argument
               "Windtrap_runtime.Verdicts.survived: a survivor names at least \
                one test") (fun () -> Verdicts.survived []));
    ]

(* Collections *)

let record_of_mutant () =
  let m =
    {
      Mutate.id = id "lib/calc.ml" 4 2 "sub";
      before = "a - b";
      after = "a + b";
      dismissed = Some "equivalent";
    }
  in
  equal string {|"lib/calc.ml":4:2:sub "a - b" -> "a + b": killed|}
    (record_row (Verdicts.record_of_mutant m Verdicts.Killed))

(* Two records of one mutant keep the smaller rendering and the verdict both
   combine into, in either order. *)
let kept_rendering (a, b) =
  let key (r : Verdicts.record) = (r.before, r.after) in
  let a = { a with Verdicts.id = b.Verdicts.id } in
  let smaller = if compare (key a) (key b) <= 0 then a else b in
  let expected = [ { smaller with verdict = join a.verdict b.verdict } ] in
  equal records (collection expected) (collection [ a; b ]);
  equal records (collection expected) (collection [ b; a ])

let ordered added =
  let ids records =
    List.map (fun (r : Verdicts.record) -> Mutate.id_to_string r.id) records
  in
  equal (list string)
    (ids
       (List.sort_uniq
          (fun (a : Verdicts.record) b -> Mutate.compare_id a.id b.id)
          added))
    (ids (Verdicts.records (collection added)))

let commutes (a, b) = equal records (Verdicts.merge a b) (Verdicts.merge b a)

let associates (a, b, c) =
  equal records
    (Verdicts.merge (Verdicts.merge a b) c)
    (Verdicts.merge a (Verdicts.merge b c))

let idempotent a = equal records a (Verdicts.merge a a)

let empty_is_unit a =
  equal records a (Verdicts.merge a Verdicts.empty);
  equal records a (Verdicts.merge Verdicts.empty a)

let merge_is_add (a, b) =
  equal records
    (List.fold_left Verdicts.add a (Verdicts.records b))
    (Verdicts.merge a b)

let unreached_is_neutral a =
  let unreached =
    collection
      (List.map
         (fun (r : Verdicts.record) -> { r with verdict = Verdicts.Unreached })
         (Verdicts.records a))
  in
  equal records a (Verdicts.merge a unreached)

let unchecked_ids =
  [
    ("an empty file", id "" 1 0 "lt");
    ("line 0", id "a.ml" 0 0 "lt");
    ("a negative column", id "a.ml" 1 (-1) "lt");
    ("an unknown rewrite", id "a.ml" 1 0 "plus");
  ]

let unchecked (_, bad) =
  let t = Verdicts.add Verdicts.empty (record bad Verdicts.Unreached) in
  let path = Filename.concat (temp_dir ()) "bad.mutants" in
  Verdicts.save path t;
  equal (list string)
    [ Mutate.id_to_string bad ]
    (List.map
       (fun (r : Verdicts.record) -> Mutate.id_to_string r.id)
       (Verdicts.records t));
  equal string path (require_match corrupt_path (Verdicts.load path))

let collections =
  group "Collections"
    [
      test
        "record_of_mutant keeps the identifier and renderings, not the \
         dismissal"
        record_of_mutant;
      test "empty holds no record" (fun () ->
          equal (list string) [] (rows Verdicts.empty));
      prop
        "add combines two verdicts by Killed > Not_evaluated > Survived > \
         Outside_tests > Unreached, and unites the reaching tests"
        ~examples:lattice_examples
        (Gen.pair small_verdict small_verdict)
        combined;
      prop
        "add keeps the smaller (before, after) pair of two records of a mutant"
        (Gen.pair small_record small_record)
        kept_rendering;
      test "add sorts a survivor's reaching tests and drops duplicates"
        (fun () ->
          let unsorted =
            Verdicts.Survived { first = [ "b" ]; others = [ [ "a" ]; [ "b" ] ] }
          in
          equal (list string)
            [ {|"lib/a.ml":1:0:lt "b" -> "a": survived by ["a"], ["b"]|} ]
            (rows (collection [ record (id "lib/a.ml" 1 0 "lt") unsorted ])));
      prop "records are in identifier order, one for each identifier"
        (Gen.list ~size:(Gen.int_range 0 6) small_record)
        ordered;
      prop "merge is commutative"
        (Gen.pair small_collection small_collection)
        commutes;
      prop "merge is associative"
        (Gen.triple small_collection small_collection small_collection)
        associates;
      prop "merge is idempotent" small_collection idempotent;
      prop "empty is the unit of merge" small_collection empty_is_unit;
      prop "merge combines two records of a mutant as add does"
        (Gen.pair small_collection small_collection)
        merge_is_add;
      prop "an Unreached verdict leaves the verdict it is merged with unchanged"
        small_collection unreached_is_neutral;
      cases "add checks no identifier, and load refuses the file save writes"
        ~name:fst unchecked_ids unchecked;
    ]

(* Verdict files *)

let sample =
  collection
    [
      record ~before:"a - b" ~after:"a + b" (id "lib/a.ml" 1 2 "add")
        Verdicts.Unreached;
      record ~before:"p && q" ~after:"not (p && q)" (id "lib/b.ml" 3 4 "not")
        Verdicts.Killed;
      record ~before:"a || b" ~after:"a && b" (id "lib/b.ml" 5 0 "or")
        (Verdicts.survived [ [ "y"; "z" ]; [ "x" ] ]);
      record ~before:"a + b" ~after:"a - b" (id "lib/c.ml" 7 1 "sub")
        Verdicts.Not_evaluated;
      record ~before:"a - b" ~after:"a + b" (id "lib/c.ml" 9 0 "add")
        Verdicts.Outside_tests;
    ]

let digest = String.make 32 'a'

let round_trip (identity, t) =
  let path = Filename.concat (temp_dir ()) "saved.mutants" in
  Verdicts.save ?identity path t;
  equal
    (result (pair (list string) (option string)) string)
    (Ok (rows t, Option.map identity_row identity))
    (load_rows (Verdicts.load path))

let order_free records =
  let dir = temp_dir () in
  let saved name t =
    let path = Filename.concat dir name in
    Verdicts.save path t;
    read path
  in
  equal string
    (saved "a.mutants" (collection records))
    (saved "b.mutants" (collection (List.rev records)))

let replaced () =
  let path = Filename.concat (temp_dir ()) "saved.mutants" in
  Verdicts.save ~identity:{ exe = "test/a.exe"; digest } path sample;
  Verdicts.save path Verdicts.empty;
  equal
    (result (pair (list string) (option string)) string)
    (Ok ([], None))
    (load_rows (Verdicts.load path))

let bad_identities =
  [
    ("an empty exe", { Verdicts.exe = ""; digest });
    ("a short digest", { exe = "a"; digest = "abc" });
    ("an uppercase digest", { exe = "a"; digest = String.make 32 'A' });
    ( "a digest that is not hexadecimal",
      { exe = "a"; digest = String.make 32 'x' } );
  ]

let refused_identity (_, identity) =
  let dir = temp_dir () in
  raises_match (Exn.invalid_arg ~substring:"Windtrap_runtime.Verdicts")
    (fun () ->
      Verdicts.save ~identity
        (Filename.concat dir "unwritten.mutants")
        Verdicts.empty);
  equal (list string) [] (Array.to_list (Sys.readdir dir))

let headers =
  [
    ("the empty file", "");
    ("a coverage file", "windtrap-coverage-v3\n1\n");
    ("a later version", "windtrap-mutants-v4\n0\n");
    ("the magic in uppercase", "WINDTRAP-MUTANTS-V1\n0\n");
    ("the magic and a digit", "windtrap-mutants-v30\n0\n");
    ("plain text", "hello\nworld\n");
  ]

let unknown_format (_, text) =
  let path, loaded = load_text text in
  equal string path
    (require_match
       (function
         | Error (Verdicts.Unknown_format { path; _ }) -> Some path | _ -> None)
       loaded)

let record_prefix = "windtrap-mutants-v3\n1\n8 lib/a.ml "

let corrupt_files =
  [
    ("the magic line alone", "windtrap-mutants-v3");
    ("a negative record count", "windtrap-mutants-v3\n-1\n");
    ("a record count past the input", "windtrap-mutants-v3\n99999999\n");
    ( "fewer records than the count",
      "windtrap-mutants-v3\n2\n8 lib/a.ml 1 2 3 add 1 b 1 a unreached\n" );
    ( "an empty file name",
      "windtrap-mutants-v3\n1\n0  1 2 3 add 0 0 1 b 1 a unreached\n" );
    ( "a file name past the input",
      "windtrap-mutants-v3\n1\n80 lib/a.ml 1 2 3 add 1 b 1 a unreached\n" );
    ("line 0", record_prefix ^ "0 2 3 add 1 b 1 a unreached\n");
    ("a negative line", record_prefix ^ "-3 2 3 add 1 b 1 a unreached\n");
    ("a negative column", record_prefix ^ "1 -2 3 add 1 b 1 a unreached\n");
    ( "a rewrite past the input",
      record_prefix ^ "1 2 30 add 1 b 1 a unreached\n" );
    ("an unknown rewrite", record_prefix ^ "1 2 4 plus 1 b 1 a unreached\n");
    ( "drop, which no instrumenter emits",
      record_prefix ^ "1 2 4 drop 1 b 1 a unreached\n" );
    ("a before past the input", record_prefix ^ "1 2 3 add 80 b 1 a unreached\n");
    ("no rendering", record_prefix ^ "1 2 3 add unreached\n");
    ("an after past the input", record_prefix ^ "1 2 3 add 1 b 80 a unreached\n");
    ("an unknown verdict", record_prefix ^ "1 2 3 add 1 b 1 a errored\n");
    ("no verdict", record_prefix ^ "1 2 3 add 1 b 1 a\n");
    ( "a survivor with no reaching test",
      record_prefix ^ "1 2 3 add 1 b 1 a survived 0\n" );
    ( "a reaching test count past the input",
      record_prefix ^ "1 2 3 add 1 b 1 a survived 99999999\n" );
    ( "a negative test path length",
      record_prefix ^ "1 2 3 add 1 b 1 a survived 1 -1\n" );
    ( "a test path shorter than its length",
      record_prefix ^ "1 2 3 add 1 b 1 a survived 2 1 1 g\n" );
    ( "a test name past the input",
      record_prefix ^ "1 2 3 add 1 b 1 a survived 1 1 80 g\n" );
    ( "two records of one identifier",
      "windtrap-mutants-v3\n\
       2\n\
       8 lib/a.ml 1 2 3 add 1 b 1 a unreached\n\
       8 lib/a.ml 1 2 3 add 1 b 1 a killed\n" );
    ( "a malformed record after a valid one",
      "windtrap-mutants-v3\n\
       2\n\
       8 lib/a.ml 1 2 3 add 1 b 1 a killed\n\
       8 lib/a.ml 0 2 3 add 1 b 1 a unreached\n" );
    ("a short identity digest", "windtrap-mutants-v3\nexe abcd 1 a\n0\n");
    ("an empty identity exe", "windtrap-mutants-v3\nexe " ^ digest ^ " 0 \n0\n");
    ("bytes after the last record", "windtrap-mutants-v3\n0\nextra\n");
  ]

let corrupt (_, text) =
  let path, loaded = load_text text in
  equal string path (require_match corrupt_path loaded)

let corrupt_reasons () =
  let reason (name, text) =
    match snd (load_text text) with
    | Error (Corrupt { reason; _ }) -> strf "%s: %s" name reason
    | Error (Unknown_format _ | Unreadable _) | Ok _ -> name ^ ": not corrupt"
  in
  expect (String.concat "\n" (List.map reason corrupt_files))
  @@ __POS_OF__
       {|
    the magic line alone: expected record count at offset 19
    a negative record count: negative record count
    a record count past the input: record count exceeds data
    fewer records than the count: expected file name length at offset 61
    an empty file name: empty file name
    a file name past the input: truncated file name
    line 0: line 0 is not 1-based
    a negative line: negative line
    a negative column: negative column
    a rewrite past the input: truncated rewrite
    an unknown rewrite: unknown rewrite "plus"
    drop, which no instrumenter emits: unknown rewrite "drop"
    a before past the input: truncated before
    no rendering: expected before length at offset 43
    an after past the input: truncated after
    an unknown verdict: unknown verdict "errored"
    no verdict: expected verdict at offset 51
    a survivor with no reaching test: a survivor names no test (survived is not unreached)
    a reaching test count past the input: reaching test count exceeds data
    a negative test path length: negative test path length
    a test path shorter than its length: expected test path length at offset 68
    a test name past the input: truncated test name
    two records of one identifier: duplicate record for lib/a.ml:1:2:add
    a malformed record after a valid one: line 0 is not 1-based
    a short identity digest: identity digest is not 32 hex characters at offset 24
    an empty identity exe: empty executable identity
    bytes after the last record: trailing data at offset 22
    |}

let error_messages () =
  let pp e = Format.asprintf "%a" Verdicts.pp_error e in
  expect
    (String.concat "\n"
       [
         pp (Unknown_format { path = "f.mutants"; header = "junk" });
         pp
           (Unreadable
              { path = "f.mutants"; reason = "No such file or directory" });
         pp
           (Corrupt
              { path = "f.mutants"; reason = "unknown verdict \"errored\"" });
       ])
  @@ __POS_OF__
       {|
    f.mutants: not a windtrap verdict file (expected header "windtrap-mutants-v3", found "junk"); files written by other windtrap versions are not readable - delete the stale verdict files, then re-run the mutation tests
    f.mutants: cannot read verdict file: No such file or directory
    f.mutants: corrupt verdict file: unknown verdict "errored"
    |}

(* Below a build directory, the identity is not the path. *)
let writer () =
  let build = Filename.concat (temp_dir ()) "_build" in
  Sys.mkdir build 0o755;
  let exe = Filename.concat build "t.exe" in
  Out_channel.with_open_bin exe (fun oc -> output_string oc "an executable");
  let digest = require_some (Instr.file_digest exe) in
  equal (option identity)
    (Some { Verdicts.exe = Instr.exe_identity ~exe; digest })
    (Verdicts.writer_identity ~exe)

let files =
  group "Verdict files"
    [
      test
        "format is the magic line, the mutants directory and extension, and \
         the verdict kind" (fun () ->
          equal (list string)
            [ "windtrap-mutants-v3"; "mutants"; "mutants"; "verdict" ]
            Verdicts.[ format.magic; format.dir; format.ext; format.kind ]);
      test
        "save writes the magic line, the count, then the records by identifier"
        (fun () ->
          expect_exact (saved sample)
          @@ __POS_OF__
               {|windtrap-mutants-v3
5
8 lib/a.ml 1 2 3 add 5 a - b 5 a + b unreached
8 lib/b.ml 3 4 3 not 6 p && q 12 not (p && q) killed
8 lib/b.ml 5 0 2 or 6 a || b 6 a && b survived 2 1 1 x 2 1 y 1 z
8 lib/c.ml 7 1 3 sub 5 a + b 5 a - b not_evaluated
8 lib/c.ml 9 0 3 add 5 a - b 5 a + b outside_tests
|});
      test "save writes an identity after the magic line" (fun () ->
          expect_exact
            (saved ~identity:{ exe = "test/a.exe"; digest } Verdicts.empty)
          @@ __POS_OF__
               {|windtrap-mutants-v3
exe aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa 10 test/a.exe
0
|});
      prop "load reads back what save wrote, identity included" any_file
        round_trip;
      prop
        "save writes one file for a collection, whatever the order of its adds"
        (Gen.list ~size:(Gen.int_range 0 6) small_record)
        order_free;
      test "save replaces the file, identity included" replaced;
      cases
        "save raises Invalid_argument on a malformed identity and writes \
         nothing"
        ~name:fst bad_identities refused_identity;
      cases "load refuses another first line as Unknown_format, naming the path"
        ~name:fst headers unknown_format;
      cases "load refuses malformed data as Corrupt, naming the path" ~name:fst
        corrupt_files corrupt;
      test "a Corrupt reason names the field and its fault" corrupt_reasons;
      test "load of a missing file is Unreadable, naming the path" (fun () ->
          let path = Filename.concat (temp_dir ()) "missing.mutants" in
          equal string ("unreadable " ^ path)
            (Result.fold
               ~ok:(fun _ -> "loaded")
               ~error:error_row (Verdicts.load path)));
      test "pp_error names the file, the fault and what to do" error_messages;
      test "output_file is Instr.output_file of format" (fun () ->
          let exe = "/w/p/_build/default/test/t.exe" in
          equal string
            (Instr.output_file Verdicts.format ~exe)
            (Verdicts.output_file ~exe));
      test "writer_identity is the executable's identity and digest" writer;
      test "writer_identity is None for a file that cannot be read" (fun () ->
          is_none ~pp:pp_identity
            (Verdicts.writer_identity
               ~exe:(Filename.concat (temp_dir ()) "no-such.exe")));
    ]

let () = exit (run "verdicts" [ verdicts; collections; files ])
