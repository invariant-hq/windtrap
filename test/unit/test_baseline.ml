(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Windtrap
module Baseline = Windtrap.Private.Baseline
module Failure = Windtrap.Private.Failure
module Loc = Windtrap.Private.Loc
module Os = Windtrap.Private.Os
module Source_patch = Windtrap.Private.Source_patch

let strf = Printf.sprintf

(* Projects on disk *)

let read path = In_channel.with_open_bin path In_channel.input_all

let put root file contents =
  let path = Filename.concat root file in
  Os.mkdir_p (Filename.dirname path);
  Out_channel.with_open_bin path (fun oc -> output_string oc contents)

let project files =
  let root = temp_dir () in
  List.iter (fun (file, contents) -> put root file contents) files;
  root

(* The files under [root] with their contents, and its empty directories, in
   the order of their paths. *)
let tree root =
  let rec entries dir =
    Sys.readdir (Filename.concat root dir)
    |> Array.to_list |> List.sort String.compare
    |> List.concat_map (fun name ->
        let file = if dir = "" then name else dir ^ "/" ^ name in
        let path = Filename.concat root file in
        if not (Sys.is_directory path) then [ strf "%s %S" file (read path) ]
        else match entries file with [] -> [ file ^ "/" ] | inside -> inside)
  in
  entries ""

let file file contents = strf "%s %S" file contents

let disk = function
  | [] -> "on disk: nothing"
  | entries -> "on disk: " ^ String.concat ", " entries

(* What a check did *)

let text (t : Failure.text) =
  if Failure.is_cut t then
    strf "%d bytes cut to %d" t.length (String.length t.kept)
  else strf "%S" t.kept

let compared = function
  | Failure.Literal { exact = true } -> "exact literal"
  | Literal { exact = false } -> "literal"
  | File path -> "file " ^ path

let state = function
  | Failure.Missing { proposed } -> "missing " ^ text proposed
  | Mismatch { expected; actual } ->
      strf "mismatch %s against %s" (text actual) (text expected)
  | Unresolvable { candidate } -> strf "unresolvable %S" candidate

let withheld = function
  | None -> ""
  | Some (Failure.Refused { line; reason }) ->
      strf ", refused at line %d: %s" line reason
  | Some Conflict -> ", conflict"
  | Some Failed_outside -> ", failed outside"
  | Some Skipped -> ", skipped"

(* The steps of a run. Each gives the rows of what it did. *)

let check ?correct subject actual t =
  match Baseline.check t ?correct subject actual with
  | () -> [ "pass" ]
  | exception
      Failure.Check_failure
        { kind = Baseline { baseline; state = s; withheld = w }; _ } ->
      [ strf "%s: %s%s" (compared baseline) (state s) (withheld w) ]
  | exception Failure.Check_failure _ -> [ "a failure of another kind" ]

let settle ~keep t = [ strf "kept %d" (Baseline.settle t ~keep) ]

let write t =
  Baseline.write t;
  []

let relative root path =
  let prefix = root ^ "/" in
  if String.starts_with ~prefix path then
    String.sub path (String.length prefix)
      (String.length path - String.length prefix)
  else "not under the root: " ^ path

let writes root t =
  List.map
    (function
      | Baseline.Written { path; literals } ->
          strf "wrote %s, %d literals" (relative root path) literals
      | Refused { path; reason } ->
          strf "refused %s: %s" (relative root path) reason)
    (Baseline.writes t)

let on_disk root _t = [ disk (tree root) ]

let edit root file contents _t =
  put root file contents;
  []

let play t steps = List.concat_map (fun step -> step t) steps
let commit root t = play t [ settle ~keep:true; write; writes root ]

let scenario ?cwd ~mode root steps =
  let cwd = Option.value cwd ~default:root in
  play (Baseline.create ~root ~cwd ~mode ()) steps

let traced = list string

let mode_name = function
  | Baseline.Check -> "Check"
  | Corrected -> "Corrected"
  | Update -> "Update"

(* Sources and subjects *)

let help = "test/help.expected"
let file_help = Baseline.File help

(* A source holding one flexible literal at line 2. The position that
   [__POS_OF__] records starts at its own token, column 19. *)
let source = "let () =\n  expect (f ()) @@ __POS_OF__ {| old |}\n"
let edited = "let () =\n  expect (f ()) @@ __POS_OF__ {| edited |}\n"

let literal ?(line = 2) ?(column = 19) ?(exact = false) value =
  Baseline.Literal { pos = ("test/t.ml", line, column, 40); value; exact }

let old = literal " old "

(* Two literals at column 14, of lines 2 and 3. *)
let two x y =
  strf
    "let () =\n\
    \  expect a @@ __POS_OF__ {| %s |};\n\
    \  expect b @@ __POS_OF__ {| %s |}\n"
    x y

(* An expect test whose body ends 38 bytes after the start of its head's
   line. *)
let expect_test = "let%expect_test _ =\n  print_string \"x\"\n"
let trailing = Baseline.Trailing { pos = ("test/t.ml", 1, 0, 38) }
let drift = Source_patch.error_message (Drifted ("test/t.ml", 2, 19, 40))

let unreadable =
  "the source file cannot be read: " ^ Unix.error_message Unix.ENOENT

(* The correction that baseline.mli names for a subject: Source_patch's
   rewrite of the literal to the text, or its insertion after the body. *)
let corrected source corrections =
  let patch = function
    | Baseline.Literal { pos; value; exact }, actual ->
        let style = if exact then Source_patch.Exact else Flexible in
        Source_patch.patch ~site:pos ~literal:value ~style actual
    | Trailing { pos }, actual -> Source_patch.trailing ~site:pos actual
    | File path, _ -> failf "a file baseline has no patch: %s" path
  in
  require_ok (Source_patch.apply source (List.map patch corrections))

(* The reason that [write] gives for a file it cannot write at [path]. *)
let write_refusal path =
  match Os.atomic_write ~path "" with
  | () -> "the write succeeded"
  | exception e -> Os.failure_reason ~path e

(* Modes *)

let by_mode claim ~files ~steps rows =
  cases claim rows
    ~name:(fun (mode, _) -> mode_name mode)
    (fun (mode, trace) ->
      let root = project files in
      equal traced trace (scenario ~mode root (steps root)))

let differing =
  "file test/help.expected: mismatch \"new\\n\" against \"old\\n\""

let modes =
  group "Modes"
    [
      by_mode
        "a missing file fails and is not created under Check, fails and gets a \
         .corrected file under Corrected, and is created under Update"
        ~files:[]
        ~steps:(fun root ->
          [
            check file_help "hello\r\nworld";
            settle ~keep:true;
            on_disk root;
            write;
            writes root;
            on_disk root;
          ])
        [
          ( Baseline.Check,
            [
              "file test/help.expected: missing \"hello\\nworld\\n\"";
              "kept 0";
              disk [];
              disk [];
            ] );
          ( Corrected,
            [
              "file test/help.expected: missing \"hello\\nworld\\n\"";
              "kept 1";
              disk [];
              "wrote test/help.expected.corrected, 0 literals";
              disk [ file "test/help.expected.corrected" "hello\nworld\n" ];
            ] );
          ( Update,
            [
              "pass";
              "kept 1";
              disk [];
              "wrote test/help.expected, 0 literals";
              disk [ file help "hello\nworld\n" ];
            ] );
        ];
      by_mode
        "a differing file fails and is kept under Check, fails and gets a \
         .corrected file under Corrected, and is replaced under Update"
        ~files:[ (help, "old\n") ]
        ~steps:(fun root ->
          [ check file_help "new"; commit root; on_disk root ])
        [
          (Baseline.Check, [ differing; "kept 0"; disk [ file help "old\n" ] ]);
          ( Corrected,
            [
              differing;
              "kept 1";
              "wrote test/help.expected.corrected, 0 literals";
              disk
                [
                  file help "old\n"; file "test/help.expected.corrected" "new\n";
                ];
            ] );
          ( Update,
            [
              "pass";
              "kept 1";
              "wrote test/help.expected, 0 literals";
              disk [ file help "new\n" ];
            ] );
        ];
    ]

(* Subjects *)

let compares claim rows =
  cases claim rows
    ~name:(fun (name, _, _, _) -> name)
    (fun (_, subject, actual, row) ->
      equal traced [ row ]
        (scenario ~mode:Check (project []) [ check subject actual ]))

let file_compares (_, on_disk, actual, row) =
  let root = project [ (help, on_disk) ] in
  equal traced [ row ] (scenario ~mode:Check root [ check file_help actual ])

let subjects =
  group "Subjects"
    [
      cases
        "a file compares as canonical lines, where CR LF and CR are LF and a \
         final newline is added"
        ~name:(fun (name, _, _, _) -> name)
        [
          ("CR LF on disk", "hello\r\nworld", "hello\nworld\n", "pass");
          ("CR on disk", "a\rb", "a\nb\n", "pass");
          ("no final newline produced", "hello\nworld\n", "hello\nworld", "pass");
          ("an empty file and an empty text", "", "", "pass");
          ( "a difference",
            "hello\n",
            "bye",
            "file test/help.expected: mismatch \"bye\\n\" against \"hello\\n\""
          );
        ]
        file_compares;
      compares
        "a flexible literal compares through normalize on both sides, an exact \
         one byte for byte"
        [
          ( "flexible, the same lines indented otherwise",
            literal "\n    a\n      b\n  ",
            "a\n  b\n",
            "pass" );
          ( "flexible, another relative indentation",
            literal "\n    a\n      b\n  ",
            "a\nb",
            "literal: mismatch \"a\\nb\" against \"a\\n  b\"" );
          ("exact, the same bytes", literal ~exact:true " a ", " a ", "pass");
          ( "exact, the bytes trimmed",
            literal ~exact:true " a ",
            "a",
            "exact literal: mismatch \"a\" against \" a \"" );
        ];
      compares
        "a trailing node's baseline is the empty text, compared as a flexible \
         literal's, and a difference is Missing"
        [
          ("no output", trailing, "", "pass");
          ("blank output", trailing, " \n\t\n", "pass");
          ("output", trailing, "  x\n", "literal: missing \"x\"");
        ];
      test
        "two spellings of one file are two keys, and write writes the later \
         content" (fun () ->
          let root = project [] in
          equal traced
            [
              "pass";
              "pass";
              "kept 2";
              "wrote a/b.txt, 0 literals";
              disk [ file "a/b.txt" "two\n" ];
            ]
            (scenario ~mode:Update root
               [
                 check (File "a/b.txt") "one";
                 check (File "a/./b.txt") "two";
                 commit root;
                 on_disk root;
               ]));
    ]

(* Registries *)

let written_a = [ "pass"; "kept 1"; "wrote a.txt, 0 literals" ]

let relative_root () =
  let top = project [] in
  chdir top;
  let t = Baseline.create ~root:"proj" ~cwd:"proj" ~mode:Update () in
  chdir (temp_dir ());
  let root = Filename.concat (Unix.realpath top) "proj" in
  equal traced written_a (play t [ check (File "a.txt") "x"; commit root ]);
  equal traced [ file "proj/a.txt" "x\n" ] (tree top)

let relative_cwd () =
  let top =
    project
      [
        ("proj/" ^ help, "source\n"); ("proj/_build/default/" ^ help, "copy\n");
      ]
  in
  chdir top;
  let cwd = "proj/_build/default/test" in
  let t = Baseline.create ~root:"proj" ~cwd ~mode:Check () in
  chdir (temp_dir ());
  equal traced [ "pass" ] (play t [ check file_help "copy" ])

let default_root () =
  let root = project [] in
  setenv "WINDTRAP_PROJECT_ROOT" (Some root);
  let t = Baseline.create ~cwd:root ~mode:Update () in
  equal traced written_a (play t [ check (File "a.txt") "x"; commit root ])

let default_cwd () =
  let root =
    Unix.realpath
      (project [ (help, "source\n"); ("_build/default/" ^ help, "copy\n") ])
  in
  chdir (Filename.concat root "_build/default/test");
  let t = Baseline.create ~root ~mode:Check () in
  equal traced [ "pass" ] (play t [ check file_help "copy" ])

let gone_cwd () =
  if Sys.win32 then skip ~reason:"POSIX only" ();
  let gone = Filename.concat (temp_dir ()) "gone" in
  Unix.mkdir gone 0o700;
  chdir gone;
  Unix.rmdir gone;
  (match Sys.getcwd () with
  | _ -> skip ~reason:"this system reads a removed working directory" ()
  | exception Sys_error _ -> ());
  setenv "WINDTRAP_PROJECT_ROOT" (Some "relative");
  raises_match Exn.sys_error (fun () -> Baseline.create ~cwd:"/" ~mode:Check ())

let mode_witness =
  Testable.make ~equal:( = ) ~pp:(fun ppf mode ->
      Format.pp_print_string ppf (mode_name mode))

let registries =
  group "Registries"
    [
      cases "mode is the mode of create" ~name:mode_name
        [ Baseline.Check; Corrected; Update ] (fun mode ->
          equal mode_witness mode
            (Baseline.mode (Baseline.create ~root:"/" ~cwd:"/" ~mode ())));
      test "a relative root is made absolute against the current directory"
        relative_root;
      test "a relative cwd is made absolute against the current directory"
        relative_cwd;
      test "root defaults to Os.project_root ()" default_root;
      test "cwd defaults to the current directory" default_cwd;
      cases
        "a run whose cwd lies in a build context under the root reads dune's \
         copy"
        ~name:Fun.id [ "_build/default"; "_build/.sandbox/3f/default" ]
        (fun context ->
          let root =
            project [ (help, "source\n"); (context ^ "/" ^ help, "copy\n") ]
          in
          let cwd = Filename.concat root (context ^ "/test") in
          equal traced [ "pass" ]
            (scenario ~mode:Check ~cwd root [ check file_help "copy" ]));
      test "a build context under another root is no build action of the run"
        (fun () ->
          let root = project [ (help, "source\n") ] in
          let other = project [ ("_build/default/" ^ help, "elsewhere\n") ] in
          let cwd = Filename.concat other "_build/default/test" in
          equal traced [ "pass" ]
            (scenario ~mode:Check ~cwd root [ check file_help "source" ]));
      test "create raises Sys_error as Os.project_root does" gone_cwd;
    ]

(* Checking *)

let unresolvable (mode, (_, subject, path, compared)) =
  let root = project [] in
  let candidate =
    match Os.reconstruct ~root path with
    | Error candidate -> candidate
    | Ok proven -> "proven " ^ proven
  in
  equal traced
    [ strf "%s: unresolvable %S" compared candidate; "kept 0" ]
    (scenario ~mode root [ check subject "v"; commit root ])

let unproven =
  [
    ("a file", Baseline.File "../x", "../x", "file ../x");
    ( "a literal",
      Baseline.Literal
        { pos = ("../t.ml", 1, 0, 0); value = "v"; exact = false },
      "../t.ml",
      "literal" );
    ( "a trailing node",
      Baseline.Trailing { pos = ("../t.ml", 1, 0, 0) },
      "../t.ml",
      "literal" );
  ]

let located () =
  let t = Baseline.create ~root:(project []) ~cwd:"/" ~mode:Check () in
  let at ?loc () =
    match Baseline.check t ?loc (File "x") "v" with
    | () -> "pass"
    | exception Failure.Check_failure { loc = None; _ } -> "no location"
    | exception Failure.Check_failure { loc = Some l; _ } ->
        strf "%s:%d:%d" l.file l.line l.column
  in
  let without = at () in
  let given = at ~loc:{ Loc.file = "test/t.ml"; line = 7; column = 2 } () in
  equal traced [ "no location"; "test/t.ml:7:2" ] [ without; given ]

let bounded () =
  let root = project [] in
  let big =
    String.concat "" (List.init 20_000 (fun i -> string_of_int i ^ "\n"))
  in
  equal traced
    [
      "file test/help.expected: missing " ^ text (Failure.text big);
      "kept 1";
      "wrote test/help.expected.corrected, 0 literals";
    ]
    (scenario ~mode:Corrected root [ check file_help big; commit root ]);
  equal string big (read (Filename.concat root (help ^ ".corrected")))

let refused_literal (_, mode, files, reason) =
  let root = project files in
  let before = tree root in
  equal traced
    [
      strf "literal: mismatch \"new\" against \"old\", refused at line 2: %s"
        reason;
      "kept 0";
      disk before;
    ]
    (scenario ~mode root [ check old "new"; commit root; on_disk root ])

let first_read () =
  let root = project [ ("test/t.ml", two "x" "y") ] in
  equal traced
    [
      "pass";
      "pass";
      "kept 2";
      "refused test/t.ml: it changed during the run: " ^ drift;
    ]
    (scenario ~mode:Update root
       [
         check (literal ~column:14 " x ") "one";
         edit root "test/t.ml" (two "x" "edited");
         check (literal ~line:3 ~column:14 " y ") "two";
         commit root;
       ])

let checking =
  group "Checking"
    [
      cases
        "a path that cannot be proven under the root is Unresolvable in every \
         mode, and records nothing"
        ~name:(fun (mode, (name, _, _, _)) ->
          strf "%s, %s" name (mode_name mode))
        (List.concat_map
           (fun mode -> List.map (fun subject -> (mode, subject)) unproven)
           [ Baseline.Check; Corrected; Update ])
        unresolvable;
      test "a file is read at the first check of its key, and not again"
        (fun () ->
          let root = project [ (help, "one\n") ] in
          equal traced
            [
              "pass";
              "pass";
              "file test/help.expected: mismatch \"two\\n\" against \"one\\n\"";
            ]
            (scenario ~mode:Check root
               [
                 check file_help "one";
                 edit root help "two\n";
                 check file_help "one";
                 check file_help "two";
               ]));
      cases
        "an accepted key compares with its accepted content alone, and another \
         content is a conflict that records nothing"
        ~name:(fun (mode, _) -> mode_name mode)
        [
          (Baseline.Corrected, "file test/help.expected: missing \"hi\\n\"");
          (Update, "pass");
        ]
        (fun (mode, first) ->
          equal traced
            [
              first;
              "pass";
              "file test/help.expected: mismatch \"yo\\n\" against \"hi\\n\", \
               conflict";
              "kept 1";
            ]
            (scenario ~mode (project [])
               [
                 check file_help "hi";
                 check file_help "hi";
                 check file_help "yo";
                 settle ~keep:true;
               ]));
      cases
        "a check with ~correct:false fails as under Check, whatever the mode"
        ~name:mode_name [ Baseline.Corrected; Update ] (fun mode ->
          equal traced
            [ "file x: missing \"v\\n\""; "kept 0" ]
            (scenario ~mode (project [])
               [ check ~correct:false (File "x") "v"; settle ~keep:true ]));
      test "a check computes no location and carries the one given" located;
      test "a failure bounds its texts, and the correction holds actual whole"
        bounded;
      test "a check raises Sys_error on a file that exists and cannot be read"
        (fun () ->
          let root = project [ ("dir.expected/x", "") ] in
          let t = Baseline.create ~root ~cwd:root ~mode:Check () in
          raises_match Exn.sys_error (fun () ->
              Baseline.check t (File "dir.expected") "v"));
      cases
        "a literal whose source cannot take its correction fails, refused, \
         whatever the mode, and records nothing"
        ~name:(fun (name, _, _, _) -> name)
        [
          ( "a drifted source under Corrected",
            Baseline.Corrected,
            [ ("test/t.ml", edited) ],
            drift );
          ( "a drifted source under Update",
            Update,
            [ ("test/t.ml", edited) ],
            drift );
          ("a missing source under Corrected", Corrected, [], unreadable);
          ("a missing source under Update", Update, [], unreadable);
        ]
        refused_literal;
      test "under Check a literal's source is not read" (fun () ->
          equal traced
            [ "literal: mismatch \"new\" against \"old\"" ]
            (scenario ~mode:Check (project []) [ check old "new" ]));
      test "a source unreadable at its first check stays so for the run"
        (fun () ->
          let root = project [] in
          let refusal =
            "literal: mismatch \"new\" against \"old\", refused at line 2: "
            ^ unreadable
          in
          equal traced [ refusal; refusal ]
            (scenario ~mode:Update root
               [
                 check old "new"; edit root "test/t.ml" source; check old "new";
               ]));
      test "a check tries its patch on the source as first read" first_read;
    ]

(* Settling *)

let settling =
  group "Settling"
    [
      test
        "settle ~keep:true is the number of corrections of the attempt, which \
         it closes" (fun () ->
          equal traced
            [ "pass"; "pass"; "kept 2"; "kept 0" ]
            (scenario ~mode:Update (project [])
               [
                 check (File "a") "1";
                 check (File "b") "2";
                 settle ~keep:true;
                 settle ~keep:true;
               ]));
      test
        "settle ~keep:false is 0, and a key it drops takes the next attempt's \
         correction" (fun () ->
          let root = project [] in
          equal traced
            [
              "file test/help.expected: missing \"hi\\n\"";
              "kept 0";
              "file test/help.expected: missing \"yo\\n\"";
              "kept 1";
              "wrote test/help.expected.corrected, 0 literals";
            ]
            (scenario ~mode:Corrected root
               [
                 check file_help "hi";
                 settle ~keep:false;
                 check file_help "yo";
                 commit root;
               ]));
      test
        "settle ~keep:false drops every correction of the attempt, and the \
         next attempt compares with the baselines as first read" (fun () ->
          let root = project [ (help, "one\n") ] in
          equal traced
            [
              "pass";
              "pass";
              "kept 0";
              "pass";
              "kept 0";
              disk [ file help "one\n" ];
            ]
            (scenario ~mode:Update root
               [
                 check file_help "two";
                 check (File "b") "2";
                 settle ~keep:false;
                 check file_help "one";
                 commit root;
                 on_disk root;
               ]));
      test "settle ~keep:false leaves the attempts kept before" (fun () ->
          let root = project [] in
          equal traced
            [ "pass"; "kept 1"; "pass"; "kept 0"; "wrote a, 0 literals" ]
            (scenario ~mode:Update root
               [
                 check (File "a") "1";
                 settle ~keep:true;
                 check (File "b") "2";
                 settle ~keep:false;
                 write;
                 writes root;
               ]));
    ]

(* Writing *)

let build = "_build/default/"
let new_ = corrected source [ (old, "new") ]

let corrected_in_build () =
  let root =
    project
      [
        (help, "source\n");
        ("test/t.ml", edited);
        (build ^ help, "copy\n");
        (build ^ "test/t.ml", source);
      ]
  in
  equal traced
    [
      "pass";
      "file test/other.expected: missing \"x\\n\"";
      "literal: mismatch \"new\" against \"old\"";
      "kept 2";
      "wrote _build/default/test/other.expected.corrected, 0 literals";
      "wrote _build/default/test/t.ml.corrected, 1 literals";
      disk
        [
          file (build ^ help) "copy\n";
          file (build ^ "test/other.expected.corrected") "x\n";
          file (build ^ "test/t.ml") source;
          file (build ^ "test/t.ml.corrected") new_;
          file help "source\n";
          file "test/t.ml" edited;
        ];
    ]
    (scenario ~mode:Corrected
       ~cwd:(Filename.concat root (build ^ "test"))
       root
       [
         check file_help "copy";
         check (File "test/other.expected") "x";
         check old "new";
         commit root;
         on_disk root;
       ])

let refused_alone () =
  if Sys.win32 then skip ~reason:"POSIX only" ();
  if Unix.geteuid () = 0 then
    skip ~reason:"root writes a read-only directory" ();
  let root = project [ ("test/t.ml", source); ("ro/.keep", "") ] in
  let read_only = Filename.concat root "ro" in
  let lock _t =
    Unix.chmod read_only 0o500;
    []
  in
  let trace =
    Fun.protect
      ~finally:(fun () -> Unix.chmod read_only 0o700)
      (fun () ->
        scenario ~mode:Update root
          [
            check old "new";
            check (File "ro/sub/x") "v";
            check (File "z.txt") "z";
            settle ~keep:true;
            lock;
            write;
            writes root;
          ])
  in
  equal traced
    [
      "pass";
      "pass";
      "pass";
      "kept 3";
      strf "refused ro/sub/x: cannot create directory %s: %s"
        (Os.display_path (Filename.concat read_only "sub"))
        (Unix.error_message EACCES);
      "wrote test/t.ml, 1 literals";
      "wrote z.txt, 0 literals";
    ]
    trace;
  equal traced
    [ file "ro/.keep" ""; file "test/t.ml" new_; file "z.txt" "z\n" ]
    (tree root)

let path_order () =
  let root = project [ ("test/b.ml", source) ] in
  let a = Filename.concat root "test/a.ml" in
  let c = Filename.concat root "test/c.ml" in
  let occupy _t =
    Os.mkdir_p a;
    Os.mkdir_p c;
    []
  in
  let b =
    Baseline.Literal
      { pos = ("test/b.ml", 2, 19, 40); value = " old "; exact = false }
  in
  let trace =
    scenario ~mode:Update root
      [
        check (File "test/c.ml") "c";
        check b "new";
        check (File "test/a.ml") "a";
        settle ~keep:true;
        occupy;
        write;
        writes root;
      ]
  in
  equal traced
    [
      "pass";
      "pass";
      "pass";
      "kept 3";
      "refused test/a.ml: " ^ write_refusal a;
      "wrote test/b.ml, 1 literals";
      "refused test/c.ml: " ^ write_refusal c;
    ]
    trace

let writing =
  group "Writing"
    [
      test
        "under Corrected a literal's correction goes to a .corrected copy of \
         its source" (fun () ->
          let root = project [ ("test/t.ml", source) ] in
          equal traced
            [
              "literal: mismatch \"new\" against \"old\"";
              "kept 1";
              "wrote test/t.ml.corrected, 1 literals";
              disk [ file "test/t.ml" source; file "test/t.ml.corrected" new_ ];
            ]
            (scenario ~mode:Corrected root
               [ check old "new"; commit root; on_disk root ]));
      test
        "under Update the literals of one source are patched together, in place"
        (fun () ->
          let root = project [ ("test/t.ml", two "x" "y") ] in
          let x = literal ~column:14 " x " in
          let y = literal ~line:3 ~column:14 " y " in
          equal traced
            [
              "pass";
              "pass";
              "kept 2";
              "wrote test/t.ml, 2 literals";
              disk
                [
                  file "test/t.ml"
                    (corrected (two "x" "y") [ (x, "one"); (y, "two") ]);
                ];
            ]
            (scenario ~mode:Update root
               [ check x "one"; check y "two"; commit root; on_disk root ]));
      cases
        "a correction is Source_patch's rewrite of the literal, or insertion \
         of the node, to the produced text"
        ~name:(fun (name, _, _, _) -> name)
        [
          ("a flexible literal", source, old, "a\n  b");
          ("an exact literal", source, literal ~exact:true " old ", "a\n b");
          ( "an exact literal with a CR",
            source,
            literal ~exact:true " old ",
            "a\r\nb" );
          ("a trailing node", expect_test, trailing, "x\n");
        ]
        (fun (_, text, subject, actual) ->
          let root = project [ ("test/t.ml", text) ] in
          equal traced
            [
              "pass";
              "kept 1";
              "wrote test/t.ml, 1 literals";
              disk [ file "test/t.ml" (corrected text [ (subject, actual) ]) ];
            ]
            (scenario ~mode:Update root
               [ check subject actual; commit root; on_disk root ]));
      test
        "under a build action Corrected writes beside dune's copies, patched \
         from the copy"
        corrected_in_build;
      test "under a build action Update writes the source, and never the copy"
        (fun () ->
          let root =
            project [ (build ^ "test/t.ml", source); ("test/t.ml", source) ]
          in
          equal traced
            [
              "pass";
              "kept 1";
              "wrote test/t.ml, 1 literals";
              disk [ file (build ^ "test/t.ml") source; file "test/t.ml" new_ ];
            ]
            (scenario ~mode:Update
               ~cwd:(Filename.concat root (build ^ "test"))
               root
               [ check old "new"; commit root; on_disk root ]));
      test "an equal baseline writes nothing" (fun () ->
          let root = project [ (help, "keep\r\n") ] in
          equal traced
            [ "pass"; "kept 0"; disk [ file help "keep\r\n" ] ]
            (scenario ~mode:Update root
               [ check file_help "keep"; commit root; on_disk root ]));
      test "a second write writes nothing" (fun () ->
          let root = project [] in
          equal traced
            [
              "pass";
              "kept 1";
              "wrote test/help.expected, 0 literals";
              disk [ file help "edited\n" ];
            ]
            (scenario ~mode:Update root
               [
                 check file_help "first";
                 settle ~keep:true;
                 write;
                 edit root help "edited\n";
                 write;
                 writes root;
                 on_disk root;
               ]));
      test
        "under Update a file is replaced whatever happened to it since its \
         check" (fun () ->
          let root = project [ (help, "old\n") ] in
          equal traced
            [
              "pass";
              "kept 1";
              "wrote test/help.expected, 0 literals";
              disk [ file help "new\n" ];
            ]
            (scenario ~mode:Update root
               [
                 check file_help "new";
                 edit root help "edited since\n";
                 commit root;
                 on_disk root;
               ]));
      test "a source changed since its check is refused and left as it is"
        (fun () ->
          let root = project [ ("test/t.ml", source) ] in
          equal traced
            [
              "pass";
              "kept 1";
              "refused test/t.ml: it changed during the run: " ^ drift;
              disk [ file "test/t.ml" edited ];
            ]
            (scenario ~mode:Update root
               [
                 check old "new";
                 edit root "test/t.ml" edited;
                 commit root;
                 on_disk root;
               ]));
      (* A directory in the source's place: opening it succeeds and reading
         it fails. *)
      test
        "a source that cannot be read at the write is refused, for a reason \
         that names no path" (fun () ->
          if Sys.win32 then
            skip ~reason:"a directory does not open as a file here" ();
          let root = project [ ("test/t.ml", source) ] in
          let replace _t =
            Sys.remove (Filename.concat root "test/t.ml");
            Unix.mkdir (Filename.concat root "test/t.ml") 0o700;
            []
          in
          equal traced
            [
              "pass";
              "kept 1";
              "refused test/t.ml: the source file cannot be read: "
              ^ Unix.error_message EISDIR;
            ]
            (scenario ~mode:Update root
               [ check old "new"; replace; commit root ]));
      (* A directory where the file goes: the rename over it fails. *)
      test "a file whose write fails is refused" (fun () ->
          let root = project [] in
          let path = Filename.concat root "out/x" in
          let occupy _t =
            Os.mkdir_p path;
            []
          in
          let trace =
            scenario ~mode:Update root
              [ check (File "out/x") "v"; occupy; commit root ]
          in
          equal traced
            [ "pass"; "kept 1"; "refused out/x: " ^ write_refusal path ]
            trace);
      test
        "a directory that cannot be created refuses its file alone, and the \
         files after it are written"
        refused_alone;
      test "writes lists written and refused files in the order of their paths"
        path_order;
    ]

let () =
  exit
    (run "baseline"
       [ modes; subjects; registries; checking; settling; writing ])
