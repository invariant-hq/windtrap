(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for `windtrap mutants`: the merge that makes a project-level
   mutation report true, and the surface around it.

   The subject is bin/main.exe, spawned as a subprocess. It merges two
   kinds of verdict file. Most are synthetic, written with the runtime's
   own serializer: the command runs no tests and drives no build, so
   discovery, the explicit-PATH contract, the staleness pass, the file
   format's rejections and every exit code need neither the instrumenter
   nor a mutation run, and stating the data by hand is what makes the
   counts exact. The first test is not synthetic (two instrumented
   executables over one library, each running its own mutation loop and
   writing its own verdict file), because the claim the command is built
   on (two executables that disagree merge to something truer than
   either) is a claim about real runs, and a fixture that assumed it
   could not test it.

   What is checked: killed-anywhere-wins across three files (including
   the case the whole verdict file exists for, one suite killing what
   another merely reaches), the union of a survivor's witnesses, each
   tagged with the executable that ran it, a survivor that survives
   everywhere, the never-reached rows, the colours as raw bytes, discovery
   under _build/_mutants and through explicit PATH arguments, the
   staleness pass, and every exit code (1 when a mutant survived the
   merge, 0 when none did, unreached mutants alone staying green).

   A windtrap suite ([run] executes tests sequentially in declaration
   order); every subject under test is a spawned process, so hosting the
   assertions under the windtrap runner nests nothing. *)

open Windtrap
module M = Windtrap_runtime.Mutate
module V = Windtrap_runtime.Verdicts
module Child = Windtrap_test_support.Child

(* [occurs ~sub s] is [true] iff [sub] occurs in [s]: a predicate, for
   finding and counting lines, where the facade's [contains] asserts. *)
let occurs ~sub s =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0

let lines_with ~sub text =
  List.filter (occurs ~sub) (String.split_on_char '\n' text)

(* Scratch and process helpers *)

(* Hermeticity: absolute paths throughout, so the test behaves the same
   under dune's sandbox and by hand; each test's scratch lives in its own
   temp_dir. Nothing is ever written under the real _build/_mutants. *)
let exe_dir = Filename.dirname Sys.executable_name

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

(* A verdict file, written as a mutation run writes it. *)
let save ?identity path t =
  mkdir_p (Filename.dirname path);
  V.save ?identity path t

(* [capture ?cwd ?color ?env ?exe args] runs [exe] (the windtrap binary by
   default) and returns (exit code, stdout, stderr). The environment is
   stated in full rather than extended: this suite asserts on
   transcripts byte for byte, and a WINDTRAP_VERBOSE in a developer's
   shell would reshape them. Color is off so the report is comparable
   bytes, except where a test asks for [~color:"always"] to read the
   escape codes themselves. *)
let capture ?cwd ?(color = "never") ?(env = []) ?(exe = windtrap_exe) args =
  let r = Child.run ?cwd ~env:(("WINDTRAP_COLOR", color) :: env) exe args in
  (Child.exit_code r, r.Child.out, r.Child.err)

let mutate ?cwd ?color args = capture ?cwd ?color ("mutants" :: args)

(* The one remedy every exclusion names: build-neutral, since the command
   does not know how the suite is run. *)
let rerun =
  "re-run every suite with its mutants (--mutate, instrumented with \
   ppx_windtrap.mutate, forcing the runs your build tool cached), then merge \
   again"

(* The fixture: one library, three test executables' verdicts *)

let calc = "lib/calc.ml"
let util = "lib/util.ml"

(* The four mutants, each spelled once and applied to a verdict: three in
   calc.ml, one in util.ml. Every field is what an instrumented build
   would have recorded, renderings included. The report is drawn from
   them and from nothing else. *)
let mutant ~file ~line ~col ~rewrite ~before ~after verdict =
  { V.id = { M.file; line; col; rewrite }; before; after; verdict }

let m_add =
  mutant ~file:calc ~line:1 ~col:14 ~rewrite:"add" ~before:"a + b"
    ~after:"a - b"

let m_sub =
  mutant ~file:calc ~line:2 ~col:14 ~rewrite:"sub" ~before:"a - b"
    ~after:"a + b"

let m_lt =
  mutant ~file:calc ~line:3 ~col:14 ~rewrite:"lt" ~before:"a < b"
    ~after:"a <= b"

let m_or =
  mutant ~file:util ~line:1 ~col:13 ~rewrite:"or" ~before:"p || q"
    ~after:"p && q"

let m_and =
  mutant ~file:util ~line:3 ~col:22 ~rewrite:"and" ~before:"p && q"
    ~after:"p || q"

let collection records = List.fold_left V.add V.empty records

(* The summary line is the report's last word and its whole contract for
   a project with nothing else to say; assert it whole rather than by
   fragment. *)
let summary out =
  match
    List.filter
      (String.starts_with ~prefix:"mutants: ")
      (String.split_on_char '\n' out)
  with
  | [ line ] -> line
  | _ -> ""

(* The one term that legitimately differs between a merge of three files
   and the same data written as one: the executables count. *)
let without_executables_term out =
  String.concat "\n"
    (List.map
       (fun line ->
         if String.starts_with ~prefix:"mutants: " line then
           match String.rindex_opt line ',' with
           | Some i -> String.sub line 0 i
           | None -> line
         else line)
       (String.split_on_char '\n' out))

(* Three executables over one library, which is the normal case:
   - [add] is killed by A and merely reached by B. The truth is killed;
     reporting B's view alone is the false survivor this command exists
     to prevent.
   - [sub] survives in B and in C, with different witnesses, and is
     unreached in A: a survivor everywhere it was reached, whose witness
     list is the union.
   - [lt] survives in A and crashes its child in B: a crash is a kill.
   - [or] and [and] are unreached in all three, and share a file: the
     unreached total counts mutants, not the lines or the files that
     carry them. *)
let file_a =
  collection
    [
      m_add V.Killed;
      m_sub V.Unreached;
      m_lt (V.survived [ [ "calc"; "compares" ] ]);
      m_or V.Unreached;
      m_and V.Unreached;
    ]

let file_b =
  collection
    [
      m_add (V.survived [ [ "cli"; "runs" ] ]);
      m_sub (V.survived [ [ "cli"; "subtracts" ] ]);
      m_lt V.Killed;
      m_or V.Unreached;
      m_and V.Unreached;
    ]

let file_c =
  collection
    [
      m_add V.Unreached;
      m_sub (V.survived [ [ "prop"; "sub law" ] ]);
      m_lt V.Unreached;
      m_or V.Unreached;
      m_and V.Unreached;
    ]

let plant_sources root =
  write_file
    (Filename.concat root calc)
    "let add a b = a + b\nlet sub a b = a - b\nlet cmp a b = a < b\n";
  write_file
    (Filename.concat root util)
    "let ok p q = p || q\n\
     let neither p q = not (p || q)\n\
     let both p q = p && q\n"

(* The three files under a project of their own. Each test that reads
   it plants its own copy, so what one test adds to the tree no other
   test sees. *)
let proj () =
  let root = scratch "proj" in
  plant_sources root;
  List.iter
    (fun (name, t) ->
      save (Filename.concat root (Filename.concat "_build/_mutants" name)) t)
    [
      ("windtrap-a.mutants", file_a);
      ("windtrap-b.mutants", file_b);
      ("windtrap-c.mutants", file_c);
    ];
  root

(* The scenario the command exists for, with real files

   Everything above this point is synthetic, which is right for pinning
   discovery, the exit codes and the file format. It cannot pin the one
   claim the command is built on: that two instrumented executables which
   disagree about a library merge to something truer than either. So this
   part runs the real thing: two executables over Mutcli_fixture.Calc,
   each driving its own mutation loop and writing its own verdict file,
   and [windtrap mutants] over what they wrote. [pins_add] pins [add] and
   merely reaches [sub]; [pins_sub] is its mirror image; both reach
   [shared] and pin nothing about it, and neither calls [never]. Each
   executable alone therefore reports a survivor the other kills. *)

let fixture_source = read_file (Filename.concat exe_dir "calc.ml")

(* The 1-based line of a binding in calc.ml, read from the fixture rather
   than transcribed: a line moving there must re-point these assertions
   with it, instead of silently pointing them at another mutant. *)
let calc_line binding =
  let rec go n = function
    | [] -> failf "no %S binding in the fixture" binding
    | line :: rest ->
        if String.starts_with ~prefix:binding line then n else go (n + 1) rest
  in
  go 1 (String.split_on_char '\n' fixture_source)

let real_project () =
  let root = scratch "two-executables" in
  (* The instrumenter records workspace-relative paths, so the source
     the survivor excerpt resolves against is planted where it was
     recorded. *)
  write_file
    (Filename.concat root "test/instr/mutants_cmd/calc.ml")
    fixture_source;
  List.iter
    (fun name ->
      let target =
        Filename.concat root (Filename.concat "_build/default/test" name)
      in
      write_file target (read_file (Filename.concat exe_dir name));
      Unix.chmod target 0o755)
    [ "pins_add.exe"; "pins_sub.exe" ];
  root

let two_executables =
  test "two executables that disagree merge to the project's truth" @@ fun () ->
  if Sys.win32 then skip ~reason:"mutation testing needs Unix.fork" ();
  let root = real_project () in
  let at binding =
    Printf.sprintf "test/instr/mutants_cmd/calc.ml:%d:" (calc_line binding)
  in
  let add = at "let add" and sub = at "let sub" and shared = at "let shared" in
  let loop name =
    (* The scope keeps this scenario's catalogue the fixture's. Under
       --instrument-with these executables link a mutation-instrumented
       windtrap core, and the claim under test (two executables that
       disagree about ONE library merge to the truth) is about calc.ml's
       mutants, not about the core's thousand. *)
    capture ~cwd:root
      ~exe:(Filename.concat root (Filename.concat "_build/default/test" name))
      [ "--mutate=test/instr/mutants_cmd/calc.ml" ]
  in
  (* Each executable is right about what it ran and wrong about the
     project. *)
  let code, out, err = loop "pins_add.exe" in
  equal ~msg:"the first executable's loop exits 0" int 0 code;
  equal ~msg:"the first executable's loop keeps stderr empty" text "" err;
  contains ~msg:"it calls the mutant its sibling kills a survivor"
    ~sub:("SURVIVED  " ^ sub) out;
  not_contains ~msg:"and kills the one it pins itself" ~sub:("SURVIVED  " ^ add)
    out;
  let code, out, err = loop "pins_sub.exe" in
  equal ~msg:"the second executable's loop exits 0" int 0 code;
  equal ~msg:"the second executable's loop keeps stderr empty" text "" err;
  contains ~msg:"it calls the other's kill a survivor" ~sub:("SURVIVED  " ^ add)
    out;
  not_contains ~msg:"and kills the one it pins itself" ~sub:("SURVIVED  " ^ sub)
    out;
  (* And neither false survivor survives the merge. This is the whole
     claim: reporting either executable's view alone sends the reader to
     write a test that already exists. *)
  let code, out, err = mutate ~cwd:root [] in
  equal ~msg:"the merged report exits 1: a mutant survived everywhere" int 1
    code;
  equal ~msg:"the merged report keeps stderr empty" text "" err;
  not_contains ~msg:"a mutant killed by one executable is not a survivor"
    ~sub:("SURVIVED  " ^ add) out;
  not_contains ~msg:"nor is the one killed by the other"
    ~sub:("SURVIVED  " ^ sub) out;
  contains ~msg:"the survivor is the one neither executable pinned"
    ~sub:("SURVIVED  " ^ shared) out;
  contains ~msg:"its witnesses are both executables' tests"
    ~sub:"2 tests in 2 executables ran this line and none failed:" out;
  (* Each witness names the executable that ran it, by the basename of
     the identity its verdict file recorded. *)
  contains ~msg:"the first executable's witness, in its column"
    ~sub:"\n      pins_add.exe  shared \u{203a} shared is nonzero\n" out;
  contains ~msg:"the second executable's witness, in its column"
    ~sub:"\n      pins_sub.exe  shared \u{203a} shared is not 99\n" out;
  contains ~msg:"the excerpt is drawn from the planted source"
    ~sub:"let shared a b = a + b" out;
  contains ~msg:"the mutant neither executable reached is still a finding"
    ~sub:
      (Printf.sprintf "\n  1  test/instr/mutants_cmd/calc.ml   lines %d\n"
         (calc_line "let never"))
    out;
  equal ~msg:"the project's summary, whole" text
    "mutants: 1 survived of 3 reached, 2 killed, 1 never reached, 2 executables"
    (summary out);
  (* The one command names the executable of the survivor's first
     reaching-test row, as dune runs it: the identity its verdict file
     recorded, less the build context. Its identifier is pasted back into
     that executable, the only proof that it is one the runtime
     resolves. *)
  let launcher =
    "reproduce: dune exec --instrument-with ppx_windtrap.mutate \
     test/pins_add.exe -- --arm "
  in
  let reproduce =
    match
      List.filter
        (String.starts_with ~prefix:"reproduce: ")
        (String.split_on_char '\n' out)
    with
    | [ line ] -> line
    | lines -> failf "%d reproduce lines in:\n%s" (List.length lines) out
  in
  starts_with ~msg:"the command is dune's, over the recorded executable"
    ~affix:(launcher ^ shared) reproduce;
  let id =
    String.sub reproduce (String.length launcher)
      (String.length reproduce - String.length launcher)
  in
  let code, armed, _ =
    capture ~cwd:root
      ~exe:(Filename.concat root "_build/default/test/pins_add.exe")
      [ "--arm"; id ]
  in
  equal ~msg:"the armed run's exit code (this mutant survives)" int 0 code;
  contains ~msg:"the pasted identifier armed the survivor"
    ~sub:("mutant " ^ id ^ " armed: a + b \u{2192} a - b")
    armed

(* The merge *)

let merge_report =
  test "killed anywhere wins across three executables" @@ fun () ->
  let proj = proj () in
  let code, out, err = mutate ~cwd:proj [] in
  equal ~msg:"the merged report exits 1: a mutant survived everywhere" int 1
    code;
  equal ~msg:"the merged report keeps stderr empty" text "" err;
  (* The load-bearing case: A killed [add], B only reached it. A report
     that listed it would send the reader to write a test that exists. *)
  not_contains ~msg:"a mutant killed by one suite is not a survivor"
    ~sub:"lib/calc.ml:1:14:add" out;
  (* A crash in one executable outranks survival in another. *)
  not_contains ~msg:"a crash in one suite kills for the project"
    ~sub:"lib/calc.ml:3:14:lt" out;
  (* The renderings come from the file: the catalogue lives in binaries
     this command never links, so a report drawn without them would be
     strictly worse than the per-executable one. Files that record no
     executable are reached through the build: the command arms the
     survivor there, by its identifier. Witnesses union across the two
     executables that reached it, each naming its executable; these
     files record no identity, so the label is the file's own name (the
     staleness tests pin the identity case). Unreached is a finding with
     its own remedy, and prints by default: one row per file, how many of
     its mutants no test reached and the lines they are on, never a block
     per mutant. The sections at rest, whole: the survivors counted, no
     blank line just inside a rule, the command above the outcome, and
     the summary the project's, over three executables. *)
  equal ~msg:"the report, whole" text
    "───────────────────── survivors (1) ──────────────────────\n\
    \  SURVIVED  lib/calc.ml:2:14:sub  a - b \u{2192} a + b\n\
    \      2 \u{2502} let sub a b = a - b\n\n\
    \    2 tests in 2 executables ran this line and none failed:\n\
    \      windtrap-b.mutants  cli \u{203a} subtracts\n\
    \      windtrap-c.mutants  prop \u{203a} sub law\n\
     ──────────────────────────────────────────────────────────\n\n\
     ─────────────────── never reached (2) ────────────────────\n\
    \  2  lib/util.ml   lines 1, 3\n\
     ──────────────────────────────────────────────────────────\n\n\
     reproduce: WINDTRAP_MUTATE_ARM=lib/calc.ml:2:14:sub dune runtest --force \
     --instrument-with ppx_windtrap.mutate\n\
     mutants: 1 survived of 3 reached, 2 killed, 2 never reached, 3 executables\n"
    out;
  (* The same report with colour on, read as the bytes it is: SURVIVED
     red, a never-reached count yellow, the counts in the suite summary's
     palette, the reproduce command plain. *)
  let code, coloured, _ = mutate ~color:"always" ~cwd:proj [] in
  equal ~msg:"colour changes no exit code" int 1 code;
  contains ~msg:"SURVIVED is red, the identifier bold"
    ~sub:
      "  \027[31mSURVIVED\027[0m  \027[1mlib/calc.ml:2:14:sub\027[0m  a - b \
       \u{2192} a + b\n"
    coloured;
  contains ~msg:"a never-reached count is yellow, the file and lines plain"
    ~sub:"\n  \027[33m2\027[0m  lib/util.ml   lines 1, 3\n" coloured;
  contains ~msg:"the counts are coloured as the suite summary's are"
    ~sub:
      "\n\
       mutants: \027[31m1 survived\027[0m of 3 reached, \027[32m2 \
       killed\027[0m, \027[33m2 never reached\027[0m, 3 executables\n"
    coloured;
  contains ~msg:"the reproduce command is never coloured"
    ~sub:
      "\n\
       reproduce: WINDTRAP_MUTATE_ARM=lib/calc.ml:2:14:sub dune runtest \
       --force --instrument-with ppx_windtrap.mutate\n"
    coloured

(* A witness row or the sentence over it: the one part of a report that
   legitimately differs between a merge of three files and the same data
   written as one, because it names the file each witness came from. *)
let is_witness_line line =
  occurs ~sub:"\u{203a}" line || occurs ~sub:"ran this line" line

let without_witnesses out =
  String.concat "\n"
    (List.filter
       (fun line -> not (is_witness_line line))
       (String.split_on_char '\n' out))

let merge_is_total =
  test "every discovered file reaches the merge" @@ fun () ->
  (* The reference is the merge computed by the runtime, written as one
     file into an identical project: identical verdicts, blocks, excerpts
     and counts mean the command folded all three and folded them the
     runtime's way. A dropped file or a re-ordered fold moves bytes. The
     witness rows are compared as a set of names, because the merged
     file cannot say which executable ran which. That attribution is
     the command's own, and the merge test above pins it. *)
  let proj = proj () in
  let reference = scratch "reference" in
  plant_sources reference;
  save
    (Filename.concat reference "_build/_mutants/merged.mutants")
    (V.merge (V.merge file_a file_b) file_c);
  let _, expected, _ = mutate ~cwd:reference [] in
  not_equal ~msg:"the reference report is not empty" text "" expected;
  let expected = without_executables_term expected in
  let _, out, _ = mutate ~cwd:proj [] in
  equal ~msg:"three files merge to the runtime's verdicts" text
    (without_witnesses expected)
    (without_witnesses (without_executables_term out));
  List.iter
    (fun witness ->
      contains ~msg:"the reference names the witness" ~sub:witness expected;
      contains ~msg:"and so does the merge" ~sub:witness out)
    [ "cli \u{203a} subtracts"; "prop \u{203a} sub law" ];
  (* And the argument order is not part of the answer. *)
  let named name = Filename.concat proj ("_build/_mutants/" ^ name) in
  let _, reversed, _ =
    mutate ~cwd:proj
      [
        named "windtrap-c.mutants";
        named "windtrap-a.mutants";
        named "windtrap-b.mutants";
      ]
  in
  equal ~msg:"reversed argument order renders identically" text out reversed

let single_file =
  test "one executable's file alone still reports its own view" @@ fun () ->
  (* The contrast that makes the merge worth having: B alone calls [add]
     a survivor. Reading B's file alone must say so (the command reports
     what it was given), which is exactly why narrowing the merge by
     accident has to be loud. *)
  let proj = proj () in
  let code, out, _ =
    mutate ~cwd:proj
      [ Filename.concat proj "_build/_mutants/windtrap-b.mutants" ]
  in
  equal ~msg:"a single file with survivors exits 1" int 1 code;
  contains ~msg:"B alone counts two survivors" ~sub:"survivors (2)" out;
  in_order
    ~msg:
      "B alone reports the false survivor, and equal witness counts leave \
       survivors in identifier order"
    ~subs:[ "SURVIVED  lib/calc.ml:1:14:add"; "SURVIVED  lib/calc.ml:2:14:sub" ]
    out;
  contains ~msg:"one executable's witnesses still name it"
    ~sub:"\n      windtrap-b.mutants  cli \u{203a} runs\n" out;
  equal ~msg:"B alone scores itself" text
    "mutants: 2 survived of 3 reached, 1 killed, 2 never reached, 1 executable"
    (summary out)

let clean_report =
  test "a project with nothing to report is one line" @@ fun () ->
  let root = scratch "clean" in
  plant_sources root;
  save
    (Filename.concat root "_build/_mutants/all.mutants")
    (collection [ m_add V.Killed; m_sub V.Killed; m_lt V.Killed ]);
  let code, out, err = mutate ~cwd:root [] in
  equal ~msg:"a clean project exits 0" int 0 code;
  equal ~msg:"a clean project keeps stderr empty" text "" err;
  equal ~msg:"and prints exactly the summary" text
    "mutants: 3 reached, 3 killed, 1 executable\n" out

let only_unreached =
  test "unreached mutants alone are a finding, not a failure" @@ fun () ->
  (* A mutant no executable reaches has no test to strengthen; the remedy
     is a new test, which is coverage's kind of finding. It is listed and
     it is not scored, so the merge stays green. *)
  let root = scratch "unreached-only" in
  plant_sources root;
  save
    (Filename.concat root "_build/_mutants/all.mutants")
    (collection [ m_add V.Killed; m_sub V.Killed; m_or V.Unreached ]);
  let code, out, err = mutate ~cwd:root [] in
  equal ~msg:"only unreached mutants exit 0" int 0 code;
  equal ~msg:"and warn about nothing" text "" err;
  not_contains ~msg:"there is no survivor section" ~sub:"survivors (" out;
  contains ~msg:"the never-reached row still prints"
    ~sub:"\n  1  lib/util.ml   lines 1\n" out;
  starts_with ~msg:"and its rule opens the report: nothing precedes it"
    ~affix:"\u{2500}" out;
  not_contains ~msg:"no survivor, no mutant to arm: no command"
    ~sub:"reproduce:" out;
  equal ~msg:"the summary counts it without a survived term" text
    "mutants: 2 reached, 2 killed, 1 never reached, 1 executable" (summary out)

let outside_tests =
  test "a site evaluated outside tests is not called never reached" @@ fun () ->
  (* [or] ran only at module initialization in A and was never evaluated
     in B: it ran, so the merge does not list it as never reached. [and]
     is never reached in both. A test's verdict in one executable outranks
     an evaluation outside tests in another, as [sub] shows. *)
  let root = scratch "outside-tests" in
  plant_sources root;
  save
    (Filename.concat root "_build/_mutants/a.mutants")
    (collection
       [ m_sub V.Outside_tests; m_or V.Outside_tests; m_and V.Unreached ]);
  save
    (Filename.concat root "_build/_mutants/b.mutants")
    (collection [ m_sub V.Killed; m_or V.Unreached; m_and V.Unreached ]);
  let code, out, err = mutate ~cwd:root [] in
  equal ~msg:"nothing survived: exit 0" int 0 code;
  equal ~msg:"stderr" text "" err;
  equal ~msg:"the report, whole" text
    "─────────────────── never reached (1) ────────────────────\n\
    \  1  lib/util.ml   lines 3\n\
     ──────────────────────────────────────────────────────────\n\n\
     ────────────── evaluated outside tests (1) ───────────────\n\
    \  These sites ran outside every test, at module initialization or in a \
     fixture release.\n\
    \  1  lib/util.ml   lines 1\n\
     ──────────────────────────────────────────────────────────\n\n\
     mutants: 1 reached, 1 killed, 1 never reached, 1 evaluated outside tests, \
     2 executables\n"
    out

let not_evaluated =
  test "a mutant one executable did not evaluate is no survivor" @@ fun () ->
  (* [sub] survives in A, and B's child passed without evaluating its site.
     B has not tested the mutant, so the merge calls no survivor: it lists
     the mutant with the command that arms it, and the build stays green. *)
  let root = scratch "not-evaluated" in
  plant_sources root;
  save
    (Filename.concat root "_build/_mutants/a.mutants")
    (collection [ m_add V.Killed; m_sub (V.survived [ [ "cli"; "runs" ] ]) ]);
  save
    (Filename.concat root "_build/_mutants/b.mutants")
    (collection [ m_add V.Killed; m_sub V.Not_evaluated ]);
  let code, out, err = mutate ~cwd:root [] in
  equal ~msg:"no survivor: exit 0" int 0 code;
  equal ~msg:"stderr" text "" err;
  equal ~msg:"the report, whole" text
    "─────────────────── not evaluated (1) ────────────────────\n\
    \  Each site ran in the dry run and not in its mutant's child.\n\
    \  lib/calc.ml:2:14:sub  a - b \u{2192} a + b\n\
    \    arm: WINDTRAP_MUTATE_ARM=lib/calc.ml:2:14:sub dune runtest --force \
     --instrument-with ppx_windtrap.mutate\n\
     ──────────────────────────────────────────────────────────\n\n\
     mutants: 2 reached, 1 killed, 1 not evaluated, 2 executables\n"
    out

(* Discovery *)

let discovery =
  test "discovery: walk-up, a cwd inside _build, and planted garbage"
  @@ fun () ->
  let proj = proj () in
  let code, out, _ = mutate ~cwd:(Filename.concat proj "lib") [] in
  equal ~msg:"walk-up discovery finds the survivor and exits 1" int 1 code;
  contains ~msg:"walk-up discovery finds the same data"
    ~sub:"mutants: 1 survived of 3 reached" out;
  contains ~msg:"walk-up discovery still resolves sources"
    ~sub:"let sub a b = a - b" out;
  (* A rule-action cwd (inside _build) resolves the root by the
     topmost-_build rule (the runtime's), never the ancestor scan. *)
  mkdir_p (Filename.concat proj "_build/default/lib");
  let code, out, _ =
    mutate ~cwd:(Filename.concat proj "_build/default/lib") []
  in
  equal ~msg:"a cwd inside _build finds the survivor too" int 1 code;
  contains ~msg:"a cwd inside _build resolves the workspace root"
    ~sub:"mutants: 1 survived of 3 reached" out;
  contains ~msg:"sources resolve from that root too" ~sub:"let sub a b = a - b"
    out;
  (* Garbage planted at _build/.sandbox/_build/_mutants must not capture
     discovery from a sandboxed action's cwd: the topmost _build wins. *)
  write_file
    (Filename.concat proj "_build/.sandbox/_build/_mutants/junk.mutants")
    "windtrap-mutants-v0\nleftover\n";
  mkdir_p (Filename.concat proj "_build/.sandbox/0abc/default");
  let code, out, err =
    mutate ~cwd:(Filename.concat proj "_build/.sandbox/0abc/default") []
  in
  equal ~msg:"a sandboxed cwd escapes planted garbage" int 1 code;
  contains ~msg:"a sandboxed cwd reports the workspace data"
    ~sub:"mutants: 1 survived of 3 reached" out;
  not_contains ~msg:"the planted file is never read" ~sub:"junk.mutants" err

let explicit_paths =
  test "explicit PATH arguments replace discovery, and are loud when invalid"
  @@ fun () ->
  let proj = proj () in
  let elsewhere = temp_dir () in
  (* A directory argument contributes what it holds. *)
  let code, out, _ =
    mutate ~cwd:elsewhere [ Filename.concat proj "_build/_mutants" ]
  in
  equal ~msg:"an explicit directory reports the survivor, exit 1" int 1 code;
  contains ~msg:"an explicit directory merges the same data"
    ~sub:"mutants: 1 survived of 3 reached" out;
  (* A nonexistent explicit path is an error naming the path and the
     reason, never a silent narrowing, which under killed-anywhere-wins
     would turn another executable's kill back into a survivor. *)
  let absent = scratch "no-such-dir/absent.mutants" in
  let code, _, err = mutate ~cwd:elsewhere [ absent ] in
  equal ~msg:"a missing explicit path exits 1" int 1 code;
  contains ~msg:"a missing explicit path is named" ~sub:absent err;
  contains ~msg:"a missing explicit path states the reason"
    ~sub:"no such file or directory" err;
  not_contains ~msg:"a missing explicit path never blames instrumentation"
    ~sub:"Instrument the library" err;
  (* An existing file without the .mutants suffix is equally loud,
     whatever its content. *)
  let renamed = scratch "renamed.verdicts" in
  save renamed file_a;
  let code, _, err = mutate ~cwd:elsewhere [ renamed ] in
  equal ~msg:"a wrong-suffix explicit file exits 1" int 1 code;
  contains ~msg:"a wrong-suffix explicit file is named" ~sub:renamed err;
  contains ~msg:"a wrong-suffix explicit file states the reason"
    ~sub:"not a .mutants file" err;
  (* An invalid path beside valid ones fails the whole invocation. *)
  let valid = Filename.concat proj "_build/_mutants/windtrap-a.mutants" in
  let code, out, err = mutate ~cwd:elsewhere [ valid; absent ] in
  equal ~msg:"one bad path fails the whole invocation" int 1 code;
  contains ~msg:"the bad path is the one named" ~sub:absent err;
  equal ~msg:"nothing is reported from the good one" text "" out;
  (* Directories keep the scan's tolerance: an empty one falls through to
     the no-data report. *)
  let empty_dir = scratch "explicit-empty" in
  mkdir_p empty_dir;
  let code, _, err = mutate ~cwd:elsewhere [ empty_dir ] in
  equal ~msg:"an empty explicit directory exits 1" int 1 code;
  contains ~msg:"an empty explicit directory is a no-data report"
    ~sub:"no .mutants files found" err;
  (* And they are searched to the bottom. A scan that stopped at the top
     level would narrow the merge without saying so, which under
     killed-anywhere-wins is exactly how a kill turns back into a
     survivor: here the file holding [add]'s kill is the deepest one. *)
  let nested = scratch "explicit-nested" in
  save (Filename.concat nested "one/two/a.mutants") file_a;
  save (Filename.concat nested "one/b.mutants") file_b;
  save (Filename.concat nested "c.mutants") file_c;
  let code, out, _ = mutate ~cwd:elsewhere [ nested ] in
  equal ~msg:"a nested explicit directory finds the survivor" int 1 code;
  equal ~msg:"and every depth reaches the merge" text
    "mutants: 1 survived of 3 reached, 2 killed, 2 never reached, 3 executables"
    (summary out)

(* The staleness pass *)

let plant_exe root exe contents =
  write_file (Filename.concat root (Filename.concat "_build" exe)) contents;
  { V.exe; digest = Digest.to_hex (Digest.string contents) }

let stale_root name =
  let root = scratch name in
  plant_sources root;
  let identity = plant_exe root "default/test/a.exe" "the instrumented build" in
  save ~identity (Filename.concat root "_build/_mutants/a.mutants") file_a;
  (root, identity)

let staleness =
  test "verdicts from a deleted or rebuilt executable are excluded, loudly"
  @@ fun () ->
  (* Fresh: the executable on disk is the file's writer. *)
  let root, _ = stale_root "stale-fresh" in
  let code, out, err = mutate ~cwd:root [] in
  equal ~msg:"a fresh identity-carrying file reports its survivor" int 1 code;
  equal ~msg:"a fresh identity-carrying file warns about nothing" text "" err;
  equal ~msg:"and is merged" text
    "mutants: 1 survived of 2 reached, 1 killed, 3 never reached, 1 executable"
    (summary out);
  contains ~msg:"its witness names the recorded executable, by basename"
    ~sub:"\n      a.exe  calc \u{203a} compares\n" out;
  contains
    ~msg:
      "an executable under a dune build directory is run again through dune: \
       its identity less the build context, the backend before it"
    ~sub:
      "\n\
       reproduce: dune exec --instrument-with ppx_windtrap.mutate test/a.exe \
       -- --arm lib/calc.ml:3:14:lt\n"
    out;
  (* Orphan: a second file whose executable no longer exists. Its data
     must not reach the report; under killed-anywhere-wins an excluded
     kill is the difference between a survivor and none, which is what
     the payload here is chosen to expose: [lt] is the live file's only
     survivor, and the orphan claims a crash killed it. *)
  let root, _ = stale_root "stale-orphan" in
  save
    ~identity:
      {
        V.exe = "default/test/gone.exe";
        digest = Digest.to_hex (Digest.string "gone");
      }
    (Filename.concat root "_build/_mutants/gone.mutants")
    (collection [ m_lt V.Killed ]);
  let code, out, err = mutate ~cwd:root [] in
  equal ~msg:"an orphan still reports the live data" int 1 code;
  equal ~msg:"the orphan's kill never reaches the report" text
    "mutants: 1 survived of 2 reached, 1 killed, 3 never reached, 1 executable"
    (summary out);
  contains ~msg:"the orphan warning names the file" ~sub:"gone.mutants" err;
  contains ~msg:"the orphan warning names the missing executable"
    ~sub:"default/test/gone.exe" err;
  contains ~msg:"the orphan warning says what it did" ~sub:"excluding it" err;
  (* One remedy sentence covers both exclusions: a re-run rewrites an
     outdated verdict, and deleting the directory drops an orphan no run
     can replace. *)
  contains ~msg:"the remedy names deletion for leftovers"
    ~sub:"; delete the files whose executable no longer exists\n" err;
  contains ~msg:"and the re-run, behind windtrap's one anchor"
    ~sub:("windtrap: " ^ rerun) err;
  not_contains ~msg:"the command's own prefix is gone" ~sub:"windtrap mutants:"
    err;
  (* Stale beside fresh: the report still renders, the outdated kill is
     excluded, and the same sentence names the re-run that rewrites a
     stale verdict. *)
  let root, _ = stale_root "stale-mixed" in
  let other = plant_exe root "default/test/b.exe" "the sibling build" in
  save ~identity:other
    (Filename.concat root "_build/_mutants/b.mutants")
    (collection [ m_lt V.Killed ]);
  write_file (Filename.concat root "_build/default/test/b.exe") "rebuilt since";
  let code, out, err = mutate ~cwd:root [] in
  equal ~msg:"a stale file beside a fresh one still reports" int 1 code;
  equal ~msg:"the stale file's kill never reaches the report" text
    "mutants: 1 survived of 2 reached, 1 killed, 3 never reached, 1 executable"
    (summary out);
  contains ~msg:"a stale file's warning says it was rebuilt"
    ~sub:"rebuilt since" err;
  contains ~msg:"and the remedy is the one sentence" ~sub:rerun err;
  equal ~msg:"the remedy is said once, however many files" int 1
    (List.length (lines_with ~sub:"then merge again" err));
  (* Stale everywhere: the executable was rebuilt since the run, detected
     by content and not by mtime. *)
  let root, identity = stale_root "stale-rebuilt" in
  write_file
    (Filename.concat root "_build/default/test/a.exe")
    "a different build";
  not_equal ~msg:"the fixture really changed the executable" string
    identity.V.digest
    (Digest.to_hex (Digest.string "a different build"));
  let code, _, err = mutate ~cwd:root [] in
  equal ~msg:"every file stale exits 1" int 1 code;
  contains ~msg:"the stale warning names the executable"
    ~sub:"default/test/a.exe" err;
  contains ~msg:"and says it did not write the file"
    ~sub:"not written by the executable now at" err;
  contains
    ~msg:
      "the all-stale message states the situation: the count, what the files \
       are and what invalidates a verdict"
    ~sub:
      "windtrap: found 1 .mutants file and every one is stale\n\
      \  A verdict is written only by a run asked to test its mutants, and it is\n\
      \  invalidated by any later build of the executable that wrote it.\n\
       windtrap: re-run every suite"
    err;
  contains ~msg:"and names the one remedy" ~sub:rerun err;
  not_contains ~msg:"and spells no dune command" ~sub:"dune " err;
  (* A rebuild excludes every verdict file of the project: three are
     named, the rest counted, and the summary splits the stale from the
     orphaned. *)
  let root, _ = stale_root "stale-many" in
  write_file
    (Filename.concat root "_build/default/test/a.exe")
    "a different build";
  List.iter
    (fun name ->
      let identity =
        plant_exe root ("default/test/" ^ name ^ ".exe") "the sibling build"
      in
      save ~identity
        (Filename.concat root ("_build/_mutants/" ^ name ^ ".mutants"))
        (collection [ m_lt V.Killed ]);
      write_file
        (Filename.concat root ("_build/default/test/" ^ name ^ ".exe"))
        "rebuilt since")
    [ "b"; "c"; "d" ];
  save
    ~identity:
      {
        V.exe = "default/test/gone.exe";
        digest = Digest.to_hex (Digest.string "gone");
      }
    (Filename.concat root "_build/_mutants/e.mutants")
    (collection [ m_lt V.Killed ]);
  let code, _, err = mutate ~cwd:root [] in
  equal ~msg:"five excluded files and nothing else exits 1" int 1 code;
  equal ~msg:"at most three files are named" int 3
    (List.length (lines_with ~sub:"; excluding it" err));
  not_contains ~msg:"the fourth is counted, not named" ~sub:"d.mutants" err;
  contains ~msg:"the rest are one line, then the summary with its split"
    ~sub:
      "excluding it\n\
       windtrap: ... and 2 more like that\n\
       windtrap: found 5 .mutants files and every one is stale or orphaned (1 \
       orphaned)\n"
    err;
  equal ~msg:"the remedy still prints once" int 1
    (List.length (lines_with ~sub:"then merge again" err))

(* The executable column *)

let executable_labels =
  test "witnesses name the executable that ran them" @@ fun () ->
  (* Three files, one survivor reached by all three and killed by none.
     A file's label is the basename of the identity it recorded; dune's
     inline-test runner, `inline-test-runner.exe` in every library's
     `.<lib>.inline-tests` directory, is named by that library, or
     every inline suite in a project would share one label; a file with
     no identity is named by its own basename. Rows sort by label, then
     test. *)
  let root = scratch "labels" in
  plant_sources root;
  let inline_exe =
    "default/lib/.my_lib_expect.inline-tests/inline-test-runner.exe"
  in
  let unit = plant_exe root "default/test/test_calc.exe" "the unit suite" in
  let inline = plant_exe root inline_exe "the inline runner" in
  let verdicts tests = collection [ m_add (V.survived tests) ] in
  save ~identity:unit
    (Filename.concat root "_build/_mutants/unit.mutants")
    (verdicts [ [ "calc"; "adds" ]; [ "calc"; "adds zero" ] ]);
  save ~identity:inline
    (Filename.concat root "_build/_mutants/inline.mutants")
    (verdicts [ [ "my_lib_expect"; "add" ] ]);
  save
    (Filename.concat root "_build/_mutants/plain.mutants")
    (verdicts [ [ "hand"; "written" ] ]);
  let code, out, err = mutate ~cwd:root [] in
  equal ~msg:"the survivor exits 1" int 1 code;
  equal ~msg:"nothing is stale" text "" err;
  contains ~msg:"the sentence counts tests and executables"
    ~sub:"4 tests in 3 executables ran this line and none failed:" out;
  (* The column is as wide as the widest label plus the gap. *)
  let labels = [ "my_lib_expect"; "plain.mutants"; "test_calc.exe" ] in
  let width =
    2 + List.fold_left (fun w l -> max w (String.length l)) 0 labels
  in
  let row exe test = Printf.sprintf "      %-*s%s\n" width exe test in
  contains ~msg:"the witness rows, labelled, in (executable, test) order"
    ~sub:
      (String.concat ""
         [
           row "my_lib_expect" "my_lib_expect \u{203a} add";
           row "plain.mutants" "hand \u{203a} written";
           row "test_calc.exe" "calc \u{203a} adds";
           row "test_calc.exe" "calc \u{203a} adds zero";
         ])
    out;
  equal ~msg:"the summary counts three executables" text
    "mutants: 1 survived of 1 reached, 3 executables" (summary out);
  (* The first row is an inline suite's. Dune's inline runner takes its
     arguments from dune alone, so the command is the build's. *)
  contains ~msg:"an inline runner is reached through the build"
    ~sub:
      "\n\
       reproduce: WINDTRAP_MUTATE_ARM=lib/calc.ml:1:14:add dune runtest \
       --force --instrument-with ppx_windtrap.mutate\n"
    out;
  (* An executable under no build directory records its absolute path,
     and is run as it is. *)
  let bare = scratch "built by hand" in
  plant_sources bare;
  let exe = Filename.concat bare "test_calc.exe" in
  write_file exe "built by hand";
  save
    ~identity:{ V.exe; digest = Digest.to_hex (Digest.string "built by hand") }
    (Filename.concat bare "_windtrap/mutants/calc.mutants")
    (verdicts [ [ "calc"; "adds" ] ]);
  let code, out, err = mutate ~cwd:bare [] in
  equal ~msg:"the hand-built project's survivor exits 1" int 1 code;
  equal ~msg:"its file is fresh" text "" err;
  contains
    ~msg:
      "an executable under no build directory is run as it is, its path quoted \
       where a shell would split it"
    ~sub:("\nreproduce: '" ^ exe ^ "' --arm lib/calc.ml:1:14:add\n")
    out;
  (* The same for the target dune is given. *)
  let spaced = scratch "spaced" in
  plant_sources spaced;
  let identity = plant_exe spaced "default/my tests/a.exe" "a suite" in
  save ~identity
    (Filename.concat spaced "_build/_mutants/a.mutants")
    (verdicts [ [ "calc"; "adds" ] ]);
  let _, out, _ = mutate ~cwd:spaced [] in
  contains ~msg:"a dune target with a space is one quoted word"
    ~sub:
      "\n\
       reproduce: dune exec --instrument-with ppx_windtrap.mutate 'my \
       tests/a.exe' -- --arm lib/calc.ml:1:14:add\n"
    out

let survivor_order =
  test "survivors are ordered by witness count, then identifier" @@ fun () ->
  (* The survivor the most tests watched is the one a reader can act on
     soonest, so it prints first however its identifier sorts. (A
     per-executable report prints its blocks as its children end, in the
     catalogue's order.) [sub] (line 2) has three witnesses across two
     files; [add] (line 1) has one. *)
  let root = scratch "order" in
  plant_sources root;
  save
    (Filename.concat root "_build/_mutants/one.mutants")
    (collection
       [
         m_add (V.survived [ [ "t"; "a" ] ]);
         m_sub (V.survived [ [ "t"; "b" ]; [ "t"; "c" ] ]);
       ]);
  save
    (Filename.concat root "_build/_mutants/two.mutants")
    (collection [ m_sub (V.survived [ [ "u"; "d" ] ]) ]);
  let code, out, _ = mutate ~cwd:root [] in
  equal ~msg:"two survivors exit 1" int 1 code;
  contains ~msg:"the most-watched survivor's sentence"
    ~sub:"3 tests in 2 executables ran this line and none failed:" out;
  in_order ~msg:"the most-watched survivor prints first"
    ~subs:[ "SURVIVED  lib/calc.ml:2:14:sub"; "SURVIVED  lib/calc.ml:1:14:add" ]
    out

(* Loud failures and usage *)

let loud_failures =
  test "failures are loud: no data, corrupt data, foreign formats, usage"
  @@ fun () ->
  let proj = proj () in
  let empty = scratch "empty-root" in
  mkdir_p empty;
  let code, _, err = mutate ~cwd:empty [] in
  equal ~msg:"no .mutants files exit 1" int 1 code;
  (* The hint names the backend and the flag a verdict needs, not a
     build tool. *)
  equal ~msg:"behind windtrap's one anchor, the hint on its own line" text
    "windtrap: no .mutants files found\n\
     Instrument the library under test with ppx_windtrap.mutate and run every \
     suite with its mutants (--mutate) first; every mutation run writes its \
     verdicts under the build directory's _mutants or under _windtrap/mutants.\n"
    err;
  (* An existing but empty _build/_mutants is "no files", loudly. *)
  let bare = scratch "bare" in
  mkdir_p (Filename.concat bare "_build/_mutants");
  let code, _, err = mutate ~cwd:bare [] in
  equal ~msg:"an empty _build/_mutants exits 1" int 1 code;
  contains ~msg:"an empty _build/_mutants prints the no-files hint"
    ~sub:"no .mutants files found" err;
  (* A truncated file is corrupt and named, never partially merged. *)
  let serialized =
    let path = scratch "whole.mutants" in
    save path file_a;
    read_file path
  in
  let trunc = scratch "trunc" in
  write_file
    (Filename.concat trunc "_build/_mutants/cut.mutants")
    (String.sub serialized 0 (String.length serialized - 6));
  let code, out, err = mutate ~cwd:trunc [] in
  equal ~msg:"a truncated file exits 1" int 1 code;
  contains ~msg:"a truncated file is named" ~sub:"cut.mutants" err;
  contains ~msg:"a truncated file is called corrupt" ~sub:"corrupt" err;
  equal ~msg:"a corrupt file reports nothing at all" text "" out;
  (* A foreign magic is rejected, not converted: no partial merge, and
     the remedy is deletion because a re-run cannot remove it. *)
  let foreign = scratch "foreign" in
  write_file
    (Filename.concat foreign "_build/_mutants/old.mutants")
    "windtrap-mutants-v0\n1\nsome older payload\n";
  let code, _, err = mutate ~cwd:foreign [] in
  equal ~msg:"a foreign-format file exits 1" int 1 code;
  contains ~msg:"a foreign-format file is named" ~sub:"old.mutants" err;
  contains ~msg:"a foreign-format file names the expected magic"
    ~sub:"windtrap-mutants-v3" err;
  contains ~msg:"a foreign format instructs deletion" ~sub:"delete" err;
  (* A coverage dump under _build/_mutants is the same rejection. *)
  let crossed = scratch "crossed" in
  write_file
    (Filename.concat crossed "_build/_mutants/cov.mutants")
    "WINDTRAP-COVERAGE-1\nsome v1 payload\n";
  let code, _, err = mutate ~cwd:crossed [] in
  equal ~msg:"a coverage dump in the mutants directory exits 1" int 1 code;
  contains ~msg:"and is named" ~sub:"cov.mutants" err;
  (* Usage errors are 2, and are the only 2 this command produces. *)
  let code, _, err = mutate ~cwd:proj [ "--frobnicate" ] in
  equal ~msg:"an unknown option exits 2" int 2 code;
  equal ~msg:"the anchored sentence, then the usage line and nothing else" text
    "windtrap: unknown option '--frobnicate'\n\
     usage: windtrap mutants [PATH...]\n"
    err;
  (* There is no threshold: one survivor is the failure. *)
  let code, _, err = mutate ~cwd:proj [ "--min"; "80" ] in
  equal ~msg:"there is no --min threshold to pass" int 2 code;
  contains ~msg:"and --min is simply unknown" ~sub:"unknown option '--min'" err;
  let code, out, _ = mutate ~cwd:proj [ "--help" ] in
  equal ~msg:"mutate --help exits 0" int 0 code;
  contains ~msg:"mutate --help says it drives nothing"
    ~sub:"Runs no tests and drives no build" out;
  contains ~msg:"mutate --help states the exit code"
    ~sub:"Exits 1 when any mutant survived every executable that reached it."
    out;
  contains
    ~msg:
      "mutate --help opens on the name line, then the usage line with the PATH \
       arguments"
    ~sub:
      "windtrap mutants - merge .mutants verdict files and report the \
       survivors\n\n\
       usage: windtrap mutants [PATH...]\n"
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
        ~msg:(Printf.sprintf "mutate --help fits 80 columns: %s" line)
        int ~than:80 (String.length line))
    (String.split_on_char '\n' out)

let dispatch =
  test "the binary dispatches mutants" @@ fun () ->
  let code, out, _ = capture [ "--help" ] in
  equal ~msg:"windtrap --help exits 0" int 0 code;
  contains ~msg:"windtrap --help lists the subcommand and what it does"
    ~sub:
      "  mutants\n\
      \      Merge .mutants verdict files and report the project's survivors.\n"
    out;
  contains ~msg:"windtrap --help still lists coverage"
    ~sub:
      "  coverage\n\
      \      Merge .coverage files and report; --min gates, --json exports.\n"
    out;
  let code, _, err = capture [ "mutant" ] in
  equal ~msg:"a near-miss command exits 2" int 2 code;
  contains ~msg:"a near-miss command is named" ~sub:"unknown command 'mutant'"
    err;
  (* The verb that promised a run is gone, not aliased: the command
     reports mutants and mutates nothing. *)
  let code, _, err = capture [ "mutate" ] in
  equal ~msg:"the old verb exits 2" int 2 code;
  contains ~msg:"the old verb is unknown" ~sub:"unknown command 'mutate'" err

(* The command at its edges *)

let edge_tests =
  [
    test "a file is judged from its header before it is loaded" (fun () ->
        (* A leftover whose header is intact and whose records are
           corrupt: two claimed records, none written. *)
        let root, identity = stale_root "judge-first" in
        let leftover ~exe ~digest =
          write_file
            (Filename.concat root "_build/_mutants/leftover.mutants")
            (Printf.sprintf "windtrap-mutants-v3\nexe %s %d %s\n2\n" digest
               (String.length exe) exe)
        in
        let live =
          "mutants: 1 survived of 2 reached, 1 killed, 3 never reached, 1 \
           executable"
        in
        (* Of a gone executable: excluded as an orphan, its records never
           read, and the live file's report stands. *)
        leftover ~exe:"default/test/gone.exe"
          ~digest:(Digest.to_hex (Digest.string "gone"));
        let code, out, err = mutate ~cwd:root [] in
        equal ~msg:"the live survivor exits 1" int 1 code;
        equal ~msg:"the live file is reported" text live (summary out);
        contains ~msg:"the corrupt orphan is excluded with its reason"
          ~sub:
            "leftover.mutants: its executable (default/test/gone.exe) no \
             longer exists; excluding it"
          err;
        not_contains ~msg:"its records are never read" ~sub:"corrupt" err;
        (* Of an earlier build of the executable on disk: excluded as
           stale. *)
        leftover ~exe:identity.V.exe
          ~digest:(Digest.to_hex (Digest.string "an earlier build"));
        let code, out, err = mutate ~cwd:root [] in
        equal ~msg:"the live survivor exits 1 again" int 1 code;
        equal ~msg:"the live file is reported again" text live (summary out);
        contains ~msg:"the corrupt stale file is excluded with its reason"
          ~sub:"leftover.mutants: not written by the executable now at" err;
        not_contains ~msg:"its records are never read either" ~sub:"corrupt" err;
        (* Of the executable on disk: a file of this build must load, since
           leaving it out could drop a kill, so its corruption ends the
           command. *)
        leftover ~exe:identity.V.exe ~digest:identity.V.digest;
        let code, out, err = mutate ~cwd:root [] in
        equal ~msg:"a corrupt file of this build exits 1" int 1 code;
        equal ~msg:"with no report" text "" out;
        contains ~msg:"the corrupt file is named"
          ~sub:"leftover.mutants: corrupt" err;
        not_contains ~msg:"and never excluded" ~sub:"excluding it" err);
    test "a survivor whose source is not found keeps its identifier and rewrite"
      (fun () ->
        let root = scratch "no-sources" in
        save
          (Filename.concat root "_build/_mutants/b.mutants")
          (collection [ m_sub (V.survived [ [ "t"; "b" ] ]) ]);
        let code, out, _ = mutate ~cwd:root [] in
        equal ~msg:"the survivor exits 1" int 1 code;
        contains ~msg:"the block's head, and no source line under it"
          ~sub:
            "  SURVIVED  lib/calc.ml:2:14:sub  a - b \u{2192} a + b\n\n\
            \    1 test ran this line and did not fail:\n\
            \      b.mutants  t \u{203a} b\n"
          out);
    test "the launcher's file is one in which the mutant survived" (fun () ->
        (* Two files bear one label, [t.exe]. The first in path order
           reached the mutant and did not let it survive, so the command is
           spelled from the second. *)
        let root = scratch "launcher" in
        plant_sources root;
        let first = plant_exe root "default/a/t.exe" "suite a"
        and second = plant_exe root "default/b/t.exe" "suite b" in
        save ~identity:first
          (Filename.concat root "_build/_mutants/1.mutants")
          (collection [ m_add V.Unreached ]);
        save ~identity:second
          (Filename.concat root "_build/_mutants/2.mutants")
          (collection [ m_add (V.survived [ [ "calc"; "adds" ] ]) ]);
        let code, out, err = mutate ~cwd:root [] in
        equal ~msg:"the survivor exits 1" int 1 code;
        equal ~msg:"both files are fresh" text "" err;
        contains ~msg:"the command runs the executable it survived"
          ~sub:
            "\n\
             reproduce: dune exec --instrument-with ppx_windtrap.mutate \
             b/t.exe -- --arm lib/calc.ml:1:14:add\n"
          out);
    test "one column of executables across every block" (fun () ->
        let root = scratch "exe-column" in
        plant_sources root;
        let short = plant_exe root "default/test/t.exe" "short"
        and long = plant_exe root "default/test/a_long_name.exe" "long" in
        save ~identity:short
          (Filename.concat root "_build/_mutants/short.mutants")
          (collection [ m_add (V.survived [ [ "calc"; "adds" ] ]) ]);
        save ~identity:long
          (Filename.concat root "_build/_mutants/long.mutants")
          (collection [ m_sub (V.survived [ [ "calc"; "subtracts" ] ]) ]);
        let _, out, _ = mutate ~cwd:root [] in
        (* [t.exe]'s block holds no longer label, and is padded to the
           other block's. *)
        let width = 2 + String.length "a_long_name.exe" in
        contains ~msg:"the short label's row, padded to the long one"
          ~sub:
            (Printf.sprintf "\n      %-*s%s\n" width "t.exe"
               "calc \u{203a} adds")
          out;
        contains ~msg:"the long label's row"
          ~sub:
            (Printf.sprintf "\n      %-*s%s\n" width "a_long_name.exe"
               "calc \u{203a} subtracts")
          out);
    test "-h and -help print the help page" (fun () ->
        let _, help, _ = mutate [ "--help" ] in
        List.iter
          (fun flag ->
            let code, out, err = mutate [ flag ] in
            equal ~msg:(flag ^ " exits 0") int 0 code;
            equal ~msg:(flag ^ " is --help") text help out;
            equal ~msg:(flag ^ " says nothing else") text "" err)
          [ "-h"; "-help" ]);
    test "a refused WINDTRAP_COLOR is a usage error" (fun () ->
        let code, out, err = mutate ~cwd:(proj ()) ~color:"sometimes" [] in
        equal ~msg:"exit code" int 2 code;
        equal ~msg:"no report" text "" out;
        equal ~msg:"the runner's sentence" text
          "windtrap: invalid value 'sometimes' for WINDTRAP_COLOR: expected \
           always, never or auto\n"
          err;
        let code, _, err = mutate ~cwd:(proj ()) [ "--color"; "never" ] in
        equal ~msg:"there is no --color flag" int 2 code;
        contains ~msg:"it is an unknown option" ~sub:"unknown option '--color'"
          err;
        let absent = scratch "absent.mutants" in
        let code, _, err = mutate ~color:"sometimes" [ absent ] in
        equal ~msg:"the colour is refused before a PATH" int 2 code;
        equal ~msg:"and the PATH is never named" text
          "windtrap: invalid value 'sometimes' for WINDTRAP_COLOR: expected \
           always, never or auto\n"
          err);
    test "the first argument that starts with a dash ends the parse" (fun () ->
        let _, help, _ = mutate [ "--help" ] in
        let code, out, err = mutate [ scratch "absent.mutants"; "-h" ] in
        equal ~msg:"a PATH before -h" int 0 code;
        equal ~msg:"is not looked at" text help out;
        equal ~msg:"and nothing is said" text "" err;
        let code, out, err = mutate [ "-x"; "--help" ] in
        equal ~msg:"an unknown option before --help" int 2 code;
        equal ~msg:"prints no help" text "" out;
        equal ~msg:"and is the one refused" text
          "windtrap: unknown option '-x'\nusage: windtrap mutants [PATH...]\n"
          err;
        let code, _, err = mutate [ "-" ] in
        equal ~msg:"a lone dash is an option" int 2 code;
        contains ~msg:"and an unknown one" ~sub:"unknown option '-'\n" err);
    test "a target without a directory is spelled from the current one"
      (fun () ->
        (* [dune exec t.exe] looks for a program named [t.exe]. An
           executable at the project root, and an identity of one component,
           take [./]. *)
        List.iter
          (fun (name, exe) ->
            let root = scratch name in
            plant_sources root;
            let identity = plant_exe root exe "a suite" in
            save ~identity
              (Filename.concat root "_build/_mutants/t.mutants")
              (collection [ m_add (V.survived [ [ "calc"; "adds" ] ]) ]);
            let code, out, err = mutate ~cwd:root [] in
            equal ~msg:(exe ^ ": the survivor exits 1") int 1 code;
            equal ~msg:(exe ^ ": the file is fresh") text "" err;
            contains
              ~msg:(exe ^ ": the target names its directory")
              ~sub:
                "\n\
                 reproduce: dune exec --instrument-with ppx_windtrap.mutate \
                 ./t.exe -- --arm lib/calc.ml:1:14:add\n"
              out)
          [ ("root-exe", "default/t.exe"); ("one-component", "t.exe") ]);
    test "reaching tests sort by their spelled path" (fun () ->
        (* By the path as a list, [a; b] comes before [a b]; spelled, the
           space sorts before the separator. *)
        let root = scratch "spelled" in
        plant_sources root;
        save
          (Filename.concat root "_build/_mutants/s.mutants")
          (collection [ m_add (V.survived [ [ "a"; "b" ]; [ "a b" ] ]) ]);
        let _, out, _ = mutate ~cwd:root [] in
        contains ~msg:"the rows in the order of their spelling"
          ~sub:"\n      s.mutants  a b\n      s.mutants  a \u{203a} b\n" out);
  ]

(* The suite *)

let () =
  exit
  @@ run "mutants_cmd"
       [
         two_executables;
         merge_report;
         merge_is_total;
         single_file;
         clean_report;
         only_unreached;
         not_evaluated;
         outside_tests;
         discovery;
         explicit_paths;
         staleness;
         executable_labels;
         survivor_order;
         loud_failures;
         dispatch;
         group "edges" edge_tests;
       ]
