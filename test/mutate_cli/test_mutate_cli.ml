(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests for `windtrap mutate`: the merge that makes a project-level
   mutation report true, and the surface around it.

   The subject is bin/main.exe, spawned as a subprocess. It merges two
   kinds of verdict file. Most are synthetic, written with the runtime's
   own serializer: the command runs no tests and drives no build, so
   discovery, the explicit-PATH contract, the staleness pass, the file
   format's rejections and every exit code need neither the instrumenter
   nor a mutation run, and stating the data by hand is what makes the
   counts exact. The first test is not synthetic — two instrumented
   executables over one library, each running its own mutation loop and
   writing its own verdict file — because the claim the command is built
   on (two executables that disagree merge to something truer than
   either) is a claim about real runs, and a fixture that assumed it
   could not test it.

   What is checked: killed-anywhere-wins across three files (including
   the case the whole verdict file exists for, one suite killing what
   another merely reaches), the union of a survivor's witnesses, each
   tagged with the executable that ran it, a survivor that survives
   everywhere, the unreached blocks, the colours as raw bytes, discovery
   under _build/_mutants and through explicit PATH arguments, the
   staleness pass, every exit code — 1 when a mutant survived the merge,
   0 when none did, unreached mutants alone staying green — and the
   Law-12 coupling budget over lib/.

   A windtrap suite ([run] executes tests sequentially in declaration
   order); every subject under test is a spawned process, so hosting the
   assertions under the windtrap runner nests nothing. *)

open Windtrap
module M = Windtrap_mutate
module V = Windtrap.Private.Mutate_verdicts

let check name cond = is_true ~msg:name cond
let check_int name ~expected ~actual = equal ~msg:name int expected actual

let check_contains name ~needle haystack =
  contains ~msg:name ~sub:needle haystack

let check_absent name ~needle haystack =
  not_contains ~msg:name ~sub:needle haystack

(* Boolean containment, for predicates rather than assertions (the Law-12
   budget's line filter). *)
let contains_sub ~sub s =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  m = 0 || go 0

(* The offset of [sub]'s first occurrence in [s], for ordering assertions:
   a block that must print before another. Fails the test when absent,
   so an ordering check never passes vacuously on a missing block. *)
let index_of ~sub s =
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i + m > n then failf "%S is not in the report" sub
    else if String.sub s i m = sub then i
    else go (i + 1)
  in
  go 0

let check_before name ~first ~second out =
  check name (index_of ~sub:first out < index_of ~sub:second out)

(* Scratch and process helpers *)

(* Hermeticity: absolute paths throughout, so the test behaves the same
   under dune's sandbox and by hand; scratch lives in a private temp
   directory removed at exit. Nothing is ever written under the real
   _build/_mutants. *)
let exe_dir = Filename.dirname Sys.executable_name

let windtrap_exe =
  Filename.concat exe_dir
    (Filename.concat ".." (Filename.concat ".." "bin/main.exe"))

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
  let dir = Filename.temp_file "windtrap_mut_cli" "" in
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

let run_counter = ref 0

(* The environment is stated in full rather than extended: this suite
   asserts on transcripts byte for byte, and a WINDTRAP_VERBOSE in a
   developer's shell would reshape them. Nothing is inherited but what a
   process needs to start. *)
let inherited =
  List.concat_map
    (fun name ->
      match Sys.getenv_opt name with
      | Some value -> [ name ^ "=" ^ value ]
      | None -> [])
    [ "PATH"; "HOME"; "TMPDIR"; "LANG"; "LC_ALL" ]

(* [capture ?cwd ?env ?exe args] runs [exe] (the windtrap binary by
   default) and returns (exit code, stdout, stderr). Color is off so the
   report is comparable bytes, except where a test asks for [~color:"always"]
   to read the escape codes themselves. *)
let capture ?cwd ?(color = "never") ?(env = []) ?(exe = windtrap_exe) args =
  incr run_counter;
  let out = scratch (Printf.sprintf "out-%d.txt" !run_counter)
  and err = scratch (Printf.sprintf "err-%d.txt" !run_counter) in
  let command =
    String.concat " "
      (List.map Filename.quote
         ((("env" :: "-i" :: inherited) @ (("WINDTRAP_COLOR=" ^ color) :: env))
         @ (exe :: args)))
    ^ " > " ^ Filename.quote out ^ " 2> " ^ Filename.quote err
  in
  let command =
    match cwd with
    | None -> command
    | Some dir -> "cd " ^ Filename.quote dir ^ " && " ^ command
  in
  let code = Sys.command command in
  (code, read_file out, read_file err)

let mutate ?cwd ?color args = capture ?cwd ?color ("mutate" :: args)

(* The one command every remedy names. *)
let rerun =
  "WINDTRAP_MUTATE=1 dune build @mutate --force --instrument-with \
   ppx_windtrap.mutate"

(* The fixture: one library, three test executables' verdicts *)

let calc = "lib/calc.ml"
let util = "lib/util.ml"

(* The four mutants, each spelled once and applied to a verdict: three in
   calc.ml, one in util.ml. Every field is what an instrumented build
   would have recorded, renderings included — the report is drawn from
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
           match String.rindex_opt line '\xb7' with
           | Some i -> String.sub line 0 (i - 2)
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

let proj =
  let root = scratch "proj" in
  plant_sources root;
  List.iter
    (fun (name, t) ->
      write_file
        (Filename.concat root (Filename.concat "_build/_mutants" name))
        (V.to_string t))
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
   part runs the real thing — two executables over Mutcli_fixture.Calc,
   each driving its own mutation loop and writing its own verdict file,
   and [windtrap mutate] over what they wrote. [pins_add] pins [add] and
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

let real_project =
  lazy
    (let root = scratch "two-executables" in
     (* The instrumenter records workspace-relative paths, so the source
        the survivor excerpt resolves against is planted where it was
        recorded. *)
     write_file (Filename.concat root "test/mutate_cli/calc.ml") fixture_source;
     List.iter
       (fun name ->
         let target =
           Filename.concat root (Filename.concat "_build/default/test" name)
         in
         write_file target (read_file (Filename.concat exe_dir name));
         Unix.chmod target 0o755)
       [ "pins_add.exe"; "pins_sub.exe" ];
     root)

let two_executables =
  test "two executables that disagree merge to the project's truth" @@ fun () ->
  let root = Lazy.force real_project in
  let at binding =
    Printf.sprintf "test/mutate_cli/calc.ml:%d:" (calc_line binding)
  in
  let add = at "let add" and sub = at "let sub" and shared = at "let shared" in
  let loop name =
    (* The scope keeps this scenario's catalogue the fixture's. Under
       --instrument-with these executables link a mutation-instrumented
       windtrap core, and the claim under test — two executables that
       disagree about ONE library merge to the truth — is about calc.ml's
       mutants, not about the core's thousand. *)
    capture ~cwd:root
      ~env:
        [ "WINDTRAP_MUTATE=1"; "WINDTRAP_MUTATE_ONLY=test/mutate_cli/calc.ml" ]
      ~exe:(Filename.concat root (Filename.concat "_build/default/test" name))
      []
  in
  (* Each executable is right about what it ran and wrong about the
     project. *)
  let code, out, err = loop "pins_add.exe" in
  check_int "the first executable's loop exits 0" ~expected:0 ~actual:code;
  check "the first executable's loop keeps stderr empty" (err = "");
  check_contains "it calls the mutant its sibling kills a survivor"
    ~needle:("SURVIVED  " ^ sub) out;
  check_absent "and kills the one it pins itself" ~needle:("SURVIVED  " ^ add)
    out;
  let code, out, err = loop "pins_sub.exe" in
  check_int "the second executable's loop exits 0" ~expected:0 ~actual:code;
  check "the second executable's loop keeps stderr empty" (err = "");
  check_contains "it calls the other's kill a survivor"
    ~needle:("SURVIVED  " ^ add) out;
  check_absent "and kills the one it pins itself" ~needle:("SURVIVED  " ^ sub)
    out;
  (* And neither false survivor survives the merge. This is the whole
     claim: reporting either executable's view alone sends the reader to
     write a test that already exists. *)
  let code, out, err = mutate ~cwd:root [] in
  check_int "the merged report exits 1: a mutant survived everywhere"
    ~expected:1 ~actual:code;
  check "the merged report keeps stderr empty" (err = "");
  check_absent "a mutant killed by one executable is not a survivor"
    ~needle:("SURVIVED  " ^ add) out;
  check_absent "nor is the one killed by the other" ~needle:("SURVIVED  " ^ sub)
    out;
  check_contains "the survivor is the one neither executable pinned"
    ~needle:("SURVIVED  " ^ shared) out;
  check_contains "its witnesses are both executables' tests"
    ~needle:"2 tests in 2 executables ran this line and none failed:" out;
  (* Each witness names the executable that ran it, by the basename of
     the identity its verdict file recorded. *)
  check_contains "the first executable's witness, in its column"
    ~needle:"\n      pins_add.exe   shared \u{203a} shared is nonzero\n" out;
  check_contains "the second executable's witness, in its column"
    ~needle:"\n      pins_sub.exe   shared \u{203a} shared is not 99\n" out;
  check_contains "the excerpt is drawn from the planted source"
    ~needle:"let shared a b = a + b" out;
  check_contains "the mutant neither executable reached is still a finding"
    ~needle:
      (Printf.sprintf "UNREACHED  test/mutate_cli/calc.ml:%d:"
         (calc_line "let never"))
    out;
  equal ~msg:"the project's summary, whole" text
    "mutants: 1 survived of 3 reached \u{00b7} 2 killed \u{00b7} 1 never \
     reached \u{00b7} 2 executables"
    (summary out)

(* The Law-12 budget (grep-based) *)

(* Law 12: core windtrap's coupling to the mutation subsystem is one
   dispatch call at run entry plus the survivor projection the shared
   renderer needs. lib/mutate is the runtime, lib/mutate_loop is the one
   core module the law allows to drive it, and lib/mutate_verdicts is
   the subsystem's verdict side (tool currency, moved out of the
   runtime), so all three are excluded from the count; what is counted
   is the lines of the remaining lib/*.ml{,i} that name any of them.
   Growth past the cap is a law violation, not a test to update. *)
(* An instrumented build leaves dune's ppx output beside each source as
   <module>.pp.ml, and those files are nothing but generated calls into
   the runtime. The law is about the coupling a maintainer WRITES, so
   counting them would make the budget a function of whether the tree
   happened to be built with --instrument-with. *)
let is_preprocessed name =
  Filename.check_suffix (Filename.remove_extension name) ".pp"

let law12_budget =
  test "the Law-12 mutation budget stays under the cap" @@ fun () ->
  let lib_dir =
    Filename.concat exe_dir (Filename.concat ".." (Filename.concat ".." "lib"))
  in
  let sources =
    Sys.readdir lib_dir |> Array.to_list
    |> List.filter (fun name ->
        (Filename.check_suffix name ".ml" || Filename.check_suffix name ".mli")
        && (not (is_preprocessed name))
        && (not (String.starts_with ~prefix:"mutate_loop." name))
        && not (String.starts_with ~prefix:"mutate_verdicts." name))
    |> List.sort String.compare
  in
  check "lib sources are visible to the budget check" (sources <> []);
  check "the driver module is excluded, not missing"
    (Sys.file_exists (Filename.concat lib_dir "mutate_loop.ml"));
  check "the verdict module is excluded, not missing"
    (Sys.file_exists (Filename.concat lib_dir "mutate_verdicts.ml"));
  let mentions =
    List.fold_left
      (fun acc name ->
        let lines =
          String.split_on_char '\n' (read_file (Filename.concat lib_dir name))
        in
        acc
        + List.length
            (List.filter
               (fun line ->
                 contains_sub ~sub:"Windtrap_mutate" line
                 || contains_sub ~sub:"Mutate_loop" line
                 || contains_sub ~sub:"Mutate_verdicts" line)
               lines))
      0 sources
  in
  check
    (Printf.sprintf "Law-12 budget: %d core lines mention mutation (<= 20)"
       mentions)
    (mentions > 0 && mentions <= 20)

(* The merge *)

let merge_report =
  test "killed anywhere wins across three executables" @@ fun () ->
  let code, out, err = mutate ~cwd:proj [] in
  check_int "the merged report exits 1: a mutant survived everywhere"
    ~expected:1 ~actual:code;
  check "the merged report keeps stderr empty" (err = "");
  (* The load-bearing case: A killed [add], B only reached it. A report
     that listed it would send the reader to write a test that exists. *)
  check_absent "a mutant killed by one suite is not a survivor"
    ~needle:"lib/calc.ml:1:14:add" out;
  (* A crash in one executable outranks survival in another. *)
  check_absent "a crash in one suite kills for the project"
    ~needle:"lib/calc.ml:3:14:lt" out;
  check_contains "the survivor is the one no suite killed"
    ~needle:"SURVIVED  lib/calc.ml:2:14:sub   a - b  \u{2192}  a + b" out;
  check_int "exactly one survivor block" ~expected:1
    ~actual:
      (List.length
         (List.filter
            (fun l -> String.length l > 2 && String.sub l 2 8 = "SURVIVED")
            (String.split_on_char '\n' out)));
  check_contains "the section counts it" ~needle:"survivors (1)" out;
  (* The renderings come from the file: the catalogue lives in binaries
     this command never links, so a report drawn without them would be
     strictly worse than the per-executable one. *)
  check_contains "the excerpt is drawn from the planted source"
    ~needle:"2 \u{2502} let sub a b = a - b" out;
  check_contains
    "the reproduce footer mirrors onto the forced, instrumented run"
    ~needle:
      "\n\
       reproduce: WINDTRAP_MUTATE_ARM=<id> dune runtest --force \
       --instrument-with ppx_windtrap.mutate\n"
    out;
  (* Witnesses union across the two executables that reached it, each
     naming its executable. These files record no identity, so the label
     is the file's own name; the staleness tests pin the identity case. *)
  check_contains "the sentence counts both witnesses and both executables"
    ~needle:"2 tests in 2 executables ran this line and none failed:" out;
  check_contains "the first executable's witness, in its column"
    ~needle:"\n      windtrap-b.mutants   cli \u{203a} subtracts\n" out;
  check_contains "the second executable's witness, in its column"
    ~needle:"\n      windtrap-c.mutants   prop \u{203a} sub law\n" out;
  (* Unreached is a finding with its own remedy, and prints by default:
     one block per mutant, the rewrite in the head row, the excerpt under
     it, in identifier order. *)
  check_contains "the never-reached section counts mutants, not files or lines"
    ~needle:"never reached (2)" out;
  check_contains "the first never-reached block: head row and excerpt"
    ~needle:
      "  UNREACHED  lib/util.ml:1:13:or    p || q  \u{2192}  p && q\n\
      \      1 \u{2502} let ok p q = p || q\n"
    out;
  check_contains "the second never-reached block: head row and excerpt"
    ~needle:
      "  UNREACHED  lib/util.ml:3:22:and   p && q  \u{2192}  p || q\n\
      \      3 \u{2502} let both p q = p && q\n"
    out;
  check_before "unreached blocks are in identifier order"
    ~first:"UNREACHED  lib/util.ml:1:13:or"
    ~second:"UNREACHED  lib/util.ml:3:22:and" out;
  (* The summary is the project's, not one executable's: the reached
     count, and how many executables it is over. *)
  equal ~msg:"the summary line, whole" text
    "mutants: 1 survived of 3 reached \u{00b7} 2 killed \u{00b7} 2 never \
     reached \u{00b7} 3 executables"
    (summary out);
  (* The same report with colour on, read as the bytes it is: SURVIVED
     red, UNREACHED yellow, the counts in the suite summary's palette, the
     reproduce footer plain. *)
  let code, coloured, _ = mutate ~color:"always" ~cwd:proj [] in
  check_int "colour changes no exit code" ~expected:1 ~actual:code;
  check_contains "SURVIVED is red, the identifier bold"
    ~needle:
      "  \027[31mSURVIVED\027[0m  \027[1mlib/calc.ml:2:14:sub\027[0m   a - b  \
       \u{2192}  a + b\n"
    coloured;
  check_contains "UNREACHED is yellow, the identifier bold"
    ~needle:
      "  \027[33mUNREACHED\027[0m  \027[1mlib/util.ml:1:13:or\027[0m    p || \
       q  \u{2192}  p && q\n"
    coloured;
  check_contains "the counts are coloured as the suite summary's are"
    ~needle:
      "\n\
       mutants: \027[31m1 survived\027[0m of 3 reached \u{00b7} \027[32m2 \
       killed\027[0m \u{00b7} \027[33m2 never reached\027[0m \u{00b7} 3 \
       executables\n"
    coloured;
  check_contains "the reproduce footer is never coloured"
    ~needle:
      "\n\
       reproduce: WINDTRAP_MUTATE_ARM=<id> dune runtest --force \
       --instrument-with ppx_windtrap.mutate\n"
    coloured

(* A witness row or the sentence over it: the one part of a report that
   legitimately differs between a merge of three files and the same data
   written as one, because it names the file each witness came from. *)
let is_witness_line line =
  contains_sub ~sub:"\u{203a}" line || contains_sub ~sub:"ran this line" line

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
     file cannot say which executable ran which — that attribution is
     the command's own, and the merge test above pins it. *)
  let reference = scratch "reference" in
  plant_sources reference;
  write_file
    (Filename.concat reference "_build/_mutants/merged.mutants")
    (V.to_string (V.merge (V.merge file_a file_b) file_c));
  let _, expected, _ = mutate ~cwd:reference [] in
  check "the reference report is not empty" (expected <> "");
  let expected = without_executables_term expected in
  let _, out, _ = mutate ~cwd:proj [] in
  equal ~msg:"three files merge to the runtime's verdicts" text
    (without_witnesses expected)
    (without_witnesses (without_executables_term out));
  List.iter
    (fun witness ->
      check_contains "the reference names the witness" ~needle:witness expected;
      check_contains "and so does the merge" ~needle:witness out)
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
     a survivor. Reading B's file alone must say so — the command reports
     what it was given — which is exactly why narrowing the merge by
     accident has to be loud. *)
  let code, out, _ =
    mutate ~cwd:proj
      [ Filename.concat proj "_build/_mutants/windtrap-b.mutants" ]
  in
  check_int "a single file with survivors exits 1" ~expected:1 ~actual:code;
  check_contains "B alone reports the false survivor"
    ~needle:"SURVIVED  lib/calc.ml:1:14:add" out;
  check_contains "B alone counts two survivors" ~needle:"survivors (2)" out;
  check_before "equal witness counts leave survivors in identifier order"
    ~first:"SURVIVED  lib/calc.ml:1:14:add"
    ~second:"SURVIVED  lib/calc.ml:2:14:sub" out;
  check_contains "one executable's witnesses still name it"
    ~needle:"\n      windtrap-b.mutants   cli \u{203a} runs\n" out;
  equal ~msg:"B alone scores itself" text
    "mutants: 2 survived of 3 reached \u{00b7} 1 killed \u{00b7} 2 never \
     reached \u{00b7} 1 executable"
    (summary out)

let clean_report =
  test "a project with nothing to report is one line" @@ fun () ->
  let root = scratch "clean" in
  plant_sources root;
  write_file
    (Filename.concat root "_build/_mutants/all.mutants")
    (V.to_string (collection [ m_add V.Killed; m_sub V.Killed; m_lt V.Killed ]));
  let code, out, err = mutate ~cwd:root [] in
  check_int "a clean project exits 0" ~expected:0 ~actual:code;
  check "a clean project keeps stderr empty" (err = "");
  equal ~msg:"and prints exactly the summary" text
    "mutants: 3 reached \u{00b7} 3 killed \u{00b7} 1 executable\n" out

let only_unreached =
  test "unreached mutants alone are a finding, not a failure" @@ fun () ->
  (* A mutant no executable reaches has no test to strengthen; the remedy
     is a new test, which is coverage's kind of finding. It is listed and
     it is not scored, so the merge stays green. *)
  let root = scratch "unreached-only" in
  plant_sources root;
  write_file
    (Filename.concat root "_build/_mutants/all.mutants")
    (V.to_string
       (collection [ m_add V.Killed; m_sub V.Killed; m_or V.Unreached ]));
  let code, out, err = mutate ~cwd:root [] in
  check_int "only unreached mutants exit 0" ~expected:0 ~actual:code;
  check "and warn about nothing" (err = "");
  check_absent "there is no survivor section" ~needle:"survivors (" out;
  check_contains "the unreached block still prints"
    ~needle:"  UNREACHED  lib/util.ml:1:13:or   p || q  \u{2192}  p && q\n" out;
  equal ~msg:"the summary counts it without a survived term" text
    "mutants: 2 reached \u{00b7} 2 killed \u{00b7} 1 never reached \u{00b7} 1 \
     executable"
    (summary out)

(* Discovery *)

let discovery =
  test "discovery: walk-up, a cwd inside _build, and planted garbage"
  @@ fun () ->
  let code, out, _ = mutate ~cwd:(Filename.concat proj "lib") [] in
  check_int "walk-up discovery finds the survivor and exits 1" ~expected:1
    ~actual:code;
  check_contains "walk-up discovery finds the same data"
    ~needle:"mutants: 1 survived of 3 reached" out;
  check_contains "walk-up discovery still resolves sources"
    ~needle:"let sub a b = a - b" out;
  (* A rule-action cwd — inside _build — resolves the root by the
     topmost-_build rule (the runtime's), never the ancestor scan. *)
  mkdir_p (Filename.concat proj "_build/default/lib");
  let code, out, _ =
    mutate ~cwd:(Filename.concat proj "_build/default/lib") []
  in
  check_int "a cwd inside _build finds the survivor too" ~expected:1
    ~actual:code;
  check_contains "a cwd inside _build resolves the workspace root"
    ~needle:"mutants: 1 survived of 3 reached" out;
  check_contains "sources resolve from that root too"
    ~needle:"let sub a b = a - b" out;
  (* Garbage planted at _build/.sandbox/_build/_mutants must not capture
     discovery from a sandboxed action's cwd: the topmost _build wins. *)
  write_file
    (Filename.concat proj "_build/.sandbox/_build/_mutants/junk.mutants")
    "windtrap-mutants-v0\nleftover\n";
  mkdir_p (Filename.concat proj "_build/.sandbox/0abc/default");
  let code, out, err =
    mutate ~cwd:(Filename.concat proj "_build/.sandbox/0abc/default") []
  in
  check_int "a sandboxed cwd escapes planted garbage" ~expected:1 ~actual:code;
  check_contains "a sandboxed cwd reports the workspace data"
    ~needle:"mutants: 1 survived of 3 reached" out;
  check_absent "the planted file is never read" ~needle:"junk.mutants" err

let explicit_paths =
  test "explicit PATH arguments replace discovery, and are loud when invalid"
  @@ fun () ->
  (* A directory argument contributes what it holds. *)
  let code, out, _ =
    mutate ~cwd:scratch_dir [ Filename.concat proj "_build/_mutants" ]
  in
  check_int "an explicit directory reports the survivor, exit 1" ~expected:1
    ~actual:code;
  check_contains "an explicit directory merges the same data"
    ~needle:"mutants: 1 survived of 3 reached" out;
  (* A nonexistent explicit path is an error naming the path and the
     reason — never a silent narrowing, which under killed-anywhere-wins
     would turn another executable's kill back into a survivor. *)
  let absent = scratch "no-such-dir/absent.mutants" in
  let code, _, err = mutate ~cwd:scratch_dir [ absent ] in
  check_int "a missing explicit path exits 1" ~expected:1 ~actual:code;
  check_contains "a missing explicit path is named" ~needle:absent err;
  check_contains "a missing explicit path states the reason"
    ~needle:"no such file or directory" err;
  check_absent "a missing explicit path never blames instrumentation"
    ~needle:"Instrument the library" err;
  (* An existing file without the .mutants suffix is equally loud,
     whatever its content. *)
  let renamed = scratch "renamed.verdicts" in
  write_file renamed (V.to_string file_a);
  let code, _, err = mutate ~cwd:scratch_dir [ renamed ] in
  check_int "a wrong-suffix explicit file exits 1" ~expected:1 ~actual:code;
  check_contains "a wrong-suffix explicit file is named" ~needle:renamed err;
  check_contains "a wrong-suffix explicit file states the reason"
    ~needle:"not a .mutants file" err;
  (* An invalid path beside valid ones fails the whole invocation. *)
  let valid = Filename.concat proj "_build/_mutants/windtrap-a.mutants" in
  let code, out, err = mutate ~cwd:scratch_dir [ valid; absent ] in
  check_int "one bad path fails the whole invocation" ~expected:1 ~actual:code;
  check_contains "the bad path is the one named" ~needle:absent err;
  check "nothing is reported from the good one" (out = "");
  (* Directories keep the scan's tolerance: an empty one falls through to
     the no-data report. *)
  let empty_dir = scratch "explicit-empty" in
  mkdir_p empty_dir;
  let code, _, err = mutate ~cwd:scratch_dir [ empty_dir ] in
  check_int "an empty explicit directory exits 1" ~expected:1 ~actual:code;
  check_contains "an empty explicit directory is a no-data report"
    ~needle:"no .mutants files found" err;
  (* And they are searched to the bottom. A scan that stopped at the top
     level would narrow the merge without saying so, which under
     killed-anywhere-wins is exactly how a kill turns back into a
     survivor: here the file holding [add]'s kill is the deepest one. *)
  let nested = scratch "explicit-nested" in
  write_file (Filename.concat nested "one/two/a.mutants") (V.to_string file_a);
  write_file (Filename.concat nested "one/b.mutants") (V.to_string file_b);
  write_file (Filename.concat nested "c.mutants") (V.to_string file_c);
  let code, out, _ = mutate ~cwd:scratch_dir [ nested ] in
  check_int "a nested explicit directory finds the survivor" ~expected:1
    ~actual:code;
  equal ~msg:"and every depth reaches the merge" text
    "mutants: 1 survived of 3 reached \u{00b7} 2 killed \u{00b7} 2 never \
     reached \u{00b7} 3 executables"
    (summary out)

(* The staleness pass *)

let plant_exe root exe contents =
  write_file (Filename.concat root (Filename.concat "_build" exe)) contents;
  { V.exe; digest = Digest.to_hex (Digest.string contents) }

let stale_root name =
  let root = scratch name in
  plant_sources root;
  let identity = plant_exe root "default/test/a.exe" "the instrumented build" in
  write_file
    (Filename.concat root "_build/_mutants/a.mutants")
    (V.to_string ~identity file_a);
  (root, identity)

let staleness =
  test "verdicts from a deleted or rebuilt executable are excluded, loudly"
  @@ fun () ->
  (* Fresh: the executable on disk is the file's writer. *)
  let root, _ = stale_root "stale-fresh" in
  let code, out, err = mutate ~cwd:root [] in
  check_int "a fresh identity-carrying file reports its survivor" ~expected:1
    ~actual:code;
  check "a fresh identity-carrying file warns about nothing" (err = "");
  equal ~msg:"and is merged" text
    "mutants: 1 survived of 2 reached \u{00b7} 1 killed \u{00b7} 3 never \
     reached \u{00b7} 1 executable"
    (summary out);
  check_contains "its witness names the recorded executable, by basename"
    ~needle:"\n      a.exe   calc \u{203a} compares\n" out;
  (* Orphan: a second file whose executable no longer exists. Its data
     must not reach the report — under killed-anywhere-wins an excluded
     kill is the difference between a survivor and none, which is what
     the payload here is chosen to expose: [lt] is the live file's only
     survivor, and the orphan claims a crash killed it. *)
  let root, _ = stale_root "stale-orphan" in
  write_file
    (Filename.concat root "_build/_mutants/gone.mutants")
    (V.to_string
       ~identity:
         {
           V.exe = "default/test/gone.exe";
           digest = Digest.to_hex (Digest.string "gone");
         }
       (collection [ m_lt V.Killed ]));
  let code, out, err = mutate ~cwd:root [] in
  check_int "an orphan still reports the live data" ~expected:1 ~actual:code;
  equal ~msg:"the orphan's kill never reaches the report" text
    "mutants: 1 survived of 2 reached \u{00b7} 1 killed \u{00b7} 3 never \
     reached \u{00b7} 1 executable"
    (summary out);
  check_contains "the orphan warning names the file" ~needle:"gone.mutants" err;
  check_contains "the orphan warning names the missing executable"
    ~needle:"default/test/gone.exe" err;
  (* And names the remedy an orphan actually has. A re-run cannot replace
     a verdict whose executable is gone, so naming it here would send the
     reader round a loop that never terminates. *)
  check_contains "an orphan's remedy is deletion"
    ~needle:"delete the orphaned files" err;
  check_absent "and never the re-run, which cannot replace it"
    ~needle:"a forced run rewrites stale verdicts" err;
  (* Stale beside fresh: the report still renders, the outdated kill is
     excluded, and here the remedy is the forced re-run — which does
     rewrite a stale verdict. *)
  let root, _ = stale_root "stale-mixed" in
  let other = plant_exe root "default/test/b.exe" "the sibling build" in
  write_file
    (Filename.concat root "_build/_mutants/b.mutants")
    (V.to_string ~identity:other (collection [ m_lt V.Killed ]));
  write_file (Filename.concat root "_build/default/test/b.exe") "rebuilt since";
  let code, out, err = mutate ~cwd:root [] in
  check_int "a stale file beside a fresh one still reports" ~expected:1
    ~actual:code;
  equal ~msg:"the stale file's kill never reaches the report" text
    "mutants: 1 survived of 2 reached \u{00b7} 1 killed \u{00b7} 3 never \
     reached \u{00b7} 1 executable"
    (summary out);
  check_contains "a stale file's remedy is the forced re-run"
    ~needle:"a forced run rewrites stale verdicts" err;
  check_contains "and the re-run is the one command, spelled in full"
    ~needle:rerun err;
  check_absent "no deletion is asked for where nothing is orphaned"
    ~needle:"delete the orphaned files" err;
  (* Stale everywhere: the executable was rebuilt since the run, detected
     by content and not by mtime. *)
  let root, identity = stale_root "stale-rebuilt" in
  write_file
    (Filename.concat root "_build/default/test/a.exe")
    "a different build";
  check "the fixture really changed the executable"
    (Digest.to_hex (Digest.string "a different build") <> identity.V.digest);
  let code, _, err = mutate ~cwd:root [] in
  check_int "every file stale exits 1" ~expected:1 ~actual:code;
  check_contains "the stale warning names the executable"
    ~needle:"default/test/a.exe" err;
  check_contains "and says it did not write the file"
    ~needle:"not written by the executable now at" err;
  check_contains "the all-stale message states the situation"
    ~needle:"and every one is stale" err;
  check_contains "and names the one command" ~needle:rerun err

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
  write_file
    (Filename.concat root "_build/_mutants/unit.mutants")
    (V.to_string ~identity:unit
       (verdicts [ [ "calc"; "adds" ]; [ "calc"; "adds zero" ] ]));
  write_file
    (Filename.concat root "_build/_mutants/inline.mutants")
    (V.to_string ~identity:inline (verdicts [ [ "my_lib_expect"; "add" ] ]));
  write_file
    (Filename.concat root "_build/_mutants/plain.mutants")
    (V.to_string (verdicts [ [ "hand"; "written" ] ]));
  let code, out, err = mutate ~cwd:root [] in
  check_int "the survivor exits 1" ~expected:1 ~actual:code;
  check "nothing is stale" (err = "");
  check_contains "the sentence counts tests and executables"
    ~needle:"4 tests in 3 executables ran this line and none failed:" out;
  (* The column is as wide as the widest label plus the gap. *)
  let labels = [ "my_lib_expect"; "plain.mutants"; "test_calc.exe" ] in
  let width =
    3 + List.fold_left (fun w l -> max w (String.length l)) 0 labels
  in
  let row exe test = Printf.sprintf "      %-*s%s\n" width exe test in
  check_contains "the witness rows, labelled, in (executable, test) order"
    ~needle:
      (String.concat ""
         [
           row "my_lib_expect" "my_lib_expect \u{203a} add";
           row "plain.mutants" "hand \u{203a} written";
           row "test_calc.exe" "calc \u{203a} adds";
           row "test_calc.exe" "calc \u{203a} adds zero";
         ])
    out;
  equal ~msg:"the summary counts three executables" text
    "mutants: 1 survived of 1 reached \u{00b7} 3 executables" (summary out)

let survivor_order =
  test "survivors are ordered by witness count, then identifier" @@ fun () ->
  (* The survivor the most tests watched is the one a reader can act on
     soonest, so it prints first however its identifier sorts — the same
     order the per-executable report uses. [sub] (line 2) has three
     witnesses across two files; [add] (line 1) has one. *)
  let root = scratch "order" in
  plant_sources root;
  write_file
    (Filename.concat root "_build/_mutants/one.mutants")
    (V.to_string
       (collection
          [
            m_add (V.survived [ [ "t"; "a" ] ]);
            m_sub (V.survived [ [ "t"; "b" ]; [ "t"; "c" ] ]);
          ]));
  write_file
    (Filename.concat root "_build/_mutants/two.mutants")
    (V.to_string (collection [ m_sub (V.survived [ [ "u"; "d" ] ]) ]));
  let code, out, _ = mutate ~cwd:root [] in
  check_int "two survivors exit 1" ~expected:1 ~actual:code;
  check_contains "the most-watched survivor's sentence"
    ~needle:"3 tests in 2 executables ran this line and none failed:" out;
  check_before "the most-watched survivor prints first"
    ~first:"SURVIVED  lib/calc.ml:2:14:sub"
    ~second:"SURVIVED  lib/calc.ml:1:14:add" out

(* Loud failures and usage *)

let loud_failures =
  test "failures are loud: no data, corrupt data, foreign formats, usage"
  @@ fun () ->
  let empty = scratch "empty-root" in
  mkdir_p empty;
  let code, _, err = mutate ~cwd:empty [] in
  check_int "no .mutants files exit 1" ~expected:1 ~actual:code;
  check_contains "no files: the hint names the backend"
    ~needle:"(instrumentation (backend ppx_windtrap.mutate))" err;
  check_contains "no files: the hint names the one command" ~needle:rerun err;
  (* An existing but empty _build/_mutants is "no files", loudly. *)
  let bare = scratch "bare" in
  mkdir_p (Filename.concat bare "_build/_mutants");
  let code, _, err = mutate ~cwd:bare [] in
  check_int "an empty _build/_mutants exits 1" ~expected:1 ~actual:code;
  check_contains "an empty _build/_mutants prints the no-files hint"
    ~needle:"no .mutants files found" err;
  (* A truncated file is corrupt and named, never partially merged. *)
  let serialized = V.to_string file_a in
  write_file
    (scratch "trunc/_build/_mutants/cut.mutants")
    (String.sub serialized 0 (String.length serialized - 6));
  let code, out, err = mutate ~cwd:(scratch "trunc") [] in
  check_int "a truncated file exits 1" ~expected:1 ~actual:code;
  check_contains "a truncated file is named" ~needle:"cut.mutants" err;
  check_contains "a truncated file is called corrupt" ~needle:"corrupt" err;
  check "a corrupt file reports nothing at all" (out = "");
  (* A foreign magic is rejected, not converted: no partial merge, and
     the remedy is deletion because a re-run cannot remove it. *)
  let foreign = scratch "foreign/_build/_mutants/old.mutants" in
  write_file foreign "windtrap-mutants-v0\n1\nsome older payload\n";
  let code, _, err = mutate ~cwd:(scratch "foreign") [] in
  check_int "a foreign-format file exits 1" ~expected:1 ~actual:code;
  check_contains "a foreign-format file is named" ~needle:"old.mutants" err;
  check_contains "a foreign-format file names the expected magic"
    ~needle:"windtrap-mutants-v3" err;
  check_contains "a foreign format instructs deletion" ~needle:"delete" err;
  (* A coverage dump under _build/_mutants is the same rejection. *)
  let crossed = scratch "crossed/_build/_mutants/cov.mutants" in
  write_file crossed "WINDTRAP-COVERAGE-1\nsome v1 payload\n";
  let code, _, err = mutate ~cwd:(scratch "crossed") [] in
  check_int "a coverage dump in the mutants directory exits 1" ~expected:1
    ~actual:code;
  check_contains "and is named" ~needle:"cov.mutants" err;
  (* Usage errors are 2, and are the only 2 this command produces. *)
  let code, _, err = mutate ~cwd:proj [ "--frobnicate" ] in
  check_int "an unknown option exits 2" ~expected:2 ~actual:code;
  check_contains "an unknown option is named"
    ~needle:"unknown option '--frobnicate'" err;
  check_contains "an unknown option prints the usage"
    ~needle:"usage: windtrap mutate" err;
  (* There is no threshold: one survivor is the failure. *)
  let code, _, err = mutate ~cwd:proj [ "--min"; "80" ] in
  check_int "there is no --min threshold to pass" ~expected:2 ~actual:code;
  check_contains "and --min is simply unknown" ~needle:"unknown option '--min'"
    err;
  let code, out, _ = mutate ~cwd:proj [ "--help" ] in
  check_int "mutate --help exits 0" ~expected:0 ~actual:code;
  check_contains "mutate --help documents the PATH arguments"
    ~needle:"[PATH...]" out;
  check_contains "mutate --help says it drives nothing"
    ~needle:"Runs no tests and drives no build" out;
  check_contains "mutate --help states the exit code"
    ~needle:"Exits 1 when any mutant survived every executable that reached it."
    out

let dispatch =
  test "the binary dispatches mutate" @@ fun () ->
  let code, out, _ = capture [ "--help" ] in
  check_int "windtrap --help exits 0" ~expected:0 ~actual:code;
  check_contains "windtrap --help lists the subcommand and what it does"
    ~needle:"mutate      Merge .mutants verdict files" out;
  check_contains "windtrap --help still lists coverage"
    ~needle:"coverage    Merge .coverage files" out;
  let code, _, err = capture [ "mutant" ] in
  check_int "a near-miss command exits 2" ~expected:2 ~actual:code;
  check_contains "a near-miss command is named"
    ~needle:"unknown command 'mutant'" err

(* The suite *)

let () =
  run "mutate_cli"
    [
      two_executables;
      merge_report;
      merge_is_total;
      single_file;
      clean_report;
      only_unreached;
      discovery;
      explicit_paths;
      staleness;
      executable_labels;
      survivor_order;
      loud_failures;
      dispatch;
      law12_budget;
    ]
