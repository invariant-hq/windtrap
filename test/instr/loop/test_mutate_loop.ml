(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Drives suite_main.exe once per scenario and asserts on what came back.
   The loop forks, so nothing here can be observed from inside this
   process; this is the drive_*.ml shape the ppx suites already use, with
   windtrap assertions in place of committed goldens because a mutation
   report carries a fresh seed and a wall-clock duration on every run.

   The environment is scrubbed rather than extended: every WINDTRAP_*
   mirror, CI and the colour variables would reshape the transcript, and
   MUTATE_FIXTURE selects the suite. Each scenario states the whole
   environment it wants, so a variable set in the developer's shell can
   never change what a test asserts. *)

open Windtrap
module M = Windtrap_runtime.Mutate
module V = Windtrap_runtime.Verdicts
module Child = Windtrap_test_support.Child

let exe_dir = Filename.dirname Sys.executable_name
let suite_exe = Filename.concat exe_dir "suite_main.exe"

(* The verdict file records no cause and the runtime publishes no
   printer, so a scenario that reads verdicts back spells them here. *)
let pp_verdict ppf = function
  | V.Killed -> Format.pp_print_string ppf "killed"
  | V.Survived { first; others } ->
      Format.fprintf ppf "survived by %s"
        (String.concat ", " (List.map (String.concat " > ") (first :: others)))
  | V.Not_evaluated -> Format.pp_print_string ppf "not evaluated"
  | V.Unreached -> Format.pp_print_string ppf "unreached"

(* A file a fixture writes, read back; a file it never wrote reads as
   nothing. *)
let read_file path =
  if Sys.file_exists path then
    In_channel.with_open_bin path In_channel.input_all
  else ""

(* [occurs ~sub s] is [true] iff [sub] occurs in [s]: a predicate, for
   counting lines, where the facade's [contains] asserts. *)
let occurs ~sub s =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  go 0

let lines_with ~sub text =
  List.filter (occurs ~sub) (String.split_on_char '\n' text)

(* A scenario's variables, over Child.environment's, and no test is ever
   reported slow, so a transcript is the same on any machine. *)
let bindings vars = ("WINDTRAP_SLOW_THRESHOLD", "0") :: vars

(* A run the test watches while it goes: started here, and waited for
   by [finish]. [stdout], when given, is the child's standard output in
   place of the file, and stays the caller's to close: a pipe whose
   reader can leave. *)
type started = { pid : int; out_path : string; err_path : string }

let start ?(exe = suite_exe) ?(args = []) ?stdout vars =
  let dir = temp_dir () in
  let out_path = Filename.concat dir "out"
  and err_path = Filename.concat dir "err" in
  let open_target path =
    Unix.openfile path
      [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC; Unix.O_CLOEXEC ]
      0o644
  in
  let out = open_target out_path and err = open_target err_path in
  let pid =
    Fun.protect
      ~finally:(fun () ->
        Unix.close out;
        Unix.close err)
      (fun () ->
        Unix.create_process_env exe
          (Array.of_list (exe :: args))
          (Child.environment (bindings vars))
          Unix.stdin
          (Option.value stdout ~default:out)
          err)
  in
  { pid; out_path; err_path }

let finish { pid; out_path; err_path } =
  let status = snd (Unix.waitpid [] pid) in
  (status, read_file out_path, read_file err_path)

(* A run from start to end: its exit code, stdout and stderr. The inline
   runtime records its correction directory at module load, from its
   working directory, so [cwd] is how a scenario moves it. *)
let spawn ?(exe = suite_exe) ?(args = []) ?cwd vars =
  let r = Child.run ?cwd ~env:(bindings vars) exe args in
  (Child.exit_code r, r.Child.out, r.Child.err)

(* Waits for a file the run under watch writes, and gives up on the run,
   not on the suite, when it never comes: a process this suite started
   must not outlive it. *)
let await ~what started path =
  let rec poll attempts =
    if read_file path <> "" then ()
    else if attempts = 0 then begin
      (try Unix.kill started.pid Sys.sigkill with Unix.Unix_error _ -> ());
      ignore (finish started);
      failf "%s never happened" what
    end
    else begin
      Unix.sleepf 0.01;
      poll (attempts - 1)
    end
  in
  poll 3000

(* The scope that keeps this suite's fixtures controlled. Under
   --instrument-with the children link a mutation-instrumented windtrap
   core, and every count here (five sites, the reach map, the verdict
   file) is written against this directory's own fixtures: subject.ml,
   runaway/spinner.ml, inline/inline_armed.ml. The loop applies
   [--mutate]'s prefixes to the population it forks over, so the
   children test exactly those mutants whatever else they link. Every
   loop scenario passes this flag; the inline runner, whose argv is
   dune's protocol, gets the same scope through the mirror. *)
let scope = "test/instr/loop/"
let mutate = "--mutate=" ^ scope
let mutate_mirror = ("WINDTRAP_MUTATE", scope)

(* Under --instrument-with the core this suite links catalogues its own
   mutants; the scenarios that need a catalogue holding the fixture's
   alone, or none at all, skip there. *)
let core_instrumented = M.catalogue () <> []

(* The catalogue, read out of the binary rather than transcribed: a line
   moving in subject.ml must not silently re-point an armed identifier at
   another site. Narrowed to the scope, as the loop narrows its
   population: under --instrument-with the binary catalogues the core's
   sites before the fixture's, and an identifier the scenarios arm by
   position or by rewrite must be this directory's. *)
let catalogue =
  lazy
    (let code, out, err = spawn [ ("MUTATE_FIXTURE", "catalogue") ] in
     if code <> 0 then failf "catalogue mode exited %d: %s" code err;
     List.filter
       (String.starts_with ~prefix:scope)
       (String.split_on_char '\n' out))

let mutant_named rewrite =
  match
    List.find_opt
      (fun id ->
        String.length id > 0 && Filename.check_suffix id (":" ^ rewrite))
      (Lazy.force catalogue)
  with
  | Some id -> id
  | None ->
      failf "no %S mutant in the catalogue: %s" rewrite
        (String.concat ", " (Lazy.force catalogue))

(* A report with what is measured or located rather than computed
   masked: the dry run's duration, and the fixture's line beside each
   reaching test (suite_main.ml moves under edits no claim here is about).
   Everything else is compared byte for byte. *)
let masked report =
  let find ~sub ?(from = 0) s =
    let n = String.length s and m = String.length sub in
    let rec go i =
      if i + m > n then None
      else if String.sub s i m = sub then Some i
      else go (i + 1)
    in
    go from
  in
  let mask_line line =
    let line =
      match find ~sub:" passed in " line with
      | Some i when String.starts_with ~prefix:"calc: " line ->
          String.sub line 0 (i + String.length " passed in ") ^ "<time>."
      | _ -> line
    in
    match find ~sub:"suite_main.ml:" line with
    | Some i ->
        let start = i + String.length "suite_main.ml:" in
        let stop = ref start in
        while
          !stop < String.length line
          && line.[!stop] >= '0'
          && line.[!stop] <= '9'
        do
          incr stop
        done;
        String.sub line 0 start ^ "<line>"
        ^ String.sub line !stop (String.length line - !stop)
    | None -> line
  in
  String.concat "\n" (List.map mask_line (String.split_on_char '\n' report))

(* The first block of [held]'s report, up to the location of its one
   reaching test. *)
let sub_block =
  "  SURVIVED  test/instr/loop/subject.ml:15:14:add  a - b \u{2192} a + b\n\
  \      15 \u{2502} let sub a b = a - b\n\n\
  \    1 test ran this line and did not fail:\n\
  \      held \u{203a} watches sub without pinning it  "

(* A real identifier of this executable's own catalogue with its line
   moved off every site: the file is one the binary was built from, so
   the identifier is stale rather than another binary's, and it is the
   case that must still refuse. Derived from the catalogue so that it
   cannot accidentally become a valid site. *)
let stale_id () =
  let id = mutant_named "add" in
  match String.split_on_char ':' id with
  | [ file; _line; col; rewrite ] ->
      String.concat ":" [ file; "999"; col; rewrite ]
  | _ -> failf "unexpected identifier %S" id

(* The [reproduce:] command's flag, as a reader would paste it: the
   identifier it carries has to be the first survivor's as its block
   spells it, and pasting it back is the only test of the line that can
   fail when that identifier is not one the runtime resolves. *)
let reproduce_arm_of report =
  let lines = String.split_on_char '\n' report in
  let id_of line =
    match String.split_on_char ' ' (String.trim line) with
    | "SURVIVED" :: rest -> List.find_opt (fun w -> w <> "") rest
    | _ -> None
  in
  let survivor =
    match List.find_map id_of lines with
    | Some id -> id
    | None -> failf "no SURVIVED row in the report:\n%s" report
  in
  match List.filter (String.starts_with ~prefix:"reproduce: ") lines with
  | [ line ] ->
      if not (String.ends_with ~suffix:(" --arm " ^ survivor) line) then
        failf "the reproduce line does not arm the first survivor %s:\n%s"
          survivor line;
      [ "--arm"; survivor ]
  | lines ->
      failf "%d reproduce lines in the report:\n%s" (List.length lines) report

(* The command of the report's one [reproduce:] line, as a reader pastes it
   into a shell. *)
let reproduce_command report =
  match
    List.filter
      (String.starts_with ~prefix:"reproduce: ")
      (String.split_on_char '\n' report)
  with
  | [ line ] ->
      String.sub line
        (String.length "reproduce: ")
        (String.length line - String.length "reproduce: ")
  | _ -> failf "no single reproduce line in:\n%s" report

(* The verdict file this executable writes, deleted before every scenario
   that is meant to produce one so that a stale file cannot pass a test
   the loop failed to write. *)
let verdict_path = V.output_file ~exe:suite_exe

(* [V.load] of the bytes a run left at [verdict_path], captured when the
   run ended: a later run of this suite replaces the file. *)
let load_saved saved =
  let path = Filename.concat (temp_dir ()) (Filename.basename verdict_path) in
  Out_channel.with_open_bin path (fun oc -> Out_channel.output_string oc saved);
  V.load path

(* The [green] suite's loop under the directory's scope, run once for
   every scenario that reads it: its exit code, its two streams, and the
   verdict file as the run left it, read at once, before another
   scenario's run rewrites the file. Each scenario still parses and
   judges what it reads. *)
type loop_run = { code : int; out : string; err : string; saved : string }

let green =
  lazy
    ((try Sys.remove verdict_path with Sys_error _ -> ());
     let code, out, err = spawn ~args:[ mutate ] [] in
     { code; out; err; saved = read_file verdict_path })

let catalogue_tests =
  [
    test "the catalogue is exactly the five sites the report accounts for"
      (fun () ->
        (* Ordered by position, so the indices the scenarios below arm by
           are the source's own order: [sub], [widen], [orphan],
           [crasher], [dismissed]. *)
        equal ~msg:"the first is [sub]'s" string
          (List.hd (Lazy.force catalogue))
          (mutant_named "add");
        equal ~msg:"five mutants, one of them dismissed" int 5
          (List.length (Lazy.force catalogue)));
  ]

let unasked_tests =
  [
    test "an instrumented run that was not asked to mutate runs ordinarily"
      (fun () ->
        let code, out, err = spawn [] in
        equal ~msg:"exit code" int 0 code;
        contains ~msg:"summary" ~sub:"calc: 6 passed" out;
        not_contains ~msg:"and says nothing about mutants" ~sub:"mutants:" out;
        equal ~msg:"stderr" text "" err);
  ]

(* [--mutate]'s prefixes narrow the population the loop forks over, not
   the registry and not the report, and the two consequences below are
   what the rest of this tree relies on: a scope that matches nothing is
   refused by name, and a scope that matches keeps the fixture's own
   mutants whole. Every other scenario in this file passes the directory
   scope through [mutate], so without these the feature would only ever
   be exercised incidentally. *)
let scope_tests =
  [
    test "a scope that matches nothing is refused, naming the scope" (fun () ->
        (* The loop declines by name rather than reporting nothing. *)
        let code, _, err = spawn ~args:[ "--mutate=::no-such-source::" ] [] in
        equal ~msg:"asking it to mutate exits 1" int 1 code;
        (* The build is instrumented and fine; the scope is what emptied
           the population, so the refusal must name it; blaming
           instrumentation would send the reader to rebuild. *)
        contains ~msg:"declines by naming the flag, value included"
          ~sub:"windtrap: --mutate=::no-such-source:: leaves no mutant" err;
        contains ~msg:"and both causes an empty scoped catalogue has"
          ~sub:
            "no instrumented file matches the prefix (is the library under \
             test instrumented with ppx_windtrap.mutate?), or the matched \
             files have no mutation sites"
          err;
        not_contains ~msg:"never the missing-backend diagnosis"
          ~sub:"links no instrumented module" err);
    test "a scope that matches keeps the whole fixture catalogue" (fun () ->
        let { code; out; _ } = Lazy.force green in
        equal ~msg:"exit code" int 0 code;
        contains ~msg:"the fixture's reach, undiminished"
          ~sub:
            "mutants: 1 survived of 2 reached by this suite, 1 killed, 2 never \
             reached\n"
          out);
    test "the mirror reads a non-boolean value as the prefixes" (fun () ->
        (* WINDTRAP_MUTATE=<prefix> is what reaches a suite no command
           line reaches: the same scope, through the flag's own parser. *)
        let code, out, _ = spawn [ mutate_mirror ] in
        equal ~msg:"exit code" int 0 code;
        contains ~msg:"the same population"
          ~sub:
            "mutants: 1 survived of 2 reached by this suite, 1 killed, 2 never \
             reached\n"
          out);
  ]

let loop_tests =
  [
    test
      "the loop kills one mutant, names the survivor's reaching tests and \
       counts the unreached by file" (fun () ->
        let { code; out; err; _ } = Lazy.force green in
        equal ~msg:"exit code (a survivor never fails the build)" int 0 code;
        equal ~msg:"stderr" text "" err;
        (* The dry run's ordinary summary, then the survivors section,
           opened before the loop knew their count, holding one block: the
           killed mutant is not a survivor and not unreached. The block is
           the finding and the footer the remedy: no per-block command, no
           attribute to paste. The mutants no test of this suite evaluated
           are one row for their file, [orphan] and [crasher] by line, never
           a block each. The reproduce command comes above the outcome,
           which is last. *)
        equal ~msg:"the report, whole" text
          ("calc: 6 passed in <time>.\n\n\
            ─────────────────────── survivors ────────────────────────\n\
           \  SURVIVED  " ^ mutant_named "sub"
         ^ "  a + b \u{2192} a - b\n\
           \      18 \u{2502} let widen a b = a + b\n\n\
           \    2 tests ran this line and none failed:\n\
           \      widen \u{203a} widen is nonzero  \
            test/instr/loop/suite_main.ml:<line>\n\
           \      widen \u{203a} widen is not 99   \
            test/instr/loop/suite_main.ml:<line>\n\
            ──────────────────────────────────────────────────────────\n\n\
            ─────────────────── never reached (2) ────────────────────\n\
           \  2  test/instr/loop/subject.ml   lines 21, 27\n\
            ──────────────────────────────────────────────────────────\n\n\
            reproduce: " ^ suite_exe ^ " --arm " ^ mutant_named "sub"
         ^ "\n\
            mutants: 1 survived of 2 reached by this suite, 1 killed, 2 never \
            reached\n")
          (masked out));
    test "a dismissed mutant is in no block and no count" (fun () ->
        let { out; _ } = Lazy.force green in
        (* The [green] suite runs the [@mutate off] site and pins nothing
           about it, so without the dismissal it would be a second
           survivor and a third reached mutant. *)
        contains ~msg:"the reached count leaves it out"
          ~sub:
            "mutants: 1 survived of 2 reached by this suite, 1 killed, 2 never \
             reached\n"
          out;
        contains ~msg:"and so does the never-reached row"
          ~sub:"   lines 21, 27\n" out;
        not_contains ~msg:"no block for the dismissed line"
          ~sub:(List.nth (Lazy.force catalogue) 4)
          out);
    test "the survivor block quotes the mutated source line" (fun () ->
        let { out; _ } = Lazy.force green in
        contains ~msg:"excerpt row"
          ~sub:"\n      18 \u{2502} let widen a b = a + b\n" out);
    test
      "a survivor's source line is read under the project root, then as \
       recorded" (fun () ->
        let plant dir comment =
          let path = Filename.concat dir "test/instr/loop/subject.ml" in
          Sys.mkdir (Filename.concat dir "test") 0o755;
          Sys.mkdir (Filename.concat dir "test/instr") 0o755;
          Sys.mkdir (Filename.dirname path) 0o755;
          Out_channel.with_open_bin path (fun oc ->
              output_string oc (String.make 17 '\n');
              output_string oc ("let widen a b = a + b " ^ comment ^ "\n"))
        in
        let root = temp_dir () and cwd = temp_dir () in
        plant root "(* under the root *)";
        plant cwd "(* as recorded *)";
        let excerpt project_root =
          let _, out, _ =
            spawn ~cwd ~args:[ mutate; "-f"; "widen" ]
              [
                ("MUTATE_FIXTURE", "weak");
                ("WINDTRAP_PROJECT_ROOT", project_root);
              ]
          in
          out
        in
        contains ~msg:"the root's copy first"
          ~sub:
            "\n      18 \u{2502} let widen a b = a + b (* under the root *)\n"
          (excerpt root);
        contains ~msg:"the recorded path when the root has none"
          ~sub:"\n      18 \u{2502} let widen a b = a + b (* as recorded *)\n"
          (excerpt (temp_dir ())));
    test "the reproduce command spells the flag that arms the survivor"
      (fun () ->
        let _, out, _ = spawn ~args:[ mutate ] [ ("INSIDE_DUNE", "1") ] in
        contains ~msg:"the backend flag, before the target"
          ~sub:"reproduce: dune exec --instrument-with ppx_windtrap.mutate " out;
        contains
          ~msg:"and --arm after the separator, with the survivor's identifier"
          ~sub:(" -- --arm " ^ mutant_named "sub" ^ "\n")
          out;
        not_contains ~msg:"no placeholder is left to fill" ~sub:"<id>" out;
        (* The footer is completed and pasted back rather than
           pattern-matched: the identifier the report prints has to be one
           the runtime's own selector grammar resolves, and the only proof
           of that is a run that announces the same rewrite. *)
        let arm = reproduce_arm_of out in
        let code, armed, _ = spawn ~args:arm [] in
        equal ~msg:"the armed run's exit code (this mutant survives)" int 0 code;
        contains ~msg:"the pasted line armed the survivor"
          ~sub:"armed: a + b \u{2192} a - b" armed);
    test "a filtered run's command arms the mutant under that filter" (fun () ->
        (* A survivor of a narrowed run survived that selection only, and a
           test left out of it may kill the mutant: the command restates
           the filter, and pasted whole it runs the same selection. *)
        let _, out, _ =
          spawn ~args:[ mutate; "-f"; "widen" ] [ ("INSIDE_DUNE", "1") ]
        in
        contains ~msg:"the selection scopes the summary"
          ~sub:"reached by the 2 selected tests" out;
        contains ~msg:"and rides the command"
          ~sub:
            ("test/instr/loop/suite_main.exe -- --arm " ^ mutant_named "sub"
           ^ " -f 'widen'\n")
          out;
        let _, out, _ = spawn ~args:[ mutate; "-f"; "widen" ] [] in
        let code, armed, err =
          spawn ~exe:"/bin/sh" ~args:[ "-c"; reproduce_command out ] []
        in
        equal ~msg:"the shell found the executable" text "" err;
        equal ~msg:"the armed run's exit code (this mutant survives)" int 0 code;
        contains ~msg:"the pasted line armed the survivor"
          ~sub:"armed: a + b \u{2192} a - b" armed;
        contains ~msg:"under the run's selection" ~sub:"2 passed" armed);
    test "the command runs as pasted from a path a shell would split" (fun () ->
        (* The executable's path goes through the shell's quoting, and the
           line is handed to a shell whole, as a reader's paste is. *)
        let root = temp_dir () in
        let dir = Filename.concat root "a suite's dir" in
        Sys.mkdir dir 0o755;
        let exe = Filename.concat dir "suite main.exe" in
        Out_channel.with_open_bin exe (fun oc ->
            output_string oc (read_file suite_exe));
        Unix.chmod exe 0o755;
        (* Narrowed, so that this copy leaves no verdict file behind. *)
        let _, out, _ = spawn ~exe ~args:[ mutate; "-f"; "widen" ] [] in
        let command = reproduce_command out in
        equal ~msg:"the path is one quoted word" string
          ("'"
          ^ Filename.concat root "a suite'\\''s dir/suite main.exe'"
          ^ " --arm " ^ mutant_named "sub" ^ " -f 'widen'")
          command;
        let code, armed, err =
          spawn ~exe:"/bin/sh" ~args:[ "-c"; command ] []
        in
        equal ~msg:"the shell found the executable" text "" err;
        equal ~msg:"the armed run's exit code (this mutant survives)" int 0 code;
        contains ~msg:"the pasted line armed the survivor"
          ~sub:"armed: a + b \u{2192} a - b" armed;
        let _, out, _ =
          spawn ~exe ~args:[ mutate; "-f"; "widen" ] [ ("INSIDE_DUNE", "1") ]
        in
        contains ~msg:"dune's target is quoted the same way"
          ~sub:
            ("suite main.exe' -- --arm " ^ mutant_named "sub" ^ " -f 'widen'\n")
          out;
        contains ~msg:"as one word after the backend"
          ~sub:"reproduce: dune exec --instrument-with ppx_windtrap.mutate '"
          out);
    test "every survivor gets a block" (fun () ->
        let _, out, _ =
          spawn ~args:[ mutate ] [ ("MUTATE_FIXTURE", "capped") ]
        in
        (* Both blocks, the second one blank line under the first, and
           the summary counts both. *)
        equal ~msg:"the report, whole" text
          ("calc: 6 passed in <time>.\n\n\
            ─────────────────────── survivors ────────────────────────\n\
           \  SURVIVED  " ^ mutant_named "sub"
         ^ "  a + b \u{2192} a - b\n\
           \      18 \u{2502} let widen a b = a + b\n\n\
           \    2 tests ran this line and none failed:\n\
           \      widen \u{203a} widen is nonzero  \
            test/instr/loop/suite_main.ml:<line>\n\
           \      widen \u{203a} widen is not 99   \
            test/instr/loop/suite_main.ml:<line>\n\n\
           \  SURVIVED  "
          ^ List.nth (Lazy.force catalogue) 2
          ^ "  a + b \u{2192} a - b\n\
            \      21 \u{2502} let orphan a b = a + b\n\n\
            \    1 test ran this line and did not fail:\n\
            \      orphan \u{203a} orphan is nonzero  \
             test/instr/loop/suite_main.ml:<line>\n\
             ──────────────────────────────────────────────────────────\n\n\
             ─────────────────── never reached (1) ────────────────────\n\
            \  1  test/instr/loop/subject.ml   lines 27\n\
             ──────────────────────────────────────────────────────────\n\n\
             reproduce: " ^ suite_exe ^ " --arm " ^ mutant_named "sub"
          ^ "\n\
             mutants: 2 survived of 3 reached by this suite, 1 killed, 1 never \
             reached\n")
          (masked out));
    test
      "survivors print in the catalogue's order, however many tests reach them"
      (fun () ->
        (* A block prints when its child ends, so the order is the one
           the children run in: [sub], which one test reaches, before
           [widen], which two do. *)
        let _, out, _ = spawn ~args:[ mutate ] [ ("MUTATE_FIXTURE", "held") ] in
        (* The survivor one test reaches prints first, and the command
           arms the first one printed. *)
        equal ~msg:"the report, whole" text
          ("calc: 3 passed in <time>.\n\n\
            ─────────────────────── survivors ────────────────────────\n"
         ^ sub_block ^ "test/instr/loop/suite_main.ml:<line>\n\n  SURVIVED  "
         ^ mutant_named "sub"
         ^ "  a + b \u{2192} a - b\n\
           \      18 \u{2502} let widen a b = a + b\n\n\
           \    2 tests ran this line and none failed:\n\
           \      held \u{203a} widen is nonzero, once the gate opens  \
            test/instr/loop/suite_main.ml:<line>\n\
           \      held \u{203a} widen is not 99                        \
            test/instr/loop/suite_main.ml:<line>\n\
            ──────────────────────────────────────────────────────────\n\n\
            ─────────────────── never reached (2) ────────────────────\n\
           \  2  test/instr/loop/subject.ml   lines 21, 27\n\
            ──────────────────────────────────────────────────────────\n\n\
            reproduce: " ^ suite_exe ^ " --arm " ^ mutant_named "add"
         ^ "\nmutants: 2 survived of 2 reached by this suite, 2 never reached\n"
          )
          (masked out));
    test
      "a loop that kills everything it reaches, and reaches everything, is two \
       lines" (fun () ->
        let code, out, err =
          spawn ~args:[ mutate ] [ ("MUTATE_FIXTURE", "pinned") ]
        in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"stderr" text "" err;
        equal ~msg:"the dry run's summary, then the outcome, and no rule" text
          "calc: 6 passed in <time>.\n\
           mutants: 4 reached by this suite, 4 killed\n"
          (masked out));
  ]

(* The reach map's boundaries. Every claim here has a wrong answer the
   implementation could give, and the [boundary] fixture is built so that
   each wrong answer changes a number this test reads. *)
let reach_tests =
  [
    test
      "module initialization, a retry and a fixture release each land on the \
       right side of a test boundary" (fun () ->
        (try Sys.remove verdict_path with Sys_error _ -> ());
        let code, out, err =
          spawn ~args:[ mutate ] [ ("MUTATE_FIXTURE", "boundary") ]
        in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"stderr" text "" err;
        (* [orphan] is evaluated at module load and [crasher] by a fixture
           release: both are outside every test, and folding either into
           the test on whose side of the boundary it sits would make it a
           permanent false survivor. The report counts only what was
           reached; the verdict file, which the merge reads, names the
           two as unreached. *)
        contains ~msg:"the two out-of-test sites are not reached"
          ~sub:
            "mutants: 1 survived of 2 reached by this suite, 1 killed, 2 never \
             reached\n"
          out;
        contains ~msg:"and are the never-reached row's two lines"
          ~sub:"\n  2  test/instr/loop/subject.ml   lines 21, 27\n" out;
        (match V.load verdict_path with
        | Error e -> failf "verdict file unreadable: %a" V.pp_error e
        | Ok (verdicts, _) ->
            equal ~msg:"orphan and crasher, by line, unreached in the file"
              (list int) [ 21; 27 ]
              (List.filter_map
                 (fun (r : V.record) ->
                   if r.V.verdict = V.Unreached then Some r.V.id.M.line
                   else None)
                 (V.records verdicts)));
        equal ~msg:"exactly one survivor" int 1
          (List.length (lines_with ~sub:"  SURVIVED  " out));
        (* The reaching tests are the whole product of the run: the first and
           third tests reach the line, the second, fourth and fifth do
           not, and a retried first test contributes one window. *)
        contains ~msg:"the count" ~sub:"2 tests ran this line and none failed:"
          out;
        contains ~msg:"the first test"
          ~sub:"widen \u{203a} first reaches widen, after a retry" out;
        contains ~msg:"the third test" ~sub:"widen \u{203a} third reaches widen"
          out;
        not_contains ~msg:"not the second"
          ~sub:"widen \u{203a} second reaches sub" out;
        not_contains ~msg:"not the fourth"
          ~sub:"widen \u{203a} fourth reaches sub" out;
        not_contains ~msg:"not the fifth"
          ~sub:"widen \u{203a} fifth reaches sub" out;
        contains ~msg:"summary"
          ~sub:
            "mutants: 1 survived of 2 reached by this suite, 1 killed, 2 never \
             reached\n"
          out);
    test "a tag selection still selects the child's tests" (fun () ->
        (* [--tag gated] is the one selection a pruned tree cannot
           express: tags are not in a path. A child that dropped the
           parent's tag predicate runs nothing, and the determinism probe
           reports a deterministic suite as non-deterministic. *)
        let code, out, err =
          spawn ~args:[ mutate ]
            [ ("MUTATE_FIXTURE", "tagged"); ("WINDTRAP_TAG", "gated") ]
        in
        equal ~msg:"exit code" int 0 code;
        not_contains ~msg:"the probe agreed" ~sub:"not deterministic" err;
        contains ~msg:"the dry run's five tests" ~sub:"calc: 5 passed" out;
        contains ~msg:"and the loop scored them, against the five it selected"
          ~sub:
            "mutants: 1 survived of 2 reached by the 5 selected tests, 1 \
             killed, 2 never reached\n"
          out;
        contains ~msg:"over the same reaching tests as the untagged suite"
          ~sub:"2 tests ran this line and none failed:" out;
        (* A tag selection is a selection: the run is not the suite's
           default predicate, so its verdicts stay in the process. *)
        contains ~msg:"and a tag-selected run persists nothing, said on stderr"
          ~sub:
            "windtrap: verdicts not saved: this run's selection narrows the \
             suite"
          err);
  ]

(* A test marked xfail reaches no mutant. The [known] suite pairs each of
   [sub] and [widen] with an ordinary test that pins nothing and an xfail
   test, and gives [orphan] an xfail test alone. [sub adds] passes where
   [sub]'s mutant is armed: counted as a reaching test, it would kill that
   mutant, and [orphan] would survive with an xfail test named as one that
   did not fail. *)
let known_loop =
  lazy
    ((try Sys.remove verdict_path with Sys_error _ -> ());
     let code, out, err =
       spawn ~args:[ mutate ] [ ("MUTATE_FIXTURE", "known") ]
     in
     { code; out; err; saved = read_file verdict_path })

let known_armed index =
  spawn
    ~args:[ "--arm"; List.nth (Lazy.force catalogue) index ]
    [ ("MUTATE_FIXTURE", "known") ]

let xfail_tests =
  [
    test "no survivor names an xfail test, and no xfail test kills a mutant"
      (fun () ->
        let { code; out; err; _ } = Lazy.force known_loop in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"stderr" text "" err;
        let summary, report =
          match String.index_opt out '\n' with
          | Some i ->
              (String.sub out 0 i, String.sub out i (String.length out - i))
          | None -> (out, "")
        in
        is_true ~msg:"the dry run counts the xfail tests as expected failures"
          (String.starts_with ~prefix:"calc: 2 passed, 3 expected failures in "
             summary);
        equal ~msg:"the report after the summary, whole" text
          ("\n\n\
            ─────────────────────── survivors ────────────────────────\n\
           \  SURVIVED  " ^ mutant_named "add"
         ^ "  a - b \u{2192} a + b\n\
           \      15 \u{2502} let sub a b = a - b\n\n\
           \    1 test ran this line and did not fail:\n\
           \      known \u{203a} watches sub without pinning it  \
            test/instr/loop/suite_main.ml:<line>\n\n\
           \  SURVIVED  " ^ mutant_named "sub"
         ^ "  a + b \u{2192} a - b\n\
           \      18 \u{2502} let widen a b = a + b\n\n\
           \    1 test ran this line and did not fail:\n\
           \      known \u{203a} widen is nonzero  \
            test/instr/loop/suite_main.ml:<line>\n\
            ──────────────────────────────────────────────────────────\n\n\
            ─────────────────── never reached (2) ────────────────────\n\
           \  2  test/instr/loop/subject.ml   lines 21, 27\n\
            ──────────────────────────────────────────────────────────\n\n\
            reproduce: " ^ suite_exe ^ " --arm " ^ mutant_named "add"
         ^ "\nmutants: 2 survived of 2 reached by this suite, 2 never reached\n"
          )
          (masked report));
    test "the verdict file names no xfail test" (fun () ->
        let { saved; _ } = Lazy.force known_loop in
        match load_saved saved with
        | Error e -> failf "verdict file unreadable: %a" V.pp_error e
        | Ok (verdicts, _) ->
            equal ~msg:"sub and widen survive their ordinary tests alone"
              (list string)
              [
                "15 survived by known > watches sub without pinning it";
                "18 survived by known > widen is nonzero";
                "21 unreached";
                "27 unreached";
              ]
              (List.map
                 (fun (r : V.record) ->
                   Format.asprintf "%d %a" r.V.id.M.line pp_verdict r.V.verdict)
                 (V.records verdicts)));
    test "an armed mutant that only an xfail test runs is not reached"
      (fun () ->
        let code, out, _ = known_armed 2 in
        equal ~msg:"the xfail test failed as expected" int 0 code;
        contains ~msg:"the closing line names the xfail tests"
          ~sub:"\nmutant not reached: only xfail tests ran the site.\n" out;
        not_contains ~msg:"no survivor claim" ~sub:"mutant survived" out);
    test "an armed mutant that an xfail test passes on survives" (fun () ->
        let code, out, _ = known_armed 0 in
        equal ~msg:"the unexpected pass fails the run" int 1 code;
        contains ~msg:"and prints as the failure it is"
          ~sub:"expected to fail (sub subtracts), but the test passed" out;
        contains ~msg:"the closing line says only xfail tests failed"
          ~sub:
            "\n\
             mutant survived: the site was evaluated 2 times and only xfail \
             tests failed.\n"
          out;
        not_contains ~msg:"no kill" ~sub:"mutant killed." out);
    test "an armed mutant an xfail test also runs counts every evaluation"
      (fun () ->
        let code, out, _ = known_armed 1 in
        equal ~msg:"exit code" int 0 code;
        contains ~msg:"the closing line"
          ~sub:
            "\n\
             mutant survived: the armed site was evaluated 2 times and no test \
             failed.\n"
          out);
  ]

(* The [memo] suite memoizes [widen]: the dry run fills the table, so the
   child that arms [widen]'s mutant reads the answer from it and never
   evaluates the site. Its test passes there, and an armed run, a new
   process, fails it. The loop must say that the child did not evaluate the
   site, and must not call the mutant a survivor. *)
let memo_loop =
  lazy
    ((try Sys.remove verdict_path with Sys_error _ -> ());
     let code, out, err =
       spawn ~args:[ mutate ] [ ("MUTATE_FIXTURE", "memo") ]
     in
     { code; out; err; saved = read_file verdict_path })

let not_evaluated_tests =
  [
    test "a mutant its child did not evaluate is no survivor" (fun () ->
        let { code; out; err; _ } = Lazy.force memo_loop in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"stderr" text "" err;
        equal ~msg:"the report, whole" text
          ("calc: 4 passed in <time>.\n\n\
            ─────────────────── not evaluated (1) ────────────────────\n\
           \  Each site ran in the dry run and not in its mutant's child.\n\
           \  " ^ mutant_named "sub" ^ "  a + b \u{2192} a - b\n    arm: "
         ^ suite_exe ^ " --arm " ^ mutant_named "sub"
         ^ "\n\
            ──────────────────────────────────────────────────────────\n\n\
            ─────────────────── never reached (2) ────────────────────\n\
           \  2  test/instr/loop/subject.ml   lines 21, 27\n\
            ──────────────────────────────────────────────────────────\n\n\
            mutants: 2 reached by this suite, 1 killed, 1 not evaluated, 2 \
            never reached\n")
          (masked out));
    test "its arm command kills it in a new process" (fun () ->
        let code, out, _ =
          spawn
            ~args:[ "--arm"; mutant_named "sub" ]
            [ ("MUTATE_FIXTURE", "memo") ]
        in
        equal ~msg:"the memoized test failed" int 1 code;
        contains ~msg:"the closing line" ~sub:"\nmutant killed.\n" out);
    test "the verdict file records it as not evaluated" (fun () ->
        let { saved; _ } = Lazy.force memo_loop in
        match load_saved saved with
        | Error e -> failf "verdict file unreadable: %a" V.pp_error e
        | Ok (verdicts, _) ->
            equal ~msg:"sub killed, widen not evaluated" (list string)
              [
                "15 killed"; "18 not evaluated"; "21 unreached"; "27 unreached";
              ]
              (List.map
                 (fun (r : V.record) ->
                   Format.asprintf "%d %a" r.V.id.M.line pp_verdict r.V.verdict)
                 (V.records verdicts)));
  ]

(* Child hygiene: a mutation child leaves through [Unix._exit] and nothing
   else, so no [at_exit] handler of the parent's image ever runs in one,
   which is what stops a crashing child overwriting the parent's
   [.coverage] dump, since that dump IS an at_exit handler. The witness
   generalizes it: the fixture appends its pid on every path through
   Stdlib's exit machinery, and after a loop that forked five children the
   file must name the parent once.

   The [fatal] fixture is the case that makes it bite: [Out_of_memory] is
   fatal, so no failure boundary in the runner may swallow it, it escapes
   [Run.execute], and the child's own wrapper is the only thing between
   it and OCaml's uncaught-exception handler, which runs [at_exit] before
   it prints. Remove that wrapper's catch-all and this file gains a
   line. *)
let no_trace_tests =
  [
    test "no at_exit handler runs in a mutation child, fatal exception included"
      (fun () ->
        let log = Filename.concat (temp_dir ()) "atexit" in
        let code, out, err =
          spawn ~args:[ mutate ]
            [ ("MUTATE_FIXTURE", "fatal"); ("MUTATE_ATEXIT_LOG", log) ]
        in
        equal ~msg:"the parent completed" int 0 code;
        equal ~msg:"stderr" text "" err;
        contains ~msg:"the fatal child scored as a crash kill, the report whole"
          ~sub:
            "mutants: 1 survived of 3 reached by this suite, 2 killed, 1 never \
             reached\n"
          out;
        let lines =
          List.filter
            (fun l -> l <> "")
            (String.split_on_char '\n' (read_file log))
        in
        equal ~msg:"exactly one process reached at_exit: the parent" int 1
          (List.length lines));
  ]

(* What a narrowed run says in place of the verdict file it does not
   write. *)
let not_saved =
  "windtrap: verdicts not saved: this run's selection narrows the suite, and a \
   partial run's verdicts would stand in the project merge as the whole.\n"

let verdict_file_tests =
  [
    test "the loop writes a verdict file the runtime can read back" (fun () ->
        let { code; saved; _ } = Lazy.force green in
        equal ~msg:"exit code" int 0 code;
        is_true ~msg:"the file exists" (saved <> "");
        match load_saved saved with
        | Error e -> failf "verdict file unreadable: %a" V.pp_error e
        | Ok (verdicts, identity) ->
            is_true ~msg:"the writer identity is recorded" (identity <> None);
            let rendered =
              List.map
                (fun (r : V.record) ->
                  ( M.id_to_string r.V.id,
                    Format.asprintf "%a" pp_verdict r.V.verdict ))
                (V.records verdicts)
            in
            equal ~msg:"one verdict per mutant" int 4 (List.length rendered);
            let killed =
              List.filter
                (fun (_, v) -> String.starts_with ~prefix:"killed" v)
                rendered
            in
            let survived =
              List.filter
                (fun (_, v) -> String.starts_with ~prefix:"survived" v)
                rendered
            in
            let unreached =
              List.filter (fun (_, v) -> v = "unreached") rendered
            in
            equal ~msg:"one kill" int 1 (List.length killed);
            equal ~msg:"one survivor" int 1 (List.length survived);
            equal ~msg:"two unreached" int 2 (List.length unreached);
            equal ~msg:"the kill is the [add] mutant" string
              (mutant_named "add")
              (fst (List.hd killed));
            contains ~msg:"the survivor names its reaching tests"
              ~sub:"widen > widen is nonzero"
              (snd (List.hd survived)));
    test "a narrowed run reports in full but persists nothing" (fun () ->
        let { code; err; saved; _ } = Lazy.force green in
        equal ~msg:"the full run's exit code" int 0 code;
        not_contains ~msg:"a full run saves without comment"
          ~sub:"verdicts not saved" err;
        is_true ~msg:"and wrote the file" (saved <> "");
        (* The file as the full run left it, whatever ran since. *)
        Out_channel.with_open_bin verdict_path (fun oc ->
            output_string oc saved);
        (* The selection reaches only [sub], whose mutant dies, so the
           loop completes, and its verdicts call [widen] unreached, which
           is exactly the selection-relative record that must not
           overwrite the full run's survivor. [mutate]'s scope is in
           force here too, so this is also the combined case: a filter
           skips the write even where the scope alone would still save. *)
        let code, out, err = spawn ~args:[ mutate; "-f"; "calc" ] [] in
        equal ~msg:"the narrowed run still completes" int 0 code;
        contains ~msg:"and still reports, against its selection"
          ~sub:
            "mutants: 1 reached by the 3 selected tests, 1 killed, 3 never \
             reached\n"
          out;
        not_contains ~msg:"a clean report has nothing to reproduce"
          ~sub:"reproduce:" out;
        (* Windtrap's own word, so on stderr: the report ends on its
           [mutants:] line. *)
        equal ~msg:"but says what it did not persist, and nothing else" text
          "windtrap: verdicts not saved: this run's selection narrows the \
           suite, and a partial run's verdicts would stand in the project \
           merge as the whole.\n"
          err;
        not_contains ~msg:"never in the report" ~sub:"verdicts not saved" out;
        equal ~msg:"the canonical file is byte-identical" text saved
          (read_file verdict_path));
    test "a prefix-scoped run still writes: its records are project-true"
      (fun () ->
        let { code; err; saved; _ } = Lazy.force green in
        equal ~msg:"exit code" int 0 code;
        not_contains ~msg:"the scope narrows the mutants, not the tests"
          ~sub:"verdicts not saved" err;
        is_true ~msg:"so the file was written" (saved <> ""));
    test "a prefix-scoped run keeps what its build wrote for other files"
      (fun () ->
        let { saved; _ } = Lazy.force green in
        let verdicts, identity =
          match load_saved saved with
          | Ok loaded -> loaded
          | Error e -> failf "verdict file unreadable: %a" V.pp_error e
        in
        (* A record of a file outside the scope, as a run under another
           prefix of this very build would have left it. *)
        let other =
          {
            V.id =
              {
                M.file = "elsewhere/other.ml";
                line = 1;
                col = 0;
                rewrite = "add";
              };
            before = "a - b";
            after = "a + b";
            verdict = V.Killed;
          }
        in
        let ids () =
          match V.load verdict_path with
          | Error e -> failf "verdict file unreadable: %a" V.pp_error e
          | Ok (t, _) ->
              List.map
                (fun (r : V.record) -> M.id_to_string r.V.id)
                (V.records t)
        in
        let rerun ?identity () =
          V.save ?identity verdict_path (V.add verdicts other);
          let code, _, err = spawn ~args:[ mutate ] [] in
          equal ~msg:"exit code" int 0 code;
          equal ~msg:"stderr" text "" err
        in
        let scoped =
          List.map
            (fun (r : V.record) -> M.id_to_string r.V.id)
            (V.records verdicts)
        in
        rerun ?identity ();
        equal ~msg:"the scope's records, and the other file's kept"
          (list string)
          ("elsewhere/other.ml:1:0:add" :: scoped)
          (ids ());
        rerun
          ?identity:
            (Option.map
               (fun (i : V.identity) ->
                 { i with V.digest = String.make 32 '0' })
               identity)
          ();
        equal ~msg:"another build's file is replaced whole" (list string) scoped
          (ids ());
        rerun ();
        equal ~msg:"and so is a file that names no writer" (list string) scoped
          (ids ()));
    test "every narrowing of the suite writes no verdict file" (fun () ->
        List.iter
          (fun flags ->
            let code, _, err =
              spawn ~args:(mutate :: flags) [ ("MUTATE_FIXTURE", "weak") ]
            in
            let name = String.concat " " flags in
            equal ~msg:(name ^ ": exit code") int 0 code;
            equal ~msg:(name ^ ": saves nothing and says so") text not_saved err)
          [
            [ "-e"; "nothing-matches" ];
            [ "--exclude-tag"; "nothing" ];
            [ "--shard"; "1/1" ];
          ]);
    test "a --failed run writes no verdict file" (fun () ->
        let log = temp_dir () in
        let code, _, _ =
          spawn ~args:[ "-o"; log ]
            [ ("MUTATE_FIXTURE", "flip"); ("MUTATE_FLIP", "1") ]
        in
        equal ~msg:"the run that records a failure" int 1 code;
        let code, out, err =
          spawn
            ~args:[ mutate; "--failed"; "-o"; log ]
            [ ("MUTATE_FIXTURE", "flip") ]
        in
        equal ~msg:"exit code" int 0 code;
        contains ~msg:"the loop ran over the recorded failure"
          ~sub:"reached by the 1 selected test" out;
        equal ~msg:"and saved nothing" text not_saved err);
    test "a run with an active focus writes no verdict file" (fun () ->
        let code, out, err =
          spawn ~args:[ mutate ] [ ("MUTATE_FIXTURE", "focused") ]
        in
        equal ~msg:"exit code" int 0 code;
        contains ~msg:"the loop ran over the focused test"
          ~sub:"reached by the 1 selected test" out;
        equal ~msg:"and saved nothing" text not_saved err);
    test "a verdict file that cannot be written is one sentence, and exit 0"
      (fun () ->
        (* A copy of the suite under a scratch build directory whose
           _mutants is a file. *)
        let root = temp_dir () in
        let exe = Filename.concat root "_build/default/suite_main.exe" in
        Sys.mkdir (Filename.concat root "_build") 0o755;
        Sys.mkdir (Filename.dirname exe) 0o755;
        Out_channel.with_open_bin exe (fun oc ->
            output_string oc (read_file suite_exe));
        Unix.chmod exe 0o755;
        Out_channel.with_open_bin (Filename.concat root "_build/_mutants")
          (fun oc -> output_string oc "a file\n");
        let code, out, err =
          spawn ~exe ~args:[ mutate ] [ ("MUTATE_FIXTURE", "weak") ]
        in
        equal ~msg:"exit code" int 0 code;
        ends_with ~msg:"the report ended as a whole loop's does"
          ~affix:
            "\n\
             mutants: 1 survived of 1 reached by this suite, 3 never reached\n"
          out;
        starts_with ~msg:"the sentence"
          ~affix:"windtrap: could not write the verdict file: " err;
        equal ~msg:"one line" int 1
          (List.length (String.split_on_char '\n' (String.trim err))));
  ]

let crash_tests =
  [
    test "a child that dies without writing a verdict is killed, not survived"
      (fun () ->
        (try Sys.remove verdict_path with Sys_error _ -> ());
        let code, out, err =
          spawn ~args:[ mutate ] [ ("MUTATE_FIXTURE", "crash") ]
        in
        equal ~msg:"the parent survives its child" int 0 code;
        equal ~msg:"stderr" text "" err;
        contains ~msg:"both kills counted, one survivor still reported"
          ~sub:
            "mutants: 1 survived of 3 reached by this suite, 2 killed, 1 never \
             reached\n"
          out;
        match V.load verdict_path with
        | Error e -> failf "verdict file unreadable: %a" V.pp_error e
        | Ok (verdicts, _) ->
            let rendered =
              List.map
                (fun (r : V.record) ->
                  ( M.id_to_string r.V.id,
                    Format.asprintf "%a" pp_verdict r.V.verdict ))
                (V.records verdicts)
            in
            (* A verdict file names no cause, so the assertion is the one
               that matters: the crashing child's mutant is recorded
               killed like the ordinary one, and never as a survivor. A
               false survivor sends the reader to strengthen a test that
               already noticed. *)
            equal ~msg:"both kills are in the file" int 2
              (List.length (List.filter (fun (_, v) -> v = "killed") rendered));
            equal ~msg:"and nothing else claims a kill" int 1
              (List.length
                 (List.filter
                    (fun (_, v) -> String.starts_with ~prefix:"survived" v)
                    rendered)));
  ]

(* What the dry run and the children do with the run's own settings.

   [baseline]'s one test pins [sub]'s answer against a file under
   WINDTRAP_PROJECT_ROOT that holds a stale answer. Under [--mutate -u]
   the dry run accepts the answer, as a [-u] run does, and the children
   check that accepted file read-only: a child that could update it would
   accept the mutant's answer and let the mutant survive. The run is also
   given [--junit], which a loop's dry run does not honour. *)

type baseline_run = {
  b_code : int;
  b_out : string;
  b_err : string;
  b_file : string;  (** The baseline, as the run left it. *)
  b_others : string list;  (** What else the run left beside it. *)
}

let baseline_run ~args ~stale =
  let root = temp_dir () in
  let file = Filename.concat root "sub.expected" in
  Out_channel.with_open_bin file (fun oc -> output_string oc stale);
  let code, out, err =
    spawn
      ~args:(args @ [ "--junit"; Filename.concat root "junit.xml" ])
      [ ("MUTATE_FIXTURE", "baseline"); ("WINDTRAP_PROJECT_ROOT", root) ]
  in
  {
    b_code = code;
    b_out = out;
    b_err = err;
    b_file = read_file file;
    b_others =
      List.filter
        (fun name -> name <> "sub.expected")
        (List.sort compare (Array.to_list (Sys.readdir root)));
  }

let baseline_loop = lazy (baseline_run ~args:[ mutate; "-u" ] ~stale:"5\n")

let dry_run_tests =
  [
    test "the dry run of --mutate -u accepts what -u accepts" (fun () ->
        let r = Lazy.force baseline_loop in
        equal ~msg:"the loop ran whole, not refused as red" int 0 r.b_code;
        equal ~msg:"stderr" text "" r.b_err;
        contains ~msg:"the dry run accepted the stale baseline"
          ~sub:"  accepted sub.expected\n" r.b_out;
        equal ~msg:"and wrote the answer it saw" text "6\n" r.b_file);
    test "children check the baselines read-only" (fun () ->
        let r = Lazy.force baseline_loop in
        contains ~msg:"the mutant's answer is a mismatch, so a kill"
          ~sub:"mutants: 1 reached by this suite, 1 killed, 3 never reached\n"
          r.b_out;
        equal ~msg:"the baseline keeps the unmutated answer" text "6\n" r.b_file);
    test "a --mutate run writes no JUnit file" (fun () ->
        let r = Lazy.force baseline_loop in
        equal ~msg:"nothing beside the baseline" (list string) [] r.b_others);
    test "a failure in a fixture release kills the mutant" (fun () ->
        (* The test pins nothing about [sub]; its fixture's release, at
           the end of the child's run, sees the change and fails. *)
        let code, out, err =
          spawn ~args:[ mutate ] [ ("MUTATE_FIXTURE", "release") ]
        in
        equal ~msg:"exit code" int 0 code;
        contains ~msg:"the release's failure is the kill"
          ~sub:"mutants: 1 reached by this suite, 1 killed, 3 never reached\n"
          out;
        (* Every child's release prints on both descriptors, the probe's
           included, and neither reaches the loop's. *)
        not_contains ~msg:"no child's stdout" ~sub:"a child's release" out;
        equal ~msg:"no child's stderr" text "" err);
    test "the runaway budget is eight evaluations per measured one, plus 1000"
      (fun () ->
        (* [budget] evaluates [sub] [k] times unarmed, so the child's
           budget is [(k * 8) + 1000]; armed it evaluates exactly the
           budget, or one more. Two values of [k] pin the formula. *)
        List.iter
          (fun (k, over) ->
            let target = (k * 8) + 1000 + over in
            let _, out, _ =
              spawn ~args:[ mutate ]
                [
                  ("MUTATE_FIXTURE", "budget");
                  ("MUTATE_BUDGET", Printf.sprintf "%d %d" k target);
                ]
            in
            contains
              ~msg:(Printf.sprintf "%d evaluations measured, %d armed" k target)
              ~sub:
                (if over = 0 then
                   "mutants: 1 survived of 1 reached by this suite, 3 never \
                    reached\n"
                 else
                   "mutants: 1 reached by this suite, 1 killed, 3 never reached\n")
              out)
          [ (5, 0); (5, 1); (50, 0); (50, 1) ]);
  ]

let refusal_tests =
  [
    test "a red dry run refuses to score anything" (fun () ->
        let code, _, err =
          spawn ~args:[ mutate ] [ ("MUTATE_FIXTURE", "red") ]
        in
        equal ~msg:"exit code" int 1 code;
        contains ~msg:"the reason" ~sub:"the dry run is red" err;
        not_contains ~msg:"no report" ~sub:"mutants: " err);
    test "a suite that does not agree with its own re-run is refused, by name"
      (fun () ->
        (* The probe's whole job. The fixture passes in the process that
           measured the reach map and fails in every fork of it, so the
           disagreement is the one the loop would otherwise blame on the
           mutants. *)
        let code, out, err =
          spawn ~args:[ mutate ] [ ("MUTATE_FIXTURE", "flaky") ]
        in
        equal ~msg:"exit code" int 1 code;
        contains ~msg:"the finding" ~sub:"the suite is not deterministic" err;
        contains ~msg:"the dry run's numbers"
          ~sub:"the dry run executed 4 tests, skipping 0 and failing none" err;
        contains ~msg:"the probe's"
          ~sub:"the probe executed 4, skipping 0 and failing 1" err;
        contains ~msg:"and the test that disagreed, by name"
          ~sub:"flaky \u{203a} passes where it was measured" err;
        not_contains ~msg:"no number was produced" ~sub:"mutants: " out);
    test "a re-run that only skips differently is refused on the skip count"
      (fun () ->
        let code, out, err =
          spawn ~args:[ mutate ] [ ("MUTATE_FIXTURE", "skippy") ]
        in
        equal ~msg:"exit code" int 1 code;
        equal ~msg:"both runs' counts, then why no number follows" text
          "windtrap: the suite is not deterministic: the dry run executed 4 \
           tests, skipping 0 and failing none; the probe executed 4, skipping \
           1 and failing 0. Mutation results over a non-deterministic suite \
           are not a weaker number, they are not a number\n"
          err;
        not_contains ~msg:"no number was produced" ~sub:"mutants: " out);
    test "a suite that spawned a domain is refused, since the loop cannot fork"
      (fun () ->
        let code, out, err =
          spawn ~args:[ mutate ] [ ("MUTATE_FIXTURE", "domain") ]
        in
        equal ~msg:"exit code" int 1 code;
        equal ~msg:"the dry run's transcript, and no mutation report" text
          "calc: 1 passed in <time>.\n" (masked out);
        equal ~msg:"why, in one sentence" text
          "windtrap: this process has spawned a domain, and OCaml refuses \
           Unix.fork in a process that has: mutation testing runs every mutant \
           in a forked child, so it cannot run in this one. Exclude the tests \
           that spawn a domain (-e) to test the rest\n"
          err);
    test "a selection that matched nothing is refused, never scored" (fun () ->
        let code, out, err = spawn ~args:[ mutate; "-f"; "no-such-test" ] [] in
        (* Never 2: "nothing ran" is a statement about a test selection
           and a mutation run does not make one. *)
        equal ~msg:"exit code" int 1 code;
        contains ~msg:"the reason" ~sub:"nothing to mutate" err;
        not_contains ~msg:"and no number was produced" ~sub:"mutants: " out);
    test "WINDTRAP_MUTATE over a selection that keeps no test runs the suite"
      (fun () ->
        (* Nothing is refused: the suite runs as it would without the
           variable, and the source of the selection decides the code. *)
        let code, out, err =
          spawn
            [ ("WINDTRAP_MUTATE", "1"); ("WINDTRAP_FILTER", "no-such-test") ]
        in
        equal ~msg:"a mirror's selection: exit code" int 0 code;
        contains ~msg:"the selection's sentence" ~sub:"no tests ran: filter" out;
        equal ~msg:"and no refusal" text "" err;
        let code, _, err =
          spawn ~args:[ "-f"; "no-such-test" ] [ ("WINDTRAP_MUTATE", "1") ]
        in
        equal ~msg:"a typed selection: exit code" int 2 code;
        equal ~msg:"and no refusal either" text "" err);
    test "a dry run refused at startup is exit 1, whatever its own code"
      (fun () ->
        (* [--failed] with no recorded failure is a startup refusal whose
           own code is 2, since nothing ran. *)
        let code, out, err =
          spawn
            ~args:[ mutate; "--failed"; "-o"; temp_dir () ]
            [ ("MUTATE_FIXTURE", "weak") ]
        in
        equal ~msg:"exit code" int 1 code;
        equal ~msg:"nothing on stdout" text "" out;
        equal ~msg:"the runner's own reason, and no other" text
          "windtrap: no recorded failures match the current suite\n" err);
    test "the refusals are tried in their order" (fun () ->
        (* A scope that leaves no mutant is tried after the dry run's exit
           code, so a red suite and an empty selection are refused as
           such under it. *)
        let code, _, err =
          spawn
            ~args:[ "--mutate=::no-such-source::" ]
            [ ("MUTATE_FIXTURE", "red") ]
        in
        equal ~msg:"red: exit code" int 1 code;
        equal ~msg:"red: the dry run's refusal" text
          "windtrap: the dry run is red. Mutation scores a passing suite; a \
           score over a failing one is not a score\n"
          err;
        let code, _, err =
          spawn ~args:[ "--mutate=::no-such-source::"; "-f"; "no-such-test" ] []
        in
        equal ~msg:"nothing ran: exit code" int 1 code;
        equal ~msg:"nothing ran: that refusal" text
          "windtrap: no test ran, so there is nothing to mutate\n" err);
    test "a scratch directory that cannot be made is refused before the probe"
      (fun () ->
        let not_a_directory = temp_file () in
        let code, out, err =
          spawn ~args:[ mutate ]
            [ ("MUTATE_FIXTURE", "weak"); ("TMPDIR", not_a_directory) ]
        in
        equal ~msg:"exit code" int 1 code;
        equal ~msg:"the dry run's transcript, and no mutation report" text
          "calc: 2 passed in <time>.\n" (masked out);
        starts_with ~msg:"the reason"
          ~affix:"windtrap: could not create the loop's scratch directory: " err);
    test "a truthy WINDTRAP_MUTATE is the bare flag" (fun () ->
        (* [1] is what a CI recipe sets for a whole tree at once: every
           mutant this executable catalogues. Under --instrument-with
           that includes the core's thousands, which is an afternoon,
           not a test; under a plain core the catalogue is the fixture's
           alone, and the bare flag must reach exactly what the scope
           does. *)
        if core_instrumented then
          skip
            ~reason:
              "under --instrument-with the core is instrumented, so the bare \
               flag would survey its mutants too"
            ();
        let code, out, _ = spawn [ ("WINDTRAP_MUTATE", "1") ] in
        equal ~msg:"exit code" int 0 code;
        contains ~msg:"the whole fixture catalogue, as the scope reaches it"
          ~sub:
            "mutants: 1 survived of 2 reached by this suite, 1 killed, 2 never \
             reached\n"
          out);
    test "a falsy WINDTRAP_MUTATE is an ordinary run" (fun () ->
        (* The mirror reads the boolean vocabulary first: [off] asks for
           nothing, exactly as unset does, so a CI recipe can turn the
           loop off without unsetting anything. *)
        let code, out, err = spawn [ ("WINDTRAP_MUTATE", "off") ] in
        equal ~msg:"exit code" int 0 code;
        contains ~msg:"the ordinary transcript" ~sub:"calc: 6 passed" out;
        not_contains ~msg:"no mutation line" ~sub:"mutants:" out;
        equal ~msg:"stderr" string "" err);
    test
      "an armed identifier stale within a file this build catalogues is refused"
      (fun () ->
        (* The other side of the leniency below: the executable WAS built
           from subject.ml, so an identifier naming a position no site of
           it occupies is wrong or stale, and running green on it is how a
           silently ignored arming becomes a false survivor. *)
        let code, out, err = spawn ~args:[ "--arm"; stale_id () ] [] in
        equal ~msg:"exit code" int 1 code;
        contains ~msg:"the identifier" ~sub:(stale_id ()) err;
        contains ~msg:"the diagnosis" ~sub:"no such mutation site" err;
        contains ~msg:"the file's real sites are named"
          ~sub:(mutant_named "add") err;
        not_contains ~msg:"and the suite did not run" ~sub:"calc: " out);
  ]

(* [baseline]'s file holds the unmutated answer, and [sub]'s mutant is
   armed under [-u]. *)
let armed_baseline =
  lazy (baseline_run ~args:[ "--arm"; mutant_named "add"; "-u" ] ~stale:"6\n")

let armed_tests =
  [
    test
      "an armed run announces the mutant before any other output and reports \
       the kill" (fun () ->
        let code, out, _ = spawn ~args:[ "--arm"; mutant_named "add" ] [] in
        equal ~msg:"the mutant made a test fail" int 1 code;
        let first = List.hd (String.split_on_char '\n' out) in
        equal ~msg:"the announcement is the first line" string
          ("mutant " ^ mutant_named "add" ^ " armed: a - b \u{2192} a + b")
          first;
        contains ~msg:"the failure block's title says a mutant is armed"
          ~sub:" (mutant armed)\n" out;
        contains ~msg:"and the block ends on its facts"
          ~sub:"    expected  6\n    actual    14\n\n  FAIL  " out;
        not_contains ~msg:"no block offers a rerun" ~sub:"rerun:" out;
        contains ~msg:"the closing line" ~sub:"mutant killed." out;
        not_contains ~msg:"a kill is the whole verdict" ~sub:"mutant survived"
          out;
        not_contains ~msg:"and the site was plainly evaluated"
          ~sub:"mutant not evaluated" out);
    test "an armed run whose selection matched nothing claims no kill"
      (fun () ->
        (* [mutant killed.] is a verdict, and a verdict is never an exit
           code: a filter that matched nothing exits 2, which
           says something about the filter and nothing about the
           mutant, so neither of the other closing lines may print
           either. *)
        let code, out, _ =
          spawn ~args:[ "--arm"; mutant_named "add"; "-f"; "no-such-test" ] []
        in
        equal ~msg:"the runner's own 'nothing ran' code" int 2 code;
        contains ~msg:"the mutant was still announced" ~sub:" armed: " out;
        not_contains ~msg:"but nothing died" ~sub:"mutant killed." out;
        not_contains ~msg:"no survivor claim over a run that made none"
          ~sub:"mutant survived" out;
        not_contains ~msg:"and no not-evaluated claim either"
          ~sub:"mutant not evaluated" out);
    test "an armed mutant that survives says so, with the evaluation count"
      (fun () ->
        (* Both weak tests run the armed line once each, so the count is a
           claim: a closing line that miscounted, or that printed on a
           run that never evaluated the site, fails here. *)
        let code, out, _ =
          spawn
            ~args:[ "--arm"; List.nth (Lazy.force catalogue) 1 ]
            [ ("MUTATE_FIXTURE", "weak") ]
        in
        equal ~msg:"exit code" int 0 code;
        contains ~msg:"the announcement" ~sub:"armed: a + b \u{2192} a - b" out;
        contains ~msg:"the suite still passed" ~sub:"calc: 2 passed" out;
        contains ~msg:"the closing line disambiguates the green"
          ~sub:
            "mutant survived: the armed site was evaluated 2 times and no test \
             failed.\n"
          out;
        not_contains ~msg:"nothing killed" ~sub:"mutant killed." out);
    test "a site evaluated once is evaluated 1 time" (fun () ->
        let code, out, _ =
          spawn
            ~args:
              [
                "--arm";
                List.nth (Lazy.force catalogue) 1;
                "-f";
                "widen is nonzero";
              ]
            [ ("MUTATE_FIXTURE", "weak") ]
        in
        equal ~msg:"exit code" int 0 code;
        contains ~msg:"the one selected test ran the line once"
          ~sub:
            "mutant survived: the armed site was evaluated 1 time and no test \
             failed.\n"
          out);
    test "an armed run whose selection never ran the site says so" (fun () ->
        (* The other green: the suite passed and proved nothing, because
           the selection deselected every test that reaches the line. The
           two endings are what make an armed run's green readable at
           all; without them this transcript and the survivor's are the
           same bytes. *)
        let code, out, _ =
          spawn
            ~args:[ "--arm"; List.nth (Lazy.force catalogue) 1; "-f"; "calc" ]
            []
        in
        equal ~msg:"the selected tests passed" int 0 code;
        contains ~msg:"the mutant was announced"
          ~sub:"armed: a + b \u{2192} a - b" out;
        contains ~msg:"the closing line blames the selection"
          ~sub:"mutant not evaluated: no selected test ran the site." out;
        not_contains ~msg:"no kill" ~sub:"mutant killed." out;
        not_contains ~msg:"and no survivor claim" ~sub:"mutant survived" out);
    test "a site evaluated only before arming counts as not evaluated"
      (fun () ->
        (* [boundary]'s module initialization evaluates [orphan] before
           anything is armed, so that window ran the ORIGINAL expression:
           billing it to the run would print a survivor count over
           evaluations the mutant never saw. The closing count starts at
           the arming. *)
        let code, out, _ =
          spawn
            ~args:[ "--arm"; List.nth (Lazy.force catalogue) 2 ]
            [ ("MUTATE_FIXTURE", "boundary") ]
        in
        equal ~msg:"the suite passed" int 0 code;
        contains ~msg:"the mutant was announced"
          ~sub:"armed: a + b \u{2192} a - b" out;
        contains ~msg:"and the module-load window is not billed to the run"
          ~sub:"mutant not evaluated: no selected test ran the site." out;
        not_contains ~msg:"no survivor claim over an unarmed window"
          ~sub:"mutant survived" out);
    test "an armed run under -u records no correction" (fun () ->
        let r = Lazy.force armed_baseline in
        equal ~msg:"the mismatch is a failure, not an accepted baseline" int 1
          r.b_code;
        contains ~msg:"so the mutant is killed" ~sub:"mutant killed.\n" r.b_out;
        not_contains ~msg:"and nothing is accepted" ~sub:"accepted" r.b_out;
        equal ~msg:"the baseline keeps the unmutated answer" text "6\n" r.b_file);
    test "an armed run writes its JUnit file" (fun () ->
        let r = Lazy.force armed_baseline in
        equal ~msg:"beside the baseline" (list string) [ "junit.xml" ]
          r.b_others);
  ]

(* Guarantee 12's read-only clause, through the runner it exists for.

   The inline runtime records its correction directory at module load
   ([Sys.getcwd ()]) and, for a recorded source [f], re-reads
   [<dir>/<basename f>] and writes [<dir>/<basename f>.corrected] (see
   [Ppx_runtime.absolute_path] and [flush_corrections_report]). So the
   child is started in a directory that carries a copy of the fixture at
   exactly the name the writer will open: with read-only checking removed
   this scenario writes [inline_armed.ml.corrected] into the staging
   directory and the assertion below fails on the file. Staging it
   anywhere else would make the write fail with ENOENT. The test would
   still go red, but on a missing file rather than on the law. *)

let inline_exe =
  Filename.concat (Filename.concat exe_dir "inline") "runner_main.exe"

let staged_source_dir () =
  let root = temp_dir () in
  let contents =
    read_file
      (Filename.concat exe_dir (Filename.concat "inline" "inline_armed.ml"))
  in
  if contents = "" then fail "the inline fixture source was not found";
  Out_channel.with_open_bin (Filename.concat root "inline_armed.ml") (fun oc ->
      output_string oc contents);
  root

(* The two halves of the read-only clause fail independently (the recorders write no
   correction, and the exit protocol reports no failure as covered by one),
   so they are two tests: whichever half regresses, the report names
   it. *)
let armed_inline () =
  let cwd = staged_source_dir () in
  let code, out, err =
    spawn ~exe:inline_exe
      ~args:[ "inline-test-runner"; "inline_armed" ]
      ~cwd
      [ ("WINDTRAP_MUTATE_ARM", List.nth (Lazy.force catalogue) 1) ]
  in
  (cwd, code, out, err)

let read_only_tests =
  [
    test
      "an armed inline partition writes no .corrected where a write would have \
       succeeded" (fun () ->
        let cwd, _, _, err = armed_inline () in
        is_true
          ~msg:
            "the fixture is staged under the name the correction writer opens, \
             so a write here could not fail on a missing file"
          (Sys.file_exists (Filename.concat cwd "inline_armed.ml"));
        is_false ~msg:"and nothing was written"
          (Sys.file_exists (Filename.concat cwd "inline_armed.ml.corrected"));
        not_contains ~msg:"no promotion notice" ~sub:"wrote" err;
        not_contains ~msg:"and none was even attempted" ~sub:"could not write"
          err);
    test
      "an armed inline mismatch is a plain failure, not a promotion-covered \
       pass" (fun () ->
        let _, code, out, _ = armed_inline () in
        (* Exit 0 here would be the correction-coverage downgrade firing on
           output an armed mutant produced on purpose: dune would record
           the partition as passed and offer a promotion. *)
        equal ~msg:"a mismatch is a plain failure" int 1 code;
        contains ~msg:"announced first" ~sub:"armed: a + b \u{2192} a - b" out;
        contains ~msg:"the mismatch is reported as a mismatch"
          ~sub:"expect: mismatch" out;
        contains ~msg:"with the literal on one side" ~sub:"- 7" out;
        contains ~msg:"against the mutated output" ~sub:"+ -1" out;
        not_contains ~msg:"and not as a merged-history CR"
          ~sub:"ran multiple times" out;
        contains ~msg:"the kill closes the loop" ~sub:"mutant killed." out);
    test
      "a loop under the inline runner kills through its children and rewrites \
       nothing" (fun () ->
        (* The whole seam through the inline runner's [Windtrap.run]: the
           loop takes the process over at run entry, so no correction is
           ever written, and armed checking has to be read-only in a forked child
           rather than in an interactive armed run. *)
        let cwd = staged_source_dir () in
        let code, out, err =
          spawn ~exe:inline_exe
            ~args:[ "inline-test-runner"; "inline_armed" ]
            ~cwd [ mutate_mirror ]
        in
        equal ~msg:"the loop completed" int 0 code;
        equal ~msg:"stderr" text "" err;
        contains ~msg:"the dry run is the partition's ordinary transcript"
          ~sub:"inline_armed: 1 passed" out;
        contains ~msg:"the child's mismatch killed the mutant"
          ~sub:"mutants: 1 reached by this suite, 1 killed, 3 never reached\n"
          out;
        is_false ~msg:"no child wrote a correction"
          (Sys.file_exists (Filename.concat cwd "inline_armed.ml.corrected"));
        not_contains ~msg:"and none was attempted" ~sub:"correction for" err);
    test "the same partition is green and silent unarmed" (fun () ->
        let cwd = staged_source_dir () in
        let code, out, err =
          spawn ~exe:inline_exe
            ~args:[ "inline-test-runner"; "inline_armed" ]
            ~cwd []
        in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"stderr" text "" err;
        not_contains ~msg:"nothing armed" ~sub:" armed: " out;
        is_false ~msg:"no .corrected"
          (Sys.file_exists (Filename.concat cwd "inline_armed.ml.corrected")));
  ]

(* The runaway hit-count budget, end to end.

   [runaway_main.exe]'s one mutant turns a terminating loop into a
   non-terminating one, and the budget must stop it BEFORE the per-child
   deadline does: the guard counts hits in microseconds where the
   deadline waits out its one-second floor. A verdict file names no
   cause, so the CLOCK is the assertion: the whole run (dry run, probe
   and one child) measures 0.06 s here, while a deadline kill would add
   the child's full one-second floor on top. A run that finishes inside
   that floor cannot have been ended by it, so the floor is the bound: the
   widest one that still tells the two kills apart. *)

let runaway_exe =
  Filename.concat (Filename.concat exe_dir "runaway") "runaway_main.exe"

let runaway_tests =
  [
    test "a mutant that would never terminate is killed by its hit budget"
      (fun () ->
        let path = V.output_file ~exe:runaway_exe in
        (try Sys.remove path with Sys_error _ -> ());
        let started = Unix.gettimeofday () in
        let code, out, err = spawn ~exe:runaway_exe ~args:[ mutate ] [] in
        let elapsed = Unix.gettimeofday () -. started in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"stderr" text "" err;
        contains ~msg:"the mutant died"
          ~sub:"mutants: 1 reached by this suite, 1 killed\n" out;
        is_true
          ~msg:
            (Printf.sprintf
               "the guard cut it short, inside the child's own 1s deadline \
                floor (%.2fs)"
               elapsed)
          (elapsed < 1.0);
        match V.load path with
        | Error e -> failf "verdict file unreadable: %a" V.pp_error e
        | Ok (verdicts, _) ->
            equal ~msg:"and the mutant is killed" (list string) [ "killed" ]
              (List.map
                 (fun (r : V.record) ->
                   Format.asprintf "%a" pp_verdict r.V.verdict)
                 (V.records verdicts)));
  ]

(* The per-child deadline, end to end.

   Every deadline below is the loop's own derivation (the fixture
   suite's measured dry-run wall clock plus ten times the scheduled
   tests' measured timings, floored at one second), so a loaded machine
   that slows the tests slows the budget with them: no scenario races an
   absolute sleep against an absolute deadline. It is the only clock over
   a child, and over a run there is none. *)

(* Waits for a run that must end on its own, and ends it when it has not
   within [seconds]: a run that the deadline under test failed to stop is
   a failure of the test, never a hang of the suite. Ten seconds is ten
   times what the one-second floor makes these runs take. The run is
   stopped by SIGTERM first, which a loop answers by killing its running
   child's group, so that no blocked child outlives the failure. *)
let finish_within ?(seconds = 10.) ~what started =
  let rec wait_until time =
    match Unix.waitpid [ Unix.WNOHANG ] started.pid with
    | 0, _ when Unix.gettimeofday () > time -> None
    | 0, _ ->
        Unix.sleepf 0.01;
        wait_until time
    | _, status -> Some status
  in
  match wait_until (Unix.gettimeofday () +. seconds) with
  | Some status ->
      (status, read_file started.out_path, read_file started.err_path)
  | None ->
      (try Unix.kill started.pid Sys.sigterm with Unix.Unix_error _ -> ());
      if wait_until (Unix.gettimeofday () +. 5.) = None then begin
        (try Unix.kill started.pid Sys.sigkill with Unix.Unix_error _ -> ());
        ignore (Unix.waitpid [] started.pid)
      end;
      failf "%s: the run was still going after %.0fs" what seconds

let exit_code = function
  | Unix.WEXITED code -> code
  | Unix.WSIGNALED signal -> failf "the run died of signal %d" signal
  | Unix.WSTOPPED signal -> failf "the run stopped on signal %d" signal

let rendered_verdicts path =
  match V.load path with
  | Error e -> failf "verdict file unreadable: %a" V.pp_error e
  | Ok (verdicts, _) ->
      List.map
        (fun (r : V.record) ->
          (M.id_to_string r.V.id, Format.asprintf "%a" pp_verdict r.V.verdict))
        (V.records verdicts)

(* One run of [block], with its grandchild, serves both claims about a
   blocked child: the deadline's kill and the death of its whole process
   group. The verdict file and the grandchild's pid are read at once. *)
let blocked =
  lazy
    ((try Sys.remove verdict_path with Sys_error _ -> ());
     let pidfile = Filename.concat (temp_dir ()) "grandchild" in
     let started = Unix.gettimeofday () in
     let status, out, err =
       finish_within ~what:"the blocked child's deadline"
         (start ~args:[ mutate ]
            [
              ("MUTATE_FIXTURE", "block"); ("MUTATE_GRANDCHILD_PIDFILE", pidfile);
            ])
     in
     let elapsed = Unix.gettimeofday () -. started in
     let saved = read_file verdict_path in
     (status, out, err, saved, read_file pidfile, elapsed))

let deadline_tests =
  [
    test "a mutant that blocks is killed by its child's deadline" (fun () ->
        let status, out, err, saved, _, _ = Lazy.force blocked in
        equal
          ~msg:"the run completes: a blocked child is a score, not a refusal"
          int 0 (exit_code status);
        equal ~msg:"stderr" text "" err;
        contains ~msg:"the kill counted"
          ~sub:"mutants: 1 reached by this suite, 1 killed, 3 never reached\n"
          out;
        match load_saved saved with
        | Error e -> failf "verdict file unreadable: %a" V.pp_error e
        | Ok (verdicts, _) ->
            equal ~msg:"the blocked mutant is killed" (list string) [ "killed" ]
              (List.filter_map
                 (fun (r : V.record) ->
                   if M.id_to_string r.V.id = mutant_named "add" then
                     Some (Format.asprintf "%a" pp_verdict r.V.verdict)
                   else None)
                 (V.records verdicts)));
    test "a child's deadline is at least one second" (fun () ->
        (* The dry run and the probe of [block] take milliseconds, and its
           child blocks until the deadline kills it: the run cannot end
           before the one-second floor. *)
        let status, _, _, _, _, elapsed = Lazy.force blocked in
        equal ~msg:"the run completed" int 0 (exit_code status);
        is_true
          ~msg:(Printf.sprintf "the child was killed no sooner (%.2fs)" elapsed)
          (elapsed >= 1.0));
    test "a slow but finite test is never killed by the clock" (fun () ->
        (* The regression the multiplier guards: the sleep runs armed and
           unarmed alike, so the dry run prices it into the deadline at
           ten times its measured cost. The slow test pins nothing about
           [sub], so its mutant survives unless the clock kills the child:
           the verdict, not a stopwatch, says which. *)
        (try Sys.remove verdict_path with Sys_error _ -> ());
        let code, out, err =
          spawn ~args:[ mutate ] [ ("MUTATE_FIXTURE", "slow") ]
        in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"stderr" text "" err;
        contains ~msg:"the slow child ran to its end and survived"
          ~sub:
            "mutants: 1 survived of 1 reached by this suite, 3 never reached\n"
          out;
        equal ~msg:"and the verdict file says so" (list string)
          [ "survived by slow > sleeps briefly and pins nothing about sub" ]
          (List.filter_map
             (fun (id, v) -> if id = mutant_named "add" then Some v else None)
             (rendered_verdicts verdict_path)));
    test
      "an expired child's process group dies whole: no grandchild outlives the \
       run" (fun () ->
        let status, out, _, _, pidfile, _ = Lazy.force blocked in
        equal ~msg:"the run completed" int 0 (exit_code status);
        contains ~msg:"and scored the blocked mutant"
          ~sub:"mutants: 1 reached by this suite, 1 killed, 3 never reached\n"
          out;
        let pids =
          List.filter_map int_of_string_opt (String.split_on_char '\n' pidfile)
        in
        equal ~msg:"the blocking test recorded its one grandchild" int 1
          (List.length pids);
        let pid = List.hd pids in
        (* The grandchild ignores SIGTERM and respawns its inner sleeps,
           so only the unignorable group SIGKILL explains its death. It
           lands before the parent reaps the child, but init's reap of
           the orphaned grandchild can lag: poll for the pid to vanish
           rather than asserting on one glance. *)
        let rec dead attempts =
          match Unix.kill pid 0 with
          | () ->
              if attempts = 0 then false
              else (
                Unix.sleepf 0.1;
                dead (attempts - 1))
          | exception Unix.Unix_error (Unix.ESRCH, _, _) -> true
          | exception Unix.Unix_error (_, _, _) -> false
        in
        is_true ~msg:"the grandchild did not outlive the run" (dead 100));
    test
      "a probe that blocks is killed by its own deadline and refused as \
       non-determinism" (fun () ->
        (* The probe shares [fork_child], so it shares the deadline; an
           expiry there means something else (nothing is armed, so the
           hang is the suite's own), and the refusal must say so rather
           than score anything. The fixture blocks only on its SECOND
           run, on the marker the dry run leaves. *)
        let marker = Filename.concat (temp_dir ()) "marker" in
        let status, _, err =
          finish_within ~what:"the probe's deadline"
            (start ~args:[ mutate ]
               [
                 ("MUTATE_FIXTURE", "probe_block");
                 ("MUTATE_PROBE_MARKER", marker);
               ])
        in
        equal ~msg:"a refusal, not a score" int 1 (exit_code status);
        contains ~msg:"named as the probe's own deadline"
          ~sub:"the determinism probe exceeded its deadline" err;
        contains ~msg:"and stated as a determinism claim" ~sub:"not a number"
          err);
  ]

(* What a loop has printed while it still runs.

   The [held] fixture's second child says it started and waits for a gate
   this suite opens, so the report is read while that child provably
   runs: the first child has ended, and the loop has not. The margin is
   the second child's deadline, at least a second. *)

let streaming_tests =
  [
    test
      "a survivor's block is printed when its child ends, not when the loop \
       does" (fun () ->
        let dir = temp_dir () in
        let began = Filename.concat dir "began"
        and gate = Filename.concat dir "gate" in
        let run =
          start ~args:[ mutate ]
            [
              ("MUTATE_FIXTURE", "held");
              ("MUTATE_STARTED", began);
              ("MUTATE_GATE", gate);
            ]
        in
        await ~what:"the second child's start" run began;
        let seen = read_file run.out_path in
        close_out (open_out gate);
        let status, out, err = finish run in
        contains ~msg:"the first survivor's block was out while the second ran"
          ~sub:
            ("\n\n─────────────────────── survivors ────────────────────────\n"
           ^ sub_block)
          seen;
        not_contains ~msg:"the second survivor's was not"
          ~sub:(mutant_named "sub") seen;
        not_contains ~msg:"nor anything that closes the report"
          ~sub:"──────────────────────────────────────────────────────────" seen;
        not_contains ~msg:"its outcome least of all" ~sub:"mutants:" seen;
        is_true ~msg:"the run then completed" (status = Unix.WEXITED 0);
        equal ~msg:"stderr" text "" err;
        is_true ~msg:"what was seen is how the report begins"
          (String.starts_with ~prefix:seen out);
        contains
          ~msg:"the second block followed, one blank line under the first"
          ~sub:("\n\n  SURVIVED  " ^ mutant_named "sub")
          out;
        contains ~msg:"and the loop closed on its outcome"
          ~sub:
            "mutants: 2 survived of 2 reached by this suite, 2 never reached\n"
          out);
  ]

(* A reader that goes away, on a real pipe.

   The loop's report goes to a pipe whose read end this suite holds, and
   closes while a child waits at the fixture's gate: what the loop writes
   next finds no reader. Both ends are close-on-exec, so the loop's only
   copy is its standard output. What is at stake is what the loop leaves
   behind: its scratch root, and the verdict file of a loop that ran
   whole. *)

(* The loop keeps its scratch root under TMPDIR, whatever it names it, so
   a run given a TMPDIR of its own leaves that directory empty or has
   left something behind: no name of the loop's is spelled here. *)
let left_nothing ~msg tmpdir =
  equal ~msg (list string) [] (Array.to_list (Sys.readdir tmpdir))

let held_at_the_gate ?exe ?(args = [ mutate ]) fixture =
  let dir = temp_dir () and tmpdir = temp_dir () in
  let began = Filename.concat dir "began"
  and gate = Filename.concat dir "gate" in
  (try Sys.remove verdict_path with Sys_error _ -> ());
  let reader, writer = Unix.pipe ~cloexec:true () in
  let run =
    start ?exe ~args ~stdout:writer
      [
        ("MUTATE_FIXTURE", fixture);
        ("MUTATE_STARTED", began);
        ("MUTATE_GATE", gate);
        ("TMPDIR", tmpdir);
      ]
  in
  await ~what:"the held child's start" run began;
  (run, tmpdir, reader, writer, fun () -> close_out (open_out gate))

let killed_verdicts () =
  match V.load verdict_path with
  | Error e -> failf "verdict file unreadable: %a" V.pp_error e
  | Ok (verdicts, _) ->
      List.length
        (List.filter
           (fun (r : V.record) -> r.V.verdict = V.Killed)
           (V.records verdicts))

let reader_tests =
  [
    test
      "a reader that leaves mid-loop: a silent death by SIGPIPE, and nothing \
       left behind" (fun () ->
        let run, tmpdir, reader, writer, open_gate = held_at_the_gate "held" in
        Unix.close writer;
        Unix.close reader;
        open_gate ();
        let status, _, err = finish run in
        is_true ~msg:"the loop died of the write its reader was not there for"
          (status = Unix.WSIGNALED Sys.sigpipe);
        equal ~msg:"the reader left on purpose: nothing is said" text "" err;
        left_nothing ~msg:"the scratch root is gone" tmpdir;
        is_false ~msg:"and a loop that did not run whole saves no verdicts"
          (Sys.file_exists verdict_path));
    test
      "a reader that leaves a loop which then runs whole: the verdict file is \
       written" (fun () ->
        (* Every mutant is killed, so the one write left when the gate
           opens is the [mutants:] line. *)
        let run, tmpdir, reader, writer, open_gate =
          held_at_the_gate "pinned"
        in
        Unix.close writer;
        Unix.close reader;
        open_gate ();
        let status, _, err = finish run in
        is_true ~msg:"the outcome line found no reader"
          (status = Unix.WSIGNALED Sys.sigpipe);
        equal ~msg:"stderr" text "" err;
        equal ~msg:"the complete run's verdicts were saved first" int 4
          (killed_verdicts ());
        left_nothing ~msg:"the scratch root is gone" tmpdir);
    test
      "started with SIGPIPE ignored, the failed write's Sys_error escapes, \
       after the scratch root is removed" (fun () ->
        let run, tmpdir, reader, writer, open_gate =
          held_at_the_gate ~exe:"/bin/sh"
            ~args:
              [ "-c"; "trap '' PIPE; exec \"$0\" \"$@\""; suite_exe; mutate ]
            "held"
        in
        Unix.close writer;
        Unix.close reader;
        open_gate ();
        let status, _, err = finish run in
        is_true ~msg:"the process ends on the uncaught exception"
          (status = Unix.WEXITED 2);
        contains ~msg:"which is the write's Sys_error" ~sub:"Sys_error" err;
        left_nothing ~msg:"the scratch root is gone" tmpdir;
        is_false ~msg:"and a loop that did not run whole saves no verdicts"
          (Sys.file_exists verdict_path));
    test "a SIGINT late in the run ends it by SIGINT with the verdicts it had"
      (fun () ->
        (* The pipe is filled and never read, so the loop is most likely
           blocked in the write of its [mutants:] line when SIGINT comes,
           a moment after the verdict file appeared; nothing says it is,
           and the claim does not need it: a signal after the write ends
           the run by that signal and costs the file nothing. The flag is
           on the open file the loop writes through as well: it is cleared
           before the loop has anything to write. *)
        let run, tmpdir, reader, writer, open_gate =
          held_at_the_gate "pinned"
        in
        Unix.set_nonblock writer;
        let rec fill chunk =
          match Unix.write writer chunk 0 (Bytes.length chunk) with
          | _ -> fill chunk
          | exception Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _)
            ->
              ()
        in
        fill (Bytes.make 4096 'x');
        fill (Bytes.make 1 'x');
        Unix.clear_nonblock writer;
        Unix.close writer;
        open_gate ();
        let rec saved attempts =
          if Sys.file_exists verdict_path then ()
          else if attempts = 0 then begin
            (try Unix.kill run.pid Sys.sigkill with Unix.Unix_error _ -> ());
            ignore (finish run);
            fail "the verdict file never appeared"
          end
          else begin
            Unix.sleepf 0.01;
            saved (attempts - 1)
          end
        in
        saved 3000;
        Unix.sleepf 0.2;
        Unix.kill run.pid Sys.sigint;
        let status, _, err = finish run in
        Unix.close reader;
        is_true ~msg:"the signal ended the blocked report"
          (status = Unix.WSIGNALED Sys.sigint);
        equal ~msg:"stderr" text "" err;
        equal ~msg:"and cost the complete run nothing" int 4
          (killed_verdicts ());
        left_nothing ~msg:"the scratch root is gone" tmpdir);
  ]

(* A loop stopped by a signal, the real one, sent to the process this
   suite started.

   [interrupted]'s second child hangs, in a session of its own with a
   grandchild that ignores SIGTERM: a parent that died of the signal
   alone would leave both behind. The pid file is how the harness knows
   the child is hanging, and whom to look for afterwards. *)

let interrupted_by signal =
  let pidfile = Filename.concat (temp_dir ()) "hanging"
  and tmpdir = temp_dir () in
  (try Sys.remove verdict_path with Sys_error _ -> ());
  let run =
    start ~args:[ mutate ]
      [
        ("MUTATE_FIXTURE", "interrupted");
        ("MUTATE_GRANDCHILD_PIDFILE", pidfile);
        ("TMPDIR", tmpdir);
      ]
  in
  await ~what:"the hanging child" run pidfile;
  Unix.kill run.pid signal;
  let status, out, err = finish run in
  (tmpdir, pidfile, status, out, err)

(* The child's session dies whole, as at a deadline: the grandchild
   ignores SIGTERM, so only the group's SIGKILL explains its death, and
   init's reap of it can lag. *)
let grandchild_died pidfile =
  let grandchild =
    match
      List.filter_map int_of_string_opt
        (String.split_on_char '\n' (read_file pidfile))
    with
    | [ pid ] -> pid
    | pids -> failf "%d grandchildren recorded" (List.length pids)
  in
  let rec dead attempts =
    match Unix.kill grandchild 0 with
    | () ->
        if attempts = 0 then false
        else (
          Unix.sleepf 0.1;
          dead (attempts - 1))
    | exception Unix.Unix_error (Unix.ESRCH, _, _) -> true
    | exception Unix.Unix_error (_, _, _) -> false
  in
  dead 100

let interrupt_tests =
  [
    test
      "a signal stops the loop: what it found, what it did not test, and a \
       death by that signal" (fun () ->
        let tmpdir, pidfile, status, out, err = interrupted_by Sys.sigint in
        is_true ~msg:"the parent died by the signal, as its own parent sees"
          (status = Unix.WSIGNALED Sys.sigint);
        equal ~msg:"standard error names the mutant whose child it stopped" text
          ("windtrap: interrupted while testing " ^ mutant_named "sub" ^ "\n")
          err;
        contains ~msg:"the survivor found before the signal is in the report"
          ~sub:sub_block out;
        is_true
          ~msg:
            "the report closes as a complete one does, over the children that \
             ended, and counts the rest"
          (String.ends_with
             ~suffix:
               ("──────────────────────────────────────────────────────────\n\n\
                 ─────────────────── never reached (1) ────────────────────\n\
                \  1  test/instr/loop/subject.ml   lines 27\n\
                 ──────────────────────────────────────────────────────────\n\n\
                 reproduce: " ^ suite_exe ^ " --arm " ^ mutant_named "add"
              ^ "\n\
                 mutants: 1 survived of 3 reached by this suite, 1 never \
                 reached, 2 not tested\n")
             out);
        not_contains ~msg:"the stopped child's mutant has no verdict"
          ~sub:("SURVIVED  " ^ mutant_named "sub")
          out;
        is_false ~msg:"a stopped run writes no verdict file"
          (Sys.file_exists verdict_path);
        left_nothing ~msg:"and leaves no scratch directory" tmpdir;
        is_true ~msg:"no process of the stopped child outlives the run"
          (grandchild_died pidfile));
    test "SIGPIPE stops it as silently as a failed write does" (fun () ->
        (* Sent from outside while a child hangs: the child dies with the
           others' guarantees, and the reader that left is told nothing. *)
        let tmpdir, pidfile, status, out, err = interrupted_by Sys.sigpipe in
        is_true ~msg:"a death by that signal"
          (status = Unix.WSIGNALED Sys.sigpipe);
        equal ~msg:"nothing is said" text "" err;
        contains ~msg:"what was printed stays" ~sub:sub_block out;
        not_contains ~msg:"and no closing report is tried" ~sub:"mutants:" out;
        is_false ~msg:"no verdict file" (Sys.file_exists verdict_path);
        left_nothing ~msg:"no scratch directory" tmpdir;
        is_true ~msg:"no process of the stopped child outlives the run"
          (grandchild_died pidfile));
    test "a signal during the determinism probe names the probe" (fun () ->
        let marker = Filename.concat (temp_dir ()) "marker"
        and tmpdir = temp_dir () in
        let run =
          start ~args:[ mutate ]
            [
              ("MUTATE_FIXTURE", "probe_block");
              ("MUTATE_PROBE_MARKER", marker);
              ("TMPDIR", tmpdir);
            ]
        in
        await ~what:"the probe's start" run (marker ^ ".probe");
        Unix.kill run.pid Sys.sigint;
        let status, out, err = finish run in
        is_true ~msg:"a death by the signal" (status = Unix.WSIGNALED Sys.sigint);
        equal ~msg:"standard error names the probe" text
          "windtrap: interrupted during the determinism probe\n" err;
        contains ~msg:"and the report closes, the reached mutant not tested"
          ~sub:
            "mutants: 1 reached by this suite, 3 never reached, 1 not tested\n"
          out;
        left_nothing ~msg:"no scratch directory" tmpdir);
    test
      "a signal after the last child ends: the file, the whole report, then a \
       death by it" (fun () ->
        (try Sys.remove verdict_path with Sys_error _ -> ());
        (* What holds the loop in the window: see [late] in suite_main.ml. *)
        Unix.mkfifo verdict_path 0o600;
        let tmpdir = temp_dir () in
        let status, out, err =
          finish
            (start ~args:[ mutate ]
               [ ("MUTATE_FIXTURE", "late"); ("TMPDIR", tmpdir) ])
        in
        is_true ~msg:"the loop died by the signal"
          (status = Unix.WSIGNALED Sys.sigint);
        equal ~msg:"and said nothing of it" text "" err;
        ends_with ~msg:"after its whole report"
          ~affix:
            "mutants: 1 survived of 1 reached by this suite, 3 never reached\n"
          out;
        is_true ~msg:"after the verdict file"
          ((Unix.stat verdict_path).Unix.st_kind = Unix.S_REG);
        left_nothing ~msg:"no scratch directory" tmpdir);
    test "SIGTERM and SIGHUP stop it alike" (fun () ->
        List.iter
          (fun (name, signal) ->
            let _, _, status, out, err = interrupted_by signal in
            is_true
              ~msg:(name ^ ": a death by that signal")
              (status = Unix.WSIGNALED signal);
            equal ~msg:(name ^ ": the same line") text
              ("windtrap: interrupted while testing " ^ mutant_named "sub"
             ^ "\n")
              err;
            contains
              ~msg:(name ^ ": the same outcome")
              ~sub:
                "mutants: 1 survived of 3 reached by this suite, 1 never \
                 reached, 2 not tested\n"
              out)
          [ ("SIGTERM", Sys.sigterm); ("SIGHUP", Sys.sighup) ]);
  ]

(* One identifier, every executable (the report's own remedy)

   A report whose suite is reached through the build (a build action's
   loop, an inline runner's verdicts) tells the reader to arm a survivor
   with [WINDTRAP_MUTATE_ARM=<id> dune runtest …], the flag's mirror,
   because a build has no single binary to name. That runs EVERY
   instrumented executable with the variable set, and
   windtrap's own lib/ is covered by seven. So
   the scenario here is the real one: one identifier handed to two
   executables built from disjoint sources (suite_main from subject.ml,
   runaway_main from spinner.ml), once to the binary that holds the
   mutant and once to a binary that does not. Both must be usable answers
   to one command, or dune fails the build BECAUSE the one binary that
   has the mutant armed it correctly. *)

let cross_executable_tests =
  [
    test "an identifier this executable holds no site of lets it run green"
      (fun () ->
        let id = mutant_named "add" in
        let code, out, err =
          spawn ~exe:runaway_exe [ ("WINDTRAP_MUTATE_ARM", id) ]
        in
        equal ~msg:"exit code (this binary is not the one it is about)" int 0
          code;
        contains ~msg:"the suite ran, ordinarily" ~sub:"spin: 1 passed" out;
        not_contains ~msg:"and armed nothing" ~sub:" armed: " out;
        (* The closing-line trio belongs to a run that armed a mutant; a
           binary that declined made no claim a closing line could
           report. *)
        not_contains ~msg:"so no closing line judges the run"
          ~sub:"mutant not evaluated" out;
        not_contains ~msg:"nor claims a survivor" ~sub:"mutant survived" out;
        contains ~msg:"it says whose mutant it is not"
          ~sub:"not this executable's mutant" err;
        contains ~msg:"naming the identifier it declined" ~sub:id err;
        equal ~msg:"and says it exactly once" int 1
          (List.length (lines_with ~sub:"not this executable's mutant" err)));
    test "the same identifier still arms the executable that does hold it"
      (fun () ->
        (* The pair is the point: one command, one identifier, and the
           binary that catalogues the site does the work while its
           siblings stand down. Asserted beside the decline so that a
           change making every executable decline cannot pass. *)
        let id = mutant_named "add" in
        let code, out, _ = spawn [ ("WINDTRAP_MUTATE_ARM", id) ] in
        equal ~msg:"the mutant made a test fail" int 1 code;
        contains ~msg:"it armed the named mutant"
          ~sub:("mutant " ^ id ^ " armed: a - b \u{2192} a + b")
          out);
  ]

(* The control: no instrumented module in the executable at all, in an
   ordinary build. Under --instrument-with it links an instrumented core
   and is a control for nothing; the one scenario that needs it to
   catalogue nothing says so and steps aside. This executable links the
   same core, so it can tell. *)

let plain_exe = Filename.concat exe_dir "plain_main.exe"

let uninstrumented_tests =
  [
    test "a build with no mutants runs exactly as it would without the seam"
      (fun () ->
        let code, out, err = spawn ~exe:plain_exe [] in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"stderr" text "" err;
        contains ~msg:"the ordinary summary" ~sub:"plain: 1 passed" out;
        not_contains ~msg:"and nothing else" ~sub:"mutants:" out);
    test "asking a build with no mutants to mutate declines by name" (fun () ->
        if core_instrumented then
          skip
            ~reason:
              "under --instrument-with the core is instrumented, so plain_main \
               catalogues its mutants"
            ();
        (* The bare flag, so this is the no-scope run: an empty catalogue
           is refused as uninstrumented whatever the scope. *)
        let code, out, err = spawn ~exe:plain_exe ~args:[ "--mutate" ] [] in
        equal ~msg:"exit code" int 1 code;
        contains ~msg:"the suite still ran" ~sub:"plain: 1 passed" out;
        contains ~msg:"the diagnosis" ~sub:"links no instrumented module" err;
        contains ~msg:"the fix names the backend, not a build tool"
          ~sub:"instrument the library under test with ppx_windtrap.mutate" err;
        not_contains ~msg:"no scope was set, so none is blamed" ~sub:"scope" err);
    test "an armed identifier declines by name and leaves the run alone"
      (fun () ->
        (* An uninstrumented executable is the commonest sibling of all:
           under [WINDTRAP_MUTATE_ARM=<id> dune runtest] every (test)
           stanza in the project gets the variable, and most of them
           catalogue nothing. Exiting 1 here would fail the build for the
           one executable that armed the mutant correctly. *)
        let code, out, err =
          spawn ~exe:plain_exe
            [ ("WINDTRAP_MUTATE_ARM", "lib/absent.ml:1:0:add") ]
        in
        equal ~msg:"exit code" int 0 code;
        contains ~msg:"the suite ran exactly as it would unarmed"
          ~sub:"plain: 1 passed" out;
        not_contains ~msg:"nothing armed" ~sub:" armed: " out;
        contains ~msg:"the identifier" ~sub:"lib/absent.ml:1:0:add" err;
        contains ~msg:"the diagnosis" ~sub:"not this executable's mutant" err;
        contains ~msg:"and the misconfiguration it could still be"
          ~sub:"instrumented with ppx_windtrap.mutate" err);
    test "a scope is loud even where there is nothing to mutate" (fun () ->
        (* Whether this binary catalogues nothing or only the core's
           mutants, a prefix matching no file refuses by name, the same
           sentence under a plain and an instrumented core, so the
           missing-backend diagnosis is never what a prefix gets. *)
        let code, _, err =
          spawn ~exe:plain_exe ~args:[ "--mutate=perhaps" ] []
        in
        equal ~msg:"exit code" int 1 code;
        contains ~msg:"the refusal names the scope"
          ~sub:
            "windtrap: --mutate=perhaps leaves no mutant in this executable's \
             catalogue"
          err;
        contains ~msg:"and asks the instrumentation question"
          ~sub:
            "is the library under test instrumented with ppx_windtrap.mutate?"
          err;
        not_contains ~msg:"never the bare flag's diagnosis"
          ~sub:"links no instrumented module" err);
    test "WINDTRAP_MUTATE with no mutant in its scope runs the suite as it is"
      (fun () ->
        (* The variable reaches every test executable of a project, and
           most of them test no mutant: exiting 1 would fail the build
           for the executables it was meant for. *)
        let code, out, err =
          spawn ~exe:plain_exe [ ("WINDTRAP_MUTATE", "perhaps") ]
        in
        equal ~msg:"exit code" int 0 code;
        contains ~msg:"the ordinary summary" ~sub:"plain: 1 passed" out;
        not_contains ~msg:"and no mutation report" ~sub:"mutants:" out;
        equal ~msg:"one sentence on why" text
          "windtrap: WINDTRAP_MUTATE is set, but no mutant of this \
           executable's catalogue is under perhaps, so the suite runs without \
           mutation\n"
          err);
    test "WINDTRAP_MUTATE on a build with no mutants names the backend"
      (fun () ->
        if core_instrumented then
          skip
            ~reason:
              "under --instrument-with the core is instrumented, so plain_main \
               catalogues its mutants"
            ();
        let code, out, err =
          spawn ~exe:plain_exe [ ("WINDTRAP_MUTATE", "1") ]
        in
        equal ~msg:"exit code" int 0 code;
        contains ~msg:"the ordinary summary" ~sub:"plain: 1 passed" out;
        equal ~msg:"one sentence on why" text
          "windtrap: WINDTRAP_MUTATE is set, but this executable links no \
           instrumented module, so the suite runs without mutation\n"
          err);
  ]

(* The fixtures' verdict files land in the real [_build/_mutants], where
   [windtrap mutants] would merge them into the tree's own answer. No
   rule runs these executables but this suite, so the suite removes their
   files when it ends. A forked child ends past [at_exit], so only the
   process that started the scenarios removes them, after all of them. *)
let () =
  at_exit (fun () ->
      List.iter
        (fun exe ->
          try Sys.remove (V.output_file ~exe) with Sys_error _ -> ())
        [ suite_exe; inline_exe; runaway_exe; plain_exe ])

let () =
  exit
  @@ run "mutate loop"
       [
         group "catalogue" catalogue_tests;
         group "unasked" unasked_tests;
         group "scope" scope_tests;
         group "loop" loop_tests;
         group "reach map" reach_tests;
         group "xfail" xfail_tests;
         group "not evaluated" not_evaluated_tests;
         group "dry run and children" dry_run_tests;
         group "no trace outside the pipe" no_trace_tests;
         group "verdict file" verdict_file_tests;
         group "crash" crash_tests;
         group "refusals" refusal_tests;
         group "armed" armed_tests;
         group "one identifier, several executables" cross_executable_tests;
         group "read-only checking" read_only_tests;
         group "runaway budget" runaway_tests;
         group "per-child deadline" deadline_tests;
         group "streaming" streaming_tests;
         group "a reader that leaves" reader_tests;
         group "interruption" interrupt_tests;
         group "uninstrumented" uninstrumented_tests;
       ]
