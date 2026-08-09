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
module M = Windtrap_mutate

let exe_dir = Filename.dirname Sys.executable_name
let suite_exe = Filename.concat exe_dir "suite_main.exe"

(* [lstat], not [Sys.is_directory]: the runner leaves a [latest] symlink
   in every log directory, and following it would delete outside the
   scratch tree. *)
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
  let dir = Filename.temp_file "windtrap_loop_scratch" "" in
  Sys.remove dir;
  Sys.mkdir dir 0o755;
  at_exit (fun () -> remove_tree dir);
  dir

let read_file path =
  match open_in_bin path with
  | exception Sys_error _ -> ""
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () -> really_input_string ic (in_channel_length ic))

(* Nothing of the ambient environment survives except what a process
   genuinely needs to start (PATH, HOME, TMPDIR and the locale). *)
let environment bindings =
  let inherited name =
    match Sys.getenv_opt name with
    | Some value -> [ name ^ "=" ^ value ]
    | None -> []
  in
  Array.of_list
    (List.concat_map inherited [ "PATH"; "HOME"; "TMPDIR"; "LANG"; "LC_ALL" ]
    @ [ "WINDTRAP_COLOR=never"; "WINDTRAP_SLOW_THRESHOLD=0" ]
    @ bindings)

let counter = ref 0

(* [fork] then [chdir] then [execve], rather than [create_process_env]:
   one scenario needs the child to start in a directory of the test's
   choosing, because the inline runtime records its correction directory
   at module load and that is the only way to move it. *)
let spawn ?(exe = suite_exe) ?(args = []) ?cwd bindings =
  incr counter;
  let prefix = Filename.concat scratch_dir (string_of_int !counter) in
  let out_path = prefix ^ ".out" and err_path = prefix ^ ".err" in
  let open_target path =
    Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o644
  in
  let out = open_target out_path and err = open_target err_path in
  let argv = Array.of_list (exe :: args) in
  let environment = environment bindings in
  let code =
    match Unix.fork () with
    | 0 -> (
        try
          (match cwd with Some dir -> Unix.chdir dir | None -> ());
          Unix.dup2 out Unix.stdout;
          Unix.dup2 err Unix.stderr;
          Unix.execve exe argv environment
        with _ -> Unix._exit 127)
    | pid -> (
        Unix.close out;
        Unix.close err;
        match snd (Unix.waitpid [] pid) with
        | Unix.WEXITED code -> code
        | Unix.WSIGNALED signal -> 128 + signal
        | Unix.WSTOPPED _ -> 255)
  in
  (code, read_file out_path, read_file err_path)

let says ~msg text sub = contains ~msg ~sub text

let has_sub text sub =
  let n = String.length text and m = String.length sub in
  let rec go i = i + m <= n && (String.sub text i m = sub || go (i + 1)) in
  m = 0 || go 0

let denies ~msg text sub =
  if has_sub text sub then failf "%s: unexpected %S in:\n%s" msg sub text

(* The catalogue, read out of the binary rather than transcribed: a line
   moving in subject.ml must not silently re-point an armed identifier at
   another site. *)
let catalogue =
  lazy
    (let code, out, err = spawn [ "MUTATE_FIXTURE=catalogue" ] in
     if code <> 0 then failf "catalogue mode exited %d: %s" code err;
     List.filter (fun line -> line <> "") (String.split_on_char '\n' out))

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

(* The [arm] line's variable binding, lifted out of a report exactly as a
   reader would copy it: from [WINDTRAP_MUTATE_ARM=] to the following
   space. Pasting it back is the only test of the hint that can fail when
   the identifier the report prints is not one the runtime resolves. *)
let arm_binding_of report =
  let prefix = M.arm_variable ^ "=" in
  let word line =
    match String.index_opt line ' ' with
    | Some i -> String.sub line 0 i
    | None -> line
  in
  match
    List.find_map
      (fun line ->
        match String.index_opt line '=' with
        | Some _ when has_sub line prefix ->
            let at = ref 0 in
            let n = String.length line and m = String.length prefix in
            while !at + m <= n && String.sub line !at m <> prefix do
              incr at
            done;
            Some (word (String.sub line !at (n - !at)))
        | _ -> None)
      (String.split_on_char '\n' report)
  with
  | Some binding -> binding
  | None -> failf "no %s line in the report:\n%s" prefix report

(* The verdict file this executable writes, deleted before every scenario
   that is meant to produce one so that a stale file cannot pass a test
   the loop failed to write. *)
let verdict_path = M.output_file ~exe:suite_exe

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

let discovery_tests =
  [
    test "an instrumented run that was not asked to mutate says what it found"
      (fun () ->
        let code, out, _ = spawn [] in
        equal ~msg:"exit code" int 0 code;
        says ~msg:"summary" out "calc: 6 passed";
        (* Four, not the catalogue's five: the discovery line offers what
           the loop would test, and the report's denominator must be the
           same number the reader was invited to test. *)
        says ~msg:"discovery line" out
          "mutants: 4 in 1 file \u{00b7} WINDTRAP_MUTATE=1 to test them");
    test "the discovery line stays out of a quiet transcript" (fun () ->
        let code, out, _ = spawn [ "WINDTRAP_QUIET=1" ] in
        equal ~msg:"exit code" int 0 code;
        denies ~msg:"quiet" out "WINDTRAP_MUTATE=1 to test them");
    test "off is off" (fun () ->
        let code, out, _ = spawn [ "WINDTRAP_MUTATE=off" ] in
        equal ~msg:"exit code" int 0 code;
        says ~msg:"still discovers" out "mutants: 4 in 1 file");
  ]

let loop_tests =
  [
    test
      "the loop kills one mutant, names the survivor's witnesses and lists the \
       unreached one" (fun () ->
        (try Sys.remove verdict_path with Sys_error _ -> ());
        let code, out, err = spawn [ "WINDTRAP_MUTATE=1" ] in
        equal ~msg:"exit code (a survivor never fails the build)" int 0 code;
        equal ~msg:"stderr" text "" err;
        says ~msg:"the dry run printed its ordinary summary" out
          "calc: 6 passed";
        says ~msg:"survivor section" out "survivors (1)";
        says ~msg:"survivor head row" out "SURVIVED";
        says ~msg:"the mutated expression" out "a + b  \u{2192}  a - b";
        says ~msg:"the sentence that is the product" out
          "2 tests ran this line and none failed when it changed:";
        says ~msg:"first witness" out "widen \u{203a} widen is nonzero";
        says ~msg:"second witness" out "widen \u{203a} widen is not 99";
        says ~msg:"the dismissal hint" out "((a + b) [@mutate off \"reason\"])";
        says ~msg:"unreached heading" out
          "unreached (2) \u{2014} no test evaluates these";
        says ~msg:"unreached file" out "subject.ml";
        says ~msg:"summary" out "mutants: 1 survived of 4";
        says ~msg:"summary terms" out "1 killed, 2 unreached";
        (* The killed mutant is not a survivor and not unreached. *)
        denies ~msg:"only one block" out "survivors (2)");
    test "a dismissed mutant is in no block, no list and no denominator"
      (fun () ->
        let _, out, _ = spawn [ "WINDTRAP_MUTATE=1" ] in
        (* The [green] suite runs the [@mutate off] site and pins nothing
           about it, so without the dismissal it would be a second
           survivor and a fifth denominator. *)
        says ~msg:"the denominator is the population" out
          "mutants: 1 survived of 4";
        says ~msg:"and it is not counted unreached" out "1 killed, 2 unreached";
        denies ~msg:"no block for the dismissed line" out
          (List.nth (Lazy.force catalogue) 4));
    test "the survivor block quotes the mutated source line" (fun () ->
        let _, out, _ = spawn [ "WINDTRAP_MUTATE=1" ] in
        says ~msg:"excerpt row" out "let widen a b = a + b");
    test "the arm hint spells a variable that arms the mutant it names"
      (fun () ->
        let _, out, _ = spawn [ "WINDTRAP_MUTATE=1"; "INSIDE_DUNE=1" ] in
        says ~msg:"the backend flag, before the target" out
          "dune exec --instrument-with ppx_windtrap.mutate ";
        (* The hint is pasted back rather than pattern-matched: the
           identifier the report prints has to be one the runtime's own
           selector grammar resolves, and the only proof of that is a run
           that announces the same rewrite. *)
        let binding = arm_binding_of out in
        let code, armed, _ = spawn [ binding ] in
        equal ~msg:"the armed run's exit code (this mutant survives)" int 0 code;
        says ~msg:"the pasted line armed the survivor" armed
          "armed: a + b \u{2192} a - b");
    test "report mode runs the loop" (fun () ->
        let code, out, _ = spawn [ "WINDTRAP_MUTATE=report" ] in
        equal ~msg:"exit code" int 0 code;
        says ~msg:"the report" out "mutants: 1 survived of 4");
    test "the survivor cap drops blocks and says how many it dropped" (fun () ->
        let uncapped () =
          let _, out, _ =
            spawn [ "MUTATE_FIXTURE=capped"; "WINDTRAP_MUTATE=1" ]
          in
          out
        in
        let out = uncapped () in
        says ~msg:"both blocks" out "survivors (2)";
        says ~msg:"most-watched first" out
          "SURVIVED  test/mutate_loop/subject.ml:18";
        says ~msg:"then the one-witness survivor" out
          "SURVIVED  test/mutate_loop/subject.ml:21";
        let _, capped, _ =
          spawn
            [
              "MUTATE_FIXTURE=capped";
              "WINDTRAP_MUTATE=1";
              "WINDTRAP_MUTATE_LIMIT=1";
            ]
        in
        says ~msg:"the label says what the reader did not see" capped
          "survivors (1 of 2)";
        says ~msg:"the block kept is the most-watched one" capped
          "SURVIVED  test/mutate_loop/subject.ml:18";
        denies ~msg:"the dropped block is gone" capped
          "SURVIVED  test/mutate_loop/subject.ml:21";
        says ~msg:"the summary still counts both" capped
          "mutants: 2 survived of 4";
        let _, uncapped_by_zero, _ =
          spawn
            [
              "MUTATE_FIXTURE=capped";
              "WINDTRAP_MUTATE=1";
              "WINDTRAP_MUTATE_LIMIT=0";
            ]
        in
        says ~msg:"0 means every block" uncapped_by_zero "survivors (2)");
  ]

(* The reach map's boundaries. Every claim here has a wrong answer the
   implementation could give, and the [boundary] fixture is built so that
   each wrong answer changes a number this test reads. *)
let reach_tests =
  [
    test
      "module initialization, a retry and a fixture release each land on the \
       right side of a test boundary" (fun () ->
        let code, out, err =
          spawn [ "MUTATE_FIXTURE=boundary"; "WINDTRAP_MUTATE=1" ]
        in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"stderr" text "" err;
        (* [orphan] is evaluated at module load and [crasher] by a fixture
           release: both are outside every test, and folding either into
           the test on whose side of the boundary it sits would make it a
           permanent false survivor. *)
        says ~msg:"both out-of-test sites stay unreached" out
          "unreached (2) \u{2014} no test evaluates these";
        says ~msg:"orphan and crasher, by line" out
          "test/mutate_loop/subject.ml   21, 27";
        says ~msg:"exactly one survivor" out "survivors (1)";
        (* The witness list is the whole product of the run: the first and
           third tests reach the line, the second, fourth and fifth do
           not, and a retried first test contributes one window. *)
        says ~msg:"the count" out
          "2 tests ran this line and none failed when it changed:";
        says ~msg:"the first test" out
          "widen \u{203a} first reaches widen, after a retry";
        says ~msg:"the third test" out "widen \u{203a} third reaches widen";
        denies ~msg:"not the second" out "widen \u{203a} second reaches sub";
        denies ~msg:"not the fourth" out "widen \u{203a} fourth reaches sub";
        denies ~msg:"not the fifth" out "widen \u{203a} fifth reaches sub";
        says ~msg:"summary" out "mutants: 1 survived of 4");
    test "a tag the default predicate drops still selects the child's tests"
      (fun () ->
        (* [--tag disabled] is the one selection a pruned tree cannot
           express: tags are not in a path. A child that dropped the
           parent's tag predicate runs nothing, and the determinism probe
           reports a deterministic suite as non-deterministic. *)
        let code, out, err =
          spawn
            [
              "MUTATE_FIXTURE=tagged";
              "WINDTRAP_TAG=disabled";
              "WINDTRAP_MUTATE=1";
            ]
        in
        equal ~msg:"exit code" int 0 code;
        denies ~msg:"the probe agreed" err "not deterministic";
        says ~msg:"the dry run's five tests" out "calc: 5 passed";
        says ~msg:"and the loop scored them" out "mutants: 1 survived of 4";
        says ~msg:"over the same witnesses as the untagged suite" out
          "2 tests ran this line and none failed when it changed:");
  ]

(* Law 16(e): a mutation child leaves through [Unix._exit] and nothing
   else, so no [at_exit] handler of the parent's image ever runs in one —
   which is what stops a crashing child overwriting the parent's
   [.coverage] dump, since that dump IS an at_exit handler. The witness
   generalizes it: the fixture appends its pid on every path through
   Stdlib's exit machinery, and after a loop that forked five children the
   file must name the parent once.

   The [fatal] fixture is the case that makes it bite: [Stack_overflow] is
   fatal, so no failure boundary in the runner may swallow it, it escapes
   [Runner.execute], and the child's own wrapper is the only thing between
   it and OCaml's uncaught-exception handler — which runs [at_exit] before
   it prints. Remove that wrapper's catch-all and this file gains a
   line. *)
let no_trace_tests =
  [
    test "no at_exit handler runs in a mutation child, fatal exception included"
      (fun () ->
        incr counter;
        let log =
          Filename.concat scratch_dir ("atexit" ^ string_of_int !counter)
        in
        let code, out, err =
          spawn
            [
              "MUTATE_FIXTURE=fatal";
              "WINDTRAP_MUTATE=1";
              "MUTATE_ATEXIT_LOG=" ^ log;
            ]
        in
        equal ~msg:"the parent completed" int 0 code;
        equal ~msg:"stderr" text "" err;
        says ~msg:"the fatal child scored as a crash kill" out "2 killed";
        says ~msg:"and the report is otherwise whole" out
          "mutants: 1 survived of 4";
        let lines =
          List.filter
            (fun l -> l <> "")
            (String.split_on_char '\n' (read_file log))
        in
        equal ~msg:"exactly one process reached at_exit — the parent" int 1
          (List.length lines));
  ]

let verdict_file_tests =
  [
    test "the loop writes a verdict file the runtime can read back" (fun () ->
        (try Sys.remove verdict_path with Sys_error _ -> ());
        let code, _, _ = spawn [ "WINDTRAP_MUTATE=1" ] in
        equal ~msg:"exit code" int 0 code;
        is_true ~msg:"the file exists" (Sys.file_exists verdict_path);
        match M.load verdict_path with
        | Error e -> failf "verdict file unreadable: %a" M.pp_error e
        | Ok (verdicts, identity) ->
            is_true ~msg:"the writer identity is recorded" (identity <> None);
            let rendered =
              List.map
                (fun (r : M.record) ->
                  ( M.id_to_string r.M.id,
                    Format.asprintf "%a" M.pp_verdict r.M.verdict ))
                (M.records verdicts)
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
            says ~msg:"the survivor names its witnesses"
              (snd (List.hd survived))
              "widen > widen is nonzero");
    test
      "a verdict file beside this one's scopes the summary and names the merge"
      (fun () ->
        (* A library covered by several test stanzas is the normal case,
           and one executable's view of it is not the project's number.
           Any [.mutants] file that is not this executable's is a sibling,
           so the fixture is one written by the runtime itself. *)
        let sibling =
          Filename.concat
            (Filename.dirname verdict_path)
            "windtrap-0000000000000000000000000000ffff.mutants"
        in
        Fun.protect
          ~finally:(fun () -> try Sys.remove sibling with Sys_error _ -> ())
          (fun () ->
            M.save sibling M.empty;
            let code, out, _ = spawn [ "WINDTRAP_MUTATE=1" ] in
            equal ~msg:"exit code" int 0 code;
            says ~msg:"the total is scoped to what this executable links" out
              "mutants: 1 survived of 4 (this executable)";
            says ~msg:"and the merge is named" out "project: dune build @mutate"));
  ]

let crash_tests =
  [
    test "a child that dies without writing a verdict is killed by crash"
      (fun () ->
        (try Sys.remove verdict_path with Sys_error _ -> ());
        let code, out, err =
          spawn [ "MUTATE_FIXTURE=crash"; "WINDTRAP_MUTATE=1" ]
        in
        equal ~msg:"the parent survives its child" int 0 code;
        equal ~msg:"stderr" text "" err;
        says ~msg:"both kills counted" out "2 killed";
        says ~msg:"and one survivor still reported" out
          "mutants: 1 survived of 4";
        match M.load verdict_path with
        | Error e -> failf "verdict file unreadable: %a" M.pp_error e
        | Ok (verdicts, _) ->
            let rendered =
              List.map
                (fun (r : M.record) ->
                  ( M.id_to_string r.M.id,
                    Format.asprintf "%a" M.pp_verdict r.M.verdict ))
                (M.records verdicts)
            in
            equal ~msg:"the crash is named as a crash" (list string)
              [ "killed (crash)" ]
              (List.filter_map
                 (fun (_, v) -> if v = "killed (crash)" then Some v else None)
                 rendered));
  ]

let refusal_tests =
  [
    test "a red dry run refuses to score anything" (fun () ->
        let code, _, err =
          spawn [ "MUTATE_FIXTURE=red"; "WINDTRAP_MUTATE=1" ]
        in
        equal ~msg:"exit code" int 1 code;
        says ~msg:"the reason" err "the dry run is red";
        denies ~msg:"no report" err "mutants: ");
    test "a suite that does not agree with its own re-run is refused, by name"
      (fun () ->
        (* The probe's whole job. The fixture passes in the process that
           measured the reach map and fails in every fork of it, so the
           disagreement is the one the loop would otherwise blame on the
           mutants. *)
        let code, out, err =
          spawn [ "MUTATE_FIXTURE=flaky"; "WINDTRAP_MUTATE=1" ]
        in
        equal ~msg:"exit code" int 1 code;
        says ~msg:"the finding" err "the suite is not deterministic";
        says ~msg:"the dry run's numbers" err
          "the dry run executed 4 test(s), skipping 0 and failing none";
        says ~msg:"the probe's" err
          "the probe executed 4, skipping 0 and failing 1";
        says ~msg:"and the test that disagreed, by name" err
          "flaky \u{203a} passes where it was measured";
        denies ~msg:"no number was produced" out "mutants: ");
    test "a suite that kills nothing at all is reported as a build problem"
      (fun () ->
        let code, _, err =
          spawn [ "MUTATE_FIXTURE=weak"; "WINDTRAP_MUTATE=1" ]
        in
        equal ~msg:"exit code" int 1 code;
        says ~msg:"the finding" err "changed nothing";
        says ~msg:"the likeliest cause" err
          "--instrument-with ppx_windtrap.mutate";
        says ~msg:"the other cause" err "[@mutate off]");
    test "a selection that matched nothing is refused, never scored" (fun () ->
        let code, out, err =
          spawn ~args:[ "-f"; "no-such-test" ] [ "WINDTRAP_MUTATE=1" ]
        in
        (* Never 2: "nothing ran" is a statement about a test selection
           and a mutation run does not make one (Law 16e). *)
        equal ~msg:"exit code" int 1 code;
        says ~msg:"the reason" err "nothing to mutate";
        denies ~msg:"and no number was produced" out "mutants: 0 survived");
    test "an unrecognized WINDTRAP_MUTATE names the variable" (fun () ->
        let code, _, err = spawn [ "WINDTRAP_MUTATE=maybe" ] in
        equal ~msg:"exit code" int 1 code;
        says ~msg:"the message" err "invalid value 'maybe' for WINDTRAP_MUTATE";
        says ~msg:"what it expected" err "1, report or off");
    test "an unrecognized WINDTRAP_MUTATE_LIMIT names the variable" (fun () ->
        let code, _, err =
          spawn [ "WINDTRAP_MUTATE=1"; "WINDTRAP_MUTATE_LIMIT=lots" ]
        in
        equal ~msg:"exit code" int 1 code;
        says ~msg:"the message" err
          "invalid value 'lots' for WINDTRAP_MUTATE_LIMIT");
    test "asking for the loop and an armed mutant at once is refused" (fun () ->
        let code, _, err =
          spawn
            [ "WINDTRAP_MUTATE=1"; M.arm_variable ^ "=" ^ mutant_named "add" ]
        in
        equal ~msg:"exit code" int 1 code;
        says ~msg:"both variables named" err "WINDTRAP_MUTATE and ";
        says ~msg:"the arming variable" err M.arm_variable);
    test
      "an armed identifier stale within a file this build catalogues is refused"
      (fun () ->
        (* The other side of the leniency below: the executable WAS built
           from subject.ml, so an identifier naming a position no site of
           it occupies is wrong or stale, and running green on it is how a
           silently ignored arming becomes a false survivor. *)
        let code, out, err = spawn [ M.arm_variable ^ "=" ^ stale_id () ] in
        equal ~msg:"exit code" int 1 code;
        says ~msg:"the identifier" err (stale_id ());
        says ~msg:"the diagnosis" err "no such mutation site";
        says ~msg:"the file's real sites are named" err (mutant_named "add");
        denies ~msg:"and the suite did not run" out "calc: ");
  ]

let armed_tests =
  [
    test
      "an armed run announces the mutant before any other output and reports \
       the kill" (fun () ->
        let code, out, _ =
          spawn [ M.arm_variable ^ "=" ^ mutant_named "add" ]
        in
        equal ~msg:"the mutant made a test fail" int 1 code;
        let first = List.hd (String.split_on_char '\n' out) in
        equal ~msg:"the announcement is the first line" string
          ("mutant " ^ mutant_named "add" ^ " armed: a - b \u{2192} a + b")
          first;
        says ~msg:"the failure block" out "FAIL";
        says ~msg:"the closing line" out "mutant killed.");
    test "an armed run whose selection matched nothing claims no kill"
      (fun () ->
        (* [mutant killed.] is a verdict, and a verdict is never an exit
           code (Law 16c): a filter that matched nothing exits 2, which
           says something about the filter and nothing about the
           mutant. *)
        let code, out, _ =
          spawn ~args:[ "-f"; "no-such-test" ]
            [ M.arm_variable ^ "=" ^ mutant_named "add" ]
        in
        equal ~msg:"the runner's own 'nothing ran' code" int 2 code;
        says ~msg:"the mutant was still announced" out " armed: ";
        denies ~msg:"but nothing died" out "mutant killed.");
    test "an armed mutant that survives says so by staying quiet" (fun () ->
        let code, out, _ =
          spawn
            [
              "MUTATE_FIXTURE=weak";
              M.arm_variable ^ "=" ^ List.nth (Lazy.force catalogue) 1;
            ]
        in
        equal ~msg:"exit code" int 0 code;
        says ~msg:"the announcement" out "armed: a + b \u{2192} a - b";
        says ~msg:"the suite still passed" out "calc: 2 passed";
        denies ~msg:"nothing killed" out "mutant killed.");
  ]

(* Law 16(d), through the runner it exists for.

   The inline runtime records its correction directory at module load
   ([Sys.getcwd ()]) and, for a recorded source [f], re-reads
   [<dir>/<basename f>] and writes [<dir>/<basename f>.corrected] — see
   [Ppx_runtime.absolute_path] and [flush_corrections_report]. So the
   child is started in a directory that carries a copy of the fixture at
   exactly the name the writer will open: with read-only checking removed
   this scenario writes [inline_armed.ml.corrected] into the staging
   directory and the assertion below fails on the file. Staging it
   anywhere else would make the write fail with ENOENT — the test would
   still go red, but on a missing file rather than on the law. *)

let inline_exe =
  Filename.concat (Filename.concat exe_dir "inline") "runner_main.exe"

let staged_source_dir () =
  let root = Filename.concat scratch_dir ("law16d" ^ string_of_int !counter) in
  (try Sys.mkdir root 0o755 with Sys_error _ -> ());
  let contents =
    read_file
      (Filename.concat exe_dir (Filename.concat "inline" "inline_armed.ml"))
  in
  if contents = "" then fail "the inline fixture source was not found";
  let oc = open_out_bin (Filename.concat root "inline_armed.ml") in
  output_string oc contents;
  close_out oc;
  root

(* The two halves of Law 16(d) fail independently — the recorders write no
   correction, and the exit protocol reports no failure as covered by one
   — so they are two tests: whichever half regresses, the report names
   it. *)
let armed_inline () =
  let cwd = staged_source_dir () in
  let code, out, err =
    spawn ~exe:inline_exe
      ~args:[ "inline-test-runner"; "inline_armed" ]
      ~cwd
      [ M.arm_variable ^ "=" ^ List.nth (Lazy.force catalogue) 1 ]
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
        denies ~msg:"no promotion notice" err "wrote";
        denies ~msg:"and none was even attempted" err "correction for");
    test
      "an armed inline mismatch is a plain failure, not a promotion-covered \
       pass" (fun () ->
        let _, code, out, _ = armed_inline () in
        (* Exit 0 here would be the correction-coverage downgrade firing on
           output an armed mutant produced on purpose: dune would record
           the partition as passed and offer a promotion. *)
        equal ~msg:"a mismatch is a plain failure" int 1 code;
        says ~msg:"announced first" out "armed: a + b \u{2192} a - b";
        says ~msg:"the mismatch is reported as a mismatch" out "expected  7";
        says ~msg:"against the mutated output" out "actual    -1";
        denies ~msg:"and not as a merged-history CR" out "ran multiple times";
        says ~msg:"the kill closes the loop" out "mutant killed.");
    test
      "a loop under the inline runner kills through its children and rewrites \
       nothing" (fun () ->
        (* The whole seam through the OTHER runner: the loop takes the
           process over inside [Ppx_runtime.run_inline_suite], so the
           correction protocol never runs at all, and Law 16(d) has to
           hold in a forked child rather than in an interactive armed
           run. *)
        let cwd = staged_source_dir () in
        let code, out, err =
          spawn ~exe:inline_exe
            ~args:[ "inline-test-runner"; "inline_armed" ]
            ~cwd [ "WINDTRAP_MUTATE=1" ]
        in
        equal ~msg:"the loop completed" int 0 code;
        equal ~msg:"stderr" text "" err;
        says ~msg:"the dry run is the partition's ordinary transcript" out
          "inline_armed: 1 passed";
        says ~msg:"the child's mismatch killed the mutant" out
          "mutants: 0 survived of 4";
        says ~msg:"and the rest of the file is unreached here" out "3 unreached";
        is_false ~msg:"no child wrote a correction"
          (Sys.file_exists (Filename.concat cwd "inline_armed.ml.corrected"));
        denies ~msg:"and none was attempted" err "correction for");
    test "the same partition is green and silent unarmed" (fun () ->
        let cwd = staged_source_dir () in
        let code, out, err =
          spawn ~exe:inline_exe
            ~args:[ "inline-test-runner"; "inline_armed" ]
            ~cwd []
        in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"stderr" text "" err;
        denies ~msg:"nothing armed" out " armed: ";
        is_false ~msg:"no .corrected"
          (Sys.file_exists (Filename.concat cwd "inline_armed.ml.corrected")));
  ]

(* The runaway hit-count budget, end to end.

   [runaway_main.exe]'s one mutant turns a terminating loop into a
   non-terminating one, and in this slice the budget is the ONLY thing
   that stops it: the whole-loop [Unix.setitimer] has a sixty-second
   floor, and the per-mutant deadline is a later release. So the shape of
   the verdict is the assertion — [killed] naming the test that failed
   means the guard raised inside the child and the runner reported it as
   an ordinary failure, which is the documented contract; [killed
   (timeout)] would mean the budget did nothing and the clock cleaned up
   after it, and this test would take a minute to say so. *)

let runaway_exe =
  Filename.concat (Filename.concat exe_dir "runaway") "runaway_main.exe"

let runaway_tests =
  [
    test "a mutant that would never terminate is killed by its hit budget"
      (fun () ->
        let path = M.output_file ~exe:runaway_exe in
        (try Sys.remove path with Sys_error _ -> ());
        let started = Unix.gettimeofday () in
        let code, out, err = spawn ~exe:runaway_exe [ "WINDTRAP_MUTATE=1" ] in
        let elapsed = Unix.gettimeofday () -. started in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"stderr" text "" err;
        says ~msg:"the mutant died" out "mutants: 0 survived of 1";
        is_true
          ~msg:
            (Printf.sprintf
               "the budget cut it short, not the loop deadline (%.1fs)" elapsed)
          (elapsed < 30.);
        match M.load path with
        | Error e -> failf "verdict file unreadable: %a" M.pp_error e
        | Ok (verdicts, _) ->
            equal ~msg:"killed by the test the guard failed" (list string)
              [ "killed by counts down to zero" ]
              (List.map
                 (fun (r : M.record) ->
                   Format.asprintf "%a" M.pp_verdict r.M.verdict)
                 (M.records verdicts)));
  ]

(* One identifier, every executable — the report's own remedy

   The report tells the reader to arm a survivor with
   [WINDTRAP_MUTATE_ARM=<id> dune runtest --instrument-with
   ppx_windtrap.mutate], because a command that links no test executable
   has no single binary to name. That runs EVERY instrumented executable
   with the variable set, and windtrap's own lib/ is covered by seven. So
   the scenario here is the real one: one identifier handed to two
   executables built from disjoint sources — suite_main from subject.ml,
   runaway_main from spinner.ml — once to the binary that holds the
   mutant and once to a binary that does not. Both must be usable answers
   to one command, or dune fails the build BECAUSE the one binary that
   has the mutant armed it correctly. *)

let cross_executable_tests =
  [
    test "an identifier this executable holds no site of lets it run green"
      (fun () ->
        let id = mutant_named "add" in
        let code, out, err =
          spawn ~exe:runaway_exe [ M.arm_variable ^ "=" ^ id ]
        in
        equal ~msg:"exit code (this binary is not the one it is about)" int 0
          code;
        says ~msg:"the suite ran, ordinarily" out "spin: 1 passed";
        denies ~msg:"and armed nothing" out " armed: ";
        says ~msg:"it says whose mutant it is not" err
          "not this executable's mutant";
        says ~msg:"naming the identifier it declined" err id;
        equal ~msg:"and says it exactly once" int 1
          (List.length
             (List.filter
                (fun line -> has_sub line "not this executable's mutant")
                (String.split_on_char '\n' err))));
    test "the same identifier still arms the executable that does hold it"
      (fun () ->
        (* The pair is the point: one command, one identifier, and the
           binary that catalogues the site does the work while its
           siblings stand down. Asserted beside the decline so that a
           change making every executable decline cannot pass. *)
        let id = mutant_named "add" in
        let code, out, _ = spawn [ M.arm_variable ^ "=" ^ id ] in
        equal ~msg:"the mutant made a test fail" int 1 code;
        says ~msg:"it armed the named mutant" out
          ("mutant " ^ id ^ " armed: a - b \u{2192} a + b"));
    test "a declining executable still takes the ordinary instrumented path"
      (fun () ->
        (* Declining is not arming: nothing is armed, so the process is
           observationally the original program and owes no read-only
           checking. The discovery line is the proof that it took the
           ordinary instrumented path rather than a hushed one. *)
        let id = mutant_named "add" in
        let code, out, _ =
          spawn ~exe:runaway_exe [ M.arm_variable ^ "=" ^ id ]
        in
        equal ~msg:"exit code" int 0 code;
        says ~msg:"the discovery line, as on any unarmed instrumented run" out
          "mutants: 1 in 1 file");
  ]

(* The control: no instrumented module in the executable at all. *)

let plain_exe = Filename.concat exe_dir "plain_main.exe"

let uninstrumented_tests =
  [
    test "a build with no mutants runs exactly as it would without the seam"
      (fun () ->
        let code, out, err = spawn ~exe:plain_exe [] in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"stderr" text "" err;
        says ~msg:"the ordinary summary" out "plain: 1 passed";
        denies ~msg:"and nothing else" out "mutants:");
    test "asking a build with no mutants to mutate declines by name" (fun () ->
        let code, out, err = spawn ~exe:plain_exe [ "WINDTRAP_MUTATE=1" ] in
        equal ~msg:"exit code" int 1 code;
        says ~msg:"the suite still ran" out "plain: 1 passed";
        says ~msg:"the diagnosis" err "links no instrumented module";
        says ~msg:"the fix" err "--instrument-with ppx_windtrap.mutate");
    test "an armed identifier declines by name and leaves the run alone"
      (fun () ->
        (* An uninstrumented executable is the commonest sibling of all:
           under [WINDTRAP_MUTATE_ARM=<id> dune runtest] every (test)
           stanza in the project gets the variable, and most of them
           catalogue nothing. Exiting 1 here would fail the build for the
           one executable that armed the mutant correctly. *)
        let code, out, err =
          spawn ~exe:plain_exe [ M.arm_variable ^ "=lib/absent.ml:1:0:add" ]
        in
        equal ~msg:"exit code" int 0 code;
        says ~msg:"the suite ran exactly as it would unarmed" out
          "plain: 1 passed";
        denies ~msg:"nothing armed" out " armed: ";
        denies ~msg:"and no discovery line, there being nothing to discover" out
          "mutants:";
        says ~msg:"the identifier" err "lib/absent.ml:1:0:add";
        says ~msg:"the diagnosis" err "not this executable's mutant";
        says ~msg:"and the misconfiguration it could still be" err
          "--instrument-with ppx_windtrap.mutate");
    test "a misspelled knob is loud even where there is nothing to mutate"
      (fun () ->
        let code, _, err = spawn ~exe:plain_exe [ "WINDTRAP_MUTATE=perhaps" ] in
        equal ~msg:"exit code" int 1 code;
        says ~msg:"the variable" err "WINDTRAP_MUTATE");
  ]

let () =
  run "mutate loop"
    [
      group "catalogue" catalogue_tests;
      group "discovery" discovery_tests;
      group "loop" loop_tests;
      group "reach map" reach_tests;
      group "no trace outside the pipe" no_trace_tests;
      group "verdict file" verdict_file_tests;
      group "crash" crash_tests;
      group "refusals" refusal_tests;
      group "armed" armed_tests;
      group "one identifier, several executables" cross_executable_tests;
      group "read-only checking" read_only_tests;
      group "runaway budget" runaway_tests;
      group "uninstrumented" uninstrumented_tests;
    ]
