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

let exe_dir = Filename.dirname Sys.executable_name
let suite_exe = Filename.concat exe_dir "suite_main.exe"

(* The verdict file records no cause and the runtime publishes no
   printer, so a scenario that reads verdicts back spells them here. *)
let pp_verdict ppf = function
  | V.Killed -> Format.pp_print_string ppf "killed"
  | V.Survived { witness; others } ->
      Format.fprintf ppf "survived by %s"
        (String.concat ", "
           (List.map (String.concat " > ") (witness :: others)))
  | V.Unreached -> Format.pp_print_string ppf "unreached"

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
  (* The scope that keeps this suite's fixtures controlled. Under
     --instrument-with the children link a mutation-instrumented windtrap
     core, and every count here — five sites, the reach map, the verdict
     file — is written against this directory's own fixtures: subject.ml,
     runaway/spinner.ml, inline/inline_armed.ml. The loop applies
     WINDTRAP_MUTATE_ONLY to the population it forks over, so the
     children test exactly those mutants whatever else they link.

     Omitted when the caller sets it, because [getenv] answers with the
     first match and a default listed first would silently win over the
     scenario's own. *)
  let sets name =
    List.exists (String.starts_with ~prefix:(name ^ "=")) bindings
  in
  let default_scope =
    if sets "WINDTRAP_MUTATE_ONLY" then []
    else [ "WINDTRAP_MUTATE_ONLY=test/mutate_loop/" ]
  in
  Array.of_list
    (List.concat_map inherited [ "PATH"; "HOME"; "TMPDIR"; "LANG"; "LC_ALL" ]
    @ [ "WINDTRAP_COLOR=never"; "WINDTRAP_SLOW_THRESHOLD=0" ]
    @ default_scope @ bindings)

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

(* The [reproduce] footer's variable binding, completed exactly as a
   reader would: the identifier from the survivor's head row pasted over
   the footer's [<id>] placeholder. Pasting it back is the only test of
   the footer that can fail when the identifier the report prints is not
   one the runtime resolves. *)
let reproduce_binding_of report =
  let lines = String.split_on_char '\n' report in
  let placeholder = M.arm_variable ^ "=<id>" in
  let footer line =
    String.starts_with ~prefix:"reproduce: " line && has_sub line placeholder
  in
  if not (List.exists footer lines) then
    failf "no reproduce footer binding %s in the report:\n%s" placeholder report;
  let id_of line =
    match String.split_on_char ' ' (String.trim line) with
    | "SURVIVED" :: rest -> List.find_opt (fun w -> w <> "") rest
    | _ -> None
  in
  match List.find_map id_of lines with
  | Some id -> M.arm_variable ^ "=" ^ id
  | None -> failf "no SURVIVED row in the report:\n%s" report

(* The verdict file this executable writes, deleted before every scenario
   that is meant to produce one so that a stale file cannot pass a test
   the loop failed to write. *)
let verdict_path = V.output_file ~exe:suite_exe

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
        says ~msg:"summary" out "calc: 6 passed";
        denies ~msg:"and says nothing about mutants" out "mutants:";
        equal ~msg:"stderr" text "" err);
  ]

(* WINDTRAP_MUTATE_ONLY narrows the population the loop forks over, not
   the registry and not the report, and the two consequences below are
   what the rest of this tree relies on: a scope that matches nothing is
   refused by name, and a scope that matches keeps the fixture's own
   mutants whole. Every other scenario in this file passes the directory
   scope through [environment], so without these the feature would only
   ever be exercised incidentally. *)
let scope_tests =
  [
    test "a scope that matches nothing is refused, naming the scope" (fun () ->
        (* The loop declines by name rather than reporting nothing. *)
        let code, _, err =
          spawn
            [ "WINDTRAP_MUTATE=1"; "WINDTRAP_MUTATE_ONLY=::no-such-source::" ]
        in
        equal ~msg:"asking it to mutate exits 1" int 1 code;
        (* The build is instrumented and fine; the scope is what emptied
           the population, so the refusal must name it — blaming
           instrumentation would send the reader to rebuild. *)
        says ~msg:"declines by naming the scope, value included" err
          "scope ::no-such-source:: left no mutants";
        says ~msg:"and both causes an empty scoped catalogue has" err
          "matches no instrumented file, or the matched files have no mutation \
           sites";
        denies ~msg:"never the missing-backend diagnosis" err
          "links no instrumented module");
    test "a scope that matches keeps the whole fixture catalogue" (fun () ->
        let code, out, _ =
          spawn
            [ "WINDTRAP_MUTATE=1"; "WINDTRAP_MUTATE_ONLY=test/mutate_loop/" ]
        in
        equal ~msg:"exit code" int 0 code;
        says ~msg:"the fixture's reach, undiminished" out
          "mutants: 1 survived of 2 reached by this suite \u{00b7} 1 killed");
  ]

let loop_tests =
  [
    test
      "the loop kills one mutant, names the survivor's witnesses and says \
       nothing of the unreached" (fun () ->
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
          "2 tests ran this line and none failed:";
        says ~msg:"first witness" out "widen \u{203a} widen is nonzero";
        says ~msg:"second witness" out "widen \u{203a} widen is not 99";
        (* The block is the finding and the footer the remedy: no per-block
           command, no attribute to paste. *)
        denies ~msg:"no arm line" out "    arm ";
        denies ~msg:"no dismissal hint" out "[@mutate off";
        says ~msg:"the reproduce footer, under the summary" out
          "mutants: 1 survived of 2 reached by this suite \u{00b7} 1 killed\n\
           reproduce: WINDTRAP_MUTATE_ARM=<id> ";
        (* One executable's unreached mutant is usually another's reached
           one: the per-executable report neither lists nor counts them. *)
        denies ~msg:"no unreached section" out "never reached";
        denies ~msg:"no unreached block" out "UNREACHED";
        denies ~msg:"no unreached term" out "unreached";
        (* The killed mutant is not a survivor and not unreached. *)
        denies ~msg:"only one block" out "survivors (2)");
    test "a dismissed mutant is in no block and no count" (fun () ->
        let _, out, _ = spawn [ "WINDTRAP_MUTATE=1" ] in
        (* The [green] suite runs the [@mutate off] site and pins nothing
           about it, so without the dismissal it would be a second
           survivor and a third reached mutant. *)
        says ~msg:"the reached count leaves it out" out
          "mutants: 1 survived of 2 reached by this suite \u{00b7} 1 killed";
        denies ~msg:"no block for the dismissed line" out
          (List.nth (Lazy.force catalogue) 4));
    test "the survivor block quotes the mutated source line" (fun () ->
        let _, out, _ = spawn [ "WINDTRAP_MUTATE=1" ] in
        says ~msg:"excerpt row" out "let widen a b = a + b");
    test "the reproduce footer spells a variable that arms the survivor"
      (fun () ->
        let _, out, _ = spawn [ "WINDTRAP_MUTATE=1"; "INSIDE_DUNE=1" ] in
        says ~msg:"the backend flag, before the target" out
          "reproduce: WINDTRAP_MUTATE_ARM=<id> dune exec --instrument-with \
           ppx_windtrap.mutate ";
        (* The footer is completed and pasted back rather than
           pattern-matched: the identifier the report prints has to be one
           the runtime's own selector grammar resolves, and the only proof
           of that is a run that announces the same rewrite. *)
        let binding = reproduce_binding_of out in
        let code, armed, _ = spawn [ binding ] in
        equal ~msg:"the armed run's exit code (this mutant survives)" int 0 code;
        says ~msg:"the pasted line armed the survivor" armed
          "armed: a + b \u{2192} a - b");
    test "a filtered run's footer reproduces the filtered run" (fun () ->
        (* The survivor survived the selection, so the remedy must restate
           it — spelled as the replay line spells a filter, and pasted back
           with the same selection to prove it still arms. *)
        let _, out, _ =
          spawn ~args:[ "-f"; "widen" ] [ "WINDTRAP_MUTATE=1"; "INSIDE_DUNE=1" ]
        in
        says ~msg:"the selection scopes the summary" out
          "reached by the 2 selected tests";
        says ~msg:"and rides the footer" out
          "test/mutate_loop/suite_main.exe -- -f 'widen'\n";
        let binding = reproduce_binding_of out in
        let code, armed, _ = spawn ~args:[ "-f"; "widen" ] [ binding ] in
        equal ~msg:"the armed run's exit code" int 0 code;
        says ~msg:"the pasted line armed the survivor" armed
          "armed: a + b \u{2192} a - b");
    test "every survivor gets a block, in most-watched order" (fun () ->
        let _, out, _ =
          spawn [ "MUTATE_FIXTURE=capped"; "WINDTRAP_MUTATE=1" ]
        in
        says ~msg:"both blocks" out "survivors (2)";
        says ~msg:"most-watched first" out
          "SURVIVED  test/mutate_loop/subject.ml:18";
        says ~msg:"then the one-witness survivor" out
          "SURVIVED  test/mutate_loop/subject.ml:21";
        says ~msg:"and the summary counts both" out
          "mutants: 2 survived of 3 reached by this suite \u{00b7} 1 killed");
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
          spawn [ "MUTATE_FIXTURE=boundary"; "WINDTRAP_MUTATE=1" ]
        in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"stderr" text "" err;
        (* [orphan] is evaluated at module load and [crasher] by a fixture
           release: both are outside every test, and folding either into
           the test on whose side of the boundary it sits would make it a
           permanent false survivor. The report counts only what was
           reached; the verdict file, which the merge reads, names the
           two as unreached. *)
        says ~msg:"the two out-of-test sites are not reached" out
          "mutants: 1 survived of 2 reached by this suite \u{00b7} 1 killed";
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
        says ~msg:"exactly one survivor" out "survivors (1)";
        (* The witness list is the whole product of the run: the first and
           third tests reach the line, the second, fourth and fifth do
           not, and a retried first test contributes one window. *)
        says ~msg:"the count" out "2 tests ran this line and none failed:";
        says ~msg:"the first test" out
          "widen \u{203a} first reaches widen, after a retry";
        says ~msg:"the third test" out "widen \u{203a} third reaches widen";
        denies ~msg:"not the second" out "widen \u{203a} second reaches sub";
        denies ~msg:"not the fourth" out "widen \u{203a} fourth reaches sub";
        denies ~msg:"not the fifth" out "widen \u{203a} fifth reaches sub";
        says ~msg:"summary" out
          "mutants: 1 survived of 2 reached by this suite \u{00b7} 1 killed");
    test "a tag selection still selects the child's tests" (fun () ->
        (* [--tag gated] is the one selection a pruned tree cannot
           express: tags are not in a path. A child that dropped the
           parent's tag predicate runs nothing, and the determinism probe
           reports a deterministic suite as non-deterministic. *)
        let code, out, err =
          spawn
            [
              "MUTATE_FIXTURE=tagged"; "WINDTRAP_TAG=gated"; "WINDTRAP_MUTATE=1";
            ]
        in
        equal ~msg:"exit code" int 0 code;
        denies ~msg:"the probe agreed" err "not deterministic";
        says ~msg:"the dry run's five tests" out "calc: 5 passed";
        says ~msg:"and the loop scored them, against the five it selected" out
          "mutants: 1 survived of 2 reached by the 5 selected tests \u{00b7} 1 \
           killed";
        says ~msg:"over the same witnesses as the untagged suite" out
          "2 tests ran this line and none failed:";
        (* A tag selection is a selection: the run is not the suite's
           default predicate, so its verdicts stay in the process. *)
        says ~msg:"and a tag-selected run persists nothing" out
          "verdicts not saved");
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
   [Run.execute], and the child's own wrapper is the only thing between
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
        says ~msg:"the fatal child scored as a crash kill, the report whole" out
          "mutants: 1 survived of 3 reached by this suite \u{00b7} 2 killed";
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
        match V.load verdict_path with
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
            says ~msg:"the survivor names its witnesses"
              (snd (List.hd survived))
              "widen > widen is nonzero");
    test "a narrowed run reports in full but persists nothing" (fun () ->
        (try Sys.remove verdict_path with Sys_error _ -> ());
        let code, out, _ = spawn [ "WINDTRAP_MUTATE=1" ] in
        equal ~msg:"the full run's exit code" int 0 code;
        denies ~msg:"a full run saves without comment" out "verdicts not saved";
        let saved = read_file verdict_path in
        is_true ~msg:"and wrote the file" (saved <> "");
        (* The selection reaches only [sub], whose mutant dies, so the
           loop completes — and its verdicts call [widen] unreached, which
           is exactly the selection-relative record that must not
           overwrite the full run's survivor. The harness's default
           WINDTRAP_MUTATE_ONLY scope is in force here too, so this is
           also the combined case: a filter skips the write even where
           the scope alone would still save. *)
        let code, out, err =
          spawn ~args:[ "-f"; "calc" ] [ "WINDTRAP_MUTATE=1" ]
        in
        equal ~msg:"the narrowed run still completes" int 0 code;
        equal ~msg:"stderr" text "" err;
        says ~msg:"and still reports, against its selection" out
          "mutants: 1 reached by the 3 selected tests \u{00b7} 1 killed";
        denies ~msg:"a clean report has nothing to reproduce" out "reproduce:";
        says ~msg:"but says what it did not persist" out
          "verdicts not saved: this run's selection narrows the suite, and a \
           partial run's verdicts would stand in the project merge as the \
           whole.";
        equal ~msg:"the canonical file is byte-identical" text saved
          (read_file verdict_path));
    test "an ONLY-scoped run still writes: its records are project-true"
      (fun () ->
        (try Sys.remove verdict_path with Sys_error _ -> ());
        let code, out, _ =
          spawn
            [ "WINDTRAP_MUTATE=1"; "WINDTRAP_MUTATE_ONLY=test/mutate_loop/" ]
        in
        equal ~msg:"exit code" int 0 code;
        denies ~msg:"the scope narrows the mutants, not the tests" out
          "verdicts not saved";
        is_true ~msg:"so the file was written" (Sys.file_exists verdict_path));
  ]

let crash_tests =
  [
    test "a child that dies without writing a verdict is killed, not survived"
      (fun () ->
        (try Sys.remove verdict_path with Sys_error _ -> ());
        let code, out, err =
          spawn [ "MUTATE_FIXTURE=crash"; "WINDTRAP_MUTATE=1" ]
        in
        equal ~msg:"the parent survives its child" int 0 code;
        equal ~msg:"stderr" text "" err;
        says ~msg:"both kills counted, one survivor still reported" out
          "mutants: 1 survived of 3 reached by this suite \u{00b7} 2 killed";
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
               killed like the ordinary one, and never as a survivor — a
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
    test "a selection that matched nothing is refused, never scored" (fun () ->
        let code, out, err =
          spawn ~args:[ "-f"; "no-such-test" ] [ "WINDTRAP_MUTATE=1" ]
        in
        (* Never 2: "nothing ran" is a statement about a test selection
           and a mutation run does not make one (Law 16e). *)
        equal ~msg:"exit code" int 1 code;
        says ~msg:"the reason" err "nothing to mutate";
        denies ~msg:"and no number was produced" out "mutants: ");
    test "an unrecognized WINDTRAP_MUTATE names the variable" (fun () ->
        let code, _, err = spawn [ "WINDTRAP_MUTATE=maybe" ] in
        equal ~msg:"exit code" int 1 code;
        says ~msg:"the message" err "invalid value 'maybe' for WINDTRAP_MUTATE";
        says ~msg:"what it expected" err ": expected 1 or 0");
    test "a falsy WINDTRAP_MUTATE is an ordinary run" (fun () ->
        (* The variable is a boolean like every other switch: [off] asks
           for nothing, exactly as unset does, so a CI recipe can turn the
           loop off without unsetting anything. *)
        let code, out, err = spawn [ "WINDTRAP_MUTATE=off" ] in
        equal ~msg:"exit code" int 0 code;
        says ~msg:"the ordinary transcript" out "calc: 6 passed";
        denies ~msg:"no mutation line" out "mutants:";
        equal ~msg:"stderr" string "" err);
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
        says ~msg:"the closing line" out "mutant killed.";
        denies ~msg:"a kill is the whole verdict" out "mutant survived";
        denies ~msg:"and the site was plainly evaluated" out
          "mutant not evaluated");
    test "an armed run whose selection matched nothing claims no kill"
      (fun () ->
        (* [mutant killed.] is a verdict, and a verdict is never an exit
           code (Law 16c): a filter that matched nothing exits 2, which
           says something about the filter and nothing about the
           mutant — so neither of the other closing lines may print
           either. *)
        let code, out, _ =
          spawn ~args:[ "-f"; "no-such-test" ]
            [ M.arm_variable ^ "=" ^ mutant_named "add" ]
        in
        equal ~msg:"the runner's own 'nothing ran' code" int 2 code;
        says ~msg:"the mutant was still announced" out " armed: ";
        denies ~msg:"but nothing died" out "mutant killed.";
        denies ~msg:"no survivor claim over a run that made none" out
          "mutant survived";
        denies ~msg:"and no not-evaluated claim either" out
          "mutant not evaluated");
    test "an armed mutant that survives says so, with the evaluation count"
      (fun () ->
        (* Both weak tests run the armed line once each, so the count is a
           claim: a closing line that miscounted, or that printed on a
           run that never evaluated the site, fails here. *)
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
        says ~msg:"the closing line disambiguates the green" out
          "mutant survived: the armed site was evaluated 2 time(s) and no test \
           failed.";
        denies ~msg:"nothing killed" out "mutant killed.");
    test "an armed run whose selection never ran the site says so" (fun () ->
        (* The other green: the suite passed and proved nothing, because
           the selection deselected every test that reaches the line. The
           two endings are what make an armed run's green readable at
           all — without them this transcript and the survivor's are the
           same bytes. *)
        let code, out, _ =
          spawn ~args:[ "-f"; "calc" ]
            [ M.arm_variable ^ "=" ^ List.nth (Lazy.force catalogue) 1 ]
        in
        equal ~msg:"the selected tests passed" int 0 code;
        says ~msg:"the mutant was announced" out "armed: a + b \u{2192} a - b";
        says ~msg:"the closing line blames the selection" out
          "mutant not evaluated: no selected test ran the site.";
        denies ~msg:"no kill" out "mutant killed.";
        denies ~msg:"and no survivor claim" out "mutant survived");
    test "a site evaluated only before arming counts as not evaluated"
      (fun () ->
        (* [boundary]'s module initialization evaluates [orphan] before
           anything is armed, so that window ran the ORIGINAL expression:
           billing it to the run would print a survivor count over
           evaluations the mutant never saw. The closing count starts at
           the arming. *)
        let code, out, _ =
          spawn
            [
              "MUTATE_FIXTURE=boundary";
              M.arm_variable ^ "=" ^ List.nth (Lazy.force catalogue) 2;
            ]
        in
        equal ~msg:"the suite passed" int 0 code;
        says ~msg:"the mutant was announced" out "armed: a + b \u{2192} a - b";
        says ~msg:"and the module-load window is not billed to the run" out
          "mutant not evaluated: no selected test ran the site.";
        denies ~msg:"no survivor claim over an unarmed window" out
          "mutant survived");
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
        denies ~msg:"and none was even attempted" err "could not write");
    test
      "an armed inline mismatch is a plain failure, not a promotion-covered \
       pass" (fun () ->
        let _, code, out, _ = armed_inline () in
        (* Exit 0 here would be the correction-coverage downgrade firing on
           output an armed mutant produced on purpose: dune would record
           the partition as passed and offer a promotion. *)
        equal ~msg:"a mismatch is a plain failure" int 1 code;
        says ~msg:"announced first" out "armed: a + b \u{2192} a - b";
        says ~msg:"the mismatch is reported as a mismatch" out
          "expect: mismatch";
        says ~msg:"with the literal on one side" out "- 7";
        says ~msg:"against the mutated output" out "+ -1";
        denies ~msg:"and not as a merged-history CR" out "ran multiple times";
        says ~msg:"the kill closes the loop" out "mutant killed.");
    test
      "a loop under the inline runner kills through its children and rewrites \
       nothing" (fun () ->
        (* The whole seam through the inline runner's [Windtrap.run]: the
           loop takes the process over at run entry, so no correction is
           ever written, and Law 16(d) has to hold in a forked child
           rather than in an interactive armed run. *)
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
          "mutants: 1 reached by this suite \u{00b7} 1 killed";
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
   non-terminating one, and the budget must stop it BEFORE the per-child
   deadline does: the guard counts hits in microseconds where the
   deadline waits out its one-second floor. A verdict file names no
   cause, so the CLOCK is the assertion: the whole run — dry run, probe
   and one child — measures 0.06 s here, while a deadline kill would add
   the child's full one-second floor on top. A run that finishes inside
   that floor cannot have been ended by it. *)

let runaway_exe =
  Filename.concat (Filename.concat exe_dir "runaway") "runaway_main.exe"

let runaway_tests =
  [
    test "a mutant that would never terminate is killed by its hit budget"
      (fun () ->
        let path = V.output_file ~exe:runaway_exe in
        (try Sys.remove path with Sys_error _ -> ());
        let started = Unix.gettimeofday () in
        let code, out, err = spawn ~exe:runaway_exe [ "WINDTRAP_MUTATE=1" ] in
        let elapsed = Unix.gettimeofday () -. started in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"stderr" text "" err;
        says ~msg:"the mutant died" out
          "mutants: 1 reached by this suite \u{00b7} 1 killed";
        is_true
          ~msg:
            (Printf.sprintf
               "the guard cut it short, inside the child's own 1s deadline \
                floor (%.2fs)"
               elapsed)
          (elapsed < 0.9);
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

   Every deadline below is the loop's own derivation — the fixture
   suite's measured dry-run wall clock plus ten times the scheduled
   tests' measured timings, floored at one second — so a loaded machine
   that slows the tests slows the budget with them: no scenario races an
   absolute sleep against an absolute deadline. It is the only clock over
   a child, and over a run there is none. *)

let rendered_verdicts path =
  match V.load path with
  | Error e -> failf "verdict file unreadable: %a" V.pp_error e
  | Ok (verdicts, _) ->
      List.map
        (fun (r : V.record) ->
          (M.id_to_string r.V.id, Format.asprintf "%a" pp_verdict r.V.verdict))
        (V.records verdicts)

let deadline_tests =
  [
    test "a mutant that blocks is killed by its child's deadline" (fun () ->
        (try Sys.remove verdict_path with Sys_error _ -> ());
        let started = Unix.gettimeofday () in
        let code, out, err =
          spawn [ "MUTATE_FIXTURE=block"; "WINDTRAP_MUTATE=1" ]
        in
        let elapsed = Unix.gettimeofday () -. started in
        equal
          ~msg:"the run completes: a blocked child is a score, not a refusal"
          int 0 code;
        equal ~msg:"stderr" text "" err;
        says ~msg:"the kill counted" out
          "mutants: 1 reached by this suite \u{00b7} 1 killed";
        is_true
          ~msg:
            (Printf.sprintf "the child's own deadline cut it short (%.1fs)"
               elapsed)
          (elapsed < 30.);
        equal ~msg:"the blocked mutant is killed" (list string) [ "killed" ]
          (List.filter_map
             (fun (id, v) -> if id = mutant_named "add" then Some v else None)
             (rendered_verdicts verdict_path)));
    test "a slow but finite test is never killed by the clock" (fun () ->
        (* The regression the multiplier guards: the sleep runs armed and
           unarmed alike, so the dry run prices it into the deadline at
           ten times its measured cost, and the kill must be the
           assertion's. *)
        (try Sys.remove verdict_path with Sys_error _ -> ());
        let started = Unix.gettimeofday () in
        let code, out, err =
          spawn [ "MUTATE_FIXTURE=slow"; "WINDTRAP_MUTATE=1" ]
        in
        let elapsed = Unix.gettimeofday () -. started in
        equal ~msg:"exit code" int 0 code;
        equal ~msg:"stderr" text "" err;
        (* The margin, asserted as a precondition rather than left to the
           verdict. The run is three executions of the suite — dry run,
           probe, one child — each containing [slow]'s 0.25 s sleep, so
           the child cost at most the sleep plus one execution's
           remainder, while its deadline was at least ten times the
           sleep the dry run measured ([sleepf] cannot undershoot). A
           machine too loaded to keep the child inside HALF that bound
           fails here, on the arithmetic, not flakily on the verdict
           below. *)
        let sleep =
          0.25
          (* [slow]'s own [sleepf] *)
        in
        let overhead = Float.max 0. ((elapsed -. (3. *. sleep)) /. 3.) in
        is_true
          ~msg:
            (Printf.sprintf
               "precondition: one child's cost (%.2fs) stays inside half its \
                %.2fs deadline floor"
               (sleep +. overhead) (10. *. sleep))
          (2. *. (sleep +. overhead) <= 10. *. sleep);
        says ~msg:"the mutant died" out
          "mutants: 1 reached by this suite \u{00b7} 1 killed";
        (* And the assertion killed it, not the clock: a deadline kill
           would have added the child's whole 10x-the-sleep floor to a
           run that already pays three sleeps. *)
        is_true
          ~msg:
            (Printf.sprintf
               "the run (%.2fs) finished inside the child's %.2fs deadline"
               elapsed (10. *. sleep))
          (elapsed < 10. *. sleep);
        equal ~msg:"the mutant is killed" (list string) [ "killed" ]
          (List.filter_map
             (fun (id, v) -> if id = mutant_named "add" then Some v else None)
             (rendered_verdicts verdict_path)));
    test
      "an expired child's process group dies whole: no grandchild outlives the \
       run" (fun () ->
        incr counter;
        let pidfile =
          Filename.concat scratch_dir ("grandchild" ^ string_of_int !counter)
        in
        let code, out, _ =
          spawn
            [
              "MUTATE_FIXTURE=block";
              "WINDTRAP_MUTATE=1";
              "MUTATE_GRANDCHILD_PIDFILE=" ^ pidfile;
            ]
        in
        equal ~msg:"the run completed" int 0 code;
        says ~msg:"and scored the blocked mutant" out
          "mutants: 1 reached by this suite \u{00b7} 1 killed";
        let pids =
          List.filter_map int_of_string_opt
            (String.split_on_char '\n' (read_file pidfile))
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
           expiry there means something else — nothing is armed, so the
           hang is the suite's own — and the refusal must say so rather
           than score anything. The fixture blocks only on its SECOND
           run, on the marker the dry run leaves. *)
        incr counter;
        let marker =
          Filename.concat scratch_dir ("marker" ^ string_of_int !counter)
        in
        let started = Unix.gettimeofday () in
        let code, _, err =
          spawn
            [
              "MUTATE_FIXTURE=probe_block";
              "WINDTRAP_MUTATE=1";
              "MUTATE_PROBE_MARKER=" ^ marker;
            ]
        in
        let elapsed = Unix.gettimeofday () -. started in
        equal ~msg:"a refusal, not a score" int 1 code;
        says ~msg:"named as the probe's own deadline" err
          "the determinism probe exceeded its deadline";
        says ~msg:"and stated as a determinism claim" err "not a number";
        is_true
          ~msg:
            (Printf.sprintf "the probe's deadline cut it short (%.1fs)" elapsed)
          (elapsed < 30.));
  ]

(* One identifier, every executable — the report's own remedy

   The aggregate report tells the reader to arm a survivor by re-running
   the instrumented suite with [WINDTRAP_MUTATE_ARM=<id>], because a
   command that links no test executable has no single binary to name.
   That runs EVERY instrumented executable with the variable set, and
   windtrap's own lib/ is covered by seven. So
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
        (* The closing-line trio belongs to a run that armed a mutant; a
           binary that declined made no claim a closing line could
           report. *)
        denies ~msg:"so no closing line judges the run" out
          "mutant not evaluated";
        denies ~msg:"nor claims a survivor" out "mutant survived";
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
  ]

(* The control: no instrumented module in the executable at all — in an
   ordinary build. Under --instrument-with it links an instrumented core
   and is a control for nothing; the one scenario that needs it to
   catalogue nothing says so and steps aside. This executable links the
   same core, so it can tell. *)

let plain_exe = Filename.concat exe_dir "plain_main.exe"
let core_instrumented = M.catalogue () <> []

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
        if core_instrumented then
          skip
            ~reason:
              "the core is instrumented, so plain_main catalogues its mutants"
            ();
        (* An empty binding is unset to Env, so this is the no-scope run:
           an empty catalogue is refused as uninstrumented whatever the
           scope, and the harness's default scope would otherwise be a
           bystander here. *)
        let code, out, err =
          spawn ~exe:plain_exe [ "WINDTRAP_MUTATE=1"; "WINDTRAP_MUTATE_ONLY=" ]
        in
        equal ~msg:"exit code" int 1 code;
        says ~msg:"the suite still ran" out "plain: 1 passed";
        says ~msg:"the diagnosis" err "links no instrumented module";
        says ~msg:"the fix names the backend, not a build tool" err
          "instrument the library under test with ppx_windtrap.mutate";
        denies ~msg:"no scope was set, so none is blamed" err "scope");
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
        says ~msg:"the identifier" err "lib/absent.ml:1:0:add";
        says ~msg:"the diagnosis" err "not this executable's mutant";
        says ~msg:"and the misconfiguration it could still be" err
          "instrumented with ppx_windtrap.mutate");
    test "a misspelled knob is loud even where there is nothing to mutate"
      (fun () ->
        let code, _, err = spawn ~exe:plain_exe [ "WINDTRAP_MUTATE=perhaps" ] in
        equal ~msg:"exit code" int 1 code;
        says ~msg:"the variable" err "WINDTRAP_MUTATE");
  ]

let () =
  exit
  @@ run "mutate loop"
       [
         group "catalogue" catalogue_tests;
         group "unasked" unasked_tests;
         group "scope" scope_tests;
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
         group "per-child deadline" deadline_tests;
         group "uninstrumented" uninstrumented_tests;
       ]
