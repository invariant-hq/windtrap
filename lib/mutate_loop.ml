(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)


(* Not mutated. This module is part of the machinery a mutation run uses
   to judge mutants — the scheduler, the ambient run state, the reporting
   spine, the loop itself — so a mutant here is armed inside the process
   that is supposed to detect it. The failure mode is not a false
   survivor but a hang or a corrupted verdict: a mutated bail counter or
   timeout does not fail the reaching tests, it stops them from
   finishing. Coverage still measures these files; only mutation is off.
   Everything below the scheduler — the verbs, the generators, the
   diffing, the renderers — is mutated. *)
[@@@mutate exclude_file]

(* The parent of a mutation run: dry run, probe, forced-fail check, fork
   loop, verdict file, report. The runtime below (Windtrap_mutate) owns
   the catalogue, the arming slot, the reach counters and the file format;
   this module owns the protocol over them and every decision the report
   shows — the ordering, the cap, the counts. Render orders nothing and
   counts nothing, and neither does the runtime. *)

module M = Windtrap_mutate

let spf = Printf.sprintf

(* The catalogue is complete only after module initialization, which is
   why it is read lazily rather than at this module's own load time.

   The catalogue is already scoped by WINDTRAP_MUTATE_ONLY: the runtime
   applies it at registration, so an out-of-scope file is not in the
   registry at all and its guard is inert. Nothing to filter here. *)
let catalogue = lazy (M.catalogue ())
let instrumented () = Lazy.force catalogue <> []

type run =
  | Ran of (Runner.outcome, Runner.startup_error) result
  | Reported of int

let note fmt =
  Printf.ksprintf
    (fun message ->
      Format.pp_print_flush Format.std_formatter ();
      Format.eprintf "windtrap mutate: %s@." message)
    fmt

let refuse fmt =
  Printf.ksprintf
    (fun message ->
      note "%s" message;
      Reported 1)
    fmt

let saturating_add x y = if x > max_int - y then max_int else x + y

(* The spine ({!Driver.t}), threaded whole: the loop replaces [config] per
   child and passes everything else through untouched, so the places that
   run the suite cannot drift in what they pass. [armed] travels beside
   it, not in it — it is this module's seam with the inline runtime
   (Law 16d), not part of what a driver consumes. Since the registry, the
   seam is registered rather than passed: [execute_and_report] builds
   [armed] from the hooks in [Registry.armed_hooks] and threads it below
   exactly as the argument used to travel. *)

(* The reach map

   Built from the runner's events while the dry run prints its ordinary
   output. The protocol is the runtime's, and its one ordering rule is
   load-bearing: on [Test_started] the window that just closed is drained
   FIRST — whatever accumulated since the previous drain was evaluated
   OUTSIDE any test (module initialization before the first test, teardown
   after the previous one) and belongs to nobody — and only THEN is a
   fresh epoch opened for the test that is starting. Bumping the epoch
   first would fold that window into the test about to run, and the very
   mutants it would fold in are the ones a warm-fork loop can never arm:
   module initialization runs before the fork, so arming one in a child
   changes nothing that child can observe, and reporting it as reached
   would produce a permanent false survivor.

   A drain at [Test_finished] is therefore exactly the set that test
   evaluated, and one final drain after the run closes the last test's
   teardown window. The observer does hashtable writes and list conses and
   nothing else: an observer that raises aborts the run. *)

type site = {
  mutant : M.mutant;
  (* Reaching tests in reverse order, each with the hits its own window
     drained — admission orders a test's candidates by the test's own
     count, not the site's total. *)
  mutable tests : (string list * int) list;
  mutable hits : int;
}

type reach = {
  sites : (M.id, site) Hashtbl.t;
  durations : (string, float) Hashtbl.t; (* joined path -> seconds *)
  mutable executed : string list list; (* reverse execution order *)
  mutable skipped : int;
}

let fresh_reach () =
  {
    sites = Hashtbl.create 256;
    durations = Hashtbl.create 256;
    executed = [];
    skipped = 0;
  }

let record_reached reach ~path (entry : M.reached) =
  let site =
    match Hashtbl.find_opt reach.sites entry.M.mutant.M.id with
    | Some site -> site
    | None ->
        let site = { mutant = entry.M.mutant; tests = []; hits = 0 } in
        Hashtbl.add reach.sites entry.M.mutant.M.id site;
        site
  in
  site.tests <- (path, entry.M.hits) :: site.tests;
  site.hits <- saturating_add site.hits entry.M.hits

let observe reach (event : Runner.event) =
  match event with
  | Runner.Run_started _ | Runner.Fixture_release _ -> ()
  | Runner.Test_started _ ->
      ignore (M.drain ());
      M.next_epoch ()
  | Runner.Test_finished result ->
      let path = result.Run.path in
      List.iter (record_reached reach ~path) (M.drain ());
      reach.executed <- path :: reach.executed;
      Hashtbl.replace reach.durations
        (Test_tree.path_to_string path)
        result.Run.duration;
      if match result.Run.outcome with Failure.Skip _ -> true | _ -> false
      then reach.skipped <- reach.skipped + 1

let reaching_tests reach (mutant : M.mutant) =
  match Hashtbl.find_opt reach.sites mutant.M.id with
  | None -> []
  | Some site -> List.rev_map fst site.tests

let site_hits reach (mutant : M.mutant) =
  match Hashtbl.find_opt reach.sites mutant.M.id with
  | None -> 0
  | Some site -> site.hits

let test_time reach path =
  Option.value ~default:0.
    (Hashtbl.find_opt reach.durations (Test_tree.path_to_string path))

(* The per-child deadline: derived, never a knob. Every child pays the
   fork and a whole process's module initialization before its first
   test, and the dry run just measured that fixed cost for free — it is
   one whole in-process run of this same suite, so its wall clock bounds
   any child's startup. On top of it the child gets ten times the dry
   run's own timings for exactly the tests it is scheduled to run, with a
   one-second floor absorbing measurement noise on fast suites. A mutant
   that blocks — a flipped comparison deadlocking a pipe reader spends
   the budget at 0% CPU, where the runtime's runaway hit-count budget
   sees nothing — is killed at the deadline and scored, and the loop goes
   on: the suite noticed the change by hanging, the same reasoning as a
   crash kill. *)

(* Provisional: generous enough that no measured child has come within an
   order of magnitude of it, and confirmed against real suites by a later
   measurement pass before it is considered settled. *)
let deadline_multiplier = 10.

let child_deadline ~dry_run_wall ~reach paths =
  let scheduled =
    List.fold_left (fun acc path -> acc +. test_time reach path) 0. paths
  in
  dry_run_wall +. Float.max 1. (deadline_multiplier *. scheduled)

(* Child hygiene (Law 16e)

   A mutation child's whole body is wrapped so that no path reaches
   Stdlib's exit machinery. [Stdlib.at_exit] handlers run on uncaught
   exceptions too, so a child dying of Out_of_memory, Stack_overflow or an
   escaped Runaway would otherwise run the coverage at-exit dump against a
   path resolved at module load — before the fork — and overwrite the
   parent's .coverage. Every exception is caught, fatal ones included,
   reduced to a verdict line, and followed by Unix._exit. The fallback
   line is a constant, so reducing an Out_of_memory allocates nothing. *)

let write_all fd line =
  let bytes = Bytes.of_string (line ^ "\n") in
  let n = Bytes.length bytes in
  let rec go offset =
    if offset < n then
      match Unix.write fd bytes offset (n - offset) with
      | written -> go (offset + written)
      | exception Unix.Unix_error (Unix.EINTR, _, _) -> go offset
  in
  go 0

(* [body] receives the pipe's write end: a one-line child returns its
   whole report and never touches it, the batch child streams its per-test
   event lines through it before returning the terminal line. *)
let child_body fd body =
  let line = match body fd with line -> line | exception _ -> "crashed" in
  (try write_all fd line with _ -> ());
  Unix._exit 0

(* A mutated program can print from anywhere — a fixture, a toplevel
   effect, a teardown — and hundreds of children printing into the
   parent's transcript would destroy the report. Both standard descriptors
   go to /dev/null; the pipe is a separate descriptor and is unaffected. *)
let silence_output () =
  match Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o600 with
  | exception Unix.Unix_error _ -> ()
  | fd ->
      Unix.dup2 fd Unix.stdout;
      Unix.dup2 fd Unix.stderr;
      if fd <> Unix.stdout && fd <> Unix.stderr then Unix.close fd

let flush_descriptors () =
  (* Both formatters and both descriptors, before every fork: buffered
     bytes duplicated into a child are printed twice. *)
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  (try flush Stdlib.stdout with Sys_error _ -> ());
  try flush Stdlib.stderr with Sys_error _ -> ()

let rec waitpid_retry pid =
  match Unix.waitpid [] pid with
  | _, status -> status
  | exception Unix.Unix_error (Unix.EINTR, _, _) -> waitpid_retry pid

(* A failure of the parent's own supervision — a pipe, fork or waitpid
   that fails, or a child that refuses to run at all — aborts the run and
   names the cause: a score over an unknown number of unsupervised
   children is not a score. *)
exception Supervision of string

type report = {
  line : string;
  lines : string list;
      (* Every complete (newline-terminated) line, in write order. The
         batch protocol reads these; [line] is the first of them, which
         is what a one-line child writes. *)
  status : Unix.process_status;
  killed : [ `No | `Deadline ];
      (* [`Deadline] is the child's own deadline: the mutant is scored
         and the loop goes on. It is the only clock over a child. *)
}

(* One child, one process group, one deadline. [setsid] at fork puts the
   child — and everything a test under it spawns — in a session of its
   own, so an expiry kills the lot with one signal to the group; the
   direct [kill pid] closes the window before [setsid] has run, where the
   group does not exist yet and a child left alive would block the drain
   below. The parent reads the pipe to EOF before it waits, which is what
   keeps a child writing more than a pipe buffer from deadlocking against
   a parent already in waitpid; [select] is what lets it wake on the
   deadline while it reads. The pipe is close-on-exec: a process a test
   [exec]s must not inherit the write end and hold the drain open past
   its group's death. *)
let fork_child ~deadline body =
  flush_descriptors ();
  let read_fd, write_fd =
    match Unix.pipe ~cloexec:true () with
    | fds -> fds
    | exception Unix.Unix_error (e, _, _) ->
        raise (Supervision (spf "pipe failed: %s" (Unix.error_message e)))
  in
  match Unix.fork () with
  | exception Unix.Unix_error (e, _, _) ->
      Unix.close read_fd;
      Unix.close write_fd;
      raise (Supervision (spf "fork failed: %s" (Unix.error_message e)))
  | 0 ->
      (try Unix.close read_fd with _ -> ());
      (try ignore (Unix.setsid ()) with _ -> ());
      child_body write_fd body
  | pid ->
      Unix.close write_fd;
      let buffer = Buffer.create 128 in
      let chunk = Bytes.create 4096 in
      let killed = ref `No in
      let expires_at = Unix.gettimeofday () +. deadline in
      let kill_group reason =
        killed := reason;
        (try Unix.kill (-pid) Sys.sigkill with Unix.Unix_error _ -> ());
        try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ()
      in
      let read_ready () =
        match Unix.read read_fd chunk 0 (Bytes.length chunk) with
        | 0 -> false
        | n ->
            Buffer.add_subbytes buffer chunk 0 n;
            true
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> true
      in
      let rec watch () =
        let remaining = expires_at -. Unix.gettimeofday () in
        if remaining <= 0. then kill_group `Deadline
        else
          match Unix.select [ read_fd ] [] [] remaining with
          | [], _, _ -> watch ()
          | _ -> if read_ready () then watch ()
          | exception Unix.Unix_error (Unix.EINTR, _, _) -> watch ()
      in
      (* After a group kill the complete lines already on the pipe are
         exactly the outcomes the admission parser must keep, so the
         drain continues to EOF — guaranteed once the writers are dead —
         under a short grace: a process that escaped the group by making
         a session of its own could otherwise hold the read open
         forever, and the lines received by then are all there are. *)
      let rec drain_killed grace_until =
        let remaining = grace_until -. Unix.gettimeofday () in
        if remaining > 0. then
          match Unix.select [ read_fd ] [] [] remaining with
          | [], _, _ -> ()
          | _ -> if read_ready () then drain_killed grace_until
          | exception Unix.Unix_error (Unix.EINTR, _, _) ->
              drain_killed grace_until
      in
      watch ();
      if !killed <> `No then drain_killed (Unix.gettimeofday () +. 1.);
      (try Unix.close read_fd with _ -> ());
      let status =
        match waitpid_retry pid with
        | status -> status
        | exception Unix.Unix_error (e, _, _) ->
            raise
              (Supervision (spf "waitpid failed: %s" (Unix.error_message e)))
      in
      let contents = Buffer.contents buffer in
      let lines =
        (* A killed or crashed child can leave a partial trailing line;
           it is not a report and is dropped. [split_on_char] never
           returns the empty list. *)
        match List.rev (String.split_on_char '\n' contents) with
        | _partial :: complete -> List.rev complete
        | [] -> []
      in
      (* Derived, so a torn write cannot decode as a survivor: bytes that
         never got their newline are not a line, and the one-line readers
         then see nothing rather than seeing "survived". A false survivor
         is the one failure mode that makes people stop running the
         tool. *)
      let line = match lines with l :: _ -> l | [] -> "" in
      { line; lines; status; killed = !killed }

(* Every child starts from the same clean post-dry-run image except for
   what the inline runtime must not inherit — its merged reach histories,
   and its licence to record a correction (Law 16d). Both are [armed]'s
   job; see [execute_and_report]'s argument. *)
(* The child's selection, in the runner's own spelling: [reach] keys tests
   by path components, [Runner]'s allowlist by the rendered path the
   filters and the last-failed store both use. *)
let allowlist_of paths = List.map Test_tree.path_to_string paths

let child_prologue ~armed =
  silence_output ();
  armed ()

(* The verdict line

   A survivor's witnesses are the parent's — they are the tests the dry
   run measured as reaching the mutant, which is exactly what the report
   claims — so a child never spells a test name. A kill names the failing
   test by its index in the list the parent handed it. The line is
   therefore ASCII, of fixed shape, and cannot be malformed by a test name
   carrying a space or a newline. *)

(* A child's [error] line carries a diagnostic the parent prints verbatim,
   and both sources of one — an arming error listing candidates, a startup
   refusal — are written for a terminal and span several lines. The line
   is the framing, so the newlines become spaces here rather than
   truncating the message at the parent's end. *)
let one_line message =
  String.map (function '\n' | '\r' | '\t' -> ' ' | c -> c) message

let index_of path paths =
  let rec go i = function
    | [] -> None
    | candidate :: rest -> if candidate = path then Some i else go (i + 1) rest
  in
  go 0 paths

let counted_failure (r : Run.result) =
  r.Run.counted
  && match r.Run.outcome with Failure.Fail _ -> true | _ -> false

(* Whether a run that had a mutant armed detected it: a counted failure on
   a test row, or a fixture-release row. Read off the results and never off
   [outcome.exit_code] (Law 16c): the exit code answers a different
   question — it is [2] for a selection that matched nothing, which is a
   statement about a filter and not about a mutant. *)
let kills (r : Run.result) = counted_failure r

let executed_test (r : Run.result) = r.Run.subject = Run.Test

let killed_by (outcome : Runner.outcome) =
  List.exists kills (Run.results outcome.Runner.run)

let encode_outcome ~paths (outcome : Runner.outcome) =
  let results = Run.results outcome.Runner.run in
  if List.exists kills results then "killed"
  else if
    (* A child that recorded no test row did not survive the mutant, it
       failed to test it: reporting a survivor here would send the reader
       to strengthen tests that never ran. The allowlist is the dry
       run's own executed paths, so this is unreachable — and a false
       survivor is the one failure mode that makes people stop running
       the tool, so it is not left to be unreachable. *)
    (not (List.exists executed_test results)) && paths <> []
  then "crashed"
  else "survived"

let decode_verdict ~paths line =
  match String.split_on_char ' ' (String.trim line) with
  | [ "survived" ] -> Ok (M.survived paths)
  | "error" :: rest -> Error (String.concat " " rest)
  (* "killed", the wrapper's own "crashed", no line at all, or a partial
     one: a child that did not report a survivor proved none. *)
  | _ -> Ok M.Killed

(* Scratch: one directory for the whole loop, one subdirectory per child,
   removed by the PARENT — a child killed at the deadline never runs its
   own cleanup, and an orphaned capture tree under the system temporary
   directory is exactly the trace Law 16(e) forbids. *)

(* [Unix.mkdir] with an EEXIST retry, never [mkdir_p]: the loop must OWN
   this directory, not adopt whatever is at a predictable path in a
   world-writable one. [mkdir_p] succeeds on an existing entry and follows
   a symbolic link, so a planted [/tmp/windtrap-mutate-<pid>] would send
   every child's capture logs somewhere the parent then declines to
   remove; [mkdir] fails with EEXIST on a symlink as it does on a
   directory. This is [Run.temp_root]'s pattern, for its reasons. *)
let scratch_root () =
  let base = Filename.get_temp_dir_name () in
  let pid = Unix.getpid () in
  let rec create n =
    let candidate = Filename.concat base (spf "windtrap-mutate-%d-%d" pid n) in
    match Unix.mkdir candidate 0o700 with
    | () -> candidate
    | exception Unix.Unix_error (Unix.EEXIST, _, _) when n < 64 -> create (n + 1)
    | exception Unix.Unix_error (e, _, _) ->
        raise
          (Supervision
             (spf "could not create the loop's scratch directory: %s"
                (Unix.error_message e)))
  in
  create 0

(* Report data *)

(* Sources, best effort: a survivor whose file cannot be read still names
   its line in the head row. Recorded paths are workspace-relative, as
   coverage's are, so they resolve against the project root — under
   [dune runtest] the cwd is inside _build, where they never open. *)
let read_source =
  let cache = Hashtbl.create 16 in
  let read path =
    match open_in_bin path with
    | exception Sys_error _ -> None
    | ic ->
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () ->
            match really_input_string ic (in_channel_length ic) with
            | contents -> Some contents
            | exception End_of_file -> None)
  in
  fun file ->
    match Hashtbl.find_opt cache file with
    | Some contents -> contents
    | None ->
        let resolved =
          match Path_ops.project_root () with
          | root -> Path_ops.reconstruct ~root file
          | exception Sys_error _ -> Error file
        in
        let contents =
          match resolved with
          | Ok absolute -> (
              match read absolute with Some _ as s -> s | None -> read file)
          | Error _ -> read file
        in
        Hashtbl.add cache file contents;
        contents

(* Other executables' verdict files beside this one's: the numbers are
   then this executable's view of the code it links, and the summary line
   scopes itself and points at the merge instead of posing as the total —
   coverage's wording, not a second one. *)
let has_siblings path =
  let dir = Filename.dirname path in
  match Sys.readdir dir with
  | exception Sys_error _ -> false
  | entries ->
      Array.exists
        (fun entry ->
          Filename.check_suffix entry ".mutants"
          && Filename.concat dir entry <> path)
        entries

let witness_locations tests =
  let table = Hashtbl.create 256 in
  List.iter
    (fun (case : Test_tree.case) ->
      Hashtbl.replace table
        (Test_tree.path_to_string case.Test_tree.path)
        case.Test_tree.loc)
    (Test_tree.flatten tests);
  table

(* The report, from the verdict collection and nothing else — which is
   why the records carry the renderings: this projection is the one
   [windtrap mutate] makes over files it did not write, so the two
   reports cannot drift in DATA the way Law 12's shared renderer already
   stops them drifting in layout. The two callers differ in exactly two
   things, and they are the parameters: where a source file is found
   (a project root here, a roots list there) and whether a witness can
   name its declaration site (the test tree is linked here and nowhere
   in the merge command). *)
let render_data ~resolve_source ~loc_of ~duration ~seed ~siblings ~total t =
  let records = M.records t in
  let survivor_of (r : M.record) witnesses : Render.survivor =
    {
      (* The identifier is spelled here, with the runtime's own function:
         Render carries it into the head row and the arm hint without
         re-spelling it. *)
      Render.id = M.id_to_string r.M.id;
      file = r.M.id.M.file;
      line = r.M.id.M.line;
      before = r.M.before;
      after = r.M.after;
      source = resolve_source r.M.id.M.file;
      witnesses =
        List.map
          (fun path ->
            let test = Test_tree.path_to_string path in
            { Render.test; loc = loc_of test })
          witnesses;
    }
  in
  let survivors =
    List.filter_map
      (fun (r : M.record) ->
        match r.M.verdict with
        | M.Survived { witness; others } ->
            Some (survivor_of r (witness :: others))
        | M.Killed | M.Unreached -> None)
      records
  in
  (* Ordered by reaching-test count descending: the survivor the most
     tests watched is the one whose block a reader can act on soonest.
     [List.stable_sort] keeps identifier order within a count. *)
  let survivors =
    List.stable_sort
      (fun (a : Render.survivor) (b : Render.survivor) ->
        compare (List.length b.witnesses) (List.length a.witnesses))
      survivors
  in
  let unreached =
    List.filter (fun (r : M.record) -> r.M.verdict = M.Unreached) records
  in
  let by_file = Hashtbl.create 16 in
  List.iter
    (fun (r : M.record) ->
      let file = r.M.id.M.file in
      let prior = Option.value ~default:[] (Hashtbl.find_opt by_file file) in
      Hashtbl.replace by_file file (r.M.id.M.line :: prior))
    unreached;
  let unreached_lines =
    Hashtbl.fold
      (fun file lines acc ->
        { Render.file; lines = List.sort_uniq compare lines } :: acc)
      by_file []
    |> List.sort (fun (a : Render.unreached) b -> compare a.file b.file)
  in
  {
    (* The arming variable, spelled with the runtime's own function: the
       report and the runtime cannot disagree about what to type. *)
    Render.arm_variable = M.arm_variable;
    survivors;
    unreached = unreached_lines;
    unreached_total = List.length unreached;
    killed =
      List.length
        (List.filter
           (fun (r : M.record) ->
             match r.M.verdict with M.Killed -> true | _ -> false)
           records);
    total;
    duration;
    seed;
    siblings;
  }

(* The determinism probe

   One unarmed fork over exactly the tests the dry run executed. Its line
   is three counts and the indices of any counted failure, which is all
   the parent needs — it holds the names. Disagreement aborts: mutation
   results over a non-deterministic suite are not a weaker number, they
   are not a number. *)

let probe_line ~armed ~paths ~spine tests (_ : Unix.file_descr) =
  child_prologue ~armed;
  match Driver.execute ~allowlist:(allowlist_of paths) spine tests with
  | Error error -> "error " ^ one_line (Runner.startup_message error)
  | Ok outcome ->
      (* Test rows only: the probe's counts answer "did the same tests run
         the same way", and a verdict row (a failed release) is not a
         test. *)
      let results =
        List.filter executed_test (Run.results outcome.Runner.run)
      in
      let skipped =
        List.length
          (List.filter
             (fun (r : Run.result) ->
               match r.Run.outcome with Failure.Skip _ -> true | _ -> false)
             results)
      in
      let failures =
        List.filter
          (fun (r : Run.result) ->
            r.Run.counted
            && match r.Run.outcome with Failure.Fail _ -> true | _ -> false)
          results
      in
      String.concat " "
        (spf "probe %d %d %d" (List.length results) skipped
           (List.length failures)
        :: List.map
             (fun (r : Run.result) ->
               string_of_int
                 (Option.value ~default:(-1) (index_of r.Run.path paths)))
             failures)

let check_determinism ~armed ~scratch ~dry_run_wall
    ~(spine : Driver.t) ~reach ~paths tests =
  let log_dir = Filename.concat scratch "probe" in
  let child =
    {
      spine with
      Driver.config =
        Run.for_subset spine.Driver.config ~log_dir ~bail:None;
    }
  in
  (* The probe re-runs exactly the dry run's executed tests, so its
     deadline is the same formula over the same schedule. *)
  let { line; status; killed } =
    fork_child
      ~deadline:(child_deadline ~dry_run_wall ~reach paths)
      (probe_line ~armed ~paths ~spine:child tests)
  in
  Run.remove_tree log_dir;
  let named indices =
    List.filter_map
      (fun index ->
        match int_of_string_opt index with
        | Some i when i >= 0 && i < List.length paths ->
            Some (Test_tree.path_to_string (List.nth paths i))
        | _ -> None)
      indices
  in
  let disagreement executed skipped failed indices =
    spf
      "the suite is not deterministic: the dry run executed %d test(s), \
       skipping %d and failing none; the probe executed %d, skipping %d and \
       failing %d%s. Mutation results over a non-deterministic suite are not a \
       weaker number, they are not a number"
      (List.length paths) reach.skipped executed skipped failed
      (match named indices with
      | [] -> ""
      | names -> " (" ^ String.concat ", " names ^ ")")
  in
  match killed with
  | `Deadline ->
      Error
        "the determinism probe exceeded its deadline: the dry run completed \
         and its unarmed re-run did not. Mutation results over a \
         non-deterministic suite are not a weaker number, they are not a \
         number"
  | `No -> (
      match (status, String.split_on_char ' ' (String.trim line)) with
    | _, "error" :: rest ->
        Error
          (spf "the determinism probe refused to run: %s"
             (String.concat " " rest))
    | Unix.WEXITED 0, "probe" :: executed :: skipped :: failed :: indices -> (
        match
          ( int_of_string_opt executed,
            int_of_string_opt skipped,
            int_of_string_opt failed )
        with
        | Some executed, Some skipped, Some failed ->
            if
              executed <> List.length paths
              || skipped <> reach.skipped || failed <> 0
            then Error (disagreement executed skipped failed indices)
            else Ok ()
        | _ -> Error "the determinism probe reported an unreadable result")
      | _ -> Error "the determinism probe died without reporting a result")

(* The bracket both loops open before their first fork: one scratch root
   for the run, removed however the run leaves, and the determinism probe
   in front — a step added here is added to both, which is what stops the
   survey and admission from disagreeing about what a fork is preceded
   by. *)
let probed ~armed ~dry_run_wall ~spine ~reach ~paths tests forks =
  let scratch = scratch_root () in
  Fun.protect
    ~finally:(fun () -> Run.remove_tree scratch)
    (fun () ->
      match
        check_determinism ~armed ~scratch ~dry_run_wall ~spine ~reach ~paths
          tests
      with
      | Error _ as error -> error
      | Ok () -> Ok (forks ~scratch))

(* One mutant *)

let mutant_line ~armed ~paths ~budget ~spine ~(mutant : M.mutant) tests
    (_ : Unix.file_descr) =
  child_prologue ~armed;
  match M.arm ~budget mutant.M.id with
  | Error error ->
      "error " ^ one_line (Format.asprintf "%a" M.pp_arm_error error)
  | Ok _ -> (
      (* After arming, so the runaway budget measures the child's own hits
         and not the dry run's accumulated ones. *)
      M.reset_reach ();
      match Driver.execute ~allowlist:(allowlist_of paths) spine tests with
      | Error error -> "error " ^ one_line (Runner.startup_message error)
      | Ok outcome -> encode_outcome ~paths outcome)

(* The runaway budget: the dry run's hit count with room to spare. A
   drained count under-reports (a site a test's teardown evaluates again
   is not marked twice), so the headroom is not decoration. *)
let budget_of hits =
  if hits > (max_int - 1000) / 8 then max_int else (hits * 8) + 1000

let run_mutant ~armed ~scratch ~dry_run_wall ~index
    ~(spine : Driver.t) ~reach ~paths ~budget ~mutant tests =
  let log_dir = Filename.concat scratch (spf "m%d" index) in
  let child =
    {
      spine with
      Driver.config =
        Run.for_subset spine.Driver.config ~log_dir ~bail:(Some 1);
    }
  in
  let { line; status; killed } =
    fork_child
      ~deadline:(child_deadline ~dry_run_wall ~reach paths)
      (mutant_line ~armed ~paths ~budget ~spine:child ~mutant tests)
  in
  Run.remove_tree log_dir;
  match killed with
  | `Deadline ->
      (* The suite noticed the change by hanging: a kill, on the crash
         kill's own reasoning. Whatever reached the pipe first is not a
         verdict — the child did not finish. *)
      M.Killed
  | `No -> (
      match decode_verdict ~paths line with
      | Error message ->
          raise
            (Supervision (spf "%s: %s" (M.id_to_string mutant.M.id) message))
      | Ok verdict -> (
          (* A child that did not leave through [Unix._exit 0] did not
             report: whatever reached the pipe is not a verdict. *)
          match status with
          | Unix.WEXITED 0 -> verdict
          | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> M.Killed))

(* The fork loop

   Most-reached mutant first, and its child is the forced-fail check: a
   mutant many tests run is the one least likely to survive a correctly
   instrumented build, so a survivor there is evidence about the build and
   not about the suite. Its verdict is kept either way, which is what
   makes the check cost nothing for a build that passes it. *)

let run_children ~armed ~scratch ~dry_run_wall ~spine ~reach ~ordered
    tests =
  let verdicts = ref M.empty in
  let forced_fail = ref None in
  let record (mutant : M.mutant) verdict =
    verdicts := M.add !verdicts (M.record_of_mutant mutant verdict)
  in
  let rec go index = function
    | [] -> (!verdicts, !forced_fail)
    | mutant :: rest ->
        let paths = reaching_tests reach mutant in
        let budget = budget_of (site_hits reach mutant) in
        let verdict =
          run_mutant ~armed ~scratch ~dry_run_wall ~index ~spine ~reach ~paths
            ~budget ~mutant tests
        in
        record mutant verdict;
        (match verdict with
        | M.Survived _ when index = 0 ->
            forced_fail := Some (M.id_to_string mutant.M.id, List.length paths)
        | M.Survived _ | M.Killed | M.Unreached -> ());
        go (index + 1) rest
  in
  go 0 ordered

(* The report *)

(* Which knobs DESIGNATE, which is admission's question (Law 17a): the
   selections that name tests — a filter, an exclude, a tag selection, a
   [--failed] rerun, an in-source focus (the runner's own finding, so the
   two cannot disagree about what focus means). An absent selection is a
   designation too: every test the run executed. *)
let designates ~(config : Run.config) ~focus =
  config.Run.filter <> None
  || config.Run.exclude <> None
  || config.Run.tags <> []
  || config.Run.exclude_tags <> []
  || config.Run.failed_only
  || focus

(* Whether the run's selection NARROWS THE SUITE, which is the survey's
   question: a designation, or the one knob that narrows work without
   naming tests — the documented relationship between the two, spelled as
   one. A narrowed run's verdicts are relative to its
   selection — a mutant only deselected tests reach records Unreached, a
   survivor survived only the selection — and the file format carries no
   partial-run marking, so a written file would stand in the project
   merge as this executable's whole answer until the next full run.
   WINDTRAP_MUTATE_ONLY is deliberately in neither: the scope narrows
   which mutants exist, not which tests judge them, so an ONLY-scoped
   run's records are project-true for this executable, merely narrower. *)
let narrows_suite ~(config : Run.config) ~focus =
  designates ~config ~focus || config.Run.shard <> None

let write_verdicts verdicts =
  let exe = Sys.executable_name in
  let path = M.output_file ~exe in
  (try M.save ?identity:(M.writer_identity ~exe) path verdicts
   with Sys_error message ->
     note "could not write the verdict file: %s" message);
  path

let print_report renderer ~population ~verdicts ~duration ~seed ~siblings tests
    =
  let locations = witness_locations tests in
  Render.mutation_report renderer
    (render_data ~resolve_source:read_source
       ~loc_of:(fun test -> Option.join (Hashtbl.find_opt locations test))
       ~duration:(Some duration) ~seed:(Some seed) ~siblings
       ~total:(List.length population) verdicts)

(* The runtime applies the scope at registration, so a value matching
   nothing leaves the same empty catalogue a missing backend does — a
   refusal blaming instrumentation would send the reader to rebuild a
   build that is fine. The runtime read the variable itself, at module
   load; [Env] reads the same process's environment, so its answer is the
   value registration saw. *)
let refuse_empty_catalogue () =
  match Env.mutate_only () with
  | [] ->
      refuse
        "this executable links no instrumented module, so there is nothing to \
         mutate. Build it with --instrument-with ppx_windtrap.mutate, or add \
         an (instrumentation (backend ppx_windtrap.mutate)) stanza to the \
         library under test"
  | prefixes ->
      refuse
        "%s=%s left no mutants in this executable's catalogue — the prefix \
         matches no instrumented file, or the matched files have no mutation \
         sites"
        M.scope_variable
        (String.concat "," prefixes)

(* The loop, end to end *)

let loop renderer ~armed (spine : Driver.t) tests =
  let config = spine.Driver.config in
  let reach = fresh_reach () in
  let started = Unix.gettimeofday () in
  match Driver.execute_and_report ~on_event:(observe reach) spine tests with
  (* The startup message is already on stderr; a refused run never
     produced a number. *)
  | Error _ -> Reported 1
  | Ok outcome -> (
      (* The last test's teardown window. *)
      ignore (M.drain ());
      let executed = List.rev reach.executed in
      if outcome.Runner.exit_code = 2 then
        refuse "no test ran, so there is nothing to mutate"
      else if outcome.Runner.exit_code <> 0 then
        refuse
          "the dry run is red. Mutation scores a passing suite; a score over a \
           failing one is not a score"
      else if Lazy.force catalogue = [] then refuse_empty_catalogue ()
      else
        let population =
          List.filter
            (fun (m : M.mutant) -> m.M.dismissed = None)
            (Lazy.force catalogue)
        in
        if population = [] then
          refuse
            "every mutant in this executable is dismissed by [@mutate off] — \
             there is nothing to test"
        else
          let reached, unreached =
            List.partition
              (fun mutant -> reaching_tests reach mutant <> [])
              population
          in
          let ordered =
            List.stable_sort
              (fun a b ->
                let count m = List.length (reaching_tests reach m) in
                match compare (count b) (count a) with
                | 0 -> M.compare_mutant a b
                | c -> c)
              reached
          in
          let dry_run_wall =
            Float.max 0.01 (Unix.gettimeofday () -. started)
          in
          let narrowed =
            narrows_suite ~config ~focus:outcome.Runner.focus_active
          in
          let outcome =
            probed ~armed ~dry_run_wall ~spine ~reach ~paths:executed tests
              (fun ~scratch ->
                run_children ~armed ~scratch ~dry_run_wall ~spine ~reach
                  ~ordered tests)
          in
          match outcome with
          | Error message -> refuse "%s" message
          | Ok (reported, forced_fail) ->
              let verdicts =
                List.fold_left
                  (fun acc (m : M.mutant) ->
                    M.add acc (M.record_of_mutant m M.Unreached))
                  reported unreached
              in
              (* A narrowed run's verdicts are never persisted — and the
                 previous file is left alone, never deleted: the run says
                 so instead, after the report it still prints in full. *)
              let path =
                if narrowed then M.output_file ~exe:Sys.executable_name
                else write_verdicts verdicts
              in
              Option.iter
                (fun (id, tests) ->
                  Render.mutation_forced_fail renderer ~id ~tests)
                forced_fail;
              print_report renderer ~population ~verdicts
                ~duration:(Unix.gettimeofday () -. started)
                ~seed:config.Run.seed ~siblings:(has_siblings path) tests;
              if narrowed then Render.mutation_not_saved renderer;
              flush_descriptors ();
              Reported 0)

(* Admission (WINDTRAP_MUTATE=admit)

   "A test is justified by the fault it kills", as one command over the
   run's ordinary selection. The survey's spine is reused whole — the dry
   run and its reach map, the determinism probe, the fork-and-arm child,
   the per-child deadline — and only the scheduling policy differs: the
   loop arms the faults the selected tests reach, stops as soon as every
   selected test has killed one, and rules per TEST, not per mutant.
   Nothing is persisted (Law 17b): early stop means most reached mutants
   were never tried, and a written file would either fabricate or
   mislabel — the durable artifact of admission is the strengthened test.
   The forced-fail check is disabled here (Law 16e's admit clause): a
   surviving first fork is the expected signature of a weak test, not a
   broken build, and the genuinely-uninstrumented case still refuses
   upstream. *)

let rec first_n n = function
  | [] -> []
  | _ when n <= 0 -> []
  | x :: rest -> x :: first_n (n - 1) rest

(* Faults listed inside one UNJUSTIFIED ruling. Derived, never a knob:
   the block's job is to show what the test watched and did not notice,
   and the first few are the most-run ones — a reader who needs the rest
   strengthens the test, which is what the block asks for anyway. The
   count that matters, [tried], is stated in the sentence above them. *)
let listed_faults = 3

(* A test's own candidate list: the undismissed faults on lines it
   reaches, ordered by ITS OWN hit count at each site — most-run first —
   so co-selected tests can never starve each other's windows on shared
   faults no selected test can kill. Ties fall back to identifier order,
   so two runs of one binary schedule identically. *)
let candidates_of reach path =
  let mine =
    Hashtbl.fold
      (fun _ site acc ->
        if site.mutant.M.dismissed <> None then acc
        else
          match List.assoc_opt path site.tests with
          | Some hits -> (site.mutant, hits) :: acc
          | None -> acc)
      reach.sites []
  in
  List.map fst
    (List.sort
       (fun (a, ha) (b, hb) ->
         match compare hb ha with 0 -> M.compare_mutant a b | c -> c)
       mine)

(* One member of the working set U. [own] is the candidate list after the
   TRY cap; [reach_count] is the list before it, which is what the ruling
   sentence owes the reader when the cap bit. [tried_rev] advances only
   for faults on [own] under which the test ran to a pass outcome — a
   fail admits instead, a skip watched nothing, and a shared fork the
   test merely rode along in charges nothing (Law 17c). *)
type entrant = {
  test : string list;
  own : M.mutant list;
  reach_count : int;
  mutable tried_rev : M.mutant list;
  mutable ruling : ruling option;
}

and ruling =
  | Admitted of {
      fault : M.mutant;
      cause : [ `Failure | `Fixture | `Crashed | `Timed_out ];
    }
  | Unjustified of { watched : M.mutant list; capped : bool }

(* The batch child: one Law-16 child — one mutant, armed, announced to
   the parent by its verdict lines, read-only checking, [Unix._exit] —
   that runs the mutant's reaching members of U WITHOUT bail. Per-test
   credit demands no-bail: with it, a mutant reached by two members
   credits only the first, and the second can be ruled unjustified for
   want of a kill it owns. The pipe protocol is incremental — [t <i>
   start] before each test, [t <i> pass|fail|skip] after, a terminal
   [done] — so a crash or hang is attributable to the one index that
   started without an outcome while every earlier outcome is kept. *)
let admit_line ~armed ~paths ~budget ~spine ~(mutant : M.mutant) tests fd =
  child_prologue ~armed;
  match M.arm ~budget mutant.M.id with
  | Error error ->
      "error " ^ one_line (Format.asprintf "%a" M.pp_arm_error error)
  | Ok _ -> (
      (* After arming, so the runaway budget measures the child's own hits
         and not the dry run's accumulated ones. *)
      M.reset_reach ();
      (* A write that fails must not abort the run from inside the
         observer: the parent holding the read end is the only reader,
         and if it is gone the child's report is moot anyway. *)
      let emit line = try write_all fd line with Unix.Unix_error _ -> () in
      let event = function
        | Runner.Test_started { path } -> (
            match index_of path paths with
            | Some i -> emit (spf "t %d start" i)
            | None -> ())
        | Runner.Test_finished (result : Run.result) -> (
            match index_of result.Run.path paths with
            | None -> ()
            | Some i ->
                let word =
                  match result.Run.outcome with
                  | Failure.Pass -> "pass"
                  | Failure.Skip _ -> "skip"
                  | Failure.Fail failures -> (
                      (* Every batch member is a designated test, so a
                         [Fail] here always counted. The cause travels
                         with the outcome: a failure in the test's own
                         setup or teardown is a kill through a dependency
                         the test declared, and the witness says so. *)
                      match failures with
                      | { Failure.phase = Failure.Setup | Failure.Teardown; _ }
                        :: _ ->
                          "fail fixture"
                      | _ -> "fail")
                in
                emit (spf "t %d %s" i word))
        | Runner.Run_started _ | Runner.Fixture_release _ -> ()
      in
      match
        Driver.execute ~on_event:event ~allowlist:(allowlist_of paths) spine
          tests
      with
      | Error error -> "error " ^ one_line (Runner.startup_message error)
      | Ok (_ : Runner.outcome) -> "done")

(* What one batch child said, per index. [None] is a test that reported
   no outcome — never started, or in flight when the child died — and
   nothing is ever credited to it. *)
let parse_batch_events ~size lines =
  let outcomes = Array.make size None in
  let pending = ref None in
  let finished = ref false in
  let error = ref None in
  let index i =
    match int_of_string_opt i with
    | Some i when i >= 0 && i < size -> Some i
    | _ -> None
  in
  List.iter
    (fun line ->
      match String.split_on_char ' ' line with
      | [ "t"; i; "start" ] -> pending := index i
      | [ "t"; i; "pass" ] ->
          Option.iter (fun i -> outcomes.(i) <- Some `Pass) (index i);
          pending := None
      | [ "t"; i; "skip" ] ->
          Option.iter (fun i -> outcomes.(i) <- Some `Skip) (index i);
          pending := None
      | [ "t"; i; "fail" ] ->
          Option.iter
            (fun i -> outcomes.(i) <- Some (`Fail `Failure))
            (index i);
          pending := None
      | [ "t"; i; "fail"; "fixture" ] ->
          Option.iter
            (fun i -> outcomes.(i) <- Some (`Fail `Fixture))
            (index i);
          pending := None
      | [ "done" ] -> finished := true
      | "error" :: rest -> error := Some (String.concat " " rest)
      (* The wrapper's own "crashed", a torn line: not a report. *)
      | _ -> ())
    lines;
  (outcomes, !pending, !finished, !error)

let run_batch ~armed ~scratch ~dry_run_wall ~index ~(spine : Driver.t)
    ~reach ~batch ~budget ~mutant tests =
  let log_dir = Filename.concat scratch (spf "a%d" index) in
  let child =
    {
      spine with
      Driver.config =
        Run.for_subset spine.Driver.config ~log_dir ~bail:None;
    }
  in
  let paths = List.map (fun e -> e.test) batch in
  let { lines; status; killed; _ } =
    fork_child
      ~deadline:(child_deadline ~dry_run_wall ~reach paths)
      (admit_line ~armed ~paths ~budget ~spine:child ~mutant tests)
  in
  Run.remove_tree log_dir;
  let outcomes, pending, finished, error =
      parse_batch_events ~size:(List.length batch) lines
    in
    match error with
    | Some message ->
        raise
          (Supervision (spf "%s: %s" (M.id_to_string mutant.M.id) message))
    | None ->
        (* A child that did not leave through [Unix._exit 0] after its
           terminal line did not finish: the fault is attributed to the
           one test that started and never reported — a hang or crash
           under a fault is a detected fault — and every earlier outcome
           in the buffer is kept. A deadline kill is the hang's name for
           it; anything else that stopped the child is a crash. *)
        (match (finished, status) with
        | true, Unix.WEXITED 0 -> ()
        | _, _ -> (
            match pending with
            | Some i when outcomes.(i) = None ->
                outcomes.(i) <-
                  Some
                    (`Fail
                       (if killed = `Deadline then `Timed_out else `Crashed))
            | Some _ | None -> ()));
        outcomes

(* The admission machine: union-scheduled batches over U, one fork per
   scheduled fault, early stop when U empties. The next fault is the one
   the most still-unruled members carry on their own lists — capped and
   ruled tests have left U and stop weighting the schedule — with the
   site's total hit count and then identifier order as tie-breaks. *)
let run_admission ~armed ~scratch ~dry_run_wall ~spine ~reach
    ~entrants tests =
  let forked = Hashtbl.create 64 in
  let forks = ref 0 in
  let live () = List.filter (fun e -> e.ruling = None) entrants in
  let reaches e (mutant : M.mutant) =
    match Hashtbl.find_opt reach.sites mutant.M.id with
    | None -> false
    | Some site -> List.mem_assoc e.test site.tests
  in
  let on_own e (mutant : M.mutant) =
    List.exists (fun m -> M.compare_mutant m mutant = 0) e.own
  in
  let rule_exhausted e =
    if
      e.ruling = None
      && List.for_all (fun (m : M.mutant) -> Hashtbl.mem forked m.M.id) e.own
    then
      e.ruling <-
        Some
          (Unjustified
             {
               watched = List.rev e.tried_rev;
               capped = e.reach_count > List.length e.own;
             })
  in
  let next_fault u =
    let remaining =
      List.sort_uniq M.compare_mutant
        (List.concat_map
           (fun e ->
             List.filter
               (fun (m : M.mutant) -> not (Hashtbl.mem forked m.M.id))
               e.own)
           u)
    in
    let weight m = List.length (List.filter (fun e -> on_own e m) u) in
    List.fold_left
      (fun best m ->
        match best with
        | None -> Some m
        | Some b ->
            let by_weight = compare (weight m) (weight b) in
            let by_hits = compare (site_hits reach m) (site_hits reach b) in
            if
              by_weight > 0
              || (by_weight = 0 && by_hits > 0)
              || (by_weight = 0 && by_hits = 0 && M.compare_mutant m b < 0)
            then Some m
            else best)
      None remaining
  in
  let rec go index =
    match live () with
    | [] -> !forks
    | u -> (
        match next_fault u with
        (* Every live member has an unforked own fault (exhaustion rules
           immediately), so an empty pick cannot happen; ruling what is
           left keeps the loop total anyway. *)
        | None ->
            List.iter rule_exhausted u;
            !forks
        | Some mutant ->
            let batch = List.filter (fun e -> reaches e mutant) u in
            let budget = budget_of (site_hits reach mutant) in
            let outcomes =
              run_batch ~armed ~scratch ~dry_run_wall ~index ~spine ~reach
                ~batch ~budget ~mutant tests
            in
            incr forks;
            Hashtbl.replace forked mutant.M.id ();
            List.iteri
              (fun i e ->
                match outcomes.(i) with
                | Some (`Fail cause) ->
                    (* A kill admits wherever it was observed — a
                       ride-along kill included — and a try is never
                       charged for a fork the test merely rode. *)
                    e.ruling <- Some (Admitted { fault = mutant; cause })
                | Some `Pass ->
                    if on_own e mutant then e.tried_rev <- mutant :: e.tried_rev
                | Some `Skip | None -> ())
              batch;
            List.iter rule_exhausted u;
            go (index + 1))
  in
  go 0

(* The admission report's data, from the rulings and the reach map. *)

let fault_of (m : M.mutant) : Render.fault =
  {
    Render.fault_id = M.id_to_string m.M.id;
    fault_file = m.M.id.M.file;
    fault_line = m.M.id.M.line;
    fault_before = m.M.before;
    fault_after = m.M.after;
    fault_source = read_source m.M.id.M.file;
  }

let admit_loop renderer ~armed (spine : Driver.t) ~tries tests =
  let config = spine.Driver.config in
  let reach = fresh_reach () in
  let started = Unix.gettimeofday () in
  match Driver.execute_and_report ~on_event:(observe reach) spine tests with
  (* The startup message is already on stderr; a refused run never
     produced a verdict. *)
  | Error _ -> Reported 1
  | Ok outcome ->
      (* The last test's teardown window. *)
      ignore (M.drain ());
      let executed = List.rev reach.executed in
      (* Designation is the run's own selection, and an absent one names
         every test the run executed — the author asking for all of them,
         which an alias can spell where a per-invocation filter cannot. *)
      let selection = designates ~config ~focus:outcome.Runner.focus_active in
      if outcome.Runner.exit_code = 2 then (
        match spine.Driver.invocation with
        | `Exe _ ->
            (* The author named one binary, so a run that admits nothing
               here is a mistake, and it is refused as one. Without a
               selection there is no filter to fix: the run itself
               executed nothing, and the message says that instead. *)
            if selection then
              refuse
                "the selection matches no test, so there is no test to \
                 admit. Fix the filter, or run the suite that declares the \
                 test"
            else
              refuse
                "admit judges the tests this run executes and this run \
                 executed none, so there is no test to admit: the suite \
                 declares no test, or every declared test was dropped \
                 before running — a tag the default predicate drops, or an \
                 empty shard"
        | `Mirrors ->
            (* The arm precedent's softness (Uncatalogued): one variable
               reaches every partition of a project-wide run, and failing
               the siblings of the suite that owns the selected tests
               would report success as failure. One line on stderr, and
               the ordinary run stands. *)
            note
              "admit: the selection matches no test in this suite, so there \
               is nothing to admit here; the suite that declares the \
               selected tests answers for them";
            Ran (Ok outcome))
      else if outcome.Runner.exit_code <> 0 then
        refuse
          "the dry run is red. Admission judges tests against a green \
           baseline — a failing test has already proved it can fail, and its \
           green co-selected tests get no verdict until it is fixed or \
           deselected"
      else if Lazy.force catalogue = [] then refuse_empty_catalogue ()
      else
        (* The admission set: the selection's executed, counted tests. A
           skip ran nothing and an xfail has already demonstrated it can
           fail; both are visible in the summary line the dry run just
           printed, and neither gets a verdict. *)
        let designated =
          List.filter
            (fun (r : Run.result) ->
              r.Run.xfail = None
              && match r.Run.outcome with Failure.Pass -> true | _ -> false)
            (Run.results outcome.Runner.run)
        in
        if designated = [] then
          refuse
            "every selected test skipped or is marked xfail — a skip ran \
             nothing and an xfail has already proved it can fail — so there \
             is nothing to admit"
        else
          (* A whole-suite admission is a legitimate ask an alias makes,
             and it is also what someone who wanted the survey types by
             mistake. One line, once, naming the other question. *)
          let () =
            if not selection then
              note
                "admitting all %d tests this run executed; the per-mutant \
                 question is WINDTRAP_MUTATE=1"
                (List.length designated)
          in
          (* The partition (Law 17d is a consequence of this shape, not a
             runtime rule): zero reached sites is NO SITES, decided before
             any fork and never consulted by the exit predicate; the rest
             form U. *)
          let reached =
            List.map
              (fun (r : Run.result) ->
                (r.Run.path, candidates_of reach r.Run.path))
              designated
          in
          let no_sites, entrants =
            List.partition_map
              (fun (path, candidates) ->
                if candidates = [] then Either.Left path
                else
                  Either.Right
                    {
                      test = path;
                      own =
                        (if tries > 0 then first_n tries candidates
                         else candidates);
                      reach_count = List.length candidates;
                      tried_rev = [];
                      ruling = None;
                    })
              reached
          in
          (* The summary's denominator: the distinct undismissed faults
             the designated tests reach, which is the union of the lists
             above rather than a second walk of the reach map spelling
             "reaches" its own way. *)
          let reached_total =
            List.length
              (List.sort_uniq M.compare_mutant
                 (List.concat_map snd reached))
          in
          let dry_run_wall =
            Float.max 0.01 (Unix.gettimeofday () -. started)
          in
          let outcome =
            if entrants = [] then
              (* Nothing reaches a site, so there is no verdict for the
                 determinism probe to validate: NO SITES is reported from
                 the dry run alone, with no fork at all. *)
              Ok 0
            else
              probed ~armed ~dry_run_wall ~spine ~reach ~paths:executed tests
                (fun ~scratch ->
                  run_admission ~armed ~scratch ~dry_run_wall ~spine ~reach
                    ~entrants tests)
          in
          match outcome with
          | Error message -> refuse "%s" message
          | Ok forks ->
              let locations = witness_locations tests in
              let loc_of path =
                Option.join
                  (Hashtbl.find_opt locations (Test_tree.path_to_string path))
              in
              let admitted =
                List.filter_map
                  (fun e ->
                    match e.ruling with
                    | Some (Admitted { fault; cause }) ->
                        Some
                          {
                            Render.admitted_test =
                              Test_tree.path_to_string e.test;
                            witness = fault_of fault;
                            cause;
                          }
                    | Some (Unjustified _) | None -> None)
                  entrants
              in
              let unjustified =
                List.filter_map
                  (fun e ->
                    match e.ruling with
                    | Some (Unjustified { watched; capped }) ->
                        Some
                          {
                            Render.unjustified_test =
                              Test_tree.path_to_string e.test;
                            unjustified_loc = loc_of e.test;
                            shown = List.map fault_of (first_n listed_faults watched);
                            tried = List.length watched;
                            candidates = List.length e.own;
                            reached = e.reach_count;
                            capped;
                          }
                    | Some (Admitted _) | None -> None)
                  entrants
              in
              let capped_rulings =
                List.length
                  (List.filter (fun (u : Render.unjustified) -> u.capped)
                     unjustified)
              in
              Render.admission_report renderer
                {
                  (* As the survey's report: the arming variable is the
                     runtime's own spelling. *)
                  Render.admission_arm_variable = M.arm_variable;
                  admitted;
                  unjustified;
                  no_sites =
                    List.map
                      (fun path ->
                        {
                          Render.no_sites_test = Test_tree.path_to_string path;
                          no_sites_loc = loc_of path;
                        })
                      no_sites;
                  designated = List.length designated;
                  admission_forks = forks;
                  admission_reached = reached_total;
                  capped_rulings;
                  tries;
                  admission_duration = Unix.gettimeofday () -. started;
                  admission_seed = spine.Driver.seed;
                  scope =
                    (match Env.mutate_only () with
                    | [] -> None
                    | prefixes ->
                        Some
                          (M.scope_variable ^ "="
                          ^ String.concat "," prefixes));
                };
              flush_descriptors ();
              (* Law 16e's admit clause: completed with no UNJUSTIFIED
                 verdict is 0 — NO SITES alone is never red (Law 17d) —
                 and any UNJUSTIFIED is 1. No verdict file is written and
                 none is read (Law 17b). *)
              Reported (if unjustified = [] then 0 else 1)

(* The two modes that are ordinary runs with something printed around them *)

let discovery_mode renderer spine tests =
  let result = Driver.execute_and_report spine tests in
  (match result with
  | Error _ -> ()
  | Ok _ ->
      (* The population the loop would test, not the catalogue: a
         dismissed mutant is one the reader took out of scope, and a
         discovery line offering to test 187 followed by a report scoring
         184 is a number the reader cannot reconcile. *)
      let mutants =
        List.filter
          (fun (m : M.mutant) -> m.M.dismissed = None)
          (Lazy.force catalogue)
      in
      let files =
        List.sort_uniq compare
          (List.map (fun (m : M.mutant) -> m.M.id.M.file) mutants)
      in
      Render.mutation_discovery renderer ~mutants:(List.length mutants)
        ~files:(List.length files));
  Ran result

let arm_mode renderer ~armed (spine : Driver.t) tests =
  match M.arm_from_env () with
  | Error (M.Uncatalogued _ as error) ->
      (* Not a refusal. One identifier is handed to every test
         executable at once — the report's own remedy is
         [WINDTRAP_MUTATE_ARM=<id> dune runtest --instrument-with
         ppx_windtrap.mutate], because a command that links no test
         executable has no single binary to name — and in a project with
         several (test) stanzas most of them were built from other
         sources. An executable that catalogues no site of the named file
         is simply not the one the identifier is about: exiting 1 here
         would fail the build for every sibling of the binary that armed
         the mutant correctly, which is the report's headline advice
         reporting failure when it works. Said once, on stderr, and
         nothing is concealed by running on: no verdict is produced here
         either way. A stale or misspelled identifier still names a
         catalogued file, comes back [Unmatched] below, and still
         refuses.

         What follows is the run this process would have made with the
         variable unset: the discovery line prints nothing when there is
         nothing to discover, so an uninstrumented sibling is left with
         its ordinary transcript and one line of stderr. *)
      note "%s" (Format.asprintf "%a" M.pp_arm_error error);
      discovery_mode renderer spine tests
  | Error error ->
      Format.eprintf "%a@." M.pp_arm_error error;
      Reported 1
  (* The caller only reaches here with the variable set, so this arm is
     the empty-string case the runtime already treats as unset. *)
  | Ok None -> discovery_mode renderer spine tests
  | Ok (Some mutant) ->
      (* An armed run never writes: no .corrected (Law 16d) and no
         accepted baseline. An armed mutant changes program output on
         purpose, and a run that promoted that output would rewrite the
         source tree from a lie. Snapshots need no flag beyond No_update:
         Snapshot maps it to Mode Check, and the write is reachable only
         under Mode Update. *)
      armed ();
      let spine =
        {
          spine with
          Driver.config =
            {
              spine.Driver.config with
              Run.update = Env.No_update;
            };
        }
      in
      Render.mutation_armed renderer
        ~id:(M.id_to_string mutant.M.id)
        ~before:mutant.M.before ~after:mutant.M.after;
      flush_descriptors ();
      (* After arming, so the closing line counts the run's own
         evaluations and not module initialization's — that window ran
         before the arming and evaluated nothing mutated. *)
      M.reset_reach ();
      let result = Driver.execute_and_report spine tests in
      (* [killed_by], not [exit_code <> 0]: a filter that matched nothing
         exits 2, and announcing [mutant killed.] there would report a
         selection mistake as a detected behaviour change (Law 16c). A
         completed run that killed nothing gets the other half of the
         verdict: green alone cannot tell "the tests prove nothing about
         this site" from "no selected test ran the line", so the closing
         line says which — except on exit 2, where the run made no claim
         about the mutant at all. *)
      (match result with
      | Ok outcome when killed_by outcome -> Render.mutation_killed renderer
      | Ok outcome when outcome.Runner.exit_code <> 2 -> (
          match M.armed_hits () with
          | 0 -> Render.mutation_not_evaluated renderer
          | hits -> Render.mutation_survived renderer ~hits)
      | Ok _ | Error _ -> ());
      flush_descriptors ();
      Ran result

(* Entry *)

let execute_and_report (spine : Driver.t) tests =
  (* What a process about to run with a mutant armed owes the inline
     runtime (Law 16d): the hooks registered in the registry, fired in
     registration order — in each forked child before its first test, and
     once in the parent under WINDTRAP_MUTATE_ARM; never by a run that
     arms nothing. Read at fire time, not captured here: registration is
     a module-load act and the loop must honor every hook the link
     produced, however the initializers were ordered. *)
  let armed () = List.iter (fun hook -> hook ()) (Registry.armed_hooks ()) in
  (* A listing is not a run: nothing executes, so there is nothing to
     observe, announce or mutate. Everything else goes through the knobs,
     instrumented or not — a variable the user set and misspelled must be
     loud in every build, and an identifier that names a site of a file
     this build does catalogue and matches none of them comes back
     [Unmatched] with the file's candidates, which is the whole
     diagnosis. *)
  if spine.Driver.config.Run.list_only then
    Ran (Driver.execute_and_report spine tests)
  else
    match Cli.mutation () with
    | Error error ->
        note "%s" (Cli.error_message error);
        Reported 1
    | Ok { Cli.mode; arm; tries } -> (
        let renderer () =
          Driver.renderer ~render:spine.Driver.render ~mode:spine.Driver.output
            ~invocation:spine.Driver.invocation ()
        in
        match (mode, arm) with
        | `Unset, None ->
            (* The one path an uninstrumented build must not pay for. *)
            if instrumented () then discovery_mode (renderer ()) spine tests
            else Ran (Driver.execute_and_report spine tests)
        | `Off, None ->
            (* [off] is the answer to the discovery line, which is the
               only thing an unasked mutation build says. An arming is
               still an explicit ask and still honoured below. *)
            Ran (Driver.execute_and_report spine tests)
        | (`Unset | `Off), Some _ -> arm_mode (renderer ()) ~armed spine tests
        | (`Loop | `Admit), Some _ ->
            refuse
              "WINDTRAP_MUTATE and %s ask for different runs — the loop arms \
               each mutant itself, so an armed parent would mutate its own dry \
               run. Unset one"
              M.arm_variable
        | `Loop, None -> (
            if Sys.win32 then
              refuse
                "mutation testing needs Unix.fork, which Windows does not \
                 have; the tests themselves still ran"
            else
              try loop (renderer ()) ~armed spine tests
              with Supervision message -> refuse "%s" message)
        | `Admit, None -> (
            if Sys.win32 then
              refuse
                "mutation testing needs Unix.fork, which Windows does not \
                 have; the tests themselves still ran"
            else
              try admit_loop (renderer ()) ~armed spine ~tries tests
              with Supervision message -> refuse "%s" message))
