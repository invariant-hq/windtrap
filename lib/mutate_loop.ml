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

(* The parent of a mutation run: dry run, probe, fork loop, verdict
   file, report. The runtime below (Windtrap_runtime.Mutate) owns the catalogue,
   the arming slot and the reach counters, and Windtrap_runtime.Verdicts
   owns the verdict collection and its file; this module owns the
   protocol over them and what the report is given: which mutants
   survived, in which order, and their reaching tests. *)

module M = Windtrap_runtime.Mutate
module V = Windtrap_runtime.Verdicts

let spf = Printf.sprintf

(* The scope: [--mutate]'s source-path prefixes, applied here to the
   population the loop forks over. Every instrumented file still registers
   and still counts reaches — the runtime reads no environment and no
   flag — and what narrows is the work: a mutant outside the prefixes is
   never forked and never recorded. *)
let in_scope ~scope (id : M.id) =
  match scope with
  | [] -> true
  | prefixes ->
      List.exists (fun prefix -> String.starts_with ~prefix id.M.file) prefixes

type run = Ran of (Run.outcome, Run.startup_error) result | Reported of int

let note fmt = Printf.ksprintf Os.say fmt

let refuse fmt =
  Printf.ksprintf
    (fun message ->
      note "%s" message;
      Reported 1)
    fmt

let saturating_add x y = if x > max_int - y then max_int else x + y

(* The configuration a loop hands its dry run. A mutation run's output
   never reports a test outcome (its output is the verdict) and its exit code
   is its own, so
   the dry run — whose whole job is to fill the reach map and prove the
   suite green — writes no JUnit. A run with one mutant armed hands the
   caller an ordinary [Ran] outcome and writes its own, exactly as an
   uninstrumented run would. *)
let dry_run (config : Run.config) = { config with Run.junit = None }

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
  mutable tests : string list list; (* reaching tests, reverse order *)
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
  site.tests <- path :: site.tests;
  site.hits <- saturating_add site.hits entry.M.hits

let observe reach (event : Run.event) =
  match event with
  | Run.Run_started _ | Run.Fixture_release _ | Run.Interrupted _ -> ()
  | Run.Test_started _ ->
      ignore (M.drain ());
      M.next_epoch ()
  | Run.Test_finished result ->
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
  | Some site -> List.rev site.tests

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

(* Child hygiene

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

let child_body fd body =
  let line = match body () with line -> line | exception _ -> "crashed" in
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

let domain_refusal =
  "this process has spawned a domain, and OCaml refuses Unix.fork in a process \
   that has: mutation testing runs every mutant in a forked child, so it \
   cannot run in this one. Exclude the tests that spawn a domain (-e) to test \
   the rest"

(* Interruption

   A child is a session of its own, so a terminal's signal reaches the
   parent alone, and a parent that died of it would leave an armed child
   behind with nothing to enforce its deadline. While the loop forks, the
   first of [interrupt_signals] is recorded and kills the running child's
   process group; [fork_child] then stops watching, and the loop reports
   what it has and dies by the signal. The handler prints nothing: it runs
   at a safepoint of whatever the parent was doing, and that can be the
   report.

   SIGPIPE is recorded too: its default action would kill the parent
   inside a write of the report, past every [Fun.protect], and leave the
   scratch root behind. Handled, the write fails with [Sys_error] instead
   and unwinds. *)

type interrupt = {
  mutable signal : int option; (* the first signal received *)
  mutable child : int option; (* the child running, not yet reaped *)
}

let interrupt = { signal = None; child = None }
let interrupt_signals = [ Sys.sigint; Sys.sigterm; Sys.sighup ]

let kill_group pid =
  (* The group does not exist until the child has run [setsid]. *)
  (try Unix.kill (-pid) Sys.sigkill with Unix.Unix_error _ -> ());
  try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ()

(* As [Run.execute] handles them: a signal the process was started
   ignoring stays ignored, the first one puts the default dispositions
   back so that a second kills at once, and a forked child, which
   inherits the handler until it installs its own run's, dies as it would
   have without one. SIGPIPE keeps its handler after the first signal:
   every later write to the reader that left has to fail, not kill. *)
let with_interrupts fn =
  let owner = Unix.getpid () in
  interrupt.signal <- None;
  interrupt.child <- None;
  let handled = Sys.sigpipe :: interrupt_signals in
  let default signals =
    List.iter (fun signal -> Sys.set_signal signal Sys.Signal_default) signals;
    ignore (Unix.sigprocmask Unix.SIG_UNBLOCK signals)
  in
  let handle signal =
    if Unix.getpid () <> owner then begin
      default handled;
      Unix.kill (Unix.getpid ()) signal
    end
    else begin
      default interrupt_signals;
      if Option.is_none interrupt.signal then interrupt.signal <- Some signal;
      Option.iter kill_group interrupt.child
    end
  in
  let previous =
    List.map
      (fun signal -> (signal, Sys.signal signal (Sys.Signal_handle handle)))
      handled
  in
  List.iter
    (fun (signal, behavior) ->
      match behavior with
      | Sys.Signal_ignore -> Sys.set_signal signal behavior
      | Sys.Signal_default | Sys.Signal_handle _ -> ())
    previous;
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun (signal, behavior) -> Sys.set_signal signal behavior)
        previous)
    fn

(* The parent sees a death by signal and no [at_exit] function runs, as
   when a signal stops [Run.execute]. The exit status is what a shell
   reports for the signal, should it not be delivered. *)
let die_by signal =
  Sys.set_signal signal Sys.Signal_default;
  ignore (Unix.sigprocmask Unix.SIG_UNBLOCK [ signal ]);
  Unix.kill (Unix.getpid ()) signal;
  Unix._exit
    (128
    +
    if signal = Sys.sighup then 1
    else if signal = Sys.sigint then 2
    else if signal = Sys.sigpipe then 13
    else 15)

type report = {
  line : string; (* the first complete (newline-terminated) line *)
  status : Unix.process_status;
  killed : [ `No | `Deadline ];
      (* [`Deadline] is the child's own deadline: the mutant is scored
         and the loop goes on. It is the only clock over a child. *)
}

(* One child, one process group, one deadline. [setsid] at fork puts the
   child — and everything a test under it spawns — in a session of its
   own, so an expiry or an interruption kills the lot with one signal to
   the group ([kill_group]); a child left alive would block the drain
   below. A recorded signal is looked for before every [select]: the
   handler may have run where no system call was there to interrupt. The
   parent reads the pipe to EOF before it waits, which is what keeps a
   child writing more than a pipe buffer from deadlocking against a parent
   already in waitpid; [select] is what lets it wake on the deadline while
   it reads. The pipe is close-on-exec: a process a test
   [exec]s must not inherit the write end and hold the drain open past
   its group's death.

   XXX two windows stay open. A second signal between [fork] returning and
   the pid being recorded kills the parent with the child alive, and a
   signal between [waitpid] and the reset of [interrupt.child] is sent to a
   pid that is already reaped. *)
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
  | exception Failure _ ->
      (* OCaml 5's [Unix.fork] refuses a process that has ever spawned a
         domain, joined or not, and says so with this [Failure]. *)
      Unix.close read_fd;
      Unix.close write_fd;
      raise (Supervision domain_refusal)
  | 0 ->
      (try Unix.close read_fd with _ -> ());
      (try ignore (Unix.setsid ()) with _ -> ());
      child_body write_fd body
  | pid ->
      interrupt.child <- Some pid;
      Unix.close write_fd;
      let buffer = Buffer.create 128 in
      let chunk = Bytes.create 4096 in
      let killed = ref `No in
      let started = Os.counter () in
      let read_ready () =
        match Unix.read read_fd chunk 0 (Bytes.length chunk) with
        | 0 -> false
        | n ->
            Buffer.add_subbytes buffer chunk 0 n;
            true
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> true
      in
      let rec watch () =
        let remaining = deadline -. Os.count_s started in
        if Option.is_some interrupt.signal then kill_group pid
        else if remaining <= 0. then begin
          killed := `Deadline;
          kill_group pid
        end
        else
          match Unix.select [ read_fd ] [] [] remaining with
          | [], _, _ -> watch ()
          | _ -> if read_ready () then watch ()
          | exception Unix.Unix_error (Unix.EINTR, _, _) -> watch ()
      in
      watch ();
      (try Unix.close read_fd with _ -> ());
      let status =
        Fun.protect
          ~finally:(fun () -> interrupt.child <- None)
          (fun () ->
            match waitpid_retry pid with
            | status -> status
            | exception Unix.Unix_error (e, _, _) ->
                raise
                  (Supervision (spf "waitpid failed: %s" (Unix.error_message e))))
      in
      let contents = Buffer.contents buffer in
      (* Derived, so a torn write cannot decode as a survivor: bytes that
         never got their newline are not a line — a killed or crashed
         child can leave a partial trailing one — and the readers then
         see nothing rather than seeing "survived". A false survivor is
         the one failure mode that makes people stop running the tool. *)
      let line =
        match String.index_opt contents '\n' with
        | Some i -> String.sub contents 0 i
        | None -> ""
      in
      { line; status; killed = !killed }

(* Every child starts from the same clean post-dry-run image; read-only
   checking (guarantee 12) is the child's config, set where it forks. *)
(* The child's selection, in the executor's own spelling: [reach] keys
   tests by path components, [Run.execute]'s allowlist by the rendered
   path the filters and the last-failed store both use. *)
let allowlist_of paths = List.map Test_tree.path_to_string paths
let child_prologue () = silence_output ()

(* The verdict line

   A survivor's reaching tests are the parent's — they are the tests the dry
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
   a test row, or a failed fixture release. Read off the outcome and never
   off [outcome.exit_code] (guarantee 12: the aggregate is the one exit
   code a build gates on): the exit code answers a different question — it
   is [2] for a selection that matched nothing, which is a statement about
   a filter and not about a mutant. *)
let killed_by (outcome : Run.outcome) =
  outcome.Run.release_failures <> []
  || List.exists counted_failure (Run.results outcome.Run.run)

let encode_outcome ~paths (outcome : Run.outcome) =
  if killed_by outcome then "killed"
  else if
    (* A child that recorded no test row did not survive the mutant, it
       failed to test it: reporting a survivor here would send the reader
       to strengthen tests that never ran. The allowlist is the dry
       run's own executed paths, so this is unreachable — and a false
       survivor is the one failure mode that makes people stop running
       the tool, so it is not left to be unreachable. *)
    Run.results outcome.Run.run = [] && paths <> []
  then "crashed"
  else "survived"

let decode_verdict ~paths line =
  match String.split_on_char ' ' (String.trim line) with
  | [ "survived" ] -> Ok (V.survived paths)
  | "error" :: rest -> Error (String.concat " " rest)
  (* "killed", the wrapper's own "crashed", no line at all, or a partial
     one: a child that did not report a survivor proved none. *)
  | _ -> Ok V.Killed

(* Scratch: one directory for the whole loop, one subdirectory per child,
   removed by the PARENT — a child killed at the deadline never runs its
   own cleanup, and an orphaned capture tree under the system temporary
   directory is exactly the trace child hygiene forbids. *)

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
          match Os.project_root () with
          | root -> Os.reconstruct ~root file
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

let test_locations tests =
  let table = Hashtbl.create 256 in
  List.iter
    (fun (case : Test_tree.case) ->
      Hashtbl.replace table
        (Test_tree.path_to_string case.Test_tree.path)
        case.Test_tree.loc)
    (Test_tree.flatten tests);
  table

(* A survivor as its block draws it. The reaching tests are the verdict's,
   which are the dry run's: the loop links the test tree, so each names
   its declaration site, and it is one executable, so none names one. *)
let survivor ~locations (r : V.record) reaching : Report_sections.survivor =
  {
    Report_sections.mutant =
      {
        (* Spelled with the runtime's own function: the report carries the
           identifier into the title and the command as it is. *)
        Report_sections.id = M.id_to_string r.V.id;
        line = r.V.id.M.line;
        before = r.V.before;
        after = r.V.after;
        source = read_source r.V.id.M.file;
      };
    witnesses =
      List.map
        (fun path ->
          let test = Test_tree.path_to_string path in
          {
            Report_sections.test;
            loc = Option.join (Hashtbl.find_opt locations test);
            exe = None;
          })
        reaching;
  }

(* The determinism probe

   One unarmed fork over exactly the tests the dry run executed. Its line
   is three counts and the indices of any counted failure, which is all
   the parent needs — it holds the names. Disagreement aborts: mutation
   results over a non-deterministic suite are not a weaker number, they
   are not a number. *)

let probe_line ~paths ~suite ~config tests () =
  child_prologue ();
  match Run.execute ~allowlist:(allowlist_of paths) config ~suite tests with
  | Error error -> "error " ^ one_line (Run.startup_message error)
  | Ok outcome ->
      let results = Run.results outcome.Run.run in
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

let check_determinism ~scratch ~dry_run_wall ~suite ~(config : Run.config)
    ~reach ~paths tests =
  let log_dir = Filename.concat scratch "probe" in
  let child = Run.for_subset config ~log_dir ~bail:false in
  (* The probe re-runs exactly the dry run's executed tests, so its
     deadline is the same formula over the same schedule. *)
  let { line; status; killed } =
    fork_child
      ~deadline:(child_deadline ~dry_run_wall ~reach paths)
      (probe_line ~paths ~suite ~config:child tests)
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
    let measured = List.length paths in
    spf
      "the suite is not deterministic: the dry run executed %d test%s, \
       skipping %d and failing none; the probe executed %d, skipping %d and \
       failing %d%s. Mutation results over a non-deterministic suite are not a \
       weaker number, they are not a number"
      measured
      (if measured = 1 then "" else "s")
      reach.skipped executed skipped failed
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

(* What the forks left: the verdicts of the children that ended, the
   survivors in the order their blocks printed, and the signal that
   stopped the forks with the mutant whose child it found running, none
   under the probe. *)
type tested = {
  verdicts : V.t;
  survivors : Report_sections.survivor list;
  stopped : (int * M.mutant option) option;
}

(* The bracket the loop opens before its first fork: one scratch root
   for the run, removed however the run leaves, and the determinism probe
   in front. A probe a signal killed reports a disagreement that is not
   the suite's: the signal outranks it. *)
let probed ~dry_run_wall ~suite ~config ~reach ~paths tests forks =
  let scratch = scratch_root () in
  Fun.protect
    ~finally:(fun () -> Run.remove_tree scratch)
    (fun () ->
      let agreed =
        check_determinism ~scratch ~dry_run_wall ~suite ~config ~reach ~paths
          tests
      in
      match (agreed, interrupt.signal) with
      | Ok (), (Some _ | None) -> Ok (forks ~scratch)
      | Error _, Some signal ->
          Ok
            {
              verdicts = V.empty;
              survivors = [];
              stopped = Some (signal, None);
            }
      | (Error _ as error), None -> error)

(* One mutant *)

(* A child that cannot arm reports an [error] line, which ends the loop
   without a score. Running on with nothing armed would give a green suite
   and score the mutant as a survivor. *)
let mutant_line ~paths ~budget ~suite ~config ~(mutant : M.mutant) tests () =
  child_prologue ();
  match M.arm ~budget mutant.M.id with
  | Error error ->
      "error " ^ one_line (Format.asprintf "%a" M.pp_arm_error error)
  | Ok _ -> (
      (* After arming, so the runaway budget measures the child's own hits
         and not the dry run's accumulated ones. *)
      M.reset_reach ();
      match Run.execute ~allowlist:(allowlist_of paths) config ~suite tests with
      | Error error -> "error " ^ one_line (Run.startup_message error)
      | Ok outcome -> encode_outcome ~paths outcome)

(* The runaway budget: the dry run's hit count with room to spare. A
   drained count under-reports (a site a test's teardown evaluates again
   is not marked twice), so the headroom is not decoration. *)
let budget_of hits =
  if hits > (max_int - 1000) / 8 then max_int else (hits * 8) + 1000

let run_mutant ~scratch ~dry_run_wall ~index ~suite ~(config : Run.config)
    ~reach ~paths ~budget ~mutant tests =
  let log_dir = Filename.concat scratch (spf "m%d" index) in
  let child = Run.for_subset config ~log_dir ~bail:true in
  let { line; status; killed } =
    fork_child
      ~deadline:(child_deadline ~dry_run_wall ~reach paths)
      (mutant_line ~paths ~budget ~suite ~config:child ~mutant tests)
  in
  Run.remove_tree log_dir;
  match killed with
  | `Deadline ->
      (* The suite noticed the change by hanging: a kill, on the crash
         kill's own reasoning. Whatever reached the pipe first is not a
         verdict — the child did not finish. *)
      V.Killed
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
          | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> V.Killed))

(* The fork loop: one child per reached mutant, in catalogue order. A
   survivor's block is committed when its child ends, which is why the
   order is the catalogue's: how many tests watched a survivor ranks it
   only among survivors, and those are known when the last child ends. A
   recorded signal is honoured when [run_mutant] returns, never before a
   fork, so the mutant the loop names as interrupted is one it did start;
   whatever that child reported, its mutant is not tested. *)
let run_children renderer ~locations ~scratch ~dry_run_wall ~suite ~config
    ~reach ~reached tests =
  let total = List.length reached in
  let rec go index verdicts survivors = function
    | [] -> { verdicts; survivors = List.rev survivors; stopped = None }
    | mutant :: rest -> (
        Report.mutation_testing renderer ~index:(index + 1) ~total
          ~id:(M.id_to_string mutant.M.id);
        let paths = reaching_tests reach mutant in
        let budget = budget_of (site_hits reach mutant) in
        let verdict =
          run_mutant ~scratch ~dry_run_wall ~index ~suite ~config ~reach ~paths
            ~budget ~mutant tests
        in
        match interrupt.signal with
        | Some signal ->
            {
              verdicts;
              survivors = List.rev survivors;
              stopped = Some (signal, Some mutant);
            }
        | None ->
            let record = V.record_of_mutant mutant verdict in
            let survivors =
              match verdict with
              | V.Survived { first; others } ->
                  let found = survivor ~locations record (first :: others) in
                  Report.mutation_survivor renderer found;
                  found :: survivors
              | V.Killed | V.Unreached -> survivors
            in
            go (index + 1) (V.add verdicts record) survivors rest)
  in
  go 0 V.empty [] reached

(* The report *)

(* Whether the run's selection NARROWS THE SUITE: a filter, an exclude,
   a tag selection, a [--failed] rerun, an in-source focus (the runner's
   own finding, so the two cannot disagree about what focus means), or a
   shard. A narrowed run's verdicts are relative to its selection — a
   mutant only deselected tests reach records Unreached, a survivor
   survived only the selection — and the file format carries no
   partial-run marking, so a written file would stand in the project
   merge as this executable's whole answer until the next full run.
   The mutation scope is deliberately not here: it narrows which mutants
   are tested, not which tests judge them, so a scoped run's records are
   project-true for this executable, merely fewer. *)
let narrows_suite ~(config : Run.config) ~focus =
  config.Run.filter <> [] || config.Run.exclude <> [] || config.Run.tags <> []
  || config.Run.exclude_tags <> []
  || config.Run.failed_only || focus || config.Run.shard <> None

(* The file holds the executable's answer for every mutant it catalogues,
   so a scoped run replaces only the records of its scope. The records of
   the other files are kept when this very build wrote them, which the
   identity proves; a file another build wrote, or none can read, says
   nothing about this one and is replaced whole. With no scope nothing is
   kept. What windtrap has to say about the file is for after the
   report. *)
let write_verdicts ~scope verdicts =
  let exe = Sys.executable_name in
  let path = V.output_file ~exe in
  let identity = V.writer_identity ~exe in
  let kept =
    match (identity, V.load path) with
    | Some identity, Ok (prior, Some writer) when writer = identity ->
        List.filter
          (fun (r : V.record) -> not (in_scope ~scope r.V.id))
          (V.records prior)
    | _ -> []
  in
  match V.save ?identity path (List.fold_left V.add verdicts kept) with
  | () -> None
  | exception Sys_error message ->
      Some (spf "could not write the verdict file: %s" message)

(* The population, before the dry run: the catalogue is complete once
   module initialization is over, and the scope is [--mutate]'s. The
   three ways it comes up empty are three different refusals. A prefix
   that leaves nothing is one sentence whatever the catalogue holds —
   the prefix is what the reader typed, and a file it matches nothing
   of is uninstrumented, misspelled or without sites in a plain build
   and an instrumented one alike — so it never blames a build that is
   instrumented and fine, and reads the same under either; the
   missing-backend diagnosis is the bare flag's, where there is no
   prefix to name. *)
let population ~scope =
  match (M.catalogue (), scope) with
  | [], [] ->
      Error
        "this executable links no instrumented module, so there is nothing to \
         mutate: instrument the library under test with ppx_windtrap.mutate \
         and re-run"
  | catalogue, _ -> (
      match
        List.filter (fun (m : M.mutant) -> in_scope ~scope m.M.id) catalogue
      with
      | [] ->
          Error
            (spf
               "--mutate=%s leaves no mutant in this executable's catalogue: \
                no instrumented file matches the prefix (is the library under \
                test instrumented with ppx_windtrap.mutate?), or the matched \
                files have no mutation sites"
               (String.concat "," scope))
      | scoped -> (
          match
            List.filter (fun (m : M.mutant) -> m.M.dismissed = None) scoped
          with
          | [] ->
              Error
                "every mutant this run could test is dismissed by [@mutate \
                 off]; there is nothing to test"
          | population -> Ok population))

(* The loop, end to end *)

let loop renderer ~scope ~suite (config : Run.config) tests =
  let population = population ~scope in
  let reach = fresh_reach () in
  let started = Os.counter () in
  match Report.run ~on_event:(observe reach) ~suite (dry_run config) tests with
  (* The startup message is already on stderr; a refused run never
     produced a number. *)
  | Error _ -> Reported 1
  | Ok outcome -> (
      (* The last test's teardown window. *)
      ignore (M.drain ());
      let executed = List.rev reach.executed in
      if outcome.Run.exit_code = 2 then
        refuse "no test ran, so there is nothing to mutate"
      else if outcome.Run.exit_code <> 0 then
        refuse
          "the dry run is red. Mutation scores a passing suite; a score over a \
           failing one is not a score"
      else
        match population with
        | Error message -> refuse "%s" message
        | Ok population -> (
            let reached, unreached =
              List.partition
                (fun mutant -> reaching_tests reach mutant <> [])
                population
            in
            let dry_run_wall = Float.max 0.01 (Os.count_s started) in
            let narrowed =
              narrows_suite ~config ~focus:outcome.Run.focus_active
            in
            (* A narrowed run's reach is its selection's, and the outcome
               line says so by the count the dry run executed. *)
            let reached_by =
              if narrowed then Report_sections.Selected (List.length executed)
              else Report_sections.Suite
            in
            (* The signals are handled from before the scratch root exists
               until the verdict file is written: a complete loop has its
               file whatever becomes of its report, and what windtrap says
               about the file is held until the report is out. A narrowed
               run's verdicts are never persisted, nor a stopped run's:
               neither is the executable's whole answer. *)
            let forks =
              try
                with_interrupts @@ fun () ->
                match
                  probed ~dry_run_wall ~suite ~config ~reach ~paths:executed
                    tests (fun ~scratch ->
                      run_children renderer ~locations:(test_locations tests)
                        ~scratch ~dry_run_wall ~suite ~config ~reach ~reached
                        tests)
                with
                | exception Supervision message -> Error message
                | Error _ as refused -> refused
                | Ok tested ->
                    let unsaved =
                      match tested.stopped with
                      | Some _ -> None
                      | None when narrowed ->
                          Some
                            "verdicts not saved: this run's selection narrows \
                             the suite, and a partial run's verdicts would \
                             stand in the project merge as the whole."
                      | None ->
                          write_verdicts ~scope
                            (List.fold_left
                               (fun acc (m : M.mutant) ->
                                 V.add acc (V.record_of_mutant m V.Unreached))
                               tested.verdicts unreached)
                    in
                    Ok (tested, unsaved)
              with Sys_error _ when interrupt.signal = Some Sys.sigpipe ->
                (* The reader of the report went away on purpose: nothing
                   is said, and no report is tried on the dead pipe. *)
                die_by Sys.sigpipe
            in
            match forks with
            | Error message ->
                Report.mutation_refused renderer message;
                Reported 1
            | Ok ({ verdicts; survivors; stopped }, unsaved) -> (
                let records = V.records verdicts in
                let report =
                  {
                    Report_sections.survivors;
                    unreached =
                      List.map
                        (fun (m : M.mutant) -> (m.M.id.M.file, m.M.id.M.line))
                        unreached;
                    killed =
                      List.length
                        (List.filter
                           (fun (r : V.record) -> r.V.verdict = V.Killed)
                           records);
                    not_tested = List.length reached - List.length records;
                    scope = reached_by;
                  }
                in
                match stopped with
                | Some (signal, _) when signal = Sys.sigpipe -> die_by signal
                | Some (signal, testing) ->
                    Report.mutation_interrupted renderer
                      ~testing:
                        (Option.map
                           (fun (m : M.mutant) -> M.id_to_string m.M.id)
                           testing)
                      report;
                    flush_descriptors ();
                    die_by signal
                | None -> (
                    (* A signal recorded once the last child had ended
                       stopped nothing: the file is written and the report
                       is whole. The loop still dies by it, so what sent it
                       sees the death it asked for. *)
                    match interrupt.signal with
                    | Some signal when signal = Sys.sigpipe -> die_by signal
                    | late -> (
                        Report.mutation_finish renderer report;
                        flush_descriptors ();
                        Option.iter (note "%s") unsaved;
                        match late with
                        | Some signal -> die_by signal
                        | None -> Reported 0)))))

(* The ordinary run with one mutant armed. [spec] is the identifier as
   [--arm] read it, unparsed: the runtime's grammar decides what it
   names. *)

let arm_mode renderer ~spec ~suite (config : Run.config) tests =
  match Result.bind (M.id_of_string spec) M.arm with
  | Error (M.Uncatalogued _ as error) ->
      (* Not a refusal. One identifier is handed to every test
         executable at once: a build action's reproduce command, and an
         inline runner's, arm it across a re-run of the whole suite,
         because a build has no single binary to name. In a
         project with several test executables most of
         them were built from other sources. An executable that
         catalogues no site of the named file
         is simply not the one the identifier is about: exiting 1 here
         would fail the build for every sibling of the binary that armed
         the mutant correctly, which is the report's headline advice
         reporting failure when it works. Said once, on stderr, and
         nothing is concealed by running on: no verdict is produced here
         either way. A stale or misspelled identifier still names a
         catalogued file, comes back [Unmatched] below, and still
         refuses.

         What follows is the run this process would have made without
         the flag, so an uninstrumented sibling is left with its ordinary
         transcript and one line of stderr. *)
      note "%s" (Format.asprintf "%a" M.pp_arm_error error);
      (* Nothing is armed here: the report must not title its failures
         [(mutant armed)] nor spell [--arm] in their hints. *)
      Ran
        (Report.run ~suite { config with Run.mutation = Run.No_mutation } tests)
  | Error error ->
      note "%s" (Format.asprintf "%a" M.pp_arm_error error);
      Reported 1
  | Ok mutant ->
      (* An armed run never writes: no .corrected (guarantee 12: armed checking is read-only) and no
         accepted baseline. An armed mutant changes program output on
         purpose, and a run that promoted that output would rewrite the
         source tree from a lie. Baselines need no flag beyond Check: a
         correction is recorded only under Corrected and Update. *)
      let id = M.id_to_string mutant.M.id in
      let config =
        { config with Run.baseline = Baseline.Check; mutation = Run.Armed id }
      in
      Report.mutation_armed renderer ~id ~before:mutant.M.before
        ~after:mutant.M.after;
      flush_descriptors ();
      (* After arming, so the closing line counts the run's own
         evaluations and not module initialization's — that window ran
         before the arming and evaluated nothing mutated. *)
      M.reset_reach ();
      let result = Report.run ~suite config tests in
      (* [killed_by], not [exit_code <> 0]: a filter that matched nothing
         exits 2, and announcing [mutant killed.] there would report a
         selection mistake as a detected behaviour change (a verdict is never
         an exit code). A
         completed run that killed nothing gets the other half of the
         verdict: green alone cannot tell "the tests prove nothing about
         this site" from "no selected test ran the line", so the closing
         line says which — except on exit 2, where the run made no claim
         about the mutant at all. *)
      (match result with
      | Ok outcome when killed_by outcome -> Report.mutation_killed renderer
      | Ok outcome when outcome.Run.exit_code <> 2 -> (
          match M.armed_hits () with
          | 0 -> Report.mutation_not_evaluated renderer
          | hits -> Report.mutation_survived renderer ~hits)
      | Ok _ | Error _ -> ());
      flush_descriptors ();
      Ran result

(* Entry *)

let execute_and_report ~suite (config : Run.config) tests =
  (* The flags are acted on in every build, instrumented or not: a loop
     asked of an executable that catalogues nothing refuses by name
     ([population]), and an identifier that names a site of a file this
     build does catalogue and matches none of them comes back [Unmatched]
     with the file's candidates, which is the whole diagnosis. *)
  let renderer () = Report.terminal config in
  match config.Run.mutation with
  | Run.No_mutation -> Ran (Report.run ~suite config tests)
  | Run.Armed spec -> arm_mode (renderer ()) ~spec ~suite config tests
  | Run.Loop scope ->
      if Sys.win32 then
        refuse "mutation testing needs Unix.fork, which Windows does not have"
      else loop (renderer ()) ~scope ~suite config tests
