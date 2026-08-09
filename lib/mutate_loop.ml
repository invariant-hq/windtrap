(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* The parent of a mutation run: dry run, probe, forced-fail check, fork
   loop, verdict file, report. The runtime below (Windtrap_mutate) owns
   the catalogue, the arming slot, the reach counters and the file format;
   this module owns the protocol over them and every decision the report
   shows — the ordering, the cap, the counts. Render orders nothing and
   counts nothing, and neither does the runtime. *)

module M = Windtrap_mutate

let spf = Printf.sprintf

(* The catalogue is complete only after module initialization, which is
   why it is read lazily rather than at this module's own load time. *)
let catalogue = lazy (M.catalogue ())
let instrumented () = Lazy.force catalogue <> []

type run =
  | Ran of (Runner.outcome * Run.result list, Runner.startup_error) result
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

(* The pseudo-path a fixture-release failure is reported under, matching
   the one Driver gives its synthetic release results: no test owns a
   release, and a declared path component may not contain the separator,
   so it cannot collide with one. *)
let release_path = [ "fixture release" ]
let saturating_add x y = if x > max_int - y then max_int else x + y

(* The spine's arguments, threaded whole

   Everything Driver.execute_and_report needs except the configuration and
   the tree, which the loop replaces per child. Passing them as one value
   keeps the four places that run the suite from drifting in what they
   pass. *)

type spine = {
  armed : unit -> unit;
  invocation : Render.invocation;
  seed : Seed.seed option;
  selection : string option;
  github : bool;
  output : [ `Quiet | `Compact | `Verbose ];
  coverage_mode : [ `Summary | `Report | `Full | `Off ];
  suite : string;
}

let drive ?on_event spine ~config tests =
  Driver.execute_and_report ?on_event ~invocation:spine.invocation
    ~seed:spine.seed ~selection:spine.selection ~github:spine.github
    ~output:spine.output ~coverage_mode:spine.coverage_mode ~config
    ~suite:spine.suite tests

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
  | Some site -> List.rev site.tests

let site_hits reach (mutant : M.mutant) =
  match Hashtbl.find_opt reach.sites mutant.M.id with
  | None -> 0
  | Some site -> site.hits

let test_time reach path =
  Option.value ~default:0.
    (Hashtbl.find_opt reach.durations (Test_tree.path_to_string path))

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

type report = { line : string; status : Unix.process_status; killed : bool }

(* One child, one line. The parent reads the pipe to EOF before it waits,
   which is what keeps a child writing more than a pipe buffer from
   deadlocking against a parent already in waitpid. *)
let spawn_child ~expired body =
  flush_descriptors ();
  let read_fd, write_fd =
    match Unix.pipe () with
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
      (* Interval timers do not survive fork, but the handler's
         disposition does; a child must not carry the loop's. *)
      (try Sys.set_signal Sys.sigalrm Sys.Signal_default with _ -> ());
      child_body write_fd body
  | pid ->
      Unix.close write_fd;
      let buffer = Buffer.create 128 in
      let chunk = Bytes.create 4096 in
      let killed = ref false in
      let rec read () =
        match Unix.read read_fd chunk 0 (Bytes.length chunk) with
        | 0 -> ()
        | n ->
            Buffer.add_subbytes buffer chunk 0 n;
            read ()
        | exception Unix.Unix_error (Unix.EINTR, _, _) ->
            if !expired then killed := true else read ()
      in
      read ();
      (try Unix.close read_fd with _ -> ());
      (if !killed then try Unix.kill pid Sys.sigkill with _ -> ());
      let status =
        match waitpid_retry pid with
        | status -> status
        | exception Unix.Unix_error (e, _, _) ->
            raise
              (Supervision (spf "waitpid failed: %s" (Unix.error_message e)))
      in
      let contents = Buffer.contents buffer in
      let line =
        match String.index_opt contents '\n' with
        | Some i -> String.sub contents 0 i
        | None -> String.trim contents
      in
      { line; status; killed = !killed }

(* A deadline the loop has not spent yet is honoured HERE, before the
   fork, and not only in the read above, because the alarm is one-shot: if
   it fired anywhere other than a blocking read — between two children, in
   the scratch cleanup, in the report — nothing would ever observe it
   again and the deadline would go silently unenforced for the rest of the
   loop. A child that is never forked reports exactly as one killed at the
   deadline does, so no caller needs a second shape. *)
let fork_child ~expired body =
  if !expired then { line = ""; status = Unix.WEXITED 0; killed = true }
  else spawn_child ~expired body

(* The child's run configuration

   The knobs that select {e by path} are cleared — filter, exclude, shard,
   the [--failed] allowlist — because the tree the child is handed IS that
   selection: it was pruned to the paths the dry run executed, so applying
   any of them again could only narrow it further.

   The knobs that select by TAG are kept verbatim, and that distinction is
   load-bearing rather than tidy. A test's tags are not in its path, so
   pruning cannot express them: [Tag.default_predicate] drops [disabled]
   by itself, so a dry run under [--tag disabled] executes tests a child
   with [tags = []] would deselect. The child would then run fewer tests
   than the parent measured — the determinism probe would call a perfectly
   deterministic suite non-deterministic, and every mutant past it would
   be scored against a smaller suite than the report claims. Keeping the
   parent's [tags], [exclude_tags] and [quick] makes the child's selection
   over the pruned tree provably the dry run's.

   [update = No_update] is what makes snapshot checking read-only by
   construction — Snapshot maps it to Mode Check and the write is
   reachable only under Mode Update — [bail] stops at the first kill, and
   the log directory is the child's own so that its capture files and its
   last-failed store cannot touch the parent's. The root seed is kept,
   which is what makes subset execution sound at all: per-case seeds
   derive from (root, path, index), so a child running 24 of 900 tests
   sees the same property cases the dry run saw. *)
let child_config ~(parent : Run.config) ~log_dir ~bail =
  {
    parent with
    Run.filter = None;
    exclude = None;
    shard = None;
    failed_only = false;
    list_only = false;
    bail;
    stream = false;
    update = Env.No_update;
    prune = false;
    junit = None;
    log_dir;
    allow_focus = true;
  }

let pruned_to paths tests =
  let keep = Hashtbl.create (List.length paths * 2) in
  List.iter (fun path -> Hashtbl.replace keep path ()) paths;
  Test_tree.prune (fun path -> Hashtbl.mem keep path) tests

(* Every child starts from the same clean post-dry-run image except for
   what the inline runtime must not inherit — its merged reach histories,
   and its licence to record a correction (Law 16d). Both are [armed]'s
   job; see the spine's field. *)
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

(* Whether a run that had a mutant armed detected it: a counted failure, or
   a fixture release that failed. Read off the results and never off
   [outcome.exit_code] (Law 16c): the exit code answers a different
   question — it is [2] for a selection that matched nothing, which is a
   statement about a filter and not about a mutant. *)
let killed_by (outcome : Runner.outcome) =
  List.exists counted_failure (Run.results outcome.Runner.run)
  || outcome.Runner.release_failures <> []

let encode_outcome ~paths (outcome : Runner.outcome) =
  let results = Run.results outcome.Runner.run in
  match List.find_opt counted_failure results with
  | Some result -> (
      match index_of result.Run.path paths with
      | Some i -> spf "killed %d" i
      | None -> "crashed")
  | None ->
      if outcome.Runner.release_failures <> [] then "killed release"
        (* A child that recorded nothing did not survive the mutant, it
           failed to test it: reporting a survivor here would send the
           reader to strengthen tests that never ran. The pruned tree is
           the dry run's own executed paths, so this is unreachable — and
           a false survivor is the one failure mode that makes people stop
           running the tool, so it is not left to be unreachable. *)
      else if results = [] && paths <> [] then "crashed"
      else "survived"

let decode_verdict ~paths line =
  match String.split_on_char ' ' (String.trim line) with
  | [ "survived" ] -> Ok (M.survived paths)
  | [ "killed"; "release" ] -> Ok (M.Killed (M.Failed release_path))
  | [ "killed"; index ] -> (
      match int_of_string_opt index with
      | Some i when i >= 0 && i < List.length paths ->
          Ok (M.Killed (M.Failed (List.nth paths i)))
      | _ -> Ok (M.Killed M.Crashed))
  | "error" :: rest -> Error (String.concat " " rest)
  (* No line at all, a partial line, or the wrapper's own "crashed". *)
  | _ -> Ok (M.Killed M.Crashed)

(* The whole-loop deadline

   One [Unix.setitimer] for the loop, not one per mutant: a per-mutant
   deadline wants a select loop, a session and a process-group kill, and
   this slice buys the runtime's runaway hit-count budget covering the
   common case — a mutant that spins — for a quarter of the code. The cost
   is that an expiry cannot say WHICH mutant hung, only which was in
   flight. The budget is the specified per-mutant formula applied to the
   whole loop: three times what the dry run says the loop should cost,
   plus five seconds, never under a minute. *)
let with_deadline seconds fn =
  let expired = ref false in
  if seconds <= 0. || not (Float.is_finite seconds) then fn expired
  else
    let previous =
      Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> expired := true))
    in
    let clear () =
      ignore
        (Unix.setitimer Unix.ITIMER_REAL
           { Unix.it_interval = 0.; it_value = 0. });
      Sys.set_signal Sys.sigalrm previous
    in
    ignore
      (Unix.setitimer Unix.ITIMER_REAL
         { Unix.it_interval = 0.; it_value = seconds });
    Fun.protect ~finally:clear (fun () -> fn expired)

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

let rec remove_tree path =
  match Unix.lstat path with
  | exception Unix.Unix_error _ -> ()
  | { Unix.st_kind = Unix.S_DIR; _ } -> (
      (match Sys.readdir path with
      | exception Sys_error _ -> ()
      | entries ->
          Array.iter
            (fun entry -> remove_tree (Filename.concat path entry))
            entries);
      try Unix.rmdir path with Unix.Unix_error _ -> ())
  | _ -> ( try Unix.unlink path with Unix.Unix_error _ -> ())

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

let survivor_of ~locations (mutant : M.mutant) witnesses : Render.survivor =
  {
    Render.file = mutant.M.id.M.file;
    line = mutant.M.id.M.line;
    col = mutant.M.id.M.col;
    rewrite = mutant.M.id.M.rewrite;
    before = mutant.M.before;
    after = mutant.M.after;
    source = read_source mutant.M.id.M.file;
    witnesses =
      List.map
        (fun path ->
          let test = Test_tree.path_to_string path in
          { Render.test; loc = Option.join (Hashtbl.find_opt locations test) })
        witnesses;
  }

let unreached_lines mutants =
  let by_file = Hashtbl.create 16 in
  List.iter
    (fun (m : M.mutant) ->
      let file = m.M.id.M.file in
      let prior = Option.value ~default:[] (Hashtbl.find_opt by_file file) in
      Hashtbl.replace by_file file (m.M.id.M.line :: prior))
    mutants;
  Hashtbl.fold
    (fun file lines acc ->
      { Render.file; lines = List.sort_uniq compare lines } :: acc)
    by_file []
  |> List.sort (fun (a : Render.unreached) b -> compare a.file b.file)

(* The determinism probe

   One unarmed fork over exactly the tests the dry run executed. Its line
   is three counts and the indices of any counted failure, which is all
   the parent needs — it holds the names. Disagreement aborts: mutation
   results over a non-deterministic suite are not a weaker number, they
   are not a number. *)

let probe_line ~armed ~paths ~config ~suite tests () =
  child_prologue ~armed;
  match Runner.execute ~config ~suite (pruned_to paths tests) with
  | Error error -> "error " ^ one_line (Runner.startup_message error)
  | Ok outcome ->
      let results = Run.results outcome.Runner.run in
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

let check_determinism ~armed ~expired ~scratch ~config ~suite ~reach ~paths
    tests =
  let log_dir = Filename.concat scratch "probe" in
  let config = child_config ~parent:config ~log_dir ~bail:None in
  let { line; status; killed } =
    fork_child ~expired (probe_line ~armed ~paths ~config ~suite tests)
  in
  remove_tree log_dir;
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
  if killed then Error "the determinism probe exceeded the loop deadline"
  else
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
    | _ -> Error "the determinism probe died without reporting a result"

(* One mutant *)

let mutant_line ~armed ~paths ~budget ~config ~suite ~mutant tests () =
  child_prologue ~armed;
  match M.arm ~budget (M.selector_of_mutant mutant) with
  | Error error ->
      "error " ^ one_line (Format.asprintf "%a" M.pp_arm_error error)
  | Ok _ -> (
      (* After arming, so the runaway budget measures the child's own hits
         and not the dry run's accumulated ones. *)
      M.reset_reach ();
      match Runner.execute ~config ~suite (pruned_to paths tests) with
      | Error error -> "error " ^ one_line (Runner.startup_message error)
      | Ok outcome -> encode_outcome ~paths outcome)

(* The runaway budget: the dry run's hit count with room to spare. A
   drained count under-reports (a site a test's teardown evaluates again
   is not marked twice), so the headroom is not decoration. *)
let budget_of hits =
  if hits > (max_int - 1000) / 8 then max_int else (hits * 8) + 1000

let run_mutant ~armed ~expired ~scratch ~index ~config ~suite ~paths ~budget
    ~mutant tests =
  let log_dir = Filename.concat scratch (spf "m%d" index) in
  let child = child_config ~parent:config ~log_dir ~bail:(Some 1) in
  let { line; status; killed } =
    fork_child ~expired
      (mutant_line ~armed ~paths ~budget ~config:child ~suite ~mutant tests)
  in
  remove_tree log_dir;
  if killed then Ok (M.Killed M.Timed_out, `Timed_out)
  else
    match decode_verdict ~paths line with
    | Error message -> Error (spf "%s: %s" (M.id_to_string mutant.M.id) message)
    | Ok verdict ->
        (* A child that did not leave through [Unix._exit 0] did not
           report: whatever reached the pipe is not a verdict. *)
        let verdict =
          match status with
          | Unix.WEXITED 0 -> verdict
          | Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _ ->
              M.Killed M.Crashed
        in
        Ok (verdict, `Reported)

(* The fork loop

   Most-reached mutant first, and its child is the forced-fail check: a
   mutant many tests run is the one least likely to survive a correctly
   instrumented build, so a survivor there is evidence about the build and
   not about the suite. Its verdict is kept either way, which is what
   makes the check cost nothing for a build that passes it. *)

let run_children ~armed ~expired ~scratch ~config ~suite ~reach ~ordered tests =
  let verdicts = ref M.empty in
  let record (mutant : M.mutant) verdict =
    verdicts := M.add !verdicts mutant.M.id verdict
  in
  let rec go index = function
    | [] -> Ok !verdicts
    | mutant :: rest -> (
        let paths = reaching_tests reach mutant in
        let budget = budget_of (site_hits reach mutant) in
        match
          run_mutant ~armed ~expired ~scratch ~index ~config ~suite ~paths
            ~budget ~mutant tests
        with
        | Error message -> raise (Supervision message)
        | Ok (verdict, `Timed_out) ->
            record mutant verdict;
            Error
              (spf
                 "the loop exceeded its deadline while running %s. The runaway \
                  budget catches a mutant that spins; a mutant that blocks \
                  needs the per-mutant deadline, which is not in this release"
                 (M.id_to_string mutant.M.id))
        | Ok (verdict, `Reported) ->
            record mutant verdict;
            let survived =
              match verdict with M.Survived _ -> true | _ -> false
            in
            if index = 0 && survived then
              Error
                (spf
                   "arming %s changed nothing: %d test(s) ran it and none \
                    failed. Either the library under test was not built with \
                    --instrument-with ppx_windtrap.mutate — the commonest \
                    cause, and then the mutants here are the test executable's \
                    own — or that mutant genuinely survives, in which case \
                    dismiss it with [@mutate off] and re-run"
                   (M.id_to_string mutant.M.id)
                   (List.length paths))
            else go (index + 1) rest)
  in
  go 0 ordered

(* The report *)

let write_verdicts verdicts =
  let exe = Sys.executable_name in
  let path = M.output_file ~exe in
  (try M.save ?identity:(M.writer_identity ~exe) path verdicts
   with Sys_error message ->
     note "could not write the verdict file: %s" message);
  path

let print_report renderer ~limit ~population ~unreached ~verdicts ~duration
    ~seed ~siblings tests =
  let locations = witness_locations tests in
  let mutant_of id =
    List.find_opt (fun (m : M.mutant) -> M.equal_id m.M.id id) population
  in
  let bindings = M.verdicts verdicts in
  let survivors =
    List.filter_map
      (fun (id, verdict) ->
        match (verdict, mutant_of id) with
        | M.Survived { witness; others }, Some mutant ->
            Some (survivor_of ~locations mutant (witness :: others))
        | _ -> None)
      bindings
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
  let shown =
    if limit <= 0 then survivors
    else List.filteri (fun i _ -> i < limit) survivors
  in
  let killed =
    List.length
      (List.filter
         (fun (_, v) -> match v with M.Killed _ -> true | _ -> false)
         bindings)
  in
  Render.mutation_report renderer
    {
      Render.survivors = shown;
      survivors_total = List.length survivors;
      unreached = unreached_lines unreached;
      unreached_total = List.length unreached;
      killed;
      total = List.length population;
      duration = Some duration;
      seed = Some seed;
      siblings;
    }

(* The loop, end to end *)

let loop renderer spine ~(config : Run.config) ~limit tests =
  let reach = fresh_reach () in
  let started = Unix.gettimeofday () in
  match drive ~on_event:(observe reach) spine ~config tests with
  (* The startup message is already on stderr; a refused run never
     produced a number. *)
  | Error _ -> Reported 1
  | Ok (outcome, _) -> (
      (* The last test's teardown window. *)
      ignore (M.drain ());
      let executed = List.rev reach.executed in
      if outcome.Runner.exit_code = 2 then
        refuse "no test ran, so there is nothing to mutate"
      else if outcome.Runner.exit_code <> 0 then
        refuse
          "the dry run is red. Mutation scores a passing suite; a score over a \
           failing one is not a score"
      else if Lazy.force catalogue = [] then
        refuse
          "this executable links no instrumented module, so there is nothing \
           to mutate. Build it with --instrument-with ppx_windtrap.mutate, or \
           add an (instrumentation (backend ppx_windtrap.mutate)) stanza to \
           the library under test"
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
          let expected =
            List.fold_left
              (fun acc mutant ->
                List.fold_left
                  (fun acc path -> acc +. test_time reach path)
                  acc
                  (reaching_tests reach mutant))
              0. ordered
          in
          let outcome =
            with_deadline
              (Float.max 60. ((3. *. expected) +. 5.))
              (fun expired ->
                let scratch = scratch_root () in
                Fun.protect
                  ~finally:(fun () -> remove_tree scratch)
                  (fun () ->
                    match
                      check_determinism ~armed:spine.armed ~expired ~scratch
                        ~config ~suite:spine.suite ~reach ~paths:executed tests
                    with
                    | Error _ as error -> error
                    | Ok () ->
                        run_children ~armed:spine.armed ~expired ~scratch
                          ~config ~suite:spine.suite ~reach ~ordered tests))
          in
          match outcome with
          | Error message -> refuse "%s" message
          | Ok reported ->
              let verdicts =
                List.fold_left
                  (fun acc (m : M.mutant) -> M.add acc m.M.id M.Unreached)
                  reported unreached
              in
              let path = write_verdicts verdicts in
              print_report renderer ~limit ~population ~unreached ~verdicts
                ~duration:(Unix.gettimeofday () -. started)
                ~seed:config.Run.seed ~siblings:(has_siblings path) tests;
              flush_descriptors ();
              Reported 0)

(* The two modes that are ordinary runs with something printed around them *)

let discovery_mode renderer spine ~config tests =
  let result = drive spine ~config tests in
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

let arm_mode renderer spine ~(config : Run.config) tests =
  match M.arm_from_env () with
  | Error error ->
      Format.eprintf "%a@." M.pp_arm_error error;
      Reported 1
  (* The caller only reaches here with the variable set, so this arm is
     the empty-string case the runtime already treats as unset. *)
  | Ok None -> discovery_mode renderer spine ~config tests
  | Ok (Some mutant) ->
      (* An armed run never writes: no .corrected (Law 16d) and no
         accepted baseline. An armed mutant changes program output on
         purpose, and a run that promoted that output would rewrite the
         source tree from a lie. Snapshots need no flag beyond No_update:
         Snapshot maps it to Mode Check, and the write is reachable only
         under Mode Update. *)
      spine.armed ();
      let config = { config with Run.update = Env.No_update; prune = false } in
      Render.mutation_armed renderer
        ~id:(M.id_to_string mutant.M.id)
        ~before:mutant.M.before ~after:mutant.M.after;
      flush_descriptors ();
      let result = drive spine ~config tests in
      (* [killed_by], not [exit_code <> 0]: a filter that matched nothing
         exits 2, and announcing [mutant killed.] there would report a
         selection mistake as a detected behaviour change (Law 16c). *)
      (match result with
      | Ok (outcome, _) when killed_by outcome ->
          Render.mutation_killed renderer
      | Ok _ | Error _ -> ());
      flush_descriptors ();
      Ran result

(* Entry *)

let execute_and_report ~armed ~invocation ~seed ~selection ~github ~output
    ~coverage_mode ~(config : Run.config) ~suite tests =
  let spine =
    { armed; invocation; seed; selection; github; output; coverage_mode; suite }
  in
  (* A listing is not a run: nothing executes, so there is nothing to
     observe, announce or mutate. Everything else goes through the knobs,
     instrumented or not — a variable the user set and misspelled must be
     loud in every build, and an identifier that names no mutant comes
     back [Unmatched] with no candidates, which is the whole diagnosis. *)
  if config.Run.list_only then Ran (drive spine ~config tests)
  else
    match Cli.mutation () with
    | Error error ->
        note "%s" (Cli.error_message error);
        Reported 1
    | Ok { Cli.mode; arm; limit } -> (
        let renderer () = Driver.renderer ~config ~mode:output ~invocation () in
        match (mode, arm) with
        | `Off, None ->
            (* The one path an uninstrumented build must not pay for. *)
            if instrumented () then
              discovery_mode (renderer ()) spine ~config tests
            else Ran (drive spine ~config tests)
        | `Off, Some _ -> arm_mode (renderer ()) spine ~config tests
        | (`Loop | `Report), Some _ ->
            refuse
              "WINDTRAP_MUTATE and %s ask for different runs — the loop arms \
               each mutant itself, so an armed parent would mutate its own dry \
               run. Unset one"
              M.arm_variable
        | (`Loop | `Report), None -> (
            if Sys.win32 then
              refuse
                "mutation testing needs Unix.fork, which Windows does not \
                 have; the tests themselves still ran"
            else
              try loop (renderer ()) spine ~config ~limit tests
              with Supervision message -> refuse "%s" message))
