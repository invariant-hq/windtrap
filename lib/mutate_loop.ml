(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Not mutated. The scheduler, the run state, the report and this loop judge
   mutants, and a mutant armed among them does not fail the tests that reach
   it: it hangs them or corrupts the verdict. Coverage still measures these
   files. *)
[@@@mutate exclude_file]

module Mutate = Windtrap_runtime.Mutate
module Verdicts = Windtrap_runtime.Verdicts

let strf = Printf.sprintf

type run = Ran of (Run.outcome, Run.startup_error) result | Reported of int

let refuse message =
  Os.say message;
  Reported 1

let ordinary ~suite (config : Run.config) tests =
  Ran (Report.run ~suite { config with mutation = Run.No_mutation } tests)

(* Population *)

let in_scope ~scope (id : Mutate.id) =
  scope = []
  || List.exists (fun prefix -> String.starts_with ~prefix id.file) scope

type no_mutant = Uninstrumented | Out_of_scope | All_dismissed

let population ~scope =
  let catalogue = Mutate.catalogue () in
  let scoped =
    List.filter (fun (m : Mutate.mutant) -> in_scope ~scope m.id) catalogue
  in
  let undismissed =
    List.filter (fun (m : Mutate.mutant) -> Option.is_none m.dismissed) scoped
  in
  match (catalogue, scoped, undismissed) with
  | [], _, _ when scope = [] -> Error Uninstrumented
  | _, [], _ -> Error Out_of_scope
  | _, _, [] -> Error All_dismissed
  | _, _, population -> Ok population

(* A prefix that leaves nothing is one sentence whatever the catalogue holds:
   the prefix is what the reader typed, and a build that is instrumented and
   fine must not be blamed for it. *)
let refusal ~scope = function
  | Uninstrumented ->
      "this executable links no instrumented module, so there is nothing to \
       mutate: instrument the library under test with ppx_windtrap.mutate and \
       re-run"
  | Out_of_scope ->
      strf
        "--mutate=%s leaves no mutant in this executable's catalogue: no \
         instrumented file matches the prefix (is the library under test \
         instrumented with ppx_windtrap.mutate?), or the matched files have no \
         mutation sites"
        (String.concat "," scope)
  | All_dismissed ->
      "every mutant this run could test is dismissed by [@mutate off]; there \
       is nothing to test"

let unmutated ~scope reason =
  let why =
    match reason with
    | Uninstrumented -> "this executable links no instrumented module"
    | Out_of_scope ->
        strf "no mutant of this executable's catalogue is under %s"
          (String.concat "," scope)
    | All_dismissed ->
        "every mutant of this executable is dismissed by [@mutate off]"
  in
  strf "WINDTRAP_MUTATE is set, but %s, so the suite runs without mutation" why

(* The reach map *)

type site = {
  mutable tests : string list list; (* the reaching tests, newest first *)
  mutable hits : int; (* the drained evaluations, saturating at [max_int] *)
}

(* Filled by the dry run's observer, which only writes: an observer that
   raises ends the run. *)
type reach = {
  sites : (Mutate.id, site) Hashtbl.t;
  durations : (string, float) Hashtbl.t; (* seconds, by path string *)
  mutable executed : string list list; (* newest first *)
  mutable skipped : int;
}

let is_skip (r : Run.result) =
  match r.outcome with
  | Failure.Skip _ -> true
  | Failure.Pass | Failure.Fail _ -> false

(* The drain at [Test_started] precedes the epoch bump: what it collects ran
   outside any test, and a warm-forked child cannot arm it. A test marked
   xfail reaches no mutant, so what it evaluated is drained and dropped. *)
let observe reach (event : Run.event) =
  match event with
  | Run.Run_started _ | Run.Fixture_release _ | Run.Interrupted _ -> ()
  | Run.Test_started _ ->
      ignore (Mutate.drain ());
      Mutate.next_epoch ()
  | Run.Test_finished result ->
      let add (reached : Mutate.reached) =
        match Hashtbl.find_opt reach.sites reached.mutant.id with
        | None ->
            Hashtbl.add reach.sites reached.mutant.id
              { tests = [ result.path ]; hits = reached.hits }
        | Some site ->
            site.tests <- result.path :: site.tests;
            site.hits <-
              (if site.hits > max_int - reached.hits then max_int
               else site.hits + reached.hits)
      in
      let reached = Mutate.drain () in
      if Option.is_none result.xfail then List.iter add reached;
      reach.executed <- result.path :: reach.executed;
      Hashtbl.replace reach.durations
        (Test_tree.path_to_string result.path)
        result.duration;
      if is_skip result then reach.skipped <- reach.skipped + 1

(* Signals *)

(* A child is a session of its own, which a terminal's signal does not reach,
   so the handler kills the running child's group. It prints nothing: it runs
   at a safepoint of whatever the parent was doing, the report included. *)
type interrupt = {
  mutable signal : int option; (* the first signal received *)
  mutable child : int option; (* the running child, until it is reaped *)
}

let interrupt = { signal = None; child = None }

(* The group exists once the child has run [setsid]. *)
let kill_group pid =
  (try Unix.kill (-pid) Sys.sigkill with Unix.Unix_error _ -> ());
  try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ()

(* SIGPIPE's default action would kill the parent inside a write of the
   report, past every [Fun.protect]; handled, the write fails with
   [Sys_error] and unwinds. *)
let with_interrupts fn =
  interrupt.signal <- None;
  interrupt.child <- None;
  let handle signal =
    if Option.is_none interrupt.signal then interrupt.signal <- Some signal;
    Option.iter kill_group interrupt.child
  in
  Os.with_signals [ Sys.sigpipe; Sys.sigint; Sys.sigterm; Sys.sighup ] handle fn

(* Children *)

(* The parent lost track of a child: a score over children it does not
   supervise is no score. *)
exception Supervision of string

let failed call error =
  Supervision (strf "%s failed: %s" call (Unix.error_message error))

let domain_refusal =
  "this process has spawned a domain, and OCaml refuses Unix.fork in a process \
   that has: mutation testing runs every mutant in a forked child, so it \
   cannot run in this one. Exclude the tests that spawn a domain (-e) to test \
   the rest"

(* Buffered bytes duplicated into a child would print twice. *)
let flush_descriptors () =
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  (try flush Stdlib.stdout with Sys_error _ -> ());
  try flush Stdlib.stderr with Sys_error _ -> ()

(* A mutated program can print from anywhere, and hundreds of children would
   bury the report. The verdict pipe is a descriptor of its own. *)
let silence_output () =
  match Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0o600 with
  | exception Unix.Unix_error _ -> ()
  | fd ->
      Unix.dup2 fd Unix.stdout;
      Unix.dup2 fd Unix.stderr;
      if fd <> Unix.stdout && fd <> Unix.stderr then Unix.close fd

let rec write_all fd s offset =
  if offset < String.length s then
    match Unix.write_substring fd s offset (String.length s - offset) with
    | written -> write_all fd s (offset + written)
    | exception Unix.Unix_error (Unix.EINTR, _, _) -> write_all fd s offset

(* No exception, a fatal one included, reaches Stdlib's exit machinery: its
   [at_exit] functions, the coverage dump among them, would write over the
   parent's files. *)
let in_child fd body =
  let line =
    match
      silence_output ();
      body ()
    with
    | line -> line
    | exception _ -> "crashed"
  in
  (try write_all fd (line ^ "\n") 0 with _ -> ());
  Unix._exit 0

type child = {
  line : string; (* the first newline-terminated line, or [""] *)
  status : Unix.process_status;
  expired : bool; (* the child passed its deadline *)
}

(* [setsid] puts the child and whatever its tests spawn in one group, which
   one signal kills. The parent reads the pipe to its end before it waits, so
   a child that writes more than a pipe holds cannot block, and [select]
   wakes it at the deadline. The pipe is close-on-exec: a process a test
   execs must not hold it open past its group's death.

   XXX two windows stay open. A second signal between [fork] returning and
   the pid being recorded kills the parent with the child alive, and a signal
   between [waitpid] and the reset of [interrupt.child] goes to a reaped
   pid. *)
let fork_child ~deadline body =
  flush_descriptors ();
  let read_fd, write_fd =
    try Unix.pipe ~cloexec:true ()
    with Unix.Unix_error (e, _, _) -> raise (failed "pipe" e)
  in
  match Unix.fork () with
  | exception Unix.Unix_error (e, _, _) ->
      Unix.close read_fd;
      Unix.close write_fd;
      raise (failed "fork" e)
  | exception Failure _ ->
      (* OCaml 5 refuses to fork a process that has ever spawned a domain. *)
      Unix.close read_fd;
      Unix.close write_fd;
      raise (Supervision domain_refusal)
  | 0 ->
      (try Unix.close read_fd with _ -> ());
      (try ignore (Unix.setsid ()) with _ -> ());
      in_child write_fd body
  | pid ->
      interrupt.child <- Some pid;
      Unix.close write_fd;
      let buffer = Buffer.create 128 in
      let chunk = Bytes.create 4096 in
      let started = Os.counter () in
      (* The signal is looked for before every [select]: the handler may have
         run where no system call was there to interrupt. *)
      let rec watch () =
        let remaining = deadline -. Os.count_s started in
        if Option.is_some interrupt.signal then (
          kill_group pid;
          false)
        else if remaining <= 0. then (
          kill_group pid;
          true)
        else
          match Unix.select [ read_fd ] [] [] remaining with
          | [], _, _ -> watch ()
          | _ -> (
              match Unix.read read_fd chunk 0 (Bytes.length chunk) with
              | 0 -> false
              | n ->
                  Buffer.add_subbytes buffer chunk 0 n;
                  watch ()
              | exception Unix.Unix_error (Unix.EINTR, _, _) -> watch ())
          | exception Unix.Unix_error (Unix.EINTR, _, _) -> watch ()
      in
      let expired = watch () in
      (try Unix.close read_fd with _ -> ());
      let rec reap () =
        match Unix.waitpid [] pid with
        | _, status -> status
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> reap ()
        | exception Unix.Unix_error (e, _, _) -> raise (failed "waitpid" e)
      in
      let status =
        Fun.protect ~finally:(fun () -> interrupt.child <- None) reap
      in
      let output = Buffer.contents buffer in
      (* Bytes without their newline are a torn write, which must not read as
         a survivor. *)
      let line =
        match String.index_opt output '\n' with
        | Some i -> String.sub output 0 i
        | None -> ""
      in
      { line; status; expired }

(* [Unix.mkdir] fails on an existing entry, a symbolic link included, so the
   loop never adopts a directory planted at a predictable path of a
   world-writable one. *)
let scratch_root () =
  let base = Filename.get_temp_dir_name () in
  let pid = Unix.getpid () in
  let rec create n =
    let candidate = Filename.concat base (strf "windtrap-mutate-%d-%d" pid n) in
    match Unix.mkdir candidate 0o700 with
    | () -> candidate
    | exception Unix.Unix_error (Unix.EEXIST, _, _) when n < 64 -> create (n + 1)
    | exception Unix.Unix_error (e, _, _) ->
        raise
          (Supervision
             (strf "could not create the loop's scratch directory: %s"
                (Unix.error_message e)))
  in
  create 0

(* What every child of one loop is forked from. *)
type loop = {
  suite : string;
  config : Run.config;
  tests : Test_tree.t list;
  reach : reach;
  dry_run_wall : float;
  scratch : string; (* a child killed at its deadline removes nothing *)
}

let deadline_multiplier = 10.

(* The dry run's wall clock bounds what a child pays before its first test:
   the fork and a whole process's module initialization. *)
let child_deadline loop paths =
  let took path =
    Hashtbl.find_opt loop.reach.durations (Test_tree.path_to_string path)
  in
  let scheduled =
    List.fold_left
      (fun acc path -> acc +. Option.value ~default:0. (took path))
      0. paths
  in
  loop.dry_run_wall +. Float.max 1. (deadline_multiplier *. scheduled)

(* A child reports on one line of fixed ASCII shape: a failed test is named
   by its index in the paths the parent handed it, and a diagnostic's line
   breaks become spaces. *)
let error_line message =
  "error " ^ String.map (function '\n' | '\r' | '\t' -> ' ' | c -> c) message

let words line =
  match String.split_on_char ' ' (String.trim line) with
  | "error" :: diagnostic -> Error (String.concat " " diagnostic)
  | words -> Ok words

let run_child loop ~log ~bail ~arm ~paths line =
  let log_dir = Filename.concat loop.scratch log in
  let config = Run.for_subset loop.config ~log_dir ~bail in
  let body () =
    match arm () with
    | Error message -> error_line message
    | Ok () -> (
        let allowlist = List.map Test_tree.path_to_string paths in
        match Run.execute ~allowlist config ~suite:loop.suite loop.tests with
        | Error error -> error_line (Run.startup_message error)
        | Ok outcome -> line outcome)
  in
  let child = fork_child ~deadline:(child_deadline loop paths) body in
  Run.remove_tree log_dir;
  child

(* The determinism probe *)

let counted_failure (r : Run.result) =
  r.counted
  &&
  match r.outcome with
  | Failure.Fail _ -> true
  | Failure.Pass | Failure.Skip _ -> false

let probe_line ~paths (outcome : Run.outcome) =
  let results = Run.results outcome.run in
  let failures = List.filter counted_failure results in
  let index (r : Run.result) =
    let rec find i = function
      | [] -> -1
      | path :: paths ->
          if List.equal String.equal path r.path then i else find (i + 1) paths
    in
    find 0 paths
  in
  String.concat " "
    (strf "probe %d %d %d" (List.length results)
       (List.length (List.filter is_skip results))
       (List.length failures)
    :: List.map (fun r -> string_of_int (index r)) failures)

let not_a_number =
  "Mutation results over a non-deterministic suite are not a weaker number, \
   they are not a number"

let probe loop =
  let paths = List.rev loop.reach.executed in
  let child =
    run_child loop ~log:"probe" ~bail:false
      ~arm:(fun () -> Ok ())
      ~paths (probe_line ~paths)
  in
  let disagreement ~executed ~skipped ~failed indices =
    let named index =
      match int_of_string_opt index with
      | Some i when i >= 0 ->
          Option.map Test_tree.path_to_string (List.nth_opt paths i)
      | Some _ | None -> None
    in
    let measured = List.length paths in
    strf
      "the suite is not deterministic: the dry run executed %d test%s, \
       skipping %d and failing none; the probe executed %d, skipping %d and \
       failing %d%s. %s"
      measured
      (if measured = 1 then "" else "s")
      loop.reach.skipped executed skipped failed
      (match List.filter_map named indices with
      | [] -> ""
      | names -> " (" ^ String.concat ", " names ^ ")")
      not_a_number
  in
  if child.expired then
    Error
      ("the determinism probe exceeded its deadline: the dry run completed and \
        its unarmed re-run did not. " ^ not_a_number)
  else
    match (words child.line, child.status) with
    | Error diagnostic, _ ->
        Error ("the determinism probe refused to run: " ^ diagnostic)
    | Ok ("probe" :: executed :: skipped :: failed :: indices), Unix.WEXITED 0
      -> (
        match
          ( int_of_string_opt executed,
            int_of_string_opt skipped,
            int_of_string_opt failed )
        with
        | Some executed, Some skipped, Some failed ->
            if
              executed = List.length paths
              && skipped = loop.reach.skipped
              && failed = 0
            then Ok ()
            else Error (disagreement ~executed ~skipped ~failed indices)
        | _ -> Error "the determinism probe reported an unreadable result")
    | Ok _, _ -> Error "the determinism probe died without reporting a result"

(* Verdicts *)

(* Read off the outcome and never off its exit code, which is [2] for a
   selection that ran nothing: a fact about a filter, not about a mutant.
   The unexpected pass of a test marked xfail counts as a failure of the run
   and kills no mutant. *)
let killed_by (outcome : Run.outcome) =
  outcome.release_failures <> []
  || List.exists
       (fun (r : Run.result) -> Option.is_none r.xfail && counted_failure r)
       (Run.results outcome.run)

(* A child that recorded no test did not test its mutant: a survivor would
   send the reader to strengthen tests that never ran. *)
let verdict_line (outcome : Run.outcome) =
  if killed_by outcome || Run.results outcome.run = [] then "killed"
  else "survived"

let budget_of hits =
  if hits > (max_int - 1000) / 8 then max_int else (hits * 8) + 1000

(* A mutant that its child cannot arm ends the loop without a score: running
   on with nothing armed would score it a survivor. *)
let test_mutant loop ~index (mutant : Mutate.mutant) (site : site) =
  let paths = List.rev site.tests in
  let arm () =
    match Mutate.arm ~budget:(budget_of site.hits) mutant.id with
    | Error error -> Error (Pp.to_string Mutate.pp_arm_error error)
    | Ok _ ->
        (* The budget counts the child's evaluations, not the dry run's. *)
        Mutate.reset_reach ();
        Ok ()
  in
  let child =
    run_child loop ~log:(strf "m%d" index) ~bail:true ~arm ~paths verdict_line
  in
  if child.expired then Verdicts.Killed
  else
    match (words child.line, child.status) with
    | Error diagnostic, _ ->
        raise
          (Supervision
             (strf "%s: %s" (Mutate.id_to_string mutant.id) diagnostic))
    | Ok [ "survived" ], Unix.WEXITED 0 -> Verdicts.survived paths
    | Ok _, (Unix.WEXITED _ | Unix.WSIGNALED _ | Unix.WSTOPPED _) ->
        Verdicts.Killed

(* Recorded paths are relative to the project root; under [dune runtest] the
   working directory is inside [_build], where they do not open. *)
let read_source =
  let cache = Hashtbl.create 16 in
  let read path =
    try Some (In_channel.with_open_bin path In_channel.input_all)
    with Sys_error _ -> None
  in
  fun file ->
    match Hashtbl.find_opt cache file with
    | Some source -> source
    | None ->
        let under_root =
          match Os.project_root () with
          | root -> Result.to_option (Os.reconstruct ~root file)
          | exception Sys_error _ -> None
        in
        let source =
          match Option.bind under_root read with
          | None -> read file
          | found -> found
        in
        Hashtbl.add cache file source;
        source

let survivor ~locations (mutant : Mutate.mutant) reaching :
    Report_sections.survivor =
  let witness path : Report_sections.witness =
    let test = Test_tree.path_to_string path in
    { test; loc = Option.join (Hashtbl.find_opt locations test); exe = None }
  in
  {
    mutant =
      {
        id = Mutate.id_to_string mutant.id;
        line = mutant.id.line;
        before = mutant.before;
        after = mutant.after;
        source = read_source mutant.id.file;
      };
    witnesses = List.map witness reaching;
  }

type tested = {
  verdicts : Verdicts.t; (* of the children that ended *)
  survivors : Report_sections.survivor list; (* as their blocks printed *)
  stopped : (int * Mutate.mutant option) option;
      (* the signal that stopped the forks, and the mutant whose child it
         found running *)
}

(* One child per reached mutant, in catalogue order. A signal is honoured
   when a child has ended, never before a fork, so the mutant named as
   interrupted is one the loop started. *)
let test_mutants renderer loop reached =
  let locations = Hashtbl.create 256 in
  List.iter
    (fun (case : Test_tree.case) ->
      Hashtbl.replace locations (Test_tree.path_to_string case.path) case.loc)
    (Test_tree.flatten loop.tests);
  let total = List.length reached in
  let rec next index verdicts survivors = function
    | [] -> { verdicts; survivors = List.rev survivors; stopped = None }
    | ((mutant : Mutate.mutant), site) :: rest -> (
        Report.mutation_testing renderer ~index:(index + 1) ~total
          ~id:(Mutate.id_to_string mutant.id);
        let verdict = test_mutant loop ~index mutant site in
        match interrupt.signal with
        | Some signal ->
            {
              verdicts;
              survivors = List.rev survivors;
              stopped = Some (signal, Some mutant);
            }
        | None ->
            let survivors =
              match verdict with
              | Verdicts.Survived { first; others } ->
                  let found = survivor ~locations mutant (first :: others) in
                  Report.mutation_survivor renderer found;
                  found :: survivors
              | Verdicts.Killed | Verdicts.Unreached -> survivors
            in
            let record = Verdicts.record_of_mutant mutant verdict in
            next (index + 1) (Verdicts.add verdicts record) survivors rest)
  in
  next 0 Verdicts.empty [] reached

(* The scratch directory is removed however the forks end. A signal
   outranks the disagreement of a probe it killed. *)
let fork renderer loop reached =
  Fun.protect ~finally:(fun () -> Run.remove_tree loop.scratch) @@ fun () ->
  match (probe loop, interrupt.signal) with
  | Ok (), _ -> Ok (test_mutants renderer loop reached)
  | Error _, Some signal ->
      Ok
        {
          verdicts = Verdicts.empty;
          survivors = [];
          stopped = Some (signal, None);
        }
  | (Error _ as refused), None -> refused

(* The loop *)

let narrows_suite (config : Run.config) ~focus =
  config.filter <> [] || config.exclude <> [] || config.tags <> []
  || config.exclude_tags <> [] || config.failed_only || focus
  || config.shard <> None

(* What the loop has to say about its verdict file, above the outcome line.
   Under a scope, the records of the other files stay when this build wrote
   them. *)
let save_verdicts ~scope ~narrowed ~unreached tested =
  match tested.stopped with
  | Some _ -> None
  | None when narrowed ->
      Some
        "verdicts not saved: this run's selection narrows the suite, and a \
         partial run's verdicts would stand in the project merge as the whole."
  | None -> (
      let exe = Sys.executable_name in
      let path = Verdicts.output_file ~exe in
      let identity = Verdicts.writer_identity ~exe in
      let kept =
        match (identity, Verdicts.load path) with
        | Some identity, Ok (prior, Some writer) when writer = identity ->
            List.filter
              (fun (r : Verdicts.record) -> not (in_scope ~scope r.id))
              (Verdicts.records prior)
        | _ -> []
      in
      let unreached =
        List.map
          (fun m -> Verdicts.record_of_mutant m Verdicts.Unreached)
          unreached
      in
      let verdicts =
        List.fold_left Verdicts.add tested.verdicts (unreached @ kept)
      in
      match Verdicts.save ?identity path verdicts with
      | () -> None
      | exception Sys_error message ->
          Some (strf "could not write the verdict file: %s" message))

(* A signal that came once the last child had ended stopped nothing: the
   report ends as usual, and the loop still dies by it. *)
let finish renderer ~narrowed ~reach ~reached ~unreached forks =
  match forks with
  | Error message ->
      Report.mutation_refused renderer message;
      Reported 1
  | Ok _ when interrupt.signal = Some Sys.sigpipe -> Os.die_by Sys.sigpipe
  | Ok ({ verdicts; survivors; stopped }, note) -> (
      let records = Verdicts.records verdicts in
      let is_killed (r : Verdicts.record) =
        match r.verdict with
        | Verdicts.Killed -> true
        | Verdicts.Survived _ | Verdicts.Unreached -> false
      in
      let report : Report_sections.mutation =
        {
          survivors;
          unreached =
            List.map
              (fun (m : Mutate.mutant) -> (m.id.file, m.id.line))
              unreached;
          killed = List.length (List.filter is_killed records);
          not_tested = List.length reached - List.length records;
          scope =
            (if narrowed then
               Report_sections.Selected (List.length reach.executed)
             else Report_sections.Suite);
        }
      in
      match stopped with
      | Some (signal, testing) ->
          Report.mutation_interrupted renderer
            ~testing:
              (Option.map
                 (fun (m : Mutate.mutant) -> Mutate.id_to_string m.id)
                 testing)
            report;
          flush_descriptors ();
          Os.die_by signal
      | None -> (
          Report.mutation_finish ?note renderer report;
          flush_descriptors ();
          match interrupt.signal with
          | Some signal -> Os.die_by signal
          | None -> Reported 0))

let loop renderer ~scope ~suite (config : Run.config) tests =
  let population = population ~scope in
  let reach =
    {
      sites = Hashtbl.create 256;
      durations = Hashtbl.create 256;
      executed = [];
      skipped = 0;
    }
  in
  let started = Os.counter () in
  match
    Report.run ~on_event:(observe reach) ~suite
      { config with junit = None }
      tests
  with
  | Error _ -> Reported 1
  | Ok outcome -> (
      (* The last test's teardown. *)
      ignore (Mutate.drain ());
      if outcome.exit_code = 2 then
        refuse "no test ran, so there is nothing to mutate"
      else if outcome.exit_code <> 0 then
        refuse
          "the dry run is red. Mutation scores a passing suite; a score over a \
           failing one is not a score"
      else
        match population with
        | Error reason -> refuse (refusal ~scope reason)
        | Ok population ->
            let reached, unreached =
              List.partition_map
                (fun (m : Mutate.mutant) ->
                  match Hashtbl.find_opt reach.sites m.id with
                  | Some site -> Either.Left (m, site)
                  | None -> Either.Right m)
                population
            in
            let dry_run_wall = Float.max 0.01 (Os.count_s started) in
            let narrowed = narrows_suite config ~focus:outcome.focus_active in
            (* Signals are handled until the verdict file is written, so a
               loop that ended has its file whatever becomes of its
               report. *)
            let forks =
              try
                with_interrupts @@ fun () ->
                let loop =
                  let scratch = scratch_root () in
                  { suite; config; tests; reach; dry_run_wall; scratch }
                in
                Result.map
                  (fun tested ->
                    (tested, save_verdicts ~scope ~narrowed ~unreached tested))
                  (fork renderer loop reached)
              with
              | Supervision message -> Error message
              | Sys_error _ when interrupt.signal = Some Sys.sigpipe ->
                  Os.die_by Sys.sigpipe
            in
            finish renderer ~narrowed ~reach ~reached ~unreached forks)

(* The armed run *)

(* The evaluations of the armed site inside tests marked xfail, which reach
   no mutant, saturating at [max_int]. *)
type xfail_hits = { mutable at_start : int; mutable hits : int }

let count_xfail_hits counter (event : Run.event) =
  match event with
  | Run.Test_started _ -> counter.at_start <- Mutate.armed_hits ()
  | Run.Test_finished { xfail = Some _; _ } ->
      let hits = Mutate.armed_hits () - counter.at_start in
      counter.hits <-
        (if counter.hits > max_int - hits then max_int else counter.hits + hits)
  | Run.Test_finished { xfail = None; _ }
  | Run.Run_started _ | Run.Fixture_release _ | Run.Interrupted _ ->
      ()

let xfail_failed (outcome : Run.outcome) =
  List.exists
    (fun (r : Run.result) -> Option.is_some r.xfail && counted_failure r)
    (Run.results outcome.run)

let armed renderer ~spec ~suite (config : Run.config) tests =
  match Result.bind (Mutate.id_of_string spec) Mutate.arm with
  | Error (Mutate.Uncatalogued _ as error) ->
      Os.say (Pp.to_string Mutate.pp_arm_error error);
      ordinary ~suite config tests
  | Error error -> refuse (Pp.to_string Mutate.pp_arm_error error)
  | Ok mutant ->
      let id = Mutate.id_to_string mutant.id in
      let config =
        { config with baseline = Baseline.Check; mutation = Run.Armed id }
      in
      Report.mutation_armed renderer ~id ~before:mutant.before
        ~after:mutant.after;
      flush_descriptors ();
      Mutate.reset_reach ();
      let xfail = { at_start = 0; hits = 0 } in
      let result =
        Report.run ~on_event:(count_xfail_hits xfail) ~suite config tests
      in
      (match result with
      | Ok outcome when killed_by outcome -> Report.mutation_killed renderer
      | Ok outcome when outcome.exit_code <> 2 -> (
          let hits = Mutate.armed_hits () in
          match (hits - xfail.hits, xfail.hits) with
          | 0, 0 -> Report.mutation_not_evaluated renderer
          | 0, _ -> Report.mutation_not_reached renderer
          | _ ->
              Report.mutation_survived renderer ~hits
                ~xfail_failed:(xfail_failed outcome))
      | Ok _ | Error _ -> ());
      flush_descriptors ();
      Ran result

let execute_and_report ~suite (config : Run.config) tests =
  match config.mutation with
  | Run.No_mutation -> ordinary ~suite config tests
  | Run.Armed spec -> armed (Report.terminal config) ~spec ~suite config tests
  | Run.Loop _ when Sys.win32 ->
      refuse "mutation testing needs Unix.fork, which Windows does not have"
  | Run.Loop scope when not config.broadcast.mutate ->
      loop (Report.terminal config) ~scope ~suite config tests
  | Run.Loop scope -> (
      match (population ~scope, Run.list_selection config ~suite tests) with
      | Error reason, _ ->
          Os.say (unmutated ~scope reason);
          ordinary ~suite config tests
      | Ok _, Ok [] -> ordinary ~suite config tests
      | Ok _, (Ok (_ :: _) | Error _) ->
          loop (Report.terminal config) ~scope ~suite config tests)
