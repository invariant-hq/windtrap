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

let ordinary ~suite (config : Run.config) tests =
  Report.run ~suite { config with mutation = Run.No_mutation } tests

(* Population *)

let in_scope ~(scope : Run.scope) (id : Mutate.id) =
  (scope.prefixes = []
  || List.exists
       (fun prefix -> String.starts_with ~prefix id.file)
       scope.prefixes)
  &&
  match scope.lines with
  | None -> true
  | Some lines -> Lines.mem lines ~file:id.file ~line:id.line

(* The scope as the reader gave it: the prefixes, then the lines. *)
let spelled ~prefixes ~lines (scope : Run.scope) =
  String.concat " "
    ((match scope.prefixes with [] -> [] | ps -> [ prefixes ps ])
    @
    match scope.lines with
    | None -> []
    | Some l -> [ lines (Lines.to_string l) ])

type no_mutant =
  | Uninstrumented
  | Out_of_scope
  | No_site_on_lines (* the lines name no line, or none with a site *)
  | All_dismissed

(* Lines that name no line, or only lines of instrumented files with no site
   on them, describe the change, not a mistake: a change to comments or
   documentation leaves nothing to mutate. Lines that name no instrumented
   file stay [Out_of_scope], as a typo would. *)
let population ~scope =
  let catalogue = Mutate.catalogue () in
  let scoped =
    List.filter (fun (m : Mutate.mutant) -> in_scope ~scope m.id) catalogue
  in
  let undismissed =
    List.filter (fun (m : Mutate.mutant) -> Option.is_none m.dismissed) scoped
  in
  let names_a_file lines =
    Lines.is_empty lines
    ||
    let files = Lines.files lines in
    List.exists
      (fun (m : Mutate.mutant) ->
        List.mem m.id.file files
        && in_scope ~scope:{ scope with lines = None } m.id)
      catalogue
  in
  match (catalogue, scoped, undismissed) with
  | [], _, _ when scope = Run.every_mutant -> Error Uninstrumented
  | _ :: _, [], _
    when Option.fold ~none:false ~some:names_a_file scope.Run.lines ->
      Error No_site_on_lines
  | _, [], _ -> Error Out_of_scope
  | _, _, [] -> Error All_dismissed
  | _, _, population -> Ok population

let no_site_on_lines =
  "the selected lines hold no mutation site, so nothing on them is \
   mutation-tested"

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
        "%s leaves no mutant in this executable's catalogue: no instrumented \
         file matches the %s (is the library under test instrumented with \
         ppx_windtrap.mutate?), or the matched %s no mutation sites"
        (spelled scope
           ~prefixes:(fun ps -> "--mutate=" ^ String.concat "," ps)
           ~lines:(fun l -> "--lines=" ^ l))
        (if scope.lines = None then "prefix" else "scope")
        (if scope.lines = None then "files have" else "lines hold")
  | No_site_on_lines -> no_site_on_lines
  | All_dismissed ->
      "every mutant this run could test is dismissed by [@mutate off]; there \
       is nothing to test"

let unmutated ~scope reason =
  let why =
    match reason with
    | Uninstrumented -> "this executable links no instrumented module"
    | Out_of_scope ->
        strf "no mutant of this executable's catalogue is %s"
          (spelled scope
             ~prefixes:(fun ps -> "under " ^ String.concat "," ps)
             ~lines:(fun l -> "on " ^ l))
    | No_site_on_lines -> no_site_on_lines
    | All_dismissed ->
        "every mutant of this executable is dismissed by [@mutate off]"
  in
  strf "WINDTRAP_MUTATE is set, but %s, so the suite runs without mutation" why

(* The reach map *)

type site = {
  mutable tests : string list list; (* the reaching tests, newest first *)
  mutable hits : int; (* the evaluations, saturating at [max_int] *)
}

type reach = {
  sites : (Mutate.id, site) Hashtbl.t;
  outside : (Mutate.id, unit) Hashtbl.t; (* evaluated outside every test *)
}

let xfail_paths tests =
  let paths = Hashtbl.create 16 in
  List.iter
    (fun (case : Test_tree.case) ->
      if Option.is_some case.xfail then Hashtbl.replace paths case.path ())
    (Test_tree.flatten tests);
  Hashtbl.mem paths

let saturating_add a b = if a > max_int - b then max_int else a + b

(* What the workers of the dry run report. A test marked xfail reaches no
   mutant, so what it evaluated is dropped. *)
let observe reach ~xfail test sites =
  match test with
  | None -> List.iter (fun (id, _) -> Hashtbl.replace reach.outside id ()) sites
  | Some path when xfail path -> ()
  | Some path ->
      List.iter
        (fun (id, hits) ->
          match Hashtbl.find_opt reach.sites id with
          | None -> Hashtbl.add reach.sites id { tests = [ path ]; hits }
          | Some site ->
              site.tests <- path :: site.tests;
              site.hits <- saturating_add site.hits hits)
        sites

(* The verdict file *)

let narrows_suite (config : Run.config) ~focus =
  config.filter <> [] || config.exclude <> [] || config.tags <> []
  || config.exclude_tags <> [] || config.failed_only || focus
  || config.shard <> None

(* What this build's file holds, if the file is this build's. *)
let this_build () =
  let exe = Windtrap_runtime.Instr.executable in
  match
    (Verdicts.writer_identity ~exe, Verdicts.load (Verdicts.output_file ~exe))
  with
  | Some identity, Ok ({ identity = Some writer; _ } as file)
    when writer = identity ->
      Some file
  | _ -> None

(* Under a scope, the records of the other files stay when this build wrote
   them and tested its suite. *)
let save ~scope ?refused records =
  let exe = Windtrap_runtime.Instr.executable in
  let kept =
    match (this_build (), refused) with
    | Some { verdicts; refused = None; _ }, None ->
        List.filter
          (fun (r : Verdicts.record) -> not (in_scope ~scope r.id))
          (Verdicts.records verdicts)
    | _ -> []
  in
  let verdicts = List.fold_left Verdicts.add Verdicts.empty (records @ kept) in
  match
    Verdicts.save
      ?identity:(Verdicts.writer_identity ~exe)
      ?refused
      (Verdicts.output_file ~exe)
      verdicts
  with
  | () -> None
  | exception Sys_error message ->
      Some (strf "could not write the verdict file: %s" message)

let recorded verdict = List.map (fun m -> Verdicts.record_of_mutant m verdict)

(* What a suite that could not be tested holds as unknown: what its run
   would have judged. A dry run that ran whole and green says what it never
   reached. *)
let unknown_records ?reach population reason =
  match reach with
  | None -> recorded (Verdicts.Unknown reason) population
  | Some (reached, unreached, outside) ->
      recorded (Verdicts.Unknown reason) (List.map fst reached)
      @ recorded Verdicts.Unreached unreached
      @ recorded Verdicts.Outside_tests outside

(* A refusal still writes the file, which says why, so the merge names the
   suite and its mutants never read as the other suites' survivors. *)
let refuse ?(say = true) ?reach ~scope population message =
  if say then Os.say message;
  Option.iter Os.say
    (save ~scope ~refused:message (unknown_records ?reach population message));
  Error (Run.Mutation_refused message)

let narrowing =
  "its selection narrowed the suite, so it saved no verdict: run it whole"

(* A narrowed run cannot unsay what a whole run of the same build found. *)
let save_narrowed ~scope population =
  let note =
    "verdicts not saved: this run's selection narrows the suite, and a partial \
     run's verdicts would stand in the project merge as the whole."
  in
  match this_build () with
  | Some _ -> Some note
  | None -> (
      match
        save ~scope ~refused:narrowing (unknown_records population narrowing)
      with
      | None -> Some note
      | Some failed -> Some (note ^ " " ^ failed))

(* Jobs *)

let deadline_multiplier = 10.

(* The dry run's wall clock bounds what a job pays besides its tests: the
   start of its worker, and its share of a loaded machine. *)
let deadline ~wall ~durations paths =
  let took path = Option.value ~default:0. (Hashtbl.find_opt durations path) in
  let scheduled = List.fold_left (fun acc path -> acc +. took path) 0. paths in
  wall +. Float.max 1. (deadline_multiplier *. scheduled)

let budget_of hits =
  if hits > (max_int - 1000) / 8 then max_int else (hits * 8) + 1000

let counted_failure (r : Run.result) =
  r.counted
  && match r.outcome with Failure.Fail _ -> true | Pass | Skip _ -> false

let is_skip (r : Run.result) =
  match r.outcome with Failure.Skip _ -> true | Pass | Fail _ -> false

let not_a_number =
  "Mutation results over a non-deterministic suite are not a weaker number, \
   they are not a number"

(* The probe agrees iff it executed as many tests as the dry run, skipped as
   many and counted no failure. *)
let probe ~dry_run (ran : Supervisor.ran) =
  let executed = List.length dry_run in
  let skipped = List.length (List.filter is_skip dry_run) in
  let failed = List.filter counted_failure ran.results in
  let ran_skipped = List.length (List.filter is_skip ran.results) in
  match ran.ended with
  | Supervisor.Expired ->
      Error
        ("the determinism probe exceeded its deadline: the dry run completed \
          and its unarmed re-run did not. " ^ not_a_number)
  | Lost how ->
      Error
        (strf "the determinism probe's run %s, where the dry run's did not. %s"
           how not_a_number)
  | Ran
    when List.length ran.results = executed
         && ran_skipped = skipped && failed = [] ->
      Ok ()
  | Ran ->
      Error
        (strf
           "the suite is not deterministic: the dry run executed %d test%s, \
            skipping %d and failing none; the probe executed %d, skipping %d \
            and failing %d%s. %s"
           executed
           (if executed = 1 then "" else "s")
           skipped (List.length ran.results) ran_skipped (List.length failed)
           (match failed with
           | [] -> ""
           | failed ->
               " ("
               ^ String.concat ", "
                   (List.map
                      (fun (r : Run.result) -> Test_tree.path_to_string r.path)
                      failed)
               ^ ")")
           not_a_number)

(* The unexpected pass of a test marked xfail counts as a failure of the run
   and kills no mutant. *)
let killed_by (ran : Supervisor.ran) =
  ran.release_failures <> []
  || List.exists
       (fun (r : Run.result) -> Option.is_none r.xfail && counted_failure r)
       ran.results

let seconds s = if s < 10. then strf "%.1fs" s else strf "%.0fs" s

(* A run that ended before its tests did judged nothing, and tests that did
   not evaluate the armed site tested nothing: neither is a survivor. *)
let verdict ~deadline paths (ran : Supervisor.ran) =
  match ran.ended with
  | Supervisor.Expired ->
      Verdicts.Unknown ("its run passed its deadline of " ^ seconds deadline)
  | Lost how -> Verdicts.Unknown ("its run " ^ how)
  | Ran ->
      if killed_by ran then Verdicts.Killed
      else if ran.armed_hits = 0 then Verdicts.Not_evaluated
      else Verdicts.survived paths

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

let shown (mutant : Mutate.mutant) : Report_sections.mutant =
  {
    id = Mutate.id_to_string mutant.id;
    file = mutant.id.file;
    line = mutant.id.line;
    col = mutant.id.col;
    end_line = mutant.end_line;
    end_col = mutant.end_col;
    before = mutant.before;
    after = mutant.after;
    source = read_source mutant.id.file;
  }

(* What the jobs found, newest first. *)
type found = {
  mutable records : Verdicts.record list;
  mutable survivors : Report_sections.survivor list;
  mutable not_evaluated : Report_sections.not_evaluated list;
  mutable unknown : Report_sections.unknown list;
}

(* A survivor's block is committed when its job is given, so a loop that
   dies has printed what it found. *)
let found_verdict renderer (config : Run.config) ~locations found mutant verdict
    =
  (match verdict with
  | Verdicts.Survived { first; others } ->
      let witness path : Report_sections.witness =
        let test = Test_tree.path_to_string path in
        {
          test;
          loc = Option.join (Hashtbl.find_opt locations test);
          exe = None;
        }
      in
      let survivor : Report_sections.survivor =
        {
          mutant = shown mutant;
          witnesses = List.map witness (first :: others);
        }
      in
      Report.mutation_survivor renderer survivor;
      found.survivors <- survivor :: found.survivors
  | Not_evaluated ->
      found.not_evaluated <-
        { mutant = shown mutant; invocation = config.invocation }
        :: found.not_evaluated
  | Unknown reason ->
      found.unknown <-
        { mutant = shown mutant; reason; invocation = config.invocation }
        :: found.unknown
  | Killed | Outside_tests | Unreached -> ());
  found.records <- Verdicts.record_of_mutant mutant verdict :: found.records

(* Signals *)

(* The first signal from the dry run's end until the verdict file is
   written: one that a job's handlers did not take, came after the last job.
   The handler only records, since it runs at a safepoint of whatever the
   loop was doing, the report included. SIGPIPE's default action would kill
   the loop inside a write of the report, past every [Fun.protect]; handled,
   the write fails with [Sys_error] and unwinds. *)
let signal = ref None
let record s = if Option.is_none !signal then signal := Some s

let with_interrupts fn =
  signal := None;
  Os.with_signals [ Sys.sigpipe; Sys.sigint; Sys.sigterm; Sys.sighup ] record fn

let flush_descriptors () =
  Format.pp_print_flush Format.std_formatter ();
  Format.pp_print_flush Format.err_formatter ();
  (try flush Stdlib.stdout with Sys_error _ -> ());
  try flush Stdlib.stderr with Sys_error _ -> ()

(* The loop *)

exception Refused of string

type ended =
  | Refused_by of string
  | Interrupted of int * Mutate.mutant option
  | Finished of string option

(* The budget counts the job's evaluations, not the dry run's. *)
let jobs_of ~deadline ~dry_run reached =
  let job ~bail ~armed paths =
    {
      Supervisor.cases = List.map Test_tree.path_to_string paths;
      deadline = deadline paths;
      bail;
      armed;
    }
  in
  job ~bail:false ~armed:None
    (List.map (fun (r : Run.result) -> r.path) dry_run)
  :: List.map
       (fun ((mutant : Mutate.mutant), site) ->
         job ~bail:true
           ~armed:(Some (mutant.id, budget_of site.hits))
           (List.rev site.tests))
       reached

(* Every job runs in a fresh worker, the probe first and then one per reached
   mutant in catalogue order, up to [-j] at once; they are given in that
   order. *)
let test_mutants renderer ~suite (config : Run.config) tests ~dry_run ~wall
    ~save reached =
  let durations = Hashtbl.create 256 in
  List.iter
    (fun (r : Run.result) -> Hashtbl.replace durations r.path r.duration)
    dry_run;
  let deadline = deadline ~wall ~durations in
  let locations = Hashtbl.create 256 in
  List.iter
    (fun (case : Test_tree.case) ->
      Hashtbl.replace locations (Test_tree.path_to_string case.path) case.loc)
    (Test_tree.flatten tests);
  let found =
    { records = []; survivors = []; not_evaluated = []; unknown = [] }
  in
  let mutants = Array.of_list reached in
  let total = Array.length mutants in
  let given = ref 0 in
  let testing i =
    if i < total then
      Report.mutation_testing renderer ~index:(i + 1) ~total
        ~id:(Mutate.id_to_string (fst mutants.(i) : Mutate.mutant).id)
  in
  let on_ran index (ran : Supervisor.ran) =
    given := index + 1;
    if index = 0 then
      match probe ~dry_run ran with
      | Ok () -> testing 0
      | Error message -> raise (Refused message)
    else
      let mutant, site = mutants.(index - 1) in
      let paths = List.rev site.tests in
      found_verdict renderer config ~locations found mutant
        (verdict ~deadline:(deadline paths) paths ran);
      testing index
  in
  (* Signals are handled until the verdict file is written, so a loop that
     ended has its file whatever becomes of its report. *)
  let ended =
    with_interrupts @@ fun () ->
    match
      Supervisor.fresh config ~suite tests
        (jobs_of ~deadline ~dry_run reached)
        ~on_ran ~on_signal:record
    with
    | exception Refused message -> Refused_by message
    | () -> (
        match !signal with
        | Some s when !given <= total ->
            Interrupted
              (s, if !given = 0 then None else Some (fst mutants.(!given - 1)))
        | Some _ | None -> Finished (save (List.rev found.records)))
  in
  (ended, found)

let report ~narrowed ~dry_run ~reached ~unreached ~outside found :
    Report_sections.mutation =
  let lines = List.map (fun (m : Mutate.mutant) -> (m.id.file, m.id.line)) in
  {
    survivors = List.rev found.survivors;
    not_evaluated = List.rev found.not_evaluated;
    unknown = List.rev found.unknown;
    refused = [];
    unreached = lines unreached;
    outside_tests = lines outside;
    killed =
      List.length
        (List.filter
           (fun (r : Verdicts.record) -> r.verdict = Verdicts.Killed)
           found.records);
    not_tested = List.length reached - List.length found.records;
    scope =
      (if narrowed then Report_sections.Selected (List.length dry_run)
       else Report_sections.Suite);
  }

let loop renderer ~scope ~suite (config : Run.config) tests =
  let population = population ~scope in
  let all = Result.value population ~default:[] in
  let reach = { sites = Hashtbl.create 256; outside = Hashtbl.create 16 } in
  let started = Os.counter () in
  let refuse ?say ?reach message = refuse ?say ?reach ~scope all message in
  if not (Supervisor.can_spawn ()) then
    refuse
      "this run is in a process that has spawned a domain, after which OCaml \
       refuses Unix.fork, and that the test executable did not start as, so a \
       new start of the executable would not reach the run: mutation testing \
       runs every mutant in a fresh worker, so it cannot run here. Spawn the \
       domain in a test instead"
  else
    let on_reach = observe reach ~xfail:(xfail_paths tests) in
    match
      Report.run
        ~execute:(Supervisor.execute ~on_reach)
        ~suite
        { config with junit = None }
        tests
    with
    | Error error ->
        (* [Report.run] said why. *)
        refuse ~say:false
          ("its dry run was refused: " ^ Run.startup_message error)
    | Ok outcome -> (
        let dry_run = Run.results outcome.run in
        if dry_run = [] then refuse "no test ran, so there is nothing to mutate"
        else if Run.exit_code (Ok outcome) <> 0 then
          refuse
            "the dry run is red. Mutation scores a passing suite; a score over \
             a failing one is not a score"
        else
          match population with
          | Error No_site_on_lines ->
              Os.say no_site_on_lines;
              Ok outcome
          | Error reason -> refuse (refusal ~scope reason)
          | Ok population -> (
              let reached, unreached =
                List.partition_map
                  (fun (m : Mutate.mutant) ->
                    match Hashtbl.find_opt reach.sites m.id with
                    | Some site -> Either.Left (m, site)
                    | None -> Either.Right m)
                  population
              in
              let outside, unreached =
                List.partition
                  (fun (m : Mutate.mutant) -> Hashtbl.mem reach.outside m.id)
                  unreached
              in
              let narrowed = narrows_suite config ~focus:outcome.focus_active in
              let save records =
                if narrowed then save_narrowed ~scope population
                else
                  save ~scope
                    (records
                    @ recorded Verdicts.Outside_tests outside
                    @ recorded Verdicts.Unreached unreached)
              in
              let wall = Float.max 0.01 (Os.count_s started) in
              match
                test_mutants renderer ~suite config tests ~dry_run ~wall ~save
                  reached
              with
              | exception Sys_error _ when !signal = Some Sys.sigpipe ->
                  Os.die_by Sys.sigpipe
              | Refused_by message, _ ->
                  Report.mutation_refused renderer message;
                  refuse ~say:false ~reach:(reached, unreached, outside) message
              | (Finished _ | Interrupted _), _ when !signal = Some Sys.sigpipe
                ->
                  Os.die_by Sys.sigpipe
              | Interrupted (s, testing), found ->
                  Report.mutation_interrupted renderer
                    ~testing:
                      (Option.map
                         (fun (m : Mutate.mutant) -> Mutate.id_to_string m.id)
                         testing)
                    (report ~narrowed ~dry_run ~reached ~unreached ~outside
                       found);
                  flush_descriptors ();
                  Os.die_by s
              | Finished note, found -> (
                  Report.mutation_finish ?note renderer
                    (report ~narrowed ~dry_run ~reached ~unreached ~outside
                       found);
                  flush_descriptors ();
                  match !signal with
                  | Some s -> Os.die_by s
                  | None -> Ok outcome)))

(* The armed run *)

let armed renderer ~spec ~suite (config : Run.config) tests =
  match Result.bind (Mutate.id_of_string spec) Mutate.arm with
  | Error (Mutate.Uncatalogued _ as error) ->
      Os.say (Pp.to_string Mutate.pp_arm_error error);
      ordinary ~suite config tests
  | Error error ->
      let message = Pp.to_string Mutate.pp_arm_error error in
      Os.say message;
      Error (Run.Mutation_refused message)
  | Ok mutant ->
      let id = Mutate.id_to_string mutant.id in
      let config =
        { config with baseline = Baseline.Check; mutation = Run.Armed id }
      in
      Report.mutation_armed renderer ~id ~before:mutant.before
        ~after:mutant.after;
      flush_descriptors ();
      (* What module initialization evaluated ran before the mutant was. *)
      Mutate.reset_reach ();
      (* The evaluations of the armed site inside tests marked xfail, which
         reach no mutant, and elsewhere. *)
      let xfail = xfail_paths tests in
      let hits = ref 0 and xfail_hits = ref 0 in
      let on_reach test sites =
        List.iter
          (fun (site, n) ->
            if Mutate.compare_id site mutant.id = 0 then
              match test with
              | Some path when xfail path ->
                  xfail_hits := saturating_add !xfail_hits n
              | Some _ | None -> hits := saturating_add !hits n)
          sites
      in
      let result =
        Report.run ~execute:(Supervisor.execute ~on_reach) ~suite config tests
      in
      let xfail_failed (outcome : Run.outcome) =
        List.exists
          (fun (r : Run.result) -> Option.is_some r.xfail && counted_failure r)
          (Run.results outcome.run)
      in
      let killed (outcome : Run.outcome) =
        outcome.release_failures <> []
        || List.exists
             (fun (r : Run.result) ->
               Option.is_none r.xfail && counted_failure r)
             (Run.results outcome.run)
      in
      (match result with
      | Ok outcome when killed outcome -> Report.mutation_killed renderer
      | Ok outcome when Run.results outcome.run <> [] -> (
          match (!hits, !xfail_hits) with
          | 0, 0 -> Report.mutation_not_evaluated renderer
          | 0, _ -> Report.mutation_not_reached renderer
          | _ ->
              Report.mutation_survived renderer
                ~hits:(saturating_add !hits !xfail_hits)
                ~xfail_failed:(xfail_failed outcome))
      | Ok _ | Error _ -> ());
      flush_descriptors ();
      result

let execute_and_report ~suite (config : Run.config) tests =
  match config.mutation with
  | Run.No_mutation -> ordinary ~suite config tests
  | Run.Armed spec -> armed (Report.terminal config) ~spec ~suite config tests
  | Run.Loop _ when Sys.win32 ->
      let message =
        "mutation testing needs worker processes, which windtrap does not \
         start on Windows"
      in
      Os.say message;
      Error (Run.Mutation_refused message)
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
