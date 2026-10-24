(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Not mutated: this module judges mutants (see lib/dune). *)
[@@@mutate exclude_file]

let strf = Printf.sprintf
let ( let* ) = Result.bind

(* Configuration *)

type invocation = [ `Exe of string | `Mirrors ]
type scope = { prefixes : string list; lines : Lines.t option }
type mutation = No_mutation | Loop of scope | Armed of string

let every_mutant = { prefixes = []; lines = None }

type broadcast = { selection : bool; mutate : bool }
type case_file = { file : string; choices : int64 array }

type config = {
  seed : Seed.seed;
  filter : string list;
  exclude : string list;
  tags : string list;
  exclude_tags : string list;
  shard : (int * int) option;
  failed_only : bool;
  bail : bool;
  stream : bool;
  jobs : int;
  in_process : bool;
  baseline : Baseline.mode;
  timeout : float option;
  default_timeout : float;
  prop_count : int option;
  replay : string option;
  case_file : case_file option;
  log_dir : string;
  color : Os.color_mode;
  slow_threshold : float;
  verbose : bool;
  junit : string option;
  mutation : mutation;
  github : bool;
  invocation : invocation;
  broadcast : broadcast;
}

let not_broadcast = { selection = false; mutate = false }

let default_config () =
  {
    seed = Seed.random ();
    filter = [];
    exclude = [];
    tags = [];
    exclude_tags = [];
    shard = None;
    failed_only = false;
    bail = false;
    stream = false;
    jobs = 1;
    in_process = false;
    baseline = Baseline.Check;
    timeout = None;
    default_timeout = 600.;
    prop_count = None;
    replay = None;
    case_file = None;
    log_dir = Os.default_log_dir ();
    color = Os.Auto;
    slow_threshold = 1.0;
    verbose = false;
    junit = None;
    mutation = No_mutation;
    github = false;
    invocation = `Mirrors;
    broadcast = not_broadcast;
  }

(* Run records *)

(* The value of an acquisition rides in its accessor's own exception, so the
   cache of a run holds accessors of every type. A skip or a fault is raised
   again to every later caller. *)
type fixture_state =
  | Acquired of exn
  | Raised of [ `Skip of string option | Failure.fault ]

type release = { name : string; loc : Loc.t option; teardown : unit -> unit }

(* A case within a test, as the last-failed store names it. *)
type case_id = Row of string | Replay of string

type result = {
  path : string list;
  outcome : Failure.outcome;
  counted : bool;
  xfail : Test_tree.xfail option;
  slow_tagged : bool;
  duration : float;
  attempts : int;
  prop_stats : Property.stats option;
  refuted : case_id list;
}

(* A baseline check that a worker leaves to its parent under a correcting
   mode: the call, the subtests open around it, and the number of the
   attempt's other failures recorded before it. The parent makes it in its
   own registry, in the order of the selection ([judge]). *)
type check = {
  subject : Baseline.subject;
  actual : string;
  correct : bool;
  site : Loc.t option;
  within : string list; (* the open subtests, innermost first *)
  before : int;
}

(* A row of a table as its attempt ran it: its identity, the attempt's
   failures and checks counted when it ended, why it may keep no correction,
   and, where the row's checks were made as it ran, its settlement: whether
   it kept its corrections and how many. *)
type row = {
  id : string;
  failures_to : int;
  checks_to : int;
  excuse : Failure.withheld option;
  settled : (bool * int) option;
}

(* An attempt as data: its failures and its deferred checks, each in order,
   the rows of a table, and its skip. *)
type attempt = {
  failures : Failure.t list;
  checks : check list;
  rows : row list;
  skip : string option option;
}

(* The outcome of an attempt, whether each of its failures carries a kept
   correction, whether it must be the last (see [settle]), and the cases it
   refuted. *)
type verdict = Failure.outcome * bool * bool * case_id list

type t = {
  config : config;
  domain : Domain.id; (* the domain that runs the tests *)
  capture : Capture.t;
  baselines : Baseline.t;
  mutable judge : (attempt -> verdict) option;
      (* in a worker under a correcting mode: the parent's verdict on an
         attempt that a retry waits for *)
  fixtures : (int, fixture_state) Hashtbl.t; (* by accessor id *)
  mutable held : release list; (* the releases still due, newest first *)
  mutable releasing : release option; (* the fixture whose release runs *)
  mutable rev_results : result list;
  mutable interrupted : int option;
      (* a signal that arrived in the runner's own code, acted on at the next
         boundary *)
  mutable stopped : string list option; (* the test after which the run stops *)
  recorded : (string, case_id list) Hashtbl.t;
      (* under --failed: the cases of each test that the store names *)
}

let create config ~capture ~baselines =
  {
    config;
    domain = Domain.self ();
    capture;
    baselines;
    judge = None;
    fixtures = Hashtbl.create 8;
    held = [];
    releasing = None;
    rev_results = [];
    interrupted = None;
    stopped = None;
    recorded = Hashtbl.create 8;
  }

let config t = t.config
let capture t = t.capture
let baselines t = t.baselines
let results t = List.rev t.rev_results
let stopped t = t.stopped

(* Events *)

(* Declared before the frames, which give [Shrinking] and [Shrunk] from
   inside an attempt. *)
type event =
  | Run_started of {
      suite : string;
      total : int;
      selected : int;
      properties : bool;
    }
  | Test_started of { path : string list }
  | Shrinking of { path : string list; steps : int }
  | Shrunk of { path : string list }
  | Test_finished of result
  | Fixture_release of { name : string }
  | Interrupted of {
      running : string list option;
      releasing : string option;
      results : result list;
      duration : float;
    }

(* Frames *)

(* What a [setenv] changed, put back when the attempt ends. *)
type binding = { name : string; prior : string option; loc : Loc.t option }

type frame = {
  run : t;
  on_event : event -> unit; (* the run's observer *)
  path : string list;
  loc : Loc.t option; (* the declaration site *)
  corrections : bool; (* whether a check may record a correction *)
  mutable phase : Failure.phase; (* the phase that runs *)
  mutable skip : string option option; (* the reason of the first skip *)
  mutable prop : Property.context option;
  mutable law_mark : int option; (* where the law's run began in the log *)
  mutable prop_stats : Property.stats option;
  mutable rev_failures : Failure.t list;
  mutable rev_checks : check list; (* left to the parent, newest first *)
  mutable rev_rows : row list; (* the rows of a table, newest first *)
  mutable subtests : string list; (* the open subtests, innermost first *)
  mutable temp_root : string option;
  mutable temp_seq : int; (* the number of the next temporary path *)
  mutable bindings : binding list; (* one per name *)
  mutable cwd : (string * Loc.t option) option; (* left by the first chdir *)
}

let frame ~on_event run (case : Test_tree.case) =
  {
    run;
    on_event;
    path = case.path;
    loc = case.loc;
    corrections = Option.is_none case.xfail;
    phase = Failure.Body;
    skip = None;
    prop = None;
    law_mark = None;
    prop_stats = None;
    rev_failures = [];
    rev_checks = [];
    rev_rows = [];
    subtests = [];
    temp_root = None;
    temp_seq = 0;
    bindings = [];
    cwd = None;
  }

(* A failure without a location was raised in tail position, past
   [Loc.capture]'s delimiter; it takes the declaration site. A nested failure,
   the [inner] of a property failure, keeps its own. *)
let at_declaration loc (failure : Failure.t) =
  match (failure.loc, loc) with
  | None, (Some _ as loc) -> { failure with loc }
  | Some _, _ | None, None -> failure

let add_failure frame failure =
  frame.rev_failures <- at_declaration frame.loc failure :: frame.rev_failures

let add_phase_failure frame phase failure =
  add_failure frame (Failure.with_phase phase failure)

let uncaught frame exn backtrace =
  Failure.raised ?loc:frame.loc
    ~actual:(Failure.exn_to_string exn)
    ~backtrace:(Failure.backtrace_to_string backtrace)
    ()

(* The ambient slot *)

(* The only reference to run state in the library: the run while [execute]
   runs, and the frame of an attempt over it while one runs. It is atomic
   because a domain that a test spawned reads it too. *)
type context = In_test of frame | In_run of t

let slot : context option Atomic.t = Atomic.make None

let with_context context fn =
  let previous = Atomic.get slot in
  Atomic.set slot (Some context);
  Fun.protect ~finally:(fun () -> Atomic.set slot previous) fn

let active () = Option.is_some (Atomic.get slot)

let active_run_error =
  "windtrap: a run is already executing; nothing inside it can start another \
   run"

let outside_run_error =
  "windtrap: no test is running; this call works only in a test's setup, body \
   or teardown, not at module top level, in a fixture's release or after the \
   run"

let other_domain_error =
  "a function that reads the running test was called from a domain other than \
   the one running the tests; hand the result back to the test's domain"

let on_run_domain run = (Domain.self () :> int) = (run.domain :> int)

(* The frame belongs to the run's domain. Another domain gets the refusal
   raised where it called, like any exception of its own, and recorded. *)
let current_frame () =
  match Atomic.get slot with
  | Some (In_test frame) when not (on_run_domain frame.run) ->
      Failure.refuse (Failure.message ?loc:(Loc.capture ()) other_domain_error)
  | Some (In_test frame) -> frame
  | Some (In_run _) | None -> invalid_arg outside_run_error

let current () = (current_frame ()).run

let stop () =
  let frame = current_frame () in
  frame.run.stopped <- Some frame.path

(* Where a label may be placed: in the law of a property, on the test's
   domain. It raises and records nothing. *)
let prop_context () =
  match Atomic.get slot with
  | Some (In_test frame) when on_run_domain frame.run -> frame.prop
  | Some (In_test _ | In_run _) | None -> None

(* The running test *)

let current_test () = (current_frame ()).path

let split_last path =
  match List.rev path with
  | name :: rev_groups -> (List.rev rev_groups, name)
  | [] -> assert false (* a case's path is never empty *)

(* Inside a subtest a failure is labelled, as data: its [msg] stays the
   user's. A failure that an inner subtest raised out of a law keeps its
   label. *)
let label ~path ~subtests (failure : Failure.t) =
  if subtests = [] || failure.subtest <> [] then failure
  else
    let _, name = split_last path in
    { failure with subtest = name :: List.rev subtests }

let labelled frame = label ~path:frame.path ~subtests:frame.subtests

(* In a law the case is the unit of failure: a recorded failure would leave
   the engine a passing case, which it never shrinks or reports. The first
   failure, caught or raised, fails the case. *)
let subtest name fn =
  let frame = current_frame () in
  let enclosing = frame.subtests in
  frame.subtests <- name :: enclosing;
  let close () = frame.subtests <- enclosing in
  match Failure.recording fn with
  | exception fatal ->
      let backtrace = Printexc.get_raw_backtrace () in
      close ();
      Printexc.raise_with_backtrace fatal backtrace
  | ended, caught -> (
      let raised =
        match ended with
        | Error (#Failure.fault as fault) -> [ Failure.of_fault fault ]
        | Ok () | Error #Failure.control -> []
      in
      let failures = List.map (labelled frame) (caught @ raised) in
      close ();
      match failures with
      | first :: _ when Option.is_some frame.prop -> Failure.refuse first
      | failures -> (
          List.iter (add_failure frame) failures;
          match ended with
          | Error (#Failure.control as c) -> Failure.reraise c
          | Ok () | Error #Failure.fault -> ()))

(* A worker under a correcting mode checks in a [Check] registry, for the
   path's proof and the read's [Sys_error], and leaves the verdict to its
   parent, since what an earlier test accepted decides it. *)
let rec check_baseline ?loc subject actual =
  let frame = current_frame () in
  match frame.prop with
  | Some _ -> in_law frame ?loc subject actual
  | None -> check_in_test frame ?loc subject actual

(* A law claims its check for every value, so a mismatch refutes the case,
   as any verb's failure does, and is never a correction: no one text holds
   for every value. It is judged where the law runs, in the registry of
   that process. *)
and in_law frame ?loc subject actual =
  match
    Baseline.check frame.run.baselines ?loc ~correct:false subject actual
  with
  | () -> ()
  | exception Failure.Check_failure failure ->
      Failure.refuse
        (labelled frame (Failure.with_withheld Failure.In_law failure))

and check_in_test frame ?loc subject actual =
  let defer () =
    let check =
      {
        subject;
        actual;
        correct = frame.corrections;
        site = loc;
        within = frame.subtests;
        before = List.length frame.rev_failures;
      }
    in
    frame.rev_checks <- check :: frame.rev_checks
  in
  let deferred = Option.is_some frame.run.judge in
  match
    Baseline.check frame.run.baselines ?loc ~correct:frame.corrections subject
      actual
  with
  | () -> if deferred then defer ()
  | exception
      Failure.Check_failure
        ({ kind = Baseline { state = Unresolvable _; _ }; _ } as unresolvable)
    ->
      Failure.refuse unresolvable
  | exception Failure.Check_failure failure ->
      if deferred then defer () else add_failure frame (labelled frame failure)

(* Temporary paths *)

let temp_attempts = 64

(* Process-wide: two runs in one process share a pid. *)
let next_temp_root = ref 0

(* Where the temporary directories go: the system's, or a worker's own, which
   its parent removes when the worker ends. *)
let temp_base = ref None

(* A stale directory of an earlier process with the same pid takes the next
   number. *)
let temp_root frame =
  match frame.temp_root with
  | Some dir -> dir
  | None ->
      let base =
        match !temp_base with
        | Some dir -> dir
        | None -> Filename.get_temp_dir_name ()
      in
      let pid = Unix.getpid () in
      let rec make attempts =
        let n = !next_temp_root in
        incr next_temp_root;
        let dir = Filename.concat base (strf "windtrap-%d-%d" pid n) in
        match Unix.mkdir dir 0o700 with
        | () -> dir
        | exception Unix.Unix_error (Unix.EEXIST, _, _)
          when attempts < temp_attempts ->
            make (attempts + 1)
      in
      let dir = make 1 in
      frame.temp_root <- Some dir;
      dir

let temp_path frame ~prefix ~suffix =
  let root = temp_root frame in
  let n = frame.temp_seq in
  frame.temp_seq <- n + 1;
  Filename.concat root (strf "%s-%d%s" prefix n suffix)

let temp_dir ?(prefix = "dir") () =
  let frame = current_frame () in
  let prefix = Os.sanitize_component prefix in
  let dir = temp_path frame ~prefix ~suffix:"" in
  Unix.mkdir dir 0o700;
  dir

let temp_file ?(suffix = "") () =
  let frame = current_frame () in
  let suffix = if suffix = "" then "" else Os.sanitize_component suffix in
  let path = temp_path frame ~prefix:"file" ~suffix in
  Unix.close
    (Unix.openfile path
       [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC ]
       0o600);
  path

(* Listing a directory takes read and search permission, and removing its
   entries write and search: a test may have taken them from its owner. *)
let rec remove_tree path =
  match Unix.lstat path with
  | { Unix.st_kind = S_DIR; st_perm; _ } -> (
      (if st_perm land 0o700 <> 0o700 then
         try Unix.chmod path (st_perm lor 0o700) with Unix.Unix_error _ -> ());
      let entries = try Sys.readdir path with Sys_error _ -> [||] in
      Array.iter (fun name -> remove_tree (Filename.concat path name)) entries;
      try Unix.rmdir path with Unix.Unix_error _ -> ())
  | _ -> ( try Unix.unlink path with Unix.Unix_error _ -> ())
  | exception Unix.Unix_error _ -> ()

(* Process state *)

(* The prior binding is recorded only once [Os.setenv] accepted the name: a
   rejected name would otherwise fail again as a restoration. *)
let setenv name value =
  let frame = current_frame () in
  let prior = Sys.getenv_opt name in
  Os.setenv name value;
  if not (List.exists (fun (b : binding) -> b.name = name) frame.bindings) then
    frame.bindings <- { name; prior; loc = Loc.capture () } :: frame.bindings

let chdir dir =
  let frame = current_frame () in
  if Option.is_none frame.cwd then
    frame.cwd <- Some (Sys.getcwd (), Loc.capture ());
  Unix.chdir dir

(* A restoration that fails is a failure of the test, located at the call
   that made the change: the next test would otherwise fail without a
   cause. *)
let restore_failure frame ?loc text =
  add_phase_failure frame Failure.Teardown (Failure.message ?loc text)

let restore_cwd frame =
  match frame.cwd with
  | None -> ()
  | Some (dir, loc) -> (
      frame.cwd <- None;
      try Unix.chdir dir
      with Unix.Unix_error (err, _, _) ->
        restore_failure frame ?loc
          (strf
             "the test changed the working directory and it could not be \
              restored to %s: %s; every later test in this process runs from \
              the wrong place"
             dir (Unix.error_message err)))

let restore_bindings frame =
  let bindings = frame.bindings in
  frame.bindings <- [];
  let restore (b : binding) =
    match Failure.catch (fun () -> Os.setenv b.name b.prior) with
    | Ok () -> ()
    | Error c ->
        restore_failure frame ?loc:b.loc
          (strf
             "the test set %s and its prior binding could not be restored: %s; \
              every later test in this process sees the test's value"
             b.name
             (Failure.caught_to_string c))
  in
  List.iter restore bindings

(* Unlike a restoration, a removal that fails is no failure of the test: the
   next attempt makes a directory of another name. *)
let remove_temp frame =
  match frame.temp_root with
  | None -> ()
  | Some dir ->
      frame.temp_root <- None;
      remove_tree dir

(* The directory comes back first: the process may sit inside the temporary
   tree. *)
let reclaim frame =
  restore_cwd frame;
  restore_bindings frame;
  remove_temp frame

(* Fixtures *)

(* Process-wide: an accessor keeps its id across runs, and each run caches
   under it anew. *)
let next_fixture_id = ref 0

let fixture : type a. ?teardown:(a -> unit) -> (unit -> a) -> unit -> a =
 fun ?teardown create ->
  let module Cell = struct
    exception Value of a
  end in
  incr next_fixture_id;
  let id = !next_fixture_id in
  let loc = Loc.capture () in
  let name =
    match loc with
    | Some loc -> "fixture (" ^ Loc.to_string loc ^ ")"
    | None -> "fixture #" ^ string_of_int id
  in
  fun () ->
    let run = current () in
    match Hashtbl.find_opt run.fixtures id with
    | Some (Acquired (Cell.Value value)) -> value
    | Some (Acquired _) -> assert false (* the id is this accessor's *)
    | Some (Raised c) -> Failure.reraise c
    | None -> (
        match Failure.catch create with
        | Ok value ->
            Hashtbl.replace run.fixtures id (Acquired (Cell.Value value));
            let due teardown =
              run.held <-
                { name; loc; teardown = (fun () -> teardown value) } :: run.held
            in
            Option.iter due teardown;
            value
        | Error ((`Skip _ | #Failure.fault) as c) ->
            Hashtbl.replace run.fixtures id (Raised c);
            Failure.reraise c
        | Error ((`Timeout _ | `Exit | `Discard) as c) ->
            (* About the calling test, not the fixture: the next call
               acquires again. *)
            Failure.reraise c)

let release_failure (release : release) c =
  Failure.message ?loc:release.loc
    (release.name ^ ": release raised " ^ Failure.caught_to_string c)
  |> Failure.with_phase Failure.Release

(* A fixture leaves [t.held] before its teardown runs, so a signal that stops
   a release goes on with the rest and never runs it again. *)
let release_fixtures t ~announce =
  let rec release_all acc =
    match t.held with
    | [] -> List.rev acc
    | release :: held -> (
        t.held <- held;
        announce release.name;
        t.releasing <- Some release;
        let released = Failure.catch (fun () -> Loc.delimit release.teardown) in
        t.releasing <- None;
        match released with
        | Ok () -> release_all acc
        | Error c -> release_all (release_failure release c :: acc))
  in
  try release_all []
  with exn ->
    let backtrace = Printexc.get_raw_backtrace () in
    t.held <- [];
    t.releasing <- None;
    Printexc.raise_with_backtrace exn backtrace

(* Properties *)

let coverage_failure ?loc (stats : Property.stats) =
  let unsatisfied (c : Property.cover_status) =
    if c.satisfied then None else Some (Pp.str "%S" c.label)
  in
  Failure.message ?loc
    (Pp.str "never covered: %s (over %d passing cases)"
       (String.concat ", " (List.filter_map unsatisfied stats.coverage))
       stats.cases)

let gave_up_failure ?loc (stats : Property.stats) =
  Failure.message ?loc
    (Pp.str
       "property gave up: %d discards exhausted the generation budget (%d \
        cases passed)"
       stats.discards stats.cases)

(* The signals that end a run (see [with_interrupts]). *)
let interrupting = [ Sys.sigint; Sys.sigterm; Sys.sighup ]

(* An event given inside an attempt reaches the observer as the others do:
   on the real descriptors, and with neither the test's limit nor a signal
   able to cut the observer mid-line. The alarm or a signal that arrives
   meanwhile is handled when it returns, in the attempt. What it raises
   would fail a test that it does not own, so it is dropped. *)
let give_in_attempt frame event =
  Os.with_blocked (Sys.sigalrm :: interrupting) (fun () ->
      try Capture.outside frame.run.capture (fun () -> frame.on_event event)
      with _ -> ())

(* A fast law accepts thousands of candidates a second, and a terminal would
   draw each: after its start a search is given at most ten times a
   second. *)
let shrink_interval = 0.1

(* Under [--failed], the cases of the running test that the store names,
   which it reruns alone. *)
let recorded () =
  let frame = current_frame () in
  Option.value ~default:[]
    (Hashtbl.find_opt frame.run.recorded (Test_tree.path_to_string frame.path))

let property ?loc ?count ?max_discard ?examples ?summary ?runs ?only gen law =
  let frame = current_frame () in
  let config = frame.run.config and capture = frame.run.capture in
  (* The command line raises a count and never lowers one, so a long run
     scales every property; an examples-only property stays one. *)
  let count =
    let declared = Option.value count ~default:Property.default_count in
    match config.prop_count with
    | Some n when n > declared && declared > 0 -> Some (`Config n)
    | Some _ | None -> Option.map (fun n -> `Declared n) count
  in
  (* Not [Fun.protect], whose frames would end the backtrace of every law
     that raises, after the law's own. *)
  let run_law context value =
    frame.law_mark <- Capture.mark capture;
    let enclosing = frame.prop in
    frame.prop <- Some context;
    match law value with
    | () -> frame.prop <- enclosing
    | exception exn ->
        let backtrace = Printexc.get_raw_backtrace () in
        frame.prop <- enclosing;
        Printexc.raise_with_backtrace exn backtrace
  in
  let output () =
    match frame.law_mark with
    | None -> None
    | Some start ->
        Option.bind (Capture.mark capture) (fun stop ->
            Capture.output_tail ~within:(start, stop) capture)
  in
  (* The steps last given and when, while a search runs. *)
  let given = ref None in
  let shrinking steps =
    match !given with
    | Some (last, at) when steps = last || Os.count_s at < shrink_interval -> ()
    | Some _ | None ->
        given := Some (steps, Os.counter ());
        give_in_attempt frame (Shrinking { path = frame.path; steps })
  in
  let shrunk () =
    if Option.is_some !given then begin
      given := None;
      give_in_attempt frame (Shrunk { path = frame.path })
    end
  in
  let fail stats failure =
    frame.prop_stats <- Some stats;
    raise (Failure.Check_failure failure)
  in
  (* [--case] checks its file's case alone: [Property.run] puts it before
     any token, and nothing is recorded for it. *)
  let case =
    Option.map
      (fun { file; choices } -> ("--case " ^ file, choices))
      config.case_file
  in
  let engine ?replay () =
    let replayed_by =
      if Option.is_some config.replay then None
      else Some "--failed: the recorded counterexample"
    in
    match
      Property.run ?loc ?count ?max_discard ?examples ?summary ?runs ?replay
        ?replayed_by ?case ?only ~output ~shrinking ~root:config.seed
        ~path:(Test_tree.path_to_string frame.path)
        gen run_law
    with
    | outcome ->
        shrunk ();
        outcome
    | exception exn ->
        let backtrace = Printexc.get_raw_backtrace () in
        shrunk ();
        Printexc.raise_with_backtrace exn backtrace
  in
  let judged : Property.outcome -> unit = function
    | Pass stats -> frame.prop_stats <- Some stats
    (* Coverage is judged once every case has run, so a property that
       stopped early reports no coverage it did not judge. *)
    | Fail { failure; stats } -> fail { stats with coverage = [] } failure
    | Coverage_failed stats ->
        fail stats (coverage_failure ?loc:frame.loc stats)
    | Gave_up stats ->
        fail { stats with coverage = [] } (gave_up_failure ?loc:frame.loc stats)
  in
  (* [--replay] checks its token. Under [--failed] a property checks again
     each counterexample it recorded, by its token, and a table its rows,
     which it selects itself through [only]. *)
  let recorded =
    if Option.is_some only || Option.is_some case then [] else recorded ()
  in
  let replays =
    match config.replay with
    | Some token -> [ token ]
    | None ->
        List.filter_map
          (function Replay token -> Some token | Row _ -> None)
          recorded
  in
  if List.exists (function Row _ -> true | Replay _ -> false) recorded then
    raise
      (Failure.Check_failure
         (Failure.message ?loc:frame.loc
            "--failed: the last-failed record names rows of a table, and this \
             test is no table; run it without --failed"));
  (* Each replay that fails but the last is recorded, as the failures of a
     test that fails more than once. *)
  let rec replay = function
    | [] -> judged (engine ())
    | [ token ] -> judged (engine ~replay:token ())
    | token :: tokens ->
        (match engine ~replay:token () with
        | Fail { failure; stats } ->
            frame.prop_stats <- Some stats;
            add_failure frame failure
        | outcome -> judged outcome);
        replay tokens
  in
  replay replays

(* The enclosing law's context comes back however [fn] ends. *)
let without_labels fn =
  match Atomic.get slot with
  | Some (In_test ({ prop = Some _ as enclosing; _ } as frame))
    when on_run_domain frame.run ->
      frame.prop <- Some (Property.scratch ());
      Fun.protect ~finally:(fun () -> frame.prop <- enclosing) fn
  | Some (In_test _ | In_run _) | None -> fn ()

let prop ?__POS__ ?tags ?timeout ?count ?max_discard ?examples ?summary name gen
    law =
  let loc = Loc.resolve ?__POS__ () in
  Test_tree.test ?__POS__ ?tags ?timeout name (fun () ->
      property ?loc ?count ?max_discard ?examples ?summary gen law)

(* Startup errors *)

type startup_error =
  | Duplicate_paths of string list
  | Focused_in_ci of Loc.t option list
  | Update_refused_in_ci
  | No_recorded_failures
  | In_process_selects of int
  | Case_selects of int
  | Mutation_refused of string

let startup_message = function
  | Duplicate_paths paths ->
      Pp.str "duplicate test paths:\n  %s\nEvery full test path must be unique."
        (String.concat "\n  " paths)
  | Focused_in_ci sites ->
      let site = function
        | Some loc -> Pp.str "focus at %s" (Loc.to_string loc)
        | None -> "focus"
      in
      Pp.str "focused tests committed (%s); remove focus to run under CI"
        (String.concat ", " (List.map site sites))
  | Update_refused_in_ci ->
      "baseline update refused: CI is set. -u rewrites baselines in place, \
       which is a developer's edit; under CI run with --corrected and accept \
       with dune promote."
  | No_recorded_failures -> "no recorded failures match the current suite"
  | In_process_selects n ->
      Pp.str
        "--in-process runs one test, and the selection holds %d; narrow it \
         with -f to the test to debug"
        n
  | Case_selects n ->
      Pp.str
        "--case checks one case of one property, and the selection holds %d \
         tests; narrow it with -f to the property to check"
        n
  | Mutation_refused reason -> reason

(* The last-failed store *)

(* A magic line, then one line per entry: a [String.escaped] path, alone
   for the whole test, or followed by a tab, a kind and a tab, then the
   escaped case: a table's row by its printed value, a property's
   counterexample by its token. An escaped string holds no tab, and a case
   of a kind this version does not know names the whole test. The first
   version's lines are paths alone, so its file reads as whole tests. *)
let store_magic = "windtrap-last-failed 2"
let store_magic_v1 = "windtrap-last-failed 1"

let store_path (config : config) ~suite =
  Filename.concat
    (Filename.concat config.log_dir (Os.sanitize_component suite))
    ".last-failed"

let case_kind = function Row _ -> "row" | Replay _ -> "replay"
let case_text = function Row s | Replay s -> s

let entry_of_line line =
  match String.split_on_char '\t' line with
  | [ path ] -> Some (Scanf.unescaped path, None)
  | [ path; "row"; row ] ->
      Some (Scanf.unescaped path, Some (Row (Scanf.unescaped row)))
  | [ path; "replay"; token ] ->
      Some (Scanf.unescaped path, Some (Replay (Scanf.unescaped token)))
  | path :: _ -> Some (Scanf.unescaped path, None)
  | [] -> None

(* Entries in the order of their first line, each test once, with the
   cases named: [[]] for the whole test, which a line naming it alone
   makes it. A directory opens as a file on POSIX systems, and the first
   read raises [Sys_error], so a read is guarded as the opening is. *)
let read_store path =
  let entries ic =
    let parse =
      match input_line ic with
      | exception End_of_file -> None
      | magic when String.equal magic store_magic -> Some entry_of_line
      | magic when String.equal magic store_magic_v1 ->
          Some (fun line -> Some (Scanf.unescaped line, None))
      | _ -> None
    in
    match parse with
    | None -> []
    | Some parse ->
        let cases = Hashtbl.create 16 in
        let add path case =
          let merged =
            match (Hashtbl.find_opt cases path, case) with
            | None, None | Some [], _ | Some _, None -> []
            | None, Some case -> [ case ]
            | Some known, Some case ->
                if List.mem case known then known else known @ [ case ]
          in
          Hashtbl.replace cases path merged
        in
        let rec lines order =
          match input_line ic with
          | exception End_of_file -> List.rev order
          | line -> (
              match parse line with
              | exception _ -> lines order
              | None | Some ("", _) -> lines order
              | Some (path, case) ->
                  let order =
                    if Hashtbl.mem cases path then order else path :: order
                  in
                  add path case;
                  lines order)
        in
        List.map (fun path -> (path, Hashtbl.find cases path)) (lines [])
  in
  match In_channel.with_open_bin path entries with
  | entries -> entries
  | exception Sys_error _ -> []

let write_store path entries =
  let buffer = Buffer.create 256 in
  let line s =
    Buffer.add_string buffer s;
    Buffer.add_char buffer '\n'
  in
  line store_magic;
  List.iter
    (fun (path, cases) ->
      match cases with
      | [] -> line (String.escaped path)
      | cases ->
          List.iter
            (fun case ->
              line
                (String.concat "\t"
                   [
                     String.escaped path;
                     case_kind case;
                     String.escaped (case_text case);
                   ]))
            cases)
    entries;
  match
    Os.mkdir_p (Filename.dirname path);
    Os.atomic_write ~path (Buffer.contents buffer)
  with
  | () -> ()
  | exception (Sys_error _ | Unix.Unix_error _) -> ()

(* Only a run that executed the whole suite knows which recorded paths are
   gone. *)
let update_store path ~full results =
  let path_string (r : result) = Test_tree.path_to_string r.path in
  let failed =
    List.filter_map
      (fun (r : result) ->
        if r.counted then Some (path_string r, r.refuted) else None)
      results
  in
  let survivors =
    if full then []
    else
      let executed = List.map path_string results in
      List.filter
        (fun (entry, _) -> not (List.mem entry executed))
        (read_store path)
  in
  write_store path (failed @ survivors)

(* Exits *)

(* The guard belongs to the process that started the run: a forked child
   inherits the registration and [active], and its [exit] must end it.
   [at_exit] runs a function once, so the guard registers itself again before
   it raises; the exception propagates out of [exit], and the process goes
   on. *)
let exit_guard_owner = ref None

let rec exit_guard () =
  let owner =
    match !exit_guard_owner with
    | Some pid -> pid = Unix.getpid ()
    | None -> false
  in
  if owner && active () then begin
    at_exit exit_guard;
    raise (Failure.Control `Exit)
  end

let install_exit_guard () =
  if Option.is_none !exit_guard_owner then at_exit exit_guard;
  exit_guard_owner := Some (Unix.getpid ())

(* Selection *)

let case_path (case : Test_tree.case) = Test_tree.path_to_string case.path

(* The buckets are a function of the path alone, stable across runs and
   machines; changing this root moves the tests of every sharded suite. *)
let shard_root = 0x77696e6473687264L (* "windshrd" *)

let shard_bucket ~shards path =
  Int64.to_int
    (Int64.unsigned_rem
       (Seed.derive ~root:shard_root ~path ~index:0)
       (Int64.of_int shards))

let tag_predicate (config : config) =
  let require p tag = Test_tree.Tag.require tag p in
  let drop p tag = Test_tree.Tag.drop tag p in
  List.fold_left drop
    (List.fold_left require Test_tree.Tag.any config.tags)
    config.exclude_tags

let is_selected (config : config) ~tags ~allowed ~focus_active
    (case : Test_tree.case) =
  let path = case_path case in
  let contains pattern = Text.contains_substring ~pattern path in
  (config.filter = [] || List.exists contains config.filter)
  && (not (List.exists contains config.exclude))
  && Test_tree.Tag.accepts tags case.tags
  && allowed path
  && (match config.shard with
    | None -> true
    | Some (k, shards) -> shard_bucket ~shards path = k - 1)
  && ((not focus_active) || case.focused)

let duplicate_paths paths =
  let seen = Hashtbl.create 64 in
  let is_duplicate path =
    let duplicate = Hashtbl.mem seen path in
    Hashtbl.replace seen path ();
    duplicate
  in
  List.sort_uniq String.compare (List.filter is_duplicate paths)

(* The checks run in the order of [startup_error], so a suite that fails two
   of them is always told of the same one. The result is the paths that
   [--failed] keeps. *)
let startup (config : config) ~suite ~focus_sites paths =
  let in_ci = Os.in_ci () in
  let* () =
    match duplicate_paths paths with
    | [] -> Ok ()
    | duplicates -> Error (Duplicate_paths duplicates)
  in
  let* () =
    if focus_sites <> [] && in_ci then Error (Focused_in_ci focus_sites)
    else Ok ()
  in
  let* () =
    match config.baseline with
    | Baseline.Update when in_ci -> Error Update_refused_in_ci
    | Baseline.Update | Baseline.Corrected | Baseline.Check -> Ok ()
  in
  if not config.failed_only then Ok None
  else
    match
      List.filter
        (fun (path, _) -> List.mem path paths)
        (read_store (store_path config ~suite))
    with
    | [] -> Error No_recorded_failures
    | entries -> Ok (Some entries)

type selection = {
  cases : Test_tree.case list;
  total : int;
  focus_active : bool;
  recorded : (string * case_id list) list;
}

let select (config : config) ~suite tests =
  (* Left on: without it a report names no raise site unless the user set
     [OCAMLRUNPARAM=b]. *)
  Printexc.record_backtrace true;
  if active () then invalid_arg active_run_error;
  (match config.shard with
  | Some (k, n) when k < 1 || n < k ->
      invalid_arg "windtrap: shard must be K/N with 1 <= K <= N"
  | Some _ | None -> ());
  let cases = Test_tree.flatten tests in
  let paths = List.map case_path cases in
  let focus_sites = Test_tree.focus_sites tests in
  let focus_active = focus_sites <> [] in
  let* recorded = startup config ~suite ~focus_sites paths in
  (* Hashed: the store can name every path of the suite. *)
  let allowed =
    match recorded with
    | None -> fun _ -> true
    | Some entries ->
        let table = Hashtbl.create (List.length entries * 2) in
        List.iter (fun (path, _) -> Hashtbl.replace table path ()) entries;
        Hashtbl.mem table
  in
  let tags = tag_predicate config in
  let selected =
    List.filter (is_selected config ~tags ~allowed ~focus_active) cases
  in
  match List.length selected with
  | n when config.in_process && n > 1 -> Error (In_process_selects n)
  | n when Option.is_some config.case_file && n <> 1 -> Error (Case_selects n)
  | _ ->
      let recorded =
        List.filter
          (fun (_, cases) -> cases <> [])
          (Option.value recorded ~default:[])
      in
      Ok { cases = selected; total = List.length cases; focus_active; recorded }

let record_cases (run : t) entries =
  List.iter
    (fun (path, cases) -> Hashtbl.replace run.recorded path cases)
    entries

(* Attempts *)

(* A frozen root, not the run's seed: [Random] gives a test a stream that
   depends on its path alone. *)
let random_root = 0x57696e6474726170L

let with_isolated_random ~path fn =
  let saved = Random.get_state () in
  Random.init (Int64.to_int (Seed.derive ~root:random_root ~path ~index:0));
  Fun.protect ~finally:(fun () -> Random.set_state saved) fn

let set_alarm seconds =
  ignore
    (Unix.setitimer Unix.ITIMER_REAL
       { Unix.it_value = seconds; it_interval = 0. })

(* A hang reported by name beats a build killed with none, and a limit too
   low fails honest tests where one too high only reports a hang later, so
   the default is ten minutes. A debugger stops a test for as long as it
   likes, so [--in-process] has none. *)
let limit config (case : Test_tree.case) =
  match (case.timeout, config.timeout) with
  | (Some _ as declared), _ -> declared
  | None, (Some _ as given) -> given
  | None, None ->
      if config.in_process then None else Some config.default_timeout

(* The alarm fires once and a timeout is not fatal, so after a body that
   timed out [renew] bounds the teardown with what remains of the limit, or
   with a whole limit. The alarm can fire during the cleanup: [armed] makes
   the handler inert, and [disarm] absorbs a timeout that interrupts it. *)
let with_timeout limit fn =
  match limit with
  | Some limit when not Sys.win32 -> (
      let armed = ref true in
      let started = Os.counter () in
      let previous =
        Sys.signal Sys.sigalrm
          (Sys.Signal_handle
             (fun _ -> if !armed then raise (Failure.Control (`Timeout limit))))
      in
      let rec disarm () =
        match
          armed := false;
          set_alarm 0.;
          Sys.set_signal Sys.sigalrm previous
        with
        | () -> ()
        | exception Failure.Control (`Timeout _) -> disarm ()
      in
      let renew () =
        if !armed then
          let remaining = limit -. Os.count_s started in
          set_alarm (if remaining > 0. then remaining else limit)
      in
      set_alarm limit;
      match fn renew with
      | value ->
          disarm ();
          value
      | exception exn ->
          let backtrace = Printexc.get_raw_backtrace () in
          disarm ();
          Printexc.raise_with_backtrace exn backtrace)
  | Some _ | None -> fn ignore

let add_caught frame phase : Failure.caught -> unit =
  let add = add_phase_failure frame phase in
  function
  | `Assertion failure -> add failure
  | `Exception (exn, backtrace) -> add (uncaught frame exn backtrace)
  | `Skip reason -> if Option.is_none frame.skip then frame.skip <- Some reason
  | `Timeout limit -> add (Failure.timeout ?loc:frame.loc limit)
  | `Exit ->
      add
        (Failure.message ?loc:frame.loc
           "the test called exit and was intercepted; a test must return or \
            raise, never exit the process")
  | `Discard ->
      add
        (Failure.message ?loc:frame.loc
           "assume or reject was called outside a property")

(* A failure that the phase caught was recorded before what it raised. *)
let add_recorded frame phase (ended, caught) =
  List.iter (add_phase_failure frame phase) caught;
  match ended with Ok () -> () | Error c -> add_caught frame phase c

(* [Loc.delimit] keeps a location capture inside user code: a tail-called
   assertion gets none, not the location of the runner's caller. *)
let run_phase frame phase fn =
  frame.phase <- phase;
  add_recorded frame phase (Failure.recording (fun () -> Loc.delimit fn))

(* Whether two raises are one exception: the body's, raised again through a
   scope that let it pass. *)
let same_raise (a : Failure.caught) (b : Failure.caught) =
  match (a, b) with
  | `Assertion a, `Assertion b -> a == b
  | `Exception (a, _), `Exception (b, _) -> a == b
  | (#Failure.control as a), (#Failure.control as b) -> a == b
  | (#Failure.fault | #Failure.control), _ -> false

(* The body's failure is added, then raised again through [scope], so a
   scope that swallows it cannot pass the test. What else leaves [scope] is
   a failure of the phase that ran, and what [scope] caught is one of the
   phase that ran when it ended. The callback runs the body once: one
   execution is what corrections, labels and temporary paths are keyed by. *)
let scoped frame ~renew scope body =
  let calls = ref 0 in
  let body_raised = ref None in
  let callback resource =
    incr calls;
    if !calls = 1 then begin
      frame.phase <- Failure.Body;
      let ended, caught =
        Failure.recording (fun () -> Loc.delimit (fun () -> body resource))
      in
      add_recorded frame Failure.Body (ended, caught);
      frame.phase <- Failure.Teardown;
      renew ();
      match ended with
      | Ok () -> ()
      | Error c -> (
          body_raised := Some c;
          match c with
          (* Added already, so a scope that catches it adds nothing. *)
          | `Assertion t -> raise (Failure.Check_failure t)
          | c -> Failure.reraise c)
    end
  in
  frame.phase <- Failure.Setup;
  let ended, caught =
    Failure.recording (fun () -> Loc.delimit (fun () -> scope callback))
  in
  List.iter (add_phase_failure frame frame.phase) caught;
  (match ended with
  | Ok () ->
      if !calls = 0 then
        add_phase_failure frame Failure.Setup
          (Failure.message ?loc:frame.loc
             "the scope returned without running the test body; a scope must \
              call its callback exactly once")
  | Error c ->
      if not (Option.equal same_raise !body_raised (Some c)) then
        add_caught frame frame.phase c);
  if !calls > 1 then
    add_phase_failure frame Failure.Body
      (Failure.message ?loc:frame.loc
         (Pp.str
            "the scope called its callback %d times and the test body ran on \
             the first call only; a scope must call it exactly once"
            !calls))

(* The attempt is undone outside the limit and the capture, on every path
   that returns here. *)
let run_attempt frame (case : Test_tree.case) ~limit ~groups ~test_name =
  let phases renew =
    match case.body with
    | Test_tree.Body fn -> run_phase frame Failure.Body fn
    | Test_tree.Scoped { scope; body } -> scoped frame ~renew scope body
  in
  let bounded () =
    with_isolated_random ~path:(case_path case) @@ fun () ->
    with_timeout limit @@ fun renew ->
    match Failure.catch (fun () -> phases renew) with
    | Ok () -> ()
    | Error (`Timeout _ as c) -> add_caught frame frame.phase c
    | Error c -> Failure.reraise c
  in
  let captured () =
    match
      Failure.catch (fun () ->
          Capture.with_capture frame.run.capture ~groups ~test_name bounded)
    with
    | Ok () -> ()
    | Error c -> add_caught frame Failure.Body c
  in
  Fun.protect
    ~finally:(fun () -> reclaim frame)
    (fun () -> with_context (In_test frame) captured)

let is_baseline (failure : Failure.t) =
  match failure.kind with
  | Baseline _ -> true
  | Equality _ | Containment _ | Raise _ | Property _ | Law _ | Timeout _
  | Message _ ->
      false

let carries_correction (failure : Failure.t) =
  match failure.kind with
  | Baseline { baseline = File _; state = Missing _; _ } -> false
  | Baseline { state = Missing _ | Mismatch _; withheld = None; _ } -> true
  | Baseline _ | Equality _ | Containment _ | Raise _ | Property _ | Law _
  | Timeout _ | Message _ ->
      false

(* A part of an attempt settles as a whole: the attempt, or a row of a
   table. It keeps its corrections when its failures are all baseline
   failures and nothing excuses it, an excuse being why it keeps none. *)
let settle_part baselines ~excuse failures =
  let baseline_only = List.for_all is_baseline failures in
  let keep = baseline_only && Option.is_none excuse in
  let kept = Baseline.settle baselines ~keep in
  let failures =
    if keep then failures
    else
      let withheld =
        match excuse with
        | Some withheld when baseline_only -> withheld
        | Some _ | None -> Failure.Failed_outside
      in
      List.map (Failure.with_withheld withheld) failures
  in
  (failures, keep, kept)

(* The cases an attempt refuted, when each of its failures belongs to one:
   the rows of a table that failed, with the table's property failure, or
   the counterexamples of a property by their tokens. Otherwise none, and
   [--failed] reruns the whole test. *)
let refuted_cases rows rest =
  let failed (row, (failures, _, _)) =
    if failures <> [] || row.excuse = Some Failure.Failed_outside then
      Some (Row row.id)
    else None
  in
  let tokens (f : Failure.t) =
    let token (f : Failure.t) =
      match f.kind with Property { token = Some t; _ } -> Some t | _ -> None
    in
    match f.kind with
    | Property { also; _ } -> List.map token (f :: also)
    | _ -> [ None ]
  in
  match (rows, rest) with
  | _ :: _, ([] | [ { Failure.kind = Property { table = true; _ }; _ } ]) ->
      List.filter_map failed rows
  | [], _ :: _ -> (
      match List.concat_map tokens rest with
      | tokens when List.for_all Option.is_some tokens ->
          List.map (fun t -> Replay (Option.get t)) tokens
      | _ -> [])
  | _ -> []

(* The outcome of an attempt, whether each of its failures carries a kept
   correction, whether it must be the last, and the cases it refuted. A kept
   correction is permanent, and a retry would agree with it. Under
   [Corrected] only a correcting check raises a failure that carries one,
   and a missing file's check never corrects ([Baseline.check]). The rows
   are settled already, in order, and the rest of the attempt settles
   last. *)
let settle baselines ~rows ~failures ~skip : verdict =
  let excuse = Option.map (fun _ -> Failure.Skipped) skip in
  let rest, rest_keep, rest_kept = settle_part baselines ~excuse failures in
  let failures = List.concat_map (fun (_, (f, _, _)) -> f) rows @ rest in
  let keep = rest_keep && List.for_all (fun (_, (_, keep, _)) -> keep) rows in
  let kept = List.fold_left (fun n (_, (_, _, k)) -> n + k) rest_kept rows in
  let corrected =
    keep
    && Baseline.mode baselines = Baseline.Corrected
    && failures <> []
    && List.for_all carries_correction failures
  in
  let outcome =
    match (failures, skip) with
    | [], Some reason -> Failure.Skip reason
    | [], None -> Failure.Pass
    | failures, _ -> Failure.Fail failures
  in
  (outcome, corrected, kept > 0, refuted_cases rows rest)

let rec take n = function
  | x :: rest when n > 0 ->
      let taken, rest = take (n - 1) rest in
      (x :: taken, rest)
  | rest -> ([], rest)

(* The checks of [attempt] are made in [baselines], each where it fell
   among the other failures, then the attempt is settled: each row of a
   table alone and in order, as the attempt of a test of its own would be,
   then the rest. A registry that sees the attempts in the order of the
   selection holds what an in-process run's would: the accepted contents,
   and the baselines and sources as first read. *)
let judge baselines (case : Test_tree.case) attempt =
  let made (c : check) =
    match
      Baseline.check baselines ?loc:c.site ~correct:c.correct c.subject c.actual
    with
    | () -> []
    | exception Failure.Check_failure failure ->
        [
          label ~path:case.path ~subtests:c.within
            (at_declaration case.loc failure);
        ]
    | exception (Sys_error _ as exn) ->
        (* The worker read the file, and it became unreadable since. *)
        [ Failure.raised ?loc:case.loc ~actual:(Failure.exn_to_string exn) () ]
  in
  (* [i] numbers the failures of the whole attempt. *)
  let rec merge i rev checks failures =
    match (checks, failures) with
    | (c : check) :: checks, _ when c.before <= i ->
        merge i (List.rev_append (made c) rev) checks failures
    | _, failure :: failures -> merge (i + 1) (failure :: rev) checks failures
    | [], [] -> List.rev rev
    | _ :: _, [] -> merge max_int rev checks []
  in
  let rec settle_rows ~i ~j rev failures checks = function
    | [] -> (List.rev rev, merge i [] checks failures)
    | row :: rows ->
        let own, failures = take (row.failures_to - i) failures in
        let own_checks, checks = take (row.checks_to - j) checks in
        let part =
          match row.settled with
          | Some (keep, kept) -> (own, keep, kept)
          | None ->
              settle_part baselines ~excuse:row.excuse
                (merge i [] own_checks own)
        in
        settle_rows ~i:row.failures_to ~j:row.checks_to ((row, part) :: rev)
          failures checks rows
  in
  let rows, failures =
    settle_rows ~i:0 ~j:0 [] attempt.failures attempt.checks attempt.rows
  in
  settle baselines ~rows ~failures ~skip:attempt.skip

(* A row of a table runs as a part of its test's attempt that settles
   alone (see [judge]): its failures are labelled with [tag], as a
   subtest's are, and a run that makes its checks as they come settles the
   row's corrections as it ends, so the next row is compared with what this
   one kept. A row is chosen in view, not drawn, so it runs outside the
   law's labels, as a test's body does. *)
let row ~tag ~id fn =
  let frame = current_frame () in
  let failures_from = List.length frame.rev_failures in
  let enclosing = frame.subtests and law = frame.prop in
  frame.subtests <- tag :: enclosing;
  frame.prop <- None;
  let ended excuse =
    frame.subtests <- enclosing;
    frame.prop <- law;
    let settled =
      match frame.run.judge with
      | Some _ -> None
      | None ->
          let own, before =
            take
              (List.length frame.rev_failures - failures_from)
              frame.rev_failures
          in
          let own, keep, kept =
            settle_part frame.run.baselines ~excuse (List.rev own)
          in
          frame.rev_failures <- List.rev_append own before;
          Some (keep, kept)
    in
    let row =
      {
        id;
        failures_to = List.length frame.rev_failures;
        checks_to = List.length frame.rev_checks;
        excuse;
        settled;
      }
    in
    frame.rev_rows <- row :: frame.rev_rows
  in
  (* A failure that the row's code caught fails the row as a raised one
     does, and reaches the engine raised. *)
  match Failure.recording fn with
  | exception fatal ->
      let backtrace = Printexc.get_raw_backtrace () in
      ended (Some Failure.Failed_outside);
      Printexc.raise_with_backtrace fatal backtrace
  | ending, caught -> (
      ended
        (match (ending, caught) with
        | Ok (), [] -> None
        | Error (`Skip _ | `Discard), [] -> Some Failure.Skipped
        | (Ok () | Error _), _ -> Some Failure.Failed_outside);
      match (caught, ending) with
      | first :: _, _ -> Failure.refuse first
      | [], Ok () -> ()
      | [], Error c -> Failure.reraise c)

(* Retries *)

(* An expected failure does not count, an unexpected pass does, and a skip
   never does. *)
let counts_failed ~(xfail : Test_tree.xfail option) (outcome : Failure.outcome)
    =
  match (outcome, xfail) with
  | Fail _, None | Pass, Some _ -> true
  | Fail _, Some _ | Pass, None | Skip _, _ -> false

let xpass_failure (case : Test_tree.case) =
  let reason =
    match case.xfail with
    | Some { reason = Some reason } -> " (" ^ reason ^ ")"
    | Some { reason = None } | None -> ""
  in
  Failure.message ?loc:case.loc
    (Pp.str "expected to fail%s, but the test passed" reason)

(* A property's failure carries the output of its counterexample's run. The
   tail of the attempt would end on the law's last run, which after a search
   is a candidate that passed. The tail is read only for a failure. *)
let attach_tail tail (outcome : Failure.outcome) =
  match outcome with
  | Fail ({ kind = Property _; _ } :: _) -> outcome
  | Fail (first :: rest) -> (
      match tail () with
      | Some tail -> Failure.Fail (Failure.with_output_tail tail first :: rest)
      | None -> outcome)
  | outcome -> outcome

(* The row of [case] from the outcome of its last attempt. *)
let result_of (case : Test_tree.case) ~tail ~duration ~attempts ~prop_stats
    ?(refuted = []) outcome =
  let counted = counts_failed ~xfail:case.xfail outcome in
  let outcome =
    match (outcome, case.xfail) with
    | Pass, Some _ -> Failure.Fail [ xpass_failure case ]
    | outcome, _ -> outcome
  in
  {
    path = case.path;
    outcome = attach_tail tail outcome;
    counted;
    xfail = case.xfail;
    slow_tagged = Test_tree.Tag.mem Test_tree.Tag.slow case.tags;
    duration;
    attempts;
    prop_stats;
    refuted = (if counted then refuted else []);
  }

(* What a case left once its attempts ended: its row, whether every failure
   of its last attempt carries a kept correction, and, from a worker, that
   attempt with its output tail when the parent must still judge its
   checks. *)
type served = {
  result : result;
  corrected : bool;
  deferred : (attempt * Failure.tail option) option;
}

(* A worker's checks are judged by its parent: at once when a retry hangs on
   the verdict, and otherwise when the parent reaches the case. *)
let run_case ~on_event run (case : Test_tree.case) =
  on_event (Test_started { path = case.path });
  let limit = limit run.config case in
  let groups, test_name = split_last case.path in
  let rec attempt number spent =
    let frame = frame ~on_event run case in
    let start = Os.counter () in
    run_attempt frame case ~limit ~groups ~test_name;
    (* A test that checked no property under [--case] would pass on a case
       it never read. *)
    if
      Option.is_some run.config.case_file
      && Option.is_none frame.prop_stats
      && Option.is_none frame.skip && frame.rev_failures = []
    then
      add_failure frame
        (Failure.message ?loc:frame.loc
           "--case: this test checks no property, so it checked no case");
    let made =
      {
        failures = List.rev frame.rev_failures;
        checks = List.rev frame.rev_checks;
        rows = List.rev frame.rev_rows;
        skip = frame.skip;
      }
    in
    let left = made.checks <> [] && number > case.retries in
    let outcome, corrected, last, refuted =
      match run.judge with
      | Some ask when made.checks <> [] && not left -> ask made
      | Some _ | None -> judge run.baselines case { made with checks = [] }
    in
    let duration = spent +. Os.count_s start in
    if
      counts_failed ~xfail:case.xfail outcome
      && (not last) && number <= case.retries
    then attempt (number + 1) duration
    else
      let tail () = Capture.output_tail run.capture in
      let result =
        result_of case ~tail ~duration ~attempts:number
          ~prop_stats:frame.prop_stats ~refuted outcome
      in
      run.rev_results <- result :: run.rev_results;
      on_event (Test_finished result);
      {
        result;
        corrected;
        deferred = (if left then Some (made, tail ()) else None);
      }
  in
  attempt 1 0.

(* Signals *)

(* No [exit]: the guard must not run, and the parent must see the signal. *)
let interrupt ~on_event run ~started signal =
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle ignore);
  set_alarm 0.;
  let frame =
    match Atomic.get slot with
    | Some (In_test frame) -> Some frame
    | Some (In_run _) | None -> None
  in
  (try Capture.abandon run.capture with _ -> ());
  (try
     on_event
       (Interrupted
          {
            running = Option.map (fun frame -> frame.path) frame;
            releasing =
              Option.map (fun (release : release) -> release.name) run.releasing;
            results = results run;
            duration = Os.count_s started;
          })
   with _ -> ());
  Option.iter remove_temp frame;
  (try ignore (release_fixtures run ~announce:ignore) with _ -> ());
  Os.die_by signal

(* A handler runs at a safepoint and may allocate, but it must not re-enter
   the report, whose formatter may be mid-line. A signal therefore acts at
   once only in user code (an attempt, a release), and elsewhere at the next
   boundary of [drive]. *)
let with_interrupts ~interrupt run fn =
  let handle signal =
    let in_user_code =
      match Atomic.get slot with
      | Some (In_test _) -> true
      | Some (In_run _) | None -> Option.is_some run.releasing
    in
    if in_user_code then interrupt signal else run.interrupted <- Some signal
  in
  Os.with_signals interrupting handle fn

(* Executing *)

let run_started ~suite (selection : selection) =
  let properties =
    List.exists
      (fun (case : Test_tree.case) ->
        Test_tree.Tag.mem Test_tree.Tag.prop case.tags)
      selection.cases
  in
  Run_started
    {
      suite;
      total = selection.total;
      selected = List.length selection.cases;
      properties;
    }

(* [drive] runs [selected] in order, up to the first counted failure under
   [config.bail], and is [true] iff a counted failure carried no kept
   correction. *)
let drive ~on_event ~interrupt run selected =
  let rec loop uncorrected = function
    | [] -> uncorrected
    | case :: cases ->
        Option.iter interrupt run.interrupted;
        let { result; corrected; _ } = run_case ~on_event run case in
        let uncorrected = uncorrected || (result.counted && not corrected) in
        if (run.config.bail && result.counted) || Option.is_some run.stopped
        then uncorrected
        else loop uncorrected cases
  in
  try loop false selected
  with exn ->
    (* Only a fatal exception or a raising observer gets here. The fixtures
       are released first, whatever the observer does with the events. *)
    let backtrace = Printexc.get_raw_backtrace () in
    let announce name =
      try on_event (Fixture_release { name }) with _ -> ()
    in
    (try ignore (release_fixtures run ~announce) with _ -> ());
    Printexc.raise_with_backtrace exn backtrace

type outcome = {
  run : t;
  selected : Test_tree.case list;
  total : int;
  focus_active : bool;
  release_failures : Failure.t list;
  duration : float;
  uncorrected : bool;
}

(* The store, then the corrections: what a run that reached its end leaves
   behind. *)
let conclude run (selection : selection) ~suite ~started ~uncorrected
    ~release_failures =
  let config = run.config in
  let results = results run in
  let bailed = config.bail && List.exists (fun r -> r.counted) results in
  let full = (not bailed) && List.length results = selection.total in
  update_store (store_path config ~suite) ~full results;
  Baseline.write run.baselines;
  {
    run;
    selected = selection.cases;
    total = selection.total;
    focus_active = selection.focus_active;
    release_failures;
    duration = Os.count_s started;
    uncorrected;
  }

let execute ?(on_event = ignore) config ~suite tests =
  let started = Os.counter () in
  install_exit_guard ();
  let* selection = select config ~suite tests in
  let baselines = Baseline.create ~mode:config.baseline () in
  let capture =
    if config.stream then Capture.disabled
    else Capture.create ~log_dir:config.log_dir ~suite ()
  in
  let run = create config ~capture ~baselines in
  record_cases run selection.recorded;
  with_context (In_run run) @@ fun () ->
  let interrupt = interrupt ~on_event run ~started in
  with_interrupts ~interrupt run @@ fun () ->
  on_event (run_started ~suite selection);
  let uncorrected = drive ~on_event ~interrupt run selection.cases in
  Option.iter interrupt run.interrupted;
  let release_failures =
    release_fixtures run ~announce:(fun name ->
        on_event (Fixture_release { name }))
  in
  let outcome =
    conclude run selection ~suite ~started ~uncorrected ~release_failures
  in
  Option.iter interrupt run.interrupted;
  Ok outcome

let list_selection config ~suite tests =
  install_exit_guard ();
  let* selection = select config ~suite tests in
  Ok (List.map case_path selection.cases)

(* Exit codes *)

let exit_code = function
  | Error (No_recorded_failures | In_process_selects _ | Case_selects _) -> 2
  | Error
      ( Duplicate_paths _ | Focused_in_ci _ | Update_refused_in_ci
      | Mutation_refused _ ) ->
      1
  | Ok outcome ->
      let refused =
        List.exists
          (function Baseline.Refused _ -> true | Baseline.Written _ -> false)
          (Baseline.writes outcome.run.baselines)
      in
      if outcome.uncorrected || outcome.release_failures <> [] || refused then 1
      else if outcome.run.rev_results <> [] then 0
        (* A mirror reaches every stanza of a project, and a stanza whose
           tests its selection leaves out holds no mistyped filter. *)
      else if outcome.total > 0 && outcome.run.config.broadcast.selection then 0
      else 2

(* Serving a supervisor *)

type request = Case of string | Verdict of verdict | Ack | Finish

type report =
  | Event of event
  | Judge of attempt
  | Done of { served : served; stopped : bool; retire : bool }
  | Released of Failure.t list
  | Exited
  | Outside of Failure.t

(* After a stop, a worker that waits for a verdict is told to finish. *)
exception Finished

let exit_text = "a test must return or raise, never exit the process"

(* What a worker reports when code under test calls exit: the running test
   ends with its worker, and a release or a domain the test left running
   fails as no test does. *)
let exited run ~last =
  match (Atomic.get slot, run.releasing, last) with
  | Some (In_test _), _, _ -> Exited
  | _, Some (release : release), _ ->
      Outside
        (Failure.with_phase Failure.Release
           (Failure.message ?loc:release.loc
              (Pp.str "%s: release called exit, which ended its worker; %s"
                 release.name exit_text)))
  | _, None, last ->
      let loc, after =
        match last with
        | Some (case : Test_tree.case) ->
            (case.loc, Pp.str " after the test %s ended" (case_path case))
        | None -> (None, "")
      in
      Outside
        (Failure.with_phase Failure.Release
           (Failure.message ?loc
              (Pp.str
                 "code under test called exit%s, on a domain the test left \
                  running, which ended its worker; %s"
                 after exit_text)))

let handler_failure (case : Test_tree.case option) exn backtrace =
  let loc, whose =
    match case with
    | Some case -> (case.loc, "the test " ^ case_path case)
    | None -> (None, "a test")
  in
  Failure.with_phase Failure.Release
    (Failure.raised ?loc
       ~msg:(Pp.str "an at_exit function that %s registered raised" whose)
       ~actual:(Failure.exn_to_string exn)
       ~backtrace:(Failure.backtrace_to_string backtrace)
       ())

(* A worker's case raised what [Failure.catch] never returns: the attempt is
   lost, the case fails on it, and the worker goes. *)
let fatal_result run (case : Test_tree.case) ~started exn backtrace =
  let failure =
    Failure.raised ?loc:case.loc
      ~msg:"the test raised what ends a process, which ended its worker"
      ~actual:(Failure.exn_to_string exn)
      ~backtrace:(Failure.backtrace_to_string backtrace)
      ()
  in
  let tail () = Capture.output_tail run.capture in
  result_of case ~tail ~duration:(Os.count_s started) ~attempts:1
    ~prop_stats:None (Failure.Fail [ failure ])

let serve config ~suite tests ~temp_dir ~send ~receive ~last =
  Printexc.record_backtrace true;
  temp_base := Some temp_dir;
  let owner = Unix.getpid () in
  (* A process that already spawned one cannot tell a test's domain from
     its own, and is never retired for one. *)
  let spawned = ref false in
  (try Domain.before_first_spawn (fun () -> spawned := true)
   with Invalid_argument _ -> ());
  let cases = Hashtbl.create 64 in
  List.iter
    (fun case -> Hashtbl.replace cases (case_path case) case)
    (Test_tree.flatten tests);
  (* Under a correcting mode the parent alone judges, accepts and writes. *)
  let baselines = Baseline.create ~mode:Baseline.Check () in
  let capture =
    if config.stream then Capture.disabled
    else Capture.create ~log_dir:config.log_dir ~suite ()
  in
  let run = create config ~capture ~baselines in
  (* The parent selected from the store, which nothing writes until the
     run ends. *)
  if config.failed_only then
    record_cases run (read_store (store_path config ~suite));
  let started = Os.counter () in
  let ending = ref false in
  let last_case = ref None and registrant = ref None in
  (* [at_exit] runs its functions newest first, so this one runs after every
     function a test registers and before the parent's, which are not the
     worker's to run: code under test that calls exit ends here, and so does
     the worker. *)
  at_exit (fun () ->
      if Unix.getpid () = owner then begin
        (if not !ending then
           try send (exited run ~last:!last_case) with _ -> ());
        last ();
        Unix._exit 0
      end);
  with_context (In_run run) @@ fun () ->
  let interrupt =
    interrupt
      ~on_event:(fun event -> try send (Event event) with _ -> ())
      run ~started
  in
  with_interrupts ~interrupt run @@ fun () ->
  let receive () =
    receive ~interrupted:(fun () -> Option.iter interrupt run.interrupted)
  in
  (* Under --stream the test writes to the real descriptors, so every event
     waits until the parent has shown it. *)
  let on_event event =
    if config.stream then Capture.drain ();
    send (Event event);
    if config.stream then
      match receive () with
      | Ack -> ()
      | Case _ | Verdict _ | Finish -> invalid_arg "Run.serve: an ack was due"
  in
  if config.baseline <> Baseline.Check then
    run.judge <-
      Some
        (fun attempt ->
          send (Judge attempt);
          match receive () with
          | Verdict verdict -> verdict
          | Finish -> raise Finished
          | Case _ | Ack -> invalid_arg "Run.serve: a verdict was due");
  let rec loop () =
    match receive () with
    | Case path -> (
        let case = Hashtbl.find cases path in
        let started = Os.counter () in
        match run_case ~on_event run case with
        | served ->
            last_case := Some case;
            (* Newest first again: this marks the functions that [case]
               registered, which run next. *)
            at_exit (fun () -> registrant := Some case);
            let stopped = Option.is_some run.stopped in
            send (Done { served; stopped; retire = !spawned });
            loop ()
        | exception Finished -> ()
        | exception ((Out_of_memory | Sys.Break) as exn) ->
            let backtrace = Printexc.get_raw_backtrace () in
            let result = fatal_result run case ~started exn backtrace in
            let served = { result; corrected = false; deferred = None } in
            send (Done { served; stopped = false; retire = true }))
    | Finish -> ()
    | Verdict _ | Ack -> loop ()
  in
  loop ();
  send
    (Released
       (release_fixtures run ~announce:(fun name ->
            on_event (Fixture_release { name }))));
  ending := true;
  let rec handlers () =
    match Stdlib.do_at_exit () with
    | () -> ()
    | exception exn ->
        let backtrace = Printexc.get_raw_backtrace () in
        send (Outside (handler_failure !registrant exn backtrace));
        handlers ()
  in
  handlers ();
  Unix._exit 0

(* The parent's side *)

let supervising config ~suite =
  let baselines = Baseline.create ~mode:config.baseline () in
  (* The parent captures nothing: it reads the log of a worker that died. *)
  let capture =
    if config.stream then Capture.disabled
    else Capture.create ~log_dir:config.log_dir ~suite ()
  in
  create config ~capture ~baselines

let in_run run fn = with_context (In_run run) fn
let add_result run result = run.rev_results <- result :: run.rev_results
let verdict run case attempt = judge run.baselines case attempt

let adjudicate run case served =
  match served.deferred with
  | None -> (served.result, served.corrected)
  | Some (attempt, tail) ->
      let { duration; attempts; prop_stats; _ } = served.result in
      let outcome, corrected, _, refuted = judge run.baselines case attempt in
      ( result_of case
          ~tail:(fun () -> tail)
          ~duration ~attempts ~prop_stats ~refuted outcome,
        corrected )

let lost run (case : Test_tree.case) failure ~duration =
  let groups, test_name = split_last case.path in
  Capture.adopt_log run.capture ~groups ~test_name;
  let tail () = Capture.output_tail run.capture in
  result_of case ~tail ~duration ~attempts:1 ~prop_stats:None
    (Failure.Fail [ failure ])
