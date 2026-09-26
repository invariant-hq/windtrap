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
type mutation = No_mutation | Loop of string list | Armed of string
type broadcast = { selection : bool; mutate : bool }

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
  baseline : Baseline.mode;
  timeout : float option;
  prop_count : int option;
  log_dir : string;
  allow_focus : bool;
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
    baseline = Baseline.Check;
    timeout = None;
    prop_count = None;
    log_dir = Os.default_log_dir ();
    allow_focus = false;
    color = Os.Auto;
    slow_threshold = 1.0;
    verbose = false;
    junit = None;
    mutation = No_mutation;
    github = false;
    invocation = `Mirrors;
    broadcast = not_broadcast;
  }

let for_subset config ~log_dir ~bail =
  {
    config with
    filter = [];
    exclude = [];
    shard = None;
    failed_only = false;
    bail;
    stream = false;
    baseline = Baseline.Check;
    log_dir;
    allow_focus = true;
    junit = None;
    mutation = No_mutation;
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

type result = {
  path : string list;
  outcome : Failure.outcome;
  counted : bool;
  xfail : Test_tree.xfail option;
  slow_tagged : bool;
  duration : float;
  attempts : int;
  prop_stats : Property.stats option;
}

type t = {
  config : config;
  capture : Capture.t;
  baselines : Baseline.t;
  fixtures : (int, fixture_state) Hashtbl.t; (* by accessor id *)
  mutable held : release list; (* the releases still due, newest first *)
  mutable releasing : string option; (* the fixture whose release runs *)
  mutable rev_results : result list;
  mutable interrupted : int option;
      (* a signal that arrived in the runner's own code, acted on at the next
         boundary *)
}

let create config ~capture ~baselines =
  {
    config;
    capture;
    baselines;
    fixtures = Hashtbl.create 8;
    held = [];
    releasing = None;
    rev_results = [];
    interrupted = None;
  }

let config t = t.config
let capture t = t.capture
let baselines t = t.baselines
let results t = List.rev t.rev_results

(* Frames *)

(* What a [setenv] changed, put back when the attempt ends. *)
type binding = { name : string; prior : string option; loc : Loc.t option }

type frame = {
  run : t;
  path : string list;
  loc : Loc.t option; (* the declaration site *)
  corrections : bool; (* whether a check may record a correction *)
  mutable phase : Failure.phase; (* the phase that runs *)
  mutable skip : string option option; (* the reason of the first skip *)
  mutable prop : Property.context option;
  mutable prop_stats : Property.stats option;
  mutable rev_failures : Failure.t list;
  mutable subtests : string list; (* the open subtests, innermost first *)
  mutable temp_root : string option;
  mutable temp_seq : int; (* the number of the next temporary path *)
  mutable bindings : binding list; (* one per name *)
  mutable cwd : (string * Loc.t option) option; (* left by the first chdir *)
}

let frame run (case : Test_tree.case) =
  {
    run;
    path = case.path;
    loc = case.loc;
    corrections = Option.is_none case.xfail;
    phase = Failure.Body;
    skip = None;
    prop = None;
    prop_stats = None;
    rev_failures = [];
    subtests = [];
    temp_root = None;
    temp_seq = 0;
    bindings = [];
    cwd = None;
  }

let prop_context frame = frame.prop

(* A failure without a location was raised in tail position, past
   [Loc.capture]'s delimiter; it takes the declaration site. A nested failure,
   the [inner] of a property failure, keeps its own. *)
let add_failure frame (failure : Failure.t) =
  let failure =
    match (failure.loc, frame.loc) with
    | None, (Some _ as loc) -> { failure with loc }
    | Some _, _ | None, None -> failure
  in
  frame.rev_failures <- failure :: frame.rev_failures

let add_phase_failure frame phase failure =
  add_failure frame (Failure.with_phase phase failure)

let uncaught frame exn backtrace =
  Failure.raised ?loc:frame.loc
    ~actual:(Failure.exn_to_string exn)
    ~backtrace:(Failure.backtrace_to_string backtrace)
    ()

(* The ambient slot *)

(* The only reference to run state in the library: the run while [execute]
   runs, and the frame of an attempt over it while one runs. *)
type context = In_test of frame | In_run of t

let slot : context option ref = ref None

let with_context context fn =
  let previous = !slot in
  slot := Some context;
  Fun.protect ~finally:(fun () -> slot := previous) fn

let active () = Option.is_some !slot

let active_run_error =
  "windtrap: a run is already executing; nothing inside it can start another \
   run"

let outside_run_error =
  "windtrap: no test is running; this call works only in a test's setup, body \
   or teardown, not at module top level, in a fixture's release or after the \
   run"

let current_frame () =
  match !slot with
  | Some (In_test frame) -> frame
  | Some (In_run _) | None -> invalid_arg outside_run_error

let current () = (current_frame ()).run

(* The running test *)

let current_test () = (current_frame ()).path

let split_last path =
  match List.rev path with
  | name :: rev_groups -> (List.rev rev_groups, name)
  | [] -> assert false (* a case's path is never empty *)

(* Inside a subtest a failure is labelled, as data: its [msg] stays the
   user's. *)
let labelled frame (failure : Failure.t) =
  if frame.subtests = [] then failure
  else
    let _, name = split_last frame.path in
    { failure with subtest = name :: List.rev frame.subtests }

let subtest name fn =
  let frame = current_frame () in
  let enclosing = frame.subtests in
  frame.subtests <- name :: enclosing;
  let close () = frame.subtests <- enclosing in
  match Failure.catch fn with
  | exception fatal ->
      let backtrace = Printexc.get_raw_backtrace () in
      close ();
      Printexc.raise_with_backtrace fatal backtrace
  | Ok () -> close ()
  | Error (`Assertion failure) ->
      add_failure frame (labelled frame failure);
      close ()
  | Error (`Exception (exn, backtrace)) ->
      add_failure frame (labelled frame (uncaught frame exn backtrace));
      close ()
  | Error (#Failure.control as c) ->
      close ();
      Failure.reraise c

let check_baseline ?loc subject actual =
  let frame = current_frame () in
  match
    Baseline.check frame.run.baselines ?loc ~correct:frame.corrections subject
      actual
  with
  | () -> ()
  | exception
      (Failure.Check_failure
         { kind = Baseline { state = Unresolvable _; _ }; _ } as unresolvable)
    ->
      raise unresolvable
  | exception Failure.Check_failure failure ->
      add_failure frame (labelled frame failure)

(* Temporary paths *)

let temp_attempts = 64

(* Process-wide: two runs in one process share a pid. *)
let next_temp_root = ref 0

(* A stale directory of an earlier process with the same pid takes the next
   number. *)
let temp_root frame =
  match frame.temp_root with
  | Some dir -> dir
  | None ->
      let base = Filename.get_temp_dir_name () in
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
        t.releasing <- Some release.name;
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

let property ?loc ?count ?max_discard ?examples ?summary gen law =
  let frame = current_frame () in
  let config = frame.run.config in
  let count =
    match count with
    | Some n -> Some (`Declared n)
    | None -> Option.map (fun n -> `Config n) config.prop_count
  in
  let run_law context value =
    let enclosing = frame.prop in
    frame.prop <- Some context;
    Fun.protect
      ~finally:(fun () -> frame.prop <- enclosing)
      (fun () -> law value)
  in
  let fail stats failure =
    frame.prop_stats <- Some stats;
    raise (Failure.Check_failure failure)
  in
  match
    Property.run ?loc ?count ?max_discard ?examples ?summary ~root:config.seed
      ~path:(Test_tree.path_to_string frame.path)
      gen run_law
  with
  | Pass stats -> frame.prop_stats <- Some stats
  | Fail { failure; stats } -> fail stats failure
  | Coverage_failed stats -> fail stats (coverage_failure ?loc:frame.loc stats)
  | Gave_up stats -> fail stats (gave_up_failure ?loc:frame.loc stats)

let prop ?__POS__ ?tags ?timeout ?count ?max_discard ?examples ?summary name gen
    law =
  let loc = Loc.resolve ?__POS__ () in
  Test_tree.test ?__POS__ ?tags ?timeout name (fun () ->
      property ?loc ?count ?max_discard ?examples ?summary gen law)

(* Events *)

type event =
  | Run_started of {
      suite : string;
      total : int;
      selected : int;
      properties : bool;
    }
  | Test_started of { path : string list }
  | Test_finished of result
  | Fixture_release of { name : string }
  | Interrupted of {
      running : string list option;
      releasing : string option;
      results : result list;
      duration : float;
    }

(* Startup errors *)

type startup_error =
  | Duplicate_paths of string list
  | Focused_in_ci of Loc.t option list
  | Update_refused_in_ci
  | No_recorded_failures

let startup_exit_code = function
  | No_recorded_failures -> 2
  | Duplicate_paths _ | Focused_in_ci _ | Update_refused_in_ci -> 1

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

(* The last-failed store *)

(* A magic line, then one [String.escaped] path per line. *)
let store_magic = "windtrap-last-failed 1"

let store_path (config : config) ~suite =
  Filename.concat
    (Filename.concat config.log_dir (Os.sanitize_component suite))
    ".last-failed"

(* A directory opens as a file on POSIX systems, and the first read raises
   [Sys_error], so a read is guarded as the opening is. *)
let read_store path =
  let entries ic =
    match input_line ic with
    | exception End_of_file -> []
    | magic when not (String.equal magic store_magic) -> []
    | _magic ->
        let seen = Hashtbl.create 16 in
        let rec lines acc =
          match input_line ic with
          | exception End_of_file -> List.rev acc
          | line -> (
              match Scanf.unescaped line with
              | "" -> lines acc
              | entry when Hashtbl.mem seen entry -> lines acc
              | entry ->
                  Hashtbl.replace seen entry ();
                  lines (entry :: acc)
              | exception _ -> lines acc)
        in
        lines []
  in
  match In_channel.with_open_bin path entries with
  | entries -> entries
  | exception Sys_error _ -> []

let write_store path entries =
  let buffer = Buffer.create 256 in
  List.iter
    (fun line ->
      Buffer.add_string buffer line;
      Buffer.add_char buffer '\n')
    (store_magic :: List.map String.escaped entries);
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
      (fun (r : result) -> if r.counted then Some (path_string r) else None)
      results
  in
  let survivors =
    if full then []
    else
      let executed = List.map path_string results in
      List.filter (fun entry -> not (List.mem entry executed)) (read_store path)
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
   of them is always told of the same one. The result is the allowlist that
   [--failed] narrows. *)
let startup (config : config) ~suite ~focus_sites ~allowlist paths =
  let in_ci = Os.in_ci () in
  let* () =
    match duplicate_paths paths with
    | [] -> Ok ()
    | duplicates -> Error (Duplicate_paths duplicates)
  in
  let* () =
    if focus_sites <> [] && in_ci && not config.allow_focus then
      Error (Focused_in_ci focus_sites)
    else Ok ()
  in
  let* () =
    match config.baseline with
    | Baseline.Update when in_ci -> Error Update_refused_in_ci
    | Baseline.Update | Baseline.Corrected | Baseline.Check -> Ok ()
  in
  if not config.failed_only then Ok allowlist
  else
    (* A caller's allowlist comes from [for_subset], which clears
       [failed_only]; intersecting the two costs nothing. *)
    let asked path =
      match allowlist with
      | None -> true
      | Some entries -> List.mem path entries
    in
    match
      List.filter
        (fun path -> List.mem path paths && asked path)
        (read_store (store_path config ~suite))
    with
    | [] -> Error No_recorded_failures
    | entries -> Ok (Some entries)

(* The selected cases, the number of declared tests, and whether the suite
   holds a focused node. *)
let select ?allowlist (config : config) ~suite tests =
  install_exit_guard ();
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
  let* allowlist = startup config ~suite ~focus_sites ~allowlist paths in
  (* Hashed: a child of the mutation loop names every path its parent ran. *)
  let allowed =
    match allowlist with
    | None -> fun _ -> true
    | Some entries ->
        let table = Hashtbl.create (List.length entries * 2) in
        List.iter (fun path -> Hashtbl.replace table path ()) entries;
        Hashtbl.mem table
  in
  let tags = tag_predicate config in
  let selected =
    List.filter (is_selected config ~tags ~allowed ~focus_active) cases
  in
  Ok (selected, List.length cases, focus_active)

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

(* [Loc.delimit] keeps a location capture inside user code: a tail-called
   assertion gets none, not the location of the runner's caller. *)
let run_phase frame phase fn =
  frame.phase <- phase;
  match Failure.catch (fun () -> Loc.delimit fn) with
  | Ok () -> ()
  | Error c -> add_caught frame phase c

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
   a failure of the phase that ran. The callback runs the body once: one
   execution is what corrections, labels and temporary paths are keyed by. *)
let scoped frame ~renew scope body =
  let calls = ref 0 in
  let body_raised = ref None in
  let callback resource =
    incr calls;
    if !calls = 1 then begin
      frame.phase <- Failure.Body;
      match Failure.catch (fun () -> Loc.delimit (fun () -> body resource)) with
      | Ok () ->
          frame.phase <- Failure.Teardown;
          renew ()
      | Error c ->
          body_raised := Some c;
          add_caught frame Failure.Body c;
          frame.phase <- Failure.Teardown;
          renew ();
          Failure.reraise c
    end
  in
  frame.phase <- Failure.Setup;
  (match Failure.catch (fun () -> Loc.delimit (fun () -> scope callback)) with
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
  | Equality _ | Containment _ | Raise _ | Property _ | Timeout _ | Message _ ->
      false

let carries_correction (failure : Failure.t) =
  match failure.kind with
  | Baseline { state = Missing _ | Mismatch _; withheld = None; _ } -> true
  | Baseline _ | Equality _ | Containment _ | Raise _ | Property _ | Timeout _
  | Message _ ->
      false

(* The outcome of an attempt, whether each of its failures carries a kept
   correction, and whether it must be the last: a kept correction is
   permanent, and a retry would agree with it. Under [Corrected] only a
   correcting check raises a failure that carries one. *)
let settle frame =
  let baselines = frame.run.baselines in
  let failures = List.rev frame.rev_failures in
  let baseline_only = List.for_all is_baseline failures in
  let keep = baseline_only && Option.is_none frame.skip in
  let kept = Baseline.settle baselines ~keep in
  let corrected =
    keep
    && Baseline.mode baselines = Baseline.Corrected
    && failures <> []
    && List.for_all carries_correction failures
  in
  let failures =
    if keep then failures
    else
      let withheld =
        if baseline_only then Failure.Skipped else Failure.Failed_outside
      in
      List.map (Failure.with_withheld withheld) failures
  in
  let outcome =
    match (failures, frame.skip) with
    | [], Some reason -> Failure.Skip reason
    | [], None -> Failure.Pass
    | failures, _ -> Failure.Fail failures
  in
  (outcome, corrected, kept > 0)

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

let attach_tail capture (outcome : Failure.outcome) =
  match outcome with
  | Fail (first :: rest) -> (
      match Capture.output_tail capture with
      | Some tail -> Failure.Fail (Failure.with_output_tail tail first :: rest)
      | None -> outcome)
  | outcome -> outcome

(* The row of [case], and whether its last attempt's failures all carry kept
   corrections. *)
let run_case ~on_event run (case : Test_tree.case) =
  on_event (Test_started { path = case.path });
  let limit =
    match case.timeout with None -> run.config.timeout | declared -> declared
  in
  let groups, test_name = split_last case.path in
  let rec attempt number spent =
    let frame = frame run case in
    let start = Os.counter () in
    run_attempt frame case ~limit ~groups ~test_name;
    let outcome, corrected, last = settle frame in
    let duration = spent +. Os.count_s start in
    let counted = counts_failed ~xfail:case.xfail outcome in
    if counted && (not last) && number <= case.retries then
      attempt (number + 1) duration
    else
      let outcome =
        match (outcome, case.xfail) with
        | Pass, Some _ -> Failure.Fail [ xpass_failure case ]
        | outcome, _ -> outcome
      in
      let result =
        {
          path = case.path;
          outcome = attach_tail run.capture outcome;
          counted;
          xfail = case.xfail;
          slow_tagged = Test_tree.Tag.mem Test_tree.Tag.slow case.tags;
          duration;
          attempts = number;
          prop_stats = frame.prop_stats;
        }
      in
      run.rev_results <- result :: run.rev_results;
      on_event (Test_finished result);
      (result, corrected)
  in
  attempt 1 0.

(* Signals *)

(* No [exit]: the guard must not run, and the parent must see the signal. *)
let interrupt ~on_event run ~started signal =
  Sys.set_signal Sys.sigalrm (Sys.Signal_handle ignore);
  set_alarm 0.;
  let frame =
    match !slot with
    | Some (In_test frame) -> Some frame
    | Some (In_run _) | None -> None
  in
  (try Capture.abandon run.capture with _ -> ());
  (try
     on_event
       (Interrupted
          {
            running = Option.map (fun frame -> frame.path) frame;
            releasing = run.releasing;
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
      match !slot with
      | Some (In_test _) -> true
      | Some (In_run _) | None -> Option.is_some run.releasing
    in
    if in_user_code then interrupt signal else run.interrupted <- Some signal
  in
  Os.with_signals [ Sys.sigint; Sys.sigterm; Sys.sighup ] handle fn

(* Executing *)

(* [drive] runs [selected] in order, up to the first counted failure under
   [config.bail], and is [true] iff a counted failure carried no kept
   correction. *)
let drive ~on_event ~interrupt run selected =
  let rec loop uncorrected = function
    | [] -> uncorrected
    | case :: cases ->
        Option.iter interrupt run.interrupted;
        let result, corrected = run_case ~on_event run case in
        let uncorrected = uncorrected || (result.counted && not corrected) in
        if run.config.bail && result.counted then uncorrected
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
  exit_code : int;
}

let execute ?(on_event = ignore) ?allowlist config ~suite tests =
  let started = Os.counter () in
  let* selected, total, focus_active = select ?allowlist config ~suite tests in
  let baselines = Baseline.create ~mode:config.baseline () in
  let capture =
    if config.stream then Capture.disabled
    else Capture.create ~log_dir:config.log_dir ~suite ()
  in
  let run = create config ~capture ~baselines in
  with_context (In_run run) @@ fun () ->
  let interrupt = interrupt ~on_event run ~started in
  with_interrupts ~interrupt run @@ fun () ->
  let properties =
    List.exists
      (fun (case : Test_tree.case) ->
        Test_tree.Tag.mem Test_tree.Tag.prop case.tags)
      selected
  in
  on_event
    (Run_started { suite; total; selected = List.length selected; properties });
  let uncorrected = drive ~on_event ~interrupt run selected in
  Option.iter interrupt run.interrupted;
  let release_failures =
    release_fixtures run ~announce:(fun name ->
        on_event (Fixture_release { name }))
  in
  let results = results run in
  let bailed = config.bail && List.exists (fun r -> r.counted) results in
  let full = (not bailed) && List.length results = total in
  update_store (store_path config ~suite) ~full results;
  Baseline.write baselines;
  Option.iter interrupt run.interrupted;
  let refused =
    List.exists
      (function Baseline.Refused _ -> true | Baseline.Written _ -> false)
      (Baseline.writes baselines)
  in
  let exit_code =
    if uncorrected || release_failures <> [] || refused then 1
    else if results = [] then 2
    else 0
  in
  Ok
    {
      run;
      selected;
      total;
      focus_active;
      release_failures;
      duration = Os.count_s started;
      exit_code;
    }

let list_selection config ~suite tests =
  let* selected, _, _ = select config ~suite tests in
  Ok (List.map case_path selected)
