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

(* Configuration *)

type invocation = [ `Exe of string | `Mirrors ]
type mutation = No_mutation | Loop of string list | Armed of string

(* One record for everything an invocation resolves. The executor reads
   the selection and execution fields; the presentation fields — color,
   the slow threshold, verbosity, the JUnit target, the GitHub envelope
   and the hint context — are read by Report alone, and the mutation
   field by the loop alone. A field the executor ignores is not a
   coupling. *)
type config = {
  seed : Seed.seed;
  filter : string option;
  exclude : string option;
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
}

let default_config () =
  {
    seed = Seed.random ();
    filter = None;
    exclude = None;
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
  }

(* The configuration for a run over a SUBTREE of another run's selection,
   used by the mutation loop's forked children. Which knobs to clear and
   which to keep is a statement about this record, so it lives here,
   where whoever adds a fourteenth selection knob is already editing.

   The knobs that select by PATH are cleared — filter, exclude, shard, the
   [--failed] store — because the allowlist the caller hands its child IS
   that selection: it names the paths the parent executed, so applying
   any of them again could only narrow it further.

   The knobs that select by TAG are kept verbatim. A test's tags are not
   in its path, so an allowlist cannot express them, and keeping them is
   what makes the child's selection the parent's by construction rather
   than by coincidence. The root seed is kept for the same
   family of reasons: per-case seeds derive from (root, path, index), so a
   child running 24 of 900 tests sees the same property cases the parent
   saw.

   [baseline = Check] makes baseline checking read-only by construction
   — a correction is recorded only under Corrected and Update — and the
   log directory is the child's own so that its capture files and its
   last-failed store cannot touch the parent's. A child reports nothing,
   so it writes no JUnit either; and it is not itself a mutation run —
   the loop is its parent, and it arms what the parent hands it. *)
let for_subset config ~log_dir ~bail =
  {
    config with
    filter = None;
    exclude = None;
    shard = None;
    failed_only = false;
    bail;
    stream = false;
    baseline = Baseline.Check;
    log_dir;
    allow_focus = true;
    junit = None;
    mutation = No_mutation;
  }

(* Run records *)

(* A fixture's cache entry. [fx_state] embeds the acquired value in the
   accessor's private exception constructor ([Acquired]), records a skip
   raised during acquisition (cached as a skip, not an error),
   or holds the acquisition error with its backtrace; [fx_release] closes
   over the typed value directly, so releasing never needs to project. *)
type fixture_state =
  | Acquired of exn
  | Skipped of string option
  | Failed of exn * Printexc.raw_backtrace

type fixture_entry = {
  fx_name : string;
  fx_loc : Loc.t option;
  fx_state : fixture_state;
  fx_release : (unit -> unit) option;
}

type subject = Test | Fixture_release

let fixture_release_path = [ "fixture release" ]

type result = {
  path : string list;
  subject : subject;
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
  fixtures : (int, fixture_entry) Hashtbl.t;
  mutable acquired : int list; (* fixture ids, most recently acquired first *)
  mutable rev_results : result list;
}

let create config ~capture ~baselines =
  {
    config;
    capture;
    baselines;
    fixtures = Hashtbl.create 8;
    acquired = [];
    rev_results = [];
  }

let config t = t.config
let capture t = t.capture
let baselines t = t.baselines

(* Per-test frames *)

(* What one [setenv] recorded: the binding to put back when the attempt
   ends, and where the change was made — a restoration that cannot happen
   is reported at the call that made the change, not at the runner's
   boundary, which is nobody's code. *)
type env_restore = {
  er_name : string;
  er_prior : string option;
  er_loc : Loc.t option;
}

type frame = {
  owner : t;
  fr_path : string list;
  fr_loc : Loc.t option; (* declaration site: the location fallback *)
  fr_corrections : bool; (* may this attempt record baseline corrections? *)
  mutable fr_prop : Property.context option;
  mutable fr_rev_failures : Failure.t list;
  mutable fr_subtests : string list; (* enclosing subtests, innermost first *)
  mutable fr_temp_root : string option; (* the attempt's scratch dir *)
  mutable fr_temp_seq : int; (* next path number within the scratch dir *)
  mutable fr_env : env_restore list; (* one entry per name, first set wins *)
  mutable fr_cwd : (string * Loc.t option) option; (* dir at the first chdir *)
}

let frame ?(corrections = true) t ~path ~loc =
  {
    owner = t;
    fr_path = path;
    fr_loc = loc;
    fr_corrections = corrections;
    fr_prop = None;
    fr_rev_failures = [];
    fr_subtests = [];
    fr_temp_root = None;
    fr_temp_seq = 0;
    fr_env = [];
    fr_cwd = None;
  }

let run_of_frame frame = frame.owner
let path frame = frame.fr_path
let loc frame = frame.fr_loc

(* The declaration site as a runner-made failure's own location: [loc] when
   the call that the failure is about left a frame, the test's declaration
   otherwise. Failures the runner constructs name their site through this
   and reach [add_failure] as [Recorded]; only a failure that names none —
   an assertion verb's, in tail position — is filled there and marked. *)
let site_or_declaration frame loc =
  match loc with Some _ -> loc | None -> frame.fr_loc

let add_failure frame failure =
  (* The one fallback point of the attribution ladder: a failure recorded
     without a location — its failing call sat in tail position, so
     Loc.capture stopped at the runner's delimiter — is attributed to the
     test's declaration, and marked [Declaration] so the report can say so.
     Only the top-level failure is filled; nested failures (a property
     failure's [inner]) are left untouched, and a failure needing no fill is
     stored as given. *)
  let failure =
    match (failure.Failure.loc, frame.fr_loc) with
    | Some _, _ | None, None -> failure
    | None, (Some _ as loc) ->
        { failure with Failure.loc; attribution = Failure.Declaration }
  in
  frame.fr_rev_failures <- failure :: frame.fr_rev_failures

let failures frame = List.rev frame.fr_rev_failures
let prop_context frame = frame.fr_prop

let with_prop_context frame ctx fn =
  let previous = frame.fr_prop in
  frame.fr_prop <- Some ctx;
  Fun.protect ~finally:(fun () -> frame.fr_prop <- previous) fn

(* The ambient slot *)

(* The one ambient slot: the only run-state [ref] in the
   library. It holds what the process is currently executing — the run
   itself while the runner's executing span is open, overlaid by the frame
   of the test attempt while one runs; the runner is sequential, one
   domain. *)
type context = In_test of frame | In_run of t

let slot : context option ref = ref None

let with_context context fn =
  let previous = !slot in
  slot := Some context;
  Fun.protect ~finally:(fun () -> slot := previous) fn

let with_frame frame fn = with_context (In_test frame) fn
let with_active t fn = with_context (In_run t) fn
let active () = Option.is_some !slot

let active_run_error =
  "windtrap: run is already active — a test body cannot start another run"

let outside_run_error =
  "windtrap: no test is running. Assertions, [output ()], [expect], [collect], \
   [setenv], [chdir] and fixture accessors work only inside a test body \
   executed by [run] — not at module toplevel, and not after the run."

let current_frame () =
  match !slot with
  | Some (In_test frame) -> frame
  | Some (In_run _) | None -> invalid_arg outside_run_error

let current () = (current_frame ()).owner

(* Test-body operations *)

let current_test () = (current_frame ()).fr_path

(* The sub-case identity rides in the failure's [subtest] slot as data;
   [msg] stays purely the user's annotation. Renderers derive the
   displayed label from the components. *)
let relabel frame (failure : Failure.t) =
  let stack = List.rev frame.fr_subtests in
  let components =
    match List.rev frame.fr_path with
    | leaf :: _ -> leaf :: stack
    | [] -> stack (* hand-built frames only; case paths are never empty *)
  in
  { failure with Failure.subtest = components }

let subtest name fn =
  let frame = current_frame () in
  frame.fr_subtests <- name :: frame.fr_subtests;
  let pop () =
    frame.fr_subtests <-
      (match frame.fr_subtests with _ :: rest -> rest | [] -> [])
  in
  match fn () with
  | () -> pop ()
  | exception Failure.Check_failure failure ->
      (* Record and return: siblings continue. The label is
         computed before popping so it includes this subtest's name. *)
      add_failure frame (relabel frame failure);
      pop ()
  | exception ((Failure.Skip_test _ | Failure.Timeout _) as control) ->
      (* The runner owns skip and timeout: they abort the whole test. *)
      let backtrace = Printexc.get_raw_backtrace () in
      pop ();
      Printexc.raise_with_backtrace control backtrace
  | exception exn when Failure.is_fatal exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      pop ();
      Printexc.raise_with_backtrace exn backtrace
  | exception exn ->
      (* Any other exception is this sub-case's failure, not the test's:
         record it labeled, with its backtrace, and let siblings run. Its
         location is the declaration, named here as the runner names it for
         an uncaught exception at the test boundary — no verb raised it, so
         there is no site to have missed. *)
      let backtrace = Printexc.get_raw_backtrace () in
      let failure =
        Failure.raised ?loc:frame.fr_loc ~actual:(Printexc.to_string exn)
          ~backtrace:(Failure.backtrace_to_string backtrace)
          ()
      in
      add_failure frame (relabel frame failure);
      pop ()

(* Baselines *)

(* The registry is the run's; the failure's location is the caller's
   ([loc], the literal's position or the call frame). A check without one
   reaches [add_failure] unfilled and is attributed there, marked. *)
(* A checkpoint, not an assertion: a mismatch is recorded on the frame
   and the call returns, so the body continues to its later expectations,
   the attempt fails at its end with every mismatch reported, and a
   correcting run records every correction in one pass — one
   [dune promote] accepts them all. The labeling is [subtest]'s, so a
   checkpoint inside a subtest carries its name. The one baseline failure
   that still raises is a path that cannot be proven under the project
   root: nothing after it is meaningful. *)
let check_baseline ?loc subject actual =
  let frame = current_frame () in
  match
    Baseline.check frame.owner.baselines ?loc ~correct:frame.fr_corrections
      subject actual
  with
  | () -> ()
  | exception
      (Failure.Check_failure
         {
           Failure.kind = Failure.Baseline { state = Failure.Unresolvable _; _ };
           _;
         } as unresolvable) ->
      raise unresolvable
  | exception Failure.Check_failure failure ->
      let failure =
        if frame.fr_subtests = [] then failure else relabel frame failure
      in
      add_failure frame failure

(* Executor-owned scratch *)

let temp_create_attempts = 64

(* Scratch identity. Not run state, for the reason [next_fixture_id] is
   not: two runs in one process share a pid, so a per-run counter would
   have the second run propose names the first already used. *)
let next_temp_seq = ref 0

(* The attempt's scratch directory, created lazily. Names are unique within
   the process ([next_temp_seq] never repeats) and carry the pid against
   concurrent runners; EEXIST from a stale directory retries with the next
   number. *)
let temp_root frame =
  match frame.fr_temp_root with
  | Some dir -> dir
  | None ->
      let base = Filename.get_temp_dir_name () in
      let pid = Unix.getpid () in
      let rec create attempts =
        let n = !next_temp_seq in
        incr next_temp_seq;
        let candidate =
          Filename.concat base (Printf.sprintf "windtrap-%d-%d" pid n)
        in
        match Unix.mkdir candidate 0o700 with
        | () -> candidate
        | exception Unix.Unix_error (Unix.EEXIST, _, _)
          when attempts < temp_create_attempts ->
            create (attempts + 1)
      in
      let dir = create 1 in
      frame.fr_temp_root <- Some dir;
      dir

let temp_dir ?(prefix = "dir") () =
  let frame = current_frame () in
  let root = temp_root frame in
  let n = frame.fr_temp_seq in
  frame.fr_temp_seq <- n + 1;
  let name = Os.sanitize_component prefix ^ "-" ^ string_of_int n in
  let dir = Filename.concat root name in
  Unix.mkdir dir 0o700;
  dir

let temp_file ?(suffix = "") () =
  let frame = current_frame () in
  let root = temp_root frame in
  let n = frame.fr_temp_seq in
  frame.fr_temp_seq <- n + 1;
  let suffix = if suffix = "" then "" else Os.sanitize_component suffix in
  let path = Filename.concat root ("file-" ^ string_of_int n ^ suffix) in
  let fd =
    Unix.openfile path
      [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_EXCL; Unix.O_CLOEXEC ]
      0o600
  in
  Unix.close fd;
  path

(* Best-effort recursive removal: [lstat] so symbolic links are removed,
   never followed; every filesystem error is swallowed — scratch cleanup
   must not fail a test or mask its outcome — resources are released on
   every path where the runner regains control. *)
let rec remove_tree path =
  match (Unix.lstat path).Unix.st_kind with
  | Unix.S_DIR -> (
      let entries = try Sys.readdir path with Sys_error _ -> [||] in
      Array.iter (fun name -> remove_tree (Filename.concat path name)) entries;
      try Unix.rmdir path with Unix.Unix_error _ -> ())
  | _ -> ( try Unix.unlink path with Unix.Unix_error _ -> ())
  | exception Unix.Unix_error _ -> ()

let remove_temp frame =
  match frame.fr_temp_root with
  | None -> ()
  | Some dir ->
      frame.fr_temp_root <- None;
      remove_tree dir

(* Executor-restored process state

   The environment and the working directory belong to the process, not to
   the test: nothing scopes them but putting them back. So the body records
   what it changed and the runner undoes it at the attempt boundary — the
   same bargain the scratch paths make, holding on every outcome for the
   same reason, that the runner regains control on every outcome. *)

let setenv name value =
  let frame = current_frame () in
  (* The prior binding is read before [Os.setenv] changes it, but recorded
     only after [Os.setenv] returns: [Os.setenv] validates the name before it
     touches the process, and a record made before that validation would be
     replayed at [reclaim] — where the same rejection reads as a
     restoration failure about a change that never happened. *)
  let prior = Sys.getenv_opt name in
  Os.setenv name value;
  (* First set wins: what gets restored is what was there before the
     attempt's first [setenv] of this name, so a test that binds a variable
     twice still leaves behind what it found. *)
  if not (List.exists (fun e -> e.er_name = name) frame.fr_env) then
    frame.fr_env <-
      { er_name = name; er_prior = prior; er_loc = Loc.capture () }
      :: frame.fr_env

let chdir dir =
  let frame = current_frame () in
  (* Captured at the first [chdir] rather than at the attempt's start: a
     test that never moves pays nothing, and the directory to return to is
     the same one either way. *)
  if frame.fr_cwd = None then
    frame.fr_cwd <- Some (Sys.getcwd (), Loc.capture ());
  Unix.chdir dir

(* A restoration that cannot happen is recorded, never raised: the attempt
   is over, so there is no phase left to interrupt — and unlike a leaked
   scratch directory, which is inert, a process left in the wrong place or
   still holding the test's binding is precisely the fact the next test's
   baffling failure needs stated up front. It is attributed to the call
   that made the change, since the boundary is nobody's code — or to the
   declaration when that call left no frame: [setenv] and [chdir] take no
   [?__POS__], so the fallback's hint would name a remedy they lack. *)
let restore_failure frame ?loc text =
  add_failure frame
    (Failure.with_phase Failure.Teardown
       (Failure.message ?loc:(site_or_declaration frame loc) text))

let restore_cwd frame =
  match frame.fr_cwd with
  | None -> ()
  | Some (dir, loc) -> (
      frame.fr_cwd <- None;
      try Unix.chdir dir
      with Unix.Unix_error (err, _, _) ->
        restore_failure frame ?loc
          (Printf.sprintf
             "the test changed the working directory and it could not be \
              restored to %s: %s — every later test in this process runs from \
              the wrong place"
             dir (Unix.error_message err)))

let restore_env frame =
  let entries = frame.fr_env in
  frame.fr_env <- [];
  List.iter
    (fun entry ->
      match Os.setenv entry.er_name entry.er_prior with
      | () -> ()
      | exception exn when not (Failure.is_fatal exn) ->
          restore_failure frame ?loc:entry.er_loc
            (Printf.sprintf
               "the test set %s and its prior binding could not be restored: \
                %s — every later test in this process sees the test's value"
               entry.er_name (Printexc.to_string exn)))
    entries

let reclaim frame =
  (* The directory first: removing the scratch tree while the process is
     still sitting inside it would strand it in a deleted directory. *)
  restore_cwd frame;
  restore_env frame;
  remove_temp frame

(* Fixtures *)

(* Accessor identity. Not run state: ids mint process-wide identities for
   fixture accessors and never reset — the per-run cache in [t] is keyed by
   them, which is what makes a later run re-acquire. *)
let next_fixture_id = ref 0

let fixture : type a. ?teardown:(a -> unit) -> (unit -> a) -> unit -> a =
 fun ?teardown create ->
  let module Cell = struct
    exception Value of a
  end in
  incr next_fixture_id;
  let id = !next_fixture_id in
  (* Best-effort declaration site, for release announcements and
     Release-phase failure locations. Not in tail position. *)
  let loc = Loc.capture () in
  let name =
    match loc with
    | Some l -> "fixture (" ^ Loc.to_string l ^ ")"
    | None -> "fixture #" ^ string_of_int id
  in
  fun () ->
    let run = (current_frame ()).owner in
    match Hashtbl.find_opt run.fixtures id with
    | Some { fx_state = Acquired (Cell.Value value); _ } -> value
    | Some { fx_state = Acquired _; _ } ->
        assert false (* the id is private to this accessor *)
    | Some { fx_state = Skipped reason; _ } ->
        (* Every later use skips with the cached reason. *)
        raise (Failure.Skip_test reason)
    | Some { fx_state = Failed (exn, backtrace); _ } ->
        Printexc.raise_with_backtrace exn backtrace
    | None -> (
        match create () with
        | value ->
            let fx_release =
              Option.map (fun teardown () -> teardown value) teardown
            in
            Hashtbl.replace run.fixtures id
              {
                fx_name = name;
                fx_loc = loc;
                fx_state = Acquired (Cell.Value value);
                fx_release;
              };
            run.acquired <- id :: run.acquired;
            value
        | exception Failure.Skip_test reason ->
            (* A skip during acquisition is cached as a skip,
               not an error — nothing is registered for release. *)
            let backtrace = Printexc.get_raw_backtrace () in
            Hashtbl.replace run.fixtures id
              {
                fx_name = name;
                fx_loc = loc;
                fx_state = Skipped reason;
                fx_release = None;
              };
            Printexc.raise_with_backtrace (Failure.Skip_test reason) backtrace
        | exception exn ->
            let backtrace = Printexc.get_raw_backtrace () in
            Hashtbl.replace run.fixtures id
              {
                fx_name = name;
                fx_loc = loc;
                fx_state = Failed (exn, backtrace);
                fx_release = None;
              };
            Printexc.raise_with_backtrace exn backtrace)

let release_failure entry exn =
  Failure.message ?loc:entry.fx_loc
    (entry.fx_name ^ ": release raised " ^ Printexc.to_string exn)
  |> Failure.with_phase Failure.Release

let release_fixtures t ~announce =
  let ids = t.acquired in
  t.acquired <- [];
  (* Release order is contract — reverse acquisition —
     so walk [ids] (most recently acquired first) with an explicit loop
     rather than lean on a fold's unspecified effect order. *)
  let rec release_all acc = function
    | [] -> List.rev acc
    | id :: ids -> (
        match Hashtbl.find_opt t.fixtures id with
        | None | Some { fx_release = None; _ } -> release_all acc ids
        | Some ({ fx_release = Some release; _ } as entry) -> (
            announce entry.fx_name;
            (* [Loc.delimit]: a location captured inside a release teardown
               must not walk past the runner into its caller. *)
            match Loc.delimit release with
            | () -> release_all acc ids
            | exception exn when not (Failure.is_fatal exn) ->
                release_all (release_failure entry exn :: acc) ids))
  in
  release_all [] ids

(* Results *)

let record t result = t.rev_results <- result :: t.rev_results
let results t = List.rev t.rev_results

(* The executor

   The sequential drive loop, SIGALRM timeout, retry loop and last-failed
   persistence derive from windtrap v1's lib/runner.ml, rebuilt over the
   per-run record above, scoped bodies (Test_tree) and the typed failure
   model, with the boundary capturing body and teardown outcomes
   independently. *)

(* The exit guard. Registered once per process (registration state, like
   Run's fixture ids, is process identity, not run state);
   per-run data flows through the ambient slot. Stdlib.at_exit runs each
   registered function at most once, so an interception consumes the
   registration: the guard re-arms itself before raising. Relies on
   Stdlib.exit = do_at_exit (); sys_exit — an exception from an at_exit
   function propagates to exit's caller (pinned by the child-status
   regression test).

   It also belongs to the process that armed it. A forked child inherits
   active and the at_exit registration, so without the owning pid the
   child's exit is intercepted too: instead of terminating, the child
   returns into the runner, executes every remaining test, prints a second
   report, rewrites the last-failed store and any JUnit file, and exits
   with the run's code rather than its own — a parent test asserting on
   the child's status then reads the wrong answer. *)
let exit_guard_owner = ref None

let owns_run () =
  match !exit_guard_owner with
  | Some pid -> pid = Unix.getpid ()
  | None -> false

let rec exit_guard () =
  if owns_run () && active () then begin
    at_exit exit_guard;
    raise Failure.Exit_attempt
  end

let install_exit_guard () =
  (* One ref, not two: an unset owner is exactly "never registered in
     this process tree". A forked child inherits [Some parent_pid] along
     with the registration itself, so it correctly registers nothing and
     only claims ownership. *)
  if !exit_guard_owner = None then at_exit exit_guard;
  exit_guard_owner := Some (Unix.getpid ())

(* Property tests *)

(* The channel between a [prop] body and the classifier below: the body
   always raises its engine outcome, the Body-phase guard of the same test
   consumes it. Never installed in run state and never visible
   to user code — the raise happens after the user's law returned. *)
exception Prop_outcome of Property.outcome

let prop ?__POS__ ?tags ?timeout ?count ?max_discard ?examples name gen law =
  let loc = Loc.resolve ?__POS__ () in
  let body () =
    let frame = current_frame () in
    let config = config (run_of_frame frame) in
    (* Case count: declaration site > --prop-count > engine default. The
       engine is told which, not just how many: it stamps a config-sourced
       count on failure payloads so the replay hint can restate the flag,
       while a declaration-site count replays by itself. *)
    let count =
      match count with
      | Some n -> Some (`Declared n)
      | None -> Option.map (fun n -> `Config n) config.prop_count
    in
    let path = Test_tree.path_to_string (path frame) in
    let outcome =
      Property.run ?loc ?count ?max_discard ?examples ~root:config.seed ~path
        gen (fun context value ->
          with_prop_context frame context (fun () -> law value))
    in
    raise (Prop_outcome outcome)
  in
  Test_tree.test ?__POS__ ?tags ?timeout name body

let coverage_failure ?loc (stats : Property.stats) =
  let unsatisfied =
    List.filter (fun c -> not c.Property.satisfied) stats.Property.coverage
  in
  Failure.message ?loc
    ("never covered: "
    ^ String.concat ", "
        (List.map (fun c -> Pp.str "%S" c.Property.label) unsatisfied)
    ^ Pp.str " (over %d passing cases)" stats.Property.cases)

let gave_up_failure ?loc (stats : Property.stats) =
  Failure.message ?loc
    (Pp.str
       "property gave up: %d discards exhausted the generation budget (%d \
        cases passed)"
       stats.Property.discards stats.Property.cases)

(* The per-test boundary *)

(* Root for the per-test global [Random] reseed: an arbitrary frozen
   constant, deliberately not the run's root seed — the property path never
   touches [Random], and [Random] users get a stream that depends on the
   test's path only. *)
let random_reseed_root = 0x57696e6474726170L

let with_isolated_random ~path fn =
  let saved = Random.get_state () in
  Random.init
    (Int64.to_int (Seed.derive ~root:random_reseed_root ~path ~index:0));
  Fun.protect ~finally:(fun () -> Random.set_state saved) fn

(* A SIGALRM window bounding setup + body + teardown, and the [renew] that
   keeps it bounding them.

   The timer is one-shot, so once it has fired the scope is unguarded — and
   [Failure.Timeout] is not fatal, so the body's phase guard absorbs it and
   [phases] goes on to run teardown. Without re-arming, a teardown that
   blocks after a body timeout runs forever: the run hangs with no output,
   which is precisely what the per-test limit exists to prevent. [renew]
   re-arms for whatever remains of the limit, or for a fresh limit when the
   earlier phases consumed it — cleanup is not optional, so it is given a
   bounded window rather than none.

   This hand-rolls the cleanup instead of Fun.protect: the alarm can expire
   exactly as the scope exits and be delivered at a poll point inside the
   cleanup itself, and that late [Timeout] must be absorbed — never surface
   as [Finally_raised] or leak into caller code. The [armed] flag inertizes
   the handler; [disarm] retries once around a delivery that interrupts it. *)
let with_timeout limit fn =
  let no_renew () = () in
  match limit with
  | None -> fn no_renew
  | Some _ when Sys.win32 -> fn no_renew (* documented no-op *)
  | Some limit -> (
      let armed = ref true in
      let started = Os.counter () in
      let previous_handler =
        Sys.signal Sys.sigalrm
          (Sys.Signal_handle
             (fun _ -> if !armed then raise (Failure.Timeout limit)))
      in
      let set_timer seconds =
        ignore
          (Unix.setitimer Unix.ITIMER_REAL
             { Unix.it_value = seconds; it_interval = 0. })
      in
      let rec disarm () =
        match
          armed := false;
          set_timer 0.;
          Sys.set_signal Sys.sigalrm previous_handler
        with
        | () -> ()
        | exception Failure.Timeout _ -> disarm ()
      in
      let renew () =
        if !armed then begin
          let remaining = limit -. Os.count_s started in
          set_timer (if remaining > 0. then remaining else limit)
        end
      in
      set_timer limit;
      match fn renew with
      | value ->
          disarm ();
          value
      | exception exn ->
          let backtrace = Printexc.get_raw_backtrace () in
          disarm ();
          Printexc.raise_with_backtrace exn backtrace)

let timeout_failure ?loc limit =
  Failure.message ?loc (Pp.str "timed out after %gs" limit)

(* Expected failures *)

(* Whether a raw attempt outcome counts as failed for retries, -x, the
   exit code, and the last-failed store, under the test's expectation: an
   expected failure does not count; an unexpected pass does; skips never
   count. Defined over the outcome as classified — the synthesized xfail-pass
   failure below is applied only after this decision. *)
let counts_failed ~(xfail : Test_tree.xfail option) (outcome : Failure.outcome)
    =
  match (outcome, xfail) with
  | Failure.Fail _, None -> true
  | Failure.Fail _, Some _ -> false
  | Failure.Pass, Some _ -> true
  | Failure.Pass, None | Failure.Skip _, _ -> false

(* The recorded failure of an [xfail] test that passed: a self-describing
   Message entry, so today's renderers already explain the red result. *)
let xpass_failure (case : Test_tree.case) =
  let reason =
    match case.Test_tree.xfail with
    | Some { Test_tree.reason = Some reason } -> " (" ^ reason ^ ")"
    | Some { Test_tree.reason = None } | None -> ""
  in
  Failure.message ?loc:case.Test_tree.loc
    (Pp.str "expected to fail%s, but the test passed" reason)

(* One attempt of one test: fresh frame installed by the caller. Returns the
   classified outcome and, for property tests, the engine's stats. Fatal
   exceptions propagate. *)
let run_attempt run frame (case : Test_tree.case) ~limit ~groups ~test_name =
  let prop_stats = ref None in
  let skipped = ref None in
  let phase = ref Failure.Body in
  let record_failure ph failure =
    add_failure frame (Failure.with_phase ph failure)
  in
  let record_prop_outcome ph = function
    | Property.Pass stats -> prop_stats := Some stats
    | Property.Fail { failure; stats } ->
        prop_stats := Some stats;
        record_failure ph failure
    | Property.Coverage_failed stats ->
        prop_stats := Some stats;
        record_failure ph (coverage_failure ?loc:case.Test_tree.loc stats)
    | Property.Gave_up stats ->
        prop_stats := Some stats;
        record_failure ph (gave_up_failure ?loc:case.Test_tree.loc stats)
  in
  let classify ph exn backtrace =
    match exn with
    | Prop_outcome outcome -> record_prop_outcome ph outcome
    | Failure.Check_failure failure -> record_failure ph failure
    | Failure.Skip_test reason -> if !skipped = None then skipped := Some reason
    | Failure.Timeout limit ->
        record_failure ph (timeout_failure ?loc:case.Test_tree.loc limit)
    | Failure.Exit_attempt ->
        record_failure ph
          (Failure.message ?loc:case.Test_tree.loc
             "the test called exit — intercepted; a test must return or raise, \
              never exit the process")
    | exn ->
        record_failure ph
          (Failure.raised ?loc:case.Test_tree.loc
             ~actual:(Printexc.to_string exn)
             ~backtrace:(Failure.backtrace_to_string backtrace)
             ())
  in
  (* Run one phase, classifying everything non-fatal it raises. [Loc.delimit]
     bounds location capture: a tail-called assertion whose own frame is gone
     yields no location here rather than the runner's caller. *)
  let guard : type r. Failure.phase -> (unit -> r) -> r option =
   fun ph fn ->
    phase := ph;
    match Loc.delimit fn with
    | value -> Some value
    | exception exn when not (Failure.is_fatal exn) ->
        let backtrace = Printexc.get_raw_backtrace () in
        classify ph exn backtrace;
        None
  in
  (* A scoping function owns acquisition, the body and release in one call,
     so the runner can only run the body inside the callback and attribute
     whatever escapes. A bracket is such a scope, derived in [Test_tree]
     from its setup and teardown, and gets exactly this treatment.

     The body's failure is recorded where it happens and then re-raised
     through [scope]: a scope that cancels or cleans up on the exception
     path still sees it, and a scope that swallows it cannot turn a failed
     test green. Anything else escaping is the scope's own, attributed by
     how far the callback got — [Setup] before it, [Teardown] after it
     returned.

     Calling back exactly once is the contract. Zero calls means the body
     never ran, which must not report as a pass; a second call is refused
     rather than served, because one execution is the unit everything else
     is keyed by (baseline corrections, subtest labels, scratch paths). *)
  let scoped : type r.
      renew:(unit -> unit) -> ((r -> unit) -> unit) -> (r -> unit) -> unit =
   fun ~renew scope body ->
    let entries = ref 0 in
    let body_left = ref false in
    let body_exn = ref None in
    let scope_raised = ref false in
    let callback resource =
      incr entries;
      if !entries = 1 then begin
        phase := Failure.Body;
        match Loc.delimit (fun () -> body resource) with
        | () ->
            body_left := true;
            phase := Failure.Teardown;
            renew ()
        | exception exn when not (Failure.is_fatal exn) ->
            let backtrace = Printexc.get_raw_backtrace () in
            body_left := true;
            body_exn := Some exn;
            classify Failure.Body exn backtrace;
            phase := Failure.Teardown;
            (* The body may have consumed the window; the scope's release
               still has to be bounded. *)
            renew ();
            Printexc.raise_with_backtrace exn backtrace
      end
    in
    phase := Failure.Setup;
    (match Loc.delimit (fun () -> scope callback) with
    | () -> ()
    | exception exn when not (Failure.is_fatal exn) ->
        let backtrace = Printexc.get_raw_backtrace () in
        scope_raised := true;
        (* The body's own exception on its way out is already recorded. *)
        if not (match !body_exn with Some e -> e == exn | None -> false) then
          classify
            (if !entries = 0 then Failure.Setup
             else if !body_left then Failure.Teardown
             else Failure.Body)
            exn backtrace);
    (* A scope that raised — or skipped — instead of calling back has
       already said what happened; only a clean return needs explaining. *)
    if !entries = 0 && not !scope_raised then
      record_failure Failure.Setup
        (Failure.message ?loc:case.Test_tree.loc
           "the scope returned without running the test body — a scope must \
            call its callback exactly once")
    else if !entries > 1 then
      record_failure Failure.Body
        (Failure.message ?loc:case.Test_tree.loc
           (Pp.str
              "the scope called its callback %d times — a scope must call it \
               exactly once; the test body ran on the first call only"
              !entries))
  in
  let phases renew =
    match case.Test_tree.body with
    | Test_tree.Body fn -> ignore (guard Failure.Body fn)
    | Test_tree.Scoped { scope; body } -> scoped ~renew scope body
  in
  let boundary () =
    with_isolated_random ~path:(Test_tree.path_to_string case.Test_tree.path)
      (fun () ->
        with_timeout limit (fun renew ->
            match phases renew with
            | () -> ()
            | exception Failure.Timeout limit ->
                (* The alarm fired between two phase guards. *)
                record_failure !phase
                  (timeout_failure ?loc:case.Test_tree.loc limit)))
  in
  (* Reclamation runs after the attempt, outside the timeout window and the
     capture redirection, on every path where the runner regains control —
     only a fatal exception escapes the frame, and it too passes through the
     cleanup. [reclaim] never raises; a restoration it could not perform is
     recorded on the frame, so the outcome below picks it up. *)
  (match
     with_frame frame (fun () ->
         match
           Capture.with_capture (capture run) ~groups ~test_name boundary
         with
         | () -> ()
         | exception exn when not (Failure.is_fatal exn) ->
             (* Capture setup or restore failed (e.g. the log file could not
                be created): a failure of this test, not of the run. *)
             let backtrace = Printexc.get_raw_backtrace () in
             classify Failure.Body exn backtrace)
   with
  | () -> reclaim frame
  | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      reclaim frame;
      Printexc.raise_with_backtrace exn backtrace);
  let failures = failures frame in
  let outcome =
    match failures with
    | [] -> (
        match !skipped with
        | Some reason -> Failure.Skip reason
        | None -> Failure.Pass)
    | failures -> Failure.Fail failures
  in
  (* The one gating rule of corrections, both storages: an attempt that
     ended in any failure that is not a baseline failure keeps none of the
     corrections it recorded, nor does one that skipped, whose verdict is
     withheld. (An [xfail] test's attempt records none in the first place:
     its frame checks read-only, since its mismatch is the failure the
     annotation expects.) The attempt is [corrected] when every failure
     it has is a baseline failure with a kept correction — [Baseline.check]
     records exactly one correction per failure it raises in Corrected
     mode, none for a failure it cannot correct (an unresolvable path, a
     second content for an accepted key) — which is what lets the exit
     code leave such an attempt to the [diff?] that follows. *)
  let baseline_only =
    List.for_all
      (fun (f : Failure.t) ->
        match f.Failure.kind with Failure.Baseline _ -> true | _ -> false)
      failures
  in
  let keep = baseline_only && !skipped = None in
  let kept = Baseline.settle (baselines run) ~keep in
  let corrected =
    failures <> [] && baseline_only && kept = List.length failures
  in
  (outcome, !prop_stats, corrected)

(* A failing test's report carries its bounded captured output — the
   final attempt's, attached to the first failure entry. *)
let attach_tail capture outcome =
  match outcome with
  | Failure.Fail (first :: rest) -> (
      match Capture.output_tail capture with
      | Some tail -> Failure.Fail (Failure.with_output_tail tail first :: rest)
      | None -> outcome)
  | outcome -> outcome

let split_last path =
  match List.rev path with
  | [] -> ([], "unnamed") (* Test_tree.case paths are never empty *)
  | name :: rev_groups -> (List.rev rev_groups, name)

(* Events *)

(* Payloads are immutable projections: counts, identities, recorded
   results — never the live run record. The run handle belongs to whoever
   owns the session (the driver reads it off the outcome); an observer
   holds only data already decided. *)
type event =
  | Run_started of { suite : string; total : int; selected : int }
  | Test_started of { path : string list }
  | Test_finished of result
  | Fixture_release of { name : string }

(* Runs one test to completion (retries included), records its result, and
   returns it with whether it counted as failed (see [counts_failed]) — the
   caller drives -x, the exit code, and the last-failed store from the
   flag, never from the recorded outcome alone — and whether its final
   attempt's failures are all kept corrections (see [run_attempt]). *)
let run_case ~on_event run (case : Test_tree.case) =
  on_event (Test_started { path = case.Test_tree.path });
  let config = config run in
  let limit =
    match case.Test_tree.timeout with
    | Some _ as declared -> declared
    | None -> config.timeout
  in
  let groups, test_name = split_last case.Test_tree.path in
  let total_attempts = case.Test_tree.retries + 1 in
  let rec attempt number spent =
    let frame =
      frame run
        ~corrections:(case.Test_tree.xfail = None)
        ~path:case.Test_tree.path ~loc:case.Test_tree.loc
    in
    let start = Os.counter () in
    let outcome, prop_stats, corrected =
      run_attempt run frame case ~limit ~groups ~test_name
    in
    let duration = spent +. Os.count_s start in
    let failed = counts_failed ~xfail:case.Test_tree.xfail outcome in
    if failed && number < total_attempts then attempt (number + 1) duration
    else
      let outcome =
        (* An xfail test that passed is recorded as a failure with a
           self-describing message; an xfail test that failed keeps its real
           failures (and [failed] above already excused them). *)
        match (outcome, case.Test_tree.xfail) with
        | Failure.Pass, Some _ -> Failure.Fail [ xpass_failure case ]
        | outcome, _ -> outcome
      in
      let outcome = attach_tail (capture run) outcome in
      let result =
        {
          path = case.Test_tree.path;
          subject = Test;
          outcome;
          (* The three rendering facts computed here and nowhere else (the
             record is the contract): whether the result counted as failed,
             the expectation annotation, and the slow-tag decision bit. *)
          counted = failed;
          xfail = case.Test_tree.xfail;
          slow_tagged = Test_tree.Tag.mem Test_tree.Tag.slow case.Test_tree.tags;
          duration;
          attempts = number;
          prop_stats;
        }
      in
      record run result;
      on_event (Test_finished result);
      (result, failed, corrected)
  in
  attempt 1 0.

(* Selection *)

(* Frozen root for --shard bucketing: buckets must be a pure
   function of the test's path — never of the run's seed — so they are
   stable across runs, machines, and suite composition. Frozen with
   Seed.derive; changing either silently repartitions every sharded CI
   matrix. *)
let shard_root = 0x77696e6473687264L (* "windshrd" *)

let shard_bucket ~shards path =
  Int64.to_int
    (Int64.unsigned_rem
       (Seed.derive ~root:shard_root ~path ~index:0)
       (Int64.of_int shards))

let selection_predicate (config : config) =
  let require p tag = Test_tree.Tag.require tag p
  and drop p tag = Test_tree.Tag.drop tag p in
  let predicate = List.fold_left require Test_tree.Tag.any config.tags in
  List.fold_left drop predicate config.exclude_tags

let case_selected (config : config) ~predicate ~allowed ~focus_active
    (case : Test_tree.case) =
  let path = Test_tree.path_to_string case.Test_tree.path in
  let contains pattern = Text.contains_substring ~pattern path in
  (match config.filter with None -> true | Some p -> contains p)
  && (match config.exclude with None -> true | Some p -> not (contains p))
  && Test_tree.Tag.accepts predicate case.Test_tree.tags
  && allowed path
  && (match config.shard with
    | None -> true
    | Some (k, shards) -> shard_bucket ~shards path = k - 1)
  && ((not focus_active) || case.Test_tree.focused)

let duplicate_paths paths =
  let seen = Hashtbl.create 64 in
  List.filter
    (fun path ->
      let dup = Hashtbl.mem seen path in
      Hashtbl.replace seen path ();
      dup)
    paths
  |> List.sort_uniq String.compare

(* The last-failed store *)

(* The format is explicitly unstable: a magic first
   line, then one String.escaped test path per line. Unrecognized content
   reads as empty; I/O errors are swallowed — the store only feeds
   [--failed], it must never fail a run. *)
let store_magic = "windtrap-last-failed 1"

let store_path (config : config) ~suite =
  Filename.concat
    (Filename.concat config.log_dir (Os.sanitize_component suite))
    ".last-failed"

let read_store path =
  match open_in_bin path with
  | exception Sys_error _ -> []
  | ic ->
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
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
              lines [])

let write_store path entries =
  let buffer = Buffer.create 256 in
  Buffer.add_string buffer store_magic;
  Buffer.add_char buffer '\n';
  List.iter
    (fun entry ->
      Buffer.add_string buffer (String.escaped entry);
      Buffer.add_char buffer '\n')
    entries;
  match
    Os.mkdir_p (Filename.dirname path);
    Os.atomic_write ~path (Buffer.contents buffer)
  with
  | () -> ()
  | exception Sys_error _ -> ()
  | exception Unix.Unix_error _ -> ()

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

(* Startup

   Everything a run must clear before a single test executes. The order is
   contractual — duplicate paths, the CI focus guard, the baseline CI guard,
   the [--failed] store — because a suite that trips two of them must always
   be told about the same one. Between them the checks also decide the two
   values the rest of the run reads out of them: the baseline mode and the
   path allowlist ([None] when neither the caller nor [--failed] narrowed by
   path, and never [Some []] under [--failed] — an allowlist matching nothing
   is the refusal above it). *)

let ( let* ) = Result.bind

let startup (config : config) ~suite ~focus_sites ~allowlist tests paths =
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
  let* mode =
    (* In-place acceptance is a developer's edit; there is no override. *)
    match config.baseline with
    | Baseline.Update when in_ci -> Error Update_refused_in_ci
    | mode -> Ok mode
  in
  let* allowlist =
    (* The two narrowings intersect rather than override, which costs
       nothing: they never co-occur — a caller-supplied allowlist comes
       from [for_subset], which clears [failed_only]. *)
    if not config.failed_only then Ok allowlist
    else
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
  in
  Ok (mode, allowlist)

(* Plans

   The staged half of [execute]: everything a run decides before a single
   test runs — the process checks, the startup checks, the selection — as
   a value, so a caller that must separate deciding from running (the
   mutation loop's forked children) does it through the same code path
   [execute] composes. The clock starts here: a run's duration has always
   included its own startup. *)

type plan = {
  config : config;
  suite : string;
  selected : Test_tree.case list;
  total : int;
  focus_active : bool;
  mode : Baseline.mode;
  started : Os.counter;
}

let plan ?allowlist ~config ~suite tests : (plan, startup_error) Stdlib.result =
  install_exit_guard ();
  (* An unexpected exception's report is only as useful as its backtrace,
     and the runtime records one only when asked. Without this a test that
     raises names the constructor and the test's declaration line and
     nothing else — no raise site — unless the user knew to set
     OCAMLRUNPARAM=b, which nothing tells them. Left on: the run owns the
     process, and every raise site here already reads the raw backtrace. *)
  Printexc.record_backtrace true;
  if active () then invalid_arg active_run_error;
  (* The CLI layer validates every layer it resolves; only a hand-built
     configuration can carry a malformed shard, and it must fail loudly
     before selection divides by N. *)
  (match config.shard with
  | Some (k, n) when k < 1 || n < k ->
      invalid_arg "windtrap: shard must be K/N with 1 <= K <= N"
  | Some _ | None -> ());
  let started = Os.counter () in
  let cases = Test_tree.flatten tests in
  let total = List.length cases in
  let paths =
    List.map (fun case -> Test_tree.path_to_string case.Test_tree.path) cases
  in
  let focus_sites = Test_tree.focus_sites tests in
  let focus_active = focus_sites <> [] in
  let* mode, allowlist =
    startup config ~suite ~focus_sites ~allowlist tests paths
  in
  let predicate = selection_predicate config in
  (* Hashed once: an allowlist is as long as the selection it names, and
     the mutation loop's children name every path their parent ran. *)
  let allowed =
    match allowlist with
    | None -> fun _ -> true
    | Some entries ->
        let table = Hashtbl.create (List.length entries * 2) in
        List.iter (fun path -> Hashtbl.replace table path ()) entries;
        Hashtbl.mem table
  in
  let selected =
    List.filter (case_selected config ~predicate ~allowed ~focus_active) cases
  in
  Ok { config; suite; selected; total; focus_active; mode; started }

(* Outcomes *)

type outcome = {
  run : t;
  selected : Test_tree.case list;
  total : int;
  focus_active : bool;
  duration : float;
  exit_code : int;
}

let release ~on_event run =
  release_fixtures run ~announce:(fun name ->
      on_event (Fixture_release { name }))

(* An end-of-run verdict recorded as a result row (one result model): every
   sink projects the one recorded list, so a verdict that only rode the exit
   code would leave the run exiting 1 under a summary that says every test
   passed. Counted, unannotated, one attempt, no duration: renderers already
   classify a failing row from those bits. *)
let verdict_result ~subject ~path failures =
  {
    path;
    subject;
    outcome = Failure.Fail failures;
    counted = true;
    xfail = None;
    slow_tagged = false;
    duration = 0.;
    attempts = 1;
    prop_stats = None;
  }

let executed_test (result : result) = result.subject = Test

(* Runs the selected tests one at a time in declaration order, stopping at
   the first counted failure under [-x]. Returns whether it bailed, how many cases
   executed (what full-run detection counts — never result rows, which the
   verdict rows below would inflate), the paths that counted as failed
   (see [counts_failed]) in execution order: what [-x], the exit code,
   and the store react to, never a recorded outcome alone — expected [xfail]
   failures are recorded but never accumulate here — and, among those, the
   paths whose failures are all kept corrections. *)
let drive ~on_event run selected =
  let config = config run in
  let bailed = ref false in
  let executed = ref 0 in
  let rev_failed = ref [] in
  let rev_corrected = ref [] in
  (try
     List.iter
       (fun case ->
         if not !bailed then begin
           let _result, failed, corrected = run_case ~on_event run case in
           incr executed;
           let path = Test_tree.path_to_string case.Test_tree.path in
           if failed then begin
             rev_failed := path :: !rev_failed;
             if corrected then rev_corrected := path :: !rev_corrected
           end;
           if config.bail && !rev_failed <> [] then bailed := true
         end)
       selected
   with exn ->
     (* Release on every path where the runner regains control. Test-level
        exceptions were classified inside the boundary, so only a fatal
        exception or a raising [on_event] observer reaches here: release best
        effort — announcements swallowed too, a teardown must not be lost to
        an observer that keeps raising — then the exception wins. *)
     let backtrace = Printexc.get_raw_backtrace () in
     let announce name =
       try on_event (Fixture_release { name }) with _ -> ()
     in
     (try ignore (release_fixtures run ~announce) with _ -> ());
     Printexc.raise_with_backtrace exn backtrace);
  (!bailed, !executed, List.rev !rev_failed, List.rev !rev_corrected)

(* Rewrites the last-failed store at [path] with this run's failures. Entries
   for tests a partial run never reached survive; only a [full] run — one that
   executed the entire declared suite — drops entries whose paths no longer
   exist. *)
let update_last_failed path ~full ~results ~failed_paths =
  let survivors =
    if full then []
    else
      let executed =
        List.map (fun r -> Test_tree.path_to_string r.path) results
      in
      List.filter (fun entry -> not (List.mem entry executed)) (read_store path)
  in
  write_store path (failed_paths @ survivors)

let execute_plan ?(on_event = fun _ -> ())
    ({ config; suite; selected; total; focus_active; mode; started } : plan) :
    outcome =
  if active () then invalid_arg active_run_error;
  let baselines = Baseline.create ~mode () in
  let capture =
    if config.stream then Capture.disabled
    else Capture.create ~log_dir:config.log_dir ~suite ()
  in
  let run = create config ~capture ~baselines in
  (* The executing span: everything from the first event to the completed
     outcome runs with the slot marked, so the exit guard covers fixture
     release, observers, and store maintenance — not only test attempts. On
     the fatal path the protect empties the slot before the exception leaves
     [execute], so the guard is inert during fatal termination. *)
  with_active run @@ fun () ->
  on_event (Run_started { suite; total; selected = List.length selected });
  let bailed, executed, failed_paths, corrected_paths =
    drive ~on_event run selected
  in
  (* Releases run after the last test, outside any per-test timeout,
     including under -x. A failure here is part of the run's verdict,
     so it is recorded the moment it happens: one row per failure, after
     every test row. *)
  let release_failures = release ~on_event run in
  List.iter
    (fun failure ->
      record run
        (verdict_result ~subject:Fixture_release ~path:fixture_release_path
           [ failure ]))
    release_failures;
  (* Store maintenance ranges over executed tests: a verdict row is not a
     test — counting one as executed would corrupt the last-failed store. *)
  let test_results = List.filter executed_test (results run) in
  (* A full run executed the entire declared suite: only such a run may drop
     store entries for tests that no longer exist. *)
  let full = (not bailed) && executed = total in
  update_last_failed (store_path config ~suite) ~full ~results:test_results
    ~failed_paths;
  (* Corrections are written once, after the last test and before the
     report; a correction that reached nothing fails the run. *)
  Baseline.write baselines;
  (* A test whose failures are all kept corrections leaves the exit code
     alone: under --corrected the [diff?] that follows is the verdict. In
     every other mode no correction is kept beside a raised failure, so the
     list is empty and every failure counts. *)
  let uncorrected =
    List.filter (fun path -> not (List.mem path corrected_paths)) failed_paths
  in
  let exit_code =
    if
      uncorrected <> [] || release_failures <> []
      || Baseline.refusals baselines <> []
    then 1
    else if executed = 0 then 2
    else 0
  in
  {
    run;
    selected;
    total;
    focus_active;
    duration = Os.count_s started;
    exit_code;
  }

let execute ?on_event ?allowlist config ~suite tests =
  Result.map (execute_plan ?on_event) (plan ?allowlist ~config ~suite tests)

(* [--list]: the deciding half alone. A listing is not a run — nothing
   executes, so there is no capture, no store rewrite and no report — and
   the caller that asked for it prints it. *)
let list_selection config ~suite tests =
  Result.map
    (fun (p : plan) ->
      List.map
        (fun (case : Test_tree.case) ->
          Test_tree.path_to_string case.Test_tree.path)
        p.selected)
    (plan ~config ~suite tests)
