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

type config = {
  seed : Seed.seed;
  filter : string option;
  exclude : string option;
  tags : string list;
  exclude_tags : string list;
  shard : (int * int) option;
  quick : bool;
  failed_only : bool;
  list_only : bool;
  bail : int option;
  stream : bool;
  update : Env.update;
  prune : bool;
  strict_snapshots : bool;
  timeout : float option;
  prop_count : int option;
  max_shrink : int option;
  junit : string option;
  log_dir : string;
  allow_focus : bool;
}

let default_config () =
  {
    seed = Seed.random ();
    filter = None;
    exclude = None;
    tags = [];
    exclude_tags = [];
    shard = None;
    quick = false;
    failed_only = false;
    list_only = false;
    bail = None;
    stream = false;
    update = Env.No_update;
    prune = false;
    strict_snapshots = false;
    timeout = None;
    prop_count = None;
    max_shrink = None;
    junit = None;
    log_dir = Path_ops.default_log_dir ();
    allow_focus = false;
  }

(* The configuration for a run over a SUBTREE of another run's selection,
   used by the mutation loop's forked children. Which knobs to clear and
   which to keep is a statement about this record, so it lives here,
   where whoever adds a fourteenth selection knob is already editing.

   The knobs that select by PATH are cleared — filter, exclude, shard, the
   [--failed] allowlist — because the tree the caller hands its child IS
   that selection: it was pruned to the paths the parent executed, so
   applying any of them again could only narrow it further.

   The knobs that select by TAG are kept verbatim. A test's tags are not
   in its path, so pruning cannot express them, and keeping them is what
   makes the child's selection the parent's by construction rather than
   by coincidence of the pruned tree. The root seed is kept for the same
   family of reasons: per-case seeds derive from (root, path, index), so a
   child running 24 of 900 tests sees the same property cases the parent
   saw.

   [update = No_update] makes snapshot checking read-only by construction
   — Snapshot maps it to Mode Check and the write is reachable only under
   Mode Update — and the log directory is the child's own so that its
   capture files and its last-failed store cannot touch the parent's. *)
let for_subset config ~log_dir ~bail =
  {
    config with
    filter = None;
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

type subject = Test | Fixture_release | Stale_baselines

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

type summary = { visited : int; total : int; siblings : bool }

type t = {
  config : config;
  capture : Capture.t;
  snapshots : Snapshot.t;
  fixtures : (int, fixture_entry) Hashtbl.t;
  mutable acquired : int list; (* fixture ids, most recently acquired first *)
  mutable temp_seq : int; (* next scratch-directory number *)
  mutable rev_results : result list;
  mutable coverage : summary option;
}

let create config ~capture ~snapshots =
  {
    config;
    capture;
    snapshots;
    fixtures = Hashtbl.create 8;
    acquired = [];
    temp_seq = 0;
    rev_results = [];
    coverage = None;
  }

let config t = t.config
let capture t = t.capture
let snapshots t = t.snapshots

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
  mutable fr_prop : Property.context option;
  mutable fr_rev_failures : Failure.t list;
  mutable fr_subtests : string list; (* enclosing subtests, innermost first *)
  mutable fr_temp_root : string option; (* the attempt's scratch dir *)
  mutable fr_temp_seq : int; (* next path number within the scratch dir *)
  mutable fr_env : env_restore list; (* one entry per name, first set wins *)
  mutable fr_cwd : (string * Loc.t option) option; (* dir at the first chdir *)
}

let frame t ~path ~loc =
  {
    owner = t;
    fr_path = path;
    fr_loc = loc;
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

let add_failure frame failure =
  (* The one fallback point of the attribution ladder: a failure recorded
     without a location — its failing call sat in tail position, so
     Loc.capture stopped at the runner's delimiter — is attributed to the
     test's declaration. Only the top-level failure is filled; nested
     failures (a property failure's [inner]) are left untouched, and a
     failure needing no fill is stored as given. *)
  let failure =
    match (failure.Failure.loc, frame.fr_loc) with
    | Some _, _ | None, None -> failure
    | None, (Some _ as loc) -> { failure with Failure.loc }
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

let outside_run_error =
  "windtrap: no test is running. Assertions, [output ()], [snapshot], \
   [collect], [setenv], [chdir] and fixture accessors work only inside a \
   test body executed by [run] — not at module toplevel, and not after the \
   run."

let current_frame () =
  match !slot with
  | Some (In_test frame) -> frame
  | Some (In_run _) | None -> invalid_arg outside_run_error

let current () = (current_frame ()).owner

let current_opt () =
  match !slot with
  | Some (In_test frame) -> Some frame
  | Some (In_run _) | None -> None

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
         record it labeled, with its backtrace, and let siblings run. *)
      let backtrace = Printexc.get_raw_backtrace () in
      let failure =
        Failure.raised ~actual:(Printexc.to_string exn)
          ~backtrace:(Failure.backtrace_to_string backtrace)
          ()
      in
      add_failure frame (relabel frame failure);
      pop ()

(* Runner-owned scratch *)

let temp_create_attempts = 64

(* The attempt's scratch directory, created lazily. Names are unique within
   the process ([temp_seq] never repeats in a run) and carry the pid against
   concurrent runners; EEXIST from a stale directory retries with the next
   number. *)
let temp_root frame =
  match frame.fr_temp_root with
  | Some dir -> dir
  | None ->
      let base = Filename.get_temp_dir_name () in
      let pid = Unix.getpid () in
      let rec create attempts =
        let n = frame.owner.temp_seq in
        frame.owner.temp_seq <- n + 1;
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
  let name = Path_ops.sanitize_component prefix ^ "-" ^ string_of_int n in
  let dir = Filename.concat root name in
  Unix.mkdir dir 0o700;
  dir

let temp_file ?(suffix = "") () =
  let frame = current_frame () in
  let root = temp_root frame in
  let n = frame.fr_temp_seq in
  frame.fr_temp_seq <- n + 1;
  let suffix = if suffix = "" then "" else Path_ops.sanitize_component suffix in
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

(* Runner-restored process state

   The environment and the working directory belong to the process, not to
   the test: nothing scopes them but putting them back. So the body records
   what it changed and the runner undoes it at the attempt boundary — the
   same bargain the scratch paths make, holding on every outcome for the
   same reason, that the runner regains control on every outcome. *)

let setenv name value =
  let frame = current_frame () in
  (* The prior binding is read before [Env.set] changes it, but recorded
     only after [Env.set] returns: [Env.set] validates the name before it
     touches the process, and a record made before that validation would be
     replayed at [reclaim] — where the same rejection reads as a
     restoration failure about a change that never happened. *)
  let prior = Sys.getenv_opt name in
  Env.set name value;
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
   that made the change, since the boundary is nobody's code. *)
let restore_failure frame ?loc text =
  add_failure frame
    (Failure.with_phase Failure.Teardown (Failure.message ?loc text))

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
      match Env.set entry.er_name entry.er_prior with
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

(* Coverage seam *)

let set_coverage t summary = t.coverage <- Some summary
let coverage t = t.coverage
