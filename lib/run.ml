(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ───── Configuration ───── *)

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
  timeout : float option;
  slow_threshold : float;
  prop_count : int option;
  junit : string option;
  color : Env.color_mode;
  columns : int option;
  tail_errors : int option;
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
    timeout = None;
    slow_threshold = 1.0;
    prop_count = None;
    junit = None;
    color = Env.Auto;
    columns = None;
    tail_errors = None;
    log_dir = Path_ops.default_log_dir ();
    allow_focus = false;
  }

(* ───── Run records ───── *)

(* A fixture's cache entry. [fx_state] embeds the acquired value in the
   accessor's private exception constructor ([Acquired]), records a skip
   raised during acquisition (amendment C1: cached as a skip, not an error),
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

type result = {
  path : string list;
  outcome : Failure.outcome;
  counted : bool;
  xfail : Test_tree.xfail option;
  slow_tagged : bool;
  duration : float;
  attempts : int;
  prop_stats : Property.stats option;
  srandom_root : Seed.seed option;
}

type summary = { visited : int; total : int; siblings : bool }

type t = {
  config : config;
  capture : Capture.t;
  snapshots : Snapshot.t;
  fixtures : (int, fixture_entry) Hashtbl.t;
  mutable acquired : int list; (* fixture ids, most recently acquired first *)
  mutable temp_seq : int; (* next scratch-directory number (B9) *)
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

(* ───── Per-test frames ───── *)

type frame = {
  owner : t;
  fr_path : string list;
  fr_file : string option;
  fr_loc : Loc.t option; (* declaration site: the location fallback (D4) *)
  mutable fr_prop : Property.context option;
  mutable fr_rev_failures : Failure.t list;
  mutable fr_subtests : string list; (* enclosing subtests, innermost first *)
  mutable fr_temp_root : string option; (* the attempt's scratch dir (B9) *)
  mutable fr_temp_seq : int; (* next path number within the scratch dir *)
  mutable fr_srandom : bool; (* the attempt called [srandom] (D5 §6) *)
}

let frame t ~path ~file ~loc =
  {
    owner = t;
    fr_path = path;
    fr_file = file;
    fr_loc = loc;
    fr_prop = None;
    fr_rev_failures = [];
    fr_subtests = [];
    fr_temp_root = None;
    fr_temp_seq = 0;
    fr_srandom = false;
  }

let run_of_frame frame = frame.owner
let path frame = frame.fr_path
let file frame = frame.fr_file
let loc frame = frame.fr_loc
let srandom_used frame = frame.fr_srandom

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

(* ───── The ambient slot ───── *)

(* The one ambient slot (RFC Law 9): the only run-state [ref] in the
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
   [collect] and fixture accessors work only inside a test body executed by \
   [run] — not at module toplevel, and not after the run."

let current_frame () =
  match !slot with
  | Some (In_test frame) -> frame
  | Some (In_run _) | None -> invalid_arg outside_run_error

let current () = (current_frame ()).owner

let current_opt () =
  match !slot with
  | Some (In_test frame) -> Some frame
  | Some (In_run _) | None -> None

(* ───── Test-body operations ───── *)

let current_test () = (current_frame ()).fr_path

let srandom () =
  let frame = current_frame () in
  (* Record the draw: the runner copies the flag into the result as
     [srandom_root], and a failing test's block prints the replay line from
     it — the root token is in the log exactly when a stochastic failure
     needs replaying (D5 §6). *)
  frame.fr_srandom <- true;
  let root = frame.owner.config.seed in
  let path = Test_tree.path_to_string frame.fr_path in
  (* The frozen derivation (RFC Law 7): a pure function of the printed root
     token and the test's path — index 0, no per-call stream. *)
  let seed = Seed.derive ~root ~path ~index:0 in
  Random.State.make
    [| Int64.to_int seed; Int64.to_int (Int64.shift_right_logical seed 32) |]

(* The failure label of the executing subtest: the test's own name, then the
   enclosing subtest names outermost first, joined like a test path. *)
let subtest_label frame =
  let stack = List.rev frame.fr_subtests in
  let components =
    match List.rev frame.fr_path with
    | leaf :: _ -> leaf :: stack
    | [] -> stack (* hand-built frames only; case paths are never empty *)
  in
  Test_tree.path_to_string components

(* The subtest label rides in the failure's [msg] slot — the one designed
   context slot renderers already show — prefixed to a user [~msg]. *)
let relabel frame (failure : Failure.t) =
  let label = subtest_label frame in
  let msg =
    match failure.Failure.msg with
    | None -> label
    | Some msg -> label ^ ": " ^ msg
  in
  { failure with Failure.msg = Some msg }

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
      (* Record and return: siblings continue (amendment B13). The label is
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
          ~backtrace:(Printexc.raw_backtrace_to_string backtrace)
          ()
      in
      add_failure frame (relabel frame failure);
      pop ()

(* ───── Runner-owned scratch (amendment B9) ───── *)

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
   must not fail a test or mask its outcome (RFC Law 8 wording: released on
   every path where the runner regains control). *)
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

(* ───── Fixtures ───── *)

(* Accessor identity. Not run state: ids mint process-wide identities for
   fixture accessors and never reset — the per-run cache in [t] is keyed by
   them, which is what makes a later run re-acquire (RFC "Resources"). *)
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
        (* Amendment C1: every later use skips with the cached reason. *)
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
            (* Amendment C1: a skip during acquisition is cached as a skip,
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
  (* Release order is contract — reverse acquisition (RFC "Resources") —
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
               must not walk past the runner into its caller (D4). *)
            match Loc.delimit release with
            | () -> release_all acc ids
            | exception exn when not (Failure.is_fatal exn) ->
                release_all (release_failure entry exn :: acc) ids))
  in
  release_all [] ids

(* ───── Results ───── *)

let record t result = t.rev_results <- result :: t.rev_results
let results t = List.rev t.rev_results

(* ───── Coverage seam (RFC Law 12) ───── *)

let set_coverage t summary = t.coverage <- Some summary
let coverage t = t.coverage
