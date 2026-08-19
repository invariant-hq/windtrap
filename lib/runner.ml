(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC

   The sequential drive loop, SIGALRM timeout, retry loop, and last-failed
   persistence derive from windtrap v1's lib/runner.ml, rebuilt over the
   per-run record (Run), data-form brackets (Test_tree), and the typed
   failure model, with the boundary capturing body and teardown outcomes
   independently.
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

(* The exit guard. Registered once per process (registration state, like
   Run's fixture ids, is process identity, not run state);
   per-run data flows through the ambient slot. Stdlib.at_exit runs each
   registered function at most once, so an interception consumes the
   registration: the guard re-arms itself before raising. Relies on
   Stdlib.exit = do_at_exit (); sys_exit — an exception from an at_exit
   function propagates to exit's caller (pinned by the child-status
   regression test).

   It also belongs to the process that armed it. A forked child inherits
   Run.active and the at_exit registration, so without the owning pid the
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
  if owns_run () && Run.active () then begin
    at_exit exit_guard;
    raise Failure.Exit_attempt
  end

let exit_guard_installed = ref false

let install_exit_guard () =
  exit_guard_owner := Some (Unix.getpid ());
  if not !exit_guard_installed then begin
    exit_guard_installed := true;
    at_exit exit_guard
  end

(* Property tests *)

(* The channel between a [prop] body and the classifier below: the body
   always raises its engine outcome, the Body-phase guard of the same test
   consumes it. Never installed in run state and never visible
   to user code — the raise happens after the user's law returned. *)
exception Prop_outcome of Property.outcome

let prop ?pos ?tags ?timeout ?count ?max_discard ?examples name gen law =
  let loc = Loc.resolve ?pos () in
  let body () =
    let frame = Run.current_frame () in
    let config = Run.config (Run.run_of_frame frame) in
    (* Case count: declaration site > --prop-count > engine default. The
       engine is told which, not just how many: it stamps a config-sourced
       count on failure payloads so the replay hint can restate the flag,
       while a declaration-site count replays by itself. *)
    let count =
      match count with
      | Some n -> Some (`Declared n)
      | None -> Option.map (fun n -> `Config n) config.Run.prop_count
    in
    let path = Test_tree.path_to_string (Run.path frame) in
    let outcome =
      Property.run ?loc ?count ?max_shrink:config.Run.max_shrink ?max_discard
        ?examples
        ~root:config.Run.seed ~path gen (fun context value ->
          Run.with_prop_context frame context (fun () -> law value))
    in
    raise (Prop_outcome outcome)
  in
  Test_tree.test ?pos ?tags ?timeout name body

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
      let started = Clock.counter () in
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
          let remaining = limit -. Clock.count_s started in
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

(* Whether a raw attempt outcome counts as failed for retries, --bail, the
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
    Run.add_failure frame (Failure.with_phase ph failure)
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
     so the runner cannot bracket the three the way it brackets [Bracket] —
     it can only run the body inside the callback and attribute whatever
     escapes.

     The body's failure is recorded where it happens and then re-raised
     through [scope]: a scope that cancels or cleans up on the exception
     path still sees it, and a scope that swallows it cannot turn a failed
     test green. Anything else escaping is the scope's own, attributed by
     how far the callback got — [Setup] before it, [Teardown] after it
     returned.

     Calling back exactly once is the contract. Zero calls means the body
     never ran, which must not report as a pass; a second call is refused
     rather than served, because one execution is the unit everything else
     is keyed by (snapshot registration, subtest labels, scratch paths). *)
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
    | Test_tree.Bracket { setup; body; teardown } -> (
        match guard Failure.Setup setup with
        | None -> () (* teardown runs iff setup succeeded *)
        | Some resource ->
            (* Body and teardown outcomes are captured independently — one
               failure entry per phase, neither masking the other; teardown
               runs on every body outcome, skip and timeout included. Never
               Fun.protect at this boundary. *)
            ignore (guard Failure.Body (fun () -> body resource));
            (* The body may have consumed the window — re-arm, or a blocking
               teardown after a body timeout would run unbounded. *)
            renew ();
            ignore (guard Failure.Teardown (fun () -> teardown resource)))
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
     Run.with_frame frame (fun () ->
         match
           Capture.with_capture (Run.capture run) ~groups ~test_name boundary
         with
         | () -> ()
         | exception exn when not (Failure.is_fatal exn) ->
             (* Capture setup or restore failed (e.g. the log file could not
                be created): a failure of this test, not of the run. *)
             let backtrace = Printexc.get_raw_backtrace () in
             classify Failure.Body exn backtrace)
   with
  | () -> Run.reclaim frame
  | exception exn ->
      let backtrace = Printexc.get_raw_backtrace () in
      Run.reclaim frame;
      Printexc.raise_with_backtrace exn backtrace);
  let outcome =
    match Run.failures frame with
    | [] -> (
        match !skipped with
        | Some reason -> Failure.Skip reason
        | None -> Failure.Pass)
    | failures -> Failure.Fail failures
  in
  (outcome, !prop_stats)

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
  | Test_finished of Run.result
  | Fixture_release of { name : string }

(* Runs one test to completion (retries included), records its result, and
   returns it with whether it counted as failed (see [counts_failed]) — the
   caller drives --bail, the exit code, and the last-failed store from the
   flag, never from the recorded outcome alone. *)
let run_case ~on_event run (case : Test_tree.case) =
  on_event (Test_started { path = case.Test_tree.path });
  let config = Run.config run in
  let limit =
    match case.Test_tree.timeout with
    | Some _ as declared -> declared
    | None -> config.Run.timeout
  in
  let groups, test_name = split_last case.Test_tree.path in
  let total_attempts = case.Test_tree.retries + 1 in
  let rec attempt number spent =
    let frame =
      Run.frame run ~path:case.Test_tree.path
        ~loc:case.Test_tree.loc
    in
    let start = Clock.counter () in
    let outcome, prop_stats =
      run_attempt run frame case ~limit ~groups ~test_name
    in
    let duration = spent +. Clock.count_s start in
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
      let outcome = attach_tail (Run.capture run) outcome in
      let result =
        {
          Run.path = case.Test_tree.path;
          subject = Run.Test;
          outcome;
          (* The three rendering facts computed here and nowhere else (the
             record is the contract): whether the result counted as failed,
             the expectation annotation, and the slow-tag decision bit. *)
          counted = failed;
          xfail = case.Test_tree.xfail;
          slow_tagged = Tag.mem Tag.slow case.Test_tree.tags;
          duration;
          attempts = number;
          prop_stats;
        }
      in
      Run.record run result;
      on_event (Test_finished result);
      (result, failed)
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

let selection_predicate (config : Run.config) =
  let require p tag = Tag.require tag p and drop p tag = Tag.drop tag p in
  let predicate = List.fold_left require Tag.any config.Run.tags in
  List.fold_left drop predicate config.Run.exclude_tags

let case_selected (config : Run.config) ~predicate ~allowed ~focus_active
    (case : Test_tree.case) =
  let path = Test_tree.path_to_string case.Test_tree.path in
  let contains pattern = Text.contains_substring ~pattern path in
  (match config.Run.filter with None -> true | Some p -> contains p)
  && (match config.Run.exclude with None -> true | Some p -> not (contains p))
  && Tag.accepts predicate case.Test_tree.tags
  && allowed path
  && (match config.Run.shard with
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

let store_path (config : Run.config) ~suite =
  Filename.concat
    (Filename.concat config.Run.log_dir (Path_ops.sanitize_component suite))
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
    Path_ops.mkdir_p (Filename.dirname path);
    Atomic_file.write ~path (Buffer.contents buffer)
  with
  | () -> ()
  | exception Sys_error _ -> ()
  | exception Unix.Unix_error _ -> ()

(* Startup errors *)

type startup_error =
  | Duplicate_paths of string list
  | Focused_in_ci of ([ `Ftest | `Fgroup ] * Loc.t option) list
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
      let site (kind, loc) =
        let name = match kind with `Ftest -> "ftest" | `Fgroup -> "fgroup" in
        match loc with
        | Some loc -> Pp.str "%s at %s" name (Loc.to_string loc)
        | None -> name
      in
      Pp.str "focused tests committed (%s); remove ftest/fgroup to run under CI"
        (String.concat ", " (List.map site sites))
  | Update_refused_in_ci ->
      "snapshot update refused: CI is set. Set WINDTRAP_UPDATE=force to update \
       baselines on a CI machine."
  | No_recorded_failures -> "no recorded failures match the current suite"

(* Startup

   Everything a run must clear before a single test executes. The order is
   contractual — duplicate paths, the CI focus guard, the snapshot CI guard,
   the [--failed] store — because a suite that trips two of them must always
   be told about the same one. Between them the checks also decide the two
   values the rest of the run reads out of them: the snapshot mode and the
   path allowlist ([None] when neither the caller nor [--failed] narrowed by
   path, and never [Some []] under [--failed] — an allowlist matching nothing
   is the refusal above it). *)

let ( let* ) = Result.bind

let startup (config : Run.config) ~suite ~focus_active ~allowlist tests paths =
  let in_ci = Env.in_ci () in
  let* () =
    match duplicate_paths paths with
    | [] -> Ok ()
    | duplicates -> Error (Duplicate_paths duplicates)
  in
  let* () =
    if focus_active && in_ci && not config.Run.allow_focus then
      Error (Focused_in_ci (Test_tree.focus_sites tests))
    else Ok ()
  in
  let* mode =
    match Snapshot.resolve_mode ~ci:in_ci config.Run.update with
    | Snapshot.Refused_in_ci -> Error Update_refused_in_ci
    | Snapshot.Mode mode -> Ok mode
  in
  let* allowlist =
    (* The two narrowings intersect rather than override, which costs
       nothing: they never co-occur — a caller-supplied allowlist comes
       from [Run.for_subset], which clears [failed_only]. *)
    if not config.Run.failed_only then Ok allowlist
    else
      let asked path =
        match allowlist with None -> true | Some entries -> List.mem path entries
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
  config : Run.config;
  suite : string;
  selected : Test_tree.case list;
  total : int;
  focus_active : bool;
  focused : int; (* focused cases in the declared suite, before selection *)
  mode : Snapshot.mode;
  started : Clock.counter;
}

let plan ?allowlist ~config ~suite tests : (plan, startup_error) result =
  install_exit_guard ();
  (* An unexpected exception's report is only as useful as its backtrace,
     and the runtime records one only when asked. Without this a test that
     raises names the constructor and the test's declaration line and
     nothing else — no raise site — unless the user knew to set
     OCAMLRUNPARAM=b, which nothing tells them. Left on: the run owns the
     process, and every raise site here already reads the raw backtrace. *)
  Printexc.record_backtrace true;
  if Run.active () then
    invalid_arg
      "windtrap: run is already active — a test body cannot start another run";
  (* The CLI layer validates every layer it resolves; only a hand-built
     configuration can carry a malformed shard, and it must fail loudly
     before selection divides by N. *)
  (match config.Run.shard with
  | Some (k, n) when k < 1 || n < k ->
      invalid_arg "windtrap: shard must be K/N with 1 <= K <= N"
  | Some _ | None -> ());
  let started = Clock.counter () in
  let cases = Test_tree.flatten tests in
  let total = List.length cases in
  let paths =
    List.map (fun case -> Test_tree.path_to_string case.Test_tree.path) cases
  in
  let focus_active = Test_tree.has_focus tests in
  let* mode, allowlist =
    startup config ~suite ~focus_active ~allowlist tests paths
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
  let focused =
    List.length (List.filter (fun case -> case.Test_tree.focused) cases)
  in
  Ok { config; suite; selected; total; focus_active; focused; mode; started }

(* Outcomes *)

type outcome = {
  run : Run.t;
  selected : Test_tree.case list;
  total : int;
  focus_active : bool;
  bailed : bool;
  failed_paths : string list;
  orphans : string list;
  duration : float;
  exit_code : int;
}

let release ~on_event run =
  Run.release_fixtures run ~announce:(fun name ->
      on_event (Fixture_release { name }))

(* An end-of-run verdict recorded as a result row (one result model): every
   sink projects the one recorded list, so a verdict that only rode the exit
   code would leave the run exiting 1 under a summary that says every test
   passed. Counted, unannotated, one attempt, no duration: renderers already
   classify a failing row from those bits. *)
let verdict_result ~subject ~path failures =
  {
    Run.path;
    subject;
    outcome = Failure.Fail failures;
    counted = true;
    xfail = None;
    slow_tagged = false;
    duration = 0.;
    attempts = 1;
    prop_stats = None;
  }

let executed_test (result : Run.result) = result.Run.subject = Run.Test

(* Runs the selected tests one at a time in declaration order, stopping once
   [--bail]'s budget is spent. Returns whether it bailed, how many cases
   executed (what full-run detection counts — never result rows, which the
   verdict rows below would inflate), and the paths that counted as failed
   (see [counts_failed]) in execution order: what [--bail], the exit code,
   and the store react to, never a recorded outcome alone — expected [xfail]
   failures are recorded but never accumulate here. *)
let drive ~on_event run selected =
  let config = Run.config run in
  let bailed = ref false in
  let executed = ref 0 in
  let rev_failed = ref [] in
  (try
     List.iter
       (fun case ->
         if not !bailed then begin
           let _result, failed = run_case ~on_event run case in
           incr executed;
           if failed then
             rev_failed :=
               Test_tree.path_to_string case.Test_tree.path :: !rev_failed;
           match config.Run.bail with
           | Some limit when List.length !rev_failed >= limit -> bailed := true
           | Some _ | None -> ()
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
     (try ignore (Run.release_fixtures run ~announce) with _ -> ());
     Printexc.raise_with_backtrace exn backtrace);
  (!bailed, !executed, List.rev !rev_failed)

(* Rewrites the last-failed store at [path] with this run's failures. Entries
   for tests a partial run never reached survive; only a [full] run — one that
   executed the entire declared suite — drops entries whose paths no longer
   exist. *)
let update_last_failed path ~full ~results ~failed_paths =
  let survivors =
    if full then []
    else
      let executed =
        List.map (fun r -> Test_tree.path_to_string r.Run.path) results
      in
      List.filter (fun entry -> not (List.mem entry executed)) (read_store path)
  in
  write_store path (failed_paths @ survivors)

(* Stale-baseline reporting, gated on how much of the suite really ran:
   only a run that executed all of it knows the full set of names the
   suite claims. Every recorded [Fail] blocks the report, expected or not:
   an [xfail] body did not complete, so its snapshots may be stale.
   Reporting never deletes — a baseline is a committed file. *)
let stale_baselines snapshots ~full ~results ~focused_count =
  let count_outcomes accepts =
    List.length (List.filter (fun r -> accepts r.Run.outcome) results)
  in
  let failed =
    count_outcomes (function
      | Failure.Fail _ -> true
      | Failure.Pass | Failure.Skip _ -> false)
  in
  let skipped =
    count_outcomes (function
      | Failure.Skip _ -> true
      | Failure.Pass | Failure.Fail _ -> false)
  in
  let clean = full && skipped = 0 && failed = 0 && focused_count = 0 in
  if clean then Snapshot.orphans snapshots else []

let execute_plan ?(on_event = fun _ -> ())
    ({ config; suite; selected; total; focus_active; focused; mode; started } :
      plan) : outcome =
  if Run.active () then
    invalid_arg
      "windtrap: run is already active — a test body cannot start another run";
  let snapshots = Snapshot.create ~mode () in
  if config.Run.list_only then
    {
      run = Run.create config ~capture:Capture.disabled ~snapshots;
      selected;
      total;
      focus_active;
      bailed = false;
      failed_paths = [];
      orphans = [];
      duration = Clock.count_s started;
      exit_code = 0;
    }
  else
    let capture =
      if config.Run.stream then Capture.disabled
      else Capture.create ~log_dir:config.Run.log_dir ~suite ()
    in
    let run = Run.create config ~capture ~snapshots in
    (* The executing span: everything from the first event to the completed
       outcome runs with the slot marked, so the exit guard covers fixture
       release, observers, and store maintenance — not only test attempts. On
       the fatal path the protect empties the slot before the exception leaves
       [execute], so the guard is inert during fatal termination. *)
    Run.with_active run @@ fun () ->
    on_event (Run_started { suite; total; selected = List.length selected });
    let bailed, executed, failed_paths = drive ~on_event run selected in
    (* Releases run after the last test, outside any per-test timeout,
       including under --bail. A failure here is part of the run's verdict,
       so it is recorded the moment it happens: one row per failure, after
       every test row. *)
    let release_failures = release ~on_event run in
    List.iter
      (fun failure ->
        Run.record run
          (verdict_result ~subject:Run.Fixture_release
             ~path:Run.fixture_release_path [ failure ]))
      release_failures;
    (* Store and snapshot maintenance range over executed tests: a verdict
       row is not a test — counting one as skipped, failed, or executed
       would silently disable orphan reporting and corrupt the last-failed
       store. *)
    let test_results = List.filter executed_test (Run.results run) in
    (* A full run executed the entire declared suite: only such a run may drop
       store entries for tests that no longer exist, or report orphans. *)
    let full = (not bailed) && executed = total in
    update_last_failed (store_path config ~suite) ~full ~results:test_results
      ~failed_paths;
    let orphans =
      stale_baselines snapshots ~full ~results:test_results
        ~focused_count:focused
    in
    let exit_code =
      if failed_paths <> [] || release_failures <> [] then 1
      else if executed = 0 then 2
      else 0
    in
    {
      run;
      selected;
      total;
      focus_active;
      bailed;
      failed_paths;
      orphans;
      duration = Clock.count_s started;
      exit_code;
    }

let execute ?on_event ?allowlist ~config ~suite tests =
  Result.map (execute_plan ?on_event) (plan ?allowlist ~config ~suite tests)
