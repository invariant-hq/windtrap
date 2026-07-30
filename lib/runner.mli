(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Sequential test execution: selection, the per-test boundary, retries,
    fixtures, the last-failed store, and exit codes.

    {!execute} drives a declared {!Test_tree.t} list under a resolved
    {!Run.config}: it applies the startup checks (duplicate paths, the CI focus
    guard, the snapshot CI guard, the [--failed] store), selects tests, runs
    them one at a time in declaration order (RFC "The runner": one process, one
    domain, sequential), releases fixtures, maintains the last-failed store, and
    computes the Law 11 exit code. It prints nothing: progress streams through
    typed {!type:event}s and everything else is data in the returned
    {!type:outcome} for renderers to project (RFC Law 4).

    {b The per-test boundary.} Every attempt gets a fresh {!Run.frame} installed
    in the ambient slot, the global [Random] state reseeded to a pure function
    of the test's path (saved and restored around the attempt — test order
    cannot perturb a test that consults [Random]), capture redirected into the
    test's log file ({!Capture.with_capture}, per-attempt truncation; a no-op
    under [--stream]), and a SIGALRM timeout arming the test's limit (else
    [config.timeout]). The timeout window covers setup, body, and teardown
    together; it is Unix-only (a documented no-op on Windows) and cannot
    interrupt blocked C calls. The runner owns [SIGALRM] while a test with a
    limit runs. After every attempt — outside the timeout window, on every path
    where the runner regains control, a fatal exception included — the attempt's
    scratch paths are removed ({!Run.remove_temp}, RFC Law 8).

    Outcomes are classified per phase: {!Failure.Check_failure} keeps its
    payload, {!Failure.Skip_test} skips the test, {!Failure.Timeout} becomes a
    failure of the phase it interrupted, [Sys.Break]/[Out_of_memory]/
    [Stack_overflow] re-raise after a best-effort fixture release, and any other
    exception becomes a {!Failure.Raise} failure carrying its backtrace.

    A user callback that calls [exit] does not terminate the process: the first
    {!execute} in a process registers a [Stdlib.at_exit] guard which, whenever
    an exit is attempted while a run is active ({!Run.active}), re-arms itself
    and raises {!Failure.Exit_attempt} — cancelling the exit and surfacing it at
    the boundary of the phase that attempted it, where it is classified as a
    [Message] failure of that phase
    (["the test called exit — intercepted; a test must return or raise, never
      exit the process"]). An exit attempted during fixture release becomes a
    {!Failure.Release} failure like any raising teardown; one attempted from an
    [on_event] observer aborts the run like any raising observer. The guard is
    inert while no run is active: exits before, after, and by the runner itself
    pass through untouched.

    A {!Test_tree.bracket}'s stored closures run as data — setup, then body,
    then teardown iff setup succeeded, teardown on every body outcome including
    skip and timeout — with the body and teardown outcomes captured
    {e independently}, one failure-list entry per failed phase, never composed
    with [Fun.protect] at the reporting boundary (RFC "Resources"). A timeout
    during teardown is a [Teardown]-phase failure alongside any body failure.

    {b Retries.} A test with [retries = n] reruns while its outcome
    {e counts as failed} (see {e Expected failures} below — for an [xfail] test
    that is an unexpected pass), up to [n + 1] attempts, each attempt a fresh
    frame and a truncated capture file. The recorded result carries the
    {e final} attempt's failures and output tail and the attempt count —
    renderers mark ["attempt N of M"] and pass-after-retries from
    {!Run.result.attempts}. Skips are never retried; the captured-output tail of
    a failed test is attached to its first failure entry (RFC Law 5).

    {b Expected failures} (amendment B12). A test marked {!Test_tree.xfail}
    still runs, but what counts as failed inverts: a failing outcome is recorded
    with its real failures yet counts as {e expected} — it does not consume the
    [--bail] budget, enter the last-failed store, or turn the exit code [1] —
    while a passing outcome counts as failed and is recorded as [Fail] with one
    message failure (["expected to fail, but the test passed"], naming the
    [?reason]). Skips are unaffected. {!outcome} [.failed_paths] is the
    authoritative projection of what counted: renderers distinguish an expected
    failure (result failed, path absent) from an unexpected pass (path present)
    with it and the flattened case's {!Test_tree.case.xfail}. For
    snapshot-orphan reporting and [--prune] gating, {e every} [Fail] result —
    expected or not — makes the run unclean.

    {b Selection} (RFC "The runner", {e Selection}): a test runs iff its path
    contains [config.filter] (when set), does not contain [config.exclude] (when
    set), its tags satisfy [--tag]/[--exclude-tag]/[-q] over
    {!Tag.default_predicate}, it survives the [--failed] allowlist, it falls in
    the requested [--shard] bucket (when set), and — when any focused node
    exists — it is focused. Deselected tests do not execute and are not
    recorded. Fixture releases run after the last executed test on every path
    where the runner regains control, including under [--bail] and after a fatal
    exception, announced through {!Fixture_release} before each teardown and
    outside any per-test timeout (RFC Law 8).

    {b Sharding} (amendment B14). [--shard K/N] partitions the suite into [N]
    buckets by a deterministic hash of each test's full path ({!Seed.derive}
    under a frozen constant root) and selects bucket [K]. Buckets are stable
    across runs, machines, and suite composition — the hash is frozen — so [N]
    concurrent [dune] partitions cover every test exactly once; renaming or
    regrouping a test may move it between buckets. Sharding composes with every
    other selection layer (the bucket applies to the already-filtered set), and
    an empty shard exits [2] like any empty selection (Law 11).

    {b The last-failed store} lives at [<log_dir>/<suite>/.last-failed], written
    atomically ({!Atomic_file}) after every executing run. Its format is
    {e unstable} (an unrecognized file reads as empty). Executed tests update
    their entries — failed recorded once regardless of attempts, passed and
    skipped cleared — entries for tests a partial run did not reach survive, and
    a run that executed the whole suite drops entries whose paths no longer
    exist. Store I/O failures are ignored: the store only feeds [--failed]. *)

(** {1:props Property tests} *)

val prop :
  ?pos:Loc.pos ->
  ?tags:string list ->
  ?count:int ->
  ?examples:'a list ->
  string ->
  'a Gen.t ->
  ('a -> unit) ->
  Test_tree.t
(** [prop name gen law] declares a property test: a leaf test whose body checks
    [law] over [gen] through the property engine ({!Property.run}), with
    per-case seeds derived from the run's root seed and the test's path (RFC Law
    7) and the engine's context installed in the frame while [law] runs (so the
    ambient [collect]/[classify]/[cover] reach it). [count] is the
    generated-case count: the declaration site wins over [--prop-count], which
    wins over the engine default of [100]. [examples] run first, unshrunk.

    The engine's outcome becomes the test's: a counterexample is a
    {!Failure.Property} failure; an unsatisfied [cover] threshold or an
    exhausted generation budget fails the test with a message naming the labels
    or the discard count; and every completed engine run — passed or failed —
    records its {!Property.stats} in {!Run.result.prop_stats}. *)

(** {1:events Events} *)

(** The type for progress events, emitted in execution order. Renderers observe
    them to stream one line per test and to attribute a hanging fixture release;
    they receive only data already decided — no observer can alter status,
    counts, or scheduling (RFC Law 4). *)
type event =
  | Run_started of { run : Run.t; suite : string; total : int; selected : int }
      (** Startup checks passed; [selected] of the suite's [total] tests are
          about to run under the (fresh, still result-less) record [run]. *)
  | Test_started of { path : string list }
      (** The test at [path] is about to run its first attempt. *)
  | Test_finished of Run.result
      (** The test completed and its result was recorded. *)
  | Fixture_release of { name : string }
      (** The fixture identified by [name] is about to release (RFC "Resources":
          announced before, so a hang is attributable). *)

(** {1:startup Startup errors} *)

(** The type for refusals decided before any test executes. *)
type startup_error =
  | Duplicate_paths of string list
      (** Two tests flattened to the same full path; the offending paths,
          sorted, each listed once. *)
  | Focused_in_ci of ([ `Ftest | `Fgroup ] * Loc.t option) list
      (** Focused nodes exist, [CI] is set, and [WINDTRAP_ALLOW_FOCUS] is not:
          the focus sites, in declaration order (RFC "The declaration surface",
          {e Focus}). *)
  | Update_refused_in_ci
      (** A snapshot update was requested under [CI] without
          [WINDTRAP_UPDATE=force] ({!Snapshot.resolve_mode}). *)
  | No_recorded_failures
      (** [--failed] was given but no stored entry names a test of the current
          suite. *)

val startup_exit_code : startup_error -> int
(** [startup_exit_code error] is the process exit code for [error]: [2] for
    {!No_recorded_failures} (nothing ran, Law 11), [1] for the others (the RFC
    fixes the focus guard at [1]; the other refusals follow it). *)

val startup_message : startup_error -> string
(** [startup_message error] is a plain-text (no ANSI) explanation of [error] for
    users, including the lifting spell where one exists
    ([WINDTRAP_ALLOW_FOCUS=1], [WINDTRAP_UPDATE=force]). Not stable for
    programmatic matching. *)

(** {1:outcomes Outcomes} *)

type outcome = {
  run : Run.t;
      (** The run record: results in execution order, the snapshot registry
          (acceptance {!Snapshot.writes} included), and the coverage seam. *)
  selected : Test_tree.case list;
      (** The selected tests in execution order — the [-l] listing data. Under
          [--bail] some may not have executed. *)
  total : int;  (** Leaf tests in the declared suite, before selection. *)
  focus_active : bool;
      (** [true] iff a focused node narrowed the selection — renderers warn on
          successful focused runs outside CI. *)
  bailed : bool;
      (** [true] iff [--bail] stopped the run before the last selected test. *)
  failed_paths : string list;
      (** The paths (as {!Test_tree.path_to_string}) that counted as failed for
          the exit code and the last-failed store, in execution order: failures
          not expected by [xfail], plus expected-failure tests that passed (see
          the preamble, {e Expected failures}). *)
  release_failures : Failure.t list;
      (** {!Failure.Release}-phase failures from end-of-run fixture teardowns,
          in release order. Any entry makes {!outcome.exit_code} [1]. *)
  orphans : string list;
      (** Stale baselines ({!Snapshot.orphans}), reported only after a full,
          clean run — no filters, focus, bail, skips, or failures — and [[]]
          otherwise. Reporting never deletes. *)
  pruned : (string list, Snapshot.prune_refusal) result option;
      (** [Some] iff [config.prune] requested pruning: the deleted paths, or the
          refusal for renderers to explain. [None] otherwise. *)
  duration : float;  (** Wall-clock seconds from startup checks to release. *)
  exit_code : int;
      (** Law 11: [1] when any test counted as failed ({!outcome.failed_paths}
          nonempty) or any release failed; else [2] when no test executed (empty
          suite or empty selection — the filter-typo case); else [0] — a
          nonempty selection whose every test skipped is deliberate and exits
          [0], and so does a run whose only failures were expected ([xfail]).
          List-only runs exit [0]. *)
}
(** The type for completed runs: everything renderers project and the facade
    needs to exit. *)

val execute :
  ?on_event:(event -> unit) ->
  config:Run.config ->
  suite:string ->
  Test_tree.t list ->
  (outcome, startup_error) result
(** [execute ~config ~suite tests] runs [tests] as described in the module
    preamble and is [Ok outcome], or [Error error] when a startup check refuses
    the run before anything executes. [on_event] observes progress (defaults to
    ignoring). [suite] names the run in the capture log directory and the
    last-failed store.

    When [config.list_only] is set, startup checks and selection still apply but
    nothing executes, no event fires, no store or log is touched, and the
    outcome carries the selection with exit code [0].

    Effects: registers a process-wide [Stdlib.at_exit] exit guard on first call
    (never removed; inert while no run is active), reads [CI] via {!Env} (the
    [WINDTRAP_ALLOW_FOCUS] lift arrives already resolved in
    [config.allow_focus]), captures test output under a fresh run directory in
    [config.log_dir] and points the [latest] links at it (unless
    [config.stream]), rewrites the last-failed store, and — in update mode —
    writes accepted baselines through the snapshot registry. Raises
    [Invalid_argument] when called while a run is already active (from a test
    body, the calling test fails with that error, per RFC "The declaration
    surface"), and when [config.shard] violates [1 <= K <= N] — the CLI layer
    validates every layer it resolves, so only a hand-built configuration can
    trip this. If [on_event] raises, the run aborts with that exception — after
    a best-effort fixture release (RFC Law 8), like a fatal exception. *)
