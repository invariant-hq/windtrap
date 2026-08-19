(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Sequential test execution: selection, the per-test boundary, retries,
    fixtures, the last-failed store, and exit codes.

    {!execute} drives a declared {!Test_tree.t} list under a resolved
    {!Run.config}: it applies the startup checks (duplicate paths, the CI focus
    guard, the snapshot CI guard, the [--failed] store), selects tests, runs
    them one at a time in declaration order (one process, one domain,
    sequential), releases fixtures, maintains the last-failed store, and
    computes the [0]/[1]/[2] exit code. It prints nothing: progress streams
    through typed {!type:event}s and everything else is data in the returned
    {!type:outcome} for renderers to project.

    {b The per-test boundary.} Every attempt gets a fresh {!Run.frame} installed
    in the ambient slot, the global [Random] state reseeded to a pure function
    of the test's path (saved and restored around the attempt — test order
    cannot perturb a test that consults [Random]), capture redirected into the
    test's log file ({!Capture.with_capture}, per-attempt truncation; a no-op
    under [--stream]), and a SIGALRM timeout arming the test's limit (else
    [config.timeout]). The timeout window covers setup, body, and teardown (for
    a scoped test, the whole scope call): setup and body share it, and it is
    re-armed before teardown for whatever remains — or for a fresh limit when
    they consumed it, since a teardown entered after a body timeout must still
    be bounded and must still run. It is Unix-only (a documented no-op on
    Windows) and cannot interrupt blocked C calls. The runner owns [SIGALRM]
    while a test with a limit runs. After every attempt — outside the timeout
    window, on every path where the runner regains control, a fatal exception
    included — the attempt is reclaimed ({!Run.reclaim}): the working directory
    and the environment bindings it changed ({!Run.chdir}, {!Run.setenv}) go
    back, and its scratch paths are removed. A restoration that fails is a
    {!Failure.Teardown}-phase failure of the test, recorded with the attempt's
    others.

    {b Scoped tests.} A {!Test_tree.Scoped} node is one call the runner does not
    control; [Windtrap.scoped] specifies the protocol enforced here — one
    callback entry, attribution by how far the callback got, the body's failure
    re-raised through [scope]. What is the runner's: the body runs inside the
    callback, its failure is recorded {e before} being re-raised, a second entry
    is refused rather than served, and the timeout window covers the whole
    [scope] call, re-armed as the body leaves the callback on the same terms as
    a bracket teardown.

    Outcomes are classified per phase: {!Failure.Check_failure} keeps its
    payload, {!Failure.Skip_test} skips the test, {!Failure.Timeout} becomes a
    failure of the phase it interrupted, [Sys.Break]/[Out_of_memory]/
    [Stack_overflow] re-raise after a best-effort fixture release, and any other
    exception becomes a {!Failure.Raise} failure carrying its backtrace.

    {!execute} enables {!Printexc.record_backtrace} for the process and does not
    restore it. The runtime keeps a backtrace only when asked, and without one a
    raising test reports its constructor and its declaration line and nothing
    else — no raise site. This is a process-wide setting, so it overrides a
    deliberate [OCAMLRUNPARAM=b=0] or an explicit
    [Printexc.record_backtrace false] in code under test; the cost scales with
    stack depth, measured at 0.04 microseconds per raise at ten frames and 2.8
    at five hundred (0.01 and 0.34 with recording off).

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
    inert in any process other than the one that armed it — a test that forks
    and calls [exit] in the child terminates the child, which is what a test
    spawning subprocesses expects, and the child does not inherit the run. It is
    likewise inert while no run is active: exits before, after, and by the
    runner itself pass through untouched.

    A {!Test_tree.bracket}'s stored closures run as data, in the order
    [Windtrap.bracket] specifies. What is the runner's: the body and teardown
    outcomes are captured {e independently}, one failure-list entry per failed
    phase, never composed with [Fun.protect] at the reporting boundary — body
    and release failures are both reported — and a timeout during teardown is a
    [Teardown]-phase failure alongside any body failure.

    {b Retries.} A test with [retries = n] reruns while its outcome
    {e counts as failed} (see {e Expected failures} below — for an [xfail] test
    that is an unexpected pass), up to [n + 1] attempts, each attempt a fresh
    frame and a truncated capture file. The recorded result carries the
    {e final} attempt's failures and output tail and the attempt count, which
    is what renderers show ({!Run.result.attempts}). Skips are never retried;
    the captured-output tail of a failed test is attached to its first failure
    entry.

    {b Expected failures.} A test marked {!Test_tree.xfail} still runs, and
    [Windtrap.xfail] states what its outcomes mean. What inverts here is what
    {e counts}: an expected failure keeps its real failures but consumes no
    [--bail] budget, enters no last-failed store entry and leaves the exit code
    alone, while an unexpected pass does all three and is recorded as [Fail]
    with one message failure. Skips are unaffected. Each recorded result carries
    the decision ({!Run.result.counted}) and the annotation
    ({!Run.result.xfail}), so renderers classify from the record alone. For
    baseline maintenance (below), {e every} [Fail] result — expected or not —
    makes the run unclean.

    {b Selection.} A test runs iff its path contains [config.filter] (when set),
    does not contain [config.exclude] (when set), its tags satisfy
    [--tag]/[--exclude-tag] over {!Tag.any}, it survives
    the [--failed] store and the caller's [?allowlist], it falls in the
    requested [--shard] bucket (when
    set), and — when any focused node exists — it is focused. Deselected tests
    do not execute and are not recorded. Fixture releases run after the last
    executed test on every path where the runner regains control, including
    under [--bail] and after a fatal exception, announced through
    {!Fixture_release} before each teardown and outside any per-test timeout.

    {b Sharding.} [--shard K/N] partitions the suite into [N] buckets by a
    deterministic hash of each test's full path ({!Seed.derive} under a frozen
    constant root) and selects bucket [K] — the stability [Windtrap.run]'s
    overview promises, since the hash never changes. Renaming or regrouping a
    test may move it between buckets. The bucket applies to the already-filtered
    set, and an empty shard exits [2] like any empty selection.

    {b Baseline maintenance.} A run that executed the whole declared suite with
    nothing filtered, focused, bailed, skipped or failed — and only such a run —
    knows the full set of baseline names the suite claims, so only such a run
    may say that a stored baseline is stale ({!Snapshot.orphans}, reported in
    {!outcome.orphans}). After any other run the set is empty: a filtered run
    cannot tell a stale baseline from one this invocation did not select. The
    report never deletes and never fails the run — a baseline is a committed
    file, so removing one is the user's edit, and the report names every path
    for it.

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
  ?timeout:float ->
  ?count:int ->
  ?max_discard:int ->
  ?examples:'a list ->
  string ->
  'a Gen.t ->
  ('a -> unit) ->
  Test_tree.t
(** [prop name gen law] declares a property test: a leaf test whose body checks
    [law] over [gen] through the property engine ({!Property.run}), with
    per-case seeds derived from the run's root seed and the test's path and the
    engine's context installed in the frame while [law] runs (so the ambient
    [collect]/[classify]/[cover] reach it). [timeout] is the underlying test's
    per-test limit ({!Test_tree.test}); as the whole property runs inside one
    test body, it budgets generation and shrinking together — a timeout during
    the shrink search ends the search at the best counterexample found
    ({!Property.run}, Shrinking). [count] is the generated-case count: the
    declaration site wins over [--prop-count], which wins over the engine
    default of [100]. [examples] run first, unshrunk.

    The engine's outcome becomes the test's: a counterexample is a
    {!Failure.Property} failure; an unsatisfied [cover] threshold or an
    exhausted generation budget fails the test with a message naming the labels
    or the discard count; and every completed engine run — passed or failed —
    records its {!Property.stats} in {!Run.result.prop_stats}. *)

(** {1:events Events} *)

(** The type for progress events, emitted in execution order. Renderers observe
    them to stream one line per test and to attribute a hanging fixture release;
    they receive only data already decided — immutable projections (counts,
    identities, recorded results), never the live run record — so no observer
    can alter status, counts, or scheduling. The run handle belongs to whoever
    owns the session: the driver reads it off the returned {!type:outcome},
    never off an event. *)
type event =
  | Run_started of { suite : string; total : int; selected : int }
      (** Startup checks passed; [selected] of the suite's [total] tests are
          about to run. *)
  | Test_started of { path : string list }
      (** The test at [path] is about to run its first attempt. *)
  | Test_finished of Run.result
      (** The test completed and its result was recorded. *)
  | Fixture_release of { name : string }
      (** The fixture identified by [name] is about to release (announced before
          the teardown runs, so a hang is attributable). *)

(** {1:startup Startup errors} *)

(** The type for refusals decided before any test executes. *)
type startup_error =
  | Duplicate_paths of string list
      (** Two tests flattened to the same full path; the offending paths,
          sorted, each listed once. *)
  | Focused_in_ci of ([ `Ftest | `Fgroup ] * Loc.t option) list
      (** Focused nodes exist and [CI] is set: the focus sites, in declaration
          order. *)
  | Update_refused_in_ci
      (** A snapshot update was requested under [CI] without
          [WINDTRAP_UPDATE=force] ({!Snapshot.resolve_mode}). *)
  | No_recorded_failures
      (** [--failed] was given but no stored entry names a test of the current
          suite. *)

val startup_exit_code : startup_error -> int
(** [startup_exit_code error] is the process exit code for [error]: [2] for
    {!No_recorded_failures} (nothing ran, so [2]), [1] for the others (a refused
    run is a failed run, not an empty one). *)

val startup_message : startup_error -> string
(** [startup_message error] is a plain-text (no ANSI) explanation of [error] for
    users, including the lifting spell where one exists
    ([WINDTRAP_UPDATE=force]). Not stable for programmatic matching. *)

(** {1:outcomes Outcomes} *)

type outcome = {
  run : Run.t;
      (** The run record: results in execution order — every executed test's
          row, then the end-of-run verdict rows ({!Run.type-subject}): one
          {!Run.Fixture_release} row per failed fixture teardown — plus the
          snapshot registry (acceptance {!Snapshot.writes} included)
          and the coverage seam. Every sink projects this one list, so a verdict
          that sets the exit code is always visible in the report. *)
  selected : Test_tree.case list;
      (** The selected tests in execution order — the [-l] listing data. Under
          [--bail] some may not have executed. *)
  total : int;  (** Leaf tests in the declared suite, before selection. *)
  focus_active : bool;
      (** [true] iff a focused node narrowed the selection — renderers warn on
          successful focused runs outside CI. *)
  orphans : string list;
      (** Baselines still stale when the run ended ({!Snapshot.orphans}),
          reported only after a full, clean run — no filters, focus, bail,
          skips, or failures — and [[]] otherwise. Advisory: it never deletes
          and never changes {!outcome.exit_code}. *)
  duration : float;  (** Wall-clock seconds from startup checks to release. *)
  exit_code : int;
      (** [1] when any recorded row counted as failed ({!Run.result.counted}) —
          a test the [xfail] annotation did not excuse, or a failed fixture
          release; else [2] when no test executed (empty suite or
          empty selection — the filter-typo case); else [0] — a nonempty
          selection whose every test skipped is deliberate and exits [0], and so
          does a run whose only failures were expected ([xfail]). List-only runs
          exit [0]. *)
}
(** The type for completed runs: everything renderers project and the facade
    needs to exit. *)

val execute :
  ?on_event:(event -> unit) ->
  ?allowlist:string list ->
  config:Run.config ->
  suite:string ->
  Test_tree.t list ->
  (outcome, startup_error) result
(** [execute ~config ~suite tests] runs [tests] as described in the module
    preamble and is [Ok outcome], or [Error error] when a startup check refuses
    the run before anything executes. [on_event] observes progress (defaults to
    ignoring). [suite] names the run in the capture log directory and the
    last-failed store. [allowlist] narrows the selection to those exact full
    paths ({!Test_tree.path_to_string}), for a caller that already holds the set
    it wants and cannot spell it as a substring filter — the mutation loop's
    children, which run one mutant's reaching tests. It composes with every
    other selection layer by intersection, [--failed] included.

    Effects: registers a process-wide [Stdlib.at_exit] exit guard on first call
    (never removed; inert while no run is active), reads [CI] via {!Env},
    captures test output under [config.log_dir] (unless [config.stream]),
    rewrites the last-failed store, and — in update mode —
    writes accepted baselines through the snapshot registry. Raises
    [Invalid_argument] when called while a run is already active (from a test
    body, the calling test fails with that error), and when [config.shard]
    violates [1 <= K <= N] — the CLI layer validates every layer it resolves, so
    only a hand-built configuration can trip this. If [on_event] raises, the run
    aborts with that exception — after a best-effort fixture release, like a
    fatal exception. *)

val list_selection :
  config:Run.config ->
  suite:string ->
  Test_tree.t list ->
  (string list, startup_error) result
(** [list_selection ~config ~suite tests] is the full paths
    ({!Test_tree.path_to_string}) {!execute} would run, in declaration order:
    the startup checks and the selection, with nothing executed. [Error error]
    on a refused run, exactly when {!execute} would refuse — [--list] does not
    excuse a mistyped [--shard] or a missing [--failed] store.

    Effects: the startup ones only — the exit-guard registration,
    [Printexc.record_backtrace true], the [CI] read and the [--failed] store
    read. No capture, no log directory, no store rewrite, no baseline. Raises
    [Invalid_argument] as {!execute} does. *)

