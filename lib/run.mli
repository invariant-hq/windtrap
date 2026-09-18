(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Test execution: the run configuration, the per-run record and its one
    ambient slot, and the sequential executor.

    A run is one {!execute} of a {!Test_tree.t} list under one {!type:config}.
    All mutable run state lives in one {!type:t} created per run: the resolved
    configuration, the capture state, the baseline registry, the fixture cache
    and the accumulated {!type:result}s. Test bodies reach it through one
    ambient slot, the only run-state [ref] in the library: the run while
    {!with_active} brackets it, overlaid by the attempt's {!type:frame} while
    {!with_frame} does. Reading the slot with no frame in it raises the
    assertions-outside-run error, an [Invalid_argument] saying the operation
    only works inside a test body executed by [run]. The executor is sequential,
    one domain; nothing here is thread-safe.

    {!execute} prints nothing: progress streams through typed {!type:event}s and
    everything else is data in the returned {!type:outcome}, which [Report]
    projects. *)

(** {1:config Configuration} *)

type invocation = [ `Exe of string | `Mirrors ]
(** The type for hint invocation contexts. [`Exe cmd] is a command that re-runs
    this executable, which hints complete with flags ([cmd --failed], [cmd -u]).
    [`Mirrors] means no command line re-runs this suite (the inline runner's, a
    [--corrected] run's and an empty [argv]'s context): hints spell [WINDTRAP_*]
    prefixes to [dune runtest] and acceptance as [dune promote]. *)

(** The type for what a mutation-instrumented build is asked to do with the run.
    Resolved by [Cli] from [--mutate] and [--arm], which it refuses together;
    acted on by [Mutate_loop]. *)
type mutation =
  | No_mutation  (** The ordinary run, mutants inert. *)
  | Loop of string list
      (** [--mutate[=PREFIX,…]]: run the mutation loop over the mutants whose
          recorded source path starts with one of the prefixes, every mutant
          when the list is empty. *)
  | Armed of string
      (** [--arm ID]: one ordinary run with mutant [ID] armed, the identifier
          unparsed ({!Windtrap_runtime.Mutate.id_of_string} owns its grammar).
      *)

type config = {
  seed : Seed.seed;  (** The run's root seed. *)
  filter : string option;
      (** [-f]/positional/[WINDTRAP_FILTER]: run only tests whose path contains
          this substring. *)
  exclude : string option;
      (** [-e]/[WINDTRAP_EXCLUDE]: drop tests whose path contains this
          substring. *)
  tags : string list;  (** [--tag] (repeatable): required tags. *)
  exclude_tags : string list;
      (** [--exclude-tag] (repeatable): dropped tags. *)
  shard : (int * int) option;
      (** [--shard K/N]/[WINDTRAP_SHARD]: run only bucket [K] of [N] (see
          {e Sharding} under {{!section-executing}executing}). Invariant
          [1 <= K <= N], validated by the CLI layer. *)
  failed_only : bool;  (** [--failed]: rerun only the last run's failures. *)
  bail : bool;  (** [-x]/[--fail-fast]: stop after the first counted failure. *)
  stream : bool;
      (** [--stream]: run against the real descriptors ({!Capture.disabled}). *)
  baseline : Baseline.mode;
      (** [-u] ({!Baseline.Update}), [--corrected] ({!Baseline.Corrected}) or
          neither ({!Baseline.Check}). Neither flag has a mirror. The executor
          refuses {!Baseline.Update} under [CI]. *)
  timeout : float option;  (** [--timeout]: default per-test limit, seconds. *)
  prop_count : int option;  (** [--prop-count]: generated cases per property. *)
  log_dir : string;
      (** [-o]/[--output]: root directory for capture logs and the last-failed
          store ({!Os.default_log_dir} when not given). *)
  allow_focus : bool;
      (** Lift the CI guard on focused tests. No flag sets it: only
          {!for_subset}, for a forked mutation child. *)
  color : Os.color_mode;
      (** [--color]/[WINDTRAP_COLOR]: the colour preference, resolved against
          the report's sink ({!Os.resolve_color}) by whoever builds it. *)
  slow_threshold : float;
      (** [--slow-threshold]/[WINDTRAP_SLOW_THRESHOLD]: seconds a test not
          tagged ["slow"] may take before the report warns ([0.] disables).
          Invariant: finite and non-negative, validated by the CLI layer. *)
  verbose : bool;  (** [-v]/[WINDTRAP_VERBOSE]: one status line per test. *)
  junit : string option;
      (** [--junit]/[WINDTRAP_JUNIT]: where a JUnit report is also written, a
          file or a directory; [None] for no report. *)
  mutation : mutation;
      (** [--mutate]/[WINDTRAP_MUTATE] and [--arm]/[WINDTRAP_MUTATE_ARM]. Acted
          on by [Mutate_loop] alone; [Report] reads an armed identifier to spell
          its hints. {!for_subset} clears it. *)
  github : bool;
      (** Whether the report is written for GitHub Actions
          ({!Os.in_github_actions}). *)
  invocation : invocation;
      (** The hint context every acceptance and replay line derives from.
          {!Cli.settings} leaves it [`Mirrors]; the facade computes it from
          [argv]. *)
}
(** The type for run configuration: everything one invocation resolves, with the
    precedence CLI > environment > default ({!Cli.settings}). [color],
    [slow_threshold], [verbose], [junit], [github] and [invocation] are read by
    [Report] alone and cannot change outcomes or exit codes. Nothing re-reads
    flags or the environment mid-run. *)

val default_config : unit -> config
(** [default_config ()] is the configuration with every field at its built-in
    default: no selection, every flag off, [Baseline.Check], [color = Os.Auto],
    [slow_threshold = 1.], [mutation = No_mutation], [invocation = `Mirrors].
    Effects: [seed] is drawn from {!Seed.random} and [log_dir] is
    {!Os.default_log_dir}[ ()]. *)

val for_subset : config -> log_dir:string -> bail:bool -> config
(** [for_subset config ~log_dir ~bail] is [config] for a run over a subtree of
    its own selection, a mutation loop's forked child: [filter], [exclude],
    [shard] and [failed_only] cleared (the child's {!execute} allowlist is the
    selection), [tags], [exclude_tags] and [seed] kept, [baseline = Check],
    [stream = true], [allow_focus = true], no JUnit report, [No_mutation], and
    [log_dir] and [bail] as given. *)

(** {1:runs Run records} *)

type t
(** The type for per-run records. Created by {!execute} at the start of a run
    and dead at its end; never reused. *)

val create : config -> capture:Capture.t -> baselines:Baseline.t -> t
(** [create config ~capture ~baselines] is a fresh run record over [capture] and
    [baselines], with an empty fixture cache and no results. *)

val config : t -> config
(** [config t] is the run's resolved configuration. *)

val capture : t -> Capture.t
(** [capture t] is the run's capture state. *)

val baselines : t -> Baseline.t
(** [baselines t] is the run's baseline registry. *)

(** {1:frames Per-test frames} *)

type frame
(** The type for per-attempt frames: the executing test's identity, its
    accumulated failures, and the property context while a property body runs.
    The executor creates a fresh frame for every attempt. *)

val frame :
  ?corrections:bool -> t -> path:string list -> loc:Loc.t option -> frame
(** [frame t ~path ~loc] is a fresh frame for one attempt of the test at [path]
    (as flattened by {!Test_tree.flatten}), declared at [loc], the fallback
    attribution for failures recorded without a location. [corrections] is
    whether the attempt may record baseline corrections ({!check_baseline});
    defaults to [true], cleared for an [xfail] test. *)

val run_of_frame : frame -> t
(** [run_of_frame frame] is the run record [frame] belongs to. *)

val path : frame -> string list
(** [path frame] is the executing test's full path, groups first. *)

val loc : frame -> Loc.t option
(** [loc frame] is the test's declaration location, as passed to {!frame}. *)

val add_failure : frame -> Failure.t -> unit
(** [add_failure frame failure] appends [failure] to the attempt's failure list,
    one entry per phase that failed ({!Failure.with_phase}). A [failure] whose
    location is [None] is recorded with the test's declaration location when the
    frame has one; nested failures (a property failure's [inner]) are left
    untouched. *)

val failures : frame -> Failure.t list
(** [failures frame] is the attempt's failures in the order they were added. *)

val prop_context : frame -> Property.context option
(** [prop_context frame] is the running property's label context, or [None] when
    no property body is executing; the ambient [collect], [classify] and [cover]
    dispatch through it and error on [None]. *)

val with_prop_context : frame -> Property.context -> (unit -> 'a) -> 'a
(** [with_prop_context frame ctx fn] is [fn ()] with [ctx] installed as
    [frame]'s property context; the previous value is restored on return and on
    raise. *)

(** {1:ambient The ambient slot} *)

val with_frame : frame -> (unit -> 'a) -> 'a
(** [with_frame frame fn] is [fn ()] with [frame] in the ambient slot; the
    previous slot value is restored on return and on raise. The executor wraps
    exactly the extent of one attempt, setup, body and teardown. *)

val with_active : t -> (unit -> 'a) -> 'a
(** [with_active t fn] is [fn ()] with [t] marked in the ambient slot as the
    executing run; the previous slot value is restored on return and on raise.
    The executor wraps the executing span of one run: events, attempts, fixture
    release, store maintenance. {!with_frame} nests inside it. *)

val active : unit -> bool
(** [active ()] is [true] iff a run is executing ({!with_active}), whether or
    not a test attempt is ({!with_frame}). *)

val active_run_error : string
(** [active_run_error] is the message every already-active refusal raises
    [Invalid_argument] with; a nested [run] gets the same sentence whichever
    check saw it first. *)

val current_frame : unit -> frame
(** [current_frame ()] is the frame of the attempt currently executing. Raises
    [Invalid_argument], the assertions-outside-run error, when no test is
    running. *)

val current : unit -> t
(** [current ()] is {!run_of_frame} of {!current_frame}. Raises
    [Invalid_argument] as {!current_frame} does. *)

(** {1:body Test-body operations}

    Ambient operations for test bodies, dispatched through the slot; each raises
    the assertions-outside-run error ([Invalid_argument], see {!current_frame})
    when no test is running. The facade re-exports them. *)

val current_test : unit -> string list
(** [current_test ()] is the executing test's full path: enclosing group names
    root first, then the test's own name. Never empty, and stable across
    attempts of the same test; joined with {!Test_tree.path_to_string} it is the
    string selection filters match. *)

val subtest : string -> (unit -> unit) -> unit
(** [subtest name fn] runs [fn ()] as a named sub-case of the executing test. An
    assertion failure or other non-fatal exception from [fn] is appended to the
    test's failures, labeled with the [" › "]-joined path of the test's name and
    the enclosing subtest names in the failure's [msg] slot (prefixed to a user
    [~msg]), and [subtest] returns: later siblings still run and the test fails
    at the end with every entry. Subtests nest; labels compose.

    A skip and a timeout propagate and abort the whole test, failures already
    recorded still failing it and a timeout's failure being the executor's,
    unlabeled; a fatal exception propagates out of the run. Retries reset
    recorded subtest failures. Sub-cases are failure entries, not tests: [-f]
    cannot select them. Inside a property body a subtest failure bypasses the
    engine: the case completes unshrunk. Entries keep the {!Failure.Body} phase
    even inside a bracket's setup or teardown. *)

(** {1:baselines Baselines} *)

val check_baseline : ?loc:Loc.t -> Baseline.subject -> string -> unit
(** [check_baseline subject actual] is {!Baseline.check} against the executing
    test's registry, read-only when the frame was created without [corrections].
    [loc] is the failure's location: the literal's position, or the call frame
    for a file; none when the call sat in tail position, and the executor then
    attributes the failure to the test's declaration.

    A checkpoint, not an assertion: a {!Failure.Missing} or {!Failure.Mismatch}
    failure is recorded on the frame, labeled as {!subtest} labels one, and the
    call returns. Only {!Failure.Unresolvable} raises {!Failure.Check_failure}.
    Raises the assertions-outside-run error when no test is running. *)

(** {1:scratch Executor-owned scratch}

    Per-test temporary paths, created lazily under one scratch directory per
    attempt and removed by the executor after the attempt on every path where it
    regains control: failure, skip, timeout and a fatal exception unwinding the
    run included. A resource that must outlive the test (one acquired by a
    {!fixture}) must not live in them. *)

val temp_dir : ?prefix:string -> unit -> string
(** [temp_dir ()] is a fresh empty directory for the executing test, created
    with permissions [0o700] in the attempt's scratch directory (itself created
    on demand under the system temporary directory). Each call returns a new
    directory. [prefix] is the basename prefix, default ["dir"], sanitized to a
    safe path component. Raises [Unix.Unix_error] if the directory cannot be
    created. *)

val temp_file : ?suffix:string -> unit -> string
(** [temp_file ()] is the path of a fresh empty file, created with permissions
    [0o600] in the executing test's scratch directory. [suffix] (e.g. [".json"];
    default none) is appended to the basename, sanitized to a safe path
    component. Raises [Unix.Unix_error] if the file cannot be created. *)

val remove_tree : string -> unit
(** [remove_tree path] removes [path] and everything under it, best effort:
    symbolic links are removed, never followed, and every filesystem error is
    swallowed. Library-internal: [Mutate_loop] uses it to remove each forked
    child's log directory from the parent, since a child killed at its deadline
    never runs its own cleanup. *)

(** {1:process Executor-restored process state}

    The environment and the working directory belong to the process; a test body
    records what it changed and the executor undoes it at the attempt boundary
    ({!reclaim}), on every outcome and per attempt. Both are process-global
    while the test runs: a thread the test spawns sees them, and a change made
    from such a thread races the restoration. *)

val setenv : string -> string option -> unit
(** [setenv name (Some value)] binds the environment variable [name] to [value]
    for the rest of the executing test; [setenv name None] unbinds it
    ({!Os.setenv}, a real unbinding). The executor restores what [name] held
    before the attempt's first [setenv] of it when the attempt ends; later calls
    change the binding without touching the restore record. A restoration that
    fails is reported at the call that made the change.

    Raises the assertions-outside-run error when no test is running, before the
    process is touched, and [Invalid_argument] for a name {!Os.setenv} refuses.
*)

val chdir : string -> unit
(** [chdir dir] changes the working directory to [dir] ([Unix.chdir]) for the
    rest of the executing test. The executor restores the directory captured at
    the attempt's first [chdir] when the attempt ends. A restoration that fails
    is reported at the call that made the change.

    Raises the assertions-outside-run error when no test is running, and
    [Unix.Unix_error] when [dir] cannot be entered. *)

val reclaim : frame -> unit
(** [reclaim frame] undoes the attempt's ambient changes and removes its
    scratch: it restores the working directory captured by {!chdir}, then the
    bindings recorded by {!setenv}, then removes the scratch directory behind
    [frame]'s {!temp_dir}/{!temp_file} paths, when one was created. Called by
    the executor after every attempt, outside the timeout window. Never raises,
    and idempotent. Scratch removal is best-effort; a restoration that fails is
    recorded on [frame] as a {!Failure.Teardown}-phase message failure located
    at the change that could not be undone. *)

(** {1:fixtures Fixtures} *)

val fixture : ?teardown:('a -> unit) -> (unit -> 'a) -> unit -> 'a
(** [fixture ?teardown create] is the accessor for a run-scoped shared resource.
    The accessor owns no state: creating it runs nothing, and its cache lives in
    the current run's record, keyed by the accessor's identity. The first call
    in a run acquires with [create ()], inside the calling test's failure
    boundary, and registers [teardown] for end-of-run release; later calls in
    the run return the cached value. An acquisition that raised is cached and
    re-raised, with its original backtrace, by every later use in that run; a
    skip raised during acquisition is cached as a skip, every later use skips
    with the same reason, and nothing is registered for release. A later [run]
    in the same process re-acquires. Calling the accessor outside a run raises
    the assertions-outside-run error. *)

val release_fixtures : t -> announce:(string -> unit) -> Failure.t list
(** [release_fixtures t ~announce] releases every fixture acquired in [t] in
    reverse acquisition order and drains the registry; a second call releases
    nothing. For each fixture that acquired successfully and has a teardown,
    [announce name] is called before the teardown runs, [name] identifying the
    fixture by its declaration site (["fixture (test/test_users.ml:12)"], or
    ["fixture #<n>"] with no location); a fixture without a teardown, or whose
    acquisition failed, releases nothing and is not announced. A teardown that
    raises contributes a {!Failure.Release}-phase failure, located at the
    declaration site, to the result (release order), and the remaining releases
    still run; [Sys.Break], [Out_of_memory] and [Stack_overflow] are re-raised
    at once instead. The executor calls this after the last test on every path
    where it regains control, [-x] included, outside any per-test timeout. *)

(** {1:results Results} *)

(** The type for what a result row reports on. Consumers that reason about tests
    (mutation verdicts, the last-failed store, full-run detection) dispatch on
    this field, never on the reporting path. *)
type subject =
  | Test  (** A declared test the executor executed. *)
  | Fixture_release
      (** An end-of-run fixture teardown that raised ({!release_fixtures}): one
          row per failure. *)

val fixture_release_path : string list
(** [fixture_release_path] is [["fixture release"]], the reporting path of
    {!Fixture_release} rows. Library-internal: [Mutate_loop]'s verdict
    vocabulary names it; row consumers dispatch on {!result.subject}. *)

type result = {
  path : string list;
      (** The row's reporting path: the test's full path for a {!Test} row,
          {!fixture_release_path} otherwise. *)
  subject : subject;  (** What the row reports on. *)
  outcome : Failure.outcome;  (** The classified outcome, failures inside. *)
  counted : bool;
      (** [true] iff the result counted as failed, the bit retries, [-x], the
          exit code and the last-failed store are driven from: an ordinary
          failure, or an [xfail] test's unexpected pass. [false] for passes,
          skips and excused expected failures; an uncounted [Fail] is an excused
          expected failure. *)
  xfail : Test_tree.xfail option;
      (** The test's expected-failure annotation ({!Test_tree.case.xfail}),
          [None] for unannotated tests. *)
  slow_tagged : bool;
      (** [true] iff the test carries the ["slow"] tag, its own or an
          ancestor's; such tests are exempt from the slow threshold. *)
  duration : float;
      (** The test's execution time in seconds, attempts summed. *)
  attempts : int;
      (** Attempts executed: [1] plus retries used. A passing row with
          [attempts > 1] is a flaky test. *)
  prop_stats : Property.stats option;
      (** The property engine's bookkeeping for property tests; [None]
          otherwise. *)
}
(** The type for result rows, one per completed test plus the end-of-run verdict
    rows: counted [Fail] rows with no annotation, one attempt and zero duration.
    The record carries every fact rendering needs; no consumer re-derives
    executor decisions from messages. *)

val record : t -> result -> unit
(** [record t result] appends [result] to the run's results. *)

val results : t -> result list
(** [results t] is the recorded rows in execution order: every executed test's
    row, then any fixture-release rows (release order). *)

(** {1:props Property tests} *)

val prop :
  ?__POS__:Loc.pos ->
  ?tags:string list ->
  ?timeout:float ->
  ?count:int ->
  ?max_discard:int ->
  ?examples:'a list ->
  ?summary:('a -> string option) ->
  string ->
  'a Gen.t ->
  ('a -> unit) ->
  Test_tree.t
(** [prop name gen law] declares a leaf test whose body checks [law] over [gen]
    through {!Property.run}, with per-case seeds derived from the run's root
    seed and the test's path and the engine's context installed in the frame
    while [law] runs. [timeout] is the test's limit and budgets generation and
    shrinking together: a timeout during the shrink search ends it at the best
    counterexample found. [count] is the generated-case count: the declaration
    wins over [--prop-count], which wins over the engine's [100]. [examples] run
    first, unshrunk. [summary] is {!Property.run}'s. A counterexample is a
    {!Failure.Property} failure; an unsatisfied [cover] threshold or an
    exhausted generation budget fails the test with a message naming the labels
    or the discard count; every completed engine run records its
    {!Property.stats} in {!result.prop_stats}. *)

(** {1:events Events} *)

(** The type for progress events, emitted in execution order. Observers receive
    only data already decided, never the live run record, so no observer can
    alter status, counts or scheduling. *)
type event =
  | Run_started of {
      suite : string;
      total : int;
      selected : int;
      properties : bool;
    }
      (** Startup checks passed; [selected] of the suite's [total] tests are
          about to run. [properties] is [true] iff a selected test carries
          {!Test_tree.Tag.prop}: the run's seed decides something. *)
  | Test_started of { path : string list }
      (** The test at [path] is about to run its first attempt. *)
  | Test_finished of result
      (** The test completed and its result was recorded. *)
  | Fixture_release of { name : string }
      (** The fixture identified by [name] is about to release. *)
  | Interrupted of {
      running : string list option;
      releasing : string option;
      results : result list;
      duration : float;
    }
      (** A signal is ending the run: [running] is the test it stopped, [None]
          outside a test; [releasing] names the fixture whose release it
          stopped, as {!Fixture_release} does; [results] are the results
          recorded so far and [duration] the run's so far. The last event,
          delivered once; the process dies by the signal after it. *)

(** {1:startup Startup errors} *)

(** The type for refusals decided before any test executes. *)
type startup_error =
  | Duplicate_paths of string list
      (** Two tests flattened to the same full path; the offending paths,
          sorted, each listed once. *)
  | Focused_in_ci of Loc.t option list
      (** Focused nodes exist and [CI] is set: their declaration sites, in
          declaration order. *)
  | Update_refused_in_ci
      (** [-u] was given under [CI]; there is no override. *)
  | No_recorded_failures
      (** [--failed] was given but no stored entry names a test of the current
          suite. *)

val startup_exit_code : startup_error -> int
(** [startup_exit_code error] is the process exit code for [error]: [2] for
    {!No_recorded_failures} (nothing ran), [1] for the others (a refused run is
    a failed run). *)

val startup_message : startup_error -> string
(** [startup_message error] is a plain-text explanation of [error] for users, to
    be printed behind [windtrap:] ({!Os.say}). Not stable for programmatic
    matching. *)

(** {1:executing Executing}

    {!execute} applies the startup checks (duplicate paths, the CI focus guard,
    the baseline CI guard, the [--failed] store), selects tests, runs them one
    at a time in declaration order, releases fixtures, maintains the last-failed
    store, writes the kept baseline corrections, and computes the [0]/[1]/[2]
    exit code.

    {b The per-test boundary.} Every attempt gets a fresh {!type:frame} in the
    ambient slot, the global [Random] state reseeded from the test's path (saved
    and restored around the attempt), capture redirected into the test's log
    file ({!Capture.with_capture}; a no-op under [--stream]), and a SIGALRM
    timeout arming the test's limit (else [config.timeout]); the executor owns
    [SIGALRM] while a test with a limit runs. The window covers setup, body and
    teardown (for a scoped test, the whole scope call), re-armed before teardown
    for whatever remains, or for a fresh limit when the earlier phases consumed
    it; it is Unix-only and cannot interrupt blocked C calls. After every
    attempt, outside the window, the attempt is reclaimed ({!reclaim}). Outcomes
    are classified per phase: {!Failure.Check_failure} keeps its payload,
    {!Failure.Skip_test} skips the test, {!Failure.Timeout} becomes a failure of
    the phase it interrupted, [Sys.Break]/[Out_of_memory]/[Stack_overflow]
    re-raise after a best-effort fixture release, and any other exception
    becomes a {!Failure.Raise} failure carrying its backtrace. A failure the
    executor makes itself (a timeout, an uncaught exception, an [xfail] test
    that passed, an intercepted [exit], a misused scope, a property that gave up
    or missed its coverage) is located at the test's declaration.

    {b Scoped tests.} For a {!Test_tree.Scoped} node the body runs inside the
    callback, its failure is recorded before being re-raised through [scope], a
    second entry into the callback is refused, and whatever else escapes is
    attributed by how far the callback got: [Setup] before it, [Teardown] after
    it returned. A {!Test_tree.bracket} is such a scope, one failure entry per
    failed phase.

    {b Backtraces and exits.} {!execute} enables {!Printexc.record_backtrace}
    and does not restore it. A callback that calls [exit] does not terminate the
    process: the first {!execute} in a process registers a [Stdlib.at_exit]
    guard which, while a run is active, re-arms itself and raises
    {!Failure.Exit_attempt}, recorded as a [Message] failure of the phase that
    attempted it. The guard is inert while no run is active, and in a forked
    child.

    {b Signals.} While a run executes, and not on Windows, the executor handles
    [SIGINT], [SIGTERM] and [SIGHUP], unless the process was started with the
    signal ignored. On the first of them the three go back to their default
    disposition, so a second one kills at once. A signal that arrives while a
    test attempt or a fixture's release runs acts at once; one that arrives in
    the executor's own code, or an observer's, acts before the next test starts,
    or at the end of the run. Acting is: the running attempt's capture is
    abandoned ({!Capture.abandon}); the {!Interrupted} event is delivered; the
    attempt's scratch directory is removed and the fixtures still held are
    released best effort, never the one whose release the signal stopped; the
    body's own teardown, which needs the stack unwound, is not run; then the
    process sends itself the same signal, so its parent sees a death by signal
    and no [at_exit] function runs. A run stopped before its last test finished
    updates no store and writes no correction. The previous handlers are
    restored when the run ends. A process a test forked inherits the handlers
    and not the run: a signal kills it as the default disposition would,
    silently.

    {b Retries.} A test with [retries = n] reruns while its outcome counts as
    failed, up to [n + 1] attempts, each a fresh frame and a truncated capture
    file. An attempt that kept a correction ({e Corrections} below) is the last
    whatever [n]: the next one would be compared with the text it recorded. The
    recorded result carries the final attempt's failures, with its output tail
    attached to the first failure entry, and the attempt count. Skips are never
    retried.

    {b Expected failures.} A test marked {!Test_tree.xfail} still runs. An
    expected failure keeps its failures but does not stop the run under [-x],
    enters no store entry and leaves the exit code alone; an unexpected pass
    does all three and is recorded as [Fail] with one message failure. Skips are
    unaffected.

    {b Selection.} A test runs iff its path contains [config.filter] (when set),
    does not contain [config.exclude] (when set), its tags satisfy
    [--tag]/[--exclude-tag] over {!Test_tree.Tag.any}, it survives the
    [--failed] store and the caller's [?allowlist], it falls in the requested
    [--shard] bucket, and, when any focused node exists, it is focused.
    Deselected tests do not execute and are not recorded.

    {b Sharding.} [--shard K/N] partitions the suite into [N] buckets by a
    deterministic hash of each test's full path ({!Seed.derive} under a frozen
    root) and selects bucket [K]. Renaming or regrouping a test may move it. The
    bucket applies to the already-filtered set; an empty shard exits [2].

    {b Corrections.} A baseline check that fails in {!Baseline.Corrected} or
    {!Baseline.Update} mode records a correction. After every attempt the
    executor keeps the attempt's corrections iff every failure of the attempt is
    a baseline failure and the attempt did not skip ({!Baseline.settle}); in
    every mode the baseline failures of an attempt that does not meet the rule
    are recorded with [withheld] set ({!Failure.with_withheld}), so a report
    offers no acceptance for them. An {!Test_tree.xfail} test checks read-only
    in every mode. After the last test the kept corrections are written once
    ({!Baseline.write}), before any report. A test whose failures are all kept
    corrections is recorded as failed but leaves the exit code alone. A
    correction that could not be written ({!Baseline.refusals}) fails the run.

    {b The last-failed store} lives at [<log_dir>/<suite>/.last-failed], written
    atomically ({!Os.atomic_write}) after every executing run; its format is
    unstable and an unrecognized file reads as empty. Executed tests update
    their entries (failed recorded once, passed and skipped cleared), entries a
    partial run did not reach survive, and a run that executed the whole suite
    drops entries whose paths no longer exist. Store I/O failures are ignored.
*)

type outcome = {
  run : t;
      (** The run record: results in execution order, every executed test's row
          then the verdict rows, plus the baseline registry ({!Baseline.writes}
          included). *)
  selected : Test_tree.case list;
      (** The selected tests in execution order, the [-l] listing data. Under
          [-x] some may not have executed. *)
  total : int;  (** Leaf tests in the declared suite, before selection. *)
  focus_active : bool;  (** [true] iff a focused node narrowed the selection. *)
  duration : float;  (** Wall-clock seconds from startup checks to release. *)
  exit_code : int;
      (** [1] when any recorded row counted as failed, except a test whose
          failures are all kept corrections, or when a correction could not be
          written; else [2] when no test executed; else [0]. A nonempty
          selection whose every test skipped exits [0]. *)
}
(** The type for completed runs: everything the report projects and the facade
    needs to exit. *)

val execute :
  ?on_event:(event -> unit) ->
  ?allowlist:string list ->
  config ->
  suite:string ->
  Test_tree.t list ->
  (outcome, startup_error) Stdlib.result
(** [execute config ~suite tests] runs [tests] as described above and is
    [Ok outcome], or [Error error] when a startup check refuses the run before
    anything executes. It prints nothing. [on_event] observes progress (default:
    ignore). [suite] names the run in the capture log directory and the
    last-failed store. [allowlist] narrows the selection to those exact full
    paths ({!Test_tree.path_to_string}), by intersection with every other
    selection layer.

    Effects: registers a process-wide [Stdlib.at_exit] exit guard on first call
    (never removed; inert while no run is active), reads [CI] via {!Os.in_ci},
    captures test output under [config.log_dir] (unless [config.stream]),
    rewrites the last-failed store, and writes the kept corrections
    ([.corrected] files or, under [-u], the files themselves). Raises
    [Invalid_argument] when called while a run is already active, and when
    [config.shard] violates [1 <= K <= N]. If [on_event] raises, the run aborts
    with that exception after a best-effort fixture release. *)

val list_selection :
  config ->
  suite:string ->
  Test_tree.t list ->
  (string list, startup_error) Stdlib.result
(** [list_selection config ~suite tests] is the full paths
    ({!Test_tree.path_to_string}) {!execute} would run, in declaration order,
    with nothing executed; [Error error] exactly when {!execute} would refuse.
    Effects: the startup ones only (the exit-guard registration,
    [Printexc.record_backtrace true], the [CI] and [--failed] store reads).
    Raises [Invalid_argument] as {!execute} does. *)
