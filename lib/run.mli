(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Test execution: the run configuration, the per-run record and its one
    ambient slot, and the sequential executor.

    A run is one {!execute} of a declared {!Test_tree.t} list under one
    {!type:config}. All mutable run state lives in one {!type:t} created per run
    — the resolved configuration, the capture state, the baseline registry, the
    fixture cache and the accumulated {!type:result}s — so a later run in the
    same process starts from a fresh record: fixtures re-acquire and baseline
    registries start empty.

    Test bodies reach that record through exactly one ambient slot — a single
    [ref] in this module, the only run-state [ref] in the library — holding what
    the process is currently executing: the run itself while {!with_active}
    brackets an executing run, overlaid by the {!type:frame} of the test attempt
    while {!with_frame} brackets an attempt. The facade's ambient operations
    ([output ()], [expect], [collect], fixture accessors and the
    {{!section-body}test-body operations} below) read the slot with
    {!current_frame}/{!current}. When the slot holds no frame, reading it raises
    the assertions-outside-run error: [Invalid_argument] with a message
    explaining that the operation only works inside a test body executed by
    [run]. The executor is sequential, one domain; nothing here is thread-safe.

    {!execute} prints nothing: progress streams through typed {!type:event}s and
    everything else is data in the returned {!type:outcome}, which [Report]
    projects. The {{!section-executing}executing} section states the per-test
    boundary, retries, expected failures, selection, sharding, corrections and
    the last-failed store. *)

(** {1:config Configuration} *)

type invocation = [ `Exe of string | `Mirrors ]
(** The type for hint invocation contexts: how a command hint spells a re-run of
    this suite. [`Exe cmd] is a command that re-runs this executable, which
    hints complete with CLI flags ([cmd --failed], [cmd -u],
    [cmd --seed … -f …]) — ["dune exec <path> --"] under dune and [argv.(0)],
    verbatim, standalone. [`Mirrors] means no command line re-runs this suite
    and hints spell [WINDTRAP_*] environment prefixes to [dune runtest] and
    acceptance as [dune promote] — the context of every run dune drives: the
    inline runner's, a [--corrected] run's, and the default. *)

(** The type for what a mutation-instrumented build is asked to do with the run.
    Resolved by the CLI layer from [--mutate] and [--arm], which it refuses
    together; acted on by [Mutate_loop], which refuses {!Loop} and {!Armed} by
    name in an executable that catalogues no mutant. *)
type mutation =
  | No_mutation  (** The ordinary run, mutants inert. *)
  | Loop of string list
      (** [--mutate[=PREFIX,…]]/[WINDTRAP_MUTATE]: run the mutation loop over
          the mutants whose recorded source path starts with one of the prefixes
          — every mutant when the list is empty, the bare flag. *)
  | Armed of string
      (** [--arm ID]/[WINDTRAP_MUTATE_ARM]: one ordinary run with mutant [ID]
          armed, the identifier unparsed —
          {!Windtrap_runtime.Mutate.id_of_string} owns that grammar. *)

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
      (** [--shard K/N]/[WINDTRAP_SHARD]: run only tests whose path hashes into
          bucket [K] of [N] (see {e Sharding} under
          {{!section-executing}executing}). Invariant [1 <= K <= N], validated
          by the CLI layer. *)
  failed_only : bool;  (** [--failed]: rerun only the last run's failures. *)
  bail : bool;  (** [-x]/[--fail-fast]: stop after the first counted failure. *)
  stream : bool;
      (** [--stream]: run against the real descriptors instead of capturing
          (capture state is {!Capture.disabled}). *)
  baseline : Baseline.mode;
      (** [-u] ({!Baseline.Update}), [--corrected] ({!Baseline.Corrected}) or
          neither ({!Baseline.Check}): what the run writes for a baseline that
          differs. Neither flag has a mirror. The executor refuses
          {!Baseline.Update} under [CI]. *)
  timeout : float option;  (** [--timeout]: default per-test limit, seconds. *)
  prop_count : int option;  (** [--prop-count]: generated cases per property. *)
  log_dir : string;
      (** [-o]/[--output]: root directory for capture logs and the last-failed
          store ({!Path_ops.default_log_dir} when neither is given). *)
  allow_focus : bool;
      (** Lift the CI guard on focused tests. No flag and no mirror sets it:
          only {!for_subset}, for a forked mutation child whose parent already
          cleared the guard. *)
  color : Env.color_mode;
      (** [--color]/[WINDTRAP_COLOR]: the color preference, resolved against the
          report's sink ({!Env.resolve_color}) by whoever builds it. *)
  slow_threshold : float;
      (** [--slow-threshold]/[WINDTRAP_SLOW_THRESHOLD]: seconds a test not
          tagged ["slow"] may take before the report warns ([0.] disables).
          Invariant: finite and non-negative, validated by the CLI layer. *)
  verbose : bool;
      (** [-v]/[WINDTRAP_VERBOSE]: one status line per test instead of the
          compact transcript. *)
  junit : string option;
      (** [--junit]/[WINDTRAP_JUNIT]: where a JUnit report is also written, a
          file or a directory; [None] for no report. *)
  mutation : mutation;
      (** [--mutate]/[WINDTRAP_MUTATE] and [--arm]/[WINDTRAP_MUTATE_ARM]: what
          the mutation loop makes of the run. Read by [Mutate_loop] alone;
          {!for_subset} clears it. *)
  github : bool;
      (** Whether the report is written for GitHub Actions — the [::group::]
          envelope and the [::error::] annotations ({!Env.in_github_actions}).
      *)
  invocation : invocation;
      (** The hint context every acceptance and replay line derives from.
          {!Cli.settings} leaves it [`Mirrors]; the facade computes it from
          [argv] at run entry. *)
}
(** The type for run configuration: everything one invocation resolves, as one
    record the CLI layer populates with the precedence CLI > environment >
    default ({!Cli.settings}). The executor reads the selection and execution
    fields; [color], [slow_threshold], [verbose], [junit], [github] and
    [invocation] are read by [Report] alone and cannot change outcomes or exit
    codes; [mutation] is the loop's. Nothing re-reads flags or the environment
    mid-run. *)

val default_config : unit -> config
(** [default_config ()] is the configuration with every field at its built-in
    default: no selection, every flag off, [Baseline.Check], [color = Env.Auto],
    [slow_threshold = 1.], [mutation = No_mutation], [invocation = `Mirrors].
    Effects: [seed] is drawn fresh from {!Seed.random} and [log_dir] is
    {!Path_ops.default_log_dir}[ ()]. *)

val for_subset : config -> log_dir:string -> bail:bool -> config
(** [for_subset config ~log_dir ~bail] is [config] adjusted for a run over a
    {e subtree} of its own selection — the mutation loop's forked children.
    Path-selecting knobs ([filter], [exclude], [shard], [failed_only]) are
    cleared, because the caller's {!execute} allowlist {e is} that selection and
    applying them again could only narrow it further; tag-selecting knobs
    ([tags], [exclude_tags]) and the root [seed] are kept verbatim, because an
    allowlist cannot express a tag and per-case seeds derive from
    [(root, path, index)]. Checking is made read-only ([baseline = Check]),
    capture is dropped ([stream]), an in-source focus is allowed, no JUnit
    report is written, the child is {!No_mutation} (its parent is the loop, and
    it arms what it is handed), and [log_dir] and [bail] are the caller's.

    A new selection knob that this function does not clear gives such a child a
    selection its parent's tree already applied, which is how a deterministic
    suite comes to look non-deterministic. *)

(** {1:runs Run records} *)

type t
(** The type for per-run records. Created by {!execute} at the start of a run
    and dead at its end; never reused. *)

val create : config -> capture:Capture.t -> baselines:Baseline.t -> t
(** [create config ~capture ~baselines] is a fresh run record over the given
    capture state and baseline registry, with an empty fixture cache and no
    results. {!execute} constructs [capture] (possibly {!Capture.disabled}) and
    [baselines] (after applying the CI guard to [config.baseline]) before
    creating the record. *)

val config : t -> config
(** [config t] is the run's resolved configuration. *)

val capture : t -> Capture.t
(** [capture t] is the run's capture state. *)

val baselines : t -> Baseline.t
(** [baselines t] is the run's baseline registry. *)

(** {1:frames Per-test frames} *)

type frame
(** The type for per-attempt frames: the executing test's identity (path and
    declaration file), its accumulated failures, and the property context while
    a property body runs. The executor creates a fresh frame for every attempt;
    the frame's run gives ambient operations the capture, baseline, and fixture
    state. *)

val frame :
  ?corrections:bool -> t -> path:string list -> loc:Loc.t option -> frame
(** [frame t ~path ~loc] is a fresh frame for one attempt of the test at [path]
    (as flattened by {!Test_tree.flatten}), declared at [loc] — the fallback
    attribution for failures recorded without a location. [corrections] is
    whether the attempt may record baseline corrections ({!check_baseline});
    defaults to [true]. The executor clears it for an [xfail] test, whose
    mismatch is the failure it expects. *)

val run_of_frame : frame -> t
(** [run_of_frame frame] is the run record [frame] belongs to. *)

val path : frame -> string list
(** [path frame] is the executing test's full path, groups first. *)

val loc : frame -> Loc.t option
(** [loc frame] is the test's declaration location, as passed to {!frame}. *)

val add_failure : frame -> Failure.t -> unit
(** [add_failure frame failure] appends [failure] to the attempt's failure list.
    The executor records one entry per phase that failed, already classified
    with {!Failure.with_phase} — a body failure and a teardown failure are two
    entries (failures are data). A [failure] whose location is [None] — its
    failing call sat in tail position, so {!Loc.capture} stopped at the
    executor's delimiter — is recorded with the test's declaration location
    instead, when the frame has one. Nested failures (a property failure's
    [inner]) are left untouched. *)

val failures : frame -> Failure.t list
(** [failures frame] is the attempt's failures in the order they were added. *)

val prop_context : frame -> Property.context option
(** [prop_context frame] is the running property's label context, or [None] when
    no property body is executing — the facade's [collect], [classify] and
    [cover] dispatch through it and error on [None]. *)

val with_prop_context : frame -> Property.context -> (unit -> 'a) -> 'a
(** [with_prop_context frame ctx fn] is [fn ()] with [ctx] installed as
    [frame]'s property context; the previous value is restored on return and on
    raise. The executor wraps each property body invocation with it. *)

(** {1:ambient The ambient slot} *)

val with_frame : frame -> (unit -> 'a) -> 'a
(** [with_frame frame fn] is [fn ()] with [frame] in the ambient slot; the
    previous slot value is restored on return and on raise. The executor wraps
    exactly the extent of one attempt — setup, body, and teardown — so ambient
    operations work in all three phases and nowhere else. *)

val with_active : t -> (unit -> 'a) -> 'a
(** [with_active t fn] is [fn ()] with [t] marked in the ambient slot as the
    executing run; the previous slot value is restored on return and on raise.
    The executor wraps the executing span of one run — events, test attempts,
    fixture release, store maintenance — so {!active} answers exactly when user
    callbacks may be running. {!with_frame} nests inside it. *)

val active : unit -> bool
(** [active ()] is [true] iff the ambient slot is occupied — a run is executing
    ({!with_active}), whether or not a test attempt is ({!with_frame}). The
    already-active refusals in {!execute} and the facade's [run], and the exit
    guard, dispatch on it. *)

val active_run_error : string
(** [active_run_error] is what every already-active refusal raises
    [Invalid_argument] with. Three checks are separately load-bearing — the
    executor's two halves, and the facade's, which must fire before [Cli.parse]
    can exit on [--help] — and the sentence a nested [run] gets must not depend
    on which one saw it first. *)

val current_frame : unit -> frame
(** [current_frame ()] is the frame of the attempt currently executing.

    Raises [Invalid_argument] — the assertions-outside-run error — when no test
    is running: ambient operations ([output ()], [expect], [collect], fixture
    accessors) only work inside a test body executed by [run], not at module
    toplevel or after the run. *)

val current : unit -> t
(** [current ()] is {!run_of_frame} of {!current_frame} — the run record the
    facade's ambient wiring dispatches on.

    Raises [Invalid_argument] as {!current_frame} does. *)

(** {1:body Test-body operations}

    Ambient operations for test bodies, dispatched through the slot; each raises
    the assertions-outside-run error ([Invalid_argument], see {!current_frame})
    when no test is running. The facade re-exports them. *)

val current_test : unit -> string list
(** [current_test ()] is the executing test's full path: enclosing group names
    root first, then the test's own name. Never empty; joined with
    {!Test_tree.path_to_string} it is exactly the string selection filters
    match. Stable across attempts of the same test. *)

val subtest : string -> (unit -> unit) -> unit
(** [subtest name fn] runs [fn ()] as a named sub-case of the executing test. If
    [fn] raises an assertion failure or any other non-fatal exception, the
    failure is appended to the test's failure list — labeled with the
    [" › "]-joined path of the test's name and the enclosing subtest names,
    carried in the failure's [msg] slot (prefixed to a user [~msg] when one is
    present) — and [subtest] {e returns}: siblings after a failing subtest still
    run, and the test fails at the end with every recorded entry. Subtests nest;
    labels compose ([test › outer › inner]).

    A skip and a timeout propagate — they abort the whole test (failures already
    recorded still fail it, and a timeout's failure is the executor's,
    unlabeled). Fatal exceptions ([Sys.Break], [Out_of_memory],
    [Stack_overflow]) propagate. Retries reset recorded subtest failures with
    the rest of the attempt's frame. Sub-cases are failure entries, not tests:
    they are not separately selectable by [-f]. Inside a property body a subtest
    failure bypasses the engine — the case completes unshrunk and the test fails
    with the recorded entries. [subtest] is a body operation: entries it records
    keep the {!Failure.Body} phase — calling it inside a bracket's setup or
    teardown still labels and records, but the entries are not re-phased. *)

(** {1:baselines Baselines} *)

val check_baseline : ?loc:Loc.t -> Baseline.subject -> string -> unit
(** [check_baseline subject actual] is {!Baseline.check} against the executing
    test's registry, read-only when the frame was created without [corrections]
    ({!frame}), [loc] the failure's location: the literal's position, or the
    call frame for a file, and none when the call sat in tail position — the
    executor then attributes the failure to the test's declaration.

    A checkpoint, not an assertion: a {!Failure.Missing} or {!Failure.Mismatch}
    failure is recorded on the frame, labeled as {!subtest} labels one, and the
    call returns, so the body continues and the attempt fails at its end with
    every mismatch reported. Only {!Failure.Unresolvable} raises
    {!Failure.Check_failure}: nothing after an unprovable path is meaningful.
    Raises the assertions-outside-run error ([Invalid_argument], see
    {!current_frame}) when no test is running. *)

(** {1:scratch Executor-owned scratch}

    Per-test temporary paths: created lazily under one scratch directory per
    test attempt, and removed by the executor after the test on every path where
    it regains control — failure, skip, timeout, and a fatal exception unwinding
    the run included — so tests never hand-roll temp lifecycles. Paths are per
    attempt: a resource that must outlive the test (e.g. one acquired by a
    {!fixture}) must not live in them. *)

val temp_dir : ?prefix:string -> unit -> string
(** [temp_dir ()] is a fresh empty directory for the executing test, created
    with permissions [0o700] in the attempt's scratch directory (itself created
    on demand under the system temporary directory) and removed as the section
    preamble describes. Each call returns a new directory. [prefix] is the
    directory's basename prefix (defaults to ["dir"]), sanitized to a safe path
    component.

    Raises [Unix.Unix_error] if the directory cannot be created — inside a test
    this fails the test. *)

val temp_file : ?suffix:string -> unit -> string
(** [temp_file ()] is the path of a fresh empty file, created with permissions
    [0o600] in the executing test's scratch directory and removed with it.
    [suffix] is appended to the basename (e.g. [".json"]; defaults to none),
    sanitized to a safe path component.

    Raises [Unix.Unix_error] if the file cannot be created. *)

val remove_tree : string -> unit
(** [remove_tree path] removes [path] and everything under it, best effort:
    [lstat] so a symbolic link is removed rather than followed, and every
    filesystem error swallowed — a scratch cleanup must not fail a test or mask
    its outcome. Exposed for the mutation loop, which removes each forked
    child's log directory from the parent: a child killed at its deadline never
    runs its own cleanup, and an orphaned capture tree is exactly the trace Law
    16(e) forbids. *)

(** {1:process Executor-restored process state}

    The environment and the working directory belong to the process, not to the
    test: nothing scopes them but putting them back. So a test body records what
    it changed and the executor undoes it at the attempt boundary ({!reclaim}) —
    on every outcome, and per attempt, on the same terms as the scratch paths
    above. Both are process-global while the test runs: a thread the test spawns
    sees them, and a change made from such a thread races the restoration. *)

val setenv : string -> string option -> unit
(** [setenv name (Some value)] binds the environment variable [name] to [value]
    for the rest of the executing test; [setenv name None] unbinds it
    ({!Env.set}, so an unbinding is a real one — [Sys.getenv_opt] answers
    [None], not [Some ""]). The executor restores the prior state of [name] when
    the attempt ends.

    What is restored is what [name] held before the attempt's {e first} [setenv]
    of it: later calls with the same name change the binding without touching
    the restore record, so a test that binds a variable twice still leaves
    behind what it found, and a variable that was unbound is unbound again. A
    restoration that fails is reported at the call that made the change.

    Raises the assertions-outside-run error ([Invalid_argument], see
    {!current_frame}) when no test is running — the frame is read before the
    process is touched, so nothing is bound that nothing would undo — and
    [Invalid_argument] for a name {!Env.set} refuses. *)

val chdir : string -> unit
(** [chdir dir] changes the process's working directory to [dir] for the rest of
    the executing test ([Unix.chdir]). The executor restores the directory
    captured at the attempt's first [chdir] when the attempt ends; later calls
    move the process without changing what is restored. A restoration that fails
    is reported at the call that made the change.

    Raises the assertions-outside-run error ([Invalid_argument], see
    {!current_frame}) when no test is running, and [Unix.Unix_error] when [dir]
    cannot be entered — inside a test that fails the test. *)

val reclaim : frame -> unit
(** [reclaim frame] undoes the attempt's ambient changes and removes its
    scratch: it restores the working directory captured by {!chdir}, then the
    prior bindings recorded by {!setenv}, then recursively removes the scratch
    directory backing [frame]'s {!temp_dir}/{!temp_file} paths, when one was
    created. The directory goes back first, so removing the scratch tree cannot
    strand the process inside it.

    Called by the executor after every attempt, outside the timeout window, on
    every path where it regains control. Never raises, and idempotent. Scratch
    removal is best-effort — errors are ignored, the paths live under the system
    temporary directory — and symbolic links are removed, never followed. A
    restoration that {e fails}, by contrast, is recorded on [frame] as a
    {!Failure.Teardown}-phase message failure located at the change that could
    not be undone: the scratch a run leaks is inert, while a process left in the
    wrong directory or holding a test's binding fails everything after it for
    reasons that name the wrong test. Because it is recorded on the frame, the
    executor picks it up with the attempt's other failures. *)

(** {1:fixtures Fixtures} *)

val fixture : ?teardown:('a -> unit) -> (unit -> 'a) -> unit -> 'a
(** [fixture ?teardown create] is the accessor for a run-scoped shared resource.
    The accessor owns no state: creating it — typically at module toplevel,
    outside any run — runs nothing, and its cache lives in the current run's
    record, keyed by the accessor's identity.

    The first call in a run acquires with [create ()] — inside the calling
    test's failure boundary, so an exception from [create] fails that test — and
    registers [teardown] for end-of-run release; later calls in the same run
    return the cached value. A fixture whose acquisition raised caches the
    exception, and every later use in that run re-raises it with the original
    acquisition backtrace — no re-acquisition within a run. A {e skip} raised
    during acquisition is cached as a skip, not an error: the acquiring test
    skips with that reason, every later use of the accessor in the run skips
    with the same reason without re-acquiring, and nothing is registered for
    release — an unavailable optional resource (a GPU device, a missing tool)
    must not turn a run red. Because the cache is per run, a later [run] in the
    same process re-acquires; a fixture no selected test touches is never
    acquired.

    Calling the accessor outside a run raises the assertions-outside-run error
    ([Invalid_argument], see {!current_frame}). *)

val release_fixtures : t -> announce:(string -> unit) -> Failure.t list
(** [release_fixtures t ~announce] releases every fixture acquired in [t] in
    reverse acquisition order and drains the registry — a second call releases
    nothing. For each fixture that acquired successfully and has a teardown,
    [announce name] is called {e before} its teardown runs, so a hanging release
    is attributable; [name] identifies the fixture by its declaration site (e.g.
    ["fixture (test/test_users.ml:12)"], or ["fixture #<n>"] when no location
    was captured). Fixtures without a teardown, and fixtures whose acquisition
    failed, release nothing and are not announced.

    A teardown that raises contributes a {!Failure.Release}-phase failure —
    located at the fixture's declaration site — to the returned list (release
    order), and the remaining releases still run; the caller reports these
    failures and they make the run exit 1. [Sys.Break], [Out_of_memory] and
    [Stack_overflow] are re-raised immediately instead, abandoning the remaining
    releases.

    The executor calls this after the last test on every path where it regains
    control — including under [-x] — and outside any per-test timeout. *)

(** {1:results Results} *)

(** The type for what a result row reports on. The executor records one {!Test}
    row per executed test and — because every sink projects the one recorded
    list — one row per end-of-run verdict that no test owns: a fixture-release
    failure. Consumers that reason about tests (mutation verdicts, the
    last-failed store, full-run detection) dispatch on this field, never on the
    reporting path: a test whose name spells a verdict label must not alias a
    verdict row. *)
type subject =
  | Test  (** A declared test the executor executed. *)
  | Fixture_release
      (** An end-of-run fixture teardown that raised ({!release_fixtures}): one
          row per failure, recorded when the release runs, carrying the
          {!Failure.Release}-phase failure. *)

val fixture_release_path : string list
(** [fixture_release_path] is [["fixture release"]] — the reporting path of
    {!Fixture_release} rows: one component, because no test owns a release.
    Exported for the mutation loop's verdict vocabulary; row consumers dispatch
    on {!result.subject}, never on this label. *)

type result = {
  path : string list;
      (** The row's reporting path: the test's full path (groups first) for a
          {!Test} row, the verdict's one-component label otherwise
          ({!fixture_release_path}). *)
  subject : subject;  (** What the row reports on; see {!type:subject}. *)
  outcome : Failure.outcome;  (** The classified outcome, failures inside. *)
  counted : bool;
      (** [true] iff the result counted as failed — the bit the executor drives
          retries, [-x], the exit code, and the last-failed store from (see
          {e Expected failures} under {{!section-executing}executing}): an
          ordinary failure, or an [xfail] test's unexpected pass. [false] for
          passes, skips, and excused expected failures. Renderers classify a
          failing result from this bit and {!result.xfail} alone — an uncounted
          [Fail] is an excused expected failure — never by reconstructing
          executor decisions from failure messages. *)
  xfail : Test_tree.xfail option;
      (** The test's expected-failure annotation ({!Test_tree.case.xfail}),
          carried so renderers can name the expectation ([XFAIL] reasons, JUnit
          skip messages); [None] for unannotated tests. *)
  slow_tagged : bool;
      (** [true] iff the test carries the ["slow"] tag — its own or an
          ancestor's, as flattened into the case's tags. Such tests are exempt
          from the report's slow threshold everywhere. *)
  duration : float;
      (** The test's execution time in seconds, attempts summed. *)
  attempts : int;
      (** Attempts executed: [1] plus retries used. A passing row with
          [attempts > 1] is a {e flaky} test — it failed and then passed — and
          the report says so. *)
  prop_stats : Property.stats option;
      (** The property engine's bookkeeping (label distribution, coverage
          statuses) for property tests; [None] otherwise. *)
}
(** The type for result rows, as recorded by the executor — one per completed
    test, plus the end-of-run verdict rows (see {!type:subject}). Verdict rows
    are counted [Fail] rows with no annotation, no attempts beyond the first,
    and zero duration. Renderers project the accumulated list; the record
    carries every fact rendering needs — outcome classification included — so no
    consumer re-derives executor decisions from tables or messages. *)

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
    records its {!Property.stats} in {!result.prop_stats}. *)

(** {1:events Events} *)

(** The type for progress events, emitted in execution order. Observers receive
    only data already decided — immutable projections (counts, identities,
    recorded results), never the live run record — so no observer can alter
    status, counts, or scheduling. The run handle belongs to whoever owns the
    session: it is read off the returned {!type:outcome}, never off an event. *)
type event =
  | Run_started of { suite : string; total : int; selected : int }
      (** Startup checks passed; [selected] of the suite's [total] tests are
          about to run. *)
  | Test_started of { path : string list }
      (** The test at [path] is about to run its first attempt. *)
  | Test_finished of result
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
    {!No_recorded_failures} (nothing ran, so [2]), [1] for the others (a refused
    run is a failed run, not an empty one). *)

val startup_message : startup_error -> string
(** [startup_message error] is a plain-text (no ANSI) explanation of [error] for
    users. Not stable for programmatic matching. *)

(** {1:executing Executing}

    {!execute} applies the startup checks (duplicate paths, the CI focus guard,
    the baseline CI guard, the [--failed] store), selects tests, runs them one
    at a time in declaration order (one process, one domain, sequential),
    releases fixtures, maintains the last-failed store, writes the baseline
    corrections the run kept, and computes the [0]/[1]/[2] exit code.

    {b The per-test boundary.} Every attempt gets a fresh {!type:frame} in the
    ambient slot, the global [Random] state reseeded to a pure function of the
    test's path (saved and restored around the attempt), capture redirected into
    the test's log file ({!Capture.with_capture}, per-attempt truncation; a
    no-op under [--stream]), and a SIGALRM timeout arming the test's limit (else
    [config.timeout]). The timeout window covers setup, body, and teardown (for
    a scoped test, the whole scope call), re-armed before teardown for whatever
    remains — or for a fresh limit when the earlier phases consumed it. It is
    Unix-only (a no-op on Windows) and cannot interrupt blocked C calls; the
    executor owns [SIGALRM] while a test with a limit runs. After every attempt
    — outside the timeout window, on every path where the executor regains
    control, a fatal exception included — the attempt is reclaimed ({!reclaim}).
    Outcomes are classified per phase: {!Failure.Check_failure} keeps its
    payload, {!Failure.Skip_test} skips the test, {!Failure.Timeout} becomes a
    failure of the phase it interrupted,
    [Sys.Break]/[Out_of_memory]/[Stack_overflow] re-raise after a best-effort
    fixture release, and any other exception becomes a {!Failure.Raise} failure
    carrying its backtrace.

    {b Scoped tests.} A {!Test_tree.Scoped} node is one call the executor does
    not control; [Windtrap.scoped] states the protocol enforced here: the body
    runs inside the callback, its failure is recorded {e before} being re-raised
    through [scope], a second entry is refused rather than served, and whatever
    else escapes is attributed by how far the callback got — [Setup] before it,
    [Teardown] after it returned. A {!Test_tree.bracket} is such a scope: body
    and teardown outcomes are captured independently, one failure-list entry per
    failed phase.

    {b Backtraces and exits.} {!execute} enables {!Printexc.record_backtrace}
    for the process and does not restore it. A user callback that calls [exit]
    does not terminate the process: the first {!execute} in a process registers
    a [Stdlib.at_exit] guard which, whenever an exit is attempted while a run is
    active ({!active}), re-arms itself and raises {!Failure.Exit_attempt} — the
    call is recorded as a [Message] failure of the phase that attempted it. The
    guard is inert in any other process (a forked child that calls [exit]
    terminates) and while no run is active.

    {b Retries.} A test with [retries = n] reruns while its outcome
    {e counts as failed}, up to [n + 1] attempts, each a fresh frame and a
    truncated capture file. The recorded result carries the {e final} attempt's
    failures and output tail and the attempt count ({!result.attempts}). Skips
    are never retried; the captured-output tail of a failed test is attached to
    its first failure entry.

    {b Expected failures.} A test marked {!Test_tree.xfail} still runs. An
    expected failure keeps its real failures but does not stop the run under
    [-x], enters no last-failed store entry and leaves the exit code alone; an
    unexpected pass does all three and is recorded as [Fail] with one message
    failure. Skips are unaffected. Each recorded result carries the decision
    ({!result.counted}) and the annotation ({!result.xfail}).

    {b Selection.} A test runs iff its path contains [config.filter] (when set),
    does not contain [config.exclude] (when set), its tags satisfy
    [--tag]/[--exclude-tag] over {!Tag.any}, it survives the [--failed] store
    and the caller's [?allowlist], it falls in the requested [--shard] bucket
    (when set), and — when any focused node exists — it is focused. Deselected
    tests do not execute and are not recorded. Fixture releases run after the
    last executed test on every path where the executor regains control,
    including under [-x] and after a fatal exception, announced through
    {!Fixture_release} before each teardown and outside any per-test timeout.

    {b Sharding.} [--shard K/N] partitions the suite into [N] buckets by a
    deterministic hash of each test's full path ({!Seed.derive} under a frozen
    constant root) and selects bucket [K]. Renaming or regrouping a test may
    move it between buckets. The bucket applies to the already-filtered set, and
    an empty shard exits [2] like any empty selection.

    {b Corrections.} A baseline check that fails in {!Baseline.Corrected} or
    {!Baseline.Update} mode records a correction ({!Baseline.check}). After
    every attempt the executor keeps the attempt's corrections iff every failure
    of the attempt is a baseline failure and the attempt did not skip
    ({!Baseline.settle}). An attempt of a test marked {!Test_tree.xfail} checks
    baselines read-only in every mode. After the last test the kept corrections
    are written once ({!Baseline.write}), before any report. A test whose
    failures are all kept corrections is recorded as failed like any other but
    leaves the exit code alone: under [--corrected] the [diff?] that follows is
    the verdict. A correction that could not be written ({!Baseline.refusals})
    fails the run.

    {b The last-failed store} lives at [<log_dir>/<suite>/.last-failed], written
    atomically ({!Atomic_file}) after every executing run. Its format is
    {e unstable} (an unrecognized file reads as empty). Executed tests update
    their entries — failed recorded once regardless of attempts, passed and
    skipped cleared — entries for tests a partial run did not reach survive, and
    a run that executed the whole suite drops entries whose paths no longer
    exist. Store I/O failures are ignored: the store only feeds [--failed]. *)

type outcome = {
  run : t;
      (** The run record: results in execution order — every executed test's
          row, then the end-of-run verdict rows ({!type:subject}) — plus the
          baseline registry (what it wrote, {!Baseline.writes}, included). Every
          sink projects this one list, so a verdict that sets the exit code is
          always visible in the report. *)
  selected : Test_tree.case list;
      (** The selected tests in execution order — the [-l] listing data. Under
          [-x] some may not have executed. *)
  total : int;  (** Leaf tests in the declared suite, before selection. *)
  focus_active : bool;
      (** [true] iff a focused node narrowed the selection — the facade warns on
          successful focused runs outside CI. *)
  duration : float;  (** Wall-clock seconds from startup checks to release. *)
  exit_code : int;
      (** [1] when any recorded row counted as failed ({!result.counted}) —
          except a test whose failures are all kept corrections — or when a
          correction could not be written; else [2] when no test executed (empty
          suite or empty selection); else [0] — a nonempty selection whose every
          test skipped exits [0], and so does a run whose only failures were
          expected. *)
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
    anything executes. It prints nothing. [on_event] observes progress (defaults
    to ignoring). [suite] names the run in the capture log directory and the
    last-failed store. [allowlist] narrows the selection to those exact full
    paths ({!Test_tree.path_to_string}), for a caller that already holds the set
    it wants — the mutation loop's children. It composes with every other
    selection layer by intersection, [--failed] included.

    Effects: registers a process-wide [Stdlib.at_exit] exit guard on first call
    (never removed; inert while no run is active), reads [CI] via {!Env},
    captures test output under [config.log_dir] (unless [config.stream]),
    rewrites the last-failed store, and writes the kept corrections through the
    baseline registry ([.corrected] files or, under [-u], the files themselves).
    Raises [Invalid_argument] when called while a run is already active (from a
    test body, the calling test fails with that error), and when [config.shard]
    violates [1 <= K <= N] — the CLI layer validates every layer it resolves, so
    only a hand-built configuration can trip this. If [on_event] raises, the run
    aborts with that exception — after a best-effort fixture release, like a
    fatal exception. *)

val list_selection :
  config ->
  suite:string ->
  Test_tree.t list ->
  (string list, startup_error) Stdlib.result
(** [list_selection config ~suite tests] is the full paths
    ({!Test_tree.path_to_string}) {!execute} would run, in declaration order:
    the startup checks and the selection, with nothing executed. [Error error]
    on a refused run, exactly when {!execute} would refuse — [--list] does not
    excuse a mistyped [--shard] or a missing [--failed] store.

    Effects: the startup ones only — the exit-guard registration,
    [Printexc.record_backtrace true], the [CI] read and the [--failed] store
    read. No capture, no log directory, no store rewrite, no baseline. Raises
    [Invalid_argument] as {!execute} does. *)
