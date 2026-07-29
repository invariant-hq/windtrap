(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The per-run record and the one ambient slot.

    All mutable run state lives in one {!type:t} created per [run] invocation
    (RFC Law 9, "no global mutable per-run state"): the resolved {!type:config},
    the capture state, the snapshot registry, the fixture cache, accumulated
    {!type:result}s, and the coverage seam field. Nothing here is a global; a
    later [run] in the same process builds a fresh record, which is what makes
    fixtures re-acquire and snapshot registries start empty.

    Test bodies reach that record through exactly one documented ambient slot —
    a single [ref] in this module, the only run-state [ref] in the library —
    holding what the process is currently executing: the run itself while
    {!with_active} brackets an executing run, overlaid by the {!type:frame} of
    the test attempt while {!with_frame} brackets an attempt. The facade's
    ambient operations ([output ()], [snapshot], [collect], fixture accessors,
    and the {{!section-body}test-body operations} below) read the slot with
    {!current_frame}/{!current} and dispatch on explicit state. When the slot
    holds no frame, reading it raises the assertions-outside-run error:
    [Invalid_argument] with a message explaining that the operation only works
    inside a test body executed by [run]. The runner is sequential, one domain
    (RFC "The runner"); nothing here is thread-safe.

    Fixture accessors are created with {!fixture} — the RFC "Resources"
    contract: per-run cache keyed by accessor identity, acquisition on first use
    inside the calling test's failure boundary, cached acquisition errors and
    skips (amendment C1), reverse-order release through {!release_fixtures}. *)

(** {1:config Configuration} *)

type config = {
  seed : Seed.seed;  (** The run's root seed (RFC Law 7). *)
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
          bucket [K] of [N] ({!Runner}, {e Selection}). Invariant [1 <= K <= N],
          validated by the CLI layer. *)
  quick : bool;  (** [-q]: drop {!Tag.slow}-tagged tests. *)
  failed_only : bool;  (** [--failed]: rerun only the last run's failures. *)
  list_only : bool;  (** [-l]: list selected tests without running them. *)
  bail : int option;
      (** [--bail N] ([-x] is [Some 1]): stop after [N] failures. *)
  stream : bool;
      (** [--stream]: run against the real descriptors instead of capturing
          (capture state is {!Capture.disabled}). *)
  update : Env.update;
      (** [-u]/[WINDTRAP_UPDATE]: the snapshot update request, merged from all
          sources but before the CI guard — the runner applies
          {!Snapshot.resolve_mode}. *)
  prune : bool;
      (** [--prune]/[WINDTRAP_PRUNE]: delete orphaned baselines after a full,
          clean update run. *)
  timeout : float option;  (** [--timeout]: default per-test limit, seconds. *)
  slow_threshold : float;
      (** [--slow-threshold]/[WINDTRAP_SLOW_THRESHOLD]: seconds a test not
          tagged ["slow"] may take before the compact renderer treats the run as
          noteworthy and warns (default [1.]; [0.] disables both). Invariant:
          finite and non-negative, validated by the CLI layer. *)
  prop_count : int option;  (** [--prop-count]: generated cases per property. *)
  junit : string option;  (** [--junit PATH]: also write JUnit XML to [PATH]. *)
  color : Env.color_mode;  (** [--color]/[WINDTRAP_COLOR]. *)
  columns : int option;
      (** [WINDTRAP_COLUMNS]: terminal width override for renderers. *)
  tail_errors : int option;
      (** [WINDTRAP_TAIL_ERRORS]: captured-output lines shown per failure. *)
  log_dir : string;  (** [-o]/[--output]: root directory for capture logs. *)
  allow_focus : bool;
      (** [WINDTRAP_ALLOW_FOCUS]: lift the CI guard on focused tests. *)
}
(** The type for resolved run configuration: one plain record the CLI layer
    populates by merging programmatic arguments, CLI flags, and environment
    mirrors in that precedence order (RFC "The runner", {e CLI}). Consumers read
    it from {!config}; nothing re-reads flags or the environment mid-run. *)

val default_config : unit -> config
(** [default_config ()] is the configuration with every field at its built-in
    default: no filters, no tags, all flags off, [color = Env.Auto],
    [slow_threshold = 1.], and no overrides. Effects: [seed] is drawn fresh from
    {!Seed.random} and [log_dir] is {!Path_ops.default_log_dir}[ ()]. *)

(** {1:runs Run records} *)

type t
(** The type for per-run records. Created by the runner at the start of a run
    and dead at its end; never reused. *)

val create : config -> capture:Capture.t -> snapshots:Snapshot.t -> t
(** [create config ~capture ~snapshots] is a fresh run record over the given
    capture state and snapshot registry, with an empty fixture cache, no
    results, and no coverage snapshot. The runner constructs [capture] (possibly
    {!Capture.disabled}) and [snapshots] (after applying the CI guard to
    [config.update]) before creating the record. *)

val config : t -> config
(** [config t] is the run's resolved configuration. *)

val capture : t -> Capture.t
(** [capture t] is the run's capture state. *)

val snapshots : t -> Snapshot.t
(** [snapshots t] is the run's snapshot registry. *)

(** {1:frames Per-test frames} *)

type frame
(** The type for per-attempt frames: the executing test's identity (path and
    declaration file), its accumulated failures, and the property context while
    a property body runs. The runner creates a fresh frame for every attempt;
    the frame's run gives ambient operations the capture, snapshot, and fixture
    state. *)

val frame :
  t -> path:string list -> file:string option -> loc:Loc.t option -> frame
(** [frame t ~path ~file ~loc] is a fresh frame for one attempt of the test at
    [path] (as flattened by [Test_tree.flatten]), declared in [file] — the
    snapshot-scoping input (RFC "Snapshots", resolution rule (2)) — at [loc];
    [loc] is the fallback attribution for failures recorded without a location.
*)

val run_of_frame : frame -> t
(** [run_of_frame frame] is the run record [frame] belongs to. *)

val path : frame -> string list
(** [path frame] is the executing test's full path, groups first. *)

val file : frame -> string option
(** [file frame] is the executing test's declaration file, when known. *)

val loc : frame -> Loc.t option
(** [loc frame] is the test's declaration location, as passed to {!frame}. *)

val srandom_used : frame -> bool
(** [srandom_used frame] is [true] iff the attempt called {!srandom}. The runner
    copies it into the result's {!result.srandom_root}, so a failing test's
    report can print the replay command for its stochastic draws. *)

val add_failure : frame -> Failure.t -> unit
(** [add_failure frame failure] appends [failure] to the attempt's failure list.
    The runner records one entry per phase that failed, already classified with
    {!Failure.with_phase} — a body failure and a teardown failure are two
    entries (RFC "Failure is data"). A [failure] whose location is [None] — its
    failing call sat in tail position, so {!Loc.capture} stopped at the runner's
    delimiter — is recorded with the test's declaration location instead, when
    the frame has one. Nested failures (a property failure's [inner]) are left
    untouched. *)

val failures : frame -> Failure.t list
(** [failures frame] is the attempt's failures in the order they were added. *)

val prop_context : frame -> Property.context option
(** [prop_context frame] is the running property's label context, or [None] when
    no property body is executing — the facade's [collect], [classify] and
    [cover] dispatch through it and error on [None]. *)

val with_prop_context : frame -> Property.context -> (unit -> 'a) -> 'a
(** [with_prop_context frame ctx fn] is [fn ()] with [ctx] installed as
    [frame]'s property context; the previous value is restored on return and on
    raise. The runner wraps each property body invocation with it. *)

(** {1:ambient The ambient slot} *)

val with_frame : frame -> (unit -> 'a) -> 'a
(** [with_frame frame fn] is [fn ()] with [frame] in the ambient slot; the
    previous slot value is restored on return and on raise. The runner wraps
    exactly the extent of one attempt — setup, body, and teardown — so ambient
    operations work in all three phases and nowhere else. *)

val with_active : t -> (unit -> 'a) -> 'a
(** [with_active t fn] is [fn ()] with [t] marked in the ambient slot as the
    executing run; the previous slot value is restored on return and on raise.
    The runner wraps the executing span of one run — events, test attempts,
    fixture release, store maintenance — so {!active} answers exactly when user
    callbacks may be running. {!with_frame} nests inside it. *)

val active : unit -> bool
(** [active ()] is [true] iff the ambient slot is occupied — a run is executing
    ({!with_active}), whether or not a test attempt is ({!with_frame}). The
    already-active refusals in [execute] and the facade's [run], and the
    runner's exit guard, dispatch on it. *)

val current_frame : unit -> frame
(** [current_frame ()] is the frame of the attempt currently executing.

    Raises [Invalid_argument] — the assertions-outside-run error — when no test
    is running: ambient operations ([output ()], [snapshot], [collect], fixture
    accessors) only work inside a test body executed by [run], not at module
    toplevel or after the run. *)

val current : unit -> t
(** [current ()] is {!run_of_frame} of {!current_frame} — the run record the
    facade's ambient wiring dispatches on.

    Raises [Invalid_argument] as {!current_frame} does. *)

val current_opt : unit -> frame option
(** [current_opt ()] is the frame in the ambient slot: [Some frame] while a test
    attempt executes and [None] otherwise — including between attempts of an
    executing run. *)

(** {1:body Test-body operations}

    Ambient operations for test bodies, dispatched through the slot; each raises
    the assertions-outside-run error ([Invalid_argument], see {!current_frame})
    when no test is running. The facade re-exports them. *)

val current_test : unit -> string list
(** [current_test ()] is the executing test's full path: enclosing group names
    root first, then the test's own name (amendment B9). Never empty; joined
    with {!Test_tree.path_to_string} it is exactly the string selection filters
    match. Stable across attempts of the same test. *)

val srandom : unit -> Random.State.t
(** [srandom ()] is a fresh pseudo-random state seeded from the run's root seed
    and the executing test's path — precisely, from
    [Seed.derive ~root ~path ~index:0] over the canonical joined path (amendment
    B10). Per RFC Law 7 the seed is a pure function of the printed root token
    and the test's path: replaying with [--seed] reproduces it, suite
    composition and filters never perturb it, and renaming or regrouping the
    test intentionally re-keys it. The derivation is frozen; the values drawn
    from the state additionally follow the stdlib's [Random] algorithm, which
    the OCaml version pins.

    Every call within the same test returns an identically seeded state — draw
    all of a test's randomness from one state rather than calling twice.
    Property tests should use [prop] and [Gen]; [srandom] serves plain tests
    that want stable stochastic inputs without hand-parsing [WINDTRAP_SEED].

    Calling it also marks the attempt (see {!srandom_used}): the runner records
    the root in the result's {!result.srandom_root} so a failing test's block
    can print the replay command. *)

val subtest : string -> (unit -> unit) -> unit
(** [subtest name fn] runs [fn ()] as a named sub-case of the executing test
    (amendment B13). If [fn] raises an assertion failure or any other non-fatal
    exception, the failure is appended to the test's failure list — labeled with
    the [" › "]-joined path of the test's name and the enclosing subtest names,
    carried in the failure's [msg] slot (prefixed to a user [~msg] when one is
    present) — and [subtest] {e returns}: siblings after a failing subtest still
    run, and the test fails at the end with every recorded entry. Subtests nest;
    labels compose ([test › outer › inner]).

    A skip and a timeout propagate — they abort the whole test (failures already
    recorded still fail it, and a timeout's failure is the runner's, unlabeled).
    Fatal exceptions ([Sys.Break], [Out_of_memory], [Stack_overflow]) propagate.
    Retries reset recorded subtest failures with the rest of the attempt's
    frame. Sub-cases are failure entries, not tests: they are not separately
    selectable by [-f]. Inside a property body a subtest failure bypasses the
    engine — the case completes unshrunk and the test fails with the recorded
    entries. [subtest] is a body operation: entries it records keep the
    {!Failure.Body} phase — calling it inside a bracket's setup or teardown
    still labels and records, but the entries are not re-phased. *)

(** {1:scratch Runner-owned scratch}

    Per-test temporary paths (amendment B9): created lazily under one scratch
    directory per test attempt, and removed by the runner after the test on
    every path where it regains control (RFC Law 8) — failure, skip, timeout,
    and a fatal exception unwinding the run included — so tests never hand-roll
    temp lifecycles. Paths are per attempt: a resource that must outlive the
    test (e.g. one acquired by a {!fixture}) must not live in them. *)

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
    [suffix] is appended to the file's basename (e.g. [".json"]; defaults to
    none), sanitized to a safe path component.

    Raises [Unix.Unix_error] if the file cannot be created. *)

val remove_temp : frame -> unit
(** [remove_temp frame] recursively removes the scratch directory backing
    [frame]'s {!temp_dir}/{!temp_file} paths, when one was created. Runner-side:
    called after every attempt, on every path where the runner regains control
    (RFC Law 8). Best-effort — removal errors are ignored, never raised (the
    paths live under the system temporary directory) — and idempotent; symbolic
    links are removed, never followed. *)

(** {1:fixtures Fixtures} *)

val fixture : ?teardown:('a -> unit) -> (unit -> 'a) -> unit -> 'a
(** [fixture ?teardown create] is the accessor for a run-scoped shared resource
    (RFC "Resources"). The accessor owns no state: creating it — typically at
    module toplevel, outside any run — runs nothing, and its cache lives in the
    current run's record, keyed by the accessor's identity.

    The first call in a run acquires with [create ()] — inside the calling
    test's failure boundary, so an exception from [create] fails that test — and
    registers [teardown] for end-of-run release; later calls in the same run
    return the cached value. A fixture whose acquisition raised caches the
    exception, and every later use in that run re-raises it with the original
    acquisition backtrace — no re-acquisition within a run. A {e skip} raised
    during acquisition is cached as a skip, not an error (amendment C1): the
    acquiring test skips with that reason, every later use of the accessor in
    the run skips with the same reason without re-acquiring, and nothing is
    registered for release — an unavailable optional resource (a GPU device, a
    missing tool) must not turn a run red. Because the cache is per run, a later
    [run] in the same process re-acquires; a fixture no selected test touches is
    never acquired.

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

    The runner calls this after the last test on every path where it regains
    control — including under [--bail] — and outside any per-test timeout (RFC
    Law 8). *)

(** {1:results Results} *)

type result = {
  path : string list;  (** The test's full path, groups first. *)
  outcome : Failure.outcome;  (** The classified outcome, failures inside. *)
  counted : bool;
      (** [true] iff the result counted as failed — the bit the runner drives
          retries, [--bail], the exit code, and the last-failed store from
          ({!Runner}, {e Expected failures}): an ordinary failure, or an [xfail]
          test's unexpected pass. [false] for passes, skips, and excused
          expected failures. Renderers classify a failing result from this bit
          and {!result.xfail} alone — an uncounted [Fail] is an excused expected
          failure — never by reconstructing runner decisions from failure
          messages. *)
  xfail : Test_tree.xfail option;
      (** The test's expected-failure annotation ({!Test_tree.case.xfail}),
          carried so renderers can name the expectation ([XFAIL] reasons, JUnit
          skip messages); [None] for unannotated tests. *)
  slow_tagged : bool;
      (** [true] iff the test carries the ["slow"] tag — its own or an
          ancestor's, as flattened into the case's tags. Such tests are exempt
          from the renderer's slow threshold everywhere. *)
  duration : float;
      (** The test's execution time in seconds, attempts summed. *)
  attempts : int;  (** Attempts executed: [1] plus retries used. *)
  prop_stats : Property.stats option;
      (** The property engine's bookkeeping (label distribution, coverage
          statuses) for property tests; [None] otherwise. *)
  srandom_root : Seed.seed option;
      (** [Some root] — the run's root seed — iff the test called {!srandom} on
          its recorded attempt; [None] otherwise. Renderers print the replay
          line of a failing stochastic test from it (RFC Law 7). *)
}
(** The type for per-test results, as recorded by the runner after a test
    completes. Renderers project the accumulated list (RFC Law 4); the record
    carries every fact rendering needs — outcome classification included — so no
    consumer re-derives runner decisions from tables or messages. *)

val record : t -> result -> unit
(** [record t result] appends [result] to the run's results. *)

val results : t -> result list
(** [results t] is the recorded results in execution order. *)

(** {1:coverage Coverage seam}

    Core windtrap's entire coverage coupling (RFC Law 12): at run end the runner
    snapshots in-process coverage — when instrumented code registered any — into
    the field below, and renderers project it like any other run data. No other
    coverage type or call appears in the core library. *)

type summary = {
  visited : int;  (** Instrumented blocks visited at least once. *)
  total : int;  (** Instrumented blocks in every registered file. *)
  siblings : bool;
      (** Whether other executables' [.coverage] dumps sit beside this process's
          dump destination at snapshot time: the numbers are then one
          executable's view of the code it links, not the project total, and
          renderers say so. The driver computes the fact when it snapshots
          coverage — a renderer projects the record and touches no filesystem.
      *)
}
(** The type for end-of-run coverage summaries. *)

val set_coverage : t -> summary -> unit
(** [set_coverage t summary] records the run's coverage snapshot. Called at most
    once, at run end. *)

val coverage : t -> summary option
(** [coverage t] is the recorded snapshot, or [None] when no coverage data was
    registered. *)
