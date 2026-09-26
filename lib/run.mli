(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Test execution: the configuration of a run, its record, the operations of
    the running test and the runner.

    {!execute} runs the selected tests of a {!Test_tree.t} list under one
    {!type-config}, one at a time and in one domain, and returns an
    {!type-outcome}. {!list_selection} makes the same selection and runs
    nothing. This module prints nothing. {!execute} gives its progress to an
    observer as {!type-event}s, and everything else is data of the outcome,
    which {!Report} renders.

    A run keeps its mutable state in one {!type-t}, which {!execute} creates. A
    later run in the same process has its own, so it acquires its fixtures again
    and starts from an empty baseline registry. The operations of
    {{!section-body}the running test} reach the record through
    {{!section-ambient}one ambient slot}. Nothing here is thread-safe. *)

(** {1:config Configuration} *)

type invocation = [ `Exe of string | `Mirrors ]
(** The type for the way a report spells a command that runs the suite again.
    - [`Exe cmd]: [cmd] runs this executable and a line appends its flags. It is
      [argv.(0)], or under dune a [dune exec <path> --]. [cmd] must be quoted
      for a shell, because a line prints it as it is.
    - [`Mirrors]: no command line runs the suite again. It is the context of a
      [--corrected] run, the inline runner's included, and of a run without
      [argv.(0)]. *)

(** The type for what a run does with the mutants of an executable built for
    mutation testing. *)
type mutation =
  | No_mutation  (** An ordinary run. The mutants are inert. *)
  | Loop of string list
      (** [--mutate[=PREFIX,…]]: the mutation loop, over the mutants whose
          recorded source path starts with one of the prefixes. The bare flag
          gives the empty list, which keeps every mutant. *)
  | Armed of string
      (** [--arm ID]: one ordinary run with the mutant [ID] armed. The
          identifier is kept as typed, and its grammar is that of
          [Windtrap_runtime.Mutate.id_of_string]. *)

type broadcast = {
  selection : bool;
      (** A mirror gave a filter, an exclusion, a tag or a shard, and the
          command line gave no [-f], bare pattern, [-e], [--tag],
          [--exclude-tag], [--shard] or [--failed]. *)
  mutate : bool;
      (** The {!Loop} came from [WINDTRAP_MUTATE], and the command line gave no
          [--mutate]. *)
}
(** The type for the parts of a configuration that the environment gave and the
    command line did not. *)

type config = {
  seed : Seed.seed;  (** [--seed]: the root seed of the run. *)
  filter : string list;
      (** [-f] and the bare patterns: keeps the tests whose path string contains
          one of them, and [[]] keeps every test (see
          {{!section-selection}selection}). *)
  exclude : string list;
      (** [-e]: drops the tests whose path string contains one of them. *)
  tags : string list;  (** [--tag]: the tags a test must carry, all of them. *)
  exclude_tags : string list;
      (** [--exclude-tag]: the tags a test must not carry, any of them. *)
  shard : (int * int) option;
      (** [--shard K/N]: keeps bucket [K] of [N]. {!execute} raises unless
          [1 <= K <= N], which {!Cli} guarantees. *)
  failed_only : bool;
      (** [--failed]: keeps the tests of
          {{!section-store}the last-failed store}. *)
  bail : bool;  (** [-x]: stops the run after the first counted failure. *)
  stream : bool;
      (** [--stream]: the tests write to the real descriptors, and the capture
          state of the run is {!Capture.disabled}. *)
  baseline : Baseline.mode;
      (** [-u] is {!Baseline.Update}, [--corrected] is {!Baseline.Corrected},
          and neither is {!Baseline.Check}. {!execute} refuses [Update] under
          [CI] (see {!Update_refused_in_ci}). *)
  timeout : float option;
      (** [--timeout]: the limit, in seconds, of a test that declares none, and
          [None] for no limit. It must be finite and positive, which {!Cli}
          guarantees and {!execute} does not check. *)
  prop_count : int option;
      (** [--prop-count]: the generated cases of a property that declares no
          [count] (see {!prop}). *)
  log_dir : string;
      (** [-o]: the root directory of the capture logs and of the last-failed
          store. {!execute} uses it as given, so a relative one follows a test
          that changes the working directory. *)
  allow_focus : bool;
      (** Whether a suite that holds a focused test runs under [CI] (see
          {!Focused_in_ci}). *)
  color : Os.color_mode;
      (** [--color]: the colour preference. Whoever builds a report resolves it
          against the sink of that report ({!Os.resolve_color}). *)
  slow_threshold : float;
      (** [--slow-threshold]: the seconds from which {!Report} lists a test as
          slow at the end of the run, and [0.] for no list. {!Report.create}
          raises unless it is finite and non-negative, which {!Cli} guarantees.
      *)
  verbose : bool;  (** [-v]: the report prints a line per test. *)
  junit : string option;
      (** [--junit]: a file, or a directory, where a JUnit report is written
          too. [None] writes none. *)
  mutation : mutation;  (** [--mutate] and [--arm]. *)
  github : bool;
      (** Whether the report is written for GitHub Actions, with its group and
          its annotations (see {!Report}). *)
  invocation : invocation;
      (** The way the report spells its commands. The caller that holds [argv]
          sets it. *)
  broadcast : broadcast;
      (** Which of the selection and the {!Loop} came from the environment
          alone. The executor does not read it. *)
}
(** The type for the configuration of a run: what one invocation resolves from
    its command line, its mirrors and the defaults, in that precedence (see
    {!Cli.settings}). It is resolved once, and nothing reads a flag or a mirror
    again while the run executes. {!Report} alone reads [color],
    [slow_threshold], [verbose], [junit], [github] and [invocation], so they
    change no outcome and no exit code. *)

val default_config : unit -> config
(** [default_config ()] is the configuration of a run given no flag: no
    selection, every boolean [false], {!Baseline.Check}, no limit and no count,
    [color = Os.Auto], [slow_threshold = 1.], {!No_mutation}, [`Mirrors] and
    nothing broadcast. Every call draws [seed] from {!Seed.random} and takes
    [log_dir] from {!Os.default_log_dir}. *)

val for_subset : config -> log_dir:string -> bail:bool -> config
(** [for_subset config ~log_dir ~bail] is [config] for a run over part of the
    selection of a run under [config].
    - [filter], [exclude], [shard] and [failed_only] are cleared, because the
      allowlist is that selection.
    - [tags], [exclude_tags] and [seed] are kept, so the child selects within
      the tags of its parent and draws the property cases its parent drew.
    - [baseline] is {!Baseline.Check}, [junit] is [None], [mutation] is
      {!No_mutation}, nothing is broadcast and [allow_focus] is [true].
    - [stream] is [false] and [log_dir] is the argument.
    - [bail] is the argument, and every other field is [config]'s.

    A selection field added to {!type-config} must be cleared here. A child
    otherwise applies a selection that the tree of its parent already applied,
    and a deterministic suite then looks non-deterministic. *)

(** {1:runs Run records} *)

type t
(** The type for run records. {!execute} creates one per run and never uses it
    again. It stays readable after the run, through {!outcome.run}, when no test
    can reach it any more. *)

val config : t -> config
(** [config t] is the configuration [t] was created with. *)

val capture : t -> Capture.t
(** [capture t] is the capture state of the run. *)

val baselines : t -> Baseline.t
(** [baselines t] is the baseline registry of the run. After the run,
    {!Baseline.writes} of it says what was written and what could not be. *)

(** {1:frames Frames} *)

type frame
(** The type for the frames of test attempts. A frame holds the path and the
    declaration site of the executing test, the failures added so far, the
    property context while a law runs, and what the end of the attempt undoes.
    The runner makes a fresh frame for every attempt, so a retry starts from
    none of it. *)

val prop_context : frame -> Property.context option
(** [prop_context frame] is the label context of the law that is running (see
    {!Property.context}), or [None] when no law is. An operation that labels a
    case reads it, and the error on [None] is that operation's. *)

(** {1:ambient The ambient slot}

    The slot is the one reference to run state in the library. It holds the run
    from the first event of {!execute} to the building of its outcome, so
    {!active} is [true] whenever a callback of the user may be running. Over the
    run it holds the frame of an attempt, for its setup, its body and its
    teardown and for nothing else. *)

val active : unit -> bool
(** [active ()] is [true] iff a run is executing, whether or not a test is
    running. It is [true] in an observer and in the release of a fixture, where
    {!current_frame} raises. {!execute} refuses to start on it, and the exit
    guard acts on it (see {{!section-exits}exits}). *)

val active_run_error : string
(** [active_run_error] is the message of the [Invalid_argument] that refuses a
    run while another executes. {!execute} and {!list_selection} raise with it.
    An entry point that can return before it reaches them, as one that parses
    [--help] first does, must check {!active} itself and raise with it. *)

val current_frame : unit -> frame
(** [current_frame ()] is the frame of the attempt that is running. Raises
    [Invalid_argument] if no test is running. *)

val current : unit -> t
(** [current ()] is the record of the run that the frame of {!current_frame}
    belongs to. Raises as {!current_frame} does. *)

(** {1:body The running test}

    Operations on the test that is executing, in its setup, its body and its
    teardown. Each reads {!current_frame} before it changes anything, so it
    raises that function's [Invalid_argument] when no test is running.
    {!remove_tree} reads nothing, and for {!fixture} the operation is the
    accessor. What an operation records on the frame lasts for the attempt, and
    the runner undoes it when the attempt ends (see
    {{!section-attempts}attempts}). *)

(* What a test's author is told about these operations, about [fixture] and
   about [prop] is stated in windtrap.mli. This file states what the other
   modules of the library rely on. *)

val current_test : unit -> string list
(** [current_test ()] is the path of the running test, outermost group first. It
    is the same in every attempt, and {!Test_tree.path_to_string} of it is the
    string that the filters match. *)

val subtest : string -> (unit -> unit) -> unit
(** [subtest name fn] runs [fn ()] as a named part of the running test. A
    {!Failure.Check_failure} of [fn] is added to the frame and [subtest]
    returns. Any other exception is added as a {!Failure.Raise} failure with its
    backtrace, located at the declaration of the test. Every {!Failure.Control}
    passes through, so a skip, a timeout or an [exit] ends the whole test, and
    an [assume] inside a subtest inside a law discards the case.

    An added failure carries its label as data, in its [subtest] field and never
    in its [msg]. The label is the name of the test, then the names of the open
    subtests, outermost first, as in [["test"; "outer"; "inner"]]. The phase of
    the failure stays {!Failure.Body}, in a setup and in a teardown too. Inside
    a law the engine sees a case that passed, so the failure is not shrunk and
    the attempt fails on the added entries. *)

val check_baseline : ?loc:Loc.t -> Baseline.subject -> string -> unit
(** [check_baseline ?loc subject actual] is {!Baseline.check} of [actual]
    against [subject] in the registry of the run, except that a mismatch is
    recorded and not raised. A {!Failure.Missing} or {!Failure.Mismatch} failure
    is added to the frame and the call returns. Inside a {!subtest} the failure
    is labelled as {!subtest} labels one. A {!Failure.Unresolvable} failure is
    raised as {!Failure.Check_failure}.

    [loc] is the location of the failure: the position of a literal, the end of
    the body for a {!Baseline.Trailing} text, or the site of the call for a
    file. Without it the failure takes the declaration site (see
    {{!section-attempts}attempts}). Under {!Baseline.Update} a baseline that
    differs records its correction and nothing fails.

    Raises [Sys_error] as {!Baseline.check} does, and [Invalid_argument] if no
    test is running. *)

(** {2:scratch Temporary paths}

    An attempt has one temporary directory, made under the system temporary
    directory with mode [0o700] by the first call below. The runner removes it
    when the attempt ends (see {{!section-attempts}attempts}), and when a signal
    ends the run. *)

val temp_dir : ?prefix:string -> unit -> string
(** [temp_dir ?prefix ()] is a fresh directory in the temporary directory of the
    attempt, with mode [0o700]. [prefix] starts its name, after
    {!Os.sanitize_component}, and defaults to ["dir"]. Raises [Unix.Unix_error]
    if a directory cannot be made, and [Invalid_argument] if no test is running.
*)

val temp_file : ?suffix:string -> unit -> string
(** [temp_file ?suffix ()] is the path of a fresh empty file in the same
    directory, with mode [0o600]. [suffix] ends its name, after
    {!Os.sanitize_component} unless it is empty, and defaults to [""]. Raises as
    {!temp_dir} does. *)

val remove_tree : string -> unit
(** [remove_tree path] removes [path] and what is under it, as far as it can. It
    removes a symbolic link without following it and ignores every file-system
    error, a missing [path] included, so it never raises. *)

(** {2:process Process state}

    The environment and the working directory belong to the process. {!setenv}
    and {!chdir} record on the frame what they change, and the runner puts it
    back when the attempt ends (see {{!section-attempts}attempts}). A signal
    that ends the run puts nothing back. *)

val setenv : string -> string option -> unit
(** [setenv name value] is {!Os.setenv}[ name value], after which the frame
    records what [name] held before. The frame keeps what the first [setenv] of
    [name] in the attempt found, and a later call leaves that record alone.

    Raises as {!Os.setenv} does. Raises [Invalid_argument] if no test is
    running, before the process is touched. *)

val chdir : string -> unit
(** [chdir dir] is [Unix.chdir dir]. The first [chdir] of the attempt records
    the working directory on the frame before it moves, and a later call leaves
    that record alone.

    Raises [Unix.Unix_error] if [dir] cannot be entered, [Sys_error] if the
    first call cannot read the current directory, and [Invalid_argument] if no
    test is running. *)

(** {2:fixtures Fixtures} *)

val fixture : ?teardown:('a -> unit) -> (unit -> 'a) -> unit -> 'a
(** [fixture ?teardown create] is an accessor for a resource that a run shares.
    The accessor holds no state. The record of the run caches the outcome of its
    first call, so nothing is acquired twice in a run.

    The first call is [create ()], inside the failure boundary of the calling
    test, and its outcome is a value, an exception with its backtrace, or a
    [Failure.Control (`Skip _)] with its reason. Only a value acquired with a
    [teardown] is registered for the {{!section-release}release} at the end of
    the run.

    The accessor is named [fixture (<file:line>)] after the site where [fixture]
    was applied, or [fixture #<n>] when {!Loc.capture} finds none. An
    {!event.Fixture_release} and the failure of a release carry that name.
    Raises [Invalid_argument] if the accessor is called while no test is
    running. *)

(** {1:results Results} *)

type result = {
  path : string list;  (** The path of the test, outermost group first. *)
  outcome : Failure.outcome;
      (** The outcome, with the failures of the last attempt. An attempt that
          skipped and also added a failure is a [Fail], and so is an [xfail]
          test that passed, with one message failure. *)
  counted : bool;
      (** [true] iff the row counts as failed. An attempt that fails counts iff
          the test has no {!Test_tree.val-xfail} annotation, one that passes iff
          it has one, and one that skips never. Retries, [config.bail] and the
          last-failed store follow this field, and {!outcome.exit_code} follows
          it with one exception. A [Fail] row that does not count is an expected
          failure, and a renderer classifies a failing row from this field and
          [xfail] alone. *)
  xfail : Test_tree.xfail option;
      (** The {!Test_tree.val-xfail} annotation of the test. [None] without one.
      *)
  slow_tagged : bool;
      (** [true] iff the test carries {!Test_tree.Tag.slow}, its own or a
          group's. *)
  duration : float;  (** The seconds the test took, its attempts summed. *)
  attempts : int;
      (** The attempts executed, [1] plus the retries used. A [Pass] row with
          [attempts > 1] is a flaky test. *)
  prop_stats : Property.stats option;
      (** The statistics of the property engine, its labels and its
          {!Property.cover} demands, for a property whose engine returned an
          outcome, which a timeout in a case does. [None] for any other test,
          and for a property that a skip ended, or a timeout outside its cases.
      *)
}
(** The type for result rows, one per executed test. A row carries every fact
    that a renderer needs, so no consumer derives a decision of the runner from
    a message. *)

val results : t -> result list
(** [results t] is the row of every test executed so far, in the order of
    execution. *)

(** {1:props Properties} *)

val property :
  ?loc:Loc.t ->
  ?count:int ->
  ?max_discard:int ->
  ?examples:'a list ->
  ?summary:('a -> string option) ->
  'a Gen.t ->
  ('a -> unit) ->
  unit
(** [property gen law] is the body of the test that {!prop} declares, [loc]
    being its declaration site. It returns [()] on a [Pass] and raises the
    failure of any other outcome as a [Failure.Check_failure] (see {!prop}).
    Raises as {!current_frame} does. *)

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
(** [prop name gen law] is a {!Test_tree.test} whose body, {!property}, runs
    {!Property.run} over [gen] and [law], then turns the {!Property.outcome}
    into the outcome of the test. It takes no [retries] of its own, and those of
    an enclosing group apply to it as to any test.
    - [__POS__], [tags] and [timeout] are {!Test_tree.test}'s. The property is
      one body, so [timeout] covers generation and shrinking together (see
      {!Property.run}).
    - [count] is the number of generated cases. A declared [count] wins over
      [config.prop_count], which wins over the default of the engine, and the
      engine is told which of the two it was given.
    - [max_discard], [examples] and [summary] are {!Property.run}'s.

    [prop] adds no tag, so a caller must add {!Test_tree.Tag.prop} to [tags].

    The engine draws from [config.seed] and from the path string of the test.
    Its context is the {!prop_context} of the frame while [law] runs. A [Fail]
    adds the {!Failure.Property} failure of the engine. [Coverage_failed] and
    [Gave_up] each add a message failure, located at the declaration of the
    test.

    Raises [Invalid_argument] as {!Test_tree.test} does. A negative [count] or
    [max_discard] makes {!Property.run} raise inside the body, which fails the
    test. *)

(** {1:events Events} *)

(** The type for progress events, given to the [on_event] of {!execute} in the
    order of execution. An event carries data that is already decided and never
    the record of the run, so an observer changes no status, no count and no
    order. The record is read off the {!type-outcome}. An observer can end the
    run, by raising (see {!execute}). *)
type event =
  | Run_started of {
      suite : string;
      total : int;
      selected : int;
      properties : bool;
    }
      (** The startup checks passed, and [selected] of the [total] tests of
          [suite] are about to run. It is the first event, given once, for an
          empty selection too. [properties] is [true] iff a selected test
          carries {!Test_tree.Tag.prop}. *)
  | Test_started of { path : string list }
      (** The test at [path] is about to run. It is given once per test, before
          its first attempt and before anything of the test runs. *)
  | Test_finished of result
      (** The test finished and its row is recorded. It is given once per test,
          after its last attempt ended and was undone. *)
  | Fixture_release of { name : string }
      (** The fixture [name] is about to be released (see
          {{!section-release}release}). The event precedes the teardown, so an
          observer can name a release that hangs. *)
  | Interrupted of {
      running : string list option;
      releasing : string option;
      results : result list;
      duration : float;
    }
      (** A signal is ending the run (see {{!section-signals}signals}).
          [running] is the path of the test it stopped, and [None] outside a
          test. [releasing] is the name of the fixture whose release it stopped,
          if any. [results] are the rows recorded so far, and [duration] the
          seconds the run took so far. It is the last event and it is given
          once. What the observer raises on it is ignored, and the process then
          dies by the signal. *)

(** {1:startup Startup errors} *)

(** The type for the refusals of a run, decided before any test executes. The
    checks run in the order of the constructors and the first that fails is
    returned. *)
type startup_error =
  | Duplicate_paths of string list
      (** Two tests have one path. The payload is the path strings concerned,
          sorted, each given once. *)
  | Focused_in_ci of Loc.t option list
      (** The suite holds a focused node and [CI] is set. The payload is the
          declaration sites of the focused nodes, in declaration order, a
          focused group counting once. The check reads the declared tree,
          whatever the selection, and [config.allow_focus] lifts it. *)
  | Update_refused_in_ci
      (** [config.baseline] is {!Baseline.Update} and [CI] is set. Nothing lifts
          it. *)
  | No_recorded_failures
      (** [config.failed_only] is set, and the last-failed store names no
          declared test that [allowlist] admits. The store is read against the
          declared paths, so a filter that keeps none of the recorded tests
          gives an empty selection and not this error. *)

val startup_exit_code : startup_error -> int
(** [startup_exit_code error] is the exit code of a run that [error] refused:
    [2] for {!No_recorded_failures}, since nothing ran, and [1] otherwise. *)

val startup_message : startup_error -> string
(** [startup_message error] explains [error] to a user in plain text. It is not
    stable enough for a program to match. *)

(** {1:executing Executing}

    {!execute} makes the {{!section-startup}startup checks}, selects the tests
    and runs them one at a time, in declaration order. It then
    {{!section-release}releases the fixtures}, updates the last-failed store,
    writes the kept corrections and computes the exit code. The subsections
    after {!list_selection} say what each step guarantees. *)

(* What a test's author is told about limits, retries, scopes, expected
   failures, corrections, exits and signals is stated in windtrap.mli. The
   subsections state the data and the order that the other modules rely on. *)

type outcome = {
  run : t;  (** The record of the run. *)
  selected : Test_tree.case list;
      (** The selected tests, in the order of execution. Under [config.bail]
          some may not have executed, and those have no row. *)
  total : int;  (** The tests that the suite declares, before any selection. *)
  focus_active : bool;
      (** [true] iff the suite holds a focused node, selected or not. *)
  release_failures : Failure.t list;
      (** The failures of the fixture releases that raised, in the order of
          release, each a {!Failure.Release} message failure located at the site
          of the fixture. No test owns them, so they are not rows. *)
  duration : float;
      (** The seconds the run took, from its startup checks to the writing of
          its corrections. *)
  exit_code : int;
      (** [1] when a row counts as failed, when a release failed, or when a
          correction could not be written (a {!Baseline.Refused} entry). A test
          whose failures are all kept corrections does not count here, because
          under [--corrected] the [diff?] that follows the run decides.
          Otherwise [2] when no test executed, in an empty suite and in an empty
          selection, and else [0]. A selection whose tests all skipped gives
          [0], and so does a run whose only failures were expected. A caller may
          return another code. *)
}
(** The type for finished runs: what a report renders, and what the caller needs
    to exit. *)

val execute :
  ?on_event:(event -> unit) ->
  ?allowlist:string list ->
  config ->
  suite:string ->
  Test_tree.t list ->
  (outcome, startup_error) Stdlib.result
(** [execute config ~suite tests] runs [tests] as this section says. It is
    [Ok outcome], or [Error error] when a startup check refuses the run, before
    anything executes.
    - [on_event] receives the {!type-event}s. Defaults to a function that
      ignores them.
    - [allowlist] keeps the tests whose path string is in the list, within every
      other layer of the {{!section-selection}selection}. Defaults to no
      narrowing.
    - [suite] names the directory of the capture logs and of the last-failed
      store, under [config.log_dir].

    [execute] reads [CI] once, at startup, and writes the capture logs unless
    [config.stream], the store and the kept corrections. On the process, it
    registers the exit guard and turns the recording of backtraces on (see
    {{!section-exits}exits}), handles three {{!section-signals}signals} while it
    runs, and owns the state that {{!section-attempts}attempts} names during
    each of them.

    An exception that [on_event] raises leaves [execute], except on
    {!Interrupted}, where it is ignored. The run then updates no store and
    writes no correction, and what becomes of the acquired fixtures depends on
    the event:
    - on {!Test_started} or {!Test_finished} they are released first, as far as
      they can be;
    - on an {!event.Fixture_release} during the release at the end of the run,
      the announced fixture and those not yet released stay unreleased.

    What {!Failure.catch} never returns, raised by a test, leaves [execute]
    after the acquired fixtures were released, as far as they can be, with no
    store updated and no correction written.

    Raises [Invalid_argument] with {!active_run_error} if a run is executing,
    which fails the calling test when a test body is the caller. Raises
    [Invalid_argument] if [config.shard] breaks [1 <= K <= N], which only a
    hand-built configuration does. *)

val list_selection :
  config ->
  suite:string ->
  Test_tree.t list ->
  (string list, startup_error) Stdlib.result
(** [list_selection config ~suite tests] is the path strings of the tests that
    {!execute} would run, in declaration order, and it runs nothing. It is
    [Error error] when {!execute} would refuse the run. Its effects are those of
    the startup alone: the exit guard, the recording of backtraces, the read of
    [CI] and, under [config.failed_only], of the store. It makes no log
    directory and rewrites no store. Raises [Invalid_argument] as {!execute}
    does. *)

(** {2:selection Selection}

    A test runs iff every layer below admits it. The layers intersect, and an
    absent one admits every test.
    - Its path string ({!Test_tree.path_to_string}) contains one pattern of
      [config.filter] and none of [config.exclude].
    - Its tags hold every tag of [config.tags] and none of
      [config.exclude_tags].
    - [allowlist] admits it, and under [config.failed_only] so does the
      last-failed store.
    - It falls in the bucket of [config.shard].
    - It is focused, when the suite holds a focused node.

    A test left out does not execute and has no row.

    The bucket of a test is a frozen hash of its path string, modulo [N]. It
    depends on nothing else, and a change to the hash moves the tests of every
    sharded suite. *)

(** {2:attempts Attempts}

    Around the three phases of an attempt the runner seeds the global [Random]
    state from the path of the test alone, whatever [config.seed], and restores
    the saved state afterwards. Unless [config.stream], it redirects the output
    into the log file of the test, truncated first ({!Capture.with_capture}). A
    capture that cannot be set up or restored is a {!Failure.Raise} failure of
    the body, and the run goes on.

    The limit of a test is its own, or else [config.timeout]. It is a [SIGALRM]
    timer over the three phases, so the runner owns that signal during the
    attempt, and it puts the previous handler back afterwards. On Windows no
    timer is set, and the limit is not enforced.

    The frame keeps every failure that is added to it, so a body failure and a
    teardown failure are two entries. What a phase raises becomes such a
    failure, with that phase set:
    - a {!Failure.Check_failure} keeps its payload;
    - a [`Timeout] is a failure of the phase it interrupted, and an [`Exit] one
      of the phase that called [exit];
    - a [`Discard], which only a property owns, is the message failure
      [assume or reject was called outside a property];
    - any other exception is a {!Failure.Raise} failure with its backtrace.

    A [`Skip] skips the test, and the first reason wins. A failure added without
    a location takes the declaration site of the test, which is the case of one
    raised from tail position, and a nested failure, as the [inner] of a
    property failure, is left as it is.

    What {!Failure.catch} never returns is not caught (see {!execute}). After
    every attempt, on every path where it regains control, the fatal one
    included, the runner undoes what the attempt changed, outside the window of
    the limit and outside the capture. It returns to the directory that {!chdir}
    recorded, then restores the bindings that {!setenv} recorded, then removes
    the temporary directory. A restoration that fails is added to the frame as a
    {!Failure.Teardown} message failure, located at the call that made the
    change, and so fails the test. *)

(** {2:scoped Scoped tests}

    The body of a {!Test_tree.Scoped} test runs inside the callback that the
    runner gives to [scope], and a {!Test_tree.bracket} is such a test. The
    runner adds the failure of the body before it raises it again through
    [scope], and does not add it a second time on its way out. Whatever else
    leaves [scope] is attributed by how far the callback got: {!Failure.Setup}
    before it was called, {!Failure.Teardown} after the body left it, and
    {!Failure.Body} in between.

    A second call of the callback runs nothing and returns, and the attempt then
    fails with a body message that gives the number of calls. A scope that
    returns without calling back fails the attempt with a setup message. A scope
    that raised or skipped instead has said what happened, and gets no such
    message. *)

(** {2:retries Retries}

    A test with [retries = n] runs again while its attempt counts as failed
    ({!result.counted}), up to [n + 1] attempts. An attempt that kept a
    correction is the last whatever [n], because the next one would be compared
    with the text it recorded. Its first failure carries the tail of what that
    attempt wrote ({!Capture.output_tail}), unless [config.stream]. *)

(** {2:corrections Corrections}

    Under {!Baseline.Corrected} and {!Baseline.Update} a check whose baseline
    differs or is missing records a correction ({!Baseline.check}). After every
    attempt the runner settles them ({!Baseline.settle}): it keeps them iff
    every failure of the attempt is a baseline failure and the attempt did not
    skip. In every mode, the baseline failures of an attempt that breaks that
    rule are marked with {!Failure.with_withheld}, so that no report offers to
    accept them. The mark is {!Failure.Skipped} when the skip alone broke the
    rule, and {!Failure.Failed_outside} otherwise. An [xfail] test checks
    without correcting, in every mode. The kept corrections are written once
    ({!Baseline.val-write}), after the last test, the release of the fixtures
    and the update of the store, and before {!execute} returns. The observer was
    given the failures that offer them earlier, as each test finished. *)

(** {2:release Fixture release}

    After the last test, under [config.bail] too and outside any limit, the
    runner releases the fixtures that the run acquired, the latest first. Before
    each teardown it gives an {!event.Fixture_release} to the observer.

    A teardown that raises adds a {!Failure.Release} message failure, located at
    the site of the fixture, and the releases after it still run. A fatal
    exception from a teardown leaves {!execute} at once and the fixtures not yet
    released are never released, as when the observer raises on the event (see
    {!execute}). *)

(** {2:store The last-failed store}

    The store is the file [<log_dir>/<suite>/.last-failed], with [suite] through
    {!Os.sanitize_component}. {!execute} rewrites it atomically
    ({!Os.atomic_write}) at the end of every run that passed its startup checks,
    a run that executed nothing included. {!list_selection} does not touch it.
    Nor does a run that an exception ended, or a {{!section-signals}signal}
    before this point.

    A test that counted as failed is recorded once, and an executed test that
    did not is cleared. The entry of a test that the run did not execute
    survives. A run that executed the whole declared suite also drops the
    entries of paths that no longer exist. The format is not stable, and a file
    that is not recognised reads as empty. Every I/O error is ignored, because
    the store only feeds [config.failed_only]. A store that cannot be opened or
    read, a directory at its path for example, reads as empty, and one that
    cannot be written keeps its content. *)

(** {2:exits Exits and backtraces}

    {!execute} and {!list_selection} turn the recording of backtraces on
    ([Printexc.record_backtrace]) and leave it on. They register an [at_exit]
    function, the exit guard.

    While a run is {!active}, a call to [exit] raises [Failure.Control `Exit]
    from the guard and the process does not end. The exception is classified
    where it lands. In a test it is a failure of the phase that called [exit],
    the acquisition of a fixture included. In the [teardown] of a fixture it is
    the failure of that release. An [exit] in an observer is an exception of
    that observer (see {!execute}).

    The guard does nothing while no run is active, and nothing in a process
    forked from the one that called {!execute}, where [exit] ends the child. A
    child that calls {!execute} itself becomes the owner of the guard. *)

(** {2:signals Signals}

    While a run executes, and not on Windows, {!execute} handles [SIGINT],
    [SIGTERM] and [SIGHUP], except one that the process was started with
    ignored, and it puts the previous handlers back when the run ends. On the
    first signal the three go back to their default disposition, so a second one
    kills at once. A signal that arrives while an attempt or the release of a
    fixture runs acts at once. One that arrives in the runner's own code, or in
    an observer, acts before the next test, after the last one, or after the
    corrections are written.

    The runner then, in order:
    + abandons the capture of the attempt ({!Capture.abandon}) and gives
      {!Interrupted} to the observer;
    + removes the temporary directory of the attempt, and releases the fixtures
      still held, with no event and never the one whose release was stopped;
    + sends the process the same signal, so its parent sees a death by signal
      and no [at_exit] function runs.

    The teardown of the interrupted test does not run and nothing is restored,
    so what {!setenv} and {!chdir} changed stays. A signal that acts at the last
    of the points above finds the store updated and the corrections written. One
    that acts earlier updates no store and writes no correction. A process that
    a test forked inherits the handlers and not the run, and a signal kills it
    as the default disposition would. *)
