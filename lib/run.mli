(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Test execution: the configuration of a run, its record, the operations of
    the running test and the runner.

    {!execute} runs the selected tests of a {!Test_tree.t} list under one
    {!type-config}, one at a time and on the domain that called it, and returns
    an {!type-outcome}. {!list_selection} makes the same selection and runs
    nothing. This module prints nothing. {!execute} gives its progress to an
    observer as {!type-event}s, and everything else is data of the outcome,
    which {!Report} renders.

    A run keeps its mutable state in one {!type-t}, which {!execute} creates. A
    later run in the same process has its own, so it acquires its fixtures again
    and starts from an empty baseline registry. The operations of
    {{!section-body}the running test} reach the record through
    {{!section-ambient}one ambient slot}. Nothing here is thread-safe, and an
    operation called from another domain raises there (see {!current_frame}). *)

(** {1:config Configuration} *)

type invocation = [ `Exe of string | `Mirrors ]
(** The type for the way a report spells a command that runs the suite again.
    - [`Exe cmd]: [cmd] runs this executable and a line appends its flags. It is
      [argv.(0)], or under dune a [dune exec <path> --]. [cmd] must be quoted
      for a shell, because a line prints it as it is.
    - [`Mirrors]: no command line runs the suite again. It is the context of a
      [--corrected] run, the inline runner's included, and of a run without
      [argv.(0)]. *)

type scope = {
  prefixes : string list;
      (** [--mutate=PREFIX,…]: a mutant is in scope when its recorded source
          path starts with one of the prefixes. The bare flag gives the empty
          list, which keeps every file. *)
  lines : Lines.t option;
      (** [--lines] and [--diff]: a mutant is in scope when it lies on one of
          these lines, by its recorded file and line. [None] keeps every line.
      *)
}
(** The type for the mutants that a mutation loop tests: those that both fields
    keep. *)

val every_mutant : scope
(** [every_mutant] keeps every mutant: no prefix and no line set. *)

(** The type for what a run does with the mutants of an executable built for
    mutation testing. *)
type mutation =
  | No_mutation  (** An ordinary run. The mutants are inert. *)
  | Loop of scope  (** [--mutate]: the mutation loop, over this scope. *)
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

type case_file = {
  file : string;  (** The path that [--case] gave, as it was given. *)
  choices : int64 array;
      (** The choices its bytes decode to (see {!Choice.of_bytes}). *)
}
(** The type for a case file, read once, before any test runs. *)

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
  jobs : int;
      (** [-j]: the number of worker processes of a supervised run, at least [1]
          (see {!Supervisor}). {!execute} does not read it. *)
  in_process : bool;
      (** [--in-process]: the run is {!execute}'s, in the calling process, and
          its selection must hold one test at most (see {!In_process_selects}).
      *)
  baseline : Baseline.mode;
      (** [-u] is {!Baseline.Update}, [--corrected] is {!Baseline.Corrected},
          and neither is {!Baseline.Check}. {!execute} refuses [Update] under
          [CI] (see {!Update_refused_in_ci}). *)
  timeout : float option;
      (** [--timeout]: the limit, in seconds, of a test that declares none, and
          [None] to leave such a test [default_timeout]. It must be finite and
          positive, which {!Cli} guarantees and {!execute} does not check. *)
  default_timeout : float;
      (** The limit, in seconds, of a test that declares none under a [None]
          [timeout] (see {!limit}). No flag sets it: {!Cli} keeps
          {!default_config}'s, and windtrap's own suites set a short one to
          stand in for it. *)
  prop_count : int option;
      (** [--prop-count]: the generated cases of a property, where it raises the
          declared or default count (see {!prop}). *)
  replay : string option;
      (** [--replay]: the replay token that every property of the run checks
          alone, in place of its examples and generated cases (see
          {!Property.run}). *)
  case_file : case_file option;
      (** [--case]: the case that the one selected test, a property, checks
          alone, in place of [replay], the last-failed store's cases, its
          examples and its generated cases (see {!Property.run}). The selection
          must hold one test (see {!Case_selects}), and a test that checks no
          property under it fails. *)
  log_dir : string;
      (** [-o]: the root directory of the capture logs and of the last-failed
          store. {!execute} uses it as given, so a relative one follows a test
          that changes the working directory. *)
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
  json : string option;
      (** [--json]: a file, or a directory, where a JSON report of the results
          and their scope is written too ({!Report_json}). [None] writes none.
      *)
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
    [slow_threshold], [verbose], [junit], [json], [github] and [invocation], so
    they change no outcome and no exit code. *)

val default_config : unit -> config
(** [default_config ()] is the configuration of a run given no flag: no
    selection, every boolean [false], no replay and no case file,
    {!Baseline.Check}, no [timeout], a [default_timeout] of [600.] seconds, ten
    minutes, no count, [color = Os.Auto], [slow_threshold = 1.], {!No_mutation},
    [`Mirrors] and nothing broadcast. Every call draws [seed] from
    {!Seed.random} and takes [log_dir] from {!Os.default_log_dir}. *)

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
    [Invalid_argument] if no test is running.

    Called from a domain other than the one that called {!execute}, it refuses
    ({!Failure.refuse}) a message failure located by {!Loc.capture}, in that
    domain, and records nothing on the frame. The failure fails the running
    test, since the domain records it in the scope it was spawned in. *)

val current : unit -> t
(** [current ()] is the record of the run that the frame of {!current_frame}
    belongs to. Raises as {!current_frame} does. *)

val stop : unit -> unit
(** [stop ()] ends the run after the running test: no later test executes, as
    under [config.bail], and {!stopped} names the test. A stateful test calls it
    when a call on another domain outlives the test's limit, since that domain
    would run the test's code inside the next test. Raises as {!current_frame}
    does. *)

val prop_context : unit -> Property.context option
(** [prop_context ()] is the label context of the law of a property that is
    running (see {!Property.context}), when it is read on the domain that called
    {!execute}, and [None] otherwise: outside a test, outside the law of a
    property and on another domain. It never raises and records nothing. An
    operation that labels a case reads {!current_frame} first, so that it
    refuses as that function does, and the error on [None] is its own. *)

(** {1:body The running test}

    Operations on the test that is executing, in its setup, its body and its
    teardown. Each reads {!current_frame} before it changes anything, so it
    raises as that function does: [Invalid_argument] when no test is running,
    and a refusal when called from another domain. {!remove_tree} reads nothing,
    and for {!fixture} the operation is the accessor. What an operation records
    on the frame lasts for the attempt, and the runner undoes it when the
    attempt ends (see {{!section-attempts}attempts}). *)

(* The facade ([{1:body}] in windtrap.mli) states each operation's contract,
   and those of [fixture] and [prop], for a test's author. The docs here state
   only what it does not: what the other modules of the library rely on. *)

val current_test : unit -> string list
(** See the facade's [current_test]. {!Test_tree.path_to_string} of it is the
    string that the filters match. *)

val subtest : string -> (unit -> unit) -> unit
(** [subtest name fn] runs [fn ()] as a named part of the running test, in a
    scope of its own ({!Failure.recording}). A {!Failure.Check_failure} of [fn]
    is added to the frame and [subtest] returns. Any other exception is added as
    a {!Failure.Raise} failure with its backtrace, located at the declaration of
    the test. A failure caught in [fn] is added before them. Every
    {!Failure.Control} passes through, after the failures are added, so a skip,
    a timeout or an [exit] ends the whole test, and an [assume] inside a subtest
    inside a law discards the case.

    Inside a law, where the frame has a property context, the first of those
    failures is refused ({!Failure.refuse}) instead of added, so the engine
    fails the case and shrinks it as an assertion's. The failure of an exception
    then has no location, as {!Failure.of_fault} makes it. Every function of a
    stateful test runs inside its law.

    The failure carries its label as data, in its [subtest] field and never in
    its [msg]. The label is the name of the test, then the names of the open
    subtests, outermost first, as in [["test"; "outer"; "inner"]]. An enclosing
    subtest keeps the label of a failure that an inner one raised. The phase of
    the failure stays {!Failure.Body}, in a setup and in a teardown too. *)

val check_baseline : ?loc:Loc.t -> Baseline.subject -> string -> unit
(** [check_baseline ?loc subject actual] is {!Baseline.check} of [actual]
    against [subject] in the registry of the run, except that a mismatch is
    recorded and not raised. A {!Failure.Missing} or {!Failure.Mismatch} failure
    is added to the frame and the call returns. Inside a {!subtest} the failure
    is labelled as {!subtest} labels one. A {!Failure.Unresolvable} failure is
    refused ({!Failure.refuse}).

    [loc] is the location of the failure: the position of a literal, the end of
    the body for a {!Baseline.Trailing} text, or the site of the call for a
    file. Without it the failure takes the declaration site (see
    {{!section-attempts}attempts}). Under {!Baseline.Update} a baseline that
    differs records its correction and nothing fails.

    Inside a law, where the frame has a property context, the check is made at
    once with [~correct:false], in every mode and in a worker too, and a failure
    is refused, withheld as {!Failure.In_law}: the law claims the check for
    every value, so a mismatch refutes the case, which the engine shrinks, and
    no correction is recorded. A table's row runs outside the law's context (see
    {!row}), so its checks are a test's.

    Raises [Sys_error] as {!Baseline.check} does, and [Invalid_argument] if no
    test is running. *)

(** {2:scratch Temporary paths}

    An attempt has one temporary directory, made under the system temporary
    directory with mode [0o700] by the first call below. The runner removes it
    when the attempt ends (see {{!section-attempts}attempts}), and when a signal
    ends the run. *)

val temp_dir : ?prefix:string -> unit -> string
(** [temp_dir ?prefix ()] makes its directory in that of the attempt, with mode
    [0o700], and passes [prefix] through {!Os.sanitize_component}. See the
    facade's [temp_dir]. *)

val temp_file : ?suffix:string -> unit -> string
(** [temp_file ?suffix ()] makes its file in that of the attempt, with mode
    [0o600], and passes a non-empty [suffix] through {!Os.sanitize_component}.
    See the facade's [temp_file]. *)

val remove_tree : string -> unit
(** [remove_tree path] removes [path] and what is under it, as far as it can. It
    gives each directory its owner's read, write and search permissions before
    it lists it. It removes a symbolic link without following it and ignores
    every file-system error, a missing [path] included, so it never raises. *)

(** {2:process Process state}

    The environment and the working directory belong to the process. {!setenv}
    and {!chdir} record on the frame what they change, and the runner puts it
    back when the attempt ends (see {{!section-attempts}attempts}). A signal
    that ends the run puts nothing back. *)

val setenv : string -> string option -> unit
(** [setenv name value] is {!Os.setenv}[ name value], after which the frame
    records what [name] held before, unless an earlier [setenv] of [name] in the
    attempt did. Raises as {!Os.setenv} does, and [Invalid_argument] if no test
    is running, before the process is touched. *)

val chdir : string -> unit
(** [chdir dir] is [Unix.chdir dir], before which the first [chdir] of the
    attempt records the working directory on the frame. Raises [Sys_error] if
    that first call cannot read the current directory. See the facade's [chdir].
*)

(** {2:fixtures Fixtures} *)

val fixture : ?teardown:('a -> unit) -> (unit -> 'a) -> unit -> 'a
(** [fixture ?teardown create] is the facade's [fixture]. The accessor holds no
    state: the record of the run caches the outcome of its first call,
    [create ()] inside the failure boundary of the calling test, as a value, an
    exception with its backtrace, or a [Failure.Control (`Skip _)] with its
    reason. Only a value acquired with a [teardown] is registered for the
    {{!section-release}release}.

    The accessor is named [fixture (<file:line>)] after the site where [fixture]
    was applied, or [fixture #<n>] when {!Loc.capture} finds none. An
    {!event.Fixture_release} and the failure of a release carry that name. *)

(** {1:results Results} *)

(** The type for a case within a test, as the
    {{!section-store}last-failed store} names it. *)
type case_id =
  | Row of string  (** A row of a table, by its printed value. *)
  | Replay of string
      (** A counterexample of a property, by its replay token. *)

type skipped_rows = {
  count : int;  (** The rows that skipped, at least [1]. *)
  reason : string option;  (** The reason that the first gave, if any. *)
}
(** The type for the rows of a table that skipped (see {!row}). *)

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
          last-failed store follow this field, and {!exit_code} follows it with
          one exception. A [Fail] row that does not count is an expected
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
          A table's [cases] are the rows that passed, and leave out the rows
          that skipped. *)
  table : bool;
      (** [true] iff the test is a table: its property was given [table] (see
          {!property}), so its cases are rows. *)
  skipped_rows : skipped_rows option;
      (** The rows of a table that skipped in the last attempt, [None] when none
          did. A table that a skip ended, every row that ran having skipped, has
          its count too. *)
  tolerances : Check.tolerance list;
      (** What each site of [equal] under a tolerance saw in the last attempt
          ({!Check.tolerances}), read in the process that ran it, so a worker's
          reach its parent with the row. [[]] for a test that compared under no
          tolerance, and for a row the parent made of a lost worker. *)
  refuted : case_id list;
      (** For a row that counts as failed, the cases that its last attempt
          refuted, when each of its failures belongs to one: the rows of a table
          that failed, by a failure they recorded or by their law raising, or
          the counterexamples of a property, each with its token. [[]] when a
          failure belongs to no case, as a timeout or an example's does, and for
          every other row. *)
}
(** The type for result rows, one per executed test. A row carries every fact
    that a renderer needs, so no consumer derives a decision of the runner from
    a message. *)

val results : t -> result list
(** [results t] is the row of every test executed so far, in the order of
    execution. *)

val stopped : t -> string list option
(** [stopped t] is the path of the test after which {!stop} ended the run, and
    [None] when nothing did. *)

(** {1:props Properties} *)

val property :
  ?loc:Loc.t ->
  ?count:int ->
  ?max_discard:int ->
  ?examples:'a list ->
  ?summary:('a -> string option) ->
  ?runs:int ->
  ?only:(int -> bool) ->
  ?table:bool ->
  'a Gen.t ->
  ('a -> unit) ->
  unit
(** [property gen law] is the body of the test that {!prop} declares, [loc]
    being its declaration site. It returns [()] on a [Pass] and raises the
    failure of any other outcome as a [Failure.Check_failure] (see {!prop}).
    [runs] and [only] are {!Property.run}'s. [table], [false] by default, says
    that the test is a table, whose row says so ([table]). A table's row that
    skipped ({!row}) is no row that passed: the statistics leave it out of
    [cases], and a table whose engine passed with no row that passed, every row
    that ran having skipped, raises the first row's skip, with no statistics.
    Raises as {!current_frame} does.

    [config.replay] is the token that the engine replays. Otherwise, under
    [config.failed_only], the {!recorded} counterexamples of the test are
    replayed in its place, one engine run each: a failure of each but the last
    is added to the frame, and the last one's outcome is the property's. A
    recorded {!Row} fails the test with a message that says so, as stale. With
    [only] the test is a table, which selects its rows itself, and nothing
    recorded is replayed.

    The engine runs [law] through {!Failure.catch}, so each run of it is a scope
    of its own: a verb's failure that the law caught fails that run, and what a
    run records reaches neither another run nor the frame. A candidate that the
    search rejects thus leaves the test nothing.

    The [output] of {!Property.run} is what a run of [law] wrote from its start
    to its end, as {!Capture.output_tail} gives it within two {!Capture.mark}s.
    The failure of a [Fail] thus carries the output of the run that failed on
    its counterexample, and keeps it through {!execute}.

    Its [shrinking] gives the search to the observer of the run, as {!Shrinking}
    events, and [property] gives {!Shrunk} when {!Property.run} returns or
    raises after one.

    What the law raises reaches the engine through windtrap's frames alone, so
    its backtrace ends on the law's own frames once
    {!Failure.backtrace_to_string} drops the trailing run of windtrap's. *)

val recorded : unit -> case_id list
(** [recorded ()] is the cases of the running test that the last-failed store
    names under [config.failed_only], and [[]] for the whole test or any other
    run. Raises as {!current_frame} does. *)

val row : tag:string -> id:string -> (unit -> unit) -> unit
(** [row ~tag ~id fn] runs [fn ()], the law of a table on its row [id], as a
    part of the running attempt that settles alone, as the attempt of a test of
    its own would: in the order of the rows, it keeps its corrections iff [fn]
    returned and every failure added while it ran is a baseline failure, and
    otherwise each such failure is withheld ({!Failure.with_withheld}), as
    {!Failure.Skipped} when [fn] raised a skip or a discard. A run that makes
    its checks as they come settles the row as it ends; a worker that leaves its
    checks to the parent sends the row's bounds with the attempt, and {!verdict}
    settles it there. A failure added while it runs is labelled as {!subtest}
    labels one, with [tag] as the innermost name. A row is chosen, not drawn, so
    [fn] runs with no property context, as a test's body does: {!prop_context}
    is [None], a {!subtest} adds its failure, and a law demands nothing. A skip
    that [fn] raises, a {!subtest}'s or a fixture's included, ends the row alone
    when the row added no failure: [row] returns, and the row counts in
    {!result.skipped_rows}. Anything else that [fn] raises is raised again. The
    row counts in {!result.refuted} when it added a failure or [fn] raised a
    fault. Raises as {!current_frame} does. *)

val without_labels : (unit -> 'a) -> 'a
(** [without_labels fn] is [fn ()] with the labels of the running law going to a
    {!Property.scratch} context, so that what [fn] marks or demands counts
    nowhere. The law's context comes back however [fn] ends. It is [fn ()] alone
    outside a law, and on a domain other than the run's. *)

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
    - [count] is the number of generated cases: the declared [count], or else
      {!Property.default_count}, raised to [config.prop_count] when that is
      larger and the count is not [0], and the engine is told which of the two
      it was given.
    - [max_discard], [examples] and [summary] are {!Property.run}'s.

    [prop] adds no tag, so a caller must add {!Test_tree.Tag.prop} to [tags].

    The engine draws from [config.seed] and from the path string of the test.
    Its context is {!prop_context} while [law] runs. A [Fail] adds the
    {!Failure.Property} failure of the engine. A [Vacuous] outcome adds a
    {!Failure.Vacuous} failure, located at the declaration of the test, whose
    one line says why.

    Raises [Invalid_argument] as {!Test_tree.test} does. A negative [count] or
    [max_discard] makes {!Property.run} raise inside the body, which fails the
    test. *)

(** {1:events Events} *)

(** The type for progress events, given to the [on_event] of {!execute} in the
    order of execution. An event carries data that is already decided and never
    the record of the run, so an observer changes no status, no count and no
    order. The record is read off the {!type-outcome}. An observer can end the
    run, by raising (see {!execute}).

    {!Shrinking} and {!Shrunk} are given inside an attempt. The observer then
    runs on the real descriptors 1 and 2 ({!Capture.outside}), the test's limit
    and the {{!section-signals}signals} are held until it returns, and what it
    raises is ignored, since it would fail the test that runs. *)
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
  | Shrinking of { path : string list; steps : int }
      (** A property of the test at [path] failed on a case, and the search
          shrinks it: it has accepted [steps] candidates (see {!Property.run}).
          It is given as the search first runs the law on a candidate, with
          [steps = 0], then as [steps] grows, at most ten times a second. *)
  | Shrunk of { path : string list }
      (** The search that the last {!Shrinking} of the test at [path] announced
          ended. It is given once per search, whatever ended it, and the test
          runs on: to its end, or to its next subtest or attempt. *)
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
      starting : starting option;
      results : result list;
      duration : float;
    }
      (** A signal is ending the run (see {{!section-signals}signals}).
          [running] is the path of the test it stopped, and [None] outside a
          test. [releasing] is the name of the fixture whose release it stopped,
          if any. [starting] is a worker that had not yet reached its run, the
          one that had been starting longest, if any. [results] are the rows
          recorded so far, and [duration] the seconds the run took so far. It is
          the last event and it is given once. What the observer raises on it is
          ignored, and the process then dies by the signal. *)

and starting = { seconds : float; output : string }
(** The type for a worker still in module initialisation when a signal came: the
    [seconds] since it was started, and the end of what it wrote, at most
    {!Failure.tail_bytes} bytes. *)

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
          whatever the selection. *)
  | Update_refused_in_ci
      (** [config.baseline] is {!Baseline.Update} and [CI] is set. Nothing lifts
          it. *)
  | No_recorded_failures
      (** [config.failed_only] is set, and the last-failed store names no
          declared test. The store is read against the declared paths, so a
          filter that keeps none of the recorded tests gives an empty selection
          and not this error. *)
  | In_process_selects of int
      (** [config.in_process] is set and the selection holds this many tests,
          more than one. *)
  | Case_selects of int
      (** [config.case_file] is set and the selection holds this many tests,
          other than one. *)
  | Mutation_refused of string
      (** A mutation run ({!Loop} or {!Armed}) could not score the suite, for
          this reason, which it has said. It is no check of {!select}: the
          mutation loop gives it, after its dry run or in place of it. *)

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
      (** The selected tests, in the order of execution. Under [config.bail] or
          after a {!stop} some may not have executed, and those have no row. *)
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
  uncorrected : bool;
      (** [true] iff a row counts as failed and some failure of its last attempt
          carries no kept correction. *)
  failed_in_ci : bool;
      (** [true] iff [selection.in_ci] and a row counts as failed, kept
          corrections or not. A correction leaves a run's exit code [0] only for
          [dune promote] to accept it, and no one promotes under CI. *)
}
(** The type for finished runs: what a report renders, and what the caller needs
    to exit ({!exit_code}). *)

val execute :
  ?on_event:(event -> unit) ->
  config ->
  suite:string ->
  Test_tree.t list ->
  (outcome, startup_error) Stdlib.result
(** [execute config ~suite tests] runs [tests] as this section says. It is
    [Ok outcome], or [Error error] when a startup check refuses the run, before
    anything executes.
    - [on_event] receives the {!type-event}s. Defaults to a function that
      ignores them.
    - [suite] names the directory of the capture logs and of the last-failed
      store, under [config.log_dir].

    [execute] reads [CI] once, at startup, and writes the capture logs unless
    [config.stream], the store and the kept corrections. On the process, it
    registers the exit guard and turns the recording of backtraces on (see
    {{!section-exits}exits}), handles three {{!section-signals}signals} while it
    runs, and owns the state that {{!section-attempts}attempts} names during
    each of them.

    An exception that [on_event] raises leaves [execute], except on
    {!Interrupted}, {!Shrinking} and {!Shrunk}, where it is ignored. The run
    then updates no store and writes no correction, and what becomes of the
    acquired fixtures depends on the event:
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

val exit_code : (outcome, startup_error) Stdlib.result -> int
(** [exit_code result] is the exit code of a run that ended with [result], the
    one rule every runner applies for the facade's exit codes:
    - [1] when [outcome.uncorrected] or [outcome.failed_in_ci], when a release
      failed, or when a correction could not be written (a {!Baseline.Refused}
      entry).
    - Otherwise [0] when a test executed, or when [config.broadcast] says the
      environment alone gave a selection that keeps none of the tests of a suite
      that declares some, and else [2].
    - For a refused startup, [2] for {!No_recorded_failures},
      {!In_process_selects} and {!Case_selects}, and [1] otherwise,
      {!Mutation_refused} included. *)

(** {2:selection Selection}

    A test runs iff every layer below admits it. The layers intersect, and an
    absent one admits every test.
    - Its path string ({!Test_tree.path_to_string}) contains one pattern of
      [config.filter] and none of [config.exclude].
    - Its tags hold every tag of [config.tags] and none of
      [config.exclude_tags].
    - Under [config.failed_only] the last-failed store names it.
    - It falls in the bucket of [config.shard].
    - It is focused, when the suite holds a focused node.

    A test left out does not execute and has no row.

    The bucket of a test is a frozen hash of its path string, modulo [N]. It
    depends on nothing else, and a change to the hash moves the tests of every
    sharded suite. *)

(** {2:attempts Attempts}

    Around the three phases of an attempt the runner seeds [Random] as the
    facade's [test] states, whatever [config.seed]. Unless [config.stream], it
    redirects the output into the log file of the test, truncated first
    ({!Capture.with_capture}). A capture that cannot be set up or restored is a
    {!Failure.Raise} failure of the body, and the run goes on.

    The limit of a test is {!limit}'s, the facade's [SIGALRM] timer over the
    three phases, and the runner puts the previous handler back afterwards.

    The frame keeps every failure that is added to it, so a body failure and a
    teardown failure are two entries. Each phase runs in a scope of its own
    ({!Failure.recording}): a failure caught in it is added, with its [caught]
    set and that phase, when the phase ends and before what it raised. A failure
    that a domain records after its phase ended is lost. What a phase raises
    becomes such a failure, with that phase set:
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
    included, the runner undoes what the attempt changed, in the order of the
    facade's [chdir], outside the window of the limit and outside the capture. A
    restoration that fails is a {!Failure.Teardown} message failure, located at
    the call that made the change. *)

(** {2:scoped Scoped tests}

    The body of a {!Test_tree.Scoped} test runs inside the callback that the
    runner gives to [scope], as the facade's [scoped] states, and a
    {!Test_tree.bracket} is such a test. The runner raises the failure of the
    body again unrecorded, and does not add it a second time on its way out. A
    failure caught in [scope] itself is one of {!Failure.Setup} when the
    callback was never called, and of {!Failure.Teardown} otherwise. Whatever
    else leaves [scope] is attributed by how far the callback got:
    {!Failure.Setup} before it was called, {!Failure.Teardown} after the body
    left it, and {!Failure.Body} in between.

    A second call of the callback runs nothing and returns, and the attempt then
    fails with a body message that gives the number of calls. A scope that
    returns without calling back, and neither raised nor skipped, fails the
    attempt with a setup message. *)

(** {2:retries Retries}

    A test runs again while its attempt counts as failed ({!result.counted}),
    within the facade's [retries], and an attempt that kept a correction is the
    last, because the next one would be compared with the text it recorded. Its
    first failure carries the tail of what that attempt wrote
    ({!Capture.output_tail}), unless [config.stream], or unless it is a
    {!Failure.Property} failure, which carries that of its counterexample's run
    (see {!property}). *)

(** {2:corrections Corrections}

    Under {!Baseline.Corrected} and {!Baseline.Update} a check whose baseline
    differs or is missing records a correction ({!Baseline.check}), except for a
    file missing under {!Baseline.Corrected}. After every attempt the runner
    settles them ({!Baseline.settle}): it keeps them iff every failure of the
    attempt is a baseline failure and the attempt did not skip. In every mode,
    the baseline failures of an attempt that breaks that rule are marked with
    {!Failure.with_withheld}, so that no report offers to accept them. The mark
    is {!Failure.Skipped} when the skip alone broke the rule, and
    {!Failure.Failed_outside} otherwise. An [xfail] test checks without
    correcting, in every mode. The kept corrections are written once
    ({!Baseline.val-write}), after the last test, the release of the fixtures
    and the update of the store, and before {!execute} returns. The observer was
    given the failures that offer them earlier, as each test finished. *)

(** {2:release Fixture release}

    The runner releases the fixtures as the facade's [fixture] states, and gives
    an {!event.Fixture_release} to the observer before each teardown. A teardown
    that raises adds a {!Failure.Release} message failure, located at the site
    of the fixture. A fatal exception from a teardown leaves {!execute} at once
    and the fixtures not yet released are never released, as when the observer
    raises on the event (see {!execute}). *)

(** {2:store The last-failed store}

    The store is the file [<log_dir>/<suite>/.last-failed-<exe>], with [suite]
    through {!Os.sanitize_component}, and [<exe>] the hexadecimal MD5 of the
    running executable's {!Windtrap_runtime.Instr.exe_identity}, which gives the
    facade's one record per executable and suite, a run under dune, in its
    sandbox, included. {!execute} rewrites it atomically ({!Os.atomic_write}) at
    the end of every run that passed its startup checks, a run that executed
    nothing included. {!list_selection} does not touch it. Nor does a run that
    an exception ended, or a {{!section-signals}signal} before this point.

    A test that counted as failed is recorded once, with the cases of its
    {!result.refuted}, and an executed test that did not is cleared. A run that
    executed the whole declared suite also drops the entries of paths that no
    longer exist.

    The file opens on the line [windtrap-last-failed 2], then holds one line per
    entry: the test's path through [String.escaped], alone for the whole test,
    or followed by a tab, [row] or [replay], a tab, and the case through
    [String.escaped], a row's printed value or a counterexample's token. A test
    with several cases has a line for each, and a line that names it alone makes
    it whole. A case of a kind this version does not know names its test whole.
    The format is not stable, and a file that is not recognised reads as empty,
    as one that cannot be read does. One that cannot be written keeps its
    content. The record of 0.2.0 is [<log_dir>/<suite>/.last-failed]. *)

(** {2:exits Exits and backtraces}

    {!execute} and {!list_selection} turn the recording of backtraces on
    ([Printexc.record_backtrace]) and leave it on. They register an [at_exit]
    function, the exit guard. A supervised run ({!Supervisor}) registers none: a
    worker that code under test exits ends, and its parent records it. The guard
    serves the runs that stay in one process: [--in-process], Windows, and a
    process that can start no worker ([Supervisor.execute]).

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
    fixture runs acts at once, and one that arrives in an observer of
    {!Shrinking} or {!Shrunk} when the observer returns. One that arrives in the
    runner's own code, or in another observer, acts before the next test, after
    the last one, or after the corrections are written.

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

(** {1:supervision Serving a supervisor}

    A supervised run ({!Supervisor}) splits {!execute} across processes. Its
    parent selects, reports, keeps the store, writes the corrections and decides
    the exit code, with the functions below. Each worker process runs {!serve}:
    the attempts, frames, fixtures and signals above, unchanged, one case per
    {!request}, and tells its parent what happened with {!report}s. Every value
    that crosses holds no closure. *)

type selection = {
  cases : Test_tree.case list;  (** The selected tests, in declaration order. *)
  total : int;  (** The tests that the suite declares. *)
  focus_active : bool;  (** [true] iff the suite holds a focused node. *)
  recorded : (string * case_id list) list;
      (** Under [config.failed_only], the tests whose cases the store names,
          with those cases. *)
  in_ci : bool;
      (** [true] iff [CI] was set ({!Os.in_ci}) when the startup checks read it,
          once for the run. *)
}
(** The type for what a run selected. *)

val limit : config -> Test_tree.case -> float option
(** [limit config case] is the limit of each attempt of [case], in seconds: its
    own, or else [config.timeout], or else [None] under [config.in_process], the
    debugger's run, and [config.default_timeout] otherwise. {!execute} sets it
    as the alarm of each attempt, and {!Supervisor} kills a worker past three
    times it (see {!Supervisor.execute}). *)

val select :
  config ->
  suite:string ->
  Test_tree.t list ->
  (selection, startup_error) Stdlib.result
(** [select config ~suite tests] makes the startup checks and the
    {{!section-selection}selection} of {!execute}, and turns the recording of
    backtraces on. It registers no exit guard. Raises as {!execute} does. *)

val run_started : suite:string -> selection -> event
(** [run_started ~suite selection] is the {!Run_started} event of a run over
    [selection]. *)

type attempt
(** The type for an attempt as data: its failures, the baseline checks it left
    to the parent, the bounds of a table's rows, and its skip. *)

type verdict
(** The type for the settlement of an attempt: its outcome, whether its failures
    all carry kept corrections, whether it must be the last, and the cases it
    refuted. *)

type served = {
  result : result;  (** The row of the case. *)
  corrected : bool;
      (** Whether every failure of its last attempt carries a kept correction.
      *)
  deferred : (attempt * Failure.tail option) option;
      (** The last attempt and its output tail, when its checks are still the
          parent's to make: [result] is then the parent's to rebuild. *)
}
(** The type for what a case left once its attempts ended. *)

(** The type for what a parent asks of its worker. *)
type request =
  | Case of string  (** Run the case of this path string. *)
  | Verdict of verdict  (** The verdict that a {!Judge} waited for. *)
  | Ack  (** The event just sent is shown, under [config.stream]. *)
  | Finish  (** Run no more cases: release the fixtures, and end the process. *)

(** The type for what a worker tells its parent. *)
type report =
  | Event of event
      (** An event of the run. Under [config.stream] the worker waits for an
          {!Ack} after each, but {!Interrupted}. *)
  | Judge of attempt
      (** An attempt that a retry may follow, whose checks the parent must make
          first; the worker waits for its {!Verdict}. *)
  | Done of { served : served; stopped : bool; retire : bool }
      (** The case ended. [stopped] is {!stop}'s, and [retire] says that the
          worker must not run another case: the case spawned a domain, which may
          still run, or raised [Out_of_memory] or [Sys.Break]. *)
  | Released of Failure.t list
      (** The fixtures are released, with these release failures. *)
  | Exited  (** Code under test called [exit] in the case that runs. *)
  | Outside of Failure.t
      (** A failure that belongs to no case: an [at_exit] function that a test
          registered raised, located at the test, or code under test called
          [exit] outside a case. It is a {!Failure.Release} failure. *)

val exit_text : string
(** [exit_text] is what every failure for a call to [exit] tells the user to do
    instead. *)

val serve :
  config ->
  suite:string ->
  Test_tree.t list ->
  temp_dir:string ->
  own_exits:bool ->
  send:(report -> unit) ->
  receive:(interrupted:(unit -> unit) -> request) ->
  last:(unit -> unit) ->
  'a
(** [serve config ~suite tests ~temp_dir ~own_exits ~send ~receive ~last] is the
    life of a worker process: it runs each case that [receive] asks for, of the
    flattened [tests], with the attempts of {!execute}, and sends what happens.
    [receive] calls [interrupted] when a signal interrupts its wait. On
    {!Finish} it releases the fixtures, then runs the [at_exit] functions that
    its tests registered. The functions that the process held before run after
    them when [own_exits], as in a worker that started the executable again,
    whose module initialisation registered them, and not in a fork, whose
    parent's they are. One of them that raises is sent as {!Outside}. It ends
    the process with [Unix._exit 0], after [last ()], and never returns.
    - Temporary directories are made under [temp_dir].
    - Under {!Baseline.Check} the worker judges its checks. Under a correcting
      mode it checks in a {!Baseline.Check} registry only to prove the path and
      read the file, and leaves each check to the parent: in the {!attempt} of
      {!Judge} when a retry may follow, else in {!served.deferred}.
    - A call to [exit] in code under test ends the process: the worker sends
      {!Exited}, or {!Outside} when no case runs, runs the functions the process
      held before when [own_exits], then [last ()].
    - A signal acts as in {!execute}, with its {!Interrupted} event sent. *)

val supervising : config -> suite:string -> t
(** [supervising config ~suite] is the record of a supervised run's parent: an
    empty registry under [config.baseline], and the capture state that reads the
    log a worker left. *)

val in_run : t -> (unit -> 'a) -> 'a
(** [in_run t fn] is [fn ()] with [t] in the ambient slot, so that {!active}
    holds while the parent runs. *)

val add_result : t -> result -> unit
(** [add_result t result] records [result] as the next row of [t]. *)

val verdict : t -> Test_tree.case -> attempt -> verdict
(** [verdict t case attempt] makes the checks of [attempt] in the registry of
    [t], each where it fell among the attempt's other failures, and settles the
    attempt as {!execute} would. A registry that sees the attempts in the order
    of the selection holds what an in-process run's would. *)

val adjudicate : t -> Test_tree.case -> served -> result * bool
(** [adjudicate t case served] is the row of [case] and whether its failures all
    carry kept corrections: [served]'s, or rebuilt from its {!served.deferred}
    attempt by {!verdict}. *)

val lost : t -> Test_tree.case -> Failure.t -> duration:float -> result
(** [lost t case failure ~duration] is the row of a case whose worker died: it
    failed with [failure] alone, carrying the tail of the log the worker left.
*)

val conclude :
  t ->
  selection ->
  suite:string ->
  started:Os.counter ->
  uncorrected:bool ->
  release_failures:Failure.t list ->
  outcome
(** [conclude t selection ~suite ~started ~uncorrected ~release_failures]
    updates the store and writes the corrections of [t], as {!execute} does
    after the release, and is the outcome. *)
