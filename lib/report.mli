(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The run report.

    The module renders the transcript of a run (its header, live line, [-v]
    rows, failure blocks, end-of-run sections and summary), the GitHub Actions
    envelope around it, and the lines that a mutation run adds. {!run} executes
    a suite and reports it. {!create}, {!observe} and {!finish} serve a renderer
    that is fed by hand.

    A report is a projection of {!type:Run.result} rows and of the {!Failure.t}
    payloads in them, written under one [ansi] decision made at {!create}.
    Nothing here alters a status, a count or the scheduling of a run (guarantee
    4 of [doc/dev/architecture.md]). The entries of a failure block and the
    sections of a mutation report are {!Report_sections}', and the JUnit
    document is {!Report_junit}'s. *)

(** {1:renderer The renderer} *)

type t
(** The type for renderers. A renderer is mutable and serves one run, because it
    counts the results and the blocks that it committed. *)

val create : out:Format.formatter -> ansi:bool -> ?live:bool -> Run.config -> t
(** [create ~out ~ansi config] is a renderer that writes to [out].
    - [ansi] is whether styling is emitted. Under [ansi:false] every line is
      stripped of the escape sequences that a test name or captured output may
      hold. Under [ansi:true] they pass through.
    - [live] is whether {!begin_test}, {!note} and {!mutation_testing} draw the
      live line, which the next write erases. Defaults to [false], and a caller
      passes whether [out] is a terminal. The live line is off whatever [live]
      is under [ansi:false] and under [config.stream]. It is the one line that
      is cut to a width, which is 80 columns ([columns] in [report.ml]).

    [create] reads [config.verbose], [config.stream], [config.slow_threshold],
    [config.invocation], from which every [accept:] and [replay:] command is
    spelled, and the identifier of a [config.mutation] that is {!Run.Armed}. It
    reads no other field.

    The names of tests, suites and fixtures, and the reasons of a skip and of an
    expected failure, print through {!Report_sections.sanitize_name}. The labels
    of a property, the path of a correction and the three strings of
    {!mutation_armed} do not go through it.

    Raises [Invalid_argument] if [config.slow_threshold] is negative or not
    finite. *)

val terminal : Run.config -> t
(** [terminal config] is the renderer of a run on [Format.std_formatter].
    Styling is {!Os.resolve_color} of [config.color], of whether standard output
    is a terminal, of {!Os.inside_dune} and of {!Os.term_dumb}. The live line is
    on iff standard output is a terminal and the run is not under GitHub Actions
    ({!Os.in_github_actions}), within what {!create} allows. [terminal] thus
    reads the terminal status of standard output and, through these functions,
    [INSIDE_DUNE], [TERM], [NO_COLOR], [CI] and [GITHUB_ACTIONS]. *)

(** {1:transcript The transcript}

    {!header} comes first. {!begin_test} and {!result} follow for each test, in
    the order in which the tests finish, and {!finish} or {!interrupted} comes
    last.

    A compact run is one without [config.verbose]. It commits the block of a
    counted failure when its test finishes. The rule that closes the blocks, the
    end-of-run sections and the summary wait for {!finish}. Under
    [config.verbose] the header prints at once and every finished test commits a
    row, with the block of a failed test under its row.

    [config.stream] changes no line of the transcript. Under it {!result},
    {!note}, {!finish} and {!interrupted} first call {!Capture.drain}, so the
    bytes that a streamed test wrote precede what the report writes next.

    Whatever a function of this section writes, it erases the live line first,
    and a function that commits lines flushes [out] before it returns. *)

val header :
  t ->
  suite:string ->
  tests:int ->
  ?declared:int ->
  ?selection:string ->
  seed:Seed.seed option ->
  unit ->
  unit
(** [header t ~suite ~tests ~seed ()] records the header of the run, which names
    [suite], counts [tests] and carries [seed] when given. It prints at once
    under [config.verbose]. A compact run prints it before its first failure
    block or end-of-run section, and when it has neither it prints no header and
    its summary names the suite and carries the seed instead.
    - [tests] is the number of selected tests. The live line counts against it,
      and the summary counts as not run those of them that have no result.
    - [declared] is the number of tests that the suite declares before
      selection. Defaults to [tests].
    - [selection] describes what narrowed the run (see
      {!selection_description}).
    - [seed] is the root seed to show.

    The header prints neither [declared] nor [selection], which the summary uses
    to say why no test ran. Without a call to [header], a renderer prints no
    header, names no suite, counts no test as not run and explains no empty run.
*)

val begin_test : t -> path:string list -> unit
(** [begin_test t ~path] draws the live line for the test at [path], with its
    position among the selected tests. The line shows only while the live line
    is on (see {!create}). *)

val result : t -> Run.result -> unit
(** [result t r] commits what [r] is owed. A client must call it once per
    finished test and in the order of the tests, because {!finish} relies on the
    order of the blocks.

    Under [config.verbose] every result commits a row with its status, its path,
    its duration and, when [r.attempts > 1], the number of attempts. A skip
    shows its reason in place of a duration, and on the terminal the reason
    shows nowhere else. A passing property that collected labels prints its
    label table under its row. A failing [r] with [r.counted = false] is an
    excused expected failure. Its row carries the reason of [r.xfail], and its
    failures print nowhere on the terminal.

    A counted failure commits its block in both kinds of run. Under
    [config.verbose] its row is the title of the block. In a compact run nothing
    else prints, and the first block follows the header and the rule that opens
    the failures, which carries no count. A block holds, in this order:
    - the title. It carries the number of attempts when [r.attempts > 1] and, in
      an armed run, the mark of the armed mutant. Under [config.verbose] it also
      says when a failure of [r] is a missing baseline file, which a compact
      title does not.
    - one {!Report_sections.pp_failure} entry per failure of [r], with its
      source line.
    - the label table of a property: the distribution of the collected labels
      over the passing cases, then the hits of each demanded label when one of
      several coverage demands is unmet.
    - the captured output of the first failure of [r] that carries a
      {!type:Failure.tail}: its last {!Report_sections.max_lines} lines, under a
      heading that counts the lines and the bytes left out, then the path of the
      full log when the capture wrote one.
    - {!Report_sections.hints} for the whole test, without a filter for a
      fixture release.

    [result] reads the record, [r.outcome] and [r.counted], and never a message.
*)

val note : t -> string -> unit
(** [note t line] shows the run-scoped notice [line], through
    {!Report_sections.sanitize_name}. Under [config.verbose] it is a committed
    line. Otherwise it is drawn as the live line when that is on, and is not
    shown when it is off. *)

val observe :
  t -> seed:Seed.seed -> selection:string option -> Run.event -> unit
(** [observe t ~seed ~selection event] renders [event] through [t]:
    - [Run_started] is {!header} over the counts of the event, with [selection],
      and with [seed] iff the event's [properties] is [true], that is iff a
      selected test is a property.
    - [Test_started] is {!begin_test}, and [Test_finished] is {!result}.
    - [Fixture_release] is [note t ("releasing " ^ name)].
    - [Interrupted] is {!interrupted}, without [before_summary].

    [observe] raises nothing of its own, whatever the event holds. An exception
    from the output functions of [out] passes through it. *)

(** {2:selection The selection} *)

val selection_description : Run.config -> string option
(** [selection_description config] describes what narrows the run, in the words
    of the command line, or is [None] when nothing does. It names in this order
    the filter, the exclusion, the tags, the excluded tags, [--failed] and the
    shard, and never an in-source focus. The parts are joined by commas and a
    final [and], as in [tag "a", "b" and shard 1/3]. A value stands in double
    quotes, with its double quotes, backslashes and control characters escaped
    and the rest as typed. *)

val empty_selection_reason :
  declared:int -> selection:string option -> string option
(** [empty_selection_reason ~declared ~selection] is why a run has no test to
    run, as the clause that follows [no tests ran: ]. It is
    [Some "the suite declares none"] when [declared] is [0], and otherwise
    [Some "<selection> matched none of <declared> tests"] when [selection] is
    given, with [test] for one. It is [None] for a suite that declares tests and
    that nothing narrowed, which has nothing to explain. *)

(** {2:ending The end of the run} *)

val finish :
  t ->
  results:Run.result list ->
  duration:float ->
  ?baselines:Baseline.t ->
  ?before_summary:(unit -> unit) ->
  unit ->
  unit
(** [finish t ~results ~duration ()] ends the transcript, in a compact and in a
    [config.verbose] run alike. It commits, in this order:
    - the failure blocks that {!result} did not commit. [results] must extend,
      in order, the results that {!result} was given, because [finish] skips as
      many of its first counted failures as [t] committed blocks. The others are
      the rows that the executor records after the last test without an event,
      which are the failed releases of fixtures.
    - in a compact run that committed a block, the rule that closes the
      failures.
    - the sections that have rows: slow tests, flaky tests, corrections. A
      compact run that has printed no header prints it before the first.
    - what [before_summary ()] writes. It runs after [out] is flushed and
      defaults to doing nothing.
    - the summary. It is the last line of the transcript, save the one hint of
      an empty run. The verdict of an armed run and the report of a loop follow
      it (see {{!section-mutation}mutation lines}).

    {b Slow tests.} One row per result whose duration is at least
    [config.slow_threshold], slowest first, under a heading that gives the
    threshold. A skip and a test tagged [slow] ([r.slow_tagged]) are exempt. A
    failed test is not, so it has its block and its row, and an excused one is
    listed too. A threshold of [0.] disables the section.

    {b Flaky tests.} One row per passing result with [r.attempts > 1], in the
    order of [results], with the attempt that passed.

    {b Corrections.} One row per file of [Baseline.writes baselines], in the
    order of the paths as {!Os.display_path} prints them. A row says whether the
    file was written beside its baseline or accepted in place, which
    {!val:Baseline.mode} decides. The row of a source file counts its
    expectations. Without [baselines] there is no section and the summary has no
    corrections term.

    {b Summary.} Its terms come in this order: passed, with the flaky among
    them, skipped, expected failures, failed, with the subtest failures among
    them, not run, and the corrections written or accepted. A term of zero is
    omitted, and [duration] closes the line. A flaky test counts as passed, an
    excused result as an expected failure only, and a failed fixture release as
    failed although it is no test. The tests not run are the [tests] of
    {!header} less the test rows of [results], never below [0]. Subtest failures
    are the entries of counted failures for which {!is_subtest_failure} holds,
    and corrections count files.

    A run with no result at all says instead that no tests ran, with the reason
    of {!empty_selection_reason} when there is one. When a selection emptied a
    suite that declares tests, one line follows the summary. It is the command
    that lists the tests under an [`Exe] invocation, and a line that names [-l]
    under [`Mirrors]. *)

val interrupted :
  t ->
  ?before_summary:(unit -> unit) ->
  ?releasing:string ->
  running:string list option ->
  results:Run.result list ->
  duration:float ->
  unit ->
  unit
(** [interrupted t ~running ~results ~duration ()] ends the transcript of a run
    that a signal is stopping. It first says on standard error ({!Os.say}) what
    the signal interrupted: the test at [running], or when [running] is [None]
    the release of the fixture [releasing], or else the gap between two tests.
    It then calls {!finish} over [results], with [before_summary] and without
    [baselines]. *)

val refusals : Baseline.t -> string list
(** [refusals baselines] is one sentence per file that the run could not write,
    in the order of {!Baseline.refusals}: the path through {!Os.display_path}
    and the reason. It prints nothing. *)

(** {1:github The GitHub Actions envelope}

    Workflow commands that fold the transcript and annotate its failures. {!run}
    writes them on standard output, at column zero, when [config.github] is set,
    so they show under GitHub Actions only. Every function here is pure and
    returns whole command lines, each ending in a newline. A message is stripped
    of escape sequences and percent-encodes [%], CR and LF. A property ([file],
    [line], [title]) also encodes [:] and [,], so no payload can end a command
    or add a property to it. *)

val group_start : string -> string
(** [group_start name] is the command that opens a folded log section named
    [name]. *)

val group_end : string
(** [group_end] is the command that closes the section that {!group_start}
    opened. *)

val annotation :
  ?invocation:Run.invocation ->
  ?armed:string ->
  path:string list ->
  Failure.t ->
  string
(** [annotation ~path f] is the [::error] command for [f], a failure of the test
    at [path]. Its [file] and [line] are those of the location of [f], when it
    has one, and its title is [Test failure: <path>], [path] through
    {!Report_sections.sanitize_name}. The message is the
    {!Report_sections.pp_failure} entry of [f] without styling and without the
    source line, hint lines included, with [path] as their filter. [invocation]
    and [armed] are those of {!Report_sections.hints}, and [invocation] defaults
    to [`Mirrors]. *)

val annotations :
  ?invocation:Run.invocation -> ?armed:string -> Run.result list -> string
(** [annotations results] is the {!annotation} of every failure of every counted
    failed result of [results] ([r.counted]), in order, concatenated, and [""]
    when there is none. *)

(** {1:mutation Mutation lines}

    The lines that a mutation run adds to the transcript: the announcement and
    the verdict of an armed run, and the report of a [--mutate] loop. They print
    on the formatter of the renderer in every mode, whatever [config.verbose]
    and [config.stream] are. *)

(** {2:armed The armed run}

    An armed process announces its mutant before any other output and ends on
    one verdict line (guarantee 12 of [doc/dev/architecture.md]). The order is
    the client's: a client must call {!mutation_armed} before {!run} and at most
    one of the three verdict functions after it.

    These four functions erase the live line and write their line, and do not
    flush: a client must flush [out] and the standard descriptors after the
    announcement and after the verdict. *)

val mutation_armed : t -> id:string -> before:string -> after:string -> unit
(** [mutation_armed t ~id ~before ~after] prints the announcement of an armed
    run: the identifier of the mutant and its rewrite, from [before] to [after].
*)

val mutation_killed : t -> unit
(** [mutation_killed t] prints the verdict of an armed run in which the mutant
    made a test fail. *)

val mutation_survived : t -> hits:int -> unit
(** [mutation_survived t ~hits] prints the verdict of an armed run in which no
    test failed although the armed site was evaluated. The line states [hits],
    the number of evaluations. *)

val mutation_not_evaluated : t -> unit
(** [mutation_not_evaluated t] prints the verdict of an armed run in which no
    selected test evaluated the armed site. *)

(** {2:loop The report of a loop}

    A client must call {!mutation_testing} before each child,
    {!mutation_survivor} when a child ends on a survivor, and {!mutation_finish}
    once, last. {!mutation_refused} and {!mutation_interrupted} are the two
    other ways in which a report ends. *)

val mutation_testing : t -> index:int -> total:int -> id:string -> unit
(** [mutation_testing t ~index ~total ~id] draws the live line for the mutant
    that the loop is trying: [index], counted from [1], [total], and [id]
    through {!Report_sections.sanitize_name}. The line shows only while the live
    line is on (see {!create}). *)

val mutation_survivor : t -> Report_sections.survivor -> unit
(** [mutation_survivor t s] commits the {!Report_sections.survivor_block} of
    [s], flushed. The first call precedes the block by the rule that opens the
    survivors, which carries no count. A block has no column of executables. *)

val mutation_finish : t -> Report_sections.mutation -> unit
(** [mutation_finish t m] ends the report of a loop with
    {!Report_sections.mutation_closing} of [m] under the configuration of [t],
    flushed. [m.survivors] must be the survivors that {!mutation_survivor} was
    given, in order. The closing rule prints iff that list is not empty,
    whatever [t] committed, and [reproduce:] arms its first. *)

val mutation_refused : t -> string -> unit
(** [mutation_refused t message] erases the live line, flushes [out] and says
    [message] on standard error ({!Os.say}). *)

val mutation_interrupted :
  t -> testing:string option -> Report_sections.mutation -> unit
(** [mutation_interrupted t ~testing m] ends the report of a loop that a signal
    is stopping. It says on standard error what was interrupted: the child of
    the mutant [testing], or the determinism probe when [testing] is [None]. It
    then calls {!mutation_finish} over [m]. *)

(** {1:projections Failure projections}

    The failure projection of {!Report_sections}, under the names of this
    module. *)

val headline : Failure.t -> string
(** [headline] is {!Report_sections.headline}. *)

val is_subtest_failure : Failure.t -> bool
(** [is_subtest_failure] is {!Report_sections.is_subtest_failure}. *)

val labeled_msg : Failure.t -> string option
(** [labeled_msg] is {!Report_sections.labeled_msg}. *)

val pp_failure :
  ansi:bool ->
  ?excerpt:bool ->
  ?hints:bool ->
  ?filter:string ->
  ?invocation:Run.invocation ->
  ?armed:string ->
  Format.formatter ->
  Failure.t ->
  unit
(** [pp_failure] is {!Report_sections.pp_failure}. *)

(** {1:running Running} *)

val run :
  ?on_event:(Run.event -> unit) ->
  suite:string ->
  Run.config ->
  Test_tree.t list ->
  (Run.outcome, Run.startup_error) result
(** [run ~suite config tests] is [Run.execute config ~suite tests] with the
    whole report of the run written. It proceeds in this order:
    + It builds [terminal config] and, when [config.github] is set, opens the
      envelope with [group_start suite].
    + It executes the run with {!observe} as its observer, over [config.seed]
      and [selection_description config].
    + For a run that the executor did not refuse, it calls {!finish} over
      {!val:Run.results} and {!val:Run.baselines}. The [before_summary] closes
      the envelope and then writes the {!val:annotations}.
    + It says the {!refusals} on standard error, then writes the JUnit file with
      {!Report_junit.write} when [config.junit] is set.
    + It flushes both standard formatters and returns [Ok] of the executor's
      outcome.

    [Error error] is a refused startup. [run] then closes the envelope and says
    [Run.startup_message error] on standard error, in place of a transcript. The
    exit code ({!Run.startup_exit_code}) is the caller's to apply.

    An interrupted run does not return. On [Interrupted], [run] ends the
    transcript with {!interrupted}, whose [before_summary] is the close of the
    envelope and the annotations, and calls [on_event]. The process then dies by
    the signal, with no refusal said and no JUnit file written.

    [on_event] is a second observer of the run, called after the transcript's
    for every event. It must not raise (see {!Run.execute}) and defaults to
    doing nothing.

    The whole report goes through [Format.std_formatter] and the standard
    descriptors. A caller that forks must therefore first flush both standard
    formatters and both descriptors, or the child prints the buffered bytes
    again. Beyond what {!Run.execute} does, [run] reads the environment
    ({!terminal}) and writes standard output, standard error and the JUnit file.
    Each block also reads the source file of a located failure and
    {!Os.project_root}. The paths of a correction, of a refusal and of a full
    log are printed against that root. *)
