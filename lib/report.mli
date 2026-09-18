(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The run report: the terminal transcript, the GitHub Actions envelope, and
    the one composition that executes a suite and reports it.

    A report is a projection of {!Run.result} rows and their {!Failure.t}
    payloads, written under one [ansi] decision made at {!create}; nothing here
    alters status, counts or scheduling (guarantee 4). The failure blocks, the
    section vocabulary and the instrumentation reports are {!Report_sections}'s;
    the JUnit document is {!Report_junit}'s.

    What is committed when: a compact run shows the erasable live tail on a
    terminal and commits each failure block when its test finishes, after the
    header and the [── failures ──] rule, so a run that dies has printed what it
    knew; the rule that closes the failures, the end-of-run sections (slow
    tests, flaky tests, corrections) and the summary follow at the end, and a
    green run with none of them is its summary line alone. Under
    [Run.config.verbose] the header prints at once and one status row is
    committed per finished test, a failed test's block under its row.
    [Run.config.stream] changes no line of the transcript: a streamed test's own
    bytes precede what the report writes next, {!Capture.drain} running before
    the report writes on (a test's standard error is not ordered against it).
    The summary is the last line, save the empty run's [list:] hint; every
    committed write is flushed. Exact layout (column positions, display bounds)
    is not contract. *)

(** {1:renderer The renderer} *)

type t
(** The type for transcript renderer state: presentation state only; dropping a
    renderer loses no run data. *)

val create : out:Format.formatter -> ansi:bool -> ?live:bool -> Run.config -> t
(** [create ~out ~ansi config] is a renderer writing to [out]. [ansi] is whether
    styling is emitted: under [ansi:false] the transcript contains no escape
    codes at all (sequences in test names and captured output are stripped),
    under [ansi:true] they pass through; compared values are escaped into
    visible text either way ({!pp_failure}). [live] (default [false], and off
    regardless under [ansi:false] and under [config.stream], where a test's own
    bytes would land on it) is whether {!begin_test} maintains a self-erasing
    progress display; pass the sink's TTY status. [config.verbose],
    [config.stream], [config.slow_threshold] ([0.] disables slow warnings),
    [config.invocation] and the identifier of a [config.mutation] that is
    {!Run.Armed} are read once here. Names print with C0 control bytes and DEL
    escaped ({!Report_sections.sanitize_name}); the width is [80] columns and a
    failure block shows the last [10] lines of the captured tail with the full
    log's path.

    Raises [Invalid_argument] if [config.slow_threshold] is negative or not
    finite. *)

val terminal : Run.config -> t
(** [terminal config] is the run's renderer on [Format.std_formatter]: colour
    from {!Os.resolve_color} over [config.color], the terminal status and
    [INSIDE_DUNE]/[TERM]; the live tail on iff standard output is a terminal and
    the run is not under GitHub Actions. Reads the environment and the terminal
    status of standard output. *)

(** {1:transcript The transcript} *)

val header :
  t ->
  suite:string ->
  tests:int ->
  ?declared:int ->
  ?selection:string ->
  seed:Seed.seed option ->
  unit ->
  unit
(** [header t ~suite ~tests ~seed] records the run header
    ([mylib: 48 tests (seed s1:…)]) and prints it under [verbose]; compact
    prints it before its first failure block or end-of-run section, and not at
    all on a run that has neither. [seed] is shown when given; [tests] is the
    number of selected tests: it scales the live tail's [[k/n]] counter and the
    summary's [N not run] is counted against it. [declared] (default [tests]),
    how many tests the suite declares before selection, and [selection]
    ({!selection_description}) are not printed; the summary uses them to say why
    nothing ran. *)

val begin_test : t -> path:string list -> unit
(** [begin_test t ~path] shows the test at [path] on the live display
    ([Running [3/48] name…] under [verbose], a faint [  [3/48] name…] tail
    otherwise), erased before anything else prints. Prints nothing unless [live]
    and [ansi] are set. *)

val result : t -> Run.result -> unit
(** [result t r] erases the live display and commits what [r] is owed, flushed.
    Under [verbose] that is [r]'s status line: status tag, full path
    ({!Test_tree.path_to_string}), duration (the skip reason for a [SKIP]), the
    attempt count when [r.attempts > 1], and a passing property's label
    distribution under it. A failing result that did not count
    ([r.counted = false]) renders as a dim [XFAIL] tag with [r.xfail]'s reason;
    an [xfail] test that passed arrives as a counted failure whose message names
    the reason. A counted failure's status line is its block's title, qualified
    by [(mutant armed)] in an armed run: the block's lines ({!finish} describes
    them) follow it, then one blank line. Compact prints nothing for a result
    that did not count as failed, and for one that did its failure block,
    preceded once per run by the header and the 58-column [── failures ──] rule
    and separated from the previous block by one blank line. *)

val note : t -> string -> unit
(** [note t line] prints the run-scoped notice [line] ([releasing db]) on its
    own line under [verbose], as an erasable live line under [live] otherwise,
    and not at all elsewhere. *)

val observe :
  t -> seed:Seed.seed -> selection:string option -> Run.event -> unit
(** [observe t ~seed ~selection event] streams [event] through [t]: the header
    on [Run_started], carrying the run's root [seed] iff a selected test is a
    property; the live tail on [Test_started]; {!result} on [Test_finished]; the
    release notice on [Fixture_release]; {!interrupted} on [Interrupted]. Never
    raises. *)

val selection_description : Run.config -> string option
(** [selection_description config] describes what narrows the run (the filter,
    exclusion, tags, [--failed], the shard) as the reader typed it
    ([filter "parser"], [tag "a", "b" and shard 1/3]), or [None] when nothing
    does. Control characters are escaped, the rest is verbatim. *)

val empty_selection_reason :
  declared:int -> selection:string option -> string option
(** [empty_selection_reason ~declared ~selection] is the clause {!finish} puts
    after ["no tests ran: "]: ["the suite declares none"] when [declared] is
    [0], else ["<selection> matched none of N tests"] when something narrowed
    it, else [None]. Exported for [--list]. *)

val finish :
  t ->
  results:Run.result list ->
  duration:float ->
  ?baselines:Baseline.t ->
  ?before_summary:(unit -> unit) ->
  unit ->
  unit
(** [finish t ~results ~duration ()] ends the transcript. [results] extends, in
    order, the results {!result} was given. In order:

    - The failure blocks not committed yet (the rows the executor records after
      the last test), as {!result} commits them: under the [── failures ──] rule
      in a compact run, under their status lines in a [verbose] one; then, in a
      compact run that committed a block, the 58-column rule that closes them
      and one blank line. A block is the title ([  FAIL  <path>], qualified by
      [(N attempts)] when [r.attempts > 1] and by [(mutant armed)] in an armed
      run), one {!pp_failure} entry per failure with its source line, a blank
      line between two entries, the property label table, the captured tail, and
      last {!Report_sections.hints} for the whole test, a fixture-release row's
      without a filter. The tail is its last {!Report_sections.max_lines} lines,
      indented two more, under [captured output (N lines):],
      [captured output (last 10 of N lines):] or
      [captured output (last 10 lines, B earlier bytes omitted):], [B] every
      byte of the output before the first line shown, then [full log: <path>] at
      the heading's column.
    - One blank line before the first section below in a [verbose] run, unless a
      failed row's block has just closed on one; one after each section.
    - [slow tests (N, over Ts):], one [<duration>  <path>] row per completed
      test over the threshold that is not [slow_tagged], slowest first; none
      when the threshold is [0.]. Skips never count as slow.
    - [flaky tests (N):], one [passed on attempt K  <path>] row per passing
      result with [attempts > 1], in run order.
    - [corrections (N):], one [wrote <path>] ([--corrected]) or
      [accepted <path>] ([-u]) row per {!Baseline.writes} entry of [baselines],
      sorted by the displayed path ({!Os.display_path}), a source file's row
      ending in [(N expectations)].
    - The summary, always last:
      [4 passed (1 flaky), 1 skipped, 2 expected failures, 6 failed (3 subtest
       failures), 2 not run, 1 correction written in 6.5s.], zero terms omitted.
      Flaky tests count as passed, excused results as expected failures only,
      [not run] is the header's [tests] minus the test rows of [results], and
      corrections count files. It is prefixed with the suite name, and followed
      by the seed the header would have carried, when no header printed. A run
      with no result says [no tests ran: <reason>.] there instead
      ({!empty_selection_reason}), and when a selection emptied it one line
      follows, the one line after an outcome: [list: <launcher> -l] under an
      [`Exe] invocation, [(list the suite's tests with -l)] under a build
      action, which has no launcher to restate.

    [before_summary] (default: nothing) runs between the last section and the
    summary, after [t]'s formatter is flushed: what it writes sits against the
    sections, and the summary stays the last line.

    One blank line follows each section (a [verbose] run has no failures
    section), the last one's after [before_summary] ran, so a compact run with
    nothing to show (no counted failure, no slow or flaky test, no correction)
    prints exactly the summary line. A measured duration prints as [N.Nms] below
    10 ms, [Nms] below one second and [N.Ns] from there, rounded before its unit
    is chosen; the threshold prints as configured. Durations are
    {!Run.result.duration}, attempts summed. *)

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
    a signal is stopping: [windtrap: interrupted in <path>] on standard error
    ({!Os.say}), [running] the test that was stopped; when it is [None],
    [windtrap: interrupted while releasing <fixture>] if the signal stopped the
    release of [releasing], else [windtrap: interrupted between tests]. Then
    {!finish} over [results], whose summary counts what did not finish as
    [N not run]: a run stopped before its first result is [N not run] alone. *)

(** {1:baselines Baselines} *)

val refusals : Baseline.t -> string list
(** [refusals baselines] is one line for {!Os.say} per file the run could not
    write ({!Baseline.refusals}): [could not write <path>: <reason>]. *)

(** {1:github The GitHub Actions envelope}

    Workflow commands for log folding and failure annotations, written to
    standard output at column zero when [Run.config.github]. Every function is
    pure and returns complete command lines ending in a newline. Messages are
    ANSI-stripped and percent-encode [%], CR and LF as [%25], [%0D] and [%0A];
    properties ([file], [line], [title]) additionally encode [:] and [,] as
    [%3A] and [%2C], so no payload can terminate or restructure a command. *)

val group_start : string -> string
(** [group_start name] is the [::group::<name>] command line. *)

val group_end : string
(** [group_end] is the [::endgroup::] command line. *)

val annotation :
  ?invocation:Run.invocation ->
  ?armed:string ->
  path:string list ->
  Failure.t ->
  string
(** [annotation ~path f] is one [::error] command line for [f], raised by the
    test at [path]: [file=]/[line=] from [f]'s location when it has one,
    [Test failure: <path>] as [title], the path spelled as the block's title
    spells it ({!Report_sections.sanitize_name}), and as message the unstyled
    {!pp_failure} block, hints included, with newlines [%0A]-encoded. A subtest
    entry annotates at the parent test, with the entry's own location and its
    [subtest] line. [invocation] (default [`Mirrors]) and [armed] spell the
    hints. *)

val annotations :
  ?invocation:Run.invocation -> ?armed:string -> Run.result list -> string
(** [annotations results] is the concatenated {!annotation} lines for every
    failure of every counted failed result ({!Run.result.counted}), in run
    order; [""] when none. Excused expected failures produce no annotation. *)

(** {1:mutation Mutation lines}

    The lines a mutation run prints on the terminal renderer in every mode; the
    report itself is {!Report_sections.mutation_report}. *)

val mutation_armed : t -> id:string -> before:string -> after:string -> unit
(** [mutation_armed t ~id ~before ~after] prints the armed announcement
    ([mutant lib/calc.ml:9:12:add armed: a - b → a + b]), which an armed process
    prints before any other output (guarantee 12). *)

val mutation_killed : t -> unit
(** [mutation_killed t] prints [mutant killed.], closing an armed run whose
    mutant made a test fail. *)

val mutation_survived : t -> hits:int -> unit
(** [mutation_survived t ~hits] prints
    [mutant survived: the armed site was evaluated 3 time(s) and no test
     failed.], closing an armed run that completed green with the site evaluated
    [hits] times. *)

val mutation_not_evaluated : t -> unit
(** [mutation_not_evaluated t] prints
    [mutant not evaluated: no selected test ran the site.], closing an armed run
    that never evaluated the site. *)

val mutation_report : t -> Report_sections.mutation -> unit
(** [mutation_report t m] prints {!Report_sections.mutation_report} of [m] under
    [t]'s invocation and styling. *)

(** {1:projections Failure projections}

    {!Report_sections}'s failure projection, re-exported. *)

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

(** {1:running Running, reported} *)

val run :
  ?on_event:(Run.event -> unit) ->
  suite:string ->
  Run.config ->
  Test_tree.t list ->
  (Run.outcome, Run.startup_error) result
(** [run ~suite config tests] is {!Run.execute}[ config ~suite tests] with the
    run's whole report on standard output: the transcript on {!terminal},
    {!observe}d as the run happens, inside the [::group::] envelope when
    [config.github], then for a run that happened {!finish} over {!Run.results}
    and {!Run.baselines}, the envelope's close and the {!annotations} block
    after it sitting between the sections and the summary, then the {!refusals}
    lines on standard error and {!Report_junit.write} to [config.junit], last.
    Both standard formatters are flushed before it returns.

    [Ok outcome] is the executor's outcome, reported. [Error error] is a refused
    startup: the envelope is closed and {!Run.startup_message} is on standard
    error; the caller decides what to do with {!Run.startup_exit_code}.

    [on_event] (default: ignore) is a second subscriber to {!Run.execute}'s
    observer, composed after the transcript's; it must be total.

    Everything reports through [Format.std_formatter] and the standard
    descriptors, so a caller that forks mid-run flushes both formatters and both
    descriptors before every fork. Reads the environment, writes standard output
    and the JUnit file when [config.junit] is set, plus everything
    {!Run.execute} does. *)
