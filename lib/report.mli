(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The run report: the terminal transcript, the GitHub Actions envelope, and
    the one composition that executes a suite and reports it.

    A report is a projection of run data — {!Run.result} rows and the typed
    {!Failure.t} payloads inside them — written under one [ansi] decision made
    when the renderer is built; nothing here alters status, counts or
    scheduling. The failure blocks, the section vocabulary and the coverage and
    mutation reports live in {!Report_sections}, whose failure projection this
    module re-exports; the JUnit document is {!Report_junit}'s.

    {b The transcript.} By default (compact) a run prints, while it runs, only
    the erasable live tail naming the executing test on a terminal, and at the
    end the failure blocks, the slow block, the flaky block, the baseline report
    and the summary line. The header ([mylib: 48 tests (seed s1:…)]) prints iff
    there is a block to print — a finish-time rule over the results — so a
    green, healthy run is exactly one named line ([mylib: 48 passed in 1.2s.],
    the root seed appended when the header carried one). Under
    [Run.config.verbose] the header prints at once and one status line streams
    per test, the same blocks and summary follow, and the slowest tests close
    the transcript. Exact layout — column positions, rule widths, display bounds
    — is illustrative, not contract.

    {!run} is the composition the facade and the mutation loop call:
    {!Run.execute} observed by {!observe}, inside the GitHub envelope when
    [Run.config.github], then {!finish}, {!report_baselines}, the annotations
    and the JUnit report. *)

(** {1:renderer The renderer} *)

type t
(** The type for transcript renderer state: the sink, the presentation decisions
    read off the configuration at {!create}, and the counters behind the live
    display. Presentation state only — dropping a renderer loses no run data. *)

val create : out:Format.formatter -> ansi:bool -> ?live:bool -> Run.config -> t
(** [create ~out ~ansi config] is a renderer writing to [out] with:

    - [ansi], whether styling and diff-highlight colors are emitted. Under
      [ansi:false] the transcript contains no escape codes at all: sequences
      arriving inside test names or captured output are stripped; under
      [ansi:true] they pass through. Compared {e values} are escaped into
      visible text under either setting ({!pp_failure}).
    - [live], whether {!begin_test} maintains a self-erasing progress display
      with terminal cursor controls. Pass the sink's TTY status; under
      [ansi:false] it is off regardless. Defaults to [false].
    - [config.verbose], [config.slow_threshold] and [config.invocation], read
      once here: the level, the seconds a test not tagged ["slow"] may take
      before it earns a slow warning ([0.] disables the warnings), and the hint
      context every acceptance and replay line derives from.

    Test and suite names print with C0 control bytes and DEL escaped OCaml-style
    on every terminal surface ({!Report_sections.sanitize_name}). The width is
    fixed at [80] columns and a failure block shows the last [10] lines of the
    {!Failure.tail} the capture retained, with the full log's path beside them.

    Raises [Invalid_argument] if [config.slow_threshold] is negative or not
    finite. *)

val terminal : Run.config -> t
(** [terminal config] is the run's renderer on [Format.std_formatter]: color
    from {!Env.resolve_color} over [config.color], the terminal status and
    [INSIDE_DUNE]/[TERM]; the live tail on iff standard output is a terminal and
    the run is not under GitHub Actions, where cursor controls would land in the
    folded CI log.

    Effects: reads the environment and the terminal status of standard output.
*)

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
    prints it at {!finish} iff the run has a block to print, and names the suite
    in its summary line otherwise. [seed] is shown when given — {!run} passes
    the root seed iff the suite declares property tests — and [tests] is the
    number of selected tests, which also scales the live tail's [[k/n]] counter.

    [declared] is how many tests the suite declares before selection (defaulting
    to [tests]) and [selection] describes the active selection in the caller's
    words ({!selection_description}). Neither is printed by the header: they are
    what lets the summary say {e why} nothing ran when a selection comes back
    empty. *)

val begin_test : t -> path:string list -> unit
(** [begin_test t ~path] shows the test at [path] on the live progress display —
    [Running [3/48] name…] under [verbose], a faint [  [3/48] name…] tail
    otherwise — erased before anything else prints, so what stays on screen is
    exactly what a pipe sees. Prints nothing unless [live] and [ansi] are set.
*)

val result : t -> Run.result -> unit
(** [result t r] erases the live display and, under [verbose], prints [r]'s
    status line: status tag, full path ({!Test_tree.path_to_string}), duration,
    and the attempt count when [r.attempts > 1]; [SKIP] lines print the skip
    reason instead of a duration; a passing property with collected labels
    prints its label distribution under the line. Compact prints nothing per
    test.

    Classification is record-driven: a failing result that did not count ([Fail]
    outcome, [r.counted = false]) is an {e excused} expected failure and renders
    as a dim [XFAIL] tag with [r.xfail]'s reason; an [xfail] test that
    {e passed} arrives as a counted failure whose message names the reason, so
    its [FAIL] line is already loud. *)

val note : t -> string -> unit
(** [note t line] prints the run-scoped notice [line] — the executor announces
    fixture releases with it ([releasing db]) — on its own line under [verbose],
    and as an erasable live line otherwise (under [live]), so a hanging fixture
    release still names itself on a terminal while a compact transcript stays
    silent. *)

val observe :
  t -> seed:Seed.seed option -> selection:string option -> Run.event -> unit
(** [observe t ~seed ~selection event] streams [event] through [t]: the header
    on [Run_started] (with [seed] and [selection]), the live tail on
    [Test_started], the status line on [Test_finished], the release notice on
    [Fixture_release]. Total: it never raises for an event. *)

val selection_description : Run.config -> string option
(** [selection_description config] describes what narrows the run — the filter,
    exclusion, tags, [--failed], the shard — in the spelling the reader typed
    ([filter "parser"], [tag "a", "b" and shard 1/3]), or [None] when nothing
    narrows it. Quoted for a reader, not for OCaml: control characters are
    escaped and the rest is verbatim. *)

val empty_selection_reason :
  declared:int -> selection:string option -> string option
(** [empty_selection_reason ~declared ~selection] is why a run selected nothing,
    as the clause {!finish} puts after ["no tests ran: "] —
    ["the suite declares none"] when [declared] is [0], else
    ["<selection> matched none of N tests"] when something narrowed it, else
    [None], a non-empty suite nothing narrowed having nothing to explain.
    Exported for [--list], which selects and stops. *)

type coverage_summary = { visited : int; total : int }
(** The type for the run-end coverage summary the transcript's last line draws
    ({!snapshot_coverage}). *)

val finish :
  t ->
  ?coverage:coverage_summary ->
  results:Run.result list ->
  duration:float ->
  unit ->
  unit
(** [finish t ~results ~duration ()] ends the transcript. The run is
    {e noteworthy} iff [results] hold a counted failure, a completed test over
    the slow threshold that is not [slow_tagged], or a flaky test — a passing
    row with [attempts > 1]. A compact run that is not noteworthy prints exactly
    one line: the summary prefixed with the suite name {!header} recorded,
    [ (seed s1:…)] appended when the header carried a seed. Otherwise (under
    [verbose], or a noteworthy compact run, whose header prints here first) the
    transcript ends with:

    - the failure section — every {e counted} failed result re-printed in full
      ([FAIL] header with the attempt count when [r.attempts > 1], then
      {!pp_failure} with source excerpts for each of its failures, the property
      label table, then its bounded captured-output tail and full-log path) —
      when any test failed;
    - the slow block: [slow tests (n):], one indented entry per test past the
      threshold whose record is not [slow_tagged] (duration right-aligned,
      slowest first), and one faint hint naming the opt-outs — the ["slow"] tag
      and the threshold knob the invocation offers. None when the threshold is
      [0.]. A slow test that also failed keeps its failure block and earns its
      one warning;
    - the flaky block: [flaky tests (n):] and one [passed on attempt k  path]
      entry per passing result with [attempts > 1], in run order. A pass on
      retry is never silent;
    - the summary line ([46 passed, 2 failed in 1.2s.]): flaky tests count as
      passed, expected failures add their own segment
      ([44 passed, 2 expected failures in 1.2s.]), and counted failures with
      subtest-labeled entries state the sub-case count
      ([2 failed (3 subtest failures)]);
    - the slowest tests, on runs slow enough to care about — [verbose] only;
    - the coverage line
      ([coverage: 87.2% (312/358 points) · project: windtrap coverage]) when
      [coverage] is given, in every mode.

    Excused results leave the failure section and the failed count alone; skips
    never count as slow (their durations are not run time). The durations
    compared and shown are {!Run.result.duration}, attempts summed. *)

(** {1:baselines The baseline report} *)

val report_baselines : t -> Run.t -> unit
(** [report_baselines t run] prints what the run wrote for its baselines, one
    line per file ({!Baseline.writes} over [run]'s registry, paths spelled by
    {!Path_ops.display}): [wrote <path>.corrected] under [--corrected] and
    [accepted <path>] under [-u], a source file's line ending in
    [(N expectations)] for the literals patched in it; then one
    [could not write <path>: <reason>] line per refusal ({!Baseline.refusals}).
    Called after {!finish}. *)

(** {1:github The GitHub Actions envelope}

    Workflow commands for log folding and failure annotations, written to
    standard output at column zero when [Run.config.github]. Every function is
    pure and returns complete command lines, each ending in a newline. The
    envelope owns its transport's validity: annotation messages are
    ANSI-stripped and percent-encode newlines as [%0A] (with [%25]/[%0D] for [%]
    and CR), and command properties — [file], [line], [title] — additionally
    encode [:] and [,] as [%3A] and [%2C], so no payload can terminate or
    restructure a workflow command. *)

val group_start : string -> string
(** [group_start name] is the [::group::<name>] command line opening a folded
    log section named [name]. *)

val group_end : string
(** [group_end] is the [::endgroup::] command line. *)

val annotation :
  ?invocation:Run.invocation -> path:string list -> Failure.t -> string
(** [annotation ~path f] is one [::error] command line for [f], raised by the
    test at [path]: [file=]/[line=] properties from [f]'s location when it has
    one, whatever its {!Failure.attribution}, a [title] naming the test, and as
    message the unstyled {!pp_failure} block with its newlines [%0A]-encoded.
    Subtest failure entries annotate at the parent test: the [title] names the
    test whose body ran them, the location is the entry's own, and the
    [parent › name] label leads the message. [invocation] defaults to
    [`Mirrors]. *)

val annotations : ?invocation:Run.invocation -> Run.result list -> string
(** [annotations results] is the concatenated {!annotation} lines for every
    failure of every counted failed result in [results] ({!Run.result.counted}),
    in run order; [""] when no test failed. Excused expected failures produce no
    annotation: an [::error] on a PR demands action, and an excused failure
    demands none. *)

(** {1:coverage The coverage seam} *)

val snapshot_coverage : unit -> coverage_summary option
(** [snapshot_coverage ()] is what instrumented code registered in this process,
    scoped by [WINDTRAP_COVERAGE_ONLY], or [None] when nothing registered. Core
    windtrap's entire coverage coupling is this read at run end. *)

(** {1:mutation Mutation lines}

    The lines a mutation run owes on the terminal renderer, each printed in
    every mode; the report itself is {!Report_sections.mutation_report} drawn
    through {!mutation_report}. *)

val mutation_armed : t -> id:string -> before:string -> after:string -> unit
(** [mutation_armed t ~id ~before ~after] prints the armed announcement
    ([mutant lib/calc.ml:9:12:add armed: a - b → a + b]). Law 16(b): a process
    with a mutant armed says so before any other output. *)

val mutation_killed : t -> unit
(** [mutation_killed t] prints [mutant killed.], the line that closes an armed
    run whose mutant made a test fail. *)

val mutation_survived : t -> hits:int -> unit
(** [mutation_survived t ~hits] prints
    [mutant survived: the armed site was evaluated 3 time(s) and no test
     failed.], the closing line of an armed run that completed green with the
    site evaluated [hits] times. *)

val mutation_not_evaluated : t -> unit
(** [mutation_not_evaluated t] prints
    [mutant not evaluated: no selected test ran the site.], the closing line of
    an armed run that never evaluated the site. *)

val mutation_not_saved : t -> unit
(** [mutation_not_saved t] prints
    [verdicts not saved: this run's selection narrows the suite, …], the line a
    narrowed mutation run prints in place of writing its verdict file. *)

val mutation_report : t -> Report_sections.mutation -> unit
(** [mutation_report t m] prints {!Report_sections.mutation_report} of [m] under
    [t]'s invocation and styling. *)

(** {1:projections Failure projections}

    {!Report_sections}'s failure projection, re-exported: the terminal failure
    blocks, the JUnit document and the GitHub annotations all draw one
    {!Failure.t} the same way. *)

val headline : Failure.t -> string
(** [headline] is {!Report_sections.headline}. *)

val is_subtest_failure : Failure.t -> bool
(** [is_subtest_failure] is {!Report_sections.is_subtest_failure}. *)

val labeled_msg : Failure.t -> string option
(** [labeled_msg] is {!Report_sections.labeled_msg}. *)

val pp_failure :
  ansi:bool ->
  ?excerpt:bool ->
  ?filter:string ->
  ?invocation:Run.invocation ->
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
    run's whole report on standard output: the transcript on {!terminal} (the
    header's seed is the root seed iff a test carries {!Tag.prop}, its selection
    {!selection_description}), inside the [::group::] envelope when
    [config.github], then — for a run that happened — {!finish} over
    {!Run.results} with {!snapshot_coverage} when [config.coverage],
    {!report_baselines}, the envelope's close, the {!annotations} block after it
    so it is never folded away, and {!Report_junit.write} to [config.junit],
    last, from the rows the terminal has already shown. Both standard formatters
    are flushed before it returns.

    [Ok outcome] is the executor's outcome, reported. [Error error] is a refused
    startup: the envelope is closed and {!Run.startup_message} is on standard
    error, so all the caller decides is what to do with
    {!Run.startup_exit_code}.

    [on_event] is a second subscriber to {!Run.execute}'s single observer,
    composed after the transcript's rather than replacing it, and subject to the
    same contract: it must be total. Defaults to ignoring. The mutation loop
    subscribes with it to build its reach map while the dry run prints its
    ordinary output.

    Everything here reports through [Format.std_formatter] and the standard
    descriptors, so a caller that forks mid-run must flush both formatters and
    both descriptors before every fork.

    Effects: reads the environment ({!terminal}), writes standard output, the
    JUnit file when [config.junit] is set, and everything {!Run.execute} does.
*)
