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

    A compact run prints, while it runs, only the erasable live tail on a
    terminal, and at the end the failure blocks, the slow block, the flaky
    block, the baseline report and the summary line; the header prints iff there
    is a block to print, so a green run is one line. Under [Run.config.verbose]
    the header prints at once, one status line streams per test, and the slowest
    tests close the transcript. Exact layout (column positions, rule widths,
    display bounds) is not contract. *)

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
    regardless under [ansi:false]) is whether {!begin_test} maintains a
    self-erasing progress display; pass the sink's TTY status. [config.verbose],
    [config.slow_threshold] ([0.] disables slow warnings) and
    [config.invocation] are read once here. Names print with C0 control bytes
    and DEL escaped ({!Report_sections.sanitize_name}); the width is [80]
    columns and a failure block shows the last [10] lines of the captured tail
    with the full log's path.

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
    prints it at {!finish} iff the run has a block to print. [seed] is shown
    when given; [tests] is the number of selected tests and scales the live
    tail's [[k/n]] counter. [declared] (default [tests]), how many tests the
    suite declares before selection, and [selection] ({!selection_description})
    are not printed; the summary uses them to say why nothing ran. *)

val begin_test : t -> path:string list -> unit
(** [begin_test t ~path] shows the test at [path] on the live display
    ([Running [3/48] name…] under [verbose], a faint [  [3/48] name…] tail
    otherwise), erased before anything else prints. Prints nothing unless [live]
    and [ansi] are set. *)

val result : t -> Run.result -> unit
(** [result t r] erases the live display and, under [verbose], prints [r]'s
    status line: status tag, full path ({!Test_tree.path_to_string}), duration
    (the skip reason for a [SKIP]), the attempt count when [r.attempts > 1], and
    a passing property's label distribution under it. A failing result that did
    not count ([r.counted = false]) renders as a dim [XFAIL] tag with
    [r.xfail]'s reason; an [xfail] test that passed arrives as a counted failure
    whose message names the reason. Compact prints nothing per test. *)

val note : t -> string -> unit
(** [note t line] prints the run-scoped notice [line] ([releasing db]) on its
    own line under [verbose], as an erasable live line under [live] otherwise,
    and not at all elsewhere. *)

val observe :
  t -> seed:Seed.seed option -> selection:string option -> Run.event -> unit
(** [observe t ~seed ~selection event] streams [event] through [t]: the header
    on [Run_started], the live tail on [Test_started], the status line on
    [Test_finished], the release notice on [Fixture_release]. Never raises. *)

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

val finish : t -> results:Run.result list -> duration:float -> unit -> unit
(** [finish t ~results ~duration ()] ends the transcript. A compact run with
    nothing to show (no counted failure, no completed test over the slow
    threshold that is not [slow_tagged], no passing row with [attempts > 1])
    prints exactly one line: the summary prefixed with the suite name, the seed
    appended when the header carried one. Otherwise the transcript ends with the
    failure section (every counted failed result: [FAIL] header with the attempt
    count when [r.attempts > 1], each failure through {!pp_failure} with source
    excerpts, the property label table, the bounded captured tail and full-log
    path); the slow block ([slow tests (n):], slowest first, with one hint
    naming the ["slow"] tag and the threshold knob; none when the threshold is
    [0.]); the flaky block ([flaky tests (n):], one [passed on attempt k path]
    per passing result with [attempts > 1], in run order); the summary line
    ([46 passed, 2 failed in 1.2s.]; flaky tests count as passed, expected
    failures add [2 expected failures], subtest-labeled entries add
    [(3 subtest failures)]); and under [verbose] the slowest tests, on runs of
    at least five seconds with at least five timed tests. Excused results leave
    the failure section and the failed count alone; skips never count as slow.
    Durations are {!Run.result.duration}, attempts summed. *)

(** {1:baselines The baseline report} *)

val report_baselines : t -> Run.t -> unit
(** [report_baselines t run] prints one line per file the run wrote for its
    baselines ({!Baseline.writes}, paths through {!Os.display_path}):
    [wrote <path>.corrected] under [--corrected], [accepted <path>] under [-u],
    a source file's line ending in [(N expectations)]; then one
    [could not write <path>: <reason>] per {!Baseline.refusals} entry. Called
    after {!finish}. *)

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
  ?invocation:Run.invocation -> path:string list -> Failure.t -> string
(** [annotation ~path f] is one [::error] command line for [f], raised by the
    test at [path]: [file=]/[line=] from [f]'s location when it has one,
    whatever its {!Failure.attribution}, a [title] naming the test, and as
    message the unstyled {!pp_failure} block with newlines [%0A]-encoded. A
    subtest entry annotates at the parent test, with the entry's own location
    and its [parent › name] label leading the message. [invocation] defaults to
    [`Mirrors]. *)

val annotations : ?invocation:Run.invocation -> Run.result list -> string
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

val mutation_not_saved : t -> unit
(** [mutation_not_saved t] prints
    [verdicts not saved: this run's selection narrows the suite, …], printed by
    a narrowed mutation run in place of writing its verdict file. *)

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
    header's seed is the root seed iff a test carries {!Test_tree.Tag.prop}),
    inside the [::group::] envelope when [config.github], then for a run that
    happened {!finish} over {!Run.results}, {!report_baselines}, the envelope's
    close, the {!annotations} block after it, and {!Report_junit.write} to
    [config.junit], last. Both standard formatters are flushed before it
    returns.

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
