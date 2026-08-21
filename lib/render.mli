(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The terminal renderer: the run transcript on standard output.

    [Render] projects run data into the terminal transcript: a header, one
    progress mark per test — by default a compact glyph row (green [.] pass, red
    [F] fail, yellow [S] skip, faint [x] expected failure), one status line per
    test with its timing under [`Verbose] — failed tests re-printed in full at
    the end of the run — location, source excerpt, highlighted diff,
    counterexample and replay line, acceptance command, bounded captured-output
    tail with the full-log path — then the slow warnings, the summary line, the
    slowest tests ([`Verbose] only), and the coverage line. The mode decides
    {e what} prints; the sink decides only color and the erasable live tail — no
    sink changes shape, and committed glyphs are flushed one by one so a crashed
    run leaves its partial row visible.

    {b The noteworthy rule.} A compact-mode run prints its header and glyph row
    only when the run is {e noteworthy}: a counted failure, or a completed test
    over the slow threshold that is not tagged ["slow"]. Until the first such
    event the header and glyphs buffer; the event commits them and everything
    after streams live. A run that stays green and healthy ends as {e one} named
    summary line ([mylib: 48 passed in 1.2s.], with the root seed appended when
    the header carried one) — see {!result} and {!finish}. [`Verbose] never
    defers.

    Deferral deliberately trades crash evidence for silence: a compact run
    killed {e before} its first noteworthy event leaves {b nothing} in a pipe or
    CI log for this suite — the buffered header and rows die with the process.
    That is the contract, not an accident: dune's own status line names the
    running action, and on a TTY the erasable {!begin_test} tail names the
    running test, so a hang is still attributable. From the flush on, the
    streaming law holds as before — a kill after it leaves the header and the
    partial row in the pipe.

    Renderers are projections: everything printed derives from {!Run.result}
    values and the typed {!Failure.t} payloads inside them. Acceptance and
    replay command lines are composed here from snapshot names, paths, and root
    seed tokens (the replay line prints the {e root} token only); diffs are
    computed here from the rendered values via {!Diff}; styling is applied here
    through {!Pp} under the explicit [ansi] decision made at {!create} — nothing
    reads the environment or the terminal. A renderer never alters status,
    counts, or scheduling.

    The runner drives one {!type:t} through the run: {!header} once, then
    {!begin_test}/{!result} per test, then {!finish}. {!headline} and
    {!pp_failure} are the shared failure projections the {!Render_junit} and
    {!Render_github} transports reuse, with [ansi:false].

    Exact layout — column positions, rule widths, the slowest-5 threshold,
    display bounds on diffs and proposed contents — is illustrative, not
    contract: tuned constants, not frozen bytes. *)

(** {1:renderer The renderer} *)

type t
(** The type for terminal renderer state: the output sink, the presentation
    flags fixed at {!create}, and the progress counters behind the glyph row and
    the live display. Presentation state only — dropping a renderer loses no run
    data. *)

type invocation = [ `Exe of string | `Mirrors ]
(** The type for hint invocation contexts: how a command hint spells a re-run of
    this suite. [`Exe cmd] is a command that re-runs this executable, which
    hints complete with CLI flags ([cmd --failed], [cmd -u],
    [cmd --seed … -f …]) — the library runner passes ["dune exec <path> --"]
    under dune and [argv.(0)], verbatim, standalone. [`Mirrors] means no CLI
    exists and hints spell [WINDTRAP_*] environment prefixes to [dune runtest] —
    the inline runner's context, and the default. The driver computes it once at
    startup; every acceptance, replay, and prune line derives from it, so no
    hint can name an invocation that would not re-run the suite. *)

type settings = {
  color : Env.color_mode;
      (** [--color]/[WINDTRAP_COLOR]: the color preference. The driver resolves
          it against the sink's terminal status ({!Env.resolve_color}) into
          {!create}'s [ansi] — this module never sniffs. *)
  tail_errors : int option;
      (** [WINDTRAP_TAIL_ERRORS]: captured-output lines shown per failure
          ({!create}'s [tail_lines]); [None] leaves the default. *)
  slow_threshold : float;
      (** [--slow-threshold]/[WINDTRAP_SLOW_THRESHOLD]: seconds a test not
          tagged ["slow"] may take before the run counts as noteworthy ([0.]
          disables). Invariant: finite and non-negative, validated by the CLI
          layer. *)
}
(** The type for resolved renderer settings: the presentation knobs the CLI
    layer resolves ({!Cli.settings}) and the runner never reads — they are
    deliberately not {!Run.config} fields, because no level or width can change
    outcomes or exit codes. The driver applies them when it constructs the run's
    renderer ({!Driver.renderer}). *)

val default_settings : settings
(** [default_settings] is the settings with every knob at its built-in default:
    [color = Env.Auto], no width or tail override, [slow_threshold = 1.]. *)

val create :
  out:Format.formatter ->
  ansi:bool ->
  ?mode:[ `Compact | `Verbose ] ->
  ?live:bool ->
  ?columns:int ->
  ?tail_lines:int ->
  ?slow_threshold:float ->
  ?invocation:invocation ->
  unit ->
  t
(** [create ~out ~ansi ()] is a renderer writing to [out] with:

    - [ansi], whether styling and diff-highlight colors are emitted. The caller
      decides from its color mode and the sink's terminal status
      ({!Env.resolve_color}); this module never sniffs. Under [ansi:false] the
      transcript contains no escape codes at all: sequences arriving inside test
      names or captured output (a user [pp] or program that styles) are
      stripped; under [ansi:true] they pass through. Compared {e values} are
      neither stripped nor passed through under either setting — they are
      escaped into visible text, see {!pp_failure}.
    - [mode], the verbosity level — one axis, [`Verbose] a superset of
      [`Compact]. [`Compact] (the default) prints the header and one glyph per
      test, deferred until the run proves noteworthy (the module preamble; a
      green, healthy run is one named line). [`Verbose] ([--verbose]) prints one
      status line per test instead of the glyph. Both print the same failure
      blocks and the same summary line.
    - [live], whether {!begin_test} maintains a self-erasing progress display
      with terminal cursor controls. Pass the sink's TTY status; under
      [ansi:false] it is off regardless. Defaults to [false].
    - [columns], the terminal width used to bound rules and the live display.
      Always [80] for a run — the transcript is a report, not a canvas, and one
      width keeps a pipe and a wide terminal byte-identical. The compact row
      wraps at 60 glyphs regardless. Renderer tests pass other widths.
    - [tail_lines], the maximum captured-output lines shown per failure block
      ([WINDTRAP_TAIL_ERRORS]). Defaults to [10].
    - [slow_threshold], the seconds a test not tagged ["slow"] may take before
      the run counts as noteworthy and the test earns a slow warning at
      {!finish} ([--slow-threshold]/[WINDTRAP_SLOW_THRESHOLD]). Defaults to
      [1.]; [0.] disables both the warnings and the noteworthy trigger — the
      compact row then flushes on a counted failure only.
    - [invocation], the hint context (see {!type:invocation}). Defaults to
      [`Mirrors].

    Test and suite names print with C0 control bytes and DEL escaped OCaml-style
    ([\n], [\t], [\xNN]) on every terminal surface — live tail, [FAIL] headers,
    verbose lines, summaries — so a payload-borne newline cannot split a header
    or leave live-tail residue; ESC follows the [ansi] policy above. Compared
    values inside a failure block follow the neighbouring but distinct rule at
    {!pp_failure}: they keep their newlines and tabs, which are their own
    layout, and they escape ESC whatever [ansi] says.

    Raises [Invalid_argument] if [columns < 20], [tail_lines < 0], or
    [slow_threshold] is negative or not finite. *)

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
(** [header t ~suite ~tests ~seed] records and, under [`Verbose], prints the run
    header ([mylib: 48 tests (seed s1:…)]). Under [`Compact] the line is
    deferred: the first noteworthy event prints it (see {!result}), and a green,
    healthy run never shows it — its named summary line carries [suite] and
    appends the seed instead.

    [seed] is shown when given; the runner passes the root seed iff the suite
    declares property tests — selection never changes it, so the token is stable
    across filtered runs — and [tests] is the number of selected tests, which
    also scales the live line's [[k/n]] counter.

    [declared] is how many tests the suite declares before selection (defaulting
    to [tests]) and [selection] describes the active selection in the caller's
    words (["filter \"parser\""], ["shard 3/8"], [None] when nothing narrows the
    run). Neither is printed by the header: they are what lets the summary say
    {e why} nothing ran when a selection comes back empty, instead of a bare
    ["no tests ran."]. The description belongs to the caller because the
    configuration does — this module only phrases it. *)

val begin_test : t -> path:string list -> unit
(** [begin_test t ~path] shows the test at [path] on the live progress display,
    erased before the next {!result} prints. Under [`Verbose] it is a
    self-erasing line ([Running [3/48] name…]); under [`Compact] a faint
    erasable tail after the last glyph of the current row ([  [3/48] name…]),
    erased so the committed row bytes equal the pipe bytes. It works from the
    start of the run, before any noteworthy flush: while the transcript is
    deferred the tail draws from column zero and never forces the header out —
    being erasable, it leaves no residue on a green run's one-line transcript.
    Prints nothing unless [live] and [ansi] are set. *)

val result : t -> Run.result -> unit
(** [result t r] prints [r]'s per-test progress, by mode:

    - [`Compact]: one glyph — green [.] pass, red [F] counted failure (assert,
      property, snapshot, timeout, unexpected pass), yellow [S] skip, faint [x]
      expected failure. Glyphs buffer until the run proves noteworthy: the first
      counted-failure result, or the first completed test with
      [r.duration >= slow_threshold] that is not [r.slow_tagged] (never when the
      threshold is [0.]), prints the deferred header, then the rows accumulated
      so far, and from there glyphs commit and flush one by one exactly as if
      they had streamed from the start — byte-identical, wraps and counters
      included. Rows wrap every 60 glyphs with a faint [ [k/n]] counter when the
      header gave a total, a bare newline otherwise; the failure blocks at
      {!finish} differentiate failure kinds, not the glyph.
    - [`Verbose]: one status line — status tag, full path
      ({!Test_tree.path_to_string}), duration, and the attempt count when
      [r.attempts > 1]. [SKIP] lines print the skip reason instead of a
      duration. A passing property with collected labels additionally prints its
      label distribution under the status line ([labels (100 passing cases):],
      one faint percentage line per label, the same projection as the failure
      blocks) — the calibration view for [collect]/[classify]; the other modes
      show distributions in failure blocks only.

    The output derives from [r] alone; [t] only counts marks for the wrap
    counter and the live display.

    Classification is record-driven: a failing result that did not count ([Fail]
    outcome, [r.counted = false]) is an {e excused} expected failure — the
    verbose line renders as a dim, informational [XFAIL] tag with [r.xfail]'s
    reason ([XFAIL  name (expected failure: issue #42)]) instead of a loud
    [FAIL], and the compact glyph as the faint [x]. An [xfail] test that
    {e passed} arrives as a counted failure whose message names the reason, so
    its [FAIL] line and block are already loud. Tests with [r.slow_tagged] are
    exempt from the slow threshold everywhere; skips never trigger it (their
    durations are not run time). The duration compared is {!Run.result.duration}
    — the attempts summed — so a retried test whose attempts together cross the
    threshold counts as slow even when its final attempt was fast. *)

val note : t -> string -> unit
(** [note t line] prints the run-scoped notice [line] on its own line and
    flushes, erasing the live display and closing a partial compact glyph row
    first — a notice printed straight to the sink would splice into the open
    row. While a compact transcript is still deferred the notice buffers with
    the rows — it prints in position if a noteworthy event flushes, and a green,
    healthy run keeps its one-line transcript — with an erasable live copy
    (under [live]) so a hanging fixture release still names itself on a
    terminal. The runner announces fixture releases with it ([releasing db]). *)

val empty_selection_reason :
  declared:int -> selection:string option -> string option
(** [empty_selection_reason ~declared ~selection] is why a run selected nothing,
    as the clause {!finish} puts after ["no tests ran: "] —
    ["the suite declares none"] when [declared] is [0], else
    ["<selection> matched none of N tests"] when something narrowed it
    ({!Driver.selection_description}), else [None], a non-empty suite nothing
    narrowed having nothing to explain.

    Exported for [--list], which selects and stops: a listing that answered a
    mistyped filter with silence would be the one place the suggestion
    ["(list the suite's tests with -l)"] leads nowhere. *)

type coverage_summary = {
  visited : int;  (** Instrumented blocks visited at least once. *)
  total : int;  (** Instrumented blocks in every registered file. *)
}
(** The type for the run-end coverage summary the transcript's last line draws
    ({!Driver.snapshot_coverage}). *)

val finish :
  t ->
  ?coverage:coverage_summary ->
  results:Run.result list ->
  duration:float ->
  unit ->
  unit
(** [finish t ~results ~duration ()] ends the transcript.

    A compact transcript still deferred here — no counted failure among
    [results] and no untagged test over the slow threshold — prints exactly one
    line: the summary prefixed with the suite name recorded by {!header}
    ([mylib: 48 passed in 1.2s.], skip and expected-failure segments as below),
    with [ (seed s1:…)] appended when the header carried a seed. No header, no
    glyph row, no slowest list; the coverage line, when [coverage] is given,
    still follows — it is the instrumentation's signal, not the run's. Otherwise
    (any mode, or a compact run made noteworthy) the transcript ends with:

    - the failure section — every {e counted} failed result of [results]
      re-printed in full ([FAIL] header, carrying the attempt count when
      [r.attempts > 1] in the same words {!result}'s status line uses, then
      {!pp_failure} with source excerpts for each of its failures, then its
      bounded captured-output tail and full-log path, printed once per test) —
      when any test failed;
    - the slow warnings: a faint-yellow block over every completed test past the
      slow threshold whose record is not [slow_tagged] — a [slow tests (n):]
      heading, then one indented entry per test with the duration in a
      right-aligned leading column ([  2.50s  parser › tokenize]), slowest first
      — and one faint hint line naming the opt-outs: the ["slow"] tag, and
      whichever threshold knob the [invocation] offers
      ([--slow-threshold SECONDS] under [`Exe], [WINDTRAP_SLOW_THRESHOLD] under
      [`Mirrors]). The duration shown and compared is {!Run.result.duration}
      (attempts summed, as {!result}); a slow test that also failed keeps its
      failure block and earns its one warning — the two report different things.
      None print when the threshold is [0.];
    - the summary line ([46 passed, 2 failed in 1.2s.]) from [results] and
      [duration], the run's wall-clock seconds. Expected failures add their own
      segment ([44 passed, 2 expected failures in 1.2s.]), and counted failures
      with subtest-labeled entries (see {!is_subtest_failure}) state the
      sub-case count ([2 failed (3 subtest failures)]);
    - the slowest tests, on runs slow enough to care about — [`Verbose] only:
      the list is diagnosis, not signal;
    - the coverage line
      ([coverage: 87.2% (312/358 points) · project: dune build @cover], the
      percentage styled by the runtime's thresholds — green at 80% and above,
      yellow at 60%, red below) when [coverage] is given. The hint is
      unconditional: an in-process number is one executable's view of the code
      it links, whatever else the project builds, and the merge is the project
      total. The caller omits [coverage] when [WINDTRAP_COVERAGE] switched the
      line off.

    Classification is record-driven, as {!result}: excused results — failing
    results that did not count ([r.counted = false]) — leave the failure section
    and the failed count alone: they did not fail the run, and a summary that
    counted them red would contradict the exit code (their stream lines already
    reported them as [XFAIL]). [slow_tagged] results are exempt from the slow
    warnings and from keeping a deferred compact transcript noteworthy; skips
    are exempt regardless.

    Failure blocks render each test's captured tail from the first
    {!Failure.tail} attached to its failures: the retained lines (at most
    [tail_lines]), what was omitted, and the tail's [log_path]. *)

(** {1:snapshots The snapshot report} *)

val report_snapshots : t -> orphans:string list -> Run.t -> unit
(** [report_snapshots t ~orphans run] prints the run's baseline maintenance
    lines on [t]'s sink: one [wrote <path> (new|updated)] line per accepted
    baseline ({!Snapshot.writes} over [run]'s registry, paths spelled by
    {!Path_ops.display} — the one producer for both runners), then
    {!stale_lines} over [orphans] ([Runner.outcome.orphans]) and the removal
    hint under them.

    The driver calls it after {!finish}, when the transcript is settled: lines
    go straight to the sink, outside the compact row and deferral machinery. *)

(** {1:sections Report sections}

    Instrumentation reports are data drawn by this module: coverage's per-file
    table and mutation's survivor blocks are two projections into one internal
    section vocabulary — the subsystem that owns the numbers builds the record
    data ({!Driver.coverage_data}, the mutation loop), and this module draws it
    knowing nothing about the runtimes that measured it. Every name a runtime
    owns (a mutant identifier, the arming variable) arrives in the data
    pre-spelled with the runtime's own functions, so the report and the runtime
    cannot disagree about what to type.

    Styling is data too: the renderer applies it under the [ansi] decision made
    at {!create}, so section data never carries escape codes and never has to
    know what sink it will meet. The source excerpts both reports show — the
    right-aligned line number, the [│] rule, the marker column, the [·····]
    between regions, and the [1-3, 7] range dialect — are one gutter renderer
    inside this module, not two layouts: a subsystem hands over which lines of
    which file, never how to draw them. *)

(** {1:coverage Coverage}

    The coverage detail projection: one layout serving the in-process
    [WINDTRAP_COVERAGE]/[--coverage] report modes and, through the facade's
    [Private], the [windtrap coverage] command over merged files — the inline
    report and the CI report cannot drift apart. Presentation only: the data
    arrives as the records below, built at the coverage seam
    ({!Driver.coverage_data}) from what the runtime measured — run data,
    rendered late. This module orders nothing and counts nothing, and it does
    not name the runtime. *)

type coverage_file = {
  file : string;  (** The source file name as recorded at instrumentation. *)
  visited : int;  (** Points visited at least once. *)
  total : int;  (** Points instrumented. *)
  uncovered : int list;
      (** The 1-based source lines the unvisited points touch, sorted, without
          duplicates. [[]] when [source] is [None] — lines cannot be attributed
          without the text. *)
  source : string option;
      (** The source text, when the builder found it and it is consistent with
          the recorded data; the excerpt block needs it. *)
  stale : bool;
      (** [true] when the source was found but changed since the data was
          recorded. [source] is then [None] and [uncovered] is [[]]: the line
          states the staleness and the fix rather than painting lines of code
          the data does not describe. *)
}
(** The type for one line of the per-file table. *)

type coverage = {
  visited : int;  (** Points visited at least once, over all files. *)
  total : int;  (** Points instrumented, over all files. *)
  files : coverage_file list;
      (** The per-file table, in the order it prints — ordered by file name by
          the builder. *)
}
(** The type for a whole coverage report. Every field is measured, not derived
    here. *)

val coverage_report : t -> mode:[ `Report | `Full ] -> coverage -> unit
(** [coverage_report t ~mode c] prints the coverage block for [c]:

    - the summary line, as {!finish}'s without the discoverability hint
      ([coverage: 87.2% (312/358 points)]);
    - one line per file — percentage (styled by the frozen thresholds the
      summary line uses: green at 80% and above, yellow at 60%, red below),
      visited/total, file name, and the uncovered line ranges
      ([uncovered: 88-94, 121]), bounded at eight regions and then
      [(+N more, -u shows them)] — a row no terminal can lay out is not a
      report. A fully covered file has no range list; a stale file states the
      staleness and the fix instead of ranges it cannot attribute; a file whose
      unvisited points have no line attribution notes the missing source;
    - under [`Full], source excerpts for each file with uncovered lines and a
      readable source: a heading ([lib/eval.ml — 75.0% (111/148)]), then each
      uncovered region with one line of context, uncovered lines carrying a
      gutter marker, regions separated by [·····].

    The caller prints it after {!finish}, having withheld [finish]'s [coverage]
    argument. *)

(** {1:mutation Mutation}

    The mutation report: the survivor blocks, the unreached blocks, the one
    summary line and the reproduce footer. One layout serving the mutation
    loop's per-executable report and, through the facade's [Private], the
    [windtrap mutate] command's aggregate over merged verdict files — the
    interactive report and the CI report cannot drift apart.

    A survivor is a failure block, not a new vocabulary: the same labelled rule,
    the same [  VERB  subject] head row, the same excerpt row, and red, because
    it is a defect report about a named test. An unreached mutant is the same
    block without the sentence, in yellow. Presentation only — the ordering and
    the witness lists are the producer's; the counts are the lists'. *)

val mutation_armed : t -> id:string -> before:string -> after:string -> unit
(** [mutation_armed t ~id ~before ~after] prints the armed announcement
    ([mutant lib/calc.ml:9:12:add armed: a - b → a + b]). Law 16(b) makes it
    normative: a process with a mutant armed says so before any other output, so
    a run whose output does not say so has none. *)

val mutation_killed : t -> unit
(** [mutation_killed t] prints [mutant killed.] — the line that closes the
    arm-and-watch loop, printed after the transcript of a run whose armed mutant
    made a test fail. Prints in every mode, as {!mutation_armed} does. *)

val mutation_survived : t -> hits:int -> unit
(** [mutation_survived t ~hits] prints
    [mutant survived: the armed site was evaluated 3 time(s) and no test
     failed.] — the closing line of an armed run that completed green with the
    site evaluated [hits] times. Its counterpart {!mutation_not_evaluated} is
    what makes it a claim: without the pair, "the tests prove nothing" and "the
    tests never ran the line" would both be a silent green transcript. Prints in
    every mode, as {!mutation_armed} does. *)

val mutation_not_evaluated : t -> unit
(** [mutation_not_evaluated t] prints
    [mutant not evaluated: no selected test ran the site.] — the closing line of
    an armed run that completed without evaluating the armed site: the run says
    nothing about the mutant, and the reader's fix is the selection, not the
    tests. Prints in every mode, as {!mutation_armed} does. *)

val mutation_not_saved : t -> unit
(** [mutation_not_saved t] prints
    [verdicts not saved: this run's selection narrows the suite, …] — the line a
    mutation run whose selection narrowed the suite prints in place of writing
    its verdict file: the file carries no partial-run marking, so a narrowed
    run's selection-relative verdicts would stand in the project merge as the
    executable's whole answer. Prints in every mode: it qualifies what the run
    just did not persist. *)

type witness = {
  test : string;
      (** The test's full path, as {!Test_tree.path_to_string} spells it
          ([calc › sub of two positives]). *)
  loc : Loc.t option;  (** Where the test is declared, when it is known. *)
  exe : string option;
      (** The test executable that ran the test, shown as the row's first
          column. [None] in a per-executable report, where every witness is this
          executable's and the column is omitted; the aggregate names each
          witness's executable, and a block whose witnesses span several says so
          in its sentence. *)
}
(** The type for survivor witnesses: a test that evaluated the mutated line and
    did not fail when it changed. *)

type mutant = {
  id : string;
      (** The mutant's identifier in the runtime's canonical spelling
          ([lib/calc.ml:9:12:add]) — spelled by the producer, which holds the
          runtime, so this module spends none of the Law-12 coupling budget
          re-spelling it. *)
  file : string;  (** The mutated source file, for the excerpt row. *)
  line : int;  (** 1-based line of the mutated expression. *)
  before : string;  (** The original expression's source text. *)
  after : string;  (** The armed expression's source text. *)
  source : string option;
      (** The mutated file's text, when the producer could read it; the excerpt
          row is dropped when it could not, as every excerpt is best-effort. *)
}
(** The type for one mutant as a block draws it: the head row and the excerpt
    row a survivor and an unreached mutant share. *)

type survivor = {
  mutant : mutant;  (** The mutant that survived. *)
  witnesses : witness list;
      (** The tests that ran the line and did not fail. Never empty for a
          verdict — a mutant no test evaluated is {e unreached}, a different
          finding with a different remedy — so the block always names someone to
          go and strengthen. Complete, never truncated: the sentence above the
          list counts this list, so a caller that dropped witnesses would print
          a count no reader could reconcile with what follows it. *)
}
(** The type for one survived mutant, as the report shows it. *)

(** The type for what a report's reached count is relative to. It spells the
    summary line's subject — [5 reached by this suite],
    [2 reached by the 2 selected tests], [12 reached · 3 executables] — and
    nothing else. *)
type scope =
  | Suite  (** A per-executable run over its whole suite. *)
  | Selected of int
      (** A per-executable run whose selection narrowed the suite to this many
          tests — the reach is theirs, not the suite's. *)
  | Executables of int
      (** The aggregate over this many executables' verdict files. *)

type mutation = {
  arm_variable : string;
      (** The runtime's arming variable ([WINDTRAP_MUTATE_ARM]), spelled by the
          producer with the runtime's own function — the reproduce footer
          completes it with the [<id>] placeholder and the invocation, so the
          report and the runtime cannot disagree about what to type. *)
  survivors : survivor list;
      (** Every mutant that survived, one block each, ordered by witness count
          descending, then by identifier. Never capped: a survivor is a failure
          block, and windtrap caps no failure block. *)
  unreached : mutant list;
      (** Every mutant no test evaluated, one block each, ordered by identifier.
          Aggregate only: a per-executable report never lists or counts them —
          one executable's unreached mutant is usually another's reached one,
          and only the merge knows — so the loop hands over [[]]. *)
  killed : int;  (** How many mutants were killed. *)
  scope : scope;  (** What the reached count is relative to. *)
  filter : string option;
      (** The run's [-f] filter, when it had one, restated in the reproduce
          footer as the property replay line restates its own — a survivor of a
          filtered run survived that selection, so the footer reproduces that
          run. [None] for the aggregate, which ran nothing. *)
}
(** The type for a whole mutation report. The reached count is
    [killed + List.length survivors], a count of the lists, not a field: the
    summary cannot disagree with the blocks above it. *)

val mutation_report : t -> mutation -> unit
(** [mutation_report t m] prints [m]:

    - the survivor section, when [m.survivors] is not empty — the labelled rule
      ([survivors (2)]) and one block per survivor, separated by a blank line. A
      block is the head row
      ([  SURVIVED  lib/calc.ml:9:12:add    a - b  →  a + b], the identifier
      column aligned across the section), the excerpt row for the mutated line,
      and the sentence that is the product
      ([3 tests ran this line and none failed:], singular
      [1 test ran this line and did not fail:], and
      [3 tests in 2 executables ran this line and none failed:] when the
      witnesses name more than one executable) over one indented row per witness
      — the executable when any witness of the report names one, the test's
      name, and its declaration site, in columns aligned across the report;
    - the unreached section, when [m.unreached] is not empty — the labelled rule
      ([never reached (2)]) and one block per mutant: the head row
      ([  UNREACHED  lib/calc.ml:22:5:le   n < limit  →  n <= limit]) and the
      excerpt row, no sentence;
    - the closing rule, when either section printed;
    - the summary line, terms separated by [·] and the zero terms omitted —
      [0 survived] never prints, the reached count standing alone as the clean
      form, and neither does [0 killed] or [0 never reached]:
      [mutants: 1 survived of 5 reached by this suite · 4 killed],
      [mutants: 5 reached by this suite · 5 killed],
      [mutants: 2 reached by the 2 selected tests · 2 killed],
      [mutants: 1 survived of 12 reached · 11 killed · 2 never reached · 3
       executables]. [N survived] is red, [N killed] green, [N never reached]
      yellow; the rest is plain;
    - the reproduce footer, when either section printed: the command that arms
      one mutant with the literal [<id>] where the reader pastes one, spelled
      from the invocation and [m.arm_variable]
      ([reproduce: WINDTRAP_MUTATE_ARM=<id> dune exec --instrument-with
        ppx_windtrap.mutate test/test_calc.exe]) — under [`Mirrors] it is
      [dune runtest --force --instrument-with ppx_windtrap.mutate], because a
      build without the backend has no mutant to arm and a warm tree would
      replay the cached run. [m.filter] rides it as the replay line's filter
      does: [-f '<filter>'] after the command under [`Exe],
      [WINDTRAP_FILTER='<filter>'] before [dune runtest] under [`Mirrors]. No
      colour, as in every hint.

    Prints in every mode. *)

(** {1:projections Failure projections}

    The kind-by-kind projection of one {!Failure.t}, shared by the terminal
    failure blocks and the {!Render_junit} and {!Render_github} transports.
    Everything derives from the typed payload: no formatting happens at failure
    sites. *)

val headline : Failure.t -> string
(** [headline f] is a one-line, unstyled summary of [f]
    ([expected true, got false], [snapshot "help": no baseline], …), for
    transports that need a single-line field (JUnit [message] attributes).
    Newlines and escape codes cannot occur — payload-borne ANSI sequences are
    stripped; long payload renderings are truncated with an ellipsis. *)

val stale_lines : string list -> string list
(** [stale_lines orphans] is one [stale baseline: <path>] line per orphan, in
    order, paths spelled by {!Path_ops.display}, followed by a
    [remove them: rm <paths>] line — a baseline is a committed file, so the
    report names the removal rather than performing it. *)

val is_subtest_failure : Failure.t -> bool
(** [is_subtest_failure f] is [true] iff [f] was recorded inside {!Run.subtest}:
    the failure's [subtest] components are non-empty. The terminal summary
    counts such entries as sub-cases and {!Render_junit} projects them as
    separate testcases. Classification is record-driven — a user [?msg] spelling
    out a [leaf › name] prefix stays an ordinary annotation. *)

val labeled_msg : Failure.t -> string option
(** [labeled_msg f] is [f]'s [msg] slot as reports display it: for a sub-case
    entry, the [leaf › name] label derived from [f]'s [subtest] components, with
    the user's [?msg] joined after [": "] when there is one; for a plain
    failure, the [?msg] annotation itself. The one derivation, shared by the
    failure block, the headline, and {!Render_junit}'s testcase names. *)

val pp_failure :
  ansi:bool ->
  ?excerpt:bool ->
  ?filter:string ->
  ?invocation:invocation ->
  Format.formatter ->
  Failure.t ->
  unit
(** [pp_failure ~ansi ppf f] formats [f]'s full report block: the phase (when
    not {!Failure.Body}) and location header, the [?msg] annotation, and the
    kind detail —

    - equality: [expected]/[actual] with the changed spans highlighted (under
      [ansi:false] a [~~~] marker line under each marked side instead of color —
      a deletion marks only the expected side), or a unified line diff
      ({!Diff.hunks}) when a rendered value spans several lines. Marks come from
      {!Diff.refine}. When it declines, the two values share too little for a
      partial mark to point at anything: under [ansi] each side is colored whole
      (green and red are side colors, so this is the same signal extended, not a
      different one), and under [ansi:false] the two labelled values print alone
      — a full-width marker line would be exactly the noise the decline exists
      to avoid. A difference the diff cannot show is stated in words: renderings
      that are byte-equal (a printer lossier than the equality), or that differ
      only by a trailing newline;
    - negated equality: the value printed once ([both sides equal: <v>]);
    - an equality whose {!Failure.kind} says it is not [diffable] — the
      predicate verbs ([satisfies], [require_match]): the claim description and
      the rendered value under the same [expected]/[actual] labels, but never
      diffed or refined against each other, a description not being a rendering;
    - containment ([contains], [not_contains], the affix verbs, [in_order]): the
      needle with its verdict ([needle "secret" — found at byte 10] /
      [needle "NOPE" — not found]), then the stored haystack excerpt —
      occurrence highlighted, or marked with a [~~~] line without color — and,
      when the excerpt is partial, one faint line stating the excerpted byte
      range and the haystack's total size
      ([(excerpt: bytes 0-8191 of a 20006-byte haystack)]). The claim
      description never prints: the verdict says more than the sentence would.
      The payload's {!Failure.containment_demand} widens that verdict rather
      than adding lines of its own — a {!Failure.Ordered} chain break reads
      [not found at or after byte 36], or names the out-of-order occurrence
      ([found at byte 13, before the search resumed at byte 36]) and marks it.
      Only [Ordered] adds a line, the [element] index, which says which
      assertion the rest of the block is about;
    - raise: expected and raised exceptions, and the recorded backtrace. When
      the payload carries a {!Failure.message_diff} — the failure site decided
      the two exceptions differ only in their message — the block diffs the
      {e messages} instead of repeating the constructor
      ([raised Invalid_argument with the wrong message:] followed by the two
      quoted messages with changed spans highlighted, as for equality). A
      payload with no expected side splits on its [predicate] flag: a
      [raises_match] rejection renders
      [raised exception does not satisfy the predicate:], and an exception
      nobody expected — a test body's escape — renders [uncaught exception:],
      each followed by the rendered exception and the recorded backtrace;
    - snapshot: the state — missing (with the proposed content), mismatch
      (unified diff against the baseline), unresolvable, duplicate (pointing at
      the first check: [first checked at <site>] when its site is known,
      [first checked by "<test>"] otherwise) — followed by the acceptance
      command line for missing and mismatched baselines, spelled from the
      invocation: [accept: <exe> -u, then review with git diff] under [`Exe],
      [accept: WINDTRAP_UPDATE=1 dune runtest, then review with git diff] under
      [`Mirrors];
    - property: the counterexample with its case index and shrink count — a
      shrink search that did not converge appends one line stating it
      ([timed out after 5s while shrinking; counterexample may not be minimal]
      from the payload's [timed_out], else [shrinking stopped after 50 steps; …]
      from its [shrink_exhausted]) — the inner failure under [which failed at:]
      (recursively, without commands; [which failed with:] when the inner
      failure has no location), and — for seeded cases only, never explicit
      examples — the replay line built from the payload's root seed, spelled
      from the invocation: [replay: <exe> --seed <root token> -f '<filter>']
      under [`Exe],
      [replay: WINDTRAP_SEED=<root token> WINDTRAP_FILTER='<filter>' dune
       runtest] under [`Mirrors]. A config-sourced case count riding the payload
      ({!Failure.kind.Property}'s [count]) is restated in the line —
      [--prop-count <n>] under [`Exe], [WINDTRAP_PROP_COUNT=<n>] under
      [`Mirrors] — because replaying a late case needs at least as many cases as
      the failing run generated; a declaration-site count replays without any
      flag;
    - message: the text ([(empty failure message)] when it is empty).

    Every line is indented four spaces and the output ends with a newline. The
    captured-output tail is {e not} rendered here — it is per test, not per
    failure; {!finish} and the transports place it. It is also the one surface
    that stays byte-verbatim: a log excerpt is read as a log, and it names the
    full log's path for the rest.

    Every surface above that prints compared data — the two equality renderings
    on both paths, the negated-equality value, the containment excerpt, the
    predicate claim and value, the rendered exceptions, the snapshot baseline
    and proposed content, the counterexample — prints each C0 byte and DEL as a
    lowercase [\xNN] escape ([\x1b], [\x00], [\x0d]), with LF and TAB the
    exceptions: line structure and indentation are the block's own layout. One
    rule, no mnemonics, so [\x] marks every escape a reader sees. Payload text
    arriving inside [%S] quotes — the containment needle, the two exception
    messages — carries OCaml's escapes instead and is left alone.

    The surfaces that are the author's own words rather than a compared value —
    the [?msg] annotation, a {!Failure.Message} text, a recorded backtrace —
    keep the [ansi] policy above, as test names do.

    This holds under [ansi:true] as much as under [ansi:false]: a terminal is
    exactly where a payload-borne [ESC] would stop being data and start being a
    command, coloring the report and eating the label beside it. The renderer's
    own styling is applied after the escape, so it is the only live sequence in
    the block.

    The escape is a projection, like color. Equality, containment, and snapshot
    storage never see it — raw bytes in, raw bytes compared, raw bytes accepted
    into a baseline — and neither do the decisions this block makes about the
    data: whether two renderings are equal, whether their line lists differ,
    which regions {!Diff.refine} marked. Only the printed glyphs and their
    column arithmetic move into escaped space, together, so a [~~~] marker
    covers all four columns of an escape it opened.

    It is not injective: a value holding the four characters [\x1b] renders like
    one holding the byte. Escaping the backslash would fix that and double every
    escape in the [%S] renderings that make up most of a transcript, which is
    the worse trade.

    [excerpt], default [false], additionally prints the located source line read
    from disk, best-effort: unreadable files print nothing. Recorded source
    paths are project-root-relative, so a relative path resolves against
    {!Path_ops.project_root} first — under [dune runtest] the process cwd is
    inside [_build], where the recorded path never opens — then, best-effort, as
    given. [filter], the replay line's filter value (the runner passes the
    test's path string), is shell-quoted here; without it the replay line
    carries the seed alone. [invocation], default [`Mirrors], is the hint
    context ({!type:invocation}) the acceptance and replay lines are spelled
    from — the transports pass the run's real invocation so their bodies match
    the terminal block bytes. [ansi] styles via {!Pp}; with [ansi:false] the
    output contains no escape codes — sequences arriving inside payload strings
    are stripped. *)
