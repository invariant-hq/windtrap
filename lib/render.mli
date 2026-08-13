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
    the header carried one) — see {!result} and {!finish}. [`Quiet] and
    [`Verbose] never defer.

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
  columns : int option;
      (** [WINDTRAP_COLUMNS]: terminal width override; [None] leaves
          {!create}'s default. *)
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
    deliberately not {!Run.config} fields, because no level or width can
    change outcomes or exit codes. The driver applies them when it constructs
    the run's renderer ({!Driver.renderer}). *)

val default_settings : settings
(** [default_settings] is the settings with every knob at its built-in default:
    [color = Env.Auto], no width or tail override, [slow_threshold = 1.]. *)

val create :
  out:Format.formatter ->
  ansi:bool ->
  ?mode:[ `Quiet | `Compact | `Verbose ] ->
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
      transcript contains no escape codes at all: sequences arriving inside
      payload strings, test names, or captured output (a user [pp] or program
      that styles) are stripped; under [ansi:true] they pass through.
    - [mode], the verbosity level — one axis, each level a superset of the one
      below. [`Quiet] ([--quiet]) prints the failure blocks and the summary,
      nothing else. [`Compact] (the default) adds the header and one glyph per
      test — deferred until the run proves noteworthy (the module preamble; a
      green, healthy run is one named line). [`Verbose] ([--verbose]) prints one
      status line per test instead of the glyph. Every level prints the same
      failure blocks and the same summary line.
    - [live], whether {!begin_test} maintains a self-erasing progress display
      with terminal cursor controls. Pass the sink's TTY status; under
      [ansi:false] or [`Quiet] it is off regardless. Defaults to [false].
    - [columns], the terminal width used to bound rules and the live display.
      Defaults to [80]. The compact row wraps at 60 glyphs regardless, so rows
      are byte-stable across terminals.
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
    or leave live-tail residue; ESC follows the [ansi] policy above.

    Raises [Invalid_argument] if [columns < 20], [tail_lines < 0], or
    [slow_threshold] is negative or not finite. *)

(** {1:transcript The transcript} *)

val header :
  t ->
  suite:string ->
  tests:int ->
  ?declared:int ->
  ?selection:string ->
  seed:Windtrap_gen.Seed.seed option ->
  unit ->
  unit
(** [header t ~suite ~tests ~seed] records and, under [`Verbose], prints the run
    header ([mylib: 48 tests (seed s1:…)]). Under [`Compact] the line is
    deferred: the first noteworthy event prints it (see {!result}), and a green,
    healthy run never shows it — its named summary line carries [suite] and
    appends the seed instead. Under [`Quiet] nothing prints, as before; the
    summary line carries [suite].

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
    Prints nothing unless [live] and [ansi] are set and the mode is not
    [`Quiet]. *)

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
    - [`Quiet]: nothing — failures re-print in full at {!finish}.

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
    row. Prints nothing under [`Quiet]: notices are stream trimmings, not
    failure blocks or the summary. While a compact transcript is still deferred
    the notice buffers with the rows — it prints in position if a noteworthy
    event flushes, and a green, healthy run keeps its one-line transcript — with
    an erasable live copy (under [live]) so a hanging fixture release still
    names itself on a terminal. The runner announces fixture releases with it
    ([releasing db]). *)

val finish :
  t ->
  ?coverage:Run.summary ->
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
      re-printed in full ([FAIL] header, then {!pp_failure} with source excerpts
      for each of its failures, then — for a result whose test drew from
      [srandom] ({!Run.result.srandom_root}) and recorded no property failure —
      the replay line spelled from the invocation with the test's path as the
      filter, then its bounded captured-output tail and full-log path, printed
      once per test) — when any test failed;
    - the slow warnings (unless [`Quiet]): a faint-yellow block over every
      completed test past the slow threshold whose record is not [slow_tagged] —
      a [slow tests (n):] heading, then one indented entry per test with the
      duration in a right-aligned leading column ([  2.50s  parser › tokenize]),
      slowest first — and one faint hint line naming the opt-outs: the ["slow"]
      tag, and whichever threshold knob the [invocation] offers
      ([--slow-threshold SECONDS] under [`Exe], [WINDTRAP_SLOW_THRESHOLD] under
      [`Mirrors]). The duration shown and compared is {!Run.result.duration}
      (attempts summed, as {!result}); a slow test that also failed keeps its
      failure block and earns its one warning — the two report different things.
      None print when the threshold is [0.];
    - the summary line ([46 passed, 2 failed in 1.2s.]) from [results] and
      [duration], the run's wall-clock seconds. Expected failures add their own
      segment ([44 passed, 2 expected failures in 1.2s.]), and counted failures
      with subtest-labeled entries (see {!is_subtest_failure}) state the
      sub-case count ([2 failed (3 subtest failures)]). In quiet mode the line
      is prefixed with the suite name recorded by {!header}
      ([unit: 448 passed in 0.4s.]) — quiet prints no header, and nothing may
      print without a name;
    - the slowest tests, on runs slow enough to care about — [`Verbose] only:
      the list is diagnosis, not signal;
    - the coverage line
      ([coverage: 87.2% (312/358 points) · WINDTRAP_COVERAGE=report for detail],
      the percentage styled by the runtime's thresholds — green at 80% and
      above, yellow at 60%, red below) when [coverage] is given (unless
      [`Quiet]). When the summary's [siblings] field is set — other executables'
      [.coverage] files sat beside this process's dump destination at snapshot
      time, several instrumented test stanzas — the line scopes itself and
      points at the aggregate instead:
      [coverage: 52.4% (11/21 points, this executable) · project: dune build
       @cover]. The fact arrives on the record ({!Run.summary}, read by the
      driver when it snapshots coverage); this renderer touches no filesystem
      for it. The caller omits [coverage] under the [report]/[full]/[off]
      coverage modes: {!coverage_report} prints its own line, without the hint.

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

(** {1:snapshots The snapshot/prune report} *)

val report_snapshots :
  t ->
  orphans:string list ->
  pruned:(string list, Snapshot.prune_refusal) result option ->
  Run.t ->
  unit
(** [report_snapshots t ~orphans ~pruned run] prints the run's baseline
    maintenance lines on [t]'s sink: one [wrote <path> (new|updated)] line per
    accepted baseline ({!Snapshot.writes} over [run]'s registry, paths spelled
    by {!Path_ops.display} — the one producer for both runners), then either the
    [pruned <path>] lines of a granted [--prune], or the
    [stale baseline: <path>] lines with the prune refusal's explanation, or the
    stale-baseline lines with the removal hint spelled from [t]'s invocation
    ({!stale_lines_with_hint} — the line class the [--strict-snapshots] failure
    block shares). [orphans] and [pruned] are the outcome's baseline-maintenance
    facts ([Runner.outcome]'s fields of the same names).

    The stale-baseline lines are dropped when [run] carries the
    {!Run.Stale_baselines} verdict row, which took the same lines into the
    failure section — under [--strict-snapshots] they are the failure, and
    naming the files twice in one transcript is noise. A prune refusal's
    explanation still prints: it says why the deletion did not happen, which the
    failure does not.

    Prints nothing under [`Quiet] — quiet keeps only the failure blocks and the
    summary. The driver calls it after {!finish}, when the transcript is
    settled: lines go straight to the sink, outside the compact row and deferral
    machinery. *)

(** {1:sections Report sections}

    The subsystem-neutral vocabulary instrumentation reports are made of:
    styled lines, hint lines, aligned rows, source excerpts, and the failure
    section's rules. Coverage's per-file table and mutation's survivor blocks
    are two projections into it — the subsystem that owns the numbers builds
    section data ({!Driver.coverage_data}, the mutation loop), and this module
    draws it knowing nothing about the runtimes that measured it. Every name a
    runtime owns (a mutant identifier, the arming variable) arrives in the data
    pre-spelled with the runtime's own functions, so the report and the runtime
    cannot disagree about what to type.

    Styling is data here ({!type:span}): the renderer applies it under the
    [ansi] decision made at {!create}, so section data never carries escape
    codes and never has to know what sink it will meet. *)

type span = {
  style : Pp.style option;
      (** The style [text] is wrapped in whole, or [None] for plain text.
          Applied by the renderer iff it emits styling; an empty [text] is
          never wrapped. *)
  text : string;  (** The run of text. *)
}
(** The type for one styled run of a section line. *)

val plain : string -> span
(** [plain text] is [text] with no style. *)

val styled : Pp.style -> string -> span
(** [styled style text] is [text] wrapped whole in [style]. *)

type column = {
  gap : string;  (** Printed before this column, every row. [""] abuts. *)
  align : [ `Left | `Right ];
      (** Which side of the column the cell's padding lands on. *)
  width : int option;
      (** The least column width. [None] sizes the column to its widest cell;
          a caller aligning several [Rows] sections against each other passes
          the width it computed across all of them, as {!excerpt}'s
          [number_width] does. *)
}
(** The type for one column of a {!section.Rows} section. *)

type excerpt = {
  file : string;
      (** The source file the lines come from. Printed on the heading line and
          nowhere else, so it is unused — and may be anything — when [heading]
          is [None]. *)
  heading : span list option;
      (** What follows ["<file> — "] on the heading line — coverage's
          percentage and point counts. [None] prints no heading and no blank
          lines around it, for an excerpt that sits inside a block whose head
          row already named the file. *)
  source : string;  (** The file's text, as read. *)
  marked_lines : int list;
      (** The 1-based lines the excerpt is about: what the regions are built
          around, and what the marker column points at. Lines outside [source]
          are ignored. *)
}
(** The type for one source-excerpt block: which lines of which file to show,
    and what to call them. Subsystem-neutral — the data is the caller's, the
    layout is this module's. *)

type section =
  | Line of span list
      (** One line, the spans concatenated; [Line []] is a blank line. *)
  | Hint of string
      (** One command-hint line, printed verbatim: a line the reader copies
          whole, so it carries no style by construction — no color in any
          hint. *)
  | Rows of { margin : string; columns : column list; rows : span list list }
      (** Aligned rows: each row is one cell per column, cells padded to the
          column's width on the [align] side (outside the cell's styling) and
          the rendered row stripped of trailing spaces. Cells beyond [columns]
          are dropped; missing trailing cells are allowed. *)
  | Excerpt of {
      context : int;
      marker : bool;
      margin : string;
      number_width : int option;
      excerpt : excerpt;
    }  (** A source-excerpt block, drawn as {!val:excerpt} draws it. *)
  | Rule of string option
      (** The failure section's 54-column faint rule: [Some label] centers the
          label in it ([survivors (2)]), [None] is the closing rule. *)
(** The type for report sections. The vocabulary is priced like
    {!Failure.kind}: additions are design amendments, not conveniences. *)

val sections : t -> section list -> unit
(** [sections t l] prints [l] in order on [t]'s sink. Sections neither erase
    the live display nor close a compact glyph row: the report entry points
    below do that once, and callers print section data after {!finish}, when
    the transcript is settled. *)

(** {1:excerpts Source excerpts}

    The one gutter renderer, shared by every subsystem that shows source: the
    right-aligned line number, the [│] rule, the source text, the region marker,
    and the [·····] between regions live here and nowhere else. Two subsystems
    may not own two copies of one renderer — coverage's file blocks and
    mutation's survivor blocks are two projections of {!type:excerpt}, not two
    layouts. The region and range computations below moved here from the
    coverage runtime with the vocabulary: layout lives with the renderer, not
    with the instrumentation that measured the lines. *)

val collapse_ranges : int list -> (int * int) list
(** [collapse_ranges lines] collapses a sorted list of line numbers (duplicates
    allowed) into inclusive contiguous ranges: [[1; 2; 3; 7; 8]] is
    [[(1, 3); (7, 8)]]. *)

val format_ranges : (int * int) list -> string
(** [format_ranges ranges] is the ranges rendered as ["1-3, 7-8"]; a single-line
    range appears without a dash, as in ["88-94, 121"] — the one dialect for
    the coverage table's uncovered lists and the mutation report's unreached
    list. *)

type excerpt_line = {
  number : int;  (** 1-based source line number. *)
  text : string;  (** The line's text, without its newline. *)
  marked : bool;  (** Whether the line is in the marked set. *)
}
(** The type for one line of source-excerpt data. *)

val excerpts :
  ?context:int -> source:string -> int list -> excerpt_line list list
(** [excerpts ~source lines] is the excerpt regions for the marked [lines] of
    [source]: each region is a contiguous run of lines covering one or more
    marked ranges plus [context] lines around each (default [1]). Regions whose
    context windows touch or overlap are one region. Line numbers outside
    [source] are ignored; the result is [[]] when no valid marked line remains
    (in particular when [source] is empty). {!val:excerpt} draws the gutter,
    markers, and separators between regions. *)

val excerpt :
  t ->
  ?context:int ->
  ?marker:bool ->
  ?margin:string ->
  ?number_width:int ->
  excerpt ->
  unit
(** [excerpt t e] prints [e]'s heading, when it has one, then one region per run
    of [e.marked_lines] ({!excerpts}), each line as
    [<margin><marker><number> │ <text>] with trailing spaces stripped, and
    [·····] between regions. With:

    - [context], the lines shown around each marked line. Defaults to [1]; [0]
      shows the marked lines alone.
    - [marker], whether marked lines carry the red [▌] gutter. Defaults to
      [true]. Pass [false] for an excerpt that {e is} its marked lines, where a
      marker on every row would mark nothing; the column then disappears rather
      than printing blank.
    - [margin], the left margin every row carries. Defaults to ["  "], which
      with the marker column is coverage's three-column gutter; a block that
      indents (a survivor's excerpt sits under a four-space indent) passes its
      own.
    - [number_width], the width the line numbers are right-aligned in. Defaults
      to the widest number in this excerpt, floored at [4]. A caller aligning
      several excerpts against each other passes the width it computed across
      all of them.

    A marked line outside [source] contributes no region; an excerpt left with
    no region prints its heading, if it has one, and nothing else. *)

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
      ([uncovered: 88-94, 121]). A fully covered file has no range list; a
      stale file states the staleness and the fix instead of ranges it cannot
      attribute; a file whose unvisited points have no line attribution notes
      the missing source;
    - under [`Full], source excerpts for each file with uncovered lines and a
      readable source: a heading ([lib/eval.ml — 75.0% (111/148)]), then each
      uncovered region with one line of context, uncovered lines carrying a
      gutter marker, regions separated by [·····].

    Prints nothing under [`Quiet] — quiet keeps only the failure blocks and the
    summary, and the coverage report is neither. The caller prints it after
    {!finish}, having withheld [finish]'s [coverage] argument. *)

(** {1:mutation Mutation}

    The mutation report: the survivor blocks, the unreached list, and the one
    summary line. One layout serving the mutation loop's in-process report and,
    through the facade's [Private], the [windtrap mutate] command over merged
    verdict files — the interactive report and the CI report cannot drift apart.

    A survivor is a failure block, not a new vocabulary: the same labelled rule,
    the same [  VERB  subject] head row, the same excerpt row, and red, because
    it is a defect report about a named test. Presentation only — the ordering,
    the cap, the witness lists and every count are the loop's. *)

val mutation_discovery : t -> mutants:int -> files:int -> unit
(** [mutation_discovery t ~mutants ~files] prints the discovery line
    ([mutants: 187 in 4 files · WINDTRAP_MUTATE=1 to test them]): what an
    instrumented build that was not asked to mutate anything found, and the one
    spelling that asks it to test them. The caller prints it after {!finish},
    where the coverage line sits — it is the same discoverability shape, and it
    follows the same rule of printing nothing under [`Quiet]. Prints nothing
    when [mutants] is [0]: a build with no mutant has nothing to offer. *)

val mutation_armed : t -> id:string -> before:string -> after:string -> unit
(** [mutation_armed t ~id ~before ~after] prints the armed announcement
    ([mutant lib/calc.ml:9:12:add armed: a - b → a + b]). Law 16(b) makes it
    normative: a process with a mutant armed says so before any other output, so
    a run whose output does not say so has none. Prints in every mode, [`Quiet]
    included — it is the guarantee, not a stream trimming. *)

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
}
(** The type for survivor witnesses: a test that evaluated the mutated line and
    did not fail when it changed. *)

type survivor = {
  id : string;
      (** The mutant's identifier in the runtime's canonical spelling
          ([lib/calc.ml:9:12:add]) — spelled by the loop, which holds the
          runtime, so this module spends none of the Law-12 coupling budget
          re-spelling it. *)
  file : string;  (** The mutated source file, for the excerpt row. *)
  line : int;  (** 1-based line of the mutated expression. *)
  before : string;  (** The original expression's source text. *)
  after : string;  (** The armed expression's source text. *)
  source : string option;
      (** The mutated file's text, when the loop could read it; the excerpt row
          is dropped when it could not, as every excerpt is best-effort. *)
  witnesses : witness list;
      (** The tests that ran the line and did not fail. Never empty for a
          verdict — a mutant no test evaluated is {e unreached}, a different
          finding with a different remedy — so the block always names someone to
          go and strengthen. Complete, never truncated: the sentence above the
          list counts this list, so a caller that dropped witnesses would print
          a count no reader could reconcile with what follows it. *)
}
(** The type for one survived mutant, as the report shows it. *)

type unreached = {
  file : string;  (** The source file. *)
  lines : int list;  (** Its unreached mutants' 1-based lines, sorted. *)
}
(** The type for one line of the unreached list: the mutants of one file that no
    test evaluates. *)

type mutation = {
  arm_variable : string;
      (** The runtime's arming variable ([WINDTRAP_MUTATE_ARM]), spelled by the
          loop with the runtime's own function — the [arm] hints complete it
          with each survivor's [id] and the invocation, so the report and the
          runtime cannot disagree about what to type. *)
  survivors : survivor list;
      (** The survivor blocks to print, in the order they print — ordered by
          witness count descending and already capped by the loop. *)
  survivors_total : int;
      (** How many mutants survived. Greater than [List.length survivors] when
          the cap dropped blocks, and the label says so. *)
  unreached : unreached list;  (** The unreached list, ordered by file. *)
  unreached_total : int;
      (** How many mutants are unreached. Not the number of lines: one line can
          carry several. *)
  killed : int;  (** How many mutants were killed. *)
  total : int;
      (** The population: every mutant the run could test — the catalogue
          {e minus} the mutants dismissed by [[@mutate off]], which the reader
          took out of scope and which no remedy applies to. It is therefore
          [killed + survivors_total + unreached_total], which is what the
          summary line reads as. *)
  duration : float option;
      (** The mutation run's wall-clock seconds, [None] for a merge, which ran
          nothing. *)
  seed : Windtrap_gen.Seed.seed option;
      (** The run's root seed, when it had one. *)
  siblings : bool;
      (** [true] when other executables' verdict files sat beside this one's:
          the numbers are then one executable's view of the code it links, and
          the summary line scopes itself and points at the merge instead of
          posing as the total. *)
}
(** The type for a whole mutation report. Every field is measured, not derived
    here: this module orders nothing and counts nothing. *)

val mutation_report : t -> mutation -> unit
(** [mutation_report t m] prints [m]:

    - the survivor section, when [m.survivors] is not empty — the labelled rule
      ([survivors (2)], or [survivors (10 of 37)] when the cap dropped blocks),
      then one block per survivor separated by a blank line, then the closing
      rule. A block is the head row
      ([  SURVIVED  lib/calc.ml:9:12:add    a - b  →  a + b], the identifier
      column aligned across the report), the excerpt row for the mutated line,
      the sentence that is the product
      ([3 tests ran this line and none failed when it changed:], singular
      [1 test ran this line and did not fail when it changed:]) with one
      indented line per witness — name and declaration site, in columns aligned
      across the report — and the [arm] and [dismiss] lines. [arm] is the
      command that arms this one mutant, spelled from the [invocation], from
      [m.arm_variable] and the survivor's [id] — under [`Mirrors] it carries
      [--instrument-with ppx_windtrap.mutate], because a build without the
      backend has no mutant to arm; [dismiss] is the attribute to paste,
      [((a - b) [@mutate off "reason"])];
    - the unreached list, when [m.unreached] is not empty — a heading carrying
      the mutant count ([unreached (4) — no test evaluates these]) and one
      compact line per file with its lines as ranges, in the shape coverage's
      per-file report uses;
    - the summary line
      ([mutants: 2 survived of 187 · 181 killed, 4 unreached in 1m44s (seed
        s1:…)]). Terms that are zero are omitted, the way a passing suite prints
      no failure count, so a report with nothing to say is {e one} line. Under
      [m.siblings] the total is scoped and the merge named
      ([mutants: 2 survived of 41 (this executable) · … · project: dune build
        @mutate]), in coverage's wording rather than a second one.

    Prints in every mode, [`Quiet] included: quiet keeps the failure blocks and
    the summary, and a mutation report is both. *)

(** {1:admission Admission}

    The admission report: per-test rulings and one summary line, for
    [WINDTRAP_MUTATE=admit]. An admit transcript makes only per-test claims
    (Law 17e) — never a survivor list, a score, or any other project-level
    statement — so this layout shares the mutation report's vocabulary (the
    labelled rule, the [  VERB  subject] head row, the excerpt row, the [arm]
    and [dismiss] hints) without sharing its record. Presentation only: the
    ordering, the caps and every count are the loop's. *)

type fault = {
  fault_id : string;
      (** The mutant's identifier in the runtime's canonical spelling
          ([lib/calc.ml:9:12:add]) — spelled by the loop, which holds the
          runtime, so this module spends none of the Law-12 coupling budget
          re-spelling it. *)
  fault_file : string;  (** The mutated source file, for the excerpt row. *)
  fault_line : int;  (** 1-based line of the mutated expression. *)
  fault_before : string;  (** The original expression's source text. *)
  fault_after : string;  (** The armed expression's source text. *)
  fault_source : string option;
      (** The mutated file's text, when the loop could read it; the excerpt row
          is dropped when it could not, as every excerpt is best-effort. *)
}
(** The type for one fault as a ruling shows it: a mutant's identity and
    renderings, without a verdict — the ruling it sits in is the verdict. *)

type admission_cause = [ `Failure | `Fixture | `Crashed ]
(** The type for how an admitted test killed its witness fault: an ordinary
    counted failure, a counted failure in the test's own setup or teardown (a
    kill through a dependency the test declared counts, and the witness says
    so), or a child that died without reporting — a crash under a fault is a
    detected fault. A deadline-killed child ([`Timed_out]) arrives with the
    per-child deadline, which this release does not have. *)

type admitted = {
  admitted_test : string;
      (** The test's full path, as {!Test_tree.path_to_string} spells it. *)
  witness : fault;  (** The fault whose arming made the test fail. *)
  cause : admission_cause;
      (** Stated in the [killed] line when it was not an ordinary failure. *)
}
(** The type for one ADMITTED ruling: the test named the fault it kills. *)

type unjustified = {
  unjustified_test : string;  (** The test's full path. *)
  unjustified_loc : Loc.t option;
      (** Where the test is declared, when it is known. *)
  shown : fault list;
      (** The tried faults to print, already capped by the loop
          ([WINDTRAP_MUTATE_LIMIT]); a skipped fault was never watched and is
          never listed. *)
  tried : int;
      (** How many faults the test watched to a pass outcome. Greater than
          [List.length shown] when the cap dropped lines, and the [… n more]
          line says so. *)
  candidates : int;
      (** The candidate list's length after the [WINDTRAP_MUTATE_TRY] cap.
          [tried] falls short of it when a skip kept a candidate unwatched,
          and the capped sentence then stops calling the tried faults the
          most-run ones — they are not. *)
  reached : int;  (** How many undismissed faults the test reaches in all. *)
  capped : bool;
      (** [true] iff [WINDTRAP_MUTATE_TRY] truncated the test's candidate
          list — the ruling's sentence then says so instead of posing as
          exhaustive. *)
}
(** The type for one UNJUSTIFIED ruling: a defect report about the named test,
    rendered as the failure block it is. *)

type no_sites = {
  no_sites_test : string;  (** The test's full path. *)
  no_sites_loc : Loc.t option;
      (** Where the test is declared, when it is known. *)
}
(** The type for one NO SITES ruling: a stated fact, never a finding. A test
    whose whole reach was dismissed lands here indistinguishably: a dismissal
    removes the guard itself, so the runtime has no reach data that could
    name it as the cause. *)

type admission = {
  admission_arm_variable : string;
      (** The runtime's arming variable, spelled by the loop with the runtime's
          own function, as {!mutation.arm_variable} is — the [arm] remedy lines
          complete it. *)
  admitted : admitted list;  (** ADMITTED rulings, in the order they print. *)
  unjustified : unjustified list;
      (** UNJUSTIFIED rulings, in the order they print. *)
  no_sites : no_sites list;  (** NO SITES rulings, in the order they print. *)
  designated : int;
      (** The admission set's size. It is the sum of the three lists' lengths,
          which is what the summary line reads as. *)
  admission_forks : int;
      (** Armed children forked; the determinism probe is the constant extra
          fork and is not counted. *)
  admission_reached : int;
      (** Distinct undismissed faults the designated tests reach, before any
          cap. [0] elides the [over … reached] term. *)
  capped_rulings : int;
      (** How many UNJUSTIFIED rulings the TRY cap truncated; [0] elides the
          summary term. *)
  tries : int;  (** The [WINDTRAP_MUTATE_TRY] value the capped term names. *)
  admission_duration : float;  (** The admit run's wall-clock seconds. *)
  admission_seed : Windtrap_gen.Seed.seed option;
      (** The run's root seed, printed under the run header's rule: [Some] iff
          the header printed one. *)
  scope : string option;
      (** The [WINDTRAP_MUTATE_ONLY=<value>] binding when the scope is set,
          spelled whole by the loop and echoed in every NO SITES block — a
          scope typo must not read as "not instrumented" — and [None] when
          unset. *)
}
(** The type for a whole admission report. Every field is measured, not derived
    here: this module orders nothing and counts nothing. *)

val admission_report : t -> admission -> unit
(** [admission_report t a] prints [a]:

    - one block per ADMITTED ruling — the head row
      ([  ADMITTED  parser › rejects empty input]) and the witness line
      ([    killed  lib/parser.ml:41:8:le   n < len  →  n <= len], the cause
      spelled when it was not an ordinary failure: [killed (crash)],
      [killed (fixture)]);
    - one block per NO SITES ruling — the head row with the declaration site
      and the two-line statement of fact, plus the [WINDTRAP_MUTATE_ONLY] echo
      when [a.scope] is set;
    - the unjustified section, when [a.unjustified] is not empty — the
      labelled rule ([unjustified (1)]), then one block per ruling: the head
      row with the declaration site, the sentence (capped
      [killed none of the 25 most-run faults on its lines, of 412 reached]
      with the [WINDTRAP_MUTATE_TRY=0] hint, exhaustive
      [killed none of the 12 faults it reaches:], or — when a skip kept a
      fault unwatched — [killed none of the 1 fault tried on its lines, of 2
      reached:], with the hint kept when the cap also bit), the tried faults
      with their excerpt rows and the [… n more]
      line when the cap dropped some, and the [arm] and [dismiss] remedy
      lines, the arm command narrowed to the ruling's own test;
    - the summary line
      ([admission: 1 admitted of 1 · 2 forks over 12 reached in 0.9s
        (seed s1:…)]) — zero terms elided, except that [0 admitted] prints
      beside an unjustified ruling, where it is the answer rather than noise;
      the [… ruling(s) capped at TRY] term only when [a.capped_rulings] is
      positive.

    Prints in every mode, [`Quiet] included, as {!mutation_report} does: an
    UNJUSTIFIED ruling is a failure block, and the summary is a summary. *)

(** {1:durations Durations} *)

val pp_run_duration : float -> string
(** [pp_run_duration secs] is the summary line's rendering of a run's wall-clock
    seconds: three significant digits, never scientific notation ([0.463],
    [1.46], [5400]; [0] below 0.1ms). Exposed so out-of-tree harnesses (the test
    tree's hand-rolled meta harness) print the same duration bytes as the
    summary line — one formatter tree-wide. *)

(** {1:projections Failure projections}

    The kind-by-kind projection of one {!Failure.t}, shared by the terminal
    failure blocks and the {!Render_junit} and {!Render_github} transports.
    Everything derives from the typed payload: no formatting happens at failure
    sites. *)

val headline : ?invocation:invocation -> Failure.t -> string
(** [headline f] is a one-line, unstyled summary of [f]
    ([expected true, got false], [snapshot "help": no baseline], …), for
    transports that need a single-line field (JUnit [message] attributes).
    Newlines and escape codes cannot occur — payload-borne ANSI sequences are
    stripped; long payload renderings are truncated with an ellipsis.
    [invocation], default [`Mirrors], spells the one hint that rides into a
    summary: the {!Failure.Stale_baselines} removal hint, whose lines flatten
    whole ({!stale_lines_with_hint}). *)

val stale_lines : string list -> string list
(** [stale_lines orphans] is one [stale baseline: <path>] line per orphan, in
    order, paths spelled by {!Path_ops.display}. The one producer of the
    stale-baseline line class: the [--strict-snapshots] failure block renders
    the {!Failure.Stale_baselines} payload with it, and the advisory snapshot
    report ({!report_snapshots}) prints the same lines, so the two surfaces
    cannot drift. *)

val stale_lines_with_hint :
  invocation:invocation -> string list -> string list
(** [stale_lines_with_hint ~invocation orphans] is {!stale_lines} followed by
    the removal hint, spelled from [invocation] like every other command hint:
    [remove stale baselines: <exe> -u --prune] under [`Exe],
    [remove stale baselines: WINDTRAP_UPDATE=1 WINDTRAP_PRUNE=1 dune runtest]
    under [`Mirrors]. *)

val is_subtest_failure : path:string list -> Failure.t -> bool
(** [is_subtest_failure ~path f] is [true] iff [f]'s [msg] carries a subtest
    label for the test at [path]: it starts with the test's own (leaf) name
    followed by the [" › "] separator — the labeling contract of [subtest]
    ({!Run.subtest}). The terminal summary counts such entries as sub-cases and
    {!Render_junit} projects them as separate testcases. The label rides the
    [msg] slot by design, so a user [?msg] beginning with that exact prefix is
    indistinguishable from a subtest label. *)

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
      different one), and under [ansi:false] the two labelled values print
      alone — a full-width marker line would be exactly the noise the decline
      exists to avoid. A difference the diff cannot show is stated in words:
      renderings that are byte-equal (a printer lossier than the equality), or
      that differ only by a trailing newline;
    - negated equality: the value printed once ([both sides equal: <v>]);
    - predicate ([satisfies], [require_match]): the claim description and the
      rendered value under the [expected]/[actual] labels, never diffed or
      refined against each other — a description is not a rendering;
    - containment ([contains], [not_contains]): the needle with its verdict
      ([needle "secret" — found at byte 10] / [needle "NOPE" — not found]), then
      the stored haystack excerpt — occurrence highlighted, or marked with a
      [~~~] line without color — and, when the excerpt is partial, one faint
      line stating the excerpted byte range and the haystack's total size
      ([(excerpt: bytes 0-8191 of a 20006-byte haystack)]). The claim
      description never prints: the verdict says more than the sentence would;
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
      shrink search that hit the per-test budget appends one line stating it
      ([timed out after 5s while shrinking; counterexample may not be minimal],
      from the payload's [timed_out]) — the inner failure under
      [which failed at:] (recursively, without commands; [which failed with:]
      when the inner failure has no location), and — for seeded cases only,
      never explicit examples — the replay line built from the payload's root
      seed, spelled from the invocation:
      [replay: <exe> --seed <root token> -f '<filter>'] under [`Exe],
      [replay: WINDTRAP_SEED=<root token> WINDTRAP_FILTER='<filter>' dune
       runtest] under [`Mirrors]. A config-sourced case count riding the payload
      ({!Failure.kind.Property}'s [count]) is restated in the line —
      [--prop-count <n>] under [`Exe], [WINDTRAP_PROP_COUNT=<n>] under
      [`Mirrors] — because replaying a late case needs at least as many cases as
      the failing run generated; a declaration-site count replays without any
      flag;
    - message: the text ([(empty failure message)] when it is empty);
    - stale baselines: one [stale baseline: <path>] line per payload path and
      the removal hint, exactly {!stale_lines_with_hint} spelled from the
      invocation — the payload carries paths, never a pre-baked command.

    Every line is indented four spaces and the output ends with a newline. The
    captured-output tail is {e not} rendered here — it is per test, not per
    failure; {!finish} and the transports place it.

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
