(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The terminal renderer: the run transcript on standard output.

    [Render] projects run data into the terminal transcript (RFC "The runner",
    {e Output}): a header, one progress mark per test — by default a compact
    glyph row (green [.] pass, red [F] fail, yellow [S] skip, faint [x] expected
    failure — amendment B12), one status line per test with its timing under
    [`Verbose] — failed tests re-printed in full at the end of the run —
    location, source excerpt, highlighted diff, counterexample and replay line,
    acceptance command, bounded captured-output tail with the full-log path —
    then the slow warnings, the summary line, the slowest tests ([`Verbose]
    only), and the coverage line. The mode decides {e what} prints; the sink
    decides only color and the erasable live tail — no sink changes shape, and
    committed glyphs are flushed one by one so a crashed run leaves its partial
    row visible.

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

    Renderers are projections (RFC Law 4): everything printed derives from
    {!Run.result} values and the typed {!Failure.t} payloads inside them.
    Acceptance and replay command lines are composed here from snapshot names,
    paths, and root seed tokens (RFC Laws 3 and 7 — the replay line prints the
    {e root} token only); diffs are computed here from the rendered values via
    {!Diff}; styling is applied here through {!Pp} under the explicit [ansi]
    decision made at {!create} — nothing reads the environment or the terminal.
    A renderer never alters status, counts, or scheduling.

    The runner drives one {!type:t} through the run: {!header} once, then
    {!begin_test}/{!result} per test, then {!finish}. {!headline} and
    {!pp_failure} are the shared failure projections the {!Render_junit} and
    {!Render_github} transports reuse, with [ansi:false].

    Exact layout — column positions, rule widths, the slowest-5 threshold,
    display bounds on diffs and proposed contents — is illustrative, not
    contract (RFC "The runner", tuned constants). *)

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
    startup; every acceptance, replay, rerun, and prune line derives from it, so
    no hint can name an invocation that would not re-run the suite (RFC Law 3).
*)

val create :
  out:Format.formatter ->
  ansi:bool ->
  ?quiet:bool ->
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
      below. [`Quiet] ([--quiet]) prints the failure blocks, the summary, and
      the rerun hint, nothing else. [`Compact] (the default) adds the header and
      one glyph per test — deferred until the run proves noteworthy (the module
      preamble; a green, healthy run is one named line). [`Verbose]
      ([--verbose]) prints one status line per test instead of the glyph. Every
      level prints the same failure blocks and the same summary line.
    - [quiet], legacy sugar: [quiet:true] is [mode:`Quiet]. Ignored when [mode]
      is given. Defaults to [false].
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

val header : t -> suite:string -> tests:int -> seed:Seed.seed option -> unit
(** [header t ~suite ~tests ~seed] records and, under [`Verbose], prints the run
    header ([mylib: 48 tests (seed s1:…)]). Under [`Compact] the line is
    deferred: the first noteworthy event prints it (see {!result}), and a green,
    healthy run never shows it — its named summary line carries [suite] and
    appends the seed instead. Under [`Quiet] nothing prints, as before; the
    summary line carries [suite].

    [seed] is shown when given; the runner passes the root seed iff the suite
    declares property tests — selection never changes it, so the token is stable
    across filtered runs — and [tests] is the number of selected tests, which
    also scales the live line's [[k/n]] counter. *)

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

val result :
  t -> ?excused:Test_tree.xfail -> ?slow_tagged:bool -> Run.result -> unit
(** [result t r] prints [r]'s per-test progress, by mode:

    - [`Compact]: one glyph — green [.] pass, red [F] counted failure (assert,
      property, snapshot, timeout, unexpected pass), yellow [S] skip, faint [x]
      expected failure. Glyphs buffer until the run proves noteworthy: the first
      counted-failure result, or the first completed test with
      [r.duration >= slow_threshold] that is not [slow_tagged] (never when the
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

    The output derives from [r], [excused], and [slow_tagged] alone; [t] only
    counts marks for the wrap counter and the live display.

    [slow_tagged], default [false], marks [r]'s test as carrying the ["slow"]
    tag (its own or an ancestor's — the caller decides from the flattened case's
    tags): such tests are exempt from the slow threshold everywhere. Skips never
    trigger the threshold; their durations are not run time. The duration
    compared is {!Run.result.duration} — the attempts summed — so a retried test
    whose attempts together cross the threshold counts as slow even when its
    final attempt was fast.

    [excused] marks [r] an {e expected} failure (amendment B12): an [xfail] test
    whose failing outcome did not count — the caller decides from the runner's
    [failed_paths] projection and the flattened case's {!Test_tree.case.xfail}.
    The verbose line then renders as a dim, informational [XFAIL] tag with the
    annotation's reason ([XFAIL  name (expected failure: issue #42)]) instead of
    a loud [FAIL], and the compact glyph as the faint [x]. An [xfail] test that
    {e passed} needs no marking: the runner records it as an ordinary counted
    failure whose message names the reason, so its [FAIL] line and block are
    already loud. [excused] is ignored unless [r]'s outcome is a failure. *)

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
  ?excused:(string * Test_tree.xfail) list ->
  ?slow_tagged:string list ->
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
    - the slow warnings (unless [`Quiet]): one faint-yellow line per completed
      test over the slow threshold not named in [slow_tagged]
      ([slow: parser › tokenize took 2.50s]), in execution order, then one faint
      hint line naming the opt-outs (the ["slow"] tag,
      [WINDTRAP_SLOW_THRESHOLD]). The duration shown and compared is
      {!Run.result.duration} (attempts summed, as {!result}); a slow test that
      also failed keeps its failure block and earns its one warning — the two
      report different things. None print when the threshold is [0.];
    - the summary line ([46 passed, 2 failed in 1.2s.]) from [results] and
      [duration], the run's wall-clock seconds. Expected failures add their own
      segment ([44 passed, 2 expected failures in 1.2s.]), and counted failures
      with subtest-labeled entries (amendment B13, see {!is_subtest_failure})
      state the sub-case count ([2 failed (3 subtest failures)]). In quiet mode
      the line is prefixed with the suite name recorded by {!header}
      ([unit: 448 passed in 0.4s.]) — quiet prints no header, and nothing may
      print without a name;
    - the rerun hint ([rerun failures only: <exe> --failed]) when tests counted
      as failed and the invocation is [`Exe] — under [`Mirrors] no hint prints:
      [--failed] has no environment mirror, and a hint that cannot be followed
      is worse than none;
    - the slowest tests, on runs slow enough to care about — [`Verbose] only:
      the list is diagnosis, not signal;
    - the coverage line
      ([coverage: 87.2% (312/358 points) · WINDTRAP_COVERAGE=report for detail],
      the percentage styled by the runtime's thresholds — green at 80% and
      above, yellow at 60%, red below) when [coverage] is given (unless
      [`Quiet]). When other executables' [.coverage] files sit beside this
      process's dump destination — several instrumented test stanzas — the line
      scopes itself and points at the aggregate instead:
      [coverage: 52.4% (11/21 points, this executable) · project: dune build
       @cover]. Sibling detection is best-effort: on a cold parallel first run
      the hint can be absent (siblings dump at exit, after this renders); it is
      deterministic from the second run on. The caller omits [coverage] under
      the [report]/[full]/[off] coverage modes: {!coverage_report} prints its
      own line, without the hint.

    [excused], default [[]], lists the expected failures (amendment B12) by
    joined path ({!Test_tree.path_to_string}) with their annotations — the
    caller derives it as for {!result}. Excused results leave the failure
    section, the failed count, and the rerun hint: they did not fail the run,
    and a summary that counted them red would contradict the exit code (their
    stream lines already reported them as [XFAIL]).

    [slow_tagged], default [[]], lists the joined paths of the tests carrying
    the ["slow"] tag — the caller derives it from the flattened cases, as
    {!result}'s per-test flag. Named results are exempt from the slow warnings
    and from keeping a deferred compact transcript noteworthy; skips are exempt
    regardless.

    Failure blocks render each test's captured tail from the first
    {!Failure.tail} attached to its failures: the retained lines (at most
    [tail_lines]), what was omitted, and the tail's [log_path]. *)

(** {1:coverage Coverage}

    The coverage detail projection (RFC "Coverage"): one layout serving the
    in-process [WINDTRAP_COVERAGE]/[--coverage] report modes and, through the
    facade's [Private], the [windtrap coverage] command over merged files — the
    inline report and the CI report cannot drift apart. Presentation only: the
    data (percentages, uncovered lines, excerpt regions) is the coverage
    runtime's (Law 12, run data rendered late). *)

val coverage_report :
  t ->
  ?source_roots:string list ->
  mode:[ `Report | `Full ] ->
  Windtrap_coverage.t ->
  unit
(** [coverage_report t ~mode c] prints the coverage block for the collection
    [c]:

    - the summary line, as {!finish}'s without the discoverability hint
      ([coverage: 87.2% (312/358 points)]);
    - one line per file — percentage (styled by the runtime's thresholds, as the
      summary line), visited/total, file name, and the uncovered line ranges
      ([uncovered: 88-94, 121]). A fully covered file has no range list; a stale
      file (source changed since the data was recorded) states the staleness and
      the fix instead of ranges it cannot attribute; a file whose source was not
      found notes that;
    - under [`Full], source excerpts for each file with uncovered lines and a
      readable source: a heading ([lib/eval.ml — 75.0% (111/148)]), then each
      uncovered region with one line of context, uncovered lines carrying a
      gutter marker, regions separated by [·····].

    [source_roots] is the source lookup list for line mapping and excerpts,
    defaulting to the current directory (the coverage runtime's [file_reports]
    default) — the [windtrap coverage] command passes the project root it
    discovered.

    Prints nothing under [`Quiet] — quiet keeps only the failure blocks and the
    summary, and the coverage report is neither. The caller prints it after
    {!finish}, having withheld [finish]'s [coverage] argument. *)

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
    sites (RFC Law 4). *)

val headline : Failure.t -> string
(** [headline f] is a one-line, unstyled summary of [f]
    ([expected true, got false], [snapshot "help": no baseline], …), for
    transports that need a single-line field (JUnit [message] attributes).
    Newlines and escape codes cannot occur — payload-borne ANSI sequences are
    stripped; long payload renderings are truncated with an ellipsis. *)

val is_subtest_failure : path:string list -> Failure.t -> bool
(** [is_subtest_failure ~path f] is [true] iff [f]'s [msg] carries a subtest
    label for the test at [path]: it starts with the test's own (leaf) name
    followed by the [" › "] separator — the labeling contract of [subtest]
    (amendment B13, {!Run.subtest}). The terminal summary counts such entries as
    sub-cases and {!Render_junit} projects them as separate testcases. The label
    rides the [msg] slot by design, so a user [?msg] beginning with that exact
    prefix is indistinguishable from a subtest label. *)

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

    - equality: [expected]/[actual] with the changed spans highlighted
      ({!Diff.refine}; under [ansi:false] a [~~~] marker line instead of color),
      or a unified line diff ({!Diff.hunks}) when a rendered value spans several
      lines. When both renderings parse as sequences of at least an internal
      size threshold (currently 8 elements), a summary line states the
      element-grain difference first
      ([lists differ at 3 of 100 elements; first at [37]: expected …, actual …]
      — {!Diff.sequences}, amendment B7), with a length form
      ([lists differ in length: expected 100 elements, actual 98]) when the
      counts differ. A difference the diff cannot show is stated in words:
      renderings that are byte-equal (a printer lossier than the equality), or
      that differ only by a trailing newline. Claims other than {!Failure.Equal}
      never diff the claim description against the value: {!Failure.Satisfies}
      and {!Failure.Matches} print the description and the rendered value
      without refinement, and {!Failure.Contains} prints the needle with its
      verdict ([needle "secret" — found at byte 10] /
      [needle "NOPE" — not found]), then the stored haystack excerpt —
      occurrence highlighted, or marked with a [~~~] line without color — and,
      when the excerpt is partial, one faint line stating the excerpted byte
      range and the haystack's total size
      ([(excerpt: bytes 0-8191 of a 20006-byte haystack)]);
    - negated equality: the value printed once ([both sides equal: <v>]);
    - raise: expected and raised exceptions, and the recorded backtrace. When
      both exceptions share their constructor and both carry a message payload
      (amendment B1, {!Failure.kind}), the block diffs the {e messages} instead
      of repeating the constructor
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
      command line for missing and mismatched baselines (RFC Law 3), spelled
      from the invocation: [accept: <exe> -u, then review with git diff] under
      [`Exe],
      [accept: WINDTRAP_UPDATE=1 dune runtest, then review with git diff] under
      [`Mirrors];
    - property: the counterexample with its case index and shrink count — a
      shrink search that hit the per-test budget appends one line stating it
      ([timed out after 5s while shrinking; counterexample may not be minimal],
      from the payload's [timed_out]) — the inner failure under
      [which failed at:] (recursively, without commands; [which failed with:]
      when the inner failure has no location), and — for seeded cases only,
      never explicit examples — the replay line built from the payload's root
      seed (RFC Law 7), spelled from the invocation:
      [replay: <exe> --seed <root token> -f '<filter>'] under [`Exe],
      [replay: WINDTRAP_SEED=<root token> WINDTRAP_FILTER='<filter>' dune
       runtest] under [`Mirrors];
    - message: the text ([(empty failure message)] when it is empty).

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
