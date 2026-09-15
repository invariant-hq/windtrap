(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The report's blocks: the failure projection every transport shares, and the
    subsystem-neutral section vocabulary the coverage and mutation reports
    project into.

    Everything here derives from data — a {!Failure.t}, a {!type:coverage} or a
    {!type:mutation} record — and prints through {!print} under one explicit
    [ansi] decision. No function reads the environment or the terminal. Styling
    is data too: section data never carries escape codes, and the excerpts both
    instrumentation reports show — the right-aligned line number, the [│] rule,
    the marker column, the [·····] between regions — are one gutter renderer,
    not two layouts.

    [Report] composes these blocks into the run transcript and re-exports the
    failure projection; [Report_junit] projects through it. *)

(** {1:failures Failure projections}

    The kind-by-kind projection of one {!Failure.t}, shared by the terminal
    failure blocks, the JUnit document and the GitHub annotations. No formatting
    happens at failure sites. *)

val headline : Failure.t -> string
(** [headline f] is a one-line, unstyled summary of [f]
    ([expected true, got false], [expect_file "help.expected": no baseline], …),
    for transports that need a single-line field (JUnit [message] attributes).
    Newlines and escape codes cannot occur — payload-borne ANSI sequences are
    stripped; long payload renderings are truncated with an ellipsis. *)

val is_subtest_failure : Failure.t -> bool
(** [is_subtest_failure f] is [true] iff [f] was recorded inside {!Run.subtest}:
    the failure's [subtest] components are non-empty. The terminal summary
    counts such entries as sub-cases and {!Report_junit} projects them as
    separate testcases. Classification is record-driven — a user [?msg] spelling
    out a [leaf › name] prefix stays an ordinary annotation. *)

val labeled_msg : Failure.t -> string option
(** [labeled_msg f] is [f]'s [msg] slot as reports display it: for a sub-case
    entry, the [leaf › name] label derived from [f]'s [subtest] components, with
    the user's [?msg] joined after [": "] when there is one; for a plain
    failure, the [?msg] annotation itself. The one derivation, shared by the
    failure block, the headline, and {!Report_junit}'s testcase names. *)

val pp_failure :
  ansi:bool ->
  ?excerpt:bool ->
  ?filter:string ->
  ?invocation:Run.invocation ->
  Format.formatter ->
  Failure.t ->
  unit
(** [pp_failure ~ansi ppf f] formats [f]'s full report block: the phase (when
    not {!Failure.Body}) and location header, the [?msg] annotation, and the
    kind detail.

    Under a location the executor filled from the test's declaration
    ({!Failure.Declaration}: the failing call sat in tail position and left no
    frame), one faint line names the remedy, once —
    [(assertion in tail position: its line is unknown; ~__POS__ names it)] —
    except for a property failure, whose location is its declaration by
    construction, for an uncaught exception, which no verb raised, and for a
    file baseline, whose call takes no position; see {!Failure.attribution}.

    - equality: [expected]/[actual] with the changed spans highlighted (under
      [ansi:false] a [~~~] marker line under each marked side instead of color —
      a deletion marks only the expected side), or a unified line diff
      ({!Diff.hunks}) when a rendered value spans several lines. Marks come from
      {!Diff.refine}; when it declines, under [ansi] each side is colored whole
      and under [ansi:false] the two labelled values print alone. A difference
      the diff cannot show is stated in words: renderings that are byte-equal (a
      printer lossier than the equality), or that differ only by a trailing
      newline;
    - negated equality: the value printed once ([both sides equal: <v>]);
    - an equality whose {!Failure.kind} says it is not [diffable] — the
      predicate verbs: the claim description and the rendered value under the
      same [expected]/[actual] labels, never diffed against each other;
    - containment ([contains], [not_contains], the affix verbs, [in_order]): the
      needle with its verdict ([needle "secret" — found at byte 10] /
      [needle "NOPE" — not found]), then the stored haystack excerpt —
      occurrence highlighted, or marked with a [~~~] line without color — and,
      when the excerpt is partial, one faint line stating the excerpted byte
      range and the haystack's total size. A {!Failure.Ordered} demand widens
      the verdict ([not found at or after byte 36], or the out-of-order
      occurrence) and adds the [element] index line;
    - raise: expected and raised exceptions, and the recorded backtrace. With a
      {!Failure.message_diff} the block diffs the {e messages} instead of
      repeating the constructor; with no expected side it renders
      [raised exception does not satisfy the predicate:] for a [raises_match]
      rejection and [uncaught exception:] otherwise;
    - baseline: the subject ([expect], or [expect_file "<path>"]) and the state
      — missing (with the proposed content, bounded), mismatch (unified diff
      against the baseline), unresolvable (with the unproven path) — followed,
      for a missing or mismatched baseline, by the acceptance line spelled from
      the invocation: [accept: <exe> -u, then review with git diff] under
      [`Exe], [accept: dune promote] under [`Mirrors] — except a missing file
      under [`Mirrors], which promotion cannot create, so its line reads
      [accept: touch '<path>' && dune runtest, then dune promote];
    - property: the counterexample with its case index and shrink count — a
      pre-image marked [from] and explained once, a shrink search that did not
      converge stated in one line — the inner failure under [which failed at:]
      (recursively, without commands; [which failed with:] when the inner
      failure has no location), and — for seeded cases only, never explicit
      examples — the replay line spelled from the invocation:
      [replay: <exe> --seed <root token> -f '<filter>'] under [`Exe],
      [replay: WINDTRAP_SEED=<root token> WINDTRAP_FILTER='<filter>' dune
       runtest] under [`Mirrors], a config-sourced case count restated
      ([--prop-count <n>] / [WINDTRAP_PROP_COUNT=<n>]);
    - message: the text ([(empty failure message)] when it is empty).

    Every line is indented four spaces and the output ends with a newline. The
    captured-output tail is {e not} rendered here — it is per test, not per
    failure; the transports place it.

    Every surface that prints compared data prints each C0 byte and DEL as a
    lowercase [\xNN] escape, LF and TAB excepted, under [ansi:true] as much as
    under [ansi:false] — a terminal is exactly where a payload-borne [ESC] would
    stop being data. Payload text arriving inside [%S] quotes carries OCaml's
    escapes instead. The surfaces that are the author's own words — the [?msg]
    annotation, a {!Failure.Message} text, a recorded backtrace — and test names
    follow the [ansi] policy: sequences are stripped under [ansi:false] and pass
    through under [ansi:true]. The escape is a projection: equality, containment
    and baseline storage never see it, and neither do the decisions this block
    makes about the data. It is not injective.

    [excerpt], default [false], additionally prints the located source line read
    from disk, best-effort: unreadable files print nothing. Recorded source
    paths are project-root-relative, so a relative path resolves against
    {!Path_ops.project_root} first, then as given. [filter], the replay line's
    filter value (the test's path string), is shell-quoted here; without it the
    replay line carries the seed alone. [invocation] defaults to [`Mirrors]. *)

val sanitize_name : string -> string
(** [sanitize_name s] is [s] with C0 control bytes and DEL escaped OCaml-style
    ([\n], [\t], [\xNN]) and ESC left alone — the spelling every terminal
    surface prints a user-controlled name with, so a payload-borne newline
    cannot split a header or leave live-tail residue. *)

(** {1:sections The section vocabulary}

    The one vocabulary instrumentation reports are made of: styled lines, hint
    lines, aligned rows, source excerpts, and the labelled rules the failure
    section uses. Priced like {!Failure.kind}: a new constructor is a design
    amendment, not a convenience. A {!Hint} carries no spans by construction —
    no color in any hint. *)

type span = { style : Pp.style option; text : string }
(** The type for a styled run of text. *)

val plain : string -> span
(** [plain text] is [text] unstyled. *)

val styled : Pp.style -> string -> span
(** [styled style text] is [text] under [style]. *)

type column = { gap : string; align : [ `Left | `Right ]; width : int option }
(** The type for a table column: the text before the cell, its alignment, and a
    width floor for tables that align across blocks ([None] fits the widest
    cell). *)

type excerpt = {
  file : string;  (** The file the excerpt is from. *)
  heading : span list option;
      (** A heading line ([<file> — <heading>]) printed before the excerpt,
          blank lines around it; [None] for none. *)
  source : string;  (** The file's text. *)
  marked_lines : int list;  (** The 1-based lines to mark. *)
}
(** The type for a source excerpt: the marked lines of [source] with their
    context, touching windows merged, [·····] between regions. *)

(** The type for report sections. *)
type section =
  | Line of span list  (** One line; [Line []] is a blank line. *)
  | Hint of string  (** One unstyled line: a command to type. *)
  | Rows of { margin : string; columns : column list; rows : span list list }
      (** A table: each row's cells padded to the widest cell of their column,
          trailing spaces stripped. *)
  | Excerpt of {
      context : int;  (** Lines of context around each marked line. *)
      marker : bool;
          (** Whether marked lines carry the red [▌] gutter marker. *)
      margin : string;  (** The text before the gutter. *)
      number_width : int option;
          (** The line-number column width; [None] fits the excerpt, floor four.
          *)
      excerpt : excerpt;
    }
  | Rule of string option
      (** A faint 54-column rule, labelled when given ([── label ──]). *)

val print : out:Format.formatter -> ansi:bool -> section list -> unit
(** [print ~out ~ansi sections] writes [sections] to [out], one section after
    the other, styled under [ansi] — with [ansi:false] the output contains no
    escape codes at all — and flushes [out]. *)

(** {1:coverage Coverage}

    The coverage detail projection, drawn by [windtrap coverage] over merged
    files. The data arrives as the records below, built by whoever holds the
    runtime: this module orders nothing, counts nothing, and does not name the
    runtime. *)

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

val coverage_line :
  ?hint:string -> visited:int -> total:int -> unit -> span list
(** [coverage_line ~visited ~total ()] is the summary line
    ([coverage: 87.2% (312/358 points)]), the percentage styled by the frozen
    thresholds — green at 80% and above, yellow at 60%, red below — with
    [ · <hint>] appended when [hint] is given. *)

val coverage_report : mode:[ `Report | `Full ] -> coverage -> section list
(** [coverage_report ~mode c] is the coverage block for [c]:

    - the summary line ({!coverage_line}, no hint);
    - one line per file — percentage (styled as the summary line's),
      visited/total, file name, and the uncovered line ranges
      ([uncovered: 88-94, 121]), bounded at eight regions and then
      [(+N more, -u shows them)]. A fully covered file has no range list; a
      stale file states the staleness and the fix instead of ranges it cannot
      attribute; a file whose unvisited points have no line attribution notes
      the missing source;
    - under [`Full], source excerpts for each file with uncovered lines and a
      readable source: a heading ([lib/eval.ml — 75.0% (111/148)]), then each
      uncovered region with one line of context, uncovered lines carrying a
      gutter marker. *)

(** {1:mutation Mutation}

    The mutation report: the survivor blocks, the unreached blocks, the one
    summary line and the reproduce footer. One layout serving the mutation
    loop's per-executable report and the [windtrap mutants] command's aggregate
    over merged verdict files, so the two cannot drift apart.

    A survivor is a failure block, not a new vocabulary: the same labelled rule,
    the same [  VERB  subject] head row, the same excerpt row, and red, because
    it is a defect report about a named test. An unreached mutant is the same
    block without the sentence, in yellow. Presentation only — the ordering and
    the witness lists are the producer's; the counts are the lists'. *)

type witness = {
  test : string;
      (** The test's full path, as {!Test_tree.path_to_string} spells it. *)
  loc : Loc.t option;  (** Where the test is declared, when it is known. *)
  exe : string option;
      (** The test executable that ran the test, shown as the row's first
          column. [None] in a per-executable report, where every witness is this
          executable's and the column is omitted; the aggregate names each
          witness's executable. *)
}
(** The type for survivor witnesses: a test that evaluated the mutated line and
    did not fail when it changed. *)

type mutant = {
  id : string;
      (** The mutant's identifier in the runtime's canonical spelling
          ([lib/calc.ml:9:12:add]) — spelled by the producer, which holds the
          runtime. *)
  file : string;  (** The mutated source file, for the excerpt row. *)
  line : int;  (** 1-based line of the mutated expression. *)
  before : string;  (** The original expression's source text. *)
  after : string;  (** The armed expression's source text. *)
  source : string option;
      (** The mutated file's text, when the producer could read it; the excerpt
          row is dropped when it could not. *)
}
(** The type for one mutant as a block draws it. *)

type survivor = {
  mutant : mutant;  (** The mutant that survived. *)
  witnesses : witness list;
      (** The tests that ran the line and did not fail. Never empty for a
          verdict — a mutant no test evaluated is {e unreached} — and never
          truncated: the sentence above the list counts it. *)
}
(** The type for one survived mutant, as the report shows it. *)

(** The type for what a report's reached count is relative to: the summary
    line's subject — [5 reached by this suite],
    [2 reached by the 2 selected tests], [12 reached · 3 executables]. *)
type scope =
  | Suite  (** A per-executable run over its whole suite. *)
  | Selected of int
      (** A per-executable run whose selection narrowed the suite to this many
          tests. *)
  | Executables of int
      (** The aggregate over this many executables' verdict files. *)

type mutation = {
  arm_variable : string;
      (** The runtime's arming variable ([WINDTRAP_MUTATE_ARM]), spelled by the
          producer with the runtime's own function; the reproduce footer
          completes it with the [<id>] placeholder and the invocation. *)
  survivors : survivor list;
      (** Every mutant that survived, one block each, ordered by witness count
          descending, then by identifier. Never capped. *)
  unreached : mutant list;
      (** Every mutant no test evaluated, one block each, ordered by identifier.
          Aggregate only: a per-executable report hands over [[]]. *)
  killed : int;  (** How many mutants were killed. *)
  scope : scope;  (** What the reached count is relative to. *)
  filter : string option;
      (** The run's [-f] filter, when it had one, restated in the reproduce
          footer; [None] for the aggregate, which ran nothing. *)
}
(** The type for a whole mutation report. The reached count is
    [killed + List.length survivors], a count of the lists, not a field. *)

val mutation_report : invocation:Run.invocation -> mutation -> section list
(** [mutation_report ~invocation m] is [m] as sections:

    - the survivor section, when [m.survivors] is not empty — the labelled rule
      ([survivors (2)]) and one block per survivor: the head row
      ([  SURVIVED  lib/calc.ml:9:12:add    a - b  →  a + b], identifiers
      aligned across the section), the excerpt row for the mutated line, and the
      sentence ([3 tests ran this line and none failed:], singular
      [1 test ran this line and did not fail:], and
      [3 tests in 2 executables ran this line and none failed:] when the
      witnesses name more than one executable) over one indented row per witness
      — the executable when any witness of the report names one, the test's
      name, and its declaration site, in columns aligned across the report;
    - the unreached section, when [m.unreached] is not empty — the labelled rule
      ([never reached (2)]) and one block per mutant, no sentence;
    - the closing rule, when either section printed;
    - the summary line, terms separated by [·] and the zero terms omitted:
      [mutants: 1 survived of 5 reached by this suite · 4 killed],
      [mutants: 5 reached by this suite · 5 killed],
      [mutants: 1 survived of 12 reached · 11 killed · 2 never reached · 3
       executables]. [N survived] is red, [N killed] green, [N never reached]
      yellow;
    - the reproduce footer, when either section printed: the command that arms
      one mutant with the literal [<id>] where the reader pastes one, spelled
      from [invocation] and [m.arm_variable]
      ([reproduce: WINDTRAP_MUTATE_ARM=<id> dune exec --instrument-with
        ppx_windtrap.mutate test/test_calc.exe]) — under [`Mirrors] it is
      [WINDTRAP_MUTATE_ARM=<id> <re-run the instrumented suite>], since no
      command line re-runs the suite there. [m.filter] rides it as the replay
      line's filter does. *)
