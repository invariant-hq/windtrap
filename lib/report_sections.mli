(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The report's blocks: the failure projection every transport shares, and the
    section vocabulary the coverage and mutation reports project into.

    Everything here derives from data ({!Failure.t}, {!type:coverage},
    {!type:mutation}) and prints under one explicit [ansi] decision; no function
    reads the environment or the terminal. Section data never carries escape
    codes. {!Report} composes these blocks into the transcript; {!Report_junit}
    projects through them. *)

(** {1:failures Failure projections}

    The projection of one {!Failure.t} shared by the terminal failure blocks,
    the JUnit document and the GitHub annotations. *)

val headline : Failure.t -> string
(** [headline f] is [f] as one unstyled sentence, for single-line fields such as
    JUnit [message] attributes: {!labeled_msg} and [": "] when there is one,
    then the failure ([contract › shape [0]: expected [1; 2], got [1; 3]]). The
    sentences are [expected X, got Y], [both sides equal: X],
    [both sides render as: X], [expected and actual differ (N diff lines)] for a
    multi-line equality, [needle N not found (K-byte haystack)],
    [needle N found at byte B],
    [element I N out of order: at byte B, before byte C],
    [element I N not found at or after byte C (K-byte haystack)],
    [expected exception E, raised F], [expected exception E, none raised],
    [expected an exception, none raised], [uncaught exception: E],
    [exception did not satisfy the predicate: E], [<subject>: mismatch],
    [<subject>: no baseline],
    [<subject>: cannot resolve the path under the project root],
    [property failed (case N, shrunk S steps[, shrinking timed out|, shrink
     limit reached]): [computed from ]<v>], [<v>] being the failure's [summary]
    when it has one, and a message's text. Newlines are spaces, escape codes are
    stripped, and past 80 code points the line ends in […]. *)

val is_subtest_failure : Failure.t -> bool
(** [is_subtest_failure f] is [true] iff [f]'s [subtest] components are
    non-empty, i.e. it was recorded inside {!Run.subtest}. Classification reads
    the record, never the [msg] text. *)

val labeled_msg : Failure.t -> string option
(** [labeled_msg f] is [f]'s [msg] slot as reports display it: for a sub-case
    entry the [leaf › name] label from its [subtest] components, with the user's
    [?msg] after [": "] when there is one; for a plain failure the [?msg]
    itself. *)

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
(** [pp_failure ~ansi ppf f] formats [f]'s entry in a report block. Every line
    but the blank one after a source line is indented four spaces, and the
    output ends with a newline. In order:

    - The location, the bare [<file:line>]. A phase other than {!Failure.Body}
      is the tag [[setup]], [[teardown]] or [[release]] before it
      ([[teardown] test/db.ml:36]), alone on the line when [f] has no location.
    - When [excerpt] is [true] (default [false]) and the file is readable, the
      located source line under it as [  <N> │ <src>], [<src>] without its
      leading and trailing whitespace and printed as a single-line value is
      (below), then one blank line: relative paths resolve against
      {!Os.project_root} first, then as given. An unreadable file prints
      neither.
    - [subtest   <name>], the anchor padded as [expected] is, for an entry
      recorded inside {!Run.subtest}, then the [?msg] annotation, each of its
      lines at the block's indentation, the other control bytes escaped as a
      name's are ({!sanitize_name}).
    - The kind's fact lines: [expected]/[actual] over two single-line values.
      Under [ansi] the changed spans print bold in their side's colour inside an
      otherwise plain value and no [~] line prints, save under a side with a
      changed span of spaces, which colour cannot show; without it one [~] per
      changed code point prints under each side that has a changed span, omitted
      when a side holds a tab or a code point outside U+0020 to U+024F. A pair
      refinement declines prints each side whole in its colour and no [~] line.
      A unified line diff for multi-line renderings; [both sides equal: <v>] for
      a negation; [both sides render as: <v>] over
      [the printer shows less than the equality compares] for a pair the printer
      cannot tell apart;
      [values differ only by a trailing newline (on the <side> side)]. A diff
      prints at most 200 lines, then [… (+N more diff lines)]; a [-]/[+] pair
      that differs only in trailing spaces and tabs has a [~] line under the [-]
      line.
    - [needle  <%S>: <verdict>] over [haystack  <excerpt>] (an [element  <i>]
      line first for an [in_order] chain break), the occurrence marked as a
      changed span is, in the haystack or in its line of a multi-line
      [haystack:] block; then [(excerpt: bytes A-B of a T-byte haystack)] iff
      the excerpt is partial.
    - [expected exception  <e>] over [raised  <e>], never marked, a rendering
      that spans lines an indented block under its anchor, and
      [but no exception was raised] in place of the second side;
      [raised <Constructor> with the wrong message:] over the two messages as
      [expected]/[actual], [%S]-quoted and marked as any two values are;
      [uncaught exception:], or
      [raised exception does not satisfy the predicate:] for a [raises_match]
      rejection, over the exception indented two more;
      [expected an exception, but none was raised]; then the backtrace, at most
      {!max_lines} frames and [… (+N more frames)].
    - A baseline's first fact line, [expect: mismatch], [expect_exact: mismatch]
      or [expect_file "<path>": mismatch|no baseline], then a mismatch's
      correction as hunks with no [---]/[+++] head, or the trailing-newline
      sentence when that is all an [expect_exact] differs by; a missing file's
      as [proposed (N lines):] over at most 20 [+ ] lines indented two more,
      then [… (+N more lines)]; an unresolvable path prints
      [<subject>: the path cannot be proven to lie under the project root],
      [unverified path: <candidate>] and
      [(set WINDTRAP_PROJECT_ROOT to the directory the path is relative to)].
    - [counterexample (case N, shrunk S steps): <v>] ([shrunk 1 step]), a
      multi-line value as a block under it; a failure with a [summary] prints
      the summary as [<v>] and its table under it, the header row faint; a
      pre-image prints [computed from <p>] and, indented two more, the aside
      [(the value has no printer, so this is the input that map and bind
       computed it from; attach a printer with Gen.with_pp to see the value)]; a
      search cut short prints
      [timed out after <T>s while shrinking; counterexample may not be minimal]
      or [shrinking stopped after S steps; counterexample may not be minimal];
      then [which failed at:], or [which failed with:] when the inner failure
      has no location, over the inner failure's entry, indented two more, no
      blank line after its source line.
    - A message's text.
    - When [hints] is [true] (the default), {!val-hints} of [[f]].

    A single-line value over 800 bytes, a needle and a source line included,
    prints its first and last 400, cut on code points, around
    [… (N bytes elided)], [N] counting the carried value's bytes, and is never
    marked.

    The captured-output tail is not rendered here; it is per test, and the
    transports place it.

    Compared data and a source line print each C0 byte and DEL as a lowercase
    [\xNN] escape (LF and TAB excepted) under both [ansi] settings, payload text
    inside [%S] quotes carrying OCaml's escapes instead; the author's own words
    ([?msg], a {!Failure.Message} text, a backtrace) and test names are
    ANSI-stripped under [ansi:false] and pass through under [ansi:true]. *)

val max_lines : int
(** [max_lines] is the one bound on a block's unbounded texts: the frames of a
    backtrace {!pp_failure} prints, and the lines of a captured tail its
    transports print. *)

val hints :
  ?armed:string ->
  ?invocation:Run.invocation ->
  filter:string option ->
  Failure.t list ->
  string list
(** [hints ~filter failures] is the hint lines of a block whose entries are
    [failures], unindented: a word and a command line that runs at least the
    block's test, and says what the block does not. One line per distinct
    command line: [accept:] for each missing or mismatched baseline, then
    [replay:] for each seeded property failure; [[]] when there is neither.

    A baseline failure whose correction the run withheld
    ({!Failure.with_withheld}) has no [accept:], the command having nothing to
    promote or rewrite, and the lines then open with the fact line that says
    why:
    [no correction was kept: the test also failed outside its expectations; fix
     that failure and rerun], or
    [no correction was kept: the test also skipped; skip before the expectation
     or not at all, and rerun].

    [filter] is the test's path string, single-quoted into the command line (as
    [$'…'] with its control bytes escaped when it holds one, so a hint is always
    one line); [None] (a fixture-release row) spells the launcher alone.
    [invocation] (default [`Mirrors]) is the launcher: under [`Exe cmd] the
    lines are [replay: cmd --seed S [--prop-count N] -f 'P'] and
    [accept: cmd -u -f 'P']; under [`Mirrors] they are
    [replay: WINDTRAP_SEED=S … dune runtest] and [accept: dune promote <file>],
    a missing file's being
    [accept: touch '<file>' && dune runtest; dune promote <file>]. [armed] is
    the armed mutant's identifier: [replay:] carries it as [--arm ID] after the
    launcher, or as a leading [WINDTRAP_MUTATE_ARM=ID] under [`Mirrors], and no
    [accept:] prints, an armed run's baseline failures being the mutant's. *)

val sanitize_name : string -> string
(** [sanitize_name s] is [s] with C0 control bytes and DEL escaped OCaml-style
    ([\n], [\t], [\xNN]) and ESC left alone: how every terminal surface prints a
    user-controlled name. *)

val shell_word : string -> string
(** [shell_word s] is [s] as one word of a command line a POSIX shell reads: [s]
    itself when it is made of letters, digits and [_-./:=+,@%], single-quoted
    otherwise ([$'…'] when it holds a control byte). What spells a launcher
    passes the executable's path through it, so a hint runs as pasted. *)

(** {1:sections The section vocabulary}

    The lines, rows, excerpts and rules instrumentation reports are made of. A
    {!Hint} carries no spans by construction. *)

type span = { style : Pp.style option; text : string }
(** The type for a styled run of text. *)

val plain : string -> span
(** [plain text] is [text] unstyled. *)

val styled : Pp.style -> string -> span
(** [styled style text] is [text] under [style]. *)

type column = { gap : string; align : [ `Left | `Right ]; width : int option }
(** The type for a table column: the text before the cell, its alignment, and a
    width floor ([None] fits the widest cell). *)

type excerpt = {
  source : string;  (** The file's text. *)
  marked_lines : int list;  (** The 1-based lines to mark. *)
}
(** The type for a source excerpt: the marked lines of [source], each with one
    line of context, touching windows merged. *)

(** The type for report sections. *)
type section =
  | Line of span list  (** One line; [Line []] is a blank line. *)
  | Hint of string  (** One unstyled line: a command to type. *)
  | Rows of { margin : string; columns : column list; rows : span list list }
      (** A table: each row's cells padded to the widest cell of their column,
          trailing spaces stripped. *)
  | Excerpt of excerpt
      (** The excerpt's regions, a faint [·····] between two: each line as
          [  <N> │ <text>], the number right-aligned in at least four columns, a
          marked line carrying the red [▌] in its margin, [<text>] escaped as
          {!pp_failure} escapes a source line. *)
  | Rule of string option  (** A faint 58-column {!rule}. *)

val rule : width:int -> string option -> string
(** [rule ~width label] is a rule of [width] columns of [─], unstyled, [label]
    centred in it when given ([── label ──]). *)

val print : out:Format.formatter -> ansi:bool -> section list -> unit
(** [print ~out ~ansi sections] writes [sections] to [out] in order, styled
    under [ansi] (no escape codes at all under [ansi:false]), and flushes [out].
*)

(** {1:coverage Coverage}

    The coverage report drawn by [windtrap coverage] over merged files. The
    records below are built by whoever holds the runtime; this module orders
    nothing and counts nothing. *)

type coverage_file = {
  file : string;  (** The source file name as recorded at instrumentation. *)
  visited : int;  (** Points visited at least once. *)
  total : int;  (** Points instrumented. *)
  uncovered : int list;
      (** The 1-based source lines the unvisited points touch, sorted, without
          duplicates. [[]] when [source] is [None]. *)
  source : string option;
      (** The source text, when the builder found it and it is consistent with
          the recorded data. *)
  stale : bool;
      (** [true] when the source was found but changed since the data was
          recorded; [source] is then [None] and [uncovered] is [[]]. *)
}
(** The type for one line of the per-file table. *)

type coverage = {
  visited : int;  (** Points visited at least once, over all files. *)
  total : int;  (** Points instrumented, over all files. *)
  files : coverage_file list;
      (** The per-file table, in print order: by file name, as the builder
          orders it. *)
}
(** The type for a whole coverage report. *)

val coverage_line : min:float option -> visited:int -> total:int -> span list
(** [coverage_line ~min ~visited ~total] is the report's outcome line,
    [coverage: 87.2% (312/358 points)], followed under a gate [min] by
    [, minimum 80%: ok] (green) or [, minimum 90%: FAILED] (red): the gate is
    met iff the unrounded percentage is at least [min]. A percentage, here and
    throughout the report, is red below [min], or below 80 when [min] is [None],
    and unstyled otherwise. *)

val coverage_report :
  mode:[ `Report | `Full ] -> min:float option -> coverage -> section list
(** [coverage_report ~mode ~min c] is the coverage report for [c]. In order:

    - When [c] has files, a faint header row naming the columns, each label at
      its column ([cover], [points], [file] and
      [uncovered lines (-u shows the source)], the parenthesis omitted under
      [`Full]; long file names take this row past 80 columns), then one row per
      file: its percentage, visited/total, name and uncovered line ranges
      ([88-94, 121]). A row prints its first eight ranges, then [(+N more)], [N]
      the ranges not shown; nothing is fitted to a width. A file whose unvisited
      points have no line attribution says [(source not found)], a stale one
      [stale: the source changed; re-run the instrumented tests].
    - Under [`Full], after one blank line, per file with uncovered lines and a
      readable source: [<file>: 75.0% (111/148)], the name bold, a blank line
      and the file's {!Excerpt}; one blank line between files and one after the
      last.
    - {!coverage_line}, always last. *)

(** {1:mutation Mutation}

    The mutation report: survivor blocks, the never-reached rows, the reproduce
    command and the outcome line. The mutation loop commits a survivor's block
    when it finds it and ends on {!mutation_closing}; [windtrap mutants] prints
    {!mutation_report} over a merge. Which mutants survived, in which order, and
    their reaching tests are the producer's. *)

type witness = {
  test : string;
      (** The test's full path, as {!Test_tree.path_to_string} spells it. *)
  loc : Loc.t option;  (** Where the test is declared, when it is known. *)
  exe : string option;
      (** The test executable that ran the test. [None] in a per-executable
          report, where the column is omitted. *)
}
(** The type for a survivor's reaching tests: a test that evaluated the mutated
    line and did not fail when it changed. *)

type mutant = {
  id : string;
      (** The mutant's identifier in the runtime's canonical spelling
          ([lib/calc.ml:9:12:add]). *)
  file : string;  (** The mutated source file. *)
  line : int;  (** 1-based line of the mutated expression. *)
  before : string;  (** The original expression's source text. *)
  after : string;  (** The armed expression's source text. *)
  source : string option;
      (** The mutated file's text, when the producer could read it; the source
          line is dropped otherwise. *)
}
(** The type for one mutant as a block draws it. *)

type survivor = {
  mutant : mutant;  (** The mutant that survived. *)
  witnesses : witness list;
      (** The tests that ran the line and did not fail. Never empty, never
          truncated. *)
}
(** The type for one survived mutant. *)

(** The type for what a report's reached count is relative to, the outcome
    line's subject: [5 reached by this suite],
    [2 reached by the 2 selected tests], [12 reached, 3 executables]. *)
type scope =
  | Suite  (** A per-executable run over its whole suite. *)
  | Selected of int
      (** A per-executable run whose selection narrowed the suite to this many
          tests. *)
  | Executables of int
      (** The aggregate over this many executables' verdict files. *)

type mutation = {
  survivors : survivor list;
      (** Every survived mutant, in the order its block prints. Never capped. *)
  unreached : (string * int) list;
      (** The source file and 1-based line of every mutant no test evaluated. *)
  killed : int;  (** How many mutants were killed. *)
  not_tested : int;
      (** How many reached mutants have no verdict: those an interrupted loop
          did not finish. [0] in every other report. *)
  scope : scope;  (** What the reached count is relative to. *)
}
(** The type for a whole mutation report. The reached count is
    [killed + List.length survivors + not_tested]. *)

val survivor_block : exe_width:int option -> survivor -> section list
(** [survivor_block ~exe_width s] is [s]'s block:

    - [  SURVIVED  lib/calc.ml:9:12:add  a - b → a + b], [SURVIVED] red and the
      identifier bold.
    - The mutated source line as {!pp_failure} prints a located one
      ([      9 │ | Sub -> a - b]), when the source is known and has the line.
    - A blank line, then [3 tests ran this line and none failed:], singular
      [1 test ran this line and did not fail:], and
      [3 tests in 2 executables ran this line and none failed:] when the
      reaching tests name several executables.
    - One row per reaching test: its name ({!sanitize_name}) padded to the
      widest of the block, then its faint location. Under [exe_width = Some w]
      the executable comes first, padded to [w] columns. *)

val mutation_closing : config:Run.config -> mutation -> section list
(** [mutation_closing ~config m] is what ends a report whose survivor blocks are
    already printed, under a [survivors] rule, after a run's summary line. In
    order:

    - The rule that closes the blocks, when [m.survivors] is not empty.
    - When [m.unreached] is not empty, after a blank line: the rule
      [never reached (N)], [N] the mutants; one row per file, by name: how many
      of them it holds, right-aligned in yellow, the file, and [lines 40-41, 57]
      over their distinct lines, eight at most as in a {!coverage_report} row; a
      closing rule.
    - A blank line, when either printed.
    - When [m.survivors] is not empty, [reproduce: <command>], the command that
      arms the first of them: [<cmd> --arm <id>] under [`Exe cmd], and under
      [`Mirrors]
      [WINDTRAP_MUTATE_ARM=<id> dune runtest --force --instrument-with
       ppx_windtrap.mutate], [cmd] and the choice of spelling being the
      invocation of [config]. The command restates the run's selection: [-f],
      [-e], [--tag], [--exclude-tag], [--shard] and [--failed] after [--arm]
      under [`Exe], their mirrors ahead of [dune runtest] under [`Mirrors],
      [--failed] excepted. Unstyled.
    - The outcome line, zero terms omitted:
      [mutants: 1 survived of 12 reached, 11 killed, 2 never reached, 3 not
       tested, 3 executables], survived red, killed green, never reached yellow.
*)

val mutation_report : invocation:Run.invocation -> mutation -> section list
(** [mutation_report ~invocation m] is [m] as a report at rest: when
    [m.survivors] is not empty the rule [survivors (N)], one {!survivor_block}
    per survivor with one blank line between two, the executables one column for
    the report, and a closing rule; then, each after one blank line when
    something precedes it, the never-reached section and the last lines of
    {!mutation_closing} for a run that selects every test. *)
