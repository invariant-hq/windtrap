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
(** [headline f] is a one-line, unstyled summary of [f]
    ([expected true, got false]) for single-line fields such as JUnit [message]
    attributes: no newlines, escape codes stripped, long renderings truncated
    with an ellipsis. *)

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
  ?filter:string ->
  ?invocation:Run.invocation ->
  Format.formatter ->
  Failure.t ->
  unit
(** [pp_failure ~ansi ppf f] formats [f]'s report block: the phase (when not
    {!Failure.Body}) and location header, the [?msg] annotation, and the kind
    detail: expected/actual with changed spans highlighted (a [~~~] marker line
    under [ansi:false]; a deletion marks only the expected side) or a unified
    line diff for multi-line renderings; the needle of a containment with its
    verdict ([found at byte 10], [not found], or under a {!Failure.Ordered}
    demand [not found at or after byte 36] plus the [element] index line) and a
    haystack excerpt, one faint line stating the excerpted byte range and the
    haystack's total size when the excerpt is partial; the expected and raised
    exceptions and backtrace of a raise; the subject and state of a baseline,
    then, for a missing or mismatched one, its acceptance command; a property's
    counterexample, shrink count, inner failure and replay line; a message's
    text. Every line is indented four spaces and the output ends with a newline.
    The captured-output tail is not rendered here; it is per test, and the
    transports place it.

    Compared data prints each C0 byte and DEL as a lowercase [\xNN] escape (LF
    and TAB excepted) under both [ansi] settings, payload text inside [%S]
    quotes carrying OCaml's escapes instead; the author's own words ([?msg], a
    {!Failure.Message} text, a backtrace) and test names are ANSI-stripped under
    [ansi:false] and pass through under [ansi:true]. Under a
    {!Failure.Declaration} location one faint line names the [~__POS__] remedy,
    except for a property, an uncaught exception or a file baseline.

    [excerpt] (default [false]) also prints the located source line, read from
    disk best-effort with relative paths resolved against {!Os.project_root}
    first, then as given. [filter] is the replay line's shell-quoted filter
    value (the test's path string); without it the replay line carries the seed
    alone. [invocation] (default [`Mirrors]) spells the acceptance and replay
    commands. *)

val sanitize_name : string -> string
(** [sanitize_name s] is [s] with C0 control bytes and DEL escaped OCaml-style
    ([\n], [\t], [\xNN]) and ESC left alone: how every terminal surface prints a
    user-controlled name. *)

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

val coverage_line : visited:int -> total:int -> unit -> span list
(** [coverage_line ~visited ~total ()] is the summary line
    ([coverage: 87.2% (312/358 points)]), the percentage green at 80% and above,
    yellow at 60%, red below. *)

val coverage_report : mode:[ `Report | `Full ] -> coverage -> section list
(** [coverage_report ~mode c] is the coverage block for [c]: the summary line
    ({!coverage_line}); one line per file with its percentage (styled as the
    summary line's), visited/total, name and uncovered line ranges
    ([uncovered: 88-94, 121], at most eight regions then
    [(+N more, -u shows them)]; none for a fully covered file), a file whose
    unvisited points have no line attribution noting the missing source, and a
    stale file stating the staleness and the fix instead; and under [`Full] a
    source excerpt per file with uncovered lines and a readable source, headed
    [lib/eval.ml — 75.0% (111/148)], each uncovered region with one line of
    context and a gutter marker. *)

(** {1:mutation Mutation}

    The mutation report: survivor blocks, unreached blocks, one summary line and
    the reproduce footer. One layout serves the mutation loop's per-executable
    report and [windtrap mutants]' aggregate. A survivor is drawn as a failure
    block, in red; an unreached mutant is the same block without the sentence,
    in yellow. Ordering and witness lists are the producer's. *)

type witness = {
  test : string;
      (** The test's full path, as {!Test_tree.path_to_string} spells it. *)
  loc : Loc.t option;  (** Where the test is declared, when it is known. *)
  exe : string option;
      (** The test executable that ran the test. [None] in a per-executable
          report, where the column is omitted. *)
}
(** The type for survivor witnesses: a test that evaluated the mutated line and
    did not fail when it changed. *)

type mutant = {
  id : string;
      (** The mutant's identifier in the runtime's canonical spelling
          ([lib/calc.ml:9:12:add]). *)
  file : string;  (** The mutated source file. *)
  line : int;  (** 1-based line of the mutated expression. *)
  before : string;  (** The original expression's source text. *)
  after : string;  (** The armed expression's source text. *)
  source : string option;
      (** The mutated file's text, when the producer could read it; the excerpt
          row is dropped otherwise. *)
}
(** The type for one mutant as a block draws it. *)

type survivor = {
  mutant : mutant;  (** The mutant that survived. *)
  witnesses : witness list;
      (** The tests that ran the line and did not fail. Never empty, never
          truncated. *)
}
(** The type for one survived mutant. *)

(** The type for what a report's reached count is relative to, the summary
    line's subject: [5 reached by this suite],
    [2 reached by the 2 selected tests], [12 reached · 3 executables]. *)
type scope =
  | Suite  (** A per-executable run over its whole suite. *)
  | Selected of int
      (** A per-executable run whose selection narrowed the suite to this many
          tests. *)
  | Executables of int
      (** The aggregate over this many executables' verdict files. *)

type mutation = {
  survivors : survivor list;
      (** Every survived mutant, ordered by witness count descending, then by
          identifier. Never capped. *)
  unreached : mutant list;
      (** Every mutant no test evaluated, ordered by identifier. Aggregate only:
          a per-executable report hands over [[]]. *)
  killed : int;  (** How many mutants were killed. *)
  scope : scope;  (** What the reached count is relative to. *)
  filter : string option;
      (** The run's [-f] filter, restated in the reproduce footer; [None] for
          the aggregate. *)
}
(** The type for a whole mutation report. The reached count is
    [killed + List.length survivors]. *)

val mutation_report : invocation:Run.invocation -> mutation -> section list
(** [mutation_report ~invocation m] is [m] as sections: the survivor section
    when [m.survivors] is not empty (the labelled rule [survivors (2)], then per
    survivor the head row [  SURVIVED  lib/calc.ml:9:12:add    a - b  →  a + b],
    the excerpt row, and the sentence [3 tests ran this line and none failed:],
    singular [1 test ran this line and did not fail:], over one row per
    witness); the unreached section when [m.unreached] is not empty
    ([never reached (2)], one block per mutant, no sentence); the closing rule
    when either printed; the summary line with zero terms omitted
    ([mutants: 1 survived of 12 reached · 11 killed · 2 never reached · 3
      executables], survived red, killed green, never reached yellow); and, when
    either section printed, the reproduce footer arming one mutant with the
    literal [<id>], spelled from [invocation]: [--arm] under [`Exe] and
    [WINDTRAP_MUTATE_ARM=<id> <re-run the instrumented suite>] under [`Mirrors],
    [m.filter] restated as the replay line's is. *)
