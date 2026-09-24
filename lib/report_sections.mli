(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The report's blocks.

    The module holds the one projection of a {!Failure.t} that every transport
    of a run shares, and the section vocabulary of the coverage and mutation
    reports. {!pp_failure} formats the entry of a failure and {!hints} the
    commands that close its block. {!coverage_report}, {!survivor_block},
    {!mutation_closing} and {!mutation_report} build {!section} lists, which
    {!print} writes.

    Everything printed here derives from data: a {!Failure.t}, a
    {!type:coverage} or a {!type:mutation}. A producer builds that data and
    never text that carries a style or a command, so nothing is formatted where
    a failure is raised. Styling is the [ansi] decision that the caller passes
    to {!pp_failure} and to {!print}, and section data holds no escape code. The
    module names no instrumentation runtime, so whoever holds one builds the
    records and spells the identifiers in them.

    No function reads the terminal or a colour setting, and each writes only on
    the formatter that it is given: where and when its text shows is its
    caller's contract. {!pp_failure} under [~excerpt:true] opens the located
    source file, and the path of a file baseline prints through
    {!Os.display_path}. Both read {!Os.project_root}, hence the environment and
    the working directory.

    A cap is a constant of [report_sections.ml], named here beside its value. *)

(** {1:failures Failure projections}

    An entry is what {!pp_failure} prints for one failure. *)

val headline : Failure.t -> string
(** [headline f] is [f] as one unstyled sentence, for a field that holds a
    single line. It is [labeled_msg f] and [": "] when there is one, then one
    clause for the facts of [f.kind], which {!pp_failure} prints in full. Line
    feeds, carriage returns and tabs become spaces. Past 80 code points
    ([max_headline_chars]) the sentence is cut and ends in an ellipsis. Any
    other control byte is left to the escaping of the field that receives the
    sentence. *)

val is_subtest_failure : Failure.t -> bool
(** [is_subtest_failure f] is [true] iff [f.subtest] is not empty, that is iff
    [f] was recorded inside {!Run.subtest}. It reads the record and never the
    text of [f.msg]. {!Report} counts such entries as the subtest failures of
    its summary, and {!Report_junit} writes each as a testcase of its own. *)

val labeled_msg : Failure.t -> string option
(** [labeled_msg f] is the label of [f] in a single-line field. For a failure
    recorded inside {!Run.subtest} it is the components of [f.subtest], the
    test's own name first, joined by {!Test_tree.path_to_string}. [": "] and
    [f.msg] follow when there is one. For any other failure it is [f.msg].

    An entry of {!pp_failure} does not print it. *)

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
(** [pp_failure ~ansi ppf f] formats the entry of [f] on [ppf], each line
    through {!render}. The entry ends with a newline and holds, in this order:
    - the location of [f], with its phase before it when [f.phase] is not
      {!Failure.Body}. The phase stands alone when [f] has no location.
    - under [~excerpt:true], the source line at that location and a blank line.
    - the names of the subtests that [f] was recorded in, without the test's own
      name.
    - [f.msg], line by line.
    - the facts of [f.kind], as the paragraphs below list them.
    - under [~hints:true], the lines of {!hints} for [f] alone.

    The captured output of the test is no part of an entry, because it belongs
    to the test and each transport places it.

    - [excerpt] defaults to [false]. The source line prints when the file can be
      read and holds the line, and the blank line prints only with it. A
      relative path is tried under {!Os.project_root} first and then as given.
      The line is bounded and escaped as a single-line value is.
    - [hints] defaults to [true].
    - [filter], [invocation] and [armed] are the arguments of {!hints}. Without
      [filter] a command carries no filter.

    {b Equality.} Two single-line renderings print as the expected side over the
    actual side, and the spans of {!Diff.refine} mark what changed. With [ansi]
    a changed span is styled in its side's colour and bold. Without it a line of
    [~] marks the span under each side that has one. A pair that {!Diff.refine}
    declines, and one whose expected side is a claim ({!Failure.predicate}),
    print each side in one style and unmarked.

    When a side spans lines the entry is the unified diff of {!val:Diff.hunks},
    the expected lines as the deleted ones. It prints at most 200 lines
    ([max_diff_lines]), hunk heads included, and then a count of the rest. A
    deleted line and the one inserted line that answers it may differ in
    trailing blanks alone. A marker line then prints under the deleted one,
    under both [ansi] settings.

    Three equalities have nothing to mark. A negated one ([not_]) prints the one
    value that both sides render as. An equality whose renderings are equal byte
    for byte prints that value too, with a line saying that the printer shows
    less than the equality compares. One whose renderings differ by a final
    newline alone prints a sentence that names the longer side.

    {b Containment.} The entry prints the chain index of an {!Failure.Ordered}
    demand, then the needle with the verdict of the search. The needle is named
    [prefix] under {!Failure.Prefix}, [suffix] under {!Failure.Suffix} and
    [needle] otherwise. The verdict says whether and at which byte the needle
    was found, for an affix found elsewhere that it is not at the start or not
    at the end, and for an ordered demand at which byte the search had resumed.
    The excerpt of the haystack follows, whole, with the occurrence marked in it
    as a changed span is. Its byte range comes last, when it is not the whole
    haystack.

    {b Raise.} The entry prints the expected exception over the raised one, each
    in one style and never marked, or over a sentence when nothing was raised. A
    [message_diff] prints the shared constructor once, then the two messages as
    the sides of an equality, quoted as OCaml strings. An assertion that named
    no exception prints the raised one, under a line that tells an uncaught
    exception from a rejected [raises_match] predicate. When nothing was raised
    it prints a sentence instead. The recorded backtrace closes the entry, at
    most {!max_lines} frames and then a count of the rest.

    {b Baseline.} The first line names the expectation and its state: [expect],
    [expect_exact], or [expect_file] with its path through {!Os.display_path}. A
    mismatch then prints the correction as hunks, from the baseline to the
    produced text and under the cap of an equality's diff. It prints the
    final-newline sentence of an equality when that is all that differs. A
    missing baseline prints the content that it would hold, at most 20 lines
    ([max_proposed_lines]) and then a count of the rest. An unresolvable path
    prints the unproven path and names [WINDTRAP_PROJECT_ROOT] as the way out.

    {b Property.} The head line names the case and carries the counterexample.
    An explicit example is named by its one-based index, any other case by its
    zero-based index and, when it has some, by its shrink steps. A [summary] and
    a {!Failure.Pre_image} print as {!type:Failure.kind} asks of a renderer. A
    search that did not converge ({!Failure.type-shrink_end}) adds a line that
    says why, and a candidate that raised adds a second line with its exception.
    The {!headline} of such a failure names the stop: [shrink limit reached],
    [shrinking stopped] or [shrinking timed out].

    The entry of the inner failure comes last. It is an entry as above, nested,
    with no blank line after its source line and no hint lines.

    {b Message.} The text prints line by line, and an empty text as a
    placeholder that says so.

    {b Bounds and escaping.} A single-line value, a needle and a source line
    included, prints whole up to 800 bytes ([max_value_bytes]). A longer one
    prints at most 400 bytes from each end around the number of bytes left out,
    and is never marked. {!Text.elide_middle} makes the cut, before any
    escaping.

    A text of several lines prints line by line, and every line of the entry is
    escaped by {!render}. A needle and the messages of a [message_diff] carry
    OCaml's escapes before that. The escape is a projection, which equality,
    containment and baseline storage never see. *)

val max_lines : int
(** [max_lines] is [10], the bound on the two texts of a block that have no
    other. {!pp_failure} prints at most that many frames of a backtrace, and
    {!Report} at most that many lines of captured output in a failure block. *)

val hints :
  ?armed:string ->
  ?invocation:Run.invocation ->
  filter:string option ->
  Failure.t list ->
  string list
(** [hints ~filter failures] is the hint lines of a block whose entries are
    [failures], unindented and unstyled. A hint line is a word and a command
    that runs as pasted and says what the block does not.

    The lines are one [accept:] for each baseline failure that is missing or
    mismatched, then one [replay:] for each {!Failure.Property} failure whose
    case was generated. Equal lines print once, and the result is [[]] when no
    failure has a command. A baseline failure whose correction is withheld
    ([withheld = Some _]) has no [accept:], since the command would accept
    nothing. The lines then open with one fact line per such failure, in their
    order, equal lines once: [correction refused (line N): <reason>] for a
    {!Failure.Refused} literal, and [no correction was kept: <reason>]
    otherwise.

    - [filter] is the path of the block's test as a string. It is single-quoted
      into each command, in the [$'…'] form when it holds a control byte, so a
      hint is one line whatever the path holds. [None] spells the commands
      without a filter.
    - [invocation] is how the run was started ({!type:Run.invocation}) and
      defaults to [`Mirrors]. Under [`Exe cmd] a command is [cmd] as given and
      then its flags. Under [`Mirrors] a [replay:] sets the mirrors of those
      flags in front of [dune runtest], and an [accept:] is [dune promote].
    - [armed] is the identifier of the armed mutant, passed through
      {!shell_word}. Every [replay:] then arms it. Neither an [accept:] nor the
      fact line of a withheld correction prints, because the baseline failures
      of an armed run are the mutant's.

    A [replay:] carries the armed mutant, the seed, the filter, and the case
    count when the failure's [count] is [Some _] (see {!type:Failure.kind}). An
    [accept:] carries [-u] and the filter, or under [`Mirrors] the file that
    holds the baseline. That file is the one of the failure's location for a
    literal, and the path of a file baseline through {!Os.display_path}. *)

(** {1:names Names and command words} *)

val release_title : string
(** [release_title] is ["fixture release"], the name under which every sink
    reports a failed fixture release. *)

val shell_word : string -> string
(** [shell_word s] is [s] as one word of a shell command line. It is [s] itself
    when [s] is not empty and made of letters, digits and the characters of
    [_-./:=+,@%], and [s] in single quotes otherwise. A word that holds a
    control byte takes the [$'…'] form, which bash, zsh and ksh read and POSIX
    [sh] does not define.

    Whoever spells the [cmd] of an [`Exe] invocation passes the path of the
    executable through it, so a printed command runs as pasted. *)

(** {1:sections The section vocabulary}

    The lines, tables, source excerpts and rules that the coverage and mutation
    reports are made of. Adding a constructor to {!section} is a design
    amendment, as adding one to {!type:Failure.kind} is. *)

type span = { style : Pp.style option; text : string }
(** The type for a run of text under one style, or under none. Styles do not
    nest ({!Pp.style}), so a line is a flat list of spans. [text] is any bytes,
    a test's included: it prints through {!Text.escape_controls}, so a span
    shows its bytes and never drives the terminal. *)

val plain : string -> span
(** [plain text] is [text] unstyled. *)

val styled : Pp.style -> string -> span
(** [styled style text] is [text] under [style]. *)

val render : ansi:bool -> span list -> string
(** [render ~ansi l] is [l] as one line: each text escaped, then, iff [ansi] and
    the text is not empty, between the SGR sequence of its style and the reset
    [ESC \[0m]. Every line that this module formats goes through it, and apart
    from the erase of {!Report}'s live line it is the only writer of an escape
    sequence. *)

val width : span list -> int
(** [width l] is the columns of [l] as it prints, in code points, escapes
    counted. Every padding and every [~] line of this module is measured so. *)

type column = { gap : string; align : [ `Left | `Right ]; width : int option }
(** The type for a column of {!Rows}. [gap] is the text before each cell,
    [align] the side on which its cells are aligned, and [width] a floor in code
    points. A column is as wide as its widest cell and at least [width].
    {!mutation_report} uses the floor to keep the executables of all its blocks
    in one column. *)

type excerpt = {
  source : string;  (** The text of the file. *)
  marked_lines : int list;
      (** The one-based lines to mark, in any order. A line outside [source] is
          dropped. *)
}
(** The type for a source excerpt: the marked lines of [source], each with one
    line of context on either side. *)

(** The type for report sections. *)
type section =
  | Line of span list  (** One line. [Line []] is a blank line. *)
  | Hint of string
      (** One line that is a command to type. It takes no span, so a hint
          carries no style. *)
  | Rows of { margin : string; columns : column list; rows : span list list }
      (** A table. A row is [margin], then for each column its [gap] and its
          cell, which is padded to the width of its column outside its style.
          Trailing spaces are stripped, and a cell beyond [columns] is not
          printed. *)
  | Excerpt of excerpt
      (** The regions of an excerpt, windows that overlap or touch forming one
          region. Each line carries its number, and a marked line a marker. The
          text of a line is whole, neither dedented nor elided. *)
  | Rule of string option
      (** A {!rule} of the width that every report shares, with its label when
          given. *)

val rule : width:int -> string option -> string
(** [rule ~width label] is a horizontal rule [width] columns wide, unstyled,
    with [label] centred in it when given. A long label takes the rule past
    [width]. *)

val print : out:Format.formatter -> ansi:bool -> section list -> unit
(** [print ~out ~ansi sections] writes [sections] to [out] in order, each line
    through {!render}, and flushes [out]. *)

(** {1:coverage Coverage}

    The report of [windtrap coverage] over merged data. This module derives the
    percentages and the line ranges from them, and changes no order and no
    count. *)

type coverage_file = {
  file : string;
      (** The name of the source file as recorded at instrumentation. *)
  visited : int;  (** The points visited at least once. *)
  total : int;  (** The points instrumented. *)
  uncovered : int list;
      (** The one-based source lines that the unvisited points touch. The
          producer must sort the list and remove its duplicates, because the
          ranges are read off it as given. It is [[]] when [source] is [None],
          because a point is attributed to a line only through the text. *)
  source : string option;
      (** The source text, when the producer found it and it agrees with the
          recorded data. The source view of [`Full] needs it. *)
  stale : bool;
      (** [true] when the source was found and has changed since the data was
          recorded. [source] is then [None] and [uncovered] is [[]]. *)
}
(** The type for the row of one file. *)

type coverage = {
  visited : int;  (** The points visited at least once, over all files. *)
  total : int;  (** The points instrumented, over all files. *)
  files : coverage_file list;  (** The files, in the order their rows print. *)
}
(** The type for a whole coverage report. *)

val percent : visited:int -> total:int -> float
(** [percent ~visited ~total] is the percentage of [visited] in [total],
    unrounded, and [100.] when [total] is [0]. Every percentage of this module
    is computed by it, and a gate on {!coverage_line} must compare it. *)

val coverage_line : min:float option -> visited:int -> total:int -> span list
(** [coverage_line ~min ~visited ~total] is the outcome line of the report:
    {!percent} with one decimal, and the two counts. Under a gate [min] the line
    goes on with [min], printed by {!Pp.decimal}, and says whether the gate is
    met, that is whether {!percent} is at least [min]. A percentage, here and in
    every line of {!coverage_report}, prints with one decimal and is styled as
    failing below [min], or below 80 when [min] is [None]. *)

val coverage_report :
  mode:[ `Report | `Full ] -> min:float option -> coverage -> section list
(** [coverage_report ~mode ~min c] is the coverage report of [c], and [`Full] is
    the [-u] of the command. It holds, in this order:
    - when [c.files] is not empty, a header row that names the columns, then one
      row per file in the order of [c.files]. A row holds the percentage of the
      file, its visited and total points, its name and its uncovered lines as
      ranges. A fully covered file has no ranges. In their place a stale file
      says that its source changed and how to refresh the data, and a file whose
      unvisited points have no line says that its source was not found.
    - under [`Full], for each file that has uncovered lines and a [source], a
      heading with the name and the numbers of the file, then the {!Excerpt} of
      its uncovered lines.
    - {!coverage_line}, always last.

    A row shows its first eight ranges ([max_ranges]), then the number of ranges
    left out. Under [`Report] the last label of the header row carries the hint
    that [-u] shows the source. Nothing is fitted to a width, so a long file
    name or a row of eight ranges can pass 80 columns. *)

(** {1:mutation Mutation}

    The mutation report: survivor blocks, the never-reached section, the
    [reproduce:] command and the outcome line. {!mutation_report} is made of
    {!survivor_block} and of the sections of {!mutation_closing}, so the two
    reports end alike.

    Which mutants survived, in which order, and their reaching tests are the
    producer's. No count is measured here, since each one is a field of
    {!type:mutation}, the length of one of its lists, or their sum. *)

type witness = {
  test : string;
      (** The path of the test, as {!Test_tree.path_to_string} spells it. *)
  loc : Loc.t option;
      (** Where the test is declared, when the producer knows it. *)
  exe : string option;
      (** The executable that ran the test. It is [None] in the report of one
          executable, which has no such column. *)
}
(** The type for a reaching test of a survivor: a test that evaluated the
    mutated site and did not fail when it changed. *)

type mutant = {
  id : string;
      (** The identifier of the mutant, spelled by the producer with the
          runtime's own function. The report prints it and hands it to [--arm]
          through {!shell_word}, without spelling it again. *)
  file : string;
      (** The mutated source file. No function of this module reads it. *)
  line : int;  (** The one-based line of the mutated expression. *)
  before : string;  (** The source text of the original expression. *)
  after : string;  (** The source text of the expression that replaces it. *)
  source : string option;
      (** The text of the mutated file, when the producer could read it. *)
}
(** The type for a mutant as a block shows it. *)

type survivor = {
  mutant : mutant;  (** The mutant that survived. *)
  witnesses : witness list;
      (** The reaching tests of [mutant]. The list must not be empty, because a
          mutant that no test evaluated is never reached and is no survivor. It
          is never truncated. *)
}
(** The type for a survived mutant. *)

(** The type for what the reached count of a report is relative to. The outcome
    line names it. *)
type scope =
  | Suite  (** The run of one executable over its whole suite. *)
  | Selected of int
      (** The run of one executable whose selection narrowed the suite to this
          many tests. *)
  | Executables of int
      (** A merge over the verdict files of this many executables. *)

type mutation = {
  survivors : survivor list;
      (** Every survivor, in the order in which its block prints. *)
  unreached : (string * int) list;
      (** The file and the one-based line of each mutant that no test evaluated,
          one pair per mutant. *)
  killed : int;  (** The number of mutants killed. *)
  not_tested : int;
      (** The number of reached mutants that have no verdict, which are those
          that an interrupted loop did not finish. It is [0] in every other
          report. *)
  scope : scope;  (** What the reached count is relative to. *)
}
(** The type for a whole mutation report. Its reached count is
    [killed + List.length survivors + not_tested]. *)

val survivor_block : exe_width:int option -> survivor -> section list
(** [survivor_block ~exe_width s] is the block of [s]. It holds, in this order:
    - the title, with the identifier of the mutant and its rewrite, from
      [before] to [after].
    - the mutated source line, as {!pp_failure} prints a source line, when
      [source] is known and holds the line.
    - the sentence that counts the reaching tests, and their executables when
      they name several.
    - one row per reaching test: its name, then its location when it has one.
      Under [exe_width = Some w] the executable comes first, in a column at
      least [w] wide that is empty for a test that names none. *)

val mutation_closing : config:Run.config -> mutation -> section list
(** [mutation_closing ~config m] is what ends a report whose survivor blocks are
    already printed under an opening rule. It holds, in this order:
    - the rule that closes the blocks, when [m.survivors] is not empty. It is
      decided on [m], whatever was printed.
    - when [m.unreached] is not empty, the never-reached section, between an
      opening rule that carries the number of mutants and a closing rule. It has
      one row per file, in name order: the number of its mutants, the file, and
      their distinct lines as ranges, bounded as {!coverage_report} bounds those
      of a row.
    - when [m.survivors] is not empty, the [reproduce:] command, which arms the
      first survivor of [m] under the selection of [config]. Under [`Exe cmd],
      the invocation of [config], it is [cmd], [--arm] and the run's [-f], [-e],
      [--tag], [--exclude-tag], [--shard] and [--failed], a repeatable flag once
      per value. Under [`Mirrors] it is the mirrors of the same flags in front
      of a forced [dune runtest] that names the mutation backend, except
      [--failed] and a filter or an exclusion of several patterns, which no
      mirror can hold.
    - the outcome line, always last: the survivors among the reached mutants,
      with the words of [m.scope], then the killed, the never reached and the
      not tested. A zero term is omitted, the reached count excepted. Under
      [Executables _] the line ends on the number of executables. *)

val mutation_report : invocation:Run.invocation -> mutation -> section list
(** [mutation_report ~invocation m] is [m] as a report at rest. It holds one
    {!survivor_block} per survivor, between an opening rule that carries their
    number and a closing rule. The executables of all blocks form one column,
    which is absent when no reaching test names an executable. The never-reached
    section, the [reproduce:] command and the outcome line follow, as
    {!mutation_closing} builds them for a run that selects every test. *)
