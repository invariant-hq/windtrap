(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The report's blocks.

    The module holds the one projection of a {!Failure.t} that every transport
    of a run shares, and the section vocabulary of the coverage and mutation
    reports. {!pp_failure} formats the entry of a failure, {!hints} the lines
    that close its block, and {!accept} and {!replay} the commands that end a
    report. {!coverage_report}, {!survivor_block}, {!mutation_closing} and
    {!mutation_report} build {!section} lists, which {!print} writes.

    Everything printed here derives from data: a {!Failure.t}, a
    {!type:coverage} or a {!type:mutation}. A producer builds that data and
    never text that carries a style or a command, so nothing is formatted where
    a failure is raised. Styling is the [ansi] decision that the caller passes
    to {!pp_failure} and to {!print}, and section data holds no escape code. The
    module names no instrumentation runtime, so whoever holds one builds the
    records and spells the identifiers in them.

    No function reads the terminal or a colour setting, and each writes only on
    the formatter that it is given. {!pp_failure} under [~excerpt:true] opens
    the located source file, and the path of a file baseline prints through
    {!Os.display_path}. Both read {!Os.project_root}, hence the environment and
    the working directory.

    A cap is a constant of [report_sections.ml], named here beside its value. *)

(** {1:failures Failure projections}

    An entry is what {!pp_failure} prints for one failure. *)

val headline : Failure.t -> string
(** [headline f] is [f] as one unstyled sentence, for a field that holds a
    single line. It is [labeled_msg f] and [": "] when there is one, then one
    clause for the facts of [f.kind]. Line feeds, carriage returns and tabs
    become spaces. Past 80 code points ([max_headline_chars]) the sentence is
    cut and ends in an ellipsis. Any other control byte is left to the escaping
    of the field that receives the sentence. *)

val is_subtest_failure : Failure.t -> bool
(** [is_subtest_failure f] is [true] iff [f.subtest] is not empty, that is iff
    [f] was recorded inside {!Run.subtest}. *)

val labeled_msg : Failure.t -> string option
(** [labeled_msg f] is the label of [f] in a single-line field. For a failure
    recorded inside {!Run.subtest} it is the components of [f.subtest], the
    test's own name first, joined by {!Test_tree.path_to_string}. [": "] and
    [f.msg] follow when there is one. For any other failure it is [f.msg]. *)

val pp_failure :
  ansi:bool ->
  ?terminal:bool ->
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
    - under [~hints:true], the lines of {!hints} for [f] alone, then its
      {!accept} and {!replay} lines for the test at [filter]. A transport that
      shows one failure at a time, away from the report, carries these.

    The captured output of the test is no part of an entry, because it belongs
    to the test and each transport places it.

    - [terminal] is whether [ppf] reaches a terminal that a reader watches. It
      defaults to [false].
    - [excerpt] defaults to [false]. The source line prints when the file can be
      read and holds the line, and the blank line prints only with it. A
      relative path is tried under {!Os.project_root} first and then as given.
      The line is bounded and escaped as a single-line value is.
    - [hints] defaults to [true].
    - [invocation] and [armed] are the arguments of {!hints}, {!accept} and
      {!replay}, and [filter] is the path of their [`Filter]. Without [filter] a
      command carries no filter.

    {b Equality.} Two single-line renderings print as the expected side over the
    actual side, and the spans of {!Diff.refine} mark what changed. A line of
    [~] marks the span under each side that has one, unless both [ansi] and
    [terminal] hold: the styling of an output that is no terminal may be
    stripped, as dune strips an action's output when its own is no terminal, or
    read as raw escapes.

    When a side spans lines the entry is the unified diff of {!val:Diff.hunks},
    the expected lines as the deleted ones, under the header pair [--- expected]
    and [+++ actual]. It prints at most 200 lines of hunks ([max_diff_lines]),
    hunk heads included and the header pair not, and then a count of the rest.

    A cut side ({!Failure.is_cut}) is compared on what the failure kept of it.

    {b Baseline.} A missing baseline prints the content that it would hold, at
    most 20 lines ([max_proposed_lines]) and then a count of the rest.

    {b Property.} An explicit example is named by its one-based index, any other
    case by its zero-based index and, when it has some, by its shrink steps. The
    entry of the inner failure comes last. It is an entry as above, nested, with
    no blank line after its source line and no hint lines.

    {b Bounds and escaping.} A single-line value, a needle and a source line
    included, prints whole up to 800 bytes ([max_value_bytes]). A longer one
    prints at most 400 bytes from each end around the number of bytes left out,
    and is never marked. {!Text.elide_middle} makes the cut, before any
    escaping. The escape is a projection, which equality, containment and
    baseline storage never see. *)

val max_lines : int
(** [max_lines] is [10], the bound on the two texts of a block that have no
    other. {!pp_failure} prints at most that many frames of a backtrace, and
    {!Report} at most that many lines of captured output in a failure block. *)

val hints :
  ?armed:string -> ?invocation:Run.invocation -> Failure.t list -> string list
(** [hints failures] is the hint lines of a block whose entries are [failures],
    unindented and unstyled. A hint line says what the block does not: a fact,
    or a word and a command that runs as pasted.

    The lines open with one fact line per baseline failure whose correction is
    withheld ([withheld = Some _]), in their order, since no command accepts it.
    Under [`Mirrors] one [accept:] follows for each other baseline failure that
    is missing or mismatched: [dune promote] and the file that holds the
    baseline. Under [`Exe _] none does: the report ends on one {!accept} for the
    whole run. Equal lines print once, and the result is [[]] when no failure
    has a line. A block has no [replay:] either: the report has one ({!replay}).

    - [invocation] is how the run was started ({!type:Run.invocation}) and
      defaults to [`Mirrors].
    - [armed] is the identifier of the armed mutant. The result is then [[]],
      because the baseline failures of an armed run are the mutant's. *)

val accept :
  ?armed:string ->
  ?invocation:Run.invocation ->
  tests:[ `Run of Run.config | `Filter of string option ] ->
  Failure.t list ->
  string option
(** [accept ~tests failures] is the [accept:] line that runs the [tests] again
    under [-u], unindented and unstyled. It is [None] under [`Mirrors], where
    each block names its file ({!hints}), in an armed run, whose baseline
    failures are the mutant's, and when no failure is a baseline failure that is
    missing or mismatched and whose correction is kept. A test that passed has
    no correction, so the line accepts those of [failures] when every test
    produces the text it produced in the run.

    - [tests] selects what runs, as for {!replay}. [`Run config] is the
      selection of a run that executed all of it: an accepted test passes, so
      the new run would go past a test where [-x] stopped the old one.
    - [invocation] defaults to [`Mirrors]. Under [`Exe cmd] the line is [cmd] as
      given, [-u] and then the flags of [tests].
    - [armed] is the identifier of the armed mutant. *)

val replay :
  ?armed:string ->
  ?invocation:Run.invocation ->
  tests:[ `Run of Run.config | `Filter of string option ] ->
  Failure.t list ->
  string option
(** [replay ~tests failures] is the [replay:] line that runs the [tests] again
    with the values that [failures] drew, unindented and unstyled. It is [None]
    when no failure drew any: none is a {!Failure.Property} failure, or a
    {!Failure.Timeout} of a property's case, whose case was generated. The line
    carries the armed mutant, the seed of the first such failure (a run has one
    root) and, when a failure's [count] is [Some _] (see {!type:Failure.kind}),
    the largest such count. A test draws its values from the seed and its path,
    so each failed test draws what it failed on.

    - [tests] selects what runs. [`Run config] is the tests of the run that
      [config] configured: the line restates its [-f], [-e], [--tag],
      [--exclude-tag] and [--shard], and its [--failed] after its [-o] when
      [config.log_dir] is not {!Os.default_log_dir}, since the last-failed store
      lies under it. [`Filter filter] is the test at the path [filter], or every
      test when it is [None].
    - [invocation] defaults to [`Mirrors]. Under [`Exe cmd] the line is [cmd] as
      given and then its flags. Under [`Mirrors] it is the mirrors of those
      flags in front of [dune runtest]. [--failed] has no mirror, and a filter
      or an exclusion of several patterns, which no mirror holds, is left out,
      so the line runs more tests, never fewer.
    - [armed] is the identifier of the armed mutant, passed through
      {!shell_word}. The line arms it, and under [`Mirrors] names the mutation
      backend, since the armed action exists only in that build. *)

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

val dune_exec : mutate:bool -> string -> Run.invocation
(** [dune_exec ~mutate path] is the [`Exe] invocation [dune exec <path> --] of
    the executable at [path], relative to the project root. [<path>] is [path]
    as one word of {!shell_word}, and [./path] when [path] has no [/]. When
    [mutate] is [true], [--instrument-with ppx_windtrap.mutate] precedes
    [<path>], and the command runs the build with the mutants. Every [dune exec]
    command of a report is one of these. *)

(** {1:sections The section vocabulary}

    Adding a constructor to {!section} is a design amendment, as adding one to
    {!type:Failure.kind} is. *)

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
    points. A column is as wide as its widest cell and at least [width]. *)

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
  | Hint of string  (** One line that is a command to type. *)
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
      row per file in the order of [c.files].
    - under [`Full], for each file that has uncovered lines and a [source], a
      heading with the name and the numbers of the file, then the {!Excerpt} of
      its uncovered lines.
    - {!coverage_line}, always last.

    A row shows its first eight ranges ([max_ranges]), then the number of ranges
    left out. Nothing is fitted to a width, so a long file name or a row of
    eight ranges can pass 80 columns. *)

(** {1:mutation Mutation}

    {!mutation_report} is made of {!survivor_block} and of the sections of
    {!mutation_closing}, so the two reports end alike.

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

type not_evaluated = {
  mutant : mutant;  (** The mutant whose child did not evaluate its site. *)
  invocation : Run.invocation;
      (** How to run again an executable whose child did not evaluate it. *)
}
(** The type for a reached mutant whose child passed without evaluating its
    site, which is no survivor. *)

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
  not_evaluated : not_evaluated list;
      (** Every reached mutant whose child did not evaluate its site, in the
          order in which it prints. *)
  unreached : (string * int) list;
      (** The file and the one-based line of each mutant that no test evaluated,
          one pair per mutant, except those of [outside_tests]. *)
  outside_tests : (string * int) list;
      (** The file and the one-based line of each mutant that no test evaluated
          and that the dry run evaluated outside every test, one pair per
          mutant. *)
  killed : int;  (** The number of mutants killed. *)
  not_tested : int;
      (** The number of reached mutants that have no verdict, which are those
          that an interrupted loop did not finish. It is [0] in every other
          report. *)
  scope : scope;  (** What the reached count is relative to. *)
}
(** The type for a whole mutation report. Its reached count is
    [killed + List.length survivors + List.length not_evaluated + not_tested].
*)

val survivor_block : exe_width:int option -> survivor -> section list
(** [survivor_block ~exe_width s] is the block of [s]. It holds, in this order:
    - the title.
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
    - when [m.not_evaluated] is not empty, the not-evaluated section, between an
      opening rule that carries their number and a closing rule. It says in one
      line that each site ran in the dry run and not in the child of its mutant,
      then gives each mutant on one line and, under it, the [arm:] command that
      runs its [invocation] with [--arm] and the selection of [config], as the
      [reproduce:] command below does.
    - when [m.unreached] is not empty, the never-reached section, between an
      opening rule that carries the number of mutants and a closing rule.
    - when [m.outside_tests] is not empty, the section of the mutants evaluated
      outside tests, which is the never-reached section of those mutants under
      its own title and a line that says where such a site runs.
    - when [m.survivors] is not empty, the [reproduce:] command, which arms the
      first survivor of [m] under the selection of [config]. Under [`Exe cmd],
      the invocation of [config], it is [cmd], [--arm] and the run's selection
      as {!replay} restates it, a repeatable flag once per value. Under
      [`Mirrors] it is the mirrors of the same flags in front of a forced
      [dune runtest] that names the mutation backend, except [--failed] and a
      filter or an exclusion of several patterns, which no mirror can hold.
    - the outcome line, always last. *)

val mutation_report : invocation:Run.invocation -> mutation -> section list
(** [mutation_report ~invocation m] is [m] as a report at rest. It holds one
    {!survivor_block} per survivor, between an opening rule that carries their
    number and a closing rule. The executables of all blocks form one column,
    which is absent when no reaching test names an executable. The not-evaluated
    section, the never-reached section, the section of the mutants evaluated
    outside tests, the [reproduce:] command and the outcome line follow, as
    {!mutation_closing} builds them for a run that selects every test. *)
