(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The [windtrap coverage] command.

    The command merges the dumps that instrumented test executables wrote and
    reports expression coverage for each source file. It only reads files, so it
    runs no test and drives no build. A test run prints no coverage number; this
    command reports it, and [--min] gates it.

    {!run} is the whole command.
    {!Windtrap.Private.Report_sections.coverage_report} states what the report
    holds. This interface states the {{!section-files}files} that are merged,
    the {{!section-gates}gates}, and the two
    {{!section-formats}machine formats}, whose keys and records are frozen. *)

(** {1:files Files}

    The dumps are those that {!Data_files.discover} finds for
    [Windtrap_runtime.Coverage.format] and the [PATH] arguments. A dump is
    judged from the identity on its header ({!Data_files.identity}) before it is
    loaded: a dump of another build is excluded whatever its records hold, and a
    dump of the current build must load. A dump of another build is one whose
    {!Data_files.val-freshness} is not [Fresh], and no flag keeps it. The first
    dump that is not excluded and cannot be read or parsed, its header included,
    ends the command with its {!Windtrap_runtime.Coverage.pp_error}.

    On standard error the command says the {!Data_files.warnings} of the
    excluded dumps, then {!Data_files.all_excluded} when no dump is left, and
    then once, in a sentence of its own, how to refresh them.

    A recorded source is looked for by {!Windtrap_runtime.Coverage.file_reports}
    under the roots of {!Data_files.discover}: the root of the project after a
    search, and the current directory under [PATH] arguments. The rows of the
    report come in the order of file names that
    {!Windtrap_runtime.Coverage.file_reports} gives. *)

(** {1:gates Gates}

    Both gates run after the report or the document is printed, whatever the
    first one finds.

    [--expect PATH] may be repeated. It requires coverage data for [PATH] when
    it is a file, and for every [.ml], [.mll] and [.mly] file under it when it
    is a directory. The walk skips [_build], [_opam] and every directory whose
    name starts with a dot. [--do-not-expect PATH] exempts a file, or the
    sources under a directory, and is read only under [--expect].

    A path and a recorded name are compared as stems. A stem is the path without
    its empty and [.] components and with its base name cut at the first dot.
    [lib/calc.ml], [./lib/calc.ml], [lib/calc.pp.ml] and [lib/calc.mll] are one
    source. The comparison is lexical, and under dune a recorded name is
    relative to the root of the project. A path must be given relative to that
    root, from which the command must run. Each source that has no data is named
    on standard error, in path order, after the report or the document.

    [--min PCT] requires the unrounded percentage of visited points to be at
    least [PCT]. A merge of no point counts as 100. The outcome line of the
    report states the gate and whether it is met
    ({!Windtrap.Private.Report_sections.coverage_line}). Coverage that prints
    equal to the threshold can thus fail, and the two counts on the line show
    why. *)

(** {1:formats Machine formats}

    [--json] and [--lcov] each make standard output the document, so the report
    is not printed and [-u] has no effect. Under either, and only under [--min],
    the text of the outcome line is said on standard error behind [windtrap:],
    whether the gate is met or not. The keys and the records below are frozen,
    so a column that is added to the report is not added to them.

    {b JSON.} The document is one object with two keys.
    - ["summary"] is an object with the keys ["visited"], ["total"] and
      ["percentage"], over all files.
    - ["files"] is an array with one object per source file, in the order of the
      recorded names under [String.compare]. Its keys are ["path"], the name as
      recorded at instrumentation, then ["visited"], ["total"], ["percentage"]
      and ["uncovered_lines"].

    A percentage is a number with two decimals, [100.00] for no point.
    ["uncovered_lines"] is the ascending array of the one-based lines that an
    unvisited point touches. It is empty for a file whose source the runtime
    does not find or judges stale ({!Windtrap_runtime.Coverage.file_reports}).

    {b LCOV.} The document is one record per source file, in the same order. A
    record is these lines, in this order:
    - [TN:], with no test name.
    - [SF:<file>], the name as recorded at instrumentation.
    - [DA:<line>,<hits>] for each line that a point touches, in line order.
      [<hits>] is the fewest visits among the points that touch the line, so it
      is [0] iff the line is uncovered.
    - [LF:<n>], the number of [DA] lines, and [LH:<n>], the number of those with
      a hit.
    - [end_of_record].

    A file whose source is not found or is judged stale has no record. It is
    named on standard error with the reason, and the exit code does not change.
*)

(** {1:running Running} *)

val run : string list -> int
(** [run args] executes the command on [args], the arguments that follow
    [coverage] on the command line, and is the exit code of the process. It
    never calls [exit].

    The flags are those of the help page. [-u] asks
    {!Windtrap.Private.Report_sections.coverage_report} for [`Full]. An argument
    that starts with [-] and is no flag is refused, and the rest are [PATH]s.
    [--color] is the runner's flag, read by {!Windtrap.Private.Cli.parse_color};
    without it the colour mode of the report is its mirror [WINDTRAP_COLOR],
    read by {!Windtrap.Private.Cli.color_mode}. The mode is resolved for
    standard output by {!Windtrap.Private.Os.resolve_color}, and a document is
    never styled.

    [run] parses the arguments, reads [WINDTRAP_COLOR] for the report when
    [--color] is absent, finds the {{!section-files}files}, judges, loads and
    merges them, prints the report or the document, and runs the two
    {{!section-gates}gates}, in that order. A step before the gates that fails
    returns its code, so standard output stays empty until the merge has
    succeeded. The report, the document and the help page print on standard
    output. Every other line goes to standard error, through
    {!Windtrap.Private.Os.say} but for the usage line that follows a usage
    error.

    The result is:
    - [0] when the report or the document was printed and every gate that was
      given is met. It is also [0] for [-h], [--help] and [-help], which print
      the help page and read nothing.
    - [1] when a [PATH] cannot be used, when no dump is found, when a dump that
      is not excluded cannot be read, is corrupt or has another format version,
      when two dumps disagree on the points of a file, or when every dump is
      excluded. It is also [1] when a gate fails: a path of [--expect] or
      [--do-not-expect] does not exist, a source under [--expect] has no data,
      or the coverage is below [--min].
    - [2] for a usage error. The usage errors are an unknown option, a [--min]
      that is not a number of the interval \[[0];[100]\], a [--color] value that
      the flag refuses, a flag that lacks its value, and [--json] given with
      [--lcov]. It is also [2] for a [WINDTRAP_COLOR] that the flag would
      refuse, when [--color] is absent and the report is printed. A machine
      format reads no [WINDTRAP_COLOR]. *)
