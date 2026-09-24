(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The JUnit report: the result rows of a run as one JUnit XML document, and
    the file that it is written to.

    {!write} is the entry point, which {!Report.run} calls under [--junit] only.
    Nothing of the document reaches the terminal. *)

(** {1:document The document}

    The document is one XML 1.0 document, declared as UTF-8. Its root is a
    [testsuites] element named [windtrap], which holds one [testsuite] named
    after the suite. Both carry the same [tests], [failures], [errors],
    [skipped] and [time] attributes. Every [time] is in seconds with three
    decimals, and that of the suite is the duration of the run.

    {b Testcases.} Each {!Run.type-result} is one [testcase], in the order of
    the rows. Its [name] is the full path of the row
    ({!Test_tree.path_to_string}). Its [classname] is the suite's name followed
    by the names of the test's groups, all joined by [.], and a dot inside a
    name is not escaped. Its [time] is the row's [duration].
    - A pass is an empty [testcase]. A pass on a retry, which is a [Pass] row
      with [attempts > 1], holds a [system-out] whose text is
      [passed on attempt N].
    - A skip holds a [skipped] element, whose [message] is the reason of the
      skip when it gave one.
    - A counted failure holds one [failure] element for each of the test's own
      failures. The [message] of the element is {!Report_sections.headline}. Its
      text is the entry of {!Report_sections.pp_failure} under [ansi:false] and
      without excerpt. The entry ends with its hint lines
      ({!Report_sections.hints}), whose filter is the full path of the test.
    - A [system-out] follows these elements when a failure of the test, that of
      a subtest included, has a captured tail. It holds the whole text of the
      first such {!Failure.type-tail}. A line before the text counts the earlier
      bytes that the capture dropped, and a line after it names the full log,
      each when there is one.

    {b Subtests.} Each failure recorded in a subtest
    ({!Report_sections.is_subtest_failure}) is a [testcase] of its own, which
    comes right after that of its test and has the same [classname]. Its [name]
    is {!Report_sections.labeled_msg}, it holds the one [failure], and its
    [time] is [0.000] because a subtest is not timed. The testcase of the test
    keeps the other failures and the captured tail, and it holds no [failure]
    when every failure is a subtest's.

    {b Fixture releases.} After the testcases of the rows, each failed release
    is one [testcase] named {!Report_sections.release_title}, with the suite as
    its [classname], [time="0.000"] because a release is not timed, and its one
    [failure], written as that of a test.

    {b Expected failures.} A [Fail] row whose [counted] is [false] is a
    [testcase] that holds a [skipped] element, whose [message] is
    [expected failure: <reason>], or [expected failure] when [xfail] gives no
    reason. Its failures are not written.

    {b Counts.} [tests], [failures] and [skipped] count the testcases of the
    document and not the rows. Each subtest failure adds one to [tests] and one
    to [failures]. A counted failing row adds one to [failures] iff the test has
    a failure of its own, however many [failure] elements that makes. A failed
    release adds one to [tests] and one to [failures]. A skip and an expected
    failure each add one to [skipped]. [errors] is always [0], and no [error]
    element is ever written, because every kind of failure is a JUnit failure.

    {b Validity.} Every string that the rows supply first goes through
    {!Text.escape_controls}, element text line by line and an attribute value
    whole, so a control byte other than TAB, and LF in text, prints as [\xNN].
    It is then reduced to the [Char] range of XML 1.0, in which any other code
    point and each malformed UTF-8 sequence becomes U+FFFD. It is escaped last,
    as element text or as an attribute value. No payload can therefore make the
    document malformed.

    {b Determinism.} The document holds no clock, no host name and no timestamp.
    Its paths are printed against {!Os.project_root}, which reads the
    environment and the working directory, so equal rows under an equal
    environment give equal documents. *)

(** {1:writing Writing} *)

val write :
  invocation:Run.invocation ->
  ?armed:string ->
  suite:string ->
  duration:float ->
  results:Run.result list ->
  release_failures:Failure.t list ->
  string ->
  unit
(** [write ~invocation ?armed ~suite ~duration ~results ~release_failures
     target] writes the {{!section-document}document} of [results] and
    [release_failures] to the file that [target], the value of [--junit], names
    for [suite].
    - A [target] that ends in [.xml] is that file, as given. The test is that of
      [Filename.check_suffix], on the name alone. Every suite that reads the
      same value writes the same file, and the last one wins.
    - Any other [target] is a directory, and the file is [<target>/<name>.xml],
      where [name] is {!Os.sanitize_component}[ suite]. Two suites that share
      the directory get two files, as far as {!Os.sanitize_component} tells
      their names apart. [write] creates the directory and its parents
      ({!Os.mkdir_p}), and it never creates the parent of an [.xml] target.

    [invocation] and [armed], which is the identifier of an armed mutant, spell
    the hint lines of each failure. The caller must pass those of the run, so
    that the text of a [failure] is the lines of the block on the terminal.

    {!Os.atomic_write} writes the file, so an existing report is replaced whole.
    A report that cannot be written is one warning on standard error
    ({!Os.warn}), at every verbosity, and never a failed run. It reads
    [could not write JUnit report to <file>: <reason>], the reason being
    {!Os.failure_reason}'s. [write] catches the [Sys_error] and the
    [Unix.Unix_error] of these two functions for it. *)

(**/**)

(* The two halves of [write], exported for the unit suite. [render ?invocation
   ?armed ~suite ~results ~release_failures ~duration ()] is the document that [write] writes, as
   a string, from its XML declaration to a final newline. [invocation] defaults
   to [`Mirrors]. It opens no file and writes nothing. [path ~suite target] is
   the file that [write] writes to for [target], and it reads no file system. *)

val render :
  ?invocation:Run.invocation ->
  ?armed:string ->
  suite:string ->
  results:Run.result list ->
  release_failures:Failure.t list ->
  duration:float ->
  unit ->
  string

val path : suite:string -> string -> string

(**/**)
