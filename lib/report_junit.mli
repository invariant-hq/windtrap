(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The JUnit report: run results as one JUnit XML document, and where it is
    written.

    A [testsuites] root wraps one [testsuite] with one [testcase] per result in
    execution order carrying its time; failures are [failure] elements whose
    text is the unstyled {!Report_sections.pp_failure} block; skips are
    [skipped] elements; a failing test's captured tail is its [system-out]; a
    pass on a retry is a [testcase] whose [system-out] says
    [passed on attempt N]. An excused expected failure ({!Run.result.counted}
    false) becomes a [skipped] testcase whose message names the expectation from
    {!Run.result.xfail}; its failures are not emitted. An [xfail] test that
    passed arrives as an ordinary counted failure whose message names the reason
    and needs no mapping. Each subtest failure entry
    ({!Report_sections.is_subtest_failure}) becomes its own [testcase] named by
    the entry's [msg] slot, under the parent's [classname], with time [0.000],
    directly after the parent's testcase; the parent keeps its non-subtest
    failures and its captured tail, and with only subtest failures carries no
    [failure] element of its own.

    Every emitted field is ANSI-stripped ({!Text.strip_ansi}), reduced to the
    XML 1.0 character range (other bytes, malformed UTF-8 included, become
    U+FFFD) and XML-escaped, so no payload can make the document malformed.
    Rendering is pure and deterministic. *)

(** {1:rendering Rendering} *)

val render :
  ?invocation:Run.invocation ->
  ?armed:string ->
  suite:string ->
  results:Run.result list ->
  duration:float ->
  unit ->
  string
(** [render ~suite ~results ~duration ()] is the complete XML document
    ([<?xml ?>] declaration, final newline) for [results] in list order. [suite]
    names the [testsuite] and prefixes every [classname]
    ([<suite>.<groups dot-joined>], [<suite>] for ungrouped tests); a
    [testcase]'s [name] is its full path ({!Test_tree.path_to_string}). [tests],
    [failures] and [skipped] count the emitted testcases (a counted failing
    result adds one to [failures] iff it has non-subtest entries; excused
    failures count as [skipped]); [errors] is always [0]. [duration] is the
    suite [time] in seconds. [invocation] (default [`Mirrors]) and [armed], the
    armed mutant's identifier, spell each failure's hint lines
    ({!Report_sections.hints}). *)

(** {1:writing Writing} *)

val path : suite:string -> string -> string
(** [path ~suite target] is the file [suite]'s report is written to for the
    [--junit] value [target]: a [target] naming an [.xml] file is that file;
    anything else is a directory and the report lands at [<target>/<suite>.xml],
    [suite] made filename-safe ({!Os.sanitize_component}). *)

val write :
  invocation:Run.invocation ->
  ?armed:string ->
  suite:string ->
  duration:float ->
  results:Run.result list ->
  string ->
  unit
(** [write ~invocation ?armed ~suite ~duration ~results target] writes
    {!render}'s document to {!path}[ ~suite target], creating the directory when
    [target] is one. A report that cannot be written is a warning on standard
    error, never a failed run. *)
