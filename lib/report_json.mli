(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The JSON report: the result rows of a run, each with the scope of its
    verdict, as one JSON document, and the file that it is written to.

    {!write} is the entry point, which {!Report.run} calls under [--json] only.
    Nothing of the document reaches the terminal. *)

(** {1:document The document}

    The document is one JSON object, encoded in UTF-8, with each test on a line
    of its own and a newline at its end:

    {v
    {"version": 1, "suite": "mylib", "seed": "s1:7be1d2c904aa31f5", "tests": [
      TEST,
      TEST
    ], "counts": {"tests": 3, "passed": 1, "failed": 1, "vacuous": 0,
       "skipped": 0, "expected_failures": 0, "flaky": 0, "not_run": 1,
       "release_failures": 0, "duration": 0.0042}}
    v}

    [version] is [1], and changes when a field changes meaning or goes. A field
    added is no change of version, so a reader ignores the fields it does not
    know. [seed] is the run's root seed. [tests] holds one object per
    {!Run.type-result}, in the order of the rows, and [counts] closes the
    document with the terms of the summary line, which agree with the exit code:
    [failed] counts the failed fixture releases too, and [not_run] the selected
    tests that have no row. [duration] is in seconds.

    {b A test.}
    {v
    {"path": ["group", "name"], "verdict": "pass", "attempts": 1,
     "duration": 0.0012, "failures": [ FAILURE, ... ], "scope": SCOPE}
    v}
    - [verdict] is ["pass"], ["fail"], ["vacuous"], ["skip"] or ["xfail"]: a
      counted failure whose every failure is {!Failure.Vacuous} is ["vacuous"],
      any other counted failure ["fail"], and a failure that the test's [xfail]
      excuses ["xfail"].
    - [reason] follows [verdict] for a skip that gave one, and for a vacuous
      test, whose reason is the text of its vacuous failures, joined by [; ].
    - [failures] holds the failures of a counted or excused failure, each
      [{"headline": ..., "file": ..., "line": ...}], with
      {!Report_sections.headline} and the failure's location, [file] and [line]
      being absent without one. It is [[]] for a pass and a skip.

    {b The scope} states what the test exercised. Every test's ends on
    [tolerances], one
    [{"file": ..., "line": ..., "comparisons": ..., "error": ..., "tolerance":
     ...}] per site of [equal] that compared under a tolerance, ordered by site,
    with the error of the comparison that came closest to its bound and that
    bound ({!Check.tolerances}); [file] and [line] are absent where the site has
    no location. A plain test's scope is so [{"tolerances": []}]. The scope of a
    property, a table or a stateful test holds before it:
    - [cases], the generated cases that passed, or the values of a domain
      checked whole, and [examples], the examples that passed;
    - [count], the cases that the property demanded, after [--prop-count];
    - [discards], the cases discarded, the examples included;
    - [replayed], for a run of replayed cases alone, under [--replay],
      [--failed] or [--case], the cases among [cases] that it replayed;
    - [values], for a domain checked whole, its number of values, which a table
      calls [rows];
    - [skipped], for a table, the rows that skipped, which [cases] leaves out;
    - [labels], one [{"label": ..., "cases": ...}] per label of
      {!Run.prop_context}, with the passing cases that marked it, sorted by
      label;
    - [cover], one [{"label": ..., "cases": ..., "met": ...}] per demand of a
      {!Property.cover}, sorted by label, when the demands were judged, and [[]]
      when a case failed or the property gave up before they were. A share
      demand adds
      [{"share": {"demand": ..., "hits": ..., "cases": ..., "low": ..., "high":
       ...}}]: the cases it was judged over, those that marked it, and the
      bounds that the last look established ({!Property.type-share}), [low] and
      [high] equal where the share is exact.

    {b Encoding.} A string escapes its quotes, its backslashes and its control
    bytes, and each malformed UTF-8 sequence becomes U+FFFD, so the document is
    always valid JSON. A number is a JSON number, a float the shortest decimal
    that reads back to it, and a float that is not finite [null].

    {b Determinism.} The document holds no clock but the durations, no host name
    and no timestamp. Its paths are printed against {!Os.project_root}. *)

val render :
  suite:string ->
  seed:Seed.seed ->
  selected:int ->
  duration:float ->
  results:Run.result list ->
  release_failures:Failure.t list ->
  unit ->
  string
(** [render ~suite ~seed ~selected ~duration ~results ~release_failures ()] is
    the {{!section-document}document} of a run of [suite] under [seed] that
    selected [selected] tests and gave [results] and [release_failures] in
    [duration] seconds. *)

(** {1:writing Writing} *)

val write :
  suite:string ->
  seed:Seed.seed ->
  selected:int ->
  duration:float ->
  results:Run.result list ->
  release_failures:Failure.t list ->
  string ->
  unit
(** [write ~suite ~seed ~selected ~duration ~results ~release_failures target]
    writes the {{!section-document}document} to the file that [target], the
    value of [--json], names for [suite], as {!Report_junit.write} does with
    [.json] for [.xml]: a [target] that ends in [.json] is that file, and any
    other is a directory that gets [<target>/<name>.json]. A report that cannot
    be written is one warning on standard error, and never a failed run. *)
