(*---------------------------------------------------------------------------
   Copyright (c) 2026 Invariant Systems. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** The JUnit renderer: run results as a JUnit XML report.

    [Render_junit] projects a run's results into one XML 1.0 document for
    [--junit PATH]: a [testsuites] root wrapping one [testsuite], with one
    [testcase] per result in execution order carrying its time; failures as
    [failure] elements whose text is the unstyled {!Render.pp_failure} block,
    replay and acceptance commands included; skips as [skipped] elements; a
    failing test's captured tail as [system-out].

    Two run features have no native JUnit form and map as follows:

    - {b Expected failures.} JUnit knows no xfail state, so an {e excused}
      failure — a failing result that did not count ({!Run.result.counted}
      false) — becomes a [skipped] testcase whose message names the expectation
      from {!Run.result.xfail} ([expected failure: issue #42]); its real
      failures are not emitted, matching the run they did not fail. An [xfail]
      test that {e passed} arrives as an ordinary counted failure whose message
      names the reason and needs no mapping.
    - {b Subtests.} Each subtest failure entry ({!Render.is_subtest_failure})
      becomes its {e own} [testcase] — named by the entry's [msg] slot, which
      carries the [parent › name] label (followed by [: <msg>] when the failing
      assertion also passed a user [?msg] — the label rides the slot and cannot
      be split back out) — under the parent's [classname], with time [0.000]
      (sub-cases are not timed) — emitted directly after the parent's testcase.
      The parent testcase keeps the test's non-subtest failures and its captured
      tail; with only subtest failures it carries no [failure] element of its
      own.

    The renderer owns its transport's validity: every emitted field — names,
    messages, failure text, captured output — is ANSI-stripped
    ({!Text.strip_ansi}), reduced to the XML 1.0 character range (bytes outside
    it, malformed UTF-8 included, become U+FFFD), and XML-escaped. No payload
    can make the document malformed.

    Rendering is pure and deterministic — no clocks, hostnames, or environment;
    equal inputs give equal documents — so writing the file is the caller's (the
    runner writes {!render}'s result to the configured path). *)

val render :
  ?invocation:Render.invocation ->
  suite:string ->
  results:Run.result list ->
  duration:float ->
  unit ->
  string
(** [render ~suite ~results ~duration ()] is the complete XML document —
    [<?xml ?>] declaration, final newline included — for [results], in list
    order.

    [suite] names the [testsuite] and prefixes every [classname]
    ([<suite>.<groups dot-joined>], [<suite>] for ungrouped tests); a
    [testcase]'s [name] is its full test path ({!Test_tree.path_to_string}).
    Counts ([tests], [failures], [skipped]) range over the {e emitted}
    testcases: each subtest failure adds one to [tests] and [failures], a
    counted failing result adds one to [failures] iff it has non-subtest
    entries, and excused failures — classified from the record, see the preamble
    — count as [skipped]; [errors] is always [0] — windtrap classifies every
    failure kind as a JUnit failure. [duration] is the run's wall-clock seconds,
    emitted as the suite [time]; per-case times come from each result.

    [invocation], default [`Mirrors], is the hint context ({!Render.invocation})
    the embedded replay and acceptance lines are spelled from — pass the run's
    real invocation so failure bodies match the terminal block bytes. *)
