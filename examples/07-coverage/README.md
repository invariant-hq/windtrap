# Coverage

One inert `(instrumentation (backend ppx_windtrap.coverage))` stanza on
the library, three test executables over it, and two commands — the
instrumented run, then the merge — with `--min` making the merge a CI
gate (test runs themselves never fail on coverage):

    dune runtest --force --instrument-with ppx_windtrap.coverage
    dune exec windtrap -- coverage --min 80

`test_calc` is deliberately partial: the `Sub` and `Mul` arms of
`calc.ml` stay untested, so `dune exec windtrap -- coverage -u` has
uncovered source to show.

## Several test stanzas

`test_a` and `test_b` share the same library. Each executable's dump
is a *view*: it counts the points of the code linked into that binary.
The linker drops modules a binary never references — `test_a` carries no
trace of `Half_b` — and `test_b` links all of `Half_a` because it calls one
function from it, so the two views have different denominators and their
percentages never sum, average, or compare.

The project number is the merge of every executable's dump: the file set is
the union, a file in several dumps must carry identical point tables, counts
add per point, and the denominator is every instrumented point linked into
at least one test executable. The honest limit: code in libraries without
the stanza — and modules no test executable links at all — never registers,
so it is silently absent from the denominator, not reported as 0%;
`--expect lib/` turns that absence into a failure.

The two commands fold into one `@cover` alias for those who want one;
the chapter's "One command, if you want it" shows the rule, and
[`../x-blueprint/test/dune`](../x-blueprint/test/dune) carries it.
