# Coverage

The example of the manual's [coverage page](../../doc/manual/coverage.md):
a library instrumented with `ppx_windtrap.coverage` and three suites over
it.

    dune runtest --force --instrument-with ppx_windtrap.coverage
    dune exec windtrap -- coverage -u

Each suite's dump counts the modules its executable links. `test_a` never
references `Half_b`, which is absent from its dump, and `test_b` calls
`Half_a.greet`, which brings every point of `Half_a` into its dump. The
percentages of two suites never add up; the project's number is the merge
of every dump.

`instrumented/` builds the same suites with the backend applied, for the
page's transcripts.
