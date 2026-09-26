# Coverage

The example of the manual's [coverage page](../../doc/manual/coverage.md):
a library instrumented with `ppx_windtrap.coverage` and three suites over
it. The page's commands run at the root of a copy of this directory made
a project of its own, as the
[blueprint](../x-blueprint/README.md#copying-the-project-out) shows. In
windtrap's repository, `--instrument-with` also instruments windtrap's
own library, and the report lists its files.

    dune runtest --instrument-with ppx_windtrap.coverage
    dune exec windtrap -- coverage -u

Each suite's dump counts the modules its executable links. `test_a` never
references `Half_b`, which is absent from its dump, and `test_b` calls
`Half_a.greet`, which brings every point of `Half_a` into its dump. The
percentages of two suites never add up; the project's number is the merge
of every dump.
