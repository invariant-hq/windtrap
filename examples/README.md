# Examples

Each numbered directory is the project of one page of the
[manual](../doc/manual/), which shows its files and the output of runs
of them. `dune runtest` builds and runs every example, and each passes.

- `01-getting-started`: `Calc` and a suite of three tests, for
  [Getting started](../doc/manual/getting-started.md).
- `02-assertions`: `Shop`, a module of shopping carts, and one group per
  section of [Assertions](../doc/manual/assertions.md).
- `03-property-testing`: `Geo`, a module of shapes, and properties over
  a shape generator, for [Property testing](../doc/manual/property-testing.md).
- `04-stateful-testing`: `Bounded_queue` checked against a list, and a
  failing program kept as a test, for
  [Stateful testing](../doc/manual/stateful-testing.md).
- `05-baselines`: `expect` literals and an `expect_file` baseline in a
  stanza run with `--corrected`, and a library of `let%expect_test`, for
  [Baselines and expect tests](../doc/manual/baselines.md).
- `06-resources-and-structure`: a suite whose groups live in four files,
  with resources, cases, subtests, a skip and a known bug, and a library
  with inline tests, for
  [Resources and structure](../doc/manual/resources-and-structure.md)
  and [Running tests](../doc/manual/running-tests.md).
- `07-coverage`: a library with the coverage backend and three suites
  over it, for [Coverage](../doc/manual/coverage.md).
- `08-mutation`: a library with the mutation backend and a suite that
  leaves two mutants alive, for
  [Mutation testing](../doc/manual/mutation.md).

`x-blueprint` is a project of its own: a library with both
instrumentation backends, a binary, and a `test/` directory with a unit
suite per module, a known-bug suite, an expect-test library and cram
tests. Its [README](x-blueprint/README.md) describes it.

To run one example, pass its directory, as in
`dune runtest examples/02-assertions`. To pass flags, run its
executable: `dune exec examples/02-assertions/test_assertions.exe -- -v`.
