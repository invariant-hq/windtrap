# Windtrap examples

Each directory is a self-contained test executable wired into `dune runtest`,
recreating the walkthrough from the windtrap guide:

- `01-first-test` — the five-minutes example: `run`, `test`, `group`, `equal`, `raises`.
- `06-expect` — `let%expect_test` with `(inline_tests)` and `(pps ppx_windtrap)`; stale `[%expect]` payloads accepted with `dune promote`.
- `08-coverage` — expression-level coverage from one inert `(instrumentation (backend ppx_windtrap.coverage))` stanza: `dune runtest --instrument-with ppx_windtrap.coverage` prints the inline percentage, `dune exec windtrap -- coverage -u` shows the uncovered source, and `dune exec windtrap -- coverage --min 80` gates CI.
- `09-coverage-aggregation` — merging `.coverage` files from several test executables into one project-wide report.
- `x-blueprint` — the canonical project layout, ready to copy: a library with both instrumentation stanzas, `test/{unit,failures,expect,cram}` with one suite per file, the project verdict aliases, a live `xfail` backlog, and a dismissed equivalent mutant — the shape the windtrap skill teaches, as a buildable project.

Run them all with `dune runtest examples`, or one directly, e.g.
`dune exec examples/01-first-test/test_mylib.exe`.
