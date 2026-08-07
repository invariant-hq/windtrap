# Windtrap examples

Each directory is a self-contained test executable wired into `dune runtest`,
recreating the walkthrough from the windtrap guide:

- `01-first-test` — the five-minutes example: `run`, `test`, `group`, `equal`, `raises`.
- `02-assertions` — the core assertion verbs, testable composition, `cases`, and `Testable.make`.
- `03-properties` — property tests over `Gen`: one `pp` feeds assertions and counterexamples; `~examples` pins regressions.
- `04-snapshots` — name-keyed snapshot baselines under `__snapshots__/`; accept changes with `WINDTRAP_UPDATE=1 dune runtest`.
- `05-resources` — `bracket` for per-test resources and `fixture` for run-scoped shared ones.
- `06-expect` — `let%expect_test` with `(inline_tests)` and `(pps ppx_windtrap)`; stale `[%expect]` payloads accepted with `dune promote`.
- `07-more-assertions` — `satisfies`/`contains`/`require_match`, the `Exn` predicates, `subtest` sub-cases, and `xfail` for known bugs.
- `08-coverage` — block coverage from one inert `(instrumentation (backend ppx_windtrap))` stanza: `dune runtest --instrument-with ppx_windtrap` prints the inline percentage, `WINDTRAP_COVERAGE=report` the per-file detail, and `dune exec windtrap -- coverage --min 80` gates CI.
- `09-coverage-aggregation` — merging `.coverage` files from several test executables into one project-wide report.

Run them all with `dune runtest examples`, or one directly, e.g.
`dune exec examples/01-first-test/test_mylib.exe`.
