# Windtrap examples

Each directory is a self-contained project wired into `dune runtest`: a
walkthrough of the library in runnable form. The prose that goes with them
is the [manual](../doc/manual/); the numbering follows its chapters.

- `01-first-test` — the five-minutes example: `run`, `test`, `group`, `equal`, `raises`.
- `02-assertions` — the core assertion verbs, testable composition, `cases`, and `Testable.make`.
- `03-properties` — property tests over `Gen`: one `pp` feeds assertions and counterexamples; `~examples` pins regressions.
- `04-snapshots` — name-keyed snapshot baselines under `__snapshots__/`; accept changes with `WINDTRAP_UPDATE=1 dune runtest`.
- `05-resources` — `bracket` for per-test resources and `fixture` for run-scoped shared ones.
- `06-expect` — `let%expect_test` with `(inline_tests)` and `(pps ppx_windtrap)`; stale `[%expect]` payloads accepted with `dune promote`.
- `07-more-assertions` — `satisfies`/`contains`/`require_match`, the `Exn` predicates, `subtest` sub-cases, and `xfail` for known bugs.
- `08-coverage` — expression-level coverage from one inert `(instrumentation (backend ppx_windtrap.coverage))` stanza: `dune runtest --instrument-with ppx_windtrap.coverage` prints the inline percentage, `dune exec windtrap -- coverage -u` shows the uncovered source, and `dune exec windtrap -- coverage --min 80` gates CI.
- `09-coverage-aggregation` — merging `.coverage` files from several test executables into one project-wide report.
- `10-stateful` — `stateful` over a bounded queue: one `command` per operation, a list as the model, preconditions that both exclude illegal calls and select the interesting state, and an `~invariant` on the per-case system its `~scope` builds.
- `x-blueprint` — the canonical project layout, ready to copy: a library with both instrumentation stanzas, `test/{unit,failures,expect,cram}` with one suite per file, the project verdict aliases, a live `xfail` backlog, and a dismissed equivalent mutant — the shape the windtrap skill teaches, as a buildable project.

Run them all with `dune runtest examples`, or one directly, e.g.
`dune exec examples/01-first-test/test_mylib.exe`. Every example here
passes; the renderer's deliberately-failing validation harness lives in
[`test/render_demo/`](../test/render_demo/), which is not an example.
