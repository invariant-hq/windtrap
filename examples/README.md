# Windtrap examples

Each directory is a self-contained project wired into `dune runtest`: the
[manual](../doc/manual/) in runnable form, one example per chapter,
numbered in the manual's order (the running-tests chapter has none — its
subject is the command line every example answers to).

- `01-getting-started` — the five-minutes example: `run`, `test`, `group`, `equal`, `raises`.
- `02-assertions` — the assertion vocabulary: equality through witnesses and `Testable.make`, the ordering verbs, `require_*`, `satisfies`, `contains`, `in_order`, the `Exn` predicates, `fail`/`skip`, and table-driven `cases`.
- `03-property-testing` — `prop` over `Gen`: one `pp` feeds assertions and counterexamples, `~examples` pins regressions, `assume` discards, `cover`/`classify` watch the distribution.
- `04-stateful-testing` — `stateful` over a bounded queue: one `command` per operation, a list as the model, preconditions that both exclude illegal calls and select the interesting state, and an `~invariant` on the per-case system its `~scope` builds.
- `05-baselines` — an `expect` literal at the call and an `expect_file` against a committed `help.expected`, in a `(test)` stanza whose `--corrected` run lets `dune promote` accept a change; beside it, `let%expect_test` in an `(inline_tests)` library with `(pps ppx_windtrap)`, accepted the same way, and a shadowed `Expect_test_config` that masks output.
- `06-resources-and-structure` — `bracket`, `scoped` and `fixture` (one that skips), `temp_dir`, `setenv` and `chdir`, a group's `~timeout` and `~retries` as defaults, a `slow` test, `subtest`, `current_test`, and `xfail` for a known bug.
- `07-coverage` — one inert `(instrumentation (backend ppx_windtrap.coverage))` stanza and three test executables over it: `dune runtest --force --instrument-with ppx_windtrap.coverage` writes the dumps, `dune exec windtrap -- coverage` merges them (`-u` shows the uncovered source, `--min 80` gates CI); its README explains why per-executable views never add up.
- `08-mutation` — the `(instrumentation (backend ppx_windtrap.mutate))` stanza and the survey: `--mutate=<file> -f <test>` shows a deliberately weak test's survivor, the unfiltered run kills everything, `--arm <id>` reproduces one, and `[@mutate off "reason"]` dismisses an equivalent mutant in the source.
- `x-blueprint` — the canonical project layout, ready to copy: a library with both instrumentation stanzas, `test/{unit,failures,expect,cram}` with one suite per file, the `@cover` and `@mutate` verdict aliases, a live `xfail` backlog, and a dismissed equivalent mutant — the shape the windtrap skill (`skills/windtrap-testing/SKILL.md`) teaches, as a buildable project.

Run them all with `dune runtest examples`, or one directly, e.g.
`dune exec examples/01-getting-started/test_mylib.exe`. Every example
here passes.
