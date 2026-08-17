# Windtrap

**One library for all your OCaml tests.**

Unit tests, property-based tests, snapshot tests, expect tests, code
coverage, and mutation testing — in a single package with one flat API. No
need to glue together Alcotest + QCheck + ppx_expect + Bisect_ppx + custom
snapshot code.

```ocaml
open Windtrap
open Calc

let () =
  run "mylib"
    [
      test "addition" (fun () -> equal int 5 (Calc.add 2 3));
      group "parser"
        [
          test "empty input" (fun () ->
              raises (Parse_error "empty") (fun () -> Calc.parse ""));
        ];
    ]
```

This is [`examples/01-first-test`](examples/01-first-test), verbatim apart
from the file's header comment. Running it prints:

```
mylib: 2 passed in 0.00317s.
```

A green, healthy run is exactly one line; failures bring out the
header, the per-test glyph row, and the full failure blocks.

## Install

```
opam install windtrap
```

For inline expect tests, code coverage, and mutation testing, also install
the PPX:

```
opam install ppx_windtrap
```

## dune setup

```lisp
(test
 (name test_mylib)
 (libraries windtrap))
```

For inline expect tests:

```lisp
(library
 (name mylib)
 (inline_tests)
 (preprocess
  (pps ppx_windtrap)))
```

For coverage and mutation testing, one inert stanza each on the library
under test:

```lisp
(library
 (name mylib)
 (instrumentation
  (backend ppx_windtrap.coverage))
 (instrumentation
  (backend ppx_windtrap.mutate)))
```

## Features

**Assertions** — Twenty-seven verbs: `equal`, `not_equal`, `is_true`, `is_false`,
`is_none`, `is_some`, `satisfies`, `greater`, `greater_equal`, `less`,
`less_equal`, `contains`, `not_contains`, `in_order`, `starts_with`, `ends_with`,
`mem`, `require_some`, `require_ok`, `require_error`, `require_match`, `raises`,
`raises_match`, `eventually`, `fail`, `failf`, `skip`. Comparisons go through an `'a testable` (a printer and an equality),
so every failure prints both values and marks what changed — for every type,
not just strings, and with no diff function to write. Values whose rendering
spans lines — including strings compared with the `text` witness — are diffed
line by line instead. The `require_*` verbs assert *and unwrap*, keeping the happy
path short.

**Property testing** — `prop` draws inputs from an `'a Gen.t`, runs an
ordinary assertion body on each, and shrinks failures to a minimal
counterexample; shrinking is integrated, there is never a shrink function to
write. `~examples` pins regressions, `~count` sets the case count, and
every failure prints an exact replay command with its `s1:` seed token.

**Stateful testing** — `stateful` checks a law over *sequences* of calls
against a model. A `command` bundles how to draw its argument, when it is
legal, what it does to the model and what it does to the real thing, and its
body asserts with the ordinary verbs — a result is produced and checked in
one expression, so there is no result type to declare and no `show_cmd` to
write. Failures print the shrunk program one numbered step per line, the
model each call was made in, and the step that broke.

**Snapshot testing** — `snapshot "name" value` compares against a committed
baseline under `__snapshots__/`. Checking is read-only: a mismatch or a
missing baseline fails with a diff and the acceptance command. Accept with
`-u` or `WINDTRAP_UPDATE=1`, review with `git diff`, and prune orphaned
baselines with `--prune`.

**Expect testing** — `let%expect_test` and `[%expect]` via `ppx_windtrap`,
with corrections accepted through `dune promote`. Compatibility with
ppx_expect is measured against Jane Street's own test corpus: supported
constructs promote byte-identically, unsupported ones fail loudly at the
exact location.

**Parameterized tests** — `cases` declares one test per input value; each
sub-test is named (derive names from values with `?name`) and individually
selectable.

**Resources** — `bracket` scopes a per-test resource with teardown on every
outcome; `fixture` shares an expensive resource across the run, released by
the runner; `temp_dir`/`temp_file` give runner-cleaned scratch paths, and
`setenv`/`chdir` bind the environment and the working directory for one test
with the runner restoring both. `subtest` names sub-cases inside a body and
`xfail` keeps known-bug reproductions in-tree without a red run.

**Code coverage** — expression-level coverage from the inert
`(instrumentation (backend ppx_windtrap.coverage))` stanza. Run
`dune runtest --instrument-with ppx_windtrap.coverage` for an inline percentage
after the results, `WINDTRAP_COVERAGE=report` for per-file detail, and
`dune exec windtrap -- coverage --min 80` (or `--json`) to gate CI.

**Mutation testing** — the second inert stanza,
`(instrumentation (backend ppx_windtrap.mutate))`, makes the test
executable its own mutation runner: `WINDTRAP_MUTATE=1` turns the run you
already make into a mutation run, which forks once per mutant and prints
every survivor as a failure block naming the line, the rewrite, and *the
tests that ran that line and did not fail when it changed*. Copy the
block's `arm` line to watch one mutant live through your green suite, and
dismiss an equivalent one in the source with `[@mutate off "reason"]`.
`dune exec windtrap -- mutate` merges the several test executables that
cover a library, because a mutant one suite kills and another merely
reaches is killed and an unmerged report would call it a survivor.

**Test runner** — filtering by name and tag, `--failed` reruns, `--shard
K/N` for CI partitioning, fail-fast, deterministic seeds, JUnit XML, and
automatic GitHub Actions annotations on failures.

## CLI

```
./test_mylib.exe [OPTIONS] [PATTERN]

  -f, --filter PATTERN     Run only tests whose path contains PATTERN
  -e, --exclude PATTERN    Skip tests whose path contains PATTERN
      --tag LABEL          Run only tests tagged LABEL (repeatable)
      --exclude-tag LABEL  Skip tests tagged LABEL (repeatable)
      --shard K/N          Run only the Kth of N deterministic path-hash buckets
      --quick              Skip slow-tagged tests
      --failed             Rerun only the last run's failures
  -l, --list               List selected tests without running them
  -x, --fail-fast          Stop after the first failure (same as --bail 1)
      --bail N             Stop after N failures
      --timeout SECONDS    Default per-test timeout in seconds
      --slow-threshold SECONDS
                           Warn when an untagged test runs longer than SECONDS (0 disables)
      --seed TOKEN         Root seed for property tests (s1:<16 hex>)
      --prop-count N       Generated cases per property
      --max-shrink N       Accepted shrink steps per failing property
      --max-prop-count N   Ceiling on every property's case count
      --max-discard N      Discarded cases tolerated per property (default 2x the count)
  -u, --update             Accept snapshot changes (refused under CI)
      --prune              Delete orphaned baselines after a full, clean update run
  -s, --stream             Stream test output instead of capturing it
  -v, --verbose            One status line per test
  -q, --quiet              Failures and summary only
      --junit PATH         Also write a JUnit XML report (PATH.xml, or a directory)
      --color MODE         Color output: always, never or auto
      --coverage MODE      Coverage output: summary, report, full or off
  -o, --output DIR         Root directory for capture logs
  -V, --version            Print the version and exit
  -h, --help               Print this help and exit
```

Every option that changes what a run does or reports has a `WINDTRAP_*`
environment mirror — under `dune runtest` the mirrors *are* the CLI
(e.g. `WINDTRAP_FILTER=parser dune runtest`, or
`WINDTRAP_JUNIT=_build/junit.xml dune runtest` in CI). Run with
`--help` for the full inventory.

## Documentation

- [`doc/manual/`](doc/manual/) — the manual: a guided tour of every
  feature.
- [`doc/cookbook.md`](doc/cookbook.md) — recipes for the things windtrap
  deliberately does not absorb.
- [`examples/`](examples/) — runnable projects covering every feature,
  wired into `dune runtest`.
- [`CHANGES.md`](CHANGES.md) — the 0.2.0 entry maps the windtrap 0.1.x
  surface to this one.

## License

ISC. Some files are under MIT or BSD-2-Clause due to derived code. See
[THIRD_PARTY_LICENSES.md](THIRD_PARTY_LICENSES.md) for details.

## Acknowledgments

Windtrap builds on ideas and code from several OCaml testing projects:

- **[Alcotest](https://github.com/mirage/alcotest)** by Thomas Gazagnaire —
  test structure and runner design
- **Craig Ferguson's Alcotest PRs**
  ([#294](https://github.com/mirage/alcotest/pull/294),
  [#247](https://github.com/mirage/alcotest/pull/247)) — API design and
  subcomponent diffing
- **[QCheck2](https://github.com/c-cube/qcheck)** by Simon Cruanes et al. —
  generator distributions and integrated shrinking
- **[ppx_expect](https://github.com/janestreet/ppx_expect)** and
  **[ppx_inline_test](https://github.com/janestreet/ppx_inline_test)** by
  Jane Street — the expect-test paradigm, dune integration, and the
  conformance corpus
- **[Bisect_ppx](https://github.com/aantron/bisect_ppx)** by Anton Bachin
  et al. — coverage instrumentation
- **[mtime](https://erratique.ch/software/mtime)** by The mtime
  programmers — monotonic clock implementation
