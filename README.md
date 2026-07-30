# Windtrap

**One library for all your OCaml tests.**

Unit tests, property-based tests, snapshot tests, expect tests, and code
coverage — in a single package with one flat API. No need to glue together
Alcotest + QCheck + ppx_expect + Bisect_ppx + custom snapshot code.

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

For inline expect tests and code coverage, also install the PPX:

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

For coverage, one inert stanza on the library under test:

```lisp
(library
 (name mylib)
 (instrumentation
  (backend ppx_windtrap)))
```

## Features

**Assertions** — sixteen verbs: `equal`, `not_equal`, `is_true`, `is_false`,
`satisfies`, `contains`, `not_contains`, `require_some`, `require_ok`,
`require_error`, `require_match`, `raises`, `raises_match`, `fail`, `failf`,
`skip`. Comparisons go through an `'a testable` (a printer and an equality),
so every failure prints a structured diff of the two values — for every
type, not just strings. The `require_*` verbs assert *and unwrap*, keeping
the happy path short.

**Property testing** — `prop` draws inputs from an `'a Gen.t`, runs an
ordinary assertion body on each, and shrinks failures to a minimal
counterexample; shrinking is integrated, there is never a shrink function to
write. `~examples` pins regressions, `~count` sets the case count, and
every failure prints an exact replay command with its `s1:` seed token.

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
the runner; `temp_dir`/`temp_file` give runner-cleaned scratch paths.
`subtest` names sub-cases inside a body and `xfail` keeps known-bug
reproductions in-tree without a red run.

**Code coverage** — expression-level coverage from the inert
`(instrumentation (backend ppx_windtrap))` stanza. Run
`dune runtest --instrument-with ppx_windtrap` for an inline percentage
after the results, `WINDTRAP_COVERAGE=report` for per-file detail, and
`dune exec windtrap -- coverage --min 80` (or `--json`) to gate CI.

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
  -u, --update             Accept snapshot changes (refused under CI)
      --prune              Delete orphaned baselines after a full, clean update run
  -s, --stream             Stream test output instead of capturing it
  -v, --verbose            One status line per test
  -q, --quiet              Failures and summary only
      --junit PATH         Also write a JUnit XML report to PATH
      --color MODE         Color output: always, never or auto
      --coverage MODE      Coverage output: summary, report, full or off
  -o, --output DIR         Root directory for capture logs
  -V, --version            Print the version and exit
  -h, --help               Print this help and exit
```

Selection, seed, snapshot, and output options have `WINDTRAP_*`
environment mirrors — under `dune runtest` the mirrors *are* the CLI
(e.g. `WINDTRAP_FILTER=parser dune runtest`). Run with `--help` for the
full inventory.

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
