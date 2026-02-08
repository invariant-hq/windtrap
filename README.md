# Windtrap

**One library for all your OCaml tests.**

Unit tests, property-based tests, snapshot tests, and expect tests -- in a
single package with one API. No need to glue together Alcotest + QCheck +
ppx_expect + custom snapshot code.

```ocaml
open Windtrap

let () =
  run "My Project"
    [
      test "addition" (fun () ->
          equal Testable.int 5 (2 + 3));

      prop "reverse is involutive"
        Testable.(list int)
        (fun l -> List.rev (List.rev l) = l);

      test "json output" (fun () ->
          snapshot ~pos:__POS__ (to_json user));

      test "greeting" (fun () ->
          greet "World";
          expect "Hello, World!");
    ]
```

## Install

```
opam install windtrap
```

For inline expect tests (`let%expect_test`, `[%expect]`):

```
opam install ppx_windtrap
```

## dune setup

```lisp
(test
 (name my_tests)
 (libraries windtrap))
```

For PPX inline tests:

```lisp
(library
 (name mylib)
 (inline_tests)
 (preprocess (pps ppx_windtrap)))
```

## Features

**Assertions** -- `equal`, `not_equal`, `is_true`, `is_some`, `is_ok`, `raises`, `raises_match`, `no_raise`, with structured diffs on failure.

**Property testing** -- `prop` generates random inputs using `Testable` types and shrinks counterexamples automatically. Same types used for assertions and generation.

**Snapshot testing** -- `snapshot` compares output against reference files stored in `__snapshots__/`. Update with `-u` or `WINDTRAP_UPDATE=1`.

**Expect testing** -- `expect` captures stdout and compares with whitespace normalization. `capture` returns the function result while checking output.

**Inline expect tests** -- `let%expect_test` and `[%expect]` via `ppx_windtrap`. Mismatches produce `.corrected` files compatible with `dune promote`.

**Parameterized tests** -- `cases` generates one test per input value from a list.

**Fixtures** -- `bracket` for per-test setup/teardown, `fixture` for lazy shared resources, `group ~setup ~teardown` for suite-level hooks.

**Test runner** -- Filtering (`-f`), fail-fast (`-x`), output formats (verbose, compact, TAP, JUnit), snapshot updates (`-u`), GitHub Actions annotations.

## CLI

```
./my_tests.exe [OPTIONS] [PATTERN]

  -f, --filter PATTERN   Filter tests by name
  -e, --exclude PATTERN  Exclude tests by name
  -x, --fail-fast        Stop on first failure
  --bail N               Stop after N failures
  --failed               Rerun only tests that failed last time
  -q, --quick            Skip slow tests
  -u, --update           Update snapshot files
  -l, --list             List tests without running
  -s, --stream           Stream output (don't capture)
  -v, --verbose          Verbose output (default: compact)
  --format FMT           compact, verbose, tap, junit
  --junit PATH           Write JUnit XML report
  --tag LABEL            Run only tests with this label (repeatable)
  --exclude-tag LABEL    Skip tests with this label (repeatable)
  --seed N               Reproducible property test seed
  --timeout N            Default timeout in seconds
  --prop-count N         Number of property test cases (default: 100)
  --color MODE           Color output: always, never, auto
  -V, --version          Show version
```

## Examples

See [`examples/`](examples/) for working code covering every feature.

## License

ISC. Some files are under MIT or BSD-2-Clause due to derived code. See
[THIRD_PARTY_LICENSES.md](THIRD_PARTY_LICENSES.md) for details.

## Acknowledgments

Windtrap builds on ideas from several OCaml testing projects:

- **[Alcotest](https://github.com/mirage/alcotest)** by Thomas Gazagnaire -- test structure and runner design
- **Craig Ferguson's Alcotest PRs** ([#294](https://github.com/mirage/alcotest/pull/294), [#247](https://github.com/mirage/alcotest/pull/247)) -- API design and subcomponent diffing
- **[ppx_expect](https://github.com/janestreet/ppx_expect)** and **[ppx_inline_test](https://github.com/janestreet/ppx_inline_test)** by Jane Street -- expect test paradigm and dune integration
- **[QCheck2](https://github.com/c-cube/qcheck)** by Simon Cruanes et al. -- generator design and integrated shrinking
- **[mtime](https://erratique.ch/software/mtime)** by The mtime programmers -- monotonic clock implementation
