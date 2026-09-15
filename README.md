# Windtrap

**One library for all your OCaml tests.**

Unit tests, property-based tests, stateful tests, snapshot tests, expect
tests, code coverage, and mutation testing — in a single package with one
flat API. No need to glue together Alcotest + QCheck + ppx_expect +
Bisect_ppx + custom snapshot code.

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

Each links the chapter that documents it.

**[Assertions](doc/manual/assertions.md)** — every comparison goes
through an `'a testable`, a printer plus an equality, so a failure
prints both values and marks what changed for every type, with no diff
function to write; values whose rendering spans lines are diffed line by
line, and the `require_*` verbs assert *and unwrap*, keeping the happy
path short.

**[Property testing](doc/manual/property-testing.md)** — `prop` draws
inputs from an `'a Gen.t`, runs an ordinary assertion body on each, and
shrinks failures to a minimal counterexample; shrinking is integrated,
so there is never a shrink function to write, and every failure prints
an exact replay command with its `s1:` seed token.

**[Stateful testing](doc/manual/stateful-testing.md)** — `stateful`
checks a law over *sequences* of calls against a model, with each
`command` bundling how to draw its argument, when it is legal, what it
does to the model and what it does to the real thing; failures print the
shrunk program one numbered step per line, the model each call was made
in, and the step that broke.

**[Snapshot testing](doc/manual/snapshots-and-expect.md)** — `snapshot
"name" value` compares against a committed baseline under
`__snapshots__/`, and checking is read-only: a mismatch or a missing
baseline fails with a diff and the acceptance command, which you accept
with `-u` and review with `git diff`.

**[Expect testing](doc/manual/snapshots-and-expect.md)** —
`let%expect_test` and `[%expect]` via `ppx_windtrap`, with corrections
accepted through `dune promote`. Compatibility with ppx_expect is
measured against Jane Street's own test corpus: supported constructs run
and promote unchanged, unsupported ones fail loudly at the exact
location.

**[Resources and structure](doc/manual/resources-and-structure.md)** —
`bracket` scopes a per-test resource with teardown on every outcome and
`scoped` takes a `with_`-style scoping function whole, `fixture` shares
an expensive one across the run, `temp_dir` and `temp_file` give
runner-cleaned scratch paths, and `setenv`/`chdir` bind the environment
and the working directory for one test with the runner restoring both;
`cases` declares one named, individually selectable test per input,
`subtest` labels sub-cases inside a body, and `xfail` keeps known-bug
reproductions in-tree without a red run.

**[Code coverage](doc/manual/coverage.md)** — expression-level coverage
from the inert `(instrumentation (backend ppx_windtrap.coverage))`
stanza: `dune runtest --instrument-with ppx_windtrap.coverage` prints an
inline percentage after the results, `dune exec windtrap -- coverage`
draws the per-file table (`-u` for the uncovered source, `--json` for
the machine-readable form, `--lcov` for coverage services and genhtml),
and `--min 80` gates CI.

**[Mutation testing](doc/manual/mutation.md)** — the second inert
stanza, `(instrumentation (backend ppx_windtrap.mutate))`, makes the
test executable its own mutation runner: `WINDTRAP_MUTATE=1` turns the
run you already make into a mutation run, which re-runs the tests once
per mutant they reach and prints every survivor as a failure block
naming the line, the rewrite, and *the tests that ran that line and did
not fail when it changed*. Scope it to the file you are working on with
`WINDTRAP_MUTATE_ONLY=lib/foo.ml`, filter to the test you just wrote
with `-f`, and dismiss an equivalent mutant in the source with
`[@mutate off "reason"]`. The project answer is one command,
`WINDTRAP_MUTATE=1 dune build @mutate --force --instrument-with
ppx_windtrap.mutate`, which runs every suite mutated and merges them
under killed-anywhere-wins — a mutant one suite kills and another
merely reaches is killed — and exits 1 on any survivor.

**[Test runner](doc/manual/running-tests.md)** — filtering by name and
tag, `--failed` reruns, `--shard K/N` for CI partitioning, fail-fast,
deterministic seeds, JUnit XML, and automatic GitHub Actions annotations
on failures.

## CLI

`./test_mylib.exe --help` prints the full inventory of flags — it is
generated from the parser, so it never drifts. The part that is not
obvious: every option that changes what a run does or reports has a
`WINDTRAP_*` environment mirror, because under `dune runtest` there is
no command line and the mirrors *are* the CLI:

```
WINDTRAP_FILTER=parser dune runtest
WINDTRAP_JUNIT=_build/junit dune runtest        # in CI
```

`-l`, `--failed`, `-x`, `-h` and `-V` have no mirror: they want a
command line. A handful of variables have no flag either — the
mutation and coverage switches among them — and `--help` lists those
too.

## Documentation

- [`doc/manual/`](doc/manual/) — the manual: a guided tour of every
  feature.
- [`doc/cookbook.md`](doc/cookbook.md) — recipes for the things windtrap
  deliberately does not absorb.
- [`examples/`](examples/) — self-contained projects, wired into `dune
  runtest`: a numbered walkthrough from the first test to stateful
  testing and coverage, plus `x-blueprint`, the canonical layout ready
  to copy.
- [`CHANGES.md`](CHANGES.md) — the 0.2.0 entry maps the windtrap 0.1.x
  surface to this one.

## License

ISC. Some files carry additional ISC or MIT notices for derived code. See
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
