# Windtrap — One library for all your OCaml tests

Windtrap runs unit, property, stateful and expect tests from one flat
interface, with coverage and mutation testing in the companion ppx. A
suite is an ordinary executable that `dune runtest` builds and runs.
Windtrap needs OCaml 5.0 or later, the ppx adds ppxlib, and both are
distributed under the ISC license.

## A first suite

A suite is declared by a `(test)` stanza, here for a module `Calc` of
two functions (the tutorial's example, `examples/01-getting-started/`).

`test/dune`:

<!-- file examples/01-getting-started/dune -->
```lisp
(test
 (name test_mylib)
 (modules test_mylib calc)
 (libraries windtrap))
```

`test/test_mylib.ml`:

<!-- file examples/01-getting-started/test_mylib.ml -->
```ocaml
open Windtrap

let add =
  group "add"
    [ test "adds two integers" (fun () -> equal int 5 (Calc.add 2 3)) ]

let parse =
  group "parse"
    [
      test "rejects the empty string" (fun () ->
          raises (Calc.Parse_error "empty") (fun () -> Calc.parse ""));
    ]

let () = exit (run "mylib" [ add; parse ])
```

A run with nothing to report prints one line:

<!-- run examples/01-getting-started -->
```
$ dune runtest
mylib: 2 passed in 0.5ms.
```

## What it does

- A failing assertion prints both values it compared, and a diff for text.
- Every generator shrinks, and a failing property prints its smallest
  counterexample and a `replay:` command.
- `stateful` checks a system against a model over generated programs of
  calls.
- A baseline is the literal at an `expect` call or the file an
  `expect_file` call names; `dune promote` accepts a change to it, and to
  a `let%expect_test`, which runs on the same runner.
- `windtrap coverage` merges the coverage of every suite into one
  report, and each mutant that survives the tests names the tests that
  ran its line.
- `run` returns `0`, `1` or `2`, and `2` means that no test ran, so a
  mistyped `-f` fails the command.

## Installation

    opam install windtrap
    opam install ppx_windtrap   # expect tests, coverage, mutation testing

## Documentation

The manual, [`doc/manual/`](doc/manual/), has one page per need:

- Tutorial: [Getting started](doc/manual/getting-started.md), the suite above and its first failure.
- How-to:
  - [Assertions](doc/manual/assertions.md): values, bounds, strings, results and exceptions.
  - [Property testing](doc/manual/property-testing.md): laws over generated values.
  - [Stateful testing](doc/manual/stateful-testing.md): a system against a model.
  - [Baselines and expect tests](doc/manual/baselines.md): `expect`, `expect_file`, `let%expect_test`.
  - [Resources and structure](doc/manual/resources-and-structure.md): a suite's layout and resources.
  - [Running tests](doc/manual/running-tests.md): selection, reruns, `dune runtest` and CI.
  - [Coverage](doc/manual/coverage.md): the code no test runs.
  - [Mutation testing](doc/manual/mutation.md): the changes no test notices.
  - [Migrating from 0.1](doc/manual/migrating-from-0.1.md): each 0.1 spelling and its replacement.
- Explanation: [Design notes](doc/manual/notes.md), why windtrap is shaped as it is.
- Reference: [`lib/windtrap.mli`](lib/windtrap.mli), also read with `odig doc windtrap`, and
  [`ppx/ppx_windtrap.mli`](ppx/ppx_windtrap.mli) for the inline test forms.

A coding agent starts with the skill
[`SKILL.md`](SKILL.md).
[`CHANGES.md`](CHANGES.md) lists the changes of each release. Questions
are welcome on the [OCaml forum](https://discuss.ocaml.org/).

## Examples

[`examples/`](examples/) holds the project of each manual page, run by
`dune runtest`; [its README](examples/README.md) lists them.

## Contributing

[`doc/dev/`](doc/dev/) describes the architecture, how windtrap tests
itself, the changelog discipline and the release checklist.

## Acknowledgments

Windtrap builds on ideas and code from several OCaml projects:

- **[Alcotest](https://github.com/mirage/alcotest)** by Thomas Gazagnaire: test structure and runner design.
- **Craig Ferguson's Alcotest PRs** ([#294](https://github.com/mirage/alcotest/pull/294), [#247](https://github.com/mirage/alcotest/pull/247)): API design, subcomponent diffing, and Levenshtein distance (ISC).
- **[QCheck2](https://github.com/c-cube/qcheck)** by Simon Cruanes et al.: generator design and integrated shrinking (BSD 2-Clause).
- **[ppx_expect](https://github.com/janestreet/ppx_expect)** and **[ppx_inline_test](https://github.com/janestreet/ppx_inline_test)** by Jane Street: expect test paradigm and dune integration.
- **[Bisect_ppx](https://github.com/aantron/bisect_ppx)** by Anton Bachin et al.: coverage instrumentation and runtime (MIT).
- **[mtime](https://erratique.ch/software/mtime)** by Daniel Bünzli: the monotonic clock (ISC).

[`THIRD_PARTY_LICENSES.md`](THIRD_PARTY_LICENSES.md) holds the notices of the code derived from them.
