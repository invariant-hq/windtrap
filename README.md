# Windtrap — One library for all your OCaml tests

Windtrap runs unit, property, stateful and expect tests from one flat
interface, with coverage and mutation testing in the companion ppx. A
suite is an ordinary executable that `dune runtest` builds and runs.

## A first suite

```ocaml
open Windtrap

let basics =
  group "basics"
    [
      test "evicts the oldest entry when full" (fun () ->
          let c = Lru.create 2 in
          List.iter (fun k -> Lru.add c k k) [ 1; 2; 3 ];
          equal (option int) None (Lru.find c 1));
      test "lists its keys, most recent first" (fun () ->
          let c = Lru.create 3 in
          List.iter (fun k -> Lru.add c k k) [ 1; 2; 3 ];
          ignore (Lru.find c 1);
          expect (Lru.to_string c) @@ __POS_OF__ {|1 3 2|});
    ]

let bounded =
  prop "never holds more than its capacity" Gen.(list int) (fun keys ->
      let c = Lru.create 3 in
      List.iter (fun k -> Lru.add c k k) keys;
      at_most int ~than:3 (Lru.size c))

let () = exit (run "lru" [ basics; bounded ])
```

A run with nothing to report prints one line:

```
$ dune runtest
mylib: 3 passed in 1.0ms (seed s1:b02192cebcec40d2).
```

## Features

### Unit tests

`test` and `group` declare a suite, and `run` runs it. Assertions such
as `equal`, `less`, `contains`, `raises` and `require_some` take a
witness, for example `int` or `list string`, and a failure prints the
values it compared.

```ocaml
test "splits on commas" (fun () ->
    equal (list string) [ "a"; "b" ] (String.split_on_char ',' "a,b"))
```

### Property tests

`prop` checks a law over values generated with `Gen`. A failing input is
reduced to a smaller one that still fails.

```ocaml
prop "rev is an involution" Gen.(list int) (fun l ->
    equal (list int) l (List.rev (List.rev l)))
```

### Stateful tests

`stateful` runs generated programs of calls on a system and on a
reference, such as a model of its state or another implementation, and
compares what each call returns or raises. A failing program is reduced
to a shorter one and printed as the calls that ran. With `~domains:2`,
the middle of each program runs on two domains at once, and the test
fails when no order of the calls explains the results.

```ocaml
command "pop" (queue ^-> returns int) Model.pop Bounded_queue.pop
```

### Expect tests

`expect` compares a string with a literal in the test's source, and
`expect_file` with a file. `ppx_windtrap` provides `let%expect_test` and
`[%expect]`. `dune promote` accepts a change.

```ocaml
expect (Printf.sprintf "%d items" (List.length cart)) @@ __POS_OF__ {|3 items|}
```

### Coverage

`ppx_windtrap.coverage` instruments a library, and `windtrap coverage`
reports the expressions that the tests did not run.

```
dune runtest --instrument-with ppx_windtrap.coverage
dune exec windtrap -- coverage
```

### Mutation testing

`ppx_windtrap.mutate` compiles mutants of a library, small changes such
as `>=` into `>`, into its test executables. `--mutate` runs the tests
on each mutant and reports the mutants that no test fails on.

```
dune exec --instrument-with ppx_windtrap.mutate test/test_calc.exe -- --mutate
```

## Installation

    opam install windtrap
    opam install ppx_windtrap   # expect tests, coverage, mutation testing

## Documentation

The manual, [`doc/manual/`](doc/manual/), has one page per need:

- Tutorial: [Getting started](doc/manual/getting-started.md), the suite above and its first failure.
- How-to:
  - [Assertions](doc/manual/assertions.md): values, bounds, strings, results and exceptions.
  - [Property testing](doc/manual/property-testing.md): laws over generated values.
  - [Stateful testing](doc/manual/stateful-testing.md): a system against a reference.
  - [Baselines and expect tests](doc/manual/baselines.md): `expect`, `expect_file`, `let%expect_test`.
  - [Resources and structure](doc/manual/resources-and-structure.md): a suite's layout and resources.
  - [Running tests](doc/manual/running-tests.md): selection, reruns, `dune runtest` and CI.
  - [Coverage](doc/manual/coverage.md): the code no test runs.
  - [Mutation testing](doc/manual/mutation.md): the changes no test notices.
  - [Migrating from 0.1](doc/manual/migrating-from-0.1.md): each 0.1 spelling and its replacement.
- Explanation: [Design notes](doc/manual/notes.md), why windtrap is shaped as it is.
- Reference: [`lib/windtrap.mli`](lib/windtrap.mli), and
  [`ppx/ppx_windtrap.mli`](ppx/ppx_windtrap.mli) for the inline test forms.

Questions are welcome on the [OCaml forum](https://discuss.ocaml.org/).

## Examples

[`examples/`](examples/) holds the project of each manual page, run by
`dune runtest`; [its README](examples/README.md) lists them.

## Acknowledgments

Windtrap builds on ideas and code from several OCaml projects:

- **[Alcotest](https://github.com/mirage/alcotest)** by Thomas Gazagnaire: test structure and runner design.
- **Craig Ferguson's Alcotest PRs** ([#294](https://github.com/mirage/alcotest/pull/294), [#247](https://github.com/mirage/alcotest/pull/247)): API design, subcomponent diffing, and Levenshtein distance (ISC).
- **[QCheck2](https://github.com/c-cube/qcheck)** by Simon Cruanes et al.: generator design and integrated shrinking (BSD 2-Clause).
- **[ppx_expect](https://github.com/janestreet/ppx_expect)** and **[ppx_inline_test](https://github.com/janestreet/ppx_inline_test)** by Jane Street: expect test paradigm and dune integration.
- **[Bisect_ppx](https://github.com/aantron/bisect_ppx)** by Anton Bachin et al.: coverage instrumentation and runtime (MIT).
- **[mtime](https://erratique.ch/software/mtime)** by Daniel Bünzli: the monotonic clock (ISC).

[`THIRD_PARTY_LICENSES.md`](THIRD_PARTY_LICENSES.md) holds the notices of the code derived from them.
