---
name: windtrap-testing
description: Guides writing OCaml test suites with windtrap 0.2, the decisions only - which kind of test for which need, the shape of a suite, the commands to run, how to read a failure and accept a baseline, and when to reach for properties, stateful tests, coverage and mutation testing - with the mechanics linked to the manual. Use when writing tests, adding a test suite, fixing a failing test, reviewing tests, or setting up coverage or mutation testing in a project that uses windtrap. Triggers on phrases like "write tests for this", "add a test suite", "test this function", "property test this", "expect test", "snapshot test", "why is this test failing", "check coverage", "run mutation testing", or "review these tests".
---

# Testing with windtrap

Windtrap runs unit, property, stateful and expect tests from one flat
interface, `open Windtrap`, with coverage and mutation testing in the
`ppx_windtrap` package. This file decides; the manual shows how. Each
section links the page that holds the mechanics.

The manual is `doc/manual/` of the repository,
<https://github.com/invariant-hq/windtrap/tree/main/doc/manual>. The
reference is `windtrap.mli`, installed with the library: read it in
`$(ocamlfind query windtrap)/windtrap.mli`, in the opam switch's
`lib/windtrap/`, or with `odig doc windtrap`. A name this file does not
explain is explained there.

## Before writing a test

- If the project has a suite, follow its framework and its layout. Read
  the `dune` files: `(test)` and `(tests)` stanzas, `(inline_tests)`,
  `(cram)` and `*.t` files say what exists and how it runs.
- List the obligations from the interface before reading the
  implementation: every exported value, every documented exception and
  `Error` case, every stated invariant. A test derived from the `.mli`
  says what the code must do; one derived from the code repeats it.
- Where the `.mli` is silent on a behaviour you must test, do not choose
  it silently: name the assumption in the test's name and report the
  gap.

## Choosing the kind of test

Take the first row that fits the behaviour.

| The code under test is | Write | Page |
| --- | --- | --- |
| A function with a law: round trip, invariant, agreement with a simpler function, algebraic identity | `prop` over a generator | [Property testing](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/property-testing.md) |
| A value with state across calls: container, cache, store, pool | `stateful` against a model | [Stateful testing](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/stateful-testing.md) |
| A function whose results the spec states for chosen inputs | `test` with `equal`, `cases` for a table of inputs | [Assertions](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/assertions.md) |
| Text too long to write by hand: help, report, pretty-printer output | `expect` or `expect_file`; `let%expect_test` inside a library | [Baselines and expect tests](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/baselines.md) |
| An executable's command line, output and exit code | a dune cram test (below) | dune's manual |

Within a test, pick the verb whose failure shows the data:

- `equal w expected actual`, expected first, under the witness `w` of
  the type (`int`, `string`, `list int`, `Testable.make ~pp ~equal` for
  a type of your own). `text` for multi-line strings, whose failure is a
  diff.
- `less`, `at_most`, `greater`, `at_least` with `~than` for a bound,
  never `is_true (n > 0)`, whose failure prints `false`.
- `require_ok`, `require_some`, `require_error` to assert a shape and
  continue with its payload.
- `raises e f` for an exact exception, `raises_match` with an `Exn`
  predicate for a message you check in part.
- `satisfies ~claim` only when no witness verb states the claim.

Rules for every test:

- An expected value comes from the spec, never from running the code.
  A value you had to run the code to learn is a baseline: write it as
  `expect`, where accepting it is a reviewed step.
- A property needs a law. Without one, write `cases` over chosen
  inputs.
- Choose inputs to break the code: empty, one element, each boundary
  and its neighbours, duplicates, `min_int` and `max_int`, `nan`,
  non-ASCII text, the format's own delimiters, every documented error.
- Test through the public interface. A helper is tested through the
  public value that reaches it.
- Assert what the claim is about and nothing more: not a whole help
  text to check one flag, not exact floats (`float eps`), not the order
  of an unordered list (`slist`), never a time or an absolute path.
- A baseline pins what the code does today. Every module also needs
  tests that state what it must do: `equal` from the spec, properties,
  stateful models.

## The shape of a suite

A suite is an executable declared by a `(test)` stanza with
`(libraries windtrap)`, and its file ends by running a list of groups:

```ocaml
open Windtrap

let parse =
  group "parse"
    [
      test "reads a number between spaces" (fun () ->
          equal int 3 (Mylib.parse " 3 "));
      test "rejects an empty string" (fun () ->
          raises (Mylib.Parse_error "empty") (fun () -> Mylib.parse ""));
    ]

let () = exit (run "mylib" [ parse ])
```

- A group is a top-level value named after the noun its tests are
  about. A test's name is a sentence stating its claim.
- A body of one to three lines stays inline. A longer body is a
  top-level function that the group lists by name.
- `run` returns the exit code, and `let () = run …` does not compile.
- A test left out of the list does not run. `-l` lists what runs.
- Tests of a library's internals are `let%test` or `let%expect_test`
  next to the code, in a library with `(inline_tests)` and
  `(preprocess (pps ppx_windtrap))`; a `let%test` body asserts with the
  same verbs and returns `unit`. The executable is for tests from
  outside the library and for tests that need `bracket`, `scoped` or
  `fixture`. A library can have both: dune runs its inline tests in the
  library's own runner, and a `(test)` executable that links the library
  runs its own tests alone. Coverage and mutation leave the inline tests
  out and instrument the code beside them.
- A resource belongs to a test: `bracket ~setup ~teardown` for one per
  test, `scoped` for a `with_`-style function, `fixture` for one shared
  across the run. `temp_dir`, `setenv` and `chdir` are undone when the
  test ends. Tests share only a `fixture`, and never through its mutable
  state, since `-f`, `--failed` and `--shard` change which tests run.
- A known bug is `xfail ~reason:"issue #N" (test …)`: green while the
  bug exists, red the day it is fixed. A test the machine cannot run is
  `skip ~reason ()`. `focus` is for a local session only; under CI a
  suite that holds one is refused.
- A stanza whose suite holds `expect` or `expect_file` runs it with
  `--corrected` and diffs each file that holds baselines, so that
  `dune promote` accepts a change:

```lisp
(test
 (name test_mylib)
 (libraries windtrap mylib)
 (deps help.expected)
 (action
  (progn
   (run %{test} --corrected)
   (diff? test_mylib.ml test_mylib.ml.corrected)
   (diff? help.expected help.expected.corrected))))
```

Mechanics: [Resources and structure](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/resources-and-structure.md).

## Running tests

| To | Run |
| --- | --- |
| run every suite | `dune runtest`; `--force` reruns suites that passed |
| run the suites of one directory | `dune runtest test/unit` |
| run one suite with flags | `dune exec test/test_mylib.exe -- FLAGS` |
| select by path, drop by path | `-f PATTERN`, `-e PATTERN`, each repeatable |
| select by tag | `--tag slow`, `--exclude-tag slow`; properties carry `prop` |
| list what a selection runs | `-l` |
| rerun the last failed tests | `--failed` |
| stop at the first failure | `-x` |
| print a line per test | `-v` |
| see a test's output as it is written | `-s` |
| pass a flag under `dune runtest` | its mirror, `WINDTRAP_FILTER=parse dune runtest --force` |
| write JUnit files under CI | `WINDTRAP_JUNIT=_build/junit dune runtest` |

The exit code is `0` when no selected test failed, `1` when one failed,
and `2` when no test ran: a filter on the command line that matched
nothing, an empty `--failed`, a usage error. Treat `2` as a failure of
the command, never as a pass. A `WINDTRAP_*` variable reaches every
stanza, and a stanza whose tests its filter misses passes, saying that
no test ran.

A suite run through `dune exec` styles its report even into a pipe or a
file. To read plain text, set `WINDTRAP_COLOR=never`, which
`windtrap coverage` and `windtrap mutants` read too.

Mechanics: [Running tests](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/running-tests.md),
and `--help` on any suite.

## Reading a failure

Read the whole block before editing anything. It holds:

- `FAIL` and the test's path, then its location and source line. An
  assertion in tail position reports the test's declaration line; add
  `~__POS__` to the assertion for its own line.
- `expected` then `actual`, or a diff marked `-` for the expected text
  and `+` for the actual; for a property, `counterexample (case K,
  shrunk N steps):` with the value, `which failed at:` and the
  assertion's failure; for a stateful test, the shrunk program as a
  table of calls with the model before each.
- `captured output`, the last lines the test printed after its last
  `output ()`, and `full log:`, the file with all of them. `[setup]` or
  `[teardown]` before the location when the failure is in one.
- A last command when it says something new: under dune, `accept:`
  promotes a baseline's file; `reproduce:` arms a mutant.

Two commands act on the whole run and sit right above the summary. Run
by hand, a report whose failures include a stale baseline has one
`accept:` line: it reruns the run's tests with `-u`. A report whose
failures include a property or a stateful test has one `replay:` line:
it reruns the run's tests with the run's seed, so each failed test draws
the values it failed on.

Then:

1. Reproduce with the `replay:` line or `--failed`, narrowed with `-f`.
2. Decide which side is wrong. A failing `equal` from the spec, a
   property or a stateful test says the code is wrong until the spec
   says otherwise. A failing baseline asks whether the change was
   intended.
3. Fix the code, or, for an intended change, accept the baseline.
4. After fixing a property's failure, add its counterexample to
   `~examples`, which runs before any generated case on every seed.

A seed replays within one version of windtrap. Mechanics: each verb's
block in
[Assertions](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/assertions.md).

## Accepting a baseline

- Under a stanza that runs `--corrected`, the block's `accept:` line
  reads `dune promote FILE`. Run it right after the failing
  `dune runtest`: dune forgets a correction at its next command, and
  keeps none from a suite where another test failed. Dune holds one
  correction per stanza per run, so with two stale files, promote, run
  the tests, promote again.
- Without such a stanza, the report's `accept:` line reruns the run's
  tests with `-u`, which rewrites every stale literal and file in place.
  Build again before the next run. `-u` is refused under CI.
- A block with `no correction was kept:` has another failure to fix
  first. `correction refused (line N):` says why the source cannot take
  the correction.
- Accepting is writing the assertion. Read every hunk with `git diff`,
  and never accept a change you cannot explain.
- Output that varies between runs (times, paths, addresses) is masked
  before the comparison: in code before `expect`, and with a shadowed
  `Expect_test_config.sanitize` for `let%expect_test`.

Mechanics: [Baselines and expect tests](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/baselines.md).

## Properties and stateful tests

- Laws to look for: decoding what was encoded, agreement with a slower
  or simpler function, an invariant after each operation, algebraic
  identities, a relation between two runs (scaling the input scales the
  output), and "never raises" on any input.
- Draw sizes and indices from `Gen.nat` or `Gen.int_range 0 n`, and
  magnitudes from `Gen.small_int`, not the full range of `int`. A structural precondition (non-empty, sorted) belongs
  in the generator; `assume` is for rare cases, since a property that
  discards too many gives up.
- Give a generator of your type a printer with `Gen.with_pp`, the same
  `pp` its witness uses. `cover "label" cond` fails the property when no
  passing case carries the label; use it for a case the law depends
  on.
- A stateful model is a persistent value, such as a list or a `Map`,
  and `~pre` and `~next` are pure. A command that leaves the model as
  it is, such as a read, omits `~next`. `~pre` both forbids a call and
  selects the state it needs. Put a `cover` in `~invariant` for a state
  a command needs, since a precondition no program meets removes the
  command without a failure.
- A command's argument cannot name a handle that does not exist yet:
  generate an index into the model's live handles, and let `~pre` keep
  the lookup defined.
- `~scope` builds a fresh system for each program and each shrink
  candidate, many times in one test. `temp_dir`, `setenv` and `chdir`
  last for the whole test, so a scope creates and removes its own
  scratch files.
- A program that failed is kept by copying its calls into a `test`.

Mechanics: [Property testing](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/property-testing.md),
[Stateful testing](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/stateful-testing.md).

## Coverage and mutation testing

Coverage finds code no test runs; mutation testing finds code the tests
run without checking. Both need the backend on the library under test,
an `instrumentation` field that does nothing until a build asks for it:

```lisp
(library
 (name mylib)
 (instrumentation
  (backend ppx_windtrap.coverage))
 (instrumentation
  (backend ppx_windtrap.mutate)))
```

Coverage, for the code you changed:

```
dune runtest --instrument-with ppx_windtrap.coverage
dune exec windtrap -- coverage -u
```

Write a test for each uncovered branch that matters, and mark code no
test should reach with `[@coverage off]`. `--min PCT` fails the command
below a percentage; set it a little under the measured number and never
lower it to pass.

Mutation, after writing or changing a test, scoped to the file it
exercises and filtered to the test:

```
dune exec --instrument-with ppx_windtrap.mutate test/test_mylib.exe -- --mutate=lib/mylib.ml -f "the test's name"
```

Each `SURVIVED` block names a rewrite of one line that the listed tests
ran and did not notice. Resolve each one:

- The assertion is weak (`is_true`, a shape check, an expected value the
  mutant also gives): strengthen it until the mutant dies. The
  `reproduce:` line runs the suite with that mutant armed.
- The mutant is equivalent, no test can tell it from the original:
  dismiss it in the source with `[@mutate off "the reason"]`.

A filtered run exits 0 whatever it finds and saves no verdict. The
project's answer merges every suite's verdicts, and exits 1 when a
mutant survived every suite that reached it:

```
WINDTRAP_MUTATE=1 dune runtest --force --instrument-with ppx_windtrap.mutate
dune exec windtrap -- mutants
```

Mutation testing forks a child per mutant and is refused on Windows.
Mechanics: [Coverage](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/coverage.md),
[Mutation testing](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/mutation.md).

## Testing an executable

A command's output and exit code are tested through the built binary in
a dune cram test, a `.t` file of shell commands and their expected
output, accepted with `dune promote`. Declare the binary as a
dependency, `(cram (deps %{bin:mytool}))` for a binary with a
`public_name` and its path otherwise, or the test runs a stale one. A
nonzero exit prints as `[N]` after the output. Mask what varies (times,
versions, home paths) with `sed` in the session. A binary built
from an instrumented library writes coverage for each command the
session runs, and `windtrap coverage` merges it with the suites'.

## Keeping failures visible

- Never weaken an assertion, change an expected value, special-case a
  test input in the code, or delete or skip a failing test to get a
  pass. A known bug is an `xfail` with its issue.
- When you conclude a test is wrong, stop: name the spec that
  contradicts it and report it, instead of editing it.
- Report what the run said: `2` is a selection that ran nothing, and a
  failed teardown beside a passing body is a failure.

## Checklist

- Obligations listed from the `.mli`; gaps reported.
- Each behaviour tested by the first row of the table that fits;
  expected values from the spec; inputs chosen to break the code.
- Suite in the shape above; baseline stanzas run `--corrected`.
- Every new test seen failing: the bug's test before the fix, the
  filtered mutation run for the others, every survivor resolved.
- Every accepted baseline hunk read.
- Coverage read for the code you changed.
