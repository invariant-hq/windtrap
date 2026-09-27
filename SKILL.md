---
name: windtrap-testing
description: Guides writing OCaml test suites with windtrap 0.2, the decisions only - which kind of test for which need, the shape of a suite, the commands to run, how to read a failure and accept a baseline, and when to reach for properties, stateful tests, coverage and mutation testing - with the section of windtrap.mli that holds each mechanism. Use when writing tests, adding a test suite, fixing a failing test, reviewing tests, or setting up coverage or mutation testing in a project that uses windtrap. Triggers on phrases like "write tests for this", "add a test suite", "test this function", "property test this", "expect test", "snapshot test", "why is this test failing", "check coverage", "run mutation testing", or "review these tests".
---

# Testing with windtrap

Windtrap runs unit, property, stateful and expect tests from one flat
interface, `open Windtrap`, with coverage and mutation testing in the
`ppx_windtrap` package. This file decides; `windtrap.mli` is the
contract, and each section below names the sections of it to read.

`windtrap.mli` is installed with the library: read it in
`$(ocamlfind query windtrap)/windtrap.mli`, in the opam switch's
`lib/windtrap/`, or with `odig doc windtrap`. Its sections are, in
order: Declaring tests (with Resources and Annotations), Assertions
(with Laws), Witnesses, Properties (with `Gen` and Discarding and
labelling cases), Stateful tests, Baselines, Captured output, The
running test, Running (with Exit codes and Command line and
environment). A name this file does not explain is explained there.

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

| The code under test is | Write | In `windtrap.mli` |
| --- | --- | --- |
| A function with a law: round trip, invariant, agreement with a simpler function, algebraic identity | `prop` over a generator, with the `Law` verb when one names the law | Properties, Laws |
| A value with state across calls: container, cache, store, pool | `stateful` against a model or a simpler implementation | Stateful tests |
| A function whose results the spec states for chosen inputs | `test` with `equal`, `cases` for a table of inputs | Declaring tests, Assertions |
| Text too long to write by hand: help, report, pretty-printer output | `expect` or `expect_file`; `let%expect_test` inside a library | Baselines, Captured output |
| An executable's command line, output and exit code | a dune cram test (below) | none: dune's cram tests |

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
- A `Law` verb, such as `Law.round_trip` or `Law.associative`, for a
  textbook law. Its failure names the law and prints each term.
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
  stateful tests against a model.

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

`expect_file` reads its path from the project root, while the stanza's
`deps` and `diff?` name the file from the stanza's directory: for the
stanza above in `test/dune`, the call is
`expect_file (Mylib.help ()) "test/help.expected"`. A path written from
the stanza's directory names a file that does not exist: its failure
says `no baseline`, and its `accept:` line cannot fix it.

In `windtrap.mli`: Declaring tests, Resources, Annotations, and `run`.

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

In `windtrap.mli`: Running, Exit codes, Command line and environment;
and `--help` on any suite.

## Reading a failure

Read the whole block before editing anything. It holds:

- `FAIL` and the test's path, then its location and source line. An
  assertion in tail position reports the test's declaration line; add
  `~__POS__` to the assertion for its own line. An assertion that ends a
  `let%test` or `let%expect_test` body reports its own line.
- `expected` then `actual`, or a diff marked `-` for the expected text
  and `+` for the actual; for a property, `counterexample (case K,
  shrunk N steps):` with the value, `which failed at:` and the
  assertion's failure; for a stateful test, the shrunk program as a
  table of the calls that ran, then the failing call with the
  reference's outcome as `expected` and the system's as `actual`, or
  `reference of call N of N` when the model itself broke.
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

A seed replays within one version of windtrap. Each verb's doc comment
in the Assertions section of `windtrap.mli` says what its failure
prints.

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

In `windtrap.mli`: Baselines, Captured output.

## Properties and stateful tests

- Laws to look for: decoding what was encoded, agreement with a slower
  or simpler function, an invariant after each operation, algebraic
  identities, a relation between two runs (scaling the input scales the
  output), and "never raises" on any input. Any law without a name
  below is an `equal`, the trusted side first.
- `Law` names seventeen textbook laws. Find what you wrote, and state
  each law of its row in a `prop` of its own, `Law.x w f` as its body:

  | You wrote | Its laws |
  | --- | --- |
  | A witness | `equivalence`, `order`, and `ignores w int hash r` for its hash |
  | A relation that orders values, such as inclusion | `partial_order`, over three values drawn as a chain |
  | A merge | `associative`; `commutative` when operand order does not matter; `neutral` for its empty value; `absorbing` for a value that absorbs every other |
  | An operation that can be undone | `associative`, `neutral` for its identity, `invertible` for its inverse |
  | Two operations of one type | `distributive op ~over` |
  | A codec | `round_trip wa wb encode decode`, and `round_trip wb wa decode encode` over canonical texts |
  | A normaliser | `idempotent`, and `ignores` for each difference it erases |
  | A reversal | `involutive` |
  | A map-like function | `commutes` with another transformation, `homomorphic` from one operation to another |
  | A cost | `monotone` |
  | An invariant | `preserves` for each operation that must keep it |

  Each law's doc comment in the Laws section of `windtrap.mli` states
  its equation and argument order.
- A witness you build with `Testable.make` gets a property of
  `Law.equivalence`, and of `Law.order` when it has an order. When a
  value has several spellings, pass `~respell`, a function that returns
  an equal value built differently. An equality that is always true
  passes every `equal` that uses it.
- A law's `never covered:` failure says no drawn case exercised the
  law. Fix the generator, the respelling or the witness, never the law.
- Draw the sizes of generated values from `Gen.nat` or a range, and a
  number the law computes with from `Gen.small_int`, so that the law
  itself cannot overflow. Draw an argument that the API bounds or
  counts with (an index, a length, a count) across its edges: 0, the
  bound, one past either end, and the extremes of `int` with their
  neighbours, where the stdlib's `Dynarray.blit ~src_pos:max_int` and
  `String.take_last (min_int + 3)` went wrong. `Gen.int` draws
  `min_int` but none of its neighbours, so with `n` the largest valid
  value:

  ```ocaml
  Gen.frequency
    [
      (6, Gen.int_range (-2) (n + 2));
      (1, Gen.int);
      ( 1,
        Gen.of_list ~pp:Format.pp_print_int
          [ min_int; min_int + 1; max_int - 1; max_int ] );
    ]
  ```

  A structural precondition (non-empty, sorted) belongs in the
  generator; `assume` is for rare cases, since a property that discards
  too many gives up.
- Give a generator of your type a printer with `Gen.with_pp`, the same
  `pp` its witness uses; a list of chosen values takes it directly,
  `Gen.of_list ~pp:Format.pp_print_int [ 0; max_int ]`. `cover "label" cond` fails the property when no
  passing case carries the label; use it for a case the law depends
  on.
- A stateful test pairs, per operation, the reference's function with
  the system's: `command name signature reference system`. The
  reference is a model written for the test, with the API's functions
  and argument order, or another implementation, such as `Set.Make` for
  a faster set. It behaves the same from run to run: no `Random`, no
  `Hashtbl` order in a result.
- An exception is an outcome. The reference raises what the API
  documents, `Full` on a full queue, and the system must raise a
  constructor of the same name. To compare a payload, both functions
  return a `result`.
- A handle that a call makes (a queue, a connection) is a value of an
  `abstract` type: a command ending in `makes q` makes one, and `q ^->`
  takes one. Never draw an index into a table of handles of your own.
- Take an index or a key that a value holds with `among`:
  `let index = among int vec (fun m -> List.init (Model.length m) Fun.id)`,
  then `vec ^-> index ^-> returns int`. The element reads the nearest
  value of its type before it in the signature, else the first after
  it, so `Map.find` takes `key ^-> map ^-> returns int` with no
  wrapper. Keep one command with a drawn index, for the bounds check.
- `~pre` keeps a call the API forbids (undefined behaviour, a call that
  blocks) from both sides. A call that raises a documented exception
  needs no `~pre`: the raise is compared. Put a `cover` in the reference
  for a state that matters, such as a full queue. A command that no
  program can call, because its `~pre` never holds or no command makes a
  type it takes, fails the test with a `never called:` message that
  names it.
- Observe after every call: give the abstract type an `~invariant`
  that compares the two sides through functions that do not change the
  state, such as `to_list` or `length`, never an LRU's `find`. A bug
  then fails at the call that caused it.
- Where the reference cannot predict an outcome (an order the API
  leaves open, a policy's decision, a system with no model), end the
  signature with `judges w`. The reference receives `Ok v` or
  `Error e`, returns to accept it, and rejects it with a verb (`equal`,
  `mem`, `failf`) or with `Error e -> raise e`. Reject every outcome
  you do not accept, in every state: a judge that raises anything else
  breaks the reference, and the failure is the test's.
- Draw the inputs a policy must treat alike, such as every spelling of
  one path (`secrets/k`, `./secrets/k`, `notes/../secrets/k`), and judge
  the policy in both directions, so that a guard that blocks everything
  fails too.
- Name the scenario a bug needs with a `cover` in the reference
  function of the call that observes it. A scenario that a case
  reaches with probability `p` goes unmet over `count` cases with
  probability `(1 - p)^count`: raise `~count` for one rarer than one
  case in twenty.
- A system that holds a resource (a file, a directory, a socket) is made
  by a command and released by `abstract ~release`, which must also
  accept a closed value. `temp_dir`, `setenv` and `chdir` last for the
  whole test, so the command makes its own scratch directory and the
  release removes it. A system whose calls perform effects, as Eio's
  do, runs with their handler around `run`.
- A program that failed is kept by copying its calls into a `test`.
- A structure shared between domains takes the same command list twice:
  `stateful` and `stateful ~domains:2`. The second runs the middle of
  each program on two domains at once and fails when no order of the
  calls explains what the system returned. There a command that makes a
  value or has a `~pre` runs only before the parallel calls, so an
  operation meant to be called concurrently must be total. Its failure
  does not replay its schedule; a structure known to be unsafe is
  `xfail (stateful ~domains:2 …)`.

In `windtrap.mli`: Properties, `Gen`, Discarding and labelling cases,
Laws, Stateful tests (with `judges`, `among` and `stateful`'s
`~domains` paragraph).

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
`windtrap coverage --help`, `windtrap mutants --help` and a suite's
`--help` (`--mutate`, `--arm`) state the flags.

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
