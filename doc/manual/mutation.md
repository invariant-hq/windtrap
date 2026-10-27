# Mutation testing

This page shows how to find the changes to a library that no test
notices, one mutant at a time, and how to kill, reproduce or dismiss
each. The reference is `windtrap mutants --help`, in the
[last section](#the-commands-options), and the suite's `--help`. The
example is `examples/08-mutation/`, and the transcripts ran at the root
of a copy of it made a project of its own, as the
[blueprint](../../examples/x-blueprint/README.md#copying-the-project-out)
shows.

## Instrumenting a library for mutation

Mutation testing is a field of the library stanza, and the tests are
ordinary `(test)` stanzas.

`dune`:

```lisp
(library
 (name windtrap_example_mutation)
 (modules calc)
 (instrumentation
  (backend ppx_windtrap.mutate)))

(test
 (name test_calc)
 (modules test_calc)
 (libraries windtrap windtrap_example_mutation))
```

The `instrumentation` field names the backend, `ppx_windtrap.mutate`.
A build that passes `--instrument-with ppx_windtrap.mutate` compiles
every mutant of the library into it, each behind a guard, and a run
executes the original code until it tests a mutant. A plain build
compiles the library as written. The field repeats, and one library can
carry both this backend and [coverage](coverage.md)'s.

## Testing a suite's mutants

The library holds a calculator.

`calc.ml`:

```ocaml
type op = Add | Sub | Mul | Div

let apply op a b =
  match op with
  | Add -> a + b
  | Sub -> a - b
  | Mul -> a * b
  | Div -> if b = 0 then invalid_arg "Calc.apply: division by zero" else a / b

let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

let abs n =
  if (n >= 0) [@mutate off "both arms yield 0 at n = 0"] then n else -n
```

`test_calc.ml`:

```ocaml
open Windtrap
module Calc = Windtrap_example_mutation.Calc

let addition =
  group "addition"
    [ test "adds" (fun () -> equal int 5 (Calc.apply Calc.Add 2 3)) ]

let subtraction =
  group "subtraction"
    [
      test "stays positive" (fun () -> is_true (Calc.apply Calc.Sub 10 4 > 0));
      test "subtracts" (fun () -> equal int 6 (Calc.apply Calc.Sub 10 4));
    ]

let multiplication =
  group "multiplication"
    [ test "multiplies" (fun () -> equal int 12 (Calc.apply Calc.Mul 3 4)) ]

let division =
  group "division"
    [
      test "divides" (fun () -> equal int 3 (Calc.apply Calc.Div 7 2));
      test "rejects a zero divisor" (fun () ->
          raises_match (Exn.invalid_arg ~sub:"division by zero") (fun () ->
              Calc.apply Calc.Div 1 0));
    ]

let sign =
  group "sign"
    [
      test "is 1 for a positive" (fun () -> equal int 1 (Calc.sign 5));
      test "is -1 for a negative" (fun () -> equal int (-1) (Calc.sign (-5)));
    ]

let abs =
  group "abs"
    [
      test "negates a negative" (fun () -> equal int 3 (Calc.abs (-3)));
      test "keeps zero" (fun () -> equal int 0 (Calc.abs 0));
    ]

let () =
  exit
    (run "calc" [ addition; subtraction; multiplication; division; sign; abs ])
```

To test the mutants a suite reaches, run the suite built with the
backend and pass `--mutate`. The run executes the suite once, recording
which tests reach each mutant, then runs each reached mutant in a
process of its own with those tests alone. A mutant that no test fails on prints
as a `SURVIVED` block: its location in the compiler's format, which
editors and dune jump to, its identifier, the rewrite, the source line
and the tests that ran it. No test calls `sign 0` or `sign 1`:

```
$ dune exec --instrument-with ppx_windtrap.mutate ./test_calc.exe -- --mutate
calc: 10 passed in 1.3ms.

─────────────────────── survivors ────────────────────────
File "calc.ml", line 10, characters 16-21:
  SURVIVED  calc.ml:10:16:ge  n > 0 → n >= 0
      10 │ let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

    2 tests ran this line and none failed:
      sign › is -1 for a negative  test_calc.ml:32
      sign › is 1 for a positive   test_calc.ml:31

File "calc.ml", line 10, characters 20-21:
  SURVIVED  calc.ml:10:20:int  0 → 1
      10 │ let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

    2 tests ran this line and none failed:
      sign › is -1 for a negative  test_calc.ml:32
      sign › is 1 for a positive   test_calc.ml:31

File "calc.ml", line 10, characters 37-42:
  SURVIVED  calc.ml:10:37:le  n < 0 → n <= 0
      10 │ let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

    1 test ran this line and did not fail:
      sign › is -1 for a negative  test_calc.ml:32
──────────────────────────────────────────────────────────

─────────────────── never reached (1) ────────────────────
  1  calc.ml   line 10
──────────────────────────────────────────────────────────

reproduce: dune exec --instrument-with ppx_windtrap.mutate ./test_calc.exe -- --arm calc.ml:10:16:ge
mutants: 3 survived of 9 reached by this suite, 6 killed, 1 never reached
```

## Reproducing a survivor

The `reproduce:` line arms the first survivor. `--arm ID` runs the
suite once with that mutant active, names it first, records no
correction, and ends on its verdict: killed, survived, or not evaluated
when no selected test ran the site. Under `dune runtest`, the mirror
arms it in every suite that holds its file, as in
`WINDTRAP_MUTATE_ARM=ID dune runtest --force --instrument-with ppx_windtrap.mutate`
(see [Mutation testing a project](#mutation-testing-a-project) for
`--force`). Arming a killed mutant shows the failure that killed it:

```
$ dune exec --instrument-with ppx_windtrap.mutate ./test_calc.exe -- --arm calc.ml:10:16:ge
mutant calc.ml:10:16:ge armed: n > 0 → n >= 0
calc: 10 passed in 2.1ms.
mutant survived: the armed site was evaluated 2 times and no test failed.
$ dune exec --instrument-with ppx_windtrap.mutate ./test_calc.exe -- --arm calc.ml:6:11:add
mutant calc.ml:6:11:add armed: a - b → a + b
calc: 10 tests
──────────────────────── failures ────────────────────────
  FAIL  subtraction › subtracts (mutant armed)
    test_calc.ml:12
      12 │ test "subtracts" (fun () -> equal int 6 (Calc.apply Calc.Sub 10 4));

    expected  6
    actual    14
──────────────────────────────────────────────────────────

9 passed, 1 failed in 1.8ms.
mutant killed.
```

## Testing one test's mutants

To judge a new test, narrow the mutation run. `--mutate=PREFIX` tests
only the mutants of the files whose path starts with a prefix, and still
saves its verdicts. The filters select the tests as in any run; a
filtered run lists the mutants its tests never reached and saves no
verdict. `stays positive` checks a sign that `a - b → a + b` keeps:

```
$ dune exec --instrument-with ppx_windtrap.mutate ./test_calc.exe -- --mutate=calc.ml -f "stays positive"
calc: 1 passed in 1.1ms.

─────────────────────── survivors ────────────────────────
File "calc.ml", line 6, characters 11-16:
  SURVIVED  calc.ml:6:11:add  a - b → a + b
      6 │ | Sub -> a - b

    1 test ran this line and did not fail:
      subtraction › stays positive  test_calc.ml:11
──────────────────────────────────────────────────────────

─────────────────── never reached (9) ────────────────────
  9  calc.ml   lines 5, 8, 10
──────────────────────────────────────────────────────────

reproduce: dune exec --instrument-with ppx_windtrap.mutate ./test_calc.exe -- --arm calc.ml:6:11:add -f 'stays positive'
windtrap: verdicts not saved: this run's selection narrows the suite, and a partial run's verdicts would stand in the project merge as the whole.
mutants: 1 survived of 1 reached by the 1 selected test, 9 never reached
```

## Testing a change's mutants

A change is judged by the mutants on its own lines. `--lines` keeps the
mutants on the lines it names, as `--lines=calc.ml:10` or
`--lines=lib/a.ml:3-7,12,lib/b.ml:40`, each file as its mutants'
identifiers spell it, relative to the project root. `--diff FILE`
reads the lines that a `git diff -U0` output adds or changes; with the
default `a/` and `b/` prefixes it names the files as the identifiers do
when the repository's root is the project's. Both narrow `--mutate`,
whose prefixes they combine with, and the run still saves its verdicts,
replacing those on its lines. `windtrap mutants` takes the same two
flags, and reports, and fails on, the mutants of those lines alone, and
`windtrap coverage` the points of those lines (see
[coverage.md](coverage.md)). Their mirrors are `WINDTRAP_LINES` and
`WINDTRAP_DIFF`; a relative path in `WINDTRAP_DIFF` is read from the
project root.

```
$ git diff -U0 > change.diff
$ dune exec --instrument-with ppx_windtrap.mutate ./test_calc.exe -- --mutate --diff change.diff
```

## What a mutation run runs

A mutation run runs in passes, and prints neither the focus warning nor
the withheld-correction warning:

- The dry run is the suite's ordinary run. Under `-u` it accepts
  corrections before any mutant is armed.
- The determinism probe runs the suite again and checks that it passes
  again.
- Each reached mutant runs with its reaching tests, up to the first
  failure. One that evaluates its site more than `hits * 8 + 1000`
  times, `hits` being the dry run's count, fails its test, and the
  mutant is killed.

Every pass runs in worker processes, and the probe and each mutant in a
worker of its own, started as a run's workers are (see [Running
tests](running-tests.md#running-tests-in-worker-processes)): none starts
from what the dry run or another mutant left, a table filled or a lazy
value forced, so a cache cannot hide a mutant. The mutant is armed once
the worker's module initialisation is over. They run up to `-j` at once,
as tests do, and are reported in catalogue order. Unless `--seed` is
given, a mutation run draws its property cases from one fixed seed,
`s1:6d7574616e747321`, so a verdict is the same on every run of one
binary.

A mutant whose run ends before its tests do, because the process
crashed or exited or passed a deadline taken from the dry run, is
unknown, and so is no survivor. The report lists it under `unknown`
with the reason and an `arm:` command, which runs the suite with it
armed and shows what happens. A mutant whose tests pass without
evaluating its site, as when an earlier test that its run leaves out
decided whether the site runs, is not evaluated and is no survivor
either, listed under `not evaluated` with its `arm:` command.

A site that no test runs, and that module initialization or a
fixture release runs, is evaluated outside tests. No run tests its
mutant, and the report lists its line under `evaluated outside tests`,
apart from the lines never reached.

A test marked `xfail` reaches no mutant. The dry run and the probe run
it, and no mutant's run does. A line that only such tests run is never
reached, and an `--arm` run of its mutant ends on `mutant not reached`.

The mutation run exits 0 whatever it finds. It exits 1, with a sentence
on standard error, when a pass fails or no mutant is left to test.

## Dismissing an equivalent mutant

A mutant that no test can tell from the original is equivalent. `abs`
returns `n` when `n >= 0`, and the mutant `n > 0` differs at zero only,
where both branches return 0. To dismiss it, put `[@mutate off
"reason"]` on the expression, as `calc.ml` does; the site is then
neither tested nor counted. It dismisses every mutant of the
expression, in this version and in any later one that gives the
expression more mutants. `[@@mutate off]` dismisses a binding,
`[@@@mutate off]` and `[@@@mutate on]` the structure items between
them, and `[@@@mutate exclude_file]` a file. Only `off` takes a reason,
which no report prints.

## Mutation testing a project

A suite's mutation run covers its own executable. To judge the project,
run every suite with `WINDTRAP_MUTATE=1` and merge the verdict files
with `windtrap mutants`. `WINDTRAP_MUTATE` is the mirror of `--mutate`,
where `1` is the bare flag, `0` its absence, and a value that spells no
boolean the prefixes. A suite with no mutant to test, such as a suite
over another library, runs as usual and says why on standard error. A
mutant killed by one executable is killed. A mutant that no executable
killed and one could not judge is unknown, and one that another did not
evaluate is not evaluated; neither is a survivor. The command exits 1
when a mutant survived every executable that reached it, listing the
most reached first, and when an executable could not mutation-test its
suite: the merge names it under `not mutation-tested`, with the mutants
its file leaves unknown.

Dune does not track `WINDTRAP_MUTATE`. A suite that ran without it
does not run again when it is set, unless its build changed, and the
merge then finds no verdict of that build.
`(deps (env_var WINDTRAP_MUTATE))` on a test stanza makes dune run it
again when the variable changes. Without it, `--force` makes dune run
every suite:

```
$ WINDTRAP_MUTATE=1 dune runtest --force --instrument-with ppx_windtrap.mutate
calc: 10 passed in 2.7ms.

─────────────────────── survivors ────────────────────────
File "calc.ml", line 10, characters 16-21:
  SURVIVED  calc.ml:10:16:ge  n > 0 → n >= 0
      10 │ let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

    2 tests ran this line and none failed:
      sign › is -1 for a negative  test_calc.ml:32
      sign › is 1 for a positive   test_calc.ml:31

File "calc.ml", line 10, characters 20-21:
  SURVIVED  calc.ml:10:20:int  0 → 1
      10 │ let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

    2 tests ran this line and none failed:
      sign › is -1 for a negative  test_calc.ml:32
      sign › is 1 for a positive   test_calc.ml:31

File "calc.ml", line 10, characters 37-42:
  SURVIVED  calc.ml:10:37:le  n < 0 → n <= 0
      10 │ let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

    1 test ran this line and did not fail:
      sign › is -1 for a negative  test_calc.ml:32
──────────────────────────────────────────────────────────

─────────────────── never reached (1) ────────────────────
  1  calc.ml   line 10
──────────────────────────────────────────────────────────

reproduce: dune exec --instrument-with ppx_windtrap.mutate ./test_calc.exe -- --arm calc.ml:10:16:ge
mutants: 3 survived of 9 reached by this suite, 6 killed, 1 never reached
$ dune exec windtrap -- mutants
───────────────────── survivors (3) ──────────────────────
File "calc.ml", line 10, characters 16-21:
  SURVIVED  calc.ml:10:16:ge  n > 0 → n >= 0
      10 │ let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

    2 tests ran this line and none failed:
      test_calc.exe  sign › is -1 for a negative
      test_calc.exe  sign › is 1 for a positive

File "calc.ml", line 10, characters 20-21:
  SURVIVED  calc.ml:10:20:int  0 → 1
      10 │ let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

    2 tests ran this line and none failed:
      test_calc.exe  sign › is -1 for a negative
      test_calc.exe  sign › is 1 for a positive

File "calc.ml", line 10, characters 37-42:
  SURVIVED  calc.ml:10:37:le  n < 0 → n <= 0
      10 │ let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

    1 test ran this line and did not fail:
      test_calc.exe  sign › is -1 for a negative
──────────────────────────────────────────────────────────

─────────────────── never reached (1) ────────────────────
  1  calc.ml   line 10
──────────────────────────────────────────────────────────

reproduce: dune exec --instrument-with ppx_windtrap.mutate ./test_calc.exe -- --arm calc.ml:10:16:ge
mutants: 3 survived of 9 reached, 6 killed, 1 never reached, 1 executable
```

Only code that a suite's own process runs reaches a mutant. A binary
that is not a suite, such as a command-line tool that a cram test or a
dune rule runs or that a test starts as a child process, reaches none,
even when it links the instrumented library. Mutants in code that only
such a binary runs are listed as never reached when a suite links their
module, and are missing from the reports when no suite links it.
`WINDTRAP_MUTATE_ARM` arms a mutant in a suite, never in such a binary.
[Coverage](coverage.md#where-the-dumps-are) does count that code, as
each instrumented executable writes its own dump. A command-line tool
can then be fully covered and have no mutant reached. To test those
mutants, put the tool's entry point in the library, call it from a
test, and check what it prints with `expect` or `expect_file`.

## Killing a survivor

A survivor names a behaviour no test checks. To kill the three survivors
of [Testing a suite's mutants](#testing-a-suites-mutants), add tests at
zero and at one to the `sign` group:

`test_calc.ml`:

```ocaml
let sign =
  group "sign"
    [
      test "is 1 for a positive" (fun () -> equal int 1 (Calc.sign 5));
      test "is -1 for a negative" (fun () -> equal int (-1) (Calc.sign (-5)));
      test "is 0 for zero" (fun () -> equal int 0 (Calc.sign 0));
      test "is 1 for one" (fun () -> equal int 1 (Calc.sign 1));
    ]
```

The mutation run then kills every mutant the suite reaches, and the
project's merge passes:

```
$ dune exec --instrument-with ppx_windtrap.mutate ./test_calc.exe -- --mutate
calc: 12 passed in 2.7ms.
mutants: 10 reached by this suite, 10 killed
$ dune exec windtrap -- mutants
mutants: 10 reached, 10 killed, 1 executable
```

## What is mutated

Each mutant rewrites one expression, and the last part of its
identifier names the rewrite:

- `not` negates a condition that is neither a comparison nor a
  connective: the condition of an `if` or a `while`, or a guard.
- A comparison in a condition moves by one boundary: `<` becomes `<=`
  (`le`), `<=` becomes `<` (`lt`), `>` becomes `>=` (`ge`), `>=` becomes
  `>` (`gt`), `=` becomes `<>` (`neq`) and `<>` becomes `=` (`eq`).
- `&&` becomes `||` (`or`), and `||` becomes `&&` (`and`).
- `+` becomes `-` (`sub`) and `-` becomes `+` (`add`); `+.` and `-.`
  swap likewise (`fsub`, `fadd`).
- `del` drops a statement: `e1` in `e1; e2` does not run. It asks
  whether a test observes what the statement does.
- `int` nudges an integer literal by one: `1` becomes `0`, and any other
  `n` becomes `n + 1`, in the type of its suffix.
- `bool` flips `true` or `false`, outside a condition and the operands
  of `&&` and `||`, where `not` already acts.

Some code carries no mutant, because no test should pin it or none can
run it. `del` leaves a resource release alone: a call of `close`,
`close_in`, `close_out`, their `_noerr` forms or `closedir`. `int`
leaves alone an operand of `+` or `-`, which `add` and `sub` move
already; an argument of a function whose name ends in `create`, as the
capacity of `Hashtbl.create 16`; the constant of a comparison in a
condition when the nudge is the comparison's own boundary move, as the
`0` of `x < 0`, since `x < 1` is `x <= 0`. `bool` leaves alone the
argument of `~cloexec`. None of the three has a mutant in code that runs
when the module is initialized, before any test, as in a top-level
`let limit = 64` or `let () = setup (); main ()`, since no run can
judge it.

A comparison outside a condition, as in `let ok = a < b`, carries no
mutant, and neither does an `assert`. In a chain of one operator, as
`a + b + c`, the outermost application alone is a site. Inline tests
(`let%test`, `let%expect_test` and `module%test` with its helpers)
carry no mutant, and the rest of their file is mutated. A file that
rebinds an operator loses the rewrites of its family: the four
arithmetic operators, the six comparisons or the two connectives.

## Where the verdicts are

A suite's `--mutate` run over the whole suite writes one verdict file
under `_build/_mutants`, and its next such run replaces it. An
executable outside any build directory writes under `_windtrap/mutants`
in its working directory. A run under `--mutate=PREFIX` replaces the
verdicts under its prefixes and keeps the others, when the same build
wrote the file. A verdict file records its executable and a digest of
its bytes, and `windtrap mutants` leaves out one whose executable was
deleted or rebuilt since, with a warning, as
[`windtrap coverage`](coverage.md#when-a-dump-is-excluded) does a dump.

## Mutation testing without dune

The mutation backend is applied without dune as
[the coverage backend](coverage.md#instrumenting-without-dune) is, and
the suite then runs with `--mutate`.

## When a run cannot mutate

A mutation run starts a worker process for each mutant. It is refused on
Windows, and in a process forked from the test executable that spawned a
domain before its run: that process cannot fork, and a new start of the
program would not reach its run. A test may spawn a domain. A run that
is refused, or whose selection narrows the suite, still writes its
verdict file when this build has none, saying why, and the project's
merge names the executable under `not mutation-tested`. `--mutate` and
`--arm` together are a usage error. An interrupt ends the run: the
running mutants are killed, the first not yet reported is named on
standard error, and no verdict file is written.

## The command's options

`windtrap mutants --help` lists its options; the suite's own flags are
in [Running tests](running-tests.md):

```
$ dune exec windtrap -- mutants --help
windtrap mutants - merge .mutants verdict files and report the survivors

usage: windtrap mutants [OPTIONS] [PATH...]

Merges the .mutants verdict files written by mutation runs and reports the
mutants that survived every test executable. Without PATH arguments the files
are found under the build directory's _mutants (or _windtrap/mutants in a tree
built without one), walking up from the current directory to the enclosing
project root; PATH arguments (.mutants files, or directories searched
recursively) replace that default.

Runs no tests and drives no build.
Exits 1 when any mutant survived every executable that reached it, or when an
executable could not mutation-test its suite.

OPTIONS:
  --lines=FILE:A-B,...
      Report only the mutants on these lines of these files.

  --diff=FILE
      Report only the mutants on the lines that FILE, the output of git diff
      -U0, adds or changes.

  --color=MODE (env WINDTRAP_COLOR)
      Color output: always, never or auto.

  -h, --help
      Print this help and exit.

ENVIRONMENT (no flag):
  NO_COLOR
      Any value: never style output (--color auto).
```
