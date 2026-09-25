# Mutation testing

This page shows how to find the changes to a library that no test
notices, one mutant at a time, and how to kill, reproduce or dismiss
each. The reference is `windtrap mutants --help`, in the
[last section](#the-commands-options), and the suite's `--help`. The
example is `examples/08-mutation/`, and the transcripts print its paths.

## Instrumenting a library for mutation

Mutation testing is a field of the library stanza, and the tests are
ordinary `(test)` stanzas.

`dune`:

<!-- file examples/08-mutation/dune -->
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

(rule
 (alias mutate)
 (deps
  (alias_rec runtest)
  (universe))
 (action
  (run %{bin:windtrap} mutants)))
```

The `instrumentation` field names the backend, `ppx_windtrap.mutate`.
A build that passes `--instrument-with ppx_windtrap.mutate` compiles
every mutant of the library into it, each behind a guard, and a run
executes the original code until it tests a mutant. A plain build
compiles the library as written. The field repeats, and one library can
carry both this backend and [coverage](coverage.md)'s. The rule at the
end is the alias of
[Mutation testing in one command](#mutation-testing-in-one-command).

## Testing a suite's mutants

The library holds a calculator.

`calc.ml`:

<!-- file examples/08-mutation/calc.ml -->
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

<!-- file examples/08-mutation/test_calc.ml -->
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
          raises_match (Exn.invalid_arg ~substring:"division by zero")
            (fun () -> Calc.apply Calc.Div 1 0));
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
which tests reach each mutant, then runs each reached mutant in a child
process with those tests alone. A mutant that no test fails on prints
as a `SURVIVED` block: its identifier, the rewrite, the source line and
the tests that ran it. No test calls `sign 0`:

<!-- run examples/08-mutation/instrumented as examples/08-mutation -->
```
$ dune exec --instrument-with ppx_windtrap.mutate examples/08-mutation/test_calc.exe -- --mutate
calc: 10 passed in 1.6ms.

─────────────────────── survivors ────────────────────────
  SURVIVED  examples/08-mutation/calc.ml:10:16:ge  n > 0 → n >= 0
      10 │ let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

    2 tests ran this line and none failed:
      sign › is -1 for a negative  examples/08-mutation/test_calc.ml:32
      sign › is 1 for a positive   examples/08-mutation/test_calc.ml:31

  SURVIVED  examples/08-mutation/calc.ml:10:37:le  n < 0 → n <= 0
      10 │ let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

    1 test ran this line and did not fail:
      sign › is -1 for a negative  examples/08-mutation/test_calc.ml:32
──────────────────────────────────────────────────────────

reproduce: dune exec --instrument-with ppx_windtrap.mutate examples/08-mutation/test_calc.exe -- --arm examples/08-mutation/calc.ml:10:16:ge
mutants: 2 survived of 5 reached by this suite, 3 killed
```

## Reproducing a survivor

The `reproduce:` line arms the first survivor. `--arm ID` runs the
suite once with that mutant active, names it first, records no
correction, and ends on its verdict: killed, survived, or not evaluated
when no selected test ran the site. Under `dune runtest`, the mirror
arms it in every suite that holds its file, as in
`WINDTRAP_MUTATE_ARM=ID dune runtest --force --instrument-with ppx_windtrap.mutate`.
Arming a killed mutant shows the failure that killed it:

<!-- run examples/08-mutation/instrumented as examples/08-mutation -->
```
$ dune exec --instrument-with ppx_windtrap.mutate examples/08-mutation/test_calc.exe -- --arm examples/08-mutation/calc.ml:10:16:ge
mutant examples/08-mutation/calc.ml:10:16:ge armed: n > 0 → n >= 0
calc: 10 passed in 0.7ms.
mutant survived: the armed site was evaluated 2 times and no test failed.
$ dune exec --instrument-with ppx_windtrap.mutate examples/08-mutation/test_calc.exe -- --arm examples/08-mutation/calc.ml:6:11:add
mutant examples/08-mutation/calc.ml:6:11:add armed: a - b → a + b
calc: 10 tests
──────────────────────── failures ────────────────────────
  FAIL  subtraction › subtracts (mutant armed)
    examples/08-mutation/test_calc.ml:12
      12 │ test "subtracts" (fun () -> equal int 6 (Calc.apply Calc.Sub 10 4));

    expected  6
    actual    14
──────────────────────────────────────────────────────────

9 passed, 1 failed in 0.7ms.
mutant killed.
```

## Testing one test's mutants

To judge a new test, narrow the mutation run. `--mutate=PREFIX` tests
only the mutants of the files whose path starts with a prefix, and still
saves its verdicts. The filters select the tests as in any run; a
filtered run lists the mutants its tests never reached and saves no
verdict. `stays positive` checks a sign that `a - b → a + b` keeps:

<!-- run examples/08-mutation/instrumented as examples/08-mutation -->
```
$ dune exec --instrument-with ppx_windtrap.mutate examples/08-mutation/test_calc.exe -- --mutate=examples/08-mutation/calc.ml -f "stays positive"
calc: 1 passed in 0.5ms.

─────────────────────── survivors ────────────────────────
  SURVIVED  examples/08-mutation/calc.ml:6:11:add  a - b → a + b
      6 │ | Sub -> a - b

    1 test ran this line and did not fail:
      subtraction › stays positive  examples/08-mutation/test_calc.ml:11
──────────────────────────────────────────────────────────

─────────────────── never reached (4) ────────────────────
  4  examples/08-mutation/calc.ml   lines 5, 8, 10
──────────────────────────────────────────────────────────

reproduce: dune exec --instrument-with ppx_windtrap.mutate examples/08-mutation/test_calc.exe -- --arm examples/08-mutation/calc.ml:6:11:add -f 'stays positive'
windtrap: verdicts not saved: this run's selection narrows the suite, and a partial run's verdicts would stand in the project merge as the whole.
mutants: 1 survived of 1 reached by the 1 selected test, 4 never reached
```

## What a mutation run runs

A mutation run runs in passes, and prints neither the focus warning nor
the withheld-correction warning:

- The dry run is the suite's ordinary run. Under `-u` it accepts
  corrections before any mutant is armed.
- The determinism probe, in a child process, checks that the suite
  passes again.
- Each reached mutant runs in a child with its reaching tests, up to the
  first failure. A child that outruns a deadline taken from the dry run,
  or evaluates its site more than `hits * 8 + 1000` times, `hits` being
  the dry run's count, is killed, and its mutant counts as killed.

The mutation run exits 0 whatever it finds. It exits 1, with a sentence
on standard error, when a pass fails or no mutant is left to test.

## Dismissing an equivalent mutant

A mutant that no test can tell from the original is equivalent. `abs`
returns `n` when `n >= 0`, and the mutant `n > 0` differs at zero only,
where both branches return 0. To dismiss it, put `[@mutate off
"reason"]` on the expression, as `calc.ml` does; the site is then
neither tested nor counted. `[@@mutate off]` dismisses a binding,
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
mutant killed by one executable is killed, and the command exits 1 when
a mutant survived every executable that reached it, listing the most
reached first. `--force` makes dune run the suites that already passed:

<!-- run examples/08-mutation/instrumented as examples/08-mutation -->
```
$ WINDTRAP_MUTATE=1 dune runtest --force --instrument-with ppx_windtrap.mutate
calc: 10 passed in 0.7ms.

─────────────────────── survivors ────────────────────────
  SURVIVED  examples/08-mutation/calc.ml:10:16:ge  n > 0 → n >= 0
      10 │ let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

    2 tests ran this line and none failed:
      sign › is -1 for a negative  examples/08-mutation/test_calc.ml:32
      sign › is 1 for a positive   examples/08-mutation/test_calc.ml:31

  SURVIVED  examples/08-mutation/calc.ml:10:37:le  n < 0 → n <= 0
      10 │ let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

    1 test ran this line and did not fail:
      sign › is -1 for a negative  examples/08-mutation/test_calc.ml:32
──────────────────────────────────────────────────────────

reproduce: dune exec --instrument-with ppx_windtrap.mutate examples/08-mutation/test_calc.exe -- --arm examples/08-mutation/calc.ml:10:16:ge
mutants: 2 survived of 5 reached by this suite, 3 killed
$ dune exec windtrap -- mutants
───────────────────── survivors (2) ──────────────────────
  SURVIVED  examples/08-mutation/calc.ml:10:16:ge  n > 0 → n >= 0
      10 │ let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

    2 tests ran this line and none failed:
      test_calc.exe  sign › is -1 for a negative
      test_calc.exe  sign › is 1 for a positive

  SURVIVED  examples/08-mutation/calc.ml:10:37:le  n < 0 → n <= 0
      10 │ let sign n = if n > 0 then 1 else if n < 0 then -1 else 0

    1 test ran this line and did not fail:
      test_calc.exe  sign › is -1 for a negative
──────────────────────────────────────────────────────────

reproduce: dune exec --instrument-with ppx_windtrap.mutate examples/08-mutation/test_calc.exe -- --arm examples/08-mutation/calc.ml:10:16:ge
mutants: 2 survived of 5 reached, 3 killed, 1 executable
```

## Mutation testing in one command

The `mutate` alias of the example's dune file runs both commands:
`WINDTRAP_MUTATE=1 dune build @mutate --force --instrument-with
ppx_windtrap.mutate`. It is built as the alias of
[coverage](coverage.md#measuring-in-one-command) is.
`(deps (env_var WINDTRAP_MUTATE))` on a test stanza makes dune
run it again when the variable changes, in place of `--force`. A
`dune-workspace` naming the backend drops `--instrument-with`, as for
[coverage](coverage.md#instrumenting-every-build).

## Killing a survivor

A survivor names a behaviour no test checks. To kill the two survivors
of [Testing a suite's mutants](#testing-a-suites-mutants), add a test at
zero to the `sign` group:

`test_calc.ml`:

<!-- file examples/08-mutation/killed/test_calc.ml from let sign -->
```ocaml
let sign =
  group "sign"
    [
      test "is 1 for a positive" (fun () -> equal int 1 (Calc.sign 5));
      test "is -1 for a negative" (fun () -> equal int (-1) (Calc.sign (-5)));
      test "is 0 for zero" (fun () -> equal int 0 (Calc.sign 0));
    ]
```

The mutation run then kills every mutant the suite reaches, and the
project's merge passes:

<!-- run examples/08-mutation/killed as examples/08-mutation -->
```
$ dune exec --instrument-with ppx_windtrap.mutate examples/08-mutation/test_calc.exe -- --mutate
calc: 11 passed in 0.9ms.
mutants: 5 reached by this suite, 5 killed
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

A comparison outside a condition, as in `let ok = a < b`, carries no
mutant, and neither does an `assert`. In a chain of one operator, as
`a + b + c`, the outermost application alone is a site. A file that
declares inline tests is not mutated, and a file that rebinds an
operator loses the rewrites of its family: the four arithmetic
operators, the six comparisons or the two connectives.

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

A mutation run forks a child process for each mutant. It is refused on
Windows, and in a process that has started a domain. `--mutate` and
`--arm` together are a usage error. An interrupt ends the run: the
running child is killed, the mutant under test is named on standard
error, and no verdict file is written.

## The command's options

`windtrap mutants --help` lists its options; the suite's own flags are
in [Running tests](running-tests.md):

<!-- run examples/08-mutation/instrumented as examples/08-mutation -->
```
$ dune exec windtrap -- mutants --help
windtrap mutants - merge .mutants verdict files and report the survivors

usage: windtrap mutants [PATH...]

Merges the .mutants verdict files written by mutation runs and reports the
mutants that survived every test executable. Without PATH arguments the files
are found under the build directory's _mutants (or _windtrap/mutants in a tree
built without one), walking up from the current directory to the enclosing
project root; PATH arguments (.mutants files, or directories searched
recursively) replace that default.

Runs no tests and drives no build.
Exits 1 when any mutant survived every executable that reached it.

OPTIONS:
  -h, --help
      Print this help and exit.

ENVIRONMENT (no flag):
  WINDTRAP_COLOR
      Color output: always, never or auto.
```
