# Coverage

This page shows how to measure which parts of a library its tests run,
read the lines no test reached, and fail a build whose coverage falls
below a minimum. The reference is `windtrap coverage --help`, in the
[last section](#the-commands-options). The example is
`examples/07-coverage/`, and the transcripts ran at the root of a copy
of it made a project of its own, as the
[blueprint](../../examples/x-blueprint/README.md#copying-the-project-out)
shows.

## Instrumenting a library

Coverage is a field of the library stanza, and the tests are ordinary
`(test)` stanzas.

`dune`:

```lisp
(library
 (name windtrap_example_coverage)
 (modules calc half_a half_b stats)
 (instrumentation
  (backend ppx_windtrap.coverage)))

(test
 (name test_calc)
 (modules test_calc)
 (libraries windtrap windtrap_example_coverage))

(test
 (name test_a)
 (modules test_a)
 (libraries windtrap windtrap_example_coverage))

(test
 (name test_b)
 (modules test_b)
 (libraries windtrap windtrap_example_coverage))
```

The `instrumentation` field names the backend, `ppx_windtrap.coverage`.
It does nothing until a build passes `--instrument-with
ppx_windtrap.coverage`, so a plain `dune runtest` builds and runs the
library as written. An instrumented build counts and changes no test's
outcome.

## Measuring coverage

The library holds a calculator, and its suite leaves the `Sub` and `Mul`
arms untested.

`calc.ml`:

```ocaml
type op = Add | Sub | Mul | Div

let apply op a b =
  match op with
  | Add -> a + b
  | Sub -> a - b
  | Mul -> a * b
  | Div -> if b = 0 then invalid_arg "Calc.apply: division by zero" else a / b

let eval start steps =
  List.fold_left (fun acc (op, operand) -> apply op acc operand) start steps

let symbol = function Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
[@@coverage off]
```

`test_calc.ml`:

```ocaml
open Windtrap
module Calc = Windtrap_example_coverage.Calc

let apply =
  group "apply"
    [
      test "adds" (fun () -> equal int 5 (Calc.apply Calc.Add 2 3));
      test "divides" (fun () -> equal int 3 (Calc.apply Calc.Div 7 2));
      test "rejects a zero divisor" (fun () ->
          raises_match (Exn.invalid_arg ~substring:"division by zero")
            (fun () -> Calc.apply Calc.Div 1 0));
    ]

let eval =
  group "eval"
    [
      test "folds the steps" (fun () ->
          equal int 3 (Calc.eval 1 [ (Calc.Add, 5); (Calc.Div, 2) ]));
    ]

let () = exit (run "calc" [ apply; eval ])
```

Two commands measure it. The instrumented run builds the library with
the backend, and each suite writes a dump when it exits. A test run
prints no coverage number. `windtrap coverage` merges the dumps into one
row per source file and prints the total last:

```
$ dune runtest --instrument-with ppx_windtrap.coverage
calc: 4 passed in 0.8ms.
half_b: 8 passed in 1.1ms.
half_a: 6 passed in 1.3ms.
$ dune exec windtrap -- coverage
   cover    points   file        uncovered lines (-u shows the source)
   77.8%     7/9     calc.ml     6-7
  100.0%     9/9     half_a.ml
  100.0%    11/11    half_b.ml
coverage: 93.1% (27/29 points)
```

## Seeing the uncovered lines

To read the source of the uncovered lines, pass `-u`. A point is the
entry of a block, such as a function body, a `match` arm or an `if`
branch, or the return of a call. A call that raises leaves its point
unvisited, except in tail position, where it has no point of its own. A
raising call still reads covered when it shares its point with the block
it opens, and calls of `raise`, `failwith`, `invalid_arg` and `exit`,
which never return, and of trivial primitives such as `/` have no point
for their return. These calls are matched by name as written, so
`Stdlib.exit 1` has a point for its return and a function of one's own
called `exit` has none. A row lists at most eight line ranges, then
`(+N more)`. `-u` shows every one, with `▌` on each line an unvisited
point touches. The percentage counts points:

```
$ dune exec windtrap -- coverage -u
   cover    points   file        uncovered lines
   77.8%     7/9     calc.ml     6-7
  100.0%     9/9     half_a.ml
  100.0%    11/11    half_b.ml

calc.ml: 77.8% (7/9)

      5 │   | Add -> a + b
  ▌   6 │   | Sub -> a - b
  ▌   7 │   | Mul -> a * b
      8 │   | Div -> if b = 0 then invalid_arg "Calc.apply: division by zero" else a / b

coverage: 93.1% (27/29 points)
```

## Failing a build below a minimum

To fail a build below a minimum, pass `--min PCT`. The last line states
the minimum and whether the total meets it, and the command exits 1 when
it does not. Under CI, run it after the instrumented `dune runtest`. A
percentage prints red below the minimum, or below 80% without one:

```
$ dune exec windtrap -- coverage --min 95
   cover    points   file        uncovered lines (-u shows the source)
   77.8%     7/9     calc.ml     6-7
  100.0%     9/9     half_a.ml
  100.0%    11/11    half_b.ml
coverage: 93.1% (27/29 points), minimum 95%: FAILED
```

## Finding modules no test links

A module that no suite links has no points in any dump, and neither has
a library without the `instrumentation` field, so the total leaves them
out. To require data for a source, pass `--expect PATH`, a file or a
directory relative to the project root. The command names each source
under it that has no data and exits 1; `--do-not-expect PATH` exempts a
file or a directory. `calc.mll` and `calc.pp.ml` count as `calc.ml`. No
suite calls `stats.ml`:

```
$ dune exec windtrap -- coverage --expect stats.ml
   cover    points   file        uncovered lines (-u shows the source)
   77.8%     7/9     calc.ml     6-7
  100.0%     9/9     half_a.ml
  100.0%    11/11    half_b.ml
coverage: 93.1% (27/29 points)
windtrap: stats.ml: expected source has no coverage data (not instrumented, or linked into no test executable that ran)
```

## Excluding code from coverage

To leave code out of the count, mark it with an attribute:
`[@coverage off]` on an expression, `[@@coverage off]` on a binding,
`[@@@coverage off]` and `[@@@coverage on]` around structure items, or
`[@@@coverage exclude_file]` for the whole file (see
[`ppx/coverage/instrument.mli`](../../ppx/coverage/instrument.mli)).
`off` takes a reason, as in `[@coverage off "reason"]`, which no report
prints. `symbol`, at the end of `calc.ml`, carries `[@@coverage off]`,
and no report on this page counts its points. Inline tests (`let%test`,
`let%expect_test` and `module%test` with its helpers) carry no point
without an attribute, and the rest of their file is counted.

## Measuring one suite

A suite's dump holds the points of every instrumented module its
executable links. `test_b` calls `Half_a.greet` and nothing else of
`Half_a`, and its dump holds all of `Half_a`. The project's report adds
the counts of each point over every dump, and `half_a.ml` reads 100%
there. To read one suite's dump, set `WINDTRAP_COVERAGE_FILE` to a path,
which each run replaces, and pass the path to `windtrap coverage`:

```
$ WINDTRAP_COVERAGE_FILE=half_b.coverage dune exec --instrument-with ppx_windtrap.coverage ./test_b.exe
half_b: 8 passed in 2.1ms.
$ dune exec windtrap -- coverage half_b.coverage
   cover    points   file        uncovered lines (-u shows the source)
   11.1%     1/9     half_a.ml   1-2, 5-7
  100.0%    11/11    half_b.ml
coverage: 60.0% (12/20 points)
```

## Exporting the report

For other tools, `--json` prints the report as JSON and `--lcov` as an
LCOV tracefile, which `genhtml`, Codecov and editor gutters read. Either
makes standard output the document, and under `--min` the gate's line
goes to standard error. `dune exec windtrap -- coverage --lcov >
lcov.info` writes the tracefile:

```
$ dune exec windtrap -- coverage --json
{ "summary": { "visited": 27, "total": 29, "percentage": 93.10 },
  "files": [
    { "path": "calc.ml", "visited": 7, "total": 9,
      "percentage": 77.78,
      "uncovered_lines": [6,7] },
    { "path": "half_a.ml", "visited": 9, "total": 9,
      "percentage": 100.00,
      "uncovered_lines": [] },
    { "path": "half_b.ml", "visited": 11, "total": 11,
      "percentage": 100.00,
      "uncovered_lines": [] } ] }
$ dune exec windtrap -- coverage --lcov
TN:
SF:calc.ml
DA:4,5
DA:5,2
DA:6,0
DA:7,0
DA:8,1
DA:11,1
LF:6
LH:4
end_of_record
TN:
SF:half_a.ml
DA:1,1
DA:2,1
DA:5,1
DA:6,1
DA:7,1
LF:5
LH:5
end_of_record
TN:
SF:half_b.ml
DA:1,1
DA:2,1
DA:3,1
LF:3
LH:3
end_of_record
```

GitLab's merge-request coverage view reads Cobertura or JaCoCo XML and
not LCOV, and windtrap writes neither. A GitLab job can still show the
total: its `coverage` keyword with the regular expression
`/coverage: \d+\.\d+%/` reads it from the report's last line.

## Where the dumps are

A suite built by dune writes its dumps under `_build/_coverage`, in a
directory of its own, one file per run. Every run keeps its dump, so a
tool that a cram test runs several times is measured over every run.
[Mutation testing](mutation.md#mutation-testing-a-project) does not
count the runs of such a tool. The first run of a rebuilt executable
removes the dumps of its predecessors. When nothing a suite depends on
has changed, dune does not run it again, and the report counts the dump
of its last run.

An instrumented executable outside any build directory writes under
`_windtrap/coverage` in its working directory, and `windtrap coverage`
finds that directory from it or from below it. Under `dune exec` the command reads the build directory dune names, so a
build with `--build-dir` reports its own dumps.

## Instrumenting without dune

Each backend, `ppx_windtrap.coverage` and `ppx_windtrap.mutate`, is a
ppxlib rewriter. Outside dune, a driver executable that links `ppxlib`
and the backend and calls `Ppxlib.Driver.standalone ()` instruments a
file when the compiler runs it as `-ppx "driver.exe --as-ppx"`, with the
installed `windtrap/runtime` directory on the include path. Instrument
the library and not its tests, link the suite against `windtrap`, and
run it, with `--mutate` for the mutation backend. Compile the suite with
`-g`, which a test's location needs. `windtrap coverage` or `windtrap
mutants` then merges what it wrote. `test/cram/run/nodune.t`
holds such a session.

## When a dump is excluded

Each dump records the executable that wrote it and a digest of its
bytes. `windtrap coverage` leaves out a dump whose executable was
deleted or rebuilt since, with a warning on standard error, then says
once how to refresh the dumps: run the suites instrumented again. A
build without `--instrument-with` rebuilds the suites uninstrumented,
and every dump they wrote is then excluded. With every dump excluded,
the command prints no report and exits 1, and it does the same with no
dump at all, as before any instrumented run, saying how to write one. A
dump in another version of the format stops the command, and the
message says to delete it.

## The command's options

`windtrap coverage --help` lists every option:

```
$ dune exec windtrap -- coverage --help
windtrap coverage - merge .coverage files and report

usage: windtrap coverage [OPTIONS] [PATH...]

Merges the .coverage files written by instrumented test executables and
reports expression coverage per source file. Without PATH arguments the files
are found under the build directory's _coverage (or _windtrap/coverage in a
tree built without one), walking up from the current directory to the
enclosing project root; PATH arguments (.coverage files, or directories
searched recursively) replace that default.

OPTIONS:
  --min=PCT
      Exit 1 when total coverage is below PCT.

  --json
      Machine-readable report on standard output.

  --lcov
      LCOV tracefile on standard output (genhtml, Codecov, Coveralls, editor
      gutters).

  --expect=PATH
      Exit 1 unless every .ml/.mll/.mly under PATH (or PATH itself) has
      coverage data; repeatable.

  --do-not-expect=PATH
      Exempt PATH, a file or a directory, from --expect.

  -u, --show-uncovered
      Also render uncovered source excerpts.

  --color=MODE (env WINDTRAP_COLOR)
      Color output: always, never or auto.

  -h, --help
      Print this help and exit.

ENVIRONMENT (no flag):
  NO_COLOR
      Any value: never style output (--color auto).
```
