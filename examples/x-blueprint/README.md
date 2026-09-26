# Blueprint project

This directory is a project layout to copy: a small library, its
binary, and a `test/` tree with one kind of test per directory and the
project's coverage and mutation aliases. It also keeps a known bug, a
dismissed mutant and a weak law, each shown in its own section below.

## The layout

```
dune-project        the package, its dependencies, cram enabled
dune-workspace      both instrumentation backends on every build
lib/                Slug and Stats, the library under test
bin/                main.exe, a command line over Slug
test/dune           the cover and mutate aliases
test/unit/          one suite per module: test_slug, test_stats
test/failures/      one suite per known bug: issue_1
test/expect/        let%expect_test over Slug, in a library
test/cram/          sessions of main.exe
```

- `dune-workspace` names both backends, `ppx_windtrap.coverage` and
  `ppx_windtrap.mutate`, in the default context, so every build of a
  copy is instrumented. Dune reads the file only at the workspace root,
  and inside windtrap's repository it is inert.
- `lib/dune` names the same two backends in `instrumentation` fields. A
  build that asks for neither compiles the library as written.
- `test/unit/` holds the properties, the specified points and an
  `expect` literal. `test_stats` runs with `--corrected`, so a stale
  literal shows as a diff and `dune promote` accepts it (see
  [Baselines](../../doc/manual/baselines.md#running-expectations-under-dune)).
- `test/failures/` holds one suite per open issue, its test under
  `xfail`.
- `test/expect/` is a library with `(inline_tests)`, and a stale
  `[%expect]` is accepted with `dune promote` (see
  [Writing expect tests inside a library](../../doc/manual/baselines.md#writing-expect-tests-inside-a-library)).
- `test/cram/` depends on `bin/main.exe`, and dune rebuilds the binary
  before a session runs.
- `test/dune` holds the two aliases. Each runs every suite under
  `test/`, then merges what the suites wrote.

## Copying the project out

Copy the directory and rename the package in `dune-project`. The
project depends on `windtrap` and `ppx_windtrap`, installed with opam as
the repository's [README](../../README.md#installation) shows. To build
against a checkout of windtrap with dune's package management instead,
add a pin to `dune-project` with the checkout's path:

```lisp
(pin
 (url "git+file:///path/to/windtrap")
 (package
  (name windtrap))
 (package
  (name ppx_windtrap)))
```

Then lock the dependencies once:

```
dune pkg lock
```

The commands below run from the copy's root. Their transcripts were
captured there, and the timings and seeds differ on every run.

## Running the tests

`dune runtest` runs every suite, the inline tests and the cram
sessions. Each suite prints its line as it ends, and a passing cram
session prints nothing:

```
$ dune runtest
issue-1: 1 expected failure in 0.5ms.
stats: 4 passed in 86ms (seed s1:8faef35f5af54995).
slug: 9 passed in 17ms (seed s1:5285379db091fe55).
windtrap_example_blueprint_expect/expect_slug.ml: 1 passed in 0.6ms.
```

To run one directory, name it, as in `dune runtest test/unit`. The two
stanzas of `test/unit/dune` declare `WINDTRAP_SEED` and
`WINDTRAP_PROP_COUNT` as dependencies, and a seed set in the
environment reruns them on a built tree, as in
`WINDTRAP_SEED=s1:4244aeac53c8f09d dune runtest test/unit`. To pass
flags to one suite, run its executable with `dune exec`, as the
sections below do. [Running tests](../../doc/manual/running-tests.md)
covers the flags and their mirrors.

## Measuring coverage

Every build of the copy is instrumented, so every test run writes its
coverage dump. The `cover` alias runs the suites and merges their dumps,
and fails below 80% (see
[Coverage](../../doc/manual/coverage.md#measuring-in-one-command)):

```
$ dune build @cover
   cover    points   file           uncovered lines (-u shows the source)
  100.0%    27/27    lib/slug.ml
  100.0%    18/18    lib/stats.ml
coverage: 100.0% (45/45 points), minimum 80%: ok
```

## Testing the mutants

The `mutate` alias runs each suite on its mutants and merges the
verdicts. `WINDTRAP_MUTATE=1`, the mirror of `--mutate`, reaches every
suite, and `--force` reruns the suites dune has cached:

```
WINDTRAP_MUTATE=1 dune build @mutate --force
```

Each suite prints its mutation report as it ends. The expect library
reaches mutants in `lib/slug.ml` that only `test/unit` pins, and lists
them. The known-bug suite's one test is under `xfail`, which reaches no
mutant, so its report lists the lines of `lib/slug.ml` as never reached
(see [What a mutation run runs](../../doc/manual/mutation.md#what-a-mutation-run-runs)).
The merge counts a mutant killed when any
executable killed it, and prints the alias's last line. `windtrap
mutants` prints the merge again:

```
$ dune exec windtrap -- mutants
mutants: 18 reached, 18 killed, 4 executables
```

The alias fails when a mutant survived every executable that reached it
(see [Mutation testing](../../doc/manual/mutation.md#mutation-testing-a-project)).

## The known bug

`slugify` drops UTF-8 letters: `"Café"` gives `"caf"`. The test of
`test/failures/issue_1.ml` asserts the fixed behaviour under
`xfail ~reason:"issue #1"`, and its failure counts as expected. `-v`
prints it:

```
$ dune exec test/failures/issue_1.exe -- -v
issue-1: 1 test
  XFAIL  keeps UTF-8 letters                       0.1ms (expected failure: issue #1)
    test/failures/issue_1.ml:13
      13 │ (test "keeps UTF-8 letters" (fun () ->

    expected  "caf\195\169"
    actual    "caf"

1 expected failure in 1.0ms.
```

When a change fixes the bug, the test passes and the suite fails. The
test then moves to `test/unit/` as a regression test, and `issue_1`
leaves `test/failures/` (see
[Keeping a known bug in the suite](../../doc/manual/resources-and-structure.md#keeping-a-known-bug-in-the-suite)).

## The dismissed mutant

`clamp` in `lib/stats.ml` maps a negative count to zero:

```ocaml
let clamp n =
  if (n > 0) [@mutate off "both arms yield zero marks at 0"] then n else 0
```

The mutant `n > 0 → n >= 0` differs from the original at zero only,
where both branches return 0. No test can kill it, and the attribute
dismisses the site with its reason. The site is neither tested nor
counted, and the stats suite reaches two mutants, both in `render` (see
[Dismissing an equivalent mutant](../../doc/manual/mutation.md#dismissing-an-equivalent-mutant)).

## The weak law

The property `prints one line per row plus the total`, in
`test/unit/test_stats.ml`, counts lines. A mutant that changes the
arithmetic inside a line keeps the count. Mutation testing the property
alone shows the two mutants it reaches and does not kill:

```
$ dune exec test/unit/test_stats.exe -- --mutate -f 'one line per row'
stats: 1 passed in 90ms (seed s1:4244aeac53c8f09d).

─────────────────────── survivors ────────────────────────
  SURVIVED  lib/stats.ml:14:18:add  width - (String.length label) → width + (String.length label)
      14 │ ^ String.make (width - String.length label) ' '

    1 test ran this line and did not fail:
      render › prints one line per row plus the total  test/unit/test_stats.ml:29

  SURVIVED  lib/stats.ml:18:48:sub  acc + (clamp n) → acc - (clamp n)
      18 │ let total = List.fold_left (fun acc (_, n) -> acc + clamp n) 0 rows in

    1 test ran this line and did not fail:
      render › prints one line per row plus the total  test/unit/test_stats.ml:29
──────────────────────────────────────────────────────────

reproduce: dune exec --instrument-with ppx_windtrap.mutate test/unit/test_stats.exe -- --arm lib/stats.ml:14:18:add -f 'one line per row'
windtrap: verdicts not saved: this run's selection narrows the suite, and a partial run's verdicts would stand in the project merge as the whole.
mutants: 2 survived of 2 reached by the 1 selected test
```

The three tests beside it pin the rendered text and kill both, and the
suite's whole run reports no survivor:

```
$ dune exec test/unit/test_stats.exe -- --mutate
stats: 4 passed in 107ms (seed s1:4fa1648bcfa77e8a).
mutants: 2 reached by this suite, 2 killed
```

To judge a new test the same way, pass its name to `-f` with
`--mutate`.

## Inside windtrap's repository

In windtrap's repository the commands run from the repository's root,
and each path gains `examples/x-blueprint/`, as in
`dune runtest examples/x-blueprint`. The `dune-workspace` file is
inert there, and an instrumented command passes `--instrument-with`.

The flag also instruments windtrap's own library, which every suite
links. A mutation run there names the example's library as its prefix,
as `--mutate=examples/x-blueprint/lib` or as the value of
`WINDTRAP_MUTATE`:

```
WINDTRAP_MUTATE=examples/x-blueprint/lib dune build @examples/x-blueprint/test/mutate --force --instrument-with ppx_windtrap.mutate
```

A run under a prefix keeps the verdicts that an earlier run of the same
build saved for other files, and the merge counts them. To drop them,
delete `_build/_mutants` before the run. A coverage report takes no
prefix, and
`dune build @examples/x-blueprint/test/cover --instrument-with ppx_windtrap.coverage`
lists windtrap's library too and fails the 80% minimum.
