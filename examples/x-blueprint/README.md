# Blueprint project

This directory is a project layout to copy: a small library, its
binary, and a `test/` tree with one kind of test per directory. It
also keeps a known bug, a dismissed mutant and a weak law, each shown in
its own section below.

## The layout

```
dune-project        the package, its dependencies, cram enabled
lib/                Slug and Stats, the library under test
bin/                main.exe, a command line over Slug
test/unit/          one suite per module: test_slug, test_stats
test/failures/      one suite per known bug: issue_1
test/expect/        let%expect_test over Slug, in a library
test/cram/          sessions of main.exe
```

- `lib/dune` names both backends, `ppx_windtrap.coverage` and
  `ppx_windtrap.mutate`, in `instrumentation` fields. A build that asks
  for neither compiles the library as written.
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

A run built with `--instrument-with ppx_windtrap.coverage` writes a
coverage dump from every suite, and `windtrap coverage` merges the
dumps. `--min 80` makes it exit 1 below 80% (see
[Coverage](../../doc/manual/coverage.md#failing-a-build-below-a-minimum)):

```
$ dune runtest --instrument-with ppx_windtrap.coverage
stats: 4 passed in 139ms (seed s1:86285a8e0eb4e567).
slug: 9 passed in 42ms (seed s1:c8e2d1bdbbcbb528).
issue-1: 1 expected failure in 0.5ms.
windtrap_example_blueprint_expect/expect_slug.ml: 1 passed in 0.7ms.
$ dune exec windtrap -- coverage --min 80
   cover    points   file           uncovered lines (-u shows the source)
  100.0%    25/25    lib/slug.ml
  100.0%    17/17    lib/stats.ml
coverage: 100.0% (42/42 points), minimum 80%: ok
```

## Testing the mutants

A run built with `--instrument-with ppx_windtrap.mutate` under
`WINDTRAP_MUTATE=1`, the mirror of `--mutate`, tests each suite's
mutants. Dune does not track the variable, and `--force` runs the
suites it has cached (see
[Mutation testing a project](../../doc/manual/mutation.md#mutation-testing-a-project)):

```
WINDTRAP_MUTATE=1 dune runtest --force --instrument-with ppx_windtrap.mutate
```

Each suite prints its mutation report as it ends. The expect library
reaches mutants in `lib/slug.ml` that only `test/unit` pins, and lists
them. The known-bug suite's one test is under `xfail`, which reaches no
mutant, so its report lists the lines of `lib/slug.ml` as never reached
(see [What a mutation run runs](../../doc/manual/mutation.md#what-a-mutation-run-runs)).
`windtrap mutants` merges the verdicts, and counts a mutant killed when
any executable killed it:

```
$ dune exec windtrap -- mutants
mutants: 18 reached, 18 killed, 4 executables
```

It exits 1 when a mutant survived every executable that reached it.

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
$ dune exec --instrument-with ppx_windtrap.mutate test/unit/test_stats.exe -- --mutate -f 'one line per row'
stats: 1 passed in 91ms (seed s1:f57d4071ad6eed49).

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
$ dune exec --instrument-with ppx_windtrap.mutate test/unit/test_stats.exe -- --mutate
stats: 4 passed in 110ms (seed s1:7eec2ad33b9c5707).
mutants: 2 reached by this suite, 2 killed
```

To judge a new test the same way, pass its name to `-f` with
`--mutate`.

## Inside windtrap's repository

In windtrap's repository the commands run from the repository's root,
and each path gains `examples/x-blueprint/`, as in
`dune runtest examples/x-blueprint`.

There `--instrument-with` also instruments windtrap's own library, which
every suite links. A mutation run there names the example's library as
its prefix, as `--mutate=examples/x-blueprint/lib` or as the value of
`WINDTRAP_MUTATE`:

```
WINDTRAP_MUTATE=examples/x-blueprint/lib dune runtest examples/x-blueprint --force --instrument-with ppx_windtrap.mutate
dune exec windtrap -- mutants
```

A run under a prefix keeps the verdicts that an earlier run of the same
build saved for other files, and the merge counts them. To drop them,
delete `_build/_mutants` before the run. A coverage report takes no
prefix. After
`dune runtest examples/x-blueprint --instrument-with ppx_windtrap.coverage`,
`dune exec windtrap -- coverage --min 80` lists windtrap's library too
and fails the 80% minimum.
