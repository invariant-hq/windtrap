# Coverage

Coverage is one stanza on the library you want measured. It is inert
without the flag — zero overhead in normal builds — so it is committed
once and forgotten:

```lisp
(library
 (name mylib)
 (instrumentation
  (backend ppx_windtrap.coverage)))
```

Coverage is a run and a merge: instrumented test executables write
their data at exit, and `windtrap coverage` — the one coverage
reporter — merges what they wrote and reports over the whole suite. A
test run prints no number of its own.

Two rules keep it honest. Coverage never changes what programs or
tests mean: instrumentation only counts, and enabling it never alters
test outcomes, counts, or exit codes. And coverage data is transient:
`.coverage` files live beside the build directory's contexts, under
`_build/_coverage`, in one directory per executable, where every run
adds a file of its own and the first run of a rebuilt executable
removes its predecessors' — nothing to commit, nothing to go stale
silently. Because every run keeps its file, a binary run several
times — a command-line tool driven by a cram test — is measured across
every invocation, not just the last.

## Two commands

The instrumented run, then the merge:

```
$ dune runtest --force --instrument-with ppx_windtrap.coverage
$ dune exec windtrap -- coverage --min 80
coverage: 80.0% (24/30 points)
   77.8%   7/9   lib/calc.ml    uncovered: 9-10
   77.8%   7/9   lib/eval.ml    uncovered: 5, 10
   83.3%  10/12  lib/lexer.ml   uncovered: 6, 8
minimum 80%: ok
```

`--force` re-runs every suite, so every dump describes the build you
are looking at; without it dune replays cached test actions, and a
dump from an earlier state of the tree is excluded from the merge
rather than merged (see below). The flag can go: declare the backend
once in `dune-workspace` and every command in this chapter loses its
`--instrument-with`:

```lisp
(lang dune 3.0)

(context
 (default
  (instrument_with ppx_windtrap.coverage)))
```

The commands then read `dune runtest --force` and `dune exec windtrap
-- coverage --min 80`, and that is the whole product: the merged table,
the exact arms you forgot to test, a gate for CI, and an LCOV tracefile
for everything else.

## What is measured

Coverage is measured at expression grade, Bisect_ppx's model: points
are the places where execution chooses — function bodies and
optional-argument defaults, `match`/`try` arms and guards, `if` branches,
`&&`/`||` condition arms, loop, `lazy`, and letop bodies, class bodies,
toplevel bindings — plus application
out-edges, which fire only when the call *returns*. Out-edges are what make the number
truthful in exception-heavy OCaml: a call that raises leaves its point
unvisited, so raising paths show up as uncovered instead of being
painted green for having been entered.

Exclude code explicitly with Bisect_ppx's spelling: `[@coverage off]`
on an expression, `[@@coverage off]` on a value or module binding,
`[@@@coverage off]` / `[@@@coverage on]` around a region of structure
items, `[@@@coverage exclude_file]` for the whole file. An uncovered error
branch is a missing test; an uncovered debug helper is what
`[@coverage off]` is for. Chase uncovered branches, not a percentage.

## `windtrap coverage`

The command finds the `.coverage` files under the build directory's
`_coverage` (or, in a tree built without one, `_windtrap/coverage`),
walking up from the current directory to the project root, merges them
— loudly rejecting files from foreign or mismatched builds — and
renders the per-file table above. Under `dune exec` the build directory
is the one dune names, so a private `--build-dir` reports its own
dumps. It runs no tests and drives no build.

`--min` exits 1 with a message when total coverage falls below the
threshold — the CI gate lives here, never in the test run itself.
`--expect PATH` is the other gate: every `.ml`, `.mll` and `.mly`
under `PATH` (a directory, walked recursively; or a single file) must
have coverage data, or the command names each one that has none and
exits 1. That closes the hole the denominator cannot show — a library
without the stanza, a module no test executable links, a test nobody
ran since the rebuild. `--do-not-expect PATH` exempts a file or a
directory. Paths are relative to the current directory, the project
root under `dune exec`; dune's `foo.pp.ml` twins and a lexer's `.mll`
count as the module they produce.
Explicit `PATH` arguments (`.coverage` files, or directories searched
recursively) replace the default search; naming a file that does not
exist or lacks the `.coverage` suffix is a loud error naming the path,
never a silent fall-through to the no-data report.
`--json` prints a machine-readable document (per-file percentages and
uncovered lines) on standard output for dashboards and diff-coverage
tooling. `--lcov` prints an LCOV tracefile instead — the format Codecov,
Coveralls, GitLab and editor coverage gutters consume, and what
`genhtml` turns into an HTML report:

```
$ dune exec windtrap -- coverage --lcov > lcov.info
$ genhtml lcov.info -o _coverage
```

A line's hit count is the fewest visits of any point touching it, so a
line holding an untested arm or a call that never returned reads as 0.
Paths are project-relative, so run it from the project root; a file
whose source is missing or has changed is omitted and named on stderr.
Under either format `--min` still gates, and its verdict moves to
stderr so standard output stays the artifact.

`-u` (`--show-uncovered`) adds the uncovered points as source
excerpts — the fastest way from a percentage to the missing test:

```
$ dune exec windtrap -- coverage -u
...
lib/calc.ml — 77.8% (7/9)

      8 │   | Add -> a + b
  ▌   9 │   | Sub -> a - b
  ▌  10 │   | Mul -> a * b
     11 │   | Div -> if b = 0 then invalid_arg "division by zero" else a / b
```

A line is marked when it intersects any unvisited point's extent, so a
one-line `function A -> 1 | B -> 2` with only `A` exercised reads as
uncovered: marking only lines wholly inside unvisited extents would
hide the untested arm. The percentages count points, not lines, so they
are unaffected.

## Several test stanzas

Each instrumented test executable reports its own view of the code
*it* links. The linker drops modules a binary never references, so two
stanzas over one library have different denominators, and
per-executable numbers never sum or average. The project number is the
merge: the union of every executable's point tables, counts added per
point. Libraries without the instrumentation stanza, code under
`[@coverage off]`, and modules no test executable links are absent
from the denominator — not reported as 0%. `--expect lib/` is what
turns that absence into a failure.

Each dump records the executable that wrote it: its `_build`-relative
path and a digest of its bytes — a digest rather than a timestamp,
because dune's cache restores rebuilt artifacts with their original
mtimes, so time cannot tell a rebuilt executable from the one that
wrote the dump. The report excludes, with one warning line
per file, dumps whose executable was deleted or rebuilt since the dump
— typically a rebuild without the backend, or a cached test action the
build tool did not re-run — and then says, once, what heals it:
re-run the suite instrumented, forcing runs your build tool cached,
then merge again; delete the `_coverage` directory to drop leftovers
of removed executables. There is no override: a total computed from a
dump that describes another build can only mislead. Foreign format
versions fail with a delete instruction: re-running never removes
stale-named files.

## Without dune

Any build can instrument: the backend is a Ppxlib rewriter, so a
driver linked against it once — `let () = Ppxlib.Driver.standalone ()`
with `ppxlib` and `ppx_windtrap.coverage` — is a `-ppx` for the
compiler, and the installed `windtrap` is two archives beside the
compiler's own library (`$lib` below, where `META` is). Instrument the
library under test, not the test file; link the test against
`windtrap`; run it; merge with the installed binary. `test/cli/nodune.t`
in windtrap's tree is this session, held by a test:

```
$ ocamlopt -ppx "./coverage_ppx.exe --as-ppx" -I "$lib/windtrap/runtime" -c calc.ml
$ ocamlopt -I +unix -I "$lib/windtrap/runtime" -I "$lib/windtrap" \
    unix.cmxa windtrap_runtime.cmxa windtrap.cmxa calc.cmx test_calc.ml -o test_calc.exe
$ ./test_calc.exe
calc: 4 passed in 0.0002s.
$ windtrap coverage --min 50
coverage: 85.7% (6/7 points)
   85.7%  6/7  calc.ml   uncovered: 3
minimum 50%: ok
```

An executable under no build directory dumps under the working
directory's own `_windtrap/coverage` — a tree built without dune never
grows a `_build` — and `windtrap coverage` finds that directory by
walking up from wherever it runs, exactly as it finds a build
directory's `_coverage`. `WINDTRAP_COVERAGE_FILE=path` sends one run's
dump to an explicit file instead (relative paths resolve against the
working directory at the first registration; the file is replaced on
every run), which is also how a build rule declares the dump as its
target.

## One command, if you want it

The two commands above fold into one alias. Add it once, at the
project root:

```lisp
(rule
 (alias cover)
 (deps (alias_rec runtest) (universe))
 (action (run %{bin:windtrap} coverage --min 80)))
```

`dune build @cover --instrument-with ppx_windtrap.coverage` runs every
out-of-date stanza, then merges every executable's data and prints the
project table; `--min` makes the alias your CI gate (test runs
themselves never fail on coverage). `(universe)` is load-bearing: the
`.coverage` files are not declarable dependencies, so it tells dune to
re-run the milliseconds-cheap aggregate on every build. Drop `--min` if
you only want the report, and add `--force` when a cached test action
must be re-run.

To make dumps ordinary build targets instead — pure dune dataflow, no
`(universe)` — set `WINDTRAP_COVERAGE_FILE` and declare the target:

```lisp
(rule
 (targets test_a.coverage)
 (deps test_a.exe (sandbox always))
 (action (setenv WINDTRAP_COVERAGE_FILE test_a.coverage (run ./test_a.exe))))

(rule
 (alias cover)
 (action (chdir %{workspace_root}
  (run %{bin:windtrap} coverage
   %{dep:test_a.coverage} %{dep:test_b.coverage}))))
```

`%{dep:…}` and the `chdir` are each load-bearing: the pform declares
the dependency and keeps the path valid across the `chdir` (inside an
action `%{workspace_root}` is the build-context root, where a plain
`test_a.coverage` names nothing and the command fails loudly naming
the missing path), and the `chdir` is what lets the
report resolve the workspace-relative source paths the dumps record —
without it every file renders `(source not found)`. An explicit file
is one run's data: it is replaced, where the default directory
accumulates.

Prices: tests run once for `@runtest` and once for capture, one capture
rule per stanza, `(inline_tests)` libraries cannot take part (dune
drives their runner and offers no target), and the build fails when
run uninstrumented (no dump is produced). The two commands at the top
of this chapter are the right choice unless you need the dump as a
declared artifact.
