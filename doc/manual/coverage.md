# Coverage

Coverage is one stanza on the library you want measured. It is inert
without the flag — zero overhead in normal builds — so it is committed
once and forgotten:

```lisp
(library
 (name mylib)
 (instrumentation
  (backend ppx_windtrap)))
```

The percentage appears in the run you already make; no second command:

```
$ dune runtest --instrument-with ppx_windtrap
calc: 4 passed in 0.00176s.
coverage: 77.8% (7/9 points) · WINDTRAP_COVERAGE=report for detail
```

Two rules keep it honest. Coverage never changes what programs or
tests mean: instrumentation only counts, and enabling it never alters
test outcomes, counts, or exit codes. And coverage data is transient:
`.coverage` files live under `_build/_coverage` only, deterministically
named per executable and overwritten on re-run — nothing to commit,
nothing to go stale silently.

## Report modes

`WINDTRAP_COVERAGE` (or `--coverage` on the executable) selects the
rendering — `summary` (the default one-liner), `report`, `full`, or
`off`:

```
$ WINDTRAP_COVERAGE=report dune runtest --instrument-with ppx_windtrap
...
coverage: 77.8% (7/9 points)
   77.8%  7/9  lib/calc.ml   uncovered: 9-10
```

`full` renders the uncovered points as source excerpts — the most
useful mode for finding the missing test:

```
lib/calc.ml — 77.8% (7/9)

      8 │   | Add -> a + b
  ▌   9 │   | Sub -> a - b
  ▌  10 │   | Mul -> a * b
     11 │   | Div -> if b = 0 then invalid_arg "division by zero" else a / b
```

## What is measured

Coverage is measured at expression grade, Bisect_ppx's model: points
are the places where execution chooses — function bodies, `match`/`try`
arms and guards, `if` branches, `&&`/`||` condition arms, loop, `lazy`,
and letop bodies, class bodies, toplevel bindings — plus application
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

## CI: `windtrap coverage`

Each instrumented test executable writes its own `.coverage` file at
exit. The `windtrap coverage` command finds them under
`_build/_coverage`, merges them — loudly rejecting files from stale or
mismatched builds — and reports over the whole suite:

```
$ dune exec windtrap -- coverage --min 80
coverage: 80.0% (24/30 points)
   77.8%   7/9   lib/calc.ml    uncovered: 9-10
   77.8%   7/9   lib/eval.ml    uncovered: 5, 10
   83.3%  10/12  lib/lexer.ml   uncovered: 6, 8
minimum 80.0%: ok
```

`--min` exits 1 with a message when total coverage falls below the
threshold — the CI gate lives here, never in the test run itself.
Explicit `PATH` arguments (`.coverage` files, or directories searched
recursively) replace the default search; naming a file that does not
exist or lacks the `.coverage` suffix is a loud error naming the path,
never a silent fall-through to the no-data report.
`--json` prints a machine-readable document (per-file percentages,
uncovered lines and ranges) on standard output for dashboards and
diff-coverage tooling.

## Several test stanzas

Each instrumented test executable reports its own percentage — its view
of the code *it* links. The linker drops modules a binary never
references, so two stanzas over one library print different
denominators, and per-executable numbers never sum or average. When
windtrap detects other executables' data it says so inline:
`coverage: 52.4% (11/21 points, this executable) · project: dune build @cover`.

The project number is the merge. Add one rule, once, at the project
root:

```lisp
(rule
 (alias cover)
 (deps (alias_rec runtest) (universe))
 (action (run %{bin:windtrap} coverage --min 80)))
```

`dune build @cover --instrument-with ppx_windtrap` runs every
out-of-date stanza, then merges every executable's data and prints the
project table; `--min` makes the alias your CI gate (test runs
themselves never fail on coverage). `(universe)` is load-bearing: the
`.coverage` files are not declarable dependencies, so it tells dune to
re-run the milliseconds-cheap aggregate on every build. Drop `--min` if
you only want the report.

Project coverage is defined over instrumented, linked code: the union of
every executable's point tables, counts added per point. Libraries
without the instrumentation stanza, code under `[@coverage off]`, and
modules no test executable links are absent from the denominator — not
reported as 0%.

Each dump records the executable that wrote it (its `_build`-relative
path and content digest). The report excludes, with a warning, dumps
whose executable was deleted (orphans) or rebuilt since the dump —
typically a re-run made without `--instrument-with`, or a dune cache hit
replaying an older binary; the warning names the remedy
(`dune build @cover --force --instrument-with ppx_windtrap`).
`--stale include|exclude|fail` overrides (default `exclude`). Foreign
format versions fail with a delete instruction: re-running never removes
stale-named files.

### Declared-output aggregation (escape hatch)

To make dumps ordinary build targets — pure dune dataflow, no
`(universe)` — set `WINDTRAP_COVERAGE_FILE` (relative paths resolve
against the action's cwd at first registration) and declare the target:

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
without it every file renders `(source not found)`.

Prices: tests run once for `@runtest` and once for capture, one capture
rule per stanza, and the build fails when run uninstrumented (no dump is
produced). The default side-channel recipe above is the right choice
unless you need the dump as a declared artifact.

For HTML reports or Coveralls upload, use Bisect_ppx — windtrap's
coverage is deliberately the 90% product: one number after every run,
and the exact arms you forgot to test.
