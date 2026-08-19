# The canonical windtrap project layout

A complete, copyable instance of the layout the windtrap skill teaches
(`SKILL.md` §3 at the repository root): one small library, its binary,
and a `test/` tree where **every child is one suite with its own
`dune` file**, split along mechanical and lifecycle boundaries — never
by test kind.

```
dune-project    the project: cram enabled, one package depending on
                windtrap and ppx_windtrap
dune-workspace  instrumentation on by default — inert in this tree,
                active the moment the directory is copied out
lib/            the code under test — two inert instrumentation
                stanzas, no PPX, no test code
bin/            a tiny CLI over the library (what cram/ tests)
test/
  dune          the project verdict aliases (see the rename note below)
  unit/         THE windtrap suite: laws, examples, snapshots — one
                test file per source module, one test stanza per file,
                each file its own run
  failures/     the known-bug backlog: one xfail suite per issue
  expect/       expect tests, as a library with (inline_tests) —
                no test code lives in lib/
  cram/         blackbox tests of the binary: exit codes and output
```

Things to try from the repository root (in windtrap's own tree the
workspace file above is inert, so these carry the `--instrument-with`
flag the standalone copy never needs):

```
dune runtest examples/x-blueprint                 # every suite
dune runtest examples/x-blueprint/test/failures   # just the bug backlog
dune build @examples/x-blueprint/test/example-cover \
  --instrument-with ppx_windtrap.coverage         # coverage, gated
dune build @examples/x-blueprint/test/example-admit \
  --instrument-with ppx_windtrap.mutate           # can every slug test fail?
WINDTRAP_MUTATE=1 WINDTRAP_MUTATE_ONLY=examples/x-blueprint/lib/slug.ml \
  dune exec --instrument-with ppx_windtrap.mutate \
  examples/x-blueprint/test/unit/test_slug.exe        # the mutation loop
```

Writing a new test here ends the way the skill teaches (`SKILL.md` §9):
admit it, and let the run name the fault it kills.

```
$ WINDTRAP_MUTATE=admit dune exec --instrument-with ppx_windtrap.mutate \
    examples/x-blueprint/test/unit/test_slug.exe -- -f idempotent
slug: 1 passed in 0.0221s (seed s1:cd98c762bb757a06).

  ADMITTED  slugify › is idempotent
    killed  examples/x-blueprint/lib/slug.ml:2:3:gt   c >= 'a'  →  c > 'a'

admission: 1 admitted of 1 · 2 forks over 62 reached in 77ms (seed s1:cd98c762bb757a06)
```

`UNJUSTIFIED` would mean the test cannot fail, and exits 1; `NO SITES`
means mutation has nothing to say about that subject. Nothing is written
to `_build/_mutants`, so admitting a test never disturbs the verdicts
`example-mutate` merges.

Drop the `-f` and the run judges every test it executes instead of the
ones a filter names — which is the whole reason an alias can carry it.
`@example-admit` is that command over `test_slug.exe`, and it reports
`9 admitted of 9 · 2 forks` here. It carries no `WINDTRAP_MUTATE_ONLY`,
so inside windtrap's tree the framework's own sites are in reach too:
the same run scoped to `examples/x-blueprint/lib` reports `8 admitted,
1 no sites` in three forks, which is what the alias sees once this
directory is copied out and windtrap is an uninstrumented dependency.
No prefix is right in both places, and wide is the safe direction —
every ruling is still true about the test it names.

Its sibling `test_stats.exe` is left out of the alias on purpose:
scoped that way, admitting it exits 1 by design. The fourth deliberate
thing below says why that red is the point rather than a defect.

## Copied out: the workspace posture

This directory is a complete project — it carries its own
`dune-project` (a nested project composes into windtrap's workspace
here, and stands alone the moment it leaves). Copy the directory,
rename the package, and until windtrap is on your package repository,
point the dependencies at a checkout by appending one pin to
`dune-project`:

```lisp
(pin
 (url "git+file:///path/to/windtrap")
 (package (name windtrap))
 (package (name ppx_windtrap)))
```

Outside this tree the shipped `dune-workspace` activates, and **every
`--instrument-with` flag in this README disappears** — the workspace
declares once what each command was repeating:

```
dune runtest                                          # every suite
dune build @example-cover                             # coverage, gated
WINDTRAP_MUTATE=1 WINDTRAP_MUTATE_ONLY=lib/slug.ml \
  dune exec test/unit/test_slug.exe                   # the mutation loop
WINDTRAP_MUTATE=admit \
  dune exec test/unit/test_slug.exe -- -f idempotent  # admit a test
dune build @example-admit                             # admit a whole suite
dune build @example-mutate                            # merge verdicts
```

Measured on a copy pinned to this tree: the whole suite runs in under
three seconds with both backends on, `admit` answers in tens of
milliseconds, and every suite's transcript ends with the two discovery
lines — the coverage percentage and the mutant count — standing
reminders of the verdicts a green run has not yet earned. The built
programs mean exactly what they meant uninstrumented: marks only
count, and a mutant changes meaning only in a forked child that armed
it.

Four things are deliberate:

- **The aliases are named `example-cover` / `example-mutate` /
  `example-admit`.** In your own project they are `cover`, `mutate` and
  `admit` — the names windtrap's manual, skill, and own root `dune`
  use. They are renamed here only because this example lives inside
  windtrap's tree, where those aliases are recursive and already mean
  the project's own aggregate.
- **Issue #1 is a real, intentional bug.** `Slug.slugify` treats UTF-8
  letters as separators (`"Café"` → `"caf"`, not `"café"`).
  `test/failures/issue_1.ml` keeps the reproduction running as an
  `xfail`: the backlog suite stays green while the bug exists and goes
  loudly red the day a change fixes it — at which point the test moves
  into `test/unit/test_slug.ml` as a regression test and the issue
  module is deleted.

- **`lib/stats.ml` carries a dismissed mutant.** `clamp`'s `n > 0`
  and its mutant `n >= 0` agree at zero, so no test can distinguish
  them; the `[@mutate off "reason"]` attribute records that reasoning
  in the source, where `git blame` keeps it, and drops the site from
  the mutation denominator. Both scoped loops above report
  `0 survived` — the suites here practice the discipline the skill
  teaches.

- **`test_stats.ml` keeps one deliberately weak law.** The
  line-count property cannot fail under either arithmetic fault in
  `lib/stats.ml`, so a scoped admission run rules it `UNJUSTIFIED` and
  exits 1 — it is the manual's living specimen ("Admitting a test"),
  and the comment above it says so. See the ruling itself with

  ```
  WINDTRAP_MUTATE=admit WINDTRAP_MUTATE_ONLY=examples/x-blueprint/lib \
    dune exec --instrument-with ppx_windtrap.mutate \
    examples/x-blueprint/test/unit/test_stats.exe   # 3 admitted, 1 unjustified
  ```

  In a real project that ruling is stop-the-line: strengthen the law
  (here, that would orphan the manual's transcripts, so the exercise is
  left to the reader — and the faults it misses are killed by the
  example tests beside it, so the survey still reports `0 survived`).
  It is also why `@example-admit` names only `test_slug.exe`: a
  scaffold whose alias is red on the day it is copied teaches that a
  red alias is normal, which is the opposite of what `UNJUSTIFIED`
  means. Yours names every unit executable, because nothing in yours is
  a specimen.

A stateful suite slots into `unit/` the same way (see
`examples/10-stateful`); this example keeps the surface small.
