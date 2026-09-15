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
  unit/         THE windtrap suite: laws, examples, expect literals —
                one test file per source module, one test stanza per
                file, each file its own run; a stanza with baselines
                runs with --corrected so dune promote accepts them
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
WINDTRAP_MUTATE=1 WINDTRAP_MUTATE_ONLY=examples/x-blueprint/lib/slug.ml \
  dune exec --instrument-with ppx_windtrap.mutate \
  examples/x-blueprint/test/unit/test_slug.exe        # the mutation loop
WINDTRAP_MUTATE=1 WINDTRAP_MUTATE_ONLY=examples/x-blueprint/lib \
  dune build @examples/x-blueprint/test/example-mutate \
  --force --instrument-with ppx_windtrap.mutate       # the project aggregate
```

The two aggregate aliases are sugar over two commands each — the
instrumented run of every suite, then `dune exec windtrap -- coverage
--min 80` or `dune exec windtrap -- mutants` over what the suites
wrote; the manual chapters teach the two-command form first.

Writing a new test here ends with the mutation loop: filter the survey
to it, and the report says which of the faults it reaches it lets
through. The deliberately weak law in `test_stats.ml` (the fourth
deliberate thing below) shows what that looks like:

```
$ WINDTRAP_MUTATE=1 WINDTRAP_MUTATE_ONLY=examples/x-blueprint/lib \
    dune exec --instrument-with ppx_windtrap.mutate \
    examples/x-blueprint/test/unit/test_stats.exe -- -f "one line per row"
stats: 1 passed in 0.0444s (seed s1:9af2e80ab07716a2).

─────────────────── survivors (2) ────────────────────

  SURVIVED  examples/x-blueprint/lib/stats.ml:14:18:add   width - (String.length label)  →  width + (String.length label)
      14 │     ^ String.make (width - String.length label) ' '

    1 test ran this line and did not fail:
      render › prints one line per row plus the total      examples/x-blueprint/test/unit/test_stats.ml:28

  …

──────────────────────────────────────────────────────

mutants: 2 survived of 2 reached by the 1 selected test
reproduce: WINDTRAP_MUTATE_ARM=<id> dune exec --instrument-with ppx_windtrap.mutate examples/x-blueprint/test/unit/test_stats.exe -- -f 'one line per row'
verdicts not saved: this run's selection narrows the suite, and a partial run's verdicts would stand in the project merge as the whole.
```

A filtered run exits 0 whatever it finds and writes no verdicts, so you
can probe one test all afternoon without disturbing what
`example-mutate` merges. The `WINDTRAP_MUTATE_ONLY` prefix is for this
tree only: here windtrap's own library carries the backend too, so an
unscoped run reaches the framework's sites as well; copied out,
windtrap is an ordinary uninstrumented dependency and the prefix can
go.

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
WINDTRAP_MUTATE=1 dune build @example-mutate --force  # the project aggregate
```

Measured on a copy pinned to this tree: the whole suite runs in under
three seconds with both backends on, and every suite's transcript ends
with the coverage percentage — a standing reminder of the verdicts a
green run has not yet earned. The built programs mean exactly what
they meant uninstrumented: marks only count, and a mutant changes
meaning only in a forked child that armed it.

Four things are deliberate:

- **The aliases are named `example-cover` / `example-mutate`.** In
  your own project they are `cover` and `mutate` — the names windtrap's
  manual, skill, and own root `dune` use for the folded form of the two
  commands. They are renamed here only because this example lives
  inside windtrap's tree, where those aliases are recursive and already
  mean the project's own aggregate.
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
  the mutation denominator. The aggregate above reports every reached
  mutant killed — the suites here practice the discipline the skill
  teaches.

- **`test_stats.ml` keeps one deliberately weak law.** The line-count
  property cannot fail under either arithmetic fault in `lib/stats.ml`
  — no arithmetic inside a line moves a line count — so the survey
  filtered to it reports both as survivors (the transcript above), and
  the comment above the law says why it stays. The tests beside it
  kill both faults, which is the point: one test's survivor is another
  test's kill, and the suite — and the project aggregate — still
  report every mutant killed. In your project the remedy for that
  transcript is a stronger law; here that would orphan the transcript
  above, so the exercise is left to the reader.

A stateful suite slots into `unit/` the same way (see
`examples/10-stateful`); this example keeps the surface small.
