# The canonical windtrap project layout

A complete, copyable instance of the layout the windtrap skill teaches
(`SKILL.md` §3 at the repository root): one small library, its binary,
and a `test/` tree where **every child is one suite with its own
`dune` file**, split along mechanical and lifecycle boundaries — never
by test kind.

```
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

Things to try from the repository root:

```
dune runtest examples/x-blueprint                 # every suite
dune runtest examples/x-blueprint/test/failures   # just the bug backlog
dune build @examples/x-blueprint/test/example-cover \
  --instrument-with ppx_windtrap                  # coverage, gated
WINDTRAP_MUTATE=1 WINDTRAP_MUTATE_ONLY=examples/x-blueprint/lib/slug.ml \
  dune exec --instrument-with ppx_windtrap.mutate \
  examples/x-blueprint/test/unit/test_slug.exe        # the mutation loop
```

Three things are deliberate:

- **The aliases are named `example-cover` / `example-mutate`.** In your
  own project they are `cover` and `mutate` — the names windtrap's
  manual, skill, and own root `dune` use. They are renamed here only
  because this example lives inside windtrap's tree, where `@cover` and
  `@mutate` are recursive and already mean the project's own aggregate.
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

A stateful suite slots into `unit/` the same way (see
`examples/10-stateful`); this example keeps the surface small.
