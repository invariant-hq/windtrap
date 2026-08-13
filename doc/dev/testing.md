# Testing windtrap

How windtrap tests itself, and the workflows that keep the special
suites honest. `dune runtest` runs everything; scope with a directory
(`dune runtest test/unit`) or run a built test binary directly with
`-f` while iterating.

## Layout

Eleven directories under `test/`:

- `unit` — the library suite, flat: one `test_<module>.ml` per `lib/`
  module, aggregated into a single windtrap-run executable
  (`main.exe`; address one module with
  `dune exec test/unit/main.exe -- -f <module>`). Four meta suites
  (`test_run`, `test_runner`, `test_ppx_runtime`, `test_windtrap`)
  drive `Runner.execute` and the ambient slot in-process with
  synthetic configs — the sanctioned way to test runner behavior with
  windtrap itself — and `execute` refuses to nest inside an active
  run, so each is a plain executable over the shared hand-rolled
  `harness.ml` (a local check counter, exit nonzero on any failure):
  the machinery being tested cannot be trusted to report its own bugs.
  The cost is real and worth knowing: those checks get no diffs, no
  filtering, no JUnit, and no per-test timing.
- `conformance` — the ppx_expect conformance corpus (below).
- `coverage`, `coverage_cli`, `coverage_ppx` — the coverage runtime,
  the `windtrap coverage` reporting command, and the instrumenter,
  including its semantics-preservation suite (below).
- `mutate`, `mutate_cli`, `mutate_loop`, `mutate_ppx` — the same four
  jobs for mutation: the runtime, the `windtrap mutate` reporting
  command, the fork loop driven end to end through a real spawned
  process, and the instrumenter's expansion goldens. The family
  deliberately mirrors the coverage one.
- `docs` — compiled documentation (below).
- `ppx` — PPX rewriting goldens (`.expected` files diffed against the
  driver's output, rejects included) and the inline-runner fixtures
  (`inline/`, `inline_coverage/`, `slow_knobs/`, `tail_loc/`).

Three kinds of compiled documentation run in the tree:

- `test/docs/test_guide.ml` — the guide's failing walkthroughs,
  executed in-process, asserting the printed diff, counterexample,
  replay and acceptance commands;
- `test/docs/test_cookbook.ml` and `test/docs/test_migrating.ml` —
  compiled mirrors of `doc/cookbook.md` and the migration reference
  (the 0.2.0 entry in `CHANGES.md`);
- `doc/manual/snippets/` — compiled mirrors of every manual chapter
  (passing snippets run green; failing walkthroughs are build-only in
  `transcript_fail.ml`, which also regenerates the manual's
  transcripts by hand).

`examples/` are real test executables wired into runtest; they double
as the run-and-exit path coverage the in-process suites cannot give.

## Which kind of test for which job

Match the test kind to the shape of the thing's contract, not to the
size of the module.

| The contract is… | Use | Because |
| --- | --- | --- |
| An algebraic law over a large domain | `prop` | The law *is* the spec, and integrated shrinking makes the counterexample free |
| A finite table of interesting inputs | `cases` | One named, individually selectable sub-test per row |
| Bytes a human reads | `snapshot` | The value *is* the artifact; review is `git diff`, not retyping |
| Bytes produced next to the assertion | `let%expect_test` | Output sits inline with the call that made it |
| A mutable object with an operation vocabulary | `stateful` | Sequences are where the bugs are |
| Generated code | golden `.expected` + `dune promote` | It is a compiler; byte-exact expansion is the contract |
| Semantics preservation under a rewrite | a real instrumented library, compared with the uninstrumented answer | Tail calls, laziness and effect order are invisible in an AST diff |
| Process-level behaviour | a subprocess driver | A forking loop cannot be observed from inside its own image |
| The runner itself | in-process `Runner.execute` with synthetic configs | Only the scheduler genuinely cannot judge itself |
| An external compatibility claim | a vendored upstream corpus | Regenerating goldens from our own output makes the bar circular |

Two habits to avoid. `let check name cond = is_true ~msg:name cond` is
still the most-copied idiom in the suite and it is the wrong one: at
every one of those call sites two values were in scope and both were
thrown away, so the failure prints `expected true / actual false` where
a typed verb would have printed the values and marked the difference.
Reach for `equal` with a witness, or `satisfies` for a genuine
predicate over one value. Likewise `failf` where a verb would do: a
formatted sentence is not a diff.

## Coverage of windtrap by windtrap

`lib/` carries an `(instrumentation (backend ppx_windtrap.coverage))`
stanza, inert without the flag — a plain `dune runtest` is
uninstrumented and free.

```
dune build @cover --instrument-with ppx_windtrap.coverage
```

runs every suite and merges their dumps through `windtrap coverage`,
gated at `--min 87` against a measured baseline. The gate ratchets:
raise it when the margin is comfortable, never lower it to make a red
build green. Not all of the remaining gap is reachable — `mutate_loop`'s
Windows-decline paths and `capture`'s C-stub error branches cannot run
in a green suite — so chase the branches the report names, not the
percentage.

**The backend is spelled `ppx_windtrap.coverage`, not the
`ppx_windtrap` the manual shows users.** Both resolve the same
rewriter, but the `ppx_windtrap` spelling carries
`(ppx_runtime_libraries ppx_windtrap.runtime ppx_windtrap.config)`
because that library's other job is the inline-test rewriter, and dune
adds a rewriter's runtime libraries to everything it preprocesses —
instrumentation included. An instrumented library therefore links the
windtrap *core*. For `lib/` that is a dependency cycle and the build
refuses; for a user it is a closure they did not ask for. See
`ppx/coverage/dune`.

Self-hosting has one consequence worth internalizing: **the coverage
registry is process-global, and windtrap's own suites can no longer
assume they are the only thing in it.** Two seams exist for that, and a
new test that reads coverage should use one:

- `Windtrap_coverage.filter` narrows a collection to chosen files;
- `WINDTRAP_COVERAGE_ONLY` scopes a whole *run*'s number to source
  prefixes, applied once at `Driver.snapshot_coverage` so the inline
  line and the report modes cannot disagree. The `.coverage` dump is
  deliberately not scoped — it is what `windtrap coverage` merges.

A suite that pins a transcript byte for byte must set
`WINDTRAP_COVERAGE=off`. Unset is *not* neutral once the core is
instrumented: the default appends an inline coverage line to every run.
The meta harness does this in `clear_env`, and the ppx transcript
drivers in their scrubbed child environments.

## Mutation of windtrap by windtrap

`lib/` carries `(instrumentation (backend ppx_windtrap.mutate))`, inert
without the flag. Mutate one file at a time:

```
WINDTRAP_MUTATE=1 WINDTRAP_MUTATE_ONLY=lib/diff.ml \
  dune exec --instrument-with ppx_windtrap.mutate test/unit/main.exe --
dune build @mutate                     # merge verdicts and report
```

Measured: 190 mutants in `diff.ml`, **155 killed and 28 survived in
41s** — an 84.7% kill rate for that file, with the whole suite running
against each mutant and no test filter needed.

Admission runs the same way — `WINDTRAP_MUTATE=admit` with a filter,
against the same instrumented build. Measured on this tree
(2026-08-12): one test admits in single-digit milliseconds of
admission work, a 109-test `-f render` selection in under a second and
12 forks, and the full 602-test audit (`-e` matching nothing) in 9
forks — batching plus ride-along admission let one killed fault admit
hundreds of tests, and no test of this suite ruled `UNJUSTIFIED`. The
admission machine's own scenarios live in `test/mutate_loop`.

Six core modules opt out with `[@@@mutate exclude_file]`: `runner`,
`run`, `driver`, `registry`, `mutate_loop` and
`windtrap`; the expect runtime, a library of its own since the
repartition, excludes itself the same way and its stanza carries no
mutation backend at all. They are the
machinery a mutation run uses to judge mutants, so a mutant there is
armed inside the process meant to detect it, and the failure mode is a
hang rather than a survivor — the first whole-core run aborted on
`lib/runner.ml:385:19:fsub`. Coverage still measures those files.

### WINDTRAP_MUTATE_ONLY, and why it is not coverage's filter

The scope is applied by the **runtime, at registration** — an
out-of-scope file never enters the registry and its guard is inert.
That is deliberate and it is the difference between the two features.
Coverage is passive: it records, so pollution is a reporting problem and
`WINDTRAP_COVERAGE_ONLY` narrows what is *reported* without changing the
run. Mutation is active: the loop forks once per mutant, so a scope that
only narrowed the report would still cost the whole afternoon. Narrowing
the registry narrows the work.

It also makes one equivalence true, and the tree depends on it: **an
executable with nothing in scope is indistinguishable from an
uninstrumented one** — empty catalogue, no discovery line, and the seam
declines by name. The equivalence stops at the refusal text: asking such
a run to mutate names the scope and its value (`WINDTRAP_MUTATE_ONLY=…
left no mutants in this executable's catalogue`), never the
missing-backend diagnosis, which would send the reader to rebuild a
build that is fine. Without the equivalence, instrumenting the core
would destroy the mutation suites by construction rather than by
accident:

- `test/mutate_loop/plain_main.exe` is the deliberate zero-mutant
  control. An instrumented core gives it 984.
- `test/mutate_loop/suite_main.exe` is a controlled fixture of exactly
  five mutants, and every count, ordering and verdict assertion is
  written against those five.

Both name their scope (`test/mutate_loop/`), so they keep a *genuine*
catalogue rather than a simulated one. `test/mutate_cli`'s two-executable
scenario names `test/mutate_cli/calc.ml` for the same reason, and the
meta harness sets a scope no file can match so a pinned transcript never
grows a discovery line.

Two suites cannot use the scope, because they test the registry itself
with synthetic file names that deliberately look real (`lib/calc.ml`).
They tell their own registrations from the process's by **time** rather
than by shape: whatever is in the catalogue at their module load — after
the library's, before any test's — is not theirs.
`test/mutate/test_mutate.ml` and `test/mutate_ppx/semantics` both do
this, and it needs no maintenance when a test adds a name.

### What still does not work

A run with **no** scope does not finish. It passes the forced-fail check
and then reaches a mutant whose child *blocks* rather than spins —
observed at 1m41s of CPU while the parent waited. The runaway hit-count
budget cannot catch a child that has stopped hitting sites, so only the
whole-loop deadline can end the run, and it can name just whichever
mutant was in flight. Admission meets the same wall on its 60 s floor:
`WINDTRAP_MUTATE=admit … -f capture` sits at 0% CPU for exactly a
minute — `lib/path_ops.ml:179:38:neq` deadlocks capture's pipe reader —
then refuses, and the refusal itself names the missing piece. The
per-mutant deadline `Mutate_loop`'s interface
already scopes out is the fix; scoping by file is the way around it
today, and it is the better habit regardless.

Two smaller sharp edges, both measured:

- The forced-fail check arms only the single most-reached mutant and
  refuses to start if it survives. Scoped by file this rarely bites; it
  did for every `-f`-narrowed run, where the most-reached mutant is
  `lib/path_ops.ml:175:48:not`, killed only by the `path_ops` tests.
  Its message leads with "the library was not built with
  --instrument-with", which is the commonest cause in general and the
  wrong one there. File scoping is not immune either: a file whose
  most-reached mutant genuinely survives locks the survey out of that
  file until the mutant is killed or dismissed —
  `WINDTRAP_MUTATE_ONLY=lib/capture.ml` refuses today on
  `lib/capture.ml:38:10:le`, reached by 15 tests and caught by none.
  (An `admit` run skips the check by design — Law 16e — so it is the
  way to interrogate such a file's tests in the meantime.)
- **A narrowed run's survivors are relative to its selection.** A mutant
  is reported as surviving when no *selected* test killed it. Such a run
  now keeps that to itself — a selection (`-f`, `-e`, tags, `--quick`,
  `--shard`, `--failed`, an in-source focus) reports in full but writes
  no verdict file and prints `verdicts not saved: …`, so `@mutate` never
  merges a partial answer. `WINDTRAP_MUTATE_ONLY` is not such a
  selection and still writes. Confirm a narrowed survivor before
  believing it, by arming it against the whole suite:

  ```
  WINDTRAP_MUTATE_ARM=lib/path_ops.ml:183:5:lt \
    dune exec --instrument-with ppx_windtrap.mutate test/unit/main.exe --
  ```

  `mutant survived: …` means it really survives —
  `mutant not evaluated: …` means the run proved nothing and the arming
  needs a wider selection. Of the first three that looked worth
  chasing, this killed one: `lib/seed.ml:101:5:lt` is caught by the
  `seed` tests, which the `-f p` selection excluded. The two that held
  are untested boundaries on lines that are fully *covered* — the class
  of defect coverage cannot see:

  ```
  lib/path_ops.ml:183:5:lt   (String.length out) <= 80  ->  < 80
  lib/property.ml:258:17:ge  count > (max_int / 2)      ->  >=
  ```

There is no gate and there deliberately will not be one (Law 16e): the
equivalent-mutant rate is a prediction until it is measured, so a
survivor is a reading list, not a build failure.

`@mutate` deliberately depends on nothing. Putting `(alias_rec runtest)`
in front of the merge — which is right for `@cover`, since running the
suite is how a coverage dump comes to exist — would rebuild every test
executable *uninstrumented* and invalidate the verdicts the merge is
about to read.

## Golden transcripts are snapshots

The renderer's goldens live under `test/unit/__snapshots__/`, not as
string literals in the test source: a transcript is an artifact, and the
point of keeping one is to read the diff when it changes. Accept with
`dune exec test/unit/main.exe -- -u` and review with `git diff`. The
coloured transcript (`verbose-ansi.snap`) pins escape sequences
literally — never strip ANSI to compare it, or the comparison is not
about the thing that broke.

## The ppx_expect conformance corpus

The compat promise ("most ppx_expect suites run unchanged after
swapping the pps and the backend") is measured, not asserted.
`test/conformance/` vendors the test suite of a *pinned* ppx_expect
commit (`54e2846…`, recorded in `test/conformance/NOTICE`) and classifies
every file in `TRIAGE.md`: HONORED (must pass, or must reproduce
upstream's `.ml.corrected.expected` byte-identically), REJECTED (must
fail loudly at expansion with a diagnostic naming the construct — or,
for the monadic config, fail to compile), N-A (Jane Street internals,
each justified). `RESULTS.md` records the measured numbers against the
bar: **≥ 90% of HONORED byte-identical, 100% of REJECTED loud.**

Triage workflow when a conformance diff appears:

1. Reproduce: the conforming sets run on `@runtest`; known divergences
   are quarantined on `@conformance-divergent`
   (`dune build @conformance-divergent` — red by design).
2. Decide which side is wrong. The upstream golden is truth for
   HONORED files; `RESULTS.md` documents the two cases where upstream
   itself is inconsistent or driven by a non-default flag.
3. A fixed divergence flips its fixture green: move its diff rules
   from `@conformance-divergent` back to `@runtest` (fixtures stay
   in place under `corpus/*/divergent/`), and update `RESULTS.md`.
4. Never edit vendored bytes silently: the only permitted tweak is
   the one-line `open Corpus_shim` substitution, and each is listed in
   `TRIAGE.md` as a finding.

Re-pinning the corpus to a newer ppx_expect is a deliberate act, not
maintenance: update the pin in `TRIAGE.md`, re-vendor, re-triage every
new or changed file, re-measure, and record the new numbers in
`RESULTS.md`. The goldens are upstream truth — regenerating them from
windtrap's own output would make the bar circular.

One golden is compiler-version-sensitive by nature:
`hello_async.compile-rejected.expected` pins an OCaml type error
message; regenerate it via `dune promote` on compiler upgrades.

## The coverage semantics-preservation suite

`test/coverage_ppx/semantics/` exists because coverage's out-edge
instrumentation wraps expressions *after* they return — exactly the
transformation that, mishandled, turns tail calls into stack growth,
forces lazy values, or reorders effects. Law 13 says coverage never
changes what programs mean; Law 14 makes this suite the enforcement:
`covsem_fixtures` is a real library instrumented unconditionally, and
`test_semantics.ml` runs it asserting deep tail recursion does not
overflow, evaluation order is untouched, lazy stays lazy, and every
result equals the uninstrumented answer, then reads the in-process
runtime to prove the visit calls actually counted. `test_linkonly.ml`
pins the link contract: an instrumented library links with *nothing*
but the coverage runtime injected.

**This suite must stay green. An instrumenter change that cannot keep
it green is rejected, not accommodated** — grow the fixtures with
every new expression form the instrumenter learns to touch.

The instrumenter's rewriting itself is pinned by expectation tests in
`test/coverage_ppx/` (`fixture_*.ml` → `.expected` expansions, and
`reject_*` diagnostics), same shape as the expect-PPX pins in
`test/ppx/`.

## Golden-file discipline

Wherever a `.expected` file pins output (PPX expansions, rejection
diagnostics, conformance corrections), updates go through
`dune promote`. Read every promoted diff as a code change — promotion
is where bugs get blessed. The conformance goldens are the exception:
they are upstream's bytes and are never promoted from windtrap output
(see above).

Windtrap's own snapshot baselines and `[%expect]` payloads (examples,
manual snippets, `test/unit/__snapshots__`, `test/ppx/inline`) follow the
user-facing workflows: `WINDTRAP_UPDATE=1 dune runtest` and
`dune promote`, reviewed with `git diff`.

## CI

`.github/workflows/build.yml` runs `dune build @runtest` on Linux, macOS
and Windows with `WINDTRAP_JUNIT` pointing at an absolute directory (one
report per suite; a relative path would scatter them through the build
tree), and uploads the reports. Failures annotate the diff by
themselves — the runner detects GitHub Actions and emits `::error`
lines.

A separate Linux-only job runs `dune build @cover`: the coverage number
is a property of the test suite, not of the OS, and instrumented builds
are slower.

`--shard` is deliberately unused. The suite is about half a minute, so
sharding would buy nothing and cost a matrix dimension; exercising a
flag is not a reason to complicate CI, and `test_runner` already covers
it.
