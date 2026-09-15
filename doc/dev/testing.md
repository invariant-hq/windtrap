# Testing windtrap

How windtrap tests itself, and the workflows that keep the special
suites honest. `dune runtest` runs everything; scope with a directory
(`dune runtest test/unit`) or run a built test binary directly with
`-f` while iterating.

## Layout

Eleven directories under `test/`:

- `unit` — the library suite, flat: one `test_<module>.ml` per `lib/`
  module, each its own executable ending in its own `run` (address one
  module by running it: `dune exec test/unit/test_gen.exe -- -f shrink`).
  One suite per file is the shape `SKILL.md` §3 teaches, and it lets
  dune parallelize across the twenty-five where the runner is
  deliberately sequential inside one. Four meta suites
  (`test_run`, `test_runner`, `test_ppx_runtime`, `test_windtrap`)
  are plain executables over the shared hand-rolled `harness.ml` (a
  local check counter, exit nonzero on any failure): three drive
  `Run.execute` and the ambient slot in-process with synthetic
  configs — the sanctioned way to test runner behavior with windtrap
  itself, since `execute` refuses to nest inside an active run — and
  `test_ppx_runtime` checks the inline runtime's registry in-process
  and re-execs itself to observe what its `exit` does to a process.
  The machinery being tested cannot be trusted to report its own bugs.
  The cost is real and worth knowing: those checks get no diffs, no
  filtering, no JUnit, and no per-test timing.
- `conformance` — the ppx_expect conformance corpus (below).
- `coverage`, `coverage_cli`, `coverage_ppx` — the coverage runtime,
  the `windtrap coverage` reporting command, and the instrumenter,
  including its semantics-preservation suite (below).
- `mutate`, `mutate_cli`, `mutate_loop`, `mutate_ppx` — the same four
  jobs for mutation: the runtime, the `windtrap mutants` reporting
  command, the fork loop driven end to end through a real spawned
  process, and the instrumenter's expansion goldens. The family
  deliberately mirrors the coverage one; `mutate_verdicts` covers the
  verdict lattice and file format, which live in the runtime beside the
  coverage format.
- `docs` — compiled documentation (below).
- `ppx` — PPX rewriting goldens (`.expected` files diffed against the
  driver's output, rejects included) and the inline-runner fixtures:
  `inline/` and `strict_flags/` are real `(inline_tests)` libraries
  under dune's backend, the rest (`cross_partition/`, `undriven/`,
  `masked_failure/`, `bad_cwd/`, `tail_loc/`, `slow_knobs/`,
  `inline_coverage/`, `release_failure/`) spawn a generated-runner
  main under a scrubbed environment through `drive/` and pin its
  transcript and exit code.

One kind of compiled documentation runs in the tree:
`doc/manual/snippets/` — compiled mirrors of every manual chapter
(passing snippets run green; failing walkthroughs are build-only in
`transcript_fail.ml`, which also regenerates the manual's transcripts
by hand). The cookbook's recipes and the migration reference's
replacement spellings (the 0.2.0 entry in `CHANGES.md`) are checked by
nothing since their mirrors were removed; a release edits them against
the tree by hand.

`examples/` are real test executables wired into runtest; they double
as the run-and-exit path coverage the in-process suites cannot give.

## Which kind of test for which job

Match the test kind to the shape of the thing's contract, not to the
size of the module.

| The contract is… | Use | Because |
| --- | --- | --- |
| An algebraic law over a large domain | `prop` | The law *is* the spec, and integrated shrinking makes the counterexample free |
| A finite table of interesting inputs | `cases` | One named, individually selectable sub-test per row |
| Bytes a human reads | `expect_file` | The value *is* the artifact; review is `git diff`, not retyping |
| Bytes produced next to the assertion | `let%expect_test` | Output sits inline with the call that made it |
| A mutable object with an operation vocabulary | `stateful` | Sequences are where the bugs are |
| Generated code | golden `.expected` + `dune promote` | It is a compiler; byte-exact expansion is the contract |
| Semantics preservation under a rewrite | a real instrumented library, compared with the uninstrumented answer | Tail calls, laziness and effect order are invisible in an AST diff |
| Process-level behaviour | a subprocess driver | A forking loop cannot be observed from inside its own image |
| The runner itself | in-process `Run.execute` with synthetic configs | Only the scheduler genuinely cannot judge itself |
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
gated at `--min 84` against a measured baseline. Both verdict
aliases — `@cover` and `@mutate` — live in `test/dune`, which is
where `SKILL.md` §3 and `examples/x-blueprint` put them; their run
halves reach past `test/` on purpose (`(alias_rec ../runtest)`), because
each merge reads every dump under `_build` and `examples/` carries
instrumented libraries of its own. The gate ratchets:
raise it when the margin is comfortable, never lower it to make a red
build green. Not all of the remaining gap is reachable — `mutate_loop`'s
Windows-decline paths and `capture`'s C-stub error branches cannot run
in a green suite — so chase the branches the report names, not the
percentage.

**The backend is spelled `ppx_windtrap.coverage`, and it is the only
spelling.** A `ppx_windtrap` backend shipped in 0.2.0 and was cut: that
library's other job is the inline-test rewriter, so it carries
`(ppx_runtime_libraries ppx_windtrap.runtime ppx_windtrap.config)`, and
dune adds a rewriter's runtime libraries to everything it preprocesses —
instrumentation included. An instrumented library therefore linked the
windtrap *core*. For `lib/` that is a dependency cycle and the build
refuses; for a user it was a closure they did not ask for. See
`ppx/coverage/dune`.

Self-hosting has one consequence worth internalizing: **the coverage
registry is process-global, and windtrap's own suites can no longer
assume they are the only thing in it.** Two seams exist for that, and a
new test that reads coverage should use one:

- `Windtrap_runtime.Coverage.filter` narrows a collection to chosen files;
- `WINDTRAP_COVERAGE_ONLY` scopes a whole *run*'s number to source
  prefixes, applied once at `Report.snapshot_coverage`. The `.coverage`
  dump is deliberately not scoped — it is what `windtrap coverage`
  merges.

A suite that pins a transcript byte for byte must set
`WINDTRAP_COVERAGE=off`. Unset is *not* neutral once the core is
instrumented: the default appends an inline coverage line to every run.
The meta harness does this in `clear_env`, and the ppx transcript
drivers in their scrubbed child environments.

## Mutation of windtrap by windtrap

`lib/` carries `(instrumentation (backend ppx_windtrap.mutate))`, inert
without the flag. Mutate one file at a time, from the executable that
owns that file's tests:

```
WINDTRAP_MUTATE=1 WINDTRAP_MUTATE_ONLY=lib/diff.ml \
  dune exec --instrument-with ppx_windtrap.mutate test/unit/test_diff.exe --
```

Measured on this tree (2026-08-21): `mutants: 17 survived of 158
reached by this suite · 141 killed`, **3.2 s wall including
`dune exec`**. The suite split is what makes that cheap and what it
means. Eleven `diff` tests run against each mutant instead of all 570,
which is why the answer arrives in seconds where one aggregated
executable took 41 s over its 190 mutants, and why a survivor here is a
survivor *of those eleven* — the summary line says so, and the run
exits 0 whatever it found. The project's answer is the aggregate, one
command:

```
WINDTRAP_MUTATE=1 dune build @mutate --force --instrument-with ppx_windtrap.mutate
```

`@mutate` depends on `(alias_rec ../runtest)` and `(universe)`, so it
runs every suite in the tree under the variable and then runs
`windtrap mutants`, whose merge is killed-anywhere-wins across every
executable that armed the same site. Each piece of the command is
load-bearing: the variable because the suites read it, the flag because
the suites must carry the mutants — this tree's workspace declares no
instrumentation, so an uninstrumented executable has an empty catalogue
and the seam declines by name — and `--force` because a mutation run is
not a cached artifact. A plain `dune build @mutate` without them
rebuilds the executables uninstrumented, which stales every verdict,
and the merge then refuses loudly. The merge exits 1 when a mutant
survived every executable that reached it; a mutant no executable
reached is listed as `UNREACHED` and never red on its own. The loop's
own scenarios live in `test/mutate_loop`, the merge's in
`test/mutate_cli`.

Four core modules opt out with `[@@@mutate exclude_file]`: `run`,
`report`, `mutate_loop` and `windtrap`; the expect runtime, a library
of its own since the repartition, excludes itself the same way and its
stanza carries no mutation backend at all. They are the machinery a
mutation run uses to judge mutants, so a mutant there is armed inside
the process meant to detect it, and the failure mode is a hang rather
than a survivor — the first whole-core run aborted on the executor's
retry loop. Coverage still measures those files.

### What a run costs, and where the deadline comes from

This is the one home for the derivation; `doc/manual/mutation.md` states
the bill in three sentences and links here.

Two suite runs — the dry run, and one unarmed fork that re-runs it to
prove the suite deterministic — then, per reached mutant, one `fork` and
the time of *its own* reaching tests, cut short at the first failure.
Three factors do the work: reach-guided selection (a child runs the
tests that touched the line, not the suite), bail at the first kill, and
forking from the warm post-dry-run image instead of spawning and
re-initializing a process. Dismissed and unreached mutants are not
forked at all.

The worst cases are unhidden. A suite whose coverage is one integration
test degenerates to "every test reaches every mutant", and the cost
approaches mutants × suite. Overlap costs too: a mutant in a file seven
suites link is dry-run, forked and scored seven times — the merge makes
the *answer* right, not the bill, and the unit split raised that bill by
turning one linker of `lib/` into twenty-five. Nothing is parallel
*inside* a loop in this release; across executables dune is, which is
what `@mutate`'s one-run-per-suite shape buys.

Every forked child — a survey mutant, the
determinism probe — runs under a deadline of its own, derived and never
a knob: **the dry run's wall clock, plus `max(1 s, 10 × the dry run's
own timings for exactly the tests that child is scheduled to run)`**.
The first term is the fixed cost every child pays before its first test,
a fork and a whole process's module initialization, and the dry run
measured it for free, being one whole in-process run of this same suite.
The second is the work the child was actually handed, with an order of
magnitude of headroom, and a floor that absorbs measurement noise on
fast suites. Generous is the right side to err on for a guard whose job
is catching a hang rather than pacing the loop.

A child that overruns is killed with its whole process group — children
`setsid` at birth, so anything a test spawned goes with them — and its
mutant is scored killed. That is not a consolation prize: a fault that
makes the suite hang is a fault the suite noticed, on the crash kill's
own reasoning, and it is the case nothing else here can see. The cheap
first line against a mutant that *spins* is a separate per-site budget
on how often the armed line may be evaluated, set from the hit count the
dry run measured there; a child that blows it dies and is scored killed
too. But a mutant that *blocks* evaluates nothing, sits at 0% CPU and
consumes no budget at all, and only a clock ever ends it.

The per-child deadline is the only clock: nothing caps a whole run, so a
run of a thousand mutants takes as long as its thousand children do and
it is you who stops it. Mutation needs `Unix.fork`, so it declines by
name on Windows.

### WINDTRAP_MUTATE_ONLY, and why it is not coverage's filter

The scope is applied by the **loop, to the population it forks over**.
Every instrumented file still registers and still counts reaches — the
runtime reads no environment — and what narrows is the work: the loop
forks once per mutant, so a scope that only narrowed the report would
still cost the whole afternoon, and a scoped run's verdict file holds
the scoped mutants alone, a true, smaller answer for its executable.
Coverage is passive by contrast: it records, so pollution is a
reporting problem and a filter over the report is enough.

The tree's own mutation suites depend on that. Under
`--instrument-with` their executables link a mutation-instrumented
core, and every count they assert — five sites in
`test/mutate_loop/suite_main.exe`, the reach map, the verdict file — is
written against their own fixtures; naming the scope
(`test/mutate_loop/`, or `test/mutate_cli/calc.ml` for the merge's
two-executable scenario) keeps the population they fork over exactly
those. A scope matching nothing is refused by name (`the mutation scope
… left no mutants in this executable's catalogue`), never with the
missing-backend diagnosis, which would send the reader to rebuild a
build that is fine. One scenario cannot be rescued by a scope: the
zero-mutant control `test/mutate_loop/plain_main.exe` catalogues the
core's mutants once the core is instrumented, so the test that asks it
to mutate and expects the missing-backend refusal checks whether the
core is instrumented and skips itself when it is.

Two suites test the registry itself with synthetic file names that
deliberately look real (`lib/calc.ml`), and tell their own
registrations from the process's by **time** rather than by shape:
whatever is in the catalogue at their module load — after the
library's, before any test's — is not theirs.
`test/mutate/test_mutate.ml` and `test/mutate_ppx/semantics` both do
this, and it needs no maintenance when a test adds a name.

### What still does not work

The blocking mutant is fixed, and what it cost is worth recording. A
child whose fault *blocks* rather than spins — a flipped comparison in
`lib/path_ops.ml` deadlocking capture's pipe reader — stops hitting
sites, so the runaway hit-count budget structurally cannot see it, and
it used to ride a whole-loop deadline: an unscoped survey never
finished, and a `-f capture` selection sat at 0% CPU for
exactly a 60 s floor before refusing. The per-child deadline above
ended that, and on expiry the mutant is scored killed, which is the
right verdict: the suite noticed the change by hanging. The whole-loop
deadline that used to sit behind it
is gone: every child is bounded on its own, and a budget computed as the
sum of those bounds can only fire on parent-side overhead it never
counted — which is a spurious refusal, not a backstop.

What is left is the bill rather than a hang: an unscoped survey still
forks once per mutant across the whole core, so mutating one file at a
time remains the habit, and the better one regardless.

One smaller sharp edge, measured:

- **A narrowed run's survivors are relative to its selection.** A mutant
  is reported as surviving when no *selected* test killed it. Such a run
  now keeps that to itself — a selection (`-f`, `-e`, tags, `--shard`,
  `--failed`, an in-source focus) reports in full but writes
  no verdict file and prints `verdicts not saved: …`, so `@mutate` never
  merges a partial answer. `WINDTRAP_MUTATE_ONLY` is not such a
  selection and still writes. Confirm a narrowed survivor before
  believing it, by arming it against the whole suite:

  ```
  WINDTRAP_MUTATE_ARM=lib/path_ops.ml:183:5:lt dune exec \
    --instrument-with ppx_windtrap.mutate test/unit/test_path_ops.exe --
  ```

  Since the split there is no one executable that is "the whole suite",
  so widen in two steps: first the executable that owns the mutated
  file's tests, unnarrowed, then
  `WINDTRAP_MUTATE_ARM=<id> dune runtest --force --instrument-with
  ppx_windtrap.mutate` — the aggregate report's footer with this tree's
  suite command in the placeholder — which arms the mutant in every
  suite at once.

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

The aggregate is the one gate (Law 16e): a per-executable run exits 0
whatever it found, because its view is one suite's, and `@mutate` exits
1 on any mutant that survived every executable that reached it — each
survivor a test to strengthen or an equivalent mutant to dismiss with
`[@mutate off "reason"]`. `@mutate`'s dependency on
`(alias_rec ../runtest)` is safe only because the one command carries
`--instrument-with ppx_windtrap.mutate`: the same alias driven without
it rebuilds every test executable *uninstrumented*, stales every
verdict, and the merge refuses loudly rather than reading them.

## Golden transcripts are file baselines

The report's goldens live under `test/unit/expected/`, not as string
literals in the test source: a transcript is an artifact, and the point
of keeping one is to read the diff when it changes. Accept with
`dune exec test/unit/test_report.exe -- -u` and review with `git diff`.
The coloured transcript (`verbose-ansi.expected`) pins escape sequences
literally — never strip ANSI to compare it, or the comparison is not
about the thing that broke.

## The ppx_expect conformance corpus

The compat promise ("most ppx_expect suites run unchanged after
swapping the pps and the backend") is measured, not asserted.
`test/conformance/` vendors the test suite of a *pinned* ppx_expect
commit (`54e2846…`, recorded in `test/conformance/NOTICE`) and classifies
every file in `TRIAGE.md`: HONORED (must run with matching semantics —
the same tests pass, the same payloads match, the same mismatches
produce corrections), REJECTED (must fail loudly at expansion with a
diagnostic naming the construct — or, for the monadic config, fail to
compile), N-A (Jane Street internals, each justified). `RESULTS.md`
records the measured numbers against the bar: **≥ 90% of HONORED runs
with matching semantics, 100% of REJECTED loud** — and says why
corrected-file byte-identity is not the bar (upstream's goldens carry a
second pipeline stage, `bin/apply-style`, that no windtrap user runs).
Since the PPX became a desugaring into the library's `expect`
(2026-09-15) two rulings there set the honored number just below the
bar: trailing output and unreached nodes are not checked.

Triage workflow when a conformance diff appears:

1. Reproduce: everything the corpus checks runs on `@runtest`. There
   is no red-by-design alias — a fixture either states a contract
   windtrap holds or it is not vendored.
2. Decide which side is wrong. The upstream golden is truth for
   HONORED files; `RESULTS.md` documents the two cases where upstream
   itself is inconsistent or driven by a non-default flag.
3. A divergence windtrap should not follow is a ruling, not a
   quarantine: write it under "Where windtrap does not follow upstream"
   in `RESULTS.md`, drop the fixture and its golden, and mark the row
   "not vendored" in `TRIAGE.md`.
4. Never edit vendored bytes silently: the only permitted tweak is
   the one-line `open Corpus_shim` substitution, and each is listed in
   `TRIAGE.md` as a finding.

Re-pinning the corpus to a newer ppx_expect is a deliberate act, not
maintenance: update the pin in `TRIAGE.md`, re-vendor, re-triage every
new or changed file, re-measure, and record the new numbers in
`RESULTS.md`. The vendored *fixtures* are upstream truth — rewriting one
to make a test pass would make the bar circular. The corrected-file
goldens are windtrap's own output by decision, so re-recording those is
ordinary work: say in `RESULTS.md` what changed and why.

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

Windtrap's own baselines and `[%expect]` payloads (examples, manual
snippets, `test/unit/expected`, `test/ppx/inline`) follow the
user-facing workflows: `dune promote` after a stanza's `--corrected`
run, or `-u` for the goldens no rule diffs, reviewed with `git diff`.

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
