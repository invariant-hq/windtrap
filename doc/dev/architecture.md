# Architecture

For maintainers. The user contract is `lib/windtrap.mli`; this file is
the map of what sits behind it and the laws that keep it coherent.

## The narrow waist

**Every test outcome flows into one `Run.t` record as typed
`Failure.t` data; every byte of output leaves that record through a
renderer.** Producers — assertions (`Check`), the property engine
(`Property`), snapshot checking (`Snapshot`), capture, the executor
(`Runner`) — construct failure data and write it into the run record.
Renderers (`Render`, `Render_junit`, `Render_github`) are pure
projections of that record: styling, diffing, and truncation exist
only there, and no renderer can alter status, counts, or scheduling.
No other module prints anything during a run. This single sentence
resolves every "where does this go?" question.

Two second-order waists, both public:

- `'a Testable.t` — printer + equality, the assertion-side witness;
- `'a Gen.t` — generation + shrinking + printing, inseparable, the
  property-side witness.

They never merge again (that was v1's mistake).

## Package map

| unit | where | contents |
| --- | --- | --- |
| library `windtrap` | `lib/` | the kernel: declaration tree, checking, generation, property engine, model-based testing, snapshots, capture, the run/driver spine, the mutation loop, renderers, CLI, the client facades and the registry; links `unix`, `windtrap.coverage` and `windtrap.mutate` only — both in-package, so Law 10's no-third-party-weight posture is untouched |
| `windtrap.instr` | `lib/instr/` | the versioned, exe-identified dump-file protocol both instrumentation runtimes share; stdlib only |
| `windtrap.coverage` | `lib/coverage/` | coverage runtime: registration, `.coverage` files, report data; stdlib only — it must never pull anything into the closure of every instrumented library |
| `windtrap.mutate` | `lib/mutate/` | mutation runtime: the catalogue, the arming guard, the reach map, `.mutants` verdict files; stdlib only, for the same reason |
| binary `windtrap` | `bin/` | the two reporting subcommands: `coverage` (`--min`, `--json`) and `mutate` (merge verdicts, report the survivors that survived everywhere); shared discovery and staleness in `data_files` |
| package `ppx_windtrap` | `ppx/` | the expect/inline PPX, the two instrumentation backends (`ppx/coverage/`, `ppx/mutate/`) over shared scaffolding (`ppx/scaffold/`), and the expect runtime itself — `Ppx_runtime` (`ppx/runtime/`) and the ambient `Expect_test_config` (`ppx/config/`) — the only unit that sees ppxlib |

## Module graph (`lib/`)

Foundation (no internal deps beyond each other): `Pp` (style-aware
Format helpers), `Text` (newline/UTF-8/substring utilities), `Env`
(how the environment is read — typed readers, value vocabularies,
CI/TTY detection, the settings with no flag; the `WINDTRAP_*` mirrors
themselves are declared in `Cli`'s table), `Tag`, `Loc` (`pos` +
backtrace-derived source attribution), `Path_ops` (project root,
sandbox reconstruction, log dirs), `Atomic_file` (temp+rename writes),
`Clock` (monotonic C-stub counter; the runner's timing source for
per-test durations and the run total), `Seed` (SplitMix64, `s1:`
tokens, `mix(root, path, index)` derivation), `Shrink_tree` (memoized
lazy rose trees).

Data: `Failure` (failure-as-data: typed kinds, phase, location,
output tail; the `Check_failure`/`Skip_test`/`Timeout` exceptions),
`Testable`, `Diff` (diff *data*: Myers hunks and character-refinement
spans; no styling).

Verbs and engines: `Check` (the Twenty-five verbs, pure, no run-state
dependency), `Gen`, `Property` (the case loop: examples-first, derived
per-case seeds, discard/give-up, shrink search, collect tables),
`Stateful` (model-based testing: the command vocabulary, compiled into
programs the property engine runs).

Subsystems (each owns a state *type*; the state *instances* live in
`Run`): `Capture` (fd-level dup2 capture into per-test log files, C
stdio flushing), `Snapshot` (name-keyed baselines, read-only checking,
atomic acceptance, orphan tracking), `Test_tree` (the declaration
tree: tests, groups, focus, xfail, flatten).

Drive and render: `Run` (THE run record and the one ambient slot; a
result row carries its `subject` — test, fixture release, or the
stale-baselines verdict — so every sink projects the one recorded
list), `Runner` (sequential executor: `plan` stages the startup checks
and selection, `execute_plan` runs the plan, `execute` is literally
their composition; per-test boundary, timeout via SIGALRM, retries,
fixture release, the last-failed store, the exit guard, Law 11 exit
codes; emits typed events with immutable payloads, prints nothing),
`Cli` (one declarative item table — flags and flagless settings — →
one resolution pass producing `Run.config` and `Render.settings`,
`--help`, and the environment layer: each flag's mirror is declared
beside it and read through the flag's own parser, so a variable cannot
accept what its flag rejects; `Env` remains the readers and platform
detection consumed below `Run`), `Render` / `Render_junit` /
`Render_github` (`Render` also owns the subsystem-neutral
report-section vocabulary: instrumentation reports arrive as section
data, and `Render` names no instrumentation runtime), `Driver` (the
spine: `Driver.t` is the record of one invocation's reporting inputs,
`execute_and_report` the one order every driver shares, and
`plan`/`execute` the staged halves for mutation-child-style callers),
`Registry` (the Law-16d armed hooks: the one cross-package cell),
`Mutate_loop` (the mutation seam: the
dry run and its reach map, the determinism probe, the forced-fail
check, the fork loop, the admission machine, the verdict file and the
report — it *wraps* `Driver.execute_and_report` rather than sitting
beside it, because a mutation run must announce an armed mutant before
any other output and fork after the dry run, which brackets the run on
both sides), and the facade `Windtrap`. The expect runtime is a
client, not a resident: `Ppx_runtime` (inline-test protocol, expect
matching, `.corrected` assembly) consumes the core through
`Windtrap.Private` — the alias block at its top is the census of that
diet, and widening it is a design act — and the ambient `Expect_test_config`
live in `ppx_windtrap`, against the facades.

Two thin drivers sit on top of `Mutate_loop.execute_and_report` —
which in every uninstrumented build and every `--list` run *is*
`Driver.execute_and_report`, same transcript, same bytes, and which in
an instrumented build the environment asked nothing of adds the one
discovery line and nothing else — and nothing else sits between them
and it: the facade's `run` (in core) and `Ppx_runtime.exit` (in
`ppx_windtrap`, through the facades). Each resolves one invocation
(`Cli.settings`), calls `execute_and_report`, and adds
only what is genuinely its own: the argv-derived invocation, the
property-aware header seed, the selection description, GitHub gating,
the `--list` listing, JUnit, the focus warning and the process exit on
one side; the fixed `` `Mirrors `` invocation, a header with neither
seed nor selection, `.corrected` flushing and dune's promotion exit
code on the other. **A transcript line either comes from a `Driver`
producer or it is a driver's own line, named as such.** That is what
keeps the two runners byte-identical.

The cycle-avoidance rule is load-bearing: subsystem modules operate on
explicit state values (`Capture.output st`, `Snapshot.check st …`);
`Run` aggregates the instances; the *ambient-reading wrappers* —
`output ()`, `snapshot`, `collect`, fixture accessors — live in the
facade, which reads `Run.current ()` and dispatches. Core modules
never read the ambient slot. Keeping the slot the only ambient thing
is what would make a parallel runner an extension rather than a
rewrite.

`Windtrap.Private` re-exports every internal module for the `test/`
suites and `ppx_windtrap`. It is explicitly unstable; nothing in it
escapes `open Windtrap`.

## Instrumentation containment

Two instrumentation subsystems, each in the same four places and no
others (Law 12): an instrumenter inside `ppx_windtrap`, a stdlib-only
runtime sub-library, one `windtrap` reporting subcommand that merges and
renders but never runs tests or drives a build, and at most one core
module that drives it.

- **Coverage** — `ppx/coverage/`, `lib/coverage/`, `bin/coverage_cmd.ml`,
  no core module. Its entire coupling is one summary snapshot read into
  the run record at run end and rendered, as section data, like any
  other run data.
- **Mutation** — `ppx/mutate/`, `lib/mutate/`, `bin/mutate_cmd.ml`, and
  `lib/mutate_loop.ml(i)`. Its coupling is one dispatch call at run
  entry (the two thin drivers call `Mutate_loop.execute_and_report` in
  place of `Driver.execute_and_report`), one *composed* observer on
  `Runner.execute`'s existing `?on_event` hook — never a replacement
  for the transcript's — and the Law-16d armed hooks in `Registry`,
  which `Ppx_runtime` registers at load and the loop fires: the one
  cross-package cell, since the expect runtime sits above the loop and
  in another package. Firing them clears the inline runtime's
  cross-run tables and revokes the corrections licence.

No instrumentation type appears in `windtrap.mli`, and neither
subsystem owns a copy of the other's layout — nor does `Render` name
either runtime: reports arrive as the subsystem-neutral section
vocabulary (rows, hints, source excerpts), which coverage's per-file
report and mutation's survivor blocks both project into, spelling
mutant identifiers and arm variables with the runtime's own functions
at the builder site.

## The Laws

Ported from the accepted v3 design RFC ("Laws", including the
2026-07-28 amendment of Law 14, and the mutation RFC's amendments to
Laws 11, 12, 13 and 15 plus the new Law 16; and the admission RFC's
2026-08-12 amendment of Law 16(b), (c) and (e) plus the new Law 17);
the RFC documents themselves were removed from the repo — this copy is
the durable record. Each law names the failure it prevents; **a change to any of
them reopens the design**.

1. **Checking never writes to the source tree.** Within an executed
   run, no test creates, updates, or deletes a baseline or any source
   file; only explicit acceptance (`-u`/`WINDTRAP_UPDATE`,
   `dune promote`) writes, atomically. *Prevents:* green runs that
   mean "baseline just got invented"; sandbox violations.
2. **Persisted snapshot identity is a name.** No baseline stored
   outside the source file is keyed by a source position. (Inline
   `[%expect]` payloads are positional by nature; they persist only
   inside the source itself and are rewritten only via `dune
   promote`.) *Prevents:* baselines orphaned by unrelated edits.
3. **Every mismatch prints its own acceptance command.** *Prevents:*
   memorized verbs; silent updates.
4. **Failures are data; renderers are projections.** Styling, diff
   highlighting, and truncation exist only in renderers, and no
   renderer can alter status, counts, or scheduling. *Prevents:* ANSI
   in JUnit; format-dependent truth.
5. **A failing test's captured output appears in its failure report**
   (bounded, with the full-log path). *Prevents:* capture cost paid,
   value withheld.
6. **Generation, shrinking, and printing are inseparable in `Gen.t`.**
   No user-written shrinker and no printerless counterexample can
   exist; a test list that constructs is a test list that runs.
   *Prevents:* declaration-time crashes; QCheck's optional-field
   disease.
7. **Per-case seeds derive from (root, path, index).** Suite
   composition never perturbs another test's stream; every failure is
   replayable from the printed root token. *Prevents:* unreproducible
   property failures.
8. **Every user callback runs inside a test's exception boundary**,
   and a resource acquired is released on every path where the runner
   regains control — test failure, `--bail`, filtered runs, end of run
   (process death by signal is the only excepted path); body and
   release failures are both reported. *Prevents:* runner crashes from
   hooks; leaked teardowns; masked errors.
9. **No global mutable per-run state**; one run record, one documented
   ambient slot. *Prevents:* parallelism foreclosure; cross-test
   contamination; `--stream`-class feature interactions.
10. **`windtrap` depends on `unix` only; only `ppx_windtrap` sees
    ppxlib, it is opt-in, and it owns no test semantics:** the
    expect/inline PPX records locations, the coverage backend inserts
    visit calls that cannot change program behavior (Law 13), and the
    mutation backend inserts guards that are inert unless armed
    (Law 16). *Prevents:* dependency weight at the bottom of every tree;
    parsetree churn in the core; PPX-resident semantics.
11. **The standalone runner exits 0 / 1 / 2** (passed / failed /
    nothing ran). The inline-tests runner follows dune's promotion
    protocol instead — exit 0 iff every failure is a
    corrections-recorded expect mismatch — and that protocol is the
    contract there. **A mutation run's exit code is its own and is
    stated by Law 16(e); it never reports a test outcome.**
    *Prevents:* filter typos reading as green CI; masked assertion
    failures.
12. **Instrumentation is contained, and the containment is typed.**
    Each instrumentation subsystem lives in exactly an instrumenter
    inside `ppx_windtrap`, a stdlib-only runtime sub-library (the RFC
    allowed stdlib+unix; neither shipped library needs unix; the shared
    dump-file protocol is `windtrap.instr`), one `windtrap` reporting
    subcommand that merges and renders but never runs tests or drives a
    build, and at most one core module that drives it — coverage needs
    none; mutation's is `lib/mutate_loop.ml`. Out-of-core client code
    (the expect runtime) is a client of the core through
    `Windtrap.Private`, its diet documented at its alias block. Core
    windtrap's
    coupling to each subsystem is one read per run — coverage's summary
    snapshot at run end, mutation's dispatch call at run entry — plus,
    for mutation alone, the Law-16d armed hooks registered in
    `Registry`. Per-test
    observation uses only the existing `Runner.execute ?on_event` hook,
    which receives immutable payloads and cannot alter status, counts,
    or scheduling, and reads only its own subsystem's runtime. **No
    instrumentation type appears in `windtrap.mli`, and `Render` names
    no instrumentation runtime:** report data arrives as the
    subsystem-neutral section vocabulary, so shared layout has one home
    and two subsystems cannot own two copies of one renderer.
    *Prevents:* instrumentation metastasizing through the framework;
    two divergent source-excerpt renderers.
13. **Coverage never changes what programs or tests mean.**
    Instrumentation is entry-sequencing only — it must never alter
    tail-call status, laziness compilation, or evaluation order — and
    enabling it must never alter test outcomes, counts, or exit codes.
    Threshold enforcement (`--min`) exists only on the reporting
    command. **The mutation backend is a different backend, selected by
    a different name, and is governed by Law 16; no build that enables
    coverage alone may contain a mutation switch, and no coverage switch
    may ever be armable.** *Prevents:* instrumentation heisenbugs;
    coverage-gated test results; a meaning-preserving backend acquiring
    a meaning-changing mode.
14. **Instrumenter scope is expression grade, frozen at the v1/Bisect
    model, and semantics preservation is enforced by suite.**
    *(Amended 2026-07-28: OCaml's exception-heavy style makes
    raise-attribution load-bearing for the coverage number; block
    grade's "entered = covered" was judged systematically wrong
    here.)* The instrumented population is v1's: expressions including
    application out-edges (a point fires only when the expression
    *returns*), `&&`/`||` condition arms, match/try/function arms and
    guards, if branches, loop and lazy and letop bodies, class bodies,
    and toplevel bindings — with the tail-position and
    lazy-compilation guards that make out-edge wrapping
    semantics-preserving. Because post-visit wrapping *can* alter
    tail-call status if mishandled, Law 13 is enforced by a mandatory
    semantics-preservation suite (`test/coverage_ppx/semantics/`) that
    must stay green; a change that cannot keep it green is rejected.
    Scope grows only by deliberate design amendment. *Prevents:*
    silently-wrong coverage numbers on raising paths; unprincipled
    scope creep; instrumentation heisenbugs.
15. **Instrumentation data is transient and never touches the source
    tree.** `.coverage` and `.mutants` files live under `_build` only,
    deterministically named per executable and overwritten on re-run;
    each on-disk format is versioned by magic string, unknown versions
    rejected loudly, and cross-version compatibility is not promised.
    **No instrumenter writes a file: a mutant catalogue is a literal in
    the code it describes. Mutation persists verdicts and nothing
    else** — never a catalogue, never a cache of prior runs.
    *Prevents:* stale-merge lies; a frozen format becoming its own
    maintenance program; a preprocessor writing into a sandboxed source
    tree.
16. **A mutant changes meaning only in a forked child, only when armed,
    and only in a build that asked for it.**
    (a) *Inert by default.* Mutation arrives through its own opt-in
    dune instrumentation backend. A build without it is byte-identical
    to one without windtrap; with it and without `WINDTRAP_MUTATE`, the
    instrumented code is observationally identical to uninstrumented
    code — same evaluation order, tail-call status, laziness, outcomes,
    counts, and exit code. Enforced by `test/mutate_ppx/semantics/`,
    which compiles `test/coverage_ppx/semantics/`'s fixture source
    (shared by `copy_files`) plus its own operand-order and
    fatal-exception fixtures under the mutate backend, checks them
    against a *second, uninstrumented* compilation of the same sources
    rather than against an editable expectation, and which must stay
    green.
    (b) *One mutant, announced and concluded.* At most one mutant is
    armed per process, named by `WINDTRAP_MUTATE_ARM` and by nothing
    else, and a process with a mutant armed prints
    `mutant <id> armed: <before> → <after>` before any other output —
    so a run whose output does not say so has none — and closes, after
    the transcript, with exactly one of `mutant killed.`,
    `mutant survived: the armed site was evaluated N time(s) and no test
    failed.` and `mutant not evaluated: no selected test ran the site.`,
    because green has two meanings there and they ask for opposite work.
    A run that exited 2 gets no closing line: a selection that matched
    nothing says something about the filter and nothing about the
    mutant.
    (c) *A verdict is data.* Killed (carrying its cause), survived, or
    unreached — never a boolean, and never an exit code: an armed run
    states its own verdict in (b)'s closing line, and the loop's live in
    its report and its verdict file. An `admit` run's per-test rulings
    are the same rule under Law 17(c).
    (d) *Armed checking is read-only.* While a mutant is armed, a
    snapshot or `[%expect]` mismatch is a plain failure: no
    `.corrected` is written and dune's promotion protocol is not
    consulted. The child additionally clears the inline runtime's
    cross-run tables — node pool, corrections, styled registry, covered
    set — before its first test, so a mismatch is reported as a
    mismatch and not as a merged-history CR against the parent's run.
    (e) *No trace outside the pipe.* A mutation child's whole body is
    wrapped so no path reaches Stdlib's exit machinery: every
    exception, fatal included, is caught, reduced to a verdict line,
    and followed by `Unix._exit`. Each child runs under its own log
    directory, and the loop removes the lot when it ends. A mutation
    run's own exit code is 0 when it
    completed — **whatever it found**, and whether or not it persisted:
    a run whose selection narrows the suite completes, reports in full,
    writes no verdict file and says so — and 1 when it refused to start
    or could not finish: red or empty dry run, probe disagreement,
    forced-fail failure, or a supervision error, each with its own
    message. **It never exits 2**, because "nothing ran" is a statement
    about a test selection and a mutation run does not make one. A
    survivor-driven nonzero exit is a later addition and is the only
    thing that may ever change this. **Exception, claiming exactly that
    reserved clause (2026-08-12): an `admit` run — which judges an
    explicit test selection at its author's request — additionally exits
    1 when a selected test killed nothing it reached. For `admit` runs
    the refusal causes additionally include a missing selection, an
    empty one under the standalone runner, and a selection of nothing
    but skipped and `xfail` tests; the forced-fail check does not
    apply.** Survey runs are unchanged forever. *Prevents:*
    mutation-gated CI; a mutation build silently reporting different
    test results;
    meaning-change escaping the child; multi-mutant interaction making
    a survivor unattributable; a mutated run being mistaken for a real
    one; a mutation run rewriting the source tree through the promotion
    protocol; a crashing child overwriting the parent's `.coverage`
    dump through `at_exit`.
17. **Admission judges tests, one selection at a time.**
    (a) *Designation is the author's selection, never inferred.* The
    admission set is the run's ordinary test selection — a filter, an
    exclude, a tag selection, `--failed`, an in-source focus — and
    nothing else: no VCS awareness, no run-to-run comparison, no store
    of tests seen before. `--shard` and `--quick` narrow work rather
    than naming tests and do not designate on their own; a run that
    designates nothing refuses, naming the survey as the question it
    probably meant. *Prevents:* silent misses that admit by omission;
    Law 15 violations by the back door; a working-tree model the
    framework cannot own.
    (b) *An admission run persists nothing.* No verdict file is
    written, none is read, and an existing one is left byte-intact.
    *Prevents:* fabricated claims about the mutants an early stop never
    tried; selection-relative poisoning of the merge; the forbidden
    cache.
    (c) *A per-test ruling is three-valued data, and a capped ruling
    says so* — admitted (with its witness and, when unusual, its
    cause), unjustified (the faults tried, with the cap stated when it
    bit), no sites (the cause when determinable). A fault counts as
    tried for a test only when that test ran to an outcome of its own
    under it: a skip watched nothing, and a fork the test merely rode
    along in charges no try, though a kill observed there still admits.
    *Prevents:* capped rulings posing as exhaustive; conflating
    "couldn't be tested" with "wasn't caught"; a skipped test billed as
    an unobservant one.
    (d) *NO SITES never fails a run.* The partition is decided before
    any fork and is never consulted by the exit predicate.
    *Prevents:* punishing legitimate tests of data and glue; teaching
    agents to delete tests to go green.
    (e) *An admit transcript makes only per-test claims* — never a
    survivor list, a mutation score, or any other project-level
    statement. *Prevents:* an admission report posing as the survey's
    answer; killed-anywhere-wins confusion read into a one-suite
    question.
