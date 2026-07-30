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
| library `windtrap` | `lib/` | everything below; links `unix` and `windtrap.coverage` (the Law-12 seam) only |
| `windtrap.clock` | `lib/clock/` | monotonic clock C stubs; built but not yet linked — the runner still times with `Unix.gettimeofday` (see the note in `lib/runner.ml`) |
| `windtrap.coverage` | `lib/coverage/` | coverage runtime: registration, `.coverage` files, report data; stdlib only — it must never pull anything into the closure of every instrumented library |
| binary `windtrap` | `bin/` | the `coverage` reporting subcommand (`--min`, `--json`) |
| package `ppx_windtrap` | `ppx/`, `ppx/coverage/` | the expect/inline PPX and the coverage instrumentation backend; the only unit that sees ppxlib |

## Module graph (`lib/`)

Foundation (no internal deps beyond each other): `Pp` (style-aware
Format helpers), `Text` (newline/UTF-8/substring utilities), `Env`
(every environment variable, CI/TTY detection), `Tag`, `Loc` (`pos` +
backtrace-derived source attribution), `Path_ops` (project root,
sandbox reconstruction, log dirs), `Atomic_file` (temp+rename writes),
`Seed` (SplitMix64, `s1:` tokens, `mix(root, path, index)`
derivation), `Shrink_tree` (memoized lazy rose trees).

Data: `Failure` (failure-as-data: typed kinds, phase, location,
output tail; the `Check_failure`/`Skip_test`/`Timeout` exceptions),
`Testable`, `Diff` (diff *data*: Myers hunks + refinement spans, no
styling).

Verbs and engines: `Check` (the sixteen verbs, pure, no run-state
dependency), `Gen`, `Property` (the case loop: examples-first, derived
per-case seeds, discard/give-up, shrink search, collect tables).

Subsystems (each owns a state *type*; the state *instances* live in
`Run`): `Capture` (fd-level dup2 capture into per-test log files, C
stdio flushing), `Snapshot` (name-keyed baselines, read-only checking,
atomic acceptance, orphan tracking), `Test_tree` (the declaration
tree: tests, groups, focus, xfail, flatten).

Drive and render: `Run` (THE run record and the one ambient slot),
`Runner` (sequential executor: selection, per-test boundary, timeout
via SIGALRM, retries, fixture release, the last-failed store, the exit
guard, Law 11 exit codes; emits typed events, prints nothing), `Cli` (one
declarative flag table → parse + env-mirror resolution), `Render` /
`Render_junit` / `Render_github`, `Ppx_runtime` (inline-test protocol,
expect matching, `.corrected` assembly), `Expect_test_config` (the
ambient config expect tests reference), and the facade `Windtrap`.

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

## Coverage containment

All coverage code lives in exactly three places: the instrumenter in
`ppx/coverage/`, the runtime `lib/coverage/`, and the reporting
subcommand in `bin/`. Core windtrap's entire coupling is one summary
snapshot read into the run record at run end and rendered like any
other run data; no coverage type appears in `windtrap.mli`.

## The Laws

Ported from the accepted v3 design RFC ("Laws", including the
2026-07-28 amendment of Law 14); the RFC document itself was removed
from the repo — this copy is the durable record. Each law names the
failure it prevents; **a change to any of them reopens the design**.

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
    expect/inline PPX records locations, and the coverage backend
    inserts visit calls that cannot change program behavior (Law 13).
    *Prevents:* dependency weight at the bottom of every tree;
    parsetree churn in the core; PPX-resident semantics.
11. **The standalone runner exits 0 / 1 / 2** (passed / failed /
    nothing ran). The inline-tests runner follows dune's promotion
    protocol instead — exit 0 iff every failure is a
    corrections-recorded expect mismatch — and that protocol is the
    contract there. *Prevents:* filter typos reading as green CI;
    masked assertion failures.
12. **Coverage is contained.** All coverage code lives in exactly
    three places: the instrumenter inside `ppx_windtrap`, the
    stdlib-only runtime sub-library `windtrap.coverage` (the RFC
    allowed stdlib+unix; the shipped library needs no unix), and the
    `windtrap coverage` reporting command. Core windtrap's entire
    coupling is one summary snapshot read into the run record at run
    end and rendered like any other run data; no coverage type appears
    in `windtrap.mli`. *Prevents:* coverage metastasizing through the
    framework.
13. **Coverage never changes what programs or tests mean.**
    Instrumentation is entry-sequencing only — it must never alter
    tail-call status, laziness compilation, or evaluation order — and
    enabling it must never alter test outcomes, counts, or exit codes.
    Threshold enforcement (`--min`) exists only on the reporting
    command. *Prevents:* instrumentation heisenbugs; coverage-gated
    test results.
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
15. **Coverage data is transient and never touches the source tree.**
    `.coverage` files live under `_build` only, deterministically
    named per executable and overwritten on re-run; the on-disk format
    is versioned by magic string, unknown versions rejected loudly,
    and cross-version compatibility is not promised. *Prevents:*
    stale-merge lies; a frozen format becoming its own maintenance
    program.
