# Architecture

For maintainers. The user contract is `lib/windtrap.mli`; this file is
the map of what sits behind it and the laws that keep it coherent.

## The narrow waist

**Every test outcome flows into one `Run.t` record as typed
`Failure.t` data; every byte of output leaves that record through the
report.** Producers — assertions (`Check`), the property engine
(`Property`), baseline checking (`Baseline`), capture, the executor
(`Run`) — construct failure data and write it into the run record. The
report (`Report`, `Report_sections`, `Report_junit`) is a pure
projection of that record: styling, diffing, and truncation exist only
there, and no projection can alter status, counts, or scheduling. No
other module prints anything during a run. This single sentence
resolves every "where does this go?" question.

Two second-order waists, both public:

- `'a Testable.t` — printer + equality, the assertion-side witness;
- `'a Gen.t` — generation + shrinking + printing, inseparable, the
  property-side witness.

They never merge again (that was v1's mistake).

## Package map

| unit | where | contents |
| --- | --- | --- |
| library `windtrap` | `lib/` | the kernel: declaration tree, checking, generation, property engine, model-based testing, baselines, capture, the executor, the mutation loop, the report, CLI and the client facade; links `unix` and `windtrap.runtime` only — in-package, so Law 10's no-third-party-weight posture is untouched — and the runtime only for the mutation loop: no run reads the coverage registry |
| `windtrap.runtime` | `lib/runtime/` | the one runtime every instrumented closure links, through both backends' `ppx_runtime_libraries`: `Windtrap_runtime.Coverage` (registration, the `.coverage` dump, report data), `Windtrap_runtime.Mutate` (the catalogue, the arming guard, the reach map), `Windtrap_runtime.Verdicts` (the verdict lattice and the `.mutants` format the loop writes and `windtrap mutants` merges) and `Windtrap_runtime.Instr` (the versioned, exe-identified file plumbing both formats share). Stdlib only — it must never pull anything into the closure of every instrumented library — and it reads no environment variable but `WINDTRAP_COVERAGE_FILE`: which mutants a run tests and which one it arms are the core's to resolve (`--mutate`, `--arm`) and hand down. Its files live beside the build directory's contexts (`_build/_coverage`, `_build/_mutants`, a private `--build-dir` likewise) or, for an executable under no build directory, under the working directory's `_windtrap` |
| binary `windtrap` | `bin/` | the two reporting subcommands: `coverage` (`--min`, `--expect`, `--json`, `--lcov`) and `mutants` (merge verdicts killed-anywhere-wins, render the aggregate with its own projection — survivors whose witnesses name their executable, UNREACHED blocks for mutants no executable reached — and exit 1 on any survivor); shared data-file lookup and staleness in `data_files`. Both merge and render, never run a test or drive a build, and every remedy they print says what to do in words rather than spelling a build tool's command |
| package `ppx_windtrap` | `ppx/` | the expect/inline PPX — a desugaring into `test`, `group`, `expect`, `expect_exact` and `output`, with the `inline_tests.backend` whose generated main calls `run --corrected` — the two instrumentation backends (`ppx/coverage/`, `ppx/mutate/`), the inline runtime `Ppx_runtime` (`ppx/runtime/`: the module-load registry, dune's runner protocol, the undriven guard; a client of the public API) and the ambient `Expect_test_config` (`ppx/config/`: `run` and `sanitize`) — the only unit that sees ppxlib |

## Module graph (`lib/`)

Read top to bottom: each layer may depend on the ones above it and on
its own, never downward.

| layer | module | what it owns |
| --- | --- | --- |
| Foundation | `Pp` | style-aware `Format` helpers |
| | `Text` | newline, UTF-8 and substring utilities |
| | `Env` | how the environment is read: typed readers, value vocabularies, CI/TTY detection, the settings with no flag (the `WINDTRAP_*` mirrors themselves are declared in `Cli`'s table) |
| | `Tag` | the tag vocabulary and selection predicates |
| | `Loc` | `pos` + backtrace-derived source attribution |
| | `Path_ops` | the project root and the log root (the build directory the process belongs to, from `INSIDE_DUNE` or the executable's path), sandbox reconstruction |
| | `Atomic_file` | temp+rename writes |
| | `Clock` | monotonic C-stub counter; the runner's timing source |
| | `Seed` | SplitMix64, `s1:` tokens, `mix(root, path, index)` derivation |
| | `Shrink_tree` | memoized lazy rose trees |
| Data | `Failure` | failure-as-data: typed kinds, phase, location, output tail; the `Check_failure`/`Skip_test`/`Timeout` exceptions |
| | `Testable` | the assertion-side witness |
| | `Diff` | diff *data*: Myers hunks and character-refinement spans, no styling |
| Verbs and engines | `Check` | the assertion verbs, pure, no run-state dependency |
| | `Gen` | the property-side witness: generation, shrinking, printing |
| | `Property` | the case loop: examples-first, derived per-case seeds, discard/give-up, shrink search, collect tables |
| | `Stateful` | model-based testing: the command vocabulary, compiled into programs `Property` runs |
| Subsystems | `Capture` | fd-level dup2 capture into per-test log files, C stdio flushing |
| | `Baseline`, `Source_patch` | baselines keyed by literal position or file path, read-only checking, corrections gated per test and written once as `.corrected` files or in place, literal rewriting |
| | `Test_tree` | the declaration tree: tests, groups, focus, xfail, flatten |
| Run and report | `Run` | one configuration record for everything an invocation resolves; THE run record and the one ambient slot; the sequential executor — startup checks, selection, the per-test boundary, SIGALRM timeouts, retries, fixture release, the last-failed store, the exit guard, Law 11 exit codes — emitting typed events with immutable payloads and printing nothing. A result row carries its `subject` — test or fixture release — so every sink projects the one recorded list |
| | `Cli` | one declarative item table — flags and flagless settings — resolved once into the one `Run.config`, plus `--help`. Each flag's mirror is declared beside it and read through the flag's own parser, so a variable cannot accept what its flag rejects |
| | `Report`, `Report_sections`, `Report_junit` | the pure projections of the run record: the transcript, the GitHub envelope and `Report.run` — execute, reported — in `Report`; the failure blocks and the subsystem-neutral section vocabulary the coverage and mutation reports project into in `Report_sections`, which names no instrumentation runtime; the JUnit document and its file in `Report_junit` |
| | `Mutate_loop` | the mutation seam: the dry run and its reach map, the scope applied to the population it forks over, the determinism probe, the fork loop — one child per reached mutant, in catalogue order, each running only the tests that reach it — the verdict file (written through the runtime's format) and the per-executable report. It *wraps* `Report.run` rather than sitting beside it, because a mutation run must announce an armed mutant before any other output and fork after the dry run — which brackets the run on both sides |
| | `Windtrap` | the facade |

The inline runtime is a client of the public API, not a resident:
`Ppx_runtime` keeps the module-load registry the generated code fills
(`Windtrap.test` and `Windtrap.group` values, per source file), parses
dune's `inline-test-runner <lib> -partition <file>` protocol, and hands
the partition to `Windtrap.run` under `--corrected`. Nothing in it names
`Windtrap.Private`; it and the ambient `Expect_test_config` live in
`ppx_windtrap`, against the facade.

One runner. The facade's `run` resolves one invocation into the one
`Run.config` (`Cli.settings`, plus the argv-derived invocation), calls
`Mutate_loop.execute_and_report` — which in every uninstrumented build,
every `--list` run, and every instrumented build the environment asked
nothing of *is* `Report.run`, same transcript, same bytes — and adds
only what is genuinely its own: the `--list` listing, the focus warning
and the exit code. `Report.run` is `Run.execute` observed by the
transcript, inside the GitHub envelope when the configuration says so,
followed by the blocks, the baseline report, the annotations and the
JUnit file. A `--corrected` run — a stanza's action or the inline
runner — spells its hints as the mirrors and its acceptance as
`dune promote`, whatever argv says. The inline runner is the same `run`
under `--corrected`, one suite per partition.

The cycle-avoidance rule is load-bearing: subsystem modules operate on
explicit state values (`Capture.output st`, `Baseline.check st …`);
`Run` aggregates the instances; the *ambient-reading wrappers* —
`output ()`, `expect`, `collect`, fixture accessors — live in the
facade, which reads `Run.current ()` and dispatches. Core modules
never read the ambient slot. Keeping the slot the only ambient thing
is what would make a parallel runner an extension rather than a
rewrite.

`Windtrap.Private` re-exports every internal module for the `test/`
suites. It is explicitly unstable; nothing in it escapes
`open Windtrap`.

## Instrumentation containment

Two instrumentation subsystems, each in the same places and no others
(Law 12): an instrumenter inside `ppx_windtrap`, the one stdlib-only
runtime library `windtrap.runtime` (both registries, both data-file
formats, the shared file plumbing), one `windtrap` reporting subcommand
that merges and renders but never runs tests or drives a build, and at
most one core module that drives it.

- **Coverage** — `ppx/coverage/`, `lib/runtime/coverage.ml(i)`,
  `bin/coverage_cmd.ml`, no core module and no coupling: a run prints
  no number of its own, and `windtrap coverage` is the one reporter,
  which builds the section data the per-file table draws from itself.
- **Mutation** — `ppx/mutate/`, `lib/runtime/mutate.ml(i)` and
  `lib/runtime/verdicts.ml(i)`, `bin/mutate_cmd.ml`,
  and `lib/mutate_loop.ml(i)`. The runtime reads no flag and no
  environment: the scope (`--mutate`'s source-path prefixes) and the
  armed identifier (`--arm`) are two rows of `Cli`'s table, with the
  mirrors every run-changing flag has, resolved into `Run.config` and
  applied by the loop — the scope to the population it forks over, the
  identifier through the runtime's own parser and `arm`. Its coupling
  is one dispatch call at run
  entry (the facade's `run` calls `Mutate_loop.execute_and_report` in
  place of `Report.run`), one *composed* observer on `Run.execute`'s
  existing `?on_event` hook — never a replacement for the transcript's
  — and the read-only baseline mode the loop sets on every armed
  process's config (Law 16d).

No instrumentation type appears in `windtrap.mli`, and neither
subsystem owns a copy of the other's layout — nor does `Report_sections`
name either runtime: reports arrive as the subsystem-neutral section
vocabulary (labelled rules, rows, source excerpts), which coverage's
per-file report and mutation's survivor and unreached blocks both
project into, spelling mutant identifiers and arm variables with the
runtime's own functions at the builder site. The mutation loop and the
`mutants` subcommand each build a `Report_sections.mutation` record of
their own — one scoped to a suite, one to the merge — and draw it
through the same projection, so the interactive report and the
aggregate cannot drift apart.

## The Laws

Ported from the accepted v3 design RFC ("Laws", including the
2026-07-28 amendment of Law 14, and the mutation RFC's amendments to
Laws 11, 12, 13 and 15 plus the new Law 16; Law 2 rewritten and Law 1's
acceptance list amended on 2026-09-15, when `snapshot` and the
`WINDTRAP_UPDATE` channel were replaced by position- and path-keyed
baselines corrected through `--corrected` and `-u`, and Law 11's inline
clause and Law 16(d)'s table-clearing clause dropped the same day, when
the expect PPX became a desugaring into the library and its runtime a
client of the public API; Law 16(e) rewritten
and Law 17 withdrawn on 2026-08-21,
when admission was removed and the project aggregate became the one
place a survivor fails a build); the RFC documents themselves were
removed from the repo — this copy is the durable record. Each law
names the failure it prevents; **a change to any of them reopens the
design**.

1. **Checking never writes to the source tree.** Within an executed
   run, no test creates, updates, or deletes a baseline or any source
   file; only explicit acceptance writes — `-u` in place, atomically
   and refused under `CI`; `--corrected` as `<file>.corrected` beside
   the file for `dune promote`, never the file itself. *Prevents:*
   green runs that mean "baseline just got invented"; sandbox
   violations.
2. **A baseline is where the source says it is.** A literal at its own
   position, which the compiler recomputes on every build, or a file at
   the path the call names; nothing is derived from a test's name or
   declaration site. *Prevents:* baselines orphaned by an edit.
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
   regains control — test failure, `-x`, filtered runs, end of run
   (process death by signal is the only excepted path); body and
   release failures are both reported. *Prevents:* runner crashes from
   hooks; leaked teardowns; masked errors.
9. **No global mutable per-run state**; one run record, one documented
   ambient slot. *Prevents:* parallelism foreclosure; cross-test
   contamination; `--stream`-class feature interactions.
10. **`windtrap` depends on `unix` only; only `ppx_windtrap` sees
    ppxlib, it is opt-in, and it owns no test semantics:** the
    expect/inline PPX desugars into the library's own `test`, `expect`
    and `output`, the coverage backend inserts
    visit calls that cannot change program behavior (Law 13), and the
    mutation backend inserts guards that are inert unless armed
    (Law 16). *Prevents:* dependency weight at the bottom of every tree;
    parsetree churn in the core; PPX-resident semantics.
11. **The runner exits 0 / 1 / 2** (passed / failed / nothing ran).
    Under `--corrected` a test whose failures are all recorded
    corrections leaves the code alone and a selection the mirrors
    empty exits 0, because the `diff?` that follows is the verdict —
    dune's promotion protocol, for a stanza's action and the inline
    runner alike, where a `WINDTRAP_*` selection spans every stanza
    and partition of the tree. **A mutation run's exit code is its
    own and is stated by Law 16(e); it never reports a test outcome.**
    *Prevents:* filter typos reading as green CI; masked assertion
    failures.
12. **Instrumentation is contained, and the containment is typed.**
    Each instrumentation subsystem lives in exactly an instrumenter
    inside `ppx_windtrap`, the one stdlib-only runtime library
    `windtrap.runtime` (the RFC allowed stdlib+unix; it needs no unix,
    and it reads no environment variable but `WINDTRAP_COVERAGE_FILE`),
    one `windtrap` reporting subcommand that merges and renders but
    never runs tests or drives a build, and at most one core module
    that drives it — coverage needs none; mutation's is
    `lib/mutate_loop.ml`. Both data-file formats live in the runtime,
    each stating its lifecycle rule once: a coverage dump per run,
    predecessors pruned by the runtime at the first dump of a rebuilt
    executable; one verdict file per executable, replaced by a full run
    and left alone by a narrowed one — written by the loop, read by the
    subcommand, never by generated code. The inline runtime
    (`ppx_windtrap.runtime`) is a client of the public API and names
    nothing in `Windtrap.Private`. Core windtrap's coupling to each
    subsystem is one read per run — coverage's summary snapshot at run
    end, mutation's dispatch call at run entry. Per-test
    observation uses only the existing `Run.execute ?on_event` hook,
    which receives immutable payloads and cannot alter status, counts,
    or scheduling, and reads only its own subsystem's runtime. **No
    instrumentation type appears in `windtrap.mli`, and the report
    sections name no instrumentation runtime:** report data arrives as the
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
    to one without windtrap; with it and without `--mutate`, the
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
    armed per process, named by `--arm` (or its mirror) and by nothing
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
    (c) *A verdict is data.* Killed, survived, or unreached — never a
    boolean, and never an exit code: an armed run
    states its own verdict in (b)'s closing line, and the loop's live in
    its report and its verdict file.
    (d) *Armed checking is read-only.* While a mutant is armed the
    run's baseline mode is `Check`: an `expect` or `[%expect]` mismatch
    is a plain failure, no `.corrected` is written and dune's promotion
    protocol is not consulted.
    (e) *No trace outside the pipe, and the exit code follows who can
    claim the truth.* A mutation child's whole body is wrapped so no
    path reaches Stdlib's exit machinery: every exception, fatal
    included, is caught, reduced to a verdict line, and followed by
    `Unix._exit`. Each child runs under its own log directory, and the
    loop removes the lot when it ends. A per-executable mutation run
    exits 0 when it completed — **whatever it found**, because its view
    is one suite's and a survivor there may be another suite's kill —
    and 1 when it refused to start or could not finish: red or empty dry
    run, probe disagreement, or a supervision error, each with its own
    message. A run whose selection narrows the suite completes, reports
    in full, writes no verdict file and says so. It never exits 2,
    because "nothing ran" is a statement about a test selection and a
    mutation run does not make one. **The aggregate — `windtrap mutants`
    over the verdicts every suite wrote, however the suites were run —
    exits 1 when any mutant survived every executable that reached
    it.** That is the project's answer, and every survivor in it
    is one of two work items: a test to strengthen, or an equivalent
    mutant to dismiss in the source with `[@mutate off "reason"]`. A
    clean aggregate is the goal state, so it is the one mutation exit
    code a build may gate on. *Prevents:* a one-suite view failing a
    build over another suite's test; a mutation build silently reporting
    different test results; meaning-change escaping the child;
    multi-mutant interaction making a survivor unattributable; a mutated
    run being mistaken for a real one; a mutation run rewriting the
    source tree through the promotion protocol; a crashing child
    overwriting the parent's `.coverage` dump through `at_exit`.
