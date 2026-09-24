# Architecture

For maintainers. The user contract is `lib/windtrap.mli`; this file is
the map of what sits behind it — the packages, the modules, and the
twelve guarantees the design holds. Layout is described here, not
legislated: files may move as long as the guarantees hold.

## The narrow waist

**Every test outcome flows into one `Run.t` record as typed
`Failure.t` data; every byte of output leaves that record through the
report.** Producers — the assertion verbs (`Check`), the property engine
(`Property`), baseline checking (`Baseline`), capture, the executor
(`Run`) — construct failure data and write it into the run record. The
report (`Report`, `Report_sections`, `Report_junit`) is a pure
projection of that record: styling, diffing and truncation exist only
there, and no projection can alter status, counts or scheduling. No
other module prints anything during a run. This one sentence resolves
every "where does this go?" question.

Two second-order waists, both public:

- `'a Testable.t` — printer + equality, the assertion-side witness;
- `'a Gen.t` — generation + shrinking + printing, inseparable, the
  property-side witness.

They never merge again (that was v1's mistake).

One run record, one ambient slot. Core modules operate on explicit
state values (`Capture.output st`, `Baseline.check st …`); `Run`
aggregates the instances; the ambient-reading wrappers — `output ()`,
`expect`, `collect`, the fixture accessors — live in the facade, which
reads `Run.current ()` and dispatches. Keeping the slot the only
ambient thing is what would make a parallel runner an extension rather
than a rewrite.

## Package map

| unit | depends on | contents |
| --- | --- | --- |
| `windtrap` | stdlib, unix, its own C stubs, `windtrap.runtime` | the library: one wrapped library, `windtrap.mli` the whole contract, the mutation loop; the runtime is linked for the loop alone — no run reads the coverage registry |
| `windtrap.runtime` | stdlib | the one runtime every instrumented closure links, through both backends' `ppx_runtime_libraries`: `Windtrap_runtime.Coverage` (registration, the `.coverage` dump, report data), `Windtrap_runtime.Mutate` (the catalogue, the arming guard, the reach map), `Windtrap_runtime.Verdicts` (the verdict lattice and the `.mutants` format) and `Windtrap_runtime.Instr` (the versioned, executable-identified file plumbing both formats share). It reads no environment variable but `WINDTRAP_COVERAGE_FILE`; which mutants a run tests and which one it arms are the core's to resolve (`--mutate`, `--arm`) and hand down |
| `windtrap` (binary) | `windtrap`, `windtrap.runtime` | `coverage [--min N] [--expect PATH] [--json] [--lcov] [-u] [PATH…]` and `mutants [PATH…]`: merge data files and render, never run a test or drive a build; every remedy they print says what to do in words rather than spelling a build tool's command |
| `ppx_windtrap` | ppxlib | the expect and inline-test rewriter, a desugaring into `test`, `group`, `tags`, `expect`, `expect_exact` and `output`; `ppx_runtime_libraries windtrap ppx_windtrap.runtime ppx_windtrap.config`; the `inline_tests.backend` whose generated main calls `run --corrected` |
| `ppx_windtrap.runtime` | `windtrap` (public API only), unix | the module-load registry, the runner main (dune's `inline-test-runner <lib> -partition <file>` protocol), the undriven-registration guard |
| `ppx_windtrap.config` | nothing | the one-module library whose module is the top-level `Expect_test_config` (`run` and `sanitize`) generated code names unqualified |
| `ppx_windtrap.coverage`, `ppx_windtrap.mutate` | ppxlib | the two instrumentation backends, each naming itself, in its own directory under `ppx/`; `ppx_runtime_libraries windtrap.runtime` |

Two opam packages — the ppxlib boundary forces the second — seven
libraries, one binary. Every library is wrapped; the instrumenters emit
`Windtrap_runtime.Coverage.…` and `Windtrap_runtime.Mutate.…` paths.

Instrumentation is contained by that map. Each subsystem is an
instrumenter inside `ppx_windtrap`, its runtime inside
`windtrap.runtime`, one reporting subcommand in the binary, and at most
one core module that drives it: coverage needs none, and a run prints
no coverage number of its own; mutation's is `Mutate_loop`, whose
coupling is one dispatch call at run entry and one composed observer on
`Run.execute`'s `?on_event` hook. No instrumentation type appears in
`windtrap.mli`, and `Report_sections` names neither runtime: both
reports arrive as the subsystem-neutral section vocabulary (labelled
rules, rows, source excerpts), so shared layout has one home. The
mutation loop and the `mutants` subcommand each build their own
`Report_sections.mutation` record — one scoped to a suite, one to the
merge — and draw it through the same projection, so the interactive
report and the aggregate cannot drift apart.

## Modules (`lib/`)

| module | owns |
| --- | --- |
| `Windtrap` | the contract and the flat re-exports; the ambient-reading wrappers |
| `Test_tree` (with `Test_tree.Tag`) | the tree, tags, focus, xfail, flatten, paths |
| `Testable`, `Check`, `Failure`, `Diff` | witnesses; the verbs, pure, with no run-state dependency; failure data (typed kinds, phase, location, output tail, the `Check_failure`/`Skip_test`/`Timeout` exceptions); diff data (Myers hunks and character-refinement spans, no styling) |
| `Gen` (with `Gen.Engine.Shrink_tree`), `Property`, `Stateful` | generators; the case loop (examples first, per-case seeds, the discard budget, the shrink search, label tables); commands and programs, compiled into properties |
| `Baseline`, `Source_patch`, `Capture` | the correction registry keyed by site or path, read-only checking, corrections gated per test and written once as `.corrected` files or in place; literal rewriting inside a source file; fd-level capture into per-test log files |
| `Cli`, `Run`, `Report`, `Report_sections`, `Report_junit` | one declarative item table — flags and flagless settings — resolved once into the one `Run.config`, each mirror declared beside its flag and read through the flag's parser; the run record and the ambient slot, the sequential executor (startup checks, selection, the per-test boundary, SIGALRM timeouts, retries, fixture release, the last-failed store, the exit guard, the exit codes), which prints nothing and emits typed events; the transcript, the GitHub envelope and `Report.run` — execute, reported; the failure projection every transport shares and the section vocabulary the coverage and mutation reports project into; the JUnit document and its file |
| `Mutate_loop` | the dry run and its reach map, the scope applied to the population it forks over, the determinism probe, the fork loop — one child per reached mutant, each running only the tests that reach it — the verdict file, the per-executable report. It wraps `Report.run` rather than sitting beside it, because a mutation run must announce an armed mutant before any other output and fork after the dry run |
| `Os`, `Pp`, `Text`, `Loc`, `Seed` | the clock, environment reading and its value vocabularies, atomic files, the project root and build-copy resolution; style-aware `Format` helpers; newline, UTF-8 and substring utilities; `pos` and backtrace-derived attribution; SplitMix64, `s1:` tokens and the `(root, path, index)` derivation |

The rows group by role, not by layer: `Os`, `Pp`, `Text`, `Loc` and
`Seed` depend on nothing else in `lib/`; `Windtrap` is the only module
that reaches every other; between them the executor (`Run`) sits over
the producers it drives, the report over the executor and the mutation
loop over the report, with `Stateful` and `Cli` reaching `Run` (for
`Run.prop` and `Run.config`). `Windtrap.Private` re-exports these
modules for windtrap's own test suite and its binary; it is not part of the public API and
carries no stability guarantee, and nothing in it escapes into scope on
`open Windtrap`. Everything user-facing is the documented surface above
(`doc/dev/testing.md` says what each test family reaches through it).

One runner. The facade's `run` resolves one invocation into the one
`Run.config` (`Cli.settings`, plus the argv-derived invocation the
hints spell), calls `Mutate_loop.execute_and_report` — which in every
uninstrumented build, every `--list` run, and every instrumented build
the configuration asked nothing of *is* `Report.run`, same transcript,
same bytes — and adds only what is its own: the `--list` listing, the
focus warning and the exit code. `Report.run` is `Run.execute` observed
by the transcript, inside the GitHub envelope when the configuration
says so, followed by the blocks, the baseline report, the annotations
and the JUnit file. A `--corrected` run — a stanza's action or the
inline runner — spells its hints as the mirrors and its acceptance as
`dune promote`, whatever argv says. The inline runner is that same
`run` under `--corrected`, one suite per partition: `Ppx_runtime` keeps
the module-load registry the generated code fills, parses dune's
protocol, and hands the partition to `Windtrap.run`. It and
`Expect_test_config` are clients of the public API.

## Module notes

Design notes the contracts no longer carry, for whoever changes the
module.

**Run.**

- `Run.for_subset`, which a mutation child runs under, clears the
  path-selecting knobs — `filter`, `exclude`, `shard`, `failed_only` —
  and keeps the tag knobs and the root seed: the child's allowlist is
  its parent's selection already applied, an allowlist cannot express a
  tag, and per-case seeds derive from root, path and index. A new
  selection knob `for_subset` does not clear gives a child a selection
  its parent's tree already applied, which is how a deterministic suite
  comes to look non-deterministic.
- `Run.active_run_error` is one string because three already-active
  checks are separately load-bearing — the executor's two halves, and
  the facade's, which must fire before `Cli.parse` can exit on `--help`
  — and the sentence a nested `run` gets must not depend on which one
  saw it first.
- Fixture-release verdicts are result rows: every sink projects the one
  recorded result list, and a verdict that sets the exit code must be
  visible in the report. Consumers dispatch on `Run.result.subject`,
  never on the rendered path, so a test named `fixture release` cannot
  alias a verdict row; renderers classify a failing row from `counted`
  and `xfail` alone (an uncounted `Fail` is an excused expected
  failure), never by reconstructing executor decisions from messages.

**Loc.** `Loc.capture` takes the first call-stack slot whose
compilation unit is neither windtrap's nor the stdlib's, via
`Printexc.get_callstack` (immune to a user-level re-raise), and returns
`None` rather than guess. The walk never crosses a `Loc.delimit` frame,
which the runner puts under every user callback: a failing call whose
own frame was consumed by tail calls yields `None`, never the line that
called the runner, and the executor then attributes the failure to the
test's declaration. The delimiter is recognized by its debug name and
pinned — never inlined, `fn` not called in tail position. `to_string`
omits the column: it is identity data (`Loc.equal`), not an editor-jump
target.

**Failure and the renderers.** Payload strings are bounded once at
construction (64 KiB), because renderings are the one thing that cannot
outlive the failure site. `Exit_attempt` works because `exit` runs the
`at_exit` handlers and an exception from one propagates to `exit`'s
caller. `backtrace_to_string` is the single conversion, so terminal,
JUnit and GitHub show the same frames, and only a trailing run of
windtrap frames is dropped (a user callback keeps itself and the frames
below it). `Report` reads its presentation settings once, at `create`.
The section vocabulary is priced like `Failure.kind`: a new constructor
is a design amendment. `Pp` has no styled printer combinator, only
`styled_string` over a finished string, because the renderer emits
whole lines through `%s` and measures with
`Text.strip_ansi` and `length_utf8`; `Pp.float_exact` is the only float
printer, so anything printed can be pasted back as the same double, and
a lossy `%g` spelling is asked for at the call site.

**Mutate_loop.**

- Child hygiene. A forked child never reaches `Stdlib`'s exit
  machinery: every exception, fatal included, is caught, reduced to a
  verdict line and followed by `Unix._exit` — otherwise a child dying of
  `Out_of_memory` would run the coverage at-exit dump against a path
  resolved before the fork and overwrite the parent's `.coverage`. For
  the same reason the parent removes each child's log directory
  (`Run.remove_tree`): a child killed at its deadline never runs its own
  cleanup, and an orphaned capture tree would be a trace of the mutant.
- No errored verdict. Under the child's bail-on-first-failure rule a
  killed child always runs fewer tests than were selected, so any verdict
  keyed on "ran fewer than expected" would fire on every kill; three
  verdicts suffice, and a failure of the parent's own supervision (a
  `fork` or `waitpid` that fails) aborts the run naming the errno,
  because a score over an unknown number of unsupervised children is not
  a score.
- Verdict files are self-describing: each record carries the
  before/after renderings, not just an identifier, because the catalogue
  lives inside the instrumented binary, which `windtrap mutants` never
  links; an identifier-only record would give a project report strictly
  worse than the per-executable one, and renderings let a report outlive
  the executable that produced it.

**The runtime (`Windtrap_runtime.Mutate`, `.Coverage`).**

- Uncatalogued is separate from unmatched. One `--arm` identifier is
  handed to every test executable of a project at once — the aggregate's
  reproduce line has no single binary to name — and most were built from
  other sources; such an executable holds no site of that file, produces
  no verdict and hides nothing by running on. Only the registry can tell
  "no site of this file" from "this file, wrong site", so it reports
  both and the loop refuses only the latter.
- Site indices are file-local. The mutate guard closes over arrays
  `register` allocates per file; a single global array indexed by an
  absolute identifier would be indexed before every file had registered
  (link order decides) and read out of bounds. The instrumenter
  therefore emits exactly one binding per file and literal indices,
  never an array.
- A point or site table mismatch is a warning, not an exception. A
  second registration of the same source file with a different table
  means the executable links two incompatible instrumentations; it is
  dropped with a warning on standard error rather than raised, because
  `register` runs at module load inside the user's program and
  instrumentation never changes what programs mean (guarantee 10). A
  rebuild from clean is the fix.

## The twelve guarantees

Each is pinned by a test (`doc/dev/testing.md` says which); changing one
is a design decision, recorded here first.

1. **Checking never writes to the source tree.** `-u` writes in place,
   atomically, and is refused under `CI`; `--corrected` writes
   `<file>.corrected` beside the file for dune to diff and promote,
   never the file itself.
2. **A baseline is where the source says it is**: a literal at its own
   position, which the compiler recomputes on every build, or a file at
   the path the call names; nothing is derived from a test's name or
   declaration site, so no edit can orphan a baseline.
3. **Every mismatch prints its own acceptance command**: `dune promote`
   under a stanza that diffs, `-u` otherwise.
4. **Failures are data; renderers are projections** and cannot alter
   status, counts or scheduling.
5. **A failing test's captured output is in its report**, bounded, with
   the full log's path.
6. **Every generator shrinks; printers derive by composition**, a
   printerless `map` or `bind` renders its pre-image, and `with_pp`
   overrides.
7. **Per-case seeds derive from (root, path, index)**; every failure
   replays from the printed token.
8. **Every user callback runs inside a test's boundary, and a resource
   acquired is released on every path where the runner regains
   control.**
9. **The exit code is 0, 1 or 2**: passed, failed, nothing ran. Under
   `--corrected` a recorded correction is not a failure and an emptied
   selection is not an error, because the `diff?` that follows is the
   verdict and the selection came from a variable spanning every stanza
   (usage errors stay 2).
10. **Coverage never changes what programs or tests mean**, and the gate
    lives only in the reporting command.
11. **Instrumentation data is transient, versioned, and never touches
    the source tree**; a mutant catalogue is a literal in the binary.
12. **A mutant changes meaning only when armed, only in a build that
    asked, and only in the process that armed it**: a forked child of
    the `--mutate` loop, or the run itself under `--arm`; an armed
    process announces it before any output and concludes with one
    verdict line; armed checking is read-only; the aggregate is the one
    mutation exit code a build may gate on.
