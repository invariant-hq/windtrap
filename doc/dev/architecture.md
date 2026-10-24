# Architecture

For maintainers. The user contract is `lib/windtrap.mli`; this file is
the map of what sits behind it: the packages, the modules, and the
twelve guarantees the design holds. Layout is described here, not
legislated: files may move as long as the guarantees hold.

## The narrow waist

**Every test outcome flows into one `Run.t` record as typed
`Failure.t` data; every byte of output leaves that record through the
report.** The producers construct failure data and write it into the
run record: the assertion verbs (`Check`), the property engine
(`Property`), baseline checking (`Baseline`), capture and the executor
(`Run`). The
report (`Report`, `Report_sections`, `Report_junit`) is a pure
projection of that record: styling, diffing and truncation exist only
there, and no projection can alter status, counts or scheduling. No
other module prints anything during a run. This one sentence resolves
every "where does this go?" question.

Two second-order waists, both public:

- `'a Testable.t`: printer + equality, the assertion-side witness;
- `'a Gen.t`: generation + shrinking + printing, inseparable, the
  property-side witness.

They never merge again (that was v1's mistake).

One run record, one ambient slot. Core modules operate on explicit
state values (`Capture.output st`, `Baseline.check st …`); `Run`
aggregates the instances; the ambient-reading wrappers (`output ()`,
`expect`, `collect`, the fixture accessors) live in the facade, which
reads `Run.current ()` and dispatches. Keeping the slot the only
ambient thing is what would make a parallel runner an extension rather
than a rewrite.

## Package map

| unit | depends on | contents |
| --- | --- | --- |
| `windtrap` | stdlib, unix, its own C stubs, `windtrap.runtime` | the library: one wrapped library, `windtrap.mli` the whole contract, the mutation loop; the runtime is linked for the loop alone, and no run reads the coverage registry |
| `windtrap.runtime` | stdlib | the one runtime every instrumented closure links, through both backends' `ppx_runtime_libraries`: `Windtrap_runtime.Coverage` (registration, the `.coverage` dump, report data), `Windtrap_runtime.Mutate` (the catalogue, the arming guard, the reach map), `Windtrap_runtime.Verdicts` (the verdict lattice and the `.mutants` format) and `Windtrap_runtime.Instr` (the versioned, executable-identified file plumbing both formats share). It reads no environment variable but `WINDTRAP_COVERAGE_FILE`; which mutants a run tests and which one it arms are the core's to resolve (`--mutate`, `--arm`) and hand down |
| `windtrap` (binary) | `windtrap`, `windtrap.runtime` | `coverage [--min N] [--expect PATH] [--json] [--lcov] [-u] [PATH…]` and `mutants [PATH…]`: merge data files and render, never run a test or drive a build; every remedy they print says what to do in words rather than spelling a build tool's command |
| `ppx_windtrap` | ppxlib | the expect and inline-test rewriter, a desugaring into `test`, `group`, `tags`, `expect`, `expect_exact` and `output`; `ppx_runtime_libraries windtrap ppx_windtrap.runtime ppx_windtrap.config`; the `inline_tests.backend` whose generated main calls `run --corrected` |
| `ppx_windtrap.runtime` | `windtrap` (public API only), unix | the module-load registry, the runner main (dune's `inline-test-runner <lib> -partition <file>` protocol), the undriven-registration guard |
| `ppx_windtrap.config` | nothing | the one-module library whose module is the top-level `Expect_test_config` (`run` and `sanitize`) generated code names unqualified |
| `ppx_windtrap.coverage`, `ppx_windtrap.mutate` | ppxlib | the two instrumentation backends, each naming itself, in its own directory under `ppx/`; `ppx_runtime_libraries windtrap.runtime` |

Two opam packages (the ppxlib boundary forces the second), seven
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
`Report_sections.mutation` record, one scoped to a suite and one to the
merge, and draw it through the same projection, so the interactive
report and the aggregate cannot drift apart.

## Modules (`lib/`)

| module | owns |
| --- | --- |
| `Windtrap` | the contract and the flat re-exports; the ambient-reading wrappers |
| `Test_tree` (with `Test_tree.Tag`) | the tree, tags, focus, xfail, flatten, paths |
| `Testable`, `Check`, `Failure`, `Diff` | witnesses; the verbs, pure, with no run-state dependency; failure data (typed kinds, phase, location, output tail, the `Check_failure` and `Control` exceptions); diff data (Myers hunks and character-refinement spans, no styling) |
| `Choice`, `Shrink`, `Gen` (with `Gen.Engine.Shrink_tree`), `Property`, `Stateful`, `Workers` | choice sequences (the source a generator parses, spans, dependents, replay tokens); the reducer of a failing sequence; generators; the case loop (examples first, per-case seeds, the discard budget, the shrink search, label tables); commands and programs, compiled into properties, and the judge of a program on several domains; the worker domains that run its branches |
| `Baseline`, `Source_patch`, `Capture` | the correction registry keyed by site or path, read-only checking, corrections gated per test and written once as `.corrected` files or in place; literal rewriting inside a source file; fd-level capture into per-test log files |
| `Cli`, `Run`, `Report`, `Report_sections`, `Report_junit` | one declarative item table (flags and flagless settings) resolved once into the one `Run.config`, each mirror declared beside its flag and read through the flag's parser; the run record and the ambient slot, the sequential executor (startup checks, selection, the per-test boundary, SIGALRM timeouts, retries, fixture release, the last-failed store, the exit guard, the one exit-code function), which prints nothing and emits typed events, and the worker's loop that serves a supervisor; the transcript, the GitHub envelope and `Report.run`, execute reported; the failure projection every transport shares and the section vocabulary the coverage and mutation reports project into; the JUnit document and its file |
| `Supervisor` | the process boundary: the parent that forks workers (or executes itself again after a domain), hands each a case, gives the events in the order of the selection, judges a correcting mode's checks in its one registry, fails a case whose worker died or outlived its kill, retires a worker after a domain, forwards signals and merges the workers' coverage |
| `Mutate_loop` | the dry run and its reach map, the scope applied to the population it forks over, the determinism probe, the fork loop (one child per reached mutant, each running only the tests that reach it), the verdict file, the per-executable report. It wraps `Report.run` rather than sitting beside it, because a mutation run must announce an armed mutant before any other output and fork after the dry run |
| `Os`, `Pp`, `Text`, `Loc`, `Seed` | the clock, environment reading and its value vocabularies, atomic files, the project root and build-copy resolution; style-aware `Format` helpers; newline, UTF-8 and substring utilities; `pos` and backtrace-derived attribution; SplitMix64, `s1:` tokens and the `(root, path, index)` derivation |

The rows group by role, not by layer: `Os`, `Pp`, `Text`, `Loc` and
`Seed` depend on nothing else in `lib/`; `Windtrap` is the only module
that reaches every other; between them the executor (`Run`) sits over
the producers it drives, the report over the executor and the mutation
loop over the report, with `Stateful` and `Cli` reaching `Run` (for
`Run.property`, `Run.without_labels` and `Run.stop`, and for
`Run.config`). `Workers` depends on `Os` alone, and knows nothing of
tests.
`Windtrap.Private` re-exports these modules for windtrap's own test
suite and its binary; it is not part of the public API and carries no
stability guarantee, and nothing in it escapes into scope on
`open Windtrap`. Everything user-facing is the documented surface above
(`doc/dev/testing.md` says what each test family reaches through it).

One runner. The facade's `run` resolves one invocation into the one
`Run.config` (`Cli.settings`, plus the argv-derived invocation the
hints spell), calls `Mutate_loop.execute_and_report` (which in every
uninstrumented build, every `--list` run, and every instrumented build
the configuration asked nothing of *is* `Report.run`, same transcript,
same bytes) and adds only what is its own: the `--list` listing, the
focus warning and `Run.exit_code`. `Report.run` is `Supervisor.execute`
(or, for the mutation loop's dry run and an `--arm` run, which count a
site's hits in their own process, `Run.execute`) observed by the
transcript, inside the GitHub envelope when the configuration
says so, followed by the blocks, the baseline report, the annotations
and the JUnit file. A `--corrected` run, a stanza's action or the
inline runner, spells its hints as the mirrors and its acceptance as
`dune promote`, whatever argv says. The inline runner is that same
`run` under `--corrected`, one suite per partition: `Ppx_runtime` keeps
the module-load registry the generated code fills, parses dune's
protocol, and hands the partition to `Windtrap.run`. It and
`Expect_test_config` are clients of the public API.

## Module notes

Design notes the contracts no longer carry, for whoever changes the
module.

**Supervisor.**

- The parent forks before any test runs, from the state module
  initialisation left, and never runs user code itself; re-executing the
  program instead cost a module initialisation per worker (41 s against
  3.6 s on windtrap's own suite) and cannot reach a `run` that only a
  forked child makes. It re-executes only when the process already
  spawned a domain, after which `fork` is refused.
- A worker never runs the `at_exit` functions its parent registered: the
  worker's own, registered first, ends the process, and so runs after
  every function a test registers. A test's `exit` therefore stops there
  too, which is how the worker says that code under test exited. A
  marker registered after each test names the test whose functions run
  next.
- Under a correcting mode a worker checks only to prove the path and
  read the file, and the parent makes every check in its one
  `Baseline.t`, in the order of the selection (see
  `_autoresearch/audits/gap-baseline.md`): two workers' registries
  would each accept a site's first content, and a replaced worker's
  would forget it.
- The mutation loop still runs its dry run, its children and `--arm` in
  one process, through `Run.execute`, and so still needs the exit
  guard; moving them onto the boundary is the guard's last removal.

**Run.**

- `Run.for_subset`, which a mutation child runs under, clears the
  path-selecting knobs (`filter`, `exclude`, `shard`, `failed_only`)
  and keeps the tag knobs and the root seed: the child's allowlist is
  its parent's selection already applied, an allowlist cannot express a
  tag, and per-case seeds derive from root, path and index. A new
  selection knob `for_subset` does not clear gives a child a selection
  its parent's tree already applied, which is how a deterministic suite
  comes to look non-deterministic. It keeps `mutation`: a stateful test
  on several domains reads it to run each program on the test's domain,
  and a mutation child must run it as the dry run did, which spawned
  nothing, so the loop can still fork.
- The ambient slot is an `Atomic.t`, and `Run.t` records the domain
  that called `execute`, so a read from another domain is defined and
  refused instead of racing. The refusal is raised in the calling domain
  and recorded nowhere: like any exception of that domain, it fails the
  test only when it reaches the test's domain.
- A signal handler runs on whichever domain polls first, among those
  that do not block the signal, and the runner's handlers act where
  they land, as in any test: the alarm raises its timeout there. A
  stateful test's workers are spawned while the test's domain blocks
  `SIGALRM`, `SIGINT`, `SIGTERM` and `SIGHUP`, and a domain is born with
  its creator's mask, so no handler of the runner ever runs on a worker
  or on a domain a worker spawns, and a limit that expires while a
  branch deadlocks lands on the test's domain, which waits in spins and
  sleeps rather than a join. The report, the temporary paths and the
  fixture releases therefore never run on a worker. A domain that the
  test's own domain spawned, such as a pool a system starts in the
  prefix, blocks nothing: while it lives it may take the alarm, whose
  timeout then ends the test only if it reaches the test's domain, and
  a deadlocked branch hangs as a deadlocked test would.
- `Run.active_run_error` is one string because three already-active
  checks each matter: the executor's two halves, and the facade's, which
  must fire before `Cli.parse` can exit on `--help`. The sentence a
  nested `run` gets must not depend on which one saw it first.
- A result row is a test's, always. A failed fixture release belongs to
  no test: it rides the outcome (`Run.outcome.release_failures`), and
  every sink of a finished run (`Report.finish`, `Report.annotations`,
  `Report_junit.write`) takes that list as a required argument, so a
  verdict that sets the exit code is visible in the report by type.
  Nothing dispatches on a row's kind, so a test named `fixture release`
  is a test. Renderers classify a failing row from `counted` and `xfail`
  alone (an uncounted `Fail` is an excused expected failure), never by
  reconstructing executor decisions from messages.

**Gen and Shrink: the choice encoding.** A generator parses a
sequence of choices. A choice is an unsigned 64-bit integer at most a
bound that the parse states when it reads it, and `0` is every choice's
target, so the value that a generator parses from zeros is its shrink
target. A replayed choice above its bound reads as the bound, and a
choice read past the end of a replayed sequence reads `0`, so every
sequence parses.

- **Drawing.** A primitive draws its value with 0.2's sampler and
  records the choices that encode it; it never draws a choice by
  itself. The samplers consume the frozen stream exactly as 0.2's did,
  `Seed.split` included at `bind`, `one_of`, `such_that` and a sized
  `list`, so every state draws the value 0.2 drew from it, and the
  distribution of every constructor is 0.2's by construction. The test
  "every generator draws 0.2's distribution" (`test_gen.ml`) pins each
  constructor's moments, histogram and a digest of its printings on
  fixed states, recorded on 0.2: the tolerance is zero, since nothing
  approximates.
- **Integers.** A value of a range with origin `o` (the point closest
  to `0`) is one choice, its rank by distance to `o`: `o` is `0`, then
  `o + 1`, `o - 1`, `o + 2`, … while both sides of the range have room,
  then the side that is left, in order. The bound is the range's width,
  so every choice names a value of the range. A whole type is the range
  of its extremes: `int64`'s ranks fill the 64 bits.
- **Floats.** A float is a sign choice (`0` is positive), absent where
  the range admits one sign only, then a magnitude choice in the
  lexicographic encoding of Hypothesis (MacIver and Donaldson, ECOOP
  2020): an integral magnitude below 2^56 is that integer, so `1.` is
  below `0.5`; any other is the tag bit 2^63, then the exponent
  reordered (exponents `0`, `1`, … first, then `-1`, `-2`, …, then the
  non-finite one last), then the mantissa with its fraction bits
  reversed, so a fraction with fewer bits is simpler. The bound of the
  magnitude is the last finite float of that order. A float therefore
  shrinks toward `0.`, an integer before a fraction, and `-0.` toward
  `0.` by its sign. `any_float` reads a class choice first (`0` finite,
  then `infinity`, `neg_infinity`, `nan`), so a non-finite value
  shrinks toward a finite one. `float_range low high` parses a float
  the same way and clamps it into \[`low`;`high`\] in the order where
  `-0.` is below `0.`, so its zeros parse to its origin: `low` when
  `low` is positive, `high` when `high` is negative or `-0.`, `0.`
  otherwise, and `-0.` belongs to the range only when `low` is negative
  or `-0.`. A sampled float is encoded exactly, so the clamp only moves
  values that a shrink step proposed.
- **The rest.** `bool`, `option` (`None`), `result` (`Ok`), `either`
  (`Left`), `of_list` and `one_of` (the first) read one choice each;
  `frequency` reads the weighted point it drew, so a branch of weight
  `0` is never parsed and the first branch of positive weight is the
  target; `char` and `uchar` rank by distance to `'a'` among their
  values, surrogates excluded; `list` reads a "one more" choice before
  each element, the two one span, and with `~size` the length is the
  size generator's value and only the elements follow; `subsequence`
  reads a keep choice per value; `permutation` reads its Lehmer code,
  whose zeros are the given order; `such_that` reads attempts, each one
  span, until one satisfies its predicate, at most 100, and discards
  after.
- **Spans and dependents.** A parse marks a span over the choices that
  one generator call read. A parse can also say that a choice counts
  past some earlier positions (a reference to the k-th newest value
  counts past the k makers newer than it): a deletion that removes such
  positions lowers that choice by as many, in the same candidate.
- **The reducer** (`Shrink`) keeps a candidate when the case still
  fails in the class of the first failure and the choices it read are
  shortlex smaller. It runs three passes to a fixed point: delete each
  span; delete runs of 8, 4, 2 and 1 choices; minimise each choice, to
  `0` and else by binary search. Every run of the law counts against
  the fixed budget of 10,000 at the law's cost, a discard at 1, and a
  candidate already tried is not run again.

**Stateful.** A call runs its system, then its reference judges the
outcome, one order for every result form. A value is made when its
system returns, so it is named and released even when the reference
then disagrees. An abstract type has a key per side, and a side rides
an exception that the key's local module declares. A local exception is
generative, so only the type that made a side projects it back: the
run's pool holds the system sides of every type, and its table the
reference sides by the step of the call that made them, with no `Obj`
and no `Type.Id` (absent from OCaml 5.0). The same reason makes release
deduplication per type, since `==` compares two system sides only when
they have one type. A choice of a value names the drawn call that made
it, and falls back to the newest value of its type, so a shrink
candidate that deletes an unrelated maker moves no later choice. A
broken reference is `Property.Oracle_failure`, a failure class of its
own: its case shrinks as any other, and neither search accepts a
candidate of the other class.

On several domains the judge never copies a reference: it replays a
fresh one along the path to every branch point but the first, so a
mutable model needs no persistence, and it checks each replay of the
prefix against the run's outcomes, since a drifting reference would
otherwise blame the system. A pending call packs its bound reference
function (parameterised by the sides it reads), its system function and
how the system ended in one existential, so the judge compares within
one unpacking. Every run is judged; nothing caches a verdict. Only the
prefix has one reference state, so only there does a call make a value
or ask a `~pre`: a value made after it would need a reference side that
exists only once an order is chosen, and a `~pre` asked after it would
need one state the orders do not share.

**Loc.** `Loc.capture` takes the first call-stack slot whose
compilation unit is neither windtrap's nor the stdlib's, via
`Printexc.get_callstack` (immune to a user-level re-raise), and returns
`None` rather than guess. The walk never crosses a `Loc.delimit` frame,
which the runner puts under every user callback: a failing call whose
own frame was consumed by tail calls yields `None`, never the line that
called the runner, and the executor then attributes the failure to the
test's declaration. The delimiter is recognized by its debug name and
pinned: never inlined, `fn` not called in tail position. `to_string`
omits the column: a report names a line, not a column.

**Failure and the renderers.** Payload texts are bounded once, by
`Failure.text` (64 KiB), because renderings are the one thing that cannot
outlive the failure site. A cut is recorded as the whole length beside
the kept prefix, never as a marker in the bytes, so a renderer never
takes a cut text for a whole one. The `exit` interception works because `exit`
runs the `at_exit` handlers and an exception from one propagates to
`exit`'s caller. `backtrace_to_string` is the single conversion, so terminal,
JUnit and GitHub show the same frames, and only a trailing run of
windtrap frames is dropped (a user callback keeps itself and the frames
below it). `Report` reads its presentation settings once, at `create`.
The section vocabulary is priced like `Failure.kind`: a new constructor
is a design amendment. `Pp` writes no escape sequence: a report line is
a list of spans that `Report_sections.render` escapes and styles once,
and that `Report_sections.width` measures; `Pp.float_exact` is the only float
printer, so anything printed can be pasted back as the same double, and
a lossy `%g` spelling is asked for at the call site.

**Mutate_loop.**

- Child hygiene. A forked child never reaches `Stdlib`'s exit
  machinery: every exception, fatal included, is caught, reduced to a
  verdict line and followed by `Unix._exit`; otherwise a child dying of
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

- Uncatalogued is separate from unmatched. `WINDTRAP_MUTATE_ARM` hands
  one identifier to every test executable of a project at once, and most
  were built from other sources; such an executable holds no site of that file, produces
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
  instrumentation never changes what programs mean. A rebuild from
  clean is the fix.

## The twelve guarantees

Changing one is a design decision, recorded here first.

- **Checking never writes to the source tree.** `-u` writes in place,
  atomically, and is refused under `CI`; `--corrected` writes
  `<file>.corrected` beside the file for dune to diff and promote,
  never the file itself.
- **A baseline is where the source says it is**: a literal at its own
  position, which the compiler recomputes on every build, or a file at
  the path the call names; nothing is derived from a test's name or
  declaration site, so no edit can orphan a baseline.
- **Every mismatch prints an acceptance command**: `dune promote` and
  its file in its block under a stanza that diffs, otherwise one `-u`
  over the run's selection above the summary.
- **Failures are data; renderers are projections** and cannot alter
  status, counts or scheduling.
- **A failing test's captured output is in its report**, bounded, with
  the full log's path, less what the test read with `output ()`.
- **Every generator shrinks; printers derive by composition**, a
  printerless `map` or `bind` renders its pre-image, and `with_pp`
  overrides.
- **Per-case seeds derive from (root, path, index)**; every failure
  replays from the printed token, within one version of windtrap, except
  that the failure of a test on several domains replays its programs,
  not its schedule: the derivation and the bit stream are frozen under
  the token's `s1` prefix, what a generator draws from the stream is not.
- **Every user callback runs inside a test's boundary, and a resource
  acquired is released on every path where the runner regains
  control.** A call on another domain that outlives its test's limit
  ends the run after that test.
- **The exit code is 0, 1 or 2**: passed, failed, nothing ran. Under
  `--corrected` a recorded correction is not a failure, because the
  `diff?` that follows is the verdict. What the environment broadcasts
  is not an error of a suite that cannot honour it: a selection that
  only the mirrors gave and that empties a suite returns 0, and
  `WINDTRAP_MUTATE` on a suite with no mutant to test runs it
  ordinarily (a command-line selection keeps 2, `--mutate` its
  refusal, and usage errors stay 2).
- **Coverage never changes what programs or tests mean**, and the gate
  lives only in the reporting command.
- **Instrumentation data is transient, versioned, and never touches
  the source tree**, with one exception: an instrumented executable that
  lies below no build directory writes under `_windtrap/` in its working
  directory. A mutant catalogue is a literal in the binary.
- **A mutant changes meaning only when armed, only in a build that
  asked, and only in the process that armed it**: a forked child of
  the `--mutate` loop, or the run itself under `--arm`; an armed
  process announces it before any output and concludes with one
  verdict line; armed checking is read-only; the aggregate is the one
  mutation exit code a build may gate on.
