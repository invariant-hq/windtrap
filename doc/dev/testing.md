# Testing windtrap

How windtrap tests itself. `dune runtest` runs everything; scope it with
a directory (`dune runtest test/cli`) or run a built test executable
directly with `-f` while iterating. This file is the home of every "why"
a dune file under `test/` points at; the dune files say only what is
non-obvious.

## The five families

`test/` has five families, the support library `support/` and one `dune`
(the self-aliases, below).

- `unit/`: windtrap suites over the library's modules, one executable
  each, reaching the internals through `Windtrap.Private` (the re-export
  block that exists for them and for `bin/`; never named by user code).
  `Report_sections` is pinned through `test_report` and
  `test_report_junit`, `Mutate_loop` in `instr/loop/`, and
  `test_ppx_runtime` pins `ppx/runtime` and `ppx/config`.
  `test_ppx_runtime` and `test_windtrap` drive `Run.execute` in process,
  which refuses to nest inside a run, so they are plain executables over
  `harness.ml`; `test_run` records its runs as it initialises, before its
  own run. `render_fixtures.ml` is the synthetic run data the report
  suites render, `xml_check.ml` checks a JUnit document for
  well-formedness, and `expect_config/` is an `(inline_tests)` library
  that shadows `Expect_test_config`. Address one suite by running it:
  `dune exec test/unit/test_gen.exe -- -f shrink`. The report's
  transcripts, the help and the JUnit documents are `expect_file`
  baselines under `unit/expected/`.
- `cli/`: cram sessions over the fixture executable `suite_main.exe`,
  pinning the command line end to end: selection, the report and its
  renderers, JUnit, the refusals, exit codes, `-u` and `--corrected`
  (`baselines.t`), a test that calls `exit` (`process.t`), the mutation
  flags (`mutation.t`) and the session without dune (`nodune.t`, below).
  `broadcast.t` lays out two stanzas in a scratch build context, as dune
  does, and runs both under the same variables: a mirror's emptied
  selection, `WINDTRAP_MUTATE` on a suite with no mutant and a mirror's
  relative path, each beside the command line's.
  `signals.t` sends INT, TERM and HUP to a waiting run through
  `send_signal.exe`, which holds the run's pid since a shell's
  background job ignores INT: the printed failures stay, the summary
  counts the tests not run, the process dies by the signal, and no JUnit
  file is written. `instrumented.t` runs `mutant_main.exe`, whose
  library is instrumented in every build, to pin `--junit` beside
  `--mutate` and `--arm`. Cram pins stdout, stderr and the exit code
  natively, and `dune promote` is its acceptance.
  `cli/inline_runner/` holds the inline runner's protocol: one
  executable per fixture mirrors the backend's generated main, and
  `drive/drive.exe` spawns it with dune's protocol argv in a stated
  environment (`Windtrap_test_support.Child.environment`, never the
  caller's), masks what is measured rather than computed, and records
  the standard output, then the standard error under a `--- stderr ---`
  line, and the exit code, each diffed against a golden. The same driver
  runs the conformance corpus's runners and `ppx/expect/correction/`;
  `cross_partition/` has its own, so that its two runs share one
  process. The fixtures are the cross-partition promotion contract, a
  partition run from a cwd with no sources, the undriven-registration
  guard, a library's tests left to its own runner by a suite and a
  runner that link it, a masked assertion failure, a raising release,
  the slow and verbose mirrors, and tail-position attribution.
- `ppx/`: one directory per rewriter, `expect/`, `coverage/` and
  `mutate/`, each with expansion and rejection goldens produced by a
  standalone ppxlib driver (`expect_pp.exe`, `coverage_pp.exe`,
  `mutate_pp.exe`) over fixtures that are never compiled. Each
  directory's `dune.inc` holds a rule pair per fixture, written by
  `gen/gen_dune.exe`: a new fixture is one `X.ml` and one `X.expected`,
  then `dune build @gen-rules --auto-promote`. The goldens pin generated
  bytes, point indexes and byte extents included, so editing a fixture
  reshuffles them: read every promoted diff. `RULES.md` is the
  catalogue of every rule the three rewriters implement, each with the
  interface line that states it and what pins it; neither self-coverage
  nor self-mutation measures `ppx/`, so the catalogue is the rewriters'
  completeness measure. `check_rules.exe` fails `runtest` when a row
  names a fixture that does not exist or does not cite the rule, when a
  fixture cites a rule whose row does not name it, and when a row is
  unpinned without a `STATED-NOT-TESTED` reason. `expect/inline/` and
  `expect/strict_flags/` are real `(inline_tests)` libraries under dune's
  own runner (the payload-shape matrix; the generated code under `-w +a
  -warn-error +a`). `coverage/semantics/` and `mutate/semantics/` are the
  semantics-preservation suites: a fixture library instrumented
  unconditionally through `(pps)`, run and compared with the
  uninstrumented answer (tail calls, evaluation order, laziness, exit
  codes), then the in-process registry read to prove the instrumentation
  counted. The mutation twin compiles the same sources a second time with
  no rewriter (`semantics/baseline/`), so "identical" is measured against
  a real twin rather than an expectation someone could edit to match a
  defect. Each has a link-only executable listing the fixture library
  alone: the rewriter's `ppx_runtime_libraries` inject the runtime, and
  nothing may pull the core in. `mutate/integration/` asks whether an
  instrumented build compiles and registers, one library per question:
  generated code under fatal warnings, a shadowed `not` and `bool`, the
  typing-order and expected-type corpora, and a registration suite that
  arms each mutant and checks that the library then behaves as the
  mutant's text says. `coverage/after_mutate/` and
  `mutate/after_coverage/` pin the two backends composed in either
  order, and `generated/` stands in for a deriver's generated code. The
  attribute grammars of the two backends are held in parity twice: every
  `reject_*` fixture with a twin of the same name on the other side is
  diffed against the twin's output modulo the namespace, and
  `coverage/parity_spellings.ml` runs through both drivers, the mutate
  one with the namespace swapped. The fixtures without a twin are the
  intended differences: only `[@mutate off]` takes a reason
  (`coverage/reject_off_reason`, `mutate/reject_off_number`,
  `mutate/reject_off_two_reasons`). An instrumenter change that cannot
  keep the semantics suites green is rejected, not accommodated; grow the
  fixtures with every expression form the instrumenter learns.
- `instr/`: the runtimes and the reporting commands. `coverage/` and
  `mutate/` are windtrap suites over the two runtimes, each with a child
  executable for the at_exit dump or for arming; `verdicts/` covers the
  verdict lattice, file format and atomic write; `loop/` drives the fork
  loop through a real spawned process (a loop that forks cannot be
  observed from inside its own image), with `plain_main.exe` as the
  zero-mutant control that must be declined by name, `runaway/` for the
  hit-count budget and `inline/` for an armed inline partition;
  `coverage_cmd/` and `mutants_cmd/` drive `bin/main.exe` as a subprocess
  over synthetic files and over real instrumented children, including
  two executables that disagree about one library.
- `conformance/`: the ppx_expect corpus (below).

`support/` is `windtrap_test_support`, a library the suites share and
nothing installs. `Scratch.dir` is a temporary directory removed at exit
(a forked child removes nothing), and `Scratch.remove_tree` never
follows a symbolic link. `Child.environment` states a child's whole
environment (`PATH`, `HOME`, the temporary directory variables, the
locale, `WINDTRAP_COLOR=never`, then the bindings a test gives), and
`Child.run` keeps its standard output and standard error apart and
gives it an empty standard input. A child started this way sees no
`WINDTRAP_*`, `CI` or `GITHUB_ACTIONS` of the machine it runs on.

Two conventions the instrumentation suites share. Fixtures under test
are preprocessed with `(pps ppx_windtrap.coverage)` or `(pps
ppx_windtrap.mutate)` rather than through an `(instrumentation)` stanza,
so their points and mutants exist in a plain `dune runtest` build and no
`--instrument-with` is needed for a suite to mean something. And under
`--instrument-with` every suite links an instrumented core, so nothing
may assume it is alone in a process-global registry: coverage suites
read only the reports of their own files; suites that must not write
under the real `_build/_coverage` point the dump elsewhere with
`WINDTRAP_COVERAGE_FILE` on the action, because the destination is
resolved at the first registration, before the test's own code runs; the
loop's scenarios pass `--mutate=test/instr/loop/` so the population they
fork over is their own fixtures; the two suites that register
synthetic sites tell their own registrations from the process's by time,
whatever the catalogue holds at their module load being not theirs; and
a suite that reads the merge's report of a child linking the core pins
the child's row, not the padding of a table the core's rows widen.

## Accepting a golden

Every golden is accepted the way windtrap tells its users to accept one,
and every promoted diff is reviewed as a code change.

- Cram sessions (`test/cli`, `examples/x-blueprint/test/cram`) and rule
  goldens (the `.expected` files under `test/ppx`,
  `test/cli/inline_runner` and `test/conformance`): `dune promote` after
  the failing `dune runtest`.
- A `ppx/` directory's `dune.inc`, after a fixture is added or removed:
  `dune build @gen-rules --auto-promote`.
- Inline suites (`test/ppx/expect/inline`, `test/ppx/expect/strict_flags`,
  `test/ppx/expect/config`, `test/unit/expect_config`, the corpus): the
  runner runs under `--corrected`, dune diffs the `.corrected` file, and
  `dune promote` accepts it.
- `test/unit/expected/`: a mismatch prints its acceptance, the suite's
  own executable under `-u` (`dune exec test/unit/test_report.exe -- -u`
  for the report's transcripts); review with `git diff`. The coloured
  transcripts pin every escape sequence by its role, as a mark such as
  `«r|FAIL»` with `»` for the reset: the suite checks that the marks turn
  back into the exact bytes and that no escape is left unmarked, so the
  comparison is of the styled output and never of stripped text. A plain
  transcript is checked to hold no escape at all.
- The corpus's vendored fixtures are upstream bytes and are never
  promoted from windtrap's output (below).

## Windtrap under its own instrumentation

`lib/` carries both instrumentation stanzas and `bin/` the coverage one,
inert without the flag: a plain `dune runtest` is uninstrumented and free.
`bin/` is the `windtrap` executable, which no suite links, so it carries
no mutation stanza; the command suites run it as a child, and its
coverage counts what those children executed. Two aliases in
`test/dune`, `self-cover` and `self-mutate`, measure windtrap with
windtrap. They are maintainers' tooling: the manual, the examples and
the skill teach no alias, and show a user the instrumented
`dune runtest` and the merge as two commands.

```
dune build @self-cover --instrument-with ppx_windtrap.coverage
WINDTRAP_MUTATE=1 dune build @self-mutate --force --instrument-with ppx_windtrap.mutate
```

Their shape: `(alias_rec runtest)` and `(alias_rec ../examples/runtest)`
run every test family and every example first, because `.coverage` and
`.mutants` files are written by test executables at exit and are not
declarable dependencies, so nothing else can express "after every suite
has run"; the examples too, because the merges read every dump under
`_build` and `examples/` carries instrumented libraries of its own; and
`(universe)` re-runs the milliseconds-cheap merge on every build, without which the
action caches against nothing and silently goes stale. The mutation
command carries the variable because a verdict file exists only when a
suite was asked to mutate (`WINDTRAP_MUTATE` is `--mutate`'s mirror, the
spelling that reaches every stanza), and `--force` because a mutation
run is not a cached artifact. Driven without `--instrument-with`, either
alias rebuilds the executables uninstrumented: the coverage merge then
reads stale dumps and the mutation merge refuses loudly.
The `nodune.t` session compiles a scratch project against the installed
core, which under the coverage backend is instrumented too and adds its
own rows to the scratch project's report; the session therefore pins the
scratch library's row alone, never the total, and it runs under every
configuration. Its dump lands under the scratch directory's `_windtrap`,
never under `_build/_coverage`, so it never enters the number.

`--min 84` is a ratchet: the measured number (86.2% on 2026-08-19, 93.0%
on 2026-09-16) less a few points of headroom. Raise it when the margin is comfortable;
never lower it to make a red build green. Two structural causes hold the
rest down, and neither is a debt to work off: the mutation loop's body
runs only under the mutation backend and its children `_exit` past the
at_exit dump, so a coverage run cannot see it; and the facade's contract
is pinned by cram, where the code runs in a child process per command
and the runtime files one dump per executable, so those points are
exercised and not counted. Chase the branches the report names, not the
percentage.

The number depends on dune's cache. The executables that rules with file
outputs start (the cram sessions, the inline-runner and conformance
drivers) write their dumps only when dune runs the rule, and `--force`
reruns alias actions only, so a warm tree or a cache hit leaves their
dumps out: a second `@self-cover` in a warm tree read 91.8% where a
cold one read 92.5%. A number to compare with another is measured in a
fresh worktree, with an empty build directory and `--cache=disabled` on
every `dune build`, the forced `@runtest` then `@self-cover`.

`ppx_windtrap.coverage` is the backend's one name. A rewriter's
`ppx_runtime_libraries` are added to everything it preprocesses,
instrumentation included, and `ppx_windtrap`'s include the core, so a
backend under that name would make `lib/` depend on itself. The two
backends therefore live in libraries of their own whose only runtime
library is the stdlib-only `windtrap.runtime`, which is also what keeps
the core out of a user's instrumented closure. The ppx runtime and
config libraries carry the coverage stanza so the measured set is whole.
The runtime carries no mutation stanza, and the modules that judge a
mutant (`run`, `report`, `mutate_loop`, the facade and the ppx runtime)
opt out with `[@@@mutate exclude_file]`: a mutant armed inside the
judging process hangs rather than survives.

While working on one file, mutate it alone from the executable that owns
its tests; the loop forks once per mutant and the prefixes narrow the
population it forks over:

```
dune exec --instrument-with ppx_windtrap.mutate test/unit/test_diff.exe -- --mutate=lib/diff.ml
```

A per-executable run exits 0 whatever it found, and a narrowed run
(`-f`, tags, `--shard`, `--failed`, a focus) writes no verdict file, so
the aggregate never merges a partial answer; a survivor is confirmed by
arming it against the whole suite, with `--arm <id>` from the footer or
`WINDTRAP_MUTATE_ARM=<id>` in front of the tree-wide command. Every
forked child runs under a deadline derived from the dry run (its wall
clock, plus ten times the measured timings of the tests that child runs,
floored at one second) and under the runtime's per-site hit-count
budget; a child that overruns or spins is killed with its process group
and scored killed, because a suite that hangs has noticed the change.
Mutation needs `Unix.fork` and declines by name on Windows.

## The conformance corpus

The compatibility promise, that most ppx_expect suites run unchanged
after swapping the pps and the backend, is measured, not asserted.
`test/conformance/` vendors the test suite of a pinned ppx_expect commit
(recorded in `NOTICE`) in two classes: HONORED (runs with matching
semantics: the same tests pass, the same payloads match, the same
mismatches produce corrections) and REJECTED (fails loudly at expansion
with a diagnostic naming the construct, or fails to compile);
`RESULTS.md` lists the files it does not vendor, each with its reason.
`RESULTS.md` states the bar: every file of the pass set passes
unchanged, and every construct windtrap does not support is refused,
both `@runtest` outcomes. `count.exe` computes the numbers from the
corpus's files into `counts.expected` on every `@runtest` (the pass set,
the corrections and how many are byte-identical to upstream's, the
refused), `dune promote` accepts new numbers, and `RESULTS.md` quotes
the file and restates none of them. The byte-identical share is reported
and gated by nothing; `RESULTS.md` says why. `converged/` runs each
corrected source again and shows that it passes.

When a conformance diff appears: reproduce on `@runtest` (there is no
red-by-design alias; a fixture either states a contract windtrap holds
or it is not vendored); decide which side is wrong, the upstream golden
being truth for HONORED files; and record a divergence windtrap should
not follow as a ruling in `RESULTS.md`, dropping the fixture. Never edit
vendored bytes silently: the only permitted tweak is the one-line
`open Corpus_shim` substitution, each listed in `NOTICE`. Re-pinning to
a newer ppx_expect is a change of its own: update the pin, re-vendor,
re-triage every changed file, re-measure, record the numbers. One
golden, `hello_async.compile-rejected.expected`, pins an OCaml type
error and is re-promoted on compiler upgrades.

## Without dune

`test/cli/nodune.t` holds the instrumentation story for a tree built
without dune: it compiles an instrumented library and a plain test
executable with `ocamlopt` alone, against the installed windtrap found
on `OCAMLPATH` or beside the compiler's own library, runs it from a
scratch directory under no build directory, and reports with the
installed binary. Its stanza depends on `(package windtrap)` and on one
ppxlib standalone driver per backend, the one-time step a findlib user
makes. The manual's "Without dune" sections are that session. The
ocamlfind route (`ocamlfind ocamlopt -package windtrap`) is documented
from the installed `META` and run by no test: `ocamlfind` is not in the
lock file, and adding it as a test-time dependency is a maintainer call.

## CI

`.github/workflows/build.yml` runs `dune build @runtest` on Linux, macOS
and Windows with `WINDTRAP_JUNIT` pointing at a directory (one report
per suite) and uploads the reports; failures annotate the diff themselves,
because the runner detects GitHub Actions. A Linux-only job runs
`@self-cover`: the coverage number is a property of the suite, not of
the OS, and instrumented builds are slower. `--shard` is
unused: the suite is about half a minute, and exercising a flag is not a
reason to add a matrix dimension.

## What pins the guarantees

The twelve guarantees of `architecture.md` and the tests that pin them.
A change that breaks one of these tests reopens the design first.

1. Checking never writes to the source tree: `baselines.t` (`-u` refused
   under `CI`, `<file>.corrected` beside the file), `test_baseline.ml`
   ("corrected: the check fails, the correction lands beside the file",
   "update: the check accepts silently and writes in place"), and
   `test_mutate_loop.ml` ("children check the baselines read-only").
2. A baseline is where the source says: `test_baseline.ml` ("a path
   that escapes the root is unresolvable in every mode", "a check
   computes no location and carries the one given") and
   `test_source_patch.ml` ("drift refusal").
3. Every mismatch prints its acceptance: `baselines.t` (`accept: dune
   promote`, and one `accept:` line that rewrites the stale baselines
   and no other), `test_report.ml` ("hints: accept and replay per
   invocation", "the accept and replay lines: one each, on the
   summary") and `test_report_junit.ml` ("bodies carry the
   invocation-spelled hints").
4. Renderers are projections: `junit.t` (a JUnit file that cannot be
   written is a warning and changes no exit code) and `test_report.ml`
   ("report: observe raises nothing of its own", "headline
   projection"). No test compares the renderers with one another.
5. A failing test's captured output is in its report: `test_capture.ml`
   ("bounded tails with drop counts", "the tail starts after the last
   read", "log paths are stable across runs") and `test_report.ml`
   ("captured tail").
6. Every generator shrinks and printers derive: `test_gen.ml` ("map
   renders the pre-image", "with_pp attaches a printer" and the
   shrinking family).
7. Seeds: `test_seed.ml` ("make's stream is frozen", "derive is frozen,
   the path hashed byte by byte") and `test_report.ml` ("seed token
   consistency").
8. Callbacks inside a test's boundary, resources released: `test_run.ml`
   ("each raising release is a release failure, in release order", "the
   fixtures are released under bail", "the directory is removed when the
   attempt ends, however it ends").
9. The exit codes: `test_run.ml` ("the exit code is"), `test_windtrap.ml`
   (an all-skipped selection exits 0) and `cli.t` (usage errors exit 2).
10. Coverage never changes meaning: `ppx/coverage/semantics/` and
    `test_coverage_cmd.ml` ("the run prints no number, and the dump is
    the report").
11. Instrumentation data: `test_coverage_cmd.ml` ("outside any build
    directory: _windtrap/coverage, found by the merge", corrupt and
    foreign files), `test_coverage.ml` ("foreign and corrupt data are
    rejected"), `test_mutants_cmd.ml` and `nodune.t`.
12. A mutant changes meaning only when armed: `ppx/mutate/semantics/`,
    `test_mutate.ml` ("arming a second mutant disarms the first, budget
    included"), `test_mutate_loop.ml` ("an armed run under -u records no
    correction", "an armed mutant that survives says so") and
    `instrumented.t`.

## Stated, not tested

Every statement of an interface is pinned by a test or listed here with
the reason no test pins it. A statement nobody would test goes back to
its interface to be cut. The rewriters' own list is the
`STATED-NOT-TESTED` rows of `test/ppx/RULES.md`.

- `lib/windtrap.mli`
  - `SIGALRM` has no effect on Windows: platform.
  - A timeout cannot interrupt a blocked C call: needs a C stub that
    blocks, and no suite links one.
  - A verb that fails at declaration ends the program: a consequence of
    what `name` raises escaping at declaration, pinned in
    `test_test_tree.ml`.
  - `open Windtrap` brings none of `Private` into scope: a compile-time
    fact, and the unit family has no harness for a file that must not
    compile.
- `lib/test_tree.mli`
  - A node without a known site has no location: needs code compiled
    without debug information, and every suite builds with `-g`.
- `lib/check.mli`
  - A passing verb resolves no location: nothing observable. That it
    calls no printer is pinned (`test_check.ml`, a counting printer).
- `lib/loc.mli`
  - Without `-g`, only an explicit position gives a location: needs a
    second build without `-g`; each half is pinned in `test_loc.ml`.
  - A client tries the recorded file under the project root first: an
    obligation on the caller.
- `lib/capture.mli`
  - `drain` reaches the C stdio streams: needs a C stub that prints
    without flushing, and no suite links one.
  - Calls on one state must not be nested: an obligation on the caller,
    whose consequence would break the test process's own descriptors.
  - `with_capture` raises `Unix.Unix_error` when the descriptors cannot
    be restored, and `abandon` when one cannot: not reachable without
    fault injection.
- `lib/seed.mli`
  - `fresh` is statistically independent of `continued`: a statistical
    property of the published construction, which `test_seed.ml` pins
    bit for bit.
- `lib/gen.mli`
  - The obligations of `Shrink_tree` on caller code (deterministic, no
    random state): obligations on the caller.
  - `sample` drops the successor state: a fact of the signature; `run`'s
    successor is pinned.
  - A `Failure.Timeout` delivered while a candidate is forced: needs a
    timed signal at a forcing point; an exception escaping a forcing is
    pinned.
- `lib/stateful.mli`
  - `pre` and `next` are pure and the model persistent: an obligation
    on the caller.
  - Deeper candidates may repeat their parent or be longer, and
    `shrunk N steps` counts a repeat: the absence of a guarantee; the
    one that holds, no immediate candidate is longer, is pinned.
  - One scope per candidate, about 2n candidates: the scope count is
    pinned, and the cost follows from `Shrink_tree.list`'s chunk
    schedule, pinned in `test_gen.ml`.
- `lib/run.mli`
  - A selection field added to the configuration must be cleared for a
    subset: a rule for whoever edits the record.
  - `fixture #<n>` when no site is found: needs code compiled without
    debug information.
  - On Windows there is no timer: platform.
  - The order of the restorations, outside the timeout window and the
    capture: it shows only where a process cannot remove its working
    directory (Windows); each failing step is pinned on its own.
  - The last-failed record is written after the store update: no hook
    runs between the two.
  - The record is rewritten atomically: a consequence of
    `Os.atomic_write`, pinned in `test_os.ml`.
  - A signal at the last point finds both done: the point lies between
    two runner steps with no gate, so a test would race it.
- `lib/report.mli`
  - `terminal` turns the live line on only when standard output is a
    terminal and the run is not under GitHub Actions: needs a
    pseudo-terminal, and the unit suites run with the output captured.
  - Under `--stream`, `interrupted` drains the streamed output first: a
    consequence, since it ends in `finish`, whose drain is pinned.
  - A caller that forks flushes first: an obligation on the caller; the
    loop's side is pinned in `instr/loop/`.
- `lib/report_sections.mli`
  - The producer sorts and dedupes `uncovered`: an obligation on the
    producer, whose side is pinned in `instr/coverage_cmd/`.
- `lib/report_junit.mli`
  - The file is written at every verbosity: `write` takes no verbosity,
    and the default is pinned by `junit.t`.
- `lib/os.mli`
  - The clock and the module's initialization raise `Sys_error`:
    platform, since the monotonic clock never fails on macOS and Linux.
  - `setenv` raises when the environment cannot be changed: platform,
    since `putenv` fails only on `ENOMEM`.
  - `is_tty_stdout`: needs a pseudo-terminal on standard output, and the
    false side alone would not tell a constant from the function.
  - `Sys.Break`, `Out_of_memory` and `Stack_overflow` pass through
    `atomic_write` after its cleanup: needs a fault-injection hook, and
    none is added to the module for its suite.
  - Without `INSIDE_DUNE`, an executable outside every build directory
    roots the project at the working directory and logs under the
    temporary directory, and one whose own name starts with `_build`
    lies in none: needs the executable at a chosen path; that its
    directory decides is pinned.
- `lib/mutate_loop.mli`
  - A mutant is killed when its child recorded no test, did not exit 0,
    or left no complete line: a child leaves only through `_exit 0`
    after its line, so the last two are its death, pinned (a crash, a
    deadline); the first cannot happen, the allowlist being the dry
    run's own tests.
  - The deadline's tenfold multiplier and the dry run's wall term, as
    numbers: pinning them costs over a second per run against the loop
    suite's time bound; the behaviour at both edges is pinned.
  - Refused on Windows: platform.
  - A child that cannot arm its mutant, or is refused at startup: the
    child arms an identifier of its own image's catalogue and repeats
    the dry run's startup checks, so no suite reaches it.
  - `pipe`, `fork` or `waitpid` fails: fault injection in the parent's
    own system calls.
  - A refusal after survivor blocks were printed: reachable only through
    the two causes above; the refusals before any block are pinned.
  - An ambiguous identifier is refused: the instrumenter emits one site
    per position and rewrite, so no instrumented build has one; the
    runtime's `Ambiguous` and the armed run's refusal are pinned.
  - A signal after the last child ends, before the verdict file is
    written: the file is written and the loop dies by the signal. No
    observable instant lies between the last reap and the write; a
    signal after the write is pinned.
  - "A second signal excepted": what a second signal interrupts depends
    on where it lands, and no gate can be put there.
- `lib/runtime/instr.mli`
  - `Corrupt` when the file shrinks while it is read: needs a concurrent
    writer to truncate it between two reads.
  - Two concurrent writers leave the last renamed file whole: a race
    that a torn writer could pass by chance; the rename it relies on is
    pinned.
  - Concurrent writers never share a temporary name, and `Sys_error`
    follows ten failed attempts: a collision of 24 random bits cannot be
    provoked; the `Sys_error` is pinned.
- `lib/runtime/mutate.mli`
  - `armed_hits` saturates at `max_int`: needs `max_int` evaluations of
    one site.
- `bin/data_files.mli`
  - `warnings` raises `Assert_failure` on a `Fresh` file among the first
    three: `Data_files` lives in the `windtrap` executable, which no
    suite links, and both commands pass it stale files only.
- `ppx/runtime/ppx_runtime.mli`
  - An `.xml` JUnit target is one file that every partition replaces: a
    consequence of the per-partition suite name and of `report_junit`'s
    rule, both pinned.
- `ppx/config/expect_test_config.mli`
  - The sanitized text is also what a correction writes: the generated
    call passes the sanitized text as the one `actual`, which a
    correction records (both pinned); end to end it needs a failing
    inline run, which fails the build.
- `ppx/ppx_windtrap.mli` and `ppx/mutate/instrument.mli`, as rows of
  `test/ppx/RULES.md`
  - E31, a test registers when the structure that holds it is evaluated:
    a consequence of E1.
  - M15, an armed ordering differs from its `after` text on NaN: a
    consequence of M4 and of the float comparisons on NaN.
  - M21, the chain rule applied to `con`: unreachable, a consequence of
    M17.
