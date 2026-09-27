# Testing windtrap

How windtrap tests itself. `dune runtest` runs everything; scope it with
a directory (`dune runtest test/cram/run`) or run a built test
executable directly with `-f` while iterating. This file is the home of
every "why" a dune file under `test/` points at; the dune files say only
what is non-obvious.

## The four families

`test/` has four families, the support library `support/` and one `dune`
(the self-aliases, below).

- `unit/`: windtrap suites over the library's modules, one executable
  each (`test_<module>.ml` tests `lib/<module>.mli`), reaching the
  internals through `Windtrap.Private` (the re-export block that exists
  for them and for `bin/`; never named by user code). `test_ppx_runtime`
  pins `ppx/runtime` and `ppx/config`. `Run.execute` refuses to nest
  inside a run, so `test_run`, `test_windtrap` and `test_ppx_runtime`
  record their runs as they initialise, before their own run.
  `render_fixtures.ml` is the synthetic run data the report suites
  render, and `gallery.ml` marks the escape sequences of the report
  galleries by their role. Address one suite by running it:
  `dune exec test/unit/test_gen.exe -- -f shrink`. The report's
  transcripts, the help and the JUnit documents are `expect_file`
  baselines under `unit/expected/`.
  `test_stateful_domains` holds the stateful tests on several domains,
  in an executable of its own because a process that has spawned a
  domain can never fork again. Its systems give results that depend on
  the domain a call runs on, never on a race, so every verdict is
  deterministic. Under mutation testing it spawns nothing: a stateful
  test on several domains then runs each program on the test's domain,
  and each test that spawns a pool skips. Its scenarios that leave a
  domain running forever or spawn every domain the runtime has run in
  children forked as the module initialises, before any domain exists.
  `runtime/` holds the suites over the runtime's modules:
  `test_coverage` and `test_mutate`, each with a child executable (for
  the at_exit dump and for a registration that runs before any window
  opens); `test_instr`, what the two file formats share (the header, the
  build paths, the atomic write); and `test_verdicts`, the verdict
  lattice and the verdict file. `mutate_loop/` drives the fork loop
  through a real spawned process (a loop that forks cannot be observed
  from inside its own image), with `plain_main.exe` as the zero-mutant
  control that must be declined by name, `runaway/` for the hit-count
  budget and `inline/` for an armed inline partition.
  Under `semantics/`, `coverage/` and `mutate/` are the
  semantics-preservation suites: a fixture library instrumented
  unconditionally through `(pps)`, run and compared with the
  uninstrumented answer (tail calls, evaluation order, laziness, exit
  codes), then the in-process registry read to prove the instrumentation
  counted. The mutation twin compiles the same sources a second time with
  no rewriter (`mutate/baseline/`), so "identical" is measured against
  a real twin rather than an expectation someone could edit to match a
  defect. Each has a link-only executable listing the fixture library
  alone: the rewriter's `ppx_runtime_libraries` inject the runtime, and
  nothing may pull the core in. `integration/` asks whether an
  instrumented build compiles and registers, one library per question:
  generated code under fatal warnings, a shadowed `not` and `bool`, the
  typing-order and expected-type corpora, and a registration suite that
  arms each mutant and checks that the library then behaves as the
  mutant's text says. An instrumenter change that cannot keep the
  semantics suites green is rejected, not accommodated; grow the fixtures
  with every expression form the instrumenter learns.
- `cram/`: cram sessions, one directory per subject. Cram pins stdout,
  stderr and the exit code natively, and `dune promote` is its
  acceptance.
  `run/` pins a suite as a process, over the fixture executable
  `suite_main.exe`: the exit code and the stream of each way a run ends
  (`run.t`), what a report leaves outside the process (`report.t`: the
  bytes under `--stream`, the logs, the replay line run as pasted), `-u`
  and `--corrected` (`baselines.t`) and the session without dune
  (`nodune.t`, below). What a message says is pinned by the unit suite
  of its module; a session shows its line beside the exit code and the
  streams that only a real process has.
  `broadcast.t` lays out two stanzas in a scratch project, as dune does,
  and runs both under the same variables: a mirror's emptied selection,
  `WINDTRAP_MUTATE` on a suite with no mutant and a mirror's relative
  path. Its second stanza runs `mutant_main.exe`, whose library is
  instrumented in every build.
  `signals.t` sends INT, TERM and HUP to a waiting run through
  `send_signal.exe`, which holds the run's pid since a shell's
  background job ignores INT: the printed failures stay, the summary
  counts the tests not run, the process dies by the signal, and no JUnit
  file is written. It holds the rest of the facade's signal contract
  too: a signal ignored at start, a signal between two tests or during a
  release, a second signal, a forked child, and the handlers a run puts
  back.
  `inline.t` holds the inline runner's protocol: `inline/runner.exe` is
  the main the `inline_tests` backend generates, over fixtures that
  belong to no library, and the session runs it with dune's protocol
  argv in a stated environment. The fixtures are the partitions and
  their listing, the undriven-registration guard, a library's tests left
  to its own runner by a suite and a runner that link it, the
  corrections a partition writes (a stale payload, sanitized text,
  trailing output), the failures that withhold one (an unreadable
  source, an uncaught exception, a test that also failed outside its
  expectations, a raising release), and a node never reached.
  `bin/` pins the `windtrap` command: its dispatch (`windtrap.t`), where
  the two merges find their files and which they merge (`data.t`), the
  coverage and mutants reports (`coverage.t`, `mutants.t`), what they
  cannot read (`unreadable.t`), and two executables that disagree about
  one library (`two_executables.t`). `mkdata.exe` writes the synthetic
  dumps and verdict files the sessions merge; `pins_add.exe` and
  `pins_sub.exe` are instrumented suites over `calc.ml`, a library under
  both backends in every build.
  `ppx/` holds the rewriters' sessions: their expansions and refusals,
  one session per rewriter (`coverage.t`, `mutate.t`, `expect.t`), over
  fixtures that are never compiled. Each block runs `pp.exe`, a
  standalone ppxlib driver linking every rewriter, with `-apply` naming
  the ones it applies. Under `-apply` ppxlib runs the instrumenting
  rewriters in link order, so `coverage_first.exe` links them the other
  way round for the one composition `pp.exe` cannot run.
  `elide.exe` condenses each registration module to its file and its
  table of points or sites, and each session shows one in full. The
  outputs pin generated bytes, point indexes and byte extents included,
  so editing a fixture reshuffles them: read every promoted diff.
  `generated.ml` stands in for a deriver's generated code, and
  `expect_typing.t` types the expansion of `wrong_run.ml` against the
  installed libraries to pin the type error of a mistyped
  `Expect_test_config.run`. A golden of text the compiler decides runs
  only from the OCaml version that prints it, so such blocks have
  sessions of their own: `coverage_functions.t` from 5.2 (the printer's
  spelling of a function), `expect_typing.t` from 5.4 (a type error's
  wording); the conformance corpus gates `escaped_strings.ml` and
  `hello_async.ml` the same way.
  `coverage.t` holds the two backends' attribute grammars in parity:
  both accept every spelling of `spellings.ml`, an `off` with a reason
  included, and each coverage refusal reads as the mutation one once
  the namespace is swapped.
- `inline/`: real `(inline_tests)` libraries under dune's own runner:
  `forms/` (the payload-shape matrix, `let%test` and `module%test`),
  `strict_flags/` (the generated code under `-w +a -warn-error +a`) and
  `config/` (the tests a shadowed `Expect_test_config` governs).
- `conformance/`: the ppx_expect corpus (below).

`support/` holds what the suites share and nothing installs. In
`windtrap_test_support`, `Scratch.dir` is a temporary directory removed
at exit (a forked child removes nothing), and `Scratch.remove_tree`
never follows a symbolic link. `slashed` spells a native Windows path
with `/`, as windtrap reports it. `Child.environment` states a child's
whole environment (`PATH`, `HOME`, the temporary directory variables,
`SYSTEMROOT`, the locale, `WINDTRAP_COLOR=never`, then the bindings a
test gives), and `Child.run` keeps its standard output and standard
error apart and gives it an empty standard input. A child started this
way sees no `WINDTRAP_*`, `CI` or `GITHUB_ACTIONS` of the machine it
runs on. `Child.forked fn` runs `fn` in a forked child and hands back
the string it returns: a test that spawns a domain spawns it there,
since a process that has spawned one can never fork again, and the
mutation loop forks from the test's process.

`Recorded` makes a suite's `Run.execute` and `Run.list_selection` calls
as it initialises, in a stated environment and with a scratch log
directory, for `test_run` and `test_windtrap` to judge with windtrap's
verbs. `drive/drive.exe` is the transcript driver of the conformance
corpus's rules: it spawns a runner through `Child.run` (dune's
`INSIDE_DUNE` passed through), masks what is measured rather than
computed, and records the standard output, then the standard error
under a `--- stderr ---` line, and the exit code, each diffed against a
golden. A diff rule needs its file even when the runner corrected
nothing, which a cram session does not give.

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
loop's scenarios pass `--mutate=test/unit/mutate_loop/` so the
population they fork over is their own fixtures; `test_mutate` registers
its synthetic sites under `t/`, a directory no instrumented source is
in, and reads the catalogue of its own files alone; and
a suite that reads the merge's report of a child linking the core pins
the child's row, not the padding of a table the core's rows widen.

## Accepting a golden

Every golden is accepted the way windtrap tells its users to accept one,
and every promoted diff is reviewed as a code change.

- Cram sessions (`test/cram`, `examples/x-blueprint/test/cram`) and rule
  goldens (the `.expected` files under `test/unit/semantics` and
  `test/conformance`): `dune promote` after the failing `dune runtest`.
- Inline suites (`test/inline/forms`, `test/inline/strict_flags`,
  `test/inline/config`, the corpus): the runner runs under
  `--corrected`, dune diffs the `.corrected` file, and `dune promote`
  accepts it.
- The unit suites' baselines (`expect` literals, and the files under
  `test/unit/expected/`): a suite that holds one runs under
  `--corrected`, so `dune promote` accepts a mismatch; review with `git
  diff`. The coloured
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
no mutation stanza; the sessions of `test/cram/bin` run it as a child,
and its coverage counts what those children executed. Two aliases in
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
scratch library's row alone, never the total. Its dump lands under the
scratch directory's `_windtrap`, never under `_build/_coverage`, so it
never enters the number. Dune 3.24, the release CI installs, reports a
`(package windtrap)` dependency as a cycle under the coverage backend, so
the coverage job binds `SKIP_PACKAGE_SESSIONS=true` to leave out the
sessions that have one, `nodune.t` and `cram/ppx/expect_typing.t`; a dune
built from main runs them under every configuration.

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
outputs start (the cram sessions and the conformance driver) write
their dumps only when dune runs the rule, and `--force` reruns alias
actions only, so a warm tree or a cache hit leaves their dumps out: a
second `@self-cover` in a warm tree read 91.8% where a cold one read
92.5%. A number to compare with another is measured in a
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

`test/cram/run/nodune.t` holds the instrumentation story for a tree
built without dune: it compiles an instrumented library and a plain test
executable with `ocamlopt` alone, against the installed windtrap found
on `OCAMLPATH` or beside the compiler's own library, runs it from a
scratch directory under no build directory, and reports with the
installed binary. Its stanza depends on `(package windtrap)` and on
`test/cram/ppx`'s `pp.exe`, a ppxlib standalone driver whose `-apply`
names the backend, the one-time step a findlib user makes. The manual's
"Without dune" sections are that session. The ocamlfind route
(`ocamlfind ocamlopt -package windtrap`) is documented from the
installed `META` and run by no test: `ocamlfind` is not in the lock
file, and adding it as a test-time dependency is a maintainer call.

## CI

`.github/workflows/build.yml` runs `dune build @runtest` on Linux, macOS
and Windows with `WINDTRAP_JUNIT` pointing at a directory (one report
per suite) and uploads the reports; failures annotate the diff themselves,
because the runner detects GitHub Actions. A Linux-only job runs
`@self-cover`: the coverage number is a property of the suite, not of
the OS, and instrumented builds are slower. `--shard` is
unused: the suite is about half a minute, and exercising a flag is not a
reason to add a matrix dimension.
