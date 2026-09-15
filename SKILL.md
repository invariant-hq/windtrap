---
name: windtrap-testing
description: Guides writing high-quality OCaml test suites with windtrap 0.2 — choosing the test kind with the strongest oracle, suite organization and dune conventions, the coverage and mutation workflows that verify the tests themselves, and the discipline that keeps a suite trustworthy. Use when writing tests, adding a test suite, reviewing tests, setting up coverage or mutation testing, or deciding which kind of test fits a behavior. Triggers on phrases like "write tests for this", "add a test suite", "test this function", "property test this", "snapshot test", "check coverage", "run mutation testing", or "review these tests".
---

# Testing with Windtrap

A test suite is the executable statement of what the code is supposed to
do. When the same session writes both the code and its tests, a test that
merely re-states what the code does is worthless — it will agree with any
bug the code has. Everything in this skill serves one principle:
**maximize oracle strength**. Write the test whose assertion is hardest
to satisfy by accident, prove every test can fail, and never let an
acceptance workflow bless behavior nobody reviewed.

Windtrap is one library for unit, property, stateful, expect, and
expect tests, plus code coverage and mutation testing. `open Windtrap`;
`test`/`group` declare inert data; `run` executes and returns the exit
code, which `main` applies — `let () = exit @@ run "mylib" […]`: 0 all
passed, 1 any failure, 2 nothing ran (the filter-typo case — treat it as
failure, never as success).

This file is the decision layer — which test, which conventions, which
discipline. The mechanics live in the manual, one chapter per subject;
§4 says which chapter answers which question, and §4's own content is
only what the chapters do not say. Read the matching chapter whenever
you need mechanics beyond what this file carries.

## 1. Survey, then derive the obligations

If the repo already has a suite, follow its framework and conventions
even where you would choose differently — a suite split across two
frameworks costs more than either framework's flaws. Grep the `dune`
files, not just `test/`: `(test`/`(tests` stanzas, `(cram` and `*.t`
files, `(inline_tests)`, and the `(libraries ...)` of test stanzas tell
you the framework, the layout, and how the suite runs. If starting
fresh, use windtrap.

Then derive the test list from the interface, **before reading the
implementation**. The `.mli` is the spec: every exported value is an
obligation; every documented exception and `Error` case is an
obligation; every invariant or law stated in a doc comment is an
obligation. Write the list down and check it off. A suite conceived
from the interface constrains what the code *should* do; one written
by reading the implementation merely describes what it does — the
self-confirming trap (§2) at suite scale. Where the `.mli` is silent
on a behavior you must test, the spec has a gap: surface it to the
maintainer, or record the assumption visibly in the test's name,
rather than silently inventing the contract.

## 2. Choose the strongest oracle the behavior allows

Work down this ladder and take the **first** row that fits. Each row
constrains strictly more behavior per line of test code than the rows
below it.

| The code under test is | Write | Why it is strongest here |
|---|---|---|
| A pure function with a law — codec, parser/printer, normalizer, arithmetic, ordering | `prop` over the law | One law constrains the whole input space; shrinking hands you the minimal counterexample |
| A stateful API — container, cache, store, pool, anything with a lifecycle | `stateful` against a model | Checks laws over *sequences* of calls; finds interaction bugs no unit test reaches |
| A pure function where only specific points are specified | `test` + `equal` through a testable | Exact expected values, written by hand from the spec |
| An executable's observable behavior — CLI parsing, exit codes, error messages, file effects | Cram test through the real binary | Tests the wiring no unit test reaches; doubles as CLI documentation |
| Rendered or serialized output too large to hand-write — help pages, reports, formatted trees | `expect` / `expect_file` / `[%expect]` | A reviewed baseline beats a hand-copied string; promotion keeps it current |
| A value against a bound | `less`/`at_most`/`greater`/`at_least` with `~than` | The failure prints the bound and the value; `is_true (n > 0)` prints `true` against `false` |
| A claim about a value no equality or order captures | `satisfies ~msg` | Last resort — the failure at least prints the value and names the predicate |

Three rules outrank the table:

- **Expected values come from the spec, never from running the code
  under test.** An expected value captured from the implementation's own
  output is a baseline with extra steps and none of the review
  discipline — if you cannot derive the expected value by hand, write it
  as an `expect` so the acceptance workflow (and its reviewer) owns it.
  The operational tell: write the assertion *before* first running the
  test. If you had to run the code to learn the value, it was a
  baseline all along.
- **Normative vs descriptive.** Properties, stateful models, and
  hand-derived `equal` expectations are *normative*: they encode the
  spec, and when one fails, suspect the code. Expect tests, inline or
  in files, are *descriptive*: they pin current behavior, and when one fails the
  question is "was this change intended?". A suite of only descriptive
  tests asserts nothing except that the code does what it does — every
  module needs a normative core.
- **Blackbox first.** Test through the public `.mli`; an internal helper
  is tested through the public path that reaches it, or the suite
  calcifies the current decomposition and breaks on every refactor. For
  an effectful function with a pure core, extract the pure core and test
  that — don't mock the effects around it.

### The bad-test catalog

Recognizing a worthless test matters as much as writing a strong one.
Reject these shapes on sight — in review, and in your own output.

*Tests that cannot fail:*

- **Self-confirming** — the expected value was captured by running the
  code under test. It agrees with every bug the code has. If the
  expected value cannot be derived by hand from the spec, the test is
  a baseline; write it as one (§4) so the acceptance workflow and its
  reviewer own it.
- **Vacuous** — executes code but checks nothing that can break: no
  assertion at all, `is_some` where the *value* matters, "does not
  raise" on a function that cannot raise. Green from the day it was
  born; the mutation survey prints it under every mutant it ran and
  failed to notice (§6).
- **Tautological** — re-derives the answer with the implementation's
  own algorithm (a "property" computing the same fold), or tests the
  language: that a record field holds what the constructor assigned,
  that `List.sort` sorts. Can only fail if OCaml is broken.

*Tests that fail wrong:*

- **Blind boolean** — `is_true (a = b)`, `is_true (n > 0)`: the
  failure prints `expected true` and hides the data. Use testables and
  the ordering verbs — `greater int ~than:0 n` keeps the bound and the
  value. And weak predicates are weak oracles
  too: `is_true (apply Sub 10 4 > 0)` survives the `a - b → a + b`
  mutant; `equal int 6 (apply Sub 10 4)` kills it.
- **Overfit** — asserts incidental detail: the whole help text to
  check one flag, exact float equality where a tolerance witness
  belongs, the order of an unordered collection (`slist` exists),
  timestamps, absolute paths. It breaks on unrelated edits, which
  trains everyone to update tests reflexively — the exact habit §7
  forbids.
- **Coupled** — depends on another test's side effects, shared mutable
  state, the wall clock, the network, or directory-listing order. It
  breaks the moment the suite is selected differently — `-f`,
  `--failed`, and `--shard` all change which tests run. Use
  `bracket`/`fixture`/`temp_dir` for state, `setenv`/`chdir` for the
  environment and the working directory (the runner puts both back);
  mask time (§4).

*Tests at the wrong level:*

- **Over-mocked** — needs several fakes to check one line; it proves
  the mocks call each other and calcifies the current decomposition.
  Move up to a cram test of the real binary, or extract the pure core
  and test that.
- **Baseline-of-everything** — one giant expectation nobody reads,
  churning on every change until promotion becomes a reflex. A
  baseline must earn its size: small, focused, masked — with the parts
  that matter asserted via `equal`/`contains` beside it.
- **Property without a law** — when there is no genuine law
  (round-trip, invariant, oracle agreement, algebraic identity,
  metamorphic relation), a property is noise around an example; write
  `cases` instead.

The catalog is mechanically checkable: nearly every entry either
survives mutants — §6's loop finds it — or fails with a message that
cannot diagnose, which §4's rules catch. When reviewing tests, run the
file-scoped mutation loop before trusting your eyes.

## 3. Suite layout and dune conventions

**Split suites along mechanical and lifecycle boundaries; within a
suite, organize by subject, never by kind.** A separate
directory-plus-stanza is justified by a different runner (cram), a
different test mechanism (`expect/`'s inline-tests library), a
different dependency closure or environment (integration tests that
need a running service), a different cadence (a nightly soak suite),
or a different lifecycle (`failures/`, the bug backlog). What never
justifies one is *test kind*. The §2
ladder is a per-behavior choice — the parser's round-trip law, its
example tests, and its error-message baseline together form Parser's
contract, and they belong in the same file. Splitting `test/unit/` from
`test/property/` from `test/expect/` scatters one module's contract
across three trees and leaves "what constrains Parser?" with no answer
location. Kinds are already selectable at run time: property and
stateful tests carry automatic tags (`--exclude-tag prop` for an
example-only pass), slow tests carry `slow` (`--exclude-tag slow` drops
them), and `-f` filters by path. Extra executables also carry a real bill:
each one links the library, splits the coverage denominator, and
re-runs the mutation loop over every file it links — the `windtrap
mutants` merge makes the *answer* right, not the cost.

Within the unit suite: one test file per source module, and **one test
stanza per file** — each file is its own suite, ending in its own
`run`; the plural `(tests (names …))` stanza declares them in one
block. Every child of `test/` is one suite directory with its own
`dune` file, named by *why it is separate*; `test/dune` itself holds
only the project verdict aliases:

```
test/
  dune                 ; the @cover/@mutate verdict aliases, if you want them (below)
  unit/                ; THE windtrap suite: laws, examples, stateful, expectations
    dune               ; one (test) stanza per file, --corrected and diff? for baselines
    test_parser.ml     ; everything that constrains Parser — its own run
    test_eval.ml
    help.expected      ; a committed file baseline, named in the stanza's deps
  failures/            ; known-bug reproductions, one suite per issue (below)
    dune               ; (tests (names issue_42))
    issue_42.ml        ; exit @@ run "issue-42" [ xfail ~reason:"issue #42" (test …) ]
  expect/              ; expect tests — no test code in lib/ (below)
    dune
    expect_render.ml
  cram/                ; blackbox tests of the binary (§5)
    dune               ; (cram (applies_to :whole_subtree) (deps %{bin:mytool}))
    help.t
  integration/         ; only when a service or heavier closure forces its own suite
    dune
    test_e2e.ml
```

Within a module's test file, state the law first: each behavior group
leads with its property (the normative core), then the pinned examples
and edge cases, then descriptive expectations.

**No test code in `lib/`.** Expect tests live in `test/expect/`, a
`(library (inline_tests) (preprocess (pps ppx_windtrap)))` that depends
on the code under test; `dune runtest` drives it like any suite and
`dune promote` accepts its corrections. Keeping `lib/` clean also pays
an instrumentation dividend: mutation skips any file that declares
inline tests, so a library with none is mutable end to end.

**Known bugs live in `test/failures/`** — one suite per issue, so the
backlog is discoverable with `ls test/failures/` and each reproduction
names its ticket in `xfail ~reason:"issue #42"`. `dune runtest
test/failures` runs the backlog. Each suite stays green while its bug
exists — an `xfail` failure is expected — and goes loudly red the day a
change cures it: `xfail`'s unexpected-pass is the "bug fixed" signal.
Fixing a bug means unwrapping the `xfail`, moving the test into the
owning module's file in `unit/` as a regression test, and deleting the
issue file with its entry in `(names …)` — when the last issue dies,
the stanza goes with it.

One thing in those stanzas is load-bearing and easy to omit: a suite
with baselines runs its executable with `--corrected` and diffs each
corrected file — `(action (progn (run %{test} --corrected) (diff?
test_parser.ml test_parser.ml.corrected) (diff? help.expected
help.expected.corrected)))` — which is what lets `dune promote` accept a
change; a file baseline must exist before dune can diff it, so a new one
starts empty (`touch`) or is accepted once with `-u`. Without the action
a stale expectation is a plain failure whose acceptance is
`dune exec test/unit/test_parser.exe -- -u`.

The project verdicts are two commands each: a run of the whole suite
with the backend on, then a `windtrap` merge of what the executables
wrote. Coverage accumulates as a side effect of any instrumented run,
so `dune runtest --force --instrument-with ppx_windtrap.coverage` then
`dune exec windtrap -- coverage --min 80` is the whole thing. A
mutation *verdict* exists only if a suite was asked to test its
mutants, so the run carries `WINDTRAP_MUTATE=1` in the environment, the
backend flag, and `--force` (a mutation run is not a cached artifact),
and `dune exec windtrap -- mutants` merges; it exits 1 when a mutant
survived every executable that reached it. Declare the backends once in
`dune-workspace` — `(context (default (instrument_with
ppx_windtrap.coverage ppx_windtrap.mutate)))` — and the flag disappears
from every command. The two `@cover` and `@mutate` rules in `test/dune`
fold each pair into one alias; they declare `(deps (alias_rec runtest)
(universe))`, because the `.coverage` and `.mutants` files test
executables write at exit are not declarable dependencies, so without
`(universe)` the merge action caches against nothing and silently goes
stale.

Set `--min` to the measured baseline minus a couple of points of
headroom, not a round number. It ratchets: raise it when the margin is
comfortable; **never lower it to make a red build green**.

On the library under test, two inert instrumentation stanzas,
committed once — zero overhead until the matching flag asks for them,
and no PPX or test code in `lib/` itself:

```lisp
(library
 (name mylib)
 (instrumentation (backend ppx_windtrap.coverage))          ; coverage
 (instrumentation (backend ppx_windtrap.mutate)))  ; mutation
```

Coverage is spelled `ppx_windtrap.coverage`, never the bare
`ppx_windtrap`, which no longer resolves at all: it once linked the
windtrap core into every instrumented library's closure — a test
framework in your production dependency cone.

CI runs three things: the suite with JUnit output for ingestion, the
coverage gate, and the mutation gate:

```yaml
- run: WINDTRAP_JUNIT=_build/junit dune runtest
- run: dune runtest --force --instrument-with ppx_windtrap.coverage && dune exec windtrap -- coverage --min 80
- run: WINDTRAP_MUTATE=1 dune runtest --force --instrument-with ppx_windtrap.mutate && dune exec windtrap -- mutants
```

Under GitHub Actions failures also surface as inline annotations with no
configuration. Under CI the runner refuses runs that would lie: focused
tests (`focus`) and in-place baseline updates (`-u`) refuse to start.

A complete, buildable instance of this whole layout — every stanza this
section describes, written out and commented, plus the backlog, a live
`xfail` and a dismissed mutant — lives in windtrap's
`examples/x-blueprint/`. Copy the stanzas from there rather than from
memory.

## 4. Where the mechanics live

Each subject has one chapter. Read the row you need; this file carries
only the judgment the chapters leave implicit.

| You need | Read |
|---|---|
| the assertion verbs, the witnesses, `Exn`, what a failure prints | `doc/manual/assertions.md` |
| `prop`, `Gen`, shrinking, seeds and replay, `collect`/`classify`/`cover` | `doc/manual/property-testing.md` |
| `stateful`, `command`/`call`, models, `~pre`/`~next`, per-case systems | `doc/manual/stateful-testing.md` |
| `expect` and `expect_file`, `--corrected` and `dune promote`, `-u`, `[%expect]`, adopting a ppx_expect suite | `doc/manual/snapshots-and-expect.md` |
| `bracket`, `scoped`, `fixture`, temp paths, `setenv`/`chdir`, `cases`, tags, focus, `xfail` | `doc/manual/resources-and-structure.md` |
| the flags, their `WINDTRAP_*` mirrors, selection, sharding, CI output | `doc/manual/running-tests.md` |
| the coverage stanza, `windtrap coverage`, `[@coverage off]` | `doc/manual/coverage.md` |
| the mutation stanza, survivor blocks, `WINDTRAP_MUTATE_ONLY`, arming one mutant, `windtrap mutants` | `doc/manual/mutation.md` |
| convergence loops, Eio, subprocess workers, scripted seams | `doc/cookbook.md` |

Those paths are a windtrap checkout's. The package installs neither
`doc/` nor this file, so from a consumer repo read the same chapters at
`https://github.com/invariant-hq/windtrap/tree/main/doc/manual/` (and
the cookbook at `.../blob/main/doc/cookbook.md`). The API reference —
`lib/windtrap.mli`, which is the contract the chapters narrate — *is*
installed, and odoc renders it.

**Assertions.** Expected first, always: `equal t expected actual`.
`text`, not `string`, for any multi-line value — `%S` buries the
difference in `\n` soup. `require_some`/`require_ok`/`require_error`/
`require_match` assert a shape *and* hand back its payload, so the happy
path keeps its value instead of drowning in `match`. One behavior per
test, named by the behavior: `"rejects empty input"` diagnoses a failure
from the list alone, `"test_parse_2"` forces reading the body. Add
`~msg` to assertions inside loops so the failure says which iteration.
Custom types: expose `pp` and `equal` in the tested module's `.mli`,
then `Testable.make ~pp:Point.pp ~equal:Point.equal`.

Pick example inputs adversarially, not representatively. For each
obligation, work the list: the empty/zero case, the singleton, a
boundary and both its neighbors (capacity, length, `0`, `-1`),
duplicates, extremes (`min_int`, `max_int`, `nan` where floats flow),
non-ASCII text, inputs containing the format's own delimiters (the
comma in a CSV codec), and every documented error input. `cases` keeps
the table readable and each row individually selectable; generators are
this list's exhaustive twin.

**Properties.** The laws to reach for: round-trip (generate the
*decoded* form), agreement with a simpler oracle, invariants after an
operation, algebraic identities, metamorphic relations, and
total-behavior claims (`parse` of arbitrary junk never raises). Sizes,
indices and arithmetic draw from `small_int` or `nat` — full-range `int`
drowns most laws in overflow noise. `assume` is for rare, cheap
preconditions; structural ones (nonempty, sorted) belong in the
generator. Write one `pp` and feed both worlds: `Testable.make ~pp` for
assertions, `Gen.with_pp pp` for counterexamples. **Pin every fixed
counterexample** in `~examples` when a property finds a bug you fix — it
runs before any generation, forever. Replay a failure by pasting the
printed replay line; fix the bug before touching the generator.

**Stateful.** The model is the specification — a persistent value
(`list`, `Map`), never a mutable structure, and never a second
implementation. Bodies check what a call *returns*; `~invariant` checks
what the state *is*. `~next` is required, so read-only calls say
`~next:Fun.id`. `~pre` both filters and *selects*: a command whose
`~pre` demands a full queue is generated exactly at capacity — that is
how you test "raises when full" — but a precondition no state satisfies
deletes the command silently, so guard it with `cover` in `~invariant`,
never in the command's own body, which is exactly the code that never
runs. Generated arguments cannot be handles that don't exist yet:
generate an *index* into the model's live set and let `~pre` keep the
lookup total. The scope runs once per case **and per shrink candidate**
— hundreds on a failing run — so `temp_dir`, `setenv` and `chdir`, all
scoped to the *test*, are the wrong tools inside one: mint scratch paths
in the scope and remove them on the way out, use absolute paths, restore
process state yourself.

**Baselines and expect tests.** Both are descriptive (§2): they pin
behavior, so they need a normative core beside them. Nondeterminism must
be masked *before* comparison or every run diffs — redact in code, or
shadow `Expect_test_config` with a `sanitize` for a whole file, and sort
anything whose order is incidental. `output ()` hands you the test's
captured stdout/stderr for post-processing when masking or a custom
comparison is needed. Everything about promotion is §7.

**Convergence has no verb.** The probe/step loop is seven lines of your
own (`doc/cookbook.md`). Windtrap never sleeps: the budget counts
probes, and the thing that advances the system — mock clock tick,
event-loop turn, queue drain — goes in the step, never a sleep. A
sleeping step hides a race instead of exposing it. Probe first, and make
the probe carry evidence that the work actually happened: a probe true
of a system nobody started converges immediately, having driven nothing.

**Coverage.** Chase the uncovered branches in code you touched, never
the percentage: an uncovered error branch is a missing test; an
uncovered debug helper is what `[@coverage off]` is for. Coverage is
expression-grade, and a call that raises leaves its out-edge unvisited,
so raising paths show up as uncovered rather than painted green for
having been entered. The gate is `windtrap coverage --min`, run after
the instrumented suite — test runs never fail on coverage. Coverage
finds *missing* tests; mutation (§6) finds *weak* ones. Run both,
routinely.

**The daily loop.** `-f`/`-e` filter by path substring, `--tag`/
`--exclude-tag` by tag, `--failed` reruns the last run's failures,
`-x` stops at the first failure, `-l` previews a selection, `--shard K/N`
partitions across CI jobs, `-s` disables capture for printf-debugging a
hang. Under `dune runtest` there is no command line, so the `WINDTRAP_*`
mirrors *are* the CLI (`WINDTRAP_FILTER=roundtrip dune runtest --force`;
a variable changes nothing on a warm tree unless the run passes
`--force` or the stanza declares `(deps (env_var WINDTRAP_FILTER))`).

When a run fails, triage before editing: read the failure block to its
end — it already carries the diff, the counterexample or program, the
captured-output tail, and the replay command. Reproduce with the replay
line or `--failed`, narrow with `-f`/`-x` if needed, and only then
decide which side is wrong (§7). Never touch the generator, the
baseline, or the assertion while the failure is still unexplained.

## 5. Cram tests (executables)

For "user runs a command and sees output", a cram test through the real
binary beats any unit test: a `foo.t` file (or `foo.t/` directory with
`run.t` plus fixtures) of shell commands with expected output, promoted
with `dune promote`. Non-obvious mechanics:

- Declare the binary or the test runs stale code:
  `(cram (applies_to :whole_subtree) (deps %{bin:mytool}))` — `%{bin:…}`
  needs a `public_name`; private executables depend on the path.
- A nonzero exit appears as a trailing `[1]` line — asserting exit
  codes is half the value; never let promotion silently absorb one.
- Dune sanitizes only the sandbox path (`$TESTCASE_ROOT`). Timestamps,
  durations, home paths, versions you sanitize yourself with `sed`, or
  assert on stable fragments with `grep -o`. Sort `ls` output.
- Coverage reaches the binary too: with §3's coverage stanza on its
  library, every command a cram test runs writes its own dump, and
  `windtrap coverage` merges them all — a CLI exercised through cram
  counts across every invocation, in the same merge.

## 6. Prove every test can fail (mutation)

A test nobody has seen fail is unverified, and windtrap mechanizes the
verification by breaking the code on purpose: run the tests with
`WINDTRAP_MUTATE=1`, and for every mutant in the code those tests
reach, windtrap re-runs them with the mutant armed. A mutant none of
them notice is reported, naming the tests that ran it.

The survey exists only where the precondition holds: the library under
test carries §3's instrumentation stanza —
`(instrumentation (backend ppx_windtrap.mutate))`, the mutate twin of
coverage's `(backend ppx_windtrap.coverage)` — and the run passes
`--instrument-with ppx_windtrap.mutate`. A library without the stanza
contributes no mutants, and the survey has nothing to report. Where
instrumentation is absent — a vendored dependency, a stanza not yet
landed — fall back to falsifying by hand: edit the assertion's expected
value to a wrong one, watch the test fail, restore it. Cruder than a
report, but it is the same evidence, and no test is exempt from
producing it.

**Survey every test you write or change** — the last step of writing
one, not a separate pass. Filter to the test and scope the mutants to
the file it exercises; the run mutates only what the selected tests
reach, so it takes about as long as those tests:

```
WINDTRAP_MUTATE=1 WINDTRAP_MUTATE_ONLY=lib/foo.ml \
  dune exec --instrument-with ppx_windtrap.mutate \
  test/unit/test_foo.exe -- -f "<test name>"
```

Read the survivors it reached. Each `SURVIVED` block names the line,
the rewrite, and the tests that ran that line and did not fail — for a
filtered run, the test you just wrote. Every block is one of two
things, and resolving it is the point; never the green:

- **A weak assertion** — `is_true`, `is_some`, a shape check where an
  exact `equal` belongs, an expected value the mutant also satisfies.
  Strengthen the assertion until the mutant dies. `WINDTRAP_MUTATE_ARM=<id>`
  on the `reproduce:` footer runs the test with that one mutant armed,
  to watch it live through the assertion before you change it.
- **An equivalent mutant** — the rewrite cannot change the program's
  observable behavior. Dismiss it in the source, with a reason —
  `((want > 16) [@mutate off "both arms yield 16 at the boundary"])` —
  never to silence a real finding, and never one you have not reasoned
  about. There is no suppression database: dismissals live in the
  source, where `git blame` sees them.

A filtered run is a reading list: it exits 0 whatever it finds and
writes no verdict file, so it never perturbs the project answer. Drop
the `-f` to survey the whole module when reviewing one — still
file-scoped by `WINDTRAP_MUTATE_ONLY`, so still seconds-fast — and the
summary reads `mutants: 1 survived of 5 reached by this suite · 4
killed`. **When fixing a bug, write the failing test first** and see it
fail; the survey is for every other test.

**The project question is the merge.** A library is normally covered by
several test executables, and per-executable reports disagree by
construction — one suite's kill is another's survivor — so the project
answer is `windtrap mutants`, under killed-anywhere-wins:

```
WINDTRAP_MUTATE=1 dune runtest --force --instrument-with ppx_windtrap.mutate
dune exec windtrap -- mutants
```

The first runs every suite with its mutants, the second merges (the
`@mutate` alias of §3 folds the two into one). The report is
the same survivor blocks with the executable beside each witness, plus
`UNREACHED` blocks for mutants no suite's tests evaluate — those mean
*write a test*: no assertion, however sharp, can catch what no test
runs. It exits 1 on any survivor, which is the one mutation exit code a
build gates on; unreached mutants alone are never red. Trust the merged
report, not a per-suite one. The survey needs `Unix.fork` and declines
by name on Windows, where `WINDTRAP_MUTATE_ARM=<id>` on one mutant is
the fallback.

## 7. The suite is a contract

Agents under pressure to go green reach, in escalating order, for:
weakening an assertion, hardcoding an expected value, special-casing
test inputs in the implementation, editing or deleting the failing
test, skipping it, or blessing bad output through promotion. Every one
of these is visible in a diff, and none may happen silently:

- **Never weaken an assertion or edit an expected value to match
  observed behavior** unless you can name the intended behavior change
  that justifies it — and then say so where the change is reviewed
  (commit message or PR). A normative test failing means the code is
  wrong until the spec says otherwise.
- **Never delete or skip a failing test to get green.** `xfail
  ~reason:"issue #42" (test …)` is the honest holding state for a known
  bug: it keeps the reproduction running, and passes loudly when the
  bug is fixed. Deletion is for behavior that no longer exists.
- **Never special-case test inputs in implementation code.**
- **Promotion and `-u` are assertion authorship**, held to the same
  standard as writing the assertion by hand. Read every promoted or
  `-u`-accepted diff as a code change you are authoring, hunk by hunk.
  If a diff surprises you, that is a bug found by the suite —
  investigate, don't accept. Never batch-accept output you have not
  read; never update a baseline to absorb a failure you cannot explain.
- **A skip is a deliberate environmental statement** (`~reason`
  required in spirit), never a disguise for a failure.
- **When you conclude the test is wrong, stop.** Changing a normative
  test is a contract change: name the spec source that contradicts it
  and surface the case to the maintainer instead of editing and moving
  on. Descriptive baselines (§4) are the ones you may re-accept
  yourself — with every hunk read.
- **When the spec is silent, don't legislate silently.** A test you
  could only write by choosing the behavior yourself carries that
  choice visibly (its name, or a comment naming the assumption), and
  the gap gets reported.
- Report what the run actually said: exit 2 is a broken filter, not a
  pass; a red teardown alongside a green body is still a finding.

## Checklist

Walk this before finishing, and for each item be able to point at the
evidence — a command you ran, a diff, a verdict in a transcript — not
merely recall having read the rule:

- [ ] Obligations derived from the `.mli` before reading the
      implementation, each checked off; spec gaps surfaced, not
      silently filled
- [ ] Every behavior at the strongest oracle its shape allows (§2
      ladder); every module has a normative core; no bad-test-catalog
      offender survives review
- [ ] Expected values derived from the spec; example inputs
      adversarial (§4 list)
- [ ] Properties encode real laws; structural preconditions live in
      generators, not `assume`; shrunk counterexamples pinned in
      `~examples`
- [ ] Stateful models are persistent values; read-only commands say
      `~next:Fun.id`; rare states covered via `cover` in `~invariant`
- [ ] Baselines deterministic (masked before comparison), their stanza
      running `--corrected` and diffing each corrected file; every
      promoted / `-u` diff read as a code change
- [ ] Cram stanzas declare `(deps %{bin:…})`; exit codes asserted
- [ ] Every new test seen failing — failing-first for bugfixes, the
      filtered `WINDTRAP_MUTATE=1` survey otherwise — with every
      survivor it reached resolved: assertion strengthened, or an
      equivalent mutant dismissed with a reason
- [ ] Coverage read on touched code; the coverage gate and the
      `windtrap mutants` merge run in CI; `--min` ratcheted, never
      lowered; the merge green
- [ ] Layout: suites split only along mechanical boundaries; files by
      subject; no test code in `lib/`; known bugs in `test/failures/`
- [ ] No §7 violation: nothing weakened, deleted, skipped, or
      blind-promoted, anywhere, without a stated justification
