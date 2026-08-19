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

Windtrap is one library for unit, property, stateful, snapshot, and
expect tests, plus code coverage and mutation testing. `open Windtrap`;
`test`/`group` declare inert data; `run` executes and exits: 0 all
passed, 1 any failure, 2 nothing ran (the filter-typo case — treat it as
failure, never as success).

This file is the decision layer — which test, which conventions, which
discipline. The full mechanics live on disk: `doc/manual/` (one chapter
each for assertions, property testing, stateful testing, snapshots and
expect, running tests, coverage, mutation) and `doc/cookbook.md`
(recipes windtrap deliberately does not absorb). Read the matching
chapter whenever you need mechanics beyond what this file carries.

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

## 2. Choose the strongest oracle the behavior admits

Work down this ladder and take the **first** row that fits. Each row
constrains strictly more behavior per line of test code than the rows
below it.

| The code under test is | Write | Why it is strongest here |
|---|---|---|
| A pure function with a law — codec, parser/printer, normalizer, arithmetic, ordering | `prop` over the law | One law constrains the whole input space; shrinking hands you the minimal counterexample |
| A stateful API — container, cache, store, pool, anything with a lifecycle | `stateful` against a model | Checks laws over *sequences* of calls; finds interaction bugs no unit test reaches |
| A pure function where only specific points are specified | `test` + `equal` through a testable | Exact expected values, written by hand from the spec |
| An executable's observable behavior — CLI parsing, exit codes, error messages, file effects | Cram test through the real binary | Tests the wiring no unit test reaches; doubles as CLI documentation |
| Rendered or serialized output too large to hand-write — help pages, reports, formatted trees | `snapshot` / `[%expect]` | A reviewed baseline beats a hand-copied string; promotion keeps it current |
| A claim about a value no equality captures | `satisfies ~msg` | Last resort — the failure at least prints the value and names the predicate |

Three rules outrank the table:

- **Expected values come from the spec, never from running the code
  under test.** An expected value captured from the implementation's own
  output is a snapshot with extra steps and none of the review
  discipline — if you cannot derive the expected value by hand, write it
  as a `snapshot` so the acceptance workflow (and its reviewer) owns it.
  The operational tell: write the assertion *before* first running the
  test. If you had to run the code to learn the value, it was a
  snapshot all along.
- **Normative vs descriptive.** Properties, stateful models, and
  hand-derived `equal` expectations are *normative*: they encode the
  spec, and when one fails, suspect the code. Snapshots and expect tests
  are *descriptive*: they pin current behavior, and when one fails the
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
  a snapshot; write it as one (§7) so the acceptance workflow and its
  reviewer own it.
- **Vacuous** — executes code but checks nothing that can break: no
  assertion at all, `is_some` where the *value* matters, "does not
  raise" on a function that cannot raise. Green from the day it was
  born; an admit run rules it `UNJUSTIFIED` (§9).
- **Tautological** — re-derives the answer with the implementation's
  own algorithm (a "property" computing the same fold), or tests the
  language: that a record field holds what the constructor assigned,
  that `List.sort` sorts. Can only fail if OCaml is broken.

*Tests that fail wrong:*

- **Blind boolean** — `is_true (a = b)`, `is_true (n > 0)`: the
  failure prints `expected true` and hides the data. Use testables and
  `satisfies ~claim:"greater than 0" int (fun n -> n > 0) n`, which
  keeps the bound and the value. And weak predicates are weak oracles
  too: `is_true (apply Sub 10 4 > 0)` survives the `a - b → a + b`
  mutant; `equal int 6 (apply Sub 10 4)` kills it.
- **Overfit** — asserts incidental detail: the whole help text to
  check one flag, exact float equality where a tolerance witness
  belongs, the order of an unordered collection (`slist` exists),
  timestamps, absolute paths. It breaks on unrelated edits, which
  trains everyone to update tests reflexively — the exact habit §12
  forbids.
- **Coupled** — depends on another test's side effects, shared mutable
  state, the wall clock, the network, or directory-listing order. It
  breaks the moment the suite is selected differently — `-f`,
  `--failed`, and `--shard` all change which tests run. Use
  `bracket`/`fixture`/`temp_dir` for state, `setenv`/`chdir` for the
  environment and the working directory (the runner puts both back);
  mask time (§7).

*Tests at the wrong level:*

- **Over-mocked** — needs several fakes to check one line; it proves
  the mocks call each other and calcifies the current decomposition.
  Move up to a cram test of the real binary, or extract the pure core
  and test that.
- **Snapshot-of-everything** — one giant baseline nobody reads,
  churning on every change until promotion becomes a reflex. A
  snapshot must earn its size: small, focused, masked — with the parts
  that matter asserted via `equal`/`contains` beside it.
- **Property without a law** — when there is no genuine law
  (round-trip, invariant, oracle agreement, algebraic identity,
  metamorphic relation), a property is noise around an example; write
  `cases` instead.

The catalog is mechanically checkable: nearly every entry either
survives mutants — §9's loop finds it — or fails with a message that
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
example tests, and its error-message snapshot together form Parser's
contract, and they belong in the same file. Splitting `test/unit/` from
`test/property/` from `test/snapshot/` scatters one module's contract
across three trees and leaves "what constrains Parser?" with no answer
location. Kinds are already selectable at run time: property and
stateful tests carry automatic tags (`--exclude-tag prop` for an
example-only pass), slow tests carry `slow` (`--exclude-tag slow` drops
them), and `-f` filters by path. Extra executables also carry a real bill:
each one links the library, splits the coverage denominator, and
re-runs the mutation loop over every file it links — the `@mutate`
merge makes the *answer* right, not the cost.

Within the unit suite: one test file per source module, and **one test
stanza per file** — each file is its own suite, ending in its own
`run`; the plural `(tests (names …))` stanza declares them in one
block. Every child of `test/` is one suite directory with its own
`dune` file, named by *why it is separate*; `test/dune` itself holds
only the project verdict aliases:

```
test/
  dune                 ; the @cover/@mutate/@admit verdict aliases (below)
  unit/                ; THE windtrap suite: laws, examples, stateful, snapshots
    dune               ; (tests (names test_parser test_eval) ...)
    test_parser.ml     ; everything that constrains Parser — its own run
    test_eval.ml
    __snapshots__/     ; committed baselines
  failures/            ; known-bug reproductions, one suite per issue (below)
    dune               ; (tests (names issue_42))
    issue_42.ml        ; run "issue-42" [ xfail ... ]
  expect/              ; expect tests — no test code in lib/ (below)
    dune
    expect_render.ml
  cram/                ; blackbox tests of the binary (§8)
    dune               ; (cram (applies_to :whole_subtree) (deps %{bin:mytool}))
    help.t
  integration/         ; only when a service or heavier closure forces its own suite
    dune
    test_e2e.ml
```

Within a module's test file, state the law first: each behavior group
leads with its property (the normative core), then the pinned examples
and edge cases, then descriptive snapshots.

**No test code in `lib/`.** Expect tests live in `test/expect/`, a
library stanza that depends on the code under test:

```lisp
(library
 (name mylib_expect)
 (inline_tests)
 (libraries mylib)
 (preprocess
  (pps ppx_windtrap)))
```

`dune runtest` drives it like any suite; `dune promote` accepts its
corrections (§7). Keeping `lib/` clean also pays an instrumentation
dividend: mutation skips any file that declares inline tests, so a
library with none is mutable end to end.

**Known bugs live in `test/failures/`** — one suite per issue, so the
backlog is discoverable with `ls test/failures/` and each reproduction
names its ticket:

```ocaml
(* test/failures/issue_42.ml *)
open Windtrap

let () =
  run "issue-42"
    [
      xfail ~reason:"issue #42"
        (test "http resolves to its TCP port" (fun () ->
             equal int 8080 (require_match tcp_port (resolve "http"))));
    ]
```

```lisp
(tests
 (names issue_42)
 (libraries windtrap mylib))
```

`dune runtest test/failures` runs the backlog. Each suite stays green
while its bug exists — an `xfail` failure is expected — and goes
loudly red the day a change cures it: `xfail`'s unexpected-pass is the
"bug fixed" signal. Fixing a bug means unwrapping the `xfail`, moving
the test into the owning module's file in `unit/` as a regression
test, and deleting the issue file with its entry in `(names …)` — when
the last issue dies, the stanza goes with it.

`test/unit/dune` — one stanza, one test per file:

```lisp
(tests
 (names test_parser test_eval)
 (libraries windtrap mylib)
 (deps
  (glob_files_rec __snapshots__/**)))
```

`test/dune` — the three project verdict aliases:

```lisp
(rule
 (alias cover)
 (deps
  (alias_rec runtest)
  (universe))
 (action
  (chdir
   %{workspace_root}
   (run %{bin:windtrap} coverage --min 80))))

(rule
 (alias mutate)
 (deps (universe))
 (action
  (chdir
   %{workspace_root}
   (run %{bin:windtrap} mutate))))

(rule
 (alias admit)
 (deps (universe) unit/test_parser.exe)
 (action (setenv WINDTRAP_MUTATE admit (run %{exe:unit/test_parser.exe}))))
```

The snapshot `deps` glob is load-bearing: baselines are runtime data,
invisible to dune, and without it editing a baseline does not re-trigger
the test. `(universe)` is load-bearing in both rules: the `.coverage`
and `.mutants` files test executables write at exit are not declarable
dependencies, so it makes the milliseconds-cheap merge re-run every
build. The `chdir %{workspace_root}` keeps the rules correct wherever
they live. The first two are asymmetric on purpose: coverage
accumulates as a side effect of any instrumented run, so `@cover` both
runs the suites and merges; a mutation *verdict* only exists if a run
was asked to test mutants (`WINDTRAP_MUTATE=1`), so `@mutate` merges
what previous runs left. `@admit` neither merges nor gates — an
admission run persists nothing, so its rulings are the whole product.
Repeat its rule for each unit executable whose subject is the
instrumented library; a suite that tests its subject through a process
it spawns has nothing to admit, because the arming never reaches the
child.

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
`ppx_windtrap`: both resolve the same rewriter, but the bare spelling's
`ppx_runtime_libraries` link the windtrap core into every instrumented
library's closure — a test framework in your production dependency
cone.

CI runs four things: the suite, the coverage gate, the mutation report,
and JUnit output for ingestion:

```yaml
- run: WINDTRAP_JUNIT=_build/junit dune runtest
- run: dune build @cover --instrument-with ppx_windtrap.coverage
- run: WINDTRAP_MUTATE=1 dune runtest --force --instrument-with ppx_windtrap.mutate
- run: dune build @mutate
```

Under GitHub Actions failures also surface as inline annotations with no
configuration. Under CI the runner refuses runs that would lie: focused
tests (`ftest`/`fgroup`) and snapshot updates refuse to start.

A complete, buildable instance of this whole layout — aliases, backlog,
dismissed mutant included — lives in windtrap's `examples/x-blueprint/`.

## 4. Unit assertions

Assert through testables — a printer plus an equality — so failures
print both values with a structural diff. Expected first, always.

```ocaml
equal (list (pair string int)) [ ("a", 1) ] (bindings t);
let id = require_some (find_user "alice") in     (* assert AND unwrap *)
equal int 1 id;
let msg = require_error (parse_port "0") in
equal string "invalid port: 0" msg
```

The vocabulary worth knowing rather than reinventing:

- `equal` / `not_equal` through witnesses: `int`, `string`, `bool`,
  `char`, `bytes`, `int32`, `int64`, `option`, `result`, `either`,
  `list`, `array`, `pair`, `triple`, `quad`, `float eps`,
  `float_rel ~rel ~abs`, `float_exact` (the only one where NaN = NaN).
- `text` — strings printed verbatim and diffed line by line. Use it for
  any multi-line string; `string`'s `%S` rendering buries the difference
  in `\n` soup.
- `slist t cmp` — lists as multisets (order ignored, multiplicity kept);
  `Testable.contramap proj t` — compare and print through a projection.
  Together they make "these events happened, in any order, ignoring
  noisy fields" a one-liner.
- `require_some` / `require_ok` / `require_error` / `require_match` —
  assert a shape and hand back its payload; the happy path keeps its
  value instead of drowning in `match`.
- `satisfies ?claim t pred v` — `claim` is the sentence on the expected
  side, so a comparison keeps its bound and its value
  (`satisfies ~claim:"greater than 0" int (fun n -> n > 0) n`) where
  `is_true (n > 0)` reports only `false`.
- Strings: `contains ~sub` / `not_contains ~sub` /
  `in_order ~subs:[...]` for substrings that must appear in that order /
  `starts_with ~affix` / `ends_with ~affix`; lists: `mem`.
- Exceptions: `raises exn fn` (structural; distinguishes "nothing
  raised" from "raised something else"), `raises_match pred fn` with
  the `Exn` helpers (`Exn.invalid_arg ~substring:"negative"`).
- Convergence has no verb: the probe/step loop is seven lines of your
  own (cookbook recipe 13). Windtrap never sleeps — the budget counts
  probes, and the thing that advances the system (mock clock tick,
  event-loop turn, queue drain) goes in the step, never a sleep. A
  sleeping step hides a race instead of exposing it.
- Escape hatches: `fail` / `failf` for unreachable branches,
  `skip ~reason ()` for unmet environment preconditions.

Custom types: expose `pp` and `equal` in the tested module's `.mli`,
then `let point = Testable.make ~pp:Point.pp ~equal:Point.equal` (or
`Testable.structural ~pp` to use `( = )`).

Style: one behavior per test, named by the behavior — `"rejects empty
input"` diagnoses a failure from the list alone; `"test_parse_2"` forces
reading the body. Assert the property you care about, not incidental
detail: matching full help text to check one flag exists breaks on every
unrelated help edit — `contains ~sub` the flag. Add `~msg` to
assertions inside loops so the failure says which iteration.

Pick example inputs adversarially, not representatively. For each
obligation, work the list: the empty/zero case, the singleton, a
boundary and both its neighbors (capacity, length, `0`, `-1`),
duplicates, extremes (`min_int`, `max_int`, `nan` where floats flow),
non-ASCII text, inputs containing the format's own delimiters (the
comma in a CSV codec), and every documented error input. `cases` keeps
the table readable and each row individually selectable; §5's
generators are this list's exhaustive twin.

## 5. Property tests

`prop name gen law` draws from an `'a Gen.t` (100 cases by default),
runs an ordinary assertion body on each, and shrinks failures to a
minimal counterexample — there is never a shrink function to write.
Every failure prints an exact replay command with the run's `s1:` seed
token.

```ocaml
prop "decode inverts encode" Gen.(list small_int) (fun l ->
    equal (list int) l (decode (encode l)))
```

The laws to reach for: round-trip (`decode (encode x) = x` — generate
the *decoded* form), agreement with a simpler oracle (`fast_sort` vs
`List.sort`), invariants (`size` after `add`), algebraic identities
(idempotence, commutativity), metamorphic relations (`search (q ^ " ")
= search q`) and total-behavior claims (`parse` of arbitrary junk never
raises).

Generator discipline is where properties quietly go wrong:

- Sizes, indices, and arithmetic use `small_int` or `nat` — full-range
  `int` drowns most laws in overflow noise.
- `assume cond` is for rare, cheap preconditions (`assume (b <> 0)`).
  Structural preconditions (nonempty, sorted) belong in the generator —
  `Gen.such_that`, or correct-by-construction with `let+`/`and+`:

```ocaml
let gen_nonempty = Gen.(list ~size:(int_range 1 20) small_int)
let gen_rect =
  Gen.(let+ w = float_range 0. 10. and+ h = float_range 0. 10. in Rect (w, h))
  |> Gen.with_pp pp_shape
```

- Composite generators print their counterexamples automatically;
  after `map`/`bind` attach `Gen.with_pp` (the report tells you when
  it is missing). Write one `pp` and feed both worlds:
  `Testable.make ~pp` for assertions, `Gen.with_pp pp` for
  counterexamples.
- A property that never fails may never reach the interesting region.
  `classify`/`collect` report the input distribution (visible under
  `-v`); `cover label cond` fails the test when no passing case reached
  the region at all. Presence, not proportion — put the `cover` where
  the body always reaches it, or it is vacuous exactly when it should
  fire.

**Pin every fixed counterexample.** When a property finds a bug and you
fix it, add the shrunk counterexample to `~examples` — it runs before
any generation, forever:

```ocaml
prop "rect area matches the formula" ~examples:[ Rect (2., 0.) ] gen_rect law
```

Replay a failure by pasting the printed replay line (`--seed s1:…`
plus the filter); fix the bug before touching the generator.

## 6. Stateful tests

For anything with internal state, `stateful` is the strongest test you
can write: it checks the API against a *model* over generated call
sequences, and shrinks failures to a minimal program.

```ocaml
let commands =
  [
    command "push" (Gen.int_range 0 9)
      ~pre:(fun m _ -> List.length m < capacity)
      ~next:(fun m x -> m @ [ x ])
      (fun _ x q -> Bounded_queue.push q x);
    call "pop"
      ~pre:(fun m -> m <> [])
      ~next:List.tl
      (fun m q -> equal int (List.hd m) (Bounded_queue.pop q));
  ]

let () =
  run "bounded_queue"
    [
      stateful "behaves like a list" ~model:[]
        ~scope:(fun run -> run (Bounded_queue.create capacity))
        ~pp_model:(Testable.pp (list int))
        ~invariant:(fun m q -> equal int (List.length m) (Bounded_queue.size q))
        commands;
    ]
```

The model is the specification (a persistent value — `list`, `Map` —
never a mutable structure), not a second implementation. Bodies check
what a call *returns*; `~invariant` (run before the first call and
after every call) checks what the state *is*. What to know:

- `~pre` both filters and *selects*: a command whose `~pre` demands a
  full queue is generated exactly at capacity — that is how you test
  "raises when full" without the generator stumbling into it. But a
  precondition no state satisfies deletes the command silently; guard
  against that with `cover` in `~invariant` (not in the command's own
  body, which is exactly the code that never runs).
- `~next` is required; read-only calls say so with `~next:Fun.id`.
  `~pre`/`~next` must be pure and must neither assert nor discard —
  assert in bodies.
- Generated arguments cannot be handles that don't exist yet: generate
  an *index* into the model's live set and let `~pre` keep the lookup
  total.
- `~scope` builds the system and reclaims it, and it takes a callback:
  a resource that only exists *inside* one (`Eio_main.run`, any
  `with_`-style API) is the plain case. An acquire/release pair binds
  `let s = acquire ()` and runs `run s` under
  `Fun.protect ~finally:(fun () -> release s)` — that `Fun.protect` is
  yours, windtrap never sees the resource, but a release failure never
  replaces the counterexample you were shown. Call the callback exactly
  once: never fails the case, twice raises `Invalid_argument`.
- The scope runs once per case **and per shrink candidate** — hundreds
  on a failing run. `temp_dir ()` is test-scoped, wrong here; mint
  scratch paths inside the scope and remove them on the way out. So are
  `setenv`/`chdir` — restored per attempt, not per case: a scope that
  moves the process or binds a variable leaks it into later cases; use
  absolute paths, restore process state yourself.
  `~steps` (default 20) is quadratic on the failing path — lower it
  first when the test is expensive; `~timeout` is the only per-test
  bound there.
- There is no `~examples` for programs: pin a fixed regression by
  copying the shrunk counterexample's steps into a plain `test`.
- These tests carry the `prop` and `stateful` tags —
  `--exclude-tag stateful` keeps them out of a fast inner loop.

## 7. Snapshot and expect tests

Both are descriptive: they pin current behavior behind an explicit
acceptance step. Choose by where the expectation lives:

| Output | Use |
|---|---|
| Short, review-worthy, produced by printing | `[%expect]` in the `test/expect/` library (§3) |
| Large or generated — help text, JSON, renders | `snapshot` under `__snapshots__/` |
| Needs masking or custom comparison first | `output ()` + ordinary assertions |

Mechanics that matter:

- `snapshot "name" value` — identity is the **name** (stable across
  refactors), storage is
  `__snapshots__/<src_basename>/<name>.snap`. Nothing is silently
  created: a missing baseline fails and prints the acceptance command.
  Accept with `-u` / `WINDTRAP_UPDATE=1`, review with `git diff`.
  `snapshot_pp` snapshots a pretty-printed value. Comparison
  canonicalizes newlines on *both* sides (CR/CRLF become LF, a trailing
  newline is forced), so a byte-exact golden test migrated to `snapshot`
  silently loses that strictness — when CR bytes or the missing final
  newline are the point, encode before snapshotting.
- Stale baselines — a baseline whose test was deleted or renamed — are
  reported after a full clean run, with the `rm` that removes them. The
  report is advisory: it never deletes and never fails the run, because
  a baseline is a committed file and removing one is your edit to
  review.
- `[%expect]` matches with ppx_expect's whitespace flexibility;
  `[%expect_exact]` is byte-for-byte. Corrections are accepted with
  `dune promote`, which must directly follow the failing `dune runtest`
  (any other dune command clears the pending set). The same trap holds
  for `dune build @fmt`: re-running the check clears the pending set,
  so "Nothing to promote" after a second run means the corrections were
  lost, not applied — `dune fmt`, which formats in place, avoids it.
  Assertion failures
  and uncaught exceptions are ordinary failures — promotion can never
  bless them; to pin an expected exception, catch and print it.
- Nondeterminism must be masked *before* comparison or every run
  diffs: redact in code (`snapshot "log" (mask_timestamps out)`), or
  shadow `Expect_test_config` with a `sanitize` for a whole file. Sort
  anything whose order is incidental.
- `output ()` consumes the test's captured stdout/stderr (C stubs and
  subprocesses included) for post-processing; `[%expect.output]` is the
  inline spelling.

**Promotion discipline — this is where bugs get blessed as expected
output.** Read every promoted or `-u`-accepted diff as a code change
you are authoring, hunk by hunk. If a diff surprises you, that is a bug
found by the suite — investigate, don't accept. Never batch-accept
output you have not read; never update a baseline to absorb a failure
you cannot explain.

## 8. Cram tests (executables)

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

## 9. Prove every test can fail (mutation)

A test nobody has seen fail is unverified, and windtrap mechanizes the
verification by breaking the code on purpose. Two modes: `admit` asks
*can this test fail?*, the survey *which of this file's faults does
nothing catch?*

Both exist only where the precondition holds: the library under test
carries §3's instrumentation stanza —
`(instrumentation (backend ppx_windtrap.mutate))`, the mutate twin of
coverage's `(backend ppx_windtrap.coverage)` — and the run passes
`--instrument-with ppx_windtrap.mutate`. A library without the stanza
contributes no fault sites: every admission ruling is `NO SITES` and
the survey has nothing to report. Where instrumentation is absent — a
vendored dependency, a stanza not yet landed — fall back to falsifying
by hand: edit the assertion's expected value to a wrong one, watch the
test fail, restore it. Cruder than a ruling, but it is the same
evidence, and no test is exempt from producing it.

**Admit every test you write or change** — the last step of writing
one, not a separate pass. The run's selection becomes the admission
set: only the faults those tests reach are armed, and each is ruled by
name, in about a second for a fast test.

```
WINDTRAP_MUTATE=admit dune exec --instrument-with ppx_windtrap.mutate \
  test/unit/test_foo.exe -- -f "<test name>"
```

- **`ADMITTED`** names the fault the test kills. Done — that line
  belongs in the PR description.
- **`UNJUSTIFIED`** is stop-the-line, and exits 1: the test ran faults
  on its lines and never failed. Strengthen the assertion or dismiss a
  genuine equivalent with a reason — the block prints both commands,
  its `arm` line reproducing the fault under that one test. Never
  proceed past one.
- **`NO SITES`** means mutation had nothing to say about that subject:
  not a failure, never a reason to delete a test, review it by eye.

The selection designates (`-f`/`-e`, tag knobs, `--failed`, an
in-source focus; `--shard` does not). Selecting nothing designates
every test the run executes, which is the whole-suite question and
belongs in an alias rather than in a filter:

```
dune build @admit --instrument-with ppx_windtrap.mutate
```

An admission run writes no verdict file, so it never perturbs
`@mutate`; each test tries at most 25 faults and says when that cap
decided the ruling (`WINDTRAP_MUTATE_TRY=0` tries every fault it
reaches).

**Survey the module when auditing or reviewing one** — file-scoped, so
it stays seconds-fast; it names the tests that watched a change and
stayed green:

```
WINDTRAP_MUTATE=1 WINDTRAP_MUTATE_ONLY=lib/foo.ml \
  dune exec --instrument-with ppx_windtrap.mutate test/unit/test_foo.exe
```

- A **survivor** means "strengthen one of these named tests" — usually
  a weak assertion (`is_true`, a shape check where an exact `equal`
  belongs). An **unreached** mutant means "write a test": no assertion,
  however sharp, can catch what no test evaluates.
- **When fixing a bug, write the failing test first** and see it fail;
  admission is for every other test.
- Dismiss a genuinely equivalent mutant in the source, with a reason —
  `((want > 16) [@mutate off "both arms yield 16 at the boundary"])` —
  never to silence a real finding, and never one you have not reasoned
  about. There is no suppression database: dismissals live in the
  source, where `git blame` sees them.
- With several test executables over one library, per-executable
  reports disagree by construction (one suite's kill is another's
  survivor); `dune build @mutate` merges under killed-anywhere-wins.
  Trust the merged report, not the per-suite one.
- Survivors never fail the build — the survey is a reading list, not a
  gate; only admission answers with its exit code. Both need `Unix.fork`
  and decline by name on Windows, where `WINDTRAP_MUTATE_ARM=<id>` on
  one mutant is the fallback.

## 10. Coverage

Coverage finds *missing* tests (unreached branches); mutation finds
*weak* ones. Both, routinely:

```
dune runtest --instrument-with ppx_windtrap.coverage    # inline % after the results
WINDTRAP_COVERAGE=full dune runtest --instrument-with ppx_windtrap.coverage
dune build @cover --instrument-with ppx_windtrap.coverage   # project merge + --min gate
```

`full` renders uncovered points as source excerpts — the mode that
shows the exact arms you forgot. Coverage is expression-grade, and a
call that raises leaves its out-edge unvisited, so raising paths show
up as uncovered instead of being painted green for having been entered.

Chase the uncovered branches in code you touched, never the
percentage: an uncovered error branch is a missing test; an uncovered
debug helper is what `[@coverage off]` is for. The gate (`--min`) lives
in the `@cover` alias only — test runs themselves never fail on
coverage. `windtrap coverage --json` is the machine-readable form.

## 11. Run and iterate

Direct execution takes flags; under `dune runtest` the `WINDTRAP_*`
environment mirrors *are* the CLI:

```
dune exec test/unit/test_parser.exe -- -x        # one module's suite, stop early
WINDTRAP_FILTER=roundtrip dune runtest           # filter within suites, under dune
```

The daily loop: `-f`/`-e` filter by path substring, `--tag`/
`--exclude-tag` by tag, `--failed` reruns only the last run's failures,
`-x`/`--bail N` stop early, `-l` previews a selection, and
`--exclude-tag slow` drops the tests the `slow` constructor tags.
Every property failure prints its replay line;
paste it. `--shard K/N` partitions a suite deterministically across CI
jobs. `-s`/`--stream` disables capture for printf-debugging a hang; the
live tail under a run names a hung test.

Structure and resources, in one pass: `cases ~name base inputs fn`
declares one selectable test per input, named by `~name` from the value
(required: a child's path keys its seeds and its `--failed` entry, so
numbered names would shift when a row is inserted);
`subtest` labels sub-cases inside one body. `bracket ~setup ~teardown`
scopes a per-test resource with teardown on every outcome; `scoped`
adapts callback-style resources (`Eio_main.run`, `with_open_text`) —
partial application builds reusable constructors from both. `fixture`
shares one expensive resource across the run (a `skip` raised during
acquisition skips every dependent test — the pattern for suites gated
on a missing device). `temp_dir ()`/`temp_file ()` are runner-cleaned
scratch paths; `setenv name value_opt` and `chdir dir` bind the
environment and the working directory for one test and the runner puts
both back on every outcome (`setenv name None` really unbinds, so the
missing-variable path is testable). `~timeout` caps a test; `~retries` is for the
flaky-by-nature only, never a way of life. `slow name fn` tags tests
that legitimately take time. `ftest`/`fgroup` focus while debugging —
remove before committing (CI refuses them; a successful focused run
warns).

When a run fails, triage before editing: read the failure block to its
end — it already carries the diff, the counterexample or program, the
captured-output tail, and the replay command. Reproduce with the
replay line or `--failed`, narrow with `-f`/`-x` if needed, and only
then decide which side is wrong (§12). Never touch the generator, the
baseline, or the assertion while the failure is still unexplained.

## 12. The suite is a contract

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
  standard as writing the assertion by hand: every hunk read, every
  surprise investigated before acceptance.
- **A skip is a deliberate environmental statement** (`~reason`
  required in spirit), never a disguise for a failure.
- **When you conclude the test is wrong, stop.** Changing a normative
  test is a contract change: name the spec source that contradicts it
  and surface the case to the maintainer instead of editing and moving
  on. Descriptive baselines (§7) are the ones you may re-accept
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
- [ ] Every behavior at the strongest oracle its shape admits (§2
      ladder); every module has a normative core; no bad-test-catalog
      offender survives review
- [ ] Expected values derived from the spec; example inputs
      adversarial (§4 list)
- [ ] Properties encode real laws; structural preconditions live in
      generators, not `assume`; shrunk counterexamples pinned in
      `~examples`
- [ ] Stateful models are persistent values; read-only commands say
      `~next:Fun.id`; rare states covered via `cover` in `~invariant`
- [ ] Snapshots named, deterministic (masked before comparison), and
      declared in the stanza's `deps`; every promoted / `-u` diff read
      as a code change
- [ ] Cram stanzas declare `(deps %{bin:…})`; exit codes asserted
- [ ] Every new test seen failing — failing-first for bugfixes,
      `WINDTRAP_MUTATE=admit` otherwise — with no `UNJUSTIFIED` ruling
      left standing, and survivors resolved or dismissed with a reason
- [ ] Coverage read on touched code; `@cover`/`@mutate`/`@admit`
      aliases present; `--min` ratcheted, never lowered
- [ ] Layout: suites split only along mechanical boundaries; files by
      subject; no test code in `lib/`; known bugs in `test/failures/`
- [ ] No §12 violation: nothing weakened, deleted, skipped, or
      blind-promoted, anywhere, without a stated justification
