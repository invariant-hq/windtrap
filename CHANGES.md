# Changelog

All notable user-facing changes are documented here — features, fixes,
performance, and anything that changes the public surface. That surface is
`lib/windtrap.mli`, the CLI and `WINDTRAP_*` contract, and the runner's exit
codes. New entries go at the top of their section.

## [Unreleased]

### Added

**`text`, a string witness that prints verbatim.** `string` renders with `%S`
— quoted, escaped, on one line — which buries the difference between two
multi-line values in `\n` soup. `text` prints the same string unescaped, and
because the rendering spans lines it takes the report's unified-diff path, so
rendered output, serialized documents and logs diff line by line. Equality is
unchanged, byte for byte: trailing whitespace and a missing final newline
still fail, and the diff marks them.

### Changed

**Backtraces stop at your code.** Under every backtrace windtrap records sit
its own frames — the callback delimiter, the attempt guard, the verb that
raised. They name none of your code and they were the majority of a short one:
a `raises` failure printed two lines, half of it `Windtrap__Check.raises`, and
an uncaught exception printed five, three of them machinery. The trailing run
of windtrap frames is now dropped, in the one place a raw backtrace becomes
report text, so the terminal, JUnit and GitHub reports agree. Only a trailing
run: a callback windtrap invoked keeps both itself and the frames below it, and
a backtrace that never crossed your code is kept whole rather than emptied.

**No empty ANSI spans.** Styling an empty string emitted an open code and its
reset with nothing between them, so every styled `FAIL` header carried a stray
`ESC[2mESC[0m` from its empty attempt suffix. Invisible on a terminal, but real
bytes for anything diffing or parsing a transcript.

**A printerless counterexample names its remedy.** A generator built with
`map` or `bind` carries no printer, so its counterexample renders as the draws
the value came from (`<from: ("a", 90)>`) — informative, but it never said what
to do about it, while the no-draws case (`<no printer — add Gen.with_pp>`) said
it inside the rendering. The advice now lives in one place, a line under the
counterexample, and covers every printerless shape including `~examples`
values; the renderings themselves are just renderings (`<no printer>`).

## [0.2.0] - 2026-08-07

Windtrap 0.2.0 is a ground-up rewrite around three commitments: **declaring a
test is pure data**, **a failure shows you the values**, and **anything random
replays**.

### Highlights

**A green run is one line, and a slow test still speaks up.**

```
$ dune runtest
mylib: 48 passed in 1.2s.
```

The header and the per-test glyph row stay hidden until the run has something
to say. A failure brings them out — and so does a passing test that ran long:

```
$ dune runtest
mylib: 1 test
.
slow tests (1):
  1.31s  search › reindex
(exempt with the "slow" tag, or raise --slow-threshold SECONDS)

1 passed in 1.31s.
```

`-v` gives one line per test (and the slowest-tests list); `-q` keeps failures
and the summary only.

**A failure marks what actually changed, for any type.** `equal` takes a
testable — a printer and an equality — and the report is derived from the
printed rendering, so a record, a variant, or an abstract type gets the same
treatment as `int` with no diff combinator to write. Lists and arrays are
compared element by element, and a mark never straddles two elements:

```
mylib: 1 test
F
──────────────────── failures (1) ────────────────────
  FAIL  users › sessions after login
    test/test_mylib.ml:19
      19 │               equal

    expected  [("alice", [1; 2; 3]); ("bob", [4])]
                                     ~~~~~~~~~~~~
    actual    [("alice", [1; 2; 3]); ("bob", [4; 5]); ("carol", [])]
                                     ~~~~~~~~~~~~~~~  ~~~~~~~~~~~~~
──────────────────────────────────────────────────────

1 failed in 0.000781s.
```

Bob's entry changed and Carol's is new — that is what the marks say. No `~pos`
annotation either: the location comes from the assertion's call stack.

**Properties are ordinary tests over generators.** One verb, an `'a Gen.t`, a
body that asserts. Shrinking is integrated and constraint-preserving, so a
counterexample arrives minimal without a shrink function.

```ocaml
prop "rev is an involution" Gen.(list int) (fun l ->
    equal (list int) l (List.rev (List.rev l)))
```

**Every failure tells you how to reproduce it.** Generated values derive from
the run's root seed, the test's path, and the case index, so a failing
property prints the exact replay command for the way the run was invoked —
`dune exec … -- --seed s1:… -f '…'` under dune, `WINDTRAP_SEED=… dune
runtest` for inline suites. `srandom` gives plain tests the same guarantee.

**Expect tests you can move a ppx_expect suite onto.** We ran Jane Street's
own ppx_expect corpus against `ppx_windtrap`: 33 of the 36 supported cases
conform — 16 pass with no correction, and 17 produce corrections
byte-identical to upstream's goldens — and all 20 unsupported constructs fail
at expansion with an error naming the exact construct, rather than quietly
doing something else.

**Coverage without a second toolchain.** One inert `(instrumentation (backend
ppx_windtrap))` stanza on the library under test; `dune runtest
--instrument-with ppx_windtrap` for an inline percentage, and `windtrap
coverage --min 80` to gate CI.

### Breaking changes

#### Cheat sheet

| 0.1.0 | 0.2.0 |
| --- | --- |
| `prop name (list int) law` (a `bool` law) | `prop name Gen.(list int) (fun l -> equal (list int) l (law l))` |
| `prop'` | `prop` (it is assertion-style now) |
| `prop2` / `prop3` / `prop4` | `prop` with `Gen.pair` / `Gen.triple` / `Gen.quad` |
| `~config` on a property | `~count` and `~examples` |
| `~gen` on a testable | a `Gen.t` argument; printers attach with `Gen.with_pp` |
| `Gen.oneofl` / `Gen.oneof` | `Gen.of_list` / `Gen.one_of` |
| `Gen.list_size sg g` | `Gen.list ~size:sg g` |
| `Gen.string_size sg cg` | `Gen.string_of ~size:sg cg` |
| `nat` / `small_int` testables | `Gen.nat` / `Gen.small_int` |
| `snapshot ~pos:__POS__ s` | `snapshot "name" s` |
| `snapshotf fmt …` | `snapshot "name" (Printf.sprintf fmt …)` |
| `expect s` / `capture fn s` | `equal string s (output ())`, or `let%expect_test` |
| `group ~before_each ~after_each` | `bracket ~setup ~teardown` on each test |
| `group ~setup ~teardown` | `fixture ?teardown create` |
| `testable ~pp ()` | `Testable.structural ~pp` |
| `testable ~pp ~equal ()` | `Testable.make ~pp ~equal` |
| `seq t` / `lazy_t t` | `contramap List.of_seq (list t)` / `contramap Lazy.force t` |
| `is_some x` | `ignore (require_some x)` |
| `is_none x` | `equal (option t) None x` |
| `is_ok r; Result.get_ok r` | `require_ok r` |
| `some t e v` | `equal (option t) (Some e) v` |
| `ok t e r` / `error t e r` | `equal t e (require_ok r)` / `equal t e (require_error r)` |
| `no_raise fn` | `fn ()` |
| `raises_invalid_arg "m" fn` | `raises (Invalid_argument "m") fn` |
| `raises_failure "m" fn` | `raises (Failure "m") fn` |
| an any-message or substring check | `raises_match Exn.invalid_arg fn`, `raises_match (Exn.failure ~substring:"…") fn` |
| `cases ty inputs name fn` | `cases name inputs fn` |
| `?here:[%here]` | `?pos:__POS__`, or nothing |
| `~tags:(Tag.labels [ "net" ])` | `~tags:[ "net" ]` |
| `Tag.speed Slow` | the `slow` declaration, or `~tags:[ "slow" ]` |
| `run ~quick ~filter ~seed …` | the CLI flags and `WINDTRAP_*` mirrors, or `run ~argv` |
| `--format` / `WINDTRAP_FORMAT` | `-q` ⊂ default ⊂ `-v`; TAP consumers move to `--junit PATH` |
| `-q` (meaning `--quick`) | `-q` means `--quiet`; `--quick` keeps its long form |
| `windtrap coverage --summary-only` | `windtrap coverage` (per-file is the default report) |
| `windtrap coverage -C N` / `--context N` / `--skip-covered` | removed |
| `windtrap coverage --coverage-path P` / `--source-path P` | positional `PATH…` |
| `windtrap coverage -j` | `--json` (long form only) |

#### What changed, and why

- **Properties are one verb.** `prop` takes a generator and an
  assertion-style body returning `unit`; shrinking is always on and
  constraint-preserving. The rest of the 0.1.0 `Gen` surface is gone —
  `fix`, `delay`, `no_shrink`, `add_shrink_invariant`, `make_primitive`,
  `find`, `ap`, `>>=`/`>|=`, and the `?origin`/`?ratio` knobs. Recursion is
  now `Gen.sized` plus `let*`. Five generators go too: `Gen.unit` →
  `Gen.constant ()`, `Gen.int32_range` / `Gen.int64_range` → `Gen.map` over
  `Gen.int_range`, `Gen.nativeint` → `Gen.map Nativeint.of_int Gen.int`, and
  `Gen.either` → `Gen.map` over `Gen.bool` and the two sides. `prop` also drops `?timeout`: the per-test
  timeout (declaration `~timeout`, or the runner's `--timeout`) covers the
  whole property, generation and shrinking included. A timeout that expires
  during shrinking ends the search and reports the best counterexample found
  so far, marked as possibly not minimal.
- **Snapshots are keyed by name, not by source position.** The baseline lives
  at `<src_dir>/__snapshots__/<src_basename>/<name>.snap`, so moving a test
  within its file no longer orphans it. Re-accept once with `-u` (or
  `WINDTRAP_UPDATE=1` under dune) and review with `git diff`, then add
  `(deps (glob_files_rec __snapshots__/**))` to the test stanza so baseline
  edits re-trigger `dune runtest`. The snapshot environment knobs
  (`WINDTRAP_SNAPSHOT_DIR`, `WINDTRAP_SNAPSHOT_DIFF_CONTEXT`,
  `WINDTRAP_SNAPSHOT_MAX_BYTES`, `WINDTRAP_SNAPSHOT_REPORT`) are gone: the
  baseline path derives from the source file, and the report is part of the
  standard output.
- **The expect-string family is removed.** `expect`, `expect_exact`,
  `capture`, and `capture_exact` have no replacement spelling. Assert on
  captured output with `equal string "…" (output ())`, snapshot it with
  `snapshot "name" (output ())`, or write a real expect test with
  `let%expect_test` and `[%expect]` (ppx_windtrap). `output ()` stays, and now
  drains standard error and subprocess output along with standard output.
- **Group hooks are removed**, so no user code runs outside a test's
  exception boundary. `group ~before_each`/`~after_each` becomes
  `bracket ~setup ~teardown` on each test — partially apply it to build a
  reusable constructor. `group ~setup`/`~teardown` for expensive shared state
  becomes `fixture ?teardown create`, acquired on first use and released by
  the runner after the last test. Accessors now work only inside a run;
  0.1.0's `fixture` was a plain lazy cache.
- **`Testable.make` requires `~equal`.** The `~gen` and `~check` fields are
  gone: generation lives in `Gen`, and diffs are computed from printed
  values, so every type gets a highlighted diff from its printer alone. The
  `nat`/`small_int` pseudo-testables were really distributions and moved to
  `Gen`; the `seq` and `lazy_t` witnesses are dropped — compare through
  `contramap List.of_seq (list t)` and `contramap Lazy.force t`.
- **The `is_some`/`is_ok`/`is_error` family asserts *and* unwraps.** Most call
  sites get shorter: `is_ok r; Result.get_ok r` collapses to `require_ok r`.
  The wrapper testables (`some`, `ok`, `error`) go through plain composites.
  On exceptions, `raises (Invalid_argument "m")` renders a wrong message as a
  message diff; any-message and substring forms move to `raises_match` with
  the new `Exn` predicates.
- **`cases` drops its testable and takes the name first.** Sub-tests are named
  `name.0`, `name.1`, … or derived from the value with `?name`
  (`cases "ports" ~name:string_of_int [ 1; 80; 8080 ] fn`), and each is
  individually selectable with `-f`.
- **`?here` is gone.** Use `?pos:__POS__`, or nothing: failure locations
  default to a best-effort call-stack capture, falling back to the enclosing
  test's declaration line when the failing call's frame is gone (a call in
  tail position).
- **Bodies return `unit`.** `test`/`ftest`/`slow` take `(unit -> unit)` and
  `bracket` bodies return `unit`; 0.1.0 accepted `(unit -> 'a)` and silently
  ignored the result. End with an assertion, or `ignore`.
- **Tags are plain strings.** Every `?tags` takes a `string list`, and the
  `Tag` module is no longer public. The Quick/Slow speed pair is now just the
  `"slow"` tag.
- **`run` keeps only `?argv`.** The programmatic configuration parameters
  (`~quick`, `~filter`, `~seed`, `~format`, `~junit`, `~update`,
  `~snapshot_dir`, …) are removed. Set the same knobs through the CLI flags or
  `WINDTRAP_*` variables they mirrored, or hand `run` a synthetic `~argv`.
- **Output formats are gone; verbosity is one axis.** `--format` (and
  `WINDTRAP_FORMAT`) is removed — terminal verbosity is `-q` ⊂ default ⊂ `-v`,
  not a format: every level prints the same failure blocks and the same
  summary, and the compact glyph row stays the default as in 0.1.0. TAP is
  gone; consumers should move to `--junit PATH` or the automatic GitHub
  Actions annotations. `-q` now means `--quiet`, not `--quick` — `--quick`
  keeps its long spelling only. `--seed` takes the printed `s1:` token, not an
  integer.
- **Coverage percentages change meaning.** The stanza and
  `--instrument-with ppx_windtrap` are unchanged, but 0.2.0 grades expression
  coverage with entry points per block *and* out-edge points on calls, which
  count as covered only when the call returns. Numbers are not comparable with
  0.1.0 runs. The per-file report is now the default `windtrap coverage`
  output, with `--min PCT` to gate CI and `--json` for a machine-readable
  artifact.
- **`open Windtrap` narrows.** It brings the flat values plus exactly four
  modules: `Testable`, `Gen`, `Exn`, and `Private` (unstable internals). The
  0.1.0 `Tag`, `Pp`, and `Ppx_runtime` modules are no longer public — the
  internals live under `Private`. Project modules with other names are no
  longer shadowed.

### Added

**Assertions.** New verbs, all of them chosen so the failure keeps the data
a boolean would have thrown away: `satisfies` (renders the rejected value),
`contains` / `not_contains` (print the needle and a bounded excerpt),
`require_some` / `require_ok` / `require_error` / `require_match` (assert and
unwrap), and the `Exn` predicates for `raises_match`. `float_exact` is a
bit-exact float witness — every NaN equal to every NaN, `0.` and `-0.`
distinct — so a test can assert that a function returns NaN.
`Testable.of_module` derives a witness from any module with the conventional
`t`/`pp`/`equal` trio.

**Failure reports mark what changed.** Both renderings are compared and the
differing regions marked: a unified diff on multi-line values, character
marks on short ones, and — when both sides are list or array renderings —
element-by-element alignment, so a mark is a whole differing element (or a
localized change inside one) and never a region spanning the tail of one
element and the head of the next. Shifted collections read correctly:
`[1; 2; 3; 4]` against `[2; 3; 4; 5]` marks the dropped `1` and the added
`5`, not every element. From eight elements up, a summary line leads with
the count and the first differing index. Plain (no-color) output carries a
`~~~` marker line under each side, so a deletion — which has nothing to show
on the actual side — is still visible.

A mark is only shown when it points at a small part of a mostly shared
value. Once it would cover half a side, the two values simply differ, and
scattering marks over them draws the eye to coincidental character
alignments — `Some _` against `None` marking `S`, `m`, `e` against `N`, `n`
says nothing the plain pair does not. Those pairs print unmarked, and under
color each side is tinted whole instead.

**Color says which side, everywhere.** Green is the expected side and red
the actual one, on every block that shows both — equalities, `raises`
mismatches, and the `satisfies`/`require_match` claims whose expected side
is a description rather than a value. The summary counts wear the glyph
row's colors too (green pass, red fail, yellow skip, faint expected
failure), so a color means one thing across the whole transcript.

**Generators.** Printers derive by composition — a composite prints exactly
when its components do — and attach with `Gen.with_pp`. New: `such_that`,
`float_any`, and the size-controlled `string_of`/`bytes_of`. `~examples` runs
pinned regressions first on every run, and `~count` overrides the case count
per declaration.

**Test structure.** `subtest` names sub-cases that all run even after one
fails. `xfail` keeps known-bug reproductions in-tree without a red run.
`temp_dir` / `temp_file` give runner-cleaned scratch paths on every outcome.
`srandom` gives plain tests replayable randomness, and `current_test` exposes
the running test's path.

**Diagnosis when a test raises.** An uncaught exception's report carries its
backtrace — the raise site, not just the constructor and the test's
declaration line — without the reader having to know about `OCAMLRUNPARAM=b`.

**Deterministic seeds.** Every generated value derives from the run's root
seed, the test's path, and the case index. The root seed prints as an `s1:`
token in the header of any suite declaring properties, and every property
failure prints the exact replay command for the way the run was invoked
(`dune exec <path> -- --seed … -f '…'` under dune, argv0 when run directly,
`WINDTRAP_SEED=… dune runtest` for inline suites). A failing test that drew
from `srandom` prints the same in its failure block.

**Shrinking you can see the end of.** A shrink search stops after 100
accepted steps. When it stops there rather than converging, the report says
so — `shrink budget of 100 steps spent; counterexample may not be minimal` —
so a truncated search never reads like a minimal one. `--max-shrink N`
(`WINDTRAP_MAX_SHRINK`) raises the budget.

**Snapshot workflow.** Checking is read-only and prints the acceptance
command. Update mode prints every path it writes and is refused under CI
(`WINDTRAP_UPDATE=force` overrides). Stale baselines are reported, and
`--prune` deletes them after a full, clean update run — printing `pruned
<path>` for each deletion, `stale baseline:` lines with the removal hint, and
an explanation when a prune is refused. The inline (ppx) runner reports all
of this identically to the library runner.

**Expect tests.** `let%expect_test`, `[%expect]`, `[%expect_exact]`,
`[%expect.output]`, `let%test`, and `module%test`, with corrections accepted
via `dune promote`. Compatibility is measured against Jane Street's pinned
ppx_expect corpus: 33/36 supported cases match upstream byte for byte
(91.7%) — 16 pass with no correction, and 17 produce corrections
byte-identical to upstream's goldens — and 20/20 unsupported constructs
rejected with a loud error at the exact location. One formatting note: a
correction re-renders every `[%expect]` node of its file in the standard
shape, so the first promote of a file whose payloads carry other formatting
— a 0.1.0 suite, hand-formatted blocks — reformats them all once. A file
with no corrections is never rewritten.

**Coverage.** An inline percentage after the test results on instrumented
runs, `--coverage`/`WINDTRAP_COVERAGE` modes (`summary`, `report`, `full`,
`off`), and a `windtrap coverage` command that merges `.coverage` files,
gates CI with `--min`, and emits `--json`. Both runners behave the same: the
inline (ppx) runner prints the same summary line and the same per-file
report, and refuses an invalid `WINDTRAP_COVERAGE` value with exit 2 rather
than ignoring it. Instrumentation never changes what a program means:
out-edge points are given up wherever taking one would cost a tail call —
ordinary tail position, `||` and `&&` arms of every shape, and the
constructor arguments of a `[@tail_mod_cons]` function — so an instrumented
run computes what the plain run computes, at the same stack depth. A
semantics-preservation suite holds that line.

**CI ergonomics.** `--shard K/N` deterministically partitions a suite across
jobs. Failures are emitted as GitHub Actions annotations when running under
Actions (`CI` and `GITHUB_ACTIONS` both set, as Actions sets them). Failure
reports include the tail of the test's captured output
(`WINDTRAP_TAIL_ERRORS` controls how much).

### Changed

- **The default output is compact, and a green run is one line.** The header
  and the per-test glyph row (green `.` pass, red `F` fail, yellow `S` skip,
  faint `x` expected failure; rows wrap at 60 with a `[k/n]` counter) print
  only when the run is noteworthy — any failure, or any test not tagged
  `slow` exceeding the slow threshold. A green, healthy run is exactly one
  named line (`mylib: 48 passed in 1.2s.`, with the root seed appended when
  the suite declares properties). A noteworthy run flushes the header and the
  buffered row at the first noteworthy event, streams the rest glyph by
  glyph, and replays the failures in full at the end. `-v` / `--verbose`
  restores the line-per-test transcript and keeps the slowest-tests list, now
  verbose-only; a passing property with collected labels prints its label
  distribution there. `-q` / `--quiet` keeps only the failure blocks and the
  summary. On a terminal, a faint erasable `[k/n] current-test…` tail runs
  from the start, so a hung test names itself before anything is committed;
  piped output has the same shape, flushed per glyph once noteworthy.
- **Slow tests announce themselves.** An untagged test exceeding the slow
  threshold puts the run in a faint-yellow `slow tests (n):` block between
  the failure blocks and the summary — slowest first, each entry indented
  with the duration in a right-aligned leading column, then one hint line
  naming the opt-outs: the `slow` tag, or `--slow-threshold SECONDS`
  (`WINDTRAP_SLOW_THRESHOLD` mirror; default 1, `0` disables the warnings
  and the noteworthy trigger). Tests tagged `slow` are exempt everywhere,
  and quiet mode prints no warnings.
- **Focus is refused under CI.** `ftest`/`fgroup` are debugging tools: when
  `CI` is set, a run containing focused tests refuses to start
  (`WINDTRAP_ALLOW_FOCUS=1` overrides), so a committed `ftest` can never
  quietly shrink a CI run to one test. Outside CI, a successful focused run
  prints a warning. 0.1.0 only warned.
- **The failure region is separated and the summary has room.** Failure
  blocks are separated by a blank line, and so are a single test's failures
  when it has several — sibling subtests, or a body and its teardown, which
  report independently. The closing rule, the slow block, and the summary
  each get their own space, so the verdict line is findable. Report paths
  are project-relative like the location lines above them, rather than
  absolute: a snapshot baseline prints
  `examples/x-demo/__snapshots__/main/usage.snap`, and a capture log keeps
  its `_build/_tests/…` prefix so the path still opens.
- **No run advertises `--failed`.** The old `rerun failures only: …` line
  under every failing run was an optimization hint, not a step; the summary
  is the last line now. Acceptance commands still print under every
  snapshot mismatch — those name a verb nobody can guess.
- **Floats print the value that failed.** Counterexamples and bit-exact
  witnesses render floats with the shortest decimal that round-trips to the
  same double, so `0.1 +. 0.2` reports `0.30000000000000004` rather than
  `0.3`. A counterexample exists to be pasted back into `~examples`; one that
  does not round-trip names a value the test never saw.
- **A fixture release failure is reported and counted.** A `teardown` that
  raises during release appears as a `fixture release` entry in the failure
  section, in the JUnit document, and in the GitHub annotations, alongside the
  exit code it already set. Releases run after the last test, so the entry
  sits outside the declared suite — a JUnit consumer sees one more testcase
  than the suite declares.
- **The per-test timeout covers teardown even after the body times out.** The
  window is re-armed before teardown with whatever remains of the limit, or a
  fresh one when setup and body consumed it: cleanup still has to happen, so
  it gets a bounded window rather than none.
- **Capture log paths are unique.** A test name is mapped to a filename by
  replacing anything outside `[A-Za-z0-9._-]`, which is many-to-one, so names
  differing only in punctuation shared one log. An altered name now carries a
  short digest of the original; unaltered names are untouched. Snapshot
  baselines are keyed by the name as written and are unaffected.
- **`-o DIR` is resolved once, at startup.** A relative log directory used to
  follow the process, so a test that changed directory sent the rest of the
  run's logs elsewhere.
- **`-q` is re-lettered from `--quick` to `--quiet`**, matching the
  near-universal CLI convention; `--quick` keeps its long form.
- **`--quiet` and `--verbose` gain environment mirrors** (`WINDTRAP_QUIET`,
  `WINDTRAP_VERBOSE`), making the output levels reachable under
  `dune runtest`, where the mirrors *are* the CLI. Quiet previously had no
  mirror at all.

### Fixed

- **A crashing test no longer swallows a library's expect corrections.** Every
  inline-test process that writes a `.corrected` file names it on stderr
  (`windtrap: wrote <file>.corrected`), and a process that wrote corrections
  and still fails adds a loud notice explaining that dune withholds every
  correction in the library from `dune promote` until the failure is fixed and
  the suite rerun.
- **A correction that cannot be written is no longer dropped silently.** When
  the source is unreadable or the target unwritable, the runner prints
  `Error: correction for <file> not written: <reason>` and the partition exits
  1 — so a failed expect test can never be recorded as passed just because its
  correction never reached disk.
- **An uncaught exception in a `let%expect_test` body is an ordinary test
  failure, never a correction.** The former behavior spliced an unreachable
  `[%expect]` node after the raising statement, which broke the build on
  promote (warning 21) or duplicated the node on every runtest+promote cycle.
  Inline runs with a raising expect test now exit 1; to pin an expected
  exception, catch and print it in the body.
- **PPX-generated code no longer relies on type-directed record
  disambiguation**, so inline-test libraries build clean under strict warning
  sets (`(flags (:standard -w +a -warn-error +a))`) — adopting projects need
  no `-w -42` workaround in their `dune` files.
- **`Stdlib.exit` from a test can no longer kill the run.** Called from a body,
  setup, teardown, or fixture release, the attempt is intercepted and recorded
  as that test's (or that release's) failure; every later test still runs, and
  the run exits through its own 0/1/2 contract. The runner owns the process
  exit — but only in the process that started the run: a test that forks and
  calls `exit` in the child terminates the child, which is what a test
  spawning subprocesses expects.
- **An assertion failure beside a stale `[%expect]` payload is not a
  promotable correction.** An inline body that both fails an assertion and
  leaves stale output exits nonzero, so dune withholds the library's
  corrections; otherwise `dune promote` would bless output the assertion had
  already rejected, and the real regression would surface a cycle later.
- **A trailing `[%expect]` inserted after a body that ends in a `match`,
  `try` or `function` lands after the body**, not inside its last arm: the
  `;` the correction appends would otherwise bind to the last arm, where the
  node runs on one branch only and the next run appends another beside it.
  The body is parenthesized as part of the same correction, so the promoted
  file means what the correction intended and converges on the next run. The
  test is on the body's tail, not its head, so the common `let … in match …`
  and `stmt; match …` shapes are covered too: the walk follows the tail
  through `let`, `;`, `if`/`else`, `open`, `let module`, `let exception`,
  `let*`, type annotations and a `fun`'s body.
- **A missing snapshot baseline now fails**, with the proposed content and the
  acceptance command. 0.1.0 silently created the baseline and passed.
- **Reading captured output under `--stream` now fails** with "this test
  requires capture". 0.1.0's expect tests silently compared against the empty
  string and passed.
- **A raising setup or teardown is an ordinary reported outcome.** 0.1.0 ran
  group hooks outside the failure boundary, where an exception could take down
  the runner; `bracket` now reports body and teardown failures independently.
- **Stopping early with `--bail`/`-x` no longer skips cleanup.** Teardowns run
  on every outcome, and acquired fixtures are released on every path where the
  runner regains control.
- **JUnit XML no longer contains ANSI escape sequences.**

## [0.1.0] - 2026-02-13

Windtrap is an all-in-one OCaml testing framework that unifies unit tests,
property-based tests, snapshot tests, and expect tests under a single API.
Instead of juggling multiple testing libraries, Windtrap gives you one
cohesive package with a PPX for inline expect tests (`ppx_windtrap`).

- Unit tests with combinators, tags, skip, brackets, and timeouts.
- Property-based testing with configurable seeds and shrinking.
- Snapshot testing with automatic file management and diffing.
- Inline expect tests via `ppx_windtrap` with automatic correction.
- CLI test runner with filtering, verbosity, and color support.
- Test coverage reporting with `bisect_ppx` integration.

### Acknowledgments

Windtrap builds on ideas and code from several OCaml projects:

- **[Alcotest](https://github.com/mirage/alcotest)** by Thomas Gazagnaire — test structure and runner design
- **Craig Ferguson's Alcotest PRs** ([#294](https://github.com/mirage/alcotest/pull/294), [#247](https://github.com/mirage/alcotest/pull/247)) — API design, subcomponent diffing, and Levenshtein distance (ISC)
- **[QCheck2](https://github.com/c-cube/qcheck)** by Simon Cruanes et al. — generator design and integrated shrinking (BSD 2-Clause)
- **[ppx_expect](https://github.com/janestreet/ppx_expect)** and **[ppx_inline_test](https://github.com/janestreet/ppx_inline_test)** by Jane Street — expect test paradigm and dune integration
- **[Bisect_ppx](https://github.com/aantron/bisect_ppx)** by Anton Bachin et al. — coverage instrumentation and runtime (MIT)
