# x-demo — the output validation playground

One suite that exercises every output surface windtrap has, with most
tests **failing by design** so every failure report renders. Use it to
eyeball the whole rendering stack after touching the renderer, the
runner, or the assertion vocabulary.

Rules of the house:

- The default run **exits 1 on purpose**. This directory is a plain
  `(executable)`, deliberately *not* wired into `runtest` — the
  whole-tree `dune runtest` stays green. The default alias still
  builds it, so it cannot bit-rot.
- All commands below run from the repository root and were verified
  as written.
- When validating escape codes, look at raw bytes (`| cat -v`), and
  judge the live surfaces in a real terminal — a pipe hides them.

The suite runs 100 tests: 67 pass, 1 skip, 1 expected failure,
31 failures (every failure rendering exactly once), ~2.7 s wall time.

## 1. The default transcript (compact level)

```sh
dune exec examples/x-demo/main.exe
```

Read it top to bottom; every group is one tour stop.

- **Header** `x-demo: 100 tests (seed s1:…)` — the seed prints because
  the suite declares properties.
- **Glyph row** — 60 green dots, then the faint `[60/100]` counter and
  the wrap. Row 2 opens with `S` (skip), faint `x` (expected
  failure), then the first `F`: an `xfail` that *passed*, reported
  loudly as `expected to fail (issue #57), but the test passed`.
- **Flush-on-noteworthy** — glyphs buffer until that first failure
  (test 63): on a TTY you see the header and the buffered row appear
  mid-run, then glyphs stream one by one. A pipe sees byte-identical
  output flushed glyph by glyph.
- **`releasing fixture (examples/x-demo/main.ml:90)`** — the runner's
  note after the last test, before the failure blocks.
- **Failure blocks** (see section 3) — each with `file:line`, the
  source-line excerpt, and the kind-specific detail.
- **Slow warning** — a `slow tests (1):` heading, one indented entry
  (`1.3xs  runtime › reindex …`) with the duration in its own leading
  column, and the hint line naming the `slow` tag and the threshold
  knob. The tagged sibling (`nightly compaction`) earns no warning.
- **Summary** — `67 passed, 1 skipped, 1 expected failure, 31 failed
  (2 subtest failures) in …s.`, and nothing after it: no run advertises
  `--failed`.

## 2. Live terminal surfaces

Run the same command in a real terminal (or through `script`):

```sh
script -q /dev/null dune exec examples/x-demo/main.exe
```

Validate: the faint `[k/n] current-test…` tail that trails the row
while each test runs (where a hung test would show its name), erased
with `\r` + `ESC[2K` before the next glyph — what stays on screen is
exactly what a pipe sees; colors on (green `.`, red `F`, yellow `S`,
faint `x`).

## 3. Every failure kind, one block each

The `assertions` group holds one exemplary failure per verb; jump
straight to it with:

```sh
dune exec examples/x-demo/main.exe -- -f assertions
```

Checklist for the blocks, in order:

| block | validate |
| --- | --- |
| `equal on nested structure` | element-grain highlight: the changed element and the added one marked whole on each side, never a span crossing `;` (marker lines `~~~` under both sides when colors are off) |
| `equal on strings` | char-level highlight: `o`→`a` and `s`→`ed` only |
| `equal on a large array` | `arrays differ at 3 of 300 elements; first at [290]: expected 84100, actual 84101` above a tight unified hunk |
| `not_equal …` | single line `both sides equal: [1; 2; 3]` |
| `is_true hides the data` | `expected true / actual false` and nothing else — no value, just the collapsed boolean; the contrast for the next block |
| `satisfies shows the data` | the `positive` msg, `expected value satisfying the predicate`, and `actual -3` — the data `is_true` hides; the claim sentence is never diffed against the value |
| `contains …` | `needle "tempest" — not found` over the `haystack` line (the excerpt is the whole paragraph here; a partial excerpt earns a faint byte-range line) |
| `not_contains …` | `needle "spice" — found at byte 20` with the occurrence marked `~~~~~` in the haystack (highlighted red instead when colors are on) |
| `require_some on None` | `expected Some _ / actual None`, located at the `require_some` line |
| `require_ok … ~pp_error` | `actual Error parse error at line 3: unexpected ']'` — the printer, not `<abstract>` |
| `require_error … ~pp_ok` | `actual Ok 42` via `~pp_ok` |
| `require_match … ~pp` | `actual Unix_socket "/var/run/mytool.sock"` via `~pp` |
| `raises with the wrong message` | `raised Invalid_argument with the wrong message:` + a *message diff*, not two exception dumps |
| `raises with the wrong exception` | the two-line form: `expected exception  Not_found` / `raised  Failure("connection reset")` |
| `raises when nothing was raised` | `expected exception  Invalid_argument("negative length")` + `but no exception was raised` |
| `raises_match …` | `raised exception does not satisfy the predicate:` + `Failure("stack underflow")` |
| `raises_match when nothing was raised` | the single line `expected an exception, but none was raised` |
| `an uncaught exception fails the test` | the body raises `Not_found` itself (no assertion); the runner catches it at the boundary and words it `uncaught exception:` + the rendered exception — distinct from `raises_match`'s predicate wording. Location falls back to the *test declaration* line. Rerun with `OCAMLRUNPARAM=b` to see the faint backtrace lines under the block |
| `fail marks unreachable branches` | the plain message |
| `failf … (retried once)` | formatted message and `(attempt 2 of 2)` after the test name (`~retries:1`) |

Every block's location points at the assertion's own line with a
source excerpt (the uncaught-exception block, which has no assertion,
points at its test declaration). In the demo each failing assertion
is followed by a statement — an assertion in tail position loses its
stack frame, and its failure is then attributed to the test's
declaration line instead of the assertion's own.

## 4. Properties

In the default transcript, validate:

- header and summary carry the root seed token;
- `reverse is the identity` — `counterexample (case k, shrunk N
  steps):` with a minimal two-element list (e.g. `[0; -1]`; the exact
  pair varies with the seed, the replayed run reproduces it exactly),
  the inner `equal` diff under `which failed at:`, and the replay line
  `replay: dune exec examples/x-demo/main.exe -- --seed s1:…
  -f 'properties › reverse is the identity (it is not)'`;
- `pinned examples run first` — `counterexample (example 1): 3`,
  unshrunk, and *no* replay line (examples do not need the seed);
- `assume filters too hard (gives up)` — `property gave up: 201
  discards exhausted the generation budget (0 cases passed)`: an
  over-constrained `assume` fails loudly instead of silently passing
  on zero cases;
- `insert keeps lists sorted (under-covered)` — `coverage requirement
  not met: long (> 10 elements) 0.0% < 75.0% (0 hits)`, the faint
  `labels (100 passing cases):` distribution from
  `collect`/`classify`, and the `coverage requirements:` table with
  `— unsatisfied`.

Replay determinism — copy the token from the header, then run twice
and compare the counterexample lines (identical, including the shrink
count):

```sh
dune exec examples/x-demo/main.exe -- --seed s1:PASTE_TOKEN -f "reverse is the identity" -q
```

`WINDTRAP_SEED=s1:PASTE_TOKEN` is the env mirror of `--seed`.

## 5. Snapshots

Three tests: a green one (`origin.snap` matches — prints nothing), a
**mismatch**, and a **missing baseline**. Validate in the default
transcript:

- `usage text drifted from its baseline` — `snapshot "usage":
  mismatch with …/__snapshots__/main/usage.snap`, a unified diff
  (baseline lacks `--trace` and says `(default 1)`), and the
  acceptance line `accept: dune exec examples/x-demo/main.exe -- -u,
  then review with git diff`;
- `release notes have no baseline yet` — `no baseline at …`, the
  `proposed (5 lines):` block with `┆` gutters, and the same accept
  line.

The update/restore cycle (both spellings verified; update requests
are refused under `CI`):

```sh
dune exec examples/x-demo/main.exe -- -u -f snapshots
# or: WINDTRAP_UPDATE=1 dune exec examples/x-demo/main.exe -- -f snapshots
```

Validate the acceptance notices, then the on-disk effect and restore
the demo's deliberate drift:

```sh
git diff examples/x-demo/__snapshots__
git restore examples/x-demo/__snapshots__/main/usage.snap
rm examples/x-demo/__snapshots__/main/release-notes.snap
```

(An update run rewrites `usage.snap` to the produced text and creates
`release-notes.snap`; the restore puts the drift back so the demo
fails again. Before the demo's first commit `git restore` has nothing
to restore from — recreate the drifted baseline by hand instead:)

```sh
printf '%s\n' 'usage: mytool [OPTION]... FILE...' '' \
  '  -o DIR      write output under DIR' \
  '  -j JOBS     run JOBS rewrites in parallel (default 1)' \
  > examples/x-demo/__snapshots__/main/usage.snap
```

The CI guard, verified (refuses before anything executes, exit 1):

```sh
CI=1 dune exec examples/x-demo/main.exe -- -u -f snapshots
# snapshot update refused: CI is set. Set WINDTRAP_UPDATE=force to
# update baselines on a CI machine.
```

## 6. Runtime surfaces

In the default transcript's `runtime` group, validate:

- `tokenizer trace precedes the failure` — the failure diff first,
  then `── captured output (last 10 of 14 lines) ──` and the
  `full log: …/_build/_tests/x-demo/…/tokenizer_trace….output` path;
- `backend contract` — two labeled sub-failures in one block
  (`… › disk` and the nested `… › s3 › auth`), the passing `memory`
  sibling still ran, and `(2 subtest failures)` in the summary;
- `counts rows in a bracketed db` — passes; bracket setup/teardown
  are silent by design;
- `body and teardown failures report independently` — one block, two
  entries: the body's `equal` diff at its own line, then the teardown
  failure under a yellow `[teardown]` phase marker with *its* own
  location — neither masks the other;
- the two fixture tests plus the `releasing fixture (…main.ml:90)`
  note after the last test;
- `spins forever` — `timed out after 0.2s` (per-test `~timeout`
  interrupting a busy loop);
- the slow warning contrast described in section 1.

Slow-threshold knobs:

```sh
WINDTRAP_SLOW_THRESHOLD=0 dune exec examples/x-demo/main.exe   # no slow warning line
X_DEMO_SLEEP_SCALE=2 dune exec examples/x-demo/main.exe -- -v  # run > 5s: the -v "slowest tests:" list renders
```

Streaming (`-s` disables capture entirely):

```sh
dune exec examples/x-demo/main.exe -- -s -f "output ()"
```

Validate: `hello, capture` prints directly (before the header — output
interleaves on the real descriptors), and the test fails with
`this test requires capture; rerun without --stream` instead of
comparing against silence.

## 7. The verbosity axis

```sh
dune exec examples/x-demo/main.exe -- -v
dune exec examples/x-demo/main.exe -- -q
```

`-v`: one status line per test — `PASS name 0.1ms`, `SKIP … (no
database in the demo environment)`, `XFAIL … (expected failure:
issue #42)`, `FAIL` — streaming as they happen; same failure blocks
and summary. `-q`: failure blocks, summary (prefixed `x-demo:`), and
the rerun hint only — no header, glyphs, slow warnings, releasing
note, or coverage. Levels change what prints, never outcomes.

## 8. Selection and exit codes

```sh
dune exec examples/x-demo/main.exe -- -l -f "equal on"   # list the selection, run nothing
dune exec examples/x-demo/main.exe -- -f "assertions › satisfies"
dune exec examples/x-demo/main.exe -- -e assertions      # exclude by substring
dune exec examples/x-demo/main.exe -- --tag prop -l      # properties carry the "prop" tag
dune exec examples/x-demo/main.exe -- --exclude-tag io -l  # the runtime group is tagged "io"
dune exec examples/x-demo/main.exe -- --quick -l         # drops the slow-tagged test
dune exec examples/x-demo/main.exe -- --shard 1/3 -l     # deterministic path-hash bucket
dune exec examples/x-demo/main.exe -- -x                 # bail at the first failure
dune exec examples/x-demo/main.exe -- --failed           # rerun only the last run's 31 failures
dune exec examples/x-demo/main.exe -- -f "compact › addition"  # all-green selection: exit 0
dune exec examples/x-demo/main.exe -- -f "no such test"; echo "exit=$?"
```

Validate: `--failed` runs exactly 31 tests, all `F`; the three
`--shard K/3` buckets partition the 100 tests (28/36/36, stable across
machines); the all-green selection is the healthy-run surface —
exactly one line, `x-demo: 41 passed in …s (seed s1:…).`, no header,
no glyphs, exit **0**; the bogus filter prints `x-demo: no tests ran.`
and exits **2**.

## 9. Color and raw bytes

```sh
WINDTRAP_COLOR=never dune exec examples/x-demo/main.exe | cat -v
```

Validate: **zero** escape bytes anywhere, and every highlight degrades
to a `~~~` marker line under its own side — both `expected` and
`actual` carry one, since a deletion shows only on the expected side.
Compare
with the default (piped output under dune is still styled):

```sh
dune exec examples/x-demo/main.exe | cat -v   # ESC[31m etc. visible
```

## 10. Coverage

The dune stanza carries an inert `(instrumentation (backend
ppx_windtrap))`; plain builds have zero overhead. The verified
instrumented commands (coverage is measured over `main.ml` itself):

```sh
dune exec --instrument-with ppx_windtrap examples/x-demo/main.exe -- -f "compact › addition"
```

prints the inline line after the summary:
`coverage: 28.5% (88/309 points, this executable) · project: dune build @cover`
(exact numbers move with any edit to `main.ml`).

```sh
WINDTRAP_COVERAGE=report dune exec --instrument-with ppx_windtrap examples/x-demo/main.exe -- -f "compact › addition"
```

adds the per-file table with uncovered line ranges. The merged view
across all instrumented executables:

```sh
dune exec windtrap -- coverage
```

(also validates the stale-dump exclusion notices when older
`.coverage` files linger under `_build/_coverage`).

## 11. CI artifacts

```sh
dune exec examples/x-demo/main.exe -- -q -f "snapshots › usage" --junit /tmp/junit.xml
cat /tmp/junit.xml
```

Validate: `<testsuites name="windtrap" tests="1" failures="1" …>`
with the failure block inside `<failure message="snapshot
&quot;usage&quot;: mismatch">`.

GitHub Actions annotations require `CI` and `GITHUB_ACTIONS` both set
(as Actions itself sets them) — with `GITHUB_ACTIONS=1` alone nothing
is emitted. The verified run:

```sh
CI=1 GITHUB_ACTIONS=1 dune exec examples/x-demo/main.exe -- -q -f "snapshots › usage"
```

wraps the ordinary compact transcript in a collapsed group and then
emits one annotation per failure, `%0A`-encoded, carrying the same
failure block bytes — acceptance hint included:

```
::group::x-demo
  …the failure block, summary, and rerun hint as in a plain -q run…
::endgroup::
::error file=examples/x-demo/main.ml,line=311,title=Test failure%3A snapshots › usage text drifted from its baseline::    examples/x-demo/main.ml:311%0A    snapshot "usage": mismatch with …%0A…
```

Validate: zero `::` lines when either variable is missing; the
`file=`/`line=` pair points at the failing snapshot call; the live
tail never appears under the GitHub sink.
