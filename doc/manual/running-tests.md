# Running tests

`run suite tests` parses the command line, executes the selected tests
sequentially in declaration order, renders the report, and exits the
process. Multi-file suites export `val tests : test list` per module
and concatenate the lists into one `run` call.

Exit codes are the contract CI scripts rely on:

| code | meaning |
| --- | --- |
| 0 | everything selected passed (or skipped — skips are deliberate) |
| 1 | at least one failure |
| 2 | nothing ran — the filter-typo case; treat it as failure, not success |

The runner owns the process exit. Code under test that calls `exit` —
from a body, setup, teardown, or fixture release — does not terminate
the run: the attempt is intercepted and recorded as that test's (or
that release's) failure, and the run continues to its own exit code.
To assert on exit behavior, run the exiting code in a subprocess.

## Two ways to drive the runner

Direct execution takes flags; under `dune runtest` there is no argv,
so the flags that make sense there have `WINDTRAP_*` environment
mirrors and *the mirrors are the CLI* (the interactive ones — `-x`,
`-l`, `--failed` — have none):

```
$ dune exec test/test_mylib.exe -- -f "parser" -x
$ WINDTRAP_FILTER=parser dune runtest
```

`--help` on the executable prints the full flag and variable
inventory. The ones that matter daily:

| flag | env mirror | effect |
| --- | --- | --- |
| `-f PATTERN` (or bare `PATTERN`) | `WINDTRAP_FILTER` | run tests whose path contains PATTERN |
| `-e PATTERN` | `WINDTRAP_EXCLUDE` | skip tests whose path contains PATTERN |
| `--tag L` / `--exclude-tag L` | `WINDTRAP_TAG` / `WINDTRAP_EXCLUDE_TAG` | select by tag (repeatable; env takes commas) |
| `--quick` | — | skip `slow`-tagged tests |
| `-x` / `--bail N` | — | stop after the first / N failures |
| `--failed` | — | rerun only the last run's failures |
| `-l`, `--list` | — | list the selection without running |
| `--seed s1:…` | `WINDTRAP_SEED` | pin the root seed (replay) |
| `--prop-count N` | `WINDTRAP_PROP_COUNT` | generated cases per property |
| `--timeout SECONDS` | `WINDTRAP_TIMEOUT` | default per-test limit |
| `--slow-threshold SECONDS` | `WINDTRAP_SLOW_THRESHOLD` | warn when an untagged test exceeds SECONDS (default 1; 0 disables) |
| `-u`, `--update` | `WINDTRAP_UPDATE` | accept snapshot changes (refused under CI) |
| `--shard K/N` | `WINDTRAP_SHARD` | run bucket K of N (see below) |
| `-s`, `--stream` | `WINDTRAP_STREAM` | stream output instead of capturing |
| `-v`, `--verbose` | `WINDTRAP_VERBOSE` | one status line per test |
| `-q`, `--quiet` | `WINDTRAP_QUIET` | failures and summary only |
| `--color MODE` / `--junit PATH` | `WINDTRAP_COLOR` (color) | color and JUnit output |

Precedence is programmatic (`?argv`) > CLI > environment > default.
A test's path is its group names then its own, joined with `" › "`;
`-f`/`-e` match that string as a substring.

## Output

Terminal verbosity is one three-level axis — `-q` ⊂ default ⊂ `-v` —
not a format: every level prints the same failure blocks and the same
summary line; the levels only add stream lines and trimmings.

By default the transcript earns its size. A green, healthy run is
exactly one line, named after the suite (with the root seed appended
when the suite declares properties):

```
$ dune runtest
mylib: 4 passed in 0.00081s.
```

The header and the per-test glyph row appear only when the run is
*noteworthy* — any failure, or any test not tagged `slow` exceeding
the slow threshold (one second by default). A noteworthy run replays
the failures in full at the end:

```
$ dune runtest
mylib: 9 tests (seed s1:fbf098819e3014cc)
..FFS.FFx
──────────────────── failures (4) ────────────────────
  FAIL  parser › tokenize
    …(location, diff, captured output — the full blocks)…
──────────────────────────────────────────────────────

3 passed, 1 skipped, 1 expected failure, 4 failed in 0.00179s.
```

| glyph | meaning |
| --- | --- |
| `.` (green) | pass |
| `F` (red) | counted failure — assert, property, snapshot, timeout, unexpected pass; the block at the end differentiates |
| `S` (yellow) | skip |
| `x` (faint) | expected failure (`xfail`) |

Rows wrap every 60 glyphs with a faint `[k/n]` counter. Glyphs buffer
until the first noteworthy event — the first failure, or the first
untagged test over the threshold — then the header and the row so far
print and everything after streams glyph by glyph, byte-identical to
having streamed from the start. On a terminal a faint
`[k/n] current-test…` tail shows from the start of the run while a
test runs — that is where a hung test shows its name — erased before
the next glyph, so what stays on screen is exactly what a pipe sees
and a green run's one-liner stays alone.

### Slow tests

A test that outgrows the threshold does not fail anything — it makes
the run noteworthy and earns a warning between the failure blocks and
the summary:

```
$ dune runtest
mylib: 5 tests
.....
slow tests (1):
  1.31s  reindex
(exempt with the "slow" tag, or raise --slow-threshold SECONDS)

5 passed in 1.31s.
```

Tests that are *supposed* to take time opt out with the `slow` tag
(the `slow` declaration constructor, `~tags:[ "slow" ]`, or a tagged
group) — they are exempt everywhere, and `--quick` skips them
entirely. `--slow-threshold SECONDS` (`WINDTRAP_SLOW_THRESHOLD`)
moves the bar; `0` disables the warnings and the noteworthy trigger,
so the row then appears on failures only. The slowest-tests list —
diagnosis rather than signal — prints under `-v` only.

Where the bar sits is a per-suite decision. Tests that do real IO —
spawning subprocesses, driving a PTY, exercising a server end to
end — legitimately spend seconds doing their job, and the default
one-second threshold would flag every one of them. Choose
deliberately: raise the bar with `--slow-threshold`
(`WINDTRAP_SLOW_THRESHOLD`) when that pace is the suite's normal and
every test should still run everywhere, or tag the tests `slow` when
a fast loop may also drop them — the tag silences the warning *and*
removes the test from `--quick` runs, so it trades noise for absence.

`-v` (`WINDTRAP_VERBOSE`) prints one status line per test instead of
the glyph, and a passing property that collected labels prints its
label distribution under its `PASS` line — the calibration view for
`collect`/`classify` ([Property testing](property-testing.md)):

```
$ dune exec test/test_mylib.exe -- -v
mylib: 9 tests (seed s1:fbf098819e3014cc)
  PASS  addition                                   0.1ms
  PASS  parser › empty input                       0.1ms
  FAIL  parser › tokenize                          0.1ms
  FAIL  parser › precedence                        0.0ms
  SKIP  users › lookup missing (needs a database)
  PASS  users › session count                      0.0ms
  FAIL  rev involutive                             0.6ms
  FAIL  snapshot demo — no baseline                0.1ms
  XFAIL  unicode width (expected failure: issue #42)  0.1ms
──────────────────── failures (4) ────────────────────
  …
```

Verbose also keeps the slowest-tests list and prints the same slow
warnings; it never defers — every line streams as it happens.

`-q` (`WINDTRAP_QUIET`) prints the failure blocks, the summary, and
the rerun hint, nothing else — no slow warnings, no coverage, no
snapshot notices. A green quiet run is the same named one-liner as the
default level. `no tests ran.` still prints; it explains exit code 2.

The level decides *what* prints; the sink only decides color and the
live tail. Piped output — redirects, CI logs — has the same shape,
with glyphs flushed one by one so a crashed run leaves its partial row
visible: uncolored for a plain pipe, still colored under dune (dune
relays to your terminal), never colored when `TERM=dumb`. Under GitHub
Actions the same compact transcript sits inside a collapsed
`::group::` block, with failures also emitted as annotations (see
[CI](#ci) below).

## The feedback loop

The last line of a failing run is the rerun command
(`… --failed`); `--failed` reruns only what failed last time, `-x`
stops at the first failure, and `-l` shows what a filter would select
before you run it:

```
$ dune exec test/test_mylib.exe -- -l -f parser
parser › empty input
```

The last-failed store lives under the capture-log directory
(`_build/_tests` by default) and is maintained automatically; its
format is unstable. `--failed` with no recorded failures for the
current suite — a fresh checkout, a wiped log directory — refuses the
run (`no recorded failures match the current suite`, exit 2) rather
than silently running everything.

## Captured output

By default each test's stdout and stderr are captured — C stubs and
subprocesses included — so a green run is quiet and a failing test's
report includes the tail of what it printed, with the path to the full
log:

```
    ── captured output (4 lines) ──
    INT 1
    PLUS
    INT 2
    EOF
    full log: /home/dev/mytool/_build/_tests/mylib/JACOM9WP/parser/tokenize.output
```

`WINDTRAP_TAIL_ERRORS` bounds the tail; `-o DIR` moves the log root.
`--stream` disables capture entirely — output interleaves on the real
descriptors, for printf-debugging a hang. `output ()` is the one
operation whose meaning requires captured bytes: under `--stream` it
fails the calling test with an explicit message instead of comparing
against silence.

## Sharding

`--shard K/N` deterministically partitions the selected tests into `N`
buckets by a frozen hash of each test's path and runs bucket `K` (1-based):
run the same suite in `N` CI jobs with `WINDTRAP_SHARD=1/4` …
`4/4` and every test runs exactly once, stable across machines and
suite composition. An empty bucket exits 2 like any empty selection.

## CI

Detection is ambient: `CI` set means CI. Under CI the runner refuses
runs that would lie — focused tests (`ftest`/`fgroup`) and snapshot
update requests refuse to start before anything executes
(`WINDTRAP_ALLOW_FOCUS=1` / `WINDTRAP_UPDATE=force` override
deliberately).

- **JUnit**: `--junit PATH` also writes a JUnit XML report — point
  your CI's test-report ingestion at it.
- **GitHub Actions**: under GitHub Actions (`CI` and `GITHUB_ACTIONS`
  both set, as Actions sets them), failures are additionally emitted
  as workflow annotations — they appear inline on the PR diff with no
  configuration.
- **Color**: on by default on a terminal and under dune, off when
  `TERM=dumb`; `--color always|never|auto` (`WINDTRAP_COLOR`)
  overrides either way.
- The run header prints the root seed token whenever the suite
  declares property tests — filters do not remove it, so a CI log line
  is all you need to replay a red property locally.

Inline (`ppx_windtrap`) suites are driven by dune's inline-test
protocol instead: dune builds a runner per library, runs the affected
partition, and applies `dune promote` corrections; the standalone exit
codes above do not apply there — the promotion protocol does.
