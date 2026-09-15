# Running tests

`run suite tests` parses the command line, executes the selected tests
sequentially in declaration order, renders the report, and returns the
exit code; the suite's `main` hands it to the process:

```ocaml
let () = exit @@ run "mylib" tests
```

Multi-file suites export `val tests : test list` per module and
concatenate the lists into one `run` call.

Exit codes are the contract CI scripts rely on:

| code | meaning |
| --- | --- |
| 0 | everything selected passed (or skipped — skips are deliberate) |
| 1 | at least one failure |
| 2 | nothing ran — the filter-typo case; treat it as failure, not success |

Under `--corrected` — a build action's run — a recorded correction is
not a failure and an emptied selection is not an error: a `WINDTRAP_*`
selection spans every stanza and inline partition of the tree, so one
it leaves empty exits 0 with its `no tests ran` line, and dune's `diff?`
is the verdict.

Every path out of `run` is a returned code — `--help` and `--version`
(0), a command line it cannot parse or resolve (2), `-l` (0), a startup
refusal (1, or 2 for `--failed` with nothing recorded), and the run's
own verdict. `run` returns the code rather than applying it so that one
binary can host two suites, or a harness can post-process a run
in-process; a `main` that forgets the `exit` is a type error rather than
a binary that is green on failure, and only a deliberate `ignore` drops
the code. `run` refuses to start inside an active run — a test body
cannot start another run.

The run owns its exit code. Code under test that calls `exit` — from a
body, setup, teardown, scope, or fixture release — does not terminate
the run: the attempt is intercepted and recorded as that test's (or
that release's) failure, and the run continues to its own code. A
handler that catches every exception around the exiting call defeats
the interception, exactly as it would swallow an assertion failure; to
assert on exit behavior, run the exiting code in a subprocess.

## Two ways to drive the runner

Direct execution takes flags. Under `dune runtest` there is no argv, so
every flag that changes what a run does or reports has a `WINDTRAP_*`
environment mirror, and there *the mirrors are the CLI*; `-l`,
`--failed`, `-x`, `-u`, `--corrected`, `-h` and `-V` have none, because
they want a command line:

```
$ dune exec test/test_mylib.exe -- -f parser
$ WINDTRAP_FILTER=parser dune runtest --force
```

An environment variable changes nothing on a warm tree: dune caches a
test action by its declared inputs, and a variable is not one of them
unless the stanza declares `(deps (env_var WINDTRAP_FILTER))` or the
run passes `--force`. Pass `--force` for an ad hoc run; declare the
dependency on a stanza that is meant to react, as a nightly profile
that raises the case count does:

```lisp
(test
 (name test_mylib)
 (libraries windtrap)
 (deps (env_var WINDTRAP_PROP_COUNT)))

(env
 (nightly (env-vars (WINDTRAP_PROP_COUNT 10000))))
```

`--help` on the executable prints the full flag and variable
inventory. The ones that matter daily:

| flag | env mirror | effect |
| --- | --- | --- |
| `-f PATTERN` (or bare `PATTERN`) | `WINDTRAP_FILTER` | run tests whose path contains PATTERN |
| `-e PATTERN` | `WINDTRAP_EXCLUDE` | skip tests whose path contains PATTERN |
| `--tag L` / `--exclude-tag L` | `WINDTRAP_TAG` / `WINDTRAP_EXCLUDE_TAG` | select by tag (repeatable; env takes commas) |
| `--failed` | — | rerun only the last run's failures |
| `-l`, `--list` | — | list the selection without running |
| `--seed s1:…` | `WINDTRAP_SEED` | pin the root seed (replay) |
| `--prop-count N` | `WINDTRAP_PROP_COUNT` | generated cases per property |
| `--timeout SECONDS` | `WINDTRAP_TIMEOUT` | default per-test limit |
| `--slow-threshold SECONDS` | `WINDTRAP_SLOW_THRESHOLD` | warn when an untagged test exceeds SECONDS (default 1; 0 disables) |
| `-u`, `--update` | — | accept baseline changes in place (refused under CI) |
| `--corrected` | — | write each correction as `<file>.corrected`, for `dune promote` |
| `--shard K/N` | `WINDTRAP_SHARD` | run bucket K of N (see below) |
| `-s`, `--stream` | `WINDTRAP_STREAM` | stream output instead of capturing |
| `-v`, `--verbose` | `WINDTRAP_VERBOSE` | one status line per test |
| `-x`, `--fail-fast` | — | stop after the first failure |
| `--color MODE` | `WINDTRAP_COLOR` | color output |
| `--junit PATH` | `WINDTRAP_JUNIT` | also write a JUnit XML report |
| `-o`, `--output DIR` | `WINDTRAP_OUTPUT` | root directory for capture logs |
| `--mutate[=PREFIX,…]` | `WINDTRAP_MUTATE` | run the mutation survey, every mutant or those under a prefix (`1` in the mirror is the bare flag; see [Mutation testing](mutation.md)) |
| `--arm ID` | `WINDTRAP_MUTATE_ARM` | run once with mutant `ID` armed |

Precedence is CLI > environment > default.
A test's path is its group names then its own, joined with `" › "`;
`-f`/`-e` match that string as a substring.

Two variables have no flag at all — the project root and the coverage
dump's path. `--help` lists them under `ENVIRONMENT`; one is worth
knowing here.

`WINDTRAP_PROJECT_ROOT` overrides where the runner thinks the project
starts: the directory baseline paths resolve under. Unset, the root is
the directory above the build directory the process belongs to — under
dune, `INSIDE_DUNE` names the build context (`<root>/_build/default`, a
private `--build-dir` included when its name starts with `_build`,
whatever the working directory), and a
test binary run by hand finds its own `_build` in its path — and
otherwise the working directory. No marker file is consulted. Set it
when neither applies: a scratch tree built by a test harness, a binary
installed outside any build directory and run from a subdirectory of
its project, or a child process you are aiming at a sandbox of your own
making. A relative value resolves against the working directory.

Capture logs and the last-failed store live under `<build dir>/_tests`
when a build directory was found — so a private `--build-dir` keeps its
own — and under `<tmp>/windtrap` otherwise, keyed by suite either way;
`-o DIR` moves both.

## Output

Terminal verbosity is one axis with two levels — default ⊂ `-v` — not
a format: both print the same blocks and the same summary line; `-v`
adds the header up front, one status line per test, and the
slowest-tests list.

By default a run prints nothing per test, and the transcript earns its
size at the end. A green, healthy run is exactly one line, named after
the suite (with the root seed appended when the suite declares
properties):

```
$ dune runtest
mylib: 4 passed in 0.00081s.
```

The header appears iff there is a block to print — a failure, a test
not tagged `slow` over the slow threshold (one second by default), or a
test that passed on a retry — and the blocks follow it, the failures in
full:

```
$ dune runtest
mylib: 9 tests (seed s1:fbf098819e3014cc)
──────────────────── failures (4) ────────────────────
  FAIL  parser › tokenize
    …(location, diff, captured output — the full blocks)…
──────────────────────────────────────────────────────

3 passed, 1 skipped, 1 expected failure, 4 failed in 0.00179s.
```

On a terminal a faint `[k/n] current-test…` tail shows while a test
runs — that is where a hung test shows its name — erased before
anything else prints, so what stays on screen is exactly what a pipe
sees and a green run's one-liner stays alone.

### Slow tests

A test that outgrows the threshold does not fail anything — it brings
the header out and earns a warning between the failure blocks and the
summary:

```
$ dune runtest
mylib: 5 tests
slow tests (1):
  1.31s  reindex
(exempt with the "slow" tag, or raise --slow-threshold SECONDS)

5 passed in 1.31s.
```

Tests that are *supposed* to take time opt out with the `slow` tag
(the `slow` declaration constructor, `~tags:[ "slow" ]`, or a tagged
group) — they are exempt everywhere, and `--exclude-tag slow` skips
them entirely. `--slow-threshold SECONDS` (`WINDTRAP_SLOW_THRESHOLD`)
moves the bar; `0` disables the warnings, so the header then comes out
on failures and flaky tests only. The slowest-tests list — diagnosis
rather than signal — prints under `-v` only.

Where the bar sits is a per-suite decision. Tests that do real IO —
spawning subprocesses, driving a PTY, exercising a server end to
end — legitimately spend seconds doing their job, and the default
one-second threshold would flag every one of them. Choose
deliberately: raise the bar with `--slow-threshold`
(`WINDTRAP_SLOW_THRESHOLD`) when that pace is the suite's normal and
every test should still run everywhere, or tag the tests `slow` when
a fast loop may also drop them — the tag silences the warning *and*
lets `--exclude-tag slow` remove the test, so it trades noise for
absence.

### Flaky tests

A test declared with `~retries` that fails and then passes counts as
passed, and never silently: the header comes out and a block between
the slow block and the summary names the test and the attempt it
passed on. `--junit` notes it in the testcase's `system-out`.

```
$ dune runtest
mylib: 5 tests
flaky tests (1):
  passed on attempt 2  network › fetches the manifest

5 passed in 0.42s.
```

`-v` (`WINDTRAP_VERBOSE`) prints the header up front and one status
line per test as it completes, and a passing property that collected
labels prints its label distribution under its `PASS` line — the
calibration view for `collect`/`classify`
([Property testing](property-testing.md)):

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
  FAIL  help page — no baseline                    0.1ms
  XFAIL  unicode width (expected failure: issue #42)  0.1ms
──────────────────── failures (4) ────────────────────
  …
```

Verbose also keeps the slowest-tests list and prints the same slow and
flaky blocks; every line streams as it happens, so a crashed verbose
run leaves its status lines behind.

The level decides *what* prints; the sink only decides color and the
live tail. Piped output — redirects, CI logs — has the same shape:
uncolored for a plain pipe, still colored under dune (dune relays to
your terminal), never colored when `TERM=dumb` or `NO_COLOR` is set.
Under GitHub Actions the same transcript sits inside a collapsed
`::group::` block, with failures also emitted as annotations (see
[CI](#ci) below).

## The feedback loop

The summary is the last line of a failing run — no run advertises a
flag. `--failed` reruns only what failed last time, `-x` stops at the
first failure, and `-l` shows what a filter would select before you
run it:

```
$ dune exec test/test_mylib.exe -- -l -f parser
parser › empty input
```

The last-failed store lives under the log root (`<build dir>/_tests`
under dune, see above) and is maintained automatically; its format is
unstable. `--failed` with no recorded failures for the
current suite — a fresh checkout, a wiped log directory — refuses the
run (`no recorded failures match the current suite`, exit 2) rather
than silently running everything.

`-o`/`--output` moves the store with the logs: it is
`<output>/<suite>/.last-failed`. Two runs with different `-o` do not
share a failure set, and the second refuses `--failed` rather than
rerunning what the first recorded. `--failed` has no mirror because a
mirror could not help: under `dune runtest` the run that failed is
exactly the one dune will not repeat until an input changes, so the
loop is real only on a directly executed binary.

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
    full log: _build/_tests/mylib/parser/tokenize.output
```

The path is the test's identity under the suite, so it is the same on
every run — type it into an editor once and reruns keep it pointing at
the current output. The tail is the last ten lines of the last 8 KiB
the capture kept, not a knob; `-o DIR` moves the log root.
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
runs that would lie — focused tests (`focus`) and in-place baseline
updates (`-u`) refuse to start before anything executes. Neither has an
override: remove the `focus`, and accept baselines under CI through a
`--corrected` run and `dune promote`.

- **JUnit**: `--junit PATH` (`WINDTRAP_JUNIT`) also writes a JUnit XML
  report. A target ending in `.xml` is that exact file; anything else
  is a **directory**, and each suite writes `<dir>/<suite>.xml` into
  it. The two forms exist because the two spellings are asked in
  different situations: a flag on one executable is one suite and one
  file, while `dune runtest` starts a process per `(test)` stanza and
  per inline-test library, and a single fixed path would have each
  silently overwrite the last. So in CI:

  ```sh
  WINDTRAP_JUNIT=_build/junit dune runtest
  ```

  and point ingestion at `_build/junit/*.xml`. Inline (`ppx_windtrap`)
  partitions write their reports there too — the mirror is the only
  spelling that reaches them, since the inline protocol has no CLI.
- **GitHub Actions**: under GitHub Actions (`CI` and `GITHUB_ACTIONS`
  both set, as Actions sets them), failures are additionally emitted
  as workflow annotations — they appear inline on the PR diff with no
  configuration.
- **Color**: on by default on a terminal and under dune, off when
  `TERM=dumb` or `NO_COLOR` is set to anything;
  `--color always|never|auto` (`WINDTRAP_COLOR`) overrides either way.
- The run header prints the root seed token whenever the suite
  declares property tests — filters do not remove it, so a CI log line
  is all you need to replay a red property locally.

Inline (`ppx_windtrap`) suites are driven by dune's inline-test
protocol: dune builds a runner per library and runs each source file as
a partition, and the runner calls `run` with `--corrected`. The exit
codes above apply there too — a partition whose failures are all
recorded corrections exits 0, dune's diff is the verdict, and
`dune promote` accepts — and the `WINDTRAP_*` mirrors are the rest of
its command line.
