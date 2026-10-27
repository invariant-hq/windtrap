What a report leaves outside the process that printed it: the bytes a
test writes under --stream, where a failing test's log lands, and the
replay line, spelled for the way the run was started and run again as
pasted. The report's layout is pinned by test/unit's report suites.

  $ run() {
  >   env -i PATH="$PATH" TMPDIR="${TMPDIR:-/tmp}" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_PROJECT_ROOT="$PWD" \
  >       WINDTRAP_OUTPUT="$PWD/_logs" "$@"
  > }
  $ scrub() {
  >   sed -E 's/ in [0-9.]+m?s\./ in DURATION./; s/s1:[0-9a-f]+/SEED/'
  > }

--stream captures nothing: each test's bytes reach standard output
whichever way they leave the process (the stdout channel left unflushed,
descriptor 1, a subprocess), in the order the tests ran and before the
report's own lines. A green streamed run is its tests' bytes and the one
line.

  $ run env FACADE_FIXTURE=stream ./suite_main.exe --stream > out 2> err
  [1]
  $ head -n 5 out
  through the stdout channel
  through descriptor 1
  through a subprocess
  before the failure
  fixture: 4 tests
  $ cat err
  $ run env FACADE_FIXTURE=stream ./suite_main.exe -s -e fails > out 2>&1
  $ scrub < out
  through the stdout channel
  through descriptor 1
  through a subprocess
  fixture: 3 passed in DURATION.

Under -j N the streamed tests still run one at a time, so the bytes keep
that order:

  $ run env FACADE_FIXTURE=stream ./suite_main.exe -s -e fails -j 3 > out 2>&1
  $ scrub < out
  through the stdout channel
  through descriptor 1
  through a subprocess
  fixture: 3 passed in DURATION.

Under GitHub Actions the transcript folds, the annotations follow the
fold's close so that they are never folded away, and the summary is still
the last line (the envelope's lines are the report suite's):

  $ run CI=true GITHUB_ACTIONS=true ./suite_main.exe -f boom > out 2>&1
  [1]
  $ grep -o -e '^::[a-z]*' out
  ::group
  ::endgroup
  ::error
  $ tail -1 out | scrub
  1 failed in DURATION.

A failing test's captured output is kept in a log, whose path ends its
block. The sessions run the executable from under the build directory,
whose _tests hold the logs; run from anywhere else (a copy in a temporary
directory, with no WINDTRAP_PROJECT_ROOT and no build directory in sight),
the logs go under the system temporary directory, keyed by suite, and no
_build grows:

  $ dir=$(mktemp -d)
  $ cp ./suite_main.exe "$dir/suite.exe"
  $ mkdir "$dir/tmp"
  $ env -i PATH="$PATH" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >   TMPDIR="$dir/tmp" FACADE_FIXTURE=noisy \
  >   "$dir/suite.exe" > out 2>&1
  [1]
  $ grep 'full log:' out | sed "s#$dir#<tmp>#g"
      full log: <tmp>/tmp/windtrap/fixture/noisy.output
  $ cat "$dir/tmp/windtrap/fixture/noisy.output"
  hello from noisy
  $ test -e "$dir/_build" || echo 'no _build grown'
  no _build grown
  $ rm -rf "$dir"

A report whose failures include a property's closes on the replay line.
Run by hand, the line restates the program as it was typed, then the
seed:

  $ run env FACADE_FIXTURE=property ./suite_main.exe > out 2> err
  [1]
  $ grep 'replay:' out | scrub
  replay: ./suite_main.exe --seed SEED
  $ cat err

Run by dune, the program is dune's test action (INSIDE_DUNE set, the
path relative to the action's directory): the line is a dune exec of the
program's path from the project root. A core built with the mutation
backend adds that backend's flag, which test/unit's mutation loop suite
pins; it is masked here so that the line reads the same in either build.

  $ mask() { sed -E 's/--instrument-with ppx_windtrap\.mutate //'; }
  $ mkdir sub && cp ./suite_main.exe sub/
  $ run INSIDE_DUNE=1 FACADE_FIXTURE=property ./sub/suite_main.exe > out 2> err
  [1]
  $ grep 'replay:' out | scrub | mask
  replay: dune exec sub/suite_main.exe -- --seed SEED
  $ rm -r sub

dune exec reads a word without a slash as the name of a program to look
up, so a program at the project root keeps its ./. At the root of a
project that holds the program, the line runs as pasted and fails again
on the same case:

  $ mkdir proj && cp ./suite_main.exe proj/ && cd proj
  $ echo '(lang dune 3.0)' > dune-project
  $ run INSIDE_DUNE=1 FACADE_FIXTURE=property ./suite_main.exe > out 2> err
  [1]
  $ sed -n 's/^ *replay: //p' out | mask > replay
  $ scrub < replay
  dune exec ./suite_main.exe -- --seed SEED
  $ eval "run INSIDE_DUNE=1 FACADE_FIXTURE=property $(cat replay)" > again 2>&1
  [1]
  $ grep 'counterexample' out > before && grep 'counterexample' again > after
  $ diff before after && echo same counterexample
  same counterexample
  $ cd .. && rm -r proj

Two properties fail on cases the seed picks, beside a test that passes.
The one replay line, pasted, runs the three tests again, and each
property fails again on its case, shrunk to the same counterexample:

  $ run env FACADE_FIXTURE=properties ./suite_main.exe > out 2> err
  [1]
  $ grep 'replay:' out | scrub
  replay: ./suite_main.exe --seed SEED
  $ eval "run env FACADE_FIXTURE=properties $(sed -n 's/^replay: //p' out)" > again 2>&1
  [1]
  $ head -1 again | scrub
  fixture: 3 tests (seed SEED)
  $ grep 'counterexample' out > before && grep 'counterexample' again > after
  $ wc -l < before | tr -d ' '
  2
  $ diff before after && echo same counterexamples
  same counterexamples

A narrowed run's line restates its selection, and runs that selection
alone:

  $ run env FACADE_FIXTURE=properties ./suite_main.exe -f even > out 2> err
  [1]
  $ grep 'replay:' out | scrub
  replay: ./suite_main.exe --seed SEED -f 'even'
  $ eval "run env FACADE_FIXTURE=properties $(sed -n 's/^replay: //p' out)" > again 2>&1
  [1]
  $ head -1 again | scrub
  fixture: 1 test (seed SEED)

A run given --failed selected the record of the last failed tests, which
lies under its -o, so its line restates both. A record that names the two
properties whole, by their paths alone, has them draw their cases again,
and the line reruns them on their cases:

  $ rm -f _logs/fixture/.last-failed-*
  $ run env FACADE_FIXTURE=properties ./suite_main.exe > /dev/null 2>&1
  [1]
  $ store=$(echo _logs/fixture/.last-failed-*)
  $ printf 'windtrap-last-failed 2\neven\nsmall\n' > "$store"
  $ run env FACADE_FIXTURE=properties ./suite_main.exe --failed > out 2> err
  [1]
  $ grep 'replay:' out | scrub
  replay: ./suite_main.exe --seed SEED -o _logs --failed
  $ eval "run env FACADE_FIXTURE=properties $(sed -n 's/^replay: //p' out)" > again 2>&1
  [1]
  $ head -1 again | scrub
  fixture: 2 tests (seed SEED)
  $ grep 'counterexample' out | sed 's/.*: //' > before
  $ grep 'counterexample' again | sed 's/.*: //' > after
  $ diff before after && echo same counterexamples
  same counterexamples

A run of the whole suite records each property with its counterexample's
token, and --failed then checks those counterexamples again, as --replay
checks one: it draws nothing, so its report needs no replay line.

  $ run env FACADE_FIXTURE=properties ./suite_main.exe > first 2>&1
  [1]
  $ run env FACADE_FIXTURE=properties ./suite_main.exe --failed > out 2> err
  [1]
  $ grep -c 'replay:' out
  0
  [1]
  $ grep 'counterexample' first | sed 's/.*: //' > before
  $ grep 'counterexample (replayed case)' out | sed 's/.*: //' > after
  $ diff before after && echo same counterexamples
  same counterexamples

--json writes the results as one JSON document, a test per line, each
with the scope of its verdict, then the closing counts: what a property,
a table and a stateful test ran, a vacuous test's reason, and an empty
scope for a test that ran no case. The durations are masked:

  $ run env FACADE_FIXTURE=scope ./suite_main.exe --seed s1:0000000000000001 --json scope.json > out 2>&1
  [1]
  $ sed -E 's/"duration": [0-9.e-]+/"duration": D/g' scope.json
  {"version": 1, "suite": "fixture", "seed": "s1:0000000000000001", "tests": [
    {"path": ["labels"], "verdict": "pass", "attempts": 1, "duration": D, "failures": [], "scope": {"cases": 200, "examples": 1, "count": 200, "discards": 26, "labels": [{"label": "even", "cases": 77}, {"label": "small", "cases": 26}], "cover": [{"label": "small", "cases": 26, "met": true}], "tolerances": []}},
    {"path": ["rows"], "verdict": "pass", "attempts": 1, "duration": D, "failures": [], "scope": {"cases": 3, "examples": 0, "count": 3, "discards": 0, "rows": 3, "skipped": 0, "labels": [], "cover": [], "tolerances": []}},
    {"path": ["counter"], "verdict": "pass", "attempts": 1, "duration": D, "failures": [], "scope": {"cases": 20, "examples": 0, "count": 20, "discards": 0, "labels": [], "cover": [], "tolerances": []}},
    {"path": ["shares"], "verdict": "pass", "attempts": 1, "duration": D, "failures": [], "scope": {"cases": 800, "examples": 0, "count": 800, "discards": 0, "labels": [{"label": "a quarter", "cases": 223}], "cover": [{"label": "a quarter", "cases": 223, "met": true, "share": {"demand": 0.2, "hits": 223, "cases": 800, "low": 0.19343228918344313, "high": 0.3837927486271963}}], "tolerances": []}},
    {"path": ["plain"], "verdict": "pass", "attempts": 1, "duration": D, "failures": [], "scope": {"tolerances": []}},
    {"path": ["close"], "verdict": "pass", "attempts": 1, "duration": D, "failures": [], "scope": {"tolerances": [{"file": "test/cram/run/suite_main.ml", "line": 138, "comparisons": 1, "error": 0.04999999999999716, "tolerance": 0.10005}, {"file": "test/cram/run/suite_main.ml", "line": 139, "comparisons": 1, "error": 3.000000026176508e-09, "tolerance": 1e-06}]}},
    {"path": ["far"], "verdict": "fail", "attempts": 1, "duration": D, "failures": [{"headline": "expected 1., got 1.5", "file": "test/cram/run/suite_main.ml", "line": 141}], "scope": {"tolerances": [{"file": "test/cram/run/suite_main.ml", "line": 141, "comparisons": 1, "error": 0.5, "tolerance": 1e-06}, {"file": "test/cram/run/suite_main.ml", "line": 142, "comparisons": 1, "error": 1.000000082740371e-09, "tolerance": 1e-06}]}},
    {"path": ["near"], "verdict": "pass", "attempts": 1, "duration": D, "failures": [], "scope": {"cases": 100, "examples": 0, "count": 100, "discards": 0, "labels": [], "cover": [], "tolerances": [{"file": "test/cram/run/suite_main.ml", "line": 144, "comparisons": 100, "error": 2.000177801164682e-12, "tolerance": 1e-09}]}},
    {"path": ["nothing"], "verdict": "vacuous", "reason": "no case and no example ran: ~count:0 runs the examples alone, and there are none", "attempts": 1, "duration": D, "failures": [{"headline": "vacuous: no case and no example ran: ~count:0 runs the examples alone, and there…", "file": "test/cram/run/suite_main.ml", "line": 146}], "scope": {"cases": 0, "examples": 0, "count": 0, "discards": 0, "labels": [], "cover": [], "tolerances": []}},
    {"path": ["skips"], "verdict": "skip", "reason": "later", "attempts": 1, "duration": D, "failures": [], "scope": {"tolerances": []}}
  ], "counts": {"tests": 10, "passed": 7, "failed": 1, "vacuous": 1, "skipped": 1, "expected_failures": 0, "flaky": 0, "not_run": 0, "release_failures": 0, "duration": D}}
  $ tail -1 out | scrub
  7 passed, 1 skipped, 1 failed, 1 vacuous in DURATION.

Under -v the terminal says the same of each passing test, on its row:

  $ run env FACADE_FIXTURE=scope ./suite_main.exe --seed s1:0000000000000001 -v -f labels -f rows -f counter -f shares -f close -f near | sed -E 's/  [0-9.]+m?s/  DURATION/' | scrub
  fixture: 6 tests (seed SEED)
    PASS  labels                                     DURATION (200 cases, 1 example, 26 discards)
      labels (201 passing cases):
         38.3%  even
         12.9%  small
    PASS  rows                                       DURATION (holds on all 3 rows)
    PASS  counter                                    DURATION (20 cases)
    PASS  shares                                     DURATION (800 cases)
      labels (800 passing cases):
         27.9%  a quarter
      cover "a quarter" ~share:0.2: 27.9% of 800 cases, at least 19.3%
    PASS  close                                      DURATION
      tolerance at test/cram/run/suite_main.ml:138: largest error 0.05 of 0.1 allowed, over 1 comparison
      tolerance at test/cram/run/suite_main.ml:139: largest error 3e-09 of 1e-06 allowed, over 1 comparison
    PASS  near                                       DURATION (100 cases)
      tolerance at test/cram/run/suite_main.ml:144: largest error 2e-12 of 1e-09 allowed, over 100 comparisons
  6 passed in DURATION.

A worker reads the scope, tolerances included, and its row carries it to
the parent, which writes the document: a run of the test in the calling
process with --in-process gives the same, and so does one under -j 3:

  $ mask() { sed -E 's/"duration": [0-9.e-]+/"duration": D/g' "$1"; }
  $ run env FACADE_FIXTURE=scope ./suite_main.exe -f near --seed s1:0000000000000001 --json worker.json > /dev/null
  $ run env FACADE_FIXTURE=scope ./suite_main.exe -f near --seed s1:0000000000000001 --in-process --json here.json > /dev/null
  $ run env FACADE_FIXTURE=scope ./suite_main.exe -f labels -f near -f close --seed s1:0000000000000001 -j 3 --json jobs.json > /dev/null
  $ mask worker.json > a; mask here.json > b; cmp a b && echo same scope
  same scope
  $ grep -c '"tolerances": \[{' jobs.json
  2

A failure states the same, in its block, as the largest error that each
comparison under a tolerance saw against the bound it allowed:

  $ run env FACADE_FIXTURE=scope ./suite_main.exe -f far | scrub
  fixture: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  far
      test/cram/run/suite_main.ml:141
      expected  1.
      actual    1.5
                  ~
      tolerance at test/cram/run/suite_main.ml:141: largest error 0.5 of 1e-06 allowed, over 1 comparison
      tolerance at test/cram/run/suite_main.ml:142: largest error 1e-09 of 1e-06 allowed, over 1 comparison
  ──────────────────────────────────────────────────────────
  
  1 failed in DURATION.

A target that does not end in .json is a directory, which gets one file
per suite, and the mirror's relative path is read from the project root:

  $ run env FACADE_FIXTURE=scope WINDTRAP_JSON=reports ./suite_main.exe -f plain > /dev/null
  $ ls reports
  fixture.json
