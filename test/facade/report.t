What the report shows beyond a failure, and where a run keeps its logs.

  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_COVERAGE=off \
  >       WINDTRAP_PROJECT_ROOT="$PWD" "$@"
  > }
  $ scrub() {
  >   sed -E 's/ in [0-9.e+-]+s\./ in DURATION./; s/suite_main\.ml:[0-9]+/suite_main.ml:LINE/'
  > }

A test that fails and then passes on a retry is never silent: the run
exits 0 and counts it as passed, but the header comes out and the flaky
block names the test with the attempt it passed on.

  $ run FACADE_FIXTURE=flaky ./suite_main.exe | scrub
  fixture: 1 test
  flaky tests (1):
    passed on attempt 2  flaky
  
  1 passed in DURATION.

Verbose already carries the attempt count on the status line, and keeps
the block:

  $ run FACADE_FIXTURE=flaky ./suite_main.exe -v | scrub | sed -E 's/  +[0-9.]+m?s /  TIME /'
  fixture: 1 test
    PASS  flaky  TIME (2 attempts)
  flaky tests (1):
    passed on attempt 2  flaky
  
  1 passed in DURATION.

JUnit has no state for it, so the testcase says so in its system-out:

  $ run FACADE_FIXTURE=flaky ./suite_main.exe --junit report.xml > /dev/null
  $ grep -A1 'name="flaky"' report.xml
      <testcase name="flaky" classname="fixture" time="0.000">
        <system-out>passed on attempt 2</system-out>

A failing test's captured output ends its block with the full log's
path. The sessions above run the executable from under the build
directory, where the logs live in its _tests; run by hand from anywhere
else — a copy in a temporary directory, with no WINDTRAP_PROJECT_ROOT
and no build directory in sight — the logs go under the system
temporary directory, keyed by suite, and never grow a _build.

  $ dir=$(mktemp -d)
  $ cp ./suite_main.exe "$dir/suite.exe"
  $ mkdir "$dir/tmp"
  $ env -i PATH="$PATH" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >   WINDTRAP_COVERAGE=off TMPDIR="$dir/tmp" FACADE_FIXTURE=noisy \
  >   "$dir/suite.exe" > out 2>&1
  [1]
  $ scrub < out | sed "s#$dir#<tmp>#g"
  fixture: 1 test
  ──────────────────── failures (1) ────────────────────
    FAIL  noisy
      test/facade/suite_main.ml:LINE
      (assertion in tail position: its line is unknown; ~__POS__ names it)
      deliberate
      expected  1
      actual    2
      ── captured output (1 line) ──
      hello from noisy
      full log: <tmp>/tmp/windtrap/fixture/noisy.output
  ──────────────────────────────────────────────────────
  
  1 failed in DURATION.
  $ cat "$dir/tmp/windtrap/fixture/noisy.output"
  hello from noisy
  $ test -e "$dir/_build" || echo 'no _build grown'
  no _build grown
  $ rm -rf "$dir"
