What the report shows beyond a failure, and where a run keeps its logs.

  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_PROJECT_ROOT="$PWD" \
  >       WINDTRAP_OUTPUT="$PWD/_logs" "$@"
  > }
  $ scrub() {
  >   sed -E 's/ in [0-9.]+m?s\./ in DURATION./; s/suite_main\.ml:[0-9]+/suite_main.ml:LINE/'
  > }

A test that fails and then passes on a retry is never silent: the run
exits 0 and counts it as passed, but the header comes out, the flaky
section names the test with the attempt it passed on, and the summary
says how many of the passes were flaky.

  $ run FACADE_FIXTURE=flaky ./suite_main.exe > out 2>&1
  $ scrub < out
  fixture: 1 test
  flaky tests (1):
    passed on attempt 2  flaky
  
  1 passed (1 flaky) in DURATION.

Verbose already carries the attempt count on the status line, and keeps
the section:

  $ run FACADE_FIXTURE=flaky ./suite_main.exe -v > out 2>&1
  $ scrub < out | sed -E 's/  +[0-9.]+m?s /  TIME /'
  fixture: 1 test
    PASS  flaky  TIME (2 attempts)
  
  flaky tests (1):
    passed on attempt 2  flaky
  
  1 passed (1 flaky) in DURATION.

Under -v a failed test's status line is its block's title: the block
prints under it as soon as the test finishes and closes on a blank line,
the rows of the tests after it follow, and no failures section repeats it
at the end.

  $ run ./suite_main.exe -v -e greeting > out 2>&1
  [1]
  $ scrub < out | sed -E 's/  +[0-9.]+m?s$/  TIME/'
  fixture: 4 tests
    PASS  math › adds  TIME
    PASS  math › subtracts  TIME
    FAIL  boom  TIME
      test/cli/suite_main.ml:LINE
      deliberate
      expected  1
      actual    2
  
    PASS  crawls  TIME
  3 passed, 1 failed in DURATION.

--stream captures nothing and changes nothing else: each test's own bytes
pass through, whichever way they leave the process (the stdout channel
unflushed, descriptor 1, a subprocess), a failed test's block follows
its bytes, and the transcript keeps the compact shape. A green streamed
run is therefore its tests' bytes and the one line.

  $ run FACADE_FIXTURE=stream ./suite_main.exe --stream > out 2> err
  [1]
  $ scrub < out | sed -E 's/  +[0-9.]+m?s$/  TIME/'
  through the stdout channel
  through descriptor 1
  through a subprocess
  before the failure
  fixture: 4 tests
  ──────────────────────── failures ────────────────────────
    FAIL  fails
      test/cli/suite_main.ml:LINE
      deliberate
      expected  1
      actual    2
  ──────────────────────────────────────────────────────────
  
  3 passed, 1 failed in DURATION.
  $ cat err
  $ run FACADE_FIXTURE=stream ./suite_main.exe -s -e fails > out 2>&1
  $ scrub < out
  through the stdout channel
  through descriptor 1
  through a subprocess
  fixture: 3 passed in DURATION.

Under GitHub Actions the transcript folds, the fold closes against the
last section, the annotations follow the close so that they are never
folded away, and the summary is still the last line. An annotation is
titled by the test's path and carries the block's lines below its title.

  $ run CI=true GITHUB_ACTIONS=true ./suite_main.exe -f boom > out 2>&1
  [1]
  $ scrub < out | sed -E 's/line=[0-9]+/line=LINE/'
  ::group::fixture
  fixture: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  boom
      test/cli/suite_main.ml:LINE
      deliberate
      expected  1
      actual    2
  ──────────────────────────────────────────────────────────
  ::endgroup::
  ::error file=test/cli/suite_main.ml,line=LINE,title=Test failure%3A boom::    test/cli/suite_main.ml:LINE%0A    deliberate%0A    expected  1%0A    actual    2
  
  1 failed in DURATION.

JUnit has no state for it, so the testcase says so in its system-out:

  $ run FACADE_FIXTURE=flaky ./suite_main.exe --junit report.xml > /dev/null
  $ grep -A1 'name="flaky"' report.xml | sed -E 's/time="[0-9.]+"/time="TIME"/'
      <testcase name="flaky" classname="fixture" time="TIME">
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
  >   TMPDIR="$dir/tmp" FACADE_FIXTURE=noisy \
  >   "$dir/suite.exe" > out 2>&1
  [1]
  $ scrub < out | sed "s#$dir#<tmp>#g"
  fixture: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  noisy
      test/cli/suite_main.ml:LINE
      deliberate
      expected  1
      actual    2
      captured output (1 line):
        hello from noisy
      full log: <tmp>/tmp/windtrap/fixture/noisy.output
  ──────────────────────────────────────────────────────────
  
  1 failed in DURATION.
  $ cat "$dir/tmp/windtrap/fixture/noisy.output"
  hello from noisy
  $ test -e "$dir/_build" || echo 'no _build grown'
  no _build grown
  $ rm -rf "$dir"

A failing property's block ends on the replay line, the command that
reruns its case, spelled for the way the run was started. Run by hand,
it restates the program as it was typed, then the seed and the test:

  $ run FACADE_FIXTURE=property ./suite_main.exe > out 2> err
  [1]
  $ scrub < out | sed -E 's/s1:[0-9a-f]+/SEED/'
  fixture: 1 test (seed SEED)
  ──────────────────────── failures ────────────────────────
    FAIL  boom
      test/cli/suite_main.ml:LINE
      counterexample (case 0, shrunk 1 step): 0
      which failed with:
        expected  1
        actual    2
      replay: ./suite_main.exe --seed SEED -f 'boom'
  ──────────────────────────────────────────────────────────
  
  1 failed in DURATION.
  $ cat err

Run by dune, the program is dune's test action (INSIDE_DUNE set, the
path relative to the action's directory): the line is a dune exec of the
program's path from the project root, with no ./ left in it.

  $ run INSIDE_DUNE=1 FACADE_FIXTURE=property ./suite_main.exe > out 2> err
  [1]
  $ grep 'replay:' out | sed -E 's/s1:[0-9a-f]+/SEED/'
      replay: dune exec suite_main.exe -- --seed SEED -f 'boom'

A host that passes run no command line gives no program to restate, so
the line spells the run through the mirrors:

  $ run FACADE_FIXTURE=no-argv ./suite_main.exe > out 2> err
  [1]
  $ grep 'replay:' out | sed -E 's/s1:[0-9a-f]+/SEED/'
      replay: WINDTRAP_SEED=SEED WINDTRAP_FILTER='boom' dune runtest

An expected failure is the run's record of the test, never a reading of
its message: a test expected to fail, whose own failure is the sentence
the runner writes for an unexpected pass, is still an expected failure.
The run exits 0 on one line, and under -v its line says XFAIL.

  $ run FACADE_FIXTURE=collide ./suite_main.exe > out 2> err
  $ scrub < out
  fixture: 1 expected failure in DURATION.
  $ run FACADE_FIXTURE=collide ./suite_main.exe -v > out 2> err
  $ scrub < out | sed -E 's/  +[0-9.]+m?s$/  TIME/'
  fixture: 1 test
    XFAIL  collide (expected failure)  TIME
  1 expected failure in DURATION.

A fixture is released after the last test, and a release that raises is
a failure row of its own, named after the fixture's site. The one test
passed, the run fails, and every sink says why: the transcript and the
JUnit report alike, rather than an exit code nothing explains.

  $ run FACADE_FIXTURE=release ./suite_main.exe --junit release.xml > out 2> err
  [1]
  $ scrub < out
  fixture: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  fixture release
      [release] test/cli/suite_main.ml:LINE
      fixture (test/cli/suite_main.ml:LINE): release raised Failure("release-boom")
  ──────────────────────────────────────────────────────────
  
  1 passed, 1 failed in DURATION.
  $ cat err
  $ sed -E 's/time="[0-9.]+"/time="TIME"/g; s/suite_main\.ml:[0-9]+/suite_main.ml:LINE/g' release.xml
  <?xml version="1.0" encoding="UTF-8"?>
  <testsuites name="windtrap" tests="2" failures="1" errors="0" skipped="0" time="TIME">
    <testsuite name="fixture" tests="2" failures="1" errors="0" skipped="0" time="TIME">
      <testcase name="touches the fixture" classname="fixture" time="TIME"/>
      <testcase name="fixture release" classname="fixture" time="TIME">
        <failure message="fixture (test/cli/suite_main.ml:LINE): release raised Failure(&quot;release-boom&quot;)">    [release] test/cli/suite_main.ml:LINE
      fixture (test/cli/suite_main.ml:LINE): release raised Failure("release-boom")
  </failure>
      </testcase>
    </testsuite>
  </testsuites>

A release runs while its run is still executing, so a release that
starts a run of its own is refused as a test body's would be: run
raises, and the raise is the release's failure.

  $ run FACADE_FIXTURE=nested ./suite_main.exe > out 2> err
  [1]
  $ scrub < out
  fixture: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  fixture release
      [release] test/cli/suite_main.ml:LINE
      fixture (test/cli/suite_main.ml:LINE): release raised Invalid_argument("windtrap: a run is already executing; nothing inside it can start another run")
  ──────────────────────────────────────────────────────────
  
  1 passed, 1 failed in DURATION.
  $ cat err
