Runs refused before anything executes, a failed release, a report that
cannot be written, and a run interrupted by a signal: the code each
leaves and the stream each message takes. Each block is one situation
and the verdict the run gives it (see doc/dev/testing.md).

`run` states the child's whole environment and keeps its logs and its
baselines in this sandbox. It prints standard output, then standard
error under a `--- stderr` line, with durations, seeds and the
fixture's line numbers masked, and returns the child's code. `brief`
prints only the FAIL lines, the last line of standard output and the
first of standard error. A block whose point is a code of 0 echoes it.

  $ scrub() {
  >   sed -E 's/ in [0-9.]+m?s([. ])/ in DURATION\1/; s/s1:[0-9a-f]{16}/SEED/g; s/(verdict_main\.ml):[0-9]+/\1:N/g; s/^( *)[0-9]+ │/\1N │/'
  > }
  $ run() {
  >   env -i PATH="$PATH" TMPDIR="${TMPDIR:-/tmp}" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_PROJECT_ROOT="$PWD" WINDTRAP_OUTPUT="$PWD/_logs" "$@" > out 2> err
  >   code=$?
  >   scrub < out
  >   if [ -s err ]; then echo '--- stderr'; scrub < err; fi
  >   return $code
  > }
  $ brief() {
  >   run "$@" > /dev/null
  >   code=$?
  >   grep '^  FAIL  ' out
  >   tail -1 out | scrub
  >   if [ -s err ]; then echo '--- stderr'; head -1 err | scrub; fi
  >   return $code
  > }

A suite with two tests of one path is refused with 1 before any test
runs, under -l too:

  $ run VERDICT_SUITE=dup_paths ./verdict_main.exe
  --- stderr
  windtrap: duplicate test paths:
    passes
  Every full test path must be unique.
  [1]
  $ run VERDICT_SUITE=dup_paths ./verdict_main.exe -l > /dev/null
  [1]

A focus under CI is refused with 1, under -l too:

  $ run CI=true VERDICT_SUITE=focus ./verdict_main.exe
  --- stderr
  windtrap: focused tests committed (focus at test/cram/verdicts/verdict_main.ml:N); remove focus to run under CI
  [1]
  $ run CI=true VERDICT_SUITE=focus ./verdict_main.exe -l > /dev/null
  [1]

A fixture's teardown that raises fails the run with 1:

  $ brief VERDICT_SUITE=release_fail ./verdict_main.exe
    FAIL  fixture release
  1 passed, 1 failed in DURATION.
  [1]

A JUnit report that cannot be written warns on standard error and
changes no code, given by flag or by mirror, on a passing and on a
failing run:

  $ run VERDICT_SUITE=junit ./verdict_main.exe --junit /nonexistent/dir/x.xml; echo "exit $?"
  junit: 1 passed in DURATION.
  --- stderr
  windtrap: warning: could not write JUnit report to /nonexistent/dir/x.xml: cannot create temporary file: No such file or directory
  exit 0
  $ run VERDICT_SUITE=junit WINDTRAP_JUNIT=/nonexistent/dir/x.xml ./verdict_main.exe; echo "exit $?"
  junit: 1 passed in DURATION.
  --- stderr
  windtrap: warning: could not write JUnit report to /nonexistent/dir/x.xml: cannot create temporary file: No such file or directory
  exit 0
  $ brief VERDICT_SUITE=junit_fail ./verdict_main.exe --junit /nonexistent/dir/x.xml
    FAIL  fails
  1 passed, 1 failed in DURATION.
  --- stderr
  windtrap: warning: could not write JUnit report to /nonexistent/dir/x.xml: cannot create temporary file: No such file or directory
  [1]
  $ brief VERDICT_SUITE=junit_fail WINDTRAP_JUNIT=/nonexistent/dir/x.xml ./verdict_main.exe
    FAIL  fails
  1 passed, 1 failed in DURATION.
  --- stderr
  windtrap: warning: could not write JUnit report to /nonexistent/dir/x.xml: cannot create temporary file: No such file or directory
  [1]

A test that sends its own process SIGINT after an earlier failure stops
the run: the summary counts the tests not run, and the process dies by
the signal, a shell's 130:

  $ brief VERDICT_SUITE=sigint ./verdict_main.exe
    FAIL  fails
  1 failed, 2 not run in DURATION.
  --- stderr
  windtrap: interrupted in interrupts itself
  [130]
