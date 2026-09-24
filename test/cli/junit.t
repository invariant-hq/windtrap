--junit in both its forms, and both ways writing it can fail. The
document itself is Report_junit's and is pinned in test_report_junit.ml;
what these sessions own is where the run puts it, and that a report
it cannot write is a warning rather than a verdict.

  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_PROJECT_ROOT="$PWD" \
  >       WINDTRAP_OUTPUT="$PWD/_logs" "$@"
  > }

A value naming an .xml file stays exactly that:

  $ run ./suite_main.exe -f math --junit report.xml > out 2> err
  $ cat err
  $ grep -c 'name="fixture"' report.xml
  1

Anything else is a directory, and each suite writes its own report into
it for CI to glob. The directory is created if it is not there:

  $ run ./suite_main.exe -f math --junit reports > out 2> err
  $ cat err
  $ ls reports
  fixture.xml
  $ grep -c 'name="fixture"' reports/fixture.xml
  1

A report the run cannot write is a side product that went missing,
not a failed run: the warning goes to stderr and the exit code is still
the tests'. Here the .xml's parent is a regular file, so the atomic
write fails:

  $ echo 'not a directory' > blocked
  $ run ./suite_main.exe -f math --junit blocked/r.xml > out 2> err
  $ sed -E 's/ in [0-9.]+m?s\./ in DURATION./' out
  fixture: 2 passed in DURATION.
  $ sed -E 's/(report to [^:]+): .*/\1: REASON/' err
  windtrap: warning: could not write JUnit report to blocked/r.xml: REASON

And here the directory form cannot make its directory, over the same
file. The warning has the one form, the reason naming the directory it
could not make. The run underneath fails, and its exit code comes
through unchanged:

  $ run ./suite_main.exe -f boom --junit blocked/out > out 2> err
  [1]
  $ sed -E 's/(report to [^:]+): .*/\1: REASON/' err
  windtrap: warning: could not write JUnit report to blocked/out/fixture.xml: REASON

Every run that names the same .xml writes the same file, and the last
one wins: the second run's report replaces the first's whole, rather
than adding its tests to it.

  $ run ./suite_main.exe -f math --junit shared.xml > out 2> err
  $ run ./suite_main.exe -f boom --junit shared.xml > out 2> err
  [1]
  $ grep -o '<testcase name="[^"]*"' shared.xml
  <testcase name="boom"

When a run also has a file it could not write, the transcript says so
in its corrections section, and the one warning on standard error is
the JUnit report's. Here the correction's path is taken by a directory,
and the report's parent is the regular file from above. The exit code
is the correction's: a correction the run could not write fails it, the
report does not.

  $ mkdir -p test/cli/greeting.expected.corrected
  $ run ./suite_main.exe -f greeting --corrected --junit blocked/r.xml > out 2> err
  [1]
  $ grep -A1 '^corrections' out | sed -E 's/(could not write [^:]+): .*/\1: REASON/'
  corrections (1):
    could not write test/cli/greeting.expected.corrected: REASON
  $ sed -E 's/(could not write [^:]+): .*/\1: REASON/' err
  windtrap: warning: could not write JUnit report to blocked/r.xml: REASON
