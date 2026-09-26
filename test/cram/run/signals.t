A signal that stops a run: INT, TERM and HUP. POSIX only, so the session
is not run on Windows, where the runner handles no signal. A signal is
sent by send_signal.exe, which starts the suite, signals it once the suite
says it is ready and prints how it ended; a process dies by the signal it
got, so whatever started it sees the signal and not a code.

  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_PROJECT_ROOT="$PWD" TMPDIR="$PWD/tmp" \
  >       WINDTRAP_OUTPUT="$PWD/_logs" "$@"
  > }
  $ scrub() {
  >   sed -E 's/ in [0-9.]+m?s\./ in DURATION./'
  > }
  $ mkdir tmp

INT, TERM and HUP end a run on what it knows. The third test of this
suite says it is ready and waits. The failure before it is already on
standard output under its rule, the rule closes, and the summary, the
last line, counts what did not run; standard error is one line naming the
stopped test. The stopped test's bytes stay in its log, and its temporary
directory is removed.

  $ stopped() {
  >   rm -rf logs
  >   run FACADE_FIXTURE=waiting ./send_signal.exe "$1" ./suite_main.exe -o logs
  >   grep -e '─' -e FAIL out
  >   tail -1 out | scrub
  >   cat err
  >   grep -q 'captured, never shown' out || echo 'not in the transcript'
  >   cat logs/fixture/deep/waits.output
  >   ls tmp
  > }
  $ stopped INT
  killed by SIGINT
  ──────────────────────── failures ────────────────────────
    FAIL  fails
  ──────────────────────────────────────────────────────────
  1 passed, 1 failed, 2 not run in DURATION.
  windtrap: interrupted in deep › waits
  not in the transcript
  captured, never shown
  $ stopped TERM
  killed by SIGTERM
  ──────────────────────── failures ────────────────────────
    FAIL  fails
  ──────────────────────────────────────────────────────────
  1 passed, 1 failed, 2 not run in DURATION.
  windtrap: interrupted in deep › waits
  not in the transcript
  captured, never shown
  $ stopped HUP
  killed by SIGHUP
  ──────────────────────── failures ────────────────────────
    FAIL  fails
  ──────────────────────────────────────────────────────────
  1 passed, 1 failed, 2 not run in DURATION.
  windtrap: interrupted in deep › waits
  not in the transcript
  captured, never shown

A signal the process was started ignoring stays ignored: the run goes on
after HUP, and the TERM after it stops the run as above.

  $ run FACADE_FIXTURE=waiting ./send_signal.exe --ignore HUP HUP,TERM ./suite_main.exe
  running after SIGHUP
  killed by SIGTERM
  $ cat err
  windtrap: interrupted in deep › waits

A signal between two tests acts before the next one starts. The process
sends it to itself after the first test, from an observer of the run:

  $ run FACADE_FIXTURE=between ./send_signal.exe - ./suite_main.exe
  killed by SIGTERM
  $ scrub < out
  fixture: 1 passed, 2 not run in DURATION.
  $ cat err
  windtrap: interrupted between tests

A signal skips the stopped test's teardown and writes no correction and
no record of the last failed tests, while a fixture's release still sees
what the stopped test set with setenv:

  $ run FACADE_FIXTURE=leaving ./send_signal.exe TERM ./suite_main.exe --corrected -o logs
  killed by SIGTERM
  $ test ! -e 'teardown ran' && test ! -e c.expected.corrected && test ! -e logs/fixture/.last-failed && echo 'none of them'
  none of them
  $ cat 'env at release'
  set by the test

A signal in a fixture's release comes after every test finished. It names
the fixture, and the fixtures still held are released:

  $ run FACADE_FIXTURE=releasing ./send_signal.exe TERM ./suite_main.exe
  killed by SIGTERM
  $ scrub < out
  fixture: 1 passed in DURATION.
  $ sed -E 's/suite_main\.ml:[0-9]+/suite_main.ml:LINE/' err
  windtrap: interrupted while releasing fixture (test/cram/run/suite_main.ml:LINE)
  $ ls 'first released' 'second released'
  first released
  second released

A second signal, while the first one's releases run, kills at once:

  $ run FACADE_FIXTURE=lingering ./send_signal.exe TERM,INT@releasing ./suite_main.exe
  running after SIGTERM
  killed by SIGINT

A process a test forks inherits the runner's handlers, and a signal
still kills it; the run goes on:

  $ run FACADE_FIXTURE=forking ./suite_main.exe | scrub
  fixture: 1 passed in DURATION.

A run puts back the handlers it found:

  $ run FACADE_FIXTURE=handlers ./suite_main.exe | scrub
  fixture: 1 passed in DURATION.
  handlers: kept kept kept

An interrupted run writes no JUnit file: the process dies by the signal
once its transcript has ended, and the file --junit names is never
created.

  $ run FACADE_FIXTURE=waiting ./send_signal.exe INT ./suite_main.exe --junit report.xml
  killed by SIGINT
  $ test -e report.xml || echo 'no report'
  no report
