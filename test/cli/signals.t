A signal that stops a run: INT, TERM and HUP. POSIX only, so the session
is not run on Windows, where the runner handles no signal.

  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_PROJECT_ROOT="$PWD" \
  >       WINDTRAP_OUTPUT="$PWD/_logs" "$@"
  > }
  $ scrub() {
  >   sed -E 's/ in [0-9.]+m?s\./ in DURATION./; s/suite_main\.ml:[0-9]+/suite_main.ml:LINE/'
  > }

INT, TERM and HUP end a run on what it knows. The third test of this
suite says it is ready and waits; send_signal.exe starts the suite,
signals it once it is ready, and says how it ended. The failure before
it is already on stdout under its rule, the rule still closes, and the
summary counts what did not run; stderr is one line naming the stopped
test; the stopped test's bytes stay in its log; and the process dies by
the signal it got, so whatever started it sees the signal and not a
code.

  $ run FACADE_FIXTURE=waiting ./send_signal.exe INT ./suite_main.exe -o logs
  killed by SIGINT
  $ scrub < out
  fixture: 4 tests
  ──────────────────────── failures ────────────────────────
    FAIL  fails
      test/cli/suite_main.ml:LINE
      deliberate
      expected  1
      actual    2
  ──────────────────────────────────────────────────────────
  
  1 passed, 1 failed, 2 not run in DURATION.
  $ cat err
  windtrap: interrupted in deep › waits
  $ cat logs/fixture/deep/waits.output
  captured, never shown
  $ run FACADE_FIXTURE=waiting ./send_signal.exe TERM ./suite_main.exe
  killed by SIGTERM
  $ tail -1 out | scrub
  1 passed, 1 failed, 2 not run in DURATION.
  $ cat err
  windtrap: interrupted in deep › waits
  $ run FACADE_FIXTURE=waiting ./send_signal.exe HUP ./suite_main.exe
  killed by SIGHUP
  $ tail -1 out | scrub
  1 passed, 1 failed, 2 not run in DURATION.
  $ cat err
  windtrap: interrupted in deep › waits

An interrupted run writes no JUnit file: the process dies by the signal
once its transcript has ended, and the file --junit names is never
created.

  $ run FACADE_FIXTURE=waiting ./send_signal.exe INT ./suite_main.exe --junit report.xml
  killed by SIGINT
  $ test -e report.xml || echo 'no report'
  no report
