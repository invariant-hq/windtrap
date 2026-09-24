What a run does about the process it runs in: a call to exit in the code
under test.

  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_PROJECT_ROOT="$PWD" \
  >       WINDTRAP_OUTPUT="$PWD/_logs" "$@"
  > }
  $ scrub() {
  >   sed -E 's/ in [0-9.]+m?s\./ in DURATION./; s/suite_main\.ml:[0-9]+/suite_main.ml:LINE/'
  > }

A call to exit in code under test does not end the run: it is the
failure of its test, the test after it still runs, and the process ends
through the run's own exit code.

  $ run FACADE_FIXTURE=exits ./suite_main.exe > out 2> err
  [1]
  $ scrub < out
  fixture: 3 tests
  ──────────────────────── failures ────────────────────────
    FAIL  bomb
      test/cli/suite_main.ml:LINE
      the test called exit and was intercepted; a test must return or raise, never exit the process
  
    FAIL  after
      test/cli/suite_main.ml:LINE
      deliberate
      expected  1
      actual    2
  ──────────────────────────────────────────────────────────
  
  1 passed, 2 failed in DURATION.
  $ cat err
