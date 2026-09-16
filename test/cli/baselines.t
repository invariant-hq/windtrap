How a run accepts what it produced. The fixture's "greeting" test compares
against test/cli/greeting.expected under WINDTRAP_PROJECT_ROOT; the
sessions below leave it missing, accept it in place, make it stale, and
record the correction the way a dune action would.

  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_PROJECT_ROOT="$PWD" "$@"
  > }
  $ scrub() {
  >   sed -E 's/ in [0-9.e+-]+s\./ in DURATION./; s/suite_main\.ml:[0-9]+/suite_main.ml:LINE/'
  > }

A missing baseline fails with the proposed content and, for an executable
run by hand, the in-place acceptance: -u.

  $ run ./suite_main.exe -f greeting > out 2>&1
  [1]
  $ scrub < out
  fixture: 1 test
  ──────────────────── failures (1) ────────────────────
    FAIL  greeting
      test/cli/suite_main.ml:LINE
      expect_file "test/cli/greeting.expected": no baseline
      proposed (1 line):
        ┆ hello from the fixture
      accept: ./suite_main.exe -u, then review with git diff
  ──────────────────────────────────────────────────────
  
  1 failed in DURATION.
  $ test -e test/cli/greeting.expected || echo 'nothing written'
  nothing written

-u accepts in place: the check passes, the run names what it accepted,
and the file holds the produced text.

  $ run ./suite_main.exe -f greeting -u | scrub
  fixture: 1 passed in DURATION.
  accepted test/cli/greeting.expected
  $ cat test/cli/greeting.expected
  hello from the fixture
  $ run ./suite_main.exe -f greeting | scrub
  fixture: 1 passed in DURATION.

--corrected is what a dune action passes: the mismatch is reported with
its diff and dune's acceptance, the correction lands beside the file as
<file>.corrected, and a run whose only failures are recorded corrections
exits 0 so that the action's diff? is the verdict.

  $ echo 'stale' > test/cli/greeting.expected
  $ run ./suite_main.exe -f greeting --corrected > out 2>&1
  $ scrub < out
  fixture: 1 test
  ──────────────────── failures (1) ────────────────────
    FAIL  greeting
      test/cli/suite_main.ml:LINE
      expect_file "test/cli/greeting.expected": mismatch
      @@ -1,1 +1,1 @@
      - stale
      + hello from the fixture
      accept: dune promote
  ──────────────────────────────────────────────────────
  
  1 failed in DURATION.
  wrote test/cli/greeting.expected.corrected
  $ cat test/cli/greeting.expected.corrected
  hello from the fixture
  $ cat test/cli/greeting.expected
  stale
  $ rm test/cli/greeting.expected.corrected

Any other failure still fails the run — the correction beside it is
still written, since it belongs to a test that is otherwise clean, but
the exit code is 1 and dune will not reach its diff? step.

  $ run ./suite_main.exe -e math --corrected > out 2>&1
  [1]
  $ grep -c 'FAIL' out
  2
  $ cat test/cli/greeting.expected.corrected
  hello from the fixture
  $ rm test/cli/greeting.expected.corrected

In-place acceptance is a developer's edit: refused under CI, before
anything runs.

  $ run env CI=1 ./suite_main.exe -f greeting -u
  baseline update refused: CI is set. -u rewrites baselines in place, which is a developer's edit; under CI run with --corrected and accept with dune promote.
  [1]
  $ cat test/cli/greeting.expected
  stale

The two acceptances contradict each other, so asking for both is a
usage error.

  $ run ./suite_main.exe -u --corrected
  options '-u' and '--corrected' cannot be combined
  usage: suite_main.exe [OPTIONS] [PATTERN]
  [2]
