How a run accepts what it produced. The fixture's "greeting" test compares
against test/cli/greeting.expected under WINDTRAP_PROJECT_ROOT; the
sessions below leave it missing, accept it in place, make it stale, and
record the correction the way a dune action would.

  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_PROJECT_ROOT="$PWD" "$@"
  > }
  $ scrub() {
  >   sed -E 's/ in [0-9.]+m?s\./ in DURATION./; s/suite_main\.ml:[0-9]+/suite_main.ml:LINE/'
  > }

A missing baseline fails with the proposed content and, for an executable
run by hand, the in-place acceptance: -u, narrowed to the block's test.

  $ run ./suite_main.exe -f greeting > out 2>&1
  [1]
  $ scrub < out
  fixture: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  greeting
      test/cli/suite_main.ml:LINE
      expect_file "test/cli/greeting.expected": no baseline
      proposed (1 line):
        + hello from the fixture
      accept: ./suite_main.exe -u -f 'greeting'
  ──────────────────────────────────────────────────────────
  
  1 failed in DURATION.
  $ test -e test/cli/greeting.expected || echo 'nothing written'
  nothing written

-u accepts in place: the check passes, the run names what it accepted,
and the file holds the produced text.

  $ run ./suite_main.exe -f greeting -u | scrub
  fixture: 1 test
  corrections (1):
    accepted test/cli/greeting.expected
  
  1 passed, 1 correction accepted in DURATION.
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
  ──────────────────────── failures ────────────────────────
    FAIL  greeting
      test/cli/suite_main.ml:LINE
      expect_file "test/cli/greeting.expected": mismatch
      @@ -1,1 +1,1 @@
      - stale
      + hello from the fixture
      accept: dune promote test/cli/greeting.expected
  ──────────────────────────────────────────────────────────
  
  corrections (1):
    wrote test/cli/greeting.expected.corrected
  
  1 failed, 1 correction written in DURATION.
  $ cat test/cli/greeting.expected.corrected
  hello from the fixture
  $ cat test/cli/greeting.expected
  stale
  $ rm test/cli/greeting.expected.corrected

Any other failure still fails the run. The correction beside it is still
written, since it belongs to a test that is otherwise clean, and its
block still offers the promotion: a block is printed when its test ends,
before the run knows its exit code. That code is 1, so dune will not
reach its diff? step and will promote nothing: the run says so after its
summary, once, on standard error.

  $ run ./suite_main.exe -e math --corrected > out 2>&1
  [1]
  $ scrub < out
  fixture: 3 tests
  ──────────────────────── failures ────────────────────────
    FAIL  boom
      test/cli/suite_main.ml:LINE
      deliberate
      expected  1
      actual    2
  
    FAIL  greeting
      test/cli/suite_main.ml:LINE
      expect_file "test/cli/greeting.expected": mismatch
      @@ -1,1 +1,1 @@
      - stale
      + hello from the fixture
      accept: dune promote test/cli/greeting.expected
  ──────────────────────────────────────────────────────────
  
  corrections (1):
    wrote test/cli/greeting.expected.corrected
  
  1 passed, 2 failed, 1 correction written in DURATION.
  $ cat test/cli/greeting.expected.corrected
  hello from the fixture
  $ rm test/cli/greeting.expected.corrected

A stale baseline in a test that also fails another way is never
corrected: the output was produced beside a failure. The block then
offers no acceptance, which would promote or rewrite nothing, and ends
on the sentence that says why. The same holds by hand, under --corrected
and under -u, where the stale baseline is no failure and is left alone.

  $ mkdir -p test/cli && echo 'stale' > test/cli/masked.expected
  $ masked() {
  >   run env FACADE_FIXTURE=masked ./suite_main.exe "$@" > out 2>&1
  >   echo "[$?]"
  >   scrub < out
  > }
  $ masked
  [1]
  fixture: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  masked
      test/cli/suite_main.ml:LINE
      expect_file "test/cli/masked.expected": mismatch
      @@ -1,1 +1,1 @@
      - stale
      + fresh from the fixture
  
      test/cli/suite_main.ml:LINE
      deliberate
      expected  1
      actual    2
      no correction was kept: the test also failed outside its expectations; fix that failure and rerun
  ──────────────────────────────────────────────────────────
  
  1 failed in DURATION.
  $ masked --corrected
  [1]
  fixture: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  masked
      test/cli/suite_main.ml:LINE
      expect_file "test/cli/masked.expected": mismatch
      @@ -1,1 +1,1 @@
      - stale
      + fresh from the fixture
  
      test/cli/suite_main.ml:LINE
      deliberate
      expected  1
      actual    2
      no correction was kept: the test also failed outside its expectations; fix that failure and rerun
  ──────────────────────────────────────────────────────────
  
  1 failed in DURATION.
  $ test -e test/cli/masked.expected.corrected || echo 'nothing to promote'
  nothing to promote
  $ masked -u
  [1]
  fixture: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  masked
      test/cli/suite_main.ml:LINE
      deliberate
      expected  1
      actual    2
  ──────────────────────────────────────────────────────────
  
  1 failed in DURATION.
  $ cat test/cli/masked.expected
  stale

In-place acceptance is a developer's edit: refused under CI, before
anything runs.

  $ run env CI=1 ./suite_main.exe -f greeting -u
  windtrap: baseline update refused: CI is set. -u rewrites baselines in place, which is a developer's edit; under CI run with --corrected and accept with dune promote.
  [1]
  $ cat test/cli/greeting.expected
  stale

The two acceptances contradict each other, so asking for both is a
usage error.

  $ run ./suite_main.exe -u --corrected
  windtrap: options '-u' and '--corrected' cannot be combined
  usage: suite_main.exe [OPTIONS] [PATTERN]
  [2]
