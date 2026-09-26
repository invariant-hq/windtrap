How a run accepts what it produced. The fixture's "greeting" test compares
against test/cram/run/greeting.expected under WINDTRAP_PROJECT_ROOT; the
blocks below leave it missing, accept it in place, make it stale, and
record the correction the way a dune action would. They are one
sequence, down to the -u run that ignores other failures: each starts
from the file the block before it left. The blocks after that plant
what they read.

  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_PROJECT_ROOT="$PWD" \
  >       WINDTRAP_OUTPUT="$PWD/_logs" "$@"
  > }
  $ scrub() {
  >   sed -E 's/ in [0-9.]+m?s\./ in DURATION./; s/suite_main\.ml:[0-9]+/suite_main.ml:LINE/'
  > }

A missing baseline fails with the proposed content. An executable run by
hand closes its report on the in-place acceptance, right above the
summary: -u, over the run's selection.

  $ run ./suite_main.exe -f greeting > out 2>&1
  [1]
  $ scrub < out
  fixture: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  greeting
      test/cram/run/suite_main.ml:LINE
      expect_file "test/cram/run/greeting.expected": no baseline
      proposed (1 line):
        + hello from the fixture
  ──────────────────────────────────────────────────────────
  
  accept: ./suite_main.exe -u -f 'greeting'
  1 failed in DURATION.
  $ test -e test/cram/run/greeting.expected || echo 'nothing written'
  nothing written

-u accepts in place: the check passes, the run names what it accepted,
and the file holds the produced text.

  $ run ./suite_main.exe -f greeting -u > out 2>&1
  $ scrub < out
  fixture: 1 test
  corrections (1):
    accepted test/cram/run/greeting.expected
  
  1 passed, 1 correction accepted in DURATION.
  $ cat test/cram/run/greeting.expected
  hello from the fixture
  $ run ./suite_main.exe -f greeting > out 2>&1
  $ scrub < out
  fixture: 1 passed in DURATION.

--corrected is what a dune action passes: the mismatch is reported with
its diff and dune's acceptance, the correction lands beside the file as
<file>.corrected, and a run whose only failures are recorded corrections
exits 0 so that the action's diff? is the verdict.

  $ echo 'stale' > test/cram/run/greeting.expected
  $ run ./suite_main.exe -f greeting --corrected > out 2>&1
  $ scrub < out
  fixture: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  greeting
      test/cram/run/suite_main.ml:LINE
      expect_file "test/cram/run/greeting.expected": mismatch
      @@ -1,1 +1,1 @@
      - stale
      + hello from the fixture
      accept: dune promote test/cram/run/greeting.expected
  ──────────────────────────────────────────────────────────
  
  corrections (1):
    wrote test/cram/run/greeting.expected.corrected
  
  1 failed, 1 correction written in DURATION.
  $ cat test/cram/run/greeting.expected.corrected
  hello from the fixture
  $ cat test/cram/run/greeting.expected
  stale
  $ rm test/cram/run/greeting.expected.corrected

Any other failure still fails the run. The correction beside it is still
written, since it belongs to a test that is otherwise clean, and its
block still offers the promotion: a block is printed when its test ends,
before the run knows its exit code. That code is 1, so dune will not
reach its diff? step and will promote nothing: the run says so after its
summary, once, on standard error.

  $ run ./suite_main.exe -e math --corrected > out 2> err
  [1]
  $ scrub < out
  fixture: 3 tests
  ──────────────────────── failures ────────────────────────
    FAIL  boom
      test/cram/run/suite_main.ml:LINE
      deliberate
      expected  1
      actual    2
  
    FAIL  greeting
      test/cram/run/suite_main.ml:LINE
      expect_file "test/cram/run/greeting.expected": mismatch
      @@ -1,1 +1,1 @@
      - stale
      + hello from the fixture
      accept: dune promote test/cram/run/greeting.expected
  ──────────────────────────────────────────────────────────
  
  corrections (1):
    wrote test/cram/run/greeting.expected.corrected
  
  1 passed, 2 failed, 1 correction written in DURATION.
  $ cat err
  windtrap: warning: dune registers a correction for promotion only when the run that wrote it exits 0, so the failures above withhold the correction written here. Fix the failures, rerun, then 'dune promote'.
  $ cat test/cram/run/greeting.expected.corrected
  hello from the fixture
  $ rm test/cram/run/greeting.expected.corrected

The warning is for that run alone. A --corrected run whose corrections
dune will promote (the session above, exit 0) printed none; nor does -u,
which accepts in place whatever else fails, nor plain checking, which
writes nothing.

  $ run ./suite_main.exe -e math -u > out 2> err
  [1]
  $ tail -1 out | scrub
  2 passed, 1 failed, 1 correction accepted in DURATION.
  $ cat err
  $ echo 'stale' > test/cram/run/greeting.expected
  $ run ./suite_main.exe -e math > out 2> err
  [1]
  $ tail -1 out | scrub
  1 passed, 2 failed in DURATION.
  $ cat err

A test declared with ~retries whose only failure is a stale baseline runs
once under --corrected: the correction its first attempt recorded is what
a second attempt would be compared with, so a retry could only pass, and
the report would call a deterministic test flaky. The run is the one the
test has without ~retries. The three runs of the fixture below read the
one stale file planted here, in turn.

  $ echo 'stale' > test/cram/run/retried.expected
  $ retried() {
  >   run env FACADE_FIXTURE=retried ./suite_main.exe "$@" > out 2>&1
  >   echo "[$?]"
  >   scrub < out
  > }
  $ retried --corrected
  [0]
  fixture: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  retried
      test/cram/run/suite_main.ml:LINE
      expect_file "test/cram/run/retried.expected": mismatch
      @@ -1,1 +1,1 @@
      - stale
      + fresh from the fixture
      accept: dune promote test/cram/run/retried.expected
  ──────────────────────────────────────────────────────────
  
  corrections (1):
    wrote test/cram/run/retried.expected.corrected
  
  1 failed, 1 correction written in DURATION.
  $ rm test/cram/run/retried.expected.corrected

Plain checking records nothing, so it retries as declared: an output that
differs from one attempt to the next is what ~retries is for.

  $ retried
  [1]
  fixture: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  retried (2 attempts)
      test/cram/run/suite_main.ml:LINE
      expect_file "test/cram/run/retried.expected": mismatch
      @@ -1,1 +1,1 @@
      - stale
      + fresh from the fixture
  ──────────────────────────────────────────────────────────
  
  accept: ./suite_main.exe -u
  1 failed in DURATION.

-u accepts on the first attempt, which passes.

  $ retried -u
  [0]
  fixture: 1 test
  corrections (1):
    accepted test/cram/run/retried.expected
  
  1 passed, 1 correction accepted in DURATION.
  $ cat test/cram/run/retried.expected
  fresh from the fixture

A kept correction leaves the exit code alone and nothing else: the test
it belongs to still failed. Under -x it stops the run, so the test after
it does not run, and the run still exits 0.

  $ echo 'stale' > test/cram/run/stops.expected
  $ run env FACADE_FIXTURE=stops ./suite_main.exe --corrected -x > out 2>&1
  $ scrub < out
  fixture: 2 tests
  ──────────────────────── failures ────────────────────────
    FAIL  stale
      test/cram/run/suite_main.ml:LINE
      expect_file "test/cram/run/stops.expected": mismatch
      @@ -1,1 +1,1 @@
      - stale
      + fresh from the fixture
      accept: dune promote test/cram/run/stops.expected
  ──────────────────────────────────────────────────────────
  
  corrections (1):
    wrote test/cram/run/stops.expected.corrected
  
  1 failed, 1 not run, 1 correction written in DURATION.
  $ rm test/cram/run/stops.expected.corrected

And it enters the record of the last failed tests, which --failed reads:
the next -l --failed lists the test whose correction was kept, not the
one that passed.

  $ run env FACADE_FIXTURE=stops ./suite_main.exe --corrected -o store > out 2>&1
  $ rm test/cram/run/stops.expected.corrected
  $ run env FACADE_FIXTURE=stops ./suite_main.exe -l --failed -o store
  stale

A stale baseline in a test that also fails another way is never
corrected: the output was produced beside a failure. The block then
offers no acceptance, which would promote or rewrite nothing, and ends
on the sentence that says why. The same holds by hand, under --corrected
and under -u, where the stale baseline is no failure and is left alone.

  $ mkdir -p test/cram/run && echo 'stale' > test/cram/run/masked.expected
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
      test/cram/run/suite_main.ml:LINE
      expect_file "test/cram/run/masked.expected": mismatch
      @@ -1,1 +1,1 @@
      - stale
      + fresh from the fixture
  
      test/cram/run/suite_main.ml:LINE
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
      test/cram/run/suite_main.ml:LINE
      expect_file "test/cram/run/masked.expected": mismatch
      @@ -1,1 +1,1 @@
      - stale
      + fresh from the fixture
  
      test/cram/run/suite_main.ml:LINE
      deliberate
      expected  1
      actual    2
      no correction was kept: the test also failed outside its expectations; fix that failure and rerun
  ──────────────────────────────────────────────────────────
  
  1 failed in DURATION.
  $ test -e test/cram/run/masked.expected.corrected || echo 'nothing to promote'
  nothing to promote
  $ masked -u
  [1]
  fixture: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  masked
      test/cram/run/suite_main.ml:LINE
      deliberate
      expected  1
      actual    2
  ──────────────────────────────────────────────────────────
  
  1 failed in DURATION.
  $ cat test/cram/run/masked.expected
  stale

One accept: line serves every block of a run by hand. The fixture holds
a literal that holds, a stale literal, a stale file baseline and a
failing property, which the replay: line under the accept: line reruns.
The source of the literals is the fixture's own, copied where their
locations point:

  $ rm -f test/cram/run/suite_main.ml && cp suite_main.ml test/cram/run/
  $ echo 'stale' > test/cram/run/accepts.expected
  $ run env FACADE_FIXTURE=accepts ./suite_main.exe > out 2>&1
  [1]
  $ grep -c 'accept:' out
  1
  $ tail -3 out | scrub | sed -E 's/s1:[0-9a-f]+/SEED/'
  accept: ./suite_main.exe -u
  replay: ./suite_main.exe --seed SEED
  1 passed, 3 failed in DURATION.

Pasted, the line rewrites the two stale baselines and nothing else: the
literal that holds is left as it is.

  $ eval "run env FACADE_FIXTURE=accepts $(sed -n 's/^accept: //p' out)" > again 2>&1
  [1]
  $ tail -1 again | scrub
  3 passed, 1 failed, 2 corrections accepted in DURATION.
  $ diff suite_main.ml test/cram/run/suite_main.ml | grep '^[<>]'
  <     test "stale literal" (fun () -> expect "fresh" @@ __POS_OF__ "stale");
  >     test "stale literal" (fun () -> expect "fresh" @@ __POS_OF__ "fresh");
  $ cat test/cram/run/accepts.expected
  fresh from the fixture

The line restates the run's selection, so a baseline the run did not
select is not accepted:

  $ rm -f test/cram/run/suite_main.ml && cp suite_main.ml test/cram/run/
  $ echo 'stale' > test/cram/run/accepts.expected
  $ run env FACADE_FIXTURE=accepts ./suite_main.exe -e file > out 2>&1
  [1]
  $ grep 'accept:' out
  accept: ./suite_main.exe -u -e 'file'
  $ eval "run env FACADE_FIXTURE=accepts $(sed -n 's/^accept: //p' out)" > again 2>&1
  [1]
  $ diff suite_main.ml test/cram/run/suite_main.ml | grep '^[<>]'
  <     test "stale literal" (fun () -> expect "fresh" @@ __POS_OF__ "stale");
  >     test "stale literal" (fun () -> expect "fresh" @@ __POS_OF__ "fresh");
  $ cat test/cram/run/accepts.expected
  stale

-x stops a run on its first failure, and -u passes the test it accepts,
so over the selection it would run on and accept what the report never
showed. A run stopped by -x accepts the test it stopped on, by name:

  $ rm -f test/cram/run/suite_main.ml && cp suite_main.ml test/cram/run/
  $ run env FACADE_FIXTURE=accepts ./suite_main.exe -x > out 2>&1
  [1]
  $ grep -e 'accept:' -e 'replay:' out
  accept: ./suite_main.exe -u -f 'stale literal'
  $ eval "run env FACADE_FIXTURE=accepts $(sed -n 's/^accept: //p' out)" > again 2>&1
  $ diff suite_main.ml test/cram/run/suite_main.ml | grep '^[<>]'
  <     test "stale literal" (fun () -> expect "fresh" @@ __POS_OF__ "stale");
  >     test "stale literal" (fun () -> expect "fresh" @@ __POS_OF__ "fresh");
  $ cat test/cram/run/accepts.expected
  stale
  $ rm test/cram/run/suite_main.ml

In-place acceptance is a developer's edit: refused under CI, before
anything runs, so a stale baseline stays as it was.

  $ echo 'stale' > test/cram/run/greeting.expected
  $ run env CI=1 ./suite_main.exe -f greeting -u > out 2> err
  [1]
  $ cat out
  $ cat err
  windtrap: baseline update refused: CI is set. -u rewrites baselines in place, which is a developer's edit; under CI run with --corrected and accept with dune promote.
  $ cat test/cram/run/greeting.expected
  stale

A listing is refused the same way, with the same code: -l makes the
checks a run makes before anything executes.

  $ run env CI=1 ./suite_main.exe -l -u > out 2> err
  [1]
  $ cat out
  $ cat err
  windtrap: baseline update refused: CI is set. -u rewrites baselines in place, which is a developer's edit; under CI run with --corrected and accept with dune promote.

The two acceptances contradict each other, so asking for both is a
usage error.

  $ run ./suite_main.exe -u --corrected > out 2> err
  [2]
  $ cat out
  $ cat err
  windtrap: options '-u' and '--corrected' cannot be combined
  usage: suite_main.exe [OPTIONS] [PATTERN...]
