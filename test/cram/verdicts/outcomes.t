What a test's outcome is when its body catches, skips, is expected to
fail, is a property that judges nothing, or is a table with no row.
Each block is one situation and the verdict the run gives it; a block
whose line begins "Held for the redesign" pins a defect as it stands
today (see doc/dev/testing.md).

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
  >   grep -E '^  (FAIL|VACUOUS)  ' out
  >   tail -1 out | scrub
  >   if [ -s err ]; then echo '--- stderr'; head -1 err | scrub; fi
  >   return $code
  > }

A failing verb records its failure before it raises, so a catch-all
around it fails the test, and the block says the failure was caught:

  $ run VERDICT_SUITE=try_with_assert_only ./verdict_main.exe
  try_with_assert_only: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  swallows a failing verb
      test/cram/verdicts/verdict_main.ml:N
      failed, then caught
      expected  1
      actual    2
  ──────────────────────────────────────────────────────────
  
  1 failed in DURATION.
  [1]

So does a catch-all on a domain the test spawned:

  $ brief VERDICT_SUITE=try_with_domain ./verdict_main.exe
    FAIL  catches on a domain
  1 failed in DURATION.
  [1]
  $ grep -c 'failed, then caught' out
  1

A law that catches its verb fails its case, which shrinks as it would
had the law raised, and the test carries no other failure:

  $ run VERDICT_SUITE=try_with_prop ./verdict_main.exe --seed s1:0000000000000001 > block; echo "exit $?"
  exit 1
  $ sed -E 's/shrunk [0-9]+ steps/shrunk N steps/' block
  try_with_prop: 1 test (seed SEED)
  ──────────────────────── failures ────────────────────────
    FAIL  catches in its law
      test/cram/verdicts/verdict_main.ml:N
      counterexample (case 0, shrunk N steps): 10
      keep it: ~examples:[ 10 ]
      which failed at:
        test/cram/verdicts/verdict_main.ml:N
        failed, then caught
        expected  less than 10
        actual    10
  ──────────────────────────────────────────────────────────
  
  replay: ./verdict_main.exe --seed SEED
  1 failed in DURATION.

What an attempt caught stays with it, so a retry that passes passes:

  $ run VERDICT_SUITE=try_with_retry ./verdict_main.exe; echo "exit $?"
  try_with_retry: 1 test
  flaky tests (1):
    passed on attempt 2  catches on its first attempt
  
  1 passed (1 flaky) in DURATION.
  exit 0

eventually calls its function again while it fails, and records only the
failure of its last call, which nothing caught:

  $ brief VERDICT_SUITE=eventually ./verdict_main.exe
    FAIL  never holds
  1 passed, 1 failed in DURATION.
  [1]
  $ grep -c 'failed, then caught' out
  0
  [1]

An expectation records its mismatch before it returns, so a catch-all
around it changes nothing:

  $ brief VERDICT_SUITE=try_with_expect ./verdict_main.exe
    FAIL  catches around expect
  1 failed in DURATION.
  [1]
  $ grep 'expect: mismatch' out
      expect: mismatch

A skip is no failure: a run of a pass, two skipped tests and a property
that skips every case exits 0:

  $ run VERDICT_SUITE=skip ./verdict_main.exe; echo "exit $?"
  skip: 1 passed, 3 skipped in DURATION (seed SEED).
  exit 0

A selection whose every test skips exits 0, typed on the command line or
given by a mirror:

  $ run VERDICT_SUITE=skip_all ./verdict_main.exe -f skips; echo "exit $?"
  skip_all: 2 skipped in DURATION.
  exit 0
  $ run VERDICT_SUITE=skip_all WINDTRAP_FILTER=skips ./verdict_main.exe; echo "exit $?"
  skip_all: 2 skipped in DURATION.
  exit 0

An expected failure leaves the code alone, an exit included, and an
expected failure that passes fails:

  $ run VERDICT_SUITE=xfail ./verdict_main.exe; echo "exit $?"
  xfail: 1 passed, 2 expected failures in DURATION.
  exit 0
  $ brief VERDICT_SUITE=xpass ./verdict_main.exe
    FAIL  passes unexpectedly
  1 failed in DURATION.
  [1]
  $ grep 'expected to fail' out
      expected to fail, but the test passed

A property whose every case is discarded gives up and is vacuous, a
verdict that fails, whether it assumes, filters or rejects:

  $ brief VERDICT_SUITE=prop_discard ./verdict_main.exe
    VACUOUS  assumes
    VACUOUS  filters
    VACUOUS  rejects
  3 vacuous in DURATION.
  [1]
  $ grep -c 'property gave up: 201 discards exhausted the generation budget (0 cases passed)' out
  3

Every value of a domain checked whole discarded is vacuous too, though
the discards stay within the budget:

  $ run VERDICT_SUITE=prop_discard_whole ./verdict_main.exe
  prop_discard_whole: 1 test (seed SEED)
  ──────────────────────── failures ────────────────────────
    VACUOUS  discards every value
      test/cram/verdicts/verdict_main.ml:N
      every case was discarded (4 discards), so no case ran
      scope: 0 of 4 values passed, 4 discards
  ──────────────────────────────────────────────────────────
  
  1 vacuous in DURATION.
  [1]

A demand of cover that no passing case meets is vacuous, naming the
label:

  $ run VERDICT_SUITE=cover_unmet ./verdict_main.exe
  cover_unmet: 1 test (seed SEED)
  ──────────────────────── failures ────────────────────────
    VACUOUS  never covers
      test/cram/verdicts/verdict_main.ml:N
      never covered: "above a million" (over 100 passing cases)
      scope: 100 cases passed
  ──────────────────────────────────────────────────────────
  
  1 vacuous in DURATION.
  [1]

A share that cover demands and the cases show unmet is vacuous too,
saying the share the cases had and the bound they established, after
the property drew more cases than its count to decide:

  $ run VERDICT_SUITE=cover_share ./verdict_main.exe --seed s1:0000000000000001
  cover_share: 1 test (seed SEED)
  ──────────────────────── failures ────────────────────────
    VACUOUS  rarely accepts
      test/cram/verdicts/verdict_main.ml:N
      cover "accepted" ~share:0.2: 9.0% of 800 cases, at most 17.1%
      scope: 800 cases passed
      labels (800 passing cases):
          9.0%  accepted
  ──────────────────────────────────────────────────────────
  
  1 vacuous in DURATION.
  [1]

A stateful command that a passing case could call and none called is
vacuous, naming the command:

  $ run VERDICT_SUITE=never_called ./verdict_main.exe
  never_called: 1 test (seed SEED)
  ──────────────────────── failures ────────────────────────
    VACUOUS  never calls
      test/cram/verdicts/verdict_main.ml:N
      never called: "never" (over 5 passing cases); a call runs only where its arguments resolve and its ~pre holds
      scope: 5 cases passed
  ──────────────────────────────────────────────────────────
  
  1 vacuous in DURATION.
  [1]

A property of count 0 with no example runs nothing, and is vacuous: it
fails, saying why, and the summary counts it apart from a failure:

  $ run VERDICT_SUITE=prop_count_zero ./verdict_main.exe -f 'never runs'; echo "exit $?"
  prop_count_zero: 1 test (seed SEED)
  ──────────────────────── failures ────────────────────────
    VACUOUS  never runs
      test/cram/verdicts/verdict_main.ml:N
      no case and no example ran: ~count:0 runs the examples alone, and there are none
  ──────────────────────────────────────────────────────────
  
  1 vacuous in DURATION.
  exit 1

The count cannot be broadcast as 0: the mirror refuses it.

  $ run VERDICT_SUITE=prop_count_zero WINDTRAP_PROP_COUNT=0 ./verdict_main.exe
  --- stderr
  windtrap: invalid value '0' for WINDTRAP_PROP_COUNT: expected a positive integer
  usage: verdict_main.exe [OPTIONS] [PATTERN...]
  [2]

A table with no row is a test that checks nothing, and is vacuous. A
mirror's selection that leaves it out exits 0, as it does for any test:

  $ run VERDICT_SUITE=table_zero ./verdict_main.exe
  table_zero: 1 test
  ──────────────────────── failures ────────────────────────
    VACUOUS  rows
      test/cram/verdicts/verdict_main.ml:N
      the table has no row, so it checks nothing
  ──────────────────────────────────────────────────────────
  
  1 vacuous in DURATION.
  [1]
  $ run VERDICT_SUITE=table_zero WINDTRAP_FILTER=nomatch ./verdict_main.exe
  table_zero: no tests ran: filter "nomatch" matched none of 1 test.

A table with no row beside a passing test is vacuous, and the run exits
1, as for a failure:

  $ run VERDICT_SUITE=table_zero_plus ./verdict_main.exe; echo "exit $?"
  table_zero_plus: 2 tests
  ──────────────────────── failures ────────────────────────
    VACUOUS  rows
      test/cram/verdicts/verdict_main.ml:N
      the table has no row, so it checks nothing
  ──────────────────────────────────────────────────────────
  
  1 passed, 1 vacuous in DURATION.
  exit 1
