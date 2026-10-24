How a run ends when code under test tries to end the process: exit, an
at_exit handler, Unix._exit, a domain, a fork, a fatal exception. Each
block is one situation and the verdict the run gives it; a block whose
line begins "Held for the redesign" pins a defect as it stands today
(see doc/dev/testing.md).

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

A call to exit in a test ends the worker process that runs it, and is
that test's failure; the tests after it run in another worker, whether
it comes before or after another failure:

  $ run VERDICT_SUITE=exit_before_fail ./verdict_main.exe
  exit_before_fail: 3 tests
  ──────────────────────── failures ────────────────────────
    FAIL  exits
      test/cram/verdicts/verdict_main.ml:N
      the test called exit, which ended its worker; a test must return or raise, never exit the process
  
    FAIL  fails
      test/cram/verdicts/verdict_main.ml:N
      expected  1
      actual    2
  ──────────────────────────────────────────────────────────
  
  1 passed, 2 failed in DURATION.
  [1]
  $ brief VERDICT_SUITE=exit_after_fail ./verdict_main.exe
    FAIL  fails
    FAIL  exits
  1 passed, 2 failed in DURATION.
  [1]

A call to exit in a property's law fails the property:

  $ brief VERDICT_SUITE=exit_in_prop ./verdict_main.exe
    FAIL  exits
  1 passed, 1 failed in DURATION.
  [1]

A call to exit in a fixture's teardown fails the release, with or
without an earlier failure:

  $ run VERDICT_SUITE=exit_release ./verdict_main.exe
  exit_release: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  fixture release
      [release] test/cram/verdicts/verdict_main.ml:N
      fixture (test/cram/verdicts/verdict_main.ml:N): release called exit, which ended its worker; a test must return or raise, never exit the process
  ──────────────────────────────────────────────────────────
  
  1 passed, 1 failed in DURATION.
  [1]
  $ brief VERDICT_SUITE=exit_release_after_fail ./verdict_main.exe
    FAIL  fails
    FAIL  fixture release
  1 passed, 2 failed in DURATION.
  [1]

A call to exit on a domain the test joins fails the test, before or after
a failure:

  $ brief VERDICT_SUITE=domain_before_fail ./verdict_main.exe
    FAIL  exits on a domain
    FAIL  fails
  1 passed, 2 failed in DURATION.
  [1]
  $ brief VERDICT_SUITE=domain_after_fail ./verdict_main.exe
    FAIL  fails
    FAIL  exits on a domain
  1 passed, 2 failed in DURATION.
  [1]

The one escape (lib/windtrap.mli, The process): a domain that outlives
the test that spawned it dies with that test's worker, so an exit it
would call later is never made, no test records it, and the run exits
0. The test after it runs in another worker, which the domain never
reaches:

  $ brief VERDICT_SUITE=domain_unjoined_only ./verdict_main.exe; echo "exit $?"
  domain_unjoined_only: 2 passed in DURATION.
  exit 0
  $ brief VERDICT_SUITE=domain_unjoined ./verdict_main.exe
    FAIL  fails
  2 passed, 1 failed in DURATION.
  [1]

An at_exit handler that a test registers and then runs by its own exit
is that test's failure:

  $ brief VERDICT_SUITE=atexit_in_test_exit ./verdict_main.exe
    FAIL  registers and exits
    FAIL  fails
  1 passed, 2 failed in DURATION.
  [1]

An at_exit handler that a test registers runs as its worker ends, and
the code it exits with is the worker's, not the run's, before or after a
failure:

  $ brief VERDICT_SUITE=atexit_after_fail ./verdict_main.exe; echo "exit $?"
    FAIL  fails
  2 passed, 1 failed in DURATION.
  exit 1
  $ brief VERDICT_SUITE=atexit_before_fail ./verdict_main.exe; echo "exit $?"
    FAIL  fails
  2 passed, 1 failed in DURATION.
  exit 1

An at_exit handler that raises fails the run, as no test does, and names
the test that registered it:

  $ run VERDICT_SUITE=atexit_raise ./verdict_main.exe
  atexit_raise: 2 tests
  ──────────────────────── failures ────────────────────────
    FAIL  fixture release
      [release] test/cram/verdicts/verdict_main.ml:N
      an at_exit function that the test registers registered raised
      uncaught exception:
        Failure("cleanup")
      Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
      Called from Stdlib.at_exit.new_exit in file "stdlib.ml", line 566, characters 59-63
  ──────────────────────────────────────────────────────────
  
  2 passed, 1 failed in DURATION.
  [1]

Unix._exit in a test ends its worker, and fails the test with the code
the worker exited with; the tests after it run:

  $ run VERDICT_SUITE=unix_exit_before_fail ./verdict_main.exe
  unix_exit_before_fail: 2 tests
  ──────────────────────── failures ────────────────────────
    FAIL  exits at once
      test/cram/verdicts/verdict_main.ml:N
      the test ended its worker process, which exited with code 0
  
    FAIL  fails
      test/cram/verdicts/verdict_main.ml:N
      expected  1
      actual    2
  ──────────────────────────────────────────────────────────
  
  2 failed in DURATION.
  [1]
  $ brief VERDICT_SUITE=unix_exit_after_fail ./verdict_main.exe
    FAIL  fails
    FAIL  exits at once
  2 failed in DURATION.
  [1]

A forked child of a test exits as it would without windtrap, and its
exit is no failure, under --stream too, where nothing prints twice:

  $ brief VERDICT_SUITE=fork_child_exit ./verdict_main.exe
    FAIL  fails
  2 passed, 1 failed in DURATION.
  [1]
  $ brief VERDICT_SUITE=fork_child_exit ./verdict_main.exe --stream
    FAIL  fails
  2 passed, 1 failed in DURATION.
  [1]

A program that spawned a domain before run cannot fork: it runs itself
again for each worker, and the verdicts are the same:

  $ brief VERDICT_DOMAIN_FIRST=1 VERDICT_SUITE=exit_before_fail ./verdict_main.exe
    FAIL  exits
    FAIL  fails
  1 passed, 2 failed in DURATION.
  [1]

A handler that catches every exception around exit catches nothing,
since exit raises none, and the test fails:

  $ run VERDICT_SUITE=try_with ./verdict_main.exe
  try_with: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  catches its exit
      test/cram/verdicts/verdict_main.ml:N
      the test called exit, which ended its worker; a test must return or raise, never exit the process
  ──────────────────────────────────────────────────────────
  
  1 failed in DURATION.
  [1]

Unbounded recursion is an ordinary failure. The stack is made small, so
that it overflows at once:

  $ run OCAMLRUNPARAM=l=1M VERDICT_SUITE=stackoverflow ./verdict_main.exe > /dev/null
  [1]
  $ grep -E 'Stack overflow|passed' out | scrub
        Stack overflow
  1 passed, 1 failed in DURATION.

Out_of_memory or Sys.Break from a test fails it and ends its worker,
and the run goes on:

  $ brief VERDICT_SUITE=fatal_oom ./verdict_main.exe
    FAIL  fails
    FAIL  runs out of memory
  2 failed in DURATION.
  [1]
  $ brief VERDICT_SUITE=fatal_break ./verdict_main.exe
    FAIL  fails
    FAIL  breaks
  2 failed in DURATION.
  [1]
