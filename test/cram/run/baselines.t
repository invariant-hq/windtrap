How a run accepts what it produced, seen from outside the process: the
files left on disk, the exit code a dune action reads, and the accept:
line run as pasted. The fixture's "greeting" test compares against
test/cram/run/greeting.expected under WINDTRAP_PROJECT_ROOT; the blocks
below leave it missing, accept it in place, make it stale and record the
correction the way a dune action would, each starting from the file the
block before it left.

  $ run() {
  >   env -i PATH="$PATH" TMPDIR="${TMPDIR:-/tmp}" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_PROJECT_ROOT="$PWD" \
  >       WINDTRAP_OUTPUT="$PWD/_logs" "$@"
  > }
  $ scrub() {
  >   sed -E 's/ in [0-9.]+m?s\./ in DURATION./; s/s1:[0-9a-f]+/SEED/'
  > }

A missing baseline fails the run and writes nothing. Run by hand, the
report closes on the in-place acceptance of the run's selection, spelled
with the program as it was typed:

  $ run ./suite_main.exe -f greeting > out 2>&1
  [1]
  $ grep 'accept:' out
  accept: ./suite_main.exe -u -f 'greeting'
  $ test -e test/cram/run/greeting.expected || echo 'nothing written'
  nothing written

-u accepts in place: the run exits 0, the file holds the produced text,
and the next run passes.

  $ run ./suite_main.exe -f greeting -u > out 2>&1
  $ cat test/cram/run/greeting.expected
  hello from the fixture
  $ run ./suite_main.exe -f greeting > out 2>&1

--corrected is what a dune action passes: the correction lands beside
the file as <file>.corrected, the file is left as it was, and a run whose
only failures are recorded corrections exits 0, so that the action's
diff? is the verdict.

  $ echo 'stale' > test/cram/run/greeting.expected
  $ run ./suite_main.exe -f greeting --corrected > out 2>&1
  $ cat test/cram/run/greeting.expected.corrected
  hello from the fixture
  $ cat test/cram/run/greeting.expected
  stale
  $ rm test/cram/run/greeting.expected.corrected

Beside any other failure the run exits 1, so dune reaches no diff? and
promotes nothing. The correction is still written, and the run says so
once, on standard error, after its report:

  $ run ./suite_main.exe -e math --corrected > out 2> err
  [1]
  $ cat err
  windtrap: warning: dune registers a correction for promotion only when the run that wrote it exits 0, so the failures above withhold the correction written here. Fix the failures, rerun, then 'dune promote'.
  $ cat test/cram/run/greeting.expected.corrected
  hello from the fixture
  $ rm test/cram/run/greeting.expected.corrected

A stale baseline in a test that also fails another way is never
corrected: under --corrected nothing lands beside it, and under -u the
file is left stale.

  $ echo 'stale' > test/cram/run/masked.expected
  $ run env FACADE_FIXTURE=masked ./suite_main.exe --corrected > out 2>&1
  [1]
  $ test -e test/cram/run/masked.expected.corrected || echo 'nothing to promote'
  nothing to promote
  $ run env FACADE_FIXTURE=masked ./suite_main.exe -u > out 2>&1
  [1]
  $ cat test/cram/run/masked.expected
  stale

One accept: line serves every block of a run by hand. The fixture holds
a literal that holds, a stale literal, a stale file baseline and a
failing property; the literals' source is the fixture's own, copied
where their locations point. Pasted, the line rewrites the two stale
baselines and nothing else:

  $ cp suite_main.ml test/cram/run/
  $ echo 'stale' > test/cram/run/accepts.expected
  $ run env FACADE_FIXTURE=accepts ./suite_main.exe > out 2>&1
  [1]
  $ grep 'accept:' out
  accept: ./suite_main.exe -u
  $ eval "run env FACADE_FIXTURE=accepts $(sed -n 's/^accept: //p' out)" > again 2>&1
  [1]
  $ tail -1 again | scrub
  3 passed, 1 failed, 2 corrections accepted in DURATION.
  $ diff -U0 suite_main.ml test/cram/run/suite_main.ml | grep '^[-+] '
  -    test "stale literal" (fun () -> expect "fresh" @@ __POS_OF__ "stale");
  +    test "stale literal" (fun () -> expect "fresh" @@ __POS_OF__ "fresh");
  $ cat test/cram/run/accepts.expected
  fresh from the fixture
