The last-failed record that --failed reads: a run never fails because it
cannot read or write it, and a record it cannot read counts as empty.
The suite "record" is two passing tests, and its record is
_logs/record/.last-failed-EXE, where EXE names the executable that ran
it. Each block is one situation and the verdict
the run gives it; a block whose line begins "Held for the redesign"
pins a defect as it stands today (see doc/dev/testing.md).

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
A run names its record by its suite and by the executable, the MD5 of
its path below the build directory:

  $ run VERDICT_SUITE=record ./verdict_main.exe > /dev/null
  $ record=$(echo _logs/record/.last-failed-*)
  $ echo "$record" | sed -E 's/-[0-9a-f]{32}$/-EXE/'
  _logs/record/.last-failed-EXE
  $ rm -r _logs

With no record at all, --failed has nothing to run and exits 2:

  $ run VERDICT_SUITE=record ./verdict_main.exe --failed
  --- stderr
  windtrap: no recorded failures match the current suite
  [2]

A record that is a directory changes no run, and --failed then finds
nothing:

  $ mkdir -p "$record"
  $ run VERDICT_SUITE=record ./verdict_main.exe; echo "exit $?"
  record: 2 passed in DURATION.
  exit 0
  $ run VERDICT_SUITE=record ./verdict_main.exe --failed
  --- stderr
  windtrap: no recorded failures match the current suite
  [2]
  $ rm -r _logs

A record with a wrong first line, one that cannot be read, one cut short
and one naming a test the suite no longer has each count as empty: --failed
exits 2, and the next whole run exits 0 and writes the record again:

  $ mkdir -p _logs/record
  $ for state in magic unreadable truncated renamed; do
  >   case $state in
  >     magic) printf 'nonsense\nfirst\n' > "$record" ;;
  >     unreadable) printf 'windtrap-last-failed 2\nfirst\n' > "$record"; chmod 000 "$record" ;;
  >     truncated) printf 'windtrap-last' > "$record" ;;
  >     renamed) printf 'windtrap-last-failed 2\nrenamed\n' > "$record" ;;
  >   esac
  >   run VERDICT_SUITE=record ./verdict_main.exe --failed > /dev/null; failed=$?
  >   run VERDICT_SUITE=record ./verdict_main.exe > /dev/null; whole=$?
  >   echo "$state: --failed [$failed], then [$whole] and $(head -1 "$record")"
  > done
  magic: --failed [2], then [0] and windtrap-last-failed 2
  unreadable: --failed [2], then [0] and windtrap-last-failed 2
  truncated: --failed [2], then [0] and windtrap-last-failed 2
  renamed: --failed [2], then [0] and windtrap-last-failed 2

A record that cannot be written changes no run: under --stream nothing
is captured, so only the record lives under the log directory, which
here is a regular file:

  $ echo 'a file' > blocked
  $ run VERDICT_SUITE=record ./verdict_main.exe --stream -o blocked; echo "exit $?"
  record: 2 passed in DURATION.
  exit 0

Two executables that name one suite alike keep each their own record,
so one's passing run leaves the other's failure recorded:

  $ mkdir a b
  $ cp verdict_main.exe a/ && cp verdict_main.exe b/
  $ brief VERDICT_SUITE=shared_a ./a/verdict_main.exe
    FAIL  same path
  1 failed in DURATION.
  [1]
  $ brief VERDICT_SUITE=shared_b ./b/verdict_main.exe; echo "exit $?"
  shared: 1 passed in DURATION.
  exit 0
  $ brief VERDICT_SUITE=shared_a ./a/verdict_main.exe --failed
    FAIL  same path
  1 failed in DURATION.
  [1]

The record that 0.2.0 wrote, keyed by the suite alone, may be any of
those executables', so no run reads it, or writes it:

  $ rm -r _logs && mkdir -p _logs/record
  $ printf 'windtrap-last-failed 1\nfirst\n' > _logs/record/.last-failed
  $ run VERDICT_SUITE=record ./verdict_main.exe --failed
  --- stderr
  windtrap: no recorded failures match the current suite
  [2]
  $ run VERDICT_SUITE=record ./verdict_main.exe > /dev/null; cat _logs/record/.last-failed
  windtrap-last-failed 1
  first

A table records the rows that failed, each by its printed value, and a
property its counterexample, by its token: the record names a case
within its test. Here rows b and d fail, and the property fails from 10
on:

  $ rm -f ran
  $ brief VERDICT_SUITE=rerun VERDICT_FAILING=b,d ./verdict_main.exe --seed s1:0000000000000001
    FAIL  rows
    FAIL  counterexample
  1 passed, 2 failed in DURATION.
  [1]
  $ grep -E 'property failed|counterexample \(|which failed|row . broke' out
      counterexample (row 2 of 4, the second): "b"
      which failed with:
        row b broke
      counterexample (row 4 of 4): "d"
      which failed with:
        row d broke
      counterexample (case 0, shrunk 8 steps): 10
      which failed with:
  $ rerun=$(echo _logs/rerun/.last-failed-*)
  $ sed 's/r1:[^ ]*/TOKEN/' "$rerun"
  windtrap-last-failed 2
  rows	row	\"b\"
  rows	row	\"d\"
  counterexample	replay	TOKEN

--failed reruns those cases and no other: rows b and d, and the
property on its counterexample alone, which it checks again as --replay
would, drawing nothing. The test that passed does not run:

  $ rm -f ran
  $ brief VERDICT_SUITE=rerun VERDICT_FAILING=b,d ./verdict_main.exe --failed
    FAIL  rows
    FAIL  counterexample
  2 failed in DURATION.
  [1]
  $ cat ran
  b
  d
  10

A row that now passes leaves the record, and the next --failed reruns
the row that still fails:

  $ brief VERDICT_SUITE=rerun VERDICT_FAILING=d ./verdict_main.exe --failed
    FAIL  rows
    FAIL  counterexample
  2 failed in DURATION.
  [1]
  $ sed 's/r1:[^ ]*/TOKEN/' "$rerun"
  windtrap-last-failed 2
  rows	row	\"d\"
  counterexample	replay	TOKEN
  $ rm -f ran
  $ brief VERDICT_SUITE=rerun VERDICT_FAILING=d ./verdict_main.exe --failed
    FAIL  rows
    FAIL  counterexample
  2 failed in DURATION.
  [1]
  $ cat ran
  d
  10

A line that names a test alone names it whole, so --failed reruns every
row of the table and draws the property's cases again:

  $ printf 'windtrap-last-failed 2\nrows\ncounterexample\n' > "$rerun"
  $ rm -f ran
  $ brief VERDICT_SUITE=rerun VERDICT_FAILING=d ./verdict_main.exe --failed
    FAIL  rows
    FAIL  counterexample
  2 failed in DURATION.
  [1]
  $ head -4 ran
  a
  b
  c
  d

A recorded row that the table no longer has is stale: the table fails and
says so, and runs none of its rows rather than pass on the ones left. The
record then names the table whole:

  $ printf 'windtrap-last-failed 2\nrows\trow\t\\"x\\"\nrows\trow\t\\"b\\"\n' > "$rerun"
  $ rm -f ran
  $ brief VERDICT_SUITE=rerun ./verdict_main.exe --failed
    FAIL  rows
  1 failed in DURATION.
  [1]
  $ grep 'last-failed record' out
      --failed: the last-failed record names a case that this table no longer has: row "x"; run it without --failed
  $ test -e ran || echo no row ran
  no row ran
  $ cat "$rerun"
  windtrap-last-failed 2
  rows

A recorded token whose generator no longer parses the value it recorded
is stale too, here a token whose digest is another value's, and fails
its property:

  $ brief VERDICT_SUITE=rerun VERDICT_FAILING=d ./verdict_main.exe > /dev/null
  [1]
  $ sed -E 's/^(counterexample.*):[0-9a-f]+$/\1:00000000/' "$rerun" > stale
  $ mv stale "$rerun"
  $ brief VERDICT_SUITE=rerun ./verdict_main.exe --failed -f counterexample
    FAIL  counterexample
  1 failed in DURATION.
  [1]
  $ grep 'recorded counterexample' out | sed 's/r1:[^:]*:/r1:CHOICES:/'
      --failed: the recorded counterexample r1:CHOICES:00000000: stale: its generator now parses 10, not the value it recorded

Two rows that print alike are refused as duplicates, and no row runs:

  $ brief VERDICT_SUITE=rerun_dup ./verdict_main.exe
    FAIL  rows
  1 failed in DURATION.
  [1]
  $ grep 'duplicate rows' out
      duplicate rows: rows 1 and 3 print "a"; a row is identified by its printed value, so no two rows may print alike
