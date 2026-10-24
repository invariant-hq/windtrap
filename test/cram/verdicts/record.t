The last-failed record that --failed reads: a run never fails because it
cannot read or write it, and a record it cannot read counts as empty.
The suite "record" is two passing tests, and its record is
_logs/record/.last-failed. Each block is one situation and the verdict
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
  $ record=_logs/record/.last-failed

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
  >     unreadable) printf 'windtrap-last-failed 1\nfirst\n' > "$record"; chmod 000 "$record" ;;
  >     truncated) printf 'windtrap-last' > "$record" ;;
  >     renamed) printf 'windtrap-last-failed 1\nrenamed\n' > "$record" ;;
  >   esac
  >   run VERDICT_SUITE=record ./verdict_main.exe --failed > /dev/null; failed=$?
  >   run VERDICT_SUITE=record ./verdict_main.exe > /dev/null; whole=$?
  >   echo "$state: --failed [$failed], then [$whole] and $(head -1 "$record")"
  > done
  magic: --failed [2], then [0] and windtrap-last-failed 1
  unreadable: --failed [2], then [0] and windtrap-last-failed 1
  truncated: --failed [2], then [0] and windtrap-last-failed 1
  renamed: --failed [2], then [0] and windtrap-last-failed 1

A record that cannot be written changes no run: under --stream nothing
is captured, so only the record lives under the log directory, which
here is a regular file:

  $ echo 'a file' > blocked
  $ run VERDICT_SUITE=record ./verdict_main.exe --stream -o blocked; echo "exit $?"
  record: 2 passed in DURATION.
  exit 0

Held for the redesign (doc/dev/testing.md): the record is keyed by the suite's name alone, so two executables of one name erase each other's.

  $ mkdir a b
  $ cp verdict_main.exe a/ && cp verdict_main.exe b/
  $ brief VERDICT_SUITE=shared_a ./a/verdict_main.exe
    FAIL  same path
  1 failed in DURATION.
  [1]
  $ brief VERDICT_SUITE=shared_b ./b/verdict_main.exe; echo "exit $?"
  shared: 1 passed in DURATION.
  exit 0
  $ run VERDICT_SUITE=shared_a ./a/verdict_main.exe --failed
  --- stderr
  windtrap: no recorded failures match the current suite
  [2]
