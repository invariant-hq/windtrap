How a selection ends a run: a selection flag that keeps nothing, a
mirror that keeps nothing, a value no flag or mirror takes, and the
one line a run with nothing to report prints. The suite is one test,
alpha, tagged fast. Each block is one situation and the verdict the run
gives it; a block whose line begins "Held for the redesign" pins a
defect as it stands today (see doc/dev/testing.md).

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

A selection typed on the command line that keeps nothing is a typo, and
exits 2, whichever flag made it:

  $ for flags in '-f nomatch' '-e alpha' '--tag slow' '--exclude-tag fast' '--shard 1/2'; do
  >   run VERDICT_SUITE=selection ./verdict_main.exe $flags > /dev/null
  >   echo "[$?] $flags: $(grep '^selection:' out | scrub)"
  > done
  [2] -f nomatch: selection: no tests ran: filter "nomatch" matched none of 1 test.
  [2] -e alpha: selection: no tests ran: exclusion "alpha" matched none of 1 test.
  [2] --tag slow: selection: no tests ran: tag "slow" matched none of 1 test.
  [2] --exclude-tag fast: selection: no tests ran: excluded tag "fast" matched none of 1 test.
  [2] --shard 1/2: selection: no tests ran: shard 1/2 matched none of 1 test.

The same selections given by the mirrors keep nothing and exit 0,
whether the suite runs directly or as a stanza under dune would run it:

  $ for mirror in WINDTRAP_FILTER=nomatch WINDTRAP_EXCLUDE=alpha WINDTRAP_TAG=slow WINDTRAP_EXCLUDE_TAG=fast WINDTRAP_SHARD=1/2; do
  >   run VERDICT_SUITE=selection "$mirror" ./verdict_main.exe > /dev/null
  >   echo "[$?] $mirror: $(head -1 out | scrub)"
  >   run VERDICT_SUITE=selection INSIDE_DUNE="$PWD" "$mirror" ./verdict_main.exe > /dev/null
  >   echo "[$?] under dune"
  > done
  [0] WINDTRAP_FILTER=nomatch: selection: no tests ran: filter "nomatch" matched none of 1 test.
  [0] under dune
  [0] WINDTRAP_EXCLUDE=alpha: selection: no tests ran: exclusion "alpha" matched none of 1 test.
  [0] under dune
  [0] WINDTRAP_TAG=slow: selection: no tests ran: tag "slow" matched none of 1 test.
  [0] under dune
  [0] WINDTRAP_EXCLUDE_TAG=fast: selection: no tests ran: excluded tag "fast" matched none of 1 test.
  [0] under dune
  [0] WINDTRAP_SHARD=1/2: selection: no tests ran: shard 1/2 matched none of 1 test.
  [0] under dune

A mirror beside a flag that selects nothing is still the mirror's
selection; beside a selection flag, the selection is the command
line's:

  $ for args in '--timeout 5' '--tag fast' '-f alpha'; do
  >   run VERDICT_SUITE=selection WINDTRAP_FILTER=nomatch ./verdict_main.exe $args > /dev/null
  >   echo "[$?] WINDTRAP_FILTER=nomatch $args: $(grep '^selection:' out | scrub)"
  > done
  [0] WINDTRAP_FILTER=nomatch --timeout 5: selection: no tests ran: filter "nomatch" matched none of 1 test.
  [2] WINDTRAP_FILTER=nomatch --tag fast: selection: no tests ran: filter "nomatch" and tag "fast" matched none of 1 test.
  [0] WINDTRAP_FILTER=nomatch -f alpha: selection: 1 passed in DURATION.
  $ run VERDICT_SUITE=selection WINDTRAP_EXCLUDE=alpha ./verdict_main.exe -f alpha > /dev/null
  [2]
  $ grep '^selection:' out | scrub
  selection: no tests ran: filter "alpha" and exclusion "alpha" matched none of 1 test.

A value that no flag or mirror takes is a usage error, and exits 2
before anything runs, under -l too:

  $ for args in '--shard 3/2' '--shard 0/1' '--bogus' '-l --bogus' '--seed 7'; do
  >   run VERDICT_SUITE=selection ./verdict_main.exe $args > /dev/null
  >   echo "[$?] $args: $(head -1 err)"
  > done
  [2] --shard 3/2: windtrap: invalid value '3/2' for --shard: expected K/N with 1 <= K <= N (e.g. 2/4)
  [2] --shard 0/1: windtrap: invalid value '0/1' for --shard: expected K/N with 1 <= K <= N (e.g. 2/4)
  [2] --bogus: windtrap: unknown option '--bogus'
  [2] -l --bogus: windtrap: unknown option '--bogus'
  [2] --seed 7: windtrap: invalid value '7' for --seed: expected an s1: token with 16 lowercase hexadecimal digits
  $ for mirror in WINDTRAP_SHARD=3/2 WINDTRAP_SHARD=bogus; do
  >   run VERDICT_SUITE=selection "$mirror" ./verdict_main.exe > /dev/null
  >   echo "[$?] $mirror: $(head -1 err)"
  > done
  [2] WINDTRAP_SHARD=3/2: windtrap: invalid value '3/2' for WINDTRAP_SHARD: expected K/N with 1 <= K <= N (e.g. 2/4)
  [2] WINDTRAP_SHARD=bogus: windtrap: invalid value 'bogus' for WINDTRAP_SHARD: expected K/N with 1 <= K <= N (e.g. 2/4)

A selection flag that keeps nothing prints two lines, the list: hint
above the summary, so the summary is the last:

  $ run VERDICT_SUITE=selection ./verdict_main.exe -f nomatch
  list: ./verdict_main.exe -l
  selection: no tests ran: filter "nomatch" matched none of 1 test.
  [2]
  $ tail -1 out
  selection: no tests ran: filter "nomatch" matched none of 1 test.

A passing run, a run whose every test skips and a suite that declares
no test each print one line:

  $ run VERDICT_SUITE=one_line ./verdict_main.exe -f passes > /dev/null; grep -c '' out
  1
  $ run VERDICT_SUITE=one_line ./verdict_main.exe -f skips > /dev/null; grep -c '' out
  1
  $ run VERDICT_SUITE=none ./verdict_main.exe > /dev/null; grep -c '' out
  1

A test that leaves a partial line under --stream does not take the
summary's line:

  $ brief VERDICT_SUITE=stream_nonl ./verdict_main.exe --stream
    FAIL  fails
  1 passed, 1 failed in DURATION.
  [1]
