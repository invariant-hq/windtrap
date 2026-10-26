A replay checks a recorded case again, by its token, under --failed or
--replay: it draws nothing, so it is judged for no vacuity, and a
replayed case that passes says so. The suite "replayed" is a counter
whose incr does nothing under VERDICT_BUG, checked on one domain and on
two; each run of a program notes its test in the file "ran" (see
doc/dev/testing.md).

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
  $ count() { sort ran | uniq -c | sed -E 's/^ +//'; rm -f ran; }

With the bug, each test fails, and the record keeps each counterexample
by its token. Neither calls reset:

  $ brief VERDICT_SUITE=replayed VERDICT_BUG=1 ./verdict_main.exe --seed s1:0000000000000001
    FAIL  counts
    FAIL  counts from two domains
  2 failed in DURATION.
  [1]
  $ grep reset out || echo 'no reset'
  no reset
  $ rm -f ran
  $ record=$(echo _logs/replayed/.last-failed-*)
  $ sed 's/r1:[^ ]*/TOKEN/' "$record"
  windtrap-last-failed 2
  counts	replay	TOKEN
  counts from two domains	replay	TOKEN
  $ token=$(grep 'two domains' "$record" | cut -f 3)

Fixed, --failed replays each case alone. Neither is vacuous for the
commands its case never calls; each says that its replayed case passed.
A case run on two domains runs as many times as the search runs one, 50,
and its schedule is not replayed:

  $ run VERDICT_SUITE=replayed ./verdict_main.exe --failed
  replayed: 2 tests (seed SEED)
  replayed (2):
    counts: the replayed case passed
    counts from two domains: its schedule is not replayed: the case passed 50 times
  
  2 passed in DURATION.
  $ count
  1 one domain
  50 two domains

A replayed case that passed leaves the record, as any test that passes
does, and --failed then has nothing to run:

  $ cat "$record"
  windtrap-last-failed 2
  $ run VERDICT_SUITE=replayed ./verdict_main.exe --failed
  --- stderr
  windtrap: no recorded failures match the current suite
  [2]

--replay of the two-domain case says the same, and so does its row
under -v:

  $ run VERDICT_SUITE=replayed ./verdict_main.exe -v -f 'two domains' --replay "$token" | sed -E 's/ +[0-9.]+m?s \(/ DURATION (/'
  replayed: 1 test (seed SEED)
    PASS  counts from two domains DURATION (its schedule is not replayed: the case passed 50 times)
  
  replayed (1):
    counts from two domains: its schedule is not replayed: the case passed 50 times
  
  1 passed in DURATION.
  $ count
  50 two domains

With the bug back, the replay stops at the first run that fails:

  $ brief VERDICT_SUITE=replayed VERDICT_BUG=1 ./verdict_main.exe -f 'two domains' --replay "$token"
    FAIL  counts from two domains
  1 failed in DURATION.
  [1]
  $ count
  1 two domains
