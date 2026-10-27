--case PATH checks one case of the one selected property, or stateful
test: the bytes of the file, read as LEB128 choices, read totally, so
that every file is a case. The fixture's properties fail on an element
from 10 on, hang on 7, discard an odd value, and hold over a generator
that reads every kind of choice; its stateful test loses a counter's
fourth increment.

  $ run() {
  >   env -i PATH="$PATH" TMPDIR="${TMPDIR:-/tmp}" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_PROJECT_ROOT="$PWD" WINDTRAP_OUTPUT="$PWD/_logs" FACADE_FIXTURE=cased "$@"
  > }
  $ scrub() {
  >   sed -E 's/ in [0-9.]+m?s([. ])/ in DURATION\1/; s/s1:[0-9a-f]{16}/SEED/g; s/(suite_main\.ml):[0-9]+/\1:N/g'
  > }

A case that the law refutes fails as a property does, and exits 1. Its
block says the case was replayed, and keeps it as an example, or as a
token where the value prints no OCaml. Nothing else runs: no example, no
generated case, no shrink.

  $ printf '\001\012' > ten
  $ run ./suite_main.exe --case ten -f small > out
  [1]
  $ scrub < out
  fixture: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  small
      test/cram/run/suite_main.ml:N
      counterexample (replayed case): [10]
      keep it: ~examples:[ [10] ]
      which failed with:
        expected  less than 10
        actual    10
  ──────────────────────────────────────────────────────────
  
  1 failed in DURATION.

A file shorter than the case reads 0 past its end, so an empty file is
the case of zeros, here the empty list:

  $ : > empty
  $ run ./suite_main.exe --case empty -f small | scrub
  fixture: 1 test
  replayed (1):
    small: the replayed case passed
  
  1 passed in DURATION.

Every string of bytes is some case: random files of any length never
fail to decode. The law that holds passes on each, and the one over
lists passes or fails on each, never refused:

  $ i=0; while [ $i -lt 20 ]; do
  >   dd if=/dev/urandom of=random bs=53 count=$i 2> /dev/null
  >   run ./suite_main.exe --case random -f anything > /dev/null 2>&1; echo "anything $?"
  >   run ./suite_main.exe --case random -f small > out 2>&1; echo "small $?"
  >   grep -l 'windtrap:' out
  >   i=$((i + 1))
  > done | sort -u
  anything 0
  small 0
  small 1

A case that hangs times out at its limit, and is never read as a pass:

  $ printf '\007' > seven
  $ run ./suite_main.exe --case seven -f 'hangs on 7' --timeout 0.2 | scrub
  fixture: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  hangs on 7
      test/cram/run/suite_main.ml:N
      timed out after 0.2s
  ──────────────────────────────────────────────────────────
  
  1 failed in DURATION.

A case that the generator or the law discards is outside the law's
domain: nothing was checked, so the test skips, saying why, and the run
exits 0:

  $ printf '\003' > three
  $ run ./suite_main.exe --case three -f even -v | scrub
  fixture: 1 test
    SKIP  even (--case three: the case was discarded, so nothing was checked)
  1 skipped in DURATION.

A test that checks no property fails, since it would pass on a case it
never read:

  $ run ./suite_main.exe --case three -f plain | scrub
  fixture: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  plain
      test/cram/run/suite_main.ml:N
      --case: this test checks no property, so it checked no case
  ──────────────────────────────────────────────────────────
  
  1 failed in DURATION.

A selection of more than one test, or of none, an unreadable file and a
replay token beside the file are refused before any test runs, and the
run exits 2:

  $ run ./suite_main.exe --case three
  windtrap: --case checks one case of one property, and the selection holds 6 tests; narrow it with -f to the property to check
  [2]
  $ run ./suite_main.exe --case three -f nothing
  windtrap: --case checks one case of one property, and the selection holds 0 tests; narrow it with -f to the property to check
  [2]
  $ run ./suite_main.exe --case missing -f even
  windtrap: invalid value 'missing' for --case: expected a readable file
  usage: suite_main.exe [OPTIONS] [PATTERN...]
  [2]
  $ run ./suite_main.exe --case three --replay 'r1:AA:00' -f even
  windtrap: options '--replay' and '--case' cannot be combined
  usage: suite_main.exe [OPTIONS] [PATTERN...]
  [2]

A case file holds what a replay token holds: the token's field between
r1: and the digest is the file's bytes in base64url. So the file of a
real counterexample, here the stateful test's, decoded from the token
its failure printed, fails again on the same calls:

  $ run ./suite_main.exe -f counts > out
  [1]
  $ token=$(sed -n 's/^ *replay it: .*--replay //p' out)
  $ run env FACADE_FIXTURE=case_of ./suite_main.exe "$token" > counts.case
  $ run ./suite_main.exe --case counts.case -f counts > again
  [1]
  $ sed -n '/counterexample/,/which failed/p' out | sed 's/(case [0-9]*, shrunk [0-9]* steps)/(replayed case)/' > before
  $ sed -n '/counterexample/,/which failed/p' again > after
  $ diff before after && echo same counterexample
  same counterexample
  $ grep 'replay it' again | sed "s/$token/TOKEN/"
      replay it: -f counts --replay TOKEN
