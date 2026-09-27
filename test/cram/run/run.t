A suite as a process: the exit code of each way a run can end, and the
stream each of its messages takes. What the messages say is pinned by the
suites of their modules; a block here shows the line it owns, beside the
code and the streams that only a real process has.

`run` states the child's whole environment, so that a developer's CI,
NO_COLOR or WINDTRAP_* setting cannot reshape a transcript, and keeps the
logs and the baseline lookups inside this sandbox. It prints standard
output, then standard error under a `--- stderr` line, durations masked,
and returns the child's code.

  $ scrub() { sed -E 's/ in [0-9.]+m?s\./ in DURATION./'; }
  $ run() {
  >   env -i PATH="$PATH" TMPDIR="${TMPDIR:-/tmp}" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_PROJECT_ROOT="$PWD" \
  >       WINDTRAP_OUTPUT="$PWD/_logs" "$@" > out 2> err
  >   code=$?
  >   scrub < out
  >   if [ -s err ]; then echo '--- stderr'; cat err; fi
  >   return $code
  > }

--help and --version exit 0 and write on standard output alone. The page
names the program by its argv.(0) (test/unit's help.expected pins the
page):

  $ run ./suite_main.exe --help > /dev/null
  $ sed -n '1p;3p' out
  suite_main.exe - windtrap test runner
  usage: suite_main.exe [OPTIONS] [PATTERN...]
  $ cat err
  $ run ./suite_main.exe --version > /dev/null
  $ sed -E 's/^(windtrap) [^ ]+$/\1 VERSION/' out
  windtrap VERSION
  $ cat err

A command line that does not parse exits 2 before anything runs, with the
error and the usage on standard error:

  $ run ./suite_main.exe --nosuchflag
  --- stderr
  windtrap: unknown option '--nosuchflag'
  usage: suite_main.exe [OPTIONS] [PATTERN...]
  [2]

A mirror is resolved after the parse, and a value it cannot take is the
same usage error, naming the variable:

  $ run WINDTRAP_PROP_COUNT=nope ./suite_main.exe
  --- stderr
  windtrap: invalid value 'nope' for WINDTRAP_PROP_COUNT: expected a positive integer
  usage: suite_main.exe [OPTIONS] [PATTERN...]
  [2]

A run whose selection passes exits 0 on one line of standard output (the
four tests left when the failing one is excluded, the slow-tagged test
and the baseline planted here among them):

  $ mkdir -p test/cram/run
  $ echo 'hello from the fixture' > test/cram/run/greeting.expected
  $ run ./suite_main.exe -e boom
  fixture: 4 passed in DURATION.

A run with a failed test exits 1, its report on standard output:

  $ run ./suite_main.exe -f boom > /dev/null
  [1]
  $ tail -n 1 out | scrub
  1 failed in DURATION.
  $ cat err

A call to exit in code under test does not end the process: the test
after it runs, and the process ends through the run's own code.

  $ run env FACADE_FIXTURE=exits ./suite_main.exe > /dev/null
  [1]
  $ tail -n 1 out | scrub
  1 passed, 2 failed in DURATION.
  $ cat err

A selection typed on the command line that keeps no test exits 2. The way
out restates the program as it was typed:

  $ run ./suite_main.exe -f zzznope
  fixture: no tests ran: filter "zzznope" matched none of 5 tests.
  list: ./suite_main.exe -l
  [2]

The selection is named back escaped for a reader rather than for OCaml:
the quote, the backslash, the three named control characters, and a hex
fallback for every other byte below space and for DEL.

  $ filter=$(printf 'a"b\\c\nd\te\rf\001g\177h')
  $ run ./suite_main.exe -l -f "$filter"
  --- stderr
  windtrap: no tests selected: filter "a\"b\\c\nd\te\rf\x01g\x7fh" matched none of 5 tests.

A suite the runner refuses to start exits 1 with nothing on standard
output, the refusal on standard error. A listing makes the same checks
and refuses the same way:

  $ run env FACADE_FIXTURE=duplicate ./suite_main.exe
  --- stderr
  windtrap: duplicate test paths:
    dup › twice
  Every full test path must be unique.
  [1]
  $ run env FACADE_FIXTURE=duplicate ./suite_main.exe -l
  --- stderr
  windtrap: duplicate test paths:
    dup › twice
  Every full test path must be unique.
  [1]

A focus outside CI narrows the run and warns on standard error, apart
from the report, whatever the code:

  $ run env FACADE_FIXTURE=focus ./suite_main.exe
  fixture: 1 passed in DURATION.
  --- stderr
  windtrap: warning: focus is active: 1 of 2 tests ran; remove the focus before committing
  $ run env FACADE_FIXTURE=focus ./suite_main.exe -f unfocused
  fixture: no tests ran: focus and filter "unfocused" matched none of 2 tests.
  list: ./suite_main.exe -l
  --- stderr
  windtrap: warning: focus is active: 0 of 2 tests ran; remove the focus before committing
  [2]

A JUnit report the run cannot write is one warning on standard error, and
the code stays the tests'. Here the report's parent is a regular file:

  $ echo 'not a directory' > blocked
  $ run ./suite_main.exe -f math --junit blocked/r.xml > /dev/null
  $ scrub < out
  fixture: 2 passed in DURATION.
  $ sed -E 's/(report to [^:]+): .*/\1: REASON/' err
  windtrap: warning: could not write JUnit report to blocked/r.xml: REASON
  $ run ./suite_main.exe -f boom --junit blocked/out > /dev/null
  [1]
  $ sed -E 's/(report to [^:]+): .*/\1: REASON/' err
  windtrap: warning: could not write JUnit report to blocked/out/fixture.xml: REASON

A correction the run cannot write fails the run, and the JUnit warning
stays the one line on standard error. Here the correction's path is taken
by a directory:

  $ mkdir -p test/cram/run/greeting.expected.corrected
  $ echo stale > test/cram/run/greeting.expected
  $ run ./suite_main.exe -f greeting --corrected --junit blocked/r.xml > /dev/null
  [1]
  $ grep -A1 '^corrections' out | sed -E 's/(could not write [^:]+): .*/\1: REASON/'
  corrections (1):
    could not write test/cram/run/greeting.expected.corrected: REASON
  $ sed -E 's/(report to [^:]+): .*/\1: REASON/' err
  windtrap: warning: could not write JUnit report to blocked/r.xml: REASON
