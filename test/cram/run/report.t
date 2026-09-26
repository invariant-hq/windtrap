What a report leaves outside the process that printed it: the bytes a
test writes under --stream, where a failing test's log lands, and the
replay line, spelled for the way the run was started and run again as
pasted. The report's layout is pinned by test/unit's report suites.

  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_PROJECT_ROOT="$PWD" \
  >       WINDTRAP_OUTPUT="$PWD/_logs" "$@"
  > }
  $ scrub() {
  >   sed -E 's/ in [0-9.]+m?s\./ in DURATION./; s/s1:[0-9a-f]+/SEED/'
  > }

--stream captures nothing: each test's bytes reach standard output
whichever way they leave the process (the stdout channel left unflushed,
descriptor 1, a subprocess), in the order the tests ran and before the
report's own lines. A green streamed run is its tests' bytes and the one
line.

  $ run env FACADE_FIXTURE=stream ./suite_main.exe --stream > out 2> err
  [1]
  $ head -n 5 out
  through the stdout channel
  through descriptor 1
  through a subprocess
  before the failure
  fixture: 4 tests
  $ cat err
  $ run env FACADE_FIXTURE=stream ./suite_main.exe -s -e fails > out 2>&1
  $ scrub < out
  through the stdout channel
  through descriptor 1
  through a subprocess
  fixture: 3 passed in DURATION.

Under GitHub Actions the transcript folds, the annotations follow the
fold's close so that they are never folded away, and the summary is still
the last line (the envelope's lines are the report suite's):

  $ run CI=true GITHUB_ACTIONS=true ./suite_main.exe -f boom > out 2>&1
  [1]
  $ grep -o -e '^::[a-z]*' out
  ::group
  ::endgroup
  ::error
  $ tail -1 out | scrub
  1 failed in DURATION.

A failing test's captured output is kept in a log, whose path ends its
block. The sessions run the executable from under the build directory,
whose _tests hold the logs; run from anywhere else (a copy in a temporary
directory, with no WINDTRAP_PROJECT_ROOT and no build directory in sight),
the logs go under the system temporary directory, keyed by suite, and no
_build grows:

  $ dir=$(mktemp -d)
  $ cp ./suite_main.exe "$dir/suite.exe"
  $ mkdir "$dir/tmp"
  $ env -i PATH="$PATH" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >   TMPDIR="$dir/tmp" FACADE_FIXTURE=noisy \
  >   "$dir/suite.exe" > out 2>&1
  [1]
  $ grep 'full log:' out | sed "s#$dir#<tmp>#g"
      full log: <tmp>/tmp/windtrap/fixture/noisy.output
  $ cat "$dir/tmp/windtrap/fixture/noisy.output"
  hello from noisy
  $ test -e "$dir/_build" || echo 'no _build grown'
  no _build grown
  $ rm -rf "$dir"

A report whose failures include a property's closes on the replay line.
Run by hand, the line restates the program as it was typed, then the
seed:

  $ run env FACADE_FIXTURE=property ./suite_main.exe > out 2> err
  [1]
  $ grep 'replay:' out | scrub
  replay: ./suite_main.exe --seed SEED
  $ cat err

Run by dune, the program is dune's test action (INSIDE_DUNE set, the
path relative to the action's directory): the line is a dune exec of the
program's path from the project root. A core built with the mutation
backend adds that backend's flag, which test/unit's mutation loop suite
pins; it is masked here so that the line reads the same in either build.

  $ mask() { sed -E 's/--instrument-with ppx_windtrap\.mutate //'; }
  $ mkdir sub && cp ./suite_main.exe sub/
  $ run INSIDE_DUNE=1 FACADE_FIXTURE=property ./sub/suite_main.exe > out 2> err
  [1]
  $ grep 'replay:' out | scrub | mask
  replay: dune exec sub/suite_main.exe -- --seed SEED
  $ rm -r sub

dune exec reads a word without a slash as the name of a program to look
up, so a program at the project root keeps its ./. At the root of a
project that holds the program, the line runs as pasted and fails again
on the same case:

  $ mkdir proj && cp ./suite_main.exe proj/ && cd proj
  $ echo '(lang dune 3.0)' > dune-project
  $ run INSIDE_DUNE=1 FACADE_FIXTURE=property ./suite_main.exe > out 2> err
  [1]
  $ sed -n 's/^ *replay: //p' out | mask > replay
  $ scrub < replay
  dune exec ./suite_main.exe -- --seed SEED
  $ eval "run INSIDE_DUNE=1 FACADE_FIXTURE=property $(cat replay)" > again 2>&1
  [1]
  $ grep 'counterexample' out > before && grep 'counterexample' again > after
  $ diff before after && echo same counterexample
  same counterexample
  $ cd .. && rm -r proj

Two properties fail on cases the seed picks, beside a test that passes.
The one replay line, pasted, runs the three tests again, and each
property fails again on its case, shrunk to the same counterexample:

  $ run env FACADE_FIXTURE=properties ./suite_main.exe > out 2> err
  [1]
  $ grep 'replay:' out | scrub
  replay: ./suite_main.exe --seed SEED
  $ eval "run env FACADE_FIXTURE=properties $(sed -n 's/^replay: //p' out)" > again 2>&1
  [1]
  $ head -1 again | scrub
  fixture: 3 tests (seed SEED)
  $ grep 'counterexample' out > before && grep 'counterexample' again > after
  $ wc -l < before | tr -d ' '
  2
  $ diff before after && echo same counterexamples
  same counterexamples

A narrowed run's line restates its selection, and runs that selection
alone:

  $ run env FACADE_FIXTURE=properties ./suite_main.exe -f even > out 2> err
  [1]
  $ grep 'replay:' out | scrub
  replay: ./suite_main.exe --seed SEED -f 'even'
  $ eval "run env FACADE_FIXTURE=properties $(sed -n 's/^replay: //p' out)" > again 2>&1
  [1]
  $ head -1 again | scrub
  fixture: 1 test (seed SEED)

A run given --failed selected the record of the last failed tests, which
lies under its -o, so its line restates both. A run of the whole suite
records the two properties, and the line reruns them on their cases:

  $ run env FACADE_FIXTURE=properties ./suite_main.exe > /dev/null 2>&1
  [1]
  $ run env FACADE_FIXTURE=properties ./suite_main.exe --failed > out 2> err
  [1]
  $ grep 'replay:' out | scrub
  replay: ./suite_main.exe --seed SEED -o _logs --failed
  $ eval "run env FACADE_FIXTURE=properties $(sed -n 's/^replay: //p' out)" > again 2>&1
  [1]
  $ head -1 again | scrub
  fixture: 2 tests (seed SEED)
  $ grep 'counterexample' out > before && grep 'counterexample' again > after
  $ diff before after && echo same counterexamples
  same counterexamples
