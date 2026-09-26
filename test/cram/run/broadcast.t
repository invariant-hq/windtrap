What the environment broadcasts is no error of a suite that cannot
honour it. Under `dune runtest` every test stanza of a project runs its
executable from its own directory of the build context, with
INSIDE_DUNE set and the same WINDTRAP_* variables, and this session
lays out two stanzas the way dune does and runs them the way dune does.
POSIX only: one of them runs the mutation loop, which forks.

The project is a scratch directory outside the build directory this
session runs in, so that its runs find their own `_build` and write
nothing into the tree's. The unit stanza runs the fixture suite, its
action excluding the failing test as a stanza's action may; the mutants
stanza runs the instrumented suite. The fixture's file baseline is
planted in the build context, where a run under dune reads it.

  $ bin=$PWD
  $ root=$(cd "$(mktemp -d)" && pwd -P)
  $ cd "$root"
  $ mkdir -p _build/default/unit _build/default/mutants _build/default/test/cram/run
  $ cp "$bin/suite_main.exe" _build/default/unit/
  $ cp "$bin/mutant_main.exe" _build/default/mutants/
  $ echo 'hello from the fixture' > _build/default/test/cram/run/greeting.expected
  $ stanza() {
  >   dir=$1; shift
  >   (cd "_build/default/$dir" && env -i PATH="$PATH" \
  >       INSIDE_DUNE="$root/_build/default" WINDTRAP_COLOR=never \
  >       WINDTRAP_SLOW_THRESHOLD=0 "$@")
  > }
  $ runtest() {
  >   stanza unit "$@" ./suite_main.exe -e boom; echo "[unit: $?]"
  >   stanza mutants "$@" ./mutant_main.exe; echo "[mutants: $?]"
  > }
  $ scrub() {
  >   sed -E 's/ in [0-9.]+m?s\./ in DURATION./'
  > }

A filter meant for one suite empties the other. The emptied stanza says
so and exits 0:

  $ runtest WINDTRAP_FILTER=math 2>&1 | scrub
  fixture: 2 passed in DURATION.
  [unit: 0]
  mutant: no tests ran: filter "math" matched none of 1 test.
  list: dune exec --instrument-with ppx_windtrap.mutate mutants/mutant_main.exe -- -l
  [mutants: 0]

WINDTRAP_MUTATE reaches both stanzas. The fixture has no mutant under
the prefix, so it runs as it would without the variable and says why on
standard error, while the instrumented suite tests its mutant:

  $ runtest WINDTRAP_MUTATE=test/cram/run/mutant_subject.ml 2>&1 | scrub
  windtrap: WINDTRAP_MUTATE is set, but no mutant of this executable's catalogue is under test/cram/run/mutant_subject.ml, so the suite runs without mutation
  fixture: 4 passed in DURATION.
  [unit: 0]
  mutant: 1 passed in DURATION.
  mutants: 1 reached by this suite, 1 killed
  [mutants: 0]

A relative path in WINDTRAP_JUNIT or WINDTRAP_OUTPUT is read from the
project root, so both stanzas write under the root's _build:

  $ runtest WINDTRAP_JUNIT=_build/junit WINDTRAP_OUTPUT=_build/logs 2>&1 | scrub
  fixture: 4 passed in DURATION.
  [unit: 0]
  mutant: 1 passed in DURATION.
  [mutants: 0]
  $ ls _build/junit _build/logs
  _build/junit:
  fixture.xml
  mutant.xml
  
  _build/logs:
  fixture
  mutant

The scratch project leaves with the session:

  $ cd "$bin" && rm -rf "$root"
