The runs of an instrumented suite, and the JUnit file each leaves. The
loop's own scenarios are test/instr/loop's; what this session pins is
what --junit does beside --mutate and --arm. POSIX only: the loop
forks, and Windows refuses it.

  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_PROJECT_ROOT="$PWD" \
  >       WINDTRAP_OUTPUT="$PWD/_logs" "$@"
  > }
  $ scrub() {
  >   sed -E 's/ in [0-9.]+m?s\./ in DURATION./'
  > }

A mutation run writes no JUnit file: its output is the verdict on the
mutants, not an outcome of the tests. The run selects its one test by
name, and both runs keep their logs under -o, so that neither writes
into the build directory the session runs in.

  $ run ./mutant_main.exe --mutate -f adds -o logs --junit loop.xml > out 2> err
  $ scrub < out
  mutant: 1 passed in DURATION.
  mutants: 1 reached by the 1 selected test, 1 killed
  $ cat err
  windtrap: verdicts not saved: this run's selection narrows the suite, and a partial run's verdicts would stand in the project merge as the whole.
  $ test -e loop.xml || echo 'no report'
  no report

A run with one mutant armed is an ordinary run with its exit code, and
it writes its JUnit file like one: the test the mutant fails is a
failure there too.

  $ run ./mutant_main.exe --arm test/cli/mutant_subject.ml:9:14:sub -o logs --junit armed.xml > out 2> err
  [1]
  $ head -1 out
  mutant test/cli/mutant_subject.ml:9:14:sub armed: a + b → a - b
  $ cat err
  $ grep -o -e '<testcase name="[^"]*"' -e '<failure message="[^"]*"' armed.xml
  <testcase name="adds"
  <failure message="expected 4, got 0"
