Baselines under CI, seen from outside the process: what a run writes,
the code a dune action reads, and the diff? that follows a --corrected
run. A stanza is laid out by hand as dune would run it: the action
`(progn (run %{test} --corrected) (diff? FILE FILE.corrected))` fails
when the run fails or when the diff? finds a difference. The fixture's
sources are copied under the project root, where a correction lands.
Each block is one situation and the verdict the run gives it (see
doc/dev/testing.md).

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
  $ mkdir -p test/cram/verdicts/inline
  $ cp verdict_main.ml test/cram/verdicts/
  $ cp inline/hello.ml test/cram/verdicts/inline/
  $ source=test/cram/verdicts/verdict_main.ml
  $ stanza() {
  >   file=$1; shift
  >   run "$@" --corrected > /dev/null
  >   code=$?
  >   tail -1 out | scrub
  >   [ $code = 0 ] || { echo "the run failed: [$code]"; return 1; }
  >   [ -e "$file.corrected" ] || return 0
  >   diff -U0 "$file" "$file.corrected" | grep '^[-+] ' && { echo "the diff? differs"; return 1; }
  >   return 0
  > }

A literal never recorded, under a stanza that diffs its correction,
under CI: the run exits 0 with its correction written, and the diff?
fails the action:

  $ stanza "$source" CI=true VERDICT_SUITE=baseline_literal ./verdict_main.exe
  1 failed, 1 correction written in DURATION.
  -      [ test "never recorded" (fun () -> expect "hello" @@ __POS_OF__ {||}) ]
  +      [ test "never recorded" (fun () -> expect "hello" @@ __POS_OF__ {| hello |}) ]
  the diff? differs
  [1]
  $ rm "$source.corrected"

An inline expectation never recorded, under CI: the partition writes
its correction and exits 0, and the diff? fails the action:

  $ run CI=true ./inline/runner.exe inline-test-runner inline -partition hello.ml > /dev/null; echo "exit $?"
  exit 0
  $ diff -U0 test/cram/verdicts/inline/hello.ml test/cram/verdicts/inline/hello.ml.corrected | grep '^[-+] '
  -  [%expect]
  +  [%expect {| hello, world |}]
  $ rm test/cram/verdicts/inline/hello.ml.corrected

A file baseline that is missing under --corrected gets no correction
and fails the run, under CI:

  $ brief CI=true VERDICT_SUITE=baseline_file ./verdict_main.exe --corrected
    FAIL  reads a missing file
  1 failed in DURATION.
  [1]
  $ grep 'accept:' out
      accept: touch 'test/cram/verdicts/never.expected' && dune runtest; dune promote test/cram/verdicts/never.expected
  $ test -e test/cram/verdicts/never.expected || echo 'nothing written'
  nothing written

A stanza that runs its suite without --corrected fails on a mismatch:

  $ brief VERDICT_SUITE=baseline_plain ./verdict_main.exe
    FAIL  mismatches
  1 failed in DURATION.
  [1]
  $ grep 'expect: mismatch' out
      expect: mismatch

Run directly under CI the literal fails; -u is refused under CI, and
cannot be combined with --corrected:

  $ brief CI=true VERDICT_SUITE=baseline_literal ./verdict_main.exe
    FAIL  never recorded
  1 failed in DURATION.
  [1]
  $ run CI=true VERDICT_SUITE=baseline_literal ./verdict_main.exe -u
  --- stderr
  windtrap: baseline update refused: CI is set. -u rewrites baselines in place, which is a developer's edit; under CI run with --corrected and accept with dune promote.
  [1]
  $ run VERDICT_SUITE=baseline_literal ./verdict_main.exe -u --corrected
  --- stderr
  windtrap: options '-u' and '--corrected' cannot be combined
  usage: verdict_main.exe [OPTIONS] [PATTERN...]
  [2]

A stanza that runs --corrected with no diff? after it passes, though its
literal was never recorded: the action is the run alone, and the run
exits 0:

  $ brief CI=true VERDICT_SUITE=baseline_literal ./verdict_main.exe --corrected; echo "exit $?"
    FAIL  never recorded
  1 failed, 1 correction written in DURATION.
  exit 0
  $ rm "$source.corrected"

None of the runs above changed a source, inline or not:

  $ cmp verdict_main.ml "$source" && cmp inline/hello.ml test/cram/verdicts/inline/hello.ml && echo 'sources untouched'
  sources untouched

CI set to 0, false or off is not CI, and -u accepts in place, which
rewrites the source:

  $ for ci in 0 false off; do
  >   rm -f "$source" && cp verdict_main.ml "$source"
  >   run CI=$ci VERDICT_SUITE=baseline_literal ./verdict_main.exe -u > /dev/null
  >   echo "CI=$ci: [$?] $(grep -c '{| hello |}' "$source") literal accepted"
  > done
  CI=0: [0] 1 literal accepted
  CI=false: [0] 1 literal accepted
  CI=off: [0] 1 literal accepted
