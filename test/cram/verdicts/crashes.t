Code under test that takes the process down or out of reach: a write
at address 0, and a loop in C that never returns to OCaml while its
test's limit passes. Each block pins a defect as it stands today (see
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

No core file is left behind:

  $ ulimit -c 0

The crashing child's streams are given inside it, so that the shell
that waits for it says nothing of the signal, whichever shell it is:

  $ crash() {
  >   { env -i PATH="$PATH" TMPDIR="${TMPDIR:-/tmp}" WINDTRAP_COLOR=never \
  >       WINDTRAP_OUTPUT="$PWD/_logs" "$@" sh -c 'exec ./verdict_main.exe > out 2> err'; } 2> /dev/null
  > }

Held for the redesign (doc/dev/testing.md): a test that crashes the process ends the run with the signal's 139 and no output at all, not even the tests before it.

  $ crash VERDICT_SUITE=segfault
  [139]
  $ cat out err

Held for the redesign (doc/dev/testing.md): a C loop holds its test past its limit, so the test times out only once the loop has run to its end.

  $ brief VERDICT_SUITE=c_loop ./verdict_main.exe
    FAIL  loops in C
  1 passed, 1 failed in DURATION.
  [1]
  $ grep -A2 'timed out' out | scrub
      timed out after 0.05s
      captured output (1 line):
        the C loop ran to its end
