Code under test that takes the process down or out of reach: a write
at address 0, and a loop in C that never returns to OCaml while its
test's limit passes. Each block is one situation and the verdict the
run gives it.

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

A test that crashes its worker fails, saying by which signal, and the
run goes on:

  $ crash VERDICT_SUITE=segfault
  [1]
  $ scrub < out; cat err
  segfault: 3 tests
  ──────────────────────── failures ────────────────────────
    FAIL  writes at address 0
      test/cram/verdicts/verdict_main.ml:N
        N │ test "writes at address 0" (fun () -> poke 0);
  
      the test ended its worker process, which was killed by SIGSEGV
  ──────────────────────────────────────────────────────────
  
  2 passed, 1 failed in DURATION.

A C loop that holds its test past its limit, where the alarm cannot
reach it, is killed with its worker, and the test times out before the
loop has run to its end:

  $ brief VERDICT_SUITE=c_loop ./verdict_main.exe
    FAIL  loops in C
  1 passed, 1 failed in DURATION.
  [1]
  $ grep 'the C loop ran to its end' out || echo 'not reached'
  not reached
  $ grep 'timed out' out | scrub
      timed out after 0.05s

A test that declares no limit, in a run given no --timeout, has the
default limit of ten minutes. VERDICT_DEFAULT_LIMIT puts a twentieth of
a second in its place: the loop is killed with its worker past three
times that, and the test times out by the default:

  $ brief VERDICT_SUITE=c_loop_default VERDICT_DEFAULT_LIMIT=0.05 ./verdict_main.exe
    FAIL  loops in C
  1 passed, 1 failed in DURATION.
  [1]
  $ grep 'the C loop ran to its end' out || echo 'not reached'
  not reached
  $ grep 'timed out' out | scrub
      timed out after 0.05s

--in-process has no default limit, so the loop runs to its end and the
test passes:

  $ run VERDICT_SUITE=c_loop_default VERDICT_DEFAULT_LIMIT=0.05 ./verdict_main.exe --in-process -s -f 'loops in C'
  the C loop ran to its end
  c_loop_default: 1 passed in DURATION.
