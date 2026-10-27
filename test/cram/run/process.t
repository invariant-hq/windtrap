What a worker process starts from. A run's first workers start the
test executable again, so a test sees what a fresh start of the program
builds. Each block runs one suite of process_main.exe in a directory of
its own, and prints standard output, then standard error under a
`--- stderr` line, with durations masked.

  $ scrub() {
  >   sed -E 's/ in [0-9.]+(m|µ)?s\./ in DURATION./; s/ [0-9.]+(m|µ)?s after / DURATION after /; s/(process_main\.ml):[0-9]+/\1:N/g; s/^( *)[0-9]+ │/\1N │/'
  > }
  $ run() {
  >   rm -rf sandbox && mkdir sandbox && cd sandbox
  >   env -i PATH="$PATH" TMPDIR="${TMPDIR:-/tmp}" WINDTRAP_COLOR=never \
  >     WINDTRAP_SLOW_THRESHOLD=0 WINDTRAP_PROJECT_ROOT="$PWD" "$@" > ../out 2> ../err
  >   code=$?
  >   cd ..
  >   scrub < out
  >   if [ -s err ]; then echo '--- stderr'; scrub < err; fi
  >   return $code
  > }

A value made as the module initialises is the worker's own, in the
first worker, in one that replaced a retired worker, and at any number
of workers. What the initialisation prints appears once, from the
process that called `run`:

  $ run PROCESS_SUITE=own_pid ../process_main.exe
  declaring the suite
  own_pid: 11 passed in DURATION.

  $ run PROCESS_SUITE=own_pid ../process_main.exe -j 8
  declaring the suite
  own_pid: 11 passed in DURATION.

So it is in a bytecode executable, where the program the process runs
is `ocamlrun`, and a worker is `ocamlrun` started again on the same
bytecode file:

  $ run CAML_LD_LIBRARY_PATH="$PWD/../../../lib" PROCESS_SUITE=own_pid \
  >   ../process_main.bc
  declaring the suite
  own_pid: 11 passed in DURATION.

A run whose standard error is closed starts its workers all the same,
since the descriptors it hands them stay clear of the three standard
ones:

  $ env -i PATH="$PATH" TMPDIR="${TMPDIR:-/tmp}" WINDTRAP_COLOR=never \
  >   PROCESS_SUITE=own_pid ./process_main.exe 2>&- | scrub
  declaring the suite
  own_pid: 11 passed in DURATION.

A run that only a child forked during initialisation reaches, after the
child spawned a domain, gives its verdict and returns. The child can
neither fork a worker nor reach its run in a new start of the program,
so its tests run in the child. Where `timeout` exists (macOS has none),
it bounds the run, so a recursion would fail rather than hang:

  $ guard=$(command -v timeout > /dev/null && echo 'timeout 60' || :)
  $ run PROCESS_SUITE=in_a_fork $guard ../process_main.exe
  child: 1 passed in DURATION.
  the child exited with code 0
  in_a_fork: 1 passed in DURATION.

An `at_exit` function that module initialisation registers is each
process's own, and runs in each as it ends: here each start of the
program makes a file named after its pid and removes it, and a run
leaves none, at any number of workers:

  $ run PROCESS_SUITE=cleaned ../process_main.exe; ls sandbox/made
  cleaned: 2 passed in DURATION.

  $ run PROCESS_SUITE=cleaned ../process_main.exe -j 2; ls sandbox/made
  cleaned: 2 passed in DURATION.

One that raises in a worker fails the run, as no test does. Here the
test removed its worker's file first:

  $ run PROCESS_SUITE=cleaned_twice ../process_main.exe > log
  [1]
  $ sed -E 's|made/[0-9]+|made/PID|; /^ *(Raised|Called from)/d' log; ls sandbox/made
  cleaned_twice: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  fixture release
      [release]
      an at_exit function that module initialisation registered raised
      uncaught exception:
        Sys_error("made/PID: No such file or directory")
  ──────────────────────────────────────────────────────────
  
  1 passed, 1 failed in DURATION.

A second run in a process that spawned a domain before the first: its
worker starts the program again, and the first run, which the worker
does not make again, returns the code it returned in the process that
made it:

  $ run PROCESS_SUITE=second_run ../process_main.exe
  first: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  fails
      test/cram/run/process_main.ml:N
      expected  1
      actual    2
  ──────────────────────────────────────────────────────────
  
  1 failed in DURATION.
  second_run: 1 passed in DURATION.

A suite must declare the same tests at each start of the program. One
named after a directory listing that its declaration changes fails, and
says so:

  $ run PROCESS_SUITE=listing ../process_main.exe
  listing: 1 test
  ──────────────────────── failures ────────────────────────
    FAIL  reads 0
      test/cram/run/process_main.ml:N
      its worker process, a new start of the test executable, exited with code 2 before it began to run tests
      captured output (1 line):
        windtrap: this worker started the test executable again, and its module initialisation declared other tests than the run's. A suite must declare the same tests each time the executable starts: no test's name, or whether it is declared, may come from the clock, a random generator or a directory listing that changes.
  ──────────────────────────────────────────────────────────
  
  1 failed in DURATION.
  [1]

A worker that ends before it runs a test fails the test with how it
ended and what it wrote, which the run shows nowhere else:

  $ run PROCESS_SUITE=held ../process_main.exe
  opening the device
  held: 2 tests
  ──────────────────────── failures ────────────────────────
    FAIL  uses the device
      test/cram/run/process_main.ml:N
      its worker process, a new start of the test executable, exited with code 3 before it began to run tests
      captured output (2 lines):
        opening the device
        the device is held by another process
  
    FAIL  uses it again
      test/cram/run/process_main.ml:N
      its worker process, a new start of the test executable, exited with code 3 before it began to run tests
      captured output (2 lines):
        opening the device
        the device is held by another process
  ──────────────────────────────────────────────────────────
  
  2 failed in DURATION.
  [1]

A worker whose declaration waits on what the run's own process holds,
here a lock, never reaches its run, and nothing bounds that wait. An
interrupt says that a worker was still in module initialisation, how
long after it started, and what it wrote:

Each start of the executable appends a line to `starts` before it
takes the lock, so the signal comes once the worker has started. A job in
the background of a script ignores an interrupt, so this one is a
termination:

  $ rm -rf sandbox && mkdir sandbox && cd sandbox
  $ sh -c '
  >   env -i PATH="$PATH" TMPDIR="${TMPDIR:-/tmp}" WINDTRAP_COLOR=never \
  >     WINDTRAP_PROJECT_ROOT="$PWD" PROCESS_SUITE=locked \
  >     ../process_main.exe > ../out 2> ../err &
  >   pid=$!
  >   until [ "$(cat starts 2> /dev/null | wc -l)" -ge 2 ]; do sleep 0.05; done
  >   kill -TERM $pid; wait $pid; echo "[$?]"' 2> /dev/null; cd ..
  > scrub < out; echo '--- stderr'; scrub < err
  [143]
  taking the lock
  locked: 1 not run in DURATION.
  --- stderr
  windtrap: interrupted while a worker was still in module initialisation, DURATION after it started, and it wrote:
    taking the lock
