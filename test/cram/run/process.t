What a worker process starts from. A run's first workers start the
test executable again, so a test sees what a fresh start of the program
builds. Each block runs one suite of process_main.exe in a directory of
its own, and prints standard output, then standard error under a
`--- stderr` line, with durations masked.

  $ scrub() {
  >   sed -E 's/ in [0-9.]+(m|µ)?s\./ in DURATION./; s/(process_main\.ml):[0-9]+/\1:N/g; s/^( *)[0-9]+ │/\1N │/'
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

A run that only a child forked during initialisation reaches, after the
child spawned a domain, gives its verdict and returns. The child can
neither fork a worker nor reach its run in a new start of the program,
so its tests run in the child:

  $ run PROCESS_SUITE=in_a_fork timeout 60 ../process_main.exe
  child: 1 passed in DURATION.
  the child exited with code 0
  in_a_fork: 1 passed in DURATION.

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
