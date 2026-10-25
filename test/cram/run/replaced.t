A program put in the place of the test executable during a run, as
dune's watch mode does when it rebuilds, starts no worker of that run:
on Linux a worker is a new start of the executable the run started as.
The first test renames another program over the executable and retires
its worker, so the second runs in a new start:

  $ cp process_main.exe copy.exe && chmod u+w copy.exe
  $ printf '#!/bin/sh\necho impostor\n' > impostor && chmod +x impostor
  $ env -i PATH="$PATH" TMPDIR="${TMPDIR:-/tmp}" WINDTRAP_COLOR=never \
  >   WINDTRAP_SLOW_THRESHOLD=0 PROCESS_SUITE=replaced ./copy.exe | sed -E 's/ in [0-9.]+(m|µ)?s\./ in DURATION./'
  replaced: 2 passed in DURATION.

