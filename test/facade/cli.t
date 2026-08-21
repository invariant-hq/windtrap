The command line before a suite runs: the two informational exits, and
the two ways a bad invocation is refused.

Every command goes through `run`, which states the child's whole
environment rather than inheriting one: a developer's CI, NO_COLOR or
WINDTRAP_* setting would otherwise reshape transcripts pinned below.
WINDTRAP_PROJECT_ROOT keeps the runs' capture logs and snapshot lookups
inside this sandbox.

  $ run() {
  >   env -i PATH="$PATH" WINDTRAP_COLOR=never WINDTRAP_SLOW_THRESHOLD=0 \
  >       WINDTRAP_COVERAGE=off \
  >       WINDTRAP_PROJECT_ROOT="$PWD" "$@"
  > }

--help prints the usage banner and exits 0 — the whole page is pinned
by test/unit's help.snap, so what is asserted here is that the facade
prints it, on stdout, and gets out of the way:

  $ run ./suite_main.exe --help > out 2> err
  $ sed -n '1p;3p' out
  suite_main.exe - windtrap test runner
  usage: suite_main.exe [OPTIONS] [PATTERN]
  $ cat err

--version prints one line and exits 0. The version itself is the
release watermark — "dev" in a working tree, a number in a distribution
tarball — so the shape is what this pins; a broken format leaves the
line unmatched and shows itself.

  $ run ./suite_main.exe --version > out 2> err
  $ sed -E 's/^(windtrap) [^ ]+$/\1 VERSION/' out
  windtrap VERSION
  $ cat err

An unknown flag exits 2, with the error and the usage on stderr and
nothing on stdout:

  $ run ./suite_main.exe --nosuchflag > out 2> err
  [2]
  $ cat out
  $ cat err
  unknown option '--nosuchflag'
  usage: suite_main.exe [OPTIONS] [PATTERN]

A flag one slip from a real one is named:

  $ run ./suite_main.exe --bial 1 > out 2> err
  [2]
  $ cat err
  unknown option '--bial'; did you mean '--bail'?
  usage: suite_main.exe [OPTIONS] [PATTERN]

A value the flag cannot take names the flag and what it expected:

  $ run ./suite_main.exe --bail x > out 2> err
  [2]
  $ cat out
  $ cat err
  invalid value 'x' for --bail: expected a positive integer
  usage: suite_main.exe [OPTIONS] [PATTERN]

Under `dune runtest` there is no command line and the mirrors are the
CLI, so a value arriving through the environment is refused with the
same sentence and the same code — naming the variable, not the flag.
This is the facade's second error exit: the first is the parse above,
this one is the resolution after it.

  $ run WINDTRAP_BAIL=nope ./suite_main.exe > out 2> err
  [2]
  $ cat err
  invalid value 'nope' for WINDTRAP_BAIL: expected a positive integer
  usage: suite_main.exe [OPTIONS] [PATTERN]
