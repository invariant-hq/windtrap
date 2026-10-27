# Running tests

This page shows how to run a suite, choose the tests that run, rerun the
tests that failed, and run under CI. The exit codes and the environment
are in the running section of
[`lib/windtrap.mli`](../../lib/windtrap.mli), and the last section lists
the flags.

The transcripts run the suite of [Resources and
structure](resources-and-structure.md), which ships as
`examples/06-resources-and-structure/` in windtrap's repository, and
print that directory's paths.

## Running every suite

`dune runtest` builds and runs every `(test)` stanza of the project. A
suite with nothing to report prints one line:

```
$ dune runtest
storage: 10 passed, 2 skipped, 1 expected failure in 3.3ms.
keys/keys.ml: 3 passed in 1.4ms.
```

Dune runs a stanza again only when something it depends on changed.
`dune runtest --force` runs every stanza, and `dune runtest DIR` runs
the stanzas under one directory, as `dune runtest test/unit` does.

## Running one suite with flags

`dune exec` runs one suite's executable, and the flags go after `--`. On
a terminal a dim line names the running test, and while a property
shrinks a counterexample it says so, with the steps taken. Under
`dune exec` the report is styled even when its output goes to a pipe or
a file, and `--color=never` turns the styling off. `-v` prints a line
per test:

```
$ dune exec examples/06-resources-and-structure/test_storage.exe -- -v -f 'process state'
storage: 3 tests
  PASS  process state › a config is written to a fresh directory  0.3ms
  PASS  process state › the token is read from the environment  0.1ms
  PASS  process state › a build writes in the working directory  0.1ms
3 passed in 1.9ms.
```

## Selecting tests by name

`-f PATTERN` keeps the tests whose path contains `PATTERN`, and
`-e PATTERN` drops them. Both repeat: a test is kept when it contains
one of the `-f` patterns and none of the `-e` patterns. A bare pattern
is one `-f`. `-l` prints the paths a selection keeps, and runs nothing:

```
$ dune exec examples/06-resources-and-structure/test_storage.exe -- -l -f server -e backup
server › it answers a ping
server › a first session gets id 1
server › reindexing keeps it running
```

## Selecting tests by tag

`--tag TAG` keeps the tests that carry every named tag, and
`--exclude-tag TAG` drops those that carry any. `tag "net" @@ t` tags
a test or a group, and `slow @@ t` carries the tag `slow`:

```
$ dune exec examples/06-resources-and-structure/test_storage.exe -- -l --tag slow
server › reindexing keeps it running
```

## Reading a selection that matches nothing

A selection given on the command line that keeps no test runs nothing,
says why, and exits `2`. The line above that is the command that lists
the suite's tests:

```
$ dune exec examples/06-resources-and-structure/test_storage.exe -- -f servr
list: dune exec examples/06-resources-and-structure/test_storage.exe -- -l
storage: no tests ran: filter "servr" matched none of 13 tests.
```

## Passing flags to dune runtest

Under `dune runtest` the executable gets no command line, and a
`WINDTRAP_*` variable, the flag's mirror, sets the flag instead. The
mirror is the long flag in capitals, as `WINDTRAP_FILTER` is for
`--filter`, but for `--arm`'s, `WINDTRAP_MUTATE_ARM`, and `--help`
lists each one. A mirror reaches every stanza of the project. A stanza
whose tests a mirror's filter misses says so and passes, as the
library's tests do here:

```
$ WINDTRAP_FILTER=gpu dune runtest --force
storage: 2 skipped in 1.4ms.
keys/keys.ml: no tests ran: filter "gpu" matched none of 3 tests.
```

To run one suite, name its directory, as in
`WINDTRAP_FILTER=gpu dune runtest test/storage --force`. The inline
tests of a library take only the mirrors. `-l`, `--failed`, `-x`, `-u`
and `--corrected` have none. A stanza meant to rerun when a variable
changes declares it, as `(deps (env_var WINDTRAP_PROP_COUNT))`.

## Reading a failure

To see a failure, make `Server.ping` answer `false` and run the tests
again. Dune prints the stanza above the output of a suite that fails.
The report opens on the number of tests, prints a block per failure as
its test ends, and closes on the counts:

```
$ dune runtest
File "examples/06-resources-and-structure/dune", line 3, characters 7-19:
3 |  (name test_storage)
           ^^^^^^^^^^^^
storage: 13 tests
──────────────────────── failures ────────────────────────
  FAIL  server › it answers a ping
    examples/06-resources-and-structure/server_tests.ml:9
      9 │ test "it answers a ping" (fun () ->

    expected  true
    actual    false
──────────────────────────────────────────────────────────

9 passed, 2 skipped, 1 expected failure, 1 failed in 2.8ms.
```

`-x` stops the run after the first failure.

## Rerunning the last failed tests

`--failed` runs the last failed tests. `dune runtest` and `dune exec`
keep one record, so after a failing `dune runtest`,
`dune exec test/test_storage.exe -- --failed` reruns its failures. The
two commands below pass `-o` to keep this page's record apart:

```
$ dune exec examples/06-resources-and-structure/test_storage.exe -- -o _build/rerun
storage: 13 tests
──────────────────────── failures ────────────────────────
  FAIL  server › it answers a ping
    examples/06-resources-and-structure/server_tests.ml:9
      9 │ test "it answers a ping" (fun () ->

    expected  true
    actual    false
──────────────────────────────────────────────────────────

9 passed, 2 skipped, 1 expected failure, 1 failed in 3.4ms.
$ dune exec examples/06-resources-and-structure/test_storage.exe -- -o _build/rerun --failed
storage: 1 test
──────────────────────── failures ────────────────────────
  FAIL  server › it answers a ping
    examples/06-resources-and-structure/server_tests.ml:9
      9 │ test "it answers a ping" (fun () ->

    expected  true
    actual    false
──────────────────────────────────────────────────────────

1 failed in 1.3ms.
```

`-l --failed` lists the tests `--failed` would run, and `--failed` with
nothing recorded runs nothing, says why, as
`--failed has nothing to rerun: the last run passed, so no failure is recorded`,
and exits `2` (see the command-line
section of `lib/windtrap.mli` for how a run updates the record). A run
never fails because it cannot read or write the record, and a record it
cannot read counts as empty.

## Seeing a test's output

The runner captures what a test writes to standard output and standard
error. A failing test's block shows the last lines it wrote after its
last `output ()` call under `captured output`, and `full log:` names the
file that holds all of them, under the log directory that `-o` sets. A
failing property or stateful test shows those of the run that failed on
its counterexample alone. `-s` turns the capture off, so the output
reaches the terminal as it is written, and a test that calls
`output ()`, as every `expect (output ())` and `let%expect_test` does,
fails.

## Focusing on one test

`focus` on a test or a group runs only the focused tests. With the
server's first test wrapped as `focus @@ test "it answers a ping" …`, one
test runs, and the run warns on standard error:

```
$ dune exec examples/06-resources-and-structure/test_storage.exe
storage: 1 passed in 1.2ms.
windtrap: warning: focus is active: 1 of 13 tests ran; remove the focus before committing
```

## Running under CI

The runner is under CI when `CI` is set to a true value (see the
command-line section of `lib/windtrap.mli`). A suite that holds a
`focus` is then refused, with the focus named:

```
$ CI=true dune exec examples/06-resources-and-structure/test_storage.exe
windtrap: focused tests committed (focus at examples/06-resources-and-structure/server_tests.ml:10); remove focus to run under CI
```

`-u` is refused under CI too, and a stale baseline fails the run even
under `--corrected` (see [Baselines](baselines.md)). With `GITHUB_ACTIONS` set as well, the
report is folded in a group, and each failure adds an annotation to the
workflow run:

```
$ CI=true GITHUB_ACTIONS=true dune exec examples/06-resources-and-structure/test_storage.exe
::group::storage
storage: 13 tests
──────────────────────── failures ────────────────────────
  FAIL  server › it answers a ping
    examples/06-resources-and-structure/server_tests.ml:9
      9 │ test "it answers a ping" (fun () ->

    expected  true
    actual    false
──────────────────────────────────────────────────────────
::endgroup::
::error file=examples/06-resources-and-structure/server_tests.ml,line=9,title=Test failure%3A server › it answers a ping::    examples/06-resources-and-structure/server_tests.ml:9%0A    expected  true%0A    actual    false

9 passed, 2 skipped, 1 expected failure, 1 failed in 2.6ms.
```

`--json PATH` writes the results as one JSON document, a test per
line, each with its verdict and its scope, which states what the test
ran: a property's cases, examples, discards, labels and `cover`
demands, a table's rows. A vacuous test carries its reason. The
document closes on the counts of the summary line. A `PATH` that ends
in `.json` is the file, and any other a directory that gets
`<suite>.json`; its mirror is `WINDTRAP_JSON`. The document's fields
are in `lib/report_json.mli`.

`--junit PATH` also writes the report as a JUnit file. A `PATH` that
ends in `.xml` is the file, and any other is a directory that gets
`<suite>.xml`. The mirror's relative path is read from the project root,
so `WINDTRAP_JUNIT=_build/junit dune runtest` writes one file per suite
under the project's `_build/junit`. `--shard K/N` keeps bucket `K` of `N` of the selection,
the same on every machine, so `N` jobs run each test once:

```
$ dune exec examples/06-resources-and-structure/test_storage.exe -- -l --shard 1/2
database › an insert adds one row
database › every inserted row is found
server › reindexing keeps it running
process state › a build writes in the working directory
```

## Reading the exit code

`run` returns `0`, `1` or `2`, and the suite's last line hands the code
to `exit`. The exit codes section of `Windtrap.run` says which runs
return which. A vacuous test, one that checked less than it claims,
returns `1` as a failure does: the summary and the report tell the two
apart, and a gate needs no other code to go red. Code under test that calls `exit` does not end the run;
the call fails its test.

## Bounding a test in time

Every attempt of a test has a limit. A test that declares no `~timeout`,
in a run given no `--timeout`, gets ten minutes, so a test that hangs
fails by name, `timed out after 600s`, instead of holding the run until
something outside it kills the build with no name. `--timeout SECONDS`
(`WINDTRAP_TIMEOUT`) sets the limit of every test that declares none,
and a test's own `~timeout`, or its group's, wins over both (see
[Resources and structure](resources-and-structure.md)). The limit is an
alarm, so a teardown still runs after a body that timed out; a test
still running three limits after it started, in C code the alarm cannot
reach, is killed with its worker and times out too.

`--in-process` has no default limit, since a debugger may hold a test as
long as it likes; a limit that `~timeout` or `--timeout` gives still
holds there. On Windows the tests run in the calling process with no
alarm, and no limit is enforced, the default included: a test that
hangs there hangs the run.

## Running tests in worker processes

The process that calls `run` runs no test. It starts a worker process,
which runs the tests one at a time, and reads each verdict from what the
worker reports, how it ends and how long it takes. A test that calls
`exit` or `Unix._exit`, crashes, raises `Out_of_memory` or outlives its
limit in C code fails, saying which, and the run goes on in a new
worker. A test that spawns a domain retires its worker after it, so the
domain cannot run into the next test.

A worker is the test executable started again, with the same command
line. Its module initialisation runs again, so a test sees what a fresh
start of the program builds, as under `--in-process`: a device, a
connection or a pool opened at declaration is the worker's own. Each
worker pays for that initialisation once, a worker that replaces a
retired one included, so expensive work at declaration belongs in a
`lazy` value or a `fixture`. What the initialisation prints appears
once, from the process that called `run`. A worker that ends before its
first test fails that test with how it ended and what it printed.

A suite must declare the same tests each time the program starts. Names
taken from the clock, a random generator or a directory listing that
changes give a worker another suite than its parent, and the tests it
was to run fail, saying so.

Only a program's first `run` starts the program again. A later `run`,
and a `run` in a process forked from the program, forks its workers
from the calling process before any test runs.

`-j N` runs the tests in `N` workers at once, each one test at a time.
The default is one: tests in two workers share no fixture, and they race
on the environment, the working directory and the files they write.
Under `-s` the tests run one at a time whatever `-j` says, so their
output reaches the terminal in order. The report, its order, the exit
code and the corrections are those of `-j 1`.

To step through one test in a debugger, run it in the process you
started, with `--in-process` and a filter that selects it alone:

```
$ dune exec examples/06-resources-and-structure/test_storage.exe -- --in-process -f 'a config is written'
storage: 1 passed in 0.2ms.
```

A selection of more than one test is refused, since the flag is a
gesture at one test, not a way to run a suite.

## Listing the flags

`--help` lists every flag with its mirror, and the variables that no
flag sets:

```
$ dune exec examples/06-resources-and-structure/test_storage.exe -- --help
test_storage.exe - windtrap test runner

usage: test_storage.exe [OPTIONS] [PATTERN...]

Each bare PATTERN is read as one -f PATTERN. (env VAR) after an option names the
variable that sets it for a run with no command line, such as dune runtest.

OPTIONS:
  -f PATTERN, --filter=PATTERN (env WINDTRAP_FILTER)
      Run only tests whose path contains PATTERN (repeatable: any of them).

  -e PATTERN, --exclude=PATTERN (env WINDTRAP_EXCLUDE)
      Skip tests whose path contains PATTERN (repeatable).

  --tag=TAG (env WINDTRAP_TAG)
      Run only tests tagged TAG (repeatable: all of them).

  --exclude-tag=TAG (env WINDTRAP_EXCLUDE_TAG)
      Skip tests tagged TAG (repeatable).

  --shard=K/N (env WINDTRAP_SHARD)
      Run only the Kth of N deterministic path-hash buckets.

  --failed
      Run only the last failed tests.

  -l, --list
      List selected tests without running them.

  -x, --fail-fast
      Stop after the first failure.

  --timeout=SECONDS (env WINDTRAP_TIMEOUT)
      Limit each attempt of a test that declares no timeout to SECONDS (default
      600; none under --in-process).

  --slow-threshold=SECONDS (env WINDTRAP_SLOW_THRESHOLD)
      Warn when an untagged test runs longer than SECONDS (0 disables).

  --seed=TOKEN (env WINDTRAP_SEED)
      Root seed for property tests (s1:<16 hex>).

  --prop-count=N (env WINDTRAP_PROP_COUNT)
      Raise every property's generated cases to at least N.

  --replay=TOKEN (env WINDTRAP_REPLAY)
      Check each selected property on the one case of TOKEN alone.

  --case=PATH
      Check the one selected property on the one case of the bytes of PATH.

  -u, --update
      Accept baseline changes in place (refused under CI).

  --corrected
      Write corrections as <file>.corrected, for dune promote.

  -s, --stream (env WINDTRAP_STREAM)
      Stream test output instead of capturing it.

  -j N, --jobs=N (env WINDTRAP_JOBS)
      Run the tests in N worker processes at once (default 1).

  --in-process
      Run the one selected test in this process, for a debugger.

  -v, --verbose (env WINDTRAP_VERBOSE)
      One status line per test.

  --junit=PATH (env WINDTRAP_JUNIT)
      Also write a JUnit XML report to PATH, or to PATH/<suite>.xml when PATH
      does not end in .xml.

  --json=PATH (env WINDTRAP_JSON)
      Also write a JSON report of the results and their scope to PATH, or to
      PATH/<suite>.json when PATH does not end in .json.

  --color=MODE (env WINDTRAP_COLOR)
      Color output: always, never or auto.

  -o DIR, --output=DIR (env WINDTRAP_OUTPUT)
      Root directory for capture logs.

  --mutate[=PREFIX,...] (env WINDTRAP_MUTATE)
      Test this executable's mutants, all or those under PREFIX.

  --lines=FILE:A-B,... (env WINDTRAP_LINES)
      Test only the mutants on these lines of these files (with --mutate).

  --diff=FILE (env WINDTRAP_DIFF)
      Test only the mutants on the lines that FILE, the output of git diff -U0,
      adds or changes (with --mutate).

  --arm=ID (env WINDTRAP_MUTATE_ARM)
      Run once with mutant ID armed.

  -V, --version
      Print the version and exit.

  -h, --help
      Print this help and exit.

ENVIRONMENT (no flag):
  WINDTRAP_PROJECT_ROOT
      Project root that baseline paths resolve under.

  WINDTRAP_COVERAGE_FILE
      Where an instrumented run writes its coverage dump.

  NO_COLOR
      Any value: never style output (--color auto).
```
