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
storage: 12 passed, 2 skipped, 1 expected failure in 2.8ms.
keys/keys.ml: 3 passed in 0.8ms.
```

Dune runs a stanza again only when something it depends on changed.
`dune runtest --force` runs every stanza, and `dune runtest DIR` runs
the stanzas under one directory, as `dune runtest test/unit` does.

## Running one suite with flags

`dune exec` runs one suite's executable, and the flags go after `--`. On
a terminal a dim line names the running test. Under `dune exec` the report is styled even when
its output goes to a pipe or a file, and `--color=never` turns the
styling off. `-v` prints a line per test:

```
$ dune exec examples/06-resources-and-structure/test_storage.exe -- -v -f 'process state'
storage: 3 tests
  PASS  process state › a config is written to a fresh directory  0.6ms
  PASS  process state › the token is read from the environment  0.1ms
  PASS  process state › a build writes in the working directory  0.5ms
3 passed in 1.4ms.
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
`--exclude-tag TAG` drops those that carry any. A test declared with
`slow` carries the tag `slow`:

```
$ dune exec examples/06-resources-and-structure/test_storage.exe -- -l --tag slow
server › reindexing keeps it running
```

## Reading a selection that matches nothing

A selection given on the command line that keeps no test runs nothing,
says why, and exits `2`. The last line is the command that lists the
suite's tests:

```
$ dune exec examples/06-resources-and-structure/test_storage.exe -- -f servr
storage: no tests ran: filter "servr" matched none of 15 tests.
list: dune exec examples/06-resources-and-structure/test_storage.exe -- -l
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
storage: 2 skipped in 0.4ms.
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
File "examples/06-resources-and-structure/dune", line 2, characters 7-19:
2 |  (name test_storage)
           ^^^^^^^^^^^^
storage: 15 tests
──────────────────────── failures ────────────────────────
  FAIL  server › it answers a ping
    examples/06-resources-and-structure/server_tests.ml:9
      9 │ test "it answers a ping" (fun () ->

    expected  true
    actual    false
──────────────────────────────────────────────────────────

11 passed, 2 skipped, 1 expected failure, 1 failed in 2.3ms.
keys/keys.ml: 3 passed in 0.5ms.
```

`-x` stops the run after the first failure.

## Rerunning the last failed tests

`--failed` runs the last failed tests. `dune runtest` and `dune exec`
keep one record, so after a failing `dune runtest`,
`dune exec test/test_storage.exe -- --failed` reruns its failures. The
two commands below pass `-o` to keep this page's record apart:

```
$ dune exec examples/06-resources-and-structure/test_storage.exe -- -o _build/rerun
storage: 15 tests
──────────────────────── failures ────────────────────────
  FAIL  server › it answers a ping
    examples/06-resources-and-structure/server_tests.ml:9
      9 │ test "it answers a ping" (fun () ->

    expected  true
    actual    false
──────────────────────────────────────────────────────────

11 passed, 2 skipped, 1 expected failure, 1 failed in 1.9ms.
$ dune exec examples/06-resources-and-structure/test_storage.exe -- -o _build/rerun --failed
storage: 1 test
──────────────────────── failures ────────────────────────
  FAIL  server › it answers a ping
    examples/06-resources-and-structure/server_tests.ml:9
      9 │ test "it answers a ping" (fun () ->

    expected  true
    actual    false
──────────────────────────────────────────────────────────

1 failed in 0.6ms.
```

`-l --failed` lists the tests `--failed` would run, and `--failed` with
nothing recorded runs nothing and exits `2` (see the command-line
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
server's first test wrapped as `focus (test "it answers a ping" …)`, one
test runs, and the run warns on standard error:

```
$ dune exec examples/06-resources-and-structure/test_storage.exe
storage: 1 passed in 1.0ms.
windtrap: warning: focus is active: 1 of 15 tests ran; remove the focus before committing
```

## Running under CI

The runner is under CI when `CI` is set to a true value (see the
command-line section of `lib/windtrap.mli`). A suite that holds a
`focus` is then refused, with the focus named:

```
$ CI=true dune exec examples/06-resources-and-structure/test_storage.exe
windtrap: focused tests committed (focus at examples/06-resources-and-structure/server_tests.ml:10); remove focus to run under CI
```

`-u` is refused under CI too. With `GITHUB_ACTIONS` set as well, the
report is folded in a group, and each failure adds an annotation to the
workflow run:

```
$ CI=true GITHUB_ACTIONS=true dune exec examples/06-resources-and-structure/test_storage.exe
::group::storage
storage: 15 tests
──────────────────────── failures ────────────────────────
  FAIL  server › it answers a ping
    examples/06-resources-and-structure/server_tests.ml:9
      9 │ test "it answers a ping" (fun () ->

    expected  true
    actual    false
──────────────────────────────────────────────────────────
::endgroup::
::error file=examples/06-resources-and-structure/server_tests.ml,line=9,title=Test failure%3A server › it answers a ping::    examples/06-resources-and-structure/server_tests.ml:9%0A    expected  true%0A    actual    false

11 passed, 2 skipped, 1 expected failure, 1 failed in 20ms.
```

`--junit PATH` also writes the report as a JUnit file. A `PATH` that
ends in `.xml` is the file, and any other is a directory that gets
`<suite>.xml`. The mirror's relative path is read from the project root,
so `WINDTRAP_JUNIT=_build/junit dune runtest` writes one file per suite
under the project's `_build/junit`. `--shard K/N` keeps bucket `K` of `N` of the selection,
the same on every machine, so `N` jobs run each test once:

```
$ dune exec examples/06-resources-and-structure/test_storage.exe -- -l --shard 1/2
database › an insert adds one row
database › a name is stored › alice
database › a name is stored › bob
database › every inserted row is found
server › reindexing keeps it running
process state › a build writes in the working directory
```

## Reading the exit code

`run` returns `0`, `1` or `2`, and the suite's last line hands the code
to `exit`. The exit codes section of `Windtrap.run` says which runs
return which. Code under test that calls `exit` does not end the run;
the call fails its test.

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
      Default per-test timeout in seconds.

  --slow-threshold=SECONDS (env WINDTRAP_SLOW_THRESHOLD)
      Warn when an untagged test runs longer than SECONDS (0 disables).

  --seed=TOKEN (env WINDTRAP_SEED)
      Root seed for property tests (s1:<16 hex>).

  --prop-count=N (env WINDTRAP_PROP_COUNT)
      Generated cases per property.

  -u, --update
      Accept baseline changes in place (refused under CI).

  --corrected
      Write corrections as <file>.corrected, for dune promote.

  -s, --stream (env WINDTRAP_STREAM)
      Stream test output instead of capturing it.

  -v, --verbose (env WINDTRAP_VERBOSE)
      One status line per test.

  --junit=PATH (env WINDTRAP_JUNIT)
      Also write a JUnit XML report to PATH, or to PATH/<suite>.xml when PATH
      does not end in .xml.

  --color=MODE (env WINDTRAP_COLOR)
      Color output: always, never or auto.

  -o DIR, --output=DIR (env WINDTRAP_OUTPUT)
      Root directory for capture logs.

  --mutate[=PREFIX,...] (env WINDTRAP_MUTATE)
      Test this executable's mutants, all or those under PREFIX.

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
