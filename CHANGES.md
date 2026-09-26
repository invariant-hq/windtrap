## Unreleased

Windtrap 0.2.0 is about two questions a test suite should answer. When
a test fails: why? A failed comparison prints both values and marks what
changed, a failing property prints the command that replays it, and a
stale expectation is corrected for you and accepted with `dune promote`.
When the suite passes: would it catch a bug? Coverage shows the code no
test runs, and mutation testing, new in this release, shows the changes
to the code that no test notices. Stateful testing is new too: it checks
a system against a model with generated sequences of calls.

Getting there took a rewrite, so a 0.1 suite needs changes to build: the
interface, the runner, the ppx and the coverage backend are new. Every
line below compares with 0.1.0, and
[`doc/manual/migrating-from-0.1.md`](doc/manual/migrating-from-0.1.md)
maps each 0.1 spelling to its replacement.

### Highlights

A failed assertion prints both values with its witness's printer and
marks what changed, for every witness: the changed span of a one-line
value, or a line diff of a multi-line one. An assertion without
`~__POS__` is located from the call stack. New verbs unwrap an option or
a result, compare with a bound and search a string, so a claim that 0.1
wrote with `is_true` now prints its values.

```ocaml
let user = require_some (Store.find store "alice") in
less int ~than:3 user.failed_logins
```

`prop` checks a law over a generator, and the law asserts with the
verbs, so a counterexample is reported with its assertion's diff. A
witness no longer carries a generator, and `Gen` builds every generated
value. A run draws one seed, prints it when a property runs, and a
failing property ends on a `replay:` command.

```ocaml
prop "rev is an involution" Gen.(list int) (fun l ->
    equal (list int) l (List.rev (List.rev l)))
```

`stateful` tests a system with generated programs of calls, each checked
against a pure model. A failing program shrinks by removing calls and
shrinking their arguments, and prints as a table of its calls.

```ocaml
call "pop" ~pre:(fun m -> m <> []) ~next:List.tl
  (fun m q -> equal int (List.hd m) (Bounded_queue.pop q))
```

A baseline is the literal of an `expect` call or the file an
`expect_file` call names, and the `snapshot` family is removed. A
mismatch records its diff and the test goes on, so one run reports every
stale expectation. Under dune a `--corrected` run writes `.corrected`
files for `dune promote`; elsewhere `-u` rewrites the baselines in place.

```ocaml
expect (output ()) @@ __POS_OF__ {|3 items in the cart|}
```

The instrumentation backend `ppx_windtrap.mutate` compiles every mutant
of a library into the test executable, each off until armed. `--mutate`
runs the suite, then each mutant with the tests that reached it, and
prints every mutant that no test failed on.

```
$ dune exec --instrument-with ppx_windtrap.mutate test/test_calc.exe -- --mutate
```

`run` returns the exit code, and a suite's main passes it to `exit`.
`run` takes no configuration arguments, so the command-line flags and
their `WINDTRAP_*` mirrors are the whole configuration.

```ocaml
let () = exit (run "mylib" [ parsing; printing ])
```

### Declaring tests

- (breaking) `run : ?argv:string array -> string -> test list -> int` returns the exit code and never calls `exit`; a suite ends on `let () = exit (run "mylib" tests)` (doc/manual/running-tests.md#reading-the-exit-code).
- (breaking) `run` takes `?argv` alone; `~quick`, `~bail`, `~fail_fast`, `~filter`, `~exclude`, `~tags`, `~exclude_tags`, `~failed`, `~list_only`, `~seed`, `~timeout`, `~prop_count`, `~format`, `~junit`, `~stream`, `~output_dir`, `~update`, `~snapshot_dir` and `type format` are removed, and the flags, their mirrors or a synthetic `~argv` configure a run.
- `run` raises `Invalid_argument` when called while a run executes, as from a test body; two runs in a row in one process are allowed.
- (breaking) A test body returns `unit`; a body that computes a value ends on an assertion or on `ignore`.
- (breaking) `?pos` and `?here` are `?__POS__` on every constructor and every verb, and `~__POS__` passes the call's position; without it the position comes from the call stack (with `-g`, dune's default), and `type here` is removed (doc/manual/assertions.md#locating-a-failing-assertion).
- (breaking) `Tag` is removed; tags are a `string list`, as in `~tags:[ "net" ]`, and a group's tags are added to those of every test under it.
- (breaking) `slow name fn` is `test` with the tag `"slow"`, an ordinary tag that `--exclude-tag slow` leaves out; a test under a slow group can no longer be marked quick.
- (breaking) The tag `"disabled"` no longer deselects a test, and no tag has a meaning for selection; `skip ()` in the body skips a test and `--exclude-tag` leaves it out.
- (breaking) `group` takes no `~setup`, `~teardown`, `~before_each` or `~after_each`, and no user code runs outside a test; `bracket` and `scoped` give each test a resource, and `fixture` shares one across the run (doc/manual/resources-and-structure.md#giving-each-test-its-own-resource).
- `test`, `group`, `slow`, `cases`, `bracket` and `scoped` take `?__POS__`, `?tags`, `?timeout` and `?retries`; on a group, `timeout` and `retries` apply to each test under it that declares none, as in `group ~timeout:30. "integration" tests`.
- (breaking) `test`, `group`, `slow`, `cases`, `bracket` and `scoped` raise `Invalid_argument` when applied if `~timeout` is not finite and positive or `~retries` is negative; 0.1 ran `~timeout:0.` as a one-second alarm.
- (breaking) `cases ~name base inputs fn` takes a naming function in place of a witness; each child is the test `name input` under the group `base`, which `-f` selects alone, and two inputs with one name make `run` refuse the suite.
- (breaking) `ftest` and `fgroup` are removed; `focus t` focuses any test or group, and the tests outside the focus have no row and no count (doc/manual/running-tests.md#focusing-on-one-test).
- (breaking) Under `CI`, `run` refuses a suite that holds a `focus`, printing `windtrap: focused tests committed (focus at <file:line>); remove focus to run under CI`, and returns 1.
- `xfail ?reason t` marks a test or group as expected to fail; its failure leaves the exit code, `-x` and `--failed` alone, and a pass fails with `expected to fail (<reason>), but the test passed` (doc/manual/resources-and-structure.md#keeping-a-known-bug-in-the-suite).

### Assertions

- (breaking) The value `testable ~pp ?equal ?gen ?check ()`, `Testable.gen`, `Testable.with_gen`, `Testable.check` and `Testable.check_result` are removed; `Testable.make ~pp ~equal` takes a required equality and no `()`, and `Testable.structural ~pp` compares with `Stdlib.( = )`.
- (breaking) `of_equal` and `contramap` live in `Testable` alone; `Testable.of_equal` prints `<abstract>`, and `slist` prints both sides sorted.
- (breaking) The witnesses `seq`, `lazy_t`, `small_int` and `nat` are removed; `Testable.contramap List.of_seq (list t)` and `Testable.contramap Lazy.force t` replace the first two, and `int` the others.
- (breaking) `Pp` is removed from the interface; a printer has type `'a printer`, which is `Format.formatter -> 'a -> unit`.
- (breaking) `some`, `ok`, `error` and `no_raise` are removed; `some t e v` is `equal (option t) (Some e) v`, `ok t e r` is `equal t e (require_ok r)`, `error t e r` is `equal t e (require_error r)`, and `no_raise fn` is `fn ()`.
- (breaking) `raises_invalid_arg` and `raises_failure` are removed; `raises (Invalid_argument "m") fn` compares the message and diffs the two messages of an `Invalid_argument`, `Failure` or `Sys_error`.
- `Exn.invalid_arg`, `Exn.failure` and `Exn.sys_error` are predicates for `raises_match`, each with `?substring`.
- `raises` and `raises_match` report a wrong exception with the raised one's backtrace.
- `raises` and `raises_match` let a `skip`, a timeout, an intercepted `exit` and `assume` pass through; 0.1's `raises` reported a skip inside it as `Wrong exception raised`, and a `raises_match` that accepted any exception swallowed it.
- (breaking) Under `float eps` and `float_rel`, NaN equals nothing, where 0.1 found NaN equal to NaN; `float eps` raises `Invalid_argument` unless `eps > 0.`, and `float_rel` when a tolerance is negative or NaN or both are zero; exact equality is `float_exact`.
- `float_rel` no longer finds an infinity equal to every float.
- `float_exact` compares floats bit for bit and prints the shortest decimal that round-trips.
- `text` is a string witness printed verbatim, so two multi-line texts fail with a line diff.
- A failure prints `expected` and `actual` and marks what changed for every witness, from the two printed values: the changed spans of one-line values, and a unified diff with `@@` hunks for multi-line ones (doc/manual/assertions.md#comparing-two-values).
- A changed span is bold in its side's colour, and a `~` line marks it unless the report is coloured on a terminal; two unequal values that print alike are reported as `both sides render as: <v>`.
- `~msg` prints a line above the values, where 0.1 printed it in place of the headline (`Values are not equal`, …), which is removed.
- A failure prints its location as `file:line`, then the source line of the call.
- An uncaught exception prints as `uncaught exception:` with its backtrace, which `run` records without `OCAMLRUNPARAM=b`, and dune's `Dune__exe__` prefix is dropped from exception names.
- `less`, `at_most`, `greater` and `at_least` compare under the witness's order, which the base witnesses carry and `Testable.with_compare` gives; a witness without an order raises `Invalid_argument`.
- `require_some`, `require_ok`, `require_error` and `require_match` assert a shape and return its payload (doc/manual/assertions.md#unwrapping-an-option-or-a-result).
- `is_none`, `is_ok` and `is_error` take `?pp` and print the payload they did not want, as `<abstract>` without it.
- `starts_with ~affix`, `ends_with ~affix`, `contains ~sub`, `not_contains ~sub` and `in_order ~subs` search a string, and a failure says where the needle was or was not found in an excerpt of the haystack.
- `satisfies ?claim t pred v` asserts `pred v`, and `mem t x xs` asserts that `xs` holds `x`; both print the values with `t`'s printer.

### Property testing

- (breaking) `prop ?__POS__ ?tags ?timeout ?count ?max_discard ?examples name gen law` takes a `Gen.t` and a law `'a -> unit` that asserts with the verbs; a 0.1 boolean law becomes `fun l -> is_true (law l)`.
- (breaking) `prop'`, `prop2`, `prop3` and `prop4` are removed; `prop'` is `prop`, and several inputs are one tuple from `Gen.pair`, `Gen.triple` or `Gen.quad`.
- (breaking) `~config` is removed; `?count` and `?max_discard` replace it, the seed comes from `--seed`, and shrinking stops at 10,000 steps.
- (breaking) `Gen.t` is abstract and built from the combinators alone; `Gen.make_primitive`, `Gen.no_shrink`, `Gen.add_shrink_invariant` and `Gen.find` are removed.
- (breaking) `Gen.oneof`, `Gen.oneofl` and `Gen.pure` are `Gen.one_of`, `Gen.of_list` and `Gen.constant`; `Gen.list_size sg g` and `Gen.string_size sg cg` are `Gen.list ~size:sg g` and `Gen.string_of ~size:sg cg`, and `Gen.array` takes `?size` too.
- (breaking) `Gen.sized f` is `Gen.bind Gen.nat f`, and `( >>= )`, `( >|= )` and `Gen.ap` give way to `let*`, `let+` and `and+`; `Gen.fix` and `Gen.delay` are removed, and a recursive type is a `let rec` generator over a depth (doc/manual/property-testing.md#generating-a-recursive-type).
- (breaking) `Gen.int32_range` and `Gen.int64_range` are removed; `Gen.map Int32.of_int (Gen.int_range lo hi)` replaces the first, and likewise for `Int64`.
- (breaking) `?origin` and `?ratio` are removed; a range shrinks toward its point closest to 0 (closest to `'a'` for `Gen.char_range`), and `Gen.frequency` weighs choices.
- (breaking) `Gen.float` draws finite floats only; `Gen.float_range low high` includes `high` and raises `Invalid_argument` on a non-finite bound.
- `Gen.such_that p gen` draws until `p` holds, at most 100 times and then discards the case, `Gen.with_pp pp gen` gives a generator its printer, and `Gen.bytes_of` generates `bytes`.
- A counterexample prints with its generator's printer, and a value computed by `Gen.map` or `Gen.bind` prints as `computed from <pre-image>`.
- (breaking) `--seed` and `WINDTRAP_SEED` take the token `s1:` and 16 lowercase hexadecimal digits; any other spelling is a usage error.
- A run has one root seed, and every case derives from it, the test's path and its index; the header or the summary prints `(seed s1:…)` when a property is selected.
- A failing property's block reads `counterexample (case N, shrunk K steps): <value>`, shows the law's own failure with its diff, and ends on a `replay:` command for the same seed and test.
- A shrink cut by the step budget or by a timeout says `counterexample may not be minimal`.
- A `skip` or timeout inside a law is no longer shrunk as a counterexample.
- A property gives up once more than `max_discard` cases are discarded, twice the effective count by default.
- `~examples:[ v1; v2 ]` runs the law on those inputs first on every run, unshrunk (doc/manual/property-testing.md#keeping-a-counterexample-as-a-regression).
- (breaking) `cover ~label ~at_least cond` is `cover label cond`, which fails the property with `never covered: "label"` when no passing case marked `label`.
- `collect` and `classify` labels print as a table in a failing property's block, and under `-v` for a passing one.
- A property carries the tag `"prop"`, and a stateful test `"prop"` and `"stateful"`.

### Stateful testing

- `stateful name ~model ~scope commands` checks generated programs of calls on a system against a pure model; `command name gen ~next body` is an operation with an argument, `call name ~next body` one without, and `?pre` limits either to the models where the call is legal (doc/manual/stateful-testing.md#writing-a-stateful-test).
- `~scope` gives each program and each shrink candidate a fresh system, and `?invariant m sut` runs on the fresh system and after every call.
- A stateful test runs `?count` programs (default 100) of at most `?steps` calls (default 20).
- A failing program shrinks by removing calls and shrinking arguments, and prints as a table of its calls, with the model before each under `?pp_model`.
- A `~pre` or `~next` that raises while a program is drawn fails the case, reported as `call 3: close, ~pre raised <exn>`.

### Baselines and expect tests

- (breaking) `snapshot`, `snapshot_pp` and `snapshotf` are removed, and no file lives under `__snapshots__`; `expect s @@ __POS_OF__ {|…|}` or `expect_file s "test/name.expected"` replaces them.
- (breaking) `expect` and `expect_exact` take the produced text and a `__POS_OF__` literal, as in `expect (output ()) @@ __POS_OF__ {|…|}`; a failure is located at the literal, and a correcting run rewrites it.
- (breaking) `capture` and `capture_exact` are removed; a test compares `output ()` with any verb.
- (breaking) `output ()` fails the test under `--stream` with `this test requires capture; rerun without --stream`, and raises `Invalid_argument` outside a test; 0.1 returned `""` in both cases.
- `output ()` and the captured log include what C code wrote with `printf`, since capture flushes the C `stdout` and `stderr` buffers before reading.
- `expect_file actual path` compares text with a file, `path` relative to the project root, which is `WINDTRAP_PROJECT_ROOT`, else the parent of dune's build directory, else the working directory, with no marker file consulted; a missing file is a mismatch whose correction is the file (doc/manual/baselines.md#keeping-a-baseline-in-a-file).
- (breaking) A mismatch records the test's failure and the call returns, `[%expect]` nodes included; one run reports, and one correcting run corrects, every stale expectation of a test.
- (breaking) `WINDTRAP_UPDATE` is not read; a `(test)` stanza runs `%{test} --corrected`, which writes `<file>.corrected` beside each file, and `diff?`s them for `dune promote` (doc/manual/baselines.md#running-expectations-under-dune).
- (breaking) `-u` rewrites each literal and each `expect_file` file in place, and every accepted test passes; it is refused under `CI` (exit 1), and `-u` with `--corrected` is a usage error.
- (breaking) `WINDTRAP_SNAPSHOT_DIR`, `WINDTRAP_SNAPSHOT_DIFF_CONTEXT`, `WINDTRAP_SNAPSHOT_MAX_BYTES` and `WINDTRAP_SNAPSHOT_REPORT` are not read.
- A correction is kept only for a test whose every failure is a baseline mismatch and whose source is unchanged since the build; the block says why when none is kept.
- A baseline failure opens on `expect: mismatch` or `expect_file "<path>": no baseline` and ends on an `accept:` line, and a correcting run lists its files under `corrections (N):`.
- A correction keeps its literal's delimiter and lays out a multi-line text like ppx_expect, and a bare `[%expect]` keeps its node.
- (breaking) Inline tests in an `(executable)` no longer run at exit, and `[%%run_tests]` is removed; such an executable exits 2 with `windtrap: registered inline tests were never driven`, and the tests belong in a library with `(inline_tests)`.
- (breaking) A library's inline tests run only in that library's inline runner, never in a `(test)` executable or program that links the library.
- (breaking) `[%expect]`, `[%expect_exact]` and `[%expect.output]` compile only inside a `let%expect_test` body, and output after the last node or a node the body never reaches is no longer checked.
- (breaking) The ppx_expect forms windtrap lacks are compile errors, as `[%expect.unreachable]` always was; an attribute such as `[@@expect.uncaught_exn]`, which 0.1 dropped silently, now fails with `[@@expect.uncaught_exn] is not supported by ppx_windtrap`.
- `Expect_test_config`, from `ppx_windtrap.config`, wraps expect-test bodies with `run` and rewrites captured output with `sanitize`, and a local module of that name overrides it (doc/manual/baselines.md#masking-what-changes-between-runs).
- The inline runner runs each partition as `run --corrected` under the suite `<lib>/<file>`, and exits 1 when a test failed outside its kept corrections.
- (breaking) The cookie `inline-test=drop` is not read; dune's `inline_tests` cookie drops the test forms.
- A test name repeated in one scope, as in a functor applied twice, gets the suffix ` (2)`.
- `module%test` keeps the module's attributes.

### Resources and process state

- What `bracket`'s `setup` raises is a `[setup]` failure, and what `teardown` raises is a `[teardown]` failure listed beside the body's; in 0.1 a raising teardown replaced the body's failure with `Fun.Finally_raised`.
- `scoped scope name fn` is the test whose body receives the resource a scope such as `In_channel.with_open_text path` provides, and a scope that swallows the body's failure cannot pass the test.
- (breaking) `fixture ?teardown create` is acquired by the first call inside a test and released after the last test of the run; calling it outside a test raises `Invalid_argument` (doc/manual/resources-and-structure.md#sharing-one-resource-across-the-run).
- `subtest name fn` runs a named part of the current test and records its failure while the rest runs, and `current_test ()` returns the running test's path.
- `temp_dir ()` and `temp_file ()` make scratch paths the runner removes when the attempt ends.
- `setenv` and `chdir` change the environment and the working directory for the running test, and the runner restores both when the attempt ends (doc/manual/resources-and-structure.md#using-files-variables-and-a-working-directory).
- (breaking) A call to `exit` inside a test fails that test with `the test called exit and was intercepted; a test must return or raise, never exit the process`, and the run continues.
- A `Stack_overflow` fails its test, and only `Sys.Break` and `Out_of_memory` end the run.
- (breaking) While a test runs, the global `Random` state is seeded from the test's path; in 0.1 every test started from `Random.init 137`.

### Running tests

- (breaking) A command line that does not parse prints one `windtrap:` line and the usage and exits 2, where 0.1 exited 1; an unknown long option names the nearest flag, as in `windtrap: unknown option '--juint'; did you mean '--junit'?`.
- (breaking) `--format`, `WINDTRAP_FORMAT`, the TAP report and the dot reporter are removed; the report has two verbosities, the default and `-v`.
- A run with nothing to show prints one line, such as `storage: 12 passed, 2 skipped, 1 expected failure in 2.8ms.`, whose duration is wall-clock time (doc/manual/running-tests.md#running-every-suite).
- (breaking) A test the selection leaves out is no longer reported as skipped, and a selection that keeps none prints `<suite>: no tests ran: filter "servr" matched none of 15 tests.` and exits 2.
- A selection given by mirrors alone that keeps no test of a suite exits 0 (doc/manual/running-tests.md#passing-flags-to-dune-runtest).
- A failure's block prints when its test ends and, when the test wrote output, closes on its last 10 lines and `full log: <path>`; 0.1 showed no captured output.
- `-v` prints one flat row per test, `PASS`, `FAIL`, `SKIP` with its reason or `XFAIL`, with the path and duration and no group header lines.
- On a terminal a dim line such as `[3/15] <path>…` names the running test, with or without `-v`.
- `--slow-threshold SECONDS` (default 1) lists under `slow tests` every test not tagged `slow` that ran that long; `Slowest tests:` is removed.
- A test that passes on a retry is listed under `flaky tests` and counted as `(N flaky)`.
- (breaking) `-f` and `-e` repeat, and a test is kept when its path holds one `-f` pattern and no `-e` pattern; every bare argument, and every argument after `--`, is a pattern.
- `--shard K/N` keeps bucket K of N of the selection, by a hash of each test path.
- (breaking) `-q`/`--quick` and `--bail N` are removed; `--exclude-tag slow` leaves out the slow tests, and `-x` stops at the first failure and counts the rest as `N not run`.
- (breaking) `-l` prints the paths of the selection in declaration order, where 0.1 printed every path sorted, and it makes the checks a run makes, so duplicate paths or a `focus` under `CI` exit 1.
- (breaking) `--failed` keeps a record per suite, and with nothing recorded it prints `windtrap: no recorded failures match the current suite` and exits 2.
- `WINDTRAP_VERBOSE`, `WINDTRAP_JUNIT`, `WINDTRAP_OUTPUT`, `WINDTRAP_SHARD` and `WINDTRAP_SLOW_THRESHOLD` are new mirrors.
- (breaking) A malformed mirror is a usage error naming the variable, and a mirror set to the empty string is unset; 0.1 ignored a bad value.
- `--help` lists each flag with its mirror.
- `WINDTRAP_TAIL_ERRORS` and `WINDTRAP_COLUMNS`, which changed nothing in 0.1, are not read.
- `--color auto` honours `NO_COLOR` and `TERM=dumb`, and `--color` takes any case.
- (breaking) `--junit PATH` writes the file beside the terminal report, and a `PATH` without `.xml` is a directory that gets `<suite>.xml`; a file that cannot be written prints `windtrap: warning: could not write JUnit report to <file>: <reason>` and leaves the exit code alone.
- The JUnit file names the suite, carries each failure's text, and leaves deselected tests out.
- Each test's captured output is kept in `<log dir>/<suite>/<groups>/<test>.output`, overwritten by each run, with no run directories, `latest` links or `Test output saved to` line.
- (breaking) `CI` and `GITHUB_ACTIONS` count as unset when empty or `0`, `false`, `no`, `n` or `off`.
- Under GitHub Actions each failure is one percent-encoded `::error` annotation after the group, and the summary is the last line.
- SIGINT, SIGTERM and SIGHUP print `windtrap: interrupted in <path>` and the summary, release the fixtures, and end the process by the same signal.
- A timeout is measured to the fraction of a second, where 0.1 rounded it up to whole seconds, and fails with `timed out after 0.5s`.
- Runner messages print on stderr behind `windtrap:`.
- A control byte in a name, value or captured line prints as `\xNN`.

### Coverage

- (breaking) `(instrumentation (backend ppx_windtrap))` and `--instrument-with ppx_windtrap` are now `ppx_windtrap.coverage`, and dune rejects `ppx_windtrap` as a backend (doc/manual/coverage.md#instrumenting-a-library).
- (breaking) A test run prints no coverage line; `windtrap coverage` reports coverage.
- (breaking) Coverage counts the entries of bodies and branches and the returns of calls, so a call that raises stays uncovered; a percentage cannot be compared with a 0.1 one.
- (breaking) `windtrap coverage` has no `--summary-only`, `-C`/`--context`, `--skip-covered`, `--coverage-path`, `--source-path` or `-j`; a positional `PATH` and `--json` replace two of them.
- The report puts each file's uncovered ranges on its row and ends on `coverage: 71.4% (312/437 points)`; `-u` keeps the table and adds the uncovered source.
- `--min PCT` exits 1 below `PCT`, and the last line then reads `coverage: 71.4% (312/437 points), minimum 80%: FAILED`.
- `--lcov` prints an LCOV tracefile.
- `--expect PATH` exits 1 unless every source under `PATH` has coverage data.
- (breaking) `--json` drops `source_available` and `uncovered_offsets`.
- (breaking) Each instrumented executable writes its dumps under `<build dir>/_coverage/windtrap-<hash>/`, and its first dump after a rebuild removes the older ones; `windtrap coverage` finds them from any subdirectory, and `-o` no longer moves them (doc/manual/coverage.md#where-the-dumps-are).
- A dump whose executable was deleted or rebuilt is excluded with a line on stderr.
- (breaking) A missing `PATH` or an unreadable, corrupt or 0.1 dump exits 1, and a usage error exits 2 behind `windtrap:`.
- (breaking) `WINDTRAP_COVERAGE_FILE` names the dump itself.
- (breaking) `WINDTRAP_COVERAGE_LOG` is not read; a dump that cannot be written is one `windtrap: warning:` line on stderr.
- `[@@coverage off]` also excludes a module binding.

### Mutation testing

- `(instrumentation (backend ppx_windtrap.mutate))` with `--instrument-with ppx_windtrap.mutate` compiles every mutant of a library into the build, each off until armed (doc/manual/mutation.md#instrumenting-a-library-for-mutation).
- A mutant negates a condition, moves a comparison by one, swaps `&&` and `||`, or swaps `+` and `-`, and is named `<file>:<line>:<col>:<rewrite>`.
- `[@mutate off "reason"]` and its `[@@…]` and `[@@@…]` forms dismiss equivalent mutants.
- `--mutate[=PREFIX,…]` runs the suite, then each reached mutant in a child process with the tests that reached it, prints each survivor with those tests, and exits 0 (doc/manual/mutation.md#testing-a-suites-mutants).
- `--arm ID` runs the suite with one mutant armed and says whether it was killed.
- `windtrap mutants` merges the verdict files of the executables and exits 1 when a mutant survived every executable that reached it.
- `--mutate` is refused (exit 1) on Windows, in a process that spawned a domain, when the dry run fails or has no mutant in scope, and when the determinism probe disagrees with the dry run.

### Packages and libraries

- (breaking) `windtrap.prop` (`Windtrap_prop`, with `Arbitrary` and `Prop.check`), `windtrap.myers` and `windtrap.clock` are removed; `windtrap` holds `Gen` and `prop`.
- (breaking) `windtrap.coverage` (`Windtrap_coverage`) is replaced by `windtrap.runtime`, which depends on the stdlib alone.
- (breaking) `Windtrap.Ppx_runtime` is removed, and `ppx_windtrap.runtime` holds the inline-test runtime.
- `ppx_windtrap` declares `windtrap` as a runtime library, so a stanza preprocessed with it need not list `windtrap` in `(libraries …)`.
- `ppx_windtrap.coverage` and `ppx_windtrap.mutate` are the instrumentation backends, and `ppx_windtrap.config` holds `Expect_test_config`.
- The `windtrap` binary has the commands `coverage` and `mutants`, and an unknown command exits 2.
- Linking windtrap no longer adds the top-level modules `Clock` and `Myers` to an executable.

## v0.1.0 2026-02-13

Windtrap is an all-in-one OCaml testing framework that unifies unit tests,
property-based tests, snapshot tests, and expect tests under a single API.
Instead of juggling multiple testing libraries, Windtrap gives you one
cohesive package with a PPX for inline expect tests (`ppx_windtrap`).

- Unit tests with combinators, tags, skip, brackets, and timeouts.
- Property-based testing with configurable seeds and shrinking.
- Snapshot testing with automatic file management and diffing.
- Inline expect tests via `ppx_windtrap` with automatic correction.
- CLI test runner with filtering, verbosity, and color support.
- Test coverage reporting with `bisect_ppx` integration.

### Acknowledgments

Windtrap builds on ideas and code from several OCaml projects:

- **[Alcotest](https://github.com/mirage/alcotest)** by Thomas Gazagnaire: test structure and runner design.
- **Craig Ferguson's Alcotest PRs** ([#294](https://github.com/mirage/alcotest/pull/294), [#247](https://github.com/mirage/alcotest/pull/247)): API design, subcomponent diffing, and Levenshtein distance (ISC).
- **[QCheck2](https://github.com/c-cube/qcheck)** by Simon Cruanes et al.: generator design and integrated shrinking (BSD 2-Clause).
- **[ppx_expect](https://github.com/janestreet/ppx_expect)** and **[ppx_inline_test](https://github.com/janestreet/ppx_inline_test)** by Jane Street: expect test paradigm and dune integration.
- **[Bisect_ppx](https://github.com/aantron/bisect_ppx)** by Anton Bachin et al.: coverage instrumentation and runtime (MIT).
