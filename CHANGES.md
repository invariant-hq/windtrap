# Changelog

## v0.2.0 2026-09-26

Windtrap 0.2.0 is a complete rewrite of Windtrap. It keeps the shape of
0.1.0's API and rebuilds it on a stronger design: one runner for every
kind of test, one seed for every generated value in a run, and failures
kept as data that the report renders. It also fixes many correctness
problems, listed below.

The main additions are stateful testing and mutation testing.

- A stateful test generates sequences of calls and runs each call on the
  system under test and on a reference, a model written for the test or
  another implementation. It fails at the first call where the system
  does not return or raise what the reference does. When a sequence
  fails, windtrap removes calls and simplifies their arguments for as
  long as the sequence keeps failing, so the report shows a short
  sequence that reproduces the bug. It follows
  [Monolith](https://gitlab.inria.fr/fpottier/monolith) and
  [qcheck-stm](https://github.com/ocaml-multicore/multicoretests), whose
  parallel mode it shares: with `~domains`, the middle of each sequence
  runs on several domains at once.
- Mutation testing makes small changes to the code under test, such as
  turning `<` into `<=` or `&&` into `||`, and reports each change that
  no test fails on. Each such change points at a gap in the suite.

Windtrap now runs five kinds of test:

- unit tests, with assertions;
- expect tests, inline in a library as with
  [ppx_expect](https://github.com/janestreet/ppx_expect) and
  [ppx_inline_test](https://github.com/janestreet/ppx_inline_test), or
  in any test with `expect`;
- snapshot tests, now part of the expect API through `expect_file`;
- property tests, as with [QCheck](https://github.com/c-cube/qcheck);
- stateful tests, as with qcheck-stm and Monolith.

It measures a suite in two ways: coverage, as with
[Bisect_ppx](https://github.com/aantron/bisect_ppx), and mutation
testing.

The core of a suite reads as before: `test`, `group`, `equal` and
witnesses such as `int` and `list` keep their meaning, and so do
`let%expect_test` and `[%expect]`. Around that core, 0.2.0 makes
substantial changes, improvements and additions to the API. The API is
flat, so
[`lib/windtrap.mli`](https://github.com/invariant-hq/windtrap/blob/main/lib/windtrap.mli)
documents all of it in one file.

See
[`doc/manual/migrating-from-0.1.md`](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/migrating-from-0.1.md)
for how to migrate from 0.1.0.

### Changes that keep a 0.1 suite compiling

These changes compile without an error and change what a 0.1 suite does.
Each one is also listed under its area below.

- A test tagged `"disabled"` runs, where 0.1 skipped it. `skip ()` in
  its body, or `--exclude-tag disabled` on the command line, leaves it
  out.
- The global `Random` state is seeded from the path of each test, where
  0.1 started every test from `Random.init 137`.
- Under `float eps` and `float_rel`, NaN equals nothing, where 0.1 found
  NaN equal to NaN, and `float_rel` no longer finds an infinity equal to
  every float.
- A mismatched `expect` or `[%expect]` records the failure and returns,
  where 0.1 raised, so the rest of the test runs.
- A call to `exit` inside a test fails that test, and the run continues.
- `output ()` fails the test under `--stream` and raises
  `Invalid_argument` outside a test, where 0.1 returned `""`.
- A library's inline tests run only in that library's inline runner,
  never in a `(test)` executable that links the library.
- A command line that does not parse exits 2, where 0.1 exited 1, and a
  selection that keeps no test exits 2, where 0.1 exited 0.
- `-f` and `-e` add up when repeated, where 0.1 kept the last of each.
- `CI` and `GITHUB_ACTIONS` count as unset when they are empty or `0`,
  `false`, `no`, `n` or `off`.
- `--junit PATH` treats a `PATH` without `.xml` as a directory and
  writes `<suite>.xml` in it.
- Coverage counts a call that raises as uncovered, so a percentage
  cannot be compared with a 0.1 one.
- `WINDTRAP_COVERAGE_FILE` names the dump file itself, where 0.1 used it
  as a prefix for generated file names.
- `WINDTRAP_UPDATE`, `WINDTRAP_COVERAGE_LOG` and the `inline-test=drop`
  cookie are not read.

### Declaring tests

- (breaking) `run : ?argv:string array -> string -> test list -> int`
  returns the exit code instead of calling `exit`; a suite ends on
  `let () = exit (run "mylib" tests)` (see
  [Reading the exit code](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/running-tests.md#reading-the-exit-code)).
- (breaking) `run`'s arguments `~quick`, `~bail`, `~fail_fast`,
  `~filter`, `~exclude`, `~tags`, `~exclude_tags`, `~failed`,
  `~list_only`, `~seed`, `~timeout`, `~prop_count`, `~format`, `~junit`,
  `~stream`, `~output_dir`, `~update` and `~snapshot_dir` are removed,
  and so is `type format`; the flags, their mirrors or a synthetic
  `~argv` configure a run.
- (breaking) A test body returns `unit`, where 0.1 ignored the body's
  result; a body that computes a value ends on an assertion or on
  `ignore`.
- (breaking) `?pos` and `?here` are replaced by `?__POS__` on every
  constructor and every verb, and `type here` is removed; `~__POS__`
  passes the call's position.
- (breaking) `Tag` is removed; tags are a `string list`, as in
  `~tags:[ "net" ]`, and a group's tags are added to those of every test
  under it.
- (breaking) `slow name fn` is `test` with the tag `"slow"`, which
  `--exclude-tag slow` leaves out; a test under a slow group can no
  longer be marked quick.
- (breaking) A test tagged `"disabled"` runs, where 0.1 skipped it, and
  no tag has a meaning for selection; `skip ()` in the body skips the
  test, and `--exclude-tag disabled` leaves it out.
- (breaking) `group`'s `~setup`, `~teardown`, `~before_each` and
  `~after_each` are removed; `bracket` and `scoped` give each test a
  resource, and `fixture` shares one across the run (see
  [Giving each test its own resource](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/resources-and-structure.md#giving-each-test-its-own-resource)).
- (breaking) `test`, `group`, `slow`, `cases`, `bracket` and `scoped`
  raise `Invalid_argument` when applied if `~timeout` is not finite and
  positive or `~retries` is negative, where 0.1 ran `~timeout:0.` as a
  one-second alarm.
- (breaking) `cases ~name base inputs fn` takes a naming function in
  place of a witness; each child is the test `name input` under the
  group `base`.
- (breaking) `ftest` and `fgroup` are removed; `focus t` focuses any
  test or group, and the tests outside the focus have no row and no
  count (see
  [Focusing on one test](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/running-tests.md#focusing-on-one-test)).
- (breaking) `run` refuses a suite that holds a `focus` under `CI`,
  prints
  `windtrap: focused tests committed (focus at <file:line>); remove focus to run under CI`
  and returns 1.
- `run` raises `Invalid_argument` when called while a run executes, as
  from a test body; two runs in a row in one process are allowed.
- Without `~__POS__`, a declaration or an assertion is located from the
  call stack, which needs `-g`, dune's default (see
  [Locating a failing assertion](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/assertions.md#locating-a-failing-assertion)).
- A `cases` child is a test that `-f` selects alone, and two inputs with
  one name make `run` refuse the suite.
- `test`, `group`, `slow`, `cases`, `bracket` and `scoped` take
  `?__POS__`, `?tags`, `?timeout` and `?retries`; on a group, `timeout`
  and `retries` apply to each test under it that declares none, as in
  `group ~timeout:30. "integration" tests`.
- `xfail ?reason t` marks a test or group as expected to fail; its
  failure leaves the exit code, `-x` and `--failed` alone, and a pass
  fails with `expected to fail (<reason>), but the test passed` (see
  [Keeping a known bug in the suite](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/resources-and-structure.md#keeping-a-known-bug-in-the-suite)).

### Assertions

- (breaking) The value `testable ~pp ?equal ?gen ?check ()`,
  `Testable.gen`, `Testable.with_gen`, `Testable.check` and
  `Testable.check_result` are removed; `Testable.make ~pp ~equal` takes
  a required equality and no `()`, and `Testable.structural ~pp`
  compares with `Stdlib.( = )`.
- (breaking) `of_equal` and `contramap` are removed from the top level;
  `Testable.of_equal` and `Testable.contramap` replace them.
- (breaking) The witnesses `seq`, `lazy_t`, `small_int` and `nat` are
  removed; `Testable.contramap List.of_seq (list t)` and
  `Testable.contramap Lazy.force t` replace the first two, and `int` the
  others.
- (breaking) `Pp` is removed from the interface; a printer has type
  `'a printer`, which is `Format.formatter -> 'a -> unit`.
- (breaking) `some`, `ok`, `error` and `no_raise` are removed;
  `some t e v` is `equal (option t) (Some e) v`, `ok t e r` is
  `equal t e (require_ok r)`, `error t e r` is
  `equal t e (require_error r)`, and `no_raise fn` is `fn ()`.
- (breaking) `raises_invalid_arg m fn` and `raises_failure m fn` are
  removed; `raises (Invalid_argument m) fn` and `raises (Failure m) fn`
  replace them.
- (breaking) Under `float eps` and `float_rel`, NaN equals nothing,
  where 0.1 found NaN equal to NaN.
- (breaking) `float eps` raises `Invalid_argument` unless `eps > 0.`;
  exact equality is `float_exact`.
- (breaking) `float_rel` raises `Invalid_argument` when a tolerance is
  negative or NaN, or when both are zero.
- `float_rel` no longer finds an infinity equal to every float.
- `Testable.of_equal` prints `<abstract>`, where 0.1 printed `<opaque>`.
- `slist` prints both sides sorted, where 0.1 printed them as given.
- `raises` and `raises_match` let a `skip`, a timeout, an intercepted
  `exit` and `assume` pass through; 0.1's `raises` reported a skip
  inside it as `Wrong exception raised`, and a `raises_match` that
  accepted any exception swallowed it.
- `raises` and `raises_match` report a wrong exception with the raised
  one's backtrace.
- When `raises` expects an `Invalid_argument`, `Failure` or `Sys_error`
  and gets the same constructor with another message, the failure diffs
  the two messages.
- A failure prints `expected` and `actual` and marks what changed for
  every witness, from the two printed values: the changed spans of
  one-line values, and a unified diff with `@@` hunks for multi-line
  ones (see
  [Comparing two values](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/assertions.md#comparing-two-values)).
- A changed span is bold in its side's colour, and a `~` line marks it
  unless the report is coloured on a terminal.
- Two unequal values that print alike are reported as
  `both sides render as: <v>`.
- `~msg` prints a line above the values, where 0.1 printed it in place
  of the headline; the headlines, such as `Values are not equal`, are
  removed.
- A failure prints its location as `file:line`, then the source line of
  the call.
- An uncaught exception prints as `uncaught exception:` with its
  backtrace, which `run` records without `OCAMLRUNPARAM=b`.
- Exception names print without dune's `Dune__exe__` prefix.
- `float_exact` compares floats bit for bit, orders `-0.` below `0.` as
  its equality tells them apart, and prints the shortest decimal that
  round-trips.
- `text` is a string witness printed verbatim, so two multi-line texts
  fail with a line diff.
- `less`, `at_most`, `greater` and `at_least` compare under the
  witness's order, which the base witnesses carry and
  `Testable.with_compare` gives; a witness without an order raises
  `Invalid_argument`.
- `require_some`, `require_ok`, `require_error` and `require_match`
  assert a shape and return its payload (see
  [Unwrapping an option or a result](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/assertions.md#unwrapping-an-option-or-a-result)).
- `is_none`, `is_ok` and `is_error` take `?pp` and print the payload
  they did not want, as `<abstract>` without it.
- `starts_with ~affix`, `ends_with ~affix`, `contains ~sub`,
  `not_contains ~sub` and `in_order ~subs` search a string, and a
  failure says where the needle was or was not found in an excerpt of
  the haystack.
- `satisfies ?claim t pred v` asserts `pred v`, and `mem t x xs` asserts
  that `xs` holds `x`; both print the values with `t`'s printer.
- `Exn.invalid_arg`, `Exn.failure` and `Exn.sys_error` are predicates
  for `raises_match`, each with `?substring`.
- `Law` asserts seventeen textbook laws, such as
  `Law.associative w op (a, b, c)`, and a failure names the law, states
  its equation and prints every term it computed;
  `prop "…" Gen.(triple g g g) (Law.associative w op)` checks one over
  drawn values (see
  [Stating a textbook law](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/property-testing.md#stating-a-textbook-law)).

### Property testing

- (breaking)
  `prop ?__POS__ ?tags ?timeout ?count ?max_discard ?examples name gen law`
  takes a `Gen.t` and a law `'a -> unit` that asserts with the verbs; a
  0.1 boolean law becomes `fun l -> is_true (law l)`.
- (breaking) `prop'`, `prop2`, `prop3` and `prop4` are removed; `prop'`
  is `prop`, and several inputs are one tuple from `Gen.pair`,
  `Gen.triple` or `Gen.quad`.
- (breaking) `~config` is removed; `?count`, `?max_discard` and `--seed`
  replace it.
- (breaking) `Gen.t` is abstract and built from the combinators alone;
  `Gen.make_primitive`, `Gen.no_shrink`, `Gen.add_shrink_invariant` and
  `Gen.find` are removed.
- (breaking) `Gen.oneof`, `Gen.oneofl` and `Gen.pure` are renamed
  `Gen.one_of`, `Gen.of_list` and `Gen.constant`; `Gen.list_size sg g`
  is `Gen.list ~size:sg g`, and `Gen.string_size sg cg` is
  `Gen.string_of ~size:sg cg`.
- `Gen.of_list ~pp` and `Gen.constant ~pp` take the printer of the
  values they list, so a list of chosen values prints without
  `Gen.with_pp`.
- (breaking) `Gen.sized` is removed; `Gen.bind Gen.nat f` replaces
  `Gen.sized f`.
- (breaking) `( >>= )`, `( >|= )` and `Gen.ap` are removed; `let*`,
  `let+` and `and+` replace them.
- (breaking) `Gen.fix` and `Gen.delay` are removed; a recursive type is
  generated by a `let rec` generator over a depth (see
  [Generating a recursive type](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/property-testing.md#generating-a-recursive-type)).
- (breaking) `Gen.int32_range` and `Gen.int64_range` are removed;
  `Gen.map Int32.of_int (Gen.int_range lo hi)` replaces the first, and
  likewise for `Int64`.
- (breaking) The `?origin` of `Gen.int_range`, `Gen.float_range` and
  `Gen.char_range` is removed; a range shrinks toward its point closest
  to 0, or closest to `'a'` for `Gen.char_range`.
- (breaking) The `?ratio` of `Gen.option`, `Gen.result` and `Gen.either`
  is removed; `Gen.frequency` weighs choices.
- (breaking) `Gen.float` draws finite floats only, where 0.1 also drew
  NaN and infinities.
- (breaking) `Gen.float_range low high` includes `high`, where 0.1
  excluded it, and sampling it raises `Invalid_argument` when a bound is
  not finite.
- (breaking) `--seed` and `WINDTRAP_SEED` take the token `s1:` and 16
  lowercase hexadecimal digits; any other spelling is a usage error.
- (breaking) `cover ~label ~at_least cond` is `cover label cond`, which
  fails the property with `never covered: "label"` when no passing case
  marked `label`. Coverage is judged once every case has run, so a
  property that fails on a case or gives up lists no label as never
  covered.
- A run has one root seed, and every case derives from it, the test's
  path and its index; the header or the summary prints `(seed s1:…)`
  when a property is selected.
- A failing property's block reads
  `counterexample (case N, shrunk K steps): <value>` and shows the law's
  own failure with its diff. A report whose failures include a property
  or a stateful test has one `replay: <command> --seed <token>` line
  above the summary, which reruns the run's selection on the values it
  drew.
- A counterexample prints with its generator's printer, and a value
  computed by `Gen.map` or `Gen.bind` prints as
  `computed from <pre-image>`.
- Shrinking runs the law at most 10,000 times, accepted and rejected
  candidates alike, where 0.1's `max_shrink` defaulted to 100.
- A shrink cut by its budget of law runs or by a timeout says
  `counterexample may not be minimal`.
- A `skip` or timeout inside a law is no longer shrunk as a
  counterexample.
- A property gives up once more than `max_discard` cases are discarded,
  twice the effective count by default.
- `collect` and `classify` labels print as a table in a failing
  property's block, and under `-v` for a passing one.
- A property carries the tag `"prop"`, and a stateful test `"prop"` and
  `"stateful"`.
- `~examples:[ v1; v2 ]` runs the law on those inputs first on every
  run, unshrunk (see
  [Keeping a counterexample as a regression](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/property-testing.md#keeping-a-counterexample-as-a-regression)).
- `Gen.such_that p gen` draws until `p` holds, at most 100 times, and
  then discards the case.
- `Gen.with_pp pp gen` gives a generator its printer.
- `Gen.array` takes `?size`, and `Gen.bytes_of` generates `bytes`.
- `Gen.list`, `Gen.array`, `Gen.string_of` and `Gen.bytes_of` without
  `~size` draw a length below 64 and about 5 on average; `~size` sets a
  longer one.
- `Gen.int`, `Gen.int_range`, `Gen.int32`, `Gen.int64` and
  `Gen.nativeint` draw a corner case with probability 0.1: a range's
  bounds, its point closest to 0 and that point's neighbours, or a
  type's 0, 1, -1 and extremes.

### Stateful testing

- `stateful name commands` runs generated programs of calls on a system
  and on a reference, a model written for the test or another
  implementation, which judges each outcome of the system, and fails at
  the first call whose outcomes differ (see
  [Writing a stateful test](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/stateful-testing.md#writing-a-stateful-test)).
- `command name signature reference system` is one operation of the
  API; a signature takes `g @-> …` for an argument drawn from `g`,
  `t ^-> …` for a value of `t` that an earlier call made, and ends in
  `returns w`, `makes t` or `chooses w`.
- `abstract prefix` declares a type whose values only calls make, named
  `q1`, `q2` under `abstract "q"`; `?pp` prints a value's reference side
  in a failing program, `?invariant r s` runs on every value after every
  call, and `?release s` releases a system side when a program ends (see
  [Releasing what a program made](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/stateful-testing.md#releasing-what-a-program-made)).
- An exception is an outcome: two exceptions are equal when their
  constructor names match without the module path, and their payloads
  are not compared (see
  [Checking the exceptions an operation raises](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/stateful-testing.md#checking-the-exceptions-an-operation-raises)).
- `chooses w` compares an outcome the API leaves open: the reference
  receives the system's outcome and returns or raises the one it
  accepts (see
  [Checking an outcome the API leaves open](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/stateful-testing.md#checking-an-outcome-the-api-leaves-open)).
- `?pre` is asked of the reference's arguments when the program runs,
  and a call it refuses is skipped on both sides and absent from the
  report.
- A stateful test runs `?count` programs (default 100) of at most
  `?steps` calls (default 20).
- A failing program shrinks by removing calls and shrinking arguments,
  and prints as a table of the calls its failing run executed, with the
  reference side of each argument before the call when its type has a
  `~pp` (see
  [Reading a failing program](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/stateful-testing.md#reading-a-failing-program)).
- A verb's failure, `Assert_failure` or `Match_failure` is never an
  outcome: in a system function it fails the case at that call, in a
  reference function, like anything a `~pre` raises, it breaks the
  reference, whose failure shrinks apart from the system's and prints as
  `reference of call N of N` (`~pre of call N of N` for a `~pre`), and
  in a `chooses` reference it is the system's mismatch (see
  [Reading a failure of the model](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/stateful-testing.md#reading-a-failure-of-the-model)).
- `assume` or `reject` in a command fails the case with
  `assume or reject in a command; a call's legality is its ~pre`.
- A drawn argument whose generator has no printer fails the test with
  `push: argument 2 has no printer; attach one with Gen.with_pp`.
- `stateful ~domains:n` runs the middle of each program on `n` domains
  at once, 50 times, and fails when no order of the calls replayed on
  the reference gives the outcomes the system gave; the test carries the
  tag `parallel` (see
  [Testing on several domains](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/stateful-testing.md#testing-on-several-domains)).
- On several domains a command that makes a value or has a `~pre` runs
  only before the parallel calls; `stateful ~domains` above 1 raises
  `Invalid_argument` when every command makes a value or has a `~pre`.
- A failing program on several domains prints a `domain` and a `result`
  column, then `no order of the calls gives these results` and the
  closest order, as `the closest order, 2 then 3, differs at call 4: length q1`.
- On several domains a reference whose replay gives a call before the
  parallel calls another outcome than the run breaks with
  `a replay of the reference differs from this run; the reference must behave the same from run to run`.
- On several domains a replay draws the same programs but not the same
  schedules, so it may pass.
- A stateful test on several domains takes no retries, a group's
  included.
- Under `--mutate` and `--arm` a stateful test on several domains runs
  each program once on the test's domain, so a kill does not depend on a
  schedule.
- A stateful test on several domains whose domains cannot be spawned
  fails with `cannot spawn a worker domain: <message>`.
- A stateful test fails with
  `never called: "pop" (over 100 passing cases); a call runs only where its arguments resolve and its ~pre holds`
  when a command that a passing case could draw was called by none; a
  command listed twice is one command, and `~count:0` judges nothing
  (see
  [Writing a stateful test](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/stateful-testing.md#writing-a-stateful-test)).

### Baselines and expect tests

- (breaking) `snapshot`, `snapshot_pp` and `snapshotf` are removed, and
  no file lives under `__snapshots__`; `expect s @@ __POS_OF__ {|…|}` or
  `expect_file s "test/name.expected"` replaces them.
- (breaking) `expect` and `expect_exact` take the produced text and a
  `__POS_OF__` literal, as in `expect (output ()) @@ __POS_OF__ {|…|}`;
  a failure is located at the literal, and a correcting run rewrites it.
- (breaking) `capture` and `capture_exact` are removed; a test compares
  `output ()` with any verb.
- (breaking) `output ()` fails the test under `--stream` with
  `this test requires capture; rerun without --stream`, and raises
  `Invalid_argument` outside a test, where 0.1 returned `""` in both
  cases.
- (breaking) A mismatched expectation or `[%expect]` node records the
  test's failure and returns, where 0.1 raised; one run reports every
  stale expectation of a test, and one correcting run corrects them.
- (breaking) `WINDTRAP_UPDATE` is not read; a `(test)` stanza runs
  `%{test} --corrected`, which writes `<file>.corrected` beside each
  file, and `diff?`s them for `dune promote` (see
  [Running expectations under dune](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/baselines.md#running-expectations-under-dune)).
- (breaking) `WINDTRAP_SNAPSHOT_DIR`, `WINDTRAP_SNAPSHOT_DIFF_CONTEXT`,
  `WINDTRAP_SNAPSHOT_MAX_BYTES` and `WINDTRAP_SNAPSHOT_REPORT` are not
  read.
- (breaking) `-u` is refused under `CI` (exit 1), and `-u` with
  `--corrected` is a usage error.
- (breaking) `[%%run_tests]` is removed, and inline tests in an
  `(executable)` no longer run at exit; the tests belong in a library
  with `(inline_tests)`.
- (breaking) An `(executable)` that registers inline tests exits 2 with
  `windtrap: registered inline tests were never driven`.
- (breaking) A library's inline tests run only in that library's inline
  runner, never in a `(test)` executable or program that links the
  library.
- (breaking) `[%expect]`, `[%expect_exact]` and `[%expect.output]`
  compile only inside a `let%expect_test` body.
- (breaking) The ppx_expect forms windtrap lacks are compile errors, as
  `[%expect.unreachable]` was in 0.1; an attribute such as
  `[@@expect.uncaught_exn]`, which 0.1 dropped silently, fails with
  `attribute expect.uncaught_exn is not supported by ppx_windtrap; catch and print the exception before an [%expect]`.
- (breaking) The cookie `inline-test=drop` is not read; dune's
  `inline_tests` cookie drops the test forms.
- `-u` rewrites each literal and each `expect_file` file in place, and
  every accepted test passes.
- `output ()` and the captured log include what C code wrote with
  `printf`, since capture flushes the C `stdout` and `stderr` buffers
  before reading.
- A correction is kept only for a test whose every failure is a baseline
  mismatch and whose source is unchanged since the build; the block says
  why when none is kept.
- A baseline failure opens on `expect: mismatch`, `expect: no baseline`
  or `expect_file "<path>": no baseline`. Under dune its block ends on
  `accept: dune promote <file>`; a run by hand ends on one
  `accept: <command> -u` line above the summary, over the run's
  selection.
- A correcting run lists its files under `corrections (N):`.
- A correction keeps its literal's delimiter and lays out a multi-line
  text like ppx_expect, and a bare `[%expect]` keeps its node.
- An `expect_exact` correction whose text holds a CR is a quoted literal
  with the CR written `\r`, so it passes once promoted.
- Output a `let%expect_test` body writes after its last node fails the
  test as `expect: no baseline`, and its correction appends `;` and an
  `[%expect]` node that holds it, as ppx_expect does; blank output
  passes (see
  [Writing expect tests inside a library](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/baselines.md#writing-expect-tests-inside-a-library)).
- An `[%expect]` or `[%expect_exact]` node that a run of its test never
  reaches fails the test with
  `the body returned without reaching this node`, and the test keeps no
  correction; each functor instance is judged alone, and an
  `Expect_test_config.run` that never calls the body fails at the body's
  first node.
- The ppx_expect conformance corpus corrects
  `negative-tests/trailing.ml` byte for byte as upstream (15
  corrections, 8 byte-identical), and the corrected
  `negative-tests/escaped_strings.ml` passes (see
  [`test/conformance/RESULTS.md`](https://github.com/invariant-hq/windtrap/blob/main/test/conformance/RESULTS.md)).
- The inline runner runs each partition as `run --corrected` under the
  suite `<lib>/<file>`, and exits 1 when a test failed outside its kept
  corrections.
- A test name repeated in one scope, as in a functor applied twice, gets
  the suffix ` (2)`.
- `module%test` keeps the module's attributes.
- `expect_file actual path` compares `actual` with the file at `path`,
  relative to the project root; a missing file is a mismatch that `-u`
  corrects by writing the file, and under `--corrected` it fails the run
  (see
  [Keeping a baseline in a file](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/baselines.md#keeping-a-baseline-in-a-file)).
- The project root is `WINDTRAP_PROJECT_ROOT`, else the parent of dune's
  build directory, else the working directory; no marker file is
  consulted.
- On Windows a backslash in the project root separates as `/` does, so
  reports print paths relative to the root.
- `Expect_test_config`, from `ppx_windtrap.config`, wraps expect-test
  bodies with `run` and rewrites captured output with `sanitize`, and a
  local module of that name overrides it (see
  [Masking what changes between runs](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/baselines.md#masking-what-changes-between-runs)).

### Resources and process state

- (breaking) `fixture ?teardown create` is acquired by the first call
  inside a test and released after the last test of the run; calling it
  outside a test raises `Invalid_argument` (see
  [Sharing one resource across the run](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/resources-and-structure.md#sharing-one-resource-across-the-run)).
- (breaking) A call to `exit` inside a test fails that test with
  `the test called exit and was intercepted; a test must return or raise, never exit the process`,
  and the run continues.
- (breaking) While a test runs, the global `Random` state is seeded from
  the test's path, where 0.1 started every test from `Random.init 137`.
- What `bracket`'s `teardown` raises is a `[teardown]` failure listed
  beside the body's, where 0.1 replaced the body's failure with
  `Fun.Finally_raised`.
- What `bracket`'s `setup` raises is a `[setup]` failure.
- A `Stack_overflow` fails its test, and only `Sys.Break` and
  `Out_of_memory` end the run.
- `scoped scope name fn` is the test whose body receives the resource a
  scope such as `In_channel.with_open_text path` provides; a scope that
  swallows the body's failure cannot pass the test.
- `subtest name fn` runs a named part of the current test and records
  its failure while the rest runs. Inside a property's law or a stateful
  test's function, its failure fails the case, which shrinks, and the
  report names the subtest under the counterexample.
- `current_test ()` returns the running test's path.
- `temp_dir ()` and `temp_file ()` make scratch paths the runner removes
  when the attempt ends.
- `setenv` and `chdir` change the environment and the working directory
  for the running test, and the runner restores both when the attempt
  ends (see
  [Using files, variables and a working directory](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/resources-and-structure.md#using-files-variables-and-a-working-directory)).

### Running tests

- (breaking) A command line that does not parse prints one `windtrap:`
  line and the usage and exits 2, where 0.1 exited 1.
- (breaking) `--format`, `WINDTRAP_FORMAT`, the TAP report and the dot
  reporter are removed; the report has two verbosities, the default and
  `-v`.
- (breaking) A test the selection leaves out is no longer reported as
  skipped.
- (breaking) A selection that keeps no test prints
  `<suite>: no tests ran: filter "servr" matched none of 15 tests.` and
  exits 2, where 0.1 exited 0.
- (breaking) `-f` and `-e` repeat, where 0.1 kept the last of each, and
  a test is kept when its path holds an `-f` pattern and no `-e`
  pattern.
- (breaking) Every bare argument, and every argument after `--`, is an
  `-f` pattern.
- (breaking) `-q`/`--quick` and `--bail N` are removed;
  `--exclude-tag slow` leaves out the slow tests, and `-x` stops at the
  first failure and counts the rest as `N not run`.
- (breaking) `-l` prints the paths of the selection in declaration
  order, where 0.1 printed every path sorted.
- (breaking) `-l` makes the checks a run makes, so duplicate paths or a
  `focus` under `CI` exit 1.
- (breaking) `--failed` keeps a record per suite, and with nothing
  recorded it prints
  `windtrap: no recorded failures match the current suite` and exits 2.
- (breaking) A malformed mirror is a usage error naming the variable,
  where 0.1 ignored a bad value.
- (breaking) `--junit PATH` writes the file beside the terminal report,
  and treats a `PATH` without `.xml` as a directory that gets
  `<suite>.xml`.
- (breaking) `CI` and `GITHUB_ACTIONS` count as unset when empty or `0`,
  `false`, `no`, `n` or `off`.
- A run never fails because it cannot read or write the `--failed`
  record, and a record it cannot read counts as empty.
- An unknown long option names the nearest flag, as in
  `windtrap: unknown option '--juint'; did you mean '--junit'?`.
- A run with nothing to show prints one line, such as
  `storage: 12 passed, 2 skipped, 1 expected failure in 2.8ms.`, whose
  duration is wall-clock time (see
  [Running every suite](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/running-tests.md#running-every-suite)).
- A selection given by mirrors alone that keeps no test of a suite exits
  0 (see
  [Passing flags to dune runtest](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/running-tests.md#passing-flags-to-dune-runtest)).
- A failure's block prints when its test ends.
- When a failing test wrote output after its last `output ()` call, its
  block closes on the last 10 lines of that output and
  `full log: <path>`, where 0.1 showed no captured output.
- `-v` prints one row per test, with its status (`PASS`, `FAIL`, `SKIP`
  with its reason, or `XFAIL`), path and duration, and no group header
  lines.
- On a terminal a dim line such as `[3/15] <path>…` names the running
  test, with or without `-v`.
- `--slow-threshold SECONDS` (default 1) lists under `slow tests` every
  test not tagged `slow` that ran that long; `Slowest tests:` is
  removed.
- A test that passes on a retry is listed under `flaky tests` and
  counted as `(N flaky)`.
- A mirror set to the empty string counts as unset.
- `WINDTRAP_TAIL_ERRORS` and `WINDTRAP_COLUMNS`, which changed nothing
  in 0.1, are not read.
- `--color auto` honours `NO_COLOR` and `TERM=dumb`, and `--color` takes
  any case.
- A JUnit file that cannot be written prints
  `windtrap: warning: could not write JUnit report to <file>: <reason>`
  and leaves the exit code alone.
- The JUnit file names the suite, carries each failure's text, and
  leaves deselected tests out.
- Each test's captured output is kept in
  `<log dir>/<suite>/<groups>/<test>.output`, overwritten by each run,
  with no run directories, `latest` links or `Test output saved to`
  line.
- Under GitHub Actions each failure is one percent-encoded `::error`
  annotation after the group, and the summary is the last line.
- SIGINT, SIGTERM and SIGHUP print `windtrap: interrupted in <path>` and
  the summary, release the fixtures, and end the process by the same
  signal.
- A timeout is measured to the fraction of a second, where 0.1 rounded
  it up to whole seconds, and fails with `timed out after 0.5s`.
- Runner messages print on stderr behind `windtrap:`.
- A control byte in a name, value or captured line prints as `\xNN`.
- `--shard K/N` keeps bucket K of N of the selection, by a hash of each
  test path.
- `WINDTRAP_VERBOSE`, `WINDTRAP_JUNIT`, `WINDTRAP_OUTPUT`,
  `WINDTRAP_SHARD` and `WINDTRAP_SLOW_THRESHOLD` are new mirrors.
- `--help` lists each flag with its mirror.
- A call on another domain that outlives its test's limit by one more
  limit fails the test as timed out and ends the run after it, with
  `run stopped after <test>: a call on another domain outlived the test's limit`
  above the summary; on Windows, where no limit is enforced, it hangs
  the run.
- `output`, `expect`, `expect_exact`, `expect_file`, `collect`,
  `classify`, `cover`, a fixture's accessor and the functions of the
  running test raise a failure when called from a domain other than the
  one that called `run`, which fails the running test when it reaches
  the test's domain, as through `Domain.join`.

### Coverage

- (breaking) `(instrumentation (backend ppx_windtrap))` and
  `--instrument-with ppx_windtrap` are now `ppx_windtrap.coverage`, and
  dune rejects `ppx_windtrap` as a backend (see
  [Instrumenting a library](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/coverage.md#instrumenting-a-library)).
- (breaking) A test run prints no coverage line; `windtrap coverage`
  reports coverage.
- (breaking) Coverage counts the entries of bodies and branches and the
  returns of calls, so a call that raises stays uncovered; a percentage
  cannot be compared with a 0.1 one.
- (breaking) The `windtrap coverage` flags `--summary-only`,
  `-C`/`--context`, `--skip-covered`, `--coverage-path`, `--source-path`
  and `-j` are removed; a positional `PATH` replaces `--coverage-path`,
  and `--json` replaces `-j`.
- (breaking) `--json` drops `source_available` and `uncovered_offsets`.
- (breaking) An instrumented executable writes its dumps under
  `<build dir>/_coverage/windtrap-<hash>/`, and the runner's `-o` no
  longer moves them (see
  [Where the dumps are](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/coverage.md#where-the-dumps-are)).
- (breaking) A missing `PATH` or an unreadable, corrupt or 0.1 dump
  makes `windtrap coverage` exit 1, and a usage error exits 2 behind
  `windtrap:`.
- (breaking) `WINDTRAP_COVERAGE_FILE` names the dump itself, where 0.1
  took it as the prefix of generated file names.
- (breaking) `WINDTRAP_COVERAGE_LOG` is not read; a dump that cannot be
  written is one `windtrap: warning:` line on stderr.
- An executable's first dump after a rebuild removes its older dumps.
- `windtrap coverage` finds the dumps from any subdirectory.
- A dump whose executable was deleted or rebuilt is excluded with a line
  on stderr.
- The report puts each file's uncovered ranges on its row and ends on
  `coverage: 71.4% (312/437 points)`; `-u` keeps the table and adds the
  uncovered source.
- `--min PCT` exits 1 below `PCT`, and the last line then reads
  `coverage: 71.4% (312/437 points), minimum 80%: FAILED`.
- `--lcov` prints an LCOV tracefile, which genhtml, Codecov, Coveralls
  and editor gutters read; GitLab's merge-request view reads neither
  format windtrap writes.
- `--expect PATH` exits 1 unless every source under `PATH` has coverage
  data, and `--do-not-expect PATH` exempts a file or directory from it.
- On Windows `--expect` names each source without coverage data once,
  spelled with `/`.
- `[@@coverage off]` also excludes a module binding.

### Mutation testing

- `(instrumentation (backend ppx_windtrap.mutate))` with
  `--instrument-with ppx_windtrap.mutate` compiles every mutant of a
  library into the build, each off until armed (see
  [Instrumenting a library for mutation](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/mutation.md#instrumenting-a-library-for-mutation)).
- A mutant negates a condition, moves a comparison by one, swaps `&&`
  and `||`, or swaps `+` and `-`, and is named
  `<file>:<line>:<col>:<rewrite>`.
- `[@mutate off "reason"]` and its `[@@…]` and `[@@@…]` forms dismiss
  equivalent mutants; a bare `[@mutate off]` dismisses every mutant of
  its expression, in later releases too, and a payload that names a
  rewrite is refused (see
  [Dismissing an equivalent mutant](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/mutation.md#dismissing-an-equivalent-mutant)).
- `--mutate[=PREFIX,…]` runs the suite, then each reached mutant in a
  child process with the tests that reached it, prints each survivor
  with those tests, and exits 0 (see
  [Testing a suite's mutants](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/mutation.md#testing-a-suites-mutants)).
- `--arm ID` runs the suite with one mutant armed and says whether it
  was killed.
- `windtrap mutants` merges the verdict files of the executables and
  exits 1 when a mutant survived every executable that reached it.
- `--mutate` is refused (exit 1) on Windows, in a process that spawned a
  domain, when the dry run fails or has no mutant in scope, and when the
  determinism probe disagrees with the dry run.
- A mutant whose child passed without evaluating its site, as when the
  dry run cached the site's result, is not evaluated and no survivor;
  the report lists it under `not evaluated` with the `arm:` command that
  tests it in a new process, and the merge keeps it not evaluated unless
  an executable killed it (see
  [What a mutation run runs](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/mutation.md#what-a-mutation-run-runs)).
- A site that no test reaches and that module initialization or a
  fixture release evaluates is listed under `evaluated outside tests`,
  apart from the lines never reached (see
  [What a mutation run runs](https://github.com/invariant-hq/windtrap/blob/main/doc/manual/mutation.md#what-a-mutation-run-runs)).

### Packages and libraries

- (breaking) `windtrap.prop` (`Windtrap_prop`, with `Arbitrary` and
  `Prop.check`), `windtrap.myers` and `windtrap.clock` are removed;
  `windtrap` holds `Gen` and `prop`.
- (breaking) `windtrap.coverage` (`Windtrap_coverage`) is replaced by
  `windtrap.runtime`, which depends on the stdlib alone.
- (breaking) `Windtrap.Ppx_runtime` is removed, and
  `ppx_windtrap.runtime` holds the inline-test runtime.
- `ppx_windtrap` declares `windtrap` as a runtime library, so a stanza
  preprocessed with it need not list `windtrap` in `(libraries …)`.
- `ppx_windtrap.coverage` and `ppx_windtrap.mutate` are the
  instrumentation backends, and `ppx_windtrap.config` holds
  `Expect_test_config`.
- The `windtrap` binary has the commands `coverage` and `mutants`, and
  an unknown command exits 2.
- Linking windtrap no longer adds the top-level modules `Clock` and
  `Myers` to an executable.

## v0.1.0 2026-02-13

Windtrap is an all-in-one OCaml testing framework that unifies unit
tests, property-based tests, snapshot tests, and expect tests under a
single API. Instead of juggling multiple testing libraries, Windtrap
gives you one cohesive package with a PPX for inline expect tests
(`ppx_windtrap`).

- Unit tests with combinators, tags, skip, brackets, and timeouts.
- Property-based testing with configurable seeds and shrinking.
- Snapshot testing with automatic file management and diffing.
- Inline expect tests via `ppx_windtrap` with automatic correction.
- CLI test runner with filtering, verbosity, and color support.
- Test coverage reporting with `bisect_ppx` integration.
