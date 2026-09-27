# Migrating from 0.1

This page ports a suite written for windtrap 0.1 to this version. Each
line maps a 0.1 spelling to its current spelling, by area, and a
spelling this page does not list is unchanged. The entries of
[`CHANGES.md`](../../CHANGES.md) from `0.2.0` on list every change.

## Declaring tests

- `let () = run "mylib" tests` → `let () = exit (run "mylib" tests)`
- `run ~quick ~filter ~seed …` and `run`'s other optional arguments →
  the command-line flags or their `WINDTRAP_*` mirrors, or `run ~argv`
- `ftest "x" fn`, `fgroup "g" ts` → `focus (test "x" fn)`,
  `focus (group "g" ts)`
- `~tags:(Tag.labels [ "net" ])` → `~tags:[ "net" ]`
- `Tag.speed Slow` → `slow name fn`, or `~tags:[ "slow" ]`
- `Tag.empty`, `Tag.add_label`, `Tag.merge`, `Tag.has_label` → a
  `string list` and the functions of `List`
- `~tags:(Tag.labels [ "disabled" ])` → `skip ()` in the body, or
  `--exclude-tag` on the command line
- `group ~before_each ~after_each` → `bracket ~setup ~teardown` on each
  test
- `group ~setup ~teardown` → `fixture ?teardown create`
- `~timeout` on every test of a group → `group ~timeout:3. "g" [ … ]`
- `cases ty inputs name fn` → `cases ~name:to_string name inputs fn`,
  whose children are named `to_string input` under the group `name`
- `?here:[%here]`, `?pos:__POS__` → `~__POS__`, or nothing
- `let helper ?pos x = equal ?pos …` →
  `let helper ?__POS__ x = equal ?__POS__ …`
- a body that returns a value → a body that returns `unit`, ending on an
  assertion or on `ignore`
- a `fixture` accessor called outside a test → a call inside a test
  body; elsewhere it raises `Invalid_argument`
- `type format` (`Compact`, `Verbose`, `Tap`, `Junit`) → `-v` and
  `--junit`

## Assertions

- `testable ~pp ()`, `Testable.make ~pp ()` → `Testable.structural ~pp`
- `testable ~pp ~equal ()`, `Testable.make ~pp ~equal ()` →
  `Testable.make ~pp ~equal`
- `Testable.gen`, `Testable.check`, `Testable.with_gen` → a `Gen.t`
  given to `prop`
- `of_equal eq`, `contramap f t` → `Testable.of_equal eq`,
  `Testable.contramap f t`
- `seq t` → `Testable.contramap List.of_seq (list t)`
- `lazy_t t` → `Testable.contramap Lazy.force t`
- the `nat` and `small_int` witnesses → `int`, and the generators
  `Gen.nat` and `Gen.small_int`
- `float 0.`, `float_rel ~rel:0. ~abs:0.` → `float_exact`
- `float eps` to compare NaN with NaN → `float_exact`; under `float eps`
  NaN equals nothing
- `is_ok r; Result.get_ok r` → `require_ok r`
- `some t e v` → `equal (option t) (Some e) v`
- `ok t e r`, `error t e r` → `equal t e (require_ok r)`,
  `equal t e (require_error r)`
- `no_raise fn` → `fn ()`
- `raises_invalid_arg "m" fn` → `raises (Invalid_argument "m") fn`
- `raises_failure "m" fn` → `raises (Failure "m") fn`
- a predicate of your own on the message → `raises_match Exn.invalid_arg
  fn`, `raises_match (Exn.failure ~substring:"…") fn`, and `Exn.sys_error`
- `is_true (a < b)` → `less int ~than:b a`, and `at_most`, `greater`,
  `at_least`
- `is_true (String.starts_with ~prefix s)` → `starts_with ~affix:prefix s`
- `is_true (List.mem x xs)` → `mem int x xs`
- `'a Pp.t` and the `Pp` combinators → `'a printer` and `Format`

## Property testing

- `prop name (list int) law`, whose law returns a `bool` →
  `prop name Gen.(list int) (fun l -> is_true (law l))`, or assertions
  in the body
- `prop'` → `prop`
- `prop2`, `prop3`, `prop4` → `prop` over `Gen.pair`, `Gen.triple`,
  `Gen.quad`, with a body over the tuple
- `~config` → `~count`, `~examples` and `~max_discard`; the seed is
  `--seed`, and the shrink budget has no option
- a witness's `~gen` → a `Gen.t` argument, with `Gen.with_pp` for its
  printer
- `Gen.oneofl`, `Gen.oneof` → `Gen.of_list`, `Gen.one_of`
- `Gen.list_size sg g`, `Gen.string_size sg cg` → `Gen.list ~size:sg g`,
  `Gen.string_of ~size:sg cg`
- `Gen.sized f` → `Gen.bind Gen.nat f`
- `Gen.pure v` → `Gen.constant v`
- `Gen.( >>= )`, `Gen.( >|= )`, `Gen.ap` → `let*` or `Gen.bind`, `let+`
  or `Gen.map`, `and+`
- `Gen.fix`, `Gen.delay` → recursion through `let*` over `Gen.nat`
- `Gen.no_shrink`, `Gen.add_shrink_invariant`, `Gen.make_primitive`,
  `Gen.find` → removed
- `?origin` on the range generators, `?ratio` on `Gen.option`,
  `Gen.result` and `Gen.either` → removed; `Gen.frequency` weighs
  choices
- a `Gen.t` written as a function of a `Random.State.t` → the
  combinators, since `Gen.t` is abstract
- `Windtrap_prop` (`Prop.check`, `Arbitrary`) → `prop` and `Gen`
- `cover ~label:"even" ~at_least:20. c` → `cover "even" c`, which fails
  the property when no passing case marks the label

## Baselines and expect tests

- `snapshot ~pos:__POS__ s`, `snapshot ~name s` →
  `expect s @@ __POS_OF__ {|…|}`
- `snapshotf fmt …` → `expect (Format.asprintf fmt …) @@ __POS_OF__ {|…|}`
- `snapshot_pp pp v` →
  `expect (Format.asprintf "%a" pp v) @@ __POS_OF__ {|…|}`
- a `__snapshots__/<file>/<key>.snap` file → an `.expected` file that a
  call names, `expect_file s "test/name.expected"`
- `expect s`, `capture fn s` → `expect (output ()) @@ __POS_OF__ {|…|}`,
  or a `let%expect_test`
- `expect_exact s`, `capture_exact fn s` →
  `expect_exact (output ()) @@ __POS_OF__ {|…|}`
- `WINDTRAP_UPDATE=1 dune runtest` → a `(test)` stanza whose action runs
  `%{test} --corrected` and `diff?`s each corrected file, then
  `dune promote`; outside dune, `-u`
- `run ~update ~snapshot_dir` → `-u` on the command line; a baseline's
  path is its call's
- `WINDTRAP_SNAPSHOT_DIR`, `WINDTRAP_SNAPSHOT_DIFF_CONTEXT`,
  `WINDTRAP_SNAPSHOT_MAX_BYTES`, `WINDTRAP_SNAPSHOT_REPORT` → removed
- `[%%run_tests "Name"]`, or inline tests in an `(executable)` →
  `(inline_tests)` on a library
- `-cookie 'inline-test=drop'`, and the inline runner's
  `-source-tree-root` and `-diff-cmd` → removed

## Running tests

- `--format`, `WINDTRAP_FORMAT` → the default report, `-v`, or
  `--junit PATH`
- `-q`, `--quick` → `--exclude-tag slow`
- `--bail` → `-x`, `--fail-fast`
- `--seed 42`, `WINDTRAP_SEED=42` → `--seed s1:<16 hex digits>`, the
  token a run prints
- `WINDTRAP_TAIL_ERRORS`, `WINDTRAP_COLUMNS` → removed
- `--junit out/report` → `--junit out/report.xml`. A path that does not
  end in `.xml` names a directory, with one file per suite.
- `run ~output_dir` → `-o DIR`, or `WINDTRAP_OUTPUT`

## Coverage

- `(instrumentation (backend ppx_windtrap))` →
  `(instrumentation (backend ppx_windtrap.coverage))`
- `--instrument-with ppx_windtrap` →
  `--instrument-with ppx_windtrap.coverage`
- `windtrap coverage --summary-only` → `windtrap coverage`, whose table
  has a row per file
- `windtrap coverage -C N`, `--context N`, `--skip-covered` → removed
- `windtrap coverage --coverage-path P` → `windtrap coverage P`
- `windtrap coverage --source-path P` → removed; sources are read from
  the project root, or from the current directory under `PATH`
  arguments
- `windtrap coverage -j` → `windtrap coverage --json`
- `WINDTRAP_COVERAGE_LOG` → removed

A 0.2 percentage counts expression points and the returns of calls, and
cannot be compared with a 0.1 percentage.

## Packages and libraries

- `windtrap.clock` in `(libraries …)` → nothing; the clock is in
  `windtrap`
- `windtrap.prop` → `windtrap`, with `Gen` and `prop`
- `windtrap.myers` → removed
- `windtrap.coverage` (`Windtrap_coverage`) → `windtrap.runtime`
  (`Windtrap_runtime.Coverage`)
- `Windtrap.Tag`, `Windtrap.Pp`, `Windtrap.Ppx_runtime` → removed from
  `Windtrap`; the inline-test runtime is `ppx_windtrap.runtime`
