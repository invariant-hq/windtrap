# Changelog

All notable changes to this project will be documented in this file.

- Only document user-facing changes (features, bug fixes, performance improvements, API changes, etc.)
- Add new entries at the top of the appropriate section (most recent first)

## [0.2.0] - 2026-07-28

Windtrap 0.2.0 is a ground-up rewrite of the whole library around one idea:
a single flat API where declaring is pure data, every assertion failure
renders a structured diff, and every stochastic result is replayable. The
assertion vocabulary settles at sixteen verbs, properties collapse into one
`prop` over `Gen` generators with integrated shrinking, snapshots become
name-keyed and read-only-by-default, expect tests reach practical ppx_expect
compatibility measured against Jane Street's own corpus, and coverage grades
expressions with out-edge points. The surface breaks at nearly every corner;
the section below doubles as the migration reference from 0.1.0.

### Breaking changes

- **Properties**: one `prop` verb, generators come from `Gen`, bodies return
  `unit` and use the ordinary assertion vocabulary; shrinking is integrated
  (there is never a shrink function to write).
  `prop name (list int) law` with a `bool` law →
  `prop name Gen.(list int) (fun l -> equal (list int) l (law l))`;
  `prop'` → `prop` (it is assertion-style now); `prop2`..`prop4` →
  `prop` + `Gen.pair`/`Gen.triple`/`Gen.quad`; `~config` → `~count` and
  `~examples`. Generator spellings: testable `~gen` → `Gen`, printers attach
  with `Gen.with_pp`; `oneofl` → `Gen.of_list`; `oneof` → `Gen.one_of`;
  `list_size sg g` → `Gen.list ~size:sg g`; `string_size sg cg` →
  `Gen.string_of ~size:sg cg`. The rest of the 0.1.0 `Gen` surface (`fix`,
  `delay`, `no_shrink`, `add_shrink_invariant`, `make_primitive`, `find`,
  `ap`, `>>=`/`>|=`, the `?origin`/`?ratio` knobs) is gone: recursion is
  `Gen.sized` + `let*`, and shrinking is always on and
  constraint-preserving. `prop` also drops `?timeout`; the per-test
  timeout (declaration `~timeout`, or the runner's `--timeout`) covers
  the whole property — generation and shrinking included: a timeout that
  expires during shrinking ends the search and reports the best
  counterexample found so far, marked as possibly not minimal.
- **Snapshots** are keyed by name, not source position:
  `snapshot ~pos:__POS__ s` → `snapshot "name" s`, with the baseline at
  `<src_dir>/__snapshots__/<src_basename>/<name>.snap`;
  `snapshotf fmt` → `snapshot name (Printf.sprintf fmt ...)`. Re-accept once
  with `-u` (or `WINDTRAP_UPDATE=1` under dune) and review with `git diff`;
  add `(deps (glob_files_rec __snapshots__/**))` to the test stanza so
  baseline edits re-trigger `dune runtest`.
- **The expect-string family is removed**: `expect`, `expect_exact`,
  `capture`, and `capture_exact` have no replacement spelling — assert on
  captured output with `equal string "..." (output ())`, snapshot it with
  `snapshot "name" (output ())`, or write a real expect test with
  `let%expect_test` and `[%expect]` (ppx_windtrap). `output ()` itself
  stays, and now drains standard error and subprocess output along with
  standard output.
- **Group hooks are removed**: no user code runs outside a test's exception
  boundary. `group ~before_each`/`~after_each` → `bracket ~setup ~teardown`
  on each test (partially apply it to build a reusable constructor);
  `group ~setup`/`~teardown` for expensive shared state →
  `fixture ?teardown create`, acquired on first use and released by the
  runner after the last test — accessors now work only inside a run
  (0.1.0's `fixture` was a plain lazy cache).
- **`Testable.make` requires `~equal`**: `testable ~pp ()` →
  `Testable.structural ~pp` (named structural equality) or
  `Testable.make ~pp ~equal`; the `~gen` and `~check` fields are gone —
  generation lives in `Gen` and diffs are computed from printed values, so
  every type gets highlighted diffs from its printer alone. The
  `nat`/`small_int` pseudo-testables were distributions and live in `Gen`;
  the `seq` and `lazy_t` witnesses are dropped — compare through
  `contramap List.of_seq (list t)` / `contramap Lazy.force t`.
- **The `is_some`/`is_ok`/`is_error` family asserts and unwraps**:
  `is_some x` → `ignore (require_some x)` — and most call sites get shorter,
  `is_ok r; Result.get_ok r` → `require_ok r`. The wrapper testables go
  through plain composites: `some t e v` → `equal (option t) (Some e) v`.
  Exception helpers: `raises_invalid_arg "m"` → `raises (Invalid_argument
  "m")` (a wrong message renders as a message diff); any-message and
  substring forms → `raises_match Exn.invalid_arg` /
  `raises_match (Exn.failure ~substring:"...")`; `no_raise fn` → `fn ()`.
- **`cases` drops its testable and takes the name first**:
  `cases ty inputs name fn` → `cases name inputs fn`; sub-tests are named
  `name.0`, `name.1`, ... or derived from the value with `?name`
  (`cases "ports" ~name:string_of_int [1; 80; 8080] fn`), and each is
  individually selectable with `-f`.
- **`?here` is gone**: use `?pos:__POS__`, or nothing — failure locations
  default to a best-effort call-stack capture, falling back to the
  enclosing test's declaration line when the failing call's frame is gone
  (a call in tail position).
- **Bodies return `unit`**: `test`/`ftest`/`slow` take `(unit -> unit)` and
  `bracket` bodies return `unit` — 0.1.0 accepted `(unit -> 'a)` and
  ignored the result; end with an assertion or `ignore`.
- **Tags are plain strings**: every `?tags` takes a `string list` —
  `~tags:(Tag.labels [ "net" ])` → `~tags:[ "net" ]` — and the `Tag`
  module is no longer public; the Quick/Slow speed pair is the `"slow"`
  tag (`Tag.speed Slow` → the `slow` declaration or `~tags:[ "slow" ]`).
- **`run` keeps only `?argv`**: the programmatic configuration parameters
  (`~quick`, `~filter`, `~seed`, `~format`, `~junit`, `~update`,
  `~snapshot_dir`, ...) are removed — set the same knobs through the CLI
  flags or `WINDTRAP_*` variables they mirrored, or hand `run` a synthetic
  `~argv`.
- **Output formats**: `--format` (and `WINDTRAP_FORMAT`) is removed —
  terminal verbosity is one three-level axis, `-q` ⊂ default ⊂ `-v`, not a
  format: every level prints the same failure blocks and the same summary,
  and the compact glyph row stays the default as in 0.1.0. TAP is gone;
  consumers should move to `--junit PATH` or the automatic GitHub Actions
  annotations. `-q` now means `--quiet`, not `--quick` — `--quick` keeps
  its long spelling only. `--seed` takes the printed `s1:` token, not an
  integer.
- **Coverage percentages change meaning**: the stanza and
  `--instrument-with ppx_windtrap` are unchanged, but 0.2.0 grades
  expression coverage with entry points per block *and* out-edge points on
  calls that count as covered only when the call returns — numbers are not
  comparable with 0.1.0 runs. `windtrap coverage --per-file` → the per-file
  report is now the default `windtrap coverage` output, with `--min PCT` to
  gate CI and `--json` for the machine-readable artifact.
- **`open Windtrap` narrows**: it brings the flat values plus exactly four
  modules — `Testable`, `Gen`, `Exn`, and `Private` (unstable internals).
  Project modules with other names are no longer shadowed.

### Changed

- **The default output is compact, and a green run is one line**: the
  header and the per-test glyph row (green `.` pass, red `F` fail, yellow
  `S` skip, faint `x` expected failure; rows wrap at 60 with a `[k/n]`
  counter) print only when the run is noteworthy — any failure, or any
  test not tagged `slow` exceeding the slow threshold. A green, healthy
  run is exactly one named line (`mylib: 48 passed in 1.2s.`, the root
  seed appended when the suite declares properties); a noteworthy run
  flushes the header and the buffered row at the first noteworthy event,
  streams the rest glyph by glyph, and replays the failures in full at
  the end. `-v` / `--verbose` restores the line-per-test transcript (and
  keeps the slowest-tests list, now verbose-only; a passing property
  with collected labels prints its label distribution there); `-q` /
  `--quiet` keeps only the failure blocks and the summary. On a terminal
  a faint erasable `[k/n] current-test…` tail runs from the start, so a
  hung test names itself even before anything is committed; piped output
  has the same shape, flushed per glyph once noteworthy.
- **Slow-test warnings**: an untagged test exceeding the slow threshold
  earns a faint-yellow `slow: <test> took <duration>` line before the
  summary, with one trailing hint naming the opt-outs — the `slow` tag,
  or `--slow-threshold SECONDS` (`WINDTRAP_SLOW_THRESHOLD` mirror;
  default 1, `0` disables the warnings and the noteworthy trigger).
  Tests tagged `slow` are exempt everywhere; quiet mode prints no
  warnings.
- **`-q` is re-lettered from `--quick` to `--quiet`**, matching the
  near-universal CLI convention; `--quick` keeps its long form.
- **`--quiet` and `--verbose` gain environment mirrors** (`WINDTRAP_QUIET`,
  `WINDTRAP_VERBOSE`), making the output levels reachable under
  `dune runtest`, where the mirrors are the CLI — quiet previously had no
  mirror at all.

### Added

- Sixteen-verb assertion vocabulary, including new verbs that keep data in
  the failure: `satisfies` (renders the rejected value), `contains` /
  `not_contains` (print the needle and a bounded excerpt), `require_some` /
  `require_ok` / `require_error` / `require_match` (assert and unwrap), and
  the `Exn` predicates for `raises_match`.
- `Gen`: printers derive by composition (a composite prints exactly when its
  components print) and attach with `Gen.with_pp`; `such_that`, `float_any`,
  and the size-controlled `string_of`/`bytes_of`; `~examples` runs pinned
  regressions first on every run and `~count` overrides the case count per
  declaration.
- `float_exact`, a bit-exact float witness — every NaN equal to every NaN,
  `0.`/`-0.` distinct — so a test can assert that a function returns NaN;
  and `Testable.of_module` for modules with the conventional
  `t`/`pp`/`equal` trio.
- `subtest` for named sub-cases that all run even after one fails; `xfail`
  keeps known-bug reproductions in-tree without a red run; `temp_dir` /
  `temp_file` give runner-cleaned scratch paths on every outcome; `srandom`
  gives plain tests replayable randomness; `current_test` exposes the
  running test's path; `--shard K/N` deterministically partitions a suite
  across CI jobs.
- Snapshot workflow: checking is read-only and prints the acceptance
  command; update mode prints every path it writes and is refused under CI
  (`WINDTRAP_UPDATE=force` overrides); stale baselines are reported and
  `--prune` deletes them after a full, clean update run.
- Deterministic seeds: every generated value derives from the run's root
  seed, the test's path, and the case index; the root seed prints as an
  `s1:` token in the run header of any suite declaring properties, and
  every property failure prints the exact replay command for the way the
  run was invoked (`dune exec <path> -- --seed … -f '…'` under dune,
  argv0 when run directly, `WINDTRAP_SEED=… dune runtest` for inline
  suites). `srandom` gives plain tests the same replayability; a failing
  test that drew from it prints the replay command in its failure block.
- Practical ppx_expect compatibility: `let%expect_test`, `[%expect]`,
  `[%expect_exact]`, `[%expect.output]`, `let%test`, and `module%test`, with
  corrections accepted via `dune promote`. Measured against Jane Street's
  pinned ppx_expect corpus: 33/36 supported cases byte-identical to
  upstream's corrections (91.7%), and 20/20 unsupported constructs rejected
  with a loud error at the exact location.
- Coverage reporting: an inline percentage after the test results on
  instrumented runs, `--coverage`/`WINDTRAP_COVERAGE` modes (`summary`,
  `report`, `full`, `off`), and a `windtrap coverage` command that merges
  `.coverage` files, gates CI with `--min`, and emits `--json`.
- Failure reports include the tail of the test's captured output
  (`WINDTRAP_TAIL_ERRORS` controls how much).
- Failures are emitted as GitHub Actions annotations when running under
  GitHub Actions (`CI` and `GITHUB_ACTIONS` both set, as Actions sets
  them).

### Fixed

- `Stdlib.exit` called from a test body, setup, teardown, or fixture
  release no longer terminates the process mid-run with the caller's exit
  code and no report: the attempt is intercepted and recorded as that
  test's (or that release's) failure, every later test still runs, and
  the run exits through its own 0/1/2 contract — the runner owns the
  process exit.
- fix(ppx): an uncaught exception in a `let%expect_test` body is an ordinary
  test failure, never a correction. The former behavior spliced an
  unreachable `[%expect]` node after the raising statement, which broke the
  build on promote (warning 21) or duplicated the node on every
  runtest+promote cycle. Inline runs with a raising expect test now exit 1.
  To pin an expected exception, catch and print it in the body.
- A missing snapshot baseline now fails with the proposed content and the
  acceptance command; 0.1.0 silently created the baseline and passed.
- Reading captured output under `--stream` now fails with "this test
  requires capture"; 0.1.0's expect tests silently compared against the
  empty string and passed.
- JUnit XML no longer contains ANSI escape sequences.
- A raising setup or teardown is now an ordinary reported test outcome —
  0.1.0 ran group hooks outside the failure boundary, where an exception
  could take down the runner; `bracket` reports body and teardown failures
  independently.
- Stopping early with `--bail`/`-x` no longer skips cleanup: teardowns run
  on every outcome and acquired fixtures are released on every path where
  the runner regains control.

## [0.1.0] - 2026-02-13

Windtrap is an all-in-one OCaml testing framework that unifies unit tests, property-based tests, snapshot tests, and expect tests under a single API. Instead of juggling multiple testing libraries, Windtrap gives you one cohesive package with a PPX for inline expect tests (`ppx_windtrap`).

- Unit tests with combinators, tags, skip, brackets, and timeouts.
- Property-based testing with configurable seeds and shrinking.
- Snapshot testing with automatic file management and diffing.
- Inline expect tests via `ppx_windtrap` with automatic correction.
- CLI test runner with filtering, verbosity, and color support.
- Test coverage reporting with `bisect_ppx` integration.

### Acknowledgments

Windtrap builds on ideas and code from several OCaml projects:

- **[Alcotest](https://github.com/mirage/alcotest)** by Thomas Gazagnaire — test structure and runner design
- **Craig Ferguson's Alcotest PRs** ([#294](https://github.com/mirage/alcotest/pull/294), [#247](https://github.com/mirage/alcotest/pull/247)) — API design, subcomponent diffing, and Levenshtein distance (ISC)
- **[QCheck2](https://github.com/c-cube/qcheck)** by Simon Cruanes et al. — generator design and integrated shrinking (BSD 2-Clause)
- **[ppx_expect](https://github.com/janestreet/ppx_expect)** and **[ppx_inline_test](https://github.com/janestreet/ppx_inline_test)** by Jane Street — expect test paradigm and dune integration
- **[Bisect_ppx](https://github.com/aantron/bisect_ppx)** by Anton Bachin et al. — coverage instrumentation and runtime (MIT)
