# ppx_expect conformance corpus — triage

RFC v3, "Capture, expect, and the PPX": the compat envelope is measured
against a conformance corpus vendored from a *pinned* ppx_expect
release. This file is the initial triage: every `.ml` file of the
upstream test suite, classified.

- **Upstream**: janestreet/ppx_expect, pinned commit
  `54e2846ae50ffd72c00e528f62fb4a33948d0be2` (see NOTICE; corpus =
  upstream `test/**/*.ml`, 97 files).
- **Classes**:
  - **HONORED** — expected to run unchanged under `ppx_windtrap`
    (swap `(pps ppx_expect)` → `(pps ppx_windtrap)` and the backend).
    Passing fixtures must pass; intentionally-failing fixtures must
    reproduce the upstream `.ml.corrected.expected` byte-identically.
  - **REJECTED** — uses constructs the RFC rejects loudly
    (`[@@expect.uncaught_exn]`, `[%expect.unreachable]`,
    `[%expect.if_reached]`, `[%expectation]`, monadic
    `Expect_test_config`); must fail with the explicit
    "not supported by ppx_windtrap" diagnostic (or, for the monadic
    config, fail to compile at the ambient `Expect_test_config`
    references — mechanism (b)).
  - **N-A** — tests of ppx_expect's own internals or Jane Street build
    machinery with no user-level equivalent; each justified below. Not
    vendored.
- **Bar** (RFC): ≥ 90 % of the honored set byte-identical, 100 % of the
  rejected set loud. Measured numbers: see `RESULTS.md`.

Harness layout: `corpus/<dir>` mirrors `test/<dir>` upstream;
`corpus/*/divergent/` holds fixtures that were quarantined at first
measurement — files stayed in place when they flipped green, only
their diff rules moved to `@runtest`; the still-divergent files are
wired to `@conformance-divergent` (see `RESULTS.md`). "T" marks the
single-line source tweak listed under [Source tweaks](#source-tweaks).

## HONORED — 36 files

### Passing fixtures (16) — `(inline_tests)` libraries under @runtest

| upstream `test/` path | vendored at `corpus/` | notes |
| --- | --- | --- |
| `escaped_strings.ml` | `root/` | quoted-string payloads, `\t` `\"` `\r\n` |
| `string_extension_syntax.ml` | `root/` | `{%expect\|…\|}` / `{%expect_exact\|…\|}` |
| `test_output.ml` | `root/` | `[%expect.output]` consumption |
| `test_stderr.ml` | `root/` | stderr capture |
| `unflushed_stubs_output.ml` | `root/divergent/` | C-stub output w/o flush (+ `non_flushing.c`); conforms since the D2 fix (rules on `@runtest`) |
| `unidiomatic_syntax.ml` | `root/` | `[%%expect_test let _ = …]` form |
| `example/chdir.ml` | `example/` | `Unix.chdir` mid-test |
| `example/control_chars.ml` (T) | `example/divergent/` | control chars in payload; conforms since the D9 fix (rules on `@runtest`) |
| `example/flexible_whitespace.ml` | `example/` | |
| `example/function.ml` | `example/` | same node executed twice per call |
| `example/functor.ml` | `example/divergent/` | `module M ()` twice → duplicate tests; conforms since the D1 fix (rules on `@runtest`) |
| `example/reordered.ml` | `example/` | node order ≠ source order |
| `example/space_nine.ml` | `example/` | `[%expect_exact]` NL matrix |
| `example/xnine.ml` | `example/` | `[%expect_exact]` NL matrix |
| `explicit-strict-false/nine.ml` | `explicit-strict-false/` | upstream passes `-expect-test-strict-indentation=false` = the default (`src/ppx_expect.ml`: `strict_indent = ref false`); flag dropped in the build layer |
| `no-output-patterns/test.ml` (+ `.mli`) | `no-output-patterns/` | "(regexp)" matched literally |

### Correction fixtures (20) — runner + upstream `.ml.corrected.expected` goldens

| upstream `test/` path | vendored at `corpus/` | notes |
| --- | --- | --- |
| `negative-tests/chdir.ml` | `negative-tests/` | conforms |
| `negative-tests/escaped_strings.ml` | `negative-tests/` | conforms since the D3 fix |
| `negative-tests/exact.ml` (T) | `negative-tests/` | conforms since the D4 fix |
| `negative-tests/export_test.ml` | `negative-tests/` | passes; no correction (covered by runner exit code) |
| `negative-tests/flexible.ml` (T) | `negative-tests/` | conforms since the D4 fix |
| `negative-tests/import_test.ml` | `negative-tests/` | passes; cross-file functor instantiation |
| `negative-tests/missing.ml` (T) | `negative-tests/` | conforms since the D4/D7 fix |
| `negative-tests/nine.ml` | `negative-tests/` | **diverges** (reformat-on-match; RFC ruling pending), diff quarantined |
| `negative-tests/normal_strings.ml` | `negative-tests/` | conforms since the D3 fix (incl. margin wrapping) |
| `negative-tests/semicolon.ml` | `negative-tests/` | conforms |
| `negative-tests/similar_distinct_outputs.ml` | `negative-tests/divergent/` | conforms since the D1 fix (rules on `@runtest`) |
| `negative-tests/spacing.ml` (T) | `negative-tests/` | conforms since the D6 fix |
| `negative-tests/string_extension_syntax.ml` | `negative-tests/` | conforms since the D5 fix (retag keeps `%expect`) |
| `negative-tests/string_padding.ml` | `negative-tests/` | conforms |
| `negative-tests/three.ml` | `negative-tests/` | **diverges** (reformat-on-match; RFC ruling pending), diff quarantined |
| `negative-tests/trailing.ml` (T) | `negative-tests/` | conforms |
| `negative-tests/unidiomatic_syntax.ml` | `negative-tests/` | conforms |
| `negative-tests/unusual_payload_location.ml` | `negative-tests/` | **diverges** (upstream golden inconsistent with its pinned source — unreachable), diff quarantined |
| `negative-tests/for-mdx/foo.ml` (T) | `negative-tests/for-mdx/` | conforms |
| `explicit-strict-false/negative-test/nine.ml` | `explicit-strict-false/negative-test/` | conforms since the D4 fix |

## REJECTED — 20 files

All harnessed: expansion via `pp.exe` must exit 1 and the stderr is
goldened (`<f>.rejected.expected`), except `hello_async.ml`
(mechanism (b): compile-must-fail, `hello_async.compile-rejected.expected`).

| upstream `test/` path | rejecting construct |
| --- | --- |
| `test_expectation.ml` | `[%expectation]` |
| `test_sanitize.ml` | `[%expect.unreachable]` + `[@@expect.uncaught_exn]` (file-granular; its sanitize-override tests are honored constructs but expansion rejects the whole file) |
| `uncaught_exn.ml` | `[@@expect.uncaught_exn]` |
| `unreachable.ml` | `[%expect.unreachable]` |
| `warning_40.ml` | `[%expect.unreachable]` |
| `zero_alloc_attr.ml` | `[@@expect.uncaught_exn]` (also `[@zero_alloc]` flambda-target attrs) |
| `example/hello_async.ml` | monadic `Expect_test_config` (Async) — mechanism (b), compile-time |
| `expect-if-reached/passing_tests.ml` | `[%expect.if_reached]` / `[%expect.unreachable]` |
| `expect-if-reached/negative-test/expect_if_reacheds_are_corrected.ml` | same |
| `expect-if-reached/negative-test/expects_still_must_be_reached.ml` | `[%expect.if_reached]` |
| `negative-tests/comment.ml` | `[@@expect.uncaught_exn]` |
| `negative-tests/exn.ml` | `[@@expect.uncaught_exn]` |
| `negative-tests/exn_and_trailing.ml` | `[@@expect.uncaught_exn]` |
| `negative-tests/exn_missing.ml` | `[@@expect.uncaught_exn]` |
| `negative-tests/expectation.ml` | `[%expectation]` |
| `negative-tests/function_with_distinct_outputs.ml` | `[%expectation]` |
| `negative-tests/functor.ml` | `[@@expect.uncaught_exn]` + `[%expect.unreachable]` |
| `negative-tests/tag.ml` | `[%expect.unreachable]` (its retag corrections are otherwise honored constructs) |
| `negative-tests/zero_alloc.ml` | `[@@expect.uncaught_exn]` |
| `negative-tests/for-mdx/mdx_cases.ml` | `[%expect.unreachable]` (+ monadic config module) |

## N-A — 41 files (not vendored)

**Link aggregators** (a Jane-Street build idiom: a module list forcing
linkage; no test content) — 16 files:
`ppx_expect_test.ml`, `example/expect_test_examples.ml`,
`duplicated-by-ppx/expect_test_copied_by_ppx_tests.ml`,
`duplicated-by-ppx/negative-tests/expect_test_copied_by_ppx_negative_tests.ml`,
`expect-if-reached/expect_test_if_unreachable_tests.ml`,
`expect-if-reached/negative-test/expect_test_if_unreachable_negative_tests.ml`,
`explicit-strict-false/expect_test_explicit_no_strict_indent.ml`,
`explicit-strict-false/negative-test/expect_test_explicit_no_strict_indent_negative.ml`,
`explicit-strict-true/expect_test_explicit_strict_indent.ml`,
`explicit-strict-true/negative-test/expect_test_explicit_strict_indent_negative.ml`,
`negative-tests/expect_test_negative_tests.ml`,
`negative-tests/for-mdx/expect_test_example_for_mdx.ml`,
`negative-tests/nesting/expect_test_nesting_tests.ml`,
`negative-tests/exit-in-test/expect_test_test_exit_in_test.ml`,
`negative-tests/exit-in-test/broken-test/expect_test_call_exit_in_test.ml`,
`no-output-patterns/ppx_expect_test_no_output_patterns.ml`,
`verbose-mode/sub/expect_test_verbose_mode_tests.ml`,
`negative-tests/disabling/lib/expect_test_disabling_test_lib.ml`.
(The corpus harness expresses the same need as explicit runner mains —
`conformance_runner.ml` — or dune's generated runner.)

**ppx_expect-internal API tests** (call `Ppx_expect_runtime.For_external`
or collector knobs windtrap deliberately does not export) — 5 files:
`bad_test.ml` (overrides `upon_unreleasable_issue`, asserts the
collector's backtrace-detection warning),
`current_test_has_output_that_does_not_match_exn.ml` and
`negative-tests/current_test_has_output_that_does_not_match_exn.ml`
(`For_external.current_test_has_output_that_does_not_match_exn`),
`negative-tests/nonempty_stack.ml` (`For_external.push_output_exn`),
`force-drop/lib/sub/expect_test_force_drop_integration_lib.ml`
(force-drop runtime internals).

**Internal test-only rewriter** (`duplicated-by-ppx/`: tests how
ppx_expect handles nodes copied by *another* PPX, driven by a rewriter
that exists only for that test) — 4 files:
`ppx-duplicate/ppx_duplicate_for_ppx_expect_internal_testing.ml`,
`duplicated_expect.ml`,
`negative-tests/duplicated_expect.ml`,
`negative-tests/duplicated_inconsistent.ml`.

**Jane Street runner/console machinery** (the asserted observable is
ppx_expect's runner console text or its `inline_tests_runner` wrapper
scripts; windtrap's runner report is its own format, outside the
envelope) — 7 files:
`negative-tests/exit-in-test/test.ml` (spawns the JS runner script,
asserts the collector's "program exited while expect test was running!"
message), `negative-tests/exit-in-test/broken-test/test.ml` (its
child), `verbose-mode/sub/print_in_the_middle.ml` and
`verbose-mode/sub/test_loops.ml` (`-verbose` console format),
`source-tree-root/expect_test_source_tree_test.ml` (fixture for a cram
harness asserting `-source-tree-root`/`-verbose` console output; no
upstream byte-golden exists for its correction),
`negative-tests/disabling/lib/test_ref.ml` and
`negative-tests/disabling/main.ml` (JS inline-test drop/cookie
machinery via ppx_jane `[%test_result]`; dune's analog is
`enabled_if`).

**Non-default driver flag** (`-expect-test-strict-indentation=true`;
windtrap has the single default rule — no equivalent knob) — 2 files:
`explicit-strict-true/nine.ml`,
`explicit-strict-true/negative-test/nine.ml`.

**Unbuildable without Jane Street libraries** (zero-new-dependency rule;
would need Core/ppx_jane deriving, not just a one-line shim; their
expect-specific constructs are covered by other honored files) — 2
files: `example/tests.ml` (`[@@deriving sexp_of]`, `Core.Sexp`; its
`{xxx|…|xxx}` weird-escaping payload is the one construct not covered
elsewhere — noted in RESULTS punch list),
`negative-tests/trailing_in_module.ml` (`Sexpable`, `print_s`,
`raise_s`; nested-functor trailing corrections partially covered by
`nine.ml`'s nested `module _`).

**Expected observable is itself a rejected-family construct** — 2 files:
`negative-tests/expect_output.ml` (upstream corrects the unreached
nodes to `[%expect.unreachable]` — windtrap's contractual behavior is a
loud per-node reachability failure, mechanism (d)),
`negative-tests/nesting/nested.ml` (upstream splices
`[@@expect.uncaught_exn]` with the collector's nested-test error;
windtrap's behavior on nested expect tests is its own failure path —
punch-listed in RESULTS to be defined and tested in windtrap's own
suite).

**Not `.ml` corpus** (for completeness): `example/tabs.ml.in`
(generated into `tabs.ml` by `apply-style` at build time — the
formatter dependency is JS-internal), `no-output-patterns/test.mli`
(vendored alongside its `.ml`), C stub `test/non_flushing.c` (vendored
with `unflushed_stubs_output.ml`).

## Source tweaks

The zero-new-dependency rule forbids Core/Async/ppx_jane. Where a
vendored fixture's *build* needed them, exactly one line was
substituted — `open Core` / `open! Core` → `open Corpus_shim` /
`open! Corpus_shim` — preserving line count and all other bytes, with
the same substitution applied to the fixture's golden.
`corpus/*/corpus_shim.ml` supplies the few Core values the bodies use
(`printf`, `List.range`, …). Each such file is a FINDING, not a silent
edit:

1. `corpus/negative-tests/trailing.ml` (+ golden) — line 1.
2. `corpus/negative-tests/exact.ml` (+ golden) — line 1.
3. `corpus/negative-tests/flexible.ml` (+ golden) — line 1.
4. `corpus/negative-tests/missing.ml` (+ golden) — line 1.
5. `corpus/negative-tests/spacing.ml` (+ golden) — line 1 (`open Core`
   also provided `printf`; shim exports it).
6. `corpus/negative-tests/for-mdx/foo.ml` (+ golden) — line 2.
7. `corpus/example/divergent/control_chars.ml` — line 1 (shim provides
   `List.range`, `List.map ~f`, `String.of_char`, `String.concat ~sep`,
   `Char.of_int_exn`).

`example/hello_async.ml` is vendored byte-identical: its `open Core` /
`open Async` resolve against windtrap-authored *shim libraries*
(`corpus/example/shim/`) whose `Expect_test_config` is genuinely
monadic, which is exactly what mechanism (b) must reject at compile
time.

No other vendored byte differs from upstream.
