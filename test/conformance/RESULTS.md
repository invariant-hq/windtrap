# ppx_expect conformance: the bar and the rulings

Corpus: janestreet/ppx_expect pinned at
`54e2846ae50ffd72c00e528f62fb4a33948d0be2` (see `NOTICE`); the upstream
files it does not vendor are listed under "Not vendored". This page
holds the bar the corpus is held to and the rulings where windtrap does
not follow upstream, each with its reason. It restates no number.

## The numbers

`counts.expected` holds them: the pass set, the corrections and how many
of them are byte-identical to upstream's, the refused. `count.exe`
computes them from the corpus's files on every `@runtest`, which fails
when the corpus no longer agrees with the file; `dune promote` accepts
the new numbers. Upstream's correction goldens are vendored beside
windtrap's, as `<f>.ml.corrected.upstream` (with the same one-line
tweak as their fixture, see `NOTICE`), so that the comparison is
made, not remembered.

## The bar

Ruled 2026-09-24: **every pass-set file passes unchanged, and every
construct windtrap does not support is refused**, at expansion with a
diagnostic naming the construct, or by the compiler at the generated
reference for a monadic `Expect_test_config`. Both are `@runtest`
outcomes, so a release that runs `@runtest` meets the bar or fails.

The share of corrections that are byte-identical to upstream's golden is
reported in `counts.expected` and gated by nothing. Upstream's goldens
are the output of two stages: the ppx_expect runtime writes the stale
payload's extent (the pin's own `test/negative-tests/test-output.expected`
records those patches), then Jane Street's `bin/apply-style`, absent
from the pinned checkout and from every windtrap build, restyles every
node of the corrected file. Windtrap writes the first stage only: it
patches the stale payload and leaves the rest of the file alone. A
golden that differs from upstream's differs where the style pass
reached (a node head left where the author wrote it, a matching node
left untouched beside a corrected one, a long quoted payload not wrapped
at 90 columns) or where a ruling below applies. The corrections' goldens
are therefore windtrap's own recorded output, and `converged/` runs each
corrected source again to show that it passes.

## Where windtrap does not follow upstream

### The PPX is a desugaring, and only calls are checked (2026-09-15)

`ppx_windtrap` rewrites `[%expect {|…|}]` into a call of the library's
own `expect` over the sanitized captured output, with the node's
position as the baseline, and `let%expect_test` into a `test` run by
the one runner under `--corrected`. The output a body writes after its
last node is checked when the body returns, as ppx_expect checks it
(`negative-tests/trailing.ml`, the first test of
`negative-tests/missing.ml`), and its correction appends a node. A node
the body never reaches fails its test, as in ppx_expect. Two
consequences are rulings, not defects:

- **An unreached node gets no correction.** ppx_expect corrects it to
  `[%expect.unreachable]`, a form windtrap refuses, so the failure names
  the node and the test keeps no correction until the node is reached
  or removed.
- **Each functor instance is judged alone.** ppx_expect passes a node
  that one instance of a functor reaches and another does not; windtrap
  registers a test per instance, and the instance that does not reach
  the node fails.

A mismatch is a checkpoint, not an assertion: the failure is recorded
and the call returns, so a body with several stale nodes reports and
corrects all of them in one run, as ppx_expect does.

Two differences are formatting. A payloadless `[%expect]` materializes
its payload on the node's line (`[%expect {|` … `|}]`) instead of
upstream's split head (`[%expect\n    {|`). A node reached by two
functor instances with outputs that normalize alike is corrected once;
two outputs that differ make the second reach a plain mismatch against
the first's correction, where upstream writes a `(* CR expect_test:
Test ran multiple times … *)` block.

### Reformat-on-match: a matching payload is left alone

`negative-tests/nine.ml` and `negative-tests/three.ml` are not vendored.
Their payloads match under the default flexibility, yet upstream's
goldens reformat every block. That is the strict-indentation mode: the
runtime corrects matching but nonstandard payloads only under
`-expect-test-strict-indentation=true`, which the upstream monorepo's
negative-tests build has in effect, while the corpus's passing twin
`explicit-strict-false/nine.ml` must produce no correction under the
default. One default cannot satisfy both goldens. Windtrap has the
flexible default and no strict knob, and a payload that matches is never
rewritten, so no first promotion churns a file.

### `unusual_payload_location.ml`: upstream's golden contradicts its source

Not vendored. The pinned source is a single-line node followed by
`;;`, but its `.corrected.expected` (and `test-output.expected`) belong
to an older source with blank lines inside the node, a dangling `]` and
no `;;`: upstream's own runtime, run on the pinned source, cannot
produce the pinned golden.

### A CR in an `[%expect_exact]` correction is escaped

`negative-tests/escaped_strings.ml` corrects `[%expect_exact]` nodes
over output that holds CR LF. Upstream writes the CR raw inside a
`{|…|}` literal, which OCaml 5.2 and later read as LF, so its corrected
source fails again. Windtrap writes such a correction as a quoted
literal with `\r`, and `converged/` runs the corrected source to show
that it passes.

## Not vendored

Every `.ml` file under upstream's `test/` at the pin is vendored under
`corpus/`, except the three the rulings above drop and the files below.
They test ppx_expect's own internals or Jane Street build machinery, and
have no user-level equivalent. Paths are relative to upstream's `test/`.

- Link aggregators, a Jane Street build idiom (a module list forcing
  linkage, no test content); the corpus's runner mains
  (`conformance_runner.ml`) and dune's generated runner do their work:
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
- ppx_expect-internal API tests, which call
  `Ppx_expect_runtime.For_external` or collector knobs windtrap does not
  export: `bad_test.ml`,
  `current_test_has_output_that_does_not_match_exn.ml`,
  `negative-tests/current_test_has_output_that_does_not_match_exn.ml`,
  `negative-tests/nonempty_stack.ml`,
  `force-drop/lib/sub/expect_test_force_drop_integration_lib.ml`.
- Tests of how ppx_expect handles nodes copied by another PPX, driven by
  a rewriter that exists only for them:
  `duplicated-by-ppx/ppx-duplicate/ppx_duplicate_for_ppx_expect_internal_testing.ml`,
  `duplicated-by-ppx/duplicated_expect.ml`,
  `duplicated-by-ppx/negative-tests/duplicated_expect.ml`,
  `duplicated-by-ppx/negative-tests/duplicated_inconsistent.ml`.
- Jane Street runner and console machinery, whose observable is
  ppx_expect's runner console text or its `inline_tests_runner` wrapper
  scripts: `negative-tests/exit-in-test/test.ml`,
  `negative-tests/exit-in-test/broken-test/test.ml`,
  `verbose-mode/sub/print_in_the_middle.ml`,
  `verbose-mode/sub/test_loops.ml`,
  `source-tree-root/expect_test_source_tree_test.ml`,
  `negative-tests/disabling/lib/test_ref.ml`,
  `negative-tests/disabling/main.ml`.
- The non-default driver flag `-expect-test-strict-indentation=true`,
  which windtrap has no equivalent of: `explicit-strict-true/nine.ml`,
  `explicit-strict-true/negative-test/nine.ml`.
- Unbuildable without Core and ppx_jane deriving, their expect constructs
  covered by other vendored files except the `{xxx|…|xxx}` payload of
  `example/tests.ml`: `example/tests.ml`,
  `negative-tests/trailing_in_module.ml`.
- An expected observable that is itself a refused construct:
  `negative-tests/expect_output.ml` (upstream corrects the unreached
  nodes to `[%expect.unreachable]`), `negative-tests/nesting/nested.ml`
  (upstream splices `[@@expect.uncaught_exn]` with the collector's
  nested-test error).

`example/tabs.ml.in`, which upstream's build generates into `tabs.ml`
with a Jane Street formatter, is not vendored either.
