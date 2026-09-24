# ppx_expect conformance: the bar and the rulings

Corpus: janestreet/ppx_expect pinned at
`54e2846ae50ffd72c00e528f62fb4a33948d0be2` (see `NOTICE`); every
upstream file is classified in `TRIAGE.md`. This page holds the bar the
corpus is held to and the rulings where windtrap does not follow
upstream, each with its reason. It restates no number.

## The numbers

`counts.expected` holds them: the pass set, the corrections and how many
of them are byte-identical to upstream's, the refused. `count.exe`
computes them from the corpus's files on every `@runtest`, which fails
when the corpus no longer agrees with the file; `dune promote` accepts
the new numbers. Upstream's correction goldens are vendored beside
windtrap's, as `<f>.ml.corrected.upstream` (with the same one-line
tweak as their fixture, see `TRIAGE.md`), so that the comparison is
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
the one runner under `--corrected`. Two consequences are rulings, not
defects:

- **Trailing output is not checked.** ppx_expect fails a test whose
  body prints after its last node and inserts a node for it
  (`negative-tests/trailing.ml`, the first test of
  `negative-tests/missing.ml`). A desugaring has nothing after the body
  to check with: the tests pass and no correction is written. End a
  test with the node that pins what it printed.
- **An unreached node is not a failure.** A node is a call, checked
  when the code around it runs.

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

## Known failure

`negative-tests/escaped_strings.ml`'s correction does not converge on
OCaml 5.2 and later: it writes the CR LF of an `[%expect_exact]` output
raw inside a `{|…|}` literal, which those compilers read as LF, so the
corrected source mismatches again. `converged/` pins that failure as it
is until the product decides.
