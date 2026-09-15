# ppx_expect conformance — measured results

Corpus: pinned janestreet/ppx_expect
`54e2846ae50ffd72c00e528f62fb4a33948d0be2` (see `TRIAGE.md`).
First measured on 2026-07-27 against the then-current `lib/` + `ppx/`
tree (21/36); re-measured the same day after the conformance-fix pass
described under [What changed](#what-changed) (33/36); re-measured on
2026-09-15 after the PPX became a desugaring into the library's own
`expect` (31/36, see the rulings under
[Where windtrap does not follow upstream](#where-windtrap-does-not-follow-upstream)).

## The bar

**Matching semantics: an adopted suite runs unchanged.** A vendored
fixture must mean to windtrap what it means to ppx_expect — the same
tests pass, the same payloads match, the same mismatches produce
corrections — or be refused loudly at expansion with a diagnostic naming
the construct.

The bar is *not* corrected-file byte-identity with the upstream goldens.
Those goldens are the output of a two-stage pipeline (see
[What changed](#what-changed)) whose second stage is Jane Street's
`bin/apply-style`, absent from the pinned checkout and from every
windtrap build. Windtrap patches the stale payload's extent and leaves
the rest of the file alone, which is what ppx_expect's *runtime* does;
the corrected-file goldens below are therefore windtrap's own recorded
output. Seven of the fifteen vendored corrected goldens are
byte-identical to the upstream bytes (the corpus's other four
`.ml.corrected.expected` files are windtrap-authored
`=== no correction produced ===` placeholders, not upstream bytes); the
eight that are not —
`negative-tests/{escaped_strings,exact,flexible,missing,normal_strings,
spacing}`, `explicit-strict-false/negative-test/nine` and
`negative-tests/trailing` — differ where the style pass used to reach
(a node head left where the author wrote it, a matching node left
untouched beside a corrected one, a long quoted payload not
continuation-wrapped at 90 columns) and where the 2026-09-15 rulings
apply.

| set | bar | measured | met? |
| --- | --- | --- | --- |
| HONORED runs with matching semantics | ≥ 90 % | **31 / 36 = 86.1 %** | **NO** — the two short of the bar are trailing output, ruled unchecked below |
| REJECTED loud with explicit diagnostic | 100 % | **20 / 20 = 100 %** | **YES** |

**Conforming (31)** — permanently pinned on `@runtest`
(`dune runtest test/conformance`):

- pass set (16): `escaped_strings`, `string_extension_syntax`,
  `test_output`, `test_stderr`, `unidiomatic_syntax` (root);
  `unflushed_stubs_output` (root/divergent — fixed D2); `chdir`,
  `flexible_whitespace`, `function`, `reordered`, `space_nine`, `xnine`
  (example); `control_chars`, `functor` (example/divergent — fixed
  D9/D1); `nine` (explicit-strict-false); `test` (no-output-patterns).
- corrections set (15): `negative-tests/{chdir,escaped_strings,exact,
  flexible,normal_strings,semicolon,spacing,string_extension_syntax,
  string_padding,unidiomatic_syntax}`,
  `negative-tests/divergent/similar_distinct_outputs`,
  `explicit-strict-false/negative-test/nine`, `for-mdx/foo` — every
  fixture mismatches where upstream's mismatches and records the
  correction upstream's runtime records, all of a body's stale nodes in
  one run, plus the promotion-protocol exit code 0 for the whole
  corrections run — and `export_test`/`import_test` passing with no
  correction. The goldens are windtrap's own output (see
  [The bar](#the-bar)); `normal_strings`, `spacing` and `nine` are
  byte-identical to the goldens the old runtime recorded, and
  `escaped_strings` differs only in the bare-node shape ruled below.
- rejected set (20): every file exits 1 at expansion with
  `… is not supported by ppx_windtrap` at the exact construct
  (goldened stderr per file), and `hello_async.ml` fails to *compile*
  at the generated `(Expect_test_config.run : (unit -> unit) -> unit)`
  reference with `unit Expect_test_config.IO.t = unit Async.Deferred.t
  is not compatible with type unit` — mechanism (b) exactly as
  contracted.

**Diverging (2)** — pinned on `@runtest` with no correction where
upstream inserts a trailing node: `negative-tests/trailing.ml` and the
first test of `negative-tests/missing.ml` (its second test conforms).

**Ruled out (3)** — not vendored, by the rulings under
[Where windtrap does not follow upstream](#where-windtrap-does-not-follow-upstream).
They were quarantined on an always-red alias while the questions were
open; the questions are closed, and three permanently red rules are a
maintenance tax on a decision, not a record of one.

## What changed (the conformance-fix pass)

The key discovery: upstream's `.corrected.expected` goldens are the
output of a *two-stage* pipeline — the ppx_expect runtime writes
payload-only patches, then Jane Street's internal `apply-style` tool
(absent from the pinned checkout: `%{workspace_root}/bin/apply-style`)
standardizes every expect node of the corrected file. The evidence is
in the pin itself: `test/negative-tests/test-output.expected` records
the runtime's own patches (head layout untouched, quote payloads with
raw newlines), while the `.corrected.expected` goldens show collapsed/
split heads and re-escaped one-line quote strings. Windtrap once folded
both stages into one renderer; it now writes the first stage only (see
[Where windtrap does not follow upstream](#where-windtrap-does-not-follow-upstream)).
The findings that pass drove out, as recorded then (the runtime they
were fixed in was replaced on 2026-09-15 by the desugaring described
under [Where windtrap does not follow upstream](#where-windtrap-does-not-follow-upstream);
the matching, formatting and escaping rules below now live in the
library's `Source_patch`, the merged reach histories of D1 and the
split-head bare node of D4/D7 do not):

1. **D5 (retag drops `%expect`) — FIXED.** Shorthand nodes
   (`{%expect|…|}`) are detected by ppx_expect's rule (payload extent
   contains the node extent) and corrected by whole-node replacement
   that keeps the extension id: `{%expect xxx|…|xxx}`.
2. **D3 (quote escaping, raw CR bytes) — FIXED.** Quote-delimited
   corrections render each line and each newline escaped onto one
   source line (`[%expect " \n a\n b\n "]`) rather than emitting raw
   bytes. The continuation-wrap at a 90-column margin that once went
   with it was the style pass's, and went with it: a long quoted
   payload now stays on its one line (`normal_strings`).
3. **D4/D6/D7 (node shape, re-indent, bare materialization) — FIXED.**
   A corrected payload is re-indented in standard shape: single-line
   contents collapse onto one line, multi-line contents sit at node
   column + 2 with the closing delimiter at node column, and a reached
   bare `[%expect]` materializes its payload. Nodes that matched are
   never rewritten (mechanism (c): match ⇒ no churn), and neither are
   skipped tests' nodes (amendment C2).
4. **D1 (duplicate registrations abort) — FIXED.** Duplicate names in a
   registration scope are renamed (`name (2)`, …) so every
   functor-instantiated test runs; expect nodes accumulate reaches
   *across* instances keyed by source span, corrections are keyed and
   replaced rather than appended, so formatted-identical outputs
   resolve to one correction (`similar_distinct_outputs` golden) and
   genuinely distinct outputs resolve to the upstream CR block.
   Trailing output resolves through the same merged history (including
   the "different trailing outputs" CR case).
5. **D2 (unflushed C stdio) — FIXED.** `Capture` now drains C stdio at
   every consumption point via `lib/capture_stubs.c`
   (`fflush(stdout); fflush(stderr)` — the exact analogue of
   `ppx_expect_runtime_flush_stubs_streams`), modeled on `lib/clock`'s
   stub layout.
6. **D9 (control-character normalization) — FIXED.** Matching now uses
   upstream's exact pipeline: split on `\n` with `\r\n` as one
   separator (a lone `\r` is an ordinary byte — the old
   `normalize_newlines` turned it into a line break), whitespace set =
   `Base.Char.is_whitespace` (adds `\011`), and the legacy
   count-spaces-but-strip-all-whitespace indentation rule.

## Where windtrap does not follow upstream

### Since 2026-09-15: the PPX is a desugaring, and only calls are checked

`ppx_windtrap` now rewrites `[%expect {|…|}]` into a call of the
library's own `expect` over the sanitized captured output, with the
node's position as the baseline, and `let%expect_test` into a `test`
run by the one runner under `--corrected`; the runtime keeps a
registry and dune's protocol and nothing else. Two consequences are
rulings, not defects, and the goldens below record them:

- **Trailing output is not checked.** ppx_expect fails a test whose
  body prints after its last node and inserts a node for it
  (`negative-tests/trailing.ml`, the first test of
  `negative-tests/missing.ml`). A desugaring has nothing after the body
  to check with; the tests pass and no correction is produced. End a
  test with the node that pins what it printed.
- **An unreached node is not a failure.** A node is a call, checked
  when the code around it runs; no vendored fixture exercises this
  (`negative-tests/expect_output.ml` is N-A).

A mismatch is a checkpoint, not an assertion: the failure is recorded
and the call returns, so a body with several stale nodes reports and
corrects all of them in one run, as ppx_expect does
(`negative-tests/{escaped_strings,normal_strings,spacing}` and
`explicit-strict-false/negative-test/nine` hold every correction).

Two more are formatting, byte-different from the goldens the old
runtime recorded: a payloadless `[%expect]` materializes its payload on
the node's line (`[%expect {|` … `|}]`) instead of the split head
(`[%expect\n    {|`); and the merged reach histories across functor
instances are gone with the `(* CR expect_test: Test ran multiple times
… *)` block — a node reached with two outputs that normalize alike still
resolves to one correction through the registry's one-content-per-run
rule (`similar_distinct_outputs` is unchanged), and two that differ make
the second reach a plain mismatch against the first's correction.

### Reformat-on-match (`nine.ml`, `three.ml`): a matching payload is left alone

Their payloads *match* under default flexibility; upstream's goldens
still reformat every block. That behavior is real but is the
*strict-indentation* mode: the runtime corrects matching-but-nonstandard
payloads only under `-expect-test-strict-indentation=true` (the
negative-tests directory of the upstream monorepo builds in a mode with
that effect — its `test-output.expected` shows the runtime itself
patching payloads that match flexibly), while the corpus's passing twin
`explicit-strict-false/nine.ml` *must not* produce a correction under
the default. One windtrap-wide default cannot satisfy both goldens, and
the RFC's mechanism (c) ("no formatting churn on first promote") settles
it for the flexible default windtrap implements: a payload that matches
is left alone. These two goldens are artifacts of a non-default driver
flag, like the N-A `explicit-strict-true` pair, and the strict knob is
not offered.

### `unusual_payload_location.ml`: upstream golden inconsistent with its source

Reclassified out of D4: the pinned checkout's
`unusual_payload_location.ml` is a normal single-line node followed by
`;;`, but its `.corrected.expected` (and `test-output.expected`)
correspond to an *older* source with blank lines inside the node, a
dangling `]`, and no `;;` — upstream's own runtime, run on the pinned
source, cannot produce the pinned golden. Byte-parity is unreachable by
construction, so the fixture is not vendored. (windtrap produces the
correct correction for the *vendored* source: standard split-head shape,
`;;` preserved.)

## Behavioural notes for the record

Unexercised by the corpus, not silent: duplicated instances are renamed
`name (2)` in windtrap's runner output where ppx_expect repeats the
name; an uncaught exception is the test's failure and withholds the
test's corrections, never a splice.

## Harness map (for whoever picks this up)

- `corpus/<dir>/` mirrors upstream `test/<dir>`; every vendored byte
  verified identical to the pin except the 7 single-line tweaks in
  `TRIAGE.md`.
- Pass sets: real `(inline_tests)` libraries — the exact swap the RFC
  promises — run by dune's backend under `@runtest`.
- Corrections sets: per-directory runner executable
  (`conformance_runner.ml`, the backend's generated main spelled out)
  driven by `drive.exe` (`test/conformance/drive.ml`), which records
  the promotion-protocol exit code and materializes
  `=== no correction produced ===` placeholders so a divergence is
  always a readable diff. Goldens: `.ml.corrected.expected`, windtrap's
  own recorded output (see [The bar](#the-bar)).
- Rejected set: `pp.exe --impl` per file, exit 1 enforced, stderr
  goldened (`*.rejected.expected`); `hello_async.ml` additionally
  typechecked against monadic shims (`corpus/example/shim/`) with a
  goldened compile error (`hello_async.compile-rejected.expected` —
  OCaml-compiler-version-sensitive by nature; regenerate via
  `dune promote` on compiler upgrades).
- The formerly-divergent fixtures under `corpus/*/divergent/` stayed in
  place when they flipped green — only their diff rules moved onto
  `@runtest` — so the vendored-path map in `TRIAGE.md` still holds.
- Everything the corpus checks is on `@runtest`. There is no quarantine
  alias: a fixture either states a contract windtrap holds, or its
  ruling is written above and the fixture is gone.
