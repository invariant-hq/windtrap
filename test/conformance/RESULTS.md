# ppx_expect conformance — measured results

Corpus: pinned janestreet/ppx_expect
`54e2846ae50ffd72c00e528f62fb4a33948d0be2` (see `TRIAGE.md`).
First measured on 2026-07-27 against the then-current `lib/` + `ppx/`
tree (21/36); re-measured the same day after the conformance-fix pass
described under [What changed](#what-changed).

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
output. Eight of the fifteen are byte-identical to the vendored upstream
bytes anyway; the seven that are not —
`negative-tests/{escaped_strings,exact,flexible,missing,normal_strings,
spacing}` and `explicit-strict-false/negative-test/nine` — differ only
where the style pass used to reach: a node head left where the author
wrote it, a matching node left untouched beside a corrected one, and a
long quoted payload not continuation-wrapped at 90 columns.

| set | bar | measured | met? |
| --- | --- | --- | --- |
| HONORED runs with matching semantics | ≥ 90 % | **33 / 36 = 91.7 %** | **YES** |
| REJECTED loud with explicit diagnostic | 100 % | **20 / 20 = 100 %** | **YES** |

**Conforming (33)** — permanently pinned on `@runtest`
(`dune runtest test/conformance`):

- pass set (16): `escaped_strings`, `string_extension_syntax`,
  `test_output`, `test_stderr`, `unidiomatic_syntax` (root);
  `unflushed_stubs_output` (root/divergent — fixed D2); `chdir`,
  `flexible_whitespace`, `function`, `reordered`, `space_nine`, `xnine`
  (example); `control_chars`, `functor` (example/divergent — fixed
  D9/D1); `nine` (explicit-strict-false); `test` (no-output-patterns).
- corrections set (17): `negative-tests/{chdir,escaped_strings,exact,
  flexible,missing,normal_strings,semicolon,spacing,
  string_extension_syntax,string_padding,trailing,unidiomatic_syntax}`,
  `negative-tests/divergent/similar_distinct_outputs` (fixed D1),
  `explicit-strict-false/negative-test/nine`, `for-mdx/foo` — every
  fixture mismatches where upstream's mismatches and records the
  correction upstream's runtime records, plus the promotion-protocol
  exit code 0 for the whole corrections run, and
  `export_test`/`import_test` passing with no correction. The goldens
  are windtrap's own output (see [The bar](#the-bar)).
- rejected set (20): every file exits 1 at expansion with
  `… is not supported by ppx_windtrap` at the exact construct
  (goldened stderr per file), and `hello_async.ml` fails to *compile*
  at `~run:Expect_test_config.run` with
  `unit Expect_test_config.IO.t = unit Async.Deferred.t is not
  compatible with type unit` — mechanism (b) exactly as contracted.

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
The findings that pass drove out, all of them still fixed:

1. **D5 (retag drops `%expect`) — FIXED.** Shorthand nodes
   (`{%expect|…|}`) are detected by ppx_expect's rule (payload extent
   contains the node extent) and corrected by whole-node replacement
   that keeps the extension id: `{%expect xxx|…|xxx}`.
2. **D3 (quote escaping, raw CR bytes) — FIXED.** Quote-delimited
   corrections render each line and each newline escaped onto one
   source line (`[%expect " \n a\n b\n "]`), wrapped with
   line-continuation escapes at the 90-column margin
   (`normal_strings`' wrapped shape reproduced byte-for-byte).
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
name; per-node reachability stays per-instance (mechanism (d)) where
upstream's `Can_reach` tolerates an instance that skips a node another
instance reached; simultaneous exception splices from several instances
keep the last instance's splice.

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
  always a readable diff. Goldens: upstream `.ml.corrected.expected`.
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
