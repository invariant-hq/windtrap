# ppx_expect conformance — measured results

Corpus: pinned janestreet/ppx_expect
`54e2846ae50ffd72c00e528f62fb4a33948d0be2` (see `TRIAGE.md`).
First measured on 2026-07-27 against the then-current `lib/` + `ppx/`
tree (21/36); re-measured the same day after the conformance-fix pass
described under [What changed](#what-changed).

## The numbers vs the RFC bar

| set | bar | measured | met? |
| --- | --- | --- | --- |
| HONORED byte-identical | ≥ 90 % | **33 / 36 = 91.7 %** | **YES** |
| REJECTED loud with explicit diagnostic | 100 % | **20 / 20 = 100 %** | **YES** |

**Conforming (33)** — permanently pinned on `@runtest`
(`dune runtest test/conformance`):

- pass set (16): `escaped_strings`, `string_extension_syntax`,
  `test_output`, `test_stderr`, `unidiomatic_syntax` (root);
  `unflushed_stubs_output` (root/divergent — fixed D2); `chdir`,
  `flexible_whitespace`, `function`, `reordered`, `space_nine`, `xnine`
  (example); `control_chars`, `functor` (example/divergent — fixed
  D9/D1); `nine` (explicit-strict-false); `test` (no-output-patterns).
- corrections byte-identical to upstream goldens (17):
  `negative-tests/{chdir,escaped_strings,exact,flexible,missing,
  normal_strings,semicolon,spacing,string_extension_syntax,
  string_padding,trailing,unidiomatic_syntax}`,
  `negative-tests/divergent/similar_distinct_outputs` (fixed D1),
  `explicit-strict-false/negative-test/nine`, `for-mdx/foo` — plus the
  promotion-protocol exit code 0 for the whole corrections run, and
  `export_test`/`import_test` passing with no correction.
- rejected set (20): every file exits 1 at expansion with
  `… is not supported by ppx_windtrap` at the exact construct
  (goldened stderr per file), and `hello_async.ml` fails to *compile*
  at `~run:Expect_test_config.run` with
  `unit Expect_test_config.IO.t = unit Async.Deferred.t is not
  compatible with type unit` — mechanism (b) exactly as contracted.

**Divergent (3)** — quarantined on `@conformance-divergent`
(`dune build @conformance-divergent` reproduces every one; red until
resolved; fixtures stay vendored and goldens stay upstream truth):

- `negative-tests/nine.ml`, `negative-tests/three.ml` — reformat-on-match
  (old D8, needs an RFC ruling; see below).
- `negative-tests/unusual_payload_location.ml` — upstream's golden is
  inconsistent with its own pinned source (see below); byte-parity is
  unreachable by construction.

## What changed (the conformance-fix pass)

The key discovery: upstream's `.corrected.expected` goldens are the
output of a *two-stage* pipeline — the ppx_expect runtime writes
payload-only patches, then Jane Street's internal `apply-style` tool
(absent from the pinned checkout: `%{workspace_root}/bin/apply-style`)
standardizes every expect node of the corrected file. The evidence is
in the pin itself: `test/negative-tests/test-output.expected` records
the runtime's own patches (head layout untouched, quote payloads with
raw newlines), while the `.corrected.expected` goldens show collapsed/
split heads and re-escaped one-line quote strings. windtrap's writer
now folds both stages into one renderer, byte-identical on all 17
correction goldens on `@runtest`:

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
   In a corrected file, every node of the file's resolved tests is
   re-rendered in standard shape: single-line payloads collapse onto
   the node's line, multi-line payloads split the head with contents at
   node column + 2, and a reached bare `[%expect]` materializes as
   `[%expect {| |}]`. Files without corrections are never rewritten
   (mechanism (c): match ⇒ no churn), and skipped tests' nodes are
   never touched (amendment C2).
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

## Still open

### D8 — reformat-on-match (`nine.ml`, `three.ml`): needs the RFC ruling

Their payloads *match* under default flexibility; upstream's goldens
still reformat every block. That behavior is real but is the
*strict-indentation* mode: the runtime corrects matching-but-nonstandard
payloads only under `-expect-test-strict-indentation=true` (the
negative-tests directory of the upstream monorepo builds in a mode with
that effect — its `test-output.expected` shows the runtime itself
patching payloads that match flexibly), while the corpus's passing twin
`explicit-strict-false/nine.ml` *must not* produce a correction under
the default. One windtrap-wide default cannot satisfy both goldens; the
RFC's mechanism (c) ("no formatting churn on first promote") argues for
the flexible default windtrap implements. The RFC should either ratify
this narrowing explicitly (recommended: these two goldens are artifacts
of a non-default driver flag, like the N-A `explicit-strict-true` pair)
or add the strict knob.

### `unusual_payload_location.ml`: upstream golden inconsistent with its source

New finding, reclassified out of D4: the pinned checkout's
`unusual_payload_location.ml` is a normal single-line node followed by
`;;`, but its `.corrected.expected` (and `test-output.expected`)
correspond to an *older* source with blank lines inside the node, a
dangling `]`, and no `;;` — upstream's own runtime, run on the pinned
source, cannot produce the pinned golden. Byte-parity is unreachable by
construction; the fixture stays quarantined as documentation. (windtrap
produces the correct correction for the *vendored* source: standard
split-head shape, `;;` preserved.)

## Punch list (remaining)

1. **RFC ruling on reformat-on-match** (D8, above) — decides `nine.ml`
   and `three.ml` (would reach 35/36; `unusual_payload_location` is
   unreachable regardless).
2. **Coverage debts created by triage** (no action in this corpus):
   sanitize-override behavior (its corpus file `test_sanitize.ml` is
   REJECTED for unrelated constructs) and the `{xxx|…|xxx}`
   weird-escaping payload (`example/tests.ml`, N-A) should be covered
   in windtrap's own `test/ppx` suite. Nested expect tests
   (`nesting/nested.ml`, N-A) need a defined windtrap behavior + test.
3. **Runtime nit**: the inline runner writes per-test capture logs to
   `_build/_tests/<random>/…` even under a sandboxed rule (escapes the
   sandbox; the random path also makes runner logs nondeterministic —
   the harness deliberately never goldens them).
4. **Behavioral notes for the record** (unexercised by the corpus, not
   silent — documented in `lib/ppx_runtime.mli`): duplicated instances
   are renamed `name (2)` in windtrap's runner output where ppx_expect
   repeats the name; per-node reachability stays per-instance
   (mechanism (d)) where upstream's `Can_reach` tolerates an instance
   that skips a node another instance reached; simultaneous exception
   splices from several instances keep the last instance's splice.

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
  place when they flipped green — only their diff rules moved from
  `@conformance-divergent` to `@runtest` — so the vendored-path map in
  `TRIAGE.md` still holds.
- Quarantine: `@conformance-divergent` (not on `@runtest`) — the three
  files above, red by design; a fixed bug flips its file green, after
  which its rules move back to `@runtest`.
