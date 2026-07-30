# Testing windtrap

How windtrap tests itself, and the workflows that keep the special
suites honest. `dune runtest` runs everything; scope with a directory
(`dune runtest test/unit`) or run a built test binary directly with
`-f` while iterating.

## Layout

Seven directories under `test/`:

- `unit` — the library suite, flat: one `test_<module>.ml` per `lib/`
  module, aggregated into a single windtrap-run executable
  (`main.exe`; address one module with
  `dune exec test/unit/main.exe -- -f <module>`). Four meta suites
  (`test_run`, `test_runner`, `test_ppx_runtime`, `test_windtrap`)
  drive `Runner.execute` and the ambient slot in-process with
  synthetic configs — the sanctioned way to test runner behavior with
  windtrap itself — and `execute` refuses to nest inside an active
  run, so each is a plain executable over the shared hand-rolled
  `harness.ml` (a local check counter, exit nonzero on any failure):
  the machinery being tested cannot be trusted to report its own bugs.
- `conformance` — the ppx_expect conformance corpus (below).
- `coverage`, `coverage_cli`, `coverage_ppx` — the coverage runtime,
  the `windtrap coverage` reporting command, and the instrumenter,
  including its semantics-preservation suite (below).
- `docs` — compiled documentation (below).
- `ppx` — PPX rewriting goldens (`.expected` files diffed against the
  driver's output, rejects included) and the inline-runner fixtures
  (`inline/`, `inline_coverage/`, `slow_knobs/`, `tail_loc/`).

Three kinds of compiled documentation run in the tree:

- `test/docs/test_guide.ml` — the guide's failing walkthroughs,
  executed in-process, asserting the printed diff, counterexample,
  replay and acceptance commands;
- `test/docs/test_cookbook.ml` and `test/docs/test_migrating.ml` —
  compiled mirrors of `doc/cookbook.md` and the migration reference
  (the 0.2.0 entry in `CHANGES.md`);
- `doc/manual/snippets/` — compiled mirrors of every manual chapter
  (passing snippets run green; failing walkthroughs are build-only in
  `transcript_fail.ml`, which also regenerates the manual's
  transcripts by hand).

`examples/` are real test executables wired into runtest; they double
as the run-and-exit path coverage the in-process suites cannot give.

## The ppx_expect conformance corpus

The compat promise ("most ppx_expect suites run unchanged after
swapping the pps and the backend") is measured, not asserted.
`test/conformance/` vendors the test suite of a *pinned* ppx_expect
commit (`54e2846…`, recorded in `test/conformance/NOTICE`) and classifies
every file in `TRIAGE.md`: HONORED (must pass, or must reproduce
upstream's `.ml.corrected.expected` byte-identically), REJECTED (must
fail loudly at expansion with a diagnostic naming the construct — or,
for the monadic config, fail to compile), N-A (Jane Street internals,
each justified). `RESULTS.md` records the measured numbers against the
bar: **≥ 90% of HONORED byte-identical, 100% of REJECTED loud.**

Triage workflow when a conformance diff appears:

1. Reproduce: the conforming sets run on `@runtest`; known divergences
   are quarantined on `@conformance-divergent`
   (`dune build @conformance-divergent` — red by design).
2. Decide which side is wrong. The upstream golden is truth for
   HONORED files; `RESULTS.md` documents the two cases where upstream
   itself is inconsistent or driven by a non-default flag.
3. A fixed divergence flips its fixture green: move its diff rules
   from `@conformance-divergent` back to `@runtest` (fixtures stay
   in place under `corpus/*/divergent/`), and update `RESULTS.md`.
4. Never edit vendored bytes silently: the only permitted tweak is
   the one-line `open Corpus_shim` substitution, and each is listed in
   `TRIAGE.md` as a finding.

Re-pinning the corpus to a newer ppx_expect is a deliberate act, not
maintenance: update the pin in `TRIAGE.md`, re-vendor, re-triage every
new or changed file, re-measure, and record the new numbers in
`RESULTS.md`. The goldens are upstream truth — regenerating them from
windtrap's own output would make the bar circular.

One golden is compiler-version-sensitive by nature:
`hello_async.compile-rejected.expected` pins an OCaml type error
message; regenerate it via `dune promote` on compiler upgrades.

## The coverage semantics-preservation suite

`test/coverage_ppx/semantics/` exists because coverage's out-edge
instrumentation wraps expressions *after* they return — exactly the
transformation that, mishandled, turns tail calls into stack growth,
forces lazy values, or reorders effects. Law 13 says coverage never
changes what programs mean; Law 14 makes this suite the enforcement:
`covsem_fixtures` is a real library instrumented unconditionally, and
`test_semantics.ml` runs it asserting deep tail recursion does not
overflow, evaluation order is untouched, lazy stays lazy, and every
result equals the uninstrumented answer, then reads the in-process
runtime to prove the visit calls actually counted. `test_linkonly.ml`
pins the link contract: an instrumented library links with *nothing*
but the coverage runtime injected.

**This suite must stay green. An instrumenter change that cannot keep
it green is rejected, not accommodated** — grow the fixtures with
every new expression form the instrumenter learns to touch.

The instrumenter's rewriting itself is pinned by expectation tests in
`test/coverage_ppx/` (`fixture_*.ml` → `.expected` expansions, and
`reject_*` diagnostics), same shape as the expect-PPX pins in
`test/ppx/`.

## Golden-file discipline

Wherever a `.expected` file pins output (PPX expansions, rejection
diagnostics, conformance corrections), updates go through
`dune promote`. Read every promoted diff as a code change — promotion
is where bugs get blessed. The conformance goldens are the exception:
they are upstream's bytes and are never promoted from windtrap output
(see above).

Windtrap's own snapshot baselines and `[%expect]` payloads (examples,
manual snippets, `test/ppx/inline`) follow the user-facing workflows:
`WINDTRAP_UPDATE=1 dune runtest` and `dune promote`, reviewed with
`git diff`.
