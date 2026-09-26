# Releasing

The checklist for cutting a windtrap release. Two packages ship
together from this repository: `windtrap` (the library, the runner,
the `windtrap` binary) and `ppx_windtrap` (the expect/inline PPX and
the coverage and mutation backends, pinned to `windtrap` with
`(= :version)`).

## Bars to verify, all of them, before tagging

- [ ] `dune build`: zero warnings; warnings are treated as broken
      implementation, never silenced.
- [ ] `dune runtest`: green, which includes the examples (the manual's
      runnable mirror, one per chapter), the PPX expansion pins, and the
      conformance corpus.
- [ ] The migration page (`doc/manual/migrating-from-0.1.md`) and the
      cookbook compile against the tree by hand. Nothing compiles them,
      so every replacement spelling and recipe is checked by reading,
      or not at all.
- [ ] `dune fmt`: clean.
- [ ] Conformance bar (`test/conformance/RESULTS.md`): every pass-set
      file passes unchanged and every unsupported construct is refused,
      both `@runtest` outcomes; `test/conformance/counts.expected`
      reports the byte-identical share of the corrections. If a count
      moved since the last release, the release notes say why.
- [ ] Both semantics-preservation suites green (`test/ppx/coverage/semantics/`
      and `test/ppx/mutate/semantics/`), which nothing waives
      (guarantees 10 and 12 in `architecture.md`; see `testing.md`).
- [ ] Instrumented smoke run:
      `dune runtest --force --instrument-with ppx_windtrap.coverage` still
      green with no change in outcomes, counts or exit codes (guarantee
      10), then `dune exec windtrap -- coverage` reports the tree.
- [ ] Docs current: `doc/manual/` pages against `lib/windtrap.mli`
      (the `.mli` is the truth), the migration page against the surface,
      the README against reality.
- [ ] `SKILL.md` against the manual: it names
      commands, flags and stanzas, and nothing checks it.
- [ ] Every command a `doc/manual/` page or the README shows is re-run
      against its example, and its output is compared with the page's
      block. Durations, seeds and absolute paths may differ; any other
      difference is fixed on the page. A block that shows a failure
      needs the edit its page describes, reverted afterwards.
- [ ] The instrumented commands of `coverage.md`, `mutation.md` and
      `examples/x-blueprint/README.md` are re-run in a copy of the
      example outside the checkout, as a user's project runs them. In
      the checkout, `--instrument-with` also instruments windtrap's own
      `lib/`, and the reports list its files. Copy the example's
      directory out, add a `dune-project` whose package depends on
      `windtrap` and `ppx_windtrap` and that pins both to the checkout
      (the blueprint's README shows the `pin` stanza), run
      `dune pkg lock`, then run the page's commands from the copy's
      root. The lock records the checkout's commit, so commit first and
      lock again after every commit.

## Versioning

The public surface is `lib/windtrap.mli` plus the CLI/env contract and
the exit codes. Breaking any of it, or any of the twelve guarantees in
`architecture.md`, is a major version and reopens the design first.
Additive surface (new verbs, generators, flags) is a minor version;
fixes are a patch. `windtrap` and `ppx_windtrap` version in lockstep;
never release one without the other.

The ppx_expect conformance envelope is part of the contract:
narrowing it (a formerly-HONORED construct now rejected) is breaking.
Re-pinning the corpus to a newer upstream is a release-notes item,
with the re-measured numbers (`testing.md`, "The conformance corpus").

## Cutting the release

1. `CHANGES.md`: rename `## Unreleased` to `## vX.Y.Z YYYY-MM-DD` and
   write the highlights above the areas (see `changelog.md`), with a line
   for any conformance count that moved. The heading is the first line of
   the file.
2. Bump nothing in source: the version comes from the git tag
   (`dune-release` and dune substitute it). Opam metadata lives in
   `dune-project` (`generate_opam_files`); if it changed, `dune build`
   regenerates the checked-in `.opam` files; commit them.
3. `dune-release tag` (annotated tag from `CHANGES.md`), then
   `dune-release distrib`; the tarball build must be clean from a
   pristine checkout, both packages.
4. `dune-release publish` (GitHub release + docs), then
   `dune-release opam pkg` and `dune-release opam submit`, one
   opam-repository PR carrying both packages.
5. Watch opam-repository CI: it builds on platforms and compilers we
   do not test locally (the floor is OCaml 5.0: no
   newer-stdlib-only functions without a guard). Fix-forward on the
   PR is normal; force-pushing the tag is not. If the tarball is
   wrong, cut a `.1` patch release.
6. After merge: verify `opam install windtrap ppx_windtrap` from the
   default repository on a clean switch, and that the examples build
   against the installed packages.

## Support policy

windtrap 0.1 stays published on opam; nobody is force-upgraded
(`doc/manual/migrating-from-0.1.md` is the bridge). Patch releases of the
current series fix bugs without surface change; there is no backport
branch unless a security-grade defect appears in 0.1.
