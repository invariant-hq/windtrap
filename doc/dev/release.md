# Releasing

The checklist for cutting a windtrap release. Two packages ship
together from this repository: `windtrap` (the library, the runner,
the `windtrap` binary) and `ppx_windtrap` (the expect/inline PPX and
the coverage backend, pinned to `windtrap` with `(= :version)`).

## Bars to verify — all of them, before tagging

- [ ] `dune build` — zero warnings; warnings are treated as broken
      implementation, never silenced.
- [ ] `dune runtest` — green, which includes the examples, the
      compiled doc mirrors (`test/docs/test_{guide,cookbook,migrating}`,
      `doc/manual/snippets/`), the PPX expansion pins, and the
      conformance corpus's conforming sets.
- [ ] `dune fmt` — clean.
- [ ] Conformance bar (`test/conformance/RESULTS.md`): HONORED
      byte-identical ≥ 90%, REJECTED loud = 100%. If either number
      moved since the last release, the release notes say why.
- [ ] Coverage semantics-preservation suite green
      (`test/coverage_ppx/semantics/`) — non-negotiable (Law 14; see
      `testing.md`).
- [ ] Instrumented smoke run:
      `dune runtest --instrument-with ppx_windtrap` still green with
      the summary line present (Law 13: outcomes unchanged).
- [ ] Docs current: `doc/manual/` chapters against `lib/windtrap.mli`
      (the `.mli` is the truth), the migration notes in `CHANGES.md`
      against the surface, README against reality. Regenerated
      transcripts if the renderer changed
      (`doc/manual/snippets/transcript_fail.ml`).

## Versioning

The public surface is `lib/windtrap.mli` plus the CLI/env contract and
the exit codes. Breaking any of it — or any Law in
`architecture.md` — is a major version and reopens the design first.
Additive surface (new verbs, generators, flags) is a minor version;
fixes are a patch. `windtrap` and `ppx_windtrap` version in lockstep;
never release one without the other.

The ppx_expect conformance envelope is part of the contract:
narrowing it (a formerly-HONORED construct now rejected) is breaking.
Re-pinning the corpus to a newer upstream is a release-notes item,
with the re-measured numbers (`testing.md`, "Re-pinning").

## Cutting the release

1. `CHANGES.md`: finalize the entry — user-visible changes, migration
   notes, conformance/coverage number movements. First line names the
   version and date.
2. Bump nothing in source: the version comes from the git tag
   (`dune-release` and dune substitute it). Opam metadata lives in
   `dune-project` (`generate_opam_files`); if it changed, `dune build`
   regenerates the checked-in `.opam` files — commit them.
3. `dune-release tag` (annotated tag from `CHANGES.md`), then
   `dune-release distrib` — the tarball build must be clean from a
   pristine checkout, both packages.
4. `dune-release publish` (GitHub release + docs), then
   `dune-release opam pkg` and `dune-release opam submit` — one
   opam-repository PR carrying both packages.
5. Watch opam-repository CI: it builds on platforms and compilers we
   do not test locally (the floor is OCaml 5.0 — no
   newer-stdlib-only functions without a guard). Fix-forward on the
   PR is normal; force-pushing the tag is not. If the tarball is
   wrong, cut a `.1` patch release.
6. After merge: verify `opam install windtrap ppx_windtrap` from the
   default repository on a clean switch, and that the examples build
   against the installed packages.

## Support policy

windtrap 0.1 stays published on opam; nobody is force-upgraded (the
0.2.0 entry in `CHANGES.md` is the bridge). Patch releases of the
current series fix bugs without surface change; there is no backport
branch unless a security-grade defect appears in 0.1.
