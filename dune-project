(lang dune 3.21)

(name windtrap)

; test/cli is a cram suite (the skill's "Testing an executable");
; x-blueprint's test/cram is the same shape under its own dune-project.
; See doc/dev/testing.md.

(cram enable)

(generate_opam_files true)

(implicit_transitive_deps false)

(source
 (github invariant-hq/windtrap))

(homepage "https://github.com/invariant-hq/windtrap")

(bug_reports "https://github.com/invariant-hq/windtrap/issues")

(authors "Thibaut Mattio <thibaut.mattio@gmail.com>")

(maintainers "Thibaut Mattio <thibaut.mattio@gmail.com>")

(license ISC)

(package
 (name windtrap)
 (synopsis "One library for all your OCaml tests")
 (description
  "Unit, property, stateful, and expect tests behind one flat API: every assertion failure prints the values and marks what changed, generators shrink integrally and replay from a printed seed, baselines are a literal in the test or a file it names and are accepted with dune promote, and the runner handles filtering, sharding, and JUnit output. Inline expect tests, code coverage, and mutation testing come from the companion ppx_windtrap package.")
 (depends
  (ocaml
   (>= 5.0)))
 (tags
  (testing unit-test property-testing stateful-testing expect-test snapshot-testing coverage mutation-testing)))

(package
 (name ppx_windtrap)
 (synopsis "PPX and instrumentation backend for windtrap")
 (description
  "Inline test syntax for windtrap (let%test, module%test, let%expect_test, [%expect], [%expect_exact], [%expect.output]) with corrections accepted via dune promote, plus two instrumentation backends enabled with dune's --instrument-with: ppx_windtrap.coverage for expression coverage and ppx_windtrap.mutate for mutation testing.")
 (depends
  (ocaml
   (>= 5.0))
  (windtrap
   (= :version))
  (ppxlib
   (>= 0.37.0)))
 (tags
  (testing ppx expect-test coverage mutation-testing)))
