(lang dune 3.21)

(name windtrap)

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
  "Unit, property, and snapshot tests behind one flat API: every assertion failure prints the values and marks what changed, generators shrink integrally and replay from a printed seed, snapshot baselines are keyed by name, and the runner handles filtering, sharding, and JUnit output. Inline expect tests and code coverage come from the companion ppx_windtrap package.")
 (depends
  (ocaml
   (>= 5.0)))
 (tags
  (testing unit-test property-testing expect-test snapshot-testing coverage)))

(package
 (name ppx_windtrap)
 (synopsis "PPX and instrumentation backend for windtrap")
 (description
  "Inline test syntax for windtrap (let%test, module%test, let%expect_test, [%expect], [%expect_exact], [%expect.output]) with corrections accepted via dune promote, plus the expression-coverage instrumentation backend enabled with dune's --instrument-with ppx_windtrap.")
 (depends
  (ocaml
   (>= 5.0))
  (windtrap
   (= :version))
  (ppxlib
   (>= 0.37.0)))
 (tags
  (testing ppx expect-test coverage)))
