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
  "Unit tests, property-based tests, snapshot tests, and expect tests in a single package with one API.")
 (depends
  (ocaml
   (>= 5.0)))
 (tags
  (testing unit-test property-testing expect-test snapshot-testing)))

(package
 (name ppx_windtrap)
 (synopsis "PPX and instrumentation backend for windtrap")
 (description
  "PPX syntax for tests (let%test, let%expect_test, module%test, [%expect], [%%run_tests]) and code coverage instrumentation for windtrap.")
 (depends
  (ocaml
   (>= 5.0))
  (windtrap
   (= :version))
  (ppxlib
   (>= 0.37.0)))
 (tags
  (testing ppx expect-test coverage)))
