(lang dune 3.22)

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
  (ocaml (>= 4.14)))
 (tags
  (testing unit-test property-testing expect-test snapshot-testing)))

(package
 (name ppx_windtrap)
 (synopsis "PPX for inline expect tests with windtrap")
 (description
  "ppx_windtrap provides PPX syntax for inline expect tests, similar to ppx_expect. It enables let%expect_test and [%expect] syntax for testing output.")
 (depends
  (ocaml (>= 4.14))
  (windtrap (= :version))
  (ppxlib (>= 0.28.0)))
 (tags
  (testing ppx expect-test inline-test)))
