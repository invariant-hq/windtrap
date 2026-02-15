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
  "Unit tests, property-based tests, snapshot tests, and expect tests in a single package with one API. Includes a PPX for inline expect tests and code coverage.")
 (depends
  (ocaml
   (>= 5.0))
  (ppxlib
   (>= 0.32.0)))
 (tags
  (testing unit-test property-testing expect-test snapshot-testing ppx)))
