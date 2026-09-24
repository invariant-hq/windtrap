; Generated from this directory's fixture list by ../gen/gen_dune.exe:
; do not edit. After adding or removing a fixture, regenerate with
; dune build @gen-rules --auto-promote

(rule
 (with-stdout-to
  fixture_visits.actual
  (run ./after_coverage_pp.exe --impl %{dep:fixture_visits.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_visits.expected fixture_visits.actual)))
