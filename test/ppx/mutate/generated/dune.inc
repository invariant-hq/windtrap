; Generated from this directory's fixture list by ../gen/gen_dune.exe:
; do not edit. After adding or removing a fixture, regenerate with
; dune build @gen-rules --auto-promote

(rule
 (with-stdout-to
  fixture_generated.actual
  (run ./generated_pp.exe --impl %{dep:fixture_generated.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_generated.expected fixture_generated.actual)))

(rule
 (with-stdout-to
  fixture_generated_chain.actual
  (run ./generated_pp.exe --impl %{dep:fixture_generated_chain.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_generated_chain.expected fixture_generated_chain.actual)))
