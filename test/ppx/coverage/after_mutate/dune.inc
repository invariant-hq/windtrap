; Generated from this directory's fixture list by ../gen/gen_dune.exe:
; do not edit. After adding or removing a fixture, regenerate with
; dune build @gen-rules --auto-promote

(rule
 (with-stdout-to
  fixture_guards.actual
  (run ./after_mutate_pp.exe --impl %{dep:fixture_guards.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_guards.expected fixture_guards.actual)))
