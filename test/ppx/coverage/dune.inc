; Generated from this directory's fixture list by ../gen/gen_dune.exe:
; do not edit. After adding or removing a fixture, regenerate with
; dune build @gen-rules --auto-promote

(rule
 (with-stdout-to
  fixture_and_or.actual
  (run ./coverage_pp.exe --impl %{dep:fixture_and_or.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_and_or.expected fixture_and_or.actual)))

(rule
 (with-stdout-to
  fixture_apply.actual
  (run ./coverage_pp.exe --impl %{dep:fixture_apply.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_apply.expected fixture_apply.actual)))

(rule
 (with-stdout-to
  fixture_class.actual
  (run ./coverage_pp.exe --impl %{dep:fixture_class.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_class.expected fixture_class.actual)))

(rule
 (with-stdout-to
  fixture_empty.actual
  (run ./coverage_pp.exe --impl %{dep:fixture_empty.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_empty.expected fixture_empty.actual)))

(rule
 (with-stdout-to
  fixture_exclude.actual
  (run ./coverage_pp.exe --impl %{dep:fixture_exclude.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_exclude.expected fixture_exclude.actual)))

(rule
 (with-stdout-to
  fixture_fun.actual
  (run ./coverage_pp.exe --impl %{dep:fixture_fun.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_fun.expected fixture_fun.actual)))

(rule
 (with-stdout-to
  fixture_if_loops.actual
  (run ./coverage_pp.exe --impl %{dep:fixture_if_loops.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_if_loops.expected fixture_if_loops.actual)))

(rule
 (with-stdout-to
  fixture_lazy.actual
  (run ./coverage_pp.exe --impl %{dep:fixture_lazy.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_lazy.expected fixture_lazy.actual)))

(rule
 (with-stdout-to
  fixture_letop.actual
  (run ./coverage_pp.exe --impl %{dep:fixture_letop.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_letop.expected fixture_letop.actual)))

(rule
 (with-stdout-to
  fixture_match.actual
  (run ./coverage_pp.exe --impl %{dep:fixture_match.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_match.expected fixture_match.actual)))

(rule
 (with-stdout-to
  fixture_off.actual
  (run ./coverage_pp.exe --impl %{dep:fixture_off.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_off.expected fixture_off.actual)))

(rule
 (with-stdout-to
  fixture_or_tail_branch.actual
  (run ./coverage_pp.exe --impl %{dep:fixture_or_tail_branch.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_or_tail_branch.expected fixture_or_tail_branch.actual)))

(rule
 (with-stdout-to
  fixture_or_tail_scope.actual
  (run ./coverage_pp.exe --impl %{dep:fixture_or_tail_scope.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_or_tail_scope.expected fixture_or_tail_scope.actual)))

(rule
 (with-stdout-to
  fixture_or_tail_wrap.actual
  (run ./coverage_pp.exe --impl %{dep:fixture_or_tail_wrap.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_or_tail_wrap.expected fixture_or_tail_wrap.actual)))

(rule
 (with-stdout-to
  fixture_pipeline.actual
  (run ./coverage_pp.exe --impl %{dep:fixture_pipeline.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_pipeline.expected fixture_pipeline.actual)))

(rule
 (with-stdout-to
  fixture_scope.actual
  (run ./coverage_pp.exe --impl %{dep:fixture_scope.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_scope.expected fixture_scope.actual)))

(rule
 (target reject_bad_payload.actual)
 (action
  (with-outputs-to
   reject_bad_payload.actual
   (with-accepted-exit-codes
    1
    (run ./coverage_pp.exe --impl %{dep:reject_bad_payload.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_bad_payload.expected reject_bad_payload.actual)))

(rule
 (target reject_misplaced_on.actual)
 (action
  (with-outputs-to
   reject_misplaced_on.actual
   (with-accepted-exit-codes
    1
    (run ./coverage_pp.exe --impl %{dep:reject_misplaced_on.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_misplaced_on.expected reject_misplaced_on.actual)))

(rule
 (target reject_off_reason.actual)
 (action
  (with-outputs-to
   reject_off_reason.actual
   (with-accepted-exit-codes
    1
    (run ./coverage_pp.exe --impl %{dep:reject_off_reason.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_off_reason.expected reject_off_reason.actual)))

(rule
 (with-stdout-to
  reject_bad_payload.here.parity
  (run ../gen/gen_dune.exe normalize %{dep:reject_bad_payload.actual})))

(rule
 (with-stdout-to
  reject_bad_payload.twin.parity
  (run
   ../gen/gen_dune.exe
   normalize
   %{dep:../mutate/reject_bad_payload.actual})))

(rule
 (alias runtest)
 (action
  (diff reject_bad_payload.here.parity reject_bad_payload.twin.parity)))

(rule
 (with-stdout-to
  reject_misplaced_on.here.parity
  (run ../gen/gen_dune.exe normalize %{dep:reject_misplaced_on.actual})))

(rule
 (with-stdout-to
  reject_misplaced_on.twin.parity
  (run
   ../gen/gen_dune.exe
   normalize
   %{dep:../mutate/reject_misplaced_on.actual})))

(rule
 (alias runtest)
 (action
  (diff reject_misplaced_on.here.parity reject_misplaced_on.twin.parity)))
