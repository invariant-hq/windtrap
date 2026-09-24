; Generated from this directory's fixture list by ../gen/gen_dune.exe:
; do not edit. After adding or removing a fixture, regenerate with
; dune build @gen-rules --auto-promote

(rule
 (with-stdout-to
  expect_basic.actual
  (run ./expect_pp.exe --impl %{dep:expect_basic.ml})))

(rule
 (alias runtest)
 (action
  (diff expect_basic.expected expect_basic.actual)))

(rule
 (target reject_bad_payload.actual)
 (action
  (with-outputs-to
   reject_bad_payload.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_bad_payload.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_bad_payload.expected reject_bad_payload.actual)))

(rule
 (target reject_expect_outside.actual)
 (action
  (with-outputs-to
   reject_expect_outside.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_expect_outside.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_expect_outside.expected reject_expect_outside.actual)))

(rule
 (target reject_expectation.actual)
 (action
  (with-outputs-to
   reject_expectation.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_expectation.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_expectation.expected reject_expectation.actual)))

(rule
 (target reject_if_reached.actual)
 (action
  (with-outputs-to
   reject_if_reached.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_if_reached.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_if_reached.expected reject_if_reached.actual)))

(rule
 (target reject_module_attr.actual)
 (action
  (with-outputs-to
   reject_module_attr.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_module_attr.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_module_attr.expected reject_module_attr.actual)))

(rule
 (target reject_pattern_attr.actual)
 (action
  (with-outputs-to
   reject_pattern_attr.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_pattern_attr.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_pattern_attr.expected reject_pattern_attr.actual)))

(rule
 (target reject_uncaught_exn.actual)
 (action
  (with-outputs-to
   reject_uncaught_exn.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_uncaught_exn.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_uncaught_exn.expected reject_uncaught_exn.actual)))

(rule
 (target reject_unreachable.actual)
 (action
  (with-outputs-to
   reject_unreachable.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_unreachable.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_unreachable.expected reject_unreachable.actual)))

(rule
 (with-stdout-to
  test_basic.actual
  (run ./expect_pp.exe --impl %{dep:test_basic.ml})))

(rule
 (alias runtest)
 (action
  (diff test_basic.expected test_basic.actual)))
