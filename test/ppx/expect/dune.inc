; Generated from this directory's fixture list by ../gen/gen_dune.exe:
; do not edit. After adding or removing a fixture, regenerate with
; dune build @gen-rules --auto-promote

(rule
 (with-stdout-to
  expect_attributes.actual
  (run ./expect_pp.exe --impl %{dep:expect_attributes.ml})))

(rule
 (alias runtest)
 (action
  (diff expect_attributes.expected expect_attributes.actual)))

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
 (target reject_dropped_body.actual)
 (action
  (with-outputs-to
   reject_dropped_body.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_dropped_body.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_dropped_body.expected reject_dropped_body.actual)))

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
 (target reject_expect_prefix.actual)
 (action
  (with-outputs-to
   reject_expect_prefix.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_expect_prefix.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_expect_prefix.expected reject_expect_prefix.actual)))

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
 (target reject_expectation_prefix.actual)
 (action
  (with-outputs-to
   reject_expectation_prefix.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_expectation_prefix.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_expectation_prefix.expected reject_expectation_prefix.actual)))

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
 (target reject_leftover_attr.actual)
 (action
  (with-outputs-to
   reject_leftover_attr.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_leftover_attr.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_leftover_attr.expected reject_leftover_attr.actual)))

(rule
 (target reject_malformed_tags.actual)
 (action
  (with-outputs-to
   reject_malformed_tags.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_malformed_tags.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_malformed_tags.expected reject_malformed_tags.actual)))

(rule
 (target reject_malformed_tags_tuple.actual)
 (action
  (with-outputs-to
   reject_malformed_tags_tuple.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_malformed_tags_tuple.ml})))))

(rule
 (alias runtest)
 (action
  (diff
   reject_malformed_tags_tuple.expected
   reject_malformed_tags_tuple.actual)))

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
 (target reject_name_pattern.actual)
 (action
  (with-outputs-to
   reject_name_pattern.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_name_pattern.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_name_pattern.expected reject_name_pattern.actual)))

(rule
 (target reject_output_payload.actual)
 (action
  (with-outputs-to
   reject_output_payload.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_output_payload.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_output_payload.expected reject_output_payload.actual)))

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
 (target reject_rec_binding.actual)
 (action
  (with-outputs-to
   reject_rec_binding.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_rec_binding.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_rec_binding.expected reject_rec_binding.actual)))

(rule
 (target reject_test_anonymous_module.actual)
 (action
  (with-outputs-to
   reject_test_anonymous_module.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_test_anonymous_module.ml})))))

(rule
 (alias runtest)
 (action
  (diff
   reject_test_anonymous_module.expected
   reject_test_anonymous_module.actual)))

(rule
 (target reject_test_binding_attr.actual)
 (action
  (with-outputs-to
   reject_test_binding_attr.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_test_binding_attr.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_test_binding_attr.expected reject_test_binding_attr.actual)))

(rule
 (target reject_test_item.actual)
 (action
  (with-outputs-to
   reject_test_item.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_test_item.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_test_item.expected reject_test_item.actual)))

(rule
 (target reject_test_name_pattern.actual)
 (action
  (with-outputs-to
   reject_test_name_pattern.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_test_name_pattern.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_test_name_pattern.expected reject_test_name_pattern.actual)))

(rule
 (target reject_test_pattern_attr.actual)
 (action
  (with-outputs-to
   reject_test_pattern_attr.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_test_pattern_attr.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_test_pattern_attr.expected reject_test_pattern_attr.actual)))

(rule
 (target reject_two_bindings.actual)
 (action
  (with-outputs-to
   reject_two_bindings.actual
   (with-accepted-exit-codes
    1
    (run ./expect_pp.exe --impl %{dep:reject_two_bindings.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_two_bindings.expected reject_two_bindings.actual)))

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
