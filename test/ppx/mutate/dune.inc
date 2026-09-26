; Generated from this directory's fixture list by ../gen/gen_dune.exe:
; do not edit. After adding or removing a fixture, regenerate with
; dune build @gen-rules --auto-promote

(rule
 (with-stdout-to
  fixture_all_dismissed.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_all_dismissed.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_all_dismissed.expected fixture_all_dismissed.actual)))

(rule
 (with-stdout-to
  fixture_ari.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_ari.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_ari.expected fixture_ari.actual)))

(rule
 (with-stdout-to
  fixture_assert.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_assert.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_assert.expected fixture_assert.actual)))

(rule
 (with-stdout-to
  fixture_chain.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_chain.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_chain.expected fixture_chain.actual)))

(rule
 (with-stdout-to
  fixture_chain_off.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_chain_off.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_chain_off.expected fixture_chain_off.actual)))

(rule
 (with-stdout-to
  fixture_cmp.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_cmp.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_cmp.expected fixture_cmp.actual)))

(rule
 (with-stdout-to
  fixture_con.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_con.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_con.expected fixture_con.actual)))

(rule
 (with-stdout-to
  fixture_contexts.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_contexts.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_contexts.expected fixture_contexts.actual)))

(rule
 (with-stdout-to
  fixture_empty.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_empty.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_empty.expected fixture_empty.actual)))

(rule
 (with-stdout-to
  fixture_exclude.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_exclude.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_exclude.expected fixture_exclude.actual)))

(rule
 (with-stdout-to
  fixture_inline_expanded.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_inline_expanded.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_inline_expanded.expected fixture_inline_expanded.actual)))

(rule
 (with-stdout-to
  fixture_inline_expect_only.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_inline_expect_only.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_inline_expect_only.expected fixture_inline_expect_only.actual)))

(rule
 (with-stdout-to
  fixture_inline_test_only.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_inline_test_only.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_inline_test_only.expected fixture_inline_test_only.actual)))

(rule
 (with-stdout-to
  fixture_inline_tests.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_inline_tests.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_inline_tests.expected fixture_inline_tests.actual)))

(rule
 (with-stdout-to
  fixture_input_name.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_input_name.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_input_name.expected fixture_input_name.actual)))

(rule
 (with-stdout-to
  fixture_lazy.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_lazy.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_lazy.expected fixture_lazy.actual)))

(rule
 (with-stdout-to
  fixture_lazy_condition.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_lazy_condition.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_lazy_condition.expected fixture_lazy_condition.actual)))

(rule
 (with-stdout-to
  fixture_lost_cmp.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_lost_cmp.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_lost_cmp.expected fixture_lost_cmp.actual)))

(rule
 (with-stdout-to
  fixture_lost_con.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_lost_con.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_lost_con.expected fixture_lost_con.actual)))

(rule
 (with-stdout-to
  fixture_neg.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_neg.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_neg.expected fixture_neg.actual)))

(rule
 (with-stdout-to
  fixture_nesting.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_nesting.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_nesting.expected fixture_nesting.actual)))

(rule
 (with-stdout-to
  fixture_off.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_off.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_off.expected fixture_off.actual)))

(rule
 (with-stdout-to
  fixture_off_edges.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_off_edges.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_off_edges.expected fixture_off_edges.actual)))

(rule
 (with-stdout-to
  fixture_off_structure.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_off_structure.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_off_structure.expected fixture_off_structure.actual)))

(rule
 (with-stdout-to
  fixture_off_unclosed.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_off_unclosed.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_off_unclosed.expected fixture_off_unclosed.actual)))

(rule
 (with-stdout-to
  fixture_payloads.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_payloads.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_payloads.expected fixture_payloads.actual)))

(rule
 (with-stdout-to
  fixture_shadow.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_shadow.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_shadow.expected fixture_shadow.actual)))

(rule
 (with-stdout-to
  fixture_texts.actual
  (run ./mutate_pp.exe --impl %{dep:fixture_texts.ml})))

(rule
 (alias runtest)
 (action
  (diff fixture_texts.expected fixture_texts.actual)))

(rule
 (target reject_bad_payload.actual)
 (action
  (with-outputs-to
   reject_bad_payload.actual
   (with-accepted-exit-codes
    1
    (run ./mutate_pp.exe --impl %{dep:reject_bad_payload.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_bad_payload.expected reject_bad_payload.actual)))

(rule
 (target reject_double_off.actual)
 (action
  (with-outputs-to
   reject_double_off.actual
   (with-accepted-exit-codes
    1
    (run ./mutate_pp.exe --impl %{dep:reject_double_off.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_double_off.expected reject_double_off.actual)))

(rule
 (target reject_empty_payload.actual)
 (action
  (with-outputs-to
   reject_empty_payload.actual
   (with-accepted-exit-codes
    1
    (run ./mutate_pp.exe --impl %{dep:reject_empty_payload.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_empty_payload.expected reject_empty_payload.actual)))

(rule
 (target reject_exclude_file_binding.actual)
 (action
  (with-outputs-to
   reject_exclude_file_binding.actual
   (with-accepted-exit-codes
    1
    (run ./mutate_pp.exe --impl %{dep:reject_exclude_file_binding.ml})))))

(rule
 (alias runtest)
 (action
  (diff
   reject_exclude_file_binding.expected
   reject_exclude_file_binding.actual)))

(rule
 (target reject_exclude_file_expr.actual)
 (action
  (with-outputs-to
   reject_exclude_file_expr.actual
   (with-accepted-exit-codes
    1
    (run ./mutate_pp.exe --impl %{dep:reject_exclude_file_expr.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_exclude_file_expr.expected reject_exclude_file_expr.actual)))

(rule
 (target reject_misplaced_exclude_file.actual)
 (action
  (with-outputs-to
   reject_misplaced_exclude_file.actual
   (with-accepted-exit-codes
    1
    (run ./mutate_pp.exe --impl %{dep:reject_misplaced_exclude_file.ml})))))

(rule
 (alias runtest)
 (action
  (diff
   reject_misplaced_exclude_file.expected
   reject_misplaced_exclude_file.actual)))

(rule
 (target reject_misplaced_on.actual)
 (action
  (with-outputs-to
   reject_misplaced_on.actual
   (with-accepted-exit-codes
    1
    (run ./mutate_pp.exe --impl %{dep:reject_misplaced_on.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_misplaced_on.expected reject_misplaced_on.actual)))

(rule
 (target reject_off_number.actual)
 (action
  (with-outputs-to
   reject_off_number.actual
   (with-accepted-exit-codes
    1
    (run ./mutate_pp.exe --impl %{dep:reject_off_number.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_off_number.expected reject_off_number.actual)))

(rule
 (target reject_off_rewrite.actual)
 (action
  (with-outputs-to
   reject_off_rewrite.actual
   (with-accepted-exit-codes
    1
    (run ./mutate_pp.exe --impl %{dep:reject_off_rewrite.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_off_rewrite.expected reject_off_rewrite.actual)))

(rule
 (target reject_off_two_reasons.actual)
 (action
  (with-outputs-to
   reject_off_two_reasons.actual
   (with-accepted-exit-codes
    1
    (run ./mutate_pp.exe --impl %{dep:reject_off_two_reasons.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_off_two_reasons.expected reject_off_two_reasons.actual)))

(rule
 (target reject_on_assert.actual)
 (action
  (with-outputs-to
   reject_on_assert.actual
   (with-accepted-exit-codes
    1
    (run ./mutate_pp.exe --impl %{dep:reject_on_assert.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_on_assert.expected reject_on_assert.actual)))

(rule
 (target reject_on_binding.actual)
 (action
  (with-outputs-to
   reject_on_binding.actual
   (with-accepted-exit-codes
    1
    (run ./mutate_pp.exe --impl %{dep:reject_on_binding.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_on_binding.expected reject_on_binding.actual)))

(rule
 (target reject_on_outside.actual)
 (action
  (with-outputs-to
   reject_on_outside.actual
   (with-accepted-exit-codes
    1
    (run ./mutate_pp.exe --impl %{dep:reject_on_outside.ml})))))

(rule
 (alias runtest)
 (action
  (diff reject_on_outside.expected reject_on_outside.actual)))
