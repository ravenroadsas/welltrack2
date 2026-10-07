test_that("rules evaluate safely", {
  r <- list(a = 5, b = NA, c = TRUE, d = "high", e = "")
  expect_true(wt_eval_rule(list(field = "a", op = ">", value = 3), r))
  expect_false(wt_eval_rule(list(field = "a", op = "<", value = 3), r))
  expect_true(wt_eval_rule(list(field = "a", op = ">=", value = 5), r))
  expect_false(wt_eval_rule(list(field = "b", op = ">", value = 0), r))
  expect_false(wt_eval_rule(list(field = "zz", op = ">", value = 0), r))
  expect_true(wt_eval_rule(list(field = "c", op = TRUE), r))
  expect_true(wt_eval_rule(list(field = "d", op = "==", value = "high"), r))
  expect_true(wt_eval_rule(list(field = "a", op = "not_null"), r))
  expect_false(wt_eval_rule(list(field = "e", op = "not_null"), r))
  expect_error(wt_eval_rule(list(field = "a", op = "%%", value = 1), r), "Unknown rule operator")
})

test_that("classification follows configured rules", {
  expect_equal(wt_classify(list(cost_kusd = 900, intervention_type = "workover"), cfg), "C")
  expect_equal(wt_classify(list(cost_kusd = 100, hse_risk = "high", intervention_type = "well_service"), cfg), "C")
  expect_equal(wt_classify(list(cost_kusd = 300, intervention_type = "well_service"), cfg), "B")
  expect_equal(wt_classify(list(cost_kusd = 60, intervention_type = "workover"), cfg), "A")
  expect_equal(wt_classify(list(cost_kusd = NA, intervention_type = "initial_completion"), cfg), "C")
})

test_that("required streams combine class, type and conditions", {
  a <- wt_required_streams(list(class = "A", intervention_type = "well_service", artificial_lift = FALSE), cfg)
  expect_equal(a, "ops_eng")
  a2 <- wt_required_streams(list(class = "A", intervention_type = "well_service", artificial_lift = TRUE), cfg)
  expect_setequal(a2, c("ops_eng", "als", "surface"))
  c <- wt_required_streams(list(class = "C", intervention_type = "workover", artificial_lift = FALSE), cfg)
  expect_length(c, 6)
  ws <- wt_applicable_workstreams(list(class = "B", artificial_lift = FALSE, regulatory_required = TRUE), cfg)
  expect_true("regulatory" %in% ws)
  expect_false("moc" %in% ws)
  expect_false("als_prep" %in% ws)
})

test_that("gate evaluation distinguishes auto, assisted and manual", {
  ctx <- make_ctx(state = "D2_PENDING", step_id = "D2")
  ev <- wt_evaluate_gate(ctx, "D2", cfg)
  expect_equal(ev$status[ev$id == "d2_streams"], "met")
  expect_equal(ev$status[ev$id == "d2_risks"], "suggested")  # no risks -> data says ok, awaits confirmation
  expect_equal(ev$status[ev$id == "d2_wpa"], "open")
  expect_lt(wt_gate_readiness(ev), 100)

  ctx$gate_checks <- data.frame(gate = "D2", criterion_id = c("d2_risks", "d2_wpa", "d2_recommend"), status = "met")
  expect_equal(wt_gate_readiness(wt_evaluate_gate(ctx, "D2", cfg)), 100)

  ctx$streams$status[1] <- "conflict"
  ev <- wt_evaluate_gate(ctx, "D2", cfg)
  expect_equal(ev$status[ev$id == "d2_no_conflict"], "open")
  expect_equal(ev$status[ev$id == "d2_streams"], "open")

  ctxa <- make_ctx(class = "A")
  expect_false("d2_econ" %in% wt_evaluate_gate(ctxa, "D2", cfg)$id)
})

test_that("economics consistency and risk checks", {
  ctx <- make_ctx(econ_basis_bopd = 200)
  expect_false(wt_run_check(list(fn = "economics_consistent"), ctx, cfg))
  ctx$risks <- data.frame(probability = 5, consequence = 4, mitigation = "", owner = "", status = "open")
  expect_false(wt_run_check(list(fn = "material_risks_mitigated"), ctx, cfg))
  expect_false(wt_run_check(list(fn = "critical_risks_closed"), ctx, cfg))
  ctx$risks$status <- "accepted"
  expect_true(wt_run_check(list(fn = "critical_risks_closed"), ctx, cfg))
  expect_error(wt_run_check(list(fn = "nope"), ctx, cfg), "Unknown check")
  ctx$changes <- data.frame(materiality = "revalidate", status = "pending")
  expect_false(wt_run_check(list(fn = "no_pending_material_change"), ctx, cfg))
})

test_that("decision transitions follow config", {
  expect_equal(wt_apply_decision("D2_PENDING", "D2", "GO", cfg), "INVEST_APPROVED")
  expect_equal(wt_apply_decision("D1_PENDING", "D1", "DEFER", cfg), "DEFERRED")
  expect_equal(wt_apply_decision("RTE_PENDING", "D3", "HOLD", cfg), "ON_HOLD")
  expect_error(wt_apply_decision("PREPARATION", "D2", "GO", cfg), "not pending")
  expect_error(wt_apply_decision("D2_PENDING", "D2", "MAYBE", cfg), "not valid")
  expect_equal(wt_next_gate("S2", cfg), "D2")
  expect_equal(wt_next_gate("D1", cfg), "D1")
  expect_true(is.na(wt_next_gate("S5", cfg)))
  expect_equal(wt_state_step("PREPARATION", cfg), "S4")
})

test_that("WPA is exception based", {
  expect_length(wt_wpa_triggers(make_ctx(), cfg), 0)
  expect_match(wt_wpa_triggers(make_ctx(class = "C"), cfg), "formal", all = FALSE)
  expect_match(wt_wpa_triggers(make_ctx(realizable_bopd = 100), cfg), "reduce potential", all = FALSE)
  expect_match(wt_wpa_triggers(make_ctx(uncertainty = "high"), cfg), "uncertainty", all = FALSE)
})

test_that("change materiality thresholds", {
  expect_equal(wt_change_materiality(5, 0, 0, cfg), "accept")
  expect_equal(wt_change_materiality(12, 0, 0, cfg), "revalidate")
  expect_equal(wt_change_materiality(0, 0, 60, cfg), "reapprove_d2")
})

test_that("status summary surfaces blockers and next action", {
  ctx <- make_ctx(state = "D2_PENDING", step_id = "D2")
  s <- wt_status_summary(ctx, cfg)
  expect_equal(s$gate, "D2")
  expect_gt(nrow(s$blockers), 0)
  expect_match(s$next_action, "Confirm|Provide")
  ctx$gate_checks <- data.frame(gate = "D2", criterion_id = c("d2_risks", "d2_wpa", "d2_recommend"), status = "met")
  s2 <- wt_status_summary(ctx, cfg)
  expect_equal(s2$readiness, 100)
  expect_match(s2$next_action, "request D2")
})
