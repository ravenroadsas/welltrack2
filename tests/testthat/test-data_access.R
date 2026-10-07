test_that("database round trip, inserts and decisions", {
  skip_if_not_installed("duckdb")
  con <- wt_db_connect(":memory:")
  on.exit(wt_db_disconnect(con))
  wt_db_seed(con, mock)
  all <- wt_db_read_all(con)
  expect_setequal(names(all), wt_db_tables())
  expect_equal(nrow(all$opportunity), nrow(mock$opportunity))
  expect_error(wt_db_read(con, "users"))

  id <- wt_db_insert_opportunity(con, list(well = "CL-999", field = "F", intervention_type = "workover",
                                           title = "t", class = "B", state = "OPPORTUNITY_FRAMED", step_id = "S1",
                                           reservoir_bopd = 200, artificial_lift = TRUE))
  expect_equal(nrow(wt_db_read(con, "opportunity", id)), 1)
  expect_equal(nrow(wt_db_read(con, "step_history", id)), 1)

  wt_db_set_gate_check(con, id, "D1", "d1_fatal", TRUE, "juan.surv")
  expect_equal(nrow(wt_db_read(con, "gate_check", id)), 1)
  wt_db_set_gate_check(con, id, "D1", "d1_fatal", FALSE, "juan.surv")
  expect_equal(nrow(wt_db_read(con, "gate_check", id)), 0)

  DBI::dbExecute(con, "UPDATE opportunity SET state = 'D1_PENDING', step_id = 'D1' WHERE opp_id = ?", params = list(id))
  new <- wt_db_record_decision(con, id, "D1", "PURSUE", "nora.lead", "ok", cfg = cfg)
  expect_equal(new, "PURSUE")
  o <- wt_db_read(con, "opportunity", id)
  expect_equal(o$step_id, "S2")
  h <- wt_db_read(con, "step_history", id)
  expect_equal(sum(is.na(h$exited_at)), 1)
  expect_equal(h$step_id[is.na(h$exited_at)], "S2")
  expect_error(wt_db_record_decision(con, id, "D2", "GO", "x", "r", cfg = cfg), "not pending")

  ss <- wt_db_step_stats(con)
  expect_true(all(c("step_id", "class", "median_days") %in% names(ss)))

  ev <- data.frame(ts = Sys.time(), session_id = "s", user = "u", input_id = "nav", value = "x", opp_id = NA_character_, phase = "Navigate")
  n0 <- nrow(wt_db_read(con, "activity_log"))
  wt_db_log_activity(con, ev)
  expect_equal(nrow(wt_db_read(con, "activity_log")), n0 + 1)
})
