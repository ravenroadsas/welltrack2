make_prod <- function(q0 = 300, d = 0.3, n = 24, noise = 0) {
  months <- seq(as.Date("2024-10-01"), by = "month", length.out = n)
  t <- as.numeric(months - months[1]) / 365.25
  data.frame(well = "W", month = months, oil_bopd = q0 * exp(-d * t) * exp(noise))
}

test_that("catalog loads, validates and is attached to the config", {
  expect_true("decline_curve" %in% names(cfg$analyses))
  expect_equal(cfg$analyses$nodal_quicklook$kind, "app")
  bad <- cfg; bad$decisions$D2$criteria[[1]]$analyses <- list("nope")
  expect_error(wt_validate_config(bad), "unknown analyses")
  tmp <- tempfile(fileext = ".yml")
  yaml::write_yaml(list(analyses = list(list(id = "x", kind = "app"))), tmp)
  expect_error(wt_load_analyses(tmp), "needs a `url`")
})

test_that("processes reference analyses by id (one analysis, several criteria)", {
  l <- wt_criterion_analyses(cfg)
  expect_true(all(l$analysis_id %in% names(cfg$analyses)))
  expect_setequal(l$criterion_id[l$analysis_id == "decline_curve"], c("d1_info", "d2_baseline"))
  expect_true(all(names(wt_analysis_registry()) %in% names(cfg$analyses)))
})

test_that("exponential decline fit recovers the true decline", {
  r <- wt_an_decline_fit(make_prod(d = 0.3), months = 24, model = "exponential")
  expect_equal(r$outputs$decline_pct_yr, round(100 * (1 - exp(-0.3)), 1), tolerance = 0.2)
  expect_equal(r$outputs$qi_bopd, 300, tolerance = 1)
  expect_gt(r$outputs$r2, 0.99)
  expect_equal(nrow(r$forecast), 12)
  expect_true(all(diff(r$forecast$oil_bopd) < 0))
  expect_match(r$data_ref, "24 points")
})

test_that("harmonic fit, downtime and errors", {
  p <- make_prod()
  p$oil_bopd[5] <- 0
  r <- wt_an_decline_fit(p, months = 24, model = "harmonic")
  expect_match(r$data_ref, "23 points")
  expect_gt(r$outputs$decline_pct_yr, 0)
  expect_error(wt_an_decline_fit(p[0, ]), "No production")
  expect_error(wt_an_decline_fit(make_prod(n = 2)), "At least 3")
})

test_that("evidence record, gate check and case analyses", {
  r <- wt_an_decline_fit(make_prod(), 12)
  rec <- wt_evidence_record("OPP-T", "D2", "d2_baseline", cfg$analyses$decline_curve, r, "ana.rmt")
  expect_equal(nrow(rec), 1)
  expect_equal(jsonlite::fromJSON(rec$outputs)$decline_pct_yr, r$outputs$decline_pct_yr)
  expect_equal(rec$status, "submitted")

  ctx <- make_ctx(state = "D2_PENDING", step_id = "D2")
  ev <- wt_evaluate_gate(ctx, "D2", cfg)
  expect_equal(ev$status[ev$id == "d2_baseline"], "met")
  ctx$evidence$criterion_id <- "d1_info"   # same analysis, other criterion: does not count
  ev <- wt_evaluate_gate(ctx, "D2", cfg)
  expect_equal(ev$status[ev$id == "d2_baseline"], "open")
  ctx$evidence <- ctx$evidence[0, ]

  ca <- wt_case_analyses(ctx, cfg)
  expect_true(all(ca$current_gate[ca$gate == "D2"]))
  expect_equal(ca$gate[1], "D2")
  expect_true(all(ca$evidence_status == "none"))
  expect_false("d2_baseline" %in% wt_case_analyses(make_ctx(class = "A"), cfg)$criterion_id)
})

test_that("deep link to external analysis app", {
  u <- wt_analysis_link(cfg$analyses$nodal_quicklook, "OPP-1", "d2_realizable", "https://x/y?a=1")
  expect_match(u, "^https://connect.example.com/nodal/\\?opp=OPP-1&criterion=d2_realizable&return=https%3A%2F%2F")
})

test_that("analysis permissions follow catalog disciplines", {
  users <- wt_read_users()
  a <- cfg$analyses$decline_curve
  expect_true(wt_can_run_analysis(wt_resolve_user("ana.rmt", character(), users, cfg), a, cfg))
  expect_false(wt_can_run_analysis(wt_resolve_user("hector.reg", character(), users, cfg), a, cfg))
  expect_true(wt_can_run_analysis(wt_resolve_user("juan.surv", character(), users, cfg), a, cfg))
})

test_that("evidence is stored and supersedes earlier evidence", {
  con <- wt_db_connect(":memory:")
  on.exit(wt_db_disconnect(con))
  wt_db_seed(con, mock)
  r <- wt_an_decline_fit(make_prod(), 12)
  id <- mock$opportunity$opp_id[1]
  wt_db_add_evidence(con, wt_evidence_record(id, "D2", "d2_baseline", cfg$analyses$decline_curve, r, "u"))
  wt_db_add_evidence(con, wt_evidence_record(id, "D2", "d2_baseline", cfg$analyses$decline_curve, r, "u", Sys.time() + 1))
  ev <- DBI::dbGetQuery(con, "SELECT status FROM evidence WHERE opp_id = ? AND criterion_id = 'd2_baseline'", params = list(id))
  expect_equal(sum(ev$status == "submitted"), 1)
  expect_gte(sum(ev$status == "superseded"), 1)
  ctx <- wt_case_context(wt_db_read_all(con), id)
  expect_true(all(ctx$production$well == ctx$opp$well))
})
