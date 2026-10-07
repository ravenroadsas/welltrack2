test_that("packaged config loads and validates", {
  expect_type(cfg, "list")
  expect_true(wt_validate_config(cfg))
  expect_equal(wt_steps(cfg)$id, c("S1", "D1", "S2", "S3", "D2", "S4", "D3", "S5", "S6"))
})

test_that("validation catches broken configs", {
  bad <- cfg; bad$steps <- NULL
  expect_error(wt_validate_config(bad), "missing sections")
  bad <- cfg; bad$decisions$D1$outcomes$PURSUE <- "NOPE"
  expect_error(wt_validate_config(bad), "unknown states")
  bad <- cfg; bad$decisions$D1$criteria[[1]]$mode <- "magic"
  expect_error(wt_validate_config(bad), "invalid mode")
  bad <- cfg; bad$decisions$D1$criteria[[1]]$check <- NULL
  expect_error(wt_validate_config(bad), "no `check`")
  bad <- cfg; bad$decisions$D3 <- NULL
  expect_error(wt_validate_config(bad), "decision step")
})

test_that("accessors return expected shapes", {
  expect_equal(wt_sla_days(cfg, "S2", "B"), 20)
  expect_true(is.na(wt_sla_days(cfg, "XX", "B")))
  st <- wt_states(cfg)
  expect_true(all(c("CLOSED", "ON_HOLD") %in% st$id))
  expect_true(st$terminal[st$id == "CLOSED"])
  expect_setequal(wt_gate_criteria(cfg, "D3")$mode, c("auto", "assisted", "manual"))
  expect_equal(nrow(wt_streams(cfg)), 6)
  expect_equal(unname(wt_type_labels(cfg)["workover"]), "Workover")
  u <- wt_read_users()
  expect_true(all(c("user", "roles", "discipline", "authority") %in% names(u)))
})
