users <- wt_read_users()

test_that("dev user resolves from env and table", {
  withr_env <- Sys.getenv("WT_DEV_USER")
  Sys.setenv(WT_DEV_USER = "laura.am")
  on.exit(Sys.setenv(WT_DEV_USER = withr_env))
  u <- wt_resolve_user(NULL, NULL, users, cfg)
  expect_true(u$dev)
  expect_equal(u$authority, "Asset Manager")
  expect_true(wt_can(u, "decide", cfg))
  expect_true(wt_can_decide(u, "D2", "B", cfg))
  expect_false(wt_can_decide(u, "D2", "C", cfg))
})

test_that("Connect groups grant roles; unknown users are viewers", {
  u <- wt_resolve_user("stranger", c("wt_integrators"), users, cfg)
  expect_false(u$dev)
  expect_true("integrator" %in% u$roles)
  expect_equal(wt_resolve_user("nobody", character(), users, cfg)$roles, "viewer")
})

test_that("contributors edit only their discipline", {
  u <- wt_resolve_user("diana.als", character(), users, cfg)
  expect_true(wt_can_edit_item(u, "ALS", "edit_stream", cfg))
  expect_false(wt_can_edit_item(u, "Reservoir", "edit_stream", cfg))
  i <- wt_resolve_user("juan.surv", character(), users, cfg)
  expect_true(wt_can_edit_item(i, "Reservoir", "edit_stream", cfg))
})
