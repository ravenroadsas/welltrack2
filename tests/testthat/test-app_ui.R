test_that("user switcher follows the ui.show_user_switcher setting", {
  users <- wt_read_users()
  hidden <- as.character(app_ui(cfg, users))
  expect_false(grepl("dev_user", hidden, fixed = TRUE))
  shown_cfg <- cfg
  shown_cfg$ui$show_user_switcher <- TRUE
  expect_true(grepl("dev_user", as.character(app_ui(shown_cfg, users)), fixed = TRUE))
})
