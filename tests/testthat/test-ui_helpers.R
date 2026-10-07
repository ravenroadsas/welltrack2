test_that("UI helpers render HTML", {
  expect_match(as.character(wt_kpi("A", "1", "x", "ok")), "wt-kpi-ok")
  expect_match(as.character(wt_status_chip("conflict")), "wt-badge-bad")
  expect_match(as.character(wt_status_chip(NA)), "-")
  expect_match(as.character(wt_progress(150)), "width:100%")
  expect_equal(wt_fmt(NA), "–")
  expect_equal(wt_fmt(1234.4, " bopd"), "1,234 bopd")
  st <- as.character(wt_stepper(cfg, "S2", "TECHNICAL_ASSURANCE"))
  expect_equal(lengths(regmatches(st, gregexpr("wt-step-decision", st))), 3)
  expect_match(st, "current")
})

test_that("board places every case in its step column", {
  o <- mock$opportunity
  html <- as.character(wt_board(o, cfg, "x"))
  expect_equal(lengths(regmatches(html, gregexpr("class=\"wt-card( wt-card-alt)?\"", html))), nrow(o))
})
