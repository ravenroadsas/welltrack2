test_that("raw inputs map to process phases (first match wins)", {
  ids <- c("opp-stream_save", "opp-select", "decisions-decide_submit", "decisions-select", "nav", "weird")
  expect_equal(wt_map_activity_phase(ids, cfg),
               c("Technical assurance", "Review opportunity", "Record decision", "Prepare decision", "Navigate", "Other"))
})

test_that("event log collapses consecutive events and counts transitions", {
  t0 <- as.POSIXct("2026-01-01 10:00:00", tz = "UTC")
  log <- data.frame(ts = t0 + c(0, 10, 20, 30, 40), session_id = c("a", "a", "a", "a", "b"), user = "u",
                    phase = c("Navigate", "Navigate", "Review opportunity", "Navigate", "Navigate"),
                    opp_id = "OPP-1", stringsAsFactors = FALSE)
  el <- wt_activity_eventlog(log)
  expect_equal(nrow(el), 4)
  expect_equal(el$n_events[1], 2)
  tr <- wt_phase_transitions(el)
  expect_equal(sum(tr$n), 2)
  expect_equal(nrow(wt_activity_eventlog(log[0, ])), 0)
})
