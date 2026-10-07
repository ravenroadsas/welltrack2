cfg <- wt_load_config()

# Minimal case context builder for tests
make_ctx <- function(...) {
  opp <- list(opp_id = "OPP-T", well = "CL-001", field = "F", intervention_type = "workover", class = "B",
              state = "TECHNICAL_ASSURANCE", step_id = "S2", title = "t", reason = "r", hypothesis = "h",
              uncertainty = "low", current_bopd = 50, reservoir_bopd = 300, realizable_bopd = 250,
              incremental_bopd = 180, cost_kusd = 300, npv_kusd = 900, econ_basis_bopd = 250,
              artificial_lift = FALSE, regulatory_required = FALSE, hse_risk = "low", novelty = FALSE)
  opp <- utils::modifyList(opp, list(...))
  req <- wt_required_streams(opp, cfg)
  list(
    opp = opp,
    streams = data.frame(opp_id = "OPP-T", stream_id = req, status = "complete", owner = "x", stringsAsFactors = FALSE),
    workstreams = data.frame(opp_id = character(), ws_id = character(), status = character()),
    risks = data.frame(probability = numeric(), consequence = numeric(), mitigation = character(), owner = character(), status = character()),
    decisions = data.frame(gate = character(), outcome = character(), superseded = logical()),
    gate_checks = data.frame(gate = character(), criterion_id = character(), status = character()),
    changes = data.frame(materiality = character(), status = character())
  )
}

mock <- wt_mock_data(cfg, n = 30, seed = 1, now = as.POSIXct("2026-10-07 08:00:00", tz = "UTC"))
mock_now <- as.POSIXct("2026-10-07 08:00:00", tz = "UTC")
