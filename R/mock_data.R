# Mock data for the UI mockup ------------------------------------------------------
#
# Generates a coherent synthetic portfolio (opportunities at every step, with
# streams, workstreams, risks, decisions, history) so the
# interface can be reviewed with realistic content. Replace with source-system
# loaders once the UI is agreed.

#' Generate a mock portfolio
#' @param cfg Config list.
#' @param n Number of opportunities.
#' @param seed Random seed.
#' @param now Reference "today".
#' @return Named list of data frames (one per database table).
#' @export
wt_mock_data <- function(cfg, n = 48, seed = 42, now = as.POSIXct("2026-10-07 08:00:00", tz = "UTC")) {
  set.seed(seed)
  users <- wt_read_users()
  steps <- wt_steps(cfg)
  owner_for <- function(discipline) {
    u <- users$user[users$discipline == discipline]
    if (length(u)) u[1] else "juan.surv"
  }

  fields <- c("Ca\u00f1o Lim\u00f3n" = "CL", "Caricare" = "CR", "La Yuca" = "LY", "Chipir\u00f3n" = "CH", "Cosecha" = "CO")
  type_ids <- c("workover", "well_service", "initial_completion")
  reasons <- c("Production decline", "ALS failure", "Integrity issue", "Reservoir opportunity", "New well", "Regulatory")
  hyp <- c("Recomplete upper sand and resize ESP", "Replace failed ESP, upsize pump stage count",
           "Add perforations in bypassed interval", "Water shut-off with mechanical isolation",
           "Convert to ESP from PCP", "Sand control and clean-out", "Initial completion with ESP, single zone",
           "Tubing leak repair and pump change")

  # Current step distribution: active pipeline + history
  step_w <- c(S1 = 5, D1 = 3, S2 = 8, S3 = 4, D2 = 3, S4 = 7, D3 = 2, S5 = 3, S6 = 13)
  opps <- vector("list", n)
  hist <- list(); streams <- list(); wss <- list(); risks <- list(); decs <- list(); gchk <- list(); chg <- list()

  for (i in seq_len(n)) {
    fld <- sample(names(fields), 1, prob = c(.4, .2, .15, .15, .1))
    type <- sample(type_ids, 1, prob = c(.55, .3, .15))
    well <- sprintf("%s-%03d", fields[[fld]], sample(10:250, 1))
    cur_step <- sample(names(step_w), 1, prob = step_w)
    cur_pos <- match(cur_step, steps$id)
    terminal_alt <- NA_character_
    if (cur_step %in% c("D1", "D2") && stats::runif(1) < .35) {
      terminal_alt <- if (cur_step == "D1") sample(c("DEFERRED", "CANCELLED"), 1) else sample(c("NO_GO", "DEFERRED"), 1)
    }
    cost <- round(switch(type,
      well_service = stats::runif(1, 40, 160),
      workover = stats::runif(1, 140, 950),
      initial_completion = stats::runif(1, 700, 2200)))
    current <- if (type == "initial_completion") 0 else round(stats::runif(1, 20, 220))
    reservoir <- round(current + stats::runif(1, 60, 520))
    rec <- list(
      opp_id = sprintf("OPP-%04d", 1000 + i), well = well, field = fld, intervention_type = type,
      title = sample(hyp, 1), reason = sample(reasons, 1),
      hypothesis = sample(hyp, 1), constraints = sample(c("Electrical capacity limited at cluster", "Rig availability Q4", "Sand production history", ""), 1),
      uncertainty = sample(c("low", "medium", "high"), 1, prob = c(.35, .45, .2)),
      originator = sample(c("ana.rmt", "juan.surv", "eduardo.prod", "diana.als"), 1),
      current_bopd = current, reservoir_bopd = reservoir,
      artificial_lift = stats::runif(1) < .6, regulatory_required = stats::runif(1) < .45,
      hse_risk = sample(c("low", "medium", "high"), 1, prob = c(.5, .4, .1)),
      novelty = stats::runif(1) < .08, cost_kusd = cost
    )
    rec$incremental_bopd <- round((reservoir - current) * 0.8)
    rec$class <- wt_classify(rec, cfg)

    # Durations per step (days): around SLA with class-dependent noise
    dur <- vapply(steps$id[seq_len(cur_pos)], function(s) {
      sla <- wt_sla_days(cfg, s, rec$class)
      max(0.5, round(sla * stats::rlnorm(1, meanlog = log(1.05), sdlog = .45), 1))
    }, numeric(1))
    closed <- cur_step == "S6" && stats::runif(1) < .7
    # Current step is partially elapsed unless the case is closed / terminal
    if (!closed && is.na(terminal_alt)) dur[cur_pos] <- round(dur[cur_pos] * stats::runif(1, .2, 1.4), 1)
    end_time <- if (closed || !is.na(terminal_alt)) now - stats::runif(1, 2, 200) * 86400 else now
    starts <- end_time - rev(cumsum(rev(dur))) * 86400
    rec$created_at <- starts[1]

    for (k in seq_len(cur_pos)) {
      exited <- if (k < cur_pos || closed || !is.na(terminal_alt)) starts[k] + dur[k] * 86400 else as.POSIXct(NA, tz = "UTC")
      hist[[length(hist) + 1]] <- data.frame(
        opp_id = rec$opp_id, step_id = steps$id[k], entered_at = starts[k], exited_at = exited,
        waiting_share = round(stats::runif(1, .25, .75), 2), recycle = FALSE, stringsAsFactors = FALSE)
    }
    # A few recycled technical cases (extra S2 pass)
    if (cur_pos > match("D2", steps$id) && stats::runif(1) < .15) {
      hist[[length(hist) + 1]] <- data.frame(
        opp_id = rec$opp_id, step_id = "S2", entered_at = starts[match("D2", steps$id)] - 5 * 86400,
        exited_at = starts[match("D2", steps$id)], waiting_share = .5, recycle = TRUE, stringsAsFactors = FALSE)
    }

    # State
    rec$step_id <- cur_step
    rec$state <- if (!is.na(terminal_alt)) terminal_alt else switch(cur_step,
      S1 = sample(c("OPPORTUNITY_DRAFT", "OPPORTUNITY_FRAMED", "NEED_MORE_INFORMATION"), 1, prob = c(.3, .55, .15)),
      D1 = "D1_PENDING", S2 = "TECHNICAL_ASSURANCE", S3 = "VALUE_ASSESSMENT", D2 = "D2_PENDING",
      S4 = if (stats::runif(1) < .15) "ON_HOLD" else "PREPARATION", D3 = "RTE_PENDING", S5 = "IN_EXECUTION",
      S6 = if (closed) "CLOSED" else "VALUE_REVIEW")
    rec$updated_at <- if (closed || !is.na(terminal_alt)) end_time else now - stats::runif(1, 0, 6) * 86400

    # Technical case & economics (available from S3 on; partial in S2)
    past_s2 <- cur_pos > match("S2", steps$id)
    rec$realizable_bopd <- if (past_s2 || (cur_step == "S2" && stats::runif(1) < .5)) round(reservoir * stats::runif(1, .72, .97)) else NA_real_
    if (!is.na(rec$realizable_bopd)) rec$incremental_bopd <- round(max(rec$realizable_bopd - current, 0) * 0.9 + 15)
    rec$cost_uncertainty_pct <- if (past_s2) sample(c(10, 15, 20, 30), 1) else NA_real_
    has_econ <- cur_pos >= match("S3", steps$id) && !(cur_step == "S3" && stats::runif(1) < .4)
    rec$npv_kusd <- if (has_econ) round(rec$incremental_bopd * stats::runif(1, 3.5, 8) - cost * .6) else NA_real_
    rec$irr_pct <- if (has_econ) round(max(5, rec$npv_kusd / cost * 40 + stats::runif(1, 10, 30))) else NA_real_
    rec$payout_months <- if (has_econ) round(max(1, cost / max(rec$incremental_bopd, 1) / 1.6), 1) else NA_real_
    rec$econ_basis_bopd <- if (has_econ) {
      if (stats::runif(1) < .15) rec$realizable_bopd + sample(c(-40, 30), 1) else rec$realizable_bopd
    } else NA_real_
    rec$baseline_version <- if (cur_pos > match("D2", steps$id)) "v1" else NA_character_

    s4_start <- if (cur_pos >= match("S4", steps$id)) starts[match("S4", steps$id)] else as.POSIXct(NA, tz = "UTC")
    rec$planned_start <- if (!is.na(s4_start)) as.Date(s4_start + wt_sla_days(cfg, "S4", rec$class) * 86400) else as.Date(NA)
    rec$planned_duration_d <- if (!is.na(s4_start)) round(wt_sla_days(cfg, "S5", rec$class) * .6) else NA_real_

    executed <- cur_pos >= match("S6", steps$id)
    rec$actual_start <- if (cur_pos >= match("S5", steps$id)) as.Date(starts[match("S5", steps$id)]) else as.Date(NA)
    rec$actual_duration_d <- if (executed) round(rec$planned_duration_d * stats::runif(1, .8, 1.5)) else NA_real_
    rec$actual_end <- if (executed) rec$actual_start + rec$actual_duration_d else as.Date(NA)
    rec$actual_cost_kusd <- if (executed) round(cost * stats::runif(1, .85, 1.35)) else NA_real_
    rec$npt_hours <- if (executed) round(stats::rexp(1, 1 / 14)) else NA_real_
    rec$actual_bopd <- if (executed) round(rec$realizable_bopd * stats::runif(1, .55, 1.15)) else NA_real_
    rec$promised_bfpd <- if (past_s2) round(rec$realizable_bopd / stats::runif(1, .25, .6)) else NA_real_
    rec$actual_bfpd <- if (executed) round(rec$promised_bfpd * stats::runif(1, .8, 1.15)) else NA_real_
    rec$energy_ratio <- if (executed) round(stats::runif(1, .85, 1.25), 2) else NA_real_
    rec$lesson <- if (closed) sample(c("Confirm electrical capacity before D2", "Pre-order pump stages for Class B",
                                       "Sand control under-designed: update template", "Good match to forecast"), 1) else NA_character_
    opps[[i]] <- rec

    # Assurance streams
    if (cur_pos >= match("S2", steps$id)) {
      req <- wt_required_streams(rec, cfg)
      gap <- if (is.na(rec$realizable_bopd)) NA_real_ else reservoir - rec$realizable_bopd
      share <- stats::runif(length(req)); share[req == "reservoir"] <- 0; share <- share / max(sum(share), 1e-9)
      for (j in seq_along(req)) {
        sdef <- Filter(function(s) s$id == req[j], cfg$assurance_streams)[[1]]
        status <- if (past_s2) "complete" else sample(c("not_started", "in_progress", "complete", "conflict"), 1, prob = c(.2, .35, .35, .1))
        streams[[length(streams) + 1]] <- data.frame(
          opp_id = rec$opp_id, stream_id = req[j], status = status, owner = owner_for(sdef$discipline),
          constraint_bopd = if (is.na(gap)) NA_real_ else round(gap * share[j]),
          note = if (status == "conflict") "Disagrees with target rate - see risk register" else "",
          updated_at = rec$updated_at - stats::runif(1, 0, 10) * 86400, stringsAsFactors = FALSE)
      }
    }

    # Readiness workstreams
    if (cur_pos >= match("S4", steps$id) && rec$state != "NO_GO") {
      app <- wt_applicable_workstreams(rec, cfg)
      for (w in app) {
        wdef <- Filter(function(x) x$id == w, cfg$readiness_workstreams)[[1]]
        status <- if (cur_pos > match("S4", steps$id)) "complete" else sample(c("not_started", "in_progress", "complete", "blocked"), 1, prob = c(.15, .4, .35, .1))
        wss[[length(wss) + 1]] <- data.frame(
          opp_id = rec$opp_id, ws_id = w, status = status, owner = owner_for(wdef$discipline),
          due_date = rec$planned_start + wdef$due_offset_days,
          note = if (w == "regulatory" && status != "complete") "F7CR filed - awaiting ANH" else "",
          stringsAsFactors = FALSE)
      }
    }

    # Risks
    if (cur_pos >= match("S2", steps$id)) {
      cats <- c("reservoir", "well integrity", "ALS", "production", "surface", "electrical", "execution", "logistics", "HSE", "regulatory", "cost", "schedule")
      descr <- c(reservoir = "Water breakthrough earlier than forecast", "well integrity" = "Casing condition unknown below packer",
                 ALS = "Pump run-life below 400 days in offset wells", production = "Unstable flow at target drawdown",
                 surface = "Flowline capacity near limit", electrical = "Transformer capacity for upsized ESP",
                 execution = "Fishing risk during pull", logistics = "Long lead pump stages", HSE = "H2S presence",
                 regulatory = "ANH approval lead time", cost = "Rig rate increase", schedule = "Rig slot conflict")
      for (r in seq_len(sample(1:4, 1))) {
        cat <- sample(cats, 1)
        p <- sample(1:5, 1); c <- sample(1:5, 1)
        mitigated <- past_s2 || stats::runif(1) < .5
        risks[[length(risks) + 1]] <- data.frame(
          opp_id = rec$opp_id, risk_id = sprintf("%s-R%d", rec$opp_id, r), category = cat, description = descr[[cat]],
          probability = p, consequence = c,
          mitigation = if (mitigated) "Mitigation defined in program" else "",
          owner = if (mitigated) owner_for(sample(c("Operations Engineering", "Production", "Asset Integrity"), 1)) else "",
          due_date = as.Date(now) + sample(-10:30, 1),
          status = if (cur_pos > match("D3", steps$id)) sample(c("closed", "accepted"), 1) else sample(c("open", "mitigating", "closed"), 1, prob = c(.5, .35, .15)),
          stringsAsFactors = FALSE)
      }
    }

    # Decisions and gate confirmations
    for (g in c("D1", "D2", "D3")) {
      gpos <- match(g, steps$id)
      auth <- cfg$decisions[[g]]$authority[[rec$class]]
      if (gpos < cur_pos || (gpos == cur_pos && !is.na(terminal_alt))) {
        outcome <- if (gpos == cur_pos) switch(terminal_alt, DEFERRED = "DEFER", CANCELLED = "REJECT", NO_GO = "NO_GO")
                   else switch(g, D1 = "PURSUE", D2 = sample(c("GO", "CONDITIONAL_GO"), 1, prob = c(.8, .2)), D3 = sample(c("READY_TO_EXECUTE", "CONDITIONAL_READY"), 1, prob = c(.85, .15)))
        decs[[length(decs) + 1]] <- data.frame(
          opp_id = rec$opp_id, gate = g, outcome = outcome, authority = auth,
          decided_by = users$user[match(auth, users$authority)],
          decided_at = starts[gpos] + dur[gpos] * 86400, baseline_version = if (g == "D1") "framing" else "v1",
          rationale = switch(g, D1 = "Attractive potential, consistent with asset plan",
                                D2 = "Economics robust at P90 cost; risks owned", D3 = "Readiness evidence complete"),
          conditions = if (outcome %in% c("CONDITIONAL_GO", "CONDITIONAL_READY")) "Confirm transformer capacity before mobilization" else "",
          superseded = FALSE, stringsAsFactors = FALSE)
      }
      if (gpos <= cur_pos) {
        for (cr in cfg$decisions[[g]]$criteria) {
          if (cr$mode == "auto" || !rec$class %in% unlist(cr$classes)) next
          if (gpos < cur_pos || stats::runif(1) < .5) {
            gchk[[length(gchk) + 1]] <- data.frame(
              opp_id = rec$opp_id, gate = g, criterion_id = cr$id, status = "met", by_user = "juan.surv",
              at = starts[gpos] + dur[gpos] * 86400 * stats::runif(1, .1, .9), evidence = "See linked document", stringsAsFactors = FALSE)
          }
        }
      }
    }

    # Change control (after D2)
    if (cur_pos > match("D2", steps$id) && stats::runif(1) < .3) {
      chg[[length(chg) + 1]] <- data.frame(
        opp_id = rec$opp_id, change_id = sprintf("%s-C1", rec$opp_id),
        description = sample(c("Cost increase 12% (rig rate)", "Equipment substitution: ESP model", "Execution delay 20 days"), 1),
        materiality = sample(c("accept", "revalidate", "reapprove_d2"), 1, prob = c(.5, .35, .15)),
        status = if (cur_pos > match("D3", steps$id)) "resolved" else sample(c("pending", "resolved"), 1),
        raised_at = now - stats::runif(1, 1, 30) * 86400, stringsAsFactors = FALSE)
    }
  }

  opp_df <- do.call(rbind, lapply(opps, function(r) as.data.frame(r, stringsAsFactors = FALSE)))
  bind <- function(x) if (length(x)) do.call(rbind, x) else data.frame()
  list(
    opportunity = opp_df,
    step_history = bind(hist),
    stream_status = bind(streams),
    workstream_status = bind(wss),
    risk = bind(risks),
    decision = bind(decs),
    gate_check = bind(gchk),
    change_request = bind(chg)
  )
}
