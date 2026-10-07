# Process time statistics and value KPIs ------------------------------------------
#
# Pure functions over the step history. In production the heavy aggregation
# moves into SQL views (see wt_db_* and README "Performance"), these functions
# then only shape results for charts.

#' Build the case context for one opportunity
#' @param data Named list of tables (see [wt_mock_data()] / [wt_db_read_all()]).
#' @param opp_id Opportunity id.
#' @export
wt_case_context <- function(data, opp_id) {
  pick <- function(tbl) {
    x <- data[[tbl]]
    if (is.null(x) || !nrow(x)) return(x)
    x[x$opp_id == opp_id, , drop = FALSE]
  }
  opp <- data$opportunity[data$opportunity$opp_id == opp_id, , drop = FALSE]
  if (!nrow(opp)) stop("Unknown opportunity ", opp_id, call. = FALSE)
  prod <- data$production_history
  list(opp = as.list(opp), streams = pick("stream_status"), workstreams = pick("workstream_status"),
       risks = pick("risk"), decisions = pick("decision"), gate_checks = pick("gate_check"),
       changes = pick("change_request"), history = pick("step_history"), evidence = pick("evidence"),
       production = if (is.null(prod)) NULL else prod[prod$well == opp$well, , drop = FALSE])
}

#' Duration of every step occurrence
#' @param history Step history table.
#' @param opps Opportunity table (for class, type, field, incremental rate).
#' @param cfg Config list.
#' @param now Reference time for open steps.
#' @return history with `days, working_days, waiting_days, sla_days, over_sla, open` and opp attributes.
#' @export
wt_step_durations <- function(history, opps, cfg, now = Sys.time()) {
  h <- merge(history, opps[, c("opp_id", "class", "intervention_type", "field", "incremental_bopd", "state")], by = "opp_id")
  h$open <- is.na(h$exited_at)
  end <- h$exited_at
  end[h$open] <- now
  h$days <- round(as.numeric(difftime(end, h$entered_at, units = "days")), 1)
  h$waiting_days <- round(h$days * h$waiting_share, 1)
  h$working_days <- h$days - h$waiting_days
  h$sla_days <- mapply(function(s, c) wt_sla_days(cfg, s, c), h$step_id, h$class)
  h$over_sla <- h$days > h$sla_days
  steps <- wt_steps(cfg)
  h$step_order <- match(h$step_id, steps$id)
  h$step_name <- steps$short[h$step_order]
  h
}

#' Summary statistics of step durations
#' @param dur Output of [wt_step_durations()].
#' @param by Grouping columns.
#' @param completed_only Use only finished step occurrences.
#' @export
wt_step_stats <- function(dur, by = "step_id", completed_only = TRUE) {
  d <- if (completed_only) dur[!dur$open, , drop = FALSE] else dur
  if (!nrow(d)) return(data.frame())
  grp <- interaction(d[by], drop = TRUE, sep = "|")
  out <- do.call(rbind, lapply(split(d, grp), function(x) {
    data.frame(x[1, by, drop = FALSE], n = nrow(x),
               median_days = round(stats::median(x$days), 1), p80_days = round(unname(stats::quantile(x$days, .8)), 1),
               mean_days = round(mean(x$days), 1), median_working = round(stats::median(x$working_days), 1),
               median_waiting = round(stats::median(x$waiting_days), 1), sla_days = stats::median(x$sla_days),
               pct_over_sla = round(100 * mean(x$over_sla)), stringsAsFactors = FALSE)
  }))
  rownames(out) <- NULL
  if ("step_id" %in% by) out <- out[order(match(out$step_id, unique(dur$step_id[order(dur$step_order)]))), ]
  out
}

#' Flow segments per case (doc section 37 flow-efficiency metrics)
#'
#' Opp->D1, D1->D2, D2->RTE (D3), RTE->Executed and total lead time, in days.
#' @param history Step history table.
#' @export
wt_lead_times <- function(history) {
  h <- history[!history$recycle, , drop = FALSE]
  cases <- unique(h$opp_id)
  get <- function(id, step, col) {
    x <- h[h$opp_id == id & h$step_id == step, col]
    if (length(x)) x[1] else as.POSIXct(NA, tz = "UTC")
  }
  d <- function(a, b) round(as.numeric(difftime(b, a, units = "days")), 1)
  do.call(rbind, lapply(cases, function(id) {
    s1 <- get(id, "S1", "entered_at"); d1 <- get(id, "D1", "exited_at"); d2 <- get(id, "D2", "exited_at")
    d3 <- get(id, "D3", "exited_at"); s5 <- get(id, "S5", "exited_at")
    data.frame(opp_id = id, opp_to_d1 = d(s1, d1), d1_to_d2 = d(d1, d2), d2_to_rte = d(d2, d3),
               rte_to_executed = d(d3, s5), lead_time = d(s1, s5), executed_at = s5, stringsAsFactors = FALSE)
  }))
}

#' Value KPIs of the process
#' @param data Named list of tables.
#' @param dur Output of [wt_step_durations()].
#' @param cfg Config list.
#' @return Named list of KPI values.
#' @export
wt_kpis <- function(data, dur, cfg) {
  o <- data$opportunity
  terminal <- wt_states(cfg)$id[wt_states(cfg)$terminal]
  active <- o[!o$state %in% terminal, ]
  lt <- wt_lead_times(data$step_history)
  executed <- o[!is.na(o$actual_bopd), ]
  past_d2 <- unique(data$decision$opp_id[data$decision$gate == "D2" & data$decision$outcome %in% c("GO", "CONDITIONAL_GO")])
  recycled <- unique(data$step_history$opp_id[data$step_history$recycle])
  open_over <- dur[dur$open & dur$over_sla & !dur$state %in% terminal, ]
  deferred_bo <- sum(pmax(open_over$days - open_over$sla_days, 0) * open_over$incremental_bopd, na.rm = TRUE)

  # Exception-managed: D2-approved cases that needed no formal WPA meeting
  exc <- vapply(past_d2, function(id) length(wt_wpa_triggers(wt_case_context(data, id), cfg)) == 0, logical(1))

  list(
    active = nrow(active),
    value_in_pipeline_bopd = sum(active$incremental_bopd, na.rm = TRUE),
    median_lead_time = stats::median(lt$lead_time, na.rm = TRUE),
    realization_pct = if (nrow(executed)) round(100 * sum(executed$actual_bopd) / sum(executed$realizable_bopd)) else NA,
    cost_variance_pct = if (nrow(executed)) round(100 * (sum(executed$actual_cost_kusd) / sum(executed$cost_kusd) - 1)) else NA,
    recycle_pct = if (length(past_d2)) round(100 * length(intersect(recycled, past_d2)) / length(past_d2)) else NA,
    exception_managed_pct = if (length(exc)) round(100 * mean(exc)) else NA,
    over_sla_open = nrow(open_over),
    deferred_bo = round(deferred_bo),
    open_blockers = sum(data$stream_status$status == "conflict") + sum(data$workstream_status$status == "blocked")
  )
}
