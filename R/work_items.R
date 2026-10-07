# Portfolio summary -------------------------------------------

#' One row per opportunity with flow and gate-readiness indicators
#' @param data Named list of tables.
#' @param cfg Config list.
#' @param now Reference time.
#' @return data.frame(opp_id, step_id, state, class, days_in_step, sla_days,
#'   over_sla, next_gate, readiness, n_blockers, wpa_meeting, terminal)
#' @export
wt_portfolio_summary <- function(data, cfg, now = Sys.time()) {
  o <- data$opportunity
  st <- wt_states(cfg)
  h <- data$step_history
  open_h <- h[is.na(h$exited_at), c("opp_id", "entered_at")]
  open_h <- open_h[!duplicated(open_h$opp_id), ]
  rows <- lapply(seq_len(nrow(o)), function(i) {
    ctx <- wt_case_context(data, o$opp_id[i])
    terminal <- o$state[i] %in% st$id[st$terminal]
    s <- if (terminal) NULL else wt_status_summary(ctx, cfg)
    ent <- open_h$entered_at[match(o$opp_id[i], open_h$opp_id)]
    days <- if (is.na(ent)) NA_real_ else round(as.numeric(difftime(now, ent, units = "days")), 1)
    sla <- wt_sla_days(cfg, o$step_id[i], o$class[i])
    data.frame(
      opp_id = o$opp_id[i], step_id = o$step_id[i], state = o$state[i], class = o$class[i],
      days_in_step = days, sla_days = sla, over_sla = !is.na(days) && days > sla,
      next_gate = if (is.null(s)) NA_character_ else s$gate,
      readiness = if (is.null(s)) NA_real_ else s$readiness,
      n_blockers = if (is.null(s)) 0L else nrow(s$blockers),
      wpa_meeting = if (is.null(s)) FALSE else length(s$wpa) > 0,
      terminal = terminal, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
