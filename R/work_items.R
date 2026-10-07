# Portfolio summary and personal work items -------------------------------------------

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

#' Work items for a user (the "My Work" inbox)
#'
#' Exception-oriented: only items that need this person's action.
#' @param data Named list of tables.
#' @param summary Output of [wt_portfolio_summary()].
#' @param user Output of [wt_resolve_user()].
#' @param cfg Config list.
#' @param now Reference time.
#' @return data.frame(priority, kind, opp_id, well, item, due, age_days, action)
#' @export
wt_my_work <- function(data, summary, user, cfg, now = Sys.time()) {
  o <- data$opportunity
  well <- function(id) o$well[match(id, o$opp_id)]
  active <- summary$opp_id[!summary$terminal]
  items <- list()
  add <- function(priority, kind, opp_id, item, due, age, action) {
    if (!length(opp_id)) return()
    items[[length(items) + 1]] <<- data.frame(priority = priority, kind = kind, opp_id = opp_id, well = well(opp_id),
                                              item = item, due = as.Date(due), age_days = round(age, 1), action = action,
                                              stringsAsFactors = FALSE)
  }
  today <- as.Date(now)

  # Assurance streams owned by me on cases in technical assurance
  s <- data$stream_status
  s <- s[s$owner %in% user$user & s$status != "complete" & s$opp_id %in% active &
           o$step_id[match(s$opp_id, o$opp_id)] == "S2", , drop = FALSE]
  if (nrow(s)) {
    sn <- wt_streams(cfg)
    add(ifelse(s$status == "conflict", "High", "Medium"), "Assurance", s$opp_id,
        paste0(sn$name[match(s$stream_id, sn$id)], " - ", gsub("_", " ", s$status)), NA,
        as.numeric(difftime(now, s$updated_at, units = "days")),
        ifelse(s$status == "conflict", "Resolve conflict", "Complete stream"))
  }
  # Readiness workstreams owned by me
  w <- data$workstream_status
  w <- w[w$owner %in% user$user & w$status != "complete" & w$opp_id %in% active, , drop = FALSE]
  if (nrow(w)) {
    wn <- wt_workstreams(cfg)
    add(ifelse(w$status == "blocked" | w$due_date < today, "High", "Medium"), "Readiness", w$opp_id,
        paste0(wn$name[match(w$ws_id, wn$id)], " - ", gsub("_", " ", w$status)), w$due_date,
        NA_real_, ifelse(w$status == "blocked", "Unblock", "Complete workstream"))
  }
  # Risks owned by me
  r <- data$risk
  r <- r[r$owner %in% user$user & r$status %in% c("open", "mitigating") & r$opp_id %in% active, , drop = FALSE]
  if (nrow(r)) {
    add(ifelse(wt_risk_score(r$probability, r$consequence) >= 15, "High", "Low"), "Risk", r$opp_id,
        paste0(r$category, ": ", r$description), r$due_date, NA_real_, "Mitigate risk")
  }
  # Decisions awaiting me
  if (wt_can(user, "decide", cfg)) {
    pend <- summary[!summary$terminal & summary$step_id %in% names(cfg$decisions), , drop = FALSE]
    mine <- pend[mapply(function(g, c) wt_can_decide(user, g, c, cfg), pend$step_id, pend$class), , drop = FALSE]
    if (nrow(mine)) {
      add(ifelse(mine$over_sla, "High", "Medium"), "Decision", mine$opp_id,
          sprintf("%s decision - package %s%% ready", mine$step_id, mine$readiness), NA, mine$days_in_step, "Decide")
    }
  }
  # Integrator: cases with blockers / suggestions to confirm, and SLA breaches
  if (wt_can(user, "edit_case", cfg)) {
    x <- summary[!summary$terminal & (summary$over_sla | summary$wpa_meeting), , drop = FALSE]
    if (nrow(x)) {
      add(ifelse(x$over_sla, "High", "Medium"), "Case flow", x$opp_id,
          ifelse(x$over_sla, sprintf("%s over SLA (%s d / %s d)", x$step_id, x$days_in_step, x$sla_days),
                 "WPA exception: meeting required"), NA, x$days_in_step,
          ifelse(x$over_sla, "Expedite", "Schedule WPA"))
    }
  }
  if (!length(items)) {
    return(data.frame(priority = character(), kind = character(), opp_id = character(), well = character(),
                      item = character(), due = as.Date(character()), age_days = numeric(), action = character()))
  }
  out <- do.call(rbind, items)
  out[order(match(out$priority, c("High", "Medium", "Low")), out$due, -out$age_days, na.last = TRUE), ]
}
