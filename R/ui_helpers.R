# Reusable UI components (compact, industrial style; styles in www/welltrack.css) --

#' Callout box
#' @param title Title text.
#' @param ... Body content.
#' @param type `info`, `ok`, `warn`, `bad`, `neutral`.
#' @param icon Font Awesome icon name.
#' @export
wt_callout <- function(title, ..., type = "info", icon = NULL) {
  ic <- icon %||% switch(type, ok = "circle-check", warn = "triangle-exclamation", bad = "circle-xmark", "circle-info")
  htmltools::div(
    class = paste("wt-callout", paste0("wt-callout-", type)),
    htmltools::div(class = "wt-callout-title", shiny::icon(ic), title),
    htmltools::div(class = "wt-callout-body", ...)
  )
}

#' KPI tile
#' @param label Label.
#' @param value Main value (formatted).
#' @param sub Sub-text (target, delta...).
#' @param status `ok`, `warn`, `bad`, `neutral`.
#' @export
wt_kpi <- function(label, value, sub = NULL, status = "neutral") {
  htmltools::div(
    class = paste("wt-kpi", paste0("wt-kpi-", status)),
    htmltools::div(class = "wt-kpi-label", label),
    htmltools::div(class = "wt-kpi-value", value),
    if (!is.null(sub)) htmltools::div(class = "wt-kpi-sub", sub)
  )
}

#' Status badge
#' @param text Text.
#' @param type CSS modifier.
#' @export
wt_badge <- function(text, type = "neutral") {
  htmltools::span(class = paste("wt-badge", paste0("wt-badge-", type)), text)
}

#' Badge for a complexity class
#' @param class Class id.
#' @param cfg Config list.
#' @export
wt_class_badge <- function(class, cfg) {
  htmltools::span(class = "wt-badge wt-class", style = sprintf("background:%s", cfg$complexity_classes[[class]]$color),
                  title = cfg$complexity_classes[[class]]$name, class)
}

#' Map a status keyword to a badge type
#' @param status Status keyword.
#' @export
wt_status_type <- function(status) {
  switch(status,
    complete = , met = , closed = , accepted = , GO = , PURSUE = , READY_TO_EXECUTE = , CLOSED = , resolved = "ok",
    in_progress = , suggested = , mitigating = , CONDITIONAL_GO = , CONDITIONAL_READY = , pending = , revalidate = "warn",
    conflict = , blocked = , open = , NO_GO = , CANCELLED = , REJECT = , HOLD = , ON_HOLD = , reapprove_d2 = "bad",
    DEFERRED = , DEFER = , not_started = , not_applicable = "neutral",
    "info")
}

#' Status chip with automatic colour
#' @param status Status keyword.
#' @export
wt_status_chip <- function(status) {
  if (is.null(status) || is.na(status)) return(wt_badge("-", "neutral"))
  wt_badge(gsub("_", " ", status), wt_status_type(status))
}

#' Icon for a criterion automation mode
#' @param mode `manual`, `assisted`, `auto`.
#' @export
wt_mode_icon <- function(mode) {
  ic <- switch(mode, auto = "robot", assisted = "wand-magic-sparkles", "user-pen")
  htmltools::span(class = paste("wt-mode", paste0("wt-mode-", mode)), title = paste("Mode:", mode), shiny::icon(ic), mode)
}

#' Thin progress bar
#' @param pct Percentage 0-100.
#' @param label Optional label.
#' @export
wt_progress <- function(pct, label = NULL) {
  pct <- if (is.na(pct)) 0 else max(0, min(100, pct))
  type <- if (pct >= 100) "ok" else if (pct >= 60) "warn" else "bad"
  htmltools::div(class = "wt-progress",
    htmltools::div(class = paste("wt-progress-bar", paste0("wt-bg-", type)), style = sprintf("width:%s%%", pct)),
    htmltools::span(class = "wt-progress-label", label %||% paste0(pct, "%")))
}

#' Format a number compactly
#' @param x Number.
#' @param suffix Unit suffix.
#' @param digits Rounding.
#' @export
wt_fmt <- function(x, suffix = "", digits = 0) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("\u2013")
  paste0(format(round(x, digits), big.mark = ",", nsmall = 0, trim = TRUE), suffix)
}

#' Decision-thread stepper
#'
#' Stages are chevrons, decisions are diamonds. Completed steps are filled,
#' the current step is highlighted, alternative states are flagged.
#' @param cfg Config list.
#' @param current_step Current step id.
#' @param state Current state id.
#' @param durations Optional named numeric vector of days per step.
#' @export
wt_stepper <- function(cfg, current_step, state = NULL, durations = NULL) {
  steps <- wt_steps(cfg)
  pos <- match(current_step, steps$id)
  alt <- !is.null(state) && state %in% wt_states(cfg)$id[wt_states(cfg)$group == "alternative"]
  closed <- identical(state, "CLOSED")
  items <- lapply(seq_len(nrow(steps)), function(i) {
    cls <- if (i < pos || (closed && i == pos)) "done" else if (i == pos) (if (alt) "current alt" else "current") else "todo"
    d <- if (!is.null(durations) && !is.na(durations[steps$id[i]])) paste0(durations[steps$id[i]], " d") else ""
    htmltools::div(
      class = paste("wt-step", paste0("wt-step-", steps$kind[i]), cls),
      title = paste0(steps$name[i], ": ", steps$question[i]),
      htmltools::div(class = "wt-step-marker", htmltools::span(if (steps$kind[i] == "decision") steps$id[i] else i - sum(steps$kind[seq_len(i)] == "decision"))),
      htmltools::div(class = "wt-step-name", steps$short[i]),
      htmltools::div(class = "wt-step-days", d)
    )
  })
  htmltools::div(class = "wt-stepper", items)
}

#' Kanban-style pipeline board
#'
#' Pure HTML rendered once per data change; clicks are sent to Shiny with
#' `Shiny.setInputValue` (no server round-trip for hover/scroll).
#' @param opps Opportunity table (active cases).
#' @param cfg Config list.
#' @param input_id Namespaced input id receiving the clicked opportunity id.
#' @param days_in_step Named numeric vector opp_id -> days in current step.
#' @param readiness Named numeric vector opp_id -> next-gate readiness %.
#' @export
wt_board <- function(opps, cfg, input_id, days_in_step = NULL, readiness = NULL) {
  steps <- wt_steps(cfg)
  cols <- lapply(seq_len(nrow(steps)), function(i) {
    s <- steps[i, ]
    x <- opps[opps$step_id == s$id, , drop = FALSE]
    x <- x[order(-x$incremental_bopd), , drop = FALSE]
    cards <- lapply(seq_len(nrow(x)), function(j) {
      o <- x[j, ]
      dd <- wt_lookup(days_in_step, o$opp_id)
      sla <- wt_sla_days(cfg, s$id, o$class)
      age_type <- if (is.na(dd)) "neutral" else if (dd > sla) "bad" else if (dd > .75 * sla) "warn" else "ok"
      rd <- wt_lookup(readiness, o$opp_id)
      htmltools::div(
        class = if (o$state %in% c("ON_HOLD", "NEED_MORE_INFORMATION")) "wt-card wt-card-alt" else "wt-card",
        onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", input_id, o$opp_id),
        htmltools::div(class = "wt-card-top", htmltools::strong(o$well), wt_class_badge(o$class, cfg),
                       htmltools::span(class = "wt-card-type", wt_type_labels(cfg)[[o$intervention_type]])),
        htmltools::div(class = "wt-card-title", o$title),
        htmltools::div(class = "wt-card-meta",
          htmltools::span(title = "Incremental potential", shiny::icon("droplet"), wt_fmt(o$incremental_bopd, " bopd")),
          htmltools::span(class = paste0("wt-age wt-age-", age_type), title = sprintf("Days in step (SLA %s d)", sla),
                          shiny::icon("clock"), wt_fmt(dd, " d")),
          if (o$state %in% wt_states(cfg)$id[wt_states(cfg)$group == "alternative"]) wt_status_chip(o$state)
        ),
        if (!is.na(rd)) wt_progress(rd, paste0("next gate ", rd, "%"))
      )
    })
    htmltools::div(
      class = paste("wt-col", paste0("wt-col-", s$kind)),
      htmltools::div(class = "wt-col-head", htmltools::span(s$short), htmltools::span(class = "wt-col-n", nrow(x))),
      htmltools::div(class = "wt-col-body", cards)
    )
  })
  htmltools::div(class = "wt-board", cols)
}

#' Safe lookup in a named vector
#' @param x Named vector (or NULL).
#' @param key Name.
#' @keywords internal
wt_lookup <- function(x, key) {
  if (is.null(x) || !key %in% names(x)) NA else x[[key]]
}
