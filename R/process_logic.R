# Process logic: rules, classification, gates and transitions ------------------
#
# Pure functions (no Shiny, no database). Everything is driven by the config so
# the process can change without code changes. A "case context" (`ctx`) is a
# list holding one opportunity and its linked records:
#   ctx$opp         named list (one opportunity row)
#   ctx$streams     data.frame(stream_id, status, owner, constraint_bopd, ...)
#   ctx$workstreams data.frame(ws_id, status, owner, due_date, ...)
#   ctx$risks       data.frame(probability, consequence, mitigation, owner, status, ...)
#   ctx$decisions   data.frame(gate, outcome, decided_at, superseded, ...)
#   ctx$gate_checks data.frame(gate, criterion_id, status, ...)  manual/assisted confirmations
#   ctx$changes     data.frame(materiality, status, ...)
#   ctx$evidence    data.frame(analysis_id, criterion_id, status, outputs, ...)  analysis results
#   ctx$production  data.frame(well, month, oil_bopd)  history of the case's well

#' Evaluate a structured rule against a record
#'
#' Rules are lists `{field, op, value}`; `op` in `>`, `>=`, `<`, `<=`, `==`,
#' `!=`, `not_null`, `true`. Missing values never satisfy a comparison. No
#' `eval()` is used, so config files cannot execute code.
#' @param rule Rule list.
#' @param record Named list.
#' @return Logical scalar.
#' @export
wt_eval_rule <- function(rule, record) {
  v <- record[[rule$field]]
  op <- rule$op
  if (identical(op, "true")) op <- TRUE  # yaml `true` keyword
  if (isTRUE(op)) return(isTRUE(as.logical(v)))
  if (identical(op, "not_null")) return(!is.null(v) && length(v) == 1 && !is.na(v) && !identical(v, ""))
  if (is.null(v) || length(v) != 1 || is.na(v)) return(FALSE)
  target <- rule$value
  if (is.numeric(target)) v <- suppressWarnings(as.numeric(v))
  if (is.na(v)) return(FALSE)
  switch(op,
    ">"  = v > target,
    ">=" = v >= target,
    "<"  = v < target,
    "<=" = v <= target,
    "==" = v == target,
    "!=" = v != target,
    stop("Unknown rule operator: ", op, call. = FALSE)
  )
}

#' Classify an opportunity into a complexity class (A/B/C)
#' @param record Named list with opportunity fields.
#' @param cfg Config list.
#' @return Class id.
#' @export
wt_classify <- function(record, cfg) {
  for (rule in cfg$classification_rules) {
    if (any(vapply(rule$any, wt_eval_rule, logical(1), record = record))) return(rule$class)
  }
  type <- Filter(function(t) t$id == record$intervention_type, cfg$intervention_types)
  if (length(type)) type[[1]]$default_class else "B"
}

#' Assurance streams required for an opportunity
#'
#' Union of the class requirements, the intervention-type mandatory streams and
#' streams whose `applies_when` condition holds.
#' @param record Named list with opportunity fields (needs `class`, `intervention_type`).
#' @param cfg Config list.
#' @return Character vector of stream ids, in config order.
#' @export
wt_required_streams <- function(record, cfg) {
  cls <- cfg$complexity_classes[[record$class]]
  type <- Filter(function(t) t$id == record$intervention_type, cfg$intervention_types)
  req <- c(unlist(cls$required_streams), if (length(type)) unlist(type[[1]]$mandatory_streams))
  for (s in cfg$assurance_streams) {
    if (!is.null(s$applies_when) && wt_eval_rule(s$applies_when, record)) req <- c(req, s$id)
  }
  ids <- vapply(cfg$assurance_streams, `[[`, "", "id")
  ids[ids %in% req]
}

#' Readiness workstreams applicable to an opportunity
#' @inheritParams wt_required_streams
#' @export
wt_applicable_workstreams <- function(record, cfg) {
  keep <- vapply(cfg$readiness_workstreams, function(w) {
    is.null(w$applies_when) || wt_eval_rule(w$applies_when, record)
  }, logical(1))
  vapply(cfg$readiness_workstreams[keep], `[[`, "", "id")
}

#' Risk score (probability x consequence, 1-5 scales)
#' @param probability,consequence Numeric vectors.
#' @export
wt_risk_score <- function(probability, consequence) probability * consequence

# Registry of named checks usable from config (`check: {fn: name}`).
# Adding automation = adding a function here and referencing it in YAML.
wt_check_registry <- function() {
  list(
    framing_complete = function(ctx, cfg, check = NULL) {
      req <- Filter(function(f) isTRUE(f$required), cfg$framing_fields)
      all(vapply(req, function(f) wt_eval_rule(list(field = f$id, op = "not_null"), ctx$opp), logical(1)))
    },
    well_context_available = function(ctx, cfg, check = NULL) {
      wt_eval_rule(list(field = "current_bopd", op = "not_null"), ctx$opp)
    },
    required_streams_complete = function(ctx, cfg, check = NULL) {
      req <- wt_required_streams(ctx$opp, cfg)
      st <- ctx$streams[ctx$streams$stream_id %in% req, , drop = FALSE]
      length(req) > 0 && nrow(st) == length(req) && all(st$status == "complete")
    },
    no_stream_conflicts = function(ctx, cfg, check = NULL) {
      !any(ctx$streams$status == "conflict")
    },
    economics_consistent = function(ctx, cfg, check = NULL) {
      a <- ctx$opp$econ_basis_bopd; b <- ctx$opp$realizable_bopd
      !is.null(a) && !is.null(b) && !is.na(a) && !is.na(b) && abs(a - b) < 1
    },
    material_risks_mitigated = function(ctx, cfg, check = NULL) {
      r <- ctx$risks
      if (is.null(r) || !nrow(r)) return(TRUE)
      mat <- r[wt_risk_score(r$probability, r$consequence) >= 10, , drop = FALSE]
      all(nzchar(mat$mitigation %||% "") & nzchar(mat$owner %||% ""))
    },
    d2_approved = function(ctx, cfg, check = NULL) {
      d <- ctx$decisions
      !is.null(d) && any(d$gate == "D2" & d$outcome %in% c("GO", "CONDITIONAL_GO") & !d$superseded)
    },
    workstreams_complete = function(ctx, cfg, check = NULL) {
      app <- wt_applicable_workstreams(ctx$opp, cfg)
      ws <- ctx$workstreams[ctx$workstreams$ws_id %in% app, , drop = FALSE]
      nrow(ws) == length(app) && all(ws$status == "complete")
    },
    critical_risks_closed = function(ctx, cfg, check = NULL) {
      r <- ctx$risks
      if (is.null(r) || !nrow(r)) return(TRUE)
      crit <- r[wt_risk_score(r$probability, r$consequence) >= 15, , drop = FALSE]
      all(crit$status %in% c("closed", "accepted"))
    },
    no_pending_material_change = function(ctx, cfg, check = NULL) {
      ch <- ctx$changes
      is.null(ch) || !any(ch$status == "pending" & ch$materiality != "accept")
    },
    evidence_submitted = function(ctx, cfg, check = NULL) {
      ev <- ctx$evidence
      if (is.null(ev) || !nrow(ev)) return(FALSE)
      hit <- ev$analysis_id == check$analysis & ev$status == "submitted"
      # evidence counts only for the criterion it was attached to
      if (!is.null(check$criterion)) hit <- hit & ev$criterion_id == check$criterion
      any(hit)
    }
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' Run one configured check against a case context
#' @param check Check list from config (`{fn}` or `{field, op, value}`).
#' @param ctx Case context.
#' @param cfg Config list.
#' @export
wt_run_check <- function(check, ctx, cfg) {
  if (!is.null(check$fn)) {
    fn <- wt_check_registry()[[check$fn]]
    if (is.null(fn)) stop("Unknown check function: ", check$fn, call. = FALSE)
    return(isTRUE(fn(ctx, cfg, check)))
  }
  wt_eval_rule(check, ctx$opp)
}

#' Evaluate gate criteria for a case
#'
#' @param ctx Case context.
#' @param gate Decision id.
#' @param cfg Config list.
#' @return data.frame with one row per applicable criterion and columns
#'   `id, label, mode, auto_result, confirmed, status` where status is
#'   `met`, `suggested` (assisted, data says yes, awaiting confirmation) or `open`.
#' @export
wt_evaluate_gate <- function(ctx, gate, cfg) {
  crit <- Filter(function(c) ctx$opp$class %in% unlist(c$classes), cfg$decisions[[gate]]$criteria)
  gc <- ctx$gate_checks
  rows <- lapply(crit, function(c) {
    auto <- if (c$mode == "manual") NA else wt_run_check(c(c$check, list(criterion = c$id)), ctx, cfg)
    confirmed <- !is.null(gc) && any(gc$gate == gate & gc$criterion_id == c$id & gc$status == "met")
    status <- switch(c$mode,
      auto = if (isTRUE(auto)) "met" else "open",
      assisted = if (confirmed) "met" else if (isTRUE(auto)) "suggested" else "open",
      manual = if (confirmed) "met" else "open"
    )
    data.frame(id = c$id, label = c$label, mode = c$mode, auto_result = auto,
               confirmed = confirmed, status = status, stringsAsFactors = FALSE)
  })
  if (!length(rows)) {
    return(data.frame(id = character(), label = character(), mode = character(),
                      auto_result = logical(), confirmed = logical(), status = character()))
  }
  do.call(rbind, rows)
}

#' Readiness percentage of a gate evaluation
#' @param eval Output of [wt_evaluate_gate()].
#' @export
wt_gate_readiness <- function(eval) {
  if (!nrow(eval)) return(100)
  round(100 * mean(eval$status == "met"))
}

#' Step that a state belongs to
#' @param state State id.
#' @param cfg Config list.
#' @export
wt_state_step <- function(state, cfg) {
  st <- wt_states(cfg)
  st$step[match(state, st$id)]
}

#' Next decision gate at or after a step
#' @param step_id Current step id.
#' @param cfg Config list.
#' @return Decision id or `NA` when no gate remains.
#' @export
wt_next_gate <- function(step_id, cfg) {
  steps <- wt_steps(cfg)
  pos <- match(step_id, steps$id)
  if (is.na(pos)) return(NA_character_)
  later <- steps[steps$order >= pos & steps$kind == "decision", ]
  if (nrow(later)) later$id[1] else NA_character_
}

#' State resulting from a decision outcome
#'
#' @param current_state Current state; must belong to the gate's step.
#' @param gate Decision id.
#' @param outcome Outcome id.
#' @param cfg Config list.
#' @export
wt_apply_decision <- function(current_state, gate, outcome, cfg) {
  if (!identical(wt_state_step(current_state, cfg), gate))
    stop("Case in state ", current_state, " is not pending decision ", gate, call. = FALSE)
  target <- cfg$decisions[[gate]]$outcomes[[outcome]]
  if (is.null(target)) stop("Outcome ", outcome, " is not valid for ", gate, call. = FALSE)
  target
}

#' Reasons that require a formal WPA meeting (empty = asynchronous approval)
#' @param ctx Case context.
#' @param cfg Config list.
#' @export
wt_wpa_triggers <- function(ctx, cfg, check = NULL) {
  t <- cfg$wpa_triggers
  o <- ctx$opp
  reasons <- character()
  if (o$class %in% unlist(t$formal_for_classes)) reasons <- c(reasons, paste0("Class ", o$class, " requires formal WPA"))
  if (isTRUE(t$stream_conflict) && any(ctx$streams$status == "conflict")) reasons <- c(reasons, "Disciplines disagree")
  if (isTRUE(t$uncertainty_high) && identical(o$uncertainty, "high")) reasons <- c(reasons, "High technical uncertainty")
  if (!is.na(o$reservoir_bopd %||% NA) && !is.na(o$realizable_bopd %||% NA) && o$reservoir_bopd > 0) {
    gap <- 100 * (o$reservoir_bopd - o$realizable_bopd) / o$reservoir_bopd
    if (gap > t$potential_gap_pct) reasons <- c(reasons, sprintf("Constraints reduce potential by %.0f%%", gap))
  }
  if (!is.na(o$npv_kusd %||% NA) && o$npv_kusd > t$value_kusd) reasons <- c(reasons, "Value above threshold")
  if (!is.na(o$cost_kusd %||% NA) && o$cost_kusd > t$cost_kusd) reasons <- c(reasons, "Cost above threshold")
  reasons
}

#' Materiality of a change after D2
#' @param cost_increase_pct,target_reduction_pct,delay_days Numeric.
#' @param cfg Config list.
#' @return `accept`, `revalidate` or `reapprove_d2`.
#' @export
wt_change_materiality <- function(cost_increase_pct = 0, target_reduction_pct = 0, delay_days = 0, cfg) {
  cc <- cfg$change_control
  vals <- c(cost_increase_pct, target_reduction_pct, delay_days)
  lims <- list(cc$cost_increase_pct, cc$target_reduction_pct, cc$delay_days)
  if (any(mapply(function(v, l) v >= l$reapprove_d2, vals, lims))) return("reapprove_d2")
  if (any(mapply(function(v, l) v >= l$revalidate, vals, lims))) return("revalidate")
  "accept"
}

#' Decision-centric status summary for a case
#'
#' Deterministic version of the "assistant" status (doc section 34). The same
#' structure will be handed to an LLM as grounded context later.
#' @param ctx Case context.
#' @param cfg Config list.
#' @return list(step, gate, readiness, blockers (data.frame), next_action, wpa)
#' @export
wt_status_summary <- function(ctx, cfg, check = NULL) {
  step <- wt_state_step(ctx$opp$state, cfg)
  gate <- if (is.na(step)) NA_character_ else wt_next_gate(step, cfg)
  blockers <- data.frame(item = character(), owner = character(), action = character(), stringsAsFactors = FALSE)
  readiness <- NA_real_
  if (!is.na(gate)) {
    ev <- wt_evaluate_gate(ctx, gate, cfg)
    readiness <- wt_gate_readiness(ev)
    open <- ev[ev$status != "met", , drop = FALSE]
    if (nrow(open)) {
      blockers <- data.frame(
        item = open$label,
        owner = ifelse(open$mode == "manual", "Integrator",
                       ifelse(open$status == "suggested", "Integrator (confirm)", "System / data")),
        action = ifelse(open$status == "suggested", "Confirm suggested evidence", "Provide evidence"),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!is.null(ctx$streams) && nrow(ctx$streams) && step %in% c("S2", "S3", "D2")) {
    req <- wt_required_streams(ctx$opp, cfg)
    pend <- ctx$streams[ctx$streams$stream_id %in% req & ctx$streams$status != "complete", , drop = FALSE]
    if (nrow(pend)) {
      sn <- wt_streams(cfg)
      blockers <- rbind(blockers, data.frame(
        item = paste0(sn$name[match(pend$stream_id, sn$id)], " assurance ", gsub("_", " ", pend$status)),
        owner = pend$owner,
        action = ifelse(pend$status == "conflict", "Resolve conflict with Integrator", "Complete stream outputs"),
        stringsAsFactors = FALSE
      ))
    }
  }
  next_action <- if (nrow(blockers)) {
    sprintf("%s - %s (%s)", blockers$action[1], blockers$item[1], blockers$owner[1])
  } else if (!is.na(gate)) {
    sprintf("Evidence complete: request %s decision", gate)
  } else {
    "No pending gate"
  }
  list(step = step, gate = gate, readiness = readiness, blockers = blockers,
       next_action = next_action, wpa = wt_wpa_triggers(ctx, cfg))
}
