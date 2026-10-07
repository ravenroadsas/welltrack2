# Analysis framework --------------------------------------------------------------------
#
# An analysis is defined once in inst/config/analyses.yml (the catalog) and
# referenced by id from any process config. Each analysis, whatever its kind,
# returns the same EVIDENCE RECORD, which gate checks (`evidence_submitted`)
# and decision records can rely on:
#
#   evidence(evidence_id, opp_id, gate, criterion_id, analysis_id,
#            analysis_version, params, outputs, data_ref, summary,
#            status, created_by, created_at)
#
# kind = "module" analyses are implemented in this package and registered in
# wt_analysis_registry(): a pure compute function (testable, callable from
# scripts/APIs) plus a Shiny module (UI around the compute function).

#' Registry of in-app analysis modules
#'
#' Maps catalog ids of `kind: module` analyses to their implementation.
#' Adding an in-app analysis = one catalog entry + one entry here.
#' @return Named list of list(compute, ui, server).
#' @export
wt_analysis_registry <- function() {
  list(
    decline_curve = list(compute = wt_an_decline_fit, ui = an_decline_ui, server = an_decline_server)
  )
}

#' Analyses offered for a case
#'
#' All (gate, criterion, analysis) links of the process that apply to the
#' case's class, with the latest evidence status. Rows for the case's next
#' gate come first.
#' @param ctx Case context (see [wt_case_context()]).
#' @param cfg Config list (with `cfg$analyses` catalog).
#' @return data.frame(gate, criterion_id, criterion, analysis_id, name, kind,
#'   current_gate, evidence_status, evidence_at)
#' @export
wt_case_analyses <- function(ctx, cfg) {
  links <- wt_criterion_analyses(cfg)
  links <- links[vapply(strsplit(links$classes, ","), function(k) ctx$opp$class %in% k, logical(1)), , drop = FALSE]
  if (!nrow(links)) return(cbind(links, name = character(), kind = character(), current_gate = logical(),
                                 evidence_status = character(), evidence_at = as.POSIXct(character())))
  step <- wt_state_step(ctx$opp$state, cfg)
  next_gate <- if (is.na(step)) NA_character_ else wt_next_gate(step, cfg)
  links$name <- vapply(links$analysis_id, function(a) cfg$analyses[[a]]$name, "")
  links$kind <- vapply(links$analysis_id, function(a) cfg$analyses[[a]]$kind, "")
  links$current_gate <- links$gate %in% next_gate
  ev <- ctx$evidence
  st <- lapply(seq_len(nrow(links)), function(i) {
    if (is.null(ev) || !nrow(ev)) return(list("none", as.POSIXct(NA)))
    hit <- ev[ev$analysis_id == links$analysis_id[i] & ev$criterion_id == links$criterion_id[i] & ev$status == "submitted", , drop = FALSE]
    if (!nrow(hit)) return(list("none", as.POSIXct(NA)))
    list("submitted", max(hit$created_at))
  })
  links$evidence_status <- vapply(st, `[[`, "", 1)
  links$evidence_at <- do.call(c, lapply(st, `[[`, 2))
  links[order(!links$current_gate, links$gate), , drop = FALSE]
}

#' Build an evidence record from an analysis result
#'
#' @param opp_id,gate,criterion_id Where the evidence is attached.
#' @param analysis Catalog entry.
#' @param result list(params, outputs, data_ref, summary) as returned by a compute function.
#' @param user User id.
#' @param now Timestamp.
#' @return One-row data.frame matching the `evidence` table.
#' @export
wt_evidence_record <- function(opp_id, gate, criterion_id, analysis, result, user, now = Sys.time()) {
  data.frame(
    evidence_id = sprintf("EV-%s-%s", format(now, "%Y%m%d%H%M%S"), substr(as.character(stats::runif(1)), 3, 6)),
    opp_id = opp_id, gate = gate, criterion_id = criterion_id,
    analysis_id = analysis$id, analysis_version = analysis$version %||% "",
    params = as.character(jsonlite::toJSON(result$params, auto_unbox = TRUE)),
    outputs = as.character(jsonlite::toJSON(result$outputs, auto_unbox = TRUE, digits = NA)),
    data_ref = result$data_ref %||% "", summary = result$summary %||% "",
    status = "submitted", created_by = user, created_at = now, stringsAsFactors = FALSE)
}

#' Deep link to an external analysis app
#'
#' The target app reads `opp`, `criterion` and `return` from the query string
#' and writes its evidence record back to the shared database / API.
#' @param analysis Catalog entry (`kind: app`).
#' @param opp_id,criterion_id Case and criterion.
#' @param return_url Optional URL to come back to.
#' @export
wt_analysis_link <- function(analysis, opp_id, criterion_id, return_url = NULL) {
  q <- c(opp = opp_id, criterion = criterion_id, return = return_url)
  paste0(analysis$url, "?", paste(names(q), utils::URLencode(q, reserved = TRUE), sep = "=", collapse = "&"))
}
