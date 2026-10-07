# Data access layer ------------------------------------------------------------------
#
# ALL database interaction lives in this file. The app talks to DBI only, so the
# move from DuckDB (development) to SQL Server / PostgreSQL (enterprise) means:
#   1. change wt_db_connect() to the enterprise driver (odbc / RPostgres),
#   2. keep SQL in this file ANSI (no DuckDB-only functions outside
#      wt_db_step_stats(), which is flagged),
#   3. run the same table definitions as migrations.
# Secrets (DSN, credentials) come from environment variables only.

#' Connect to the WellTrack database
#' @param path DuckDB file; `":memory:"` for the mockup. Defaults to env var `WT_DB_PATH`.
#' @export
wt_db_connect <- function(path = Sys.getenv("WT_DB_PATH", ":memory:")) {
  DBI::dbConnect(duckdb::duckdb(), dbdir = path)
}

#' Disconnect and shut down
#' @param con DBI connection.
#' @export
wt_db_disconnect <- function(con) {
  DBI::dbDisconnect(con, shutdown = TRUE)
}

#' Tables managed by the app
#' @export
wt_db_tables <- function() {
  c("opportunity", "step_history", "stream_status", "workstream_status", "risk",
    "decision", "gate_check", "change_request", "activity_log", "production_history", "evidence")
}

#' Seed the database from a list of data frames (mockup / tests)
#' @param con DBI connection.
#' @param data Named list of data frames.
#' @export
wt_db_seed <- function(con, data) {
  for (tbl in intersect(names(data), wt_db_tables())) {
    DBI::dbWriteTable(con, tbl, data[[tbl]], overwrite = TRUE)
  }
  invisible(con)
}

#' Read one table, optionally filtered to one opportunity
#' @param con DBI connection.
#' @param table Table name (validated against [wt_db_tables()]).
#' @param opp_id Optional opportunity id.
#' @export
wt_db_read <- function(con, table, opp_id = NULL) {
  table <- match.arg(table, wt_db_tables())
  if (is.null(opp_id)) return(DBI::dbReadTable(con, table))
  DBI::dbGetQuery(con, sprintf("SELECT * FROM %s WHERE opp_id = ?", table), params = list(opp_id))
}

#' Read all tables into a named list
#' @param con DBI connection.
#' @export
wt_db_read_all <- function(con) {
  tbls <- intersect(wt_db_tables(), DBI::dbListTables(con))
  stats::setNames(lapply(tbls, function(t) DBI::dbReadTable(con, t)), tbls)
}

#' Next opportunity id
#' @param con DBI connection.
#' @export
wt_db_next_opp_id <- function(con) {
  n <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM opportunity")$n
  sprintf("OPP-%04d", 1001 + n)
}

#' Insert a new framed opportunity and open its first step
#' @param con DBI connection.
#' @param rec Named list of opportunity fields (missing columns become NA).
#' @param now Timestamp.
#' @return The new opportunity id.
#' @export
wt_db_insert_opportunity <- function(con, rec, now = Sys.time()) {
  cols <- DBI::dbListFields(con, "opportunity")
  rec$opp_id <- rec$opp_id %||% wt_db_next_opp_id(con)
  rec$created_at <- now
  rec$updated_at <- now
  template <- DBI::dbGetQuery(con, "SELECT * FROM opportunity LIMIT 0")
  row <- template[NA_integer_, , drop = FALSE]
  for (c in intersect(names(rec), cols)) row[[c]] <- wt_coerce_like(rec[[c]], template[[c]])
  DBI::dbWithTransaction(con, {
    DBI::dbAppendTable(con, "opportunity", row)
    DBI::dbAppendTable(con, "step_history", data.frame(
      opp_id = rec$opp_id, step_id = rec$step_id %||% "S1", entered_at = now,
      exited_at = as.POSIXct(NA), waiting_share = 0, recycle = FALSE))
  })
  rec$opp_id
}

#' Coerce a value to the type of a template column
#' @keywords internal
wt_coerce_like <- function(x, template) {
  if (is.null(x) || !length(x)) x <- NA
  x <- x[[1]]
  if (inherits(template, "POSIXct")) return(as.POSIXct(x, tz = "UTC"))
  if (inherits(template, "Date")) return(as.Date(x))
  if (is.logical(template)) return(as.logical(x))
  if (is.numeric(template)) return(suppressWarnings(as.numeric(x)))
  as.character(x)
}

#' Confirm (or un-confirm) a manual / assisted gate criterion
#' @param con DBI connection.
#' @param opp_id,gate,criterion_id Identifiers.
#' @param met Logical.
#' @param user User id.
#' @param evidence Free text / link.
#' @export
wt_db_set_gate_check <- function(con, opp_id, gate, criterion_id, met, user, evidence = "") {
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM gate_check WHERE opp_id = ? AND gate = ? AND criterion_id = ?",
                   params = list(opp_id, gate, criterion_id))
    if (isTRUE(met)) {
      DBI::dbAppendTable(con, "gate_check", data.frame(
        opp_id = opp_id, gate = gate, criterion_id = criterion_id, status = "met",
        by_user = user, at = Sys.time(), evidence = evidence))
    }
  })
  invisible(TRUE)
}

#' Update the status of an assurance stream or readiness workstream
#' @param con DBI connection.
#' @param kind `"stream"` or `"workstream"`.
#' @param opp_id Opportunity id.
#' @param id Stream / workstream id.
#' @param status New status.
#' @export
wt_db_set_item_status <- function(con, kind = c("stream", "workstream"), opp_id, id, status) {
  kind <- match.arg(kind)
  sql <- switch(kind,
    stream = "UPDATE stream_status SET status = ?, updated_at = ? WHERE opp_id = ? AND stream_id = ?",
    workstream = "UPDATE workstream_status SET status = ? WHERE opp_id = ? AND ws_id = ?")
  params <- switch(kind, stream = list(status, Sys.time(), opp_id, id), workstream = list(status, opp_id, id))
  DBI::dbExecute(con, sql, params = params)
}

#' Record a gate decision and move the case to its next state / step
#'
#' One transaction: insert decision record, supersede earlier decisions of the
#' same gate, update opportunity state and step, close the open step and open
#' the next one.
#' @param con DBI connection.
#' @param opp_id Opportunity id.
#' @param gate Decision id.
#' @param outcome Outcome id.
#' @param user Deciding user id.
#' @param rationale,conditions Text.
#' @param cfg Config list.
#' @param now Timestamp.
#' @return New state id.
#' @export
wt_db_record_decision <- function(con, opp_id, gate, outcome, user, rationale, conditions = "", cfg, now = Sys.time()) {
  opp <- wt_db_read(con, "opportunity", opp_id)
  new_state <- wt_apply_decision(opp$state, gate, outcome, cfg)
  new_step <- wt_state_step(new_state, cfg)
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "UPDATE decision SET superseded = TRUE WHERE opp_id = ? AND gate = ?", params = list(opp_id, gate))
    DBI::dbAppendTable(con, "decision", data.frame(
      opp_id = opp_id, gate = gate, outcome = outcome,
      authority = cfg$decisions[[gate]]$authority[[opp$class]] %||% "", decided_by = user, decided_at = now,
      baseline_version = opp$baseline_version %||% "v1", rationale = rationale, conditions = conditions,
      superseded = FALSE))
    DBI::dbExecute(con, "UPDATE opportunity SET state = ?, step_id = COALESCE(?, step_id), updated_at = ? WHERE opp_id = ?",
                   params = list(new_state, new_step, now, opp_id))
    if (!is.na(new_step) && new_step != gate) {
      DBI::dbExecute(con, "UPDATE step_history SET exited_at = ? WHERE opp_id = ? AND exited_at IS NULL",
                     params = list(now, opp_id))
      DBI::dbAppendTable(con, "step_history", data.frame(
        opp_id = opp_id, step_id = new_step, entered_at = now, exited_at = as.POSIXct(NA),
        waiting_share = 0, recycle = identical(new_state, "RECYCLE_TECHNICAL_CASE")))
    }
  })
  new_state
}

#' Store an analysis evidence record
#'
#' Earlier submitted evidence of the same analysis for the same criterion is
#' marked `superseded` (kept for traceability).
#' @param con DBI connection.
#' @param record One-row data.frame from [wt_evidence_record()].
#' @export
wt_db_add_evidence <- function(con, record) {
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "UPDATE evidence SET status = 'superseded'
                         WHERE opp_id = ? AND criterion_id = ? AND analysis_id = ? AND status = 'submitted'",
                   params = list(record$opp_id, record$criterion_id, record$analysis_id))
    DBI::dbAppendTable(con, "evidence", record)
  })
  invisible(record$evidence_id)
}

#' Append raw activity events
#' @param con DBI connection.
#' @param events data.frame(ts, session_id, user, input_id, value, opp_id, phase).
#' @export
wt_db_log_activity <- function(con, events) {
  if (!nrow(events)) return(invisible(0))
  DBI::dbAppendTable(con, "activity_log", events[, DBI::dbListFields(con, "activity_log")])
}

#' Step duration statistics computed in the database
#'
#' Example of pushing aggregation to the database instead of the R process.
#' Uses DuckDB `quantile_cont`; the SQL Server equivalent is
#' `PERCENTILE_CONT(...) WITHIN GROUP`.
#' @param con DBI connection.
#' @export
wt_db_step_stats <- function(con) {
  DBI::dbGetQuery(con, "
    SELECT h.step_id, o.class,
           COUNT(*) AS n,
           quantile_cont(date_diff('hour', h.entered_at, h.exited_at) / 24.0, 0.5) AS median_days
    FROM step_history h JOIN opportunity o USING (opp_id)
    WHERE h.exited_at IS NOT NULL
    GROUP BY h.step_id, o.class
    ORDER BY h.step_id, o.class")
}
