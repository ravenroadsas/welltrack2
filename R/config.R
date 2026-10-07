# Configuration loading and accessors ------------------------------------------
#
# The process definition lives in inst/config/process.yml (hierarchical, so it
# stays YAML). Tabular parameters such as the user/role table live in a CSV that
# will move to a pin on Posit Connect (see wt_read_users()).

#' Path to a file shipped in inst/
#' @keywords internal
wt_sys_file <- function(...) {
  path <- system.file(..., package = "welltrack2")
  if (!nzchar(path)) stop("File not found in package: ", file.path(...), call. = FALSE)
  path
}

#' Load the process configuration
#'
#' @param path YAML file. Defaults to env var `WT_CONFIG_PATH`, else the
#'   packaged `inst/config/process.yml`.
#' @param analyses_path Analysis catalog YAML. Defaults to env var
#'   `WT_ANALYSES_PATH`, else the packaged `inst/config/analyses.yml`. The
#'   catalog is shared by all processes and attached as `cfg$analyses`.
#' @return A validated list.
#' @export
wt_load_config <- function(path = Sys.getenv("WT_CONFIG_PATH", ""),
                           analyses_path = Sys.getenv("WT_ANALYSES_PATH", "")) {
  if (!nzchar(path)) path <- wt_sys_file("config", "process.yml")
  cfg <- yaml::read_yaml(path)
  cfg$analyses <- wt_load_analyses(analyses_path)
  wt_validate_config(cfg)
  cfg
}

#' Load the analysis catalog
#' @param path Catalog YAML (see [wt_load_config()]).
#' @return List of analysis definitions named by id.
#' @export
wt_load_analyses <- function(path = Sys.getenv("WT_ANALYSES_PATH", "")) {
  if (!nzchar(path)) path <- wt_sys_file("config", "analyses.yml")
  cat <- yaml::read_yaml(path)$analyses
  ids <- vapply(cat, `[[`, "", "id")
  if (anyDuplicated(ids)) stop("Duplicated analysis ids in catalog", call. = FALSE)
  for (a in cat) {
    if (!a$kind %in% c("module", "app", "report", "external"))
      stop("Analysis ", a$id, " has invalid kind '", a$kind, "'", call. = FALSE)
    if (a$kind == "app" && is.null(a$url)) stop("Analysis ", a$id, " (app) needs a `url`", call. = FALSE)
  }
  stats::setNames(cat, ids)
}

#' Validate the structure of a process configuration
#'
#' Fails fast with a readable message so a bad YAML edit never reaches users.
#' @param cfg Config list.
#' @return `TRUE` invisibly.
#' @export
wt_validate_config <- function(cfg) {
  required <- c("steps", "states", "decisions", "intervention_types",
                "complexity_classes", "assurance_streams",
                "readiness_workstreams", "roles", "activity_phases")
  missing <- setdiff(required, names(cfg))
  if (length(missing)) stop("Config missing sections: ", paste(missing, collapse = ", "), call. = FALSE)

  step_ids <- vapply(cfg$steps, `[[`, "", "id")
  if (anyDuplicated(step_ids)) stop("Duplicated step ids in config", call. = FALSE)

  dec_steps <- step_ids[vapply(cfg$steps, function(s) s$kind == "decision", logical(1))]
  if (!setequal(dec_steps, names(cfg$decisions)))
    stop("Every decision step needs a `decisions` entry (and vice versa)", call. = FALSE)

  all_states <- wt_states(cfg)
  bad_steps <- setdiff(stats::na.omit(all_states$step), step_ids)
  if (length(bad_steps)) stop("States reference unknown steps: ", paste(bad_steps, collapse = ", "), call. = FALSE)

  for (d in names(cfg$decisions)) {
    targets <- unlist(cfg$decisions[[d]]$outcomes)
    bad <- setdiff(targets, all_states$id)
    if (length(bad)) stop("Decision ", d, " leads to unknown states: ", paste(bad, collapse = ", "), call. = FALSE)
    for (cr in cfg$decisions[[d]]$criteria) {
      if (!cr$mode %in% c("manual", "assisted", "auto"))
        stop("Criterion ", cr$id, " has invalid mode '", cr$mode, "'", call. = FALSE)
      if (cr$mode != "manual" && is.null(cr$check))
        stop("Criterion ", cr$id, " is ", cr$mode, " but has no `check`", call. = FALSE)
      unknown <- setdiff(unlist(cr$analyses), names(cfg$analyses))
      if (length(unknown))
        stop("Criterion ", cr$id, " references unknown analyses: ", paste(unknown, collapse = ", "), call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Steps as a data frame (ordered)
#' @param cfg Config list.
#' @export
wt_steps <- function(cfg) {
  data.frame(
    id = vapply(cfg$steps, `[[`, "", "id"),
    kind = vapply(cfg$steps, `[[`, "", "kind"),
    name = vapply(cfg$steps, `[[`, "", "name"),
    short = vapply(cfg$steps, `[[`, "", "short"),
    question = vapply(cfg$steps, `[[`, "", "question"),
    output = vapply(cfg$steps, `[[`, "", "output"),
    owner_role = vapply(cfg$steps, `[[`, "", "owner_role"),
    order = seq_along(cfg$steps),
    stringsAsFactors = FALSE
  )
}

#' States as a data frame (main + alternative)
#' @param cfg Config list.
#' @export
wt_states <- function(cfg) {
  to_df <- function(x, group) {
    data.frame(
      id = vapply(x, `[[`, "", "id"),
      step = vapply(x, function(s) if (is.null(s$step)) NA_character_ else s$step, ""),
      terminal = vapply(x, function(s) isTRUE(s$terminal), logical(1)),
      group = group,
      stringsAsFactors = FALSE
    )
  }
  rbind(to_df(cfg$states$main, "main"), to_df(cfg$states$alternative, "alternative"))
}

#' SLA target (days) for a step and complexity class
#' @param cfg Config list.
#' @param step_id Step id.
#' @param class Complexity class (A/B/C).
#' @export
wt_sla_days <- function(cfg, step_id, class) {
  step <- Filter(function(s) s$id == step_id, cfg$steps)
  if (!length(step)) return(NA_real_)
  val <- step[[1]]$sla_days[[class]]
  if (is.null(val)) NA_real_ else as.numeric(val)
}

#' Criteria of a decision gate as a data frame
#' @param cfg Config list.
#' @param gate Decision id (D1, D2, D3).
#' @export
wt_gate_criteria <- function(cfg, gate) {
  crit <- cfg$decisions[[gate]]$criteria
  data.frame(
    gate = gate,
    id = vapply(crit, `[[`, "", "id"),
    label = vapply(crit, `[[`, "", "label"),
    mode = vapply(crit, `[[`, "", "mode"),
    classes = vapply(crit, function(c) paste(unlist(c$classes), collapse = ","), ""),
    stringsAsFactors = FALSE
  )
}

#' Assurance streams as a data frame
#' @param cfg Config list.
#' @export
wt_streams <- function(cfg) {
  data.frame(
    id = vapply(cfg$assurance_streams, `[[`, "", "id"),
    name = vapply(cfg$assurance_streams, `[[`, "", "name"),
    discipline = vapply(cfg$assurance_streams, `[[`, "", "discipline"),
    question = vapply(cfg$assurance_streams, `[[`, "", "question"),
    stringsAsFactors = FALSE
  )
}

#' Readiness workstreams as a data frame
#' @param cfg Config list.
#' @export
wt_workstreams <- function(cfg) {
  data.frame(
    id = vapply(cfg$readiness_workstreams, `[[`, "", "id"),
    name = vapply(cfg$readiness_workstreams, `[[`, "", "name"),
    discipline = vapply(cfg$readiness_workstreams, `[[`, "", "discipline"),
    due_offset_days = vapply(cfg$readiness_workstreams, function(w) as.numeric(w$due_offset_days), 0),
    stringsAsFactors = FALSE
  )
}

#' Named vector id -> label for intervention types
#' @param cfg Config list.
#' @export
wt_type_labels <- function(cfg) {
  stats::setNames(vapply(cfg$intervention_types, `[[`, "", "name"),
                  vapply(cfg$intervention_types, `[[`, "", "id"))
}

#' Read the user / role table
#'
#' Temporary CSV source; on Posit Connect this becomes
#' `pins::pin_read(board_connect(), Sys.getenv("WT_USERS_PIN"))`.
#' @param path CSV path; defaults to env var `WT_USERS_PATH` or packaged file.
#' @export
wt_read_users <- function(path = Sys.getenv("WT_USERS_PATH", "")) {
  if (!nzchar(path)) path <- wt_sys_file("config", "users.csv")
  users <- utils::read.csv(path, stringsAsFactors = FALSE, na.strings = "", encoding = "UTF-8")
  users$authority[is.na(users$authority)] <- ""
  users
}

#' Analyses referenced by gate criteria, as a data frame
#'
#' One row per (gate, criterion, analysis) link. This is how a process
#' "calls" catalog analyses.
#' @param cfg Config list.
#' @export
wt_criterion_analyses <- function(cfg) {
  rows <- list()
  for (g in names(cfg$decisions)) for (cr in cfg$decisions[[g]]$criteria) for (a in unlist(cr$analyses)) {
    rows[[length(rows) + 1]] <- data.frame(gate = g, criterion_id = cr$id, criterion = cr$label,
                                           classes = paste(unlist(cr$classes), collapse = ","),
                                           analysis_id = a, stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(data.frame(gate = character(), criterion_id = character(), criterion = character(),
                                       classes = character(), analysis_id = character()))
  do.call(rbind, rows)
}
