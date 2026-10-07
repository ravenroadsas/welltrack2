# Users, roles and permissions --------------------------------------------------------
#
# On Posit Connect, `session$user` and `session$groups` identify the viewer.
# Roles come from (a) the user table (CSV now, pin later) and (b) Connect group
# membership mapped in config `roles[*].connect_groups`. Locally there is no
# Connect user, so `WT_DEV_USER` (default "juan.surv") is used and the header
# shows a role switcher to preview the UI per account type.

#' Resolve the current user
#' @param session_user `session$user` (NULL locally).
#' @param session_groups `session$groups`.
#' @param users User table from [wt_read_users()].
#' @param cfg Config list.
#' @return list(user, name, roles, discipline, authority, dev)
#' @export
wt_resolve_user <- function(session_user, session_groups, users, cfg) {
  dev <- is.null(session_user) || !nzchar(session_user)
  uid <- if (dev) Sys.getenv("WT_DEV_USER", "juan.surv") else session_user
  row <- users[users$user == uid, , drop = FALSE]
  roles <- if (nrow(row)) strsplit(row$roles[1], ";", fixed = TRUE)[[1]] else character()
  for (r in names(cfg$roles)) {
    if (length(intersect(unlist(cfg$roles[[r]]$connect_groups), session_groups))) roles <- c(roles, r)
  }
  roles <- unique(roles)
  if (!length(roles)) roles <- "viewer"
  list(
    user = uid,
    name = if (nrow(row)) row$display_name[1] else uid,
    roles = roles,
    discipline = if (nrow(row)) row$discipline[1] else "",
    authority = if (nrow(row)) row$authority[1] else "",
    dev = dev
  )
}

#' Permissions granted by a set of roles
#' @param roles Character vector of role ids.
#' @param cfg Config list.
#' @export
wt_permissions <- function(roles, cfg) {
  unique(unlist(lapply(roles, function(r) cfg$roles[[r]]$permissions)))
}

#' Does a user hold a permission?
#' @param user Output of [wt_resolve_user()].
#' @param permission Permission id.
#' @param cfg Config list.
#' @export
wt_can <- function(user, permission, cfg) {
  permission %in% wt_permissions(user$roles, cfg)
}

#' Can a user decide a gate for a given class?
#'
#' Requires the `decide` permission and an authority matching the gate's
#' authority for that class (admins are not decision authorities).
#' @param user Output of [wt_resolve_user()].
#' @param gate Decision id.
#' @param class Complexity class.
#' @param cfg Config list.
#' @export
wt_can_decide <- function(user, gate, class, cfg) {
  wt_can(user, "decide", cfg) && identical(user$authority, cfg$decisions[[gate]]$authority[[class]])
}

#' Can a user run an analysis and submit its evidence?
#'
#' Integrators always; contributors when their discipline is listed in the
#' analysis catalog entry (`disciplines`).
#' @param user Output of [wt_resolve_user()].
#' @param analysis Catalog entry.
#' @param cfg Config list.
#' @export
wt_can_run_analysis <- function(user, analysis, cfg) {
  wt_can(user, "edit_case", cfg) ||
    (wt_can(user, "edit_stream", cfg) && user$discipline %in% unlist(analysis$disciplines))
}

#' Can a user edit a discipline-owned item (assurance stream / readiness workstream)?
#'
#' Integrators can edit any item; contributors only items of their discipline.
#' @param user Output of [wt_resolve_user()].
#' @param discipline Discipline owning the item.
#' @param permission `edit_stream` or `edit_workstream`.
#' @param cfg Config list.
#' @export
wt_can_edit_item <- function(user, discipline, permission, cfg) {
  wt_can(user, "edit_case", cfg) || (wt_can(user, permission, cfg) && identical(user$discipline, discipline))
}
