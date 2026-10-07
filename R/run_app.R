#' Run WellTrack 2.0
#'
#' @param mock Seed an in-memory database with mock data (UI mockup mode).
#' @param cfg Process configuration (see [wt_load_config()]).
#' @param show_user_switcher Show the "view as user" selector. `NULL` (default)
#'   uses config `ui.show_user_switcher`; `TRUE`/`FALSE` overrides it for this run.
#' @param ... Passed to [shiny::shinyApp()] `options`.
#' @export
run_app <- function(mock = TRUE, cfg = wt_load_config(), show_user_switcher = NULL, ...) {
  if (!is.null(show_user_switcher)) cfg$ui$show_user_switcher <- isTRUE(show_user_switcher)
  con <- wt_db_connect()
  if (mock) wt_db_seed(con, wt_mock_data(cfg))
  users <- wt_read_users()
  shiny::addResourcePath("wt-www", wt_sys_file("app", "www"))
  shiny::onStop(function() wt_db_disconnect(con))
  shiny::shinyApp(
    ui = app_ui(cfg, users),
    server = app_server(cfg, con, users),
    options = list(...)
  )
}
