#' Run WellTrack 2.0
#'
#' @param mock Seed an in-memory database with mock data (UI mockup mode).
#' @param cfg Process configuration (see [wt_load_config()]).
#' @param ... Passed to [shiny::shinyApp()] `options`.
#' @export
run_app <- function(mock = TRUE, cfg = wt_load_config(), ...) {
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
