#' Application server
#'
#' Builds the shared `app` context passed to every tab module:
#' config, connection, reactive data, current user, navigation helpers.
#' Preview edition: read-only, the UI never writes to the database.
#' @param cfg Config list.
#' @param con DBI connection.
#' @param users User table.
#' @keywords internal
app_server <- function(cfg, con, users) {
  function(input, output, session) {
    base_user <- wt_resolve_user(session$user, session$groups, users, cfg)
    user <- shiny::reactiveVal(base_user)

    # Demo only (config ui.show_user_switcher): view the app as any user
    shiny::observeEvent(input$dev_user, {
      if (base_user$dev && isTRUE(cfg$ui$show_user_switcher))
        user(wt_resolve_user(input$dev_user, character(), users, cfg))
    })
    output$user_roles <- shiny::renderUI({
      htmltools::tagList(lapply(user()$roles, function(r) wt_badge(cfg$roles[[r]]$name, "role")))
    })

    data <- shiny::reactive(wt_db_read_all(con))
    now <- Sys.time()
    summary <- shiny::reactive(wt_portfolio_summary(data(), cfg, now))

    selected_opp <- shiny::reactiveVal(NULL)
    app <- list(
      cfg = cfg, con = con, users = users, data = data, summary = summary, user = user, now = now,
      selected_opp = selected_opp,
      open_opp = function(id) {
        selected_opp(id)
        bslib::nav_select("nav", "opportunity", session = session)
      },
      open_tab = function(tab) bslib::nav_select("nav", tab, session = session)
    )

    tab_pipeline_server("pipeline", app)
    tab_opportunity_server("opp", app)
    tab_decisions_server("decisions", app)
    tab_stats_server("stats", app)
    mod_newopp_server("newopp", app)
  }
}
