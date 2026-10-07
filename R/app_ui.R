#' Application UI
#' @param cfg Config list.
#' @param users User table.
#' @keywords internal
app_ui <- function(cfg, users) {
  theme <- bslib::bs_theme(
    version = 5, primary = "#1f4e79", secondary = "#5b6770",
    success = "#2e7d32", warning = "#ed8c00", danger = "#c62828", info = "#1565c0",
    base_font = bslib::font_collection("Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
    "font-size-base" = "0.8rem", "border-radius" = "2px", "card-spacer-y" = "0.5rem", "card-spacer-x" = "0.6rem"
  )
  dev_choices <- stats::setNames(users$user, sprintf("%s (%s)", users$display_name, gsub(";", ", ", users$roles)))

  bslib::page_navbar(
    id = "nav",
    title = htmltools::span(class = "wt-brand", shiny::icon("oil-well"), "WellTrack", htmltools::tags$sup("2.0 preview")),
    window_title = "WellTrack 2.0 (preview)",
    theme = theme,
    bg = "#1d252c",
    inverse = TRUE,
    fillable = FALSE,
    header = htmltools::tagList(
      htmltools::tags$link(rel = "stylesheet", href = "wt-www/welltrack.css"),
      htmltools::div(class = "wt-mockbar", shiny::icon("flask"),
        htmltools::strong("EARLY PREVIEW"), " \u2014 a concept to collect your feedback, not a working tool. ",
        "Data is fictitious, screens will change, and nothing you enter is saved.")
    ),
    bslib::nav_panel("Pipeline", value = "pipeline", icon = shiny::icon("table-columns"), tab_pipeline_ui("pipeline", cfg)),
    bslib::nav_panel("Opportunity", value = "opportunity", icon = shiny::icon("folder-open"), tab_opportunity_ui("opp")),
    bslib::nav_panel("Decisions", value = "decisions", icon = shiny::icon("gavel"), tab_decisions_ui("decisions")),
    bslib::nav_panel("Process Stats", value = "stats", icon = shiny::icon("chart-column"), tab_stats_ui("stats", cfg)),
    bslib::nav_spacer(),
    bslib::nav_item(shiny::actionButton("newopp-open", "New opportunity", icon = shiny::icon("plus"), class = "btn-sm btn-warning wt-new")),
    bslib::nav_item(htmltools::div(class = "wt-user",
      shiny::icon("user"),
      shiny::selectInput("dev_user", NULL, choices = dev_choices, selected = Sys.getenv("WT_DEV_USER", "juan.surv"),
                         width = "230px", selectize = FALSE),
      shiny::uiOutput("user_roles", inline = TRUE)
    ))
  )
}
