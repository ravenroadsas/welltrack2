#' Process Stats tab - UI
#'
#' Time spent per process step against its target, and current work in progress.
#' @param id Module id.
#' @param cfg Config list.
#' @keywords internal
tab_stats_ui <- function(id, cfg) {
  ns <- shiny::NS(id)
  types <- wt_type_labels(cfg)
  chart_card <- function(title, out, hint) {
    bslib::card(bslib::card_header(title, htmltools::span(class = "wt-hint", hint)),
                echarts4r::echarts4rOutput(ns(out), height = "280px"))
  }
  htmltools::div(class = "wt-page",
    htmltools::div(class = "wt-filterbar",
      shiny::selectInput(ns("type"), "Type", c("All" = "", stats::setNames(names(types), types)), width = "150px")
    ),
    shiny::uiOutput(ns("kpis")),
    bslib::layout_columns(col_widths = c(7, 5),
      chart_card("Time per step", "step_time", "median days of completed steps vs target"),
      chart_card("Work in progress", "wip", "active cases by current step")
    )
  )
}
