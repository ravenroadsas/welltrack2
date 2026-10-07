#' Process Stats tab - UI
#'
#' Time per process step, flow segments, bottlenecks, WIP ageing and value
#' realization. Filters in one row above the charts.
#' @param id Module id.
#' @param cfg Config list.
#' @keywords internal
tab_stats_ui <- function(id, cfg) {
  ns <- shiny::NS(id)
  types <- wt_type_labels(cfg)
  chart_card <- function(title, out, hint = NULL, height = "250px") {
    bslib::card(bslib::card_header(title, if (!is.null(hint)) htmltools::span(class = "wt-hint", hint)),
                echarts4r::echarts4rOutput(ns(out), height = height))
  }
  htmltools::div(class = "wt-page",
    htmltools::div(class = "wt-filterbar",
      shiny::selectInput(ns("window"), "Period", c("Last 3 months" = 90, "Last 6 months" = 180, "Last 12 months" = 365, "All" = 100000),
                         selected = 365, width = "140px"),
      shiny::selectInput(ns("type"), "Type", c("All" = "", stats::setNames(names(types), types)), width = "150px"),
      shiny::selectInput(ns("class"), "Class", c("All" = "", "A", "B", "C"), width = "90px"),
      shiny::selectInput(ns("field"), "Field", c("All" = ""), width = "150px"),
      shiny::selectInput(ns("group_by"), "Bottleneck by", c("Class" = "class", "Type" = "intervention_type", "Field" = "field"), width = "130px")
    ),
    shiny::uiOutput(ns("kpis")),
    bslib::layout_columns(col_widths = c(6, 6),
      chart_card("Time per step: median vs SLA", "step_time", "completed step occurrences, days"),
      chart_card("Where the time goes", "work_wait", "median working vs waiting days per step")
    ),
    bslib::layout_columns(col_widths = c(4, 4, 4),
      chart_card("Flow segments (doc \u00a737)", "segments", "days, box = P25-P75"),
      chart_card("Lead time trend", "trend", "median opportunity \u2192 executed, by month"),
      chart_card("Bottlenecks: % of SLA", "heat", "median duration / SLA")
    ),
    bslib::layout_columns(col_widths = c(6, 6),
      chart_card("Work in progress ageing", "wip", "open cases by current step"),
      chart_card("Value realization", "realization", "promised vs actual oil rate, bopd")
    ),
    bslib::card(bslib::card_header("Step statistics", htmltools::span(class = "wt-hint", "table view of the charts above")),
                DT::DTOutput(ns("table")))
  )
}
