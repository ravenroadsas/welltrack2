#' My Work tab - UI
#'
#' Role-aware inbox: only what needs this person's action, ranked.
#' @param id Module id.
#' @keywords internal
tab_mywork_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::div(class = "wt-page",
    shiny::uiOutput(ns("hello")),
    shiny::uiOutput(ns("kpis")),
    bslib::layout_columns(
      col_widths = c(8, 4),
      bslib::card(
        bslib::card_header(shiny::icon("list-check"), "Action inbox",
                           htmltools::span(class = "wt-hint", "Click a row to open the opportunity")),
        DT::DTOutput(ns("inbox"))
      ),
      htmltools::div(
        bslib::card(
          bslib::card_header(shiny::icon("triangle-exclamation"), "Exceptions to look at"),
          shiny::uiOutput(ns("exceptions"))
        ),
        bslib::card(
          bslib::card_header(shiny::icon("filter"), "Pipeline at a glance"),
          echarts4r::echarts4rOutput(ns("funnel"), height = "230px")
        )
      )
    )
  )
}
