#' Decisions tab - UI
#'
#' Cases waiting for D1 / D2 / D3 and the evidence package of the selected case.
#' @param id Module id.
#' @keywords internal
tab_decisions_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::div(class = "wt-page",
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        bslib::card_header(shiny::icon("gavel"), "Waiting for a decision"),
        DT::DTOutput(ns("queue"))
      ),
      bslib::card(
        bslib::card_header(shiny::icon("box-archive"), "Decision package"),
        shiny::uiOutput(ns("package"))
      )
    )
  )
}
