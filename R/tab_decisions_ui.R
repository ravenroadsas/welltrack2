#' Decisions tab - UI
#'
#' Queue of cases pending D1 / D2 / D3 and the decision package of the
#' selected case, with the decision form (outcomes from config).
#' @param id Module id.
#' @keywords internal
tab_decisions_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::div(class = "wt-page",
    bslib::layout_columns(
      col_widths = c(5, 7),
      bslib::card(
        bslib::card_header(shiny::icon("gavel"), "Decision queue",
          shiny::radioButtons(ns("scope"), NULL, c("Mine" = "mine", "All" = "all"), selected = "all", inline = TRUE)),
        DT::DTOutput(ns("queue"))
      ),
      bslib::card(
        bslib::card_header(shiny::icon("box-archive"), "Decision package"),
        shiny::uiOutput(ns("package")),
        shiny::uiOutput(ns("form"))
      )
    )
  )
}
