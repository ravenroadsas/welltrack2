#' Opportunity workspace - UI
#'
#' One shared record per intervention: header with key numbers, the
#' decision-thread stepper, a few section tabs and a case-status panel.
#' @param id Module id.
#' @keywords internal
tab_opportunity_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::div(class = "wt-page",
    htmltools::div(class = "wt-opp-select",
      shiny::selectizeInput(ns("select"), NULL, choices = NULL, width = "520px",
                            options = list(placeholder = "Select an opportunity (well, id, text)..."))
    ),
    shiny::uiOutput(ns("header")),
    shiny::uiOutput(ns("stepper")),
    bslib::layout_columns(
      col_widths = c(9, 3),
      bslib::navset_card_underline(
        id = ns("tabs"),
        bslib::nav_panel("Overview", value = "overview", shiny::uiOutput(ns("overview"))),
        bslib::nav_panel("Technical assurance", value = "assurance", shiny::uiOutput(ns("streams"))),
        bslib::nav_panel("Gates", value = "gates", shiny::uiOutput(ns("gates"))),
        bslib::nav_panel("Readiness", value = "readiness", shiny::uiOutput(ns("readiness")))
      ),
      htmltools::div(class = "wt-assistant", shiny::uiOutput(ns("status")))
    )
  )
}
