#' Pipeline tab - UI
#'
#' Portfolio along the decision thread: board (one column per step) or table.
#' @param id Module id.
#' @param cfg Config list.
#' @keywords internal
tab_pipeline_ui <- function(id, cfg) {
  ns <- shiny::NS(id)
  types <- wt_type_labels(cfg)
  htmltools::div(class = "wt-page",
    htmltools::div(class = "wt-filterbar",
      shiny::radioButtons(ns("view"), NULL, c("Board" = "board", "Table" = "table"), inline = TRUE),
      shiny::selectInput(ns("field"), "Field", c("All" = ""), width = "150px"),
      shiny::selectInput(ns("type"), "Type", c("All" = "", stats::setNames(names(types), types)), width = "150px"),
      shiny::selectInput(ns("class"), "Class", c("All" = "", "A", "B", "C"), width = "90px"),
      shiny::textInput(ns("search"), "Search", placeholder = "well, text...", width = "160px"),
      shiny::checkboxInput(ns("only_late"), "Only over SLA", FALSE),
      shiny::checkboxInput(ns("show_closed"), "Include closed / terminal", FALSE)
    ),
    shiny::uiOutput(ns("legend")),
    shiny::conditionalPanel(sprintf("input['%s'] == 'board'", ns("view")), shiny::uiOutput(ns("board"))),
    shiny::conditionalPanel(sprintf("input['%s'] == 'table'", ns("view")),
      bslib::card(DT::DTOutput(ns("table"))))
  )
}
