#' Pipeline tab - UI
#'
#' Portfolio along the decision thread: one board column per step.
#' @param id Module id.
#' @param cfg Config list.
#' @keywords internal
tab_pipeline_ui <- function(id, cfg) {
  ns <- shiny::NS(id)
  types <- wt_type_labels(cfg)
  htmltools::div(class = "wt-page",
    htmltools::div(class = "wt-filterbar",
      shiny::selectInput(ns("field"), "Field", c("All" = ""), width = "150px"),
      shiny::selectInput(ns("type"), "Type", c("All" = "", stats::setNames(names(types), types)), width = "150px"),
      shiny::textInput(ns("search"), "Search", placeholder = "well, text...", width = "160px")
    ),
    shiny::uiOutput(ns("legend")),
    shiny::uiOutput(ns("board"))
  )
}
