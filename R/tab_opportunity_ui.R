#' Opportunity workspace - UI
#'
#' One shared object per intervention; every discipline contributes here.
#' Header (key numbers) + decision-thread stepper + section tabs + assistant
#' status panel (decision and exception first, not a task list).
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
        bslib::nav_panel("Technical assurance", value = "assurance",
          bslib::layout_columns(col_widths = c(7, 5),
            shiny::uiOutput(ns("streams")),
            htmltools::div(
              htmltools::div(class = "wt-section", "Reservoir \u2192 realizable potential"),
              echarts4r::echarts4rOutput(ns("waterfall"), height = "260px"),
              shiny::uiOutput(ns("wpa"))
            )
          ),
          shiny::uiOutput(ns("stream_edit"))
        ),
        bslib::nav_panel("Value & investment", value = "value", shiny::uiOutput(ns("value"))),
        bslib::nav_panel("Gates & decisions", value = "gates", shiny::uiOutput(ns("gates"))),
        bslib::nav_panel("Analysis", value = "analysis", icon = shiny::icon("chart-line"),
          bslib::layout_columns(col_widths = c(3, 6, 3),
            shiny::uiOutput(ns("an_list")), shiny::uiOutput(ns("an_workspace")), shiny::uiOutput(ns("an_attach")))),
        bslib::nav_panel("Readiness", value = "readiness", shiny::uiOutput(ns("readiness")), shiny::uiOutput(ns("ws_edit"))),
        bslib::nav_panel("Execution & value", value = "execution",
          bslib::layout_columns(col_widths = c(6, 6),
            echarts4r::echarts4rOutput(ns("baseline"), height = "260px"),
            shiny::uiOutput(ns("execution")))),
        bslib::nav_panel("Risks & changes", value = "risks",
          htmltools::div(class = "wt-section", "Risk register"), DT::DTOutput(ns("risks")),
          htmltools::div(class = "wt-section", "Change control after D2"), DT::DTOutput(ns("changes"))),
        bslib::nav_panel("History", value = "history", shiny::uiOutput(ns("history")))
      ),
      htmltools::div(class = "wt-assistant", shiny::uiOutput(ns("assistant")))
    )
  )
}
