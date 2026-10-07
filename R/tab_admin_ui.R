#' Admin tab - UI
#'
#' Read-only view of the process configuration (what drives the app), the
#' account types / permission matrix, and the activity log mapped to process
#' phases (process-mining preview).
#' @param id Module id.
#' @keywords internal
tab_admin_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::div(class = "wt-page",
    bslib::navset_card_underline(
      id = ns("tabs"),
      bslib::nav_panel("Process configuration",
        shiny::uiOutput(ns("cfg_head")),
        bslib::layout_columns(col_widths = c(6, 6),
          htmltools::div(htmltools::div(class = "wt-section", "Lifecycle steps & SLA (days A/B/C)"), DT::DTOutput(ns("steps"))),
          htmltools::div(htmltools::div(class = "wt-section", "Gate criteria & automation mode"),
                         echarts4r::echarts4rOutput(ns("automation"), height = "130px"), DT::DTOutput(ns("criteria")))),
        bslib::layout_columns(col_widths = c(6, 6),
          htmltools::div(htmltools::div(class = "wt-section", "Complexity classes"), DT::DTOutput(ns("classes"))),
          htmltools::div(htmltools::div(class = "wt-section", "Assurance streams & readiness workstreams"), DT::DTOutput(ns("streams"))))
      ),
      bslib::nav_panel("Account types & users",
        htmltools::div(class = "wt-section", "Account types (roles) and permissions"), DT::DTOutput(ns("roles")),
        htmltools::div(class = "wt-section", "Users (CSV now \u2192 pin on Posit Connect)"), DT::DTOutput(ns("users"))
      ),
      bslib::nav_panel("Activity & process mining",
        wt_callout("How it works", type = "info",
          "Raw Shiny events (input changes, navigation) are captured in the browser and sent in batches, mapped to human-readable ",
          "phases through config `activity_phases`, and stored as an event log (case = opportunity, activity = phase, resource = user). ",
          "Export feeds bupaR / pm4py / Celonis."),
        bslib::layout_columns(col_widths = c(5, 7),
          htmltools::div(htmltools::div(class = "wt-section", "Time spent by phase (minutes)"),
                         echarts4r::echarts4rOutput(ns("phase_time"), height = "300px")),
          htmltools::div(htmltools::div(class = "wt-section", "Directly-follows matrix (phase \u2192 next phase)"),
                         echarts4r::echarts4rOutput(ns("dfg"), height = "300px"))),
        htmltools::div(class = "wt-section", "Event log", shiny::actionLink(ns("refresh"), "refresh", icon = shiny::icon("rotate")),
                       shiny::downloadLink(ns("export"), "export CSV")),
        DT::DTOutput(ns("eventlog"))
      ),
      bslib::nav_panel("Raw YAML", htmltools::tags$pre(class = "wt-yaml", shiny::textOutput(ns("yaml"))))
    )
  )
}
