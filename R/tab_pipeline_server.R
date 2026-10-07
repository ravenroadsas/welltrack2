#' Pipeline tab - server
#' @param id Module id.
#' @param app Shared app context.
#' @keywords internal
tab_pipeline_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    cfg <- app$cfg

    shiny::observe({
      f <- sort(unique(app$data()$opportunity$field))
      shiny::updateSelectInput(session, "field", choices = c("All" = "", f), selected = shiny::isolate(input$field))
    })

    filtered <- shiny::reactive({
      o <- app$data()$opportunity
      s <- app$summary()
      o <- merge(o, s[, c("opp_id", "days_in_step", "readiness", "terminal")], by = "opp_id")
      o <- o[!o$terminal, ]
      if (nzchar(input$field %||% "")) o <- o[o$field == input$field, ]
      if (nzchar(input$type %||% "")) o <- o[o$intervention_type == input$type, ]
      q <- trimws(input$search %||% "")
      if (nzchar(q)) o <- o[grepl(q, paste(o$well, o$title, o$opp_id), ignore.case = TRUE), ]
      o
    })

    output$legend <- shiny::renderUI({
      o <- filtered()
      htmltools::div(class = "wt-legend",
        sprintf("%d active cases", nrow(o)),
        htmltools::span(class = "wt-age wt-age-ok", shiny::icon("clock"), "within target time"),
        htmltools::span(class = "wt-age wt-age-warn", shiny::icon("clock"), "close to target"),
        htmltools::span(class = "wt-age wt-age-bad", shiny::icon("clock"), "over target"),
        lapply(names(cfg$complexity_classes), function(k) htmltools::span(wt_class_badge(k, cfg), cfg$complexity_classes[[k]]$name))
      )
    })

    output$board <- shiny::renderUI({
      o <- filtered()
      wt_board(o, cfg, session$ns("open_opp"),
               days_in_step = stats::setNames(o$days_in_step, o$opp_id),
               readiness = stats::setNames(o$readiness, o$opp_id))
    })

    shiny::observeEvent(input$open_opp, app$open_opp(input$open_opp))
  })
}
