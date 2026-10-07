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
      o <- merge(o, s[, c("opp_id", "days_in_step", "sla_days", "over_sla", "next_gate", "readiness", "n_blockers", "terminal")], by = "opp_id")
      if (!isTRUE(input$show_closed)) o <- o[!o$terminal, ]
      if (nzchar(input$field %||% "")) o <- o[o$field == input$field, ]
      if (nzchar(input$type %||% "")) o <- o[o$intervention_type == input$type, ]
      if (nzchar(input$class %||% "")) o <- o[o$class == input$class, ]
      if (isTRUE(input$only_late)) o <- o[o$over_sla, ]
      q <- trimws(input$search %||% "")
      if (nzchar(q)) o <- o[grepl(q, paste(o$well, o$title, o$opp_id), ignore.case = TRUE), ]
      o
    })

    output$legend <- shiny::renderUI({
      o <- filtered()
      htmltools::div(class = "wt-legend",
        sprintf("%d cases \u00b7 %s bopd incremental potential in view", nrow(o), wt_fmt(sum(o$incremental_bopd, na.rm = TRUE))),
        htmltools::span(class = "wt-age wt-age-ok", shiny::icon("clock"), "within SLA"),
        htmltools::span(class = "wt-age wt-age-warn", shiny::icon("clock"), ">75% SLA"),
        htmltools::span(class = "wt-age wt-age-bad", shiny::icon("clock"), "over SLA"),
        lapply(names(cfg$complexity_classes), function(k) htmltools::span(wt_class_badge(k, cfg), cfg$complexity_classes[[k]]$name))
      )
    })

    output$board <- shiny::renderUI({
      o <- filtered()
      wt_board(o, cfg, session$ns("open_opp"),
               days_in_step = stats::setNames(o$days_in_step, o$opp_id),
               readiness = stats::setNames(o$readiness, o$opp_id))
    })

    output$table <- DT::renderDT({
      o <- filtered()
      steps <- wt_steps(cfg)
      shown <- data.frame(
        Case = o$opp_id, Well = o$well, Field = o$field, Type = wt_type_labels(cfg)[o$intervention_type], Class = o$class,
        Step = steps$short[match(o$step_id, steps$id)], State = o$state, `Days in step` = o$days_in_step, SLA = o$sla_days,
        `Next gate` = o$next_gate, `Ready %` = o$readiness, Blockers = o$n_blockers,
        `Reservoir bopd` = o$reservoir_bopd, `Realizable bopd` = o$realizable_bopd, `Cost kUSD` = o$cost_kusd,
        `NPV kUSD` = o$npv_kusd, check.names = FALSE)
      DT::datatable(shown, rownames = FALSE, selection = "single", class = "compact hover wt-dt", filter = "top",
                    options = list(dom = "tip", pageLength = 20, scrollX = TRUE)) |>
        DT::formatStyle("Ready %", background = DT::styleColorBar(c(0, 100), "#cfe0f1"), backgroundSize = "95% 70%",
                        backgroundRepeat = "no-repeat", backgroundPosition = "center")
    })

    shiny::observeEvent(input$table_rows_selected, app$open_opp(filtered()$opp_id[input$table_rows_selected]))
    shiny::observeEvent(input$open_opp, app$open_opp(input$open_opp))
  })
}
