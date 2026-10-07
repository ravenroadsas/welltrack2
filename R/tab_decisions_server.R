#' Decisions tab - server
#' @param id Module id.
#' @param app Shared app context.
#' @keywords internal
tab_decisions_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cfg <- app$cfg

    queue <- shiny::reactive({
      s <- app$summary()
      o <- app$data()$opportunity
      q <- s[!s$terminal & s$step_id %in% names(cfg$decisions), , drop = FALSE]
      q$well <- o$well[match(q$opp_id, o$opp_id)]
      q[order(q$step_id, -q$readiness), ]
    })

    output$queue <- DT::renderDT({
      q <- queue()
      shown <- data.frame(Gate = q$step_id, Case = q$opp_id, Well = q$well, Class = q$class,
                          `Evidence %` = q$readiness, `Waiting (d)` = q$days_in_step, check.names = FALSE)
      DT::datatable(shown, rownames = FALSE, selection = list(mode = "single", selected = if (nrow(q)) 1),
                    class = "compact hover wt-dt", options = list(dom = "tp", pageLength = 15, ordering = FALSE))
    })

    sel <- shiny::reactive({
      q <- queue()
      if (!nrow(q)) return(NULL)
      i <- input$queue_rows_selected
      if (!length(i) || i > nrow(q)) i <- 1
      q[i, ]
    })

    output$package <- shiny::renderUI({
      q <- sel()
      if (is.null(q)) return(wt_callout("Nothing waiting", type = "neutral", "No case is waiting for a decision."))
      ctx <- wt_case_context(app$data(), q$opp_id)
      o <- ctx$opp
      g <- q$step_id
      step <- cfg$steps[[match(g, wt_steps(cfg)$id)]]
      ev <- wt_evaluate_gate(ctx, g, cfg)
      htmltools::tagList(
        htmltools::div(class = "wt-pkg-head",
          htmltools::span(class = "wt-opp-well", o$well), wt_class_badge(o$class, cfg),
          htmltools::strong(step$name), htmltools::span(class = "wt-hint", step$question),
          shiny::actionLink(ns("open_case"), "open case \u2197")),
        bslib::layout_column_wrap(width = 1 / 3,
          wt_kpi("Realizable potential", wt_fmt(o$realizable_bopd, " bopd"), paste("reservoir", wt_fmt(o$reservoir_bopd))),
          wt_kpi("Estimated cost", wt_fmt(o$cost_kusd, " kUSD")),
          wt_kpi("Evidence", paste0(wt_gate_readiness(ev), "%"), sprintf("%d of %d criteria", sum(ev$status == "met"), nrow(ev)),
                 if (wt_gate_readiness(ev) == 100) "ok" else "warn")),
        htmltools::tags$table(class = "wt-crit", lapply(seq_len(nrow(ev)), function(i) htmltools::tags$tr(
          htmltools::tags$td(wt_status_chip(if (ev$status[i] == "met") "met" else "open")), htmltools::tags$td(ev$label[i])))),
        htmltools::div(class = "wt-hint", style = "margin-top:8px",
          "Possible outcomes: ", paste(gsub("_", " ", names(cfg$decisions[[g]]$outcomes)), collapse = " \u00b7 "))
      )
    })

    shiny::observeEvent(input$open_case, app$open_opp(sel()$opp_id))
  })
}
