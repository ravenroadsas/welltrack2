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
      q$npv <- o$npv_kusd[match(q$opp_id, o$opp_id)]
      q$authority <- mapply(function(g, c) cfg$decisions[[g]]$authority[[c]], q$step_id, q$class)
      if (identical(input$scope, "mine")) q <- q[q$authority == app$user()$authority, , drop = FALSE]
      q[order(q$step_id, -q$readiness, -q$days_in_step), ]
    })

    output$queue <- DT::renderDT({
      q <- queue()
      shown <- data.frame(Gate = q$step_id, Case = q$opp_id, Well = q$well, Class = q$class, `Ready %` = q$readiness,
                          Waiting = q$days_in_step, SLA = q$sla_days,
                          WPA = ifelse(q$wpa_meeting, "meeting", "async"), Authority = q$authority, check.names = FALSE)
      DT::datatable(shown, rownames = FALSE, selection = list(mode = "single", selected = if (nrow(q)) 1),
                    class = "compact hover wt-dt", options = list(dom = "tp", pageLength = 15, ordering = FALSE)) |>
        DT::formatStyle("Ready %", color = DT::styleInterval(c(59, 99), c("#c62828", "#ed8c00", "#2e7d32")), fontWeight = "bold") |>
        DT::formatStyle("WPA", color = DT::styleEqual(c("meeting", "async"), c("#ed8c00", "#2e7d32")))
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
      if (is.null(q)) return(wt_callout("Select a case", type = "neutral", "Pick a case in the queue to review its decision package."))
      ctx <- wt_case_context(app$data(), q$opp_id)
      o <- ctx$opp
      g <- q$step_id
      ev <- wt_evaluate_gate(ctx, g, cfg)
      s <- wt_status_summary(ctx, cfg)
      htmltools::tagList(
        htmltools::div(class = "wt-pkg-head",
          htmltools::span(class = "wt-opp-well", o$well), wt_class_badge(o$class, cfg),
          htmltools::strong(cfg$steps[[match(g, wt_steps(cfg)$id)]]$name), htmltools::span(class = "wt-hint", cfg$steps[[match(g, wt_steps(cfg)$id)]]$question),
          shiny::actionLink(ns("open_case"), "open full case \u2197")),
        bslib::layout_column_wrap(width = 1 / 5,
          wt_kpi("Realizable", wt_fmt(o$realizable_bopd, " bopd"), paste("reservoir", wt_fmt(o$reservoir_bopd))),
          wt_kpi("Incremental", wt_fmt(o$incremental_bopd, " bopd")),
          wt_kpi("Cost", wt_fmt(o$cost_kusd, " kUSD"), if (!is.na(o$cost_uncertainty_pct)) paste0("\u00b1", o$cost_uncertainty_pct, "%")),
          wt_kpi("NPV", wt_fmt(o$npv_kusd, " kUSD"), status = if (is.na(o$npv_kusd)) "neutral" else if (o$npv_kusd > 0) "ok" else "bad"),
          wt_kpi("Evidence", paste0(wt_gate_readiness(ev), "%"), sprintf("%d/%d criteria", sum(ev$status == "met"), nrow(ev)),
                 if (wt_gate_readiness(ev) == 100) "ok" else "warn")),
        htmltools::tags$table(class = "wt-crit", lapply(seq_len(nrow(ev)), function(i) htmltools::tags$tr(
          htmltools::tags$td(wt_status_chip(ev$status[i])), htmltools::tags$td(ev$label[i]), htmltools::tags$td(wt_mode_icon(ev$mode[i]))))),
        if (length(s$wpa)) wt_callout("Exceptions for discussion", type = "warn", htmltools::tags$ul(lapply(s$wpa, htmltools::tags$li))),
        if (nrow(ctx$risks)) {
          r <- ctx$risks[wt_risk_score(ctx$risks$probability, ctx$risks$consequence) >= 10 & ctx$risks$status != "closed", ]
          if (nrow(r)) wt_callout(sprintf("%d material open risk(s)", nrow(r)), type = "info",
                                  htmltools::tags$ul(lapply(paste0(r$category, ": ", r$description), htmltools::tags$li)))
        }
      )
    })

    output$form <- shiny::renderUI({
      q <- sel()
      if (is.null(q)) return(NULL)
      u <- app$user()
      g <- q$step_id
      if (!wt_can_decide(u, g, q$class, cfg)) {
        return(wt_callout("Read-only", type = "neutral",
          sprintf("%s for class %s is decided by: %s. You are: %s.", g, q$class, q$authority,
                  if (nzchar(u$authority)) u$authority else paste(u$roles, collapse = ", ")),
          htmltools::div(class = "wt-hint", "Mockup tip: switch user in the top-right selector to act as that authority.")))
      }
      outcomes <- names(cfg$decisions[[g]]$outcomes)
      htmltools::div(class = "wt-decide",
        htmltools::div(class = "wt-section", htmltools::span(class = "wt-live", "LIVE"), "Record decision"),
        shiny::radioButtons(ns("decide_outcome"), NULL, stats::setNames(outcomes, gsub("_", " ", outcomes)), inline = TRUE),
        shiny::textAreaInput(ns("decide_rationale"), "Rationale", rows = 2, width = "100%"),
        shiny::textInput(ns("decide_conditions"), "Conditions / open actions (optional)", width = "100%"),
        shiny::actionButton(ns("decide_submit"), "Sign decision", class = "btn-sm btn-warning", icon = shiny::icon("signature")),
        htmltools::span(class = "wt-hint", " Stored with approver, date, evidence version and resulting state.")
      )
    })

    shiny::observeEvent(input$decide_submit, {
      q <- sel()
      shiny::req(q)
      if (!nzchar(trimws(input$decide_rationale))) {
        shiny::showNotification("A rationale is required for every decision record.", type = "error")
        return()
      }
      new_state <- tryCatch(
        wt_db_record_decision(app$con, q$opp_id, q$step_id, input$decide_outcome, app$user()$user,
                              input$decide_rationale, input$decide_conditions, cfg),
        error = function(e) { shiny::showNotification(conditionMessage(e), type = "error"); NULL })
      if (!is.null(new_state)) {
        app$bump()
        shiny::showNotification(sprintf("%s recorded: %s \u2192 %s", q$step_id, input$decide_outcome, new_state), type = "message")
      }
    })

    shiny::observeEvent(input$open_case, app$open_opp(sel()$opp_id))
  })
}
