#' Opportunity workspace - server
#' @param id Module id.
#' @param app Shared app context.
#' @keywords internal
tab_opportunity_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    cfg <- app$cfg

    # --- selection (synced with the app-wide selected opportunity) -----------
    shiny::observe({
      o <- app$data()$opportunity
      o <- o[!o$state %in% wt_states(cfg)$id[wt_states(cfg)$terminal], ]
      o <- o[order(o$opp_id, decreasing = TRUE), ]
      ch <- stats::setNames(o$opp_id, sprintf("%s \u00b7 %s \u00b7 %s", o$opp_id, o$well, substr(o$title, 1, 60)))
      sel <- shiny::isolate(app$selected_opp()) %||% o$opp_id[o$step_id == "D2"][1]
      shiny::updateSelectizeInput(session, "select", choices = ch, selected = sel)
    })
    shiny::observeEvent(app$selected_opp(), {
      if (!identical(input$select, app$selected_opp()))
        shiny::updateSelectizeInput(session, "select", selected = app$selected_opp())
    })
    shiny::observeEvent(input$select, {
      if (nzchar(input$select)) app$selected_opp(input$select)
    })

    ctx <- shiny::reactive({
      shiny::req(input$select)
      wt_case_context(app$data(), input$select)
    })
    status <- shiny::reactive(wt_status_summary(ctx(), cfg))
    opp <- shiny::reactive(ctx()$opp)

    # --- header ----------------------------------------------------------------
    output$header <- shiny::renderUI({
      o <- opp()
      htmltools::div(class = "wt-opp-header",
        htmltools::div(class = "wt-opp-title",
          htmltools::span(class = "wt-opp-well", o$well), wt_class_badge(o$class, cfg),
          wt_badge(wt_type_labels(cfg)[[o$intervention_type]], "info"), wt_status_chip(o$state),
          htmltools::span(class = "wt-opp-text", o$title)),
        htmltools::div(class = "wt-opp-figures wt-opp-figures-4",
          wt_kpi("Current", wt_fmt(o$current_bopd, " bopd")),
          wt_kpi("Reservoir potential", wt_fmt(o$reservoir_bopd, " bopd")),
          wt_kpi("Realizable potential", wt_fmt(o$realizable_bopd, " bopd"), if (is.na(o$realizable_bopd)) "pending assurance"),
          wt_kpi("Estimated cost", wt_fmt(o$cost_kusd, " kUSD"))
        )
      )
    })

    output$stepper <- shiny::renderUI({
      h <- ctx()$history
      h <- h[!h$recycle, ]
      end <- h$exited_at; end[is.na(end)] <- app$now
      days <- stats::setNames(round(as.numeric(difftime(end, h$entered_at, units = "days"))), h$step_id)
      wt_stepper(cfg, opp()$step_id, opp()$state, days)
    })

    # --- case status -----------------------------------------------------------
    output$status <- shiny::renderUI({
      o <- opp(); s <- status()
      gate_name <- if (is.na(s$gate)) "No pending gate" else cfg$steps[[match(s$gate, wt_steps(cfg)$id)]]$name
      n <- nrow(s$blockers)
      htmltools::tagList(
        htmltools::div(class = "wt-section", "Case status"),
        htmltools::div(class = "wt-assist-head", htmltools::strong("Next gate: ", gate_name),
                       if (!is.na(s$readiness)) wt_progress(s$readiness, paste0(s$readiness, "% of evidence in place"))),
        if (n) wt_callout(sprintf("%d open item%s", n, if (n > 1) "s" else ""), type = "warn",
          htmltools::tags$ul(lapply(seq_len(min(3, n)), function(i)
            htmltools::tags$li(s$blockers$item[i], htmltools::span(class = "wt-hint", " \u00b7 ", sub(" \\(confirm\\)", "", s$blockers$owner[i]))))),
          if (n > 3) htmltools::div(class = "wt-hint", sprintf("+ %d more", n - 3)))
        else if (!is.na(s$gate)) wt_callout("Ready for decision", type = "ok", paste0("All evidence for ", s$gate, " is in place."))
      )
    })

    # --- overview ----------------------------------------------------------------
    output$overview <- shiny::renderUI({
      o <- opp()
      f <- function(label, value) htmltools::tags$tr(
        htmltools::tags$th(label),
        htmltools::tags$td(if (is.null(value) || is.na(value) || identical(value, "")) "\u2013" else value))
      bslib::layout_columns(col_widths = c(6, 6),
        htmltools::div(htmltools::div(class = "wt-section", "Framed opportunity"),
          htmltools::tags$table(class = "wt-kv",
            f("Opportunity", o$opp_id), f("Well", o$well), f("Field / asset", o$field),
            f("Intervention type", wt_type_labels(cfg)[[o$intervention_type]]), f("Reason", o$reason),
            f("Hypothesis", o$hypothesis), f("Known constraints", o$constraints), f("Uncertainty", o$uncertainty),
            f("Originator", o$originator), f("Created", format(o$created_at, "%Y-%m-%d")))),
        htmltools::div(htmltools::div(class = "wt-section", "Classification"),
          htmltools::tags$table(class = "wt-kv",
            f("Complexity class", paste(o$class, "-", cfg$complexity_classes[[o$class]]$name)),
            f("Artificial lift", if (isTRUE(o$artificial_lift)) "Yes" else "No"),
            f("ANH / F7CR required", if (isTRUE(o$regulatory_required)) "Yes" else "No"),
            f("Required disciplines", paste(wt_streams(cfg)$name[match(wt_required_streams(o, cfg), wt_streams(cfg)$id)], collapse = ", ")))))
    })

    # --- technical assurance -------------------------------------------------------
    output$streams <- shiny::renderUI({
      o <- opp(); st <- ctx()$streams
      if (!o$step_id %in% c("S2", "S3", "D2", "S4", "D3", "S5", "S6"))
        return(wt_callout("Not started", "Technical assurance starts after D1 - PURSUE.", type = "neutral"))
      req <- wt_required_streams(o, cfg)
      sdef <- Filter(function(s) s$id %in% req, cfg$assurance_streams)
      htmltools::div(class = "wt-stream-grid", lapply(sdef, function(s) {
        row <- st[st$stream_id == s$id, , drop = FALSE]
        stat <- if (nrow(row)) row$status[1] else "not_started"
        htmltools::div(class = paste("wt-stream", paste0("wt-stream-", stat)),
          htmltools::div(class = "wt-stream-head", htmltools::strong(s$name), wt_status_chip(stat)),
          htmltools::div(class = "wt-stream-q", s$question),
          htmltools::div(class = "wt-stream-meta", shiny::icon("user"), if (nrow(row)) row$owner[1] else "unassigned"))
      }))
    })

    # --- gates -------------------------------------------------------------------
    output$gates <- shiny::renderUI({
      c0 <- ctx(); o <- c0$opp
      cards <- lapply(names(cfg$decisions), function(g) {
        ev <- wt_evaluate_gate(c0, g, cfg)
        dec <- c0$decisions[c0$decisions$gate == g & !c0$decisions$superseded, , drop = FALSE]
        gate_open <- identical(o$step_id, g)
        bslib::card(class = if (gate_open) "wt-gate-open",
          bslib::card_header(htmltools::strong(cfg$steps[[match(g, wt_steps(cfg)$id)]]$name),
                             if (nrow(dec)) wt_status_chip(dec$outcome[1]) else if (gate_open) wt_badge("PENDING", "warn")),
          wt_progress(wt_gate_readiness(ev)),
          htmltools::tags$table(class = "wt-crit", lapply(seq_len(nrow(ev)), function(i) htmltools::tags$tr(
            htmltools::tags$td(wt_status_chip(if (ev$status[i] == "met") "met" else "open")), htmltools::tags$td(ev$label[i])))),
          if (nrow(dec)) htmltools::div(class = "wt-decision-rec",
            htmltools::strong(dec$outcome[1]), " on ", format(dec$decided_at[1], "%Y-%m-%d"),
            htmltools::br(), htmltools::em(dec$rationale[1]))
        )
      })
      do.call(bslib::layout_column_wrap, c(list(width = 1 / 3), cards))
    })

    # --- readiness ---------------------------------------------------------------
    output$readiness <- shiny::renderUI({
      o <- opp(); ws <- ctx()$workstreams
      if (!o$step_id %in% c("S4", "D3", "S5", "S6"))
        return(wt_callout("Preparation starts after D2 - GO", type = "neutral",
                          "Preparation workstreams run in parallel once investment is approved."))
      wn <- wt_workstreams(cfg)
      htmltools::tags$table(class = "wt-table",
        htmltools::tags$thead(htmltools::tags$tr(lapply(c("Workstream", "Owner", "Due", "Status"), htmltools::tags$th))),
        htmltools::tags$tbody(lapply(seq_len(nrow(ws)), function(i) htmltools::tags$tr(
          htmltools::tags$td(wn$name[match(ws$ws_id[i], wn$id)]), htmltools::tags$td(ws$owner[i]),
          htmltools::tags$td(format(ws$due_date[i])), htmltools::tags$td(wt_status_chip(ws$status[i]))))))
    })
  })
}
