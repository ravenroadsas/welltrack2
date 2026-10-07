#' Opportunity workspace - server
#' @param id Module id.
#' @param app Shared app context.
#' @keywords internal
tab_opportunity_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cfg <- app$cfg

    # --- selection (synced with the app-wide selected opportunity) -----------
    shiny::observe({
      o <- app$data()$opportunity
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
      gap <- if (!is.na(o$realizable_bopd)) round(100 * (1 - o$realizable_bopd / o$reservoir_bopd)) else NA
      htmltools::div(class = "wt-opp-header",
        htmltools::div(class = "wt-opp-title",
          htmltools::span(class = "wt-opp-well", o$well), wt_class_badge(o$class, cfg),
          wt_badge(wt_type_labels(cfg)[[o$intervention_type]], "info"), wt_status_chip(o$state),
          htmltools::span(class = "wt-opp-text", o$title)),
        htmltools::div(class = "wt-opp-figures",
          wt_kpi("Current", wt_fmt(o$current_bopd, " bopd")),
          wt_kpi("Reservoir potential", wt_fmt(o$reservoir_bopd, " bopd")),
          wt_kpi("Realizable", wt_fmt(o$realizable_bopd, " bopd"),
                 if (!is.na(gap)) paste0("-", gap, "% vs reservoir") else "pending assurance",
                 if (is.na(gap)) "neutral" else if (gap > 30) "warn" else "ok"),
          wt_kpi("Incremental", wt_fmt(o$incremental_bopd, " bopd")),
          wt_kpi("Cost", wt_fmt(o$cost_kusd, " kUSD"), if (!is.na(o$cost_uncertainty_pct)) paste0("\u00b1", o$cost_uncertainty_pct, "%")),
          wt_kpi("NPV", wt_fmt(o$npv_kusd, " kUSD"), if (!is.na(o$irr_pct)) paste0("IRR ", o$irr_pct, "% \u00b7 payout ", o$payout_months, " m"),
                 if (is.na(o$npv_kusd)) "neutral" else if (o$npv_kusd > 0) "ok" else "bad")
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

    # --- assistant (decision-centric status) ---------------------------------
    output$assistant <- shiny::renderUI({
      o <- opp(); s <- status()
      gate_name <- if (is.na(s$gate)) "No pending gate" else cfg$steps[[match(s$gate, wt_steps(cfg)$id)]]$name
      htmltools::tagList(
        htmltools::div(class = "wt-section", shiny::icon("robot"), "Case status"),
        htmltools::div(class = "wt-assist-head", htmltools::strong(o$well, " \u2014 ", gate_name),
                       if (!is.na(s$readiness)) wt_progress(s$readiness, paste0(s$readiness, "% ready"))),
        htmltools::tags$table(class = "wt-mini",
          htmltools::tags$tr(htmltools::tags$td("Reservoir potential"), htmltools::tags$td(wt_fmt(o$reservoir_bopd, " BOPD"))),
          htmltools::tags$tr(htmltools::tags$td("Realizable potential"), htmltools::tags$td(wt_fmt(o$realizable_bopd, " BOPD"))),
          htmltools::tags$tr(htmltools::tags$td("Expected incremental"), htmltools::tags$td(wt_fmt(o$incremental_bopd, " BOPD"))),
          htmltools::tags$tr(htmltools::tags$td("Estimated cost"), htmltools::tags$td(wt_fmt(o$cost_kusd, " kUSD"),
            if (!is.na(o$cost_uncertainty_pct)) paste0(" \u00b1", o$cost_uncertainty_pct, "%"))),
          htmltools::tags$tr(htmltools::tags$td("Economics"), htmltools::tags$td(
            if (is.na(o$npv_kusd)) "not evaluated" else if (o$npv_kusd > 0) "attractive" else "negative NPV"))
        ),
        if (nrow(s$blockers)) wt_callout(sprintf("%d blocking item%s", nrow(s$blockers), if (nrow(s$blockers) > 1) "s" else ""),
          type = "bad",
          htmltools::tags$ul(lapply(seq_len(min(5, nrow(s$blockers))), function(i)
            htmltools::tags$li(htmltools::strong(s$blockers$item[i]), htmltools::br(),
                               htmltools::span(class = "wt-hint", "Owner: ", s$blockers$owner[i], " \u00b7 ", s$blockers$action[i])))))
        else if (!is.na(s$gate)) wt_callout("No blockers", type = "ok", paste0("Evidence complete: the case can go to ", s$gate, ".")),
        wt_callout("Next action", type = "info", s$next_action),
        if (length(s$wpa)) wt_callout("WPA meeting required", type = "warn", htmltools::tags$ul(lapply(s$wpa, htmltools::tags$li)))
        else if (o$step_id %in% c("S2", "S3", "D2")) wt_callout("Asynchronous approval", type = "ok", "No WPA exception triggered."),
        htmltools::div(class = "wt-ask",
          shiny::textInput(ns("ask"), NULL, placeholder = "Ask: What is missing for D2? (LLM, phase 2)"),
          htmltools::span(class = "wt-hint", "Assistant answers will be grounded on this case object and the process config."))
      )
    })

    # --- overview ----------------------------------------------------------------
    output$overview <- shiny::renderUI({
      o <- opp()
      f <- function(label, value, auto = FALSE) htmltools::tags$tr(
        htmltools::tags$th(label, if (auto) htmltools::span(class = "wt-auto", title = "Retrieved automatically from master data", "auto")),
        htmltools::tags$td(if (is.null(value) || is.na(value) || identical(value, "")) "\u2013" else value))
      bslib::layout_columns(col_widths = c(6, 6),
        htmltools::div(htmltools::div(class = "wt-section", "Framed opportunity (Stage 1)"),
          htmltools::tags$table(class = "wt-kv",
            f("Opportunity", o$opp_id), f("Well", o$well, TRUE), f("Field / asset", o$field, TRUE),
            f("Intervention type", wt_type_labels(cfg)[[o$intervention_type]]), f("Reason", o$reason),
            f("Hypothesis", o$hypothesis), f("Known constraints", o$constraints), f("Uncertainty", o$uncertainty),
            f("Originator", o$originator), f("Created", format(o$created_at, "%Y-%m-%d")))),
        htmltools::div(htmltools::div(class = "wt-section", "Well context & classification"),
          htmltools::tags$table(class = "wt-kv",
            f("Current oil rate", wt_fmt(o$current_bopd, " bopd"), TRUE), f("Artificial lift", if (isTRUE(o$artificial_lift)) "Yes" else "No", TRUE),
            f("HSE / integrity risk", o$hse_risk), f("Novel technology", if (isTRUE(o$novelty)) "Yes" else "No"),
            f("ANH / F7CR required", if (isTRUE(o$regulatory_required)) "Yes" else "No"),
            f("Complexity class", paste(o$class, "-", cfg$complexity_classes[[o$class]]$name)),
            f("Required streams", paste(wt_streams(cfg)$name[match(wt_required_streams(o, cfg), wt_streams(cfg)$id)], collapse = ", ")),
            f("WPA mode", cfg$complexity_classes[[o$class]]$wpa_meeting), f("WRR mode", cfg$complexity_classes[[o$class]]$wrr)),
          wt_callout("Automatic retrieval", type = "info",
            "Fields marked ", htmltools::span(class = "wt-auto", "auto"),
            " come from well master, production and ALS systems (phase 2 connectors); users only add judgment.")))
    })

    # --- technical assurance -------------------------------------------------------
    output$streams <- shiny::renderUI({
      o <- opp(); st <- ctx()$streams
      req <- wt_required_streams(o, cfg)
      sdef <- cfg$assurance_streams
      if (!o$step_id %in% c("S2", "S3", "D2", "S4", "D3", "S5", "S6"))
        return(wt_callout("Not started", "Technical assurance starts after D1 - PURSUE.", type = "neutral"))
      htmltools::div(class = "wt-stream-grid", lapply(sdef, function(s) {
        row <- st[st$stream_id == s$id, , drop = FALSE]
        required <- s$id %in% req
        stat <- if (nrow(row)) row$status[1] else if (required) "not_started" else "not_applicable"
        htmltools::div(class = paste("wt-stream", paste0("wt-stream-", stat), if (!required) "wt-stream-na"),
          htmltools::div(class = "wt-stream-head", htmltools::strong(s$name), wt_status_chip(stat)),
          htmltools::div(class = "wt-stream-q", s$question),
          if (required) htmltools::div(class = "wt-stream-meta",
            shiny::icon("user"), if (nrow(row)) row$owner[1] else "unassigned",
            if (nrow(row) && !is.na(row$constraint_bopd[1])) htmltools::span(" \u00b7 constraint ", wt_fmt(-row$constraint_bopd[1], " bopd")),
            if (nrow(row) && nzchar(row$note[1])) htmltools::div(class = "wt-stream-note", shiny::icon("comment"), row$note[1]))
          else htmltools::div(class = "wt-hint", "Not required for this class/type"),
          if (required) htmltools::div(class = "wt-stream-out", paste(unlist(s$outputs), collapse = " \u00b7 "))
        )
      }))
    })

    output$waterfall <- echarts4r::renderEcharts4r({
      o <- opp(); st <- ctx()$streams
      shiny::validate(shiny::need(!is.na(o$realizable_bopd), "Realizable potential not yet defined"))
      st <- st[!is.na(st$constraint_bopd) & st$constraint_bopd > 0, ]
      sn <- wt_streams(cfg)
      lab <- c("Reservoir", sn$name[match(st$stream_id, sn$id)], "Realizable")
      delta <- c(o$reservoir_bopd, -st$constraint_bopd, NA)
      level <- cumsum(c(o$reservoir_bopd, -st$constraint_bopd))
      base <- c(0, level[-1], 0)
      val <- c(o$reservoir_bopd, st$constraint_bopd, o$realizable_bopd)
      df <- data.frame(step = factor(lab, levels = lab), base = base, value = val)
      df |>
        echarts4r::e_charts(step) |>
        echarts4r::e_bar(base, stack = "w", itemStyle = list(color = "transparent"), name = "base", tooltip = list(show = FALSE)) |>
        echarts4r::e_bar(value, stack = "w", name = "BOPD", color = "#1f4e79",
                         label = list(show = TRUE, position = "top", fontSize = 9)) |>
        echarts4r::e_legend(show = FALSE) |>
        echarts4r::e_tooltip() |>
        echarts4r::e_grid(left = 40, right = 10, top = 20, bottom = 55) |>
        echarts4r::e_x_axis(axisLabel = list(fontSize = 9, rotate = 30, interval = 0))
    })

    output$wpa <- shiny::renderUI({
      w <- status()$wpa
      if (length(w)) wt_callout("WPA by exception: meeting required", type = "warn", htmltools::tags$ul(lapply(w, htmltools::tags$li)))
      else wt_callout("WPA: asynchronous", type = "ok", "Evidence complete and consistent; no meeting needed.")
    })

    output$stream_edit <- shiny::renderUI({
      req <- wt_required_streams(opp(), cfg)
      sn <- wt_streams(cfg)
      sn <- sn[sn$id %in% req, ]
      editable <- sn$id[mapply(function(d) wt_can_edit_item(app$user(), d, "edit_stream", cfg), sn$discipline)]
      if (!length(editable) || !opp()$step_id %in% c("S2", "S3")) return(NULL)
      htmltools::div(class = "wt-editbar", htmltools::span(class = "wt-live", "LIVE"), "Update my stream:",
        shiny::selectInput(ns("stream_sel"), NULL, stats::setNames(editable, sn$name[match(editable, sn$id)]), width = "200px"),
        shiny::selectInput(ns("stream_status"), NULL, unlist(cfg$stream_statuses), selected = "complete", width = "150px"),
        shiny::actionButton(ns("stream_save"), "Save", class = "btn-sm btn-primary"))
    })
    shiny::observeEvent(input$stream_save, {
      wt_db_set_item_status(app$con, "stream", opp()$opp_id, input$stream_sel, input$stream_status)
      app$bump()
      shiny::showNotification("Stream updated - gate readiness recalculated", type = "message")
    })

    # --- value & investment ------------------------------------------------------
    output$value <- shiny::renderUI({
      o <- opp()
      consistent <- !is.na(o$econ_basis_bopd) && !is.na(o$realizable_bopd) && abs(o$econ_basis_bopd - o$realizable_bopd) < 1
      htmltools::tagList(
        bslib::layout_column_wrap(width = 1 / 6,
          wt_kpi("NPV", wt_fmt(o$npv_kusd, " kUSD"), status = if (is.na(o$npv_kusd)) "neutral" else if (o$npv_kusd > 0) "ok" else "bad"),
          wt_kpi("IRR", wt_fmt(o$irr_pct, "%")),
          wt_kpi("Payout", wt_fmt(o$payout_months, " months", 1)),
          wt_kpi("Cost", wt_fmt(o$cost_kusd, " kUSD"), if (!is.na(o$cost_uncertainty_pct)) paste0("\u00b1", o$cost_uncertainty_pct, "%")),
          wt_kpi("Incremental", wt_fmt(o$incremental_bopd, " bopd")),
          wt_kpi("Value at risk", wt_fmt(if (is.na(o$cost_uncertainty_pct)) NA else o$cost_kusd * o$cost_uncertainty_pct / 100, " kUSD"))
        ),
        if (is.na(o$npv_kusd)) wt_callout("Economic case not yet built", type = "neutral",
                                          "Built in Stage 3 from the realizable potential approved in technical assurance.")
        else if (consistent) wt_callout("Consistency check passed", type = "ok",
          sprintf("Economics use realizable potential %s bopd (baseline %s).", o$realizable_bopd, o$baseline_version %||% "draft"))
        else wt_callout("Economic case must be refreshed", type = "bad",
          sprintf("Economics use %s bopd but current realizable potential is %s bopd. Flagged automatically.", o$econ_basis_bopd, o$realizable_bopd)),
        htmltools::div(class = "wt-section", "Investment inputs"),
        htmltools::tags$table(class = "wt-kv",
          htmltools::tags$tr(htmltools::tags$th("Production profile basis"), htmltools::tags$td(wt_fmt(o$econ_basis_bopd, " bopd"))),
          htmltools::tags$tr(htmltools::tags$th("Intervention scope"), htmltools::tags$td(o$hypothesis)),
          htmltools::tags$tr(htmltools::tags$th("Expected execution window"), htmltools::tags$td(if (is.na(o$planned_start)) "\u2013" else format(o$planned_start))),
          htmltools::tags$tr(htmltools::tags$th("Decision authority (D2)"), htmltools::tags$td(cfg$decisions$D2$authority[[o$class]])))
      )
    })

    # --- gates -------------------------------------------------------------------
    output$gates <- shiny::renderUI({
      c0 <- ctx(); o <- c0$opp
      can_edit <- wt_can(app$user(), "edit_case", cfg)
      links <- wt_case_analyses(c0, cfg)
      cards <- lapply(names(cfg$decisions), function(g) {
        ev <- wt_evaluate_gate(c0, g, cfg)
        dec <- c0$decisions[c0$decisions$gate == g & !c0$decisions$superseded, , drop = FALSE]
        gate_open <- identical(o$step_id, g)
        bslib::card(class = if (gate_open) "wt-gate-open",
          bslib::card_header(htmltools::strong(cfg$steps[[match(g, wt_steps(cfg)$id)]]$name),
                             if (nrow(dec)) wt_status_chip(dec$outcome[1]) else if (gate_open) wt_badge("PENDING", "warn"),
                             htmltools::span(class = "wt-hint", "Authority: ", cfg$decisions[[g]]$authority[[o$class]])),
          wt_progress(wt_gate_readiness(ev)),
          htmltools::tags$table(class = "wt-crit", lapply(seq_len(nrow(ev)), function(i) {
            r <- ev[i, ]
            toggle <- if (r$mode != "auto" && can_edit && !nrow(dec)) htmltools::tags$button(
              class = "btn btn-xs wt-crit-btn",
              onclick = sprintf("Shiny.setInputValue('%s', {gate:'%s', id:'%s', met:%s, n:Math.random()})",
                                ns("crit_toggle"), g, r$id, tolower(!r$confirmed)),
              if (r$confirmed) "undo" else if (r$status == "suggested") "confirm" else "mark met")
            ln <- links[links$gate == g & links$criterion_id == r$id, , drop = FALSE]
            analyze <- if (nrow(ln)) htmltools::tags$button(
              class = "btn btn-xs wt-crit-btn wt-analyze", title = paste("Analyses:", paste(ln$name, collapse = ", ")),
              onclick = sprintf("Shiny.setInputValue('%s', {a:'%s', c:'%s', g:'%s', go:true, n:Math.random()})",
                                ns("an_pick"), ln$analysis_id[1], r$id, g),
              shiny::icon(if (any(ln$evidence_status == "submitted")) "paperclip" else "chart-line"), "Analyze")
            htmltools::tags$tr(htmltools::tags$td(wt_status_chip(r$status)), htmltools::tags$td(r$label),
                               htmltools::tags$td(wt_mode_icon(r$mode)), htmltools::tags$td(analyze, toggle))
          })),
          if (nrow(dec)) htmltools::div(class = "wt-decision-rec",
            htmltools::strong(dec$outcome[1]), " by ", dec$decided_by[1], " on ", format(dec$decided_at[1], "%Y-%m-%d"),
            htmltools::br(), htmltools::em(dec$rationale[1]),
            if (nzchar(dec$conditions[1] %||% "")) htmltools::div(class = "wt-hint", "Conditions: ", dec$conditions[1]),
            htmltools::div(class = "wt-hint", "Evidence/baseline: ", dec$baseline_version[1]))
          else if (gate_open) shiny::actionButton(ns("goto_decide"), "Open decision package", class = "btn-sm btn-warning")
        )
      })
      do.call(bslib::layout_column_wrap, c(list(width = 1 / 3), cards))
    })
    shiny::observeEvent(input$crit_toggle, {
      x <- input$crit_toggle
      wt_db_set_gate_check(app$con, opp()$opp_id, x$gate, x$id, isTRUE(x$met), app$user()$user)
      app$bump()
    })
    shiny::observeEvent(input$goto_decide, app$open_tab("decisions"))

    # --- analysis workbench ----------------------------------------------------------
    # One sub-tab hosts every analysis; the list comes from the process config
    # (criteria `analyses:`) and the catalog, so new analyses add no tabs.
    registry <- wt_analysis_registry()
    an_results <- stats::setNames(lapply(names(registry), function(a) registry[[a]]$server(paste0("an_", a), ctx)),
                                  names(registry))
    an_links <- shiny::reactive(wt_case_analyses(ctx(), cfg))
    an_sel <- shiny::reactiveVal(NULL)

    shiny::observeEvent(an_links(), {
      l <- an_links(); cur <- an_sel()
      keep <- !is.null(cur) && any(l$analysis_id == cur$a & l$criterion_id == cur$c)
      if (!keep) an_sel(if (nrow(l)) list(a = l$analysis_id[1], c = l$criterion_id[1], g = l$gate[1]) else NULL)
    })
    shiny::observeEvent(input$an_pick, {
      x <- input$an_pick
      an_sel(list(a = x$a, c = x$c, g = x$g))
      if (isTRUE(x$go)) bslib::nav_select("tabs", "analysis", session = session)
    })

    output$an_list <- shiny::renderUI({
      l <- an_links(); cur <- an_sel()
      if (!nrow(l)) return(wt_callout("No analyses configured", type = "neutral", "No gate criterion of this process references an analysis for this class."))
      htmltools::tagList(
        htmltools::div(class = "wt-section", "Analyses for this case"),
        lapply(seq_len(nrow(l)), function(i) {
          active <- !is.null(cur) && cur$a == l$analysis_id[i] && cur$c == l$criterion_id[i]
          htmltools::div(
            class = paste("wt-an-item", if (active) "active", if (l$current_gate[i]) "wt-an-current"),
            onclick = sprintf("Shiny.setInputValue('%s', {a:'%s', c:'%s', g:'%s', n:Math.random()})",
                              ns("an_pick"), l$analysis_id[i], l$criterion_id[i], l$gate[i]),
            htmltools::div(class = "wt-an-name", shiny::icon(if (l$kind[i] == "module") "chart-line" else "up-right-from-square"), l$name[i]),
            htmltools::div(class = "wt-an-crit", wt_badge(l$gate[i], if (l$current_gate[i]) "warn" else "neutral"), l$criterion[i]),
            htmltools::div(if (l$evidence_status[i] == "submitted")
              htmltools::span(class = "wt-an-ev", shiny::icon("paperclip"), "evidence ", format(l$evidence_at[i], "%Y-%m-%d"))
              else htmltools::span(class = "wt-hint", "no evidence yet")))
        }),
        htmltools::div(class = "wt-hint", style = "margin-top:6px", "Current gate first. Analyses come from the shared catalog (analyses.yml).")
      )
    })

    output$an_workspace <- shiny::renderUI({
      cur <- an_sel(); shiny::req(cur)
      a <- cfg$analyses[[cur$a]]
      head <- htmltools::div(class = "wt-section", a$name, htmltools::span(class = "wt-hint", paste0("v", a$version, " \u00b7 ", a$kind)))
      if (a$kind == "module" && cur$a %in% names(registry)) {
        return(htmltools::tagList(head, htmltools::div(class = "wt-hint", a$description), registry[[cur$a]]$ui(ns(paste0("an_", cur$a)))))
      }
      link <- wt_analysis_link(a, opp()$opp_id, cur$c)
      htmltools::tagList(head,
        wt_callout("Runs in a separate application", type = "info", a$description,
          htmltools::div(style = "margin-top:6px",
            htmltools::tags$a(class = "btn btn-sm btn-primary", href = link, target = "_blank", shiny::icon("up-right-from-square"), " Open ", a$name)),
          htmltools::div(class = "wt-hint", style = "margin-top:4px", "Deep link carries case and criterion: ", htmltools::code(link)),
          htmltools::div(class = "wt-hint", "The app writes its evidence record back to WellTrack; it then appears here and in the gate.")),
        wt_callout("Mockup", type = "neutral", "The external app is a placeholder: the link target does not exist yet."))
    })

    output$an_attach <- shiny::renderUI({
      cur <- an_sel(); shiny::req(cur)
      a <- cfg$analyses[[cur$a]]
      crit <- an_links()[an_links()$analysis_id == cur$a & an_links()$criterion_id == cur$c, , drop = FALSE]
      ev <- ctx()$evidence
      ev <- if (is.null(ev)) NULL else ev[ev$analysis_id == cur$a & ev$criterion_id == cur$c & ev$status == "submitted", , drop = FALSE]
      labels <- stats::setNames(vapply(a$outputs, `[[`, "", "label"), vapply(a$outputs, `[[`, "", "id"))
      units <- stats::setNames(vapply(a$outputs, function(x) x$unit %||% "", ""), vapply(a$outputs, `[[`, "", "id"))
      res <- if (a$kind == "module" && cur$a %in% names(an_results)) an_results[[cur$a]]() else NULL
      can_run <- wt_can_run_analysis(app$user(), a, cfg)
      htmltools::tagList(
        htmltools::div(class = "wt-section", "Evidence for"),
        htmltools::div(class = "wt-an-crit", wt_badge(cur$g, "warn"), htmltools::strong(crit$criterion[1])),
        if (!is.null(res) && is.null(res$error)) htmltools::tagList(
          htmltools::tags$table(class = "wt-mini", lapply(names(res$outputs), function(k) htmltools::tags$tr(
            htmltools::tags$td(labels[[k]] %||% k), htmltools::tags$td(paste(format(res$outputs[[k]], big.mark = ","), units[[k]] %||% ""))))),
          htmltools::div(class = "wt-hint", "Data: ", res$data_ref),
          if (can_run) htmltools::div(style = "margin-top:6px", htmltools::span(class = "wt-live", "LIVE"),
            shiny::actionButton(ns("an_submit"), "Submit as evidence", icon = shiny::icon("paperclip"), class = "btn-sm btn-warning"))
          else htmltools::div(class = "wt-hint", "Submitting requires the integrator role or one of: ", paste(unlist(a$disciplines), collapse = ", "))),
        htmltools::div(class = "wt-section", "Attached evidence"),
        if (is.null(ev) || !nrow(ev)) htmltools::div(class = "wt-hint", "None yet.")
        else lapply(seq_len(nrow(ev)), function(i) htmltools::div(class = "wt-decision-rec",
          htmltools::strong(format(ev$created_at[i], "%Y-%m-%d")), " by ", ev$created_by[i], htmltools::br(),
          ev$summary[i], htmltools::div(class = "wt-hint", ev$data_ref[i], " \u00b7 ", ev$evidence_id[i])))
      )
    })

    shiny::observeEvent(input$an_submit, {
      cur <- an_sel(); shiny::req(cur)
      res <- an_results[[cur$a]]()
      shiny::req(is.null(res$error))
      rec <- wt_evidence_record(opp()$opp_id, cur$g, cur$c, cfg$analyses[[cur$a]], res, app$user()$user)
      wt_db_add_evidence(app$con, rec)
      app$bump()
      shiny::showNotification(sprintf("Evidence attached to %s \u2014 gate readiness recalculated", cur$g), type = "message")
    })

    # --- readiness ---------------------------------------------------------------
    output$readiness <- shiny::renderUI({
      o <- opp(); ws <- ctx()$workstreams
      if (!o$step_id %in% c("S4", "D3", "S5", "S6"))
        return(wt_callout("Preparation starts after D2 - GO", type = "neutral",
                          "Workstreams run in parallel once investment is approved. Applicable workstreams for this case: ",
                          paste(wt_workstreams(cfg)$name[match(wt_applicable_workstreams(o, cfg), wt_workstreams(cfg)$id)], collapse = ", ")))
      wn <- wt_workstreams(cfg)
      pct <- if (nrow(ws)) round(100 * mean(ws$status == "complete")) else 0
      wrr <- o$planned_start + cfg$wrr_offset_days
      htmltools::tagList(
        bslib::layout_column_wrap(width = 1 / 4,
          wt_kpi("RTE readiness", paste0(pct, "%"), sprintf("%d / %d workstreams", sum(ws$status == "complete"), nrow(ws)),
                 if (pct == 100) "ok" else "warn"),
          wt_kpi("Planned start", format(o$planned_start)),
          wt_kpi("WRR (T", paste0(cfg$wrr_offset_days, ")"), format(wrr), cfg$complexity_classes[[o$class]]$wrr),
          wt_kpi("Blocked", sum(ws$status == "blocked"), NULL, if (any(ws$status == "blocked")) "bad" else "ok")),
        htmltools::tags$table(class = "wt-table",
          htmltools::tags$thead(htmltools::tags$tr(lapply(c("Workstream", "Discipline", "Owner", "Due", "Status", "Note"), htmltools::tags$th))),
          htmltools::tags$tbody(lapply(seq_len(nrow(ws)), function(i) {
            late <- ws$status[i] != "complete" && ws$due_date[i] < as.Date(app$now)
            htmltools::tags$tr(
              htmltools::tags$td(wn$name[match(ws$ws_id[i], wn$id)]), htmltools::tags$td(wn$discipline[match(ws$ws_id[i], wn$id)]),
              htmltools::tags$td(ws$owner[i]), htmltools::tags$td(class = if (late) "wt-late", format(ws$due_date[i])),
              htmltools::tags$td(wt_status_chip(ws$status[i])), htmltools::tags$td(ws$note[i]))
          })))
      )
    })
    output$ws_edit <- shiny::renderUI({
      ws <- ctx()$workstreams
      if (!nrow(ws) || opp()$step_id != "S4") return(NULL)
      wn <- wt_workstreams(cfg)
      editable <- ws$ws_id[mapply(function(d) wt_can_edit_item(app$user(), d, "edit_workstream", cfg), wn$discipline[match(ws$ws_id, wn$id)])]
      if (!length(editable)) return(NULL)
      htmltools::div(class = "wt-editbar", htmltools::span(class = "wt-live", "LIVE"), "Update workstream:",
        shiny::selectInput(ns("ws_sel"), NULL, stats::setNames(editable, wn$name[match(editable, wn$id)]), width = "220px"),
        shiny::selectInput(ns("ws_status"), NULL, c("not_started", "in_progress", "blocked", "complete"), selected = "complete", width = "150px"),
        shiny::actionButton(ns("ws_save"), "Save", class = "btn-sm btn-primary"))
    })
    shiny::observeEvent(input$ws_save, {
      wt_db_set_item_status(app$con, "workstream", opp()$opp_id, input$ws_sel, input$ws_status)
      app$bump()
    })

    # --- execution & value ---------------------------------------------------------
    output$baseline <- echarts4r::renderEcharts4r({
      o <- opp()
      shiny::validate(shiny::need(!is.na(o$actual_bopd), "Available after execution (Stage 5)"))
      df <- data.frame(metric = c("Oil rate", "Fluid rate", "Cost", "Duration"),
                       baseline = c(o$realizable_bopd, o$promised_bfpd, o$cost_kusd, o$planned_duration_d),
                       actual = c(o$actual_bopd, o$actual_bfpd, o$actual_cost_kusd, o$actual_duration_d))
      # Indexed to the approved D2 baseline (= 100%) so all measures share one axis
      df$pct <- round(100 * df$actual / df$baseline)
      df$label <- sprintf("%s: actual %s vs baseline %s", df$metric, df$actual, df$baseline)
      df |>
        echarts4r::e_charts(metric) |>
        echarts4r::e_bar(pct, name = "Actual as % of D2 baseline", color = "#2a78d6", barMaxWidth = 26, bind = label,
                         itemStyle = list(borderRadius = c(4, 4, 0, 0)), label = list(show = TRUE, position = "top", fontSize = 9, formatter = "{c}%")) |>
        echarts4r::e_mark_line(data = list(yAxis = 100), symbol = "none", lineStyle = list(color = "#52514e", type = "dashed"),
                               label = list(formatter = "baseline", fontSize = 9)) |>
        echarts4r::e_tooltip(formatter = htmlwidgets::JS("function(p){return p.name;}")) |>
        echarts4r::e_legend(show = FALSE) |>
        echarts4r::e_title("Actual vs approved baseline (D2 = 100%)", textStyle = list(fontSize = 11, color = "#52514e")) |>
        echarts4r::e_grid(left = 40, right = 50, top = 35, bottom = 25) |>
        echarts4r::e_x_axis(axisLabel = list(fontSize = 9, interval = 0))
    })
    output$execution <- shiny::renderUI({
      o <- opp()
      if (is.na(o$actual_bopd)) return(wt_callout("Not executed yet", type = "neutral",
        "Execution captures actual start/end, scope, NPT, deviations, cost and initial response against the D2 baseline."))
      real <- round(100 * o$actual_bopd / o$realizable_bopd)
      htmltools::tagList(
        bslib::layout_column_wrap(width = 1 / 3,
          wt_kpi("Oil realization", paste0(real, "%"), "actual / promised", if (real >= cfg$kpis$realization_target_pct) "ok" else "bad"),
          wt_kpi("Cost ratio", sprintf("%.2f", o$actual_cost_kusd / o$cost_kusd), "actual / theoretical",
                 if (o$actual_cost_kusd / o$cost_kusd <= 1.1) "ok" else "warn"),
          wt_kpi("Energy ratio", sprintf("%.2f", o$energy_ratio), "actual / theoretical"),
          wt_kpi("NPT", wt_fmt(o$npt_hours, " h")),
          wt_kpi("Duration", paste0(o$actual_duration_d, " / ", o$planned_duration_d, " d")),
          wt_kpi("Window", paste(format(o$actual_start, "%d-%b"), "\u2192", format(o$actual_end, "%d-%b")))),
        if (!is.na(o$lesson)) wt_callout("Lesson learned (feeds future opportunities)", type = "info", o$lesson)
        else wt_callout("Value review pending", type = "warn", "Post-audit compares the D2 baseline with actuals; record variance, root cause and lessons.")
      )
    })

    # --- risks & changes -------------------------------------------------------------
    output$risks <- DT::renderDT({
      r <- ctx()$risks
      shiny::validate(shiny::need(nrow(r) > 0, "No risks registered"))
      shown <- data.frame(Category = r$category, Risk = r$description, P = r$probability, C = r$consequence,
                          Score = wt_risk_score(r$probability, r$consequence), Mitigation = r$mitigation, Owner = r$owner,
                          Due = format(r$due_date), Status = r$status)
      DT::datatable(shown, rownames = FALSE, class = "compact wt-dt", options = list(dom = "t", ordering = FALSE)) |>
        DT::formatStyle("Score", backgroundColor = DT::styleInterval(c(9, 14), c("#e8f5e9", "#fff3e0", "#ffebee")))
    })
    output$changes <- DT::renderDT({
      ch <- ctx()$changes
      shiny::validate(shiny::need(!is.null(ch) && nrow(ch) > 0, "No changes since D2"))
      DT::datatable(data.frame(Change = ch$description, Materiality = ch$materiality, Status = ch$status,
                               Raised = format(ch$raised_at, "%Y-%m-%d")),
                    rownames = FALSE, class = "compact wt-dt", options = list(dom = "t"))
    })

    # --- history -------------------------------------------------------------------
    output$history <- shiny::renderUI({
      c0 <- ctx()
      steps <- wt_steps(cfg)
      h <- c0$history
      ev <- rbind(
        data.frame(ts = h$entered_at, what = paste0("Entered ", steps$name[match(h$step_id, steps$id)], ifelse(h$recycle, " (recycle)", "")),
                   who = "", type = "step"),
        if (nrow(c0$decisions)) data.frame(ts = c0$decisions$decided_at,
                                            what = paste0(c0$decisions$gate, " decision: ", c0$decisions$outcome, " \u2014 ", c0$decisions$rationale),
                                            who = c0$decisions$decided_by, type = "decision"),
        if (!is.null(c0$gate_checks) && nrow(c0$gate_checks)) data.frame(ts = c0$gate_checks$at,
                                            what = paste0(c0$gate_checks$gate, " evidence confirmed: ", c0$gate_checks$criterion_id),
                                            who = c0$gate_checks$by_user, type = "evidence"))
      ev <- ev[order(ev$ts, decreasing = TRUE), ]
      htmltools::div(class = "wt-timeline", lapply(seq_len(nrow(ev)), function(i)
        htmltools::div(class = paste("wt-tl", paste0("wt-tl-", ev$type[i])),
          htmltools::span(class = "wt-tl-ts", format(ev$ts[i], "%Y-%m-%d %H:%M")),
          htmltools::span(ev$what[i]), if (nzchar(ev$who[i])) htmltools::span(class = "wt-hint", " \u00b7 ", ev$who[i]))))
    })
  })
}
