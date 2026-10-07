#' My Work tab - server
#' @param id Module id.
#' @param app Shared app context (see [app_server()]).
#' @keywords internal
tab_mywork_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    cfg <- app$cfg

    work <- shiny::reactive(wt_my_work(app$data(), app$summary(), app$user(), cfg, app$now))

    output$hello <- shiny::renderUI({
      u <- app$user()
      htmltools::div(class = "wt-hello",
        htmltools::strong("Good morning, ", u$name), " \u2014 ",
        paste(vapply(u$roles, function(r) cfg$roles[[r]]$name, ""), collapse = " / "),
        if (nzchar(u$discipline)) paste0(" \u00b7 ", u$discipline),
        if (nzchar(u$authority)) paste0(" \u00b7 authority: ", u$authority))
    })

    output$kpis <- shiny::renderUI({
      w <- work(); s <- app$summary()
      act <- s[!s$terminal, ]
      bslib::layout_column_wrap(width = 1 / 5, class = "wt-kpi-row",
        wt_kpi("My open items", nrow(w), "assigned to me", if (nrow(w)) "warn" else "ok"),
        wt_kpi("High priority", sum(w$priority == "High"), "conflicts, overdue, over SLA", if (any(w$priority == "High")) "bad" else "ok"),
        wt_kpi("Decisions awaiting me", sum(w$kind == "Decision"), "within my authority", "neutral"),
        wt_kpi("Active cases", nrow(act), sprintf("%s over SLA", sum(act$over_sla)), if (mean(act$over_sla) > .3) "warn" else "ok"),
        wt_kpi("Ready for a decision", sum(act$readiness == 100 & act$step_id %in% names(cfg$decisions), na.rm = TRUE),
               "evidence 100% complete", "ok")
      )
    })

    output$inbox <- DT::renderDT({
      w <- work()
      shown <- data.frame(
        Priority = w$priority, Type = w$kind, Case = w$opp_id, Well = w$well, Item = w$item,
        Due = format(w$due), `Age (d)` = w$age_days, `Next action` = w$action, check.names = FALSE)
      DT::datatable(shown, rownames = FALSE, selection = "single", class = "compact hover wt-dt",
                    options = list(dom = "ftp", pageLength = 15, ordering = FALSE,
                                   language = list(emptyTable = "Nothing needs your action. \u2714"))) |>
        DT::formatStyle("Priority", color = DT::styleEqual(c("High", "Medium", "Low"), c("#c62828", "#ed8c00", "#5b6770")),
                        fontWeight = "bold")
    })

    shiny::observeEvent(input$inbox_rows_selected, {
      app$open_opp(work()$opp_id[input$inbox_rows_selected])
    })

    output$exceptions <- shiny::renderUI({
      d <- app$data(); s <- app$summary()
      o <- d$opportunity
      act <- s[!s$terminal, ]
      over <- act[act$over_sla, ]
      over <- over[order(-(over$days_in_step - over$sla_days)), ][seq_len(min(3, nrow(over))), ]
      conflicts <- unique(d$stream_status$opp_id[d$stream_status$status == "conflict" & d$stream_status$opp_id %in% act$opp_id])
      refresh <- o$opp_id[o$opp_id %in% act$opp_id & !is.na(o$econ_basis_bopd) & !is.na(o$realizable_bopd) &
                            abs(o$econ_basis_bopd - o$realizable_bopd) >= 1]
      blocked <- unique(d$workstream_status$opp_id[d$workstream_status$status == "blocked" & d$workstream_status$opp_id %in% act$opp_id])
      w <- function(ids) paste(o$well[match(ids, o$opp_id)], collapse = ", ")
      htmltools::tagList(
        if (nrow(over)) wt_callout("Longest SLA breaches", type = "bad",
          htmltools::tags$ul(lapply(seq_len(nrow(over)), function(i)
            htmltools::tags$li(htmltools::strong(o$well[match(over$opp_id[i], o$opp_id)]),
                               sprintf(" %s: %s d vs %s d SLA", over$step_id[i], over$days_in_step[i], over$sla_days[i]))))),
        if (length(conflicts)) wt_callout(sprintf("%d discipline conflicts", length(conflicts)), type = "warn", w(conflicts),
                                          htmltools::div(class = "wt-hint", "WPA meeting triggered by exception")),
        if (length(refresh)) wt_callout(sprintf("%d economic cases need refresh", length(refresh)), type = "warn", w(refresh),
                                        htmltools::div(class = "wt-hint", "Economics not using current realizable potential")),
        if (length(blocked)) wt_callout(sprintf("%d cases with blocked readiness", length(blocked)), type = "info", w(blocked))
      )
    })

    output$funnel <- echarts4r::renderEcharts4r({
      s <- app$summary()
      steps <- wt_steps(cfg)
      act <- s[!s$terminal, ]
      df <- data.frame(step = factor(steps$short, levels = steps$short),
                       n = as.numeric(table(factor(act$step_id, levels = steps$id))),
                       over = as.numeric(table(factor(act$step_id[act$over_sla], levels = steps$id))))
      df$within <- df$n - df$over
      df |>
        echarts4r::e_charts(step) |>
        echarts4r::e_bar(within, name = "Within SLA", stack = "a", color = "#1f4e79") |>
        echarts4r::e_bar(over, name = "Over SLA", stack = "a", color = "#c62828") |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_legend(bottom = 0, textStyle = list(fontSize = 10)) |>
        echarts4r::e_grid(left = 30, right = 10, top = 10, bottom = 70) |>
        echarts4r::e_x_axis(axisLabel = list(fontSize = 9, rotate = 35, interval = 0))
    })
  })
}
