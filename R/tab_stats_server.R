#' Process Stats tab - server
#' @param id Module id.
#' @param app Shared app context.
#' @keywords internal
tab_stats_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    cfg <- app$cfg
    col <- list(s1 = "#2a78d6", s2 = "#eb6834", ink = "#52514e", grid = "#e6e8eb",
                ok = "#2e7d32", warn = "#ed8c00", bad = "#c62828")
    axis_style <- function(e) {
      e |>
        echarts4r::e_x_axis(axisLabel = list(fontSize = 9, color = col$ink), splitLine = list(show = FALSE)) |>
        echarts4r::e_y_axis(axisLabel = list(fontSize = 9, color = col$ink), splitLine = list(lineStyle = list(color = col$grid)))
    }

    shiny::observe({
      f <- sort(unique(app$data()$opportunity$field))
      shiny::updateSelectInput(session, "field", choices = c("All" = "", f), selected = shiny::isolate(input$field))
    })

    opps <- shiny::reactive({
      o <- app$data()$opportunity
      if (nzchar(input$type %||% "")) o <- o[o$intervention_type == input$type, ]
      if (nzchar(input$class %||% "")) o <- o[o$class == input$class, ]
      if (nzchar(input$field %||% "")) o <- o[o$field == input$field, ]
      o
    })
    dur <- shiny::reactive({
      o <- opps()
      d <- wt_step_durations(app$data()$step_history[app$data()$step_history$opp_id %in% o$opp_id, ], o, cfg, app$now)
      since <- app$now - as.numeric(input$window) * 86400
      d[d$open | d$exited_at >= since, ]
    })
    step_stats <- shiny::reactive(wt_step_stats(dur(), "step_id"))
    lead <- shiny::reactive({
      lt <- wt_lead_times(app$data()$step_history[app$data()$step_history$opp_id %in% opps()$opp_id, ])
      since <- app$now - as.numeric(input$window) * 86400
      lt[is.na(lt$executed_at) | lt$executed_at >= since, ]
    })

    output$kpis <- shiny::renderUI({
      d <- app$data()
      d$opportunity <- opps()
      k <- wt_kpis(d, dur(), cfg)
      lt <- lead()
      target <- stats::median(unlist(cfg$kpis$lead_time_target_days[unique(opps()$class)]))
      bslib::layout_column_wrap(width = 1 / 7, class = "wt-kpi-row",
        wt_kpi("Median lead time", wt_fmt(stats::median(lt$lead_time, na.rm = TRUE), " d"),
               paste0("opportunity \u2192 executed \u00b7 target ", target, " d"),
               if (isTRUE(stats::median(lt$lead_time, na.rm = TRUE) <= target)) "ok" else "warn"),
        wt_kpi("Opp \u2192 D1", wt_fmt(stats::median(lt$opp_to_d1, na.rm = TRUE), " d"), "median"),
        wt_kpi("D1 \u2192 D2", wt_fmt(stats::median(lt$d1_to_d2, na.rm = TRUE), " d"), "median"),
        wt_kpi("D2 \u2192 RTE", wt_fmt(stats::median(lt$d2_to_rte, na.rm = TRUE), " d"), "median"),
        wt_kpi("Managed by exception", wt_fmt(k$exception_managed_pct, "%"), paste0("no WPA meeting \u00b7 target ", cfg$kpis$exception_managed_target_pct, "%"),
               if (isTRUE(k$exception_managed_pct >= cfg$kpis$exception_managed_target_pct)) "ok" else "warn"),
        wt_kpi("Value realization", wt_fmt(k$realization_pct, "%"), paste0("actual / promised oil \u00b7 target ", cfg$kpis$realization_target_pct, "%"),
               if (isTRUE(k$realization_pct >= cfg$kpis$realization_target_pct)) "ok" else "bad"),
        wt_kpi("Deferred oil (delays)", wt_fmt(k$deferred_bo / 1000, " kbbl", 1), sprintf("%d open steps over SLA", k$over_sla_open),
               if (k$deferred_bo > 0) "bad" else "ok")
      )
    })

    output$step_time <- echarts4r::renderEcharts4r({
      s <- step_stats()
      shiny::validate(shiny::need(nrow(s) > 0, "No completed steps in period"))
      s$step <- wt_steps(cfg)$short[match(s$step_id, wt_steps(cfg)$id)]
      s |>
        echarts4r::e_charts(step) |>
        echarts4r::e_bar(median_days, name = "Median days", color = col$s1, barMaxWidth = 22,
                         itemStyle = list(borderRadius = c(4, 4, 0, 0))) |>
        echarts4r::e_scatter(sla_days, name = "SLA target", symbol = "rect", symbol_size = c(26, 3), color = col$ink) |>
        echarts4r::e_line(p80_days, name = "P80", color = col$s2, lineStyle = list(width = 2), symbol = "circle", symbolSize = 8) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_legend(bottom = 0, textStyle = list(fontSize = 10)) |>
        echarts4r::e_grid(left = 35, right = 10, top = 15, bottom = 40) |>
        axis_style()
    })

    output$work_wait <- echarts4r::renderEcharts4r({
      s <- step_stats()
      shiny::validate(shiny::need(nrow(s) > 0, "No data"))
      s$step <- wt_steps(cfg)$short[match(s$step_id, wt_steps(cfg)$id)]
      s <- s[rev(seq_len(nrow(s))), ]
      s |>
        echarts4r::e_charts(step) |>
        echarts4r::e_bar(median_working, name = "Working", stack = "t", color = col$s1, barMaxWidth = 16,
                         itemStyle = list(borderColor = "#fff", borderWidth = 1)) |>
        echarts4r::e_bar(median_waiting, name = "Waiting", stack = "t", color = col$s2, barMaxWidth = 16,
                         itemStyle = list(borderColor = "#fff", borderWidth = 1)) |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_legend(bottom = 0, textStyle = list(fontSize = 10)) |>
        echarts4r::e_grid(left = 70, right = 15, top = 10, bottom = 40) |>
        echarts4r::e_x_axis(axisLabel = list(fontSize = 9)) |>
        echarts4r::e_y_axis(axisLabel = list(fontSize = 9))
    })

    output$segments <- echarts4r::renderEcharts4r({
      lt <- lead()
      seg <- c(opp_to_d1 = "Opp\u2192D1", d1_to_d2 = "D1\u2192D2", d2_to_rte = "D2\u2192RTE", rte_to_executed = "RTE\u2192Exec")
      long <- do.call(rbind, lapply(names(seg), function(k) data.frame(segment = seg[[k]], days = lt[[k]])))
      long <- long[!is.na(long$days), ]
      shiny::validate(shiny::need(nrow(long) > 0, "No data"))
      long$segment <- factor(long$segment, levels = seg)
      long |>
        dplyr::group_by(segment) |>
        echarts4r::e_charts() |>
        echarts4r::e_boxplot(days, outliers = TRUE, itemStyle = list(color = "#dbe8f7", borderColor = col$s1)) |>
        echarts4r::e_tooltip() |>
        echarts4r::e_legend(show = FALSE) |>
        echarts4r::e_grid(left = 35, right = 10, top = 15, bottom = 25) |>
        axis_style()
    })

    output$trend <- echarts4r::renderEcharts4r({
      lt <- lead()
      lt <- lt[!is.na(lt$lead_time), ]
      shiny::validate(shiny::need(nrow(lt) > 1, "Not enough executed cases"))
      lt$month <- format(lt$executed_at, "%Y-%m")
      m <- stats::aggregate(lead_time ~ month, lt, stats::median)
      m$n <- as.numeric(table(lt$month)[m$month])
      m |>
        echarts4r::e_charts(month) |>
        echarts4r::e_line(lead_time, name = "Median lead time (d)", color = col$s1, lineStyle = list(width = 2),
                          symbol = "circle", symbolSize = 8) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_legend(show = FALSE) |>
        echarts4r::e_grid(left = 35, right = 10, top = 15, bottom = 25) |>
        axis_style()
    })

    output$heat <- echarts4r::renderEcharts4r({
      s <- wt_step_stats(dur(), c("step_id", input$group_by))
      shiny::validate(shiny::need(nrow(s) > 0, "No data"))
      steps <- wt_steps(cfg)
      s$step <- factor(steps$short[match(s$step_id, steps$id)], levels = steps$short)
      s$group <- as.character(s[[input$group_by]])
      if (input$group_by == "intervention_type") s$group <- wt_type_labels(cfg)[s$group]
      s$pct <- round(100 * s$median_days / s$sla_days)
      s <- s[order(s$step), ]
      s |>
        echarts4r::e_charts(step) |>
        echarts4r::e_heatmap(group, pct, label = list(show = TRUE, fontSize = 9)) |>
        echarts4r::e_visual_map(pct, min = 0, max = 200, show = FALSE,
                                inRange = list(color = c(col$s1, "#e8e8e6", col$s2))) |>
        echarts4r::e_tooltip() |>
        echarts4r::e_grid(left = 80, right = 10, top = 10, bottom = 40) |>
        echarts4r::e_x_axis(axisLabel = list(fontSize = 9, rotate = 35, interval = 0)) |>
        echarts4r::e_y_axis(axisLabel = list(fontSize = 9))
    })

    output$wip <- echarts4r::renderEcharts4r({
      d <- dur()
      d <- d[d$open & !d$state %in% wt_states(cfg)$id[wt_states(cfg)$terminal], ]
      shiny::validate(shiny::need(nrow(d) > 0, "No open cases"))
      steps <- wt_steps(cfg)
      ratio <- d$days / d$sla_days
      d$bucket <- ifelse(ratio <= .75, "Within SLA", ifelse(ratio <= 1, "Near SLA (>75%)", "Over SLA"))
      tab <- as.data.frame.matrix(table(factor(d$step_id, levels = steps$id), factor(d$bucket, levels = c("Within SLA", "Near SLA (>75%)", "Over SLA"))))
      tab$step <- steps$short
      tab |>
        echarts4r::e_charts(step) |>
        echarts4r::e_bar(`Within SLA`, stack = "w", color = col$ok, barMaxWidth = 22, itemStyle = list(borderColor = "#fff", borderWidth = 1)) |>
        echarts4r::e_bar(`Near SLA (>75%)`, stack = "w", color = col$warn, barMaxWidth = 22, itemStyle = list(borderColor = "#fff", borderWidth = 1)) |>
        echarts4r::e_bar(`Over SLA`, stack = "w", color = col$bad, barMaxWidth = 22, itemStyle = list(borderColor = "#fff", borderWidth = 1)) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_legend(bottom = 0, textStyle = list(fontSize = 10)) |>
        echarts4r::e_grid(left = 30, right = 10, top = 15, bottom = 40) |>
        axis_style()
    })

    output$realization <- echarts4r::renderEcharts4r({
      o <- opps()
      o <- o[!is.na(o$actual_bopd), ]
      shiny::validate(shiny::need(nrow(o) > 0, "No executed cases"))
      mx <- max(c(o$realizable_bopd, o$actual_bopd)) * 1.05
      o$label <- o$well
      o |>
        echarts4r::e_charts(realizable_bopd) |>
        echarts4r::e_scatter(actual_bopd, name = "Executed case", symbol_size = 9, color = col$s1, bind = label) |>
        echarts4r::e_mark_line(data = list(list(coord = c(0, 0)), list(coord = c(mx, mx))),
                               lineStyle = list(color = col$ink, type = "dashed"), symbol = "none",
                               label = list(formatter = "actual = promised", fontSize = 9)) |>
        echarts4r::e_tooltip(formatter = htmlwidgets::JS("function(p){return p.name + '<br>promised ' + p.value[0] + ' / actual ' + p.value[1] + ' bopd';}")) |>
        echarts4r::e_legend(show = FALSE) |>
        echarts4r::e_x_axis(name = "promised", nameTextStyle = list(fontSize = 9), max = round(mx)) |>
        echarts4r::e_y_axis(name = "actual", nameTextStyle = list(fontSize = 9), max = round(mx)) |>
        echarts4r::e_grid(left = 40, right = 40, top = 25, bottom = 30) |>
        axis_style()
    })

    output$table <- DT::renderDT({
      s <- step_stats()
      s$step_id <- wt_steps(cfg)$name[match(s$step_id, wt_steps(cfg)$id)]
      names(s) <- c("Step", "N", "Median d", "P80 d", "Mean d", "Median working d", "Median waiting d", "SLA d", "% over SLA")
      DT::datatable(s, rownames = FALSE, class = "compact wt-dt", options = list(dom = "t", ordering = FALSE)) |>
        DT::formatStyle("% over SLA", color = DT::styleInterval(c(25, 50), c(col$ok, col$warn, col$bad)), fontWeight = "bold")
    })
  })
}
