#' Process Stats tab - server
#' @param id Module id.
#' @param app Shared app context.
#' @keywords internal
tab_stats_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    cfg <- app$cfg
    col <- list(s1 = "#2a78d6", ink = "#52514e", grid = "#e6e8eb", ok = "#2e7d32", warn = "#ed8c00", bad = "#c62828")
    axis_style <- function(e) {
      e |>
        echarts4r::e_x_axis(axisLabel = list(fontSize = 9, color = col$ink, interval = 0), splitLine = list(show = FALSE)) |>
        echarts4r::e_y_axis(axisLabel = list(fontSize = 9, color = col$ink), splitLine = list(lineStyle = list(color = col$grid)))
    }

    opps <- shiny::reactive({
      o <- app$data()$opportunity
      if (nzchar(input$type %||% "")) o <- o[o$intervention_type == input$type, ]
      o
    })
    dur <- shiny::reactive({
      o <- opps()
      h <- app$data()$step_history
      wt_step_durations(h[h$opp_id %in% o$opp_id, ], o, cfg, app$now)
    })

    output$kpis <- shiny::renderUI({
      d <- dur()
      lt <- wt_lead_times(app$data()$step_history[app$data()$step_history$opp_id %in% opps()$opp_id, ])
      terminal <- wt_states(cfg)$id[wt_states(cfg)$terminal]
      open <- d[d$open & !d$state %in% terminal, ]
      bslib::layout_column_wrap(width = 1 / 3, class = "wt-kpi-row",
        wt_kpi("Median lead time", wt_fmt(stats::median(lt$lead_time, na.rm = TRUE), " d"), "opportunity \u2192 executed"),
        wt_kpi("Active cases", nrow(open)),
        wt_kpi("Over target time", paste0(if (nrow(open)) round(100 * mean(open$over_sla)) else 0, "%"),
               "of active cases in their current step", if (nrow(open) && mean(open$over_sla) > .3) "warn" else "ok")
      )
    })

    output$step_time <- echarts4r::renderEcharts4r({
      s <- wt_step_stats(dur(), "step_id")
      shiny::validate(shiny::need(nrow(s) > 0, "No completed steps"))
      s$step <- wt_steps(cfg)$short[match(s$step_id, wt_steps(cfg)$id)]
      s |>
        echarts4r::e_charts(step) |>
        echarts4r::e_bar(median_days, name = "Median days", color = col$s1, barMaxWidth = 22,
                         itemStyle = list(borderRadius = c(4, 4, 0, 0))) |>
        echarts4r::e_scatter(sla_days, name = "Target", symbol = "rect", symbol_size = c(26, 3), color = col$ink) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_legend(bottom = 0, textStyle = list(fontSize = 10)) |>
        echarts4r::e_grid(left = 35, right = 10, top = 15, bottom = 45) |>
        axis_style()
    })

    output$wip <- echarts4r::renderEcharts4r({
      d <- dur()
      d <- d[d$open & !d$state %in% wt_states(cfg)$id[wt_states(cfg)$terminal], ]
      shiny::validate(shiny::need(nrow(d) > 0, "No active cases"))
      steps <- wt_steps(cfg)
      d$bucket <- ifelse(d$over_sla, "Over target", "Within target")
      tab <- as.data.frame.matrix(table(factor(d$step_id, levels = steps$id), factor(d$bucket, levels = c("Within target", "Over target"))))
      tab$step <- steps$short
      tab |>
        echarts4r::e_charts(step) |>
        echarts4r::e_bar(`Within target`, stack = "w", color = col$ok, barMaxWidth = 22, itemStyle = list(borderColor = "#fff", borderWidth = 1)) |>
        echarts4r::e_bar(`Over target`, stack = "w", color = col$bad, barMaxWidth = 22, itemStyle = list(borderColor = "#fff", borderWidth = 1)) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_legend(bottom = 0, textStyle = list(fontSize = 10)) |>
        echarts4r::e_grid(left = 30, right = 10, top = 15, bottom = 60) |>
        axis_style() |>
        echarts4r::e_x_axis(axisLabel = list(rotate = 35, fontSize = 9, interval = 0))
    })
  })
}
