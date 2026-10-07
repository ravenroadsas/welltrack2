#' Admin tab - server
#' @param id Module id.
#' @param app Shared app context.
#' @keywords internal
tab_admin_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    cfg <- app$cfg
    dt <- function(x, ...) DT::datatable(x, rownames = FALSE, class = "compact wt-dt",
                                         options = list(dom = "t", ordering = FALSE, pageLength = 50), ...)

    output$cfg_head <- shiny::renderUI({
      wt_callout(sprintf("Process '%s' \u00b7 config version %s", cfg$process$name, cfg$version), type = "info",
        "Steps, gates, criteria, classes, streams, workstreams, roles and thresholds are read from ",
        htmltools::code("inst/config/process.yml"), ". Items marked TBV need business validation (doc \u00a740). ",
        if (!wt_can(app$user(), "admin", cfg)) htmltools::strong("Read-only for your account."))
    })

    output$steps <- DT::renderDT({
      s <- wt_steps(cfg)
      s$sla <- vapply(s$id, function(i) paste(vapply(c("A", "B", "C"), function(k) wt_sla_days(cfg, i, k), 0), collapse = " / "), "")
      dt(s[, c("id", "kind", "name", "question", "owner_role", "sla")])
    })

    crit <- do.call(rbind, lapply(names(cfg$decisions), function(g) wt_gate_criteria(cfg, g)))
    output$automation <- echarts4r::renderEcharts4r({
      tab <- as.data.frame.matrix(table(crit$gate, factor(crit$mode, levels = c("auto", "assisted", "manual"))))
      tab$gate <- rownames(tab)
      tab |>
        echarts4r::e_charts(gate) |>
        echarts4r::e_bar(auto, name = "auto", stack = "m", color = "#2a78d6", itemStyle = list(borderColor = "#fff", borderWidth = 1)) |>
        echarts4r::e_bar(assisted, name = "assisted", stack = "m", color = "#1baf7a", itemStyle = list(borderColor = "#fff", borderWidth = 1)) |>
        echarts4r::e_bar(manual, name = "manual", stack = "m", color = "#eb6834", itemStyle = list(borderColor = "#fff", borderWidth = 1)) |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_legend(top = 0, right = 0, textStyle = list(fontSize = 10)) |>
        echarts4r::e_grid(left = 30, right = 10, top = 25, bottom = 15) |>
        echarts4r::e_x_axis(axisLabel = list(fontSize = 9)) |>
        echarts4r::e_y_axis(axisLabel = list(fontSize = 9))
    })
    output$criteria <- DT::renderDT(dt(crit[, c("gate", "label", "mode", "classes")]))

    output$classes <- DT::renderDT({
      cl <- cfg$complexity_classes
      dt(data.frame(Class = names(cl), Name = vapply(cl, `[[`, "", "name"),
                    `Required streams` = vapply(cl, function(x) paste(unlist(x$required_streams), collapse = ", "), ""),
                    WPA = vapply(cl, `[[`, "", "wpa_meeting"), WRR = vapply(cl, `[[`, "", "wrr"),
                    Approval = vapply(cl, `[[`, "", "approval"), check.names = FALSE))
    })

    output$streams <- DT::renderDT({
      s <- wt_streams(cfg); w <- wt_workstreams(cfg)
      dt(rbind(data.frame(Kind = "Assurance (S2)", Id = s$id, Name = s$name, Discipline = s$discipline),
               data.frame(Kind = "Readiness (S4)", Id = w$id, Name = w$name, Discipline = w$discipline)))
    })

    output$analyses <- DT::renderDT({
      a <- cfg$analyses
      used <- wt_criterion_analyses(cfg)
      dt(data.frame(
        Id = names(a), Name = vapply(a, `[[`, "", "name"), Kind = vapply(a, `[[`, "", "kind"),
        Version = vapply(a, function(x) x$version %||% "", ""),
        Outputs = vapply(a, function(x) paste(vapply(x$outputs, `[[`, "", "id"), collapse = ", "), ""),
        Disciplines = vapply(a, function(x) paste(unlist(x$disciplines), collapse = ", "), ""),
        `Used by criteria` = vapply(names(a), function(i) paste(paste0(used$gate, " ", used$criterion_id)[used$analysis_id == i], collapse = "; "), ""),
        check.names = FALSE))
    })

    output$roles <- DT::renderDT({
      perms <- unique(unlist(lapply(cfg$roles, `[[`, "permissions")))
      m <- t(vapply(cfg$roles, function(r) ifelse(perms %in% unlist(r$permissions), "\u2714", ""), character(length(perms))))
      colnames(m) <- perms
      dt(data.frame(Role = vapply(cfg$roles, `[[`, "", "name"), Description = vapply(cfg$roles, `[[`, "", "description"),
                    `Connect groups` = vapply(cfg$roles, function(r) paste(unlist(r$connect_groups), collapse = ", "), ""),
                    m, check.names = FALSE))
    })
    output$users <- DT::renderDT(dt(app$users))

    # --- process mining -------------------------------------------------------------
    log_version <- shiny::reactiveVal(0)
    shiny::observeEvent(input$refresh, log_version(log_version() + 1))
    eventlog <- shiny::reactive({
      log_version()
      wt_activity_eventlog(wt_db_read(app$con, "activity_log"))
    })

    output$phase_time <- echarts4r::renderEcharts4r({
      el <- eventlog()
      el$minutes <- as.numeric(difftime(el$end, el$start, units = "mins")) + 0.25
      agg <- stats::aggregate(minutes ~ activity, el, sum)
      agg <- agg[order(agg$minutes), ]
      agg$minutes <- round(agg$minutes)
      agg |>
        echarts4r::e_charts(activity) |>
        echarts4r::e_bar(minutes, name = "Minutes", color = "#2a78d6", barMaxWidth = 14,
                         label = list(show = TRUE, position = "right", fontSize = 9)) |>
        echarts4r::e_flip_coords() |>
        echarts4r::e_tooltip() |>
        echarts4r::e_legend(show = FALSE) |>
        echarts4r::e_grid(left = 130, right = 30, top = 5, bottom = 20) |>
        echarts4r::e_y_axis(axisLabel = list(fontSize = 9)) |>
        echarts4r::e_x_axis(axisLabel = list(fontSize = 9))
    })

    output$dfg <- echarts4r::renderEcharts4r({
      tr <- wt_phase_transitions(eventlog())
      shiny::validate(shiny::need(nrow(tr) > 0, "No transitions yet"))
      tr |>
        echarts4r::e_charts(to) |>
        echarts4r::e_heatmap(from, n, label = list(show = TRUE, fontSize = 9)) |>
        echarts4r::e_visual_map(n, show = FALSE, inRange = list(color = c("#eef4fb", "#2a78d6"))) |>
        echarts4r::e_tooltip() |>
        echarts4r::e_grid(left = 130, right = 10, top = 5, bottom = 80) |>
        echarts4r::e_x_axis(axisLabel = list(fontSize = 9, rotate = 40, interval = 0)) |>
        echarts4r::e_y_axis(axisLabel = list(fontSize = 9))
    })

    output$eventlog <- DT::renderDT({
      el <- eventlog()
      el <- el[order(el$start, decreasing = TRUE), ]
      el$start <- format(el$start, "%Y-%m-%d %H:%M:%S"); el$end <- format(el$end, "%H:%M:%S")
      DT::datatable(el, rownames = FALSE, class = "compact wt-dt", options = list(dom = "tp", pageLength = 10))
    })
    output$export <- shiny::downloadHandler(
      filename = function() sprintf("welltrack_eventlog_%s.csv", format(Sys.Date())),
      content = function(file) utils::write.csv(eventlog(), file, row.names = FALSE)
    )

    output$yaml <- shiny::renderText(paste(readLines(Sys.getenv("WT_CONFIG_PATH", wt_sys_file("config", "process.yml")), warn = FALSE), collapse = "\n"))
  })
}
