# Analysis: production history & decline (catalog id `decline_curve`) -----------------
#
# Compute function (pure, no Shiny) + Shiny module. The module only collects
# parameters and draws; everything numeric comes from wt_an_decline_fit(), so
# the same analysis can run in a script, a scheduled report or an API.

#' Fit a decline curve to oil-rate history
#'
#' @param prod data.frame(month = Date, oil_bopd). Zero-rate months (downtime)
#'   are excluded from the fit.
#' @param months Number of most recent months used for the fit.
#' @param model `"exponential"` or `"harmonic"`.
#' @param horizon Forecast months.
#' @return list(params, outputs, data_ref, summary, history, fitted, forecast).
#'   `outputs`: qi_bopd, decline_pct_yr (effective annual), forecast_12m_bbl, r2.
#' @export
wt_an_decline_fit <- function(prod, months = 12, model = c("exponential", "harmonic"), horizon = 12) {
  model <- match.arg(model)
  if (is.null(prod) || !nrow(prod)) stop("No production history for this well", call. = FALSE)
  prod <- prod[order(prod$month), , drop = FALSE]
  win <- utils::tail(prod, months)
  x <- win[win$oil_bopd > 0, , drop = FALSE]
  if (nrow(x) < 3) stop("At least 3 producing months are needed for a decline fit", call. = FALSE)

  t0 <- x$month[1]
  t <- as.numeric(x$month - t0) / 365.25
  q <- x$oil_bopd
  if (model == "exponential") {
    fit <- stats::lm(log(q) ~ t)
    qi <- exp(unname(stats::coef(fit)[1])); d <- -unname(stats::coef(fit)[2])
    rate <- function(tt) qi * exp(-d * tt)
    eff <- 100 * (1 - exp(-d))
  } else {
    fit <- stats::lm(I(1 / q) ~ t)
    qi <- 1 / unname(stats::coef(fit)[1]); d <- unname(stats::coef(fit)[2]) * qi
    rate <- function(tt) qi / (1 + d * tt)
    eff <- 100 * (1 - 1 / (1 + d))
  }
  qhat <- rate(t)
  r2 <- 1 - sum((q - qhat)^2) / sum((q - mean(q))^2)

  last <- prod$month[nrow(prod)]
  f_month <- seq(last, by = "month", length.out = horizon + 1)[-1]
  f_rate <- pmax(rate(as.numeric(f_month - t0) / 365.25), 0)
  outputs <- list(qi_bopd = round(qi, 1), decline_pct_yr = round(eff, 1),
                  forecast_12m_bbl = round(sum(f_rate[seq_len(min(12, horizon))]) * 30.4),
                  r2 = round(r2, 3))
  list(
    params = list(months = months, model = model, horizon = horizon),
    outputs = outputs,
    data_ref = sprintf("production_history %s..%s (%d points)", format(x$month[1], "%Y-%m"), format(last, "%Y-%m"), nrow(x)),
    summary = sprintf("%s decline %.1f%%/yr from %.0f bopd (R2 %.2f); 12-month baseline %s bbl",
                      model, eff, qi, r2, format(outputs$forecast_12m_bbl, big.mark = ",")),
    history = prod,
    fitted = data.frame(month = x$month, oil_bopd = round(qhat, 1)),
    forecast = data.frame(month = f_month, oil_bopd = round(f_rate, 1))
  )
}

#' Decline analysis module - UI
#' @param id Module id.
#' @keywords internal
an_decline_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    htmltools::div(class = "wt-filterbar",
      shiny::selectInput(ns("months"), "Fit window", c("6 months" = 6, "12 months" = 12, "24 months" = 24, "36 months" = 36),
                         selected = 12, width = "130px"),
      shiny::radioButtons(ns("model"), "Model", c("Exponential" = "exponential", "Harmonic" = "harmonic"), inline = TRUE)
    ),
    shiny::uiOutput(ns("kpis")),
    echarts4r::echarts4rOutput(ns("plot"), height = "300px")
  )
}

#' Decline analysis module - server
#' @param id Module id.
#' @param ctx Reactive case context.
#' @return Reactive analysis result (or a list with `error`).
#' @keywords internal
an_decline_server <- function(id, ctx) {
  shiny::moduleServer(id, function(input, output, session) {
    result <- shiny::reactive({
      prod <- ctx()$production
      tryCatch(wt_an_decline_fit(prod, as.numeric(input$months %||% 12), input$model %||% "exponential"),
               error = function(e) list(error = conditionMessage(e)))
    })

    output$kpis <- shiny::renderUI({
      r <- result()
      if (!is.null(r$error)) return(wt_callout("Analysis not possible", type = "warn", r$error))
      o <- r$outputs
      bslib::layout_column_wrap(width = 1 / 4, class = "wt-kpi-row",
        wt_kpi("Initial rate of fit", wt_fmt(o$qi_bopd, " bopd")),
        wt_kpi("Annual decline", wt_fmt(o$decline_pct_yr, " %/yr", 1)),
        wt_kpi("12-month baseline", wt_fmt(o$forecast_12m_bbl / 1000, " kbbl", 1), "no-intervention volume"),
        wt_kpi("Fit quality R\u00b2", sprintf("%.2f", o$r2), NULL, if (o$r2 >= .8) "ok" else if (o$r2 >= .5) "warn" else "bad"))
    })

    output$plot <- echarts4r::renderEcharts4r({
      r <- result()
      shiny::validate(shiny::need(is.null(r$error), r$error))
      h <- r$history; f <- r$fitted; fc <- r$forecast
      df <- merge(merge(data.frame(month = h$month, history = h$oil_bopd),
                        data.frame(month = f$month, fit = f$oil_bopd), all = TRUE),
                  data.frame(month = fc$month, forecast = fc$oil_bopd), all = TRUE)
      df$month <- format(df$month, "%Y-%m")
      df |>
        echarts4r::e_charts(month) |>
        echarts4r::e_scatter(history, name = "Oil rate (monthly)", symbol_size = 8, color = "#2a78d6") |>
        echarts4r::e_line(fit, name = "Fit", color = "#eb6834", symbol = "none", lineStyle = list(width = 2)) |>
        echarts4r::e_line(forecast, name = "12-month forecast", color = "#eb6834", symbol = "none",
                          lineStyle = list(width = 2, type = "dashed")) |>
        echarts4r::e_tooltip(trigger = "axis") |>
        echarts4r::e_legend(bottom = 0, textStyle = list(fontSize = 10)) |>
        echarts4r::e_y_axis(name = "bopd", nameTextStyle = list(fontSize = 9), axisLabel = list(fontSize = 9)) |>
        echarts4r::e_x_axis(axisLabel = list(fontSize = 9)) |>
        echarts4r::e_grid(left = 45, right = 15, top = 25, bottom = 45)
    })

    result
  })
}
