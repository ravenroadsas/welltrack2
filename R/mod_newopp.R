# New opportunity (Stage 1 framing) - modal module ------------------------------------
#
# The form is generated from config `framing_fields`. A side panel shows how
# the process will treat the case. Preview edition: nothing is saved.

#' Build one framing input from its config definition
#' @param f Field definition.
#' @param ns Namespace function.
#' @param cfg Config list.
#' @param wells Known wells.
#' @keywords internal
wt_framing_input <- function(f, ns, cfg, wells) {
  id <- ns(paste0("f_", f$id))
  label <- htmltools::tagList(f$label, if (isTRUE(f$required)) htmltools::span(class = "wt-req", "*"))
  choices <- if (!is.null(f$choices_from)) {
    stats::setNames(vapply(cfg[[f$choices_from]], `[[`, "", "id"), vapply(cfg[[f$choices_from]], `[[`, "", "name"))
  } else unlist(f$choices)
  switch(f$type,
    select = if (f$id == "well") shiny::selectizeInput(id, label, choices = c("", wells), width = "100%",
                                                       options = list(create = TRUE, placeholder = "Pick or type a well"))
             else shiny::selectInput(id, label, choices = c("", choices), width = "100%"),
    number = shiny::numericInput(id, label, value = NA, width = "100%"),
    textarea = shiny::textAreaInput(id, label, rows = 2, width = "100%"),
    checkbox = shiny::checkboxInput(id, label, FALSE),
    shiny::textInput(id, label, width = "100%")
  )
}

#' New opportunity module - server (button lives in the navbar as `newopp-open`)
#' @param id Module id.
#' @param app Shared app context.
#' @keywords internal
mod_newopp_server <- function(id, app) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    cfg <- app$cfg
    fields <- cfg$framing_fields

    shiny::observeEvent(input$open, {
      if (!wt_can(app$user(), "create_opportunity", cfg)) {
        shiny::showNotification("Your account type cannot create opportunities.", type = "warning")
        return()
      }
      wells <- sort(unique(app$data()$opportunity$well))
      shiny::showModal(shiny::modalDialog(
        title = htmltools::span(shiny::icon("seedling"), "Frame a new opportunity (Stage 1)"),
        size = "xl", easyClose = TRUE,
        bslib::layout_columns(col_widths = c(8, 4),
          htmltools::div(class = "wt-form-grid", lapply(fields, wt_framing_input, ns = ns, cfg = cfg, wells = wells)),
          htmltools::div(class = "wt-assistant", shiny::uiOutput(ns("preview")))
        ),
        footer = htmltools::tagList(
          htmltools::span(class = "wt-hint", "Preview: the form is not saved. "),
          htmltools::tags$button(type = "button", class = "btn btn-sm btn-default", `data-dismiss` = "modal", `data-bs-dismiss` = "modal", "Cancel"),
          shiny::actionButton(ns("submit"), "Submit", class = "btn-sm btn-warning")
        )
      ))
    })

    record <- shiny::reactive({
      rec <- lapply(fields, function(f) input[[paste0("f_", f$id)]])
      names(rec) <- vapply(fields, `[[`, "", "id")
      rec <- lapply(rec, function(v) if (is.null(v) || identical(v, "")) NA else v)
      rec$hse_risk <- "low"; rec$novelty <- FALSE
      rec$incremental_bopd <- if (is.na(rec$reservoir_bopd)) NA else max(rec$reservoir_bopd - (rec$current_bopd %||% 0), 0) * 0.8
      rec$intervention_type <- if (is.na(rec$intervention_type)) "workover" else rec$intervention_type
      rec$class <- wt_classify(rec, cfg)
      rec
    })

    missing_required <- shiny::reactive({
      rec <- record()
      req <- Filter(function(f) isTRUE(f$required), fields)
      vapply(req, `[[`, "", "label")[vapply(req, function(f) is.na(rec[[f$id]]), logical(1))]
    })

    output$preview <- shiny::renderUI({
      rec <- record()
      sn <- wt_streams(cfg)
      miss <- missing_required()
      htmltools::tagList(
        htmltools::div(class = "wt-section", "How the process will treat this case"),
        htmltools::tags$table(class = "wt-mini",
          htmltools::tags$tr(htmltools::tags$td("Complexity class"), htmltools::tags$td(wt_class_badge(rec$class, cfg), cfg$complexity_classes[[rec$class]]$name)),
          htmltools::tags$tr(htmltools::tags$td("Required disciplines"), htmltools::tags$td(
            paste(sn$name[match(wt_required_streams(rec, cfg), sn$id)], collapse = ", ")))),
        if (length(miss)) wt_callout("Missing for D1", type = "warn", paste(miss, collapse = ", "))
        else wt_callout("Framing complete", type = "ok", "Ready to go to D1.")
      )
    })

    shiny::observeEvent(input$submit, {
      if (length(missing_required())) {
        shiny::showNotification(paste("Complete:", paste(missing_required(), collapse = ", ")), type = "error")
        return()
      }
      shiny::removeModal()
      shiny::showNotification("Preview only: the opportunity was not saved. Thank you for trying the form!", type = "message")
    })
  })
}
