# New opportunity (Stage 1 framing) - modal module ------------------------------------
#
# The form is generated from config `framing_fields`; fields with
# `source: auto` are pre-filled from master data when a well is chosen
# (simulated here from existing records). A live preview shows how the
# configuration will treat the case (class, required streams, authority).

#' Build one framing input from its config definition
#' @param f Field definition.
#' @param ns Namespace function.
#' @param cfg Config list.
#' @param wells Known wells.
#' @keywords internal
wt_framing_input <- function(f, ns, cfg, wells) {
  id <- ns(paste0("f_", f$id))
  label <- htmltools::tagList(f$label, if (isTRUE(f$required)) htmltools::span(class = "wt-req", "*"),
                              if (identical(f$source, "auto")) htmltools::span(class = "wt-auto", "auto"))
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
          htmltools::span(class = "wt-live", "LIVE"),
          htmltools::tags$button(type = "button", class = "btn btn-sm btn-default", `data-dismiss` = "modal", `data-bs-dismiss` = "modal", "Cancel"),
          shiny::actionButton(ns("save_draft"), "Save draft", class = "btn-sm"),
          shiny::actionButton(ns("submit"), "Submit framed opportunity", class = "btn-sm btn-warning")
        )
      ))
    })

    # Simulated master-data retrieval for auto fields
    shiny::observeEvent(input$f_well, {
      o <- app$data()$opportunity
      hit <- o[o$well == input$f_well, ]
      if (!nrow(hit)) return()
      hit <- hit[which.max(hit$created_at), ]
      shiny::updateTextInput(session, "f_field", value = hit$field)
      shiny::updateNumericInput(session, "f_current_bopd", value = if (is.na(hit$actual_bopd)) hit$current_bopd else hit$actual_bopd)
      shiny::updateCheckboxInput(session, "f_artificial_lift", value = isTRUE(hit$artificial_lift))
    }, ignoreInit = TRUE)

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
        htmltools::div(class = "wt-section", shiny::icon("robot"), "How the process will treat this case"),
        htmltools::tags$table(class = "wt-mini",
          htmltools::tags$tr(htmltools::tags$td("Complexity class"), htmltools::tags$td(wt_class_badge(rec$class, cfg), cfg$complexity_classes[[rec$class]]$name)),
          htmltools::tags$tr(htmltools::tags$td("D1 authority"), htmltools::tags$td(cfg$decisions$D1$authority[[rec$class]])),
          htmltools::tags$tr(htmltools::tags$td("D1 SLA"), htmltools::tags$td(wt_sla_days(cfg, "D1", rec$class), " days")),
          htmltools::tags$tr(htmltools::tags$td("WPA"), htmltools::tags$td(cfg$complexity_classes[[rec$class]]$wpa_meeting)),
          htmltools::tags$tr(htmltools::tags$td("Assurance streams"), htmltools::tags$td(
            paste(sn$name[match(wt_required_streams(rec, cfg), sn$id)], collapse = ", ")))),
        if (length(miss)) wt_callout("Missing for D1", type = "warn", paste(miss, collapse = ", "))
        else wt_callout("Framing complete", type = "ok", "D1 criterion 'Opportunity clearly defined' will be met automatically.")
      )
    })

    save <- function(state) {
      if (state == "OPPORTUNITY_FRAMED" && length(missing_required())) {
        shiny::showNotification(paste("Complete:", paste(missing_required(), collapse = ", ")), type = "error")
        return()
      }
      rec <- record()
      rec$state <- state; rec$step_id <- "S1"; rec$originator <- app$user()$user
      id <- wt_db_insert_opportunity(app$con, rec)
      shiny::removeModal()
      app$bump()
      app$open_opp(id)
      shiny::showNotification(sprintf("%s created (%s, class %s)", id, state, rec$class), type = "message")
    }
    shiny::observeEvent(input$submit, save("OPPORTUNITY_FRAMED"))
    shiny::observeEvent(input$save_draft, save("OPPORTUNITY_DRAFT"))
  })
}
