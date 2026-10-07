# Entry point for Posit Connect (rsconnect::deployApp()) and local runs.
# Locally: shiny::runApp() or welltrack2::run_app().
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
welltrack2::run_app(mock = TRUE)
