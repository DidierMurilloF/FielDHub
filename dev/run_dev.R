# Run from the repository root. Documentation is a separate development task.
pkgload::load_all(".", export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
shiny::runApp(FielDHub::run_app())
