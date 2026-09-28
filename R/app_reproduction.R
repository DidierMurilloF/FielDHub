#' Shared design archive and R code panel for every design page
#' @noRd
app_reproduction_ui <- function(ns) {
  shiny::tags$details(
    class = "fieldhub-reproduction",
    shiny::tags$summary("Save design and reproduction code"),
    shiny::helpText(
      "After generating a design, download its complete RDS result, including",
      "the seed, parameters, random-number settings, and FielDHub version.",
      "The R code below rebuilds this design with FielDHub. Large uploads or",
      "older records use the saved RDS instead. Use Save experiment (ZIP)",
      "to keep the CSV, displayed field book, simulation record, layout settings,",
      "software versions, and reconstruction code together.",
      "Keep the original RDS to preserve the exact result across software upgrades."
    ),
    shiny::downloadButton(ns("design_rds"), "Save design (RDS)"),
    shiny::verbatimTextOutput(ns("design_code"))
  )
}

#' Connect the shared exports to a module's existing public-API result
#' @noRd
app_reproduction_outputs <- function(output, design) {
  ready <- function() {
    x <- design()
    shiny::req(x)
    x
  }
  handlers <- design_export_handlers(ready)
  output$design_rds <- shiny::downloadHandler(
    filename = function() validate_design(handlers$filename()),
    content = function(file) validate_design(handlers$content(file)),
    contentType = "application/octet-stream"
  )
  output$design_code <- shiny::renderText(validate_design(handlers$code()))
  invisible(NULL)
}
