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
