#' Plain specifications for spatial correlation controls
#' @noRd
spatial_correlation_spec <- function(suffix) {
  if (!is.character(suffix) || length(suffix) != 1L ||
      is.na(suffix) || !nzchar(suffix)) {
    fieldhub_abort("The spatial control suffix must be one non-empty string.")
  }
  list(
    correlation_x = list(
      inputId = paste0("ROX", suffix), label = "Adjacent columns (within a row):",
      choices = seq(0.1, 0.9, 0.1), selected = 0.5
    ),
    correlation_y = list(
      inputId = paste0("ROY", suffix), label = "Adjacent rows (within a column):",
      choices = seq(0.1, 0.9, 0.1), selected = 0.5
    )
  )
}

#' Shared spatial controls using each module's established input identifiers
#' @noRd
app_spatial_correlations <- function(ns, suffix) {
  columns <- lapply(spatial_correlation_spec(suffix), function(spec) {
    spec$inputId <- ns(spec$inputId)
    shiny::column(6, do.call(shiny::selectInput, spec))
  })
  do.call(shiny::fluidRow, unname(columns))
}
