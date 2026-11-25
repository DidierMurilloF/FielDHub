#' Accept simulation settings atomically in every design module
#' @noRd
app_simulation_controls <- function(input, session, ids, field_book,
                                     correlation_suffix = ".simulation") {
  ids <- simulation_control_ids(ids)
  correlations <- spatial_correlation_spec(correlation_suffix)
  settings <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input[[ids[["submit"]]]], {
    book <- field_book()
    shiny::req(book)
    candidate <- app_attempt(
      report = function(problem) app_report_problem(problem, severity = "warning"),
      {
        spatial <- FALSE
        if (isTRUE(input[[simulation_spatial_id(ids)]])) {
          read <- function(spec) {
            value <- input[[spec$inputId]]
            if (is.null(value)) return(spec$selected)
            suppressWarnings(as.numeric(value))
          }
          spatial <- list(rho_col = read(correlations$correlation_x),
                          rho_row = read(correlations$correlation_y))
        }
        simulation_request(
          mean = input[[ids[["mean"]]]], residual_sd = input[[ids[["sd"]]]],
          trait = input[[ids[["trait"]]]], other = input[[ids[["other"]]]],
          field_book = book, spatial = spatial
        )
      }
    )
    if (!is.null(candidate)) {
      settings(candidate)
      shiny::removeModal(session = session)
    }
  }, ignoreInit = TRUE)
  shiny::reactive(settings())
}

simulation_spatial_id <- function(ids) paste0(ids[["submit"]], "_spatial")
