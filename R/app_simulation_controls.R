#' Accept simulation settings atomically in every design module
#'
#' Register once when the module is initialized. Invalid submissions retain
#' both the open dialog and the previous settings; validation is plain R.
#' @noRd
app_simulation_controls <- function(input, session, ids, field_book,
                                      correlation_ids = NULL) {
  ids <- simulation_control_ids(ids)
  if (!is.null(correlation_ids) &&
      (!is.character(correlation_ids) || !is.null(dim(correlation_ids)) ||
       !identical(names(correlation_ids), c("x", "y")) || anyNA(correlation_ids) ||
       anyDuplicated(c(ids, correlation_ids)) ||
       !all(grepl("^[A-Za-z][A-Za-z0-9_.]*$", correlation_ids)))) {
    fieldhub_abort("Spatial simulation controls need distinct x and y input identifiers.")
  }
  settings <- shiny::reactiveVal(NULL)
  notification_id <- session$ns("simulation-settings-error")
  read_correlation <- function(id) {
    value <- input[[id]]
    if (is.null(value)) NA_real_ else value
  }
  shiny::observeEvent(input[[ids[["submit"]]]], {
    book <- field_book()
    shiny::req(book)
    candidate <- tryCatch(
      simulation_request(
        min_value = input[[ids[["minimum"]]]], max_value = input[[ids[["maximum"]]]],
        trait = input[[ids[["trait"]]]], other = input[[ids[["other"]]]],
        field_columns = names(book),
        correlations = if (!is.null(correlation_ids)) {
          c(x = read_correlation(correlation_ids[["x"]]),
            y = read_correlation(correlation_ids[["y"]]))
        }
      ),
      fieldhub_error = function(e) {
        shiny::showNotification(conditionMessage(e), type = "error", duration = 8,
                                 id = notification_id, session = session)
        NULL
      }
    )
    if (!is.null(candidate)) {
      settings(candidate)
      shiny::removeNotification(notification_id, session = session)
      shiny::removeModal(session = session)
    }
  }, ignoreInit = TRUE)
  shiny::reactive(settings())
}
