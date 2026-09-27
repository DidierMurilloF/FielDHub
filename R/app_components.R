#' Explicit styling shared by app spinners
#'
#' @param ... Per-spinner overrides.
#' @noRd
fieldhub_spinner_options <- function(...) {
  utils::modifyList(
    list(color = "#2c7da3", color.background = "#ffffff", size = 2),
    list(...)
  )
}

#' Wrap an output in a spinner without changing process-wide options
#'
#' @param ui_element The output to wrap.
#' @param ... Arguments passed to `shinycssloaders::withSpinner()`.
#' @noRd
fieldhub_spinner <- function(ui_element, ...) {
  do.call(shinycssloaders::withSpinner,
          c(list(ui_element = ui_element), fieldhub_spinner_options(...)))
}
