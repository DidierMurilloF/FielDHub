#' Runtime packages installed with the application
#' @noRd
app_dependencies <- function() {
  c("shiny", "htmltools", "DT", "bslib", "promises", "shinyjs")
}

#' Explain missing app packages without installing or loading the application
#' @noRd
app_check_dependencies <- function(available = function(package) {
  minimum <- c(shiny = "1.8.1")
  requireNamespace(package, quietly = TRUE) &&
    (!package %in% names(minimum) || utils::packageVersion(package) >= minimum[[package]])
}) {
  packages <- app_dependencies()
  missing <- packages[!vapply(packages, available, logical(1))]
  if (length(missing)) {
    quoted <- paste(encodeString(missing, quote = '"'), collapse = ", ")
    fieldhub_abort(
      "The FielDHub app needs missing or updated runtime packages. Install them with ",
      "install.packages(c(", quoted, ")). These packages are normally installed with FielDHub.",
      class = "fieldhub_dependency_error", data = list(packages = missing)
    )
  }
  invisible(NULL)
}
