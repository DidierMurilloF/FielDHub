#' Optional packages needed to run the application
#' @noRd
app_dependencies <- function() {
  c("golem", "shiny", "htmltools", "DT", "bslib", "shinycssloaders",
    "plotly", "shinyalert", "shinyjs", "zip")
}

#' Explain missing app packages without installing or loading the application
#' @noRd
check_app_dependencies <- function(available = function(package) {
  requireNamespace(package, quietly = TRUE)
}) {
  packages <- app_dependencies()
  missing <- packages[!vapply(packages, available, logical(1))]
  if (length(missing)) {
    quoted <- paste(encodeString(missing, quote = '"'), collapse = ", ")
    fieldhub_abort(
      "The FielDHub app needs additional packages. Install them with ",
      "install.packages(c(", quoted, ")). The R design functions remain available.",
      class = "fieldhub_dependency_error", data = list(packages = missing)
    )
  }
  invisible(NULL)
}
