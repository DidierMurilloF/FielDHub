#' Download only the requested table as a plain CSV
#' @noRd
app_csv_download <- function(filename, data) {
  handlers <- csv_download_handlers(filename, data)
  shiny::downloadHandler(
    filename = function() validate_design(handlers$filename()),
    content = function(file) validate_design(handlers$content(file)),
    contentType = "text/csv; charset=utf-8"
  )
}
