#' Shared CSV-plus-metadata download for a completed app workflow
#' @noRd
app_csv_archive <- function(filename, data, design, field_book,
                              simulation = function() NULL, layout = function() NULL,
                              kind = "field_book") {
  handlers <- csv_archive_handlers(filename, data, design, field_book, simulation, layout, kind)
  shiny::downloadHandler(
    filename = function() validate_design(handlers$filename()),
    content = function(file) validate_design(handlers$content(file)),
    contentType = "application/zip"
  )
}
