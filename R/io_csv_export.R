#' Descriptive CSV names without user-provided paths
#' @noRd
csv_export_filename <- function(design, kind = "fieldbook", location = NULL, date = Sys.Date()) {
  paste0("FielDHub_", design$metadata$design, "_", kind,
         if (!is.null(location)) paste0("_location_", location), "_", date, ".csv")
}

#' Plain CSV callbacks, evaluated only when the current table is downloaded
#' @noRd
csv_download_handlers <- function(filename, data) {
  list(
    filename = function() {
      name <- filename()
      if (!is.character(name) || length(name) != 1L || is.na(name) ||
          grepl("[/\\\\[:cntrl:]]", name) || !grepl("[.]csv$", name, ignore.case = TRUE)) {
        fieldhub_abort("Use one CSV filename without directories or control characters.")
      }
      name
    },
    content = function(file) {
      table <- data()
      validate_export_table(table)
      tryCatch(
        utils::write.csv(table, file, row.names = FALSE, fileEncoding = "UTF-8"),
        error = function(e) fieldhub_abort("Could not save the CSV: ", conditionMessage(e),
          class = "fieldhub_export_error", data = list(parent = e))
      )
      invisible(NULL)
    }
  )
}
