#' Shared metadata-bearing configuration for table Copy, Excel and Print
#' @noRd
app_table_export_buttons <- function(design, table, location = NULL, print = FALSE) {
  record <- validate_design(table_export_record(design, table, location))
  text <- table_export_text(record)
  summary <- paste0("FielDHub ", record$metadata$package_version, " | ", record$metadata$design,
                     " | seed: ", record$metadata$seed, " | ", table,
                     if (!is.null(location)) paste0(" | location: ", location),
                     " | Full parameters: FielDHub metadata worksheet.")
  script <- readLines(system.file("app/www/table-export-metadata.js", package = "FielDHub"))
  buttons <- list(
    list(extend = "copy", text = "Copy + metadata", messageTop = text, messageBottom = NULL),
    list(extend = "excel", text = "Excel + metadata", messageTop = summary, messageBottom = NULL,
          fieldhubMetadata = as.list(table_export_chunks(text)), customize = DT::JS(script))
  )
  if (isTRUE(print)) {
    buttons[[length(buttons) + 1L]] <- list(
      extend = "print", text = "Print + metadata", messageBottom = NULL,
      messageTop = paste0('<pre style="white-space:pre-wrap;overflow-wrap:anywhere">',
                          htmltools::htmlEscape(text), "</pre>")
    )
  }
  buttons
}
