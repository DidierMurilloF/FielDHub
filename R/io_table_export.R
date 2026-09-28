#' Complete, plain reproducibility record for an exported table view
#' @noRd
table_export_record <- function(design, table, location = NULL) {
  reproduction_engine(design)
  if (!is.character(table) || length(table) != 1L || is.na(table) || !nzchar(trimws(table))) {
    fieldhub_abort("An exported table needs one nonblank view name.")
  }
  if (!is.null(location)) validate_iteration_budget(location, "location")
  list(schema_version = 1L, metadata = design$metadata,
        view = list(table = table, location = location),
        software = list(R = as.character(getRversion()), platform = R.version$platform))
}

#' Render the entire table record as data, without evaluating its contents
#' @noRd
table_export_text <- function(record) {
  dump <- utils::capture.output(dput(record, control = c("keepNA", "keepInteger", "showAttributes", "hexNumeric")))
  paste(c("# FielDHub table export: complete reproducibility record",
          "# Table filtering or sorting does not change the recorded core design.",
          "# Keep the design RDS or experiment ZIP to preserve exact results.",
          "# Software or platform changes can alter reconstructed results.",
          "# Do not evaluate records received from untrusted sources.",
          paste0("record <- ", dump[1L]), dump[-1L]), collapse = "\n")
}

#' Split metadata without discarding text or exceeding Excel's UTF-16 cell limit
#' @noRd
table_export_chunks <- function(text) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    fieldhub_abort("Export metadata must be one nonmissing string.")
  }
  size <- nchar(text, type = "chars")
  if (size == 0L) return("")
  starts <- seq.int(1, size, by = 16000L)
  substring(text, starts, pmin(starts + 15999, size))
}
