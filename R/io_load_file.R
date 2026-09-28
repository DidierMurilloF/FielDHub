#' @importFrom utils read.csv
load_file <- function(name, path, sep, check = FALSE, design = NULL) {
  ext <- tools::file_ext(name)
  if (all(c("csv", "CSV") != ext)) return(list(bad_format = TRUE))
  dataUp <- read_upload_csv(path, sep)
  if (is.null(dataUp)) return(list(bad_format = TRUE))
  if (check) {
    valid <- check_input(design, dataUp)
    if (is.null(valid)) return(list(missing_cols = TRUE))
    if (!valid) return(list(duplicated_vals = TRUE))
  }
  list(dataUp = dataUp)
}

#' Read an upload without repairing ragged records or inferring row names
#' @noRd
read_upload_csv <- function(path, sep) {
  tryCatch({
    fields <- utils::count.fields(path, sep = sep, quote = "\"",
                                  comment.char = "", blank.lines.skip = TRUE)
    # Continued quoted records have NA counts on their unfinished lines.
    fields <- fields[!is.na(fields)]
    if (length(fields) < 2L || any(fields != fields[1L])) return(NULL)
    data <- read.csv(path, header = TRUE, sep = sep, fill = FALSE,
                     na.strings = c("", " ", "NA"))
    if (nrow(data) != length(fields) - 1L) return(NULL)
    data
  }, error = function(e) NULL)
}

#' Translate the existing upload-result flags into a shared error message
#' @noRd
upload_error_message <- function(result, missing_columns) {
  kind <- names(result)
  if (length(kind) != 1L) return(NULL)
  switch(kind,
         bad_format = "Invalid file; Please upload a .csv file.",
         duplicated_vals = "Check input file for duplicate values.",
         missing_cols = missing_columns,
         NULL)
}
