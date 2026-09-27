#' @importFrom utils read.csv
load_file <- function(name, path, sep, check = FALSE, design = NULL) {
  
  ext <- tools::file_ext(name)
  bad_format <- FALSE
  duplicated_vals <- FALSE
  if (all(c("csv", "CSV") != ext)) {
    bad_format = TRUE
    return(list(bad_format = bad_format))
  } else {
    # A file read.csv() cannot parse (ragged rows, empty file) is reported
    # like a wrong format instead of stopping the app session
    dataUp <- tryCatch(
      read.csv(path,
               header = TRUE,
               sep = sep,
               na.strings = c("", " ","NA")),
      error = function(e) NULL
    )
    if (is.null(dataUp)) {
      bad_format = TRUE
      return(list(bad_format = bad_format))
    }
    dataUp <- as.data.frame(dataUp)
    if (check) {
      if (!is.null(check_input(design, dataUp))) {
        if (!check_input(design, dataUp)) {
          duplicated_vals = TRUE
          return(list(duplicated_vals = duplicated_vals))
        } else return(list(dataUp = dataUp))
      } else return(list(missing_cols = TRUE))
    } else return(list(dataUp = dataUp))
  }
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
