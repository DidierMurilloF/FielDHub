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

#' Read and validate an uploaded design entries file
#'
#' @description The shared entry point every module's upload boils down to:
#' parses \code{path} as a CSV (\code{load_file()}/\code{read_upload_csv()}),
#' checks it against \code{design}'s column rule
#' (\code{upload_validation_rule()}/\code{check_input()}), and raises a
#' classed \code{fieldhub_input_error} with the same wording modules have
#' always shown (\code{upload_error_message()}) for anything that fails: an
#' unreadable/non-CSV file, ragged rows, or duplicate values in the columns
#' \code{design} requires to be unique. A malformed upload never raises an
#' uncaught error; it always reports this one condition.
#'
#' @param path Path to the uploaded file (e.g. \code{input$file$datapath}).
#' @param sep Field separator (\code{","}, \code{";"} or \code{"\t"}).
#' @param design One of the design keys \code{upload_validation_rule()}
#'   understands (e.g. \code{"crd"}, \code{"sdiag"}, \code{"sspd"}, ...).
#' @param missing_columns The message shown when the file parses but is
#'   missing, or duplicates, the columns \code{design} requires; each design
#'   writes its own (e.g. "Data input needs at least two columns: ENTRY and
#'   NAME").
#' @param name The uploaded file's own name, used only to check its
#'   extension; defaults to \code{basename(path)} for callers (tests) that
#'   have no separate name, such as a temp file uploaded as itself.
#' @param check Whether to apply \code{design}'s column/uniqueness rule at
#'   all; \code{FALSE} skips it (a module lets the same entries repeat
#'   across experiments, so duplicate values are expected).
#' @return \code{list(data = <data.frame>)}.
#' @noRd
read_design_upload <- function(path, sep, design, missing_columns = NULL,
                               name = basename(path), check = TRUE) {
  data_ingested <- load_file(name = name, path = path, sep = sep,
                             check = check, design = design)
  if (identical(names(data_ingested), "dataUp")) {
    return(list(data = data_ingested$dataUp))
  }
  fieldhub_abort(upload_error_message(data_ingested, missing_columns))
}
