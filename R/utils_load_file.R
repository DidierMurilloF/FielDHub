#' @importFrom utils read.csv
load_file <- function(name, path, sep) {
  ext <- tools::file_ext(name)
  if (all(c("csv", "CSV") != ext)) { # ext != "csv"
    validate("Invalid file; Please upload a .csv file.")
  }
  dataUp <- read.csv(path, header = TRUE, sep = sep, na.strings = c("", " ","NA"))
  dataUp <- as.data.frame(dataUp)
  return(dataUp)
}