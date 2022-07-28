#' @importFrom utils read.csv
load_file <- function(name, path, sep, check = FALSE, design = NULL) {
  
  ext <- tools::file_ext(name)
  bad_format <- FALSE
  duplicated_vals <- FALSE
  if (all(c("csv", "CSV") != ext)) {
    bad_format = TRUE
    return(list(bad_format = bad_format))
  } else {
    dataUp <- read.csv(path,
                       header = TRUE, 
                       sep = sep, 
                       na.strings = c("", " ","NA"))
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