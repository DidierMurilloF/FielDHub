#' @importFrom utils read.csv
load_file <- function(name, path, sep, check = FALSE, design = NULL) {
  ext <- tools::file_ext(name)
  duplicated_vals = FALSE
  
  if (all(c("csv", "CSV") != ext)) { 
    return(duplicated_vals)
  }
  
  dataUp <- read.csv(path, header = TRUE, sep = sep, na.strings = c("", " ","NA"))
  dataUp <- as.data.frame(dataUp)
  
  if (check) {
    if (!check_input(design,dataUp)) {
      duplicated_vals = TRUE
      return(duplicated_vals)
    }
  }
  
  if (!duplicated_vals) {
    return(dataUp)
  }
}