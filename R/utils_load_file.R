#' @importFrom utils read.csv
load_file <- function(name, path, sep, check = FALSE, design = NULL) {
  ext <- tools::file_ext(name)
  if (all(c("csv", "CSV") != ext)) { # ext != "csv"
    validate("Invalid file; Please upload a .csv file.")
  }
  dataUp <- read.csv(path, header = TRUE, sep = sep, na.strings = c("", " ","NA"))
  dataUp <- as.data.frame(dataUp)
  
  if (isTRUE((check == TRUE) & isTRUE(check_input(design,dataUp)))) {
    print("1")
    print(design)
    print(check_input(design,dataUp))
    print((check == TRUE) & isTRUE(check_input(design,dataUp)))
    
  }else {
    print("2")
    print((check == TRUE) & isTRUE(check_input(design,dataUp)))
    print(check_input(design,dataUp))
    print(design)
    validate("Make sure the correct entries in the .csv file are unique for the design.")
  }
  
  #validate((check == TRUE) & isTRUE(check_input(design,dataUp)) ,"Make sure the correct entries in the .csv file are unique for the design.")
  
  return(dataUp)
}