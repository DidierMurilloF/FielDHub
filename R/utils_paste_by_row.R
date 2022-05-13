paste_by_row <- function(files_list){
  len_list <- length(files_list)
  file_range <- 2:len_list
  if (len_list >= 2) {
    data_output <- files_list[[1]]
    for (d in file_range){
      data_output  <- rbind(data_output, files_list[[d]])
      data_output <- data_output 
    }
  }else{
    data_output <- files_list[[1]]
  }
  return(data_output)
}