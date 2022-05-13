split_name <- function(n_rows = NULL, n_cols = NULL, Name_expt = NULL, by_row = FALSE,
                       col_sets = NULL, row_sets = NULL){
  
  my_split <- matrix(data = 0, nrow = n_rows, ncol = n_cols, byrow = F)
  
  if (length(Name_expt) > 1) {
    if (by_row == FALSE) {
      li_cols <- turner::matrix_to_blocks(my_split, col_sets, byrow = FALSE)
      k = 1
      for (l in 1:length(li_cols)){
        for (i in 1:nrow(my_split)){
          for (j in  1:ncol(li_cols[[k]])){
            li_cols[[l]][i,j] <- Name_expt[k]
          }
        }
        k = k + 1
      }
      if (length(li_cols) > 2){
        split_name <- cbind(li_cols[[1]], li_cols[[2]])
        for (d in 3:length(li_cols)){
          split_name <- cbind(split_name, li_cols[[d]])
          split_name <- split_name
        }
      }else{
        split_name <- cbind(li_cols[[1]], li_cols[[2]])
      }
    }else{
      li_by_rows <- turner::matrix_to_blocks(my_split, row_sets, byrow = TRUE)
      k = 1
      for (l in 1:length(li_by_rows)){
        for (i in 1:nrow(li_by_rows[[l]])){
          for (j in  1:ncol(my_split)){
            li_by_rows[[l]][i,j] <- Name_expt[k]
          }
        }
        k = k + 1
      }
      len_list <- length(li_by_rows)
      if (len_list > 2){
        split_name <- rbind(li_by_rows[[len_list]], li_by_rows[[len_list - 1]])
        for (d in (len_list - 2):1){
          split_name <- rbind(split_name, li_by_rows[[d]])
          split_name <- split_name
        }
      }else{
        split_name <- rbind(li_by_rows[[len_list]], li_by_rows[[len_list - 1]])
      }
    }
    return(split_name)
  }else{
    for (i in 1:n_rows){
      for (j in 1:n_cols){
        my_split[i,j] <- Name_expt[1]
      }
    }
    return(my_split)
  }
}