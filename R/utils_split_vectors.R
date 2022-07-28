split_vectors <- function(x, len_cuts){
  
  if (length(x) != sum(len_cuts)) {
    return(NULL)
  } 
  cut_test <- x; dim_each_split <- len_cuts
  entris <- list()
  v = 1; s = 1
  h <- dim_each_split[v]
  for (j in 1:length(dim_each_split)) {
    entris[[v]] <- cut_test[s:h]
    s = s + dim_each_split[v]
    v = v + 1
    h <- h + dim_each_split[v]
  }
  
  return(entris)
}