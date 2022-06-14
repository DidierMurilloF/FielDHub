# example_matrix <- read.csv("example_matrix.csv")
# example_matrix <- as.matrix(example_matrix)
# test_matrix <- example_matrix[,-1]



checks <- Checks
blocks <- length(data_dim_each_block)
w_map_letters1 <- w_map_letters
Name_expt <- expt_name
if (length(Name_expt) == blocks || !is.null(Name_expt)) {
  name_blocks <- Name_expt
}else {
  name_blocks <- paste(rep("Expt", blocks), 1:blocks, sep = "")
}
Index_block <- paste0("B", 1:blocks)
Name_expt <- expt_name
if (length(Name_expt) == blocks || !is.null(Name_expt)) {
  name_blocks <- Name_expt
}else {
  name_blocks <- paste(rep("Expt", blocks), 1:blocks, sep = "")
}
z <- 1
for(i in Index_block){ 
  w_map_letters1[w_map_letters1 == i] <- name_blocks[z] 
  z <- z + 1 
} 
w_map_letters1
checks_ch <- as.character(checks) 
for(j in 1:ncol(w_map_letters1)) { #ncol(w_map_letters1)
  for(i in nrow(w_map_letters1):1) { 
    if (any(checks_ch %in% w_map_letters1[i, j]) && w_map_letters1[i,j] != "Filler") {
      if (i != 1) {
        if (w_map_letters1[i - 1, j] == "Filler") {
          w_map_letters1[i, j] <- w_map_letters1[i - 1, j]
        } else w_map_letters1[i, j] <- w_map_letters1[i - 1, j]
      } else if (i == 1) {
        w_map_letters1[i, j] <- w_map_letters1[i + 1, j]
      }
    }
  }
}
split_names <- w_map_letters1
split_names


