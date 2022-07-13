set_augmented_blocks <- function(lines, checks, start = 5) {
  if (lines > 40) div <- 3 else div <- 2
  blocks <- start:ceiling(lines / div)
  b <- vector(mode = "numeric")
  checked_dims <- list()
  blocks_dims <- matrix(ncol = 2, byrow = TRUE)
  n <- 1
  for (i in blocks) {
    all_genotypes <- lines + checks * i
    plots_per_block <- base::ceiling(all_genotypes / i)
    lines_per_plot <- plots_per_block - checks
    excedent <- plots_per_block * i
    Fillers <- excedent - all_genotypes
    dims <- factor_subsets(plots_per_block, augmented = TRUE)$combos
    default_dim <- c(1 * i, plots_per_block)
    options_dims <- list(default_dim)
    if (!is.null(dims)) {
      for (k in 1:length(dims)) {
        options_dims[[k + 1]] <- as.vector(dims[[k]]) * c(i,1)
      }
    }
    for (m in 1:length(options_dims)) {
      if (Fillers < (options_dims[[m]][2] - checks - 1)) {
        dim_option <- as.vector(options_dims[[m]])
        dims_expt <- paste(dim_option[1], "x", dim_option[2], sep = " ")
        checked_dims[[n]] <- dims_expt
        b[n] <- i
        blocks_dims <- rbind(blocks_dims, c(i, dims_expt))
        n <- n + 1
      } 
    }
  }
  blocks_and_dims <- blocks_dims[-1,]
  if (!is.matrix(blocks_and_dims)) {
    blocks_and_dims <- matrix(data = blocks_and_dims, ncol = 2, byrow = TRUE)
  }
  return(list(b = b, 
              option_dims = checked_dims, 
              blocks_dims = blocks_and_dims))
}