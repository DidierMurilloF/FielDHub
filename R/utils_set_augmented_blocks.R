set_augmented_blocks <- function(lines, checks, start = 5){
  if (lines > 40) div <- 3 else div <- 2
  blocks <- start:ceiling(lines/div)
  b <- vector(mode = "numeric")
  n <- 1
  for (i in blocks) {
    all_genotypes <- lines + checks * i
    plots_per_block <- base::ceiling(all_genotypes/i)
    lines_per_plot <- plots_per_block - checks
    excedent <- plots_per_block * i
    Fillers <- excedent - all_genotypes
    dim_block <- plots_per_block
    if (Fillers < (plots_per_block - checks - 2)) {
      b[n] <- i
      n <- n + 1
    } 
  }
  return(b)
}
set_augmented_blocks(lines = 55, checks = 3)
