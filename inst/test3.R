x <- list(c(row = 5, col = 10), c(row = 4, col = 7))
x
y <- as.vector(x[[1]])
paste(y[1], "x", y[2], sep = " ")


checks <- 4
lines <- 50
b <- 5
all_genotypes <- lines + checks * b
plots_per_block <- base::ceiling(all_genotypes / b)
dims <- FielDHub:::factor_subsets(plots_per_block, augmented = TRUE)$combos
default_dim <- c(1 * b, plots_per_block)
init_option <- paste(default_dim[1], "x", default_dim[2], sep = " ")
options_dims <- list(init_option)
options_dims
if (!is.null(dims)) {
    for (i in 1:length(dims)) {
    dim_option <- as.vector(dims[[i]] * c(b,1))
    dims_expt <- paste(dim_option[1], "x", dim_option[2], sep = " ")
    options_dims[[i + 1]] <- dims_expt 
    }
}
options_dims

repsExpt <- 1
nrows <- 10
b <- 5
B <- paste("Block", rep(rep(1:b, each = nrows / b), repsExpt), sep = "")
E <- paste("E", rep(repsExpt:1, each = nrows), sep = "")
paste(B,E)
rownames(df) <- paste(B,E)


