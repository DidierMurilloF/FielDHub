# Get started

Install FielDHub from CRAN.

```r
install.packages("FielDHub")
FielDHub::run_app()
```

Create your first design in R.

```r
design <- FielDHub::diagonal_arrangement(
  nrows = 15, ncols = 20, lines = 270, checks = 4, seed = 1987
)
plot(design)
head(design$fieldBook)
```

Read the [diagonal arrangement guide](articles/diagonal_arrangement.md) and
[function reference](reference/diagonal_arrangement.md).
