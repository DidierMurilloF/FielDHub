library(FielDHub)

replication_alias_cases <- list(
  incomplete_blocks = list(t = 12, k = 3),
  alpha_lattice = list(t = 12, k = 3),
  square_lattice = list(t = 16, k = 4),
  rectangular_lattice = list(t = 12, k = 3),
  row_column = list(t = 24, nrows = 6, iterations = 10),
  strip_plot = list(Hplots = 3, Vplots = 2)
)

test_that("complete replication uses reps across design families", {
  for (name in names(replication_alias_cases)) {
    fun <- get(name, asNamespace("FielDHub"))
    args <- c(replication_alias_cases[[name]], list(plotNumber = 101, seed = 29))
    expect_silent(canonical <- do.call(fun, c(args, list(reps = 2))))

    old <- if (name == "strip_plot") "b" else "r"
    legacy_args <- c(args, stats::setNames(list(2), old))
    expect_warning(legacy <- do.call(fun, legacy_args),
                   "reps", class = "fieldhub_deprecated_warning")
    expect_identical(legacy, canonical, info = name)

    positional_args <- c(unname(args[1:2]), list(2), args[-(1:2)])
    expect_warning(positional <- do.call(fun, positional_args),
                   "reps", class = "fieldhub_deprecated_warning")
    expect_identical(positional, canonical, info = name)
  }
})

test_that("replication aliases reject ambiguous calls before generation", {
  for (name in names(replication_alias_cases)) {
    fun <- get(name, asNamespace("FielDHub"))
    old <- if (name == "strip_plot") "b" else "r"
    args <- c(replication_alias_cases[[name]], list(reps = 2),
              stats::setNames(list(3), old))
    expect_error(do.call(fun, args), "Supply only", class = "fieldhub_input_error")
  }
})
