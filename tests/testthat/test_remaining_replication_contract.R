# Counts are validated independently of generated versus supplied entry data.
test_that("classic engines require finite scalar replication counts on every data path", {
  specs <- list(
    RCBD = list(list(t = 4), list(data = data.frame(TREATMENT = LETTERS[1:4]))),
    latin_square = list(list(t = 4), list(data = data.frame(ROW = 1:4, COLUMN = 1:4, TREATMENT = LETTERS[1:4]))),
    full_factorial = list(list(setfactors = c(2, 3)), list(data = data.frame(FACTOR = c("A", "A", "B", "B"), LEVEL = c(1, 2, 1, 2)))),
    split_plot = list(list(wp = 3, sp = 2), list(data = data.frame(WP = c("A", "B"), SP = c("a", "b")))),
    split_split_plot = list(list(wp = 3, sp = 2, ssp = 2), list(data = data.frame(WP = c("A", "B"), SP = c("a", "b"), SSP = c("X", "Y")))),
    strip_plot = list(list(Hplots = 3, Vplots = 2), list(data = data.frame(H = c("A", "B"), V = c("a", "b"))))
  )
  invalid <- list(NULL, numeric(), NA_real_, NaN, Inf, -Inf, 0, -1, 1.5, "2", TRUE, c(2, 3), matrix(2))
  for (name in names(specs)) for (spec in specs[[name]]) for (bad in invalid) {
    args <- c(spec, list(reps = bad, seed = 41, plotNumber = 101))
    set.seed(764)
    before <- .Random.seed
    expect_error(suppressWarnings(do.call(name, args)), class = "fieldhub_input_error",
                 info = paste(name, is.null(spec$data), deparse(bad)))
    expect_identical(.Random.seed, before)
  }
})

test_that("factorial and split-plot type choices are numeric scalar alternatives", {
  specs <- list(full_factorial = list(setfactors = c(2, 3)),
                split_plot = list(wp = 3, sp = 2),
                split_split_plot = list(wp = 3, sp = 2, ssp = 2))
  for (name in names(specs)) for (bad in list(NULL, numeric(), NA_real_, NaN, Inf, -Inf,
                                             0, 3, 1.5, "2", TRUE, c(1, 2), matrix(2))) {
    expect_error(suppressWarnings(do.call(name, c(specs[[name]], list(reps = 2, type = bad, seed = 41)))),
                 class = "fieldhub_input_error", info = paste(name, deparse(bad)))
  }
})

test_that("row-column inputs share block count validation before optimization", {
  for (supplied in c(FALSE, TRUE)) {
    args <- list(t = 12, nrows = 3, reps = 2, seed = 41, iterations = 1)
    if (supplied) args$data <- data.frame(ENTRY = 1:12, TREATMENT = paste0("T", 1:12))
    for (parameter in c("t", "nrows", "reps")) {
      invalid <- list(NULL, numeric(), NA_real_, NaN, Inf, -Inf, 0, -1, 1.5, TRUE, matrix(2))
      if (parameter != "t") invalid <- c(invalid, list("2", c(2, 3)))
      for (bad in invalid) {
        candidate <- args
        candidate[parameter] <- list(bad)
        set.seed(764)
        before <- .Random.seed
        expect_error(suppressWarnings(do.call(row_column, candidate)), class = "fieldhub_input_error",
                     info = paste(parameter, supplied, deparse(bad)))
        expect_identical(.Random.seed, before)
      }
    }
  }
})
