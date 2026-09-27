test_that("split and strip factor controls are complete scalar counts or label vectors", {
  specifications <- list(split_plot = list(wp = 3, sp = 2),
    split_split_plot = list(wp = 3, sp = 2, ssp = 2), strip_plot = list(Hplots = 3, Vplots = 2))
  bad_values <- list(NULL, numeric(), NA_real_, NaN, Inf, -Inf, 0, -1, 2.5, TRUE,
    list(1, 2), matrix(2), c("A", NA), c("A", " "), c("A", "A"))
  for (engine in names(specifications)) {
    for (parameter in names(specifications[[engine]])) for (bad in bad_values) {
      args <- c(specifications[[engine]], list(reps = 2, plotNumber = 101, seed = 19))
      args[parameter] <- list(bad)
      set.seed(718)
      before <- .Random.seed
      expect_error(do.call(engine, args), class = "fieldhub_input_error",
                    info = paste(engine, parameter, paste(deparse(bad), collapse = " ")))
      expect_identical(.Random.seed, before)
    }
  }
})

test_that("full factorial level counts are complete finite positive whole numbers", {
  for (bad in list(NULL, numeric(), c(NA, 2), c(NaN, 2), c(Inf, 2), c(-Inf, 2),
                   c(0, 2), c(-1, 2), c(2.5, 2), c(TRUE, FALSE), list(2, 3),
                   matrix(c(2, 3)), rep(1, 27))) {
    expect_error(full_factorial(setfactors = bad, reps = 2, seed = 19),
                  class = "fieldhub_input_error")
  }
})

test_that("split and strip tables need complete usable level columns", {
  for (engine in c("split_plot", "split_split_plot", "strip_plot")) {
    columns <- if (engine == "split_split_plot") 3L else 2L
    for (bad in list(data.frame(), data.frame(A = 1:3),
                     as.data.frame(rep(list(character()), columns)),
                     as.data.frame(rep(list(c(NA, NA)), columns)),
                     as.data.frame(rep(list(c("A", "A", "B")), columns)),
                     as.data.frame(rep(list(c("A", " ")), columns)))) {
      expect_error(do.call(engine, list(data = bad, reps = 2, seed = 19, plotNumber = 101)),
                    class = "fieldhub_input_error", info = engine)
    }
  }
})

test_that("factor count products are bounded before any expansion", {
  expect_identical(validate_factor_counts(c(2, 3), 2, 1), c(2, 3))
  expect_error(validate_factor_counts(c(2^20, 2^20), 2, 1), class = "fieldhub_input_error")
})

test_that("supplied full-factorial names and levels cannot be blank", {
  for (column in c("FACTOR", "LEVEL")) {
    entries <- data.frame(FACTOR = rep(c("N", "P"), each = 2),
                          LEVEL = rep(c("low", "high"), 2))
    entries[[column]][1] <- " "
    expect_error(full_factorial(data = entries, reps = 2, seed = 19), class = "fieldhub_input_error")
  }
})

test_that("count and label forms can be combined consistently across factors", {
  specifications <- list(
    split_plot = list(wp = c(3, 5), sp = 2),
    split_split_plot = list(wp = c("A", "B"), sp = c(3, 5), ssp = 2),
    strip_plot = list(Hplots = c("A", "B"), Vplots = 2))
  for (engine in names(specifications)) {
    out <- tryCatch(do.call(engine, c(specifications[[engine]],
      list(reps = 2, seed = 19, plotNumber = 101))), error = identity)
    expect_false(inherits(out, "error"), info = engine)
    if (inherits(out, "error")) next
    factors <- switch(engine, split_plot = c("WHOLE_PLOT", "SUB_PLOT"),
      split_split_plot = c("WHOLE_PLOT", "SUB_PLOT", "SUB_SUB_PLOT"), strip_plot = c("HSTRIP", "VSTRIP"))
    expect_true(all(table(out$fieldBook[c("REP", factors)]) == 1L), info = engine)
    expect_identical(nrow(out$fieldBook), as.integer(2^(length(factors) + 1L)))
    expect_identical(do.call(engine, out$metadata$parameters), out)
  }
})
