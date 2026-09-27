test_that("remaining design families replay their recorded parameters", {
  engines <- c("latin_square", "full_factorial", "split_plot", "split_split_plot",
               "strip_plot", "diagonal_arrangement", "optimized_arrangement",
               "RCBD_augmented", "partially_replicated", "multi_location_prep",
               "sparse_allocation", "split_families")
  for (name in names(catalogue)) {
    engine <- catalogue[[name]]$fun
    if (!engine %in% engines) next
    x <- catalogue_design(name)
    expect_type(x$metadata$parameters, "list")
    if (is.null(x$metadata$parameters)) next
    replay <- suppressWarnings(do.call(engine, x$metadata$parameters))
    expect_identical(replay, x, info = name)
    if ("year" %in% names(formals(get(engine)))) {
      expect_identical(x$metadata$parameters$year, "2026")
    }
  }
})

test_that("split plot replay retains label vectors and CRD/RCBD type", {
  for (engine in c("split_plot", "split_split_plot")) for (type in 1:2) {
    args <- list(wp = c("Dry", "Wet"), sp = c("Low", "High"),
                 type = type, reps = 2, seed = 38)
    if (engine == "split_split_plot") args$ssp <- c("Early", "Late")
    x <- do.call(engine, args)
    expect_identical(x$metadata$parameters$wp, args$wp)
    expect_identical(x$metadata$parameters$sp, args$sp)
    expect_identical(do.call(engine, x$metadata$parameters), x)
  }
})

test_that("factorial replay preserves generated versus supplied factor levels", {
  x <- full_factorial(setfactors = c(2, 3), reps = 2, seed = 38)
  expect_identical(x$metadata$parameters$setfactors, c(2, 3))
  expect_null(x$metadata$parameters$data)
  expect_identical(do.call(full_factorial, x$metadata$parameters), x)
})

test_that("augmented replay records full field dimensions and custom check entries", {
  x <- RCBD_augmented(lines = 50, checks = 3, b = 4, nrows = 2, ncols = 32,
                      seed = 38, year = 2026)
  expect_identical(x$metadata$parameters$ncols, 32)
  expect_identical(do.call(RCBD_augmented, x$metadata$parameters), x)
  x <- optimized_arrangement(nrows = 12, ncols = 10, lines = 100, rep_checks = 20,
                             checks = c(101, 102, 103, 104), seed = 38, year = 2026)
  expect_identical(x$metadata$parameters$checks, c(101, 102, 103, 104))
  expect_identical(do.call(optimized_arrangement, x$metadata$parameters), x)
})
