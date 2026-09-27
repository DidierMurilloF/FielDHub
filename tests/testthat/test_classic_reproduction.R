test_that("CRD and RCBD results replay their evaluated inputs", {
  for (name in c("CRD_count", "CRD_labels", "CRD_data", "RCBD_two_locations",
                  "RCBD_checks", "RCBD_cartesian")) {
    x <- catalogue_design(name)
    expect_type(x$metadata$parameters, "list")
    expect_identical(x$metadata$parameters$seed, x$metadata$seed)
    replay <- do.call(get(catalogue[[name]]$fun), x$metadata$parameters)
    expect_identical(replay, x, info = name)
  }
})

test_that("RCBD records starting plots instead of expanded block plot numbers", {
  x <- RCBD(t = 5, reps = 3, l = 2, plotNumber = c(101, 1001), seed = 42)
  expect_identical(x$metadata$parameters$plotNumber, c(101, 1001))
  expect_identical(do.call(RCBD, x$metadata$parameters), x)
})

test_that("automatic seeds and caller data are sufficient for reconstruction", {
  entries <- data.frame(TREATMENT = c("B", "A", "C"), REPS = c(2, 3, 2))
  x <- CRD(data = entries)
  expect_identical(x$metadata$parameters$data$Treatment, entries$TREATMENT)
  expect_identical(x$metadata$parameters$data$Reps, entries$REPS)
  expect_identical(do.call(CRD, x$metadata$parameters), x)
  x <- RCBD(t = 5, reps = 3)
  expect_type(x$metadata$parameters$seed, "integer")
  expect_identical(do.call(RCBD, x$metadata$parameters), x)
})

test_that("recorded parameter lists are named and agree with the recorded seed", {
  x <- RCBD(t = 5, reps = 3, seed = 38)
  invalid <- list(1, list(38), list(seed = 39), list(seed = 38, seed = 38))
  for (parameters in invalid) {
    bad <- x
    bad$metadata$parameters <- parameters
    expect_error(validate_fieldhub_design(bad), class = "fieldhub_internal_error")
  }
})
