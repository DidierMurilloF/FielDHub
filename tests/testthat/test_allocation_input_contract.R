test_that("allocation engines reject malformed line, copy and check counts", {
  specs <- list(
    do_optim = list(design = "sparse", lines = 20, l = 4, copies_per_entry = 3, add_checks = TRUE, checks = 2),
    sparse_allocation = list(lines = 120, l = 4, copies_per_entry = 3, checks = 4),
    multi_location_prep = list(lines = 80, l = 4, copies_per_entry = 5, checks = 2,
                               rep_checks = c(4, 4), allow_fillers = TRUE))
  for (engine in names(specs)) for (parameter in c("lines", "copies_per_entry", "checks")) {
    for (bad in list(numeric(), NA_real_, NaN, Inf, 0, -1, 2.5, "2", TRUE, list(2), matrix(2), c(2, 2))) {
      args <- c(specs[[engine]], list(seed = 19)); args[parameter] <- list(bad)
      set.seed(819); before <- .Random.seed
      expect_error(do.call(engine, args), class = "fieldhub_input_error",
                   info = paste(engine, parameter, paste(deparse(bad), collapse = " ")))
      expect_identical(.Random.seed, before)
    }
  }
})

test_that("p-rep allocation validates active check replications", {
  for (bad in list(numeric(), NA_real_, Inf, c(4, NA), c(4, .5), c(4, -1), "4", TRUE, matrix(c(4, 4)))) {
    expect_error(do_optim(design = "prep", lines = 20, l = 4, copies_per_entry = 5,
                          add_checks = TRUE, checks = 2, rep_checks = bad, seed = 1),
                 class = "fieldhub_input_error")
  }
})

test_that("p-rep averages must be finite positive numeric scalars", {
  for (bad in list(NULL, numeric(), NA_real_, Inf, 0, -1, "2", TRUE, matrix(2), c(2, 2))) {
    expect_error(multi_location_prep(lines = 80, l = 4, desired_avg = bad,
                                     seed = 1, allow_fillers = TRUE), class = "fieldhub_input_error")
  }
})

test_that("allocation uses its declared design default and rejects malformed choices", {
  args <- list(lines = 20, l = 4, copies_per_entry = 3, add_checks = TRUE, checks = 2, seed = 1)
  for (bad in list(NA_character_, c("sparse", "prep"), matrix("sparse"), list("sparse"))) {
    expect_error(do.call(do_optim, c(list(design = bad), args)), class = "fieldhub_input_error")
  }
  expect_identical(do.call(do_optim, args), do.call(do_optim, c(list(design = "sparse"), args)))
})

test_that("allocations report missing copies and infeasible block models as input errors", {
  for (design in c("sparse", "prep")) {
    expect_error(do_optim(design = design, lines = 20, l = 4, add_checks = TRUE,
                          checks = 2, seed = 1), class = "fieldhub_input_error")
  }
  expect_error(do_optim(design = "sparse", lines = 2, l = 10, copies_per_entry = 1,
                        add_checks = TRUE, checks = 2, seed = 1), class = "fieldhub_input_error")
})
