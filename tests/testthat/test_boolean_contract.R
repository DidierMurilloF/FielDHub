boolean_controls <- list(
  RCBD = c("continuous", "spread_checks"),
  full_factorial = c("continuous", "factorLabels"),
  split_plot = "factorLabels", split_split_plot = "factorLabels",
  strip_plot = c("factorLabels", "randomizeH", "randomizeV"),
  row_column = "latinize",
  partially_replicated = c("spread_reps", "multiLocationData", "allow_fillers"),
  RCBD_augmented = "random",
  diagonal_arrangement = c("multiLocationData", "sameEntries"),
  optimized_arrangement = "spread_reps",
  multi_location_prep = c("spread_reps", "allow_fillers"),
  do_optim = c("add_checks", "force_balance")
)

test_that("Boolean validation accepts only one nonmissing logical value", {
  expect_identical(validate_flag(TRUE, "flag"), TRUE)
  expect_identical(validate_flag(FALSE, "flag"), FALSE)
  expect_false(withVisible(validate_flag(TRUE, "flag"))$visible)
  for (bad in list(NULL, logical(), NA, c(TRUE, FALSE), 0, 1, "TRUE", list(TRUE),
                   matrix(TRUE), array(FALSE, 1))) {
    error <- tryCatch(validate_flag(bad, "flag"), error = identity)
    expect_s3_class(error, "fieldhub_input_error")
    expect_identical(error$argument, "flag")
    expect_identical(error$value, bad)
    expect_identical(error$options, c(FALSE, TRUE))
  }
})

test_that("every public Boolean control rejects missing values before design work", {
  for (engine in names(boolean_controls)) for (flag in boolean_controls[[engine]]) {
    entry <- names(catalogue)[vapply(catalogue, function(x) x$fun == engine, logical(1))][1L]
    arguments <- catalogue_design(entry)$metadata$parameters
    arguments[flag] <- list(NA)
    set.seed(817)
    before <- .Random.seed
    error <- tryCatch(do.call(get(engine), arguments), error = identity)
    expect_s3_class(error, "fieldhub_input_error")
    expect_identical(error$argument, flag)
    expect_identical(error$value, NA)
    expect_identical(error$options, c(FALSE, TRUE))
    expect_true(identical(.Random.seed, before))
  }
})

test_that("the Boolean contract covers every logical engine formal", {
  engines <- unique(unname(fieldhub_engine_registry()))
  discovered <- lapply(engines, function(engine) {
    arguments <- formals(get(engine))
    names(arguments)[vapply(arguments, function(value) {
      is.logical(value) && length(value) == 1L
    }, logical(1))]
  })
  names(discovered) <- engines
  discovered <- Filter(length, discovered)
  expect_identical(discovered, boolean_controls)
})
