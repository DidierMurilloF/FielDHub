test_that("location counts cannot carry matrix dimensions", {
  expect_error(validate_locations(matrix(2)), class = "fieldhub_input_error")
  expect_error(RCBD(t = 4, reps = 2, l = matrix(2), plotNumber = c(101, 1001), seed = 19),
               class = "fieldhub_input_error")
})

test_that("location label validation retains valid values and legacy length fallbacks", {
  values <- list(c("A", "B"), c(a = "A", b = "B"), 1:2, c(1, 2),
                 factor(c("A", "B"), levels = c("unused", "B", "A")))
  for (value in values) expect_identical(validate_location_labels(value, 2), value)
  expect_null(validate_location_labels(NULL, 2))
  expect_identical(validate_location_labels(c("A", "A"), 1), c("A", "A"))
  expect_error(validate_location_labels(c("A", "A"), 2), class = "fieldhub_input_error")
})

test_that("case-normalized location names remain distinct", {
  expect_error(RCBD(t = 4, reps = 2, l = 2, locationNames = c("a", "A"),
                    plotNumber = c(101, 1001), seed = 19), class = "fieldhub_input_error")
  expect_error(RCBD_augmented(lines = 40, checks = 4, b = 4, l = 2,
    locationNames = c("a", "A"), plotNumber = c(101, 1001), seed = 19), class = "fieldhub_input_error")
})

test_that("location labels are validated before constructing field books", {
  cases <- names(catalogue)[!duplicated(vapply(catalogue, `[[`, character(1), "fun"))]
  for (name in cases) {
    engine <- catalogue[[name]]$fun
    if (!"locationNames" %in% names(formals(get(engine)))) next
    args <- catalogue_design(name)$metadata$parameters
    locations <- if (is.null(args[["l"]])) 1 else args[["l"]]
    for (bad in list(rep(NA_character_, locations), rep("", locations), rep("  ", locations),
                     as.list(rep("A", locations)), matrix(rep("A", locations)))) {
      args$locationNames <- bad
      expect_error(suppressWarnings(do.call(engine, args)), class = "fieldhub_input_error",
                   info = paste(engine, paste(deparse(bad), collapse = " ")))
    }
  }
})

test_that("effective multi-location labels cannot collapse different fields", {
  cases <- names(catalogue)[!duplicated(vapply(catalogue, `[[`, character(1), "fun"))]
  for (name in cases) {
    engine <- catalogue[[name]]$fun
    if (!all(c("l", "locationNames") %in% names(formals(get(engine))))) next
    args <- catalogue_design(name)$metadata$parameters
    args$l <- max(2, args$l)
    args$plotNumber <- 101 + seq_len(args$l) * 1000
    args$locationNames <- rep("A", args$l)
    expect_error(suppressWarnings(do.call(engine, args)), class = "fieldhub_input_error", info = engine)
  }
})
