# Plain input validation and static module bindings; no Shiny sessions.
test_that("simulation requests validate all controls before returning settings", {
  expected <- list(min_value = 0, max_value = 20, response_name = "YIELD")
  expect_identical(simulation_request(0, 20, "YIELD"), expected)
  expect_identical(simulation_request(0L, 20L, "Other", "plant height"),
                   modifyList(expected, list(response_name = "plant height")))
  old <- expected
  expect_error(old <- simulation_request(1, 25, "Other", " "),
                class = "fieldhub_input_error")
  expect_identical(old, expected)
  set.seed(917)
  before <- .Random.seed
  simulation_request(0, 20, "YIELD")
  expect_identical(.Random.seed, before)
})

test_that("incomplete, nonfinite, or malformed simulation bounds are rejected", {
  for (bad in list(NULL, numeric(), NA_real_, NaN, Inf, -Inf, c(1, 2),
                   "1", TRUE, 1 + 1i, matrix(1))) {
    expect_error(simulation_request(bad, 20, "YIELD"), class = "fieldhub_input_error")
    expect_error(simulation_request(0, bad, "YIELD"), class = "fieldhub_input_error")
  }
  expect_error(simulation_request(1, 1, "YIELD"), class = "fieldhub_input_error")
  expect_error(simulation_request(2, 1, "YIELD"), class = "fieldhub_input_error")
})

test_that("trait names are checked against both choices and the current field book", {
  for (bad in list(NULL, character(), NA_character_, "", " ", "unknown", c("YIELD", "HEIGHT"))) {
    expect_error(simulation_request(0, 20, bad), class = "fieldhub_input_error")
  }
  for (bad in list(NULL, NA_character_, "", " \t", 1, c("a", "b"), matrix("a"))) {
    expect_error(simulation_request(0, 20, "Other", bad), class = "fieldhub_input_error")
  }
  for (name in c("ID", "PLOT", "LOCATION", "text")) {
    expect_error(simulation_request(0, 20, "Other", name,
                                   field_columns = c("ID", "PLOT", "LOCATION")),
                  class = "fieldhub_input_error")
  }
  expect_error(simulation_request(0, 20, "YIELD", field_columns = "YIELD"),
                class = "fieldhub_input_error")
})

test_that("spatial requests parse select values and reject invalid correlations", {
  expect_identical(simulation_request(0, 20, "HEIGHT", correlations = c(x = "0.4", y = "0.5")),
                   list(min_value = 0, max_value = 20, response_name = "HEIGHT",
                        correlation_x = 0.4, correlation_y = 0.5))
  for (bad in list(c(x = NA, y = .5), c(x = "bad", y = "0.5"), c(x = Inf, y = .5),
                   c(x = 1, y = .5), c(x = -.9, y = .9), c(.4, .5), list(x = .4, y = .5),
                   c(x = .4), c(x = .4, y = .5, z = .6))) {
    expect_error(simulation_request(0, 20, "YIELD", correlations = bad),
                  class = "fieldhub_input_error")
  }
  for (name in c("ZST", "genot")) {
    expect_error(simulation_request(0, 20, "Other", name, correlations = c(x = .4, y = .5)),
                  class = "fieldhub_input_error")
  }
})

test_that("every design module shares atomic simulation settings", {
  for (entry in fieldhub_app_registry()) {
    code <- body(get(entry$server, asNamespace("FielDHub")))
    if (!is.null(entry$workflow)) {
      expect_identical(sum(all.names(code) == "app_classic_workflow"), 1L)
      code <- body(app_classic_workflow)
    }
    expect_identical(sum(all.names(code) == "app_simulation_controls"), 1L)
  }
})
