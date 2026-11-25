test_that("Shiny validates the trait mean and residual SD atomically", {
  expected <- list(mean = 100, residual_sd = 8, trait = "YIELD", spatial = FALSE)
  expect_identical(simulation_request(100, 8, "YIELD"), expected)
  expect_identical(simulation_request(100L, 8L, "Other", "plant height"),
                   modifyList(expected, list(trait = "plant height")))
  expect_identical(simulation_request(-3, 0, "HEIGHT")$mean, -3)
  set.seed(917)
  before <- .Random.seed
  for (bad in list(NULL, numeric(), NA_real_, NaN, Inf, -Inf, c(1, 2),
                   "1", TRUE, 1 + 1i, matrix(1))) {
    expect_error(simulation_request(bad, 8, "YIELD"), class = "fieldhub_input_error")
    expect_error(simulation_request(100, bad, "YIELD"), class = "fieldhub_input_error")
  }
  expect_error(simulation_request(100, -1, "YIELD"), class = "fieldhub_input_error")
  expect_identical(.Random.seed, before)
})

test_that("trait names are checked against choices and the field book", {
  book <- data.frame(ID = 1L, PLOT = 101L, LOCATION = "A", YIELD = 6)
  for (bad in list(NULL, character(), NA_character_, "", " ", "unknown", c("YIELD", "HEIGHT"))) {
    expect_error(simulation_request(100, 8, bad), class = "fieldhub_input_error")
  }
  for (bad in list(NULL, NA_character_, "", " ", 1, c("a", "b"), matrix("a"),
                   "ID", "PLOT", "LOCATION", "text")) {
    expect_error(simulation_request(100, 8, "Other", bad, field_book = book),
      class = "fieldhub_input_error")
  }
  expect_error(simulation_request(100, 8, "YIELD", field_book = book),
    class = "fieldhub_input_error")
})

test_that("spatial requests validate coordinates and correlations before submission", {
  book <- data.frame(LOCATION = c("A", "A"), ROW = 1:2, COLUMN = 1L)
  request <- simulation_request(100, 8, "HEIGHT", field_book = book,
                                spatial = list(rho_col = -.9, rho_row = .9))
  expect_identical(request$spatial$rho_col, -.9)
  expect_identical(request$spatial$rho_row, .9)
  expect_error(simulation_request(100, 8, "YIELD", spatial = TRUE), "physical ROW and COLUMN")
  expect_error(simulation_request(100, 8, "YIELD", field_book = book, spatial = list(rho_col = 1)),
    class = "fieldhub_input_error")
  expect_error(simulation_request(100, 8, "YIELD", field_book = book[c(1, 1), ], spatial = TRUE),
    "unique physical")
})

test_that("every design module shares atomic simulation settings", {
  for (entry in fieldhub_app_registry()) {
    # a spatial page of the generic module runs its workflow in
    # app_spatial_page() (spatial_server_body())
    code <- if (identical(entry$workflow_family, "spatial")) {
      spatial_server_body(entry$workflow)
    } else {
      body(get(entry$server, asNamespace("FielDHub")))
    }
    workflow <- paste0("app_", entry$workflow_family, "_workflow")
    expect_identical(sum(all.names(code) == workflow), 1L)
    code <- body(get(workflow, asNamespace("FielDHub")))
    expect_identical(sum(all.names(code) == "app_simulation_controls"), 1L)
  }
})
