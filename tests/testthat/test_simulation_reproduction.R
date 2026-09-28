recorded_spatial_test <- function(seed = 17) {
  book <- data.frame(ID = 1:6, LOCATION = "A", ROW = rep(1:2, each = 3),
                     COLUMN = rep(1:3, 2), ENTRY = c(1, 2, 1, 2, 3, 0))
  simulate_spatial_field_book(book, 2, 3, 0.4, 0.5, 10, 20, "HEIGHT", seed)
}

recorded_classic_test <- function(seed = 17) {
  book <- data.frame(ID = 1:6, LOCATION = "B", PLOT = 101:106,
                     TREATMENT = rep(letters[1:3], 2))
  simulate_classic_field_book(book, 10, 20, "HEIGHT", seed)
}

test_that("spatial simulations retain the exact input and effective parameters", {
  x <- recorded_spatial_test()
  expect_identical(x$metadata$model, "ar1xar1")
  expect_identical(x$metadata$schema_version, 1L)
  expect_identical(x$metadata$seed, 17L)
  expect_identical(x$metadata$parameters$seed, x$seed)
  expect_identical(x$metadata$parameters$response_name, "HEIGHT")
  expect_identical(x$metadata$rng_kind, RNGkind())
  expect_identical(x$metadata$package_version, as.character(utils::packageVersion("FielDHub")))
  expect_identical(x$input_field_book, x$field_book[names(x$input_field_book)])
  expect_named(x$metadata$parameters, c("nrows", "ncols", "correlation_x", "correlation_y",
                                       "min_value", "max_value", "response_name", "seed"))
})

test_that("both recorded simulation models replay without advancing the caller stream", {
  for (build in list(recorded_classic_test, recorded_spatial_test)) {
    for (seed in list(17, NULL, -1, 17.9)) {
      x <- build(seed)
      set.seed(39)
      before <- .Random.seed
      expect_identical(reproduce_simulation(x), x)
      expect_identical(.Random.seed, before)
    }
  }
})

test_that("simulation reconstruction validates metadata and never evaluates input expressions", {
  x <- recorded_spatial_test()
  for (bad in list(NULL, list(), within(x, rm(metadata)),
                   within(x, metadata$model <- "system"),
                   within(x, metadata$schema_version <- 99L),
                   within(x, metadata$parameters$seed <- 2L),
                   within(x, metadata$parameters$unknown <- TRUE))) {
    expect_error(reproduce_simulation(bad), class = "fieldhub_input_error")
  }
  ran <- FALSE
  x$metadata$parameters$min_value <- quote({ran <<- TRUE; 10})
  expect_error(reproduce_simulation(x), class = "fieldhub_input_error")
  expect_false(ran)
})

test_that("simulation reconstruction restores RNG settings on success and failure", {
  x <- recorded_spatial_test()
  previous <- RNGkind()
  on.exit(do.call(RNGkind, as.list(previous)), add = TRUE)
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  set.seed(67)
  before <- .Random.seed
  expect_identical(reproduce_simulation(x), x)
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind()[1], "L'Ecuyer-CMRG")
  x$metadata$rng_kind[1] <- "invalid"
  expect_error(reproduce_simulation(x), class = "fieldhub_input_error")
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind()[1], "L'Ecuyer-CMRG")
})

test_that("simulation reconstruction warns about changed package versions", {
  x <- recorded_classic_test()
  x$metadata$package_version <- "0.0.0"
  expect_warning(reproduce_simulation(x), class = "fieldhub_reproduction_warning")
})

test_that("spatial modules retain the simulation record without rewriting its output", {
  modules <- c("Diagonal", "diagonal_multiple", "sparse_allocation", "Optim",
                "RCBD_augmented", "pREPS", "multi_loc_preps")
  records <- function(code) {
    if (missing(code) || (!is.call(code) && !is.pairlist(code))) return(list())
    if (is.call(code) && identical(code[[1]], as.name("list")) &&
        "simulation" %in% names(code) && identical(code$simulation, as.name("simulation"))) {
      return(list(code))
    }
    unlist(lapply(as.list(code), records), recursive = FALSE)
  }
  for (module in modules) {
    expect_identical(sum(all.names(spatial_server_body(module)) == "app_spatial_workflow"), 1L)
  }
  expect_identical(sum(all.names(body(app_spatial_workflow)) == "spatial_workflow_book"), 1L)
  expect_length(records(body(spatial_workflow_book)), 1L)
  for (code in list(body(app_spatial_workflow), body(spatial_workflow_book))) {
    expect_false(grepl("simulation$field_book$ID <-", paste(deparse(code), collapse = " "),
                       fixed = TRUE))
  }
})
