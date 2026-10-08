library(FielDHub)

with_dimension_test_rng <- function(code) {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  })
  force(code)
}

test_that("single diagonal choices are feasible and ordered by squareness", {
  for (planter in c("serpentine", "cartesian")) {
    choices <- FielDHub:::diagonal_dimension_choices(
      lines = 105, checks = 1:4, planter = planter, minimum_extra = 0.11
    )
    expect_type(choices, "character")
    expect_gt(length(choices), 0)
    expect_identical(choices, c("11 x 11", "10 x 12", "12 x 10", "9 x 13",
                               "9 x 14", "8 x 15", "7 x 17", "7 x 18",
                               "6 x 20", "6 x 21", "5 x 24", "5 x 25"))
    dims <- do.call(rbind, strsplit(choices, " x ", fixed = TRUE))
    storage.mode(dims) <- "integer"
    difference <- abs(dims[, 1] - dims[, 2])
    expect_identical(difference, sort(difference))
    expect_true(all(dims[, 1] * dims[, 2] >= floor(105 * 1.11)))
    expect_true(all(dims[, 1] * dims[, 2] <= ceiling(105 * 1.20)))
    for (i in seq_len(nrow(dims))) {
      result <- FielDHub:::available_percent(
        n_rows = dims[i, 1], n_cols = dims[i, 2], checks = 1:4,
        Option_NCD = TRUE, kindExpt = "SUDC", planter_mov1 = planter,
        dim_data = 109, dim_data_1 = 105
      )
      expect_gt(nrow(result$dt), 0)
    }
  }
})

test_that("multiple diagonal choices accept both block orientations", {
  data <- data.frame(
    ENTRY = 1:124, NAME = paste0("E", 1:124),
    BLOCK = c(rep("ALL", 4), rep(1:3, c(30, 50, 40)))
  )
  for (stacked in c("By Row", "By Column")) {
    choices <- FielDHub:::diagonal_dimension_choices(
      lines = 120, checks = 1:4, kindExpt = "DBUDC", stacked = stacked,
      data = data
    )
    expect_type(choices, "character")
    expect_gt(length(choices), 0)
  }
})

test_that("dimension searches return an empty vector for fields too small", {
  expect_identical(
    FielDHub:::diagonal_dimension_choices(lines = 1, checks = 1:4),
    character()
  )
})

test_that("dimension searches preserve the absence of an RNG state", {
  with_dimension_test_rng({
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
    FielDHub:::diagonal_dimension_choices(80, 1:4)
    expect_false(exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
  })
})

test_that("dimension searches reject malformed inputs with core conditions", {
  for (lines in list(NULL, NA_real_, Inf, "105", c(105, 106), 0, 1.5)) {
    expect_error(FielDHub:::diagonal_dimension_choices(lines, checks = 1:4),
                 "lines", class = "fieldhub_input_error")
  }
  for (checks in list(NULL, NA_real_, Inf, "4", 0, c(1, 1), 1.5)) {
    expect_error(FielDHub:::diagonal_dimension_choices(105, checks),
                 "checks", class = "fieldhub_input_error")
  }
  expect_error(FielDHub:::diagonal_dimension_choices(105, 1:4, kindExpt = "bad"),
               "kindExpt", class = "fieldhub_input_error")
  expect_error(FielDHub:::diagonal_dimension_choices(105, 1:4, stacked = "bad"),
               "stacked", class = "fieldhub_input_error")
  expect_error(FielDHub:::diagonal_dimension_choices(105, 1:4, planter = "bad"),
               "planter", class = "fieldhub_input_error")
  expect_error(FielDHub:::diagonal_dimension_choices(105, 1:4, minimum_extra = 0.3),
               "minimum_extra", class = "fieldhub_input_error")
  expect_error(FielDHub:::diagonal_dimension_choices(105, 1:4, kindExpt = "DBUDC"),
               "data", class = "fieldhub_input_error")
})

test_that("searching dimensions does not consume random numbers", {
  with_dimension_test_rng({
    set.seed(28)
    before <- .Random.seed
    FielDHub:::diagonal_dimension_choices(80, 1:4)
    expect_identical(.Random.seed, before)
  })
})
