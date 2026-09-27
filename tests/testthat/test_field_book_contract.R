library(FielDHub)

test_that("result validation requires the shared field-book keys", {
  design <- RCBD(t = 4, reps = 2, seed = 1)
  for (column in c("ID", "LOCATION", "PLOT")) {
    bad <- design
    bad$fieldBook[[column]] <- NULL
    expect_error(FielDHub:::validate_fieldhub_design(bad),
                 column, class = "fieldhub_internal_error")
  }
  bad <- design
  names(bad$fieldBook)[2] <- "ID"
  expect_error(FielDHub:::validate_fieldhub_design(bad),
               "duplicate column names", class = "fieldhub_internal_error")
})

test_that("field-book numeric keys must be finite numeric vectors", {
  design <- RCBD(t = 4, reps = 2, seed = 1)
  for (column in c("ID", "PLOT")) {
    for (values in list(rep("1", 8), rep(NA_real_, 8), rep(Inf, 8),
                        rep(1 + 1i, 8), matrix(1:8, ncol = 1))) {
      bad <- design
      bad$fieldBook[[column]] <- values
      expect_error(FielDHub:::validate_fieldhub_design(bad),
                   column, class = "fieldhub_internal_error")
    }
  }
})

test_that("field-book locations are nonmissing atomic identifiers", {
  design <- RCBD(t = 4, reps = 2, seed = 1)
  for (values in list(rep(NA_character_, 8), rep(list("A"), 8), matrix(1:8, ncol = 1))) {
    bad <- design
    bad$fieldBook$LOCATION <- values
    expect_error(FielDHub:::validate_fieldhub_design(bad),
                 "LOCATION", class = "fieldhub_internal_error")
  }
  # Additional user columns and established numeric/character location IDs
  # remain supported; validation does not coerce saved field-book types.
  design$fieldBook$LOCATION <- rep("A", 8)
  design$fieldBook$NOTES <- NA_character_
  expect_identical(FielDHub:::validate_fieldhub_design(design), design)
})
