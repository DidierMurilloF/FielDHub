library(FielDHub)

test_that("CRD accepts the locationNames argument used by other designs", {
  canonical <- CRD(t = 4, reps = 2, locationNames = "FARGO", seed = 19)
  expect_identical(unique(as.character(canonical$fieldBook$LOCATION)), "FARGO")

  expect_warning(
    legacy <- CRD(t = 4, reps = 2, locationName = "FARGO", seed = 19),
    "locationNames", class = "fieldhub_deprecated_warning"
  )
  expect_identical(legacy, canonical)

  expect_warning(
    positional <- CRD(4, 2, 101, "FARGO", 19), class = "fieldhub_deprecated_warning"
  )
  expect_identical(positional, canonical)
})

test_that("CRD rejects both location-name spellings without choosing one", {
  expect_error(
    CRD(t = 4, reps = 2, locationName = "FARGO", locationNames = "CASSLETON"),
    "Supply only", class = "fieldhub_input_error"
  )
  expect_error(
    CRD(t = 4, reps = 2, locationName = NULL, locationNames = "FARGO"),
    "Supply only", class = "fieldhub_input_error"
  )
})

test_that("CRD keeps its default location and does not warn on canonical calls", {
  expect_silent(default <- CRD(t = 4, reps = 2, seed = 19))
  expect_silent(explicit <- CRD(t = 4, reps = 2, locationNames = NULL, seed = 19))
  expect_identical(default, explicit)
})
