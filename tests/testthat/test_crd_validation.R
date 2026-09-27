test_that("CRD rejects invalid scalar counts before drawing a design", {
  invalid <- list(NA_real_, NaN, Inf, -Inf, 0, -1, 1.5, numeric(), c(2, 3), "2", TRUE)
  for (value in invalid) {
    expect_error(CRD(t = 3, reps = value, seed = 1), class = "fieldhub_input_error")
    expect_error(CRD(t = letters[1:3], reps = value, seed = 1), class = "fieldhub_input_error")
    expect_error(CRD(t = 3, reps = 2, plotNumber = value, seed = 1), class = "fieldhub_input_error")
  }
  for (value in list(NA_real_, Inf, numeric(), c(2, 3), TRUE, list(2), matrix(2))) {
    expect_error(CRD(t = value, reps = 2, seed = 1), class = "fieldhub_input_error")
  }
  expect_error(validate_crd_size(.Machine$integer.max, 2), class = "fieldhub_input_error")
})

test_that("CRD validates the selected input table without silently truncating replication", {
  for (reps in list(c(1.5, 2.5), c(0, 2), c(-1, 2), c(Inf, 2), c(TRUE, FALSE))) {
    data <- data.frame(TREATMENT = c("A", "B"), REP = reps)
    expect_error(CRD(data = data, seed = 1), class = "fieldhub_input_error")
  }
  expect_error(CRD(data = data.frame(TREATMENT = c("A", "A"), REP = c(2, 3)), seed = 1),
               "unique treatment labels", class = "fieldhub_input_error")
  expect_error(CRD(data = data.frame(TREATMENT = character(), REP = integer()), seed = 1),
               class = "fieldhub_input_error")
  expect_error(CRD(data = data.frame(TREATMENT = c("A", "B"), REP = c(NA, NA)), seed = 1),
               class = "fieldhub_input_error")
  expect_error(validate_crd_size(1, rep(.Machine$integer.max, 2)), class = "fieldhub_input_error")
})

test_that("CRD rejects missing and blank generated labels and malformed locations", {
  for (labels in list(c("A", NA), c("A", ""), c("A", "  "))) {
    expect_error(CRD(t = labels, reps = 2, seed = 1), class = "fieldhub_input_error")
  }
  for (location in list(c("A", "B"), character(), NA_character_, "", "  ", list("A"))) {
    expect_error(CRD(t = 3, reps = 2, locationNames = location, seed = 1),
                 class = "fieldhub_input_error")
  }
  set.seed(102)
  before <- .Random.seed
  expect_error(CRD(t = letters[1:3], reps = 1.5, seed = 2), class = "fieldhub_input_error")
  expect_identical(.Random.seed, before)
})
