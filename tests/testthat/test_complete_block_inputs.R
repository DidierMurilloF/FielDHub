test_that("RCBD counts and labels fail with input conditions before allocation", {
  for (bad in list(NULL, numeric(), NA_real_, NaN, Inf, -Inf, 0, -1, 2.5, TRUE,
                   list(1, 2), matrix(3), c(2, 3), c("A", NA), c("A", " "))) {
    set.seed(718)
    before <- .Random.seed
    expect_error(RCBD(t = bad, reps = 2, seed = 9), class = "fieldhub_input_error",
                  info = paste(deparse(bad), collapse = " "))
    expect_identical(.Random.seed, before)
  }
  for (bad in list(0, NA_real_, NaN, Inf, -Inf, 2.5, matrix(3))) {
    expect_error(RCBD(t = bad, checks = "CK", rep_checks = 1, reps = 2, seed = 9),
                  class = "fieldhub_input_error")
  }
})

test_that("complete-block entry tables have usable columns and labels", {
  for (checks in list(NULL, "CK")) {
    for (bad in list(data.frame(), data.frame(T = character()),
                     data.frame(T = c(NA, NA)), data.frame(T = c("CK", " ")))) {
      expect_error(RCBD(data = bad, reps = 2, checks = checks, rep_checks = 1, seed = 9),
                    class = "fieldhub_input_error")
    }
  }
  for (bad in list(data.frame(), data.frame(R = 1:3), data.frame(R = 1:3, C = 1:3),
                   data.frame(R = 1, C = 1, T = "A"),
                   data.frame(R = numeric(), C = numeric(), T = character()),
                   data.frame(R = c(NA, NA), C = 1:2, T = c("a", "b")),
                   data.frame(R = 1:3, C = 1:3, T = c("a", " ", "b")))) {
    expect_error(latin_square(data = bad, seed = 9), class = "fieldhub_input_error")
  }
})

test_that("Latin-square treatment counts are finite scalar whole numbers", {
  for (bad in list(NULL, numeric(), NA_real_, NaN, Inf, -Inf, 0, -1, 2.5, TRUE,
                   list(2), matrix(3), c(2, 3), "3")) {
    set.seed(718)
    before <- .Random.seed
    expect_error(latin_square(t = bad, seed = 9), class = "fieldhub_input_error")
    expect_identical(.Random.seed, before)
  }
})

test_that("RCBD checks and supplied labels follow the same input contract", {
  for (checks in list(c("CK", NA_character_), c("CK", " "), matrix(1))) {
    expect_error(RCBD(t = 3, reps = 2, checks = checks, rep_checks = 1, seed = 9),
                  class = "fieldhub_input_error")
  }
  expect_error(RCBD(data = data.frame(T = c("A", "A", "B")), reps = 2, seed = 9),
                class = "fieldhub_input_error")
})

test_that("field-book size validation rejects overflow without allocating", {
  expect_identical(validate_design_size(c(4, 2, 3)), 24)
  for (bad in list(c(4, .Machine$integer.max), c(4, Inf), c(4, NA_real_), numeric())) {
    expect_error(validate_design_size(bad), class = "fieldhub_input_error")
  }
})
