test_that("factorial uploads allow the same level label in different factors", {
  entries <- data.frame(FACTOR = rep(c("N", "P"), each = 2L),
                         LEVEL = rep(c("Low", "High"), 2L))
  expect_true(check_input("factorial", entries))
  path <- tempfile(fileext = ".csv")
  utils::write.csv(entries, path, row.names = FALSE)
  expect_identical(load_file("entries.csv", path, ",", check = TRUE,
                             design = "factorial"), list(dataUp = entries))
  out <- full_factorial(reps = 2, l = 2, plotNumber = c(101, 1001),
                         seed = 38, data = entries)
  expect_equal(out$infoDesign$runs, 4L)
  expect_true(all(table(out$fieldBook$LOCATION, out$fieldBook$REP,
                        out$fieldBook$FACTOR_N, out$fieldBook$FACTOR_P) == 1L))
})

test_that("factorial generation rejects duplicate levels within one factor", {
  entries <- data.frame(FACTOR = c("N", "N", "P", "P"),
                         LEVEL = c("Low", "Low", "Low", "High"))
  expect_false(check_input("factorial", entries))
  set.seed(101)
  before <- .Random.seed
  expect_error(full_factorial(reps = 2, seed = 38, data = entries),
                "unique within each factor", class = "fieldhub_input_error")
  expect_identical(.Random.seed, before)
})

test_that("factorial input shape errors use the input condition contract", {
  for (entries in list(data.frame(FACTOR = "N"), data.frame())) {
    expect_error(full_factorial(reps = 2, seed = 38, data = entries),
                  "at least two columns", class = "fieldhub_input_error")
  }
  for (entries in list(data.frame(FACTOR = character(), LEVEL = character()),
                       data.frame(FACTOR = c("N", "P"), LEVEL = c(NA, NA)))) {
    expect_error(full_factorial(reps = 2, seed = 38, data = entries),
                  "at least one complete", class = "fieldhub_input_error")
  }
})

test_that("factorial uniqueness follows the complete rows used by the engine", {
  entries <- data.frame(FACTOR = c("N", "N", "P", "P", NA, NA),
                         LEVEL = c("Low", "High", "Low", "High", "X", "X"))
  expect_true(check_input("factorial", entries))
  out <- full_factorial(reps = 2, seed = 38, data = entries)
  expect_equal(out$infoDesign$runs, 4L)
})
