library(FielDHub)

test_that("optimized field choices retain both orientations and their order", {
  expect_identical(FielDHub:::optimized_dimension_choices(120),
                   c("10 x 12", "12 x 10", "15 x 8", "8 x 15",
                     "6 x 20", "20 x 6", "5 x 24", "24 x 5",
                     "30 x 4", "4 x 30"))
  expect_identical(FielDHub:::optimized_dimension_choices(16), "4 x 4")
})

test_that("optimized field choices report an empty search without fake dimensions", {
  for (plots in c(1, 12, 13, 26)) {
    expect_identical(FielDHub:::optimized_dimension_choices(plots), character())
  }
})

test_that("optimized field choices validate their plot count", {
  for (plots in list(NULL, NA_real_, Inf, "120", c(100, 120), 0, -1, 3.5)) {
    expect_error(FielDHub:::optimized_dimension_choices(plots),
                 "plots", class = "fieldhub_input_error")
  }
})
