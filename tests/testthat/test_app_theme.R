library(FielDHub)

test_that("the app keeps its Flatly Bootstrap 3 theme", {
  skip_if_not_installed("bslib")
  theme <- fieldhub_theme()

  expect_s3_class(theme, "bs_theme")
  expect_identical(bslib::theme_version(theme), "3")
  expect_true("bootswatch" %in% names(unclass(theme)$layers))
})
