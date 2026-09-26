library(FielDHub)

test_that("the app uses a lightweight viridis scale without the viridis package", {
  imports <- utils::packageDescription("FielDHub", fields = "Imports")
  packages <- trimws(sub("\\s*\\(.*$", "", strsplit(imports, ",")[[1L]]))

  expect_false("viridis" %in% packages)
  expect_true("ggplot2" %in% packages)
  expect_true("viridisLite" %in% packages)
})

test_that("the lightweight viridis scale preserves the existing heatmap palette", {
  points <- seq(0, 1, length.out = 11)
  expected <- c(
    "#440154", "#482575", "#414487", "#35608D", "#2A788E", "#21908D",
    "#22A884", "#43BE71", "#7AD151", "#BCDF27", "#FDE725"
  )

  expect_identical(fieldhub_viridis_scale()$palette(points), expected)
})
