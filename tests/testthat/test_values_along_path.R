library(FielDHub)

test_that("map extraction follows the planting path and preserves legacy types", {
  legacy <- function(map, path) {
    cells <- numeric()
    for (k in seq_len(nrow(path))) cells[k] <- map[path[k, "row"], path[k, "col"]]
    cells
  }
  for (rows in c(1, 2, 3, 10)) {
    for (cols in c(1, 2, 5)) {
      for (planter in c("cartesian", "serpentine")) {
        path <- FielDHub:::field_path(rows, cols, planter)
        map <- matrix(seq_len(rows * cols), nrow = rows)
        for (values in list(map, map / 3, map %% 2 == 0, matrix(as.character(map), rows))) {
          expect_identical(FielDHub:::values_along_path(values, path), legacy(values, path))
        }
      }
    }
  }
})

test_that("map extraction retains missing values and character filler labels", {
  map <- matrix(c("Filler", "CK1", NA_character_, "L4"), nrow = 2)
  path <- FielDHub:::field_path(2, 2, "serpentine")
  expect_identical(FielDHub:::values_along_path(map, path),
                   c("CK1", "L4", NA_character_, "Filler"))
  expect_identical(FielDHub:::values_along_path(map, path[FALSE, , drop = FALSE]), numeric())
})
