library(FielDHub)

write_upload <- function(lines) {
  path <- tempfile(fileext = ".csv")
  writeLines(lines, path)
  path
}

test_that("load_file() reads a well-formed CSV", {
  path <- write_upload(c("ENTRY,NAME", "1,G1", "2,G2"))
  out <- load_file(name = "entries.csv", path = path, sep = ",")
  expect_named(out, "dataUp")
  expect_equal(nrow(out$dataUp), 2)
})

test_that("load_file() reports a malformed CSV instead of failing", {
  # Regression test: read.csv() errors were not caught, so uploading a file
  # with more fields than column names, or an empty file, raised an error
  # inside the module's reactive and ended the user's session.
  ragged <- write_upload(c("ENTRY,NAME", "1,G1,extra,fields"))
  expect_identical(load_file(name = "ragged.csv", path = ragged, sep = ","),
                   list(bad_format = TRUE))
  empty <- write_upload(character(0))
  expect_identical(load_file(name = "empty.csv", path = empty, sep = ","),
                   list(bad_format = TRUE))
})

test_that("load_file() rejects files that are not CSV", {
  expect_identical(load_file(name = "entries.xlsx", path = "entries.xlsx", sep = ","),
                   list(bad_format = TRUE))
})
