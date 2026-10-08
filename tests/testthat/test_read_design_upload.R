library(FielDHub)

write_upload <- function(lines, fileext = ".csv") {
  path <- tempfile(fileext = fileext)
  writeLines(lines, path, useBytes = TRUE)
  path
}

write_upload_bom <- function(lines) {
  path <- tempfile(fileext = ".csv")
  con <- file(path, "wb")
  writeBin(charToRaw(paste0("\xef\xbb\xbf", paste(lines, collapse = "\n"), "\n")), con)
  close(con)
  path
}

test_that("read_design_upload() parses comma, semicolon and tab uploads", {
  for (sep in c(",", ";", "\t")) {
    lines <- gsub(",", sep, c("ENTRY,NAME", "1,G1", "2,G2"), fixed = TRUE)
    out <- read_design_upload(write_upload(lines), sep, "alpha")
    expect_named(out, "data")
    expect_equal(out$data$ENTRY, 1:2)
    expect_equal(out$data$NAME, c("G1", "G2"))
  }
})

test_that("read_design_upload() strips a UTF-8 BOM from the first column name", {
  path <- write_upload_bom(c("ENTRY,NAME", "1,G1", "2,G2"))
  out <- read_design_upload(path, ",", "alpha")
  expect_identical(names(out$data), c("ENTRY", "NAME"))
})

test_that("read_design_upload() accepts extra columns beyond the design's own", {
  lines <- c("ENTRY,NAME,EXTRA", "1,G1,x", "2,G2,y")
  out <- read_design_upload(write_upload(lines), ",", "alpha")
  expect_identical(names(out$data), c("ENTRY", "NAME", "EXTRA"))
})

test_that("read_design_upload() ignores trailing blank lines", {
  lines <- c("ENTRY,NAME", "1,G1", "2,G2", "", "")
  out <- read_design_upload(write_upload(lines), ",", "alpha")
  expect_equal(nrow(out$data), 2)
})

test_that("read_design_upload() accepts a single data row", {
  out <- read_design_upload(write_upload(c("ENTRY,NAME", "1,G1")), ",", "alpha")
  expect_equal(nrow(out$data), 1)
})

test_that("read_design_upload() raises a classed error for an empty file", {
  err <- tryCatch(
    read_design_upload(write_upload(character(0)), ",", "alpha",
                       missing_columns = "Use ENTRY and NAME"),
    error = function(e) e
  )
  expect_s3_class(err, "fieldhub_input_error")
  expect_identical(conditionMessage(err), "Invalid file; Please upload a .csv file.")
})

test_that("read_design_upload() raises a classed error for ragged rows", {
  lines <- c("ENTRY,NAME", "1,G1", "2")
  err <- tryCatch(read_design_upload(write_upload(lines), ",", "alpha"),
                  error = function(e) e)
  expect_s3_class(err, "fieldhub_input_error")
  expect_identical(conditionMessage(err), "Invalid file; Please upload a .csv file.")
})

test_that("read_design_upload() raises the design's own message for missing columns", {
  lines <- c("ENTRY", "1", "2")
  err <- tryCatch(
    read_design_upload(write_upload(lines), ",", "alpha",
                       missing_columns = "Data input needs at least two columns: ENTRY and NAME"),
    error = function(e) e
  )
  expect_s3_class(err, "fieldhub_input_error")
  expect_identical(conditionMessage(err),
                   "Data input needs at least two columns: ENTRY and NAME")
})

test_that("read_design_upload() raises a classed error for duplicate values", {
  lines <- c("TREATMENT", "T1", "T1", "T2")
  err <- tryCatch(
    read_design_upload(write_upload(lines), ",", "crd",
                       missing_columns = "Use TREATMENT"),
    error = function(e) e
  )
  expect_s3_class(err, "fieldhub_input_error")
  expect_identical(conditionMessage(err), "Check input file for duplicate values.")
})

test_that("read_design_upload() rejects a non-CSV file by extension", {
  path <- write_upload(c("ENTRY,NAME", "1,G1"), fileext = ".txt")
  err <- tryCatch(read_design_upload(path, ",", "alpha", name = "entries.txt"),
                  error = function(e) e)
  expect_s3_class(err, "fieldhub_input_error")
  expect_identical(conditionMessage(err), "Invalid file; Please upload a .csv file.")
})

test_that("read_design_upload() covers each column-count rule set", {
  one_column <- read_design_upload(write_upload(c("TREATMENT", "T1", "T2")), ",", "crd")
  expect_named(one_column$data, "TREATMENT")

  three_columns <- read_design_upload(
    write_upload(c("ROW,COLUMN,TREATMENT", "1,1,A", "2,2,B")), ",", "lsd"
  )
  expect_named(three_columns$data, c("ROW", "COLUMN", "TREATMENT"))

  paired <- read_design_upload(
    write_upload(c("FACTOR,LEVEL", "A,a0", "A,a1", "B,b0")), ",", "factorial"
  )
  expect_named(paired$data, c("FACTOR", "LEVEL"))
})

test_that("read_design_upload() returns list(data = <data.frame>) on success", {
  out <- read_design_upload(write_upload(c("ENTRY,NAME", "1,G1")), ",", "alpha")
  expect_type(out, "list")
  expect_named(out, "data")
  expect_s3_class(out$data, "data.frame")
})
