write_structural_upload <- function(lines) {
  path <- tempfile(fileext = ".csv")
  writeLines(lines, path)
  path
}

test_that("uploads reject extra fields instead of inferring row names", {
  for (sep in c(",", ";", "\t")) {
    lines <- gsub(",", sep, c("ENTRY,NAME", "1,G1,extra", "2,G2,more"),
                  fixed = TRUE)
    path <- write_structural_upload(lines)
    expect_identical(load_file("entries.csv", path, sep), list(bad_format = TRUE))
  }
})

test_that("uploads reject short records instead of silently padding them", {
  for (bad_row in c(2L, 8L)) {
    lines <- c("ENTRY,NAME", paste0(seq_len(8L), ",G", seq_len(8L)))
    lines[bad_row] <- "99"
    expect_identical(load_file("entries.csv", write_structural_upload(lines), ","),
                     list(bad_format = TRUE))
  }
})

test_that("uploads require data records after the header", {
  expect_identical(load_file("entries.csv", write_structural_upload("ENTRY,NAME"), ","),
                   list(bad_format = TRUE))
})

test_that("structural checks preserve quoted records and explicit empty cells", {
  lines <- c("ENTRY,NAME", '1,"G,1"', '2,"G""2"', '3,"G',
             'three"', "4,", "", "5,G5")
  path <- write_structural_upload(lines)
  expect_identical(load_file("entries.csv", path, ","),
                   list(dataUp = utils::read.csv(path, na.strings = c("", " ", "NA"))))
  padded <- write_structural_upload(c("WHOLEPLOT,SUBPLOT", "W1,S1", "W2,S2", ",S3"))
  expect_named(load_file("entries.csv", padded, ",", check = TRUE, design = "spd"),
               "dataUp")
})

test_that("unterminated quoted uploads report one format error without parser warnings", {
  for (lines in list(c("ENTRY,NAME", '1,"unterminated'),
                     c("ENTRY,NAME", "1,G1", '2,"unterminated', "next line"))) {
    path <- write_structural_upload(lines)
    expect_warning(error <- tryCatch(read_design_upload(path, ",", "alpha"),
                                     error = identity), NA)
    expect_s3_class(error, "fieldhub_input_error")
    expect_identical(conditionMessage(error), "Invalid file; Please upload a .csv file.")
  }
})

test_that("complete CSV records do not require a final newline", {
  for (ending in c("", "\n", "\r\n")) {
    path <- tempfile(fileext = ".csv")
    writeBin(charToRaw(paste0('ENTRY,NAME\n1,"G,1"\n2,"G\ntwo"', ending)), path)
    expect_warning(out <- read_design_upload(path, ",", "alpha"), NA)
    expect_identical(out$data, data.frame(ENTRY = 1:2, NAME = c("G,1", "G\ntwo")))
  }
})
