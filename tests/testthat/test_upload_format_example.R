library(FielDHub)

test_that("upload_format_example() returns a data frame for every module upload key", {
  keys <- c("crd", "rcbd", "alpha", "rect", "rcd", "square", "mdiag", "sdiag",
            "sparse_allocation", "arcbd", "factorial", "ibd", "lsd",
            "multi_loc_prep", "optim", "prep", "spd", "sspd", "strip")
  for (key in keys) {
    example <- upload_format_example(key)
    expect_s3_class(example, "data.frame")
    expect_gt(nrow(example), 0L)
  }
})

test_that("upload_format_example() keeps each design's own column names", {
  expect_identical(names(upload_format_example("crd")), "TREATMENT")
  expect_identical(names(upload_format_example("rcbd")), "TREATMENT")
  expect_identical(names(upload_format_example("alpha")), c("ENTRY", "NAME"))
  expect_identical(names(upload_format_example("lsd")),
                   c("ROW", "COLUMN", "TREATMENT"))
  expect_identical(names(upload_format_example("factorial")),
                   c("FACTOR", "LEVEL"))
  expect_identical(names(upload_format_example("optim")),
                   c("ENTRY", "NAME", "REPS"))
  expect_identical(names(upload_format_example("prep")),
                   c("ENTRY", "NAME", "REPS"))
  expect_identical(names(upload_format_example("spd")),
                   c("WHOLEPLOT", "SUBPLOT"))
  expect_identical(names(upload_format_example("sspd")),
                   c("WHOLPLOT", "SUBPLOT", "SUB_SUBPLOT"))
  expect_identical(names(upload_format_example("strip")),
                   c("HPLOTS", "VPLOTS"))
})

test_that("upload_format_example() gives modules sharing a validation rule their own table", {
  multi_loc_prep <- upload_format_example("multi_loc_prep")
  sdiag <- upload_format_example("sdiag")
  sparse <- upload_format_example("sparse_allocation")

  expect_identical(nrow(multi_loc_prep), 10L)
  expect_identical(sdiag, sparse)
  expect_false(identical(multi_loc_prep, sdiag))
})

test_that("upload_format_example() raises a classed error for an unknown key", {
  expect_error(upload_format_example("not-a-design"), class = "fieldhub_input_error")
})
