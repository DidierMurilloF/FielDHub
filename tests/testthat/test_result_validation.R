library(FielDHub)

test_that("result validation reports malformed objects through classed conditions", {
  for (x in list(1, "design", TRUE, new.env(parent = emptyenv()))) {
    expect_error(FielDHub:::validate_fieldhub_design(x),
                 "not a FielDHub list", class = "fieldhub_internal_error")
  }
})

test_that("result metadata requires one nonmissing nonempty design name", {
  design <- RCBD(t = 4, reps = 2, seed = 1)
  for (name in list(NA_character_, "", character(), c("rcbd", "crd"))) {
    bad <- design
    bad$metadata$design <- name
    class(bad) <- c(paste0("fieldhub_", name), "FielDHub")
    expect_error(FielDHub:::validate_fieldhub_design(bad),
                 "no metadata naming the design", class = "fieldhub_internal_error")
  }
})

test_that("the result constructor rejects malformed inputs before accessing fields", {
  for (x in list(1, list(infoDesign = 1), list())) {
    expect_error(FielDHub:::new_fieldhub_design(x, "rcbd"),
                 "infoDesign", class = "fieldhub_internal_error")
  }
})
