test_that("upload column rules describe every legacy design alias", {
  two_columns <- c("sdiag", "mdiag", "optim", "arcbd", "prep", "square",
                    "rect", "alpha", "ibd", "rcd", "factorial", "spd", "strip")
  for (design in c(two_columns, "crd", "rcbd", "lsd", "sspd")) {
    rule <- upload_validation_rule(design)
    expected <- if (design %in% c("crd", "rcbd")) 1L else {
      if (design %in% c("lsd", "sspd")) 1:3 else 1:2
    }
    expect_identical(rule$columns, expected, info = design)
    expect_identical(rule$omit_na, design %in% c("spd", "sspd", "strip"),
                     info = design)
    expect_identical(rule$paired, design == "factorial", info = design)
  }
})

test_that("incomplete upload validation requests return the missing-columns status", {
  data <- data.frame(ENTRY = 1:2, NAME = c("A", "B"))
  for (design in list(NULL, NA_character_, character(), c("crd", "rcbd"),
                      1, "unknown")) {
    expect_null(check_input(design, data))
  }
  expect_null(check_input("crd", NULL))
  expect_null(check_input("crd", 1:3))
})

test_that("load_file validates each parsed upload only once", {
  calls <- 0L
  loader <- load_file
  environment(loader) <- list2env(list(check_input = function(design, dataIn) {
    calls <<- calls + 1L
    TRUE
  }), parent = environment(load_file))
  path <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(ENTRY = 1:2, NAME = c("A", "B")),
                    path, row.names = FALSE)
  expect_named(loader("entries.csv", path, ",", check = TRUE, design = "sdiag"),
                 "dataUp")
  expect_identical(calls, 1L)
})
