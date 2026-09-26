test_that("the catalogue covers every exported function", {
  namespace <- readLines(system.file("NAMESPACE", package = "FielDHub"))
  exports <- sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", namespace, value = TRUE))
  covered <- unique(vapply(catalogue, function(entry) entry$fun, character(1)))
  expect_setequal(covered, setdiff(exports, "run_app"))
})

test_that("every catalogue entry builds a design", {
  # Runs on every platform, including those where the golden snapshots are
  # skipped
  for (name in names(catalogue)) {
    expect_false(is.null(catalogue_design(name)), info = name)
  }
})
