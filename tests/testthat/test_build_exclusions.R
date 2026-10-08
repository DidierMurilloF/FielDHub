test_that("local planning artifacts are excluded from source packages", {
  root <- testthat::test_path("..", "..")
  path <- file.path(root, ".Rbuildignore")
  if (!file.exists(path)) skip("Source packaging rules are not installed")
  patterns <- readLines(path)
  expect_true(any(vapply(patterns, grepl, logical(1), x = ".superpowers", ignore.case = TRUE)))
})
