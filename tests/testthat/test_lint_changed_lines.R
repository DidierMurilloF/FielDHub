library(FielDHub)

lint_script <- system.file("ci/lint-changed-lines.R", package = "FielDHub")
lint_env <- new.env(parent = baseenv())
sys.source(lint_script, envir = lint_env)

test_that("changed-line linting follows the new side of every diff hunk", {
  patch <- c(
    "@@ -2,3 +2,4 @@",
    " context",
    "-old",
    "+replacement",
    "+addition",
    " context",
    "@@ -10,2 +11,2 @@",
    "-old",
    "+new"
  )

  expect_identical(lint_env$changed_lines_from_patch(patch), c(3L, 4L, 11L))
})

test_that("changed-line linting handles new and deleted files", {
  added <- c("@@ -0,0 +1,2 @@", "+first", "+second")
  deleted <- c("@@ -1,2 +0,0 @@", "-first", "-second")

  expect_identical(lint_env$changed_lines_from_patch(added), 1:2)
  expect_identical(lint_env$changed_lines_from_patch(deleted), integer())
})
