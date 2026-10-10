# Test-only helpers for the augmented row-column engine (research M1 specification,
# section 10). Nothing here calls the engine in R/engine_augmented_row_column.R:
# the reference evaluator must stay an independent algorithm.

# A design fixture from tests/testthat/fixtures/augmented-row-column/ as an
# integer matrix. Lines starting with "#" carry provenance and are skipped.
arc_fixture <- function(name) {
  path <- test_path("fixtures", "augmented-row-column", name)
  values <- utils::read.csv(path, comment.char = "#", check.names = FALSE)
  matrix <- as.matrix(values)
  storage.mode(matrix) <- "integer"
  dimnames(matrix) <- NULL
  matrix
}
