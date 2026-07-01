library(FielDHub)

test_that("split_families() assigns entries for valid three-column data", {
  # Regression test: the na.omit() assignment to gen.list was placed after
  # stop() inside the `if (ncol(data) < 3)` block, so for valid three-column
  # input gen.list was never created and the function failed at
  # colnames(gen.list) with "object 'gen.list' not found". The assignment now
  # runs after the column-count guard.
  gen.list <- data.frame(
    ENTRY = 1:12,
    NAME = paste0("SB-", 1:12),
    FAMILY = rep(1:3, each = 4)
  )
  out <- split_families(l = 2, data = gen.list)
  expect_s3_class(out, "FielDHub")
  expect_equal(nrow(out$data_locations), 12)
})
