library(FielDHub)

test_that("full_factorial() keeps plots in their location when plotNumber repeats", {
  # Regression test: the RCBD field book of every location was sorted by PLOT
  # after the locations were stacked, and LOCATION was then assigned by
  # position. With the same starting plot in every location the rows of the
  # locations interleaved, giving duplicated (LOCATION, PLOT) pairs.
  fact <- full_factorial(
    setfactors = c(2, 2), reps = 2, l = 2, type = 2,
    plotNumber = c(101, 101), seed = 1
  )
  fb <- fact$fieldBook
  expect_equal(anyDuplicated(fb[, c("LOCATION", "PLOT")]), 0)
  expect_equal(as.vector(table(fb$LOCATION)), c(8L, 8L))
  expect_equal(fb$ID, seq_len(nrow(fb)))
})

test_that("full_factorial() keeps level labels that contain spaces", {
  # Regression test: treatment combinations were pasted with " " and split
  # again on " ", so a level such as "Low N" was split into two values and
  # the factor columns received the wrong labels.
  data_factorial <- data.frame(
    factor = c("N", "N", "P", "P"),
    level = c("Low N", "High N", "Low P", "High P")
  )
  fact <- full_factorial(reps = 2, l = 1, type = 2, seed = 1, data = data_factorial)
  fb <- fact$fieldBook
  expect_setequal(unique(fb$FACTOR_N), c("Low N", "High N"))
  expect_setequal(unique(fb$FACTOR_P), c("Low P", "High P"))
  expect_equal(fb$TRT_COMB, paste(fb$FACTOR_N, fb$FACTOR_P, sep = "*"))
})
