library(FielDHub)

test_that("unsupported seed values give classed errors without coercion warnings", {
  for (seed in list(.Machine$integer.max + 1, -.Machine$integer.max - 1, 1e308, 1 + 1i)) {
    expect_warning(
      expect_error(RCBD(t = 4, reps = 2, seed = seed), class = "fieldhub_input_error"),
      NA
    )
  }
  # Preserve the real-valued seed convention: R truncates accepted values,
  # while the result continues to record the value supplied by the caller.
  for (seed in c(0, 1.5, -1.5, .Machine$integer.max + 0.5, -.Machine$integer.max - 0.5)) {
    design <- RCBD(t = 4, reps = 2, seed = seed)
    expect_identical(design$infoDesign$seed, seed)
    expect_identical(design$fieldBook, RCBD(t = 4, reps = 2, seed = trunc(seed))$fieldBook)
  }
})
