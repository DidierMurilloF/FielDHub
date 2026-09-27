incomplete_reproduction_cases <- list(
  incomplete_blocks = list(t = 12, k = 4, reps = 2, seed = 38),
  alpha_lattice = list(t = 12, k = 4, reps = 2, seed = 38),
  square_lattice = list(t = 16, k = 4, reps = 2, seed = 38),
  rectangular_lattice = list(t = 12, k = 3, reps = 2, seed = 38),
  row_column = list(t = 12, nrows = 3, reps = 2, seed = 38)
)

for (engine in names(incomplete_reproduction_cases)) local({
  fun <- engine
  args <- incomplete_reproduction_cases[[fun]]
  test_that(paste(fun, "records reproducible generated and supplied entries"), {
    for (supplied in c(FALSE, TRUE)) {
      inputs <- args
      if (supplied) {
        inputs$data <- data.frame(ENTRY = seq_len(inputs$t) + 100L,
                                   NAME = paste0("Custom ", seq_len(inputs$t)))
      }
      x <- do.call(fun, inputs)
      expect_type(x$metadata$parameters, "list")
      expect_identical(do.call(fun, x$metadata$parameters), x)
    }
  })
})

test_that("recording replication aliases does not supply both spellings", {
  expect_warning(x <- incomplete_blocks(t = 12, k = 4, r = 2, seed = 38),
                   class = "fieldhub_deprecated_warning")
  expect_identical(x$metadata$parameters$reps, 2)
  expect_false("r" %in% names(x$metadata$parameters))
  expect_silent(replay <- do.call(incomplete_blocks, x$metadata$parameters))
  expect_identical(replay, x)
})
