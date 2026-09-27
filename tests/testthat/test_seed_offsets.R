test_that("derived seeds retain ordinary values and wrap only beyond the supported range", {
  for (seed in list(1, -17, 3.7, -3.7, 1L, c(named = 3))) {
    for (offset in c(1L, 2L)) {
      expect_identical(offset_design_seed(seed, offset), seed + offset)
    }
  }
  expect_identical(offset_design_seed(.Machine$integer.max, 1L), -.Machine$integer.max)
  expect_identical(offset_design_seed(.Machine$integer.max + 0.5, 2L), -.Machine$integer.max + 1)
})

test_that("row-column location seeds cannot overflow at the public seed boundary", {
  for (method in c("onestage", "twostage")) {
    set.seed(27)
    previous <- .Random.seed
    expect_no_warning(design <- row_column(t = 12, nrows = 3, reps = 2, l = 2,
                                           seed = .Machine$integer.max, iterations = 1,
                                           plotNumber = c(101, 201), method = method))
    expect_identical(.Random.seed, previous)
    expect_identical(design$metadata$seed, .Machine$integer.max)
    expect_identical(reproduce_design(design), design)
  }
})
