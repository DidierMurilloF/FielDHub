# Square-lattice starts and the tuned search budget (spec-m5 section 6.4): when
# v = q^2 and blocks hold q plots, the first search starts from a square lattice.

lattice_concurrence <- function(treatments, q, b) {
  blocks <- rep(seq_len(b), each = q)
  N <- table(factor(treatments, levels = seq_len(q * q)), factor(blocks, levels = seq_len(b)))
  N <- matrix(as.integer(N), nrow(N))
  list(N = N, NNt = N %*% t(N))
}

test_that("the lattice start is a square lattice with random labels", {
  for (x in list(c(10, 2), c(10, 3), c(4, 3), c(7, 4), c(7, 8), c(5, 6))) {
    q <- x[1]; r <- x[2]
    set.seed(1)
    start <- lattice_start_probe_cpp(q * q, r, r * q)
    expect_length(start, q * q * r)
    m <- lattice_concurrence(start, q, r * q)
    expect_true(all(m$N <= 1))                          # q distinct treatments per block
    expect_true(all(rowSums(m$N) == r))                 # r copies of each treatment
    off <- m$NNt[upper.tri(m$NNt)]
    expect_true(all(off <= 1))                          # two treatments meet at most once
    expect_identical(sum(off), r * q * q * (q - 1) / 2) # every block pair is a new pair
  }
  set.seed(1); a <- lattice_start_probe_cpp(100, 3, 30)
  set.seed(2); b <- lattice_start_probe_cpp(100, 3, 30)
  expect_false(identical(a, b))
})

test_that("shapes that are not square lattices get no lattice start", {
  for (x in list(c(100, 3, 25), c(50, 3, 15), c(100, 1, 10), c(100, 4, 40), c(36, 4, 24),
                 c(49, 9, 63), c(1, 2, 2))) {
    set.seed(1)
    expect_length(lattice_start_probe_cpp(x[1], x[2], x[3]), 0)
    expect_identical(runif(1), {set.seed(1); runif(1)})  # and no random draws
  }
})

test_that("the engine reaches the lattice's D on lattice shapes", {
  skip_on_cran()
  # blocksdesign's D at these shapes, seeds 1-3 (m5/tuning-rule.md)
  for (sd in 1:3) {
    set.seed(sd)
    expect_gte(blocks_engine(100, 3, 30)$Blocks_model[["D-Efficiency"]], 0.8953135 - 1e-7)
    set.seed(sd)
    expect_gte(blocks_engine(100, 2, 20)$Blocks_model[["D-Efficiency"]], 0.8815913 - 1e-7)
  }
})

test_that("the default budget follows the tuned rule", {
  expect_identical(blocks_engine_default_budget(40, 20), 20 * 25 * 780)          # pairs <= 2000
  expect_identical(blocks_engine_default_budget(300, 20), 20 * 10 * 44850)       # full passes
  expect_identical(blocks_engine_default_budget(600, 20), 20 * 10 * 179700)
  expect_identical(blocks_engine_default_budget(1500, 20), 20 * 50 * 1500)       # sampled
  expect_identical(blocks_engine_default_budget(70, 20), 20 * 10 * 2415)         # 10 passes exceed the floor
  expect_identical(blocks_engine_default_budget(10, 20), 20 * 5000)              # floor
})

test_that("the default escape size follows the search mode", {
  expect_identical(blocks_engine_default_jumps(150), 3L)    # full passes
  expect_identical(blocks_engine_default_jumps(632), 3L)    # 199,396 pairs
  expect_identical(blocks_engine_default_jumps(633), 10L)   # sampled
})

test_that("full-pass searches reach or beat blocksdesign's D on sparse shapes", {
  skip_on_cran()
  skip_if_not(Sys.info()[["sysname"]] == "Linux", "search paths recorded on Linux")
  # blocksdesign 4.9 medians over seeds 1-3 (searches = 20, one BLAS thread);
  # the target is a case median within 1e-4.
  cases <- list(list(50, 3, 30, 0.7887762), list(50, 3, 50, 0.6231683), list(50, 3, 20, 0.8695983))
  for (k in cases) {
    d <- vapply(1:3, function(sd) {
      set.seed(sd)
      blocks_engine(k[[1]], k[[2]], k[[3]])$Blocks_model[["D-Efficiency"]]
    }, numeric(1))
    expect_gte(median(d), k[[4]] - 1e-4)
  }
})
