test_that("engine draws reproduce sample.int under the same seed", {
  set.seed(1)
  x <- engine_draw_indices_cpp(10L, 1000L)
  set.seed(1)
  expect_identical(x, sample.int(10L, 1000L, replace = TRUE))
})

test_that("engine draws follow a non-default sample.kind", {
  old <- RNGkind()
  on.exit(suppressWarnings(RNGkind(old[1], old[2], old[3])), add = TRUE)
  suppressWarnings(RNGkind(sample.kind = "Rounding"))
  set.seed(2)
  x <- engine_draw_indices_cpp(7L, 500L)
  set.seed(2)
  expect_identical(x, sample.int(7L, 500L, replace = TRUE))
})

test_that("block-space ratios match the eigenvalue reference", {
  set.seed(11)
  checked <- 0L
  for (case in seq_len(200)) {
    v <- sample(4:25, 1); b <- sample(2:8, 1); r <- sample(1:4, 1)
    if (v * r < v + b - 1 || v * r < 2 * b) next
    N <- engine_ref_random_counts(v, r, b)
    if (engine_ref_efficiency(N, r)[["rank"]] < v - 1) next
    s <- engine_ref_random_swap(N)
    probe <- block_space_probe_cpp(as.integer(N), v, b, r, as.integer(s), 0L)
    expected <- engine_ref_ratio(N, s[1], s[2], s[3], s[4])
    expect_true(probe$connected)
    expect_lte(abs(probe$ratios - expected) / max(1, expected), 1e-8)
    checked <- checked + 1L
  }
  expect_gte(checked, 100L)
})

test_that("disconnecting swaps have a ratio of at most 1e-9", {
  set.seed(12)
  found <- 0L
  for (case in seq_len(3000)) {
    v <- sample(3:6, 1); b <- sample(2:4, 1); r <- sample(1:2, 1)
    if (v * r < v + b - 1 || v * r < 2 * b) next
    N <- engine_ref_random_counts(v, r, b)
    if (engine_ref_efficiency(N, r)[["rank"]] < v - 1) next
    s <- engine_ref_random_swap(N)
    if (engine_ref_ratio(N, s[1], s[2], s[3], s[4]) > 0) next
    probe <- block_space_probe_cpp(as.integer(N), v, b, r, as.integer(s), 0L)
    expect_lte(probe$ratios, 1e-9)
    expect_identical(probe$applied, 0L)
    found <- found + 1L
  }
  expect_gte(found, 20L)
})

test_that("malformed or invalid swaps are skipped, not applied", {
  N <- matrix(c(1L, 1L, 0L, 0L, 1L, 1L, 1L, 0L, 1L), 3, 3)
  bad <- c(4L, 1L, 1L, 2L,  1L, 7L, 2L, 2L,  1L, 1L, 1L, 2L,  3L, 1L, 1L, 2L)
  probe <- block_space_probe_cpp(as.integer(N), 3L, 3L, 2L, bad, 0L)
  expect_true(probe$connected)
  expect_true(all(is.na(probe$ratios)))
  expect_identical(probe$applied, 0L)
  expect_identical(probe$counts, as.integer(N))
})

test_that("a disconnected start reports NA ratios and no efficiencies", {
  N <- matrix(c(2L, 0L, 0L, 2L), 2, 2)
  probe <- block_space_probe_cpp(as.integer(N), 2L, 2L, 2L, c(1L, 1L, 2L, 2L), 0L)
  expect_false(probe$connected)
  expect_true(is.na(probe$ratios))
  expect_true(is.na(probe$d_efficiency))
})

test_that("block-space efficiencies match the eigenvalue definitions", {
  set.seed(13)
  for (case in seq_len(100)) {
    v <- sample(4:30, 1); b <- sample(2:10, 1); r <- sample(1:6, 1)
    if (v * r < v + b - 1 || v * r < 2 * b) next
    N <- engine_ref_random_counts(v, r, b)
    ref <- engine_ref_efficiency(N, r)
    if (ref[["rank"]] < v - 1) next
    probe <- block_space_probe_cpp(as.integer(N), v, b, r, integer(), 0L)
    expect_lte(abs(probe$d_efficiency - ref[["D"]]), 1e-10)
    expect_lte(abs(probe$a_efficiency - ref[["A"]]), 1e-10)
  }
})

test_that("drift stays below 1e-8 over 10,000 accepted swaps with refreshes", {
  set.seed(14)
  v <- 40L; b <- 6L; r <- 3L
  N <- engine_ref_random_counts(v, r, b)
  swaps <- unlist(lapply(seq_len(100000), function(i) {
    trt <- sample.int(v, 2); blk <- sample.int(b, 2)
    c(trt[1], blk[1], trt[2], blk[2])  # (a, p, c, q)
  }))
  probe <- block_space_probe_cpp(as.integer(N), v, b, r, as.integer(swaps), 256L)
  expect_gte(probe$applied, 10000L)
  expect_lte(probe$drift, 1e-8)
  final <- matrix(probe$counts, v, b)
  expect_identical(rowSums(final), rep(as.double(r), v))
  expect_lte(abs(probe$d_efficiency - engine_ref_efficiency(final, r)[["D"]]), 1e-10)
})

test_that("the A-bound is valid and exact for the (7, 3, 1) BIBD", {
  set.seed(15)
  for (case in seq_len(200)) {
    v <- sample(4:30, 1); b <- sample(2:12, 1); r <- sample(1:6, 1)
    if (v * r < v + b - 1 || v * r < 2 * b) next
    N <- engine_ref_random_counts(v, r, b)
    ref <- engine_ref_efficiency(N, r)
    if (ref[["rank"]] < v - 1) next
    expect_lte(ref[["A"]], block_a_bound(v, r, colSums(N)) + 1e-10)
  }
  expect_equal(block_a_bound(7, 3, rep(3, 7)), 7 / 9, tolerance = 1e-12)
})
