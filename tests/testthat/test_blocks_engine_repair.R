# The even-up repair (spec-m5 section 6.1): a budget-stopped search may return
# an uneven design; the engine evens it with deterministic swaps.

repair_layout_blocks <- function(v, r, b) {
  n <- v * r
  rep(seq_len(b), times = n %/% b + (seq_len(b) <= n %% b))
}
repair_counts <- function(treatments, v, r, b) {
  table(factor(treatments, levels = seq_len(v)),
        factor(repair_layout_blocks(v, r, b), levels = seq_len(b)))
}
repair_is_even <- function(N, r, b) {
  low <- r %/% b
  all(N >= low & N <= low + 1)
}
# An even engine design (30 entries, 3 copies, 4 blocks) made uneven: copies of
# entries 1-6 are moved into blocks that already hold them, by swaps that keep
# every margin and the connectivity
repair_unbalanced_design <- function() {
  set.seed(4)
  treatments <- as.integer(as.character(blocks_engine(30, 3, 4)$Design$treatments))
  blocks <- repair_layout_blocks(30L, 3L, 4L)
  for (a in 1:6) {
    held <- unique(blocks[treatments == a])
    x <- which(treatments == a & blocks == held[1])[1]
    y <- which(blocks == held[2] & treatments != a)[1]
    treatments[c(x, y)] <- treatments[c(y, x)]
  }
  treatments
}

test_that("a budget-stopped uneven design is evened (M1 row 300, 6, 5, seed 8)", {
  skip_on_cran()
  set.seed(8)
  e <- blocks_engine(300, 5, 6)
  N <- table(e$Design$treatments, e$Design$Level_1)
  expect_true(repair_is_even(N, 5, 6))
  ref <- engine_ref_efficiency(matrix(as.integer(N), nrow(N)), 5)
  expect_equal(e$Blocks_model[["D-Efficiency"]], ref[["D"]], tolerance = 1e-12)
  expect_equal(ref[["rank"]], 299)
  # The sampled search path that needs the repair here was recorded on Linux;
  # other platforms' floating point can take another path
  skip_if_not(Sys.info()[["sysname"]] == "Linux", "search path recorded on Linux")
  expect_gte(e$search$even_repairs, 1)
  expect_identical(e$search$stop_reason, "budget")
})

test_that("uneven app shapes come back even with blocksdesign's efficiency", {
  skip_on_cran()
  # Shapes the M5 spec review found uneven before the repair, with the
  # D-efficiency blocksdesign 4.9 reaches on them (7 printed decimals)
  cases <- list(c(50, 10, 19, 1, 0.9994906), c(100, 10, 19, 1, 0.9997479),
                c(100, 8, 15, 3, 0.9996851))
  runs <- lapply(cases, function(x) {
    set.seed(x[4])
    blocks_engine(x[1], x[3], x[2])
  })
  for (k in seq_along(cases)) {
    N <- table(runs[[k]]$Design$treatments, runs[[k]]$Design$Level_1)
    expect_true(repair_is_even(N, cases[[k]][3], cases[[k]][2]))
  }
  # The per-seed search paths (and so the repair counts and single-seed D) were
  # recorded on Linux
  skip_if_not(Sys.info()[["sysname"]] == "Linux", "search path recorded on Linux")
  for (k in seq_along(cases)) {
    expect_gte(runs[[k]]$search$even_repairs, 1)
    expect_gte(runs[[k]]$Blocks_model[["D-Efficiency"]], cases[[k]][5] - 1e-7)
  }
})

test_that("the repair evens a connected uneven design and keeps its margins", {
  v <- 30L; r <- 3L; b <- 4L
  treatments <- repair_unbalanced_design()
  N0 <- repair_counts(treatments, v, r, b)
  expect_false(repair_is_even(N0, r, b))
  expect_equal(engine_ref_efficiency(matrix(as.integer(N0), v), r)[["rank"]], v - 1)
  out <- even_repair_probe_cpp(treatments, v, r, b)
  N <- repair_counts(out$treatments, v, r, b)
  expect_true(repair_is_even(N, r, b))
  expect_identical(as.vector(rowSums(N)), as.vector(rowSums(N0)))
  expect_identical(as.vector(colSums(N)), as.vector(colSums(N0)))
  # Each swap lowers the sum of squared counts by at least 2
  expect_gt(out$swaps, 0L)
  expect_lte(out$swaps, (sum(N0^2) - sum(N^2)) / 2)
  expect_equal(engine_ref_efficiency(matrix(as.integer(N), v), r)[["rank"]], v - 1)
})

test_that("the repair refuses a disconnected design instead of guessing", {
  # The engine only ever repairs connected designs; a disconnected input
  # (every entry's copies side by side) is reported, not evened
  out <- even_repair_probe_cpp(rep(1:12, each = 5), 12L, 5L, 4L)
  expect_identical(out$swaps, -1L)
})

test_that("an even design passes through the repair unchanged", {
  set.seed(4)
  e <- blocks_engine(30, 3, 4)
  treatments <- as.integer(as.character(e$Design$treatments))
  out <- even_repair_probe_cpp(treatments, 30L, 3L, 4L)
  expect_identical(out$swaps, 0L)
  expect_identical(out$treatments, treatments)
  expect_identical(e$search$even_repairs, 0L)
})

test_that("the repair draws no random numbers and is deterministic", {
  treatments <- repair_unbalanced_design()
  expect_false(repair_is_even(repair_counts(treatments, 30L, 3L, 4L), 3L, 4L))
  set.seed(1)
  before <- .Random.seed
  a <- even_repair_probe_cpp(treatments, 30L, 3L, 4L)
  expect_identical(.Random.seed, before)
  b <- even_repair_probe_cpp(treatments, 30L, 3L, 4L)
  expect_identical(a, b)
})

test_that("a search stopped right after its random start is still returned even", {
  # A budget just above the start's cost stops the search after a few swaps,
  # leaving a near-random shuffle for the repair (spec-m5 section 6.1)
  set.seed(2)
  e <- blocks_engine(200, 7, 5, budget = 200 * 7 + 5 + 50)
  N <- table(e$Design$treatments, e$Design$Level_1)
  expect_true(repair_is_even(N, 7, 5))
  expect_identical(e$search$stop_reason, "budget")
  expect_gt(e$search$even_repairs, 10)
  # Each repair swap lowers the sum of squared counts by at least 2, and no
  # design has a larger sum than every copy of each entry in one block
  expect_lte(e$search$even_repairs, (200 * 7^2 - sum(N^2)) / 2)
  ref <- engine_ref_efficiency(matrix(as.integer(N), nrow(N)), 7)
  expect_equal(ref[["rank"]], 199)
  expect_equal(e$Blocks_model[["D-Efficiency"]], ref[["D"]], tolerance = 1e-12)
})
