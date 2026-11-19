library(FielDHub)

test_that("square_lattice() rejects k that is not sqrt(t)", {
  # Regression test (B3): square_lattice() only checked that t is a square
  # number, not that k == sqrt(t). With t = 64, k = 4 the call silently produced
  # a malformed design (128 rows, wrong lambda) instead of requiring
  # k = sqrt(64) = 8. The guard now rejects k != sqrt(t).
  expect_error(square_lattice(t = 64, k = 4, reps = 2, seed = 1), "k to equal")
})

test_that("square_lattice() still accepts k = sqrt(t)", {
  sq <- square_lattice(t = 64, k = 8, reps = 2, seed = 1)
  expect_s3_class(sq, "FielDHub")
})

test_that("square_lattice() and rectangular_lattice() return resolvable designs with their reported efficiencies", {
  expect_resolvable_blocks(square_lattice(t = 25, k = 5, reps = 3, seed = 1), k = 5)
  expect_resolvable_blocks(rectangular_lattice(t = 20, k = 4, reps = 2, seed = 1), k = 4)
})

test_that("square_lattice() randomizes the plot order within blocks", {
  # A field book is "patterned" when, in every pair of replicates, the UNIT
  # position in one replicate determines the IBLOCK in the other: the mark of
  # an algebraic layout whose plots were never shuffled within blocks.
  determines <- function(book, from, to) {
    a <- book[book$REP == from, c("ENTRY", "UNIT")]
    b <- book[book$REP == to, c("ENTRY", "IBLOCK")]
    m <- merge(a, b, by = "ENTRY")
    all(tapply(m$IBLOCK, m$UNIT, function(x) length(unique(x))) == 1L)
  }
  patterned <- vapply(1:20, function(seed) {
    book <- square_lattice(t = 16, k = 4, reps = 3, seed = seed)$fieldBook
    pairs <- utils::combn(sort(unique(book$REP)), 2)
    all(apply(pairs, 2, function(p) determines(book, p[1], p[2]) || determines(book, p[2], p[1])))
  }, logical(1))
  expect_lte(sum(patterned), 1)
})

test_that("square lattices with a finite-field construction are A-optimal", {
  # Bose (1938) squares over GF(q) give r <= q + 1 orthogonal replicates when q
  # is a prime power; search alone misses (64, 8, 4) on every seed.
  level2 <- function(x) x$blocksModel[x$blocksModel$Level == 2, ]
  expect_optimal <- function(x) {
    m <- level2(x)
    expect_gte(m[["A-Efficiency"]], m[["A-Bound"]] - 1.5e-7)
  }
  for (shape in list(c(64, 8, 4), c(81, 9, 4), c(64, 8, 5), c(25, 5, 6))) {
    for (seed in 1:3) {
      expect_optimal(square_lattice(t = shape[1], k = shape[2], reps = shape[3], seed = seed))
    }
  }
  # Seeds where the search alone stopped below the bound.
  expect_optimal(square_lattice(t = 25, k = 5, reps = 4, seed = 3))
  expect_optimal(square_lattice(t = 16, k = 4, reps = 4, seed = 140))
  # The start depends on the shape, not on the calling function.
  expect_optimal(incomplete_blocks(t = 25, k = 5, reps = 4, seed = 3))
  expect_optimal(alpha_lattice(t = 25, k = 5, reps = 4, seed = 3))
  # q = 6 has no finite field; the design is still built.
  expect_s3_class(square_lattice(t = 36, k = 6, reps = 4, seed = 1), "FielDHub")
})
