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

# A-efficiency of a square lattice of order s with r replicates (spec 5.2): the
# A-bound of every resolvable design of that shape, attained by the lattice.
a_square <- function(s, r) {
  (s^2 - 1) / ((s + 1 - r) * (s - 1) + r^2 * (s - 1) / (r - 1))
}

# Level 2 of a square-lattice design reaches the closed-form bound.
expect_square_optimal <- function(x) {
  m <- x$blocksModel[x$blocksModel$Level == 2, ]
  reps <- x$blocksModel$Blocks[x$blocksModel$Level == 1]
  expect_gte(m[["A-Efficiency"]], a_square(m$Blocks / reps, reps) - 1.5e-7)
}

# Level 2 of a rectangular-lattice design reaches the bound recorded for its
# shape from blocksdesign 4.9 (fixtures/rectangular-lattice-bounds.csv).
expect_rectangular_optimal <- function(x, t, k, reps) {
  recorded <- utils::read.csv(test_path("fixtures", "rectangular-lattice-bounds.csv"))
  bound <- recorded$bound[recorded$t == t & recorded$k == k & recorded$r == reps]
  expect_length(bound, 1L)
  m <- x$blocksModel[x$blocksModel$Level == 2, ]
  expect_gte(m[["A-Efficiency"]], bound - 1.5e-7)
}

test_that("square lattices with a finite-field construction are A-optimal", {
  # Bose (1938) squares over GF(q) give r <= q + 1 orthogonal replicates when q
  # is a prime power; search alone misses (64, 8, 4) on every seed.
  expect_optimal <- expect_square_optimal
  for (shape in list(c(64, 8, 4), c(81, 9, 4), c(64, 8, 5), c(25, 5, 6))) {
    for (seed in 1:3) {
      expect_optimal(square_lattice(t = shape[1], k = shape[2], reps = shape[3], seed = seed))
    }
  }
  # Seeds where the search alone stopped below the bound.
  expect_optimal(square_lattice(t = 25, k = 5, reps = 4, seed = 3))
  expect_optimal(square_lattice(t = 16, k = 4, reps = 4, seed = 140))
  expect_optimal(square_lattice(t = 81, k = 9, reps = 3, seed = 74))
  # The start depends on the shape, not on the calling function.
  expect_optimal(incomplete_blocks(t = 25, k = 5, reps = 4, seed = 3))
  expect_optimal(alpha_lattice(t = 25, k = 5, reps = 4, seed = 3))
  # q = 6 has no finite field; the design is still built.
  expect_s3_class(square_lattice(t = 36, k = 6, reps = 4, seed = 1), "FielDHub")
})

test_that("square lattices of composite order are A-optimal", {
  # MacNeish (1922) products of the prime-power factors' squares, the order
  # 3m + 1 pair of Bose, Shrikhande & Parker (1960) for q = 10, and its product
  # with the order 3 squares for q = 30. Search alone misses every shape here
  # on every seed.
  expect_optimal <- expect_square_optimal
  for (shape in list(c(144, 12, 4), c(225, 15, 4), c(100, 10, 4), c(900, 30, 4))) {
    for (seed in 1:3) {
      expect_optimal(square_lattice(t = shape[1], k = shape[2], reps = shape[3], seed = seed))
    }
  }
  for (seed in 1:3) {
    expect_optimal(alpha_lattice(t = 100, k = 10, reps = 4, seed = seed))
    expect_optimal(incomplete_blocks(t = 100, k = 10, reps = 3, seed = seed))
  }
  # A seed where the search alone stopped below the bound.
  expect_optimal(incomplete_blocks(t = 100, k = 10, reps = 3, seed = 11))
})

test_that("square lattices of orders 14, 18 and 26 are A-optimal with four replicates", {
  # No construction covers these orders; the orthogonal pairs come from
  # FielDHub's own search (tools/search-mols.R). Search alone falls short of
  # the bound on every seed.
  for (q in c(14, 18, 26)) {
    for (seed in 1:3) {
      expect_square_optimal(square_lattice(t = q^2, k = q, reps = 4, seed = seed))
    }
  }
})

test_that("large lattices are built at their A-bound", {
  # q = 64, r = 4: the construction attains the bound, so the engine returns it
  # without searching (before, one search spent about 7 s confirming it).
  expect_square_optimal(square_lattice(t = 4096, k = 64, reps = 4, seed = 1))
  # s = 150, r = 3: too large for the search, so the rectangular construction
  # is returned with its efficiencies computed from the block concurrences.
  expect_rectangular_optimal(rectangular_lattice(t = 22350, k = 149, reps = 3, seed = 1),
                             22350, 149, 3)
})

test_that("rectangular lattices with a construction are A-optimal", {
  # Shrikhande's method (Cochran & Cox 1957, p. 417) deletes one block of an
  # (r + 1)-replicate square lattice of order s; at r = 3 and s = 6 (no
  # orthogonal pair) the rows, columns and symbols of a Latin square with a
  # transversal deleted. s = 10 and 14 have a pair (3m + 1 and the stored
  # pair), so they use Shrikhande's method. Search alone misses every shape
  # here on seed 1.
  for (shape in list(c(42, 6, 4), c(56, 7, 4), c(72, 8, 4), c(30, 5, 3), c(90, 9, 3),
                     c(182, 13, 3))) {
    for (seed in 1:3) {
      x <- rectangular_lattice(t = shape[1], k = shape[2], reps = shape[3], seed = seed)
      expect_rectangular_optimal(x, shape[1], shape[2], shape[3])
    }
  }
  # Seeds where the search alone stopped below the bound.
  for (call in list(c(20, 4, 5, 108), c(72, 8, 3, 195))) {
    x <- rectangular_lattice(t = call[1], k = call[2], reps = call[3], seed = call[4])
    expect_rectangular_optimal(x, call[1], call[2], call[3])
  }
})

test_that("the A-Bound is FielDHub's own valid bound", {
  # Spec 5.2b and 6.4: the smallest A-bound valid for a resolvable design of
  # the shape. It is attained by the two-replicate rectangular lattice and the
  # square lattice, and lies above the rectangular lattice 12/3/3.
  model <- function(x, level) x$blocksModel[x$blocksModel$Level == level, ]
  calls <- list(
    list(x = rectangular_lattice(t = 12, k = 3, reps = 2, seed = 1), bound = 0.5945946,
         a = 0.5945946),
    list(x = rectangular_lattice(t = 12, k = 3, reps = 3, seed = 1), bound = 0.6848249,
         a = 0.6800618),
    list(x = rectangular_lattice(t = 20, k = 4, reps = 3, seed = 1), bound = 0.7480315),
    list(x = rectangular_lattice(t = 42, k = 6, reps = 4, seed = 1), bound = 0.8328497),
    list(x = alpha_lattice(t = 30, k = 5, reps = 3, seed = 1), bound = 0.7877759),
    list(x = square_lattice(t = 16, k = 4, reps = 2, seed = 1),
         bound = (16 - 1) / ((4 + 1 - 2) * (4 - 1) + 2^2 * (4 - 1) / (2 - 1)))
  )
  for (call in calls) {
    expect_equal(model(call$x, 2)[["A-Bound"]], call$bound, tolerance = 1e-7)
    expect_equal(model(call$x, 1)[["A-Bound"]], 1)
    if (!is.null(call$a)) {
      expect_equal(model(call$x, 2)[["A-Efficiency"]], call$a, tolerance = 1e-7)
    }
  }
  # 12/3/3: the lattice is below the reported bound.
  expect_lt(model(calls[[2]]$x, 2)[["A-Efficiency"]], model(calls[[2]]$x, 2)[["A-Bound"]])
})
