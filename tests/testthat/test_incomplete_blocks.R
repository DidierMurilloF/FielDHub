# Incomplete-block designs: incomplete_blocks(), alpha_lattice(),
# square_lattice(), rectangular_lattice() and the block engine behind them and
# behind do_optim()'s location allocations.

# Reported efficiencies and bounds are rounded to 7 decimals, so a reported
# value differs from the exact one by at most 5e-8; 1e-10 more covers the
# error of the dense eigenvalue references below (at most a few hundred
# treatments with condition numbers stated per test, n * eps * kappa < 1e-11).
round_tol <- 5e-8 + 1e-10
# Two values that are each rounded to 7 decimals (a reported value against a
# value recorded by blocksdesign 4.9, or two reported values): two roundings.
recorded_tol <- 1e-7 + 1e-10

# D- and A-efficiency of a treatment x block incidence matrix (equal
# replication r): the geometric and harmonic means of the v - 1 largest
# eigenvalues of C = diag(r) - N K^-1 N', divided by r; rank is the number of
# eigenvalues above 1e-9 (v - 1 when the design is connected).
incidence_efficiency <- function(N) {
  N <- unclass(as.matrix(N))
  r <- rowSums(N)[1L]
  info <- diag(rowSums(N), nrow(N)) - N %*% diag(1 / colSums(N), ncol(N)) %*% t(N)
  ev <- sort(eigen(info, symmetric = TRUE, only.values = TRUE)$values, decreasing = TRUE)
  ev <- ev[seq_len(nrow(N) - 1L)]
  c(D = exp(mean(log(pmax(ev, 0) / r))), A = 1 / mean(r / ev), rank = sum(ev > 1e-9))
}

# The test-entry allocation of do_optim() (one check, which the allocation
# table excludes); force_balance = FALSE returns the block engine's
# allocation, which the documentation describes before balancing.
allocation_of <- function(design, lines, l, copies, seed, force_balance = TRUE) {
  x <- suppressWarnings(do_optim(design, lines = lines, l = l, copies_per_entry = copies,
                                 add_checks = TRUE, checks = 1,
                                 rep_checks = if (design == "prep") 1,
                                 force_balance = force_balance, seed = seed))
  unclass(as.matrix(x$allocation))
}

level2 <- function(x) x$blocksModel[x$blocksModel$Level == 2, ]

# A-efficiency of a square lattice of order s with r <= s + 1 replicates: the
# A-bound of every resolvable design of that shape, attained by the lattice
# (efficiency factors (r - 1) / r on r (s - 1) contrasts, 1 on the rest).
a_square <- function(s, r) (s^2 - 1) / ((s + 1 - r) * (s - 1) + r^2 * (s - 1) / (r - 1))
# Its D-efficiency, from the same efficiency factors.
d_square <- function(s, r) ((r - 1) / r)^(r / (s + 1))

# Least sum of squares of n non-negative integers that sum to a.
least_squares <- function(a, n) n * (a %/% n)^2 + (2 * (a %/% n) + 1) * (a %% n)
# Concurrence bound on the A-efficiency of a resolvable design of v = s k
# treatments in r replicates of s blocks of k plots. The efficiency factors
# are 1 - mu_i for the v - 1 non-trivial eigenvalues mu_i of N N' / (r k);
# sum(mu) = T = s - 1 and sum(mu^2) = S = |N'N|^2 / (r k)^2 - 1. By
# Cauchy-Schwarz sum(mu / (1 - mu)) >= T^2 / (T - S), so
# A <= (v - 1) / (v - 1 + T^2 / (T - S)), which falls as S grows. S is at
# least each of: the blocks' side (a block meets the s blocks of another
# replicate in k plots, as evenly as possible), the treatments' side (a
# treatment's r (k - 1) concurrences spread evenly over the other v - 1) and
# T^2 / m for the m = min(v, r s - r + 1) - 1 non-zero mu (the rank of N).
# A square lattice gives a_square(s, r).
a_resolvable <- function(v, k, r) {
  s <- v / k
  T <- s - 1
  S <- max((s + (r - 1) * s * least_squares(k, s) / k^2) / r,
           v * (r^2 + least_squares(r * (k - 1), v - 1)) / (r * k)^2,
           1 + T^2 / (min(v, r * s - r + 1) - 1)) - 1
  (v - 1) / (v - 1 + T^2 / (T - S))
}

# Block sizes ---------------------------------------------------------------

test_that("block-design functions accept exactly their feasible block sizes", {
  build <- function(fun, t, k) {
    switch(fun,
      row_column = row_column(t = t, nrows = k, reps = 2, seed = 1, iterations = 1),
      get(fun)(t = t, k = k, reps = 2, seed = 1))
  }
  # Proper divisors for incomplete blocks, alpha lattices and row-column rows;
  # sqrt(t) for square lattices; k with t = k (k + 1) for rectangular ones.
  cases <- list(
    list("incomplete_blocks", 12, c(2, 3, 4, 6), c(1, 5, 7, 12)),
    list("alpha_lattice", 12, c(2, 3, 4, 6), c(1, 5, 7, 12)),
    list("row_column", 12, c(2, 3, 4, 6), c(1, 5, 7, 12)),
    list("incomplete_blocks", 13, numeric(), c(2, 13)),
    list("alpha_lattice", 13, numeric(), c(2, 13)),
    list("square_lattice", 64, 8, c(2, 4, 16)),
    list("square_lattice", 42, numeric(), 6),
    list("rectangular_lattice", 42, 6, 7),
    list("rectangular_lattice", 56, 7, 8),
    list("rectangular_lattice", 64, numeric(), c(7, 8))
  )
  for (case in cases) {
    fun <- case[[1]]; t <- case[[2]]
    for (k in case[[3]]) {
      expect_s3_class(suppressWarnings(build(fun, t, k)), "FielDHub")
    }
    for (k in case[[4]]) {
      expect_error(suppressWarnings(build(fun, t, k)), class = "fieldhub_input_error",
                   info = paste(fun, t, k))
    }
  }
})

# Treatment labels ----------------------------------------------------------

test_that("incomplete-block designs keep the treatment labels they are given", {
  # Each ENTRY carries the label at its position in `t`; numeric `t` gives
  # "G-<entry>" labels.
  v12 <- paste0("V", 1:12)
  v16 <- paste0("V", 1:16)
  cases <- list(
    incomplete_blocks = list(incomplete_blocks(t = v12, k = 4, reps = 2, seed = 1), v12),
    alpha_lattice = list(alpha_lattice(t = v12, k = 4, reps = 2, seed = 1), v12),
    rectangular_lattice = list(rectangular_lattice(t = v12, k = 3, reps = 2, seed = 1), v12),
    row_column = list(row_column(t = v12, nrows = 3, reps = 2, seed = 1), v12),
    square_lattice = list(square_lattice(t = v16, k = 4, reps = 2, seed = 1), v16),
    numeric_t = list(incomplete_blocks(t = 12, k = 4, reps = 2, seed = 1), paste0("G-", 1:12)),
    alpha_integer_vector = list(alpha_lattice(t = 1:8, k = 2, reps = 2, seed = 1), NULL),
    alpha_character_vector = list(alpha_lattice(t = LETTERS[1:8], k = 2, reps = 2, seed = 1),
                                  LETTERS[1:8])
  )
  for (nm in names(cases)) {
    fb <- cases[[nm]][[1]]$fieldBook
    labels <- cases[[nm]][[2]]
    if (is.null(labels)) {
      # t = 1:8 is eight treatments, each in both replicates
      expect_equal(as.vector(table(fb$ENTRY)), rep(2L, 8), info = nm)
    } else {
      expect_equal(list(sort(unique(as.character(fb$TREATMENT))), as.character(fb$TREATMENT)),
                   list(sort(labels), labels[fb$ENTRY]), info = nm)
    }
  }
})

test_that("designs reject duplicated treatment labels", {
  labels <- c("A", "A", paste0("V", 1:10))
  calls <- list(
    incomplete_blocks = function() incomplete_blocks(t = labels, k = 4, reps = 2, seed = 1),
    row_column = function() row_column(t = labels, nrows = 3, reps = 2, seed = 1),
    RCBD = function() RCBD(t = c("A", "A", "B"), reps = 2, seed = 1),
    CRD = function() CRD(t = c("A", "A", "B"), reps = 2, seed = 1)
  )
  for (nm in names(calls)) expect_error(calls[[nm]](), class = "fieldhub_input_error", info = nm)
})

test_that("incomplete_blocks() keeps supplied entries and labels once per replicate", {
  data <- data.frame(ENTRY = 101:120, NAME = paste0("G", 101:120))
  book <- incomplete_blocks(t = 20, k = 4, reps = 2, l = 2, data = data, seed = 3)$fieldBook
  expect_equal(as.character(book$TREATMENT), data$NAME[match(book$ENTRY, data$ENTRY)])
  counts <- table(factor(book$TREATMENT, levels = data$NAME),
                  interaction(book$LOCATION, book$REP, drop = TRUE))
  expect_true(all(counts == 1))
})

# Efficiency report ---------------------------------------------------------

test_that("block efficiencies of degenerate block models are exact", {
  # Disconnected models have zero efficiency (an earlier inverse-eigenvalue sum
  # reported A = 0.6 for the second case, where treatment 5 is isolated in
  # blocks 8 and 11); unused factor levels are ignored; a single block or
  # complete blocks are fully efficient.
  cases <- list(
    disconnected = list(factor(c(1, 2, 2)), factor(c(3, 1, 2)), 0),
    isolated_treatment = list(
      factor(c(1, 1, 1, 2, 3, 3, 3, 4, 5, 5, 6, 6, 6, 7, 7, 7)),
      factor(c(4, 5, 9, 2, 4, 6, 2, 1, 8, 11, 3, 10, 1, 7, 12, 3)), 0),
    unused_levels = list(factor(rep(1:4, 3), levels = 1:5),
                         factor(rep(1:3, each = 4), levels = 1:4), 1),
    single_block = list(factor(rep(1:4, 3), levels = 1:5), factor(rep(1, 12)), 1)
  )
  for (nm in names(cases)) {
    case <- cases[[nm]]
    expect_no_warning(result <- block_incidence_efficiency(block_incidence(case[[1]], case[[2]])))
    expect_equal(unlist(result), c(Deffic = case[[3]], Aeffic = case[[3]]),
                 tolerance = 1e-12, info = nm)
  }
})

test_that("blocksModel reports named efficiency columns with complete replicates first", {
  bm <- incomplete_blocks(t = 12, k = 4, reps = 3, seed = 1)$blocksModel
  expect_identical(names(bm), c("Level", "Blocks", "D-Efficiency", "A-Efficiency", "A-Bound"))
  # The replicates are complete blocks: efficiency and bound 1.
  expect_equal(unlist(bm[1, ]), c(Level = 1, Blocks = 3, `D-Efficiency` = 1,
                                  `A-Efficiency` = 1, `A-Bound` = 1))
})

test_that("resolvable designs report the efficiencies of their field books", {
  # expect_resolvable_blocks(): every replicate holds each entry once, every
  # block has k plots, and the Level 2 D and A equal an eigenvalue reference.
  # The reported A never exceeds the reported A-Bound.
  cases <- list(
    list("alpha_lattice", 30, 5, 3), list("incomplete_blocks", 20, 4, 2),
    list("square_lattice", 25, 5, 3), list("rectangular_lattice", 20, 4, 2),
    list("alpha_lattice", 30, 3, 2), list("alpha_lattice", 50, 25, 4),
    list("incomplete_blocks", 50, 10, 3), list("incomplete_blocks", 20, 2, 2)
  )
  for (case in cases) {
    x <- get(case[[1]])(t = case[[2]], k = case[[3]], reps = case[[4]], seed = 1)
    expect_resolvable_blocks(x, k = case[[3]])
    m <- level2(x)
    expect_lte(m[["A-Efficiency"]], m[["A-Bound"]], label = paste(unlist(case), collapse = " "))
  }
})

test_that("weakly connected designs (blocks of two, two replicates) stay connected", {
  # n = 2b: every block of two plots must link two treatments for the design
  # to be connected. block_efficiency_oracle() returns 0 for a disconnected
  # design. kappa of C up to about 1e4 at t = 300, so the oracle is good to
  # 1e-11.
  for (t in c(40, 100, 300)) {
    got <- vapply(1:3, function(seed) {
      x <- incomplete_blocks(t = t, k = 2, reps = 2, seed = seed)
      c(reported = level2(x)[["A-Efficiency"]], oracle = block_efficiency_oracle(x$fieldBook))
    }, numeric(2))
    err <- abs(got["reported", ] - got["oracle", ])
    expect(all(got["oracle", ] > 0 & err <= round_tol),
           sprintf("t = %d: seed %d oracle A %.3g, reported %.9f", t, which.max(err),
                   got["oracle", which.max(err)], got["reported", which.max(err)]))
  }
})

# A-Bound -------------------------------------------------------------------

test_that("the reported A-Bound is a valid bound, attained where the optimum is known", {
  # Valid: every design's A is at most its Level 2 bound, and the complete
  # replicates (Level 1) have bound 1. The Level 2 bound is the closed-form
  # concurrence bound a_resolvable() (176/257 for 12/3/3, 95/127 for 20/4/3;
  # the treatments' side decides 12/3/6); with two replicates a tighter
  # pairing bound applies, attained by the rectangular lattice 12/3/2, and
  # equal to a_square() for the square lattice.
  cases <- list(
    list(x = rectangular_lattice(t = 12, k = 3, reps = 2, seed = 1), attained = TRUE),
    list(x = rectangular_lattice(t = 12, k = 3, reps = 3, seed = 1), bound = a_resolvable(12, 3, 3)),
    list(x = rectangular_lattice(t = 20, k = 4, reps = 3, seed = 1), bound = a_resolvable(20, 4, 3)),
    list(x = rectangular_lattice(t = 42, k = 6, reps = 4, seed = 1), bound = a_resolvable(42, 6, 4)),
    list(x = alpha_lattice(t = 30, k = 5, reps = 3, seed = 1), bound = a_resolvable(30, 5, 3)),
    list(x = alpha_lattice(t = 12, k = 3, reps = 6, seed = 1), bound = a_resolvable(12, 3, 6)),
    list(x = square_lattice(t = 16, k = 4, reps = 2, seed = 1), bound = a_square(4, 2)),
    list(x = incomplete_blocks(t = 50, k = 5, reps = 3, seed = 2), bound = a_resolvable(50, 5, 3))
  )
  for (i in seq_along(cases)) {
    case <- cases[[i]]
    bm <- case$x$blocksModel
    m <- level2(case$x)
    expect_equal(bm[["A-Bound"]][bm$Level == 1], 1, info = i)
    expect_lte(m[["A-Efficiency"]], m[["A-Bound"]], label = paste("case", i))
    if (isTRUE(case$attained)) {
      expect_lte(abs(m[["A-Efficiency"]] - m[["A-Bound"]]), recorded_tol, label = paste("case", i))
    }
    if (!is.null(case$bound)) {
      expect_lte(abs(m[["A-Bound"]] - case$bound), round_tol,
                 label = sprintf("case %d: A-Bound %.7f, closed form %.7f; difference", i,
                                 m[["A-Bound"]], case$bound))
    }
  }
  # The one-way bound of the (7, 3, 1) BIBD is its efficiency 7/9, which the
  # engine reaches.
  set.seed(1)
  model <- blocks_engine(7, 3, 7)$Blocks_model
  expect_lte(max(abs(unlist(model[c("A-Efficiency", "A-Bound")]) - 7 / 9)), round_tol)
})

# Lattice optimality --------------------------------------------------------

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

test_that("square lattices reach the closed-form A-bound", {
  # Rows: function, t, k, reps, seeds. Bose (1938) squares over GF(q) for
  # prime powers; MacNeish (1922) products and the order 3m + 1 pair of Bose,
  # Shrikhande & Parker (1960) for composite orders; FielDHub's stored pairs
  # for orders 14, 18 and 26. The search alone misses most of these shapes;
  # the single seeds are ones where it stopped below the bound. The start
  # depends on the shape, not on the calling function.
  cases <- list(
    list("square_lattice", 64, 8, 4, 1:3), list("square_lattice", 81, 9, 4, 1:3),
    list("square_lattice", 64, 8, 5, 1:3), list("square_lattice", 25, 5, 6, 1:3),
    list("square_lattice", 25, 5, 4, 3), list("square_lattice", 16, 4, 4, 140),
    list("square_lattice", 81, 9, 3, 74), list("incomplete_blocks", 25, 5, 4, 3),
    list("alpha_lattice", 25, 5, 4, 3),
    list("square_lattice", 144, 12, 4, 1:3), list("square_lattice", 225, 15, 4, 1:3),
    list("square_lattice", 100, 10, 4, 1:3), list("square_lattice", 900, 30, 4, 1:3),
    list("alpha_lattice", 100, 10, 4, 1:3), list("incomplete_blocks", 100, 10, 3, c(1:3, 11)),
    list("square_lattice", 196, 14, 4, 1:3), list("square_lattice", 324, 18, 4, 1:3),
    list("square_lattice", 676, 26, 4, 1:3),
    # q = 64: the construction attains the bound, so no search runs
    list("square_lattice", 4096, 64, 4, 1),
    # Prime-power orders with every replicate count up to q + 1
    list("square_lattice", 9, 3, 2, 1), list("square_lattice", 9, 3, 3, 1),
    list("square_lattice", 9, 3, 4, 1), list("square_lattice", 16, 4, 2, 1),
    list("square_lattice", 16, 4, 3, 1), list("square_lattice", 16, 4, 5, 1),
    list("square_lattice", 25, 5, 2, 1), list("square_lattice", 25, 5, 5, 1),
    list("square_lattice", 49, 7, 2, 1), list("square_lattice", 49, 7, 5, 1),
    list("square_lattice", 49, 7, 8, 1)
  )
  for (case in cases) {
    s <- case[[3]]; r <- case[[4]]
    a <- vapply(case[[5]], function(seed) {
      level2(get(case[[1]])(t = case[[2]], k = s, reps = r, seed = seed))[["A-Efficiency"]]
    }, numeric(1))
    expect(all(a >= a_square(s, r) - round_tol),
           sprintf("%s(%d, %d, %d): seed %d A = %.7f, bound %.7f", case[[1]], case[[2]], s, r,
                   case[[5]][which.min(a)], min(a), a_square(s, r)))
  }
  # q = 6 has no orthogonal pair; the design is still built.
  expect_s3_class(square_lattice(t = 36, k = 6, reps = 4, seed = 1), "FielDHub")
})

test_that("incomplete blocks and allocations on lattice shapes reach the lattice's D-efficiency", {
  # 100 treatments in blocks of 10: the square lattice's D is
  # ((r - 1) / r)^(r / (s + 1)) for s = 10. The allocation of 100 lines with r
  # copies to 10 r locations has the same shape without replicates (the D
  # blocksdesign 4.9 recorded for it, 0.8953135 and 0.8815913, is the
  # lattice's); its D is computed here from the incidence, good to 1e-13.
  for (r in 2:3) {
    d <- vapply(1:3, function(seed) {
      c(level2(incomplete_blocks(t = 100, k = 10, reps = r, seed = seed))[["D-Efficiency"]],
        incidence_efficiency(allocation_of("sparse", 100, 10 * r, r, seed, force_balance = FALSE))[["D"]])
    }, numeric(2))
    worst <- which(d == min(d), arr.ind = TRUE)[1, ]
    expect(all(d >= d_square(10, r) - round_tol),
           sprintf("r = %d: %s seed %d D = %.7f, lattice %.7f", r,
                   c("incomplete_blocks", "allocation")[worst[1]], worst[2], min(d), d_square(10, r)))
  }
})

test_that("rectangular lattices reach the bounds recorded from blocksdesign 4.9", {
  # Shrikhande's method (Cochran & Cox 1957, p. 417) deletes one block of an
  # (r + 1)-replicate square lattice; s = 6 deletes a transversal of a Latin
  # square; s = 150 is too large for the search, so the construction is
  # returned. The single seeds are ones where the search alone stopped below
  # the bound.
  recorded <- utils::read.csv(test_path("fixtures", "rectangular-lattice-bounds.csv"))
  cases <- list(c(42, 6, 4), c(56, 7, 4), c(72, 8, 4), c(30, 5, 3), c(90, 9, 3), c(182, 13, 3))
  cases <- c(lapply(cases, function(x) list(x, 1:3)),
             list(list(c(20, 4, 5), 108), list(c(72, 8, 3), 195), list(c(22350, 149, 3), 1)))
  for (case in cases) {
    shape <- case[[1]]
    bound <- recorded$bound[recorded$t == shape[1] & recorded$k == shape[2] & recorded$r == shape[3]]
    a <- vapply(case[[2]], function(seed) {
      level2(rectangular_lattice(t = shape[1], k = shape[2], reps = shape[3], seed = seed))[["A-Efficiency"]]
    }, numeric(1))
    expect(length(bound) == 1L && all(a >= bound - recorded_tol),
           sprintf("rectangular_lattice(%s): A = %.7f, recorded bound %s",
                   paste(shape, collapse = ", "), min(a), paste(bound, collapse = ",")))
  }
})

# Block engine (location allocations) ---------------------------------------

test_that("allocations are even, equireplicate and connected", {
  # do_optim()'s documented engine result: each entry has floor(copies / l) or
  # one more copies at each location, location totals differ by at most one,
  # and the entry x location incidence is connected (rank v - 1). Rows:
  # design, lines, locations, copies, seeds. The last three are app shapes
  # that came back uneven before the engine's even-up step.
  cases <- list(
    list("sparse", 20, 4, 2, 1:2), list("sparse", 101, 4, 2, 1:2), list("prep", 50, 5, 6, 1:2),
    list("sparse", 13, 13, 4, 1:2), list("prep", 30, 6, 11, 1:2), list("sparse", 5, 20, 8, 1:2),
    list("prep", 50, 10, 19, 1), list("prep", 100, 10, 19, 1), list("prep", 100, 8, 15, 3)
  )
  for (case in cases) {
    for (seed in case[[5]]) {
      N <- allocation_of(case[[1]], case[[2]], case[[3]], case[[4]], seed, force_balance = FALSE)
      low <- case[[4]] %/% case[[3]]
      problems <- c(
        if (!identical(dim(N), as.integer(c(case[[2]], case[[3]])))) "wrong dimensions",
        if (any(rowSums(N) != case[[4]])) "copies per entry differ from copies_per_entry",
        if (any(N < low | N > low + 1)) "uneven entry counts",
        if (diff(range(colSums(N))) > 1) "location totals differ by more than one",
        if (incidence_efficiency(N)[["rank"]] != case[[2]] - 1) "disconnected"
      )
      expect(length(problems) == 0L, sprintf("%s(%d, %d, %d) seed %d: %s", case[[1]], case[[2]],
                                              case[[3]], case[[4]], seed, paste(problems, collapse = "; ")))
    }
  }
})

test_that("allocations reach the optima recorded from blocksdesign 4.9", {
  skip_on_cran()
  # The engine's search must reach the known optimum (or blocksdesign 4.9's
  # best value) in at least 95 of 100 seeds. Rows of blocks-engine-optima.rds:
  # v lines, b locations, r copies; the criterion is D for the sparse shapes
  # and A for the p-rep shape. The allocation's efficiency is computed here
  # from its incidence (equal location sizes, so no balancing applies).
  optima <- readRDS(test_path("fixtures", "blocks-engine-optima.rds"))
  for (i in seq_len(nrow(optima))) {
    o <- optima[i, ]
    design <- if (o$criterion == "A") "prep" else "sparse"
    hits <- vapply(1:100, function(seed) {
      incidence_efficiency(allocation_of(design, o$v, o$b, o$r, seed))[[o$criterion]] >=
        o$target - 1e-10
    }, logical(1))
    expect_gte(sum(hits), 95, label = o$name)
  }
})

test_that("allocation efficiencies agree with blocksdesign 4.9 on known optima", {
  skip_on_cran()
  # The BIBDs (7, 3, 1) and (13, 4, 1): D = A = lambda v / (r k) = 7/9 and
  # 13/16, recorded by blocksdesign at 7 decimals (rounding 5e-8).
  optima <- readRDS(test_path("fixtures", "blocksdesign-references.rds"))$optima
  for (i in seq_len(nrow(optima))) {
    got <- incidence_efficiency(allocation_of("sparse", optima$v[i], optima$b[i], optima$r[i], 1))
    err <- abs(got[c("D", "A")] - c(optima$D[i], optima$A[i]))
    expect_lte(max(err), round_tol, label = sprintf("v = %d", optima$v[i]))
  }
})

test_that("allocations reach or beat blocksdesign's D on sparse shapes", {
  skip_on_cran()
  # blocksdesign 4.9 medians of D over seeds 1-3 (searches = 20) for 50 lines
  # with 3 copies in 30, 50 and 20 locations; the median over the same seeds
  # must reach it within 1e-4, a margin for search paths that differ by
  # platform.
  cases <- list(list(30, 0.7887762), list(50, 0.6231683), list(20, 0.8695983))
  for (case in cases) {
    d <- vapply(1:3, function(seed) {
      incidence_efficiency(allocation_of("sparse", 50, case[[1]], 3, seed, force_balance = FALSE))[["D"]]
    }, numeric(1))
    expect_gte(median(d), case[[2]] - 1e-4, label = sprintf("50 lines in %d locations", case[[1]]))
  }
})

test_that("the engine's incremental swap updates agree with a fresh computation", {
  # FIELDHUB_ENGINE_DEBUG=1 recomputes the information after every accepted
  # swap and reports the largest difference from the rank-2 update.
  old <- Sys.getenv("FIELDHUB_ENGINE_DEBUG", unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv("FIELDHUB_ENGINE_DEBUG") else
    Sys.setenv(FIELDHUB_ENGINE_DEBUG = old), add = TRUE)
  Sys.setenv(FIELDHUB_ENGINE_DEBUG = "1")
  errors <- vapply(list(c(13, 4, 13), c(100, 2, 4), c(30, 11, 6)), function(case) {
    set.seed(8)
    blocks_engine(case[1], case[2], case[3])$search$debug_max_error
  }, numeric(1))
  expect_lte(max(errors), 1e-8)
  Sys.unsetenv("FIELDHUB_ENGINE_DEBUG")
  set.seed(8)
  expect_true(is.na(blocks_engine(13, 4, 13)$search$debug_max_error))
})
