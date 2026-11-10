# Random agreement, worst-pair, identity, invariance, and randomization-policy
# tests for the augmented row-column engine (research M1 specification,
# sections 6, 7, 9 and 10.3; implementation plan task 12). Every random test
# seeds through arc_local_seed() (helper-arc-random.R), which restores the
# caller's RNG state on exit, and records attempted, structurally valid,
# connected, and scored counts, asserting a minimum on the scored count.

arc_random_tol <- 1e-10
arc_random_means <- c("E_aug", "V_tt", "V_tc", "V_cc")
arc_random_all <- c(arc_random_means, "max_tt", "max_tc")

# Draw `n` random designs from parameter set `p` (helper-arc-random.R) with
# the caller's RNG stream. Recoloring sets are made row-binary with
# arc_recolor_rows(). Each case keeps the contraction, v, mode, and whether
# arc_check_contraction() accepts it in the set's mode.
arc_random_cases <- function(p, n) {
  lapply(seq_len(n), function(draw) {
    contraction <- arc_random_draw(p)
    if (p$recolor) {
      contraction <- arc_recolor_rows(contraction, p$v)
    }
    valid <- tryCatch({
      arc_check_contraction(contraction, p$v, p$mode)
      TRUE
    }, fieldhub_input_error = function(condition) FALSE)
    list(contraction = contraction, v = p$v, k = p$k, mode = p$mode,
         valid = valid)
  })
}

# Objectives `names` of two evaluator results as a numeric vector
arc_pick <- function(result, names) {
  unlist(result[names], use.names = TRUE)
}

# Largest absolute difference between two results over `names`
arc_max_diff <- function(first, second, names) {
  max(abs(arc_pick(first, names) - arc_pick(second, names)))
}

# Uniform random permutation of a layout's rows and columns, one global
# permutation of the check codes, and a random assignment of the test codes
# to the test cells (specification section 9). Codes 1..T are tests and
# T + 1..T + k checks, as from arc_expand().
arc_randomize_layout <- function(codes, k, permute_tests = TRUE) {
  n_tests <- length(codes) - k * ncol(codes)
  relabel <- c(if (permute_tests) sample.int(n_tests) else seq_len(n_tests),
               n_tests + sample.int(k))
  out <- matrix(relabel[codes], nrow(codes), ncol(codes))
  out[sample.int(nrow(out)), sample.int(ncol(out)), drop = FALSE]
}

test_that("random designs agree across the evaluators and the oracle", {
  arc_local_seed(20261012)
  draws <- c(example1 = 60L, example2 = 20L, unequal = 70L, unbalanced = 70L)
  minimum <- c(example1 = 55L, example2 = 18L, unequal = 60L,
               unbalanced = 60L)
  counts <- matrix(0L, length(draws), 4L,
                   dimnames = list(names(draws),
                                   c("attempted", "valid", "connected",
                                     "scored")))
  worst <- 0
  max_r_above_k <- 0L
  for (set in names(draws)) {
    p <- arc_random_sets[[set]]
    for (case in arc_random_cases(p, draws[[set]])) {
      counts[set, "attempted"] <- counts[set, "attempted"] + 1L
      if (!case$valid) next
      counts[set, "valid"] <- counts[set, "valid"] + 1L
      if (max(tabulate(case$contraction, case$v)) > case$k) {
        max_r_above_k <- max_r_above_k + 1L
      }
      codes <- arc_expand(case$contraction, case$v, case$mode)$codes
      layout <- arc_evaluate_layout(codes)
      single <- arc_evaluate_objectives(case$contraction, case$v, case$mode)
      paper <- arc_evaluate_contraction(case$contraction, case$v, case$mode)
      oracle <- arc_oracle(codes)
      verdicts <- c(layout$connected, single$connected, paper$connected,
                    oracle$connected)
      expect_true(all(verdicts == oracle$connected))
      if (!oracle$connected) next
      counts[set, "connected"] <- counts[set, "connected"] + 1L
      deviations <- c(arc_max_diff(layout, oracle, arc_random_means),
                      arc_max_diff(single, oracle, arc_random_means),
                      arc_max_diff(single, layout, arc_random_means),
                      abs(paper$E_aug - oracle$E_aug))
      expect_lte(max(deviations), arc_random_tol)
      worst <- max(worst, deviations)
      counts[set, "scored"] <- counts[set, "scored"] + 1L
    }
  }
  # Recolored contractions are valid restricted designs; the column-binary
  # draws are valid by construction.
  expect_identical(counts[, "valid"], counts[, "attempted"])
  for (set in names(draws)) {
    expect_gte(counts[set, "scored"], minimum[[set]])
  }
  expect_gte(sum(counts[, "scored"]), 200L)
  # The unbalanced set includes designs with some r_h > k.
  expect_gte(max_r_above_k, 20L)
  expect_lte(worst, arc_random_tol)
})

test_that("random worst-pair maxima equal the oracle's brute-force maxima", {
  arc_local_seed(20261013)
  draws <- c(example1 = 10L, unequal = 25L, unbalanced = 25L)
  attempted <- valid <- connected <- scored <- 0L
  worst <- 0
  for (set in names(draws)) {
    p <- arc_random_sets[[set]]
    for (case in arc_random_cases(p, draws[[set]])) {
      attempted <- attempted + 1L
      if (!case$valid) next
      valid <- valid + 1L
      single <- arc_evaluate_objectives(case$contraction, case$v, case$mode,
                                        worst_pair = TRUE)
      oracle <- arc_oracle(arc_expand(case$contraction, case$v,
                                      case$mode)$codes)
      expect_identical(single$connected, oracle$connected)
      if (!oracle$connected) next
      connected <- connected + 1L
      # Brute force over the oracle's full pairwise matrix
      pairwise <- oracle$pairwise
      n_tests <- (case$v - case$k) * ncol(case$contraction)
      tests <- seq_len(n_tests)
      checks <- n_tests + seq_len(case$k)
      block_tt <- pairwise[tests, tests]
      brute <- c(max_tt = max(block_tt[upper.tri(block_tt)]),
                 max_tc = max(pairwise[tests, checks]))
      deviation <- max(abs(arc_pick(single, names(brute)) - brute),
                       abs(arc_pick(oracle, names(brute)) - brute))
      expect_lte(deviation, arc_random_tol)
      worst <- max(worst, deviation)
      scored <- scored + 1L
    }
  }
  expect_identical(valid, attempted)
  expect_gte(scored, 50L)
  expect_lte(worst, arc_random_tol)
})

test_that("the weighted-variance identity holds on random designs", {
  arc_local_seed(20261012)
  # The same seed and draws as the agreement test, so the same designs
  draws <- c(example1 = 60L, example2 = 20L, unequal = 70L, unbalanced = 70L)
  attempted <- valid <- connected <- scored <- 0L
  unequal_r <- 0L
  worst <- 0
  for (set in names(draws)) {
    p <- arc_random_sets[[set]]
    for (case in arc_random_cases(p, draws[[set]])) {
      attempted <- attempted + 1L
      if (!case$valid) next
      valid <- valid + 1L
      single <- arc_evaluate_objectives(case$contraction, case$v, case$mode)
      if (!single$connected) next
      connected <- connected + 1L
      s <- ncol(case$contraction)
      k <- case$k
      n_tests <- (case$v - k) * s
      n_treatments <- n_tests + k
      lhs <- case$v * s * (n_treatments - 1) / single$E_aug
      rhs <- choose(n_tests, 2) * single$V_tt + s * n_tests * k * single$V_tc +
        s^2 * choose(k, 2) * single$V_cc
      relative <- abs(lhs - rhs) / abs(lhs)
      expect_lte(relative, 1e-8)
      worst <- max(worst, relative)
      scored <- scored + 1L
      if (length(unique(tabulate(case$contraction, case$v))) > 1L) {
        unequal_r <- unequal_r + 1L
      }
    }
  }
  expect_identical(valid, attempted)
  expect_gte(scored, 200L)
  expect_gte(unequal_r, 100L)
  expect_lte(worst, 1e-8)
})

# Example contractions plus a few random designs from each set, for the
# invariance and randomization tests (the caller seeds the RNG)
arc_invariance_cases <- function(per_set = 2L) {
  examples <- list(
    list(contraction = arc_fixture("example1-contraction.csv"), v = 12L,
         k = 3L, mode = "restricted", valid = TRUE),
    list(contraction = arc_fixture("example2-contraction.csv"), v = 24L,
         k = 5L, mode = "restricted", valid = TRUE)
  )
  random <- unlist(lapply(arc_random_sets[c("example1", "unequal",
                                            "unbalanced")],
                          arc_random_cases, n = per_set),
                   recursive = FALSE)
  cases <- c(examples, unname(random))
  Filter(function(case) {
    case$valid && isTRUE(arc_evaluate_objectives(case$contraction, case$v,
                                                 case$mode)$connected)
  }, cases)
}

test_that("contraction permutations leave the objectives unchanged", {
  arc_local_seed(20261014)
  cases <- arc_invariance_cases()
  expect_gte(length(cases), 7L)
  worst <- 0
  for (case in cases) {
    m <- case$contraction
    base <- arc_evaluate_objectives(m, case$v, case$mode, worst_pair = TRUE)
    relabel <- sample.int(case$v)
    variants <- list(
      columns = m[, sample.int(ncol(m)), drop = FALSE],
      labels = matrix(relabel[m], nrow(m), ncol(m)),
      rows = m[sample.int(nrow(m)), , drop = FALSE]
    )
    variants$all <- matrix(relabel[m], nrow(m), ncol(m))[
      sample.int(nrow(m)), sample.int(ncol(m)), drop = FALSE]
    for (variant in variants) {
      result <- arc_evaluate_objectives(variant, case$v, case$mode,
                                        worst_pair = TRUE)
      expect_true(result$connected)
      deviation <- arc_max_diff(result, base, arc_random_all)
      expect_lte(deviation, arc_random_tol)
      worst <- max(worst, deviation)
    }
  }
  expect_lte(worst, arc_random_tol)
})

test_that("permuting test codes among fixed test cells changes no objective", {
  arc_local_seed(20261015)
  cases <- arc_invariance_cases()
  worst <- 0
  for (case in cases) {
    codes <- arc_expand(case$contraction, case$v, case$mode)$codes
    n_tests <- (case$v - case$k) * ncol(codes)
    relabel <- c(sample.int(n_tests), n_tests + seq_len(case$k))
    permuted <- matrix(relabel[codes], nrow(codes), ncol(codes))
    expect_false(identical(permuted, codes))
    checks <- n_tests + seq_len(case$k)
    layout <- arc_evaluate_layout(codes)
    oracle <- arc_oracle(codes)
    for (check_codes in list(NULL, checks)) {
      deviations <- c(
        arc_max_diff(arc_evaluate_layout(permuted, check_codes), layout,
                     arc_random_all),
        arc_max_diff(arc_oracle(permuted, check_codes), oracle,
                     arc_random_all)
      )
      expect_lte(max(deviations), arc_random_tol)
      worst <- max(worst, deviations)
    }
  }
  expect_lte(worst, arc_random_tol)
})

test_that("transposing a fixed layout leaves layout objectives unchanged", {
  arc_local_seed(20261016)
  layouts <- c(
    lapply(arc_invariance_cases(), function(case) {
      arc_expand(case$contraction, case$v, case$mode)$codes
    }),
    list(arc_fixture("example1-layout.csv"),
         arc_fixture("bailey-haines-c-1-4-7.csv"))
  )
  worst <- 0
  for (codes in layouts) {
    base <- arc_evaluate_layout(codes)
    transposed <- arc_evaluate_layout(t(codes))
    expect_true(transposed$connected)
    expect_identical(transposed$residual_df_rank, base$residual_df_rank)
    deviation <- arc_max_diff(transposed, base, arc_random_all)
    expect_lte(deviation, arc_random_tol)
    worst <- max(worst, deviation)
  }
  expect_lte(worst, arc_random_tol)
})

test_that("the randomization contract leaves every objective unchanged", {
  # Specification section 9: uniform row and column permutations, one global
  # permutation of the check codes, and random assignment of tests to test
  # cells. Each is applied alone and then all together.
  arc_local_seed(20261017)
  cases <- arc_invariance_cases()
  worst <- 0
  for (case in cases) {
    codes <- arc_expand(case$contraction, case$v, case$mode)$codes
    base <- arc_evaluate_layout(codes)
    n_tests <- (case$v - case$k) * ncol(codes)
    check_relabel <- c(seq_len(n_tests), n_tests + sample.int(case$k))
    variants <- list(
      rows = codes[sample.int(nrow(codes)), , drop = FALSE],
      columns = codes[, sample.int(ncol(codes)), drop = FALSE],
      checks = matrix(check_relabel[codes], nrow(codes), ncol(codes)),
      contract = arc_randomize_layout(codes, case$k, permute_tests = FALSE),
      with_tests = arc_randomize_layout(codes, case$k)
    )
    for (variant in variants) {
      result <- arc_evaluate_layout(variant)
      expect_true(result$connected)
      deviation <- arc_max_diff(result, base, arc_random_all)
      expect_lte(deviation, arc_random_tol)
      worst <- max(worst, deviation)
    }
  }
  expect_lte(worst, arc_random_tol)
})

test_that("an independent shuffle of check occurrences can change E_aug", {
  # Preserved counterexample (specification sections 9 and 10.3), found by a
  # deterministic scan of every within-column check swap of the Example 1
  # layout: swapping the checks in rows 3 and 7 of column 1 (codes 73 and 75)
  # lowers E_aug from 0.388111888112 to 0.375846501129 (by 0.012265387).
  # This shows only that independent occurrence shuffles carry no general
  # invariance guarantee, which is why the randomization contract forbids
  # them; it is not a claim that every such shuffle changes the objectives.
  codes <- arc_expand(arc_fixture("example1-contraction.csv"), 12,
                      "restricted")$codes
  column <- 1L
  rows <- c(3L, 7L)
  expect_identical(codes[rows, column], c(73L, 75L))
  shuffled <- codes
  shuffled[rows, column] <- codes[rev(rows), column]
  base <- arc_evaluate_layout(codes)
  changed <- arc_evaluate_layout(shuffled)
  expect_true(changed$connected)
  expect_gt(abs(changed$E_aug - base$E_aug), 1e-6)
  expect_lt(abs(changed$E_aug - 0.375846501129), 1e-10)
})
