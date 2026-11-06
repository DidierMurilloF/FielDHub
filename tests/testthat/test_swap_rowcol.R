# The row/column rule of swap_pairs_core() (spec-m15 section 5).
brute_shared <- function(X, covered_values) {
  nr <- nrow(X)
  sum(vapply(covered_values, function(g) {
    cells <- which(X == g)
    if (length(cells) < 2L) return(0L)
    pairs <- utils::combn(cells, 2L)
    r <- (pairs - 1L) %% nr
    c <- (pairs - 1L) %/% nr
    as.integer(sum(r[1, ] == r[2, ]) + sum(c[1, ] == c[2, ]))
  }, integer(1)))
}
rowcol_field <- function(nr, nc, m, seed) {
  set.seed(seed)
  matrix(as.numeric(sample(c(rep(seq_len(m), each = 2L), seq.int(m + 1L, nr * nc - m)))), nr)
}

test_that(".shared_pairs() counts pairs in one row plus pairs in one column", {
  # cells (1,1), (1,3), (2,3) of a 2-row field: one row pair, one column pair
  expect_identical(.shared_pairs(c(1L, 5L, 6L), 2L), 2L)
  expect_identical(.shared_pairs(c(1L, 4L), 2L), 0L)
  expect_identical(.shared_pairs(7L, 3L), 0L)
})

test_that(".shared_delta() equals the brute-force change in S on random swaps", {
  skip_on_cran()
  set.seed(31)
  for (t in seq_len(1000)) {
    nr <- sample(2:8, 1L); nc <- sample(2:8, 1L); n <- nr * nc
    X <- matrix(as.numeric(sample(c(rep(1:3, c(2, 3, 4)), 4:n), n)), nr)
    scope_all <- runif(1) < 0.5
    k <- tabulate(match(X, sort(unique(as.vector(X)))))
    values <- sort(unique(as.vector(X)))
    covered_values <- values[if (scope_all) k >= 2L else k == 2L]
    ab <- sample.int(n, 2L)
    Y <- X; Y[ab] <- Y[rev(ab)]
    expected <- brute_shared(Y, covered_values) - brute_shared(X, covered_values)
    actual <- .shared_delta(which(X == X[ab[1]]), which(X == X[ab[2]]), ab[1], ab[2],
                            X[ab[1]] == X[ab[2]], X[ab[1]] %in% covered_values,
                            X[ab[2]] %in% covered_values, nr)
    expect_identical(actual, as.integer(expected))
  }
})

test_that("shared delta is zero for same-entry swaps", {
  # two copies in distinct rows and columns: the M1 formula would give 4 (spec M1 condition)
  expect_identical(.shared_delta(c(1L, 4L), c(1L, 4L), 1L, 4L, TRUE, TRUE, TRUE, 2L), 0L)
})

test_that(".rowcol_pick() ranks strictly or by weight, ties to the first", {
  scores <- c(5, 9, 7, 9)
  dS <- c(0L, 1L, -1L, -1L)
  expect_identical(.rowcol_pick(scores, dS, Inf, FALSE), 4L)
  expect_identical(.rowcol_pick(scores, c(0L, 0L, 0L, 0L), Inf, FALSE), 2L)
  # weight 0.5: 5, 8.5, 7.5, 9.5
  expect_identical(.rowcol_pick(scores, dS, 0.5, FALSE), 4L)
  # weight 0.5: 5, 8.5, 7.5, 8.5, a tie that goes to the first
  expect_identical(.rowcol_pick(c(5, 9, 7, 8), dS, 0.5, FALSE), 2L)
  expect_identical(.rowcol_pick(scores, dS, 3, FALSE), 4L)
  expect_identical(.rowcol_pick(scores, c(0L, 1L, 2L, 0L), Inf, TRUE), NA_integer_)
  expect_identical(.rowcol_pick(scores, dS, Inf, TRUE), 4L)
})

test_that("the strict rule removes every shared pair on typical fields without losing distance", {
  skip_on_cran()
  for (seed in 1:5) {
    X <- rowcol_field(10, 20, 40, seed)
    set.seed(seed); off <- swap_pairs_core(X, starting_dist = 3, rowcol_penalty = 0)
    set.seed(seed); on <- swap_pairs_core(X, starting_dist = 3, rowcol_penalty = Inf)
    expect_identical(on$diagnostics$shared_pairs_final, 0L)
    expect_identical(on$diagnostics$entries_sharing_final, 0L)
    expect_identical(brute_shared(on$optim_design, 1:40), 0L)
    expect_gte(on$min_distance, off$min_distance - 1)
    expect_identical(sort(as.vector(on$optim_design)), sort(as.vector(X)))
  }
})

test_that("diagnostics gain the rule fields only when the rule is on", {
  X <- rowcol_field(10, 20, 40, 1)
  set.seed(1); off <- swap_pairs_core(X)
  set.seed(1); on <- swap_pairs_core(X, rowcol_penalty = 2)
  fields <- c("rowcol_penalty", "rowcol_scope", "retained_threshold", "shared_pairs_input",
              "shared_pairs_final", "entries_sharing_final")
  expect_false(any(fields %in% names(off$diagnostics)))
  expect_identical(utils::tail(names(on$diagnostics), 6L), fields)
  expect_identical(on$diagnostics$rowcol_penalty, 2)
  expect_identical(on$diagnostics$shared_pairs_input, brute_shared(X, 1:40))
})

test_that("the kept layout is the fewest-shared step and the trace ends there", {
  # Pinned case where an earlier step has fewer shared pairs than the last one
  X <- rowcol_field(12, 20, 60, 3)
  set.seed(3); on <- swap_pairs_core(X, rowcol_penalty = 0.5)
  successful <- on$diagnostics$thresholds_attempted -
    (on$diagnostics$stop_reason == "iteration_limit")
  expect_lt(length(on$designs) - 1L, successful)
  expect_identical(on$diagnostics$retained_threshold, 3 + length(on$designs) - 2)
  expect_identical(on$designs[[length(on$designs)]], on$optim_design)
  expect_identical(on$distances[[length(on$distances)]], on$pairwise_distance)
  for (seed in 1:5) {
    X <- rowcol_field(12, 20, 60, seed)
    set.seed(seed); on <- swap_pairs_core(X, rowcol_penalty = Inf)
    steps <- on$designs[-1L]
    if (length(steps) == 0L) next
    s <- vapply(steps, brute_shared, integer(1), covered_values = 1:60)
    expect_identical(on$diagnostics$shared_pairs_final, s[length(s)])
    expect_true(all(s[length(s)] <= s))
    expect_identical(on$designs[[length(on$designs)]], on$optim_design)
    expect_identical(on$distances[[length(on$distances)]], on$pairwise_distance)
    expect_length(on$rows_incidence, length(on$designs) - 1L)
    expect_identical(on$diagnostics$retained_min_distance, on$min_distance)
  }
})

test_that("unfixable entries do not stall the sweeps", {
  # five copies on a 4-row field must share a row; "all" covers them
  set.seed(4)
  X <- matrix(as.numeric(sample(c(rep(1:3, each = 5L), 4:36))), 4)
  set.seed(4)
  on <- swap_pairs_core(X, starting_dist = 1, stop_iter = 1000, rowcol_penalty = Inf, rowcol_scope = "all")
  # The sweep-stop rule must end every successful threshold early even though
  # S cannot reach 0: only a failing threshold may use the full sweep budget.
  budget_used <- 1000 * (on$diagnostics$stop_reason == "iteration_limit")
  expect_lt(on$diagnostics$iterations - budget_used, 100)
  expect_gt(on$diagnostics$shared_pairs_final, 0L)
  expect_lte(on$diagnostics$shared_pairs_final, on$diagnostics$shared_pairs_input)
})

test_that("invalid rule controls are rejected", {
  X <- rowcol_field(10, 20, 40, 1)
  for (bad in list(-1, NA_real_, NaN, "a", c(1, 2), matrix(1))) {
    expect_error(swap_pairs_core(X, rowcol_penalty = bad), class = "fieldhub_error")
  }
  for (bad in list("rows", NA_character_, c("pairs", "all"), 1)) {
    expect_error(swap_pairs_core(X, rowcol_scope = bad), class = "fieldhub_error")
  }
})
