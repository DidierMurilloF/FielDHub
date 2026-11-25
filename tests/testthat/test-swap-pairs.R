# swap_pairs(): the pair-swap optimizer of repeated entries.
#
# Distances are checked against an independent brute-force computation
# (stats::dist on the coordinates of every entry's copies). Both sides take the
# square root (or the sum) of the same small integers, so they agree to a few
# ulps; DIST_TOL is that rounding, scaled by the distance.
DIST_TOL <- 1e-12

# All within-entry distances of a layout, sorted, by brute force
swap_brute_distances <- function(X, method = "euclidean") {
  values <- as.numeric(names(which(table(X) > 1L)))
  sort(unlist(lapply(values, function(g) {
    as.numeric(stats::dist(which(X == g, arr.ind = TRUE), method = method))
  })))
}

# Pairs of copies of the covered entries that share a row or a column
swap_brute_shared <- function(X, covered) {
  sum(vapply(covered, function(g) {
    rc <- which(X == g, arr.ind = TRUE)
    if (nrow(rc) < 2L) return(0L)
    p <- utils::combn(nrow(rc), 2L)
    as.integer(sum(rc[p[1, ], 1] == rc[p[2, ], 1]) + sum(rc[p[1, ], 2] == rc[p[2, ], 2]))
  }, integer(1)))
}

# A field of `n_rep` entries with `copies` copies each, filled with single
# entries, and `n_na` inactive (missing) cells; fixed seed
swap_field <- function(nr, nc, n_rep, copies = 2L, n_na = 0L, seed = 1L) {
  set.seed(seed)
  n_single <- nr * nc - n_na - n_rep * copies
  cells <- c(rep(seq_len(n_rep), each = copies), n_rep + seq_len(n_single), rep(NA, n_na))
  matrix(as.numeric(sample(cells)), nr)
}

near <- function(a, b) length(a) == length(b) && all(abs(a - b) <= DIST_TOL * pmax(1, abs(b)))

# The properties every returned layout must have; a named logical vector
swap_result_properties <- function(out, X, method) {
  Y <- out$optim_design
  d <- swap_brute_distances(Y, method)
  c(
    same_dimensions = identical(dim(Y), dim(X)),
    inactive_cells_fixed = identical(is.na(Y), is.na(X)),
    copy_counts_kept = identical(table(Y, dnn = NULL), table(X, dnn = NULL)),
    pairwise_distance_is_brute_force = near(sort(out$pairwise_distance$DIST), d),
    min_distance_is_brute_force = near(out$min_distance, min(d)),
    no_inactive_cell_in_pairs = !anyNA(out$pairwise_distance$geno),
    steps_start_at_input_end_at_result = identical(out$designs[[1L]], X) &&
      identical(out$designs[[length(out$designs)]], Y) &&
      length(out$designs) == length(out$distances),
    replays = identical(reproduce_design(out), out)
  )
}

expect_properties <- function(ok, case) {
  expect(all(ok), sprintf("case %s: property %s fails", case, paste(names(ok)[!ok], collapse = ", ")))
}

# ---- Result contract: the layout is a permutation with brute-force distances ----

test_that("swap_pairs() returns a permutation of the input whose reported distances are the true ones", {
  cases <- list(
    small_euclidean = list(X = swap_field(8, 10, 10, seed = 101), lambda = 0),
    small_manhattan = list(X = swap_field(8, 10, 10, seed = 102), lambda = 2, dist_method = "manhattan"),
    wide_lambda = list(X = swap_field(12, 20, 30, seed = 103), lambda = 0.5),
    one_candidate = list(X = swap_field(12, 20, 30, seed = 77), candidate_sample_size = 1),
    many_candidates = list(X = swap_field(12, 20, 30, seed = 78), candidate_sample_size = 8),
    inactive_cells = list(X = swap_field(12, 20, 30, n_na = 15, seed = 201)),
    three_copies_manhattan = list(X = swap_field(12, 20, 20, copies = 3, n_na = 12, seed = 303),
                                  starting_dist = 2, dist_method = "manhattan"),
    five_copies = list(X = swap_field(16, 25, 10, copies = 5, seed = 305), starting_dist = 2),
    fractional_distance = list(X = swap_field(6, 8, 8, n_na = 2, seed = 9), starting_dist = 1.5,
                               stop_iter = 1, lambda = 0, candidate_sample_size = 100,
                               dist_method = "manhattan")
  )
  for (nm in names(cases)) {
    args <- c(cases[[nm]], list(seed = 11))
    method <- if (is.null(args$dist_method)) "euclidean" else args$dist_method
    out <- do.call(swap_pairs, args)
    ok <- swap_result_properties(out, args$X, method)
    # A failed threshold is never kept, so the result is never worse than the input
    ok["not_worse_than_input"] <- out$min_distance >= min(swap_brute_distances(args$X, method)) - DIST_TOL
    expect_properties(ok, nm)
  }
})

test_that("swap_pairs() separates the copies in a typical field", {
  set.seed(123)
  X <- matrix(sample(c(rep(1:10, 2), 11:50)), ncol = 10)
  out <- swap_pairs(X, starting_dist = 3, stop_iter = 100, seed = 123)
  expect_gte(min(swap_brute_distances(out$optim_design)), 3)
})

# ---- Measurement: no swap attempted, the input's distances are reported ----

test_that("swap_pairs() reports the input's distances when no threshold is attempted", {
  cases <- list(
    hand_euclidean = list(X = matrix(c(1, 2, 3, 4, 2, 5, 6, 1, 7, 8, 2, 9), ncol = 3),
                          want = c(sqrt(2), sqrt(5), sqrt(5), sqrt(10))),
    manhattan = list(X = matrix(c(1, 2, 3, 1), 2), dist_method = "manhattan", want = 2),
    inactive_euclidean = list(X = matrix(c(1, 2, 1, 3, 2, 1, NA, 4, 4, 5, 6, 7), 3)),
    inactive_manhattan = list(X = matrix(c(1, 2, 1, 3, 2, 1, NA, 4, 4, 5, 6, 7), 3),
                              dist_method = "manhattan")
  )
  for (nm in names(cases)) {
    case <- cases[[nm]]
    method <- if (is.null(case$dist_method)) "euclidean" else case$dist_method
    want <- if (is.null(case$want)) swap_brute_distances(case$X, method) else case$want
    out <- swap_pairs(case$X, starting_dist = 99, dist_method = method, seed = 1)
    expect_equal(list(layout = out$optim_design, dist = sort(out$pairwise_distance$DIST),
                      min = out$min_distance, first_step = sort(out$distances[[1]]$DIST)),
                 list(layout = case$X, dist = want, min = min(want), first_step = want),
                 tolerance = DIST_TOL, info = nm)
  }
})

test_that("distances of plots far apart do not overflow", {
  for (transposed in c(FALSE, TRUE)) {
    x <- matrix(NA_real_, 60000, 2)
    x[cbind(c(1, 60000), c(1, 2))] <- 1
    if (transposed) x <- t(x)
    expect_no_warning(out <- swap_pairs(x, starting_dist = 1e6, seed = 1))
    expect_equal(out$pairwise_distance$DIST, sqrt(59999^2 + 1), tolerance = DIST_TOL, info = transposed)
  }
})

test_that("swap_pairs() handles fields smaller than the starting distance", {
  # Regression: seq(starting_dist, diagonal, 1) errored when the diagonal
  # (about 2.83 here) was below starting_dist = 3.
  set.seed(1)
  X <- matrix(sample(c(1, 1, 2, 2)), nrow = 2, ncol = 2)
  expect_equal(swap_pairs(X)$optim_design, X)
})

# ---- Diagnostics: stop reasons and the per-threshold budget ----

test_that("swap diagnostics report why and where the search stopped", {
  cases <- list(
    budget_stop = list(args = list(X = matrix(c(rep(1:2, 2), 3:10), 3), starting_dist = 3, stop_iter = 0),
                       want = list(stop_reason = "iteration_limit", iterations = 0, thresholds_attempted = 1L,
                                   last_threshold = 3),
                       unchanged = TRUE),
    empty_range = list(args = list(X = matrix(c(1, 2, 3, 1), 2), starting_dist = 99),
                       want = list(stop_reason = "no_distance_thresholds", iterations = 0,
                                   thresholds_attempted = 0L, last_threshold = NA_real_)),
    range_complete = list(args = list(X = matrix(c(1, 2, 3, 1), 2), starting_dist = 1, stop_iter = 0,
                                      dist_method = "manhattan"),
                          want = list(stop_reason = "distance_range_complete", distance_method = "manhattan",
                                      thresholds_attempted = 2L, iterations = 0))
  )
  for (nm in names(cases)) {
    case <- cases[[nm]]
    out <- do.call(swap_pairs, c(case$args, seed = 1))
    got <- out$diagnostics[names(case$want)]
    if (isTRUE(case$unchanged)) {
      # a failed attempt is not accepted: the input is kept, below the threshold tried
      got$kept_input <- identical(out$optim_design, case$args$X)
      got$retained_is_min <- identical(out$diagnostics$retained_min_distance, out$min_distance)
      got$attempt_below <- out$diagnostics$last_attempt_min_distance < out$diagnostics$last_threshold
      case$want <- c(case$want, kept_input = TRUE, retained_is_min = TRUE, attempt_below = TRUE)
    }
    expect_equal(got, case$want, info = nm)
  }
})

test_that("swap work stays within the per-threshold budget", {
  x <- matrix(c(rep(1:3, 2), 4:13), 4)
  for (method in c("euclidean", "manhattan")) {
    info <- swap_pairs(x, starting_dist = 2, stop_iter = 3, dist_method = method, seed = 47)$diagnostics
    ok <- c(budget_recorded = info$max_iterations_per_threshold == 3,
            within_budget = info$iterations <= 3 * info$thresholds_attempted,
            known_reason = info$stop_reason %in% c("iteration_limit", "distance_range_complete"))
    expect_properties(ok, method)
  }
})

# ---- Input errors ----

test_that("swap_pairs() rejects malformed fields and controls, leaving the caller's stream", {
  x <- matrix(c(1, 2, 3, 1), 2)
  rowcol_field <- swap_field(10, 14, 30, seed = 5)
  bad <- list(
    stop_iter = list(Inf, NA_real_, -1, 1.5, c(1, 2), "10", .Machine$integer.max + 1),
    starting_dist = list(Inf, NA_real_, -1, c(1, 2), "3"),
    lambda = list(Inf, NA_real_, -1, c(1, 2), "0.5"),
    candidate_sample_size = list(Inf, NA_real_, 0, -1, 1.5, c(1, 2), "4", .Machine$integer.max + 1),
    dist_method = list(NULL, NA_character_, c("euclidean", "manhattan"), 1),
    X = list(matrix(1:9, 3), matrix(c(1, 1, 2, Inf), 2), matrix(c(1.5, 1.5, 1, 2), 2),
             matrix(c(rep(.Machine$integer.max + 1, 2), 1, 2), 2)),
    seed = list("18"),
    rowcol_penalty = list(-1, NA_real_, NaN, "a", c(1, 2), matrix(1)),
    rowcol_scope = list("rows", NA_character_, c("pairs", "all"), 1)
  )
  for (parameter in names(bad)) {
    ok <- vapply(bad[[parameter]], function(value) {
      # The row/column controls are checked before the automatic seed is
      # drawn; the others are checked with an explicit seed.
      args <- if (startsWith(parameter, "rowcol")) list(X = rowcol_field) else list(X = x, seed = 1)
      args[parameter] <- list(value)
      set.seed(9)
      before <- .Random.seed
      err <- tryCatch({ do.call(swap_pairs, args); NULL }, error = identity)
      inherits(err, "fieldhub_input_error") && identical(.Random.seed, before)
    }, logical(1))
    expect(all(ok), sprintf("%s = %s: no fieldhub_input_error, or the caller's stream moved", parameter,
                            paste(deparse(bad[[parameter]][[which(!ok)[1]]]), collapse = " ")))
  }
})

# ---- Seeds and reproduction ----

test_that("a seedless swap consumes exactly one draw from the caller's stream", {
  x <- matrix(c(rep(1:3, 2), 4:13), 4)
  set.seed(99)
  expected <- sample.int(.Machine$integer.max, 1L)
  after_one_draw <- .Random.seed
  set.seed(99)
  out <- swap_pairs(x, starting_dist = 2, stop_iter = 3, candidate_sample_size = 1)
  expect_true(identical(.Random.seed, after_one_draw))
  expect_s3_class(out, "fieldhub_optimization")
  expect_identical(out$metadata$design, "pair_swap")
  expect_identical(out$metadata$seed, expected)
  expect_equal(out$metadata$parameters$X, x)
  expect_identical(reproduce_design(out), out)
  expect_true(identical(.Random.seed, after_one_draw))
})

test_that("explicit swap seeds reproduce independently of caller state", {
  x <- matrix(c(rep(1:4, 2), 5:11, NA), 4)
  for (method in c("euclidean", "manhattan")) {
    set.seed(21)
    before <- .Random.seed
    first <- swap_pairs(x, starting_dist = 2, stop_iter = 2, dist_method = method, seed = 18)
    untouched <- identical(.Random.seed, before)
    set.seed(94)
    before <- .Random.seed
    again <- swap_pairs(x, starting_dist = 2, stop_iter = 2, dist_method = method, seed = 18)
    ok <- c(caller_stream_kept = untouched && identical(.Random.seed, before),
            same_seed_same_result = identical(again, first),
            replays = identical(reproduce_design(first), first),
            seed_recorded = identical(first$metadata$parameters$seed, 18),
            rng_kind_recorded = identical(first$metadata$rng_kind, RNGkind()))
    expect_properties(ok, method)
  }
})

test_that("swap errors and seeded calls create no .Random.seed", {
  local_rng_state()
  if (exists(".Random.seed", globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  out <- swap_pairs(matrix(c(1, 2, 1, 2), 2), seed = 18)
  replay <- reproduce_design(out)
  try(swap_pairs(matrix(1:4, 2), seed = 18), silent = TRUE)
  try(swap_pairs(matrix(c(1, 2, 1, 2), 2), seed = "18"), silent = TRUE)
  expect_identical(replay, out)
  expect_false(exists(".Random.seed", globalenv(), inherits = FALSE))
})

test_that("corrupted swap records are refused on replay", {
  # Fields are built before each call: X is evaluated lazily, after swap_pairs() seeds its stream.
  field <- swap_field(10, 14, 30, seed = 4)
  x <- swap_pairs(matrix(c(1, 2, 1, 2), 2), seed = 18)
  rule <- swap_pairs(field, seed = 6, rowcol_penalty = 2)
  off <- swap_pairs(field, seed = 6)
  corrupt <- list(
    seed_changed = function() { b <- x; b$metadata$parameters$seed <- 19; b },
    entry_changed = function() { b <- x; b$optim_design[1] <- 99; b },
    shape_changed = function() { b <- x; b$optim_design <- t(b$optim_design)[, 1, drop = FALSE]; b },
    rule_diagnostics_disagree = function() { b <- rule; b$diagnostics$rowcol_penalty <- 3; b },
    rule_diagnostics_without_rule = function() { b <- off; b$diagnostics$shared_pairs_final <- 0L; b },
    rule_parameter_missing = function() { b <- rule; b$metadata$parameters$rowcol_scope <- NULL; b }
  )
  for (nm in names(corrupt)) {
    expect_error(reproduce_design(corrupt[[nm]]()), class = "fieldhub_internal_error", info = nm)
  }
})

test_that("swap results saved by earlier versions, before the row/column rule, replay to the same layout", {
  field <- swap_field(10, 14, 30, seed = 3)
  x <- swap_pairs(field, seed = 5)
  legacy <- x
  p <- legacy$metadata$parameters
  legacy$metadata$parameters <- p[setdiff(names(p), c("rowcol_penalty", "rowcol_scope"))]
  legacy$metadata$package_version <- "1.6.0"
  expect_warning(replay <- reproduce_design(legacy), class = "fieldhub_reproduction_warning")
  expect_identical(replay[c("optim_design", "diagnostics")], x[c("optim_design", "diagnostics")])
})

# ---- The row/column rule ----

test_that("the row/column rule is recorded off by default and adds its diagnostics only when on", {
  X <- swap_field(10, 20, 40, seed = 1)
  off <- swap_pairs(X, seed = 1)
  on <- swap_pairs(X, seed = 1, rowcol_penalty = 2)
  fields <- c("rowcol_penalty", "rowcol_scope", "retained_threshold", "shared_pairs_input",
              "shared_pairs_final", "entries_sharing_final")
  ok <- c(default_recorded = identical(off$metadata$parameters[c("rowcol_penalty", "rowcol_scope")],
                                       list(rowcol_penalty = 0, rowcol_scope = "pairs")),
          off_has_no_rule_fields = !any(fields %in% names(off$diagnostics)),
          on_has_rule_fields = all(fields %in% names(on$diagnostics)),
          penalty_reported = identical(on$diagnostics$rowcol_penalty, 2),
          input_shared_is_brute_force = identical(on$diagnostics$shared_pairs_input, swap_brute_shared(X, 1:40)),
          final_shared_is_brute_force = identical(on$diagnostics$shared_pairs_final,
                                                  swap_brute_shared(on$optim_design, 1:40)))
  expect_properties(ok, "rule off and on")
})

test_that("the strict rule removes every shared pair of a typical field without losing distance", {
  skip_on_cran()
  for (seed in 1:5) {
    X <- swap_field(10, 20, 40, seed = seed)
    off <- swap_pairs(X, starting_dist = 3, seed = seed)
    on <- swap_pairs(X, starting_dist = 3, seed = seed, rowcol_penalty = Inf)
    ok <- c(swap_result_properties(on, X, "euclidean"),
            reported_zero = identical(on$diagnostics[c("shared_pairs_final", "entries_sharing_final")],
                                      list(shared_pairs_final = 0L, entries_sharing_final = 0L)),
            brute_force_zero = swap_brute_shared(on$optim_design, 1:40) == 0L,
            penalty_recorded = identical(on$metadata$parameters$rowcol_penalty, Inf),
            distance_kept = on$min_distance >= off$min_distance - 1)
    expect_properties(ok, paste("seed", seed))
  }
})

test_that("with the rule on, the kept layout is the successful step with the fewest shared pairs", {
  # Pinned case where an earlier step has fewer shared pairs than the last one
  X <- swap_field(12, 20, 60, seed = 3)
  on <- swap_pairs(X, seed = 3, rowcol_penalty = 0.5)
  successful <- on$diagnostics$thresholds_attempted - (on$diagnostics$stop_reason == "iteration_limit")
  ok <- c(swap_result_properties(on, X, "euclidean"),
          later_steps_dropped = length(on$designs) - 1L < successful,
          retained_threshold = on$diagnostics$retained_threshold == 3 + length(on$designs) - 2)
  expect_properties(ok, "pinned")
  for (seed in 1:5) {
    X <- swap_field(12, 20, 60, seed = seed)
    on <- swap_pairs(X, seed = seed, rowcol_penalty = Inf)
    steps <- on$designs[-1L]
    if (length(steps) == 0L) next
    s <- vapply(steps, swap_brute_shared, integer(1), covered = 1:60)
    ok <- c(swap_result_properties(on, X, "euclidean"),
            final_is_kept_step = identical(on$diagnostics$shared_pairs_final, s[length(s)]),
            fewest_shared = all(s[length(s)] <= s),
            retained_min = identical(on$diagnostics$retained_min_distance, on$min_distance))
    expect_properties(ok, paste("seed", seed))
  }
})

test_that("entries that cannot avoid sharing a row do not stall the sweeps", {
  # five copies on a 4-row field must share a row; scope "all" covers them
  set.seed(4)
  X <- matrix(as.numeric(sample(c(rep(1:3, each = 5L), 4:36))), 4)
  on <- swap_pairs(X, starting_dist = 1, stop_iter = 200, seed = 4, rowcol_penalty = Inf, rowcol_scope = "all")
  # Only a failing threshold may use the full sweep budget; a successful one
  # that ran to its budget would use 200 sweeps
  budget_used <- 200 * (on$diagnostics$stop_reason == "iteration_limit")
  ok <- c(swap_result_properties(on, X, "euclidean"),
          sweeps_end_early = on$diagnostics$iterations - budget_used < 100,
          sharing_remains = on$diagnostics$shared_pairs_final > 0L,
          sharing_not_worse = on$diagnostics$shared_pairs_final <= on$diagnostics$shared_pairs_input)
  expect_properties(ok, "unfixable")
})
