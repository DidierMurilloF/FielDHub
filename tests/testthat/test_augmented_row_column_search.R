# Tests for the augmented row-column search engine (research M2
# specification, revision 3, sections 2 and 4-8; implementation plan tasks
# 3-8). Fast tests use tiny budgets and small fixtures. Slow tests are gated
# by FIELDHUB_ARC_SEARCH_SLOW=true (arc_skip_unless_search_slow()).

# Test-only stub algorithm: entry check first, then one objective call.
arc_stub_search <- function(v, s, k, objective, fn) {
  entry <- arc_search_entry(v, s, k, objective)
  if (!is.na(entry)) {
    return(arc_search_result(status = entry))
  }
  score <- fn()
  arc_search_result(score = score, status = "ok",
                    calls = c(objective = 1, proxy = 0))
}

test_that("arc_search_entry returns each status", {
  expect_identical(arc_search_entry(6, 6, 2, "E_aug"), "below_min_df")
  expect_identical(arc_search_entry(48, 16, 4, "E_aug"), "infeasible_df")
  expect_identical(arc_search_entry(12, 8, 3, "E_aug"), NA_character_)
  expect_identical(arc_search_entry(12L, 8L, 3L, "V_tc"), NA_character_)
  expect_identical(arc_search_entry(12, 8, 3, "foo"), "unsupported_objective")
  for (o in arc_search_objectives) {
    expect_identical(arc_search_entry(12, 8, 3, o), NA_character_)
  }
})

test_that("arc_search_entry rejects bad objectives, objective checked first", {
  bad <- list(NULL, NA_character_, c("E_aug", "V_tt"), 1, NA, "e_aug",
              character(0), list("E_aug"))
  for (o in bad) {
    expect_identical(arc_search_entry(12, 8, 3, o), "unsupported_objective")
  }
  expect_identical(arc_search_entry("a", 8, 3, "foo"),
                   "unsupported_objective")
})

test_that("arc_search_entry rejects invalid dimensions without condition", {
  big <- .Machine$integer.max
  bad <- list(
    list(c(12, 13), 8, 3), list(12, c(8, 9), 3), list(12, 8, c(3, 4)),
    list(NA, 8, 3), list(12, NA_real_, 3), list(12, 8, NA_integer_),
    list(Inf, 8, 3), list(12, -Inf, 3), list(12, 8, NaN),
    list(0, 8, 3), list(12, 0, 3), list(-12, 8, 3), list(12, -8, 3),
    list(12.5, 8, 3), list(12, 8.5, 3), list(12, 8, 2.5),
    list(1e10, 8, 3), list(12, 1e10, 3), list(12, 8, 1e10),
    list(big + 1, 8, 3), list(1e10, 1e10, 2), list(big, big, big - 1),
    list(12, 13, 3), list(12, 8, 1), list(12, 8, 0), list(12, 8, 12),
    list(12, 8, 13), list("12", 8, 3), list(12, "8", 3), list(12, 8, "3"),
    list(NULL, 8, 3), list(12, NULL, 3), list(12, 8, NULL),
    list(TRUE, 8, 3), list(12, 8, matrix(3)), list(list(12), 8, 3),
    list(big, 2, 1e10)
  )
  for (case in bad) {
    result <- arc_search_entry(case[[1]], case[[2]], case[[3]], "E_aug")
    expect_no_condition(
      arc_search_entry(case[[1]], case[[2]], case[[3]], "E_aug"))
    expect_identical(result, "invalid_dimensions")
  }
})

test_that("arc_search_entry is safe near the integer boundary", {
  big <- .Machine$integer.max
  expect_no_condition(arc_search_entry(big, big, 2, "E_aug"))
  expect_identical(arc_search_entry(big, 1, 2, "E_aug"), "infeasible_df")
  expect_identical(arc_search_entry(big, 40000, 2, "E_aug"), "infeasible_df")
  expect_identical(arc_search_entry(40000, 20000, 4, "E_aug"), NA_character_)
  expect_identical(arc_search_entry(3, 1, 2, "E_aug"), "infeasible_df")
})

test_that("arc_search_result has stable fields and validates status", {
  res <- arc_search_result(status = "ok", score = 1.5, seed = 7L,
                           calls = c(objective = 4, proxy = 2))
  expect_named(res, c("design", "score", "status", "phases", "trace",
                      "objective_calls", "proxy_calls", "proposals",
                      "failed_proposals", "disconnected", "search_secs",
                      "verify_secs", "total_secs", "seed", "final", "valid"))
  expect_identical(res$objective_calls, 4)
  expect_identical(res$proxy_calls, 2)
  expect_null(res$design)
  expect_identical(res$seed, 7L)
  expect_true(is.na(res$valid))
  for (st in list(NULL, NA_character_, "bogus", c("ok", "no_design"), 1)) {
    expect_error(arc_search_result(status = st),
                 class = "fieldhub_arc_input_error")
  }
  expect_error(arc_search_result(), class = "fieldhub_arc_input_error")
  for (st in arc_search_statuses) {
    expect_identical(arc_search_result(status = st)$status, st)
  }
})

test_that("entry failures make zero objective calls", {
  cases <- list(
    list(6, 6, 2, "E_aug"), list(48, 16, 4, "V_tt"), list(12, 8, 3, "foo"),
    list(12.5, 8, 3, "E_aug"), list(NA, 8, 3, "E_aug"),
    list(1e10, 8, 3, "E_aug"), list(12, 8, 1, "E_aug"),
    list(12, 8, 3, NA_character_), list("12", 8, 3, "V_tc")
  )
  for (case in cases) {
    counter <- arc_counting_objective()
    res <- arc_stub_search(case[[1]], case[[2]], case[[3]], case[[4]],
                           counter$fn)
    expect_identical(counter$count(), 0L)
    expect_null(res$design)
    expect_false(res$status %in% c("ok", "no_design"))
    expect_identical(res$objective_calls, 0)
  }
  counter <- arc_counting_objective(2)
  res <- arc_stub_search(12, 8, 3, "E_aug", counter$fn)
  expect_identical(counter$count(), 1L)
  expect_identical(res$status, "ok")
})

# ---- Task 4: global budget, accounting and deadlines ----

test_that("arc_budget_new builds the environment with defaults", {
  clk <- arc_fake_clock(5)
  b <- arc_budget_new(max_evals = 10, seconds = 3, now = clk$now)
  expect_true(is.environment(b))
  expect_identical(b$max_evals, 10)
  expect_identical(b$t_start, 5)
  expect_identical(b$deadline, 8)
  expect_identical(b$evals, c(objective = 0, proxy = 0))
  expect_identical(b$proposals, 0)
  expect_identical(b$failed, 0)
  expect_identical(b$disconnected, 0)
  expect_identical(b$events, character(0))
  expect_identical(b$trace, list())
  expect_identical(b$max_consec_fail, 200L)
  expect_identical(b$proposal_factor, 20)
  b2 <- arc_budget_new()
  expect_identical(b2$max_evals, Inf)
  expect_identical(b2$deadline, Inf)
  expect_true(is.numeric(arc_clock()))
})

test_that("arc_budget_new validates every argument", {
  bad_call <- function(...) {
    expect_error(arc_budget_new(...),
                 class = "fieldhub_arc_input_error")
  }
  for (x in list(-1, NA_real_, c(1, 2), "a", NULL, NaN, TRUE)) {
    bad_call(max_evals = x)
  }
  for (x in list(NA_real_, c(1, 2), "a", NULL, NaN, TRUE)) {
    bad_call(seconds = x)
  }
  for (x in list(0, 1.5, NA_real_, c(1, 2), "a", NULL, Inf, -3)) {
    bad_call(max_consec_fail = x)
  }
  for (x in list(0, -1, Inf, NA_real_, c(1, 2), "a", NULL, NaN)) {
    bad_call(proposal_factor = x)
  }
  for (x in list(1, "f", NULL, NA)) {
    bad_call(now = x)
  }
  expect_error(arc_budget_new(max_evals = -1),
               class = "fieldhub_input_error")
  expect_silent(arc_budget_new(max_evals = 0))
  expect_silent(arc_budget_new(seconds = -5))
})

test_that("the cap is exact and internal count equals external count", {
  b <- arc_budget_new(max_evals = 37)
  ctr <- arc_counting_objective(1)
  got <- NULL
  for (i in 1:100) {
    y <- arc_budget_eval(b, ctr$fn, i)
    if (!is.null(y)) got <- c(got, y)
  }
  expect_identical(ctr$count(), 37L)
  expect_identical(b$evals[["objective"]], 37)
  expect_identical(arc_budget_used(b), 37)
  expect_length(got, 37)
  expect_identical(arc_budget_left(b), 0)
  expect_identical(arc_budget_out(b), "budget")
  expect_null(arc_budget_eval(b, ctr$fn, 1))
  expect_identical(ctr$count(), 37L)
})

test_that("objective and proxy calls are counted separately, both count", {
  b <- arc_budget_new(max_evals = 37)
  obj <- arc_counting_objective(1)
  prx <- arc_counting_objective(2)
  for (i in 1:100) {
    if (i %% 3 == 0) {
      arc_budget_eval(b, prx$fn, i, kind = "proxy")
    } else {
      arc_budget_eval(b, obj$fn, i)
    }
  }
  expect_identical(obj$count() + prx$count(), 37L)
  expect_identical(b$evals[["objective"]], as.double(obj$count()))
  expect_identical(b$evals[["proxy"]], as.double(prx$count()))
  expect_gt(prx$count(), 0L)
  calls <- arc_budget_calls(b)
  expect_identical(names(calls), c("objective", "proxy"))
  expect_identical(sum(calls), 37)
  res <- arc_search_result(status = "budget", calls = calls)
  expect_identical(res$objective_calls, calls[["objective"]])
  expect_identical(res$proxy_calls, calls[["proxy"]])
})

test_that("arc_budget_eval rejects an unknown kind", {
  b <- arc_budget_new()
  for (k in list("other", NA_character_, 1, c("objective", "proxy"), NULL)) {
    expect_error(arc_budget_eval(b, function(x) 1, 1, kind = k),
                 class = "fieldhub_arc_input_error")
  }
  expect_identical(arc_budget_used(b), 0)
})

test_that("an erroring objective still counts", {
  b <- arc_budget_new(max_evals = 5)
  expect_error(arc_budget_eval(b, function(x) stop("boom"), 1), "boom")
  expect_identical(b$evals[["objective"]], 1)
})

test_that("NA objective values count as disconnected", {
  b <- arc_budget_new(max_evals = 10)
  y <- arc_budget_eval(b, function(x) NA_real_, 1)
  expect_true(is.na(y))
  arc_budget_eval(b, function(x) 3, 1)
  arc_budget_eval(b, function(x) NA_real_, 1, kind = "proxy")
  expect_identical(b$disconnected, 2)
  expect_identical(arc_budget_used(b), 3)
})

test_that("an expired deadline spends no call", {
  ctr <- arc_counting_objective(1)
  b <- arc_budget_new(seconds = -1)
  expect_identical(arc_budget_out(b), "time_limit")
  expect_null(arc_budget_eval(b, ctr$fn, 1))
  clk <- arc_fake_clock(100)
  b0 <- arc_budget_new(seconds = 0, now = clk$now)
  expect_identical(arc_budget_out(b0), "time_limit")
  expect_null(arc_budget_eval(b0, ctr$fn, 1, kind = "proxy"))
  expect_identical(ctr$count(), 0L)
  expect_identical(arc_budget_used(b0), 0)
})

test_that("deadline overrun is at most one indivisible operation", {
  clk <- arc_fake_clock(0)
  b <- arc_budget_new(seconds = 1.0, now = clk$now)
  calls <- 0L
  fn <- function(x) {
    calls <<- calls + 1L
    clk$advance(0.3)
    1
  }
  repeat {
    if (is.null(arc_budget_eval(b, fn, 1))) break
  }
  expect_identical(calls, 4L)
  expect_identical(arc_budget_out(b), "time_limit")
  expect_lte(clk$now() - b$deadline, 0.3 + 1e-9)
  expect_gte(clk$now(), b$deadline)
})

test_that("budget exhaustion takes precedence over the deadline", {
  clk <- arc_fake_clock(0)
  b <- arc_budget_new(max_evals = 1, seconds = 1, now = clk$now)
  arc_budget_eval(b, function(x) 1, 1)
  clk$advance(5)
  expect_identical(arc_budget_out(b), "budget")
  b2 <- arc_budget_new(max_evals = 0, seconds = -1)
  expect_identical(arc_budget_out(b2), "budget")
})

test_that("uncapped budget, elapsed and seconds left follow the clock", {
  clk <- arc_fake_clock(10)
  b <- arc_budget_new(seconds = 4, now = clk$now)
  expect_identical(arc_budget_left(b), Inf)
  expect_true(is.na(arc_budget_out(b)))
  clk$advance(1.5)
  expect_equal(arc_budget_elapsed(b), 1.5)
  expect_equal(arc_budget_secs_left(b), 2.5)
  clk$advance(10)
  expect_lt(arc_budget_secs_left(b), 0)
  expect_identical(arc_budget_secs_left(arc_budget_new()), Inf)
  b3 <- arc_budget_new(max_evals = 10)
  arc_budget_eval(b3, function(x) 0, 1)
  expect_identical(arc_budget_left(b3), 9)
})

test_that("log, trace and proposal counters accumulate", {
  b <- arc_budget_new()
  expect_null(arc_budget_trace(b))
  arc_budget_log(b, "start ", 1)
  arc_budget_log(b, "stop")
  expect_identical(b$events, c("start 1", "stop"))
  arc_budget_add_trace(b, data.frame(phase = "a", score = 1))
  arc_budget_add_trace(b, data.frame(phase = "b", score = 2))
  tr <- arc_budget_trace(b)
  expect_identical(nrow(tr), 2L)
  expect_identical(tr$phase, c("a", "b"))
  arc_budget_note_proposal(b, FALSE)
  arc_budget_note_proposal(b, TRUE)
  arc_budget_note_proposal(b, TRUE)
  expect_identical(b$proposals, 3)
  expect_identical(b$failed, 2)
})

# ---- Fresh search evaluator (specification section 6.1; plan task 5) ----

# Compare the search evaluator with arc_evaluate_objectives() on one
# contraction: identical classification, objectives within 1e-12.
arc_expect_evaluator_match <- function(contraction, v, mode, label) {
  k <- nrow(contraction)
  s <- ncol(contraction)
  prep <- arc_search_prep(v, s, k)
  got <- arc_search_evaluate(contraction, prep)
  ref <- arc_evaluate_objectives(contraction, v, mode)
  expect_identical(got$connected, ref$connected, label = label)
  if (isTRUE(ref$connected)) {
    for (o in c("E_aug", "V_tt", "V_tc", "V_cc")) {
      expect_lte(abs(got[[o]] - ref[[o]]), 1e-12,
                 label = paste(label, o))
    }
    expect_lte(abs(got$lambda_min_rel - ref$lambda_min_rel), 1e-12,
               label = paste(label, "lambda_min_rel"))
  }
  invisible(got)
}

test_that("arc_search_prep holds the precomputed pieces", {
  p <- arc_search_prep(12L, 8L, 3L)
  expect_identical(p$n_tests, 72L)
  expect_identical(p$v_star, 75L)
  expect_identical(p$df, arc_df_formula(12, 8, 3))
  expect_identical(dim(p$q0q0), c(20L, 20L))
  expect_identical(dim(p$col_block), c(8L, 8L))
  expect_identical(p$col_index, rep(1:8, each = 3L))
  expect_identical(p$row_index, rep(1:3, times = 8L))
})

test_that("the search evaluator matches M1 on every restricted fixture", {
  manifest <- utils::read.csv(
    test_path("fixtures", "augmented-row-column", "manifest.csv"),
    stringsAsFactors = FALSE
  )
  rows <- manifest[manifest$kind == "contraction" &
                     manifest$mode == "restricted", ]
  expect_gt(nrow(rows), 20L)
  for (i in seq_len(nrow(rows))) {
    contraction <- arc_fixture(rows$file[i])
    arc_expect_evaluator_match(contraction, rows$v[i], "restricted",
                               rows$file[i])
  }
})

# Random column-binary k x s matrix with replication r (r <= s), filled
# column by column: labels that must appear in every remaining column are
# forced, the rest drawn with probability proportional to what remains.
# Bounded restarts; NULL when none is found.
arc_greedy_column_binary <- function(k, s, r, max_restarts = 50L) {
  for (restart in seq_len(max_restarts)) {
    left <- r
    m <- matrix(0L, k, s)
    ok <- TRUE
    for (j in seq_len(s)) {
      forced <- which(left == s - j + 1L)
      pool <- setdiff(which(left > 0L), forced)
      need <- k - length(forced)
      if (need < 0L || need > length(pool)) {
        ok <- FALSE
        break
      }
      extra <- if (need > 0L) {
        pool[sample.int(length(pool), need, prob = left[pool])]
      }
      col <- c(forced, extra)
      m[, j] <- sample(col, k)
      left[col] <- left[col] - 1L
    }
    if (ok) return(m)
  }
  NULL
}

test_that("a seeded sweep matches M1, including df 0, df 1, disconnected", {
  arc_local_seed(20261010)
  # (v, s, k) with df = (k - 1)(s - 1) - v + 1: forced df 0 and df 1 sets,
  # then random small sets with v <= 14.
  forced <- list(c(7, 4, 3), c(6, 6, 2), c(9, 5, 3), c(10, 4, 4),
                 c(6, 4, 3), c(8, 5, 3), c(9, 4, 4), c(12, 5, 4))
  random <- list()
  while (length(random) < 16L) {
    v <- sample(4:14, 1L)
    k <- sample(2:(v - 1L), 1L)
    s <- sample(2:v, 1L)
    if (arc_df_formula(v, s, k) >= 0L && k <= s) {
      random[[length(random) + 1L]] <- c(v, s, k)
    }
  }
  counts <- c(states = 0, disconnected = 0, df0 = 0, df1 = 0)
  for (vsk in c(forced, random)) {
    v <- as.integer(vsk[1])
    s <- as.integer(vsk[2])
    k <- as.integer(vsk[3])
    df <- arc_df_formula(v, s, k)
    for (rep in seq_len(6L)) {
      balanced <- rep %% 2L == 1L
      total <- k * s
      r <- as.integer(sample(rep(c(total %/% v, total %/% v + 1L),
                                 c(v - total %% v, total %% v))))
      if (!balanced) {
        # Unbalance by random unit transfers within 1 <= r <= min(k, s).
        for (move in seq_len(v)) {
          from <- which(r > 1L)
          to <- which(r < min(k, s))
          if (length(from) == 0L || length(to) == 0L) break
          a <- from[sample.int(length(from), 1L)]
          b <- to[sample.int(length(to), 1L)]
          if (a != b) {
            r[a] <- r[a] - 1L
            r[b] <- r[b] + 1L
          }
        }
      }
      filled <- arc_greedy_column_binary(k, s, r)
      if (is.null(filled)) next
      contraction <- arc_recolor_rows(filled, v)
      mode <- if (balanced) "restricted" else "column_binary"
      got <- arc_expect_evaluator_match(
        contraction, v, mode, sprintf("v%d s%d k%d rep%d", v, s, k, rep))
      counts[["states"]] <- counts[["states"]] + 1
      counts[["disconnected"]] <- counts[["disconnected"]] + !got$connected
      counts[["df0"]] <- counts[["df0"]] + (df == 0L)
      counts[["df1"]] <- counts[["df1"]] + (df == 1L)
    }
  }
  expect_gte(counts[["disconnected"]], 1)
  expect_gte(counts[["df0"]], 1)
  expect_gte(counts[["df1"]], 1)
  expect_gte(counts[["states"]], 100)
})

# Latin square on labels x: rows x, x shifted by one, x shifted by two.
arc_two_group_design <- function() {
  latin <- function(x) rbind(x, x[c(2L, 3L, 1L)], x[c(3L, 1L, 2L)])
  m <- cbind(latin(1:3), latin(4:6))
  dimnames(m) <- NULL
  m
}

test_that("the constructed two-group design is disconnected in both", {
  m <- arc_two_group_design()
  expect_identical(arc_df_formula(6, 6, 3), 5L)
  prep <- arc_search_prep(6L, 6L, 3L)
  got <- arc_search_evaluate(m, prep)
  ref <- arc_evaluate_objectives(m, 6L, "restricted")
  expect_false(got$connected)
  expect_false(ref$connected)
  expect_lt(got$lambda_min_rel, 1e-8)
  expect_identical(names(got), c("connected", "lambda_min_rel"))
  for (o in arc_search_objectives) {
    expect_identical(arc_search_score(m, prep, o), NA_real_)
  }
})

test_that("negative df returns disconnected without matrix work", {
  # (48, 16, 4) has df < 0; the contraction content is irrelevant.
  prep <- arc_search_prep(48L, 16L, 4L)
  expect_lt(prep$df, 0L)
  got <- arc_search_evaluate(matrix(1L, 4L, 16L), prep)
  expect_identical(got, list(connected = FALSE, lambda_min_rel = NA_real_))
})

test_that("a row repeat raises the classed restricted-row error", {
  m <- arc_fixture("example1-contraction.csv")
  prep <- arc_search_prep(12L, 8L, 3L)
  # Within column j, move label a = m[1, 1] into row 1: still column-binary,
  # but row 1 now holds a twice.
  a <- m[1, 1]
  hit <- which(m == a, arr.ind = TRUE)
  hit <- hit[hit[, 2] != 1, , drop = FALSE]
  i <- hit[1, 1]
  j <- hit[1, 2]
  bad <- m
  bad[c(1, i), j] <- m[c(i, 1), j]
  expect_gt(anyDuplicated(bad[1, ]), 0L)
  expect_true(all(apply(bad, 2L, anyDuplicated) == 0L))
  cond <- tryCatch(arc_search_evaluate(bad, prep), error = function(e) e)
  expect_s3_class(cond, "fieldhub_arc_row_repeat_error")
  expect_s3_class(cond, "fieldhub_arc_input_error")
  expect_s3_class(cond, "fieldhub_input_error")
  expect_error(arc_search_score(bad, prep, "E_aug"),
               class = "fieldhub_arc_row_repeat_error")
})

test_that("arc_search_score negates variances and rejects other objectives", {
  m <- arc_fixture("example1-contraction.csv")
  prep <- arc_search_prep(12L, 8L, 3L)
  e <- arc_search_evaluate(m, prep)
  expect_true(e$connected)
  expect_identical(arc_search_score(m, prep, "E_aug"), e$E_aug)
  expect_identical(arc_search_score(m, prep, "V_tt"), -e$V_tt)
  expect_identical(arc_search_score(m, prep, "V_tc"), -e$V_tc)
  for (o in list("V_cc", "foo", NA_character_, c("E_aug", "V_tt"), 1)) {
    expect_error(arc_search_score(m, prep, o),
                 class = "fieldhub_arc_input_error")
  }
})

test_that("arc_column_proxy matches an independent A-efficiency", {
  m <- arc_fixture("example1-contraction.csv")
  v <- 12L
  k <- nrow(m)
  s <- ncol(m)
  n <- matrix(0, v, s)
  for (j in seq_len(s)) {
    for (i in seq_len(k)) {
      n[m[i, j], j] <- n[m[i, j], j] + 1
    }
  }
  r <- rowSums(n)
  info <- diag(r) - n %*% t(n) / k
  j_v <- matrix(1 / v, v, v)
  trace_pinv <- sum(diag(solve(info + j_v))) - 1
  expected <- (v - 1) / (mean(r) * trace_pinv)
  expect_lte(abs(arc_column_proxy(m, v) - expected), 1e-12)
  # Column-binary input with repeated row labels is accepted.
  shuffled <- apply(m, 2L, sample)
  expect_lte(abs(arc_column_proxy(shuffled, v) - expected), 1e-12)
  # The two-group design has a disconnected column design.
  expect_identical(arc_column_proxy(arc_two_group_design(), 6L), NA_real_)
})

test_that("the evaluator decides connectivity by the M1 spectral predicate", {
  src <- paste(c(deparse(arc_search_evaluate), deparse(arc_search_score)),
               collapse = "\n")
  expect_false(grepl("det(", src, fixed = TRUE))
  expect_false(grepl("determinant", src, fixed = TRUE))
  expect_false(grepl("pivot", src, fixed = TRUE))
  expect_true(grepl("arc_classify_spectrum", src, fixed = TRUE))
  expect_true(grepl("arc_search_evaluate", deparse1(arc_search_score),
                    fixed = TRUE))
})

# ---- Moves, lazy scans and exhaustion (task 6; specification 5.1, 5.4) ----

test_that("cyclic 5 x 5 has no A/B/C/K move and exactly 10 column moves", {
  arc_local_seed(601)
  cyclic <- arc_fixture("cyclic-5x5-k4.csv")
  never <- function(candidate) FALSE
  joint <- arc_scan_neighbourhood(cyclic, "joint", never)
  expect_identical(joint, list(status = "complete", n_feasible = 0L))
  row <- arc_scan_neighbourhood(cyclic, "row", never)
  expect_identical(row, list(status = "complete", n_feasible = 0L))
  column <- arc_scan_neighbourhood(cyclic, "column", never)
  expect_identical(column, list(status = "complete", n_feasible = 10L))
  expect_identical(arc_classify_exhaustion(cyclic, "joint"),
                   "empty_neighbourhood")
  expect_identical(arc_classify_exhaustion(cyclic, "row"),
                   "empty_neighbourhood")
  expect_identical(arc_classify_exhaustion(cyclic, "column"),
                   "sampling_exhausted")
  expect_null(arc_propose_joint(cyclic))
  expect_null(arc_propose_row(cyclic))
  expect_false(is.null(arc_propose_column(cyclic, tries = 200L)))
})

test_that("an interrupted or oversized exhaustion check certifies nothing", {
  arc_local_seed(602)
  cyclic <- arc_fixture("cyclic-5x5-k4.csv")
  always <- function() TRUE
  scan <- arc_scan_neighbourhood(cyclic, "joint", function(x) FALSE,
                                 stop_fn = always)
  expect_identical(scan, list(status = "interrupted", n_feasible = 0L))
  expect_identical(arc_classify_exhaustion(cyclic, "joint", stop_fn = always),
                   "sampling_exhausted")
  expect_identical(arc_classify_exhaustion(cyclic, "joint", cap_cells = 19L),
                   "sampling_exhausted")
  expect_identical(arc_classify_exhaustion(cyclic, "joint", cap_cells = 20L),
                   "empty_neighbourhood")
})

test_that("scan stops on visit and checks stop_fn before every move", {
  arc_local_seed(603)
  start <- arc_random_start(12, 8, 3)
  hits <- 0L
  scan <- arc_scan_neighbourhood(start, "joint", function(x) {
    hits <<- hits + 1L
    hits >= 3L
  })
  expect_identical(scan$status, "stopped")
  expect_identical(scan$n_feasible, 3L)
  for (kind in c("joint", "column", "row")) {
    for (n in c(0L, 1L, 5L, 20L)) {
      stopper <- arc_stop_after(n)
      visits <- 0L
      scan <- arc_scan_neighbourhood(start, kind, function(x) {
        visits <<- visits + 1L
        FALSE
      }, stop_fn = stopper$fn)
      expect_identical(scan$status, "interrupted")
      expect_lte(visits, n)
      expect_identical(scan$n_feasible, visits)
      expect_identical(stopper$calls(), n + 1L)
    }
  }
  expect_error(arc_scan_neighbourhood(start, "bogus", function(x) FALSE))
})

test_that("generated moves keep binarity and the right incidence", {
  candidate_ok <- function(m, base, kind) {
    ok <- arc_is_column_binary(m) &&
      identical(sort(as.integer(m)), sort(as.integer(base)))
    if (kind == "column") {
      ok && arc_changed_columns(m, base) == 2L
    } else if (kind == "row") {
      ok && arc_is_row_binary(m) &&
        identical(arc_column_sets(m), arc_column_sets(base))
    } else {
      ok && arc_is_row_binary(m)
    }
  }
  samplers <- list(joint = arc_propose_joint, column = arc_propose_column,
                   row = arc_propose_row)
  cases <- list(c(12, 8, 3), c(10, 6, 4), c(9, 9, 2))
  seen <- c(joint = 0L, column = 0L, row = 0L)
  for (seed in 611:614) {
    arc_local_seed(seed)
    starts <- lapply(cases, function(d) arc_random_start(d[1L], d[2L], d[3L]))
    starts <- c(starts, list(arc_fixture("restricted-12x8-a.csv")))
    for (base in starts) {
      expect_true(arc_is_row_binary(base))
      for (kind in names(samplers)) {
        drawn <- Filter(Negate(is.null),
                        lapply(1:15, function(d) samplers[[kind]](base)))
        seen[[kind]] <- seen[[kind]] + length(drawn)
        ok <- vapply(drawn, candidate_ok, logical(1), base = base,
                     kind = kind)
        expect_true(all(ok), info = paste(seed, kind, "sampler"))
        ok <- logical(0)
        arc_scan_neighbourhood(base, kind, function(m) {
          ok[length(ok) + 1L] <<- candidate_ok(m, base, kind)
          length(ok) >= 25L
        })
        seen[[kind]] <- seen[[kind]] + length(ok)
        expect_true(all(ok), info = paste(seed, kind, "scan"))
      }
    }
  }
  # some row neighbourhoods are empty (k = 2 with one Kempe cycle), but
  # every kind produced candidates overall
  expect_true(all(seen > 0L))
})

test_that("two-cell feasibility follows the binarity rules", {
  # rows (1, 2, 5), (2, 5, 1), (3, 6, 4)
  m <- matrix(c(1L, 2L, 3L,
                2L, 5L, 6L,
                5L, 1L, 4L), nrow = 3L)
  # same cell and equal labels are never feasible
  expect_false(arc_swap_feasible(m, 1, 1, 1, 1))
  expect_false(arc_swap_feasible(m, 2, 1, 1, 2))
  # same column (A): 2 would enter row 1, which already holds 2
  expect_false(arc_swap_feasible(m, 1, 1, 2, 1))
  expect_true(arc_swap_feasible(m, 1, 1, 2, 1, restricted = FALSE))
  expect_true(arc_swap_feasible(m, 1, 1, 3, 1))
  expect_true(arc_swap_feasible(m, 2, 2, 3, 2))
  # different row and column: 1 <-> 6 keeps both binarities
  expect_true(arc_swap_feasible(m, 1, 1, 3, 2))
  # 1 <-> 5 is column binary but 5 is already in row 1 (column 3)
  expect_false(arc_swap_feasible(m, 1, 1, 2, 2))
  expect_true(arc_swap_feasible(m, 1, 1, 2, 2, restricted = FALSE))
  # column binarity: 2 is already in column 1
  expect_false(arc_swap_feasible(m, 3, 1, 1, 2))
  expect_false(arc_swap_feasible(m, 3, 1, 1, 2, restricted = FALSE))
  swapped <- arc_swap_apply(m, 1, 1, 3, 2)
  expect_identical(swapped[1, 1], 6L)
  expect_identical(swapped[3, 2], 1L)
})

test_that("Kempe components partition columns and swaps keep N_C", {
  for (seed in 621:623) {
    arc_local_seed(seed)
    base <- arc_random_start(12, 8, 3)
    for (i in 1:2) for (ii in (i + 1L):3) {
      comps <- arc_kempe_components(base, i, ii)
      expect_identical(sort(unlist(comps)), seq_len(ncol(base)))
      for (cols in comps) {
        m <- arc_kempe_apply(base, i, ii, cols)
        expect_true(arc_is_row_binary(m))
        expect_true(arc_is_column_binary(m))
        expect_identical(arc_column_sets(m), arc_column_sets(base))
      }
    }
  }
  cyclic <- arc_fixture("cyclic-5x5-k4.csv")
  expect_length(arc_kempe_components(cyclic, 1, 2), 1L)
})

test_that("random recolouring keeps N_C and is restricted", {
  arc_local_seed(631)
  base <- arc_fixture("restricted-12x8-a.csv")
  for (rep in 1:5) {
    m <- arc_random_recolor(base, 12)
    expect_true(is.integer(m))
    expect_true(arc_is_row_binary(m))
    expect_identical(arc_column_sets(m), arc_column_sets(base))
  }
})

test_that("balanced replication and random starts are bounded", {
  arc_local_seed(641)
  for (d in list(c(12, 8, 3), c(10, 6, 4), c(9, 9, 2), c(20, 15, 4))) {
    r <- arc_balanced_replication(d[1L], d[2L], d[3L])
    expect_true(is.integer(r))
    expect_identical(sum(r), as.integer(d[2L] * d[3L]))
    expect_lte(max(r) - min(r), 1L)
    m <- arc_random_start(d[1L], d[2L], d[3L], replication = r)
    expect_identical(dim(m), as.integer(c(d[3L], d[2L])))
    expect_true(arc_is_row_binary(m) && arc_is_column_binary(m))
    expect_identical(tabulate(m, nbins = d[1L]), r)
  }
  expect_null(arc_random_start(12, 8, 3, should_stop = function() TRUE))
  # r_1 = 9 exceeds s = 8: no column-binary fill exists
  impossible <- c(9L, 3L, 2L, 2L, rep(1L, 8L))
  expect_identical(sum(impossible), 24L)
  expect_null(arc_random_start(12, 8, 3, replication = impossible))
  expect_null(arc_random_start(12, 8, 3, replication = 1:3))
  # a tiny budget gives up without hanging
  arc_local_seed(642)
  tiny <- arc_random_start(20, 20, 10, tries = 1L, repair = 1L)
  expect_null(tiny)
})
