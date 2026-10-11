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
