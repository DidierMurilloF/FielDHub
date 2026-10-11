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
