# swap_pairs() exposes the row/column rule; results recorded before it still validate and replay.
# Fields are built before each call: X is evaluated lazily, after swap_pairs() seeds its stream.
api_field <- function(seed) {
  set.seed(seed)
  matrix(as.numeric(sample(c(rep(1:30, each = 2L), 31:140))), 10)
}

test_that("swap_pairs() records the rule and returns its diagnostics", {
  X1 <- api_field(1)
  x <- swap_pairs(X1, seed = 3, rowcol_penalty = Inf, rowcol_scope = "pairs")
  expect_identical(utils::tail(names(x$metadata$parameters), 2L), c("rowcol_penalty", "rowcol_scope"))
  expect_identical(x$metadata$parameters$rowcol_penalty, Inf)
  expect_identical(x$diagnostics$shared_pairs_final, 0L)
  expect_silent(validate_fieldhub_optimization(x))
  expect_identical(reproduce_design(x)$optim_design, x$optim_design)
})

test_that("the default records the rule off and adds no diagnostics", {
  X2 <- api_field(2)
  x <- swap_pairs(X2, seed = 4)
  expect_identical(x$metadata$parameters$rowcol_penalty, 0)
  expect_identical(x$metadata$parameters$rowcol_scope, "pairs")
  expect_false("shared_pairs_final" %in% names(x$diagnostics))
})

test_that("a result recorded before the rule validates and replays to the same layout", {
  X3 <- api_field(3)
  x <- swap_pairs(X3, seed = 5)
  legacy <- x
  p <- legacy$metadata$parameters
  legacy$metadata$parameters <- p[setdiff(names(p), c("rowcol_penalty", "rowcol_scope"))]
  expect_silent(validate_fieldhub_optimization(legacy))
  replay <- reproduce_design(legacy)
  expect_identical(replay$optim_design, x$optim_design)
  expect_identical(replay$diagnostics, x$diagnostics)
})

test_that("the validator rejects rule diagnostics that disagree with the parameters", {
  X4 <- api_field(4)
  x <- swap_pairs(X4, seed = 6, rowcol_penalty = 2)
  bad <- x
  bad$diagnostics$rowcol_penalty <- 3
  expect_error(validate_fieldhub_optimization(bad), class = "fieldhub_internal_error")
  off <- swap_pairs(X4, seed = 6)
  extra <- off
  extra$diagnostics$shared_pairs_final <- 0L
  expect_error(validate_fieldhub_optimization(extra), class = "fieldhub_internal_error")
  partial <- x
  partial$metadata$parameters$rowcol_scope <- NULL
  expect_error(validate_fieldhub_optimization(partial), class = "fieldhub_internal_error")
})

test_that("invalid rule arguments fail before any random draw", {
  X5 <- api_field(5)
  set.seed(9)
  before <- .Random.seed
  expect_error(swap_pairs(X5, rowcol_penalty = -1), class = "fieldhub_error")
  expect_error(swap_pairs(X5, rowcol_scope = "rows"), class = "fieldhub_error")
  expect_identical(.Random.seed, before)
})

test_that("a truncated rule-on result validates and replays", {
  set.seed(3)
  X <- matrix(as.numeric(sample(c(rep(seq_len(60), each = 2L), seq.int(61L, 180L)))), 12)
  x <- swap_pairs(X, seed = 3, rowcol_penalty = 0.5)
  expect_lt(length(x$designs) - 1L, x$diagnostics$thresholds_attempted -
              (x$diagnostics$stop_reason == "iteration_limit"))
  expect_silent(validate_fieldhub_optimization(x))
  expect_identical(reproduce_design(x)$optim_design, x$optim_design)
})

test_that("a rule-on result with unfixable entries validates", {
  set.seed(4)
  X <- matrix(as.numeric(sample(c(rep(1:3, each = 5L), 4:36))), 4)
  x <- swap_pairs(X, starting_dist = 1, stop_iter = 50, seed = 4, rowcol_penalty = Inf, rowcol_scope = "all")
  expect_gt(x$diagnostics$shared_pairs_final, 0L)
  expect_silent(validate_fieldhub_optimization(x))
})
