engine_cases <- list(c(20, 2, 4), c(101, 2, 4), c(50, 6, 5), c(13, 4, 13), c(30, 11, 6), c(5, 8, 20))

test_that("designs have exact margins and are connected", {
  for (case in engine_cases) {
    for (seed in 1:3) {
      set.seed(seed)
      d <- blocks_engine(case[1], case[2], case[3], searches = 5L)
      expect_true(has_invariant_counts(d$Design, list(treatments = levels(d$Design$treatments)),
                                       expected = case[2]))
      N <- engine_ref_counts(d$Design)
      n <- case[1] * case[2]
      expect_identical(as.vector(rowSums(N)), rep(as.double(case[2]), case[1]))
      expect_setequal(unique(colSums(N)), unique(n %/% case[3] + (seq_len(case[3]) <= n %% case[3])))
      expect_identical(engine_ref_efficiency(N, case[2])[["rank"]], case[1] - 1)
    }
  }
})

test_that("the margin check rejects a corrupted design", {
  set.seed(1)
  d <- blocks_engine(20, 2, 4, searches = 2L)
  corrupted <- d$Design
  corrupted$treatments[1] <- corrupted$treatments[2]
  if (identical(corrupted$treatments[1], d$Design$treatments[1])) {
    corrupted$treatments[1] <- levels(corrupted$treatments)[
      (as.integer(corrupted$treatments[1]) %% 20) + 1]
  }
  expect_false(has_invariant_counts(corrupted, list(treatments = levels(corrupted$treatments)),
                                    expected = 2))
})

test_that("reported efficiencies match the eigenvalue reference and respect the bound", {
  for (case in engine_cases) {
    set.seed(2)
    d <- blocks_engine(case[1], case[2], case[3], searches = 5L)
    ref <- engine_ref_efficiency(engine_ref_counts(d$Design), case[2])
    expect_lte(abs(d$Blocks_model[["D-Efficiency"]] - ref[["D"]]), 1e-10)
    expect_lte(abs(d$Blocks_model[["A-Efficiency"]] - ref[["A"]]), 1e-10)
    expect_lte(d$Blocks_model[["A-Efficiency"]], d$Blocks_model[["A-Bound"]] + 1e-10)
  }
})

test_that("evaluations never exceed the budget and the stop reason is recorded", {
  set.seed(3)
  small <- blocks_engine(100, 2, 4, budget = 1000)
  expect_lte(small$search$evaluations, 1000)
  expect_identical(small$search$stop_reason, "budget")
  full <- blocks_engine(20, 2, 4)
  expect_lte(full$search$evaluations, full$search$budget)
  expect_true(full$search$stop_reason %in% c("budget", "stale"))
})

test_that("the time limit stops the search with a design warning", {
  set.seed(4)
  expect_warning(
    d <- blocks_engine(1000, 2, 4, budget = 1e9, time_limit = 1e-6),
    class = "fieldhub_design_warning"
  )
  expect_identical(d$search$stop_reason, "time_limit")
})

test_that("a stop before the first start is reported as such", {
  expect_error(blocks_engine(2000, 2, 6, budget = 100), class = "fieldhub_input_error",
               regexp = "budget was reached before a connected starting design")
})

test_that("the engine follows and advances the caller's random stream", {
  set.seed(5); a <- blocks_engine(30, 2, 4); after_a <- runif(1)
  set.seed(5); b <- blocks_engine(30, 2, 4); after_b <- runif(1)
  set.seed(5); plain <- runif(1)
  expect_identical(a, b)
  expect_identical(after_a, after_b)
  expect_false(identical(after_a, plain))
})

test_that("debug mode checks every accepted swap against a fresh computation", {
  old <- Sys.getenv("FIELDHUB_ENGINE_DEBUG", unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv("FIELDHUB_ENGINE_DEBUG") else
    Sys.setenv(FIELDHUB_ENGINE_DEBUG = old), add = TRUE)
  Sys.setenv(FIELDHUB_ENGINE_DEBUG = "1")
  for (case in list(c(13, 4, 13), c(100, 2, 4), c(30, 11, 6))) {
    set.seed(8)
    d <- blocks_engine(case[1], case[2], case[3])
    expect_lte(d$search$debug_max_error, 1e-8)
  }
  Sys.unsetenv("FIELDHUB_ENGINE_DEBUG")
  set.seed(8)
  expect_true(is.na(blocks_engine(13, 4, 13)$search$debug_max_error))
})

test_that("output has the blocksdesign::blocks() shape", {
  set.seed(6)
  d <- blocks_engine(10, 2, 4)
  expect_named(d$Design, c("Level_1", "plots", "treatments"))
  expect_identical(levels(d$Design$Level_1), paste0("B", 1:4))
  expect_identical(levels(d$Design$treatments), as.character(1:10))
  expect_named(d$Blocks_model, c("Level", "Blocks", "D-Efficiency", "A-Efficiency", "A-Bound"))
  skip_if_not_installed("blocksdesign")
  ref <- blocksdesign::blocks(treatments = 10, replicates = 2, blocks = 4, searches = 1, seed = 1)
  expect_identical(vapply(d$Design, class, ""), vapply(ref$Design, class, ""))
  expect_identical(levels(d$Design$Level_1), levels(ref$Design$Level_1))
  expect_identical(vapply(d$Blocks_model, class, ""), vapply(ref$Blocks_model, class, ""))
})

test_that("the smallest accepted sizes (n = 2b) always give a connected design", {
  for (case in list(c(4, 2, 4), c(10, 2, 10), c(30, 2, 30))) {
    for (seed in 1:5) {
      set.seed(seed)
      d <- blocks_engine(case[1], case[2], case[3], searches = 2L)
      N <- engine_ref_counts(d$Design)
      expect_identical(engine_ref_efficiency(N, 2)[["rank"]], case[1] - 1)
    }
  }
})

test_that("block counts beyond the supported range are rejected", {
  expect_error(blocks_engine(2, 50000, 50000), class = "fieldhub_input_error",
               regexp = "at most 1000 blocks")
})

test_that("plot counts beyond the integer range are rejected", {
  expect_error(blocks_engine(50000, 50000, 4), class = "fieldhub_input_error",
               regexp = "too many plots")
})

test_that("invalid inputs are rejected before any search", {
  expect_error(blocks_engine(1, 2, 4), class = "fieldhub_input_error")
  expect_error(blocks_engine(10, 2, 1), class = "fieldhub_input_error")
  expect_error(blocks_engine(10, 1, 4), class = "fieldhub_input_error")
  expect_error(blocks_engine(10, 2, 4, time_limit = 0), class = "fieldhub_input_error")
  expect_error(blocks_engine(10, 2.5, 4), class = "fieldhub_input_error")
})

test_that("known optima are reached in at least 95 of 100 seeds", {
  skip_on_cran()
  optima <- readRDS(test_path("fixtures", "blocks-engine-optima.rds"))
  for (i in seq_len(nrow(optima))) {
    o <- optima[i, ]
    column <- if (o$criterion == "D") "D-Efficiency" else "A-Efficiency"
    hits <- vapply(1:100, function(seed) {
      set.seed(seed)
      blocks_engine(o$v, o$r, o$b)$Blocks_model[[column]] >= o$target - 1e-10
    }, logical(1))
    expect_gte(sum(hits), 95, label = o$name)
  }
})

test_that("efficiency definitions agree with blocksdesign on known optima", {
  skip_on_cran()
  skip_if_not_installed("blocksdesign")
  for (case in list(c(7, 3, 7), c(13, 4, 13))) {
    set.seed(1)
    ours <- blocks_engine(case[1], case[2], case[3])$Blocks_model
    theirs <- blocksdesign::blocks(treatments = case[1], replicates = case[2], blocks = case[3],
                                   searches = 20, seed = 1)$Blocks_model
    # blocksdesign reports efficiencies rounded to 7 decimals.
    expect_identical(round(ours[["D-Efficiency"]], 7), theirs[["D-Efficiency"]])
    expect_identical(round(ours[["A-Efficiency"]], 7), theirs[["A-Efficiency"]])
  }
})

test_that("weakly connected designs (n = 2b) stay connected at many blocks", {
  skip_on_cran()
  for (case in list(c(40, 2, 40), c(100, 2, 100), c(300, 2, 300))) {
    for (seed in 1:3) {
      set.seed(seed)
      d <- blocks_engine(case[1], case[2], case[3], searches = 2L)
      N <- engine_ref_counts(d$Design)
      expect_identical(engine_ref_efficiency(N, 2)[["rank"]], case[1] - 1)
      expect_equal(d$Blocks_model[["D-Efficiency"]], engine_ref_efficiency(N, 2)[["D"]],
                   tolerance = 1e-8)
    }
  }
})

test_that("debug mode stays accurate on weakly connected designs", {
  skip_on_cran()
  old <- Sys.getenv("FIELDHUB_ENGINE_DEBUG", unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv("FIELDHUB_ENGINE_DEBUG") else
    Sys.setenv(FIELDHUB_ENGINE_DEBUG = old), add = TRUE)
  Sys.setenv(FIELDHUB_ENGINE_DEBUG = "1")
  for (case in list(c(40, 2, 40), c(150, 2, 150))) {
    set.seed(2)
    d <- blocks_engine(case[1], case[2], case[3], searches = 1L)
    expect_lte(d$search$debug_max_error, 1e-8)
  }
})

test_that("starts and refreshes fit the default budget when there are many blocks", {
  skip_on_cran()
  for (case in list(c(200, 2, 150), c(2000, 2, 1000))) {
    set.seed(4)
    d <- blocks_engine(case[1], case[2], case[3], searches = 1L)
    expect_identical(d$search$searches_done, 1L)
    expect_gt(d$search$refreshes, 0)
    expect_lte(d$search$evaluations, d$search$budget)
    N <- engine_ref_counts(d$Design)
    expect_identical(engine_ref_efficiency(N, 2)[["rank"]], case[1] - 1)
  }
})

test_that("refreshes at many blocks respect the time limit", {
  skip_on_cran()
  set.seed(5)
  elapsed <- system.time(
    suppressWarnings(d <- blocks_engine(1000, 2, 1000, budget = 2e9, time_limit = 3))
  )[["elapsed"]]
  # Before the fix this took 32 s. What remains past the limit is the final
  # O(b^3) refresh and A-efficiency at b = 1000 (about 1.3 s each on a fast
  # core) plus the operation running when the limit is reached.
  expect_lt(elapsed, 20)
})
