library(FielDHub)

test_that("creating a design leaves the caller's random numbers unchanged", {
  # Regression test: every design function called set.seed() and never
  # restored the random-number stream, so the numbers drawn after creating a
  # design depended only on its seed (a loop calling RCBD(seed = 1) and then
  # rnorm() got the same draws on every iteration).
  seeded <- setdiff(names(catalogue), "swap_pairs")
  for (name in seeded) {
    set.seed(42)
    suppressWarnings(suppressMessages(utils::capture.output(catalogue[[name]]$build())))
    after <- stats::runif(3)
    set.seed(42)
    expect_identical(after, stats::runif(3), info = name)
  }
})

test_that("a design does not leave the session seeded when it was not", {
  global <- globalenv()
  had_seed <- exists(".Random.seed", envir = global, inherits = FALSE)
  if (had_seed) saved <- get(".Random.seed", envir = global, inherits = FALSE)
  on.exit(if (had_seed) assign(".Random.seed", saved, envir = global))
  if (had_seed) rm(".Random.seed", envir = global)
  RCBD(t = 5, reps = 2, seed = 1)
  expect_false(exists(".Random.seed", envir = global, inherits = FALSE))
})

test_that("without a seed, the design records an integer seed that reproduces it", {
  rcbd <- RCBD(t = 5, reps = 3)
  seed <- rcbd$infoDesign$seed
  expect_equal(seed %% 1, 0)
  expect_identical(RCBD(t = 5, reps = 3, seed = seed)$fieldBook, rcbd$fieldBook)
  diag <- diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4, year = 2026)
  again <- diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4,
                                seed = diag$infoDesign$seed, year = 2026)
  expect_identical(again$fieldBook, diag$fieldBook)
})

test_that("a seed that is not a single number is rejected", {
  # CRD() used to ignore a character seed and pick a random one
  expect_error(CRD(t = 4, reps = 2, seed = "123"), "'seed' must be a single number",
               class = "fieldhub_input_error")
  expect_error(RCBD(t = 4, reps = 2, seed = c(1, 2)), class = "fieldhub_input_error")
})

test_that("optimized_arrangement() is reproducible when checks do not divide evenly", {
  # Regression test: the replication of the checks was sampled before the
  # seed was set, from the caller's random-number stream
  build <- function() {
    optimized_arrangement(nrows = 12, ncols = 10, lines = 99, amountChecks = 21,
                          checks = 1:5, seed = 7, year = 2026)
  }
  set.seed(1)
  first <- build()
  set.seed(2)
  expect_identical(build(), first)
})
