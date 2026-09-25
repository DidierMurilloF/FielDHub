library(FielDHub)

# A field book with one treatment replicated ten times and 19 unreplicated
sim_input <- function() {
  data.frame(
    LOCATION = 1,
    PLOT = 1:29,
    TREATMENT = c(rep("A", 10), paste0("E", 1:19))
  )
}

# A 4 x 5 field with one entry per plot, as used by the spatial simulations
field_input <- function() {
  data.frame(
    ID = 1:20,
    ROW = rep(1:4, each = 5),
    COLUMN = rep(1:5, times = 4),
    ENTRY = 1:20
  )
}

simulate_field <- function(seed = NULL) {
  AR1xAR1_simulation(nrows = 4, ncols = 5, ROX = 0.4, ROY = 0.4,
                     minValue = 1, maxValue = 10, fieldbook = field_input(),
                     trail = "YIELD", seed = seed)$outOrder
}

test_that("norm_trunc() draws each treatment's responses from one distribution", {
  # Regression test: responses were drawn in blocks sized by the counts of
  # the sorted treatment levels but labelled with shuffled treatments, so
  # under unequal replication a treatment's responses came from several
  # distributions (a range of about 50 instead of a few units).
  sim <- norm_trunc(a = 0, b = 100, data = sim_input(), seed = 2)
  expect_equal(as.vector(table(sim$TREATMENT)["A"]), 10)
  expect_lt(diff(range(sim$RESP[sim$TREATMENT == "A"])), 15)
})

test_that("norm_trunc() keeps the caller's random-number stream when seed is NULL", {
  # Regression test: norm_trunc(seed = NULL) called set.seed(NULL), which
  # discarded the seed the app had just set, so simulated data could not be
  # reproduced.
  set.seed(5)
  first <- norm_trunc(a = 0, b = 100, data = sim_input())
  set.seed(5)
  second <- norm_trunc(a = 0, b = 100, data = sim_input())
  expect_identical(first, second)
})

test_that("AR1xAR1_simulation() with seed = NULL follows the caller's seed", {
  # Regression test: with seed = NULL it called set.seed(runif(1)), and
  # set.seed() truncates any value in [0, 1) to 0, so every simulation used
  # seed 0 whatever seed the app had set.
  set.seed(1)
  first <- simulate_field()
  set.seed(2)
  second <- simulate_field()
  expect_false(isTRUE(all.equal(first$YIELD, second$YIELD)))
  set.seed(1)
  expect_identical(simulate_field(), first)
})
