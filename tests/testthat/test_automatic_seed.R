library(FielDHub)

test_that("automatic seed resolution preserves the caller's random stream", {
  set.seed(42)
  expected_seed <- sample.int(.Machine$integer.max, 1)
  set.seed(42)
  before <- .Random.seed
  actual <- FielDHub:::resolve_seed(NULL)
  expect_identical(actual, expected_seed)
  expect_identical(.Random.seed, before)
})

test_that("designs without explicit seeds preserve the caller's random stream", {
  for (build in list(
    function() RCBD(t = 5, reps = 3),
    function() CRD(t = 5, reps = 3),
    function() diagonal_arrangement(nrows = 15, ncols = 20, lines = 270,
                                    checks = 4, year = 2026)
  )) {
    set.seed(73)
    before <- .Random.seed
    design <- build()
    expect_identical(.Random.seed, before)
    expect_identical(design$metadata$seed, design$infoDesign$seed)
  }
})

test_that("automatic seeds do not leave an initially unseeded session seeded", {
  (function() {
    global <- globalenv()
    had_seed <- exists(".Random.seed", global, inherits = FALSE)
    saved <- if (had_seed) get(".Random.seed", global, inherits = FALSE)
    on.exit({
      if (had_seed) {
        assign(".Random.seed", saved, global)
      } else if (exists(".Random.seed", global, inherits = FALSE)) {
        rm(".Random.seed", envir = global)
      }
    })
    if (had_seed) rm(".Random.seed", envir = global)
    RCBD(t = 5, reps = 3)
    expect_false(exists(".Random.seed", global, inherits = FALSE))
  })()
})
