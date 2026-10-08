library(FielDHub)

test_that("a seedless design consumes exactly one draw from the caller's stream", {
  for (name in names(catalogue)) {
    entry <- catalogue[[name]]
    fn_body <- as.list(body(entry$build))
    last <- length(fn_body)
    call <- fn_body[[last]]
    if (!"seed" %in% names(call)) next
    call$seed <- NULL
    # Statements before the final call build any input the design needs (such
    # as a random matrix for swap_pairs()); run them first, in their own
    # environment, so they do not interfere with the one draw this test
    # measures from the seedless final call. A few entries additionally wrap
    # that setup in local_rng_state(), only so the catalogue's explicit-seed
    # demonstration leaves the caller's stream untouched; that protection is
    # dropped here since it is unrelated to the final call's own seed.
    setup <- fn_body[-c(1L, last)]
    setup <- setup[!vapply(setup, function(expr) {
      grepl("local_rng_state", paste(deparse(expr), collapse = " "), fixed = TRUE)
    }, logical(1))]
    env <- new.env(parent = globalenv())
    for (expr in setup) eval(expr, envir = env)

    set.seed(99)
    expected <- sample.int(.Machine$integer.max, 1L)
    after_one_draw <- .Random.seed
    set.seed(99)
    design <- suppressWarnings(suppressMessages(eval(call, envir = env)))
    expect_identical(.Random.seed, after_one_draw, info = name)
    expect_identical(as.integer(design$metadata$seed), expected, info = name)
  }
})

test_that("repeated seedless calls differ and set.seed() reproduces them", {
  set.seed(1)
  a <- CRD(t = 5, reps = 3)
  b <- CRD(t = 5, reps = 3)
  expect_false(identical(a$metadata$seed, b$metadata$seed))
  set.seed(1)
  expect_identical(CRD(t = 5, reps = 3)$fieldBook, a$fieldBook)
})

test_that("explicit seeds leave the caller's stream untouched", {
  set.seed(5)
  before <- .Random.seed
  RCBD(t = 5, reps = 3, seed = 10)
  expect_identical(.Random.seed, before)
})

test_that("a seedless design draw creates .Random.seed when the session had none", {
  global <- globalenv()
  had_seed <- exists(".Random.seed", envir = global, inherits = FALSE)
  if (had_seed) saved <- get(".Random.seed", envir = global, inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", saved, envir = global)
    } else if (exists(".Random.seed", envir = global, inherits = FALSE)) {
      rm(".Random.seed", envir = global)
    }
  })
  if (had_seed) rm(".Random.seed", envir = global)
  RCBD(t = 5, reps = 3)
  expect_true(exists(".Random.seed", envir = global, inherits = FALSE))
})
