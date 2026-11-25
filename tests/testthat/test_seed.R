# Seeds of design functions: the caller's random-number stream, automatic
# seeds, and the accepted seed range.

# Run `expr` in a session without .Random.seed and restore the caller's
# state afterwards. Returns whether `expr` left a .Random.seed behind.
leaves_random_seed <- function(expr) {
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
  force(expr)
  exists(".Random.seed", envir = global, inherits = FALSE)
}

# The caller's stream ---------------------------------------------------------

test_that("creating a design leaves the caller's random numbers unchanged", {
  # Regression: every design function called set.seed() and never restored
  # the stream, so numbers drawn after a design depended only on its seed.
  changed <- Filter(function(name) {
    set.seed(42)
    suppressWarnings(suppressMessages(utils::capture.output(catalogue[[name]]$build())))
    after <- stats::runif(3)
    set.seed(42)
    !identical(after, stats::runif(3))
  }, names(catalogue))
  expect_identical(changed, character(0))
})

test_that("a design with a seed does not leave an unseeded session seeded", {
  expect_false(leaves_random_seed(RCBD(t = 5, reps = 2, seed = 1)))
})

test_that("a seedless design draw creates .Random.seed when the session had none", {
  expect_true(leaves_random_seed(RCBD(t = 5, reps = 3)))
})

test_that("optimized_arrangement() is reproducible when checks do not divide evenly", {
  # Regression: the check replication was sampled from the caller's stream
  # before the seed was set
  build <- function() {
    optimized_arrangement(nrows = 12, ncols = 10, lines = 99, rep_checks = 21,
                          checks = 1:5, seed = 7, year = 2026)
  }
  set.seed(1)
  first <- build()
  set.seed(2)
  expect_identical(build(), first)
})

# Automatic seeds -------------------------------------------------------------

test_that("a seedless design consumes exactly one draw from the caller's stream", {
  failed <- character(0)
  for (name in names(catalogue)) {
    fn_body <- as.list(body(catalogue[[name]]$build))
    last <- length(fn_body)
    call <- fn_body[[last]]
    if (!"seed" %in% names(call)) next
    call$seed <- NULL
    # Statements before the final call build its inputs (such as a random
    # matrix for swap_pairs()); run them first so they do not take part in
    # the one draw measured. The catalogue's local_rng_state() wrappers only
    # protect its own explicit-seed demonstration and are dropped here.
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
    if (!identical(.Random.seed, after_one_draw) ||
        !identical(as.integer(design$metadata$seed), expected)) {
      failed <- c(failed, name)
    }
  }
  expect_identical(failed, character(0))
})

test_that("without a seed, the design records an integer seed that reproduces it", {
  rcbd <- RCBD(t = 5, reps = 3)
  seed <- rcbd$infoDesign$seed
  expect_identical(list(whole = seed %% 1 == 0,
                        rebuilds = identical(RCBD(t = 5, reps = 3, seed = seed)$fieldBook,
                                             rcbd$fieldBook)),
                   list(whole = TRUE, rebuilds = TRUE))
  diag <- diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4, year = 2026)
  again <- diagonal_arrangement(nrows = 15, ncols = 20, lines = 270, checks = 4,
                                seed = diag$infoDesign$seed, year = 2026)
  expect_identical(again$fieldBook, diag$fieldBook)
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

# Accepted seeds ----------------------------------------------------------------

test_that("unsupported seed values give classed errors without coercion warnings", {
  cases <- list(`not a number` = function() CRD(t = 4, reps = 2, seed = "123"),
                `two values` = function() RCBD(t = 4, reps = 2, seed = c(1, 2)),
                `above integer range` = function() RCBD(t = 4, reps = 2, seed = .Machine$integer.max + 1),
                `below integer range` = function() RCBD(t = 4, reps = 2, seed = -.Machine$integer.max - 1),
                huge = function() RCBD(t = 4, reps = 2, seed = 1e308),
                complex = function() RCBD(t = 4, reps = 2, seed = 1 + 1i))
  warnings <- character(0)
  for (name in names(cases)) {
    withCallingHandlers(
      expect_error(cases[[name]](), class = "fieldhub_input_error", info = name),
      warning = function(w) {
        warnings <<- c(warnings, paste0(name, ": ", conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    )
  }
  expect_identical(warnings, character(0))
})

test_that("fractional seeds are truncated like set.seed() and recorded as supplied", {
  for (seed in c(0, 1.5, -1.5, .Machine$integer.max + 0.5, -.Machine$integer.max - 0.5)) {
    design <- RCBD(t = 4, reps = 2, seed = seed)
    expect_identical(list(design$infoDesign$seed, design$fieldBook),
                     list(seed, RCBD(t = 4, reps = 2, seed = trunc(seed))$fieldBook),
                     info = format(seed, digits = 12))
  }
})

test_that("row-column location seeds cannot overflow at the public seed boundary", {
  # An integer seed and the same seed as a double wrap alike
  cases <- list(onestage = .Machine$integer.max, twostage = .Machine$integer.max,
                twostage = as.double(.Machine$integer.max))
  for (i in seq_along(cases)) {
    method <- names(cases)[i]
    seed <- cases[[i]]
    set.seed(27)
    previous <- .Random.seed
    expect_no_warning(design <- row_column(t = 12, nrows = 3, reps = 2, l = 2,
                                           seed = seed, iterations = 1,
                                           plotNumber = c(101, 201), method = method))
    expect_identical(list(stream = .Random.seed, seed = design$metadata$seed,
                          replays = identical(reproduce_design(design), design)),
                     list(stream = previous, seed = seed, replays = TRUE),
                     info = paste(method, typeof(seed)))
  }
})
