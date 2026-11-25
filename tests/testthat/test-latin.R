# latin_rectangle() and latin_square(): Latin designs, their outputs, input
# errors and random-stream contract.

# ---- Latin rectangles: combinatorial and model properties -------------------

test_that("Latin rectangles satisfy independent row, column and model-rank invariants", {
  # For t treatments in `rows` rows: every row holds all t treatments, no
  # column repeats one, every cell is used once, and the additive model
  # row + column + treatment has rank rows + 2t - 2 (residual df
  # (rows - 2)(t - 1)).
  grid <- do.call(rbind, lapply(3:8, function(t) expand.grid(
    t = t, rows = 2:t, seed = c(1, 27, .Machine$integer.max))))
  for (i in seq_len(nrow(grid))) {
    t <- grid$t[i]; rows <- grid$rows[i]
    nm <- sprintf("t=%d rows=%d seed=%d", t, rows, grid$seed[i])
    book <- latin_rectangle(t, rows, seed = grid$seed[i])$fieldBook
    model <- stats::model.matrix(~ factor(ROW) + factor(COLUMN) + factor(TREATMENT), book)
    got <- list(
      plots = nrow(book),
      replication = sort(unique(as.vector(table(book$TREATMENT)))),
      rows_complete = all(vapply(split(book$TREATMENT, book$ROW),
                                 function(x) length(unique(x)) == t, logical(1))),
      columns_distinct = all(vapply(split(book$TREATMENT, book$COLUMN),
                                    function(x) !anyDuplicated(x), logical(1))),
      cells = nrow(unique(book[c("ROW", "COLUMN")])),
      rank = qr(model)$rank)
    want <- list(plots = t * rows, replication = rows, rows_complete = TRUE,
                 columns_distinct = TRUE, cells = t * rows, rank = rows + 2 * t - 2)
    expect_equal(got, want, info = nm)
  }
})

# ---- Latin rectangles: labels, locations, numbering, round-trips ------------

test_that("Latin rectangles preserve labels, locations, numbering and planting paths", {
  labels <- c("No nitrogen", "A*B", "é 3", "4", "five")
  for (planter in c("serpentine", "cartesian")) {
    x <- latin_rectangle(labels, 3, l = 2, plotNumber = c(11, 201),
      locationNames = c("Fargo", " CASSELTON"), planter = planter, seed = 7)
    book <- x$fieldBook
    expected_columns <- if (planter == "serpentine") c(1:5, 5:1, 1:5) else rep(1:5, 3)
    expect_identical(
      list(locations = unique(book$LOCATION), labels = sort(unique(book$TREATMENT)),
           plots = book$PLOT, ids = book$ID, columns = as.numeric(book$COLUMN)),
      list(locations = c("Fargo", " CASSELTON"), labels = sort(labels),
           plots = c(11:25, 201:215) * 1, ids = seq_len(30),
           columns = as.numeric(rep(expected_columns, 2))),
      info = planter)
    expect_identical(field_layout(x), book)
    expect_identical(reproduce_design(x), x)
    expect_identical(eval(parse(text = design_call_code(x))), x)
    expect_output(print(x), "Latin Rectangle")
    expect_output(print(summary(x)), "Latin Rectangle")
    expect_s3_class(plot_layout(x)$out_layout, "ggplot")
  }
})

# ---- Latin rectangles: input errors and random stream -----------------------

test_that("Latin rectangle input errors precede randomization and allocations", {
  cases <- list(list(t = 1), list(t = NA_real_), list(t = c(3, 4)), list(t = TRUE),
    list(t = c("a", "a")), list(t = c("a", "")), list(t = matrix(3)),
    list(rows = 1), list(rows = 3.5), list(rows = 7), list(rows = Inf),
    list(l = 0), list(l = matrix(1)), list(locationNames = c("A", "B")),
    list(l = 2, locationNames = c("A", "A")), list(locationNames = NA_character_),
    list(plotNumber = 0), list(plotNumber = .Machine$integer.max),
    list(l = 2, plotNumber = c(1, 101, 201)), list(plotNumber = 1.2),
    list(planter = "diagonal"), list(t = .Machine$integer.max, rows = 3))
  stream_moved <- character()
  for (case in cases) {
    nm <- paste(deparse(case), collapse = "")
    arguments <- utils::modifyList(list(t = 5, rows = 3), case)
    set.seed(900)
    before <- .Random.seed
    expect_error(do.call(latin_rectangle, arguments), class = "fieldhub_input_error",
                 info = nm)
    if (!identical(.Random.seed, before)) stream_moved <- c(stream_moved, nm)
  }
  expect(length(stream_moved) == 0,
         sprintf("the random stream moved on error for %s", stream_moved[1]))
})

test_that("Latin rectangle seeding consumes only the automatic recorded seed", {
  set.seed(810)
  before <- .Random.seed
  x <- latin_rectangle(5, 3, seed = 100)
  expect_identical(.Random.seed, before)
  expected_seed <- sample.int(.Machine$integer.max, 1L)
  after <- .Random.seed
  set.seed(810)
  automatic <- latin_rectangle(5, 3)
  expect_identical(automatic$metadata$seed, expected_seed)
  expect_identical(.Random.seed, after)
  expect_identical(reproduce_design(automatic), automatic)
  expect_identical(x, latin_rectangle(5, 3, seed = 100))
})

# ---- Latin squares: failed searches -----------------------------------------
# The square search only fails on a tiny iteration budget, which the public
# API does not expose; the mock lowers it so the failure contract is reachable.

test_that("a failed Latin-square search still restores the caller's RNG", {
  bounded_lsq <- lsq
  testthat::local_mocked_bindings(
    lsq = function(len, reps, ...) bounded_lsq(len, reps, max_iterations = 1L, ...)
  )
  set.seed(42)
  previous <- .Random.seed
  expect_error(latin_square(t = 4, seed = 1), class = "fieldhub_search_error")
  expect_identical(.Random.seed, previous)
})

test_that("a failed search identifies the square requested by the public API", {
  bounded_lsq <- lsq
  calls <- 0L
  testthat::local_mocked_bindings(
    lsq = function(len, reps, ...) {
      calls <<- calls + 1L
      limit <- if (calls == 2L) 1L else 100000L
      bounded_lsq(len, reps, max_iterations = limit, ...)
    }
  )
  err <- expect_error(latin_square(t = 4, reps = 2, seed = 1), class = "fieldhub_search_error")
  expect_identical(err$square, 2L)
  expect_match(conditionMessage(err), "square 2")
})
