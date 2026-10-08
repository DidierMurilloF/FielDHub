test_that("Latin rectangles satisfy independent row, column and model-rank invariants", {
  for (t in 3:8) for (rows in 2:t) for (seed in c(1, 27, .Machine$integer.max)) {
    x <- latin_rectangle(t, rows, seed = seed)
    book <- x$fieldBook
    expect_equal(nrow(book), t * rows)
    expect_equal(unname(table(book$TREATMENT)), rep(rows, t), ignore_attr = TRUE)
    expect_true(all(vapply(split(book$TREATMENT, book$ROW), function(x) length(unique(x)) == t, logical(1))))
    expect_true(all(vapply(split(book$TREATMENT, book$COLUMN), function(x) !anyDuplicated(x), logical(1))))
    expect_equal(nrow(unique(book[c("ROW", "COLUMN")])), t * rows)
    model <- stats::model.matrix(~ factor(ROW) + factor(COLUMN) + factor(TREATMENT), book)
    expect_equal(qr(model)$rank, rows + 2 * t - 2)
    expect_equal(nrow(model) - qr(model)$rank, (rows - 2) * (t - 1))
  }
})

test_that("Latin rectangles preserve labels, locations, numbering and planting paths", {
  labels <- c("No nitrogen", "A*B", "é 3", "4", "five")
  for (planter in c("serpentine", "cartesian")) {
    x <- latin_rectangle(labels, 3, l = 2, plotNumber = c(11, 201),
      locationNames = c("Fargo", " CASSELTON"), planter = planter, seed = 7)
    book <- x$fieldBook
    expect_identical(unique(book$LOCATION), c("Fargo", " CASSELTON"))
    expect_setequal(book$TREATMENT, labels)
    expect_identical(book$PLOT, c(11:25, 201:215) * 1)
    expect_identical(book$ID, seq_len(30))
    expected_columns <- if (planter == "serpentine") c(1:5, 5:1, 1:5) else rep(1:5, 3)
    expect_equal(book$COLUMN, rep(expected_columns, 2))
    expect_identical(field_layout(x), book)
    expect_identical(reproduce_design(x), x)
    expect_identical(eval(parse(text = design_call_code(x))), x)
    expect_output(print(x), "Latin Rectangle")
    expect_output(print(summary(x)), "Latin Rectangle")
    expect_s3_class(plot_layout(x)$out_layout, "ggplot")
  }
})

test_that("Latin rectangle input errors precede randomization and allocations", {
  cases <- list(list(t = 1), list(t = NA_real_), list(t = c(3, 4)), list(t = TRUE),
    list(t = c("a", "a")), list(t = c("a", "")), list(t = matrix(3)),
    list(rows = 1), list(rows = 3.5), list(rows = 7), list(rows = Inf),
    list(l = 0), list(l = matrix(1)), list(locationNames = c("A", "B")),
    list(l = 2, locationNames = c("A", "A")), list(locationNames = NA_character_),
    list(plotNumber = 0), list(plotNumber = .Machine$integer.max),
    list(l = 2, plotNumber = c(1, 101, 201)), list(plotNumber = 1.2),
    list(planter = "diagonal"), list(t = .Machine$integer.max, rows = 3))
  for (case in cases) {
    arguments <- utils::modifyList(list(t = 5, rows = 3), case)
    set.seed(900)
    before <- .Random.seed
    expect_error(do.call(latin_rectangle, arguments), class = "fieldhub_input_error")
    expect_identical(.Random.seed, before)
  }
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

test_that("Latin rectangle integration is declarative rather than design-specific dispatch", {
  entry <- fieldhub_design_registry()$latin_rectangle
  expect_identical(entry$engine, "latin_rectangle")
  expect_identical(entry$layout, "field_book_layouts")
  for (generic in c("layout_options", "draw_layout", "print")) {
    expect_null(getS3method(generic, "fieldhub_latin_rectangle", optional = TRUE))
  }
  x <- latin_rectangle(5, 3, seed = 1)
  for (column in c("ROW", "COLUMN", "REP", "TREATMENT")) {
    bad <- x
    bad$fieldBook[[column]] <- NULL
    expect_error(validate_fieldhub_design(bad), column, class = "fieldhub_internal_error")
  }
})
