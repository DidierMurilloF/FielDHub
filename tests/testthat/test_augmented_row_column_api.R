# Tests for the augmented row-column design API (research M3 specification,
# revision 2, sections 3, 5 and 6; implementation plan tasks 2-4): orientation,
# randomization, field books, replay and presentation. Placement and scores are
# checked from the returned field book alone with helper-arc-design.R.

# Task 2: orientation, randomization, field books and the oracle -------------
# (sections 3.2, 3.3, 3.5-3.7 and 6). The fixtures are small M1 reference
# contractions; the manifest values come from the independent M1 oracle.

arc_api_fixtures <- list(
  example1 = list(file = "example1-contraction.csv", v = 12L, k = 3L),
  table1 = list(file = "table1-v16-s8-k4.csv", v = 16L, k = 4L),
  cyclic = list(file = "cyclic-5x5-k4.csv", v = 5L, k = 4L)
)
arc_api_directions <- c("columns", "rows")
arc_api_metrics <- c("E_aug", "V_tt", "V_tc", "V_cc", "max_tt", "max_tc")
# M1 section 10.1: agreement between evaluators and with full-precision
# references, absolute
arc_api_tol <- 1e-10

arc_api_manifest <- function(file) {
  manifest <- utils::read.csv(
    test_path("fixtures", "augmented-row-column", "manifest.csv"),
    stringsAsFactors = FALSE
  )
  manifest[manifest$file == file, , drop = FALSE]
}

# Physical dimensions of a v x s design laid out along `direction`
arc_api_dims <- function(v, s, direction) {
  if (identical(direction, "columns")) {
    c(nrows = v, ncols = s)
  } else {
    c(nrows = s, ncols = v)
  }
}

# The unrandomized placement of a contraction: check h at cell
# (contraction[h, j], j), and the tests in column-major order of the test
# cells, as in the M1 expansion
arc_api_identity <- function(contraction, v) {
  k <- nrow(contraction)
  s <- ncol(contraction)
  check_index <- matrix(0L, v, s)
  for (j in seq_len(s)) {
    check_index[contraction[, j], j] <- seq_len(k)
  }
  list(check_index = check_index, test_order = seq_len((v - k) * s))
}

# A field book of fixture `case` laid out along `direction`. With a seed the
# placement comes from arc_randomize() under that seed, and the caller's RNG
# is restored on return; without one it is the unrandomized placement.
arc_api_book <- function(case, direction, seed = NULL, checks = NULL,
                         tests = NULL, entry_ids = NULL,
                         planter = "serpentine", plot_start = 101) {
  contraction <- arc_fixture(case$file)
  v <- case$v
  k <- case$k
  s <- ncol(contraction)
  n_tests <- (v - k) * s
  dims <- arc_api_dims(v, s, direction)
  orientation <- arc_orient(dims[["nrows"]], dims[["ncols"]], direction)
  if (is.null(seed)) {
    placement <- arc_api_identity(contraction, v)
  } else {
    arc_local_seed(seed)
    placement <- arc_randomize(contraction, v, s, k, n_tests)
  }
  if (is.null(checks)) checks <- paste0("CH", seq_len(k))
  if (is.null(tests)) tests <- paste0("G", k + seq_len(n_tests))
  fb <- arc_field_book(placement, orientation, checks, tests,
                       entry_ids = entry_ids, location = "LOC1",
                       expt = "Expt1", year = "2026",
                       plot_start = plot_start, planter = planter)
  list(fb = fb, placement = placement, v = v, s = s, k = k,
       n_tests = n_tests, nrows = dims[["nrows"]], ncols = dims[["ncols"]],
       checks = checks, tests = tests)
}

# One field-book column as the physical nrows x ncols map
arc_api_map <- function(fb, column) {
  map <- matrix(NA, max(fb$ROW), max(fb$COLUMN))
  map[cbind(fb$ROW, fb$COLUMN)] <- fb[[column]]
  map
}

# The coordinate checks of section 6 on one field book, from
# arc_reconstruct(): one plot per cell; every check exactly once in every
# line along CHECK_DIRECTION, so s times in total (a binary check-by-line
# incidence); and M1 rules 6-7 on the cross-lines.
arc_api_placement_ok <- function(fb) {
  rec <- arc_reconstruct(fb)
  bounds <- c(floor(rec$k * rec$s / rec$v), ceiling(rec$k * rec$s / rec$v))
  all(rec$cells == 1L) && all(rec$by_line == 1L) &&
    all(rowSums(rec$by_line) == rec$s) && all(rec$by_cross <= 1L) &&
    all(rec$cross_checks %in% bounds) && all(rec$cross_checks >= 1L) &&
    sum(rec$cross_checks) == rec$k * rec$s
}

test_that("arc_orient maps check_direction to v, s and the coordinates", {
  fields <- c("check_direction", "nrows", "ncols", "v", "s")
  columns <- arc_orient(12, 8, "columns")
  expect_identical(columns[fields],
                   list(check_direction = "columns", nrows = 12L, ncols = 8L,
                        v = 12L, s = 8L))
  expect_identical(columns$map, c(ROW = "math_row", COLUMN = "math_column"))
  rows <- arc_orient(8, 12, "rows")
  expect_identical(rows[fields],
                   list(check_direction = "rows", nrows = 8L, ncols = 12L,
                        v = 12L, s = 8L))
  expect_identical(rows$map, c(ROW = "math_column", COLUMN = "math_row"))
  # A square field fits both directions
  expect_identical(arc_orient(5, 5, "rows")[c("v", "s")],
                   list(v = 5L, s = 5L))

  math <- matrix(seq_len(96L), 12L, 8L)
  expect_identical(arc_physical_matrix(math, columns), math)
  expect_identical(arc_physical_matrix(math, rows), t(math))
  expect_identical(arc_math_coordinates("columns", 1:3, 4:6),
                   list(row = 1:3, column = 4:6))
  expect_identical(arc_math_coordinates("rows", 1:3, 4:6),
                   list(row = 4:6, column = 1:3))
})

test_that("arc_orient requires check_direction and never infers it", {
  missing_direction <- tryCatch(arc_orient(12, 8), error = identity)
  expect_identical(class(missing_direction)[1:2],
                   c("fieldhub_arc_input_error", "fieldhub_input_error"))
  expect_match(conditionMessage(missing_direction), "no default", fixed = TRUE)
  bad <- list(NULL, NA_character_, "", "column", "Columns",
              c("columns", "rows"), 1)
  for (value in bad) {
    expect_error(arc_orient(12, 8, value), class = "fieldhub_arc_input_error")
  }
  for (dims in list(c(0, 8), c(12.5, 8), c(12, NA))) {
    expect_error(arc_orient(dims[1], dims[2], "columns"),
                 class = "fieldhub_arc_input_error")
  }
})

test_that("arc_orient rejects s > v and names the other check_direction", {
  wide <- tryCatch(arc_orient(8, 12, "columns"), error = identity)
  expect_identical(class(wide)[1:2], c("fieldhub_arc_dimension_error",
                                       "fieldhub_dimension_error"))
  expect_match(conditionMessage(wide), "check_direction = \"rows\"",
               fixed = TRUE)
  expect_identical(wide$suggested, "rows")
  long <- tryCatch(arc_orient(12, 8, "rows"), error = identity)
  expect_identical(class(long)[1:2], c("fieldhub_arc_dimension_error",
                                       "fieldhub_dimension_error"))
  expect_match(conditionMessage(long), "check_direction = \"columns\"",
               fixed = TRUE)
  expect_identical(long$suggested, "columns")
  # The named direction fits the same field
  expect_identical(arc_orient(8, 12, "rows")$s, 8L)
  expect_identical(arc_orient(12, 8, "columns")$s, 8L)
})

test_that("field books have the spatial columns, types and planting order", {
  columns <- c("ID", "EXPT", "LOCATION", "YEAR", "PLOT", "ROW", "COLUMN",
               "CHECKS", "ENTRY", "TREATMENT", "CHECK_DIRECTION")
  for (direction in arc_api_directions) {
    for (planter in c("serpentine", "cartesian")) {
      book <- arc_api_book(arc_api_fixtures$example1, direction, seed = 3,
                           planter = planter, plot_start = 1001)
      fb <- book$fb
      n <- book$nrows * book$ncols
      expect_identical(names(fb), columns)
      expect_identical(nrow(fb), n)
      expect_identical(fb$ID, seq_len(n))
      for (column in c("ROW", "COLUMN", "CHECKS", "ENTRY")) {
        expect_type(fb[[column]], "integer")
      }
      expect_type(fb$TREATMENT, "character")
      expect_identical(fb$CHECK_DIRECTION, rep(direction, n))
      expect_identical(fb$EXPT, rep("Expt1", n))
      expect_identical(fb$LOCATION, rep("LOC1", n))
      expect_identical(fb$YEAR, rep("2026", n))
      expect_identical(fb$PLOT, 1001 + (seq_len(n) - 1))
      # Planting order: row by row from row 1, column 1; a serpentine planter
      # goes back along every second row
      row <- rep(seq_len(book$nrows), each = book$ncols)
      column <- rep(seq_len(book$ncols), times = book$nrows)
      if (identical(planter, "serpentine")) {
        back <- row %% 2L == 0L
        column[back] <- book$ncols + 1L - column[back]
      }
      expect_identical(fb$ROW, row)
      expect_identical(fb$COLUMN, column)
    }
  }
})

test_that("each check appears once per line along check_direction", {
  for (case in arc_api_fixtures) {
    for (direction in arc_api_directions) {
      book <- arc_api_book(case, direction, seed = 20261011)
      fb <- book$fb
      rec <- arc_reconstruct(fb)
      expect_identical(c(rec$v, rec$s, rec$k), c(book$v, book$s, book$k))
      # Binary cells: one plot in every cell of the physical grid
      expect_identical(dim(rec$cells), c(book$nrows, book$ncols))
      expect_true(all(rec$cells == 1L))
      # Every check once in every line (a binary check-by-line incidence),
      # so s times in total
      expect_true(all(rec$by_line == 1L))
      expect_identical(as.vector(table(fb$CHECKS[fb$CHECKS != 0L])),
                       rep(book$s, book$k))
      # M1 rules 6-7 on the physical cross-lines
      expect_true(all(rec$by_cross %in% c(0L, 1L)))
      bounds <- c(floor(book$k * book$s / book$v),
                  ceiling(book$k * book$s / book$v))
      expect_true(all(rec$cross_checks %in% bounds))
      expect_true(all(rec$cross_checks >= 1L))
      expect_identical(sum(rec$cross_checks), book$k * book$s)
      expect_true(arc_api_placement_ok(fb))
      # The reconstruction equals the engine's recovery, which passes the
      # M1 restricted-mode checks
      recovered <- arc_contraction_from_field_book(fb, book$k, direction)
      expect_identical(rec$contraction, recovered)
      expect_no_error(arc_check_contraction(recovered, book$v, "restricted"))
    }
  }
})

test_that("the field-book oracle agrees with arc_final_score()", {
  for (name in names(arc_api_fixtures)) {
    case <- arc_api_fixtures[[name]]
    reference <- arc_api_manifest(case$file)
    expect_identical(nrow(reference), 1L)
    for (direction in arc_api_directions) {
      label <- paste(name, direction)
      book <- arc_api_book(case, direction, seed = 7)
      oracle <- arc_field_book_oracle(book$fb)
      recovered <- arc_contraction_from_field_book(book$fb, book$k, direction)
      final <- arc_final_score(recovered, book$v, "restricted")
      expect_true(oracle$connected, label = label)
      expect_true(final$connected, label = label)
      expect_identical(oracle$residual_df, final$df_formula, label = label)
      expect_identical(oracle$residual_df_svd, oracle$residual_df,
                       label = label)
      expect_identical(oracle$residual_df,
                       as.integer(reference$ref_df_formula), label = label)
      for (metric in arc_api_metrics) {
        expect_lte(abs(oracle[[metric]] - final[[metric]]), arc_api_tol,
                   label = paste(label, metric, "vs arc_final_score"))
        expect_lte(abs(oracle[[metric]] - reference[[paste0("ref_", metric)]]),
                   arc_api_tol, label = paste(label, metric, "vs manifest"))
      }
    }
  }
})

test_that("every test appears once and labels round-trip unchanged", {
  case <- arc_api_fixtures$example1
  n_tests <- (case$v - case$k) * 8L
  checks <- c("Check A", "std-2 (local); \"B\"", "Cé #3")
  tests <- sprintf("Line %02d / x.%s, y", seq_len(n_tests),
                   rep_len(c("a", " b", "c'"), n_tests))
  for (direction in arc_api_directions) {
    fb <- arc_api_book(case, direction, seed = 13, checks = checks,
                       tests = tests)$fb
    on_test <- fb$CHECKS == 0L
    expect_identical(sum(on_test), n_tests)
    expect_identical(sort(fb$TREATMENT[on_test], method = "radix"),
                     sort(tests, method = "radix"))
    expect_identical(
      as.vector(table(factor(fb$TREATMENT[!on_test], levels = checks))),
      rep(8L, 3L)
    )
    # Each label stays bound to its input position: ENTRY 1..k for the
    # checks, k + 1.. for the tests
    expect_identical(fb$TREATMENT, c(checks, tests)[fb$ENTRY])
  }
})

test_that("ENTRY and CHECKS follow the input order or supplied identifiers", {
  case <- arc_api_fixtures$example1
  for (direction in arc_api_directions) {
    book <- arc_api_book(case, direction, seed = 17)
    fb <- book$fb
    on_check <- fb$CHECKS != 0L
    # Without uploaded identifiers: checks 1..k and tests k + 1..k + n, in
    # input order
    expect_identical(fb$ENTRY[on_check],
                     match(fb$TREATMENT[on_check], book$checks))
    expect_identical(fb$ENTRY[!on_check],
                     book$k + match(fb$TREATMENT[!on_check], book$tests))
    expect_identical(fb$CHECKS[on_check], fb$ENTRY[on_check])
    expect_true(all(fb$CHECKS[!on_check] == 0L))
    expect_true(all(fb$TREATMENT[on_check] %in% book$checks))

    # Uploaded identifiers, the checks' not in increasing order
    ids <- c(905L, 17L, 300L, 2000L - seq_len(book$n_tests))
    uploaded <- arc_api_book(case, direction, seed = 17, entry_ids = ids)$fb
    labels <- c(book$checks, book$tests)
    expect_identical(uploaded$ENTRY, ids[match(uploaded$TREATMENT, labels)])
    expect_identical(uploaded$CHECKS,
                     ifelse(uploaded$TREATMENT %in% book$checks,
                            uploaded$ENTRY, 0L))
    # The same seed gives the same placement; only identifiers differ
    expect_identical(uploaded[c("ROW", "COLUMN", "TREATMENT")],
                     fb[c("ROW", "COLUMN", "TREATMENT")])
    # Contraction rows follow the checks' input order given their
    # identifiers; without them, increasing identifiers (17, 300, 905)
    in_order <- arc_contraction_from_field_book(uploaded, 3, direction,
                                                check_ids = ids[1:3])
    expect_identical(in_order, arc_contraction_from_field_book(fb, 3,
                                                               direction))
    expect_identical(arc_contraction_from_field_book(uploaded, 3, direction),
                     in_order[c(2, 3, 1), ])
  }
})

test_that("the unrandomized placement reproduces the M1 expansion", {
  for (case in arc_api_fixtures) {
    contraction <- arc_fixture(case$file)
    codes <- arc_expand(contraction, case$v, "restricted")$codes
    n_tests <- (case$v - case$k) * ncol(codes)
    # M1 codes the tests 1..T and check i as T + i; the default identifiers
    # are 1..k for the checks and k + 1, ... for the tests
    expected <- ifelse(codes > n_tests, codes - n_tests, codes + case$k)
    for (direction in arc_api_directions) {
      fb <- arc_api_book(case, direction)$fb
      physical <- if (identical(direction, "columns")) expected else t(expected)
      expect_identical(arc_api_map(fb, "ENTRY"), physical)
      expect_identical(arc_contraction_from_field_book(fb, case$k, direction),
                       contraction)
    }
  }
})

test_that("a transposed or mis-mapped field book fails the coordinate checks", {
  # A non-square design: v != s, so a transposed layout cannot pass
  case <- arc_api_fixtures$example1
  for (direction in arc_api_directions) {
    fb <- arc_api_book(case, direction, seed = 19)$fb
    other <- setdiff(arc_api_directions, direction)
    expect_true(arc_api_placement_ok(fb))

    transposed <- fb
    transposed$ROW <- fb$COLUMN
    transposed$COLUMN <- fb$ROW
    expect_false(arc_api_placement_ok(transposed))
    expect_false(all(arc_reconstruct(transposed)$by_line == 1L))
    expect_error(arc_contraction_from_field_book(transposed, case$k,
                                                 direction),
                 class = "fieldhub_arc_field_book_error")

    misread <- fb
    misread$CHECK_DIRECTION <- other
    expect_false(arc_api_placement_ok(misread))
    expect_error(arc_contraction_from_field_book(misread, case$k, other),
                 class = "fieldhub_arc_field_book_error")
    # The recorded direction must match the requested one
    expect_error(arc_contraction_from_field_book(fb, case$k, other),
                 class = "fieldhub_arc_field_book_error")
  }
  # Why the negative case needs v != s: a square restricted placement has
  # every check once in every row as well, so its transpose passes
  square <- arc_api_book(arc_api_fixtures$cyclic, "columns", seed = 19)$fb
  flipped <- square
  flipped$ROW <- square$COLUMN
  flipped$COLUMN <- square$ROW
  expect_true(arc_api_placement_ok(flipped))
})

test_that("contraction recovery rejects malformed field books", {
  fb <- arc_api_book(arc_api_fixtures$example1, "columns", seed = 23)$fb
  error <- "fieldhub_arc_field_book_error"
  expect_error(arc_contraction_from_field_book(fb[, -6], 3, "columns"),
               class = error)
  expect_error(arc_contraction_from_field_book(fb[-1, ], 3, "columns"),
               class = error)
  duplicated_cell <- fb
  duplicated_cell$COLUMN[1] <- duplicated_cell$COLUMN[2]
  expect_error(arc_contraction_from_field_book(duplicated_cell, 3, "columns"),
               class = error)
  expect_error(arc_contraction_from_field_book(fb, 4, "columns"),
               class = error)
  expect_error(arc_contraction_from_field_book(fb, 3, "columns",
                                               check_ids = c(1, 2, 4)),
               class = error)
  expect_error(arc_contraction_from_field_book(fb, 3, "diagonal"),
               class = "fieldhub_arc_input_error")
})

test_that("randomization is reproducible and leaves the caller's RNG alone", {
  arc_local_seed(101) # restores the session's RNG when this test ends
  contraction <- arc_fixture("example1-contraction.csv")
  draw <- function(seed) {
    arc_local_seed(seed)
    arc_randomize(contraction, 12, 8, 3, 72)
  }
  before <- get(".Random.seed", envir = globalenv())
  first <- draw(2026)
  again <- draw(2026)
  other <- draw(2027)
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  expect_identical(again, first)
  expect_false(identical(other$check_index, first$check_index))
  for (direction in arc_api_directions) {
    one <- arc_api_book(arc_api_fixtures$example1, direction, seed = 2026)
    two <- arc_api_book(arc_api_fixtures$example1, direction, seed = 2026)
    expect_identical(two$fb, one$fb)
  }
  expect_identical(get(".Random.seed", envir = globalenv()), before)
})

test_that("randomization takes its four draws in the documented order", {
  contraction <- arc_fixture("example1-contraction.csv")
  v <- 12L
  s <- 8L
  k <- 3L
  n_tests <- 72L
  arc_local_seed(31)
  expected <- list(row_order = sample.int(v), column_order = sample.int(s),
                   label_order = sample.int(k),
                   test_order = sample.int(n_tests))
  draw <- function() {
    arc_local_seed(31)
    arc_randomize(contraction, v, s, k, n_tests)
  }
  placement <- draw()
  expect_identical(placement[names(expected)], expected)

  # Whole lines and labels move: the randomized contraction is the input
  # with its rows (pseudo-treatments), columns and check labels permuted
  randomized <- apply(placement$check_index, 2L,
                      function(column) match(seq_len(k), column))
  permuted <- matrix(0L, k, s)
  for (h in seq_len(k)) {
    for (j in seq_len(s)) {
      permuted[placement$label_order[h], j] <-
        match(contraction[h, placement$column_order[j]], placement$row_order)
    }
  }
  expect_identical(randomized, permuted)

  for (direction in arc_api_directions) {
    fb <- arc_api_book(arc_api_fixtures$example1, direction, seed = 31)$fb
    # Mathematical columns are physical columns for "columns" and physical
    # rows for "rows"
    expect_identical(arc_contraction_from_field_book(fb, k, direction),
                     permuted)
    # Test cells, in column-major mathematical order, hold the tests in
    # test_order
    entry <- arc_api_map(fb, "ENTRY")
    math <- if (identical(direction, "columns")) entry else t(entry)
    expect_identical(math[placement$check_index == 0L],
                     k + placement$test_order)
  }
})

test_that("randomization changes nothing the field-book oracle measures", {
  for (name in c("example1", "table1")) {
    case <- arc_api_fixtures[[name]]
    for (direction in arc_api_directions) {
      base <- arc_field_book_oracle(arc_api_book(case, direction)$fb)
      expect_true(base$connected)
      for (seed in c(41, 42)) {
        label <- paste(name, direction, seed)
        randomized <- arc_field_book_oracle(
          arc_api_book(case, direction, seed = seed)$fb
        )
        expect_identical(randomized$connected, base$connected, label = label)
        expect_identical(randomized$residual_df, base$residual_df,
                         label = label)
        for (metric in arc_api_metrics) {
          expect_lte(abs(randomized[[metric]] - base[[metric]]), arc_api_tol,
                     label = paste(label, metric))
        }
      }
    }
  }
})

test_that("arc_randomize checks its input before drawing", {
  arc_local_seed(51)
  contraction <- arc_fixture("example1-contraction.csv")
  before <- get(".Random.seed", envir = globalenv())
  repeated <- contraction
  repeated[1, 2] <- repeated[1, 1]
  expect_error(arc_randomize(repeated, 12, 8, 3, 72),
               class = "fieldhub_arc_row_repeat_error")
  expect_error(arc_randomize(contraction, 12, 8, 3, 71),
               class = "fieldhub_arc_internal_error")
  expect_error(arc_randomize(contraction, 12, 7, 3, 72),
               class = "fieldhub_arc_internal_error")
  expect_identical(get(".Random.seed", envir = globalenv()), before)
})

test_that("arc_field_book rejects inconsistent internal inputs", {
  case <- arc_api_fixtures$example1
  contraction <- arc_fixture(case$file)
  placement <- arc_api_identity(contraction, 12L)
  orientation <- arc_orient(12, 8, "columns")
  checks <- paste0("CH", 1:3)
  tests <- paste0("G", 3L + seq_len(72L))
  build <- function(...) {
    arguments <- utils::modifyList(
      list(placement = placement, orientation = orientation, checks = checks,
           tests = tests, entry_ids = NULL, location = "LOC1",
           expt = "Expt1", year = "2026", plot_start = 101,
           planter = "serpentine"),
      list(...)
    )
    do.call(arc_field_book, arguments)
  }
  expect_s3_class(build(), "data.frame")
  internal <- "fieldhub_arc_internal_error"
  expect_no_error(build(orientation = arc_orient(8, 12, "rows")))
  expect_error(build(orientation = arc_orient(12, 9, "columns")),
               class = internal)
  expect_error(build(tests = c(tests[-1], "CH1")), class = internal)
  expect_error(build(tests = tests[-1]), class = internal)
  expect_error(build(entry_ids = c(1:74, 1L)), class = internal)
  expect_error(build(entry_ids = 1:74), class = internal)
  expect_error(build(planter = "zigzag"), class = "fieldhub_input_error")
})

test_that("arc_deterministic_view drops only timing and the package version", {
  x <- structure(
    list(
      infoDesign = list(check_direction = "columns", seed = 1L),
      fieldBook = data.frame(ID = 1:2, TREATMENT = c("A", "B")),
      timing = list(list(search = 0.1, verification = 0.2, total = 0.3)),
      metadata = list(design = "augmented_row_column", seed = 1L,
                      package_version = "1.6.0.9000",
                      parameters = list(nrows = 12, ncols = 8))
    ),
    class = c("fieldhub_augmented_row_column", "FielDHub")
  )
  view <- arc_deterministic_view(x)
  expect_identical(class(view), class(x))
  expect_identical(names(view), c("infoDesign", "fieldBook", "metadata"))
  expect_identical(names(view$metadata), c("design", "seed", "parameters"))
  expect_identical(view$infoDesign, x$infoDesign)
  expect_identical(view$fieldBook, x$fieldBook)
  expect_identical(view$metadata$parameters, x$metadata$parameters)
  # Timing and version differences leave the view unchanged; others do not
  slower <- x
  slower$timing[[1]]$total <- 9
  slower$metadata$package_version <- "9.9.9"
  expect_identical(arc_deterministic_view(slower), view)
  changed <- x
  changed$fieldBook$TREATMENT[2] <- "C"
  expect_false(identical(arc_deterministic_view(changed), view))
  # A result without those elements is returned unchanged
  expect_identical(arc_deterministic_view(view), view)
})

# Task 3: the public function and replay registration -----------------------
# (sections 3.1-3.7, 5 and 6). Small fields and budgets keep these fast; the
# tests check placement, scores and recording from the inputs and the
# returned field books, never from the result's own summary.

# A 10 x 8 field with 4 checks along the columns: v = 10, s = 8, 48 tests,
# residual df 12 (no low-df warning)
arc_api_call <- list(nrows = 10, ncols = 8, checks = 4,
                     check_direction = "columns", budget = 600, seed = 2026)

# augmented_row_column() with arc_api_call, overridden by `...`
arc_api_run <- function(...) {
  do.call(augmented_row_column, utils::modifyList(arc_api_call, list(...)))
}

# The same design laid out along `direction`
arc_api_oriented <- function(direction, ...) {
  dims <- arc_api_dims(10L, 8L, direction)
  arc_api_run(nrows = dims[["nrows"]], ncols = dims[["ncols"]],
              check_direction = direction, ...)
}

# The field book of one location
arc_api_location_book <- function(x, location) {
  x$fieldBook[x$fieldBook$LOCATION == location, , drop = FALSE]
}

arc_api_view <- function(x) arc_deterministic_view(x)

test_that("augmented_row_column rejects invalid inputs before drawing a seed", {
  arc_local_seed(303) # restores the session's RNG when this test ends
  before <- get(".Random.seed", envir = globalenv())
  input <- "fieldhub_input_error"
  arc_input <- "fieldhub_arc_input_error"
  dimension <- "fieldhub_arc_dimension_error"
  labels4 <- c("A", "B", "C", "D")
  tests48 <- paste0("T", 1:48)
  upload <- data.frame(ENTRY = 1:52, NAME = c(labels4, tests48))
  cases <- list(
    list(list(check_direction = NULL), arc_input),
    list(list(check_direction = "Columns"), arc_input),
    list(list(check_direction = NA_character_), arc_input),
    list(list(check_direction = c("columns", "rows")), arc_input),
    # s > v in the mapped orientation
    list(list(nrows = 8, ncols = 10), dimension),
    list(list(nrows = 0), input),
    list(list(nrows = 10.5), input),
    list(list(ncols = "8"), input),
    list(list(method = "anneal"), arc_input),
    list(list(method = c("staged", "staged")), arc_input),
    list(list(objective = "v_tt"), arc_input),
    list(list(objective = "A-opt"), arc_input),
    list(list(objective = c("V_tt", "E_aug")), arc_input),
    list(list(objective = NA_character_), arc_input),
    list(list(checks = 1), input),
    list(list(checks = 2.5), input),
    list(list(checks = c(1, 2, 3, 4)), arc_input),
    list(list(checks = "A"), arc_input),
    list(list(checks = c("A", "A", "B", "C")), arc_input),
    list(list(checks = c("A", NA, "B", "C")), input),
    list(list(checks = c("A", "", "B", "C")), input),
    list(list(checks = c("A", "  ", "B", "C")), input),
    list(list(checks = TRUE), arc_input),
    list(list(checks = factor(labels4)), arc_input),
    # k >= v: no test in a line
    list(list(checks = 10), dimension),
    # residual df below 1: -2 here, 0 for 6 x 6 with 2 checks
    list(list(checks = 2), "fieldhub_arc_infeasible_error"),
    list(list(nrows = 6, ncols = 6, checks = 2), "fieldhub_arc_df_error"),
    # entries: counts, labels and types
    list(list(entries = 47), arc_input),
    list(list(entries = 0), input),
    list(list(entries = tests48[-1]), arc_input),
    list(list(entries = c(tests48[-1], "T1 ")), NULL),
    list(list(entries = c(tests48[-48], "T1")), arc_input),
    list(list(entries = c(tests48[-48], NA)), input),
    list(list(entries = c(48, 1)), arc_input),
    list(list(entries = TRUE), arc_input),
    # check and test labels must be disjoint, also with generated labels
    list(list(checks = labels4, entries = c("A", tests48[-1])), arc_input),
    list(list(checks = c("G5", "X", "Y", "Z")), arc_input),
    # data: conflicts, columns, identifiers, labels and rows
    list(list(data = upload, entries = tests48), arc_input),
    list(list(data = upload, checks = labels4), arc_input),
    list(list(data = upload[, "NAME", drop = FALSE]), arc_input),
    list(list(data = cbind(upload, BLOCK = 1)), arc_input),
    list(list(data = "upload.csv"), arc_input),
    list(list(data = transform(upload, ENTRY = ENTRY + 0.5)), arc_input),
    list(list(data = transform(upload, ENTRY = c(1:51, 1L))), arc_input),
    list(list(data = transform(upload, ENTRY = 0:51)), arc_input),
    list(list(data = transform(upload, ENTRY = c(NA, 2:52))), arc_input),
    list(list(data = transform(upload, NAME = 1:52)), arc_input),
    list(list(data = transform(upload, NAME = c("A", "A", NAME[-(1:2)]))),
         arc_input),
    list(list(data = transform(upload, NAME = c(" ", NAME[-1]))), input),
    list(list(data = upload[-52, ]), arc_input),
    # search and layout controls
    list(list(budget = 0), input),
    list(list(budget = 10.5), input),
    list(list(budget = "600"), input),
    list(list(time_limit = 0), arc_input),
    list(list(time_limit = -1), arc_input),
    list(list(time_limit = Inf), arc_input),
    list(list(time_limit = NA_real_), arc_input),
    list(list(time_limit = c(1, 2)), arc_input),
    list(list(time_limit = "5"), arc_input),
    list(list(l = 0), input),
    list(list(l = 1.5), input),
    list(list(planter = "zigzag"), input),
    list(list(plotNumber = 0), arc_input),
    list(list(plotNumber = 10.5), input),
    list(list(plotNumber = c(1, 101)), arc_input),
    list(list(plotNumber = .Machine$integer.max - 10), arc_input),
    list(list(locationNames = c("A", "B")), arc_input),
    list(list(l = 2, locationNames = c("A", "A")), input),
    list(list(l = 2, locationNames = c("A", "")), input),
    list(list(exptName = c("E1", "E2")), arc_input),
    list(list(exptName = ""), input),
    list(list(year = c(2025, 2026)), input),
    list(list(seed = "2026"), input),
    list(list(seed = NA_real_), input)
  )
  for (case in cases) {
    arguments <- utils::modifyList(arc_api_call, case[[1]])
    if ("check_direction" %in% names(case[[1]]) &&
        is.null(case[[1]]$check_direction)) {
      arguments$check_direction <- NULL
    }
    label <- paste(deparse(case[[1]], width.cutoff = 500L), collapse = "")
    if (is.null(case[[2]])) {
      # A trailing space makes a distinct label: accepted, not an error
      expect_no_error(do.call(augmented_row_column,
                              utils::modifyList(arguments, list(budget = 100))))
      before <- get(".Random.seed", envir = globalenv())
      next
    }
    expect_error(do.call(augmented_row_column, arguments), class = case[[2]],
                 label = label)
    expect_identical(get(".Random.seed", envir = globalenv()), before,
                     label = paste("RNG after", label))
  }
  # Required arguments
  expect_error(augmented_row_column(ncols = 8, checks = 4,
                                    check_direction = "columns"),
               class = arc_input)
  expect_error(augmented_row_column(10, 8, check_direction = "columns"),
               class = arc_input)
  missing_direction <- tryCatch(augmented_row_column(10, 8, 4),
                                error = identity)
  expect_s3_class(missing_direction, arc_input)
  expect_match(conditionMessage(missing_direction), "no default", fixed = TRUE)
  expect_identical(get(".Random.seed", envir = globalenv()), before)
})

test_that("rejections name the fix", {
  wide <- tryCatch(arc_api_run(nrows = 8, ncols = 10), error = identity)
  expect_s3_class(wide, "fieldhub_dimension_error")
  expect_match(conditionMessage(wide), "check_direction = \"rows\"",
               fixed = TRUE)
  long <- tryCatch(arc_api_run(check_direction = "rows"), error = identity)
  expect_match(conditionMessage(long), "check_direction = \"columns\"",
               fixed = TRUE)
  infeasible <- tryCatch(arc_api_run(checks = 2), error = identity)
  expect_identical(infeasible$df_formula, -2L)
  expect_match(conditionMessage(infeasible), "Add a check", fixed = TRUE)
  expect_match(conditionMessage(infeasible), "rows and columns", fixed = TRUE)
  saturated <- tryCatch(arc_api_run(nrows = 6, ncols = 6, checks = 2),
                        error = identity)
  expect_identical(saturated$df_formula, 0L)
  expect_match(conditionMessage(saturated), "at least 1", fixed = TRUE)
  count <- tryCatch(arc_api_run(entries = 47), error = identity)
  expect_identical(count$argument, "entries")
  expect_identical(count$required, 48L)
  expect_match(conditionMessage(count), "48 test plots", fixed = TRUE)
  method <- tryCatch(arc_api_run(method = "multistart"), error = identity)
  expect_identical(method$options, "staged")
  objective <- tryCatch(arc_api_run(objective = "A"), error = identity)
  expect_identical(objective$options, c("E_aug", "V_tt", "V_tc"))
  overlap <- tryCatch(arc_api_run(checks = c("G5", "X", "Y", "Z")),
                      error = identity)
  expect_identical(overlap$labels, "G5")
  conflict <- tryCatch(
    arc_api_run(data = data.frame(ENTRY = 1:52, NAME = paste0("N", 1:52)),
                entries = 48),
    error = identity
  )
  expect_match(conditionMessage(conflict), "not both", fixed = TRUE)
})

test_that("low residual df warns once, with the M1 wording", {
  # 12 x 8 with 3 checks: residual df 3
  warnings <- list()
  x <- withCallingHandlers(
    arc_api_run(nrows = 12, ncols = 8, checks = 3, l = 2, budget = 300),
    fieldhub_arc_low_df_warning = function(w) {
      warnings[[length(warnings) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1L)
  w <- warnings[[1L]]
  expect_identical(w$df_formula, 3L)
  expect_match(conditionMessage(w), "only 3 residual degrees", fixed = TRUE)
  expect_match(conditionMessage(w), "adding a check", fixed = TRUE)
  expect_match(conditionMessage(w), "rows or columns", fixed = TRUE)
  expect_no_match(conditionMessage(w), "optimi|search|longer|iteration|budget",
                  ignore.case = TRUE)
  expect_identical(x$infoDesign$residual_df, 3L)
  expect_identical(x$statistics$residual_df, c(3L, 3L))
  # The warning comes before the seed: a warning turned into an error
  # leaves the caller's RNG alone
  arc_local_seed(5)
  before <- get(".Random.seed", envir = globalenv())
  expect_error(withCallingHandlers(
    arc_api_run(nrows = 12, ncols = 8, checks = 3, seed = NULL),
    fieldhub_arc_low_df_warning = function(w) stop("as error")
  ), "as error")
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  expect_no_warning(arc_api_run(budget = 100))
})

test_that("results have the documented structure and complete parameters", {
  x <- arc_api_run(l = 2, locationNames = c("North", "South"),
                   plotNumber = c(1, 1001))
  expect_s3_class(x, c("fieldhub_augmented_row_column", "FielDHub"),
                  exact = TRUE)
  expect_identical(names(x), c("infoDesign", "fieldBook", "statistics",
                               "diagnostics", "contraction", "timing",
                               "metadata"))
  expect_identical(x$metadata$design, "augmented_row_column")
  expect_identical(
    x$infoDesign,
    list(nrows = 10L, ncols = 8L, check_direction = "columns", v = 10L,
         s = 8L, k = 4L, checks = paste0("CH", 1:4), entries = 48L,
         locations = 2, locationNames = c("North", "South"),
         residual_df = 12L, objective = "V_tt", method = "staged",
         budget = 600, seed = 2026, id_design = "augmented_row_column")
  )
  # The parameters name every argument, with effective values
  expect_identical(names(x$metadata$parameters),
                   names(formals(augmented_row_column)))
  expect_identical(
    x$metadata$parameters,
    list(nrows = 10, ncols = 8, checks = 4, check_direction = "columns",
         entries = NULL, objective = "V_tt", method = "staged", budget = 600,
         time_limit = NULL, l = 2, plotNumber = c(1, 1001),
         planter = "serpentine", locationNames = c("North", "South"),
         exptName = "Expt1", year = format(Sys.Date(), "%Y"), seed = 2026,
         data = NULL)
  )
  fb <- x$fieldBook
  expect_identical(names(fb), c("ID", "EXPT", "LOCATION", "YEAR", "PLOT",
                                "ROW", "COLUMN", "CHECKS", "ENTRY",
                                "TREATMENT", "CHECK_DIRECTION"))
  expect_identical(nrow(fb), 160L)
  expect_identical(rownames(fb), as.character(seq_len(160L)))
  expect_identical(fb$LOCATION, rep(c("North", "South"), each = 80L))
  expect_identical(fb$PLOT, c(1 + 0:79, 1001 + 0:79))
  expect_identical(fb$ID, rep(seq_len(80L), 2L))
  expect_identical(
    names(x$statistics),
    c("LOCATION", "E_aug", "V_tt", "V_tc", "V_cc", "max_tt", "max_tc",
      "residual_df", "authority", "lambda_min_rel")
  )
  expect_identical(x$statistics$LOCATION, c("North", "South"))
  expect_identical(x$statistics$authority, rep("full_layout", 2L))
  expect_identical(names(x$diagnostics), c("North", "South"))
  expect_identical(names(x$contraction), c("North", "South"))
  for (site in names(x$diagnostics)) {
    d <- x$diagnostics[[site]]
    expect_identical(names(d), c("location", "seed", "status", "phases",
                                 "objective_calls", "proxy_calls",
                                 "proposals", "failed_proposals",
                                 "disconnected", "time_limit_hit"))
    expect_identical(d$location, site)
    expect_identical(d$status, "ok")
    expect_type(d$phases, "character")
    expect_false(d$time_limit_hit)
    expect_identical(dim(x$contraction[[site]]), c(4L, 8L))
    expect_type(x$contraction[[site]], "integer")
  }
  expect_identical(names(x$timing),
                   c("LOCATION", "search", "verification", "total"))
  expect_true(all(x$timing$total >= x$timing$search))
  expect_true(all(x$timing[c("search", "verification", "total")] >= 0))
})

test_that("returned field books pass the coordinate checks and the oracle", {
  checks <- c("Check A", "std-2 (local); \"B\"", "Cé #3", "D/4")
  tests <- sprintf("Line %02d / x.%s, y", 1:48, rep_len(c("a", " b", "c'"), 48))
  for (direction in arc_api_directions) {
    for (objective in c("V_tt", "E_aug")) {
      x <- arc_api_oriented(direction, checks = checks, entries = tests,
                            objective = objective, l = 2, seed = 41)
      for (site in 1:2) {
        label <- paste(direction, objective, site)
        fb <- arc_api_location_book(x, site)
        expect_identical(nrow(fb), 80L, label = label)
        expect_identical(fb$CHECK_DIRECTION, rep(direction, 80L))
        # Each check once per line along check_direction, s = 8 times in
        # total; M1 rules 6-7 on the cross-lines; one plot per cell
        expect_true(arc_api_placement_ok(fb), label = label)
        rec <- arc_reconstruct(fb)
        expect_identical(c(rec$v, rec$s, rec$k), c(10L, 8L, 4L))
        # The reconstruction from coordinates is the returned contraction
        expect_identical(rec$contraction, x$contraction[[site]], label = label)
        expect_no_error(arc_check_contraction(x$contraction[[site]], 10L,
                                              "restricted"))
        # Every test once; labels round-trip bound to their identifiers
        on_test <- fb$CHECKS == 0L
        expect_identical(sort(fb$TREATMENT[on_test], method = "radix"),
                         sort(tests, method = "radix"))
        expect_identical(as.vector(table(factor(fb$TREATMENT[!on_test],
                                                levels = checks))),
                         rep(8L, 4L))
        expect_identical(fb$TREATMENT, c(checks, tests)[fb$ENTRY])
        expect_identical(fb$CHECKS[!on_test], fb$ENTRY[!on_test])
        # The independent least-squares oracle agrees with the statistics
        oracle <- arc_field_book_oracle(fb)
        stats <- x$statistics[site, ]
        expect_true(oracle$connected, label = label)
        expect_identical(oracle$residual_df, stats$residual_df, label = label)
        expect_identical(oracle$residual_df_svd, 12L, label = label)
        for (metric in arc_api_metrics) {
          expect_lte(abs(oracle[[metric]] - stats[[metric]]), arc_api_tol,
                     label = paste(label, metric))
        }
      }
    }
  }
})

test_that("statistics are arc_final_score() of the returned contraction", {
  for (direction in arc_api_directions) {
    x <- arc_api_oriented(direction, objective = "V_tc", l = 2, seed = 43)
    for (site in 1:2) {
      final <- arc_final_score(x$contraction[[site]], 10L, "restricted")
      stats <- x$statistics[site, ]
      for (metric in c(arc_api_metrics, "lambda_min_rel", "authority")) {
        expect_identical(stats[[metric]], final[[metric]],
                         label = paste(direction, site, metric))
      }
      expect_identical(stats$residual_df, final$df_formula)
    }
  }
})

test_that("reproduce_design() replays the deterministic content exactly", {
  for (direction in arc_api_directions) {
    x <- arc_api_oriented(direction, checks = c("A 1", "B-2", "C.3", "D,4"),
                          l = 2, seed = 47, planter = "cartesian",
                          locationNames = c("Field 1", "Field 2"),
                          exptName = "Trial 26", year = 2025)
    replay <- reproduce_design(x)
    expect_identical(arc_api_view(replay), arc_api_view(x))
    direct <- do.call(augmented_row_column, x$metadata$parameters)
    expect_identical(arc_api_view(direct), arc_api_view(x))
    # Timing is the only difference the definition allows
    expect_identical(names(replay), names(x))
    expect_false(identical(arc_api_view(x), x))
  }
})

test_that("explicit seeds are recorded and leave the caller's RNG alone", {
  arc_local_seed(71) # restores the session's RNG when this test ends
  before <- get(".Random.seed", envir = globalenv())
  x <- arc_api_run(seed = 5, budget = 200)
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  expect_identical(x$metadata$seed, 5)
  expect_identical(x$infoDesign$seed, 5)
  expect_identical(x$diagnostics[[1]]$seed, 5)
  # Different seeds give different placements
  other <- arc_api_run(seed = 6, budget = 200)
  expect_false(identical(other$fieldBook$TREATMENT, x$fieldBook$TREATMENT))
  expect_identical(get(".Random.seed", envir = globalenv()), before)
})

test_that("an automatic seed consumes exactly one recorded draw and replays", {
  # Each helper seeds and restores the RNG in its own frame
  automatic_run <- function() {
    arc_local_seed(72)
    x <- arc_api_run(seed = NULL, budget = 200)
    list(x = x, after = get(".Random.seed", envir = globalenv()))
  }
  one_draw <- function() {
    arc_local_seed(72)
    seed <- sample.int(.Machine$integer.max, 1L)
    list(seed = seed, after = get(".Random.seed", envir = globalenv()))
  }
  run <- automatic_run()
  expected <- one_draw()
  expect_identical(run$x$infoDesign$seed, expected$seed)
  expect_identical(run$x$metadata$parameters$seed, expected$seed)
  # Exactly one draw reached the caller's stream
  expect_identical(run$after, expected$after)
  expect_identical(arc_api_view(reproduce_design(run$x)), arc_api_view(run$x))
})

test_that("locations are independent runs with offset seeds", {
  x <- arc_api_run(l = 3, seed = 100, locationNames = c("A", "B", "C"),
                   budget = 300)
  expect_identical(vapply(x$diagnostics, `[[`, numeric(1), "seed"),
                   c(A = 100, B = 101, C = 102))
  layout <- c("ROW", "COLUMN", "CHECKS", "ENTRY", "TREATMENT")
  for (i in 1:3) {
    alone <- arc_api_run(seed = 99 + i, budget = 300)
    site <- c("A", "B", "C")[i]
    fb <- arc_api_location_book(x, site)
    rownames(fb) <- NULL
    expect_identical(fb[layout], alone$fieldBook[layout], label = site)
    expect_identical(x$contraction[[site]], alone$contraction[[1]])
    expect_identical(x$statistics[i, -1], alone$statistics[1, -1],
                     ignore_attr = TRUE)
    expect_identical(x$diagnostics[[site]][-1], alone$diagnostics[[1]][-1])
  }
  expect_false(identical(x$contraction$A, x$contraction$B))
})

test_that("the evaluation budget is respected and resolved by field size", {
  for (budget in c(150, 450)) {
    x <- arc_api_run(budget = budget, l = 2)
    for (d in x$diagnostics) {
      expect_lte(d$objective_calls + d$proxy_calls, budget)
      expect_gt(d$objective_calls, 0)
    }
  }
  expect_identical(arc_default_budget(20, 30), 60000)
  expect_identical(arc_default_budget(25, 25), 100000)
  expect_identical(arc_default_budget(48, 16), 100000)
  # NULL resolves to the size rule, recorded as the effective budget; the
  # rule's budgets are made small here so the search stays fast
  local_mocked_bindings(arc_design_defaults = function() {
    list(objective = "V_tt", methods = "staged", budget_cells = 600,
         budget_small = 120, budget_large = 160)
  })
  small <- arc_api_run(budget = NULL)
  expect_identical(small$infoDesign$budget, 120)
  expect_identical(small$metadata$parameters$budget, 120)
  expect_lte(small$diagnostics[[1]]$objective_calls +
               small$diagnostics[[1]]$proxy_calls, 120)
  large <- arc_api_run(nrows = 26, ncols = 24, checks = 5, budget = NULL)
  expect_identical(large$infoDesign$budget, 160)
  expect_identical(arc_api_view(reproduce_design(large)), arc_api_view(large))
})

test_that("the provisional default objective is recorded and always explicit", {
  expect_identical(arc_design_defaults()$objective, "V_tt")
  expect_identical(formals(augmented_row_column)$objective,
                   quote(arc_design_defaults()$objective))
  expect_identical(formals(augmented_row_column)$method, "staged")
  x <- arc_api_run(budget = 100)
  expect_identical(x$infoDesign$objective, "V_tt")
  expect_identical(x$metadata$parameters$objective, "V_tt")
  code <- paste(design_call_code(x), collapse = "\n")
  expect_match(code, "objective = \"V_tt\"", fixed = TRUE)
  expect_match(code, "budget = 100", fixed = TRUE)
})

test_that("time_limit cuts each location's search and verification still runs", {
  # A clock that advances 0.01 s on every reading makes the cutoff
  # deterministic
  clock <- arc_fake_clock()
  local_mocked_bindings(arc_clock = function() clock$advance(0.01))
  full <- arc_api_run(budget = 2000, seed = 3)
  expect_false(full$diagnostics[[1]]$time_limit_hit)
  limit <- full$timing$search / 2
  limited <- arc_api_run(budget = 2000, seed = 3, time_limit = limit, l = 2)
  expect_identical(limited$metadata$parameters$time_limit, limit)
  for (site in 1:2) {
    d <- limited$diagnostics[[site]]
    expect_true(d$time_limit_hit)
    expect_true("time_limit" %in% d$phases)
    expect_identical(d$status, "ok")
    expect_lt(d$objective_calls + d$proxy_calls, 2000)
    # The search stopped at the deadline, give or take one scored step
    expect_gte(limited$timing$search[site], limit)
    expect_lt(limited$timing$search[site], limit + 1)
    # Verification and assembly ran after the cutoff
    expect_gt(limited$timing$verification[site], 0)
    expect_gt(limited$timing$total[site], limited$timing$search[site])
    fb <- arc_api_location_book(limited, site)
    expect_true(arc_api_placement_ok(fb))
    expect_identical(arc_reconstruct(fb)$contraction,
                     limited$contraction[[site]])
    final <- arc_final_score(limited$contraction[[site]], 10L, "restricted")
    expect_identical(limited$statistics$V_tt[site], final$V_tt)
  }
  # A cutoff before any design exists is a classed search error
  arc_local_seed(9)
  before <- get(".Random.seed", envir = globalenv())
  early <- tryCatch(arc_api_run(budget = 2000, time_limit = 0.001),
                    error = identity)
  expect_s3_class(early, "fieldhub_arc_search_error")
  expect_s3_class(early, "fieldhub_search_error")
  expect_true(early$time_limit_hit)
  expect_match(conditionMessage(early), "time_limit", fixed = TRUE)
  expect_identical(get(".Random.seed", envir = globalenv()), before)
})

test_that("a search without a design raises a classed error, never a fallback", {
  arc_local_seed(11)
  before <- get(".Random.seed", envir = globalenv())
  tiny <- tryCatch(arc_api_run(budget = 1), error = identity)
  expect_s3_class(tiny, "fieldhub_arc_search_error")
  expect_identical(tiny$status, "no_design")
  expect_false(tiny$time_limit_hit)
  expect_match(conditionMessage(tiny), "Increase budget", fixed = TRUE)
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  local_mocked_bindings(arc_random_start = function(...) NULL)
  exhausted <- tryCatch(arc_api_run(l = 2), error = identity)
  expect_s3_class(exhausted, "fieldhub_arc_search_error")
  expect_identical(exhausted$status, "start_search_exhausted")
  expect_identical(exhausted$location, 1L)
  expect_match(conditionMessage(exhausted), "starting design", fixed = TRUE)
  expect_identical(get(".Random.seed", envir = globalenv()), before)
})

test_that("exptName, year and locationNames are recorded as effective values", {
  x <- arc_api_run(l = 2, budget = 100)
  this_year <- format(Sys.Date(), "%Y")
  expect_identical(x$metadata$parameters$exptName, "Expt1")
  expect_identical(x$metadata$parameters$year, this_year)
  expect_identical(x$metadata$parameters$locationNames, 1:2)
  expect_identical(x$infoDesign$locationNames, 1:2)
  expect_identical(unique(x$fieldBook$EXPT), "Expt1")
  expect_identical(unique(x$fieldBook$YEAR), this_year)
  expect_identical(unique(x$fieldBook$LOCATION), 1:2)
  # Replay in a later year reproduces the recorded YEAR
  local_mocked_bindings(resolve_year = function(year) {
    if (is.null(year)) "2099" else as.character(year)
  })
  expect_identical(arc_api_run(budget = 100)$fieldBook$YEAR[1], "2099")
  expect_identical(arc_api_view(reproduce_design(x)), arc_api_view(x))
  named <- arc_api_run(budget = 100, exptName = "AYT 26", year = 2027,
                       locationNames = "Casselton")
  expect_identical(named$metadata$parameters$year, "2027")
  expect_identical(unique(named$fieldBook$EXPT), "AYT 26")
  expect_identical(unique(named$fieldBook$LOCATION), "Casselton")
})

test_that("standard-template uploads are recorded as labels", {
  checks <- c("Check one", "Check two", "Check three", "Check four")
  tests <- paste("Line", 1:48)
  upload <- data.frame(ENTRY = 1:52, NAME = c(checks, tests))
  x <- arc_api_run(checks = 4, data = upload, budget = 200)
  expect_null(x$metadata$parameters$data)
  expect_identical(x$metadata$parameters$checks, checks)
  expect_identical(x$metadata$parameters$entries, tests)
  expect_identical(x$infoDesign$checks, checks)
  expect_identical(x$fieldBook$ENTRY, match(x$fieldBook$TREATMENT,
                                            upload$NAME))
  # The same call with labels gives the same deterministic content
  labelled <- arc_api_run(checks = checks, entries = tests, budget = 200)
  expect_identical(arc_api_view(labelled), arc_api_view(x))
  expect_identical(arc_api_view(reproduce_design(x)), arc_api_view(x))
  # Double identifiers and factor labels are the same template
  as_factor <- data.frame(ENTRY = as.numeric(1:52),
                          NAME = factor(upload$NAME, levels = upload$NAME))
  expect_identical(arc_api_view(arc_api_run(checks = 4, data = as_factor,
                                            budget = 200)),
                   arc_api_view(x))
})

test_that("non-standard identifiers keep data and the checks' input order", {
  checks <- paste0("C", 1:4)
  tests <- paste0("T", 1:48)
  ids <- c(905L, 17L, 300L, 41L, 2000L - 1:48)
  upload <- data.frame(NAME = c(checks, tests), ENTRY = ids)
  for (direction in arc_api_directions) {
    x <- arc_api_oriented(direction, checks = 4, data = upload, budget = 300)
    expect_identical(x$metadata$parameters$data, upload)
    expect_identical(x$metadata$parameters$checks, 4)
    expect_null(x$metadata$parameters$entries)
    fb <- x$fieldBook
    expect_identical(fb$ENTRY, ids[match(fb$TREATMENT, upload$NAME)])
    expect_identical(fb$CHECKS, ifelse(fb$TREATMENT %in% checks, fb$ENTRY,
                                       0L))
    # Contraction rows follow the checks' input order, not their
    # identifiers' order, so labels give the same design
    expect_identical(x$contraction[[1]],
                     arc_contraction_from_field_book(fb, 4, direction,
                                                     check_ids = ids[1:4]))
    labelled <- arc_api_oriented(direction, checks = checks, entries = tests,
                                 budget = 300)
    expect_identical(x$contraction, labelled$contraction)
    expect_identical(x$statistics, labelled$statistics)
    expect_identical(fb[c("ROW", "COLUMN", "TREATMENT")],
                     labelled$fieldBook[c("ROW", "COLUMN", "TREATMENT")])
    expect_identical(arc_api_view(reproduce_design(x)), arc_api_view(x))
  }
})

test_that("a field-scale standard upload gives standalone code that replays", {
  # Section 5: 48 x 16 with 5 checks, 688 tests (693 entry rows); a reduced
  # budget checks the recording route, not search quality
  upload <- data.frame(ENTRY = 1:693,
                       NAME = c(paste("Check", LETTERS[1:5]),
                                sprintf("BRD-%04d", 1:688)))
  x <- augmented_row_column(nrows = 48, ncols = 16, checks = 5,
                            check_direction = "columns", data = upload,
                            budget = 300, seed = 112660)
  expect_identical(nrow(x$fieldBook), 768L)
  expect_identical(x$infoDesign$residual_df, 13L)
  expect_null(x$metadata$parameters$data)
  code <- design_call_code(x)
  expect_type(code, "character")
  expect_gt(length(code), 0L)
  expect_null(attr(code, "reason"))
  text <- paste(code, collapse = "\n")
  expect_false(grepl("data =", text, fixed = TRUE))
  expect_match(text, "entries = c(", fixed = TRUE)
  expect_match(text, "objective = \"V_tt\"", fixed = TRUE)
  rebuilt <- eval(parse(text = code), envir = new.env(parent = baseenv()))
  expect_identical(arc_api_view(rebuilt), arc_api_view(x))
})

test_that("a non-standard upload over 200 rows falls back to the saved result", {
  # 24 x 12 with 5 checks: 228 tests, 233 entry rows
  upload <- data.frame(ENTRY = 5000L + 1:233,
                       NAME = c(paste0("CK", 1:5), paste0("L", 1:228)))
  x <- augmented_row_column(nrows = 24, ncols = 12, checks = 5,
                            check_direction = "columns", data = upload,
                            budget = 200, seed = 15)
  expect_identical(x$metadata$parameters$data, upload)
  code <- design_call_code(x)
  expect_identical(length(code), 0L)
  expect_match(attr(code, "reason"), "`data`: an uploaded table has more ",
               fixed = TRUE)
  expect_match(attr(code, "reason"), "more than 200 rows", fixed = TRUE)
  section <- design_call_section(x)
  expect_match(section[1], "Standalone call unavailable", fixed = TRUE)
  expect_match(section[2], "saved RDS", fixed = TRUE)
  # The saved result still replays
  expect_identical(arc_api_view(reproduce_design(x)), arc_api_view(x))
  # Up to 200 rows a non-standard upload keeps standalone code
  small <- arc_api_run(checks = 4, budget = 100,
                       data = data.frame(ENTRY = 100L + 1:52,
                                         NAME = paste0("E", 1:52)))
  small_code <- design_call_code(small)
  expect_gt(length(small_code), 0L)
  expect_identical(
    arc_api_view(eval(parse(text = small_code),
                      envir = new.env(parent = baseenv()))),
    arc_api_view(small)
  )
})

test_that("the registry declares the replay fields of the design", {
  entry <- fieldhub_design_registry()$augmented_row_column
  expect_identical(entry$engine, "augmented_row_column")
  expect_identical(entry$columns,
                   c("EXPT", "YEAR", "ROW", "COLUMN", "CHECKS", "ENTRY",
                     "TREATMENT", "CHECK_DIRECTION"))
  expect_identical(entry$title, "Augmented Row-Column Design")
  expect_identical(fieldhub_engine_registry()[["augmented_row_column"]],
                   "augmented_row_column")
  x <- arc_api_run(budget = 100)
  expect_identical(reproduction_engine(x), augmented_row_column)
  expect_identical(validate_fieldhub_design(x), x)
  for (column in c("CHECK_DIRECTION", "CHECKS", "TREATMENT", "ROW")) {
    bad <- x
    bad$fieldBook[[column]] <- NULL
    expect_error(validate_fieldhub_design(bad), column,
                 class = "fieldhub_internal_error")
  }
  expect_output(print(x, n = 2), "Augmented Row-Column Design", fixed = TRUE)
})

# Task 4: presentation ------------------------------------------------------
# (section 3.8). print(), summary(), field_layout(), plot() and draw_layout()
# reach the design only through its registry entry: the title and the layout,
# render and details handlers. Residual df come from the inputs and the
# printed statistics from the independent field-book oracle.

# The lines of print(x, n = 6), the app's summary path (app_spatial_page.R)
arc_api_printed <- function(x) utils::capture.output(print(x, n = 6))

# The details block of one location: its heading and the indented lines
# that follow it
arc_api_details_block <- function(lines, site) {
  start <- match(paste0("Location ", site, ":"), lines)
  if (is.na(start)) return(character())
  end <- start
  while (end < length(lines) && startsWith(lines[end + 1L], "  ")) end <- end + 1L
  lines[start:end]
}

# The number printed after "<name> = " on a line
arc_api_printed_value <- function(line, name) {
  as.numeric(sub(paste0(".*\\b", name, " = ([-+.0-9e]+).*"), "\\1", line))
}

# Unit cell edges of a map's segments, as "h x y" (horizontal, centred at
# x) and "v x y" (vertical, centred at y)
arc_api_segment_edges <- function(segments) {
  edges <- character()
  for (i in seq_len(nrow(segments))) {
    s <- segments[i, ]
    if (s$y == s$yend) {
      centres <- seq(min(s$x, s$xend) + 0.5, max(s$x, s$xend) - 0.5, by = 1)
      edges <- c(edges, paste("h", centres, s$y))
    } else {
      centres <- seq(min(s$y, s$yend) + 0.5, max(s$y, s$yend) - 0.5, by = 1)
      edges <- c(edges, paste("v", s$x, centres))
    }
  }
  unique(edges)
}

# Interior edges of a field book between neighbouring plots, split by
# whether the two plots differ in CHECKS and whether both are test plots
arc_api_book_edges <- function(fb) {
  key <- paste(fb$COLUMN, fb$ROW)
  checks <- stats::setNames(fb$CHECKS, key)
  right <- paste(fb$COLUMN + 1L, fb$ROW)
  up <- paste(fb$COLUMN, fb$ROW + 1L)
  edges <- c(paste("v", fb$COLUMN + 0.5, fb$ROW), paste("h", fb$COLUMN, fb$ROW + 0.5))
  here <- c(fb$CHECKS, fb$CHECKS)
  there <- unname(checks[c(right, up)])
  inside <- !is.na(there)
  list(differ = edges[inside & here != there],
       tests = edges[inside & here == 0L & there == 0L])
}

test_that("the registry declares the presentation fields of the design", {
  registry <- fieldhub_design_registry()
  entry <- registry$augmented_row_column
  expect_identical(entry$layout, "field_book_layouts")
  expect_identical(entry$render, "draw_augmented_row_column_layout")
  expect_identical(entry$details, "arc_summary_lines")
  for (handler in c(entry$layout, entry$render, entry$details)) {
    expect_true(is.function(get(handler, envir = asNamespace("FielDHub"),
                                inherits = FALSE)), label = handler)
  }
  # Presentation needs no design-specific S3 method
  for (generic in c("print", "summary", "plot", "layout_options", "draw_layout")) {
    expect_null(getS3method(generic, "fieldhub_augmented_row_column",
                            optional = TRUE), label = generic)
  }
  expect_null(getS3method("print", "summary.fieldhub_augmented_row_column",
                          optional = TRUE))
  # No other entry declares details, so no other output changes
  declared <- names(Filter(function(e) !is.null(e$details), registry))
  expect_identical(declared, "augmented_row_column")
})

test_that("print() and summary() show each location's statistics, as the app does", {
  v <- 10L
  s <- 8L
  k <- 4L
  df <- k * s - k - v - s + 2L
  sites <- c("Fargo", "Casselton")
  for (direction in arc_api_directions) {
    for (objective in c("V_tt", "E_aug")) {
      x <- arc_api_oriented(direction, objective = objective, l = 2,
                            budget = 300, locationNames = sites)
      printed <- arc_api_printed(x)
      summarised <- utils::capture.output(print(summary(x)))
      # The statistics follow the title, before the design parameters
      expect_identical(printed[1:3], c(
        "Augmented Row-Column Design ", "",
        "Statistics by location (variances in units of the residual variance):"
      ))
      expect_identical(summarised[1:3], printed[1:3])
      expect_lt(match("Location Casselton:", printed),
                match("Information on the design parameters: ", printed))
      expect_lt(match("Location Casselton:", summarised),
                match("1. Information on the design parameters: ", summarised))
      sense <- if (objective == "E_aug") "maximized" else "minimized"
      for (site in sites) {
        label <- paste(direction, objective, site)
        block <- arc_api_details_block(printed, site)
        expect_length(block, 6L)
        expect_identical(arc_api_details_block(summarised, site), block,
                         label = label)
        expect_identical(block[2], paste0("  Residual df: ", df), label = label)
        oracle <- arc_field_book_oracle(arc_api_location_book(x, site))
        expect_match(block[3], paste0("^  Optimized objective: ", objective,
                                      " = [-+.0-9e]+ \\(", sense, "\\)$"),
                     label = label)
        expect_equal(arc_api_printed_value(block[3], objective),
                     oracle[[objective]], tolerance = 1e-3, label = label)
        for (metric in c("E_aug", "V_tt", "V_tc")) {
          expect_equal(arc_api_printed_value(block[4], metric), oracle[[metric]],
                       tolerance = 1e-3, label = paste(label, metric))
        }
        for (metric in c("max_tt", "max_tc")) {
          expect_equal(arc_api_printed_value(block[5], metric), oracle[[metric]],
                       tolerance = 1e-3, label = paste(label, metric))
        }
        expect_identical(block[6], "  Search status: ok", label = label)
      }
      # Twelve residual df: no low-df notice
      expect_no_match(printed, "Notice", fixed = TRUE)
      expect_no_match(summarised, "Notice", fixed = TRUE)
    }
  }
})

test_that("a low-df design shows the notice on every print and summary", {
  notice <- c(
    "  Notice: only 3 residual degrees of freedom (fewer than 10). Only adding a",
    "  check or changing the number of rows or columns raises them."
  )
  for (direction in arc_api_directions) {
    # A 12 x 8 field with 3 checks: k s - k - v - s + 2 = 3 residual df
    dims <- arc_api_dims(12L, 8L, direction)
    expect_warning(
      x <- arc_api_run(nrows = dims[["nrows"]], ncols = dims[["ncols"]],
                       checks = 3, check_direction = direction, l = 2,
                       budget = 300),
      class = "fieldhub_arc_low_df_warning"
    )
    # Persistent: shown each time, with no further warning
    for (time in 1:2) {
      expect_no_warning(printed <- arc_api_printed(x))
      expect_no_warning(summarised <- utils::capture.output(print(summary(x))))
      for (site in 1:2) {
        label <- paste(direction, time, site)
        block <- arc_api_details_block(printed, site)
        expect_identical(block[2], "  Residual df: 3", label = label)
        expect_identical(utils::tail(block, 2L), notice, label = label)
        expect_identical(arc_api_details_block(summarised, site), block,
                         label = label)
      }
    }
  }
})

test_that("a location stopped by time_limit says so when printed", {
  clock <- arc_fake_clock()
  local_mocked_bindings(arc_clock = function() clock$advance(0.01))
  full <- arc_api_run(budget = 600, seed = 3)
  limited <- arc_api_run(budget = 600, seed = 3, time_limit = full$timing$search / 2)
  expect_true(limited$diagnostics[[1]]$time_limit_hit)
  expect_identical(arc_api_details_block(arc_api_printed(full), 1)[6],
                   "  Search status: ok")
  expect_identical(arc_api_details_block(arc_api_printed(limited), 1)[6],
                   "  Search status: ok (time limit reached)")
})

test_that("field_layout(), plot() and draw_layout() work in both orientations", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  planters <- c("serpentine", "cartesian")
  for (direction in arc_api_directions) {
    dims <- arc_api_dims(10L, 8L, direction)
    title <- paste0("Augmented Row-Column Design ", dims[["nrows"]], "X",
                    dims[["ncols"]])
    for (planter in planters) {
      label <- paste(direction, planter)
      x <- arc_api_oriented(direction, planter = planter, l = 2, budget = 100,
                            locationNames = c("A", "B"))
      # Plots are placed at construction: every planter and the only layout
      # option return the field book as it is
      for (asked in planters) {
        expect_identical(field_layout(x, planter = asked), x$fieldBook,
                         label = label)
      }
      expect_error(field_layout(x, layout = 2), class = "fieldhub_input_error")
      plotted <- plot(x, l = 2)
      expect_s3_class(plotted$p, "ggplot")
      expect_identical(plotted$field_book, x$fieldBook, label = label)
      book <- arc_api_location_book(x, "B")
      rownames(book) <- NULL
      view <- plot_layout(x, l = 2)
      expect_identical(view$fieldBookXY, book, label = label)
      drawn <- draw_layout(x, book)
      expect_identical(drawn$data, book, label = label)
      for (map in list(drawn$p1, drawn$p2, view$out_layout, view$out_layoutPlots)) {
        expect_s3_class(map, "ggplot")
        expect_identical(map$labels$title, title, label = label)
        expect_silent(ggplot2::ggplot_build(map))
      }
      # The plot-number map labels every plot with its number on a neutral fill
      built <- ggplot2::ggplot_build(drawn$p2)$data
      expect_identical(nrow(built[[1]]), nrow(book))
      expect_true(all(built[[1]]$fill == fieldhub_layout_neutral()))
      text <- built[[length(built)]]
      expect_identical(text$label[match(paste(book$COLUMN, book$ROW),
                                        paste(text$x, text$y))],
                       sprintf("%d", as.integer(book$PLOT)), label = label)
    }
  }
})

test_that("the treatment map fills and outlines the checks by CHECKS", {
  checks <- c("Check A", "B", "C", "D")
  for (direction in arc_api_directions) {
    x <- arc_api_oriented(direction, checks = checks, budget = 100, seed = 17)
    book <- x$fieldBook
    p1 <- draw_layout(x, book)$p1
    built <- ggplot2::ggplot_build(p1)$data
    geoms <- vapply(p1$layers, function(layer) class(layer$geom)[1L], "")
    tiles <- built[[match("GeomTile", geoms)]]
    text <- built[[match("GeomText", geoms)]]
    segments <- built[[match("GeomSegment", geoms)]]
    at <- paste(book$COLUMN, book$ROW)
    fill <- tiles$fill[match(at, paste(tiles$x, tiles$y))]
    expect_false(anyNA(fill))
    # Each plot shows its treatment label
    expect_identical(text$label[match(at, paste(text$x, text$y))], book$TREATMENT)
    # Fill groups are exactly the CHECKS groups: tests neutral, one shared
    # palette colour per check, distinct across checks
    test <- book$CHECKS == 0L
    expect_true(all(fill[test] == fieldhub_layout_neutral()), label = direction)
    expect_identical(length(unique(fill)), length(unique(book$CHECKS)))
    expect_identical(nrow(unique(data.frame(fill, book$CHECKS))),
                     length(unique(book$CHECKS)))
    by_check <- vapply(split(fill[!test], book$CHECKS[!test]),
                       function(f) paste(unique(f), collapse = " "), "")
    expect_identical(unname(by_check), fieldhub_layout_palette()[2:5],
                     label = direction)
    # Check borders: every edge between plots of different CHECKS is drawn,
    # and none between two test plots
    expect_true(all(segments$colour == "gray50"))
    drawn_edges <- arc_api_segment_edges(segments)
    expected <- arc_api_book_edges(book)
    expect_true(all(expected$differ %in% drawn_edges), label = direction)
    expect_false(any(expected$tests %in% drawn_edges), label = direction)
  }
})

test_that("latin_rectangle print and summary output is unchanged by details", {
  # The output that print_design() and print.summary.FielDHub() gave before
  # the details field, rebuilt from base functions
  x <- latin_rectangle(c("No nitrogen", "A*B", "e 3", "4", "five"), 3, l = 2,
                       locationNames = c("Fargo", " CASSELTON"), seed = 7)
  expect_null(fieldhub_design_registry()$latin_rectangle$details)
  expect_identical(registered_design_details(x), character())
  for (n in c(10L, 6L)) {
    expected <- utils::capture.output({
      cat("Latin Rectangle Design", "\n\n")
      cat("Information on the design parameters:", "\n")
      str(x$infoDesign[names(x$infoDesign) != "id_design"])
      cat("\n", n, "First observations of the data frame with the latin_rectangle field book:",
          "\n")
      print(head(x$fieldBook, n = n))
    })
    expect_identical(utils::capture.output(print(x, n = n)), expected)
  }
  expected <- utils::capture.output({
    cat("Latin Rectangle Design", "\n\n")
    cat("1. Information on the design parameters:", "\n")
    str(x$infoDesign)
    cat("\n")
    cat("2. Structure of the field book:", "\n\n")
    str(x$fieldBook)
  })
  expect_identical(utils::capture.output(print(summary(x))), expected)
})
