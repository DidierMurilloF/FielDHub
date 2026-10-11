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
