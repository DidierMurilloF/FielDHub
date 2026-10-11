# Field-design layer of the augmented row-column design (research M3
# specification, revision 2, sections 3.3, 3.5, 3.6 and 3.7; implementation
# plan task 2): orientation, randomization, field-book assembly, contraction
# recovery from coordinates, and the deterministic view used for replay. The
# engine (engine_augmented_row_column.R) works in the mathematical v x s
# orientation, in which every check occurs once in each mathematical column;
# this file maps that orientation to the physical field.

#' The two values of check_direction
#' @noRd
arc_check_directions <- c("columns", "rows")

#' Check the check_direction argument
#'
#' It is required, has no default and is never inferred from the field shape
#' (M3 specification section 3.3).
#' @param check_direction `"columns"` or `"rows"`.
#' @return `check_direction`, invisibly.
#' @noRd
arc_check_direction <- function(check_direction) {
  if (missing(check_direction) || !is.character(check_direction) ||
      length(check_direction) != 1L || is.na(check_direction) ||
      !check_direction %in% arc_check_directions) {
    fieldhub_abort("check_direction must be \"columns\" or \"rows\": the ",
                   "field direction along which every check appears once ",
                   "per line. It has no default and is not inferred from ",
                   "the field shape.",
                   class = c("fieldhub_arc_input_error",
                             "fieldhub_input_error"),
                   data = list(argument = "check_direction",
                               choices = arc_check_directions))
  }
  invisible(check_direction)
}

#' Map the physical field to the mathematical orientation
#'
#' M3 specification section 3.3. `check_direction` names the physical
#' direction along which every check appears once per line:
#' - `"columns"`: each check once in every physical column, so `v = nrows`,
#'   `s = ncols`, physical `ROW` = mathematical row and physical `COLUMN` =
#'   mathematical column;
#' - `"rows"`: each check once in every physical row, so `v = ncols`,
#'   `s = nrows`, physical `ROW` = mathematical column and physical `COLUMN`
#'   = mathematical row.
#'
#' Restricted mode needs `s <= v` (M1 rule 3). A field with `s > v` in the
#' chosen direction is rejected with a dimension error that names the other
#' direction, which always fits it. Only the orientation is checked here;
#' the remaining dimension rules (`2 <= k < v`, `s >= 2`, degrees of freedom)
#' belong to the search entry check.
#'
#' @param nrows,ncols Physical field rows and columns, whole numbers >= 1.
#' @param check_direction `"columns"` or `"rows"`; required.
#' @return A list with `check_direction`, integer `nrows`, `ncols`, `v` and
#'   `s`, and `map`, a named character vector giving the mathematical source
#'   of each physical coordinate: `c(ROW = "math_row", COLUMN =
#'   "math_column")` for `"columns"` and the reverse for `"rows"`.
#' @noRd
arc_orient <- function(nrows, ncols, check_direction) {
  arc_check_direction(check_direction)
  for (dimension in list(nrows, ncols)) {
    if (!arc_is_whole_number(dimension) || dimension < 1) {
      fieldhub_abort("nrows and ncols must be single whole numbers of at ",
                     "least 1.",
                     class = c("fieldhub_arc_input_error",
                               "fieldhub_input_error"))
    }
  }
  nrows <- as.integer(nrows)
  ncols <- as.integer(ncols)
  by_columns <- identical(check_direction, "columns")
  v <- if (by_columns) nrows else ncols
  s <- if (by_columns) ncols else nrows
  if (s > v) {
    other <- setdiff(arc_check_directions, check_direction)
    lines <- if (by_columns) "columns" else "rows"
    cross <- if (by_columns) "rows" else "columns"
    fieldhub_abort("With check_direction = \"", check_direction, "\", every ",
                   "check appears once in each of the ", s, " field ", lines,
                   ", which needs at least ", s, " field ", cross, "; the ",
                   "field has ", v, " (s = ", s, " > v = ", v, "). Use ",
                   "check_direction = \"", other, "\", or change the ",
                   "numbers of rows and columns.",
                   class = c("fieldhub_arc_dimension_error",
                             "fieldhub_dimension_error"),
                   data = list(check_direction = check_direction,
                               suggested = other, v = v, s = s))
  }
  map <- if (by_columns) {
    c(ROW = "math_row", COLUMN = "math_column")
  } else {
    c(ROW = "math_column", COLUMN = "math_row")
  }
  list(check_direction = check_direction, nrows = nrows, ncols = ncols,
       v = v, s = s, map = map)
}

#' A v x s mathematical matrix as the nrows x ncols physical field
#' @param math A v x s matrix in mathematical orientation.
#' @param orientation The result of `arc_orient()`.
#' @noRd
arc_physical_matrix <- function(math, orientation) {
  if (identical(orientation$map[["ROW"]], "math_row")) math else t(math)
}

#' Mathematical coordinates of physical plots
#'
#' The inverse of the `arc_orient()` map.
#' @param check_direction `"columns"` or `"rows"`.
#' @param row,column Physical ROW and COLUMN vectors.
#' @return A list with `row` (mathematical row, the cross-line) and `column`
#'   (mathematical column, the line along `check_direction`).
#' @noRd
arc_math_coordinates <- function(check_direction, row, column) {
  if (identical(check_direction, "columns")) {
    list(row = row, column = column)
  } else {
    list(row = column, column = row)
  }
}

#' Randomize a contraction into a mathematical placement
#'
#' M3 specification section 3.5 and M1 section 9. The input contraction is
#' k x s with entry (h, j) the mathematical row of check h in mathematical
#' column j; it is checked in restricted mode before any random number is
#' drawn. Its unrandomized placement P0 is the v x s matrix with check index
#' h at cell (contraction[h, j], j) and 0 on the test cells.
#'
#' Four draws are taken from the active RNG, always in this order, because
#' replay depends on it:
#' 1. `row_order <- sample.int(v)`: P1 = P0[row_order, column_order], so new
#'    mathematical row i holds old row `row_order[i]`;
#' 2. `column_order <- sample.int(s)`: new mathematical column j holds old
#'    column `column_order[j]` (applied together with step 1);
#' 3. `label_order <- sample.int(k)`: one global relabelling of the checks,
#'    check index h of P1 becomes `label_order[h]`;
#' 4. `test_order <- sample.int(n_tests)`: the t-th test cell of the
#'    randomized placement, in column-major (mathematical) order, receives
#'    test `test_order[t]`, an index into the test labels.
#'
#' Whole rows, whole columns and whole check labels are permuted; check
#' occurrences are never shuffled individually, so no objective changes (M1
#' derivation 7). The randomized contraction satisfies
#' `L[label_order[h], j] == match(contraction[h, column_order[j]],
#' row_order)`.
#'
#' @param contraction Restricted-mode k x s contraction (the search output).
#' @param v,s,k Mathematical rows, columns and checks; must match the
#'   contraction.
#' @param n_tests Number of tests, `(v - k) * s`.
#' @return A list with `check_index`, the randomized v x s integer placement
#'   (check index 1..k, 0 on test cells), `test_order`, and the three
#'   permutations `row_order`, `column_order` and `label_order`.
#' @noRd
arc_randomize <- function(contraction, v, s, k, n_tests) {
  checked <- arc_check_contraction(contraction, v, "restricted")
  if (!arc_is_whole_number(s) || !arc_is_whole_number(k) ||
      !arc_is_whole_number(n_tests) || checked$s != s || checked$k != k ||
      n_tests != (checked$v - checked$k) * checked$s) {
    fieldhub_abort("arc_randomize() needs s, k and n_tests = (v - k)s that ",
                   "match the contraction.",
                   class = c("fieldhub_arc_internal_error",
                             "fieldhub_internal_error"))
  }
  v <- checked$v
  s <- checked$s
  k <- checked$k
  start <- matrix(0L, nrow = v, ncol = s)
  start[cbind(as.integer(contraction), rep(seq_len(s), each = k))] <-
    rep(seq_len(k), times = s)

  # The draw order below is part of the replay contract; do not reorder.
  row_order <- sample.int(v)
  column_order <- sample.int(s)
  label_order <- sample.int(k)
  test_order <- sample.int(as.integer(n_tests))

  placed <- start[row_order, column_order, drop = FALSE]
  on_check <- placed > 0L
  placed[on_check] <- label_order[placed[on_check]]
  list(check_index = placed, test_order = test_order, row_order = row_order,
       column_order = column_order, label_order = label_order)
}

#' Assemble the field book of one location
#'
#' M3 specification sections 3.2 and 3.6. Columns, in the order of the
#' other spatial designs: `ID`, `EXPT`, `LOCATION`, `YEAR`, `PLOT`, `ROW`,
#' `COLUMN`, `CHECKS`, `ENTRY`, `TREATMENT`, plus `CHECK_DIRECTION`.
#' - Rows follow the planting path of `planting_path()`, from row 1,
#'   column 1; `PLOT` is `plot_start` plus 0, 1, ... along that path (double,
#'   as in the spatial family) and `ID` is the integer row number.
#' - `ROW` and `COLUMN` are integer physical coordinates, from the
#'   mathematical placement through the `arc_orient()` map.
#' - Check index h carries label `checks[h]` and the h-th entry identifier;
#'   test i carries `tests[i]` and identifier `k + i`. The t-th test cell in
#'   column-major mathematical order holds test `placement$test_order[t]`
#'   (the order `arc_randomize()` documents).
#' - `ENTRY` (integer): `entry_ids`, checks first and then tests, or
#'   `1..(k + n)` when it is NULL. `CHECKS` (integer): the check's `ENTRY` on
#'   check plots and 0 on test plots.
#' - `TREATMENT` (character): the labels, copied unchanged.
#' - `CHECK_DIRECTION` (character): constant.
#' - `EXPT` and `LOCATION` are recorded as given; `YEAR` is character, as in
#'   the spatial family.
#'
#' @param placement The result of `arc_randomize()`, or a list with the same
#'   `check_index` and `test_order` elements.
#' @param orientation The result of `arc_orient()`.
#' @param checks Character vector of the k check labels, in input order.
#' @param tests Character vector of the n test labels, in input order.
#' @param entry_ids NULL, or k + n distinct positive whole numbers: the
#'   check identifiers followed by the test identifiers (an uploaded `ENTRY`
#'   column).
#' @param location Location label, recorded in `LOCATION`.
#' @param expt Experiment name, recorded in `EXPT`.
#' @param year Year, recorded in `YEAR` as character.
#' @param plot_start First plot number, a whole number.
#' @param planter `"serpentine"` or `"cartesian"`.
#' @return A data frame with one row per plot.
#' @noRd
arc_field_book <- function(placement, orientation, checks, tests,
                           entry_ids = NULL, location, expt, year, plot_start,
                           planter) {
  internal <- c("fieldhub_arc_internal_error", "fieldhub_internal_error")
  check_index <- placement$check_index
  test_order <- placement$test_order
  k <- length(checks)
  n_tests <- length(tests)
  labels <- c(checks, tests)
  if (!is.matrix(check_index) ||
      !identical(dim(check_index), c(orientation$v, orientation$s)) ||
      !all(check_index %in% 0:k) || sum(check_index == 0L) != n_tests ||
      !identical(sort(as.integer(test_order)), seq_len(n_tests))) {
    fieldhub_abort("The placement does not match the orientation, the checks ",
                   "and the tests.", class = internal)
  }
  if (!is.character(labels) || anyNA(labels) || anyDuplicated(labels) > 0L) {
    fieldhub_abort("Check and test labels must be distinct character values.",
                   class = internal)
  }
  if (is.null(entry_ids)) {
    entry_ids <- seq_len(k + n_tests)
  } else if (!is.numeric(entry_ids) || length(entry_ids) != k + n_tests ||
             anyNA(entry_ids) || any(!is.finite(entry_ids)) ||
             any(entry_ids != round(entry_ids)) || any(entry_ids < 1) ||
             any(entry_ids > .Machine$integer.max) ||
             anyDuplicated(entry_ids) > 0L) {
    fieldhub_abort("entry_ids must be ", k + n_tests, " distinct positive ",
                   "whole numbers.", class = internal)
  }
  entry_ids <- as.integer(entry_ids)
  if (length(location) != 1L || length(expt) != 1L || length(year) != 1L) {
    fieldhub_abort("location, expt and year must be single values.",
                   class = internal)
  }
  validate_plot_starts(plot_start)
  if (length(plot_start) != 1L) {
    fieldhub_abort("plot_start must be a single whole number.",
                   class = internal)
  }
  validate_planter(planter)

  # Item of every mathematical cell: check index h, or k + i for test i
  item <- check_index
  item[check_index == 0L] <- k + as.integer(test_order)
  physical <- arc_physical_matrix(item, orientation)
  path <- planting_path(orientation$nrows, orientation$ncols, planter)
  plot_item <- physical[path]
  n <- length(plot_item)
  entry <- entry_ids[plot_item]
  data.frame(
    ID = seq_len(n),
    EXPT = rep(expt, n),
    LOCATION = rep(location, n),
    YEAR = rep(as.character(year), n),
    PLOT = as.numeric(plot_start) + (seq_len(n) - 1),
    ROW = unname(path[, "ROW"]),
    COLUMN = unname(path[, "COLUMN"]),
    CHECKS = ifelse(plot_item <= k, entry, 0L),
    ENTRY = entry,
    TREATMENT = unname(labels[plot_item]),
    CHECK_DIRECTION = rep(orientation$check_direction, n),
    stringsAsFactors = FALSE
  )
}

#' Recover the contraction of a placement from field-book coordinates
#'
#' M3 specification section 3.5: the returned `contraction` describes the
#' final randomized placement. Entry (h, j) is the mathematical row (the
#' cross-line) that holds check h in mathematical column j (the j-th line
#' along `check_direction`): physical column j and its row for `"columns"`,
#' physical row j and its column for `"rows"`. Rows follow the checks'
#' `ENTRY` identifiers in `check_ids` order, which is the `checks` input
#' order; when `check_ids` is NULL they are taken in increasing order, which
#' equals the input order for the default identifiers `1..k` and for the
#' standard upload template.
#'
#' Only `ROW`, `COLUMN` and `CHECKS` are read, plus `CHECK_DIRECTION` when
#' present (it must equal `check_direction`). The field book must be a
#' complete grid with one plot per cell, hold exactly k checks, and place
#' every check exactly once in every line; otherwise a
#' `fieldhub_arc_field_book_error` is raised. Restricted-mode rules are not
#' checked here: pass the result to `arc_check_contraction()`.
#'
#' @param fb A field book.
#' @param k Number of checks.
#' @param check_direction `"columns"` or `"rows"`.
#' @param check_ids NULL, or the k check identifiers in input order.
#' @return The k x s integer contraction.
#' @noRd
arc_contraction_from_field_book <- function(fb, k, check_direction,
                                            check_ids = NULL) {
  arc_check_direction(check_direction)
  error_class <- c("fieldhub_arc_field_book_error", "fieldhub_input_error")
  if (!is.data.frame(fb) || !all(c("ROW", "COLUMN", "CHECKS") %in% names(fb))) {
    fieldhub_abort("The field book needs the columns ROW, COLUMN and CHECKS.",
                   class = error_class)
  }
  whole <- function(x) {
    is.numeric(x) && !anyNA(x) && all(is.finite(x)) && all(x == round(x))
  }
  row <- fb$ROW
  column <- fb$COLUMN
  check <- fb$CHECKS
  if (!whole(row) || !whole(column) || !whole(check) || length(row) == 0L ||
      any(row < 1) || any(column < 1) || any(check < 0)) {
    fieldhub_abort("ROW and COLUMN must be positive whole numbers and CHECKS ",
                   "non-negative whole numbers, with no missing values.",
                   class = error_class)
  }
  if ("CHECK_DIRECTION" %in% names(fb) &&
      !isTRUE(all(fb$CHECK_DIRECTION == check_direction))) {
    fieldhub_abort("The field book's CHECK_DIRECTION differs from ",
                   "check_direction = \"", check_direction, "\".",
                   class = error_class)
  }
  if (length(row) != max(row) * max(column) ||
      anyDuplicated(cbind(row, column)) > 0L) {
    fieldhub_abort("The field book must hold one plot in every cell of its ",
                   "ROW x COLUMN grid.", class = error_class)
  }
  if (!arc_is_whole_number(k)) {
    fieldhub_abort("k must be a single whole number.", class = error_class)
  }
  k <- as.integer(k)
  on_check <- check != 0
  present <- unique(check[on_check])
  ids <- if (is.null(check_ids)) sort(present) else check_ids
  if (length(ids) != k || length(present) != k || anyDuplicated(ids) > 0L ||
      !all(present %in% ids)) {
    fieldhub_abort("The field book must hold exactly k = ", k, " checks",
                   if (!is.null(check_ids)) ", the ones in check_ids", ".",
                   class = error_class)
  }
  math <- arc_math_coordinates(check_direction, row, column)
  s <- as.integer(max(math$column))
  h <- match(check[on_check], ids)
  i <- as.integer(math$row[on_check])
  j <- as.integer(math$column[on_check])
  counts <- matrix(tabulate(h + (j - 1L) * k, nbins = k * s), k, s)
  if (any(counts != 1L)) {
    lines <- if (identical(check_direction, "columns")) "column" else "row"
    fieldhub_abort("Every check must appear exactly once in every field ",
                   lines, " (check_direction = \"", check_direction, "\").",
                   class = error_class, data = list(counts = counts))
  }
  contraction <- matrix(0L, nrow = k, ncol = s)
  contraction[cbind(h, j)] <- i
  contraction
}

#' The deterministic content of an augmented row-column result
#'
#' M3 specification section 3.7: the whole result except the `timing`
#' element and `metadata$package_version`. Exact replay means `identical()`
#' deterministic views; this one definition serves the replay tests, golden
#' snapshots and the reproduction wording.
#' @param x A result list.
#' @return `x` without those two elements, with its class kept.
#' @noRd
arc_deterministic_view <- function(x) {
  x[["timing"]] <- NULL
  if (is.list(x[["metadata"]])) {
    x[["metadata"]][["package_version"]] <- NULL
  }
  x
}
