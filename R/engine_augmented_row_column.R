#' Whether a value is a single whole number
#' @noRd
arc_is_whole_number <- function(x) {
  is.numeric(x) && length(x) == 1L && is.null(dim(x)) && is.finite(x) &&
    x == round(x)
}

#' Structural checks for an augmented row-column contraction
#'
#' Applies structural rules 1-7 of the M1 specification (section 4.2) in
#' order and raises a classed condition, specific class first, at the first
#' failure. It raises only for malformed input: connectivity and degrees of
#' freedom are decided elsewhere. The contraction is k x s in mathematical
#' orientation; entry l at cell (i, j) places check i in field row l of
#' column j. Restricted mode forbids row repeats and requires balanced
#' replication; column-binary mode requires only that every pseudo-treatment
#' occurs. Both modes require s <= v.
#'
#' @param contraction Integer-valued k x s matrix of pseudo-treatments 1..v.
#' @param v Number of pseudo-treatments (field rows), a whole number.
#' @param mode "restricted" or "column_binary"; there is no default.
#' @return Invisibly, a list with integer `k`, `s`, `v`, the replications `r`
#'   (length v), and `mode`.
#' @noRd
arc_check_contraction <- function(contraction, v, mode) {
  input_class <- c("fieldhub_arc_input_error", "fieldhub_input_error")
  dimension_class <- c("fieldhub_arc_dimension_error",
                       "fieldhub_dimension_error")

  # Rule 1: types. Doubles are accepted when every value is integral.
  if (!is.matrix(contraction) || !is.numeric(contraction) ||
      anyNA(contraction) || !all(is.finite(contraction)) ||
      any(contraction != round(contraction))) {
    fieldhub_abort("The contraction must be a matrix of whole numbers with no ",
                   "missing values.", class = input_class)
  }
  if (!arc_is_whole_number(v)) {
    fieldhub_abort("The number of pseudo-treatments v must be a single whole ",
                   "number.", class = input_class)
  }
  if (missing(mode) || !is.character(mode) || length(mode) != 1L ||
      is.na(mode) || !mode %in% c("restricted", "column_binary")) {
    fieldhub_abort("The contraction mode must be given as \"restricted\" or ",
                   "\"column_binary\".", class = input_class)
  }
  k <- nrow(contraction)
  s <- ncol(contraction)
  v <- as.integer(v)

  # Rule 2: at least two checks, fewer checks than field rows, two columns.
  if (k < 2L || k >= v || s < 2L) {
    fieldhub_abort("The contraction needs 2 <= k < v and s >= 2; got k = ", k,
                   ", s = ", s, ", v = ", v, ".", class = dimension_class,
                   data = list(k = k, s = s, v = v))
  }
  # Rule 3: no more columns than field rows (orientation decision).
  if (s > v) {
    fieldhub_abort("The contraction needs s <= v; got s = ", s, ", v = ", v,
                   ".", class = dimension_class,
                   data = list(k = k, s = s, v = v))
  }
  # Rule 4: entries are pseudo-treatment labels.
  if (any(contraction < 1 | contraction > v)) {
    fieldhub_abort("Contraction entries must lie in 1..", v, ".",
                   class = input_class)
  }
  storage.mode(contraction) <- "integer"

  # Rule 5: a repeat within a column puts two checks in one field cell.
  collided <- which(apply(contraction, 2L, anyDuplicated) > 0L)
  if (length(collided) > 0L) {
    column <- collided[1L]
    fieldhub_abort("Contraction column ", column, " repeats a pseudo-",
                   "treatment, so two checks would share a field cell.",
                   class = c("fieldhub_arc_collision_error",
                             "fieldhub_input_error"),
                   data = list(column = column))
  }
  # Rule 6: restricted mode keeps each check out of any field row twice.
  if (identical(mode, "restricted")) {
    repeated <- which(apply(contraction, 1L, anyDuplicated) > 0L)
    if (length(repeated) > 0L) {
      row <- repeated[1L]
      fieldhub_abort("Contraction row ", row, " repeats a pseudo-treatment, ",
                     "which restricted mode does not allow.",
                     class = c("fieldhub_arc_row_repeat_error",
                               "fieldhub_input_error"),
                     data = list(row = row))
    }
  }
  # Rule 7: replication. r_h is also the number of checks in field row h.
  r <- tabulate(contraction, nbins = v)
  replicated <- all(r >= 1L) && sum(r) == k * s
  if (identical(mode, "restricted")) {
    balanced <- c(floor(k * s / v), ceiling(k * s / v))
    replicated <- replicated && all(r %in% balanced)
  }
  if (!replicated) {
    rule <- if (identical(mode, "restricted")) {
      "every pseudo-treatment to occur floor(ks/v) or ceiling(ks/v) times"
    } else {
      "every pseudo-treatment to occur at least once"
    }
    fieldhub_abort("The contraction replication is invalid: ", mode,
                   " mode requires ", rule, ".",
                   class = c("fieldhub_arc_replication_error",
                             "fieldhub_input_error"),
                   data = list(r = r))
  }
  invisible(list(k = k, s = s, v = v, r = r, mode = mode))
}

#' Check that a label vector is complete, unique, and of the expected length
#' @noRd
arc_check_labels <- function(labels, expected, what) {
  if (!is.character(labels) || !is.null(dim(labels)) || anyNA(labels) ||
      length(labels) != expected || anyDuplicated(labels) > 0L) {
    fieldhub_abort("The ", what, " labels must be ", expected,
                   " unique, non-missing character values.",
                   class = c("fieldhub_arc_input_error",
                             "fieldhub_input_error"),
                   data = list(expected = expected))
  }
  invisible(labels)
}

#' Expand a contraction into the augmented row-column field layout
#'
#' Pseudo-treatment l at contraction cell (i, j) places check i at field cell
#' (l, j) (specification section 3). The T = (v - k)s remaining cells hold
#' the tests, coded 1..T in column-major order of the empty cells; check i is
#' coded T + i. The structural checks run first, so collisions are rejected
#' before any cell is written. The public validator is never called.
#'
#' Labels are optional, but check and test labels are supplied together:
#' `check_labels` has k unique values, `test_labels` has T unique values, and
#' no label appears in both. Labels are copied without any change, so they
#' round-trip byte-for-byte.
#'
#' @inheritParams arc_check_contraction
#' @param check_labels NULL, or a character vector of k unique check labels.
#' @param test_labels NULL, or a character vector of T unique test labels.
#' @return A list with `codes`, a v x s integer matrix, and `labels`, a v x s
#'   character matrix of the labels of `codes`, or NULL without labels.
#' @noRd
arc_expand <- function(contraction, v, mode, check_labels = NULL,
                       test_labels = NULL) {
  checked <- arc_check_contraction(contraction, v, mode)
  k <- checked$k
  s <- checked$s
  v <- checked$v
  n_tests <- (v - k) * s

  if (is.null(check_labels) != is.null(test_labels)) {
    fieldhub_abort("Supply both check and test labels, or neither.",
                   class = c("fieldhub_arc_input_error",
                             "fieldhub_input_error"))
  }
  with_labels <- !is.null(check_labels)
  if (with_labels) {
    arc_check_labels(check_labels, k, "check")
    arc_check_labels(test_labels, n_tests, "test")
    if (any(check_labels %in% test_labels)) {
      fieldhub_abort("A label cannot name both a check and a test.",
                     class = c("fieldhub_arc_input_error",
                               "fieldhub_input_error"))
    }
  }

  # as.vector() is column-major: cell (i, j) of the contraction comes at
  # position i + (j - 1)k.
  codes <- matrix(0L, nrow = v, ncol = s)
  codes[cbind(as.integer(contraction), rep(seq_len(s), each = k))] <-
    n_tests + rep(seq_len(k), times = s)
  codes[codes == 0L] <- seq_len(n_tests)

  labels <- NULL
  if (with_labels) {
    labels <- matrix(c(unname(test_labels), unname(check_labels))[codes],
                     nrow = v, ncol = s)
  }
  list(codes = codes, labels = labels)
}
