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

#' Classify connectivity and conditioning from the spectrum of M-tilde
#'
#' Specification section 5. With `lambda` the symmetric eigenvalues of
#' M-tilde = M + Q0 Q0' (any order), `lambda_min_rel` = min(lambda) /
#' max(lambda). The design is connected when `lambda_min_rel >= tau_rank`
#' (the boundary value counts as connected) and near-singular when it is
#' connected and `lambda_min_rel < tau_warn` (the boundary value is not
#' near-singular). The function is pure: it emits no warning and does not
#' apply the integer df check, which callers run first.
#'
#' @param lambda Numeric vector of eigenvalues, finite, with a positive
#'   maximum.
#' @param tau_rank Connectivity threshold on `lambda_min_rel`.
#' @param tau_warn Conditioning-warning threshold on `lambda_min_rel`.
#' @return A list with logical `connected`, numeric `lambda_min_rel`, and
#'   logical `near_singular`.
#' @noRd
arc_classify_spectrum <- function(lambda, tau_rank = 1e-8, tau_warn = 1e-6) {
  if (!is.numeric(lambda) || length(lambda) == 0L ||
      !all(is.finite(lambda)) || max(lambda) <= 0) {
    fieldhub_abort("The spectrum must be a non-empty vector of finite ",
                   "eigenvalues with a positive maximum.",
                   class = c("fieldhub_arc_input_error",
                             "fieldhub_input_error"))
  }
  lambda_min_rel <- min(lambda) / max(lambda)
  connected <- lambda_min_rel >= tau_rank
  list(connected = connected,
       lambda_min_rel = lambda_min_rel,
       near_singular = connected && lambda_min_rel < tau_warn)
}

#' Residual df of a connected design, ks - k - v - s + 2, as an integer
#'
#' Specification section 4.3, decision 8. Computed with integer arithmetic
#' before any matrix work; a negative value means no connected design exists.
#' @noRd
arc_df_formula <- function(v, s, k) {
  v <- as.integer(v)
  s <- as.integer(s)
  k <- as.integer(k)
  k * s - k - v - s + 2L
}

#' Contraction-level matrices of the row-column information
#'
#' Specification sections 1, 5, and 7.3 step 1. The contraction must already
#' have passed `arc_check_contraction()`; nothing is checked here.
#' M = [[diag(r) - W/s, M12], [M12', k (I_s - J_s/s)]] with
#' M12 = N_C - r 1_s'/s, Q0 holds the normalized row-constant and
#' column-constant vectors, and M_tilde = M + Q0 Q0'.
#'
#' @inheritParams arc_check_contraction
#' @return A list with `nc` (v x s binary column incidence), `nr` (v x k row
#'   incidence counts), `w` = nr nr', integer `r`, `M`, `Q0`, and `M_tilde`.
#' @noRd
arc_contraction_matrices <- function(contraction, v) {
  k <- nrow(contraction)
  s <- ncol(contraction)
  v <- as.integer(v)
  labels <- as.integer(contraction)

  nc <- matrix(0, nrow = v, ncol = s)
  nc[cbind(labels, rep(seq_len(s), each = k))] <- 1
  nr <- vapply(seq_len(k), function(i) {
    as.numeric(tabulate(contraction[i, ], nbins = v))
  }, numeric(v))
  nr <- matrix(nr, nrow = v, ncol = k)
  r <- tabulate(labels, nbins = v)
  w <- tcrossprod(nr)

  m12 <- nc - outer(r, rep(1, s)) / s
  column_block <- k * (diag(s) - matrix(1 / s, s, s))
  m <- rbind(cbind(diag(r, nrow = v) - w / s, m12),
             cbind(t(m12), column_block))
  m <- (m + t(m)) / 2
  q0 <- cbind(c(rep(1, v), rep(0, s)) / sqrt(v),
              c(rep(0, v), rep(1, s)) / sqrt(s))
  m_tilde <- m + tcrossprod(q0)
  m_tilde <- (m_tilde + t(m_tilde)) / 2
  list(nc = nc, nr = nr, w = w, r = r, M = m, Q0 = q0, M_tilde = m_tilde)
}

#' Rearrange a column-binary contraction within its columns to be row-binary
#'
#' Specification section 4.2, recoloring guarantee. The contraction is read
#' as a bipartite multigraph between columns and pseudo-treatments, each
#' column of degree k and pseudo-treatment h of degree r_h. When
#' max(r) <= k, Konig's theorem gives a proper k-edge-coloring, found here by
#' the alternating-path (Kempe chain) method; contraction row i then holds
#' the pseudo-treatment colored i in each column. Edges are coloured in
#' column-major order with the lowest free colours, so the result is
#' deterministic and draws no random numbers. Recoloring keeps every
#' column's set and r; it does not balance an unbalanced r.
#'
#' @inheritParams arc_check_contraction
#' @return An integer k x s row-binary contraction.
#' @noRd
arc_recolor_rows <- function(contraction, v) {
  checked <- arc_check_contraction(contraction, v, mode = "column_binary")
  k <- checked$k
  r <- checked$r
  if (max(r) > k) {
    fieldhub_abort("Recoloring needs every pseudo-treatment to occur at most ",
                   "k = ", k, " times; the largest replication is ", max(r),
                   ", so no row-binary rearrangement exists.",
                   class = c("fieldhub_arc_recolor_error",
                             "fieldhub_input_error"),
                   data = list(r = r, k = k))
  }
  arc_edge_color(contraction, checked$v, k)
}

#' Proper k-edge-coloring of the column x pseudo-treatment multigraph
#'
#' Alternating-path method for bipartite graphs of maximum degree k. Assumes
#' column-binary input with max(r) <= k (see `arc_recolor_rows()`).
#' `column_color[j, c]` is the pseudo-treatment of colour c in column j and
#' `label_color[h, c]` the column where pseudo-treatment h has colour c
#' (0 when free).
#' @noRd
arc_edge_color <- function(contraction, v, k) {
  s <- ncol(contraction)
  column_color <- matrix(0L, nrow = s, ncol = k)
  label_color <- matrix(0L, nrow = v, ncol = k)

  for (j in seq_len(s)) {
    for (h in as.integer(contraction[, j])) {
      a <- which(column_color[j, ] == 0L)[1L]
      b <- which(label_color[h, ] == 0L)[1L]
      if (label_color[h, a] != 0L) {
        # Colour a is free at column j but used at h, and b is free at h.
        # Swap a and b along the a/b path from h. Columns on it are entered
        # by a-edges, so it cannot reach column j, where a is free.
        path <- arc_alternating_path(h, a, b, column_color, label_color)
        column_color[path[, c(1L, 3L), drop = FALSE]] <- 0L
        label_color[path[, c(2L, 3L), drop = FALSE]] <- 0L
        swapped <- ifelse(path[, 3L] == a, b, a)
        column_color[cbind(path[, 1L], swapped)] <- path[, 2L]
        label_color[cbind(path[, 2L], swapped)] <- path[, 1L]
      }
      column_color[j, a] <- h
      label_color[h, a] <- j
    }
  }
  t(column_color)
}

#' Edges of the maximal a/b alternating path that starts at label h with a
#' @return Integer matrix with one row (column, label, colour) per edge.
#' @noRd
arc_alternating_path <- function(h, a, b, column_color, label_color) {
  edges <- list()
  node <- h
  at_label <- TRUE
  color <- a
  repeat {
    if (at_label) {
      next_node <- label_color[node, color]
      if (next_node == 0L) break
      edges[[length(edges) + 1L]] <- c(next_node, node, color)
    } else {
      next_node <- column_color[node, color]
      if (next_node == 0L) break
      edges[[length(edges) + 1L]] <- c(node, next_node, color)
    }
    node <- next_node
    at_label <- !at_label
    color <- if (color == a) b else a
  }
  matrix(as.integer(unlist(edges)), ncol = 3L, byrow = TRUE)
}

#' Validate an augmented row-column contraction (public contract)
#'
#' Specification sections 4.1, 4.3, and 4.4, in order: the structural checks
#' of `arc_check_contraction()`; the integer df formula, where a negative
#' value means no connected design exists and no matrix work is done;
#' connectivity from the spectrum of M-tilde (section 5); the public floor of
#' one residual df; then the warnings. Every condition raised or signalled
#' here carries `df_formula`, `lambda_min_rel` (NA when not computed), `v`,
#' `s`, and `k` as data fields.
#'
#' @inheritParams arc_check_contraction
#' @param mode "restricted" (the default) or "column_binary".
#' @return A list with integer `v`, `s`, `k`, the replications `r`,
#'   `df_formula`, `connected` (always TRUE), `lambda_min_rel`, and `mode`.
#' @noRd
arc_validate_contraction <- function(contraction, v, mode = "restricted") {
  checked <- arc_check_contraction(contraction, v, mode)
  v <- checked$v
  s <- checked$s
  k <- checked$k
  df <- arc_df_formula(v, s, k)
  data <- list(df_formula = df, lambda_min_rel = NA_real_, v = v, s = s,
               k = k)

  if (df < 0L) {
    fieldhub_abort("No connected design exists for v = ", v, " rows, s = ", s,
                   " columns, and k = ", k, " checks: a connected design ",
                   "would have ks - k - v - s + 2 = ", df, " residual degrees ",
                   "of freedom.",
                   class = c("fieldhub_arc_infeasible_error",
                             "fieldhub_dimension_error"),
                   data = data)
  }

  matrices <- arc_contraction_matrices(contraction, v)
  lambda <- eigen(matrices$M_tilde, symmetric = TRUE,
                  only.values = TRUE)$values
  spectrum <- arc_classify_spectrum(lambda)
  data$lambda_min_rel <- spectrum$lambda_min_rel
  if (!spectrum$connected) {
    fieldhub_abort("The design is not connected: some treatment comparisons ",
                   "cannot be estimated from these row and column positions ",
                   "of the checks.",
                   class = c("fieldhub_arc_disconnected_error",
                             "fieldhub_input_error"),
                   data = data)
  }
  if (df < 1L) {
    fieldhub_abort("The design has ", df, " residual degrees of freedom, so ",
                   "the residual variance cannot be estimated; at least 1 is ",
                   "required.",
                   class = c("fieldhub_arc_df_error",
                             "fieldhub_dimension_error"),
                   data = data)
  }

  if (df < 10L) {
    relative_sd <- round(100 * sqrt(2 / df))
    fieldhub_warn("The design has only ", df, " residual degrees of freedom. ",
                  "Under independent normal errors, the estimate of the ",
                  "residual variance then has a relative standard deviation ",
                  "of about ", relative_sd, "%. Only adding a check or ",
                  "changing the number of rows or columns raises the ",
                  "residual degrees of freedom.",
                  class = "fieldhub_arc_low_df_warning", data = data)
  }
  if (spectrum$near_singular) {
    fieldhub_warn("The design is numerically near-singular: lambda_min_rel ",
                  "= ", format(spectrum$lambda_min_rel, digits = 3),
                  " is below 1e-6, so its computed variances may lose ",
                  "accuracy. This is a numerical-conditioning warning.",
                  class = "fieldhub_arc_near_singular_warning", data = data)
  }

  list(v = v, s = s, k = k, r = checked$r, df_formula = df,
       connected = TRUE, lambda_min_rel = spectrum$lambda_min_rel,
       mode = mode)
}
