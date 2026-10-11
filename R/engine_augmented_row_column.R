#' Whether a value is a single whole number
#' @noRd
arc_is_whole_number <- function(x) {
  is.numeric(x) && length(x) == 1L && is.null(dim(x)) && is.finite(x) &&
    x == round(x) && abs(x) <= .Machine$integer.max
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

#' Validate a field layout for the full-layout evaluator
#'
#' Specification section 7.1. `codes` is a field-rows x field-columns matrix
#' of whole-number treatment codes with no missing values. The checks are
#' `check_codes` when supplied, or else the codes occurring more than once:
#' with `check_codes = NULL` a duplicated code is by definition a check, so a
#' duplicated test code is detected only when `check_codes` is supplied.
#' Every other code (a test) must occur exactly once, and there must be at
#' least two checks. The size cap is applied right after the type check,
#' before anything of the size of the layout is tabulated or allocated.
#'
#' @inheritParams arc_evaluate_layout
#' @return A list with the plot codes in column-major order (`plots`), the
#'   sorted distinct codes (`treatments`), their replications (`u`), and the
#'   logical `is_check` over `treatments`.
#' @noRd
arc_check_layout <- function(codes, check_codes) {
  layout_class <- c("fieldhub_arc_layout_error", "fieldhub_input_error")
  if (!is.matrix(codes) || !is.numeric(codes) || length(codes) == 0L ||
      anyNA(codes) || !all(is.finite(codes)) ||
      any(codes != round(codes)) ||
      any(abs(codes) > .Machine$integer.max)) {
    fieldhub_abort("The layout must be a matrix of whole-number treatment ",
                   "codes within the integer range, with no missing values.",
                   class = layout_class)
  }
  if (!is.null(check_codes) &&
      (!is.numeric(check_codes) || length(check_codes) == 0L ||
       anyNA(check_codes) || !all(is.finite(check_codes)) ||
       any(check_codes != round(check_codes)) ||
       any(abs(check_codes) > .Machine$integer.max))) {
    fieldhub_abort("The check codes must be whole numbers with no missing ",
                   "values.", class = layout_class)
  }

  n <- length(codes)
  cap <- getOption("FielDHub.arc_oracle_max_plots", 2500)
  if (n > cap) {
    fieldhub_abort("The layout has ", n, " plots, more than the full-layout ",
                   "evaluator's limit of ", cap, " plots.",
                   class = c("fieldhub_arc_size_error",
                             "fieldhub_dimension_error"),
                   data = list(n = n, cap = cap))
  }

  plots <- as.vector(codes)
  treatments <- sort(unique(plots))
  u <- tabulate(match(plots, treatments), nbins = length(treatments))
  if (is.null(check_codes)) {
    is_check <- u > 1L
  } else {
    check_codes <- unique(as.vector(check_codes))
    if (!all(check_codes %in% treatments)) {
      fieldhub_abort("Every check code must occur in the layout.",
                     class = layout_class)
    }
    is_check <- treatments %in% check_codes
    if (any(u[!is_check] != 1L)) {
      fieldhub_abort("Every test code must occur exactly once; code ",
                     treatments[!is_check & u != 1L][1L], " occurs ",
                     u[!is_check & u != 1L][1L], " times.",
                     class = layout_class)
    }
  }
  if (sum(is_check) < 2L) {
    fieldhub_abort("The layout needs at least two checks; found ",
                   sum(is_check), ".", class = layout_class)
  }
  list(plots = plots, treatments = treatments, u = u, is_check = is_check)
}

#' Moore-Penrose inverse of a symmetric positive semidefinite matrix
#'
#' From a symmetric eigen-decomposition: eigenvalues at or below
#' `tol` times the largest are treated as zero, the others inverted.
#' @noRd
arc_pinv_symmetric <- function(symmetric, tol = 1e-8) {
  decomposition <- eigen(symmetric, symmetric = TRUE)
  values <- decomposition$values
  keep <- values > tol * max(values)
  vectors <- decomposition$vectors[, keep, drop = FALSE]
  vectors %*% (t(vectors) / values[keep])
}

#' Mean and maximum pairwise variance over two treatment classes
#'
#' `pairwise` is the symmetric v* x v* matrix of Var(tau_i - tau_j), with a
#' zero diagonal. Within one class the mean is over unordered distinct pairs,
#' which is the block sum over the m(m - 1) ordered pairs; between classes it
#' is over every pair. NA when the class has no pair.
#' @noRd
arc_pair_summary <- function(pairwise, first, second) {
  block <- pairwise[first, second, drop = FALSE]
  pairs <- if (identical(first, second)) {
    length(first) * (length(first) - 1L)
  } else {
    length(first) * length(second)
  }
  if (pairs == 0L) {
    return(c(mean = NA_real_, max = NA_real_))
  }
  c(mean = sum(block) / pairs, max = max(block))
}

#' Full-layout augmented row-column evaluator
#'
#' Specification section 7.1. Evaluates any field layout under the model
#' with fixed treatment, row, and column effects (section 2), without
#' requiring the contraction structure, so transposed and Bailey-Haines
#' square layouts are valid. Variances are standardized (sigma^2 = 1).
#'
#' With X the n x v* plot-level treatment indicators (treatments in
#' increasing code order) and Z = [1, Z_R, Z_C], the information matrix is
#' C = X'(I - P_Z)X with P_Z = Z (Z'Z)^+ Z', computed as
#' X'X - (X'Z)(Z'Z)^+(Z'X) so that no n x n matrix is formed. Connectivity:
#' exactly one eigenvalue of A* = diag(u)^(-1/2) C diag(u)^(-1/2) below
#' tau_rank = 1e-8 times the largest; `rank_C` is the number of the others.
#' `residual_df_rank` = n - rank([X Z]) uses rank([X Z]) = rank(Z) + rank(C)
#' (C is the Gram matrix of the columns of X after projecting out Z), with
#' rank(Z) = rows + columns - 1 on a complete field grid. It therefore reuses
#' the eigenvalues that decide connectivity, so the two never disagree.
#' Neither cross-product eigenvalues nor their square roots are used: the
#' first drop genuine small singular values, and the square roots lift
#' rounding noise of the zero eigenvalues to about 1e-8 relative, so the
#' rank decision would depend on the BLAS (section 7.1).
#' Objectives (section 6) come from the Moore-Penrose inverse of C and are
#' returned only for a connected layout (section 5, refusal rule).
#'
#' @param codes Matrix of whole-number treatment codes, field rows x field
#'   columns, at most `getOption("FielDHub.arc_oracle_max_plots", 2500)`
#'   plots.
#' @param check_codes NULL to take the codes occurring more than once as the
#'   checks, or the check codes, each of which must occur in `codes`.
#' @return A list with `connected`, `rank_C`, and `residual_df_rank`; when
#'   connected, also the v* - 1 nontrivial eigenvalues `cefs` of A*, `E_aug`,
#'   the mean pairwise variances `V_tt`, `V_tc`, `V_cc`, and the maxima
#'   `max_tt` and `max_tc`.
#' @noRd
arc_evaluate_layout <- function(codes, check_codes = NULL) {
  tau_rank <- 1e-8
  layout <- arc_check_layout(codes, check_codes)
  n <- length(layout$plots)
  n_treatments <- length(layout$treatments)
  u <- layout$u

  x <- matrix(0, nrow = n, ncol = n_treatments)
  x[cbind(seq_len(n), match(layout$plots, layout$treatments))] <- 1
  z_rows <- matrix(0, nrow = n, ncol = nrow(codes))
  z_rows[cbind(seq_len(n), as.vector(row(codes)))] <- 1
  z_cols <- matrix(0, nrow = n, ncol = ncol(codes))
  z_cols[cbind(seq_len(n), as.vector(col(codes)))] <- 1
  z <- cbind(1, z_rows, z_cols)

  # C = X'X - (X'Z)(Z'Z)^+(Z'X), the projection form of X'(I - P_Z)X
  xz <- crossprod(x, z)
  zz <- crossprod(z)
  information <- diag(u, nrow = n_treatments) -
    xz %*% arc_pinv_symmetric(zz, tau_rank) %*% t(xz)
  information <- (information + t(information)) / 2

  scale <- 1 / sqrt(u)
  scaled <- information * outer(scale, scale)
  lambda <- eigen(scaled, symmetric = TRUE, only.values = TRUE)$values
  # The eigenvalues of A* are canonical efficiency factors: 0 <= C <= X'X =
  # diag(u) in the Loewner order, so they lie in [0, 1]. The tolerance is
  # therefore absolute on that fixed scale, never relative to max(lambda): a
  # zero information matrix has an all-noise spectrum whose maximum is itself
  # rounding error (M1 gate review, P1).
  small <- lambda < tau_rank
  connected <- sum(small) == 1L
  rank_c <- sum(!small)

  # rank([X Z]) = rank(Z) + rank(C); rank(Z) = rows + columns - 1
  residual_df_rank <- n - (nrow(codes) + ncol(codes) - 1L) - rank_c

  result <- list(connected = connected, rank_C = rank_c,
                 residual_df_rank = residual_df_rank)
  if (!connected) {
    return(result)
  }

  # eigen() sorts decreasingly; the trivial eigenvalue (for sqrt(u)) is last
  cefs <- lambda[seq_len(n_treatments - 1L)]
  e_aug <- (n_treatments - 1L) / sum(1 / cefs)

  # C^+ drops the single null direction (the constant vector) of C
  decomposition <- eigen(information, symmetric = TRUE)
  keep <- seq_len(n_treatments - 1L)
  vectors <- decomposition$vectors[, keep, drop = FALSE]
  omega <- vectors %*% (t(vectors) / decomposition$values[keep])
  omega <- (omega + t(omega)) / 2
  variances <- diag(omega)
  pairwise <- outer(variances, variances, `+`) - 2 * omega
  diag(pairwise) <- 0

  tests <- which(!layout$is_check)
  checks <- which(layout$is_check)
  tt <- arc_pair_summary(pairwise, tests, tests)
  tc <- arc_pair_summary(pairwise, tests, checks)
  cc <- arc_pair_summary(pairwise, checks, checks)

  c(result, list(cefs = cefs, E_aug = e_aug,
                 V_tt = unname(tt["mean"]), V_tc = unname(tc["mean"]),
                 V_cc = unname(cc["mean"]), max_tt = unname(tt["max"]),
                 max_tc = unname(tc["max"])))
}

#' Orthonormal basis of the complement of the constant vector
#'
#' The normalized Helmert contrasts: an n x (n - 1) matrix U with U'U = I and
#' U'1_n = 0. Restricting a symmetric matrix that maps 1_n to 0 to this basis
#' removes its trivial eigenvalue by projection.
#' @noRd
arc_contrast_basis <- function(n) {
  helmert <- stats::contr.helmert(n)
  sweep(helmert, 2L, sqrt(colSums(helmert^2)), "/")
}

#' Paper-route augmented row-column evaluator
#'
#' Specification section 7.2 (Piepho and Williams 2026, eqs. 4 and 5, with
#' the corrections of section 8). Supplies E_aug and its supporting
#' quantities only; it computes no variances. The contraction is checked
#' structurally first (raising for malformed input); a negative df_formula
#' returns at once with `lambda_min_rel = NA` and no matrix work, and
#' connectivity is the M-tilde spectrum decision of section 5.
#'
#' When connected, C_contr = diag(r) - W/s - N_C N_C'/k + r r'/(ks) maps 1_v
#' to 0, and S = P_s [I_s - (1/k) M12' (diag(r) - W/s + J_v)^(-1) M12] P_s
#' (corrected eq. 4, M12 = N_C - r 1_s'/s) maps 1_s to 0. Each is restricted
#' to an orthonormal basis of the complement of the constant vector, so the
#' trivial eigenvalue is removed by projection, never by dropping the
#' smallest one; connectivity makes both restrictions positive definite.
#' `Cbar_v` is the harmonic mean of the v - 1 remaining eigenvalues of
#' C_contr divided by mean(r), and `Cbar_s` the harmonic mean of the s - 1
#' remaining eigenvalues of S. `a` = tr C_contr^+ and `b` = tr S^+ / k.
#'
#' @inheritParams arc_check_contraction
#' @return A list with `connected`, the integer `df_formula`, and
#'   `lambda_min_rel` (NA after the negative-df return); when connected, also
#'   `Cbar_v`, `Cbar_s`, `E_aug`, `a`, and `b`.
#' @noRd
arc_evaluate_contraction <- function(contraction, v, mode) {
  checked <- arc_check_contraction(contraction, v, mode)
  v <- checked$v
  s <- checked$s
  k <- checked$k
  df_formula <- arc_df_formula(v, s, k)
  if (df_formula < 0L) {
    return(list(connected = FALSE, df_formula = df_formula,
                lambda_min_rel = NA_real_))
  }

  matrices <- arc_contraction_matrices(contraction, v)
  lambda <- eigen(matrices$M_tilde, symmetric = TRUE,
                  only.values = TRUE)$values
  spectrum <- arc_classify_spectrum(lambda)
  result <- list(connected = spectrum$connected, df_formula = df_formula,
                 lambda_min_rel = spectrum$lambda_min_rel)
  if (!spectrum$connected) {
    return(result)
  }

  r <- matrices$r
  nc <- matrices$nc
  row_block <- diag(r, nrow = v) - matrices$w / s

  # Row quantity: C_contr on the complement of 1_v
  c_contr <- row_block - tcrossprod(nc) / k + tcrossprod(r) / (k * s)
  basis_v <- arc_contrast_basis(v)
  reduced_v <- crossprod(basis_v, c_contr %*% basis_v)
  values_v <- eigen((reduced_v + t(reduced_v)) / 2, symmetric = TRUE,
                    only.values = TRUE)$values
  a <- sum(1 / values_v)
  cbar_v <- ((v - 1) / a) / mean(r)

  # Column quantity: corrected eq. (4) on the complement of 1_s. M12 1_s = 0,
  # so U_s' P_s = U_s' and the projection is carried by the basis.
  m12 <- nc - outer(r, rep(1, s)) / s
  bracket <- diag(s) -
    crossprod(m12, solve(row_block + matrix(1, v, v), m12)) / k
  basis_s <- arc_contrast_basis(s)
  reduced_s <- crossprod(basis_s, bracket %*% basis_s)
  values_s <- eigen((reduced_s + t(reduced_s)) / 2, symmetric = TRUE,
                    only.values = TRUE)$values
  trace_s <- sum(1 / values_s)
  cbar_s <- (s - 1) / trace_s
  b <- trace_s / k

  # Eq. (5)
  v_star <- (v - k) * s + k
  e_aug <- (v_star - 1) /
    (v_star - (v + s) + 1 + (v / k) * ((v - 1) / cbar_v + (s - 1) / cbar_s))

  c(result, list(Cbar_v = cbar_v, Cbar_s = cbar_s, E_aug = e_aug, a = a,
                 b = b))
}

#' Worst-pair block size from the memory budget option
#'
#' Specification section 6.4. Reads
#' `getOption("FielDHub.arc_worst_pair_block_mib", 64)`, the budget in MiB
#' for one double-precision sub-kernel of the worst-pair computation, and
#' returns the largest block size b with b^2 doubles inside that budget (at
#' least 1). Peak memory is a small multiple of the budget because of
#' temporaries.
#'
#' @return A single positive whole number (double).
#' @noRd
arc_worst_pair_block_size <- function() {
  mib <- getOption("FielDHub.arc_worst_pair_block_mib", 64)
  valid <- is.numeric(mib) && length(mib) == 1L && is.null(dim(mib)) &&
    is.finite(mib) && mib > 0
  if (!valid) {
    fieldhub_abort("The option FielDHub.arc_worst_pair_block_mib must be a ",
                   "single finite positive number (MiB).",
                   class = c("fieldhub_arc_input_error",
                             "fieldhub_input_error"),
                   data = list(value = mib))
  }
  max(1, floor(sqrt(mib * 2^20 / 8)))
}

#' Blocked worst-pair maxima over test cells
#'
#' Specification section 6.4. With test cells p = (a_p, b_p), x_p =
#' e_(a_p) + e_(b_p) on the (v + s) scale and the T x T test-cell kernel
#' K = X_T Omega X_T', a distinct test pair has variance
#' 2 + K_pp + K_qq - 2 K_pq and a test-check pair (p, i) has variance
#' 1 + 1/s + K_pp + h_i' Omega h_i - 2 (X_T Omega H_C')_pi. The test cells are
#' cut into consecutive blocks of `block_size`; only the |P| x |Q| sub-kernel
#' of each block pair P <= Q (upper triangle of blocks) is formed, and on a
#' diagonal block only its strict upper triangle is used, so self-pairs are
#' excluded and each unordered pair is seen once. `max_tc` is blocked over
#' test rows. No T x T matrix is formed.
#'
#' @param omega The (v + s) x (v + s) symmetric matrix Omega = M^+.
#' @param a,b Integer vectors of length T >= 2: the row index and the
#'   (v + s)-scale column index of each test cell.
#' @param omega_h_c The (v + s) x k matrix Omega H_C'.
#' @param quad_c Length-k vector of h_i' Omega h_i.
#' @param s Number of columns.
#' @param block_size A positive whole number; values above T act as T.
#' @return A list with `max_tt`, `max_tc`, and `argmax_tt`, the test-cell
#'   indices p < q of a pair attaining `max_tt`.
#' @noRd
arc_worst_pair_maxima <- function(omega, a, b, omega_h_c, quad_c, s,
                                  block_size) {
  n <- length(a)
  block_size <- min(block_size, n)
  quad_t <- omega[cbind(a, a)] + 2 * omega[cbind(a, b)] + omega[cbind(b, b)]
  starts <- seq.int(1L, n, by = block_size)
  max_tt <- -Inf
  argmax_tt <- c(NA_integer_, NA_integer_)
  max_tc <- -Inf
  for (i in starts) {
    p <- seq.int(i, min(i + block_size - 1, n))
    a_p <- a[p]
    b_p <- b[p]
    kernel_tc <- omega_h_c[a_p, , drop = FALSE] +
      omega_h_c[b_p, , drop = FALSE]
    max_tc <- max(max_tc,
                  1 + 1 / s + outer(quad_t[p], quad_c, `+`) - 2 * kernel_tc)
    for (j in starts[starts >= i]) {
      q <- seq.int(j, min(j + block_size - 1, n))
      a_q <- a[q]
      b_q <- b[q]
      # 2 + K_pp + K_qq - 2 K_pq, built in place to limit temporaries
      pairs <- omega[a_p, a_q, drop = FALSE] + omega[a_p, b_q, drop = FALSE] +
        omega[b_p, a_q, drop = FALSE] + omega[b_p, b_q, drop = FALSE]
      pairs <- (2 + quad_t[p]) - 2 * pairs
      pairs <- pairs + rep(quad_t[q], each = length(p))
      if (i == j) {
        if (length(p) < 2L) next
        pairs[lower.tri(pairs, diag = TRUE)] <- -Inf
      }
      w <- which.max(pairs)
      if (pairs[w] > max_tt) {
        max_tt <- pairs[w]
        index <- arrayInd(w, dim(pairs))
        argmax_tt <- c(p[index[1L]], q[index[2L]])
      }
    }
  }
  list(max_tt = max_tt, max_tc = max_tc, argmax_tt = argmax_tt)
}

#' Single-matrix augmented row-column evaluator (production route)
#'
#' Specification section 7.3. Computes every objective of section 6 from one
#' symmetric factorization of size v + s, without forming any v* x v* matrix.
#' The contraction is checked structurally first (raising for malformed
#' input); a negative df_formula returns at once with `lambda_min_rel = NA`
#' and no matrix work, and connectivity is the M-tilde spectrum decision of
#' section 5. A design that is not connected gets diagnostics only.
#'
#' When connected, Omega = M^+ = M_tilde^(-1) - Q0 Q0' and
#' E_aug = (v* - 1) / [v* - v - s + 1 + s tr Omega_RR + v tr Omega_CC].
#' Pairwise variances are those of G = diag(1/u) + H Omega H', with
#' H = diag(u)^(-1) X'Z: test cell (a, b) has H-row x = (e_a; e_b) and check
#' i has H-row h_i = (N_R[, i]; 1_s)/s. G is never formed; the class means
#' come from the aggregates tr G_TT = T + <Omega, S_T>,
#' 1'G_TT 1 = T + h_T' Omega h_T, G_CC = I_k/s + H_C Omega H_C', and
#' 1'G_TC 1 = h_T' Omega H_C' 1_k, with TI = J - N_C the test incidence,
#' S_T = [[diag(TI 1), TI], [TI', diag(TI' 1)]] = sum of x x' over test
#' cells, and h_T = (TI 1; TI' 1) = sum of x over test cells.
#'
#' With `worst_pair = TRUE`, the maxima over distinct test pairs and over
#' test-check pairs come from the test-cell kernel X_T Omega X_T' and the
#' cross kernel X_T Omega H_C', computed exactly in blocks by
#' `arc_worst_pair_maxima()` (section 6.4): only block-by-block sub-kernels
#' are formed, never the T x T kernel. The block size comes from the memory
#' budget `getOption("FielDHub.arc_worst_pair_block_mib", 64)` (MiB per
#' sub-kernel), validated before any matrix work; an invalid value raises
#' `fieldhub_arc_input_error`. This is for reporting once per returned
#' design, not for search.
#'
#' @inheritParams arc_check_contraction
#' @param worst_pair If TRUE, also return `max_tt` and `max_tc`.
#' @return A list with `connected`, the integer `df_formula`, and
#'   `lambda_min_rel` (NA after the negative-df return); when connected, also
#'   `E_aug`, `V_tt`, `V_tc`, `V_cc`, and, with `worst_pair = TRUE`,
#'   `max_tt` and `max_tc`.
#' @noRd
arc_evaluate_objectives <- function(contraction, v, mode, worst_pair = FALSE) {
  if (!is.logical(worst_pair) || length(worst_pair) != 1L ||
      is.na(worst_pair)) {
    fieldhub_abort("worst_pair must be TRUE or FALSE.",
                   class = c("fieldhub_arc_input_error",
                             "fieldhub_input_error"))
  }
  block_size <- if (worst_pair) arc_worst_pair_block_size()
  checked <- arc_check_contraction(contraction, v, mode)
  v <- checked$v
  s <- checked$s
  k <- checked$k
  df_formula <- arc_df_formula(v, s, k)
  if (df_formula < 0L) {
    return(list(connected = FALSE, df_formula = df_formula,
                lambda_min_rel = NA_real_))
  }

  matrices <- arc_contraction_matrices(contraction, v)
  lambda <- eigen(matrices$M_tilde, symmetric = TRUE,
                  only.values = TRUE)$values
  spectrum <- arc_classify_spectrum(lambda)
  result <- list(connected = spectrum$connected, df_formula = df_formula,
                 lambda_min_rel = spectrum$lambda_min_rel)
  if (!spectrum$connected) {
    return(result)
  }

  # Omega = M^+ from a Cholesky solve of the positive definite M_tilde
  q0 <- matrices$Q0
  omega <- chol2inv(chol(matrices$M_tilde)) - tcrossprod(q0)
  omega <- (omega + t(omega)) / 2
  rows <- seq_len(v)
  cols <- v + seq_len(s)

  n_tests <- (v - k) * s
  v_star <- n_tests + k
  diag_omega <- diag(omega)
  e_aug <- (v_star - 1) /
    (v_star - v - s + 1 + s * sum(diag_omega[rows]) +
       v * sum(diag_omega[cols]))

  # Test aggregates: sum of x x' (S_T) and of x (h_T) over test cells
  ti <- 1 - matrices$nc
  ti_rows <- rowSums(ti)
  ti_cols <- colSums(ti)
  s_t <- rbind(cbind(diag(ti_rows, nrow = v), ti),
               cbind(t(ti), diag(ti_cols, nrow = s)))
  h_t <- c(ti_rows, ti_cols)
  omega_h_t <- drop(omega %*% h_t)
  trace_tt <- n_tests + sum(omega * s_t)
  sum_tt <- n_tests + sum(h_t * omega_h_t)

  # Check aggregates: H_C is k x (v + s), row i = (N_R[, i]; 1_s)/s
  h_c <- cbind(t(matrices$nr), matrix(1, k, s)) / s
  omega_h_c <- omega %*% t(h_c)
  g_cc <- diag(1 / s, nrow = k) + h_c %*% omega_h_c
  trace_cc <- sum(diag(g_cc))
  sum_cc <- sum(g_cc)
  sum_tc <- sum(omega_h_t * colSums(h_c))

  result <- c(result, list(
    E_aug = e_aug,
    V_tt = (n_tests * trace_tt - sum_tt) / choose(n_tests, 2),
    V_tc = (k * trace_tt + n_tests * trace_cc - 2 * sum_tc) / (n_tests * k),
    V_cc = (k * trace_cc - sum_cc) / choose(k, 2)
  ))
  if (!worst_pair) {
    return(result)
  }

  # Test cell p = (a, b) has x_p = e_a + e_(v + b) on the (v + s) scale
  cells <- which(ti == 1, arr.ind = TRUE)
  quad_c <- colSums(t(h_c) * omega_h_c)
  maxima <- arc_worst_pair_maxima(omega, cells[, 1L], v + cells[, 2L],
                                  omega_h_c, quad_c, s, block_size)
  c(result, maxima[c("max_tt", "max_tc")])
}

#' Final scoring of an augmented row-column contraction
#'
#' Specification section 7.4, with the section 5 cross-check. With
#' n = v s plots and the cap `getOption("FielDHub.arc_oracle_max_plots",
#' 2500)`:
#' - n <= cap: the full-layout evaluator on the expanded layout is the
#'   authority, cross-checked against the single-matrix evaluator;
#' - n > cap: the single-matrix evaluator with `worst_pair = TRUE` is the
#'   authority, cross-checked against the paper route.
#' The two routes must agree on connectivity and, when both are connected, on
#' E_aug to 1e-10; otherwise `fieldhub_arc_numerical_error` is raised with
#' both values in its data. When both are disconnected, diagnostics are
#' returned without comparing absent E_aug values.
#'
#' `df_formula` and `lambda_min_rel` always come from the contraction-scale
#' route (the full-layout evaluator does not compute them). A connected
#' result has the same fields on both sides of the cap; route-specific
#' extras (`cefs`, `rank_C`, `residual_df_rank`) are not included. A
#' disconnected result carries `residual_df_rank` from the full-layout
#' route, and NA above the cap, so its fields are also cap-independent.
#'
#' @inheritParams arc_check_contraction
#' @return A list with `connected`, `authority` ("full_layout" or
#'   "single_matrix"), `df_formula`, and `lambda_min_rel`; when connected,
#'   also `E_aug`, `V_tt`, `V_tc`, `V_cc`, `max_tt`, and `max_tc`; when
#'   disconnected, also `residual_df_rank`.
#' @noRd
arc_final_score <- function(contraction, v, mode) {
  checked <- arc_check_contraction(contraction, v, mode)
  n <- checked$v * checked$s
  cap <- getOption("FielDHub.arc_oracle_max_plots", 2500)
  if (n <= cap) {
    authority_name <- "full_layout"
    authority <- arc_evaluate_layout(arc_expand(contraction, v, mode)$codes)
    cross <- arc_evaluate_objectives(contraction, v, mode, worst_pair = FALSE)
    contraction_scale <- cross
  } else {
    authority_name <- "single_matrix"
    authority <- arc_evaluate_objectives(contraction, v, mode,
                                         worst_pair = TRUE)
    cross <- arc_evaluate_contraction(contraction, v, mode)
    contraction_scale <- authority
  }
  numerical_class <- c("fieldhub_arc_numerical_error",
                       "fieldhub_numerical_error")
  authority_connected <- isTRUE(authority$connected)
  cross_connected <- isTRUE(cross$connected)
  if (authority_connected != cross_connected) {
    fieldhub_abort("The evaluation routes disagree on connectivity (",
                   authority_name, " authority: ", authority_connected,
                   ", cross-check: ", cross_connected, ").",
                   class = numerical_class,
                   data = list(authority = authority_name,
                               authority_connected = authority_connected,
                               cross_connected = cross_connected,
                               lambda_min_rel =
                                 contraction_scale$lambda_min_rel))
  }
  diagnostics <- list(connected = authority_connected,
                      authority = authority_name,
                      df_formula = contraction_scale$df_formula,
                      lambda_min_rel = contraction_scale$lambda_min_rel)
  if (!authority_connected) {
    residual_df_rank <- if (is.null(authority$residual_df_rank)) {
      NA_integer_
    } else {
      authority$residual_df_rank
    }
    return(c(diagnostics, list(residual_df_rank = residual_df_rank)))
  }

  difference <- abs(authority$E_aug - cross$E_aug)
  if (!is.finite(difference) || difference > 1e-10) {
    fieldhub_abort("The evaluation routes disagree on E_aug (",
                   authority_name, " authority: ", format(authority$E_aug,
                                                          digits = 15),
                   ", cross-check: ", format(cross$E_aug, digits = 15), ").",
                   class = numerical_class,
                   data = list(authority = authority_name,
                               authority_E_aug = authority$E_aug,
                               cross_E_aug = cross$E_aug,
                               difference = difference,
                               lambda_min_rel =
                                 contraction_scale$lambda_min_rel))
  }
  c(diagnostics, authority[c("E_aug", "V_tt", "V_tc", "V_cc", "max_tt",
                             "max_tc")])
}
