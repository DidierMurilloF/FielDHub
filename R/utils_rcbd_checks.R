#' Resolve the entry pool for an RCBD with repeated checks
#'
#' @description
#' Turns the user-facing `t` / `checks` / `rep_checks` / `data` arguments into a
#' single tidy entry table. Checks occupy the leading rows so that `ENTRY` ids
#' are stable across blocks and locations, matching the convention used by
#' \code{RCBD_augmented()}.
#'
#' @param t Treatment count, or a character vector of labels.
#' @param checks A positive integer (the first N entries of the pool) or a
#'   character vector of check labels.
#' @param rep_checks Times each check repeats within a block; scalar or one
#'   value per check.
#' @param data Optional data frame whose first column holds the entry labels.
#'
#' @return A data frame with columns ENTRY, TREATMENT, CHECKS, reps_per_block.
#' @noRd
rcbd_resolve_entries <- function(t = NULL,
                                 checks = NULL,
                                 rep_checks = NULL,
                                 data = NULL) {
  pool <- NULL
  if (!is.null(data)) {
    if (!is.data.frame(data)) stop("Data must be a data frame.")
    pool <- as.character(stats::na.omit(data[[1]]))
  } else if (is.character(t) && length(t) > 1) {
    pool <- as.character(t)
  }
  if (!is.null(pool) && anyDuplicated(pool) > 0) {
    stop("RCBD() requires unique entry labels; duplicated: ",
         paste(unique(pool[duplicated(pool)]), collapse = ", "))
  }

  if (is.numeric(checks) && length(checks) == 1) {
    if (checks %% 1 != 0 || checks < 1) {
      stop("RCBD() requires 'checks' to be a positive integer when given as a count.")
    }
    if (is.null(pool)) {
      stop("RCBD() requires 'data' (or a character vector 't') when 'checks' is given as a count.")
    }
    if (checks >= length(pool)) {
      stop("RCBD() requires at least one test entry: 'checks' must be fewer than the entries supplied.")
    }
    check_names <- pool[seq_len(checks)]
    test_names  <- pool[-seq_len(checks)]
  } else if (is.character(checks) && length(checks) >= 1) {
    if (anyDuplicated(checks) > 0) {
      stop("RCBD() requires 'checks' labels to be unique.")
    }
    check_names <- as.character(checks)
    if (is.null(pool)) {
      if (is.null(t) || !is.numeric(t) || length(t) != 1) {
        stop("RCBD() requires a numeric 't', 'data', or a character vector 't' alongside character 'checks'.")
      }
      test_names <- paste0("T", seq_len(t))
      clash <- intersect(check_names, test_names)
      if (length(clash) > 0) {
        stop("RCBD() cannot auto-generate test labels: check label(s) ",
             paste(clash, collapse = ", "), " collide with the generated names ",
             "T1..T", t, ". Supply the entries explicitly through 'data' or a ",
             "character vector 't'.")
      }
    } else {
      test_names <- setdiff(pool, check_names)
    }
  } else {
    stop("RCBD() requires 'checks' to be a positive integer or a character vector of labels.")
  }

  if (length(test_names) < 1) {
    stop("RCBD() requires at least one test entry after resolving 'checks'.")
  }

  n_checks <- length(check_names)
  if (is.null(rep_checks)) {
    rep_checks <- rep(1, n_checks)
    message("'rep_checks' was missing; it was set to 1 for every check. ",
            "This is an ordinary RCBD with the checks included in the entry list.")
  }
  if (!is.numeric(rep_checks) || anyNA(rep_checks)) {
    stop("RCBD() requires 'rep_checks' to be numeric.")
  }
  if (any(rep_checks %% 1 != 0)) {
    stop("RCBD() requires 'rep_checks' to be integers.")
  }
  if (any(rep_checks < 1)) {
    stop("RCBD() requires 'rep_checks' to be greater than or equal to 1.")
  }
  if (length(rep_checks) == 1) {
    rep_checks <- rep(rep_checks, n_checks)
  } else if (length(rep_checks) != n_checks) {
    stop(sprintf(
      "RCBD() requires 'rep_checks' to be of length 1 or %d (the number of checks); got %d.",
      n_checks, length(rep_checks)))
  }

  n_test <- length(test_names)
  entries <- data.frame(
    ENTRY          = seq_len(n_checks + n_test),
    TREATMENT      = c(check_names, test_names),
    CHECKS         = c(seq_len(n_checks), rep(0L, n_test)),
    reps_per_block = c(rep_checks, rep(1, n_test)),
    stringsAsFactors = FALSE
  )
  entries$ENTRY  <- as.integer(entries$ENTRY)
  entries$CHECKS <- as.integer(entries$CHECKS)

  # The field book is joined back to this table by label (Task 4 Step 7), so a
  # duplicated TREATMENT would silently mislabel an entry. Guard the invariant.
  if (anyDuplicated(entries$TREATMENT) > 0) {
    stop("RCBD() produced duplicated entry labels: ",
         paste(unique(entries$TREATMENT[duplicated(entries$TREATMENT)]),
               collapse = ", "))
  }

  n_units <- sum(entries$reps_per_block)
  if (sum(rep_checks) > n_units / 2) {
    warning("Checks occupy more than half of each block (",
            sum(rep_checks), " of ", n_units, " plots).")
  }
  entries
}
