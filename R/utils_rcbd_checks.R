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
      if (t < 0 || t != as.integer(t)) {
        stop("RCBD() requires 't' to be a single non-negative integer.")
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
      # When pool is supplied, all check labels must be in the pool
      missing_checks <- setdiff(check_names, pool)
      if (length(missing_checks) > 0) {
        # Look for case-insensitive near-misses and build the error message
        msg_parts <- character(0)
        for (missing in missing_checks) {
          matches <- pool[tolower(pool) == tolower(missing)]
          if (length(matches) > 0) {
            msg_parts <- c(msg_parts, paste0("\"", missing, "\". Did you mean \"", matches[1], "\"?"))
          } else {
            msg_parts <- c(msg_parts, paste0("\"", missing, "\""))
          }
        }
        stop("RCBD() check label(s) not found in the supplied entries: ",
             paste(msg_parts, collapse = ", "))
      }
      test_names <- setdiff(pool, check_names)
    }
  } else if (is.na(checks)) {
    stop("RCBD() requires 'checks' to be a positive integer or a character vector of labels.")
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
  if (!is.numeric(rep_checks)) {
    stop("RCBD() requires 'rep_checks' to be numeric.")
  }
  if (anyNA(rep_checks)) {
    stop("RCBD() requires 'rep_checks' to be numeric.")
  }
  if (any(!is.finite(rep_checks))) {
    stop("RCBD() requires 'rep_checks' to be finite.")
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
            sum(rep_checks), " of ", n_units, " plots). At this density the stratified ",
            "placement becomes tightly constrained and the position of a repeated check ",
            "may be nearly or fully determined rather than random.")
  }
  entries
}

#' Partition block positions into near-equal contiguous strata
#'
#' @param n_units Block size.
#' @param r Number of strata.
#' @return A list of `r` integer vectors covering `1:n_units`.
#' @noRd
rcbd_strata_bounds <- function(n_units, r) {
  base  <- n_units %/% r
  extra <- n_units %% r
  sizes <- rep(base, r) + c(rep(1L, extra), rep(0L, r - extra))
  ends   <- cumsum(sizes)
  starts <- c(1L, utils::head(ends, -1) + 1L)
  Map(seq, starts, ends)
}

#' Randomize a single complete block containing repeated checks
#'
#' @description
#' With `spread_checks = TRUE` the copies of each check are placed one per
#' stratum, so they span the block regardless of the row-by-column geometry
#' chosen later by the plotting layer. Checks are placed most-replicated first;
#' test entries then fill the remaining positions.
#'
#' @param entries Entry table from \code{rcbd_resolve_entries()}.
#' @param spread_checks Stratify the repeated copies. Default TRUE.
#' @param max_tries Attempts before falling back to unrestricted randomization.
#' @return An integer vector of ENTRY ids in plot order.
#' @details
#' Stratified placement is a constraint that reduces the space of valid layouts.
#' At high check density, the set of valid layouts can collapse to just one or
#' a very small set. This is not a bug in the algorithm, but a property of the
#' design itself: when checks occupy ~67% or less of the block, randomization
#' proceeds normally; at ~78% or higher it approaches determinism. Most field
#' trials use far lower check density and are unaffected.
#' @noRd
rcbd_randomize_block <- function(entries, spread_checks = TRUE, max_tries = 100) {
  units   <- rep(entries$ENTRY, times = entries$reps_per_block)
  n_units <- length(units)

  shuffle_all <- function() units[sample.int(n_units)]

  check_rows <- entries[entries$CHECKS != 0 & entries$reps_per_block > 1, , drop = FALSE]
  if (!spread_checks || nrow(check_rows) == 0) {
    return(shuffle_all())
  }

  ord        <- order(-check_rows$reps_per_block, check_rows$ENTRY)
  check_rows <- check_rows[ord, , drop = FALSE]
  rest       <- units[!(units %in% check_rows$ENTRY)]

  for (attempt in seq_len(max_tries)) {
    slots <- rep(NA_integer_, n_units)
    ok    <- TRUE

    for (i in seq_len(nrow(check_rows))) {
      r      <- check_rows$reps_per_block[i]
      strata <- rcbd_strata_bounds(n_units, r)
      for (s in seq_len(r)) {
        free <- strata[[s]][is.na(slots[strata[[s]]])]
        if (length(free) == 0) { ok <- FALSE; break }
        slots[free[sample.int(length(free), 1)]] <- check_rows$ENTRY[i]
      }
      if (!ok) break
    }

    if (ok) {
      empty <- which(is.na(slots))
      slots[empty] <- rest[sample.int(length(rest))]
      return(slots)
    }
  }

  warning("Could not place the repeated checks into distinct strata after ",
          max_tries, " attempts; falling back to unrestricted randomization ",
          "for this block.")
  shuffle_all()
}

#' Field book column order for RCBD, with and without checks
#'
#' @param has_checks Logical.
#' @return A character vector of column names in output order.
#' @noRd
rcbd_fieldbook_cols <- function(has_checks) {
  if (has_checks) {
    c("ID", "LOCATION", "PLOT", "REP", "ENTRY", "TREATMENT", "CHECKS")
  } else {
    c("ID", "LOCATION", "PLOT", "REP", "TREATMENT")
  }
}
