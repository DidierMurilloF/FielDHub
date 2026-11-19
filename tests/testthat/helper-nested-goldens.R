# Golden efficiencies of the block-design callers (spec 7). The tools under
# tools/ source this file; the CI test uses the same code. It only calls the
# public functions, so it measures the same thing before and after an engine
# change.
#
# CI mode (nested_acceptance(bootstrap = FALSE), 5 seeds) does not apply the
# worst-seed rule to the row_* and joint_* efficiencies of row_column (ruling
# R11): they come from the random greedy row step improve_efficiency(), which
# the engine does not change, and its 5-seed tails are noise. They stay judged
# by both the bootstrap and the worst-seed rule in the full run
# (bootstrap = TRUE). The column level (col_*, D, A, bound) keeps every rule.

nested_golden_efficiencies <- c("D", "A", "row_D", "row_A", "col_D", "col_A", "joint_D", "joint_A")
nested_golden_tolerance <- 1.5e-7

# The shapes to run. `k` is the block size, nrows for row_column. `iterations`
# is NA except for the CI row-column shapes, which run with 50.
# The "rc" protocol (spec 6.8) is the extended row_column twostage grid only.
nested_golden_grid <- function(protocol = c("full", "ci", "rc")) {
  protocol <- match.arg(protocol)
  seeds_for <- function(t) if (t <= 300) 200L else if (t == 500) 50L else 5L
  shape <- function(caller, t, k, r) {
    data.frame(caller = caller, t = t, k = k, r = r,
               seeds = vapply(t, seeds_for, integer(1)), iterations = NA_integer_,
               stringsAsFactors = FALSE)
  }
  # smallest, largest and up to two evenly spaced values in between
  thin <- function(x) if (length(x) <= 4L) x else x[unique(round(seq(1, length(x), length.out = 4)))]
  divisors <- function(t, from) {
    d <- seq.int(from, t %/% 2L)
    d[t %% d == 0L]
  }
  if (protocol == "rc") {
    rc <- list()
    for (t in c(20, 30, 50, 100, 200, 300, 500)) {
      iterations <- if (t %in% c(50, 200)) list(NA_integer_, 100L, 5000L) else list(NA_integer_)
      for (k in thin(divisors(t, 2L))) {
        for (it in iterations) {
          x <- shape("row_column", t, k, 2:4)
          x$iterations <- it
          rc[[length(rc) + 1L]] <- x
        }
      }
    }
    rc <- do.call(rbind, rc)
    rownames(rc) <- NULL
    return(rc)
  }
  grid <- list()
  q_extra <- c(3, 4, 5, 7)
  for (q in c(3:12, 14)) {
    r <- unique(c(2:4, if (q %in% q_extra) c(5, q + 1)))
    grid[[length(grid) + 1L]] <- shape("square_lattice", q^2, q, r)
  }
  for (s in c(4:12, 14)) {
    r <- unique(c(2:4, if (s %in% c(5, 7)) c(5, s)))
    grid[[length(grid) + 1L]] <- shape("rectangular_lattice", s * (s - 1), s - 1, r)
  }
  for (caller in c("alpha_lattice", "incomplete_blocks")) {
    for (t in c(20, 30, 50, 100, 150, 200, 300, 500)) {
      grid[[length(grid) + 1L]] <- do.call(rbind, lapply(thin(divisors(t, 3L)), function(k) {
        shape(caller, t, k, 2:4)
      }))
    }
  }
  grid[[length(grid) + 1L]] <- do.call(rbind, lapply(c(20, 100, 1000, 2000), function(t) {
    shape("incomplete_blocks", t, 2L, 2L)
  }))
  grid[[length(grid) + 1L]] <- do.call(rbind, lapply(c(1000, 2000), function(t) {
    shape("incomplete_blocks", t, c(4L, 5L), 2L)
  }))
  for (t in c(20, 30, 50, 100, 200)) {
    grid[[length(grid) + 1L]] <- do.call(rbind, lapply(thin(divisors(t, 2L)), function(k) {
      shape("row_column", t, k, 2:3)
    }))
  }
  grid <- do.call(rbind, grid)
  grid <- grid[!duplicated(grid[c("caller", "t", "k", "r")]), ]
  if (protocol == "ci") {
    rowcol <- grid$caller == "row_column"
    keep <- grid$t <= 50 & !rowcol
    ci_rowcol <- rbind(shape("row_column", 20, 4L, 2L), shape("row_column", 30, 5L, 3L),
                       shape("row_column", 50, 10L, 2L))
    ci_rowcol$iterations <- 50L
    grid <- rbind(grid[keep, ], ci_rowcol)
    grid$seeds <- 5L
  }
  rownames(grid) <- NULL
  grid
}

# Runs one shape at one seed through the public function and reads blocksModel
nested_golden_record <- function(caller, t, k, r, seed, iterations = NULL) {
  fun <- getExportedValue("FielDHub", caller)
  args <- if (caller == "row_column") {
    list(t = t, nrows = k, reps = r, seed = seed, method = "twostage", iterations = iterations)
  } else {
    list(t = t, k = k, reps = r, seed = seed)
  }
  record <- data.frame(caller = caller, t = t, k = k, r = r, seed = seed,
                       D = NA_real_, A = NA_real_, bound = NA_real_,
                       row_D = NA_real_, row_A = NA_real_, col_D = NA_real_, col_A = NA_real_,
                       joint_D = NA_real_, joint_A = NA_real_,
                       seconds = NA_real_, status = "ok", message = "",
                       iterations = if (is.null(iterations)) NA_integer_ else as.integer(iterations),
                       stringsAsFactors = FALSE)
  design <- NULL
  record$seconds <- system.time(tryCatch(
    suppressWarnings(suppressMessages(utils::capture.output(
      design <- do.call(fun, args)))),
    error = function(e) {
      record$status <<- "error"
      record$message <<- gsub("[\r\n,]+", " ", conditionMessage(e))
    }))[["elapsed"]]
  if (record$status == "error") return(record)
  model <- design$blocksModel
  if (!is.data.frame(model)) model <- model[[1]]
  level <- as.character(model$Level)
  get <- function(row, column) {
    value <- model[[column]][level == row]
    if (length(value) == 1L) as.numeric(value) else NA_real_
  }
  if (caller == "row_column") {
    parts <- c(Row = "row", Column = "col", "Row-by-Column" = "joint")
    for (row in names(parts)) {
      part <- parts[[row]]
      record[[paste0(part, "_D")]] <- get(row, "D-Efficiency")
      record[[paste0(part, "_A")]] <- get(row, "A-Efficiency")
    }
    record$D <- record$col_D
    record$A <- record$col_A
    record$bound <- get("Column", "A-Bound")
  } else {
    record$D <- get("2", "D-Efficiency")
    record$A <- get("2", "A-Efficiency")
    record$bound <- get("2", "A-Bound")
  }
  record
}

# Number of mutually orthogonal Latin squares that spec 6.5 expects the package
# to build for order q, by arithmetic only. It never calls the package. Prime
# power: q - 1. Otherwise the least p^e - 1 over the prime-power factors, and
# when that is below 2: 2 if a pair of order q was found by search (spec 6.7,
# the literal orders below), if q = 3m + 1 with m not 2 or 6 and two squares of
# order m, or if q = m n with two squares of each order.
nested_searched_orders <- c(14L, 18L, 26L)
expected_mols <- function(q) {
  q <- as.integer(q)
  if (q < 2L) return(0L)
  factors <- nested_prime_power_factors(q)
  mols <- min(factors) - 1L
  if (length(factors) == 1L || mols >= 2L) return(as.integer(mols))
  if (q %in% nested_searched_orders) return(2L)
  if (q %% 3L == 1L) {
    m <- (q - 1L) %/% 3L
    if (!m %in% c(2L, 6L) && expected_mols(m) >= 2L) return(2L)
  }
  m <- 2L
  while (m * m <= q) {
    if (q %% m == 0L && expected_mols(m) >= 2L && expected_mols(q %/% m) >= 2L) return(2L)
    m <- m + 1L
  }
  as.integer(mols)
}

# The prime-power factors p^e of q
nested_prime_power_factors <- function(q) {
  factors <- integer(0)
  p <- 2L
  while (q > 1L) {
    if (q %% p == 0L) {
      power <- 1L
      while (q %% p == 0L) {
        q <- q %/% p
        power <- power * p
      }
      factors <- c(factors, power)
    }
    p <- p + 1L
  }
  factors
}

# TRUE when spec 6.5 lists the lattice shape as a known gap, where the package
# has no construction and the search is judged on non-inferiority instead
nested_gap <- function(caller, t, r) {
  if (caller == "square_lattice") {
    q <- round(sqrt(t))
    return(r > 2 + expected_mols(q))
  }
  if (caller == "rectangular_lattice") {
    s <- round((1 + sqrt(1 + 4 * t)) / 2)
    return(r >= 4 && r + 1 > 2 + expected_mols(s))
  }
  FALSE
}

# The shape key; `iterations` joins it only when set (the rc protocol runs one
# shape at several iteration counts)
nested_shape_key <- function(x) {
  key <- paste(x$caller, x$t, x$k, x$r, sep = "|")
  if ("iterations" %in% names(x)) key <- ifelse(is.na(x$iterations), key, paste(key, x$iterations, sep = "|"))
  key
}

# One row per shape with mean, SD, min, max and range of every efficiency, the
# number of seeds that hit the A-bound, and the per-seed values in `values`
nested_summary <- function(records) {
  base <- paste(records$caller, records$t, records$k, records$r, sep = "|")
  if ("iterations" %in% names(records)) {
    # iterations is kept only for shapes recorded at several iteration counts
    varied <- tapply(records$iterations, base, function(v) length(unique(v)) > 1L)
    records$iterations[!varied[base]] <- NA
  } else {
    records$iterations <- NA_integer_
  }
  key <- nested_shape_key(records)
  rows <- lapply(split(records, factor(key, levels = unique(key))), function(x) {
    ok <- x[x$status == "ok", , drop = FALSE]
    row <- data.frame(caller = x$caller[1], t = x$t[1], k = x$k[1], r = x$r[1],
                      iterations = as.integer(x$iterations[1]), seeds = nrow(ok), errors = nrow(x) - nrow(ok),
                      stringsAsFactors = FALSE)
    for (e in nested_golden_efficiencies) {
      v <- ok[[e]][!is.na(ok[[e]])]
      has <- length(v) > 0L
      row[[paste0(e, "_mean")]] <- if (has) mean(v) else NA_real_
      row[[paste0(e, "_sd")]] <- if (length(v) > 1L) stats::sd(v) else NA_real_
      row[[paste0(e, "_min")]] <- if (has) min(v) else NA_real_
      row[[paste0(e, "_max")]] <- if (has) max(v) else NA_real_
      row[[paste0(e, "_range")]] <- if (has) max(v) - min(v) else NA_real_
      row[[paste0(e, "_na")]] <- sum(is.na(ok[[e]]))
    }
    row$bound <- if (nrow(ok) > 0L) mean(ok$bound) else NA_real_
    row$hits <- sum(ok$A >= ok$bound - nested_golden_tolerance, na.rm = TRUE)
    row$seconds <- if (nrow(ok) > 0L) stats::median(ok$seconds) else NA_real_
    row$values <- list(ok[c("seed", nested_golden_efficiencies, "bound", "seconds")])
    row
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Bootstrap lower bound (95%, one-sided) of the candidate mean minus the baseline mean
nested_bootstrap_lower <- function(baseline, candidate, resamples = 2000L) {
  draw <- function(x) {
    index <- matrix(sample.int(length(x), resamples * length(x), replace = TRUE), ncol = resamples)
    colMeans(matrix(x[index], ncol = resamples))
  }
  unname(stats::quantile(draw(candidate) - draw(baseline), 0.05))
}

# Applies the acceptance rules of spec 7 to two summaries (shapes of `baseline`
# are looked up in `candidate`). `bootstrap = FALSE` is the CI mode: no
# bootstrap. A constant baseline is SD = 0 over at least 50 seeds, so at the 5
# seeds of CI no shape qualifies. Optimum rule (spec 7): a non-gap lattice shape
# must hit the A-bound (A >= bound - 1.5e-7) on every seed, and so must any other
# shape with t < 1000 where the baseline hit on every seed; for row_column the
# hit compares col_A with the Column A-Bound. `accepted_loss` is NULL or a data.frame of caller, t, k, r
# that the owner accepted: those shapes get pass = NA and report the shortfall.
nested_acceptance <- function(baseline, candidate, accepted_loss = NULL, bootstrap = TRUE) {
  tol <- nested_golden_tolerance
  candidate_key <- nested_shape_key(candidate)
  accepted_key <- if (is.null(accepted_loss)) character(0) else nested_shape_key(accepted_loss)
  if (bootstrap) {
    # the bootstrap seeds the generator; the caller's stream is restored
    seed_state <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv())
    on.exit(if (is.null(seed_state)) {
      if (exists(".Random.seed", envir = globalenv())) rm(".Random.seed", envir = globalenv())
    } else assign(".Random.seed", seed_state, envir = globalenv()))
  }
  rows <- lapply(seq_len(nrow(baseline)), function(i) {
    b <- baseline[i, ]
    j <- match(nested_shape_key(b), candidate_key)
    row <- data.frame(caller = b$caller, t = b$t, k = b$k, r = b$r,
                      noninferior_ok = NA, worst_ok = FALSE, constant_ok = TRUE,
                      optimum_ok = FALSE, errors_ok = FALSE, time_ratio = NA_real_,
                      pass = FALSE, accepted_loss = FALSE, shortfall = NA_real_,
                      stringsAsFactors = FALSE)
    if (is.na(j)) return(row)
    cand <- candidate[j, ]
    bv <- b$values[[1]]
    cv <- cand$values[[1]]
    row$errors_ok <- cand$errors <= b$errors
    row$time_ratio <- cand$seconds / b$seconds
    worst <- TRUE
    constant <- TRUE
    run_bootstrap <- bootstrap && b$t < 1000
    noninferior <- TRUE
    if (run_bootstrap) set.seed(20261118, kind = "Mersenne-Twister", normal.kind = "Inversion",
                                sample.kind = "Rejection")
    for (e in nested_golden_efficiencies) {
      bx <- bv[[e]][!is.na(bv[[e]])]
      cx <- cv[[e]][!is.na(cv[[e]])]
      if (length(bx) == 0L) next
      # NA joint efficiencies are compared as counts
      if (sum(is.na(cv[[e]])) > sum(is.na(bv[[e]])) || length(cx) == 0L) {
        worst <- FALSE
        next
      }
      greedy_rows <- !bootstrap && grepl("^(row|joint)_", e)  # ruling R11
      if (!greedy_rows) worst <- worst && min(cx) >= min(bx) - (max(bx) - min(bx)) - tol
      if (b$t < 1000 && length(bx) >= 50L && max(bx) - min(bx) < 1e-12) {
        constant <- constant && all(cx >= bx[1] - tol)
      }
      if (run_bootstrap && length(bx) > 1L) {
        margin <- max(2e-6, 0.5 * stats::sd(bx))
        noninferior <- noninferior && nested_bootstrap_lower(bx, cx) >= -margin
      }
    }
    row$worst_ok <- worst
    row$constant_ok <- constant
    row$noninferior_ok <- if (run_bootstrap) noninferior else NA
    base_all_hit <- b$t < 1000 && b$seeds > 0L && b$hits == b$seeds
    required <- (b$caller %in% c("square_lattice", "rectangular_lattice") &&
                   !nested_gap(b$caller, b$t, b$r)) || base_all_hit
    row$optimum_ok <- !required || (cand$seeds > 0L && cand$hits == cand$seeds)
    row$pass <- all(c(row$noninferior_ok, row$worst_ok, row$constant_ok, row$optimum_ok,
                      row$errors_ok), na.rm = TRUE)
    if (nested_shape_key(b) %in% accepted_key) {
      row$pass <- NA
      row$accepted_loss <- TRUE
      row$shortfall <- b$A_mean - cand$A_mean
    }
    row
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Speed rule (spec 7, owner decision 2026-11-18): from paired timings (one row
# per shape and seed with bd_seconds and engine_seconds, as written by
# tools/time-nested-pairs.R), one row per shape with the median time of each
# side; the shape is fast enough when the engine's median is at most
# blocksdesign's median + `slack` seconds.
nested_speed <- function(timings, slack = 0.05) {
  key <- nested_shape_key(timings)
  rows <- lapply(split(timings, factor(key, levels = unique(key))), function(x) {
    data.frame(caller = x$caller[1], t = x$t[1], k = x$k[1], r = x$r[1], seeds = nrow(x),
               bd_median = stats::median(x$bd_seconds),
               engine_median = stats::median(x$engine_seconds),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out$speed_ok <- out$engine_median <= out$bd_median + slack
  out
}
