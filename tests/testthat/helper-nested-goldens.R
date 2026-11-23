# Golden efficiencies of the block-design callers (spec 7). The tools under
# tools/ source this file; the CI test uses the same code. It only calls the
# public functions, so it measures the same thing before and after an engine
# change.
#
# CI mode (nested_acceptance(bootstrap = FALSE), 5 seeds) does not apply the
# worst-seed rule to the joint_* efficiencies of row_column: the twostage
# search optimises the columns and then the rows, not the joint Row-by-Column
# criterion, so the 5-seed tails of joint_* are noise. The row_* efficiencies
# keep the rule in CI, because the block engine now runs the row step
# (improve_efficiency()) and a change to it must not go unseen. The full run
# (bootstrap = TRUE) judges every efficiency by both the bootstrap and the
# worst-seed rule.
#
# The optimum rule is gated on the recorded baseline bound (phase 2 spec 6.4):
# a candidate seed is a hit when A >= the baseline's recorded bound - 1.5e-7,
# for every protocol. The candidate's own reported A-Bound never gates, so a
# change to the reported bound cannot turn an unchanged design into a miss.
# `hits` in a summary is that summary's own count (A against the bound recorded
# with it); nested_acceptance() reads it from the baseline only.

nested_golden_efficiencies <- c("D", "A", "row_D", "row_A", "col_D", "col_A", "joint_D", "joint_A")
nested_golden_tolerance <- 1.5e-7

# The shapes to run. `k` is the block size, nrows for row_column. `iterations`
# is NA except for the CI row-column shapes, which run with 50.
# The "rc" protocol (spec 6.8) is the extended row_column twostage grid only.
# The "os" protocol (phase 2 spec 7) is the onestage row-column grid: callers
# "onestage_nested" and "onestage_latinized" (row_column(method = "onestage",
# latinize = FALSE / TRUE)), `k` is nrows, `iterations` is 200 or 20.
nested_onestage_callers <- c("onestage_nested", "onestage_latinized")

# Seeds of the os shapes with t >= 200: fixed by the Step 4 calibration and the
# owner's decision (2026-11-20), the recording budget is 50 seeds per row.
#
# Trim at t >= 300 (owner decision, 2026-11-20): the calibration (one seed per
# row, 16 shards, phase2-baseline library) projected ~87 h at 50 seeds for the
# full grid (t = 300 ~26 h, t = 500 ~49 h), over the 12 h budget. The owner
# chose to trim t >= 300 to one nrows per r (the thinned divisor closest to
# sqrt(t), ties to the smaller) at iterations = 20 only; projection ~15 h.
# The acceptance rules are unchanged.
nested_os_large_seeds <- 50L

nested_golden_grid <- function(protocol = c("full", "ci", "rc", "os")) {
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
  if (protocol == "os") {
    shapes <- list()
    for (t in c(20, 30, 50, 100, 200, 300, 500)) {
      ks <- thin(divisors(t, 2L))
      if (t >= 300) ks <- ks[which.min(round(abs(ks - sqrt(t)), 9))]
      shapes[[length(shapes) + 1L]] <- expand.grid(t = t, k = ks, r = 2:4)
    }
    for (t in c(4, 6, 8, 9, 10, 12, 14, 15, 16)) {
      shapes[[length(shapes) + 1L]] <- expand.grid(t = t, k = FielDHub:::valid_block_sizes(t, "row_column"),
                                                   r = 2:4)
    }
    extras <- data.frame(t = c(45, 36, 48, 24, 30), k = c(5, 6, 6, 6, 5), r = c(3, 3, 3, 2, 2))
    shapes <- do.call(rbind, c(shapes, list(extras)))
    shapes <- shapes[!duplicated(shapes), ]
    os <- do.call(rbind, lapply(nested_onestage_callers, function(caller) {
      do.call(rbind, lapply(c(200L, 20L), function(it) {
        sh <- if (it == 200L) shapes[shapes$t < 300, ] else shapes
        data.frame(caller = caller, t = as.numeric(sh$t), k = as.integer(sh$k),
                   r = as.integer(sh$r),
                   seeds = ifelse(sh$t >= 200, as.integer(nested_os_large_seeds), 200L),
                   iterations = it, stringsAsFactors = FALSE)
      }))
    }))
    os$t <- as.integer(os$t)
    rownames(os) <- NULL
    return(os)
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
    ci_os <- rbind(
      data.frame(caller = "onestage_nested", t = c(9L, 24L, 48L), k = c(3L, 6L, 6L), r = c(2L, 2L, 3L)),
      data.frame(caller = "onestage_latinized", t = c(24L, 30L), k = c(6L, 5L), r = 2L))
    ci_os$seeds <- 5L
    ci_os$iterations <- 20L
    grid <- rbind(grid, ci_os)
  }
  rownames(grid) <- NULL
  grid
}

# Elapsed seconds spent inside build_row_column_onestage(), which the onestage
# record reads into `optimiser_seconds`. It measures design() on the pre-change
# package and the whole onestage build on the candidate.
# The timing is opt-in: a tool that wants it calls nested_trace_onestage() once
# per process, after loading FielDHub. Nothing else installs the trace, so the
# test suite never runs against a traced builder and its records carry NA.
# Every call of the builder within one record adds to `seconds`.
nested_onestage_timer <- new.env()
nested_onestage_timer$installed <- FALSE
nested_onestage_timer$start <- NA_real_
nested_onestage_timer$seconds <- 0
nested_onestage_timer$calls <- 0L
# Installs the trace and returns TRUE. Where the package has no
# build_row_column_onestage(), or the trace cannot be set, it returns FALSE and
# the records carry NA in `optimiser_seconds`; no record is aborted.
nested_trace_onestage <- function() {
  timer <- nested_onestage_timer
  if (isTRUE(timer$installed)) return(invisible(TRUE))
  namespace <- asNamespace("FielDHub")
  if (!exists("build_row_column_onestage", envir = namespace, mode = "function", inherits = FALSE)) {
    return(invisible(FALSE))
  }
  timer$installed <- tryCatch({
    suppressMessages(trace(
      "build_row_column_onestage", where = namespace, print = FALSE,
      tracer = bquote(assign("start", proc.time()[["elapsed"]], envir = .(timer))),
      exit = bquote(local({
        timer <- .(timer)
        timer$seconds <- timer$seconds + proc.time()[["elapsed"]] - timer$start
        timer$calls <- timer$calls + 1L
      }))))
    TRUE
  }, error = function(e) FALSE)
  invisible(timer$installed)
}

# Number of repeated levels of ROW (or COLUMN) within each ENTRY of a field book
nested_clashes <- function(level, entry) sum(tapply(level, entry, function(x) sum(duplicated(x))))

# Runs one shape at one seed through the public function and reads blocksModel.
# The onestage callers run row_column(method = "onestage"); `k` is nrows.
# status is "ok", "fallback" (onestage returned the twostage design) or
# "error". There is no fallback for size (phase 2 spec 7, Amendment 1), so
# every fallback is one of spec 5.1b. `optimiser_seconds` is NA unless the
# process called nested_trace_onestage().
nested_golden_record <- function(caller, t, k, r, seed, iterations = NULL) {
  onestage <- caller %in% nested_onestage_callers
  fun <- getExportedValue("FielDHub", if (onestage) "row_column" else caller)
  args <- if (onestage) {
    list(t = t, nrows = k, reps = r, seed = seed, method = "onestage",
         latinize = caller == "onestage_latinized", iterations = iterations)
  } else if (caller == "row_column") {
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
                       row_bound = NA_real_, col_bound = NA_real_,
                       row_clash = NA_real_, col_clash = NA_real_,
                       optimiser_seconds = NA_real_,
                       stringsAsFactors = FALSE)
  design <- NULL
  timed <- onestage && isTRUE(nested_onestage_timer$installed)
  if (timed) {
    nested_onestage_timer$seconds <- 0
    nested_onestage_timer$calls <- 0L
  }
  record$seconds <- system.time(tryCatch(
    withCallingHandlers(
      suppressMessages(utils::capture.output(design <- do.call(fun, args))),
      warning = function(w) invokeRestart("muffleWarning")),
    error = function(e) {
      record$status <<- "error"
      record$message <<- gsub("[\r\n,]+", " ", conditionMessage(e))
    }))[["elapsed"]]
  # NA when the trace is not installed or the builder was never entered
  if (timed && nested_onestage_timer$calls > 0L) record$optimiser_seconds <- nested_onestage_timer$seconds
  if (record$status == "error") return(record)
  model <- design$blocksModel
  if (!is.data.frame(model)) model <- model[[1]]
  level <- as.character(model$Level)
  get <- function(row, column) {
    value <- model[[column]][level == row]
    if (length(value) == 1L) as.numeric(value) else NA_real_
  }
  if (caller == "row_column" || onestage) {
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
  if (onestage) {
    record$row_bound <- get("Row", "A-Bound")
    record$col_bound <- get("Column", "A-Bound")
    book <- design$fieldBook
    record$row_clash <- nested_clashes(book$ROW, book$ENTRY)
    record$col_clash <- nested_clashes(book$COLUMN, book$ENTRY)
    if (identical(design$infoDesign$optimization, "twostage")) {
      record$status <- "fallback"
    }
  }
  record
}

# The degrees-of-freedom rule of spec 5.1b, by arithmetic only: TRUE when the
# joint additive row-and-column model can estimate the treatment contrasts.
# latinize = FALSE (nested): r (nrows - 1)(ncols - 1) >= t - 1;
# latinize = TRUE (shared): n - r - nrows - ncols + 2 >= t - 1, with n = t r.
onestage_rule_holds <- function(t, nrows, r, latinize) {
  ncols <- t / nrows
  if (latinize) t * r - r - nrows - ncols + 2 >= t - 1
  else r * (nrows - 1) * (ncols - 1) >= t - 1
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
  # records written before the onestage columns existed carry them as NA
  for (column in c("row_bound", "col_bound", "row_clash", "col_clash", "optimiser_seconds")) {
    if (!column %in% names(records)) records[[column]] <- NA_real_
  }
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
                      iterations = as.integer(x$iterations[1]), seeds = nrow(ok), errors = sum(x$status == "error"),
                      fallbacks = sum(x$status == "fallback"),
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
    row$row_bound <- if (nrow(ok) > 0L) mean(ok$row_bound) else NA_real_
    row$col_bound <- if (nrow(ok) > 0L) mean(ok$col_bound) else NA_real_
    clash_max <- function(v) if (length(v) > 0L && !all(is.na(v))) max(v, na.rm = TRUE) else NA_real_
    row$row_clash_max <- clash_max(ok$row_clash)
    row$col_clash_max <- clash_max(ok$col_clash)
    row$hits <- sum(ok$A >= ok$bound - nested_golden_tolerance, na.rm = TRUE)
    row$seconds <- if (nrow(ok) > 0L) stats::median(ok$seconds) else NA_real_
    row$values <- list(ok[c("seed", nested_golden_efficiencies, "bound", "row_bound", "col_bound",
                            "row_clash", "col_clash", "seconds")])
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
# hit compares col_A with the Column A-Bound. The bound is always the
# baseline's recorded one (spec 6.4), never the candidate's own: `b$hits` is
# the baseline's own count, and a candidate seed hits when its A reaches
# `b$bound`. `accepted_loss` is NULL or a data.frame of caller, t, k, r
# (and iterations, where the shape key carries it) that the owner accepted:
# those shapes get pass = NA and report the shortfall.
# Accepted loss of the onestage callers (phase 2 spec 7, owner decision
# 2026-11-22). `noninferior_failed` names the statistics among row_D, row_A,
# col_D, col_A, joint_D, joint_A whose bootstrap rule fails, comma-separated
# (D and A of these callers are Column D and Column A, and are named so).
# `accepted_loss_eligible` is TRUE when the caller is "onestage_nested",
# `noninferior_failed` is not empty and names only col_D, col_A or joint_A, the
# candidate's mean joint D is at least the baseline's (no tolerance), and
# worst_ok, constant_ok, optimum_ok, errors_ok, fallback_ok and clash_ok are
# all TRUE. A listed onestage row gets pass = NA only when it is eligible;
# otherwise the list changes nothing and the row is judged by the rules, so a
# failing row fails. The speed rule is not judged here: the check tool fails a
# listed row that is too slow. For the other callers the list works as before.
# Onestage callers: `fallback_ok` is the count rule (candidate fallbacks <=
# baseline fallbacks, and 0 where onestage_rule_holds()). `clash_ok` is the
# latinization rule and is judged for "onestage_latinized" only; it is TRUE for
# every other caller, "onestage_nested" included.
# The result keeps the baseline's `iterations` (NA unless the shape was recorded
# at several iteration counts), so nested_shape_key() tells the variants apart.
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
                      iterations = if (is.null(b$iterations)) NA_integer_ else as.integer(b$iterations),
                      noninferior_ok = NA, worst_ok = FALSE, constant_ok = TRUE,
                      optimum_ok = FALSE, errors_ok = FALSE, fallback_ok = TRUE, clash_ok = TRUE,
                      time_ratio = NA_real_,
                      pass = FALSE, accepted_loss = FALSE, shortfall = NA_real_,
                      noninferior_failed = "", accepted_loss_eligible = FALSE,
                      stringsAsFactors = FALSE)
    if (is.na(j)) return(row)
    cand <- candidate[j, ]
    bv <- b$values[[1]]
    cv <- cand$values[[1]]
    row$errors_ok <- cand$errors <= b$errors
    onestage <- b$caller %in% nested_onestage_callers
    if (onestage) {
      # an NA count on either side is a failure, never a pass
      row$fallback_ok <- isTRUE(cand$fallbacks <= b$fallbacks &&
        (!onestage_rule_holds(b$t, b$k, b$r, b$caller == "onestage_latinized") || cand$fallbacks == 0))
    }
    if (b$caller == "onestage_latinized") {
      # Latinization rule (phase 2 spec 7): it applies to latinize = TRUE only.
      # The nested caller records its clash counts, but they are not judged:
      # a nested design is free to repeat a treatment in a row or a column.
      # The counts are compared over the seeds where onestage ran (`*_clash_max`).
      # - Baseline without an ok seed: there is no baseline count, so the rule
      #   is vacuous and holds.
      # - Baseline with ok seeds, candidate without: there is no candidate
      #   count to compare, so the rule holds here; the candidate then has more
      #   fallbacks or errors than the baseline, and fallback_ok or errors_ok
      #   fails the shape.
      # - Otherwise both counts exist, and an NA in either is a failure.
      clash_fine <- function(cand_max, base_max) {
        if (b$seeds == 0L || cand$seeds == 0L) return(TRUE)
        isTRUE(cand_max <= base_max) && (isTRUE(base_max > 0) || isTRUE(cand_max == 0))
      }
      row$clash_ok <- clash_fine(cand$row_clash_max, b$row_clash_max) &&
        clash_fine(cand$col_clash_max, b$col_clash_max)
    }
    row$time_ratio <- cand$seconds / b$seconds
    worst <- TRUE
    constant <- TRUE
    run_bootstrap <- bootstrap && b$t < 1000
    noninferior <- TRUE
    failed <- character(0)
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
      # CI skips the worst-seed rule for joint_* only (see the top of this file)
      unsearched_joint <- !bootstrap && b$caller == "row_column" && startsWith(e, "joint_")
      if (!unsearched_joint) worst <- worst && min(cx) >= min(bx) - (max(bx) - min(bx)) - tol
      if (b$t < 1000 && length(bx) >= 50L && max(bx) - min(bx) < 1e-12) {
        constant <- constant && all(cx >= bx[1] - tol)
      }
      if (run_bootstrap && length(bx) > 1L) {
        margin <- max(2e-6, 0.5 * stats::sd(bx))
        fine <- nested_bootstrap_lower(bx, cx) >= -margin
        noninferior <- noninferior && fine
        if (!isTRUE(fine)) failed <- c(failed, e)
      }
    }
    row$worst_ok <- worst
    row$constant_ok <- constant
    row$noninferior_ok <- if (run_bootstrap) noninferior else NA
    base_all_hit <- b$t < 1000 && b$seeds > 0L && b$hits == b$seeds
    required <- (b$caller %in% c("square_lattice", "rectangular_lattice") &&
                   !nested_gap(b$caller, b$t, b$r)) || base_all_hit
    # candidate hits against the baseline's recorded bound (spec 6.4); an NA
    # A or an NA recorded bound is no hit
    cand_hits <- sum(cv$A >= b$bound - tol, na.rm = TRUE)
    row$optimum_ok <- !required || (cand$seeds > 0L && cand_hits == cand$seeds)
    if (onestage) {
      # a hit is A >= the baseline's recorded bound - tol, per factor; the
      # factor must hit on every ok seed wherever the baseline did
      hit_all <- function(a, bound) length(a) > 0L && !anyNA(a) && all(a >= bound - tol)
      ok_factor <- function(a_name, bound_name) {
        base_hits <- b$t < 1000 && hit_all(bv[[a_name]], bv[[bound_name]])
        !base_hits || hit_all(cv[[a_name]], b[[bound_name]])
      }
      row$optimum_ok <- ok_factor("row_A", "row_bound") && ok_factor("col_A", "col_bound")
    }
    row$pass <- all(c(row$noninferior_ok, row$worst_ok, row$constant_ok, row$optimum_ok,
                      row$errors_ok, row$fallback_ok, row$clash_ok), na.rm = TRUE)
    if (onestage) {
      failed[failed == "D"] <- "col_D"
      failed[failed == "A"] <- "col_A"
      failed <- intersect(c("row_D", "row_A", "col_D", "col_A", "joint_D", "joint_A"), failed)
      row$noninferior_failed <- paste(failed, collapse = ",")
      row$accepted_loss_eligible <- b$caller == "onestage_nested" && length(failed) > 0L &&
        all(failed %in% c("col_D", "col_A", "joint_A")) &&
        isTRUE(cand$joint_D_mean >= b$joint_D_mean) &&
        isTRUE(row$worst_ok) && isTRUE(row$constant_ok) && isTRUE(row$optimum_ok) &&
        isTRUE(row$errors_ok) && isTRUE(row$fallback_ok) && isTRUE(row$clash_ok)
    }
    if (nested_shape_key(b) %in% accepted_key && (!onestage || row$accepted_loss_eligible)) {
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
# blocksdesign's median + `slack` seconds. A shape timed at several iteration
# counts gets one row per count (`iterations`, NA for the default).
nested_speed <- function(timings, slack = 0.05) {
  key <- nested_shape_key(timings)
  rows <- lapply(split(timings, factor(key, levels = unique(key))), function(x) {
    data.frame(caller = x$caller[1], t = x$t[1], k = x$k[1], r = x$r[1],
               iterations = if (is.null(x$iterations)) NA_integer_ else as.integer(x$iterations[1]),
               seeds = nrow(x), bd_median = stats::median(x$bd_seconds),
               engine_median = stats::median(x$engine_seconds),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out$speed_ok <- out$engine_median <= out$bd_median + slack
  out
}
