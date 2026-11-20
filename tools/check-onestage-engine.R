# Engine checks of the staged onestage row-column search (phase 2 spec 6.1).
# Engine correctness is verified here, not by testthat (constraints: Tests).
#
#   Rscript tools/check-onestage-engine.R <stage1|crossed|repair> [baseline] [library]
#           [seeds=20] [tmax=100]
#
# One PASS / FAIL line per check; exit status 1 on any FAIL. [library]
# defaults to ~/fieldhub-libs/dev; install first with
# `R CMD INSTALL . --preclean` (AGENTS.md). [baseline] is either a per-seed
# golden recording (.csv or .csv.gz, as tools/generate-nested-goldens.R
# writes it, e.g. tools/nested-goldens-os-baseline.csv.gz) or a summary
# data frame (.rds, as tests/testthat/fixtures/nested-goldens-ci.rds); pass
# "-" for none.
#
# stage1: onestage_stage1() for every `os` shape (nested_golden_grid("os"))
# with t <= tmax, both codings (latinize FALSE / TRUE), seeds 1:seeds
# (set.seed(seed) before each call). Checks, per shape and coding:
# - structure: one treatment per plot, every replicate holds each treatment
#   once, and stage 1 emits no warning;
# - the Row A floor of spec 6.1: Row A >= the baseline's constant Row A
#   - 1.5e-7 on every seed, wherever the baseline Row A has SD 0 among the
#   seeds where onestage ran (status "ok", at least two of them; if both
#   iteration counts qualify, the larger constant). Row A is the A-efficiency
#   of the rows alone: nested in the replicates (latinize = FALSE) or shared
#   across them (latinize = TRUE).
# - the four baseline constants that spec 6.1 measured (SD 0, 12 seeds) are
#   checked whether or not a baseline is given: 48/6/3 nested 0.8581470,
#   24/6/2 nested 0.6500942, 30/5/2 nested 0.7742182, 24/6/2 latinized
#   0.8697479.
# - a given baseline must be usable: it FAILs when it cannot be read, lacks
#   the needed columns, or holds no constant Row A shape; and every constant
#   shape (baseline and spec) with t <= tmax must have been checked.
# The stage 1 run time of each shape (all seeds) is printed; it is not a
# gate (timings are controller jobs).
#
# crossed: the joint stage, engine mode 3 (crossed_search(); phase 2 Task 6).
# [baseline], seeds= and tmax= are ignored. Checks:
# (a) values: 20 random complete designs (nested and shared, v <= 30, set.seed
#     fixed), crossed_search(budget = 1): `full_rank` agrees with the dense
#     reference (smallest canonical efficiency of C/r >= 1e-7, C from
#     qr.resid() as in row_column_joint_efficiency(), unrounded); where full
#     rank, `joint_log_det` (sum of log efficiencies) and `joint_d` (their
#     geometric mean) match it to 1e-9, and `row_d`, `row_a` (and the stage 1
#     values of the start) match the one-way eigenvalue reference to 1e-9.
#     This verifies the block-space identity and its full-rank test.
# (b) invariants after full searches from onestage_stage1() starts (24/6/2,
#     30/5/2, 48/6/3 nested and 24/6/2, 30/5/2 latinized, seeds 1:8), run with
#     FIELDHUB_ENGINE_DEBUG=1: every replicate complete; row_d >=
#     stage1_row_d and row_a >= stage1_row_a (relative 1e-9: the engine
#     compares running values with the fixed stage 1 values, 1e-10 slack, so
#     nothing accumulates); joint log det >= the start's (reference) - 1e-9;
#     the final values match the reference to 1e-9; debug_max_error <= 1e-8
#     (the largest relative error of an accepted rank-2 ratio, joint or Row,
#     against fresh refreshes: this verifies the low-rank update). The same
#     invariants on 12/4/2 and 16/4/2 nested (stage 1 often singular or
#     nearly so) and on 60 random complete starts (v 8-16, many
#     near-singular: the ill-conditioned states where a rank-2 score loses
#     digits), at least 20 of them searched.
# (c) parity: single-search mean joint_d >= the spec 6.1 "Floor on Row only"
#     value - 0.001: 0.5929 (24/6/2 nested), 0.6349 (30/5/2 nested), 0.7173
#     (48/6/3 nested, seeds 1:4), 0.8466 (24/6/2 latinized).
#
# repair: Task 7.

arguments <- commandArgs(trailingOnly = TRUE)
named <- grepl("^[a-z]+=", arguments)
opts <- arguments[named]
positional <- arguments[!named]
usage <- "Usage: Rscript tools/check-onestage-engine.R <stage1|crossed|repair> [baseline|-] [library] [seeds=20] [tmax=100]"
if (length(positional) < 1L || length(positional) > 3L ||
    !positional[1] %in% c("stage1", "crossed", "repair")) {
  message(usage)
  quit(status = 1)
}
opt <- function(name, default) {
  x <- grep(paste0("^", name, "="), opts, value = TRUE)
  if (length(x) == 0L) default else as.numeric(sub(paste0("^", name, "="), "", x[1]))
}
unknown <- setdiff(sub("=.*$", "", opts), c("seeds", "tmax"))
if (length(unknown)) { message("Unknown argument(s): ", paste(unknown, collapse = ", "), "\n", usage); quit(status = 1) }
check <- positional[1]
baseline_path <- if (length(positional) >= 2L && positional[2] != "-") path.expand(positional[2]) else NULL
library_path <- if (length(positional) == 3L) positional[3] else "~/fieldhub-libs/dev"
n_seeds <- as.integer(opt("seeds", 20))
tmax <- opt("tmax", 100)
.libPaths(c(normalizePath(library_path, mustWork = TRUE), .libPaths()))
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
suppressMessages(library(FielDHub))
source(file.path(script_dir, "..", "tests", "testthat", "helper-nested-goldens.R"))
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

TOL <- 1.5e-7
failures <- 0L
report <- function(ok, label) {
  cat(if (ok) "PASS" else "FAIL", label, "\n")
  if (!ok) failures <<- failures + 1L
  invisible(ok)
}

if (check == "crossed") {
  ns <- asNamespace("FielDHub")
  for (f in c("crossed_search", "crossed_default_budget", "onestage_stage1")) {
    if (!exists(f, envir = ns, inherits = FALSE)) {
      report(FALSE, paste0(f, "() exists in the installed FielDHub"))
      quit(status = 1)
    }
  }
  crossed <- get("crossed_search", envir = ns)
  stage1 <- get("onestage_stage1", envir = ns)
  work_limit <- get("onestage_work_limit", envir = ns)
  VAL <- 1e-9
  # Canonical plot order: replicate, row, column (constraints).
  layout_of <- function(v, r, nrows, shared) {
    ncols <- v / nrows
    rep_of <- rep(seq_len(r), each = v)
    row_in <- rep(rep(seq_len(nrows), each = ncols), r)
    col_in <- rep(seq_len(ncols), nrows * r)
    list(rep = rep_of,
         row = if (shared) row_in else (rep_of - 1L) * nrows + row_in,
         col = if (shared) col_in else (rep_of - 1L) * ncols + col_in)
  }
  # Unrounded dense references: joint efficiencies from qr.resid() of the
  # Rows + Cols model (as row_column_joint_efficiency()), one-way from
  # C = r I - N N' / k.
  joint_ref <- function(trt, lay, v, r) {
    blocks <- data.frame(Rows = factor(lay$row), Cols = factor(lay$col))
    Tind <- stats::model.matrix(~ factor(trt, levels = seq_len(v)) - 1)
    resid <- qr.resid(qr(stats::model.matrix(~ Rows + Cols, data = blocks)), Tind)
    e <- sort(eigen(crossprod(resid), symmetric = TRUE, only.values = TRUE)$values,
              decreasing = TRUE)[seq_len(v - 1L)] / r
    list(full = min(e) >= 1e-7, log_det = if (min(e) > 0) sum(log(e)) else -Inf,
         d = if (min(e) > 0) exp(mean(log(e))) else 0)
  }
  row_ref <- function(trt, block, v, r) {
    b <- max(block)
    N <- matrix(tabulate((block - 1L) * v + trt, v * b), v, b)
    C <- r * diag(v) - N %*% (t(N) / colSums(N))
    e <- sort(eigen(C / r, symmetric = TRUE, only.values = TRUE)$values, decreasing = TRUE)[seq_len(v - 1L)]
    c(d = exp(mean(log(e))), a = (v - 1) / sum(1 / e))
  }
  close <- function(x, y) is.numeric(x) && length(x) == 1L && !is.na(x) && abs(x - y) <= VAL * max(1, abs(y))
  complete_reps <- function(trt, lay, v, r) {
    length(trt) == v * r && !anyNA(trt) &&
      all(vapply(seq_len(r), function(m) identical(sort(as.integer(trt[lay$rep == m])), seq_len(v)), logical(1)))
  }

  # (a) values on random complete designs, budget = 1
  set.seed(20261120)
  shapes <- list()
  for (v in c(4, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 28, 30)) {
    d <- seq_len(v)[v %% seq_len(v) == 0L]
    d <- d[d >= 2L & d <= v / 2]
    if (length(d)) shapes[[length(shapes) + 1L]] <- expand.grid(v = v, nrows = d)
  }
  shapes <- do.call(rbind, shapes)
  pick <- shapes[sample.int(nrow(shapes), 20L, replace = nrow(shapes) < 20L), ]
  pick$r <- sample(2:4, 20L, replace = TRUE)
  pick$shared <- rep(c(FALSE, TRUE), 10L)
  n_full <- 0L
  for (i in seq_len(nrow(pick))) {
    v <- pick$v[i]; nrows <- pick$nrows[i]; r <- pick$r[i]; shared <- pick$shared[i]
    lay <- layout_of(v, r, nrows, shared)
    start <- as.vector(replicate(r, sample.int(v)))
    label <- sprintf("crossed values %d/%d/%d %s", v, nrows, r, if (shared) "shared" else "nested")
    res <- tryCatch(crossed(start, v, r, nrows, v / nrows, shared, budget = 1, work_cap = work_limit),
                    error = function(e) e)
    if (inherits(res, "error")) { report(FALSE, paste0(label, ": error ", conditionMessage(res))); next }
    ref0 <- joint_ref(start, lay, v, r)
    if (!identical(res$full_rank, ref0$full)) {
      report(FALSE, sprintf("%s: full_rank %s, reference %s", label, format(res$full_rank), ref0$full))
      next
    }
    if (!ref0$full) {
      report(identical(as.integer(res$treatments), as.integer(start)) && res$search$evaluations == 0,
             paste0(label, ": not full rank, start returned without search"))
      next
    }
    n_full <- n_full + 1L
    trt <- as.integer(res$treatments)
    ref <- joint_ref(trt, lay, v, r)
    rw <- row_ref(trt, lay$row, v, r)
    rw0 <- row_ref(start, lay$row, v, r)
    ok <- complete_reps(trt, lay, v, r) && close(res$joint_log_det, ref$log_det) &&
      close(res$joint_d, ref$d) && close(res$row_d, rw[["d"]]) && close(res$row_a, rw[["a"]]) &&
      close(res$stage1_row_d, rw0[["d"]]) && close(res$stage1_row_a, rw0[["a"]]) &&
      res$search$evaluations <= 1
    report(ok, sprintf("%s: joint log det %.12g (ref %.12g), joint D %.10f (ref %.10f), Row D %.10f (ref %.10f), Row A %.10f (ref %.10f)",
                       label, res$joint_log_det, ref$log_det, res$joint_d, ref$d,
                       res$row_d, rw[["d"]], res$row_a, rw[["a"]]))
  }
  report(n_full >= 10L, sprintf("crossed values: %d of 20 random designs full rank (need >= 10)", n_full))

  # (b) invariants and (c) parity after full searches from stage 1 starts
  Sys.setenv(FIELDHUB_ENGINE_DEBUG = "1")
  cases <- data.frame(t = c(24, 30, 48, 24, 30, 12, 16), nrows = c(6, 5, 6, 6, 5, 4, 4),
                      r = c(2, 2, 3, 2, 2, 2, 2),
                      shared = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
                      target = c(0.5929, 0.6349, 0.7173, 0.8466, NA, NA, NA),
                      parity_seeds = c(8, 8, 4, 8, NA, NA, NA))
  for (g in seq_len(nrow(cases))) {
    v <- cases$t[g]; nrows <- cases$nrows[g]; r <- cases$r[g]; shared <- cases$shared[g]
    ncols <- v / nrows
    lay <- layout_of(v, r, nrows, shared)
    label <- sprintf("%d/%d/%d %s", v, nrows, r, if (shared) "latinized" else "nested")
    joint_d <- rep(NA_real_, 8L)
    problems <- character()
    worst_debug <- 0
    seconds <- system.time(for (seed in 1:8) {
      set.seed(seed)
      s1 <- stage1(v, nrows, ncols, r, shared, work_limit)
      ref0 <- joint_ref(s1$treatments, lay, v, r)
      res <- tryCatch(crossed(s1$treatments, v, r, nrows, ncols, shared,
                              work_cap = max(work_limit - s1$work, 1)), error = function(e) e)
      if (inherits(res, "error")) { problems <- c(problems, sprintf("seed %d error: %s", seed, conditionMessage(res))); next }
      trt <- as.integer(res$treatments)
      fail <- function(what) problems <<- c(problems, sprintf("seed %d: %s", seed, what))
      if (!complete_reps(trt, lay, v, r)) { fail("a replicate is not complete"); next }
      if (!isTRUE(res$full_rank)) { if (ref0$full) fail("full-rank start reported not full rank"); next }
      ref <- joint_ref(trt, lay, v, r)
      rw <- row_ref(trt, lay$row, v, r)
      if (!(res$row_d >= res$stage1_row_d * (1 - VAL))) fail(sprintf("Row D %.10f below stage 1 %.10f", res$row_d, res$stage1_row_d))
      if (!(res$row_a >= res$stage1_row_a * (1 - VAL))) fail(sprintf("Row A %.10f below stage 1 %.10f", res$row_a, res$stage1_row_a))
      if (!(ref$log_det >= ref0$log_det - VAL)) fail(sprintf("joint log det %.10f below the start's %.10f", ref$log_det, ref0$log_det))
      if (!(close(res$joint_log_det, ref$log_det) && close(res$joint_d, ref$d) &&
            close(res$row_d, rw[["d"]]) && close(res$row_a, rw[["a"]]))) fail("final values differ from the reference")
      dbg <- res$search$debug_max_error
      if (!is.numeric(dbg) || is.na(dbg) || dbg > 1e-8) fail(sprintf("debug_max_error %s", format(dbg)))
      else worst_debug <- max(worst_debug, dbg)
      joint_d[seed] <- res$joint_d
    })[["elapsed"]]
    report(length(problems) == 0L, sprintf("crossed invariants %s (8 seeds, max debug error %.2e)%s", label,
                                           worst_debug, if (length(problems)) paste0(": ", problems[1]) else ""))
    cat(sprintf("  %s: %.1f s; joint D by seed %s\n", label, seconds,
                paste(sprintf("%.4f", joint_d), collapse = " ")))
    if (!is.na(cases$target[g])) {
      m <- mean(joint_d[seq_len(cases$parity_seeds[g])])
      report(!is.na(m) && m >= cases$target[g] - 0.001,
             sprintf("crossed parity %s: mean joint D %.4f over seeds 1:%d, target %.4f - 0.001",
                     label, m, cases$parity_seeds[g], cases$target[g]))
    }
  }
  # Ill-conditioned states: random complete starts, many near-singular, where
  # a rank-2 ratio loses digits (the prototype review's pitfall).
  set.seed(20261121)
  worst_debug <- 0
  problems <- character()
  searched <- 0L
  for (i in 1:60) {
    v <- sample(c(8, 9, 12, 16), 1L); nrows <- if (v == 9) 3L else 4L; r <- sample(2:3, 1L)
    shared <- i %% 3L == 0L
    lay <- layout_of(v, r, nrows, shared)
    start <- as.vector(replicate(r, sample.int(v)))
    ref0 <- joint_ref(start, lay, v, r)
    res <- crossed(start, v, r, nrows, v / nrows, shared, work_cap = work_limit)
    if (!identical(res$full_rank, ref0$full)) { problems <- c(problems, sprintf("start %d: full_rank differs", i)); next }
    if (!ref0$full) next
    searched <- searched + 1L
    trt <- as.integer(res$treatments)
    ref <- joint_ref(trt, lay, v, r)
    dbg <- res$search$debug_max_error
    if (!complete_reps(trt, lay, v, r) || !(ref$log_det >= ref0$log_det - VAL) ||
        !(res$row_d >= res$stage1_row_d * (1 - VAL)) || !(res$row_a >= res$stage1_row_a * (1 - VAL)) ||
        !close(res$joint_log_det, ref$log_det) || !is.numeric(dbg) || is.na(dbg) || dbg > 1e-8) {
      problems <- c(problems, sprintf("start %d (%d/%d/%d): invariant or debug error %s", i, v, nrows, r, format(dbg)))
    } else worst_debug <- max(worst_debug, dbg)
  }
  report(length(problems) == 0L && searched >= 20L,
         sprintf("crossed ill-conditioned random starts: %d searched, max debug error %.2e%s", searched,
                 worst_debug, if (length(problems)) paste0(": ", problems[1]) else ""))
  Sys.unsetenv("FIELDHUB_ENGINE_DEBUG")
  cat(failures, "FAIL(s)\n")
  quit(status = if (failures > 0L) 1 else 0)
}
if (check != "stage1") {
  report(FALSE, paste(check, "is not implemented yet (phase 2 Task 7)"))
  quit(status = 1)
}

# A-efficiency of one block factor: C = r I - N K^-1 N', canonical efficiency
# factors the nonzero eigenvalues of C / r; 0 when the design is disconnected.
one_way_a <- function(treat, block, v, r) {
  b <- max(block)
  N <- matrix(tabulate((block - 1L) * v + treat, v * b), v, b)
  k <- colSums(N)
  C <- r * diag(v) - N %*% (t(N) / k)
  e <- sort(eigen(C / r, symmetric = TRUE, only.values = TRUE)$values, decreasing = TRUE)[seq_len(v - 1L)]
  if (min(e) < 1e-7) return(0)
  (v - 1) / sum(1 / e)
}

# Baseline Row A constants, keyed "t|k|r|coding". A baseline that cannot be
# read, lacks the columns the floor needs, or holds no constant Row A shape
# stops the check with a FAIL: it would otherwise pass with no floor checked.
baseline_fail <- function(reason) {
  report(FALSE, paste0("stage1 baseline ", baseline_path, ": ", reason))
  cat(failures, "FAIL(s)\n")
  quit(status = 1)
}
baseline_floors <- function(path) {
  need <- function(x, columns, what) {
    absent <- setdiff(columns, names(x))
    if (length(absent)) baseline_fail(paste0(what, " lacks column(s) ", paste(absent, collapse = ", ")))
  }
  summary <- tryCatch({
    if (grepl("\\.rds$", path)) readRDS(path) else {
      records <- utils::read.csv(path)
      need(records, c("caller", "t", "k", "r", "seed", "status", "row_A"), "the recording")
      nested_summary(records)
    }
  }, error = function(e) baseline_fail(paste("cannot be read:", conditionMessage(e))))
  if (!is.data.frame(summary)) baseline_fail("is not a data frame")
  need(summary, c("caller", "t", "k", "r", "seeds", "row_A_sd", "row_A_min"), "the summary")
  summary <- summary[summary$caller %in% nested_onestage_callers, , drop = FALSE]
  constant <- !is.na(summary$row_A_sd) & summary$row_A_sd == 0 & summary$seeds >= 2L &
    !is.na(summary$row_A_min)
  summary <- summary[constant, , drop = FALSE]
  if (nrow(summary) == 0L) {
    baseline_fail("no onestage shape with a constant Row A (SD 0 among at least two ran seeds)")
  }
  coding <- ifelse(summary$caller == "onestage_latinized", "latinized", "nested")
  key <- paste(summary$t, summary$k, summary$r, coding, sep = "|")
  floors <- tapply(summary$row_A_min, key, max)
  stats::setNames(as.numeric(floors), names(floors))
}
spec_floors <- c("48|6|3|nested" = 0.8581470, "24|6|2|nested" = 0.6500942,
                 "30|5|2|nested" = 0.7742182, "24|6|2|latinized" = 0.8697479)
floors <- if (is.null(baseline_path)) numeric() else baseline_floors(baseline_path)
cat("Baseline:", if (is.null(baseline_path)) "none" else baseline_path, "-",
    length(floors), "constant Row A shape(s)\n")

grid <- nested_golden_grid("os")
grid <- unique(grid[grid$t <= tmax, c("t", "k", "r")])
grid <- grid[order(grid$t, grid$k, grid$r), ]
exists_stage1 <- exists("onestage_stage1", envir = asNamespace("FielDHub"), inherits = FALSE)
if (!exists_stage1) {
  report(FALSE, "onestage_stage1() exists in the installed FielDHub")
  quit(status = 1)
}
stage1 <- get("onestage_stage1", envir = asNamespace("FielDHub"))
work_limit <- get("onestage_work_limit", envir = asNamespace("FielDHub"))
checked_floor <- character()
for (g in seq_len(nrow(grid))) {
  t <- grid$t[g]; nrows <- grid$k[g]; r <- grid$r[g]; ncols <- t / nrows
  for (latinize in c(FALSE, TRUE)) {
    coding <- if (latinize) "latinized" else "nested"
    key <- paste(t, nrows, r, coding, sep = "|")
    label <- sprintf("%d/%d/%d %s", t, nrows, r, coding)
    rep_of <- rep(seq_len(r), each = t)
    row_in <- rep(rep(seq_len(nrows), each = ncols), r)
    row_block <- if (latinize) row_in else (rep_of - 1L) * nrows + row_in
    row_a <- rep(NA_real_, n_seeds)
    structure_ok <- TRUE
    problems <- character()
    seconds <- system.time(for (seed in seq_len(n_seeds)) {
      set.seed(seed)
      warned <- character()
      res <- tryCatch(withCallingHandlers(stage1(t, nrows, ncols, r, latinize, work_limit),
                                          warning = function(w) {
                                            warned <<- c(warned, conditionMessage(w))
                                            invokeRestart("muffleWarning")
                                          }),
                      error = function(e) e)
      if (inherits(res, "error")) {
        structure_ok <- FALSE
        problems <- c(problems, sprintf("seed %d error: %s", seed, conditionMessage(res)))
        next
      }
      trt <- res$treatments
      complete <- is.numeric(trt) && length(trt) == t * r && !anyNA(trt) &&
        all(vapply(seq_len(r), function(m) identical(sort(as.integer(trt[rep_of == m])), seq_len(t)),
                   logical(1)))
      if (!complete) {
        structure_ok <- FALSE
        problems <- c(problems, sprintf("seed %d: a replicate does not hold each treatment once", seed))
        next
      }
      if (length(warned)) {
        structure_ok <- FALSE
        problems <- c(problems, sprintf("seed %d warned: %s", seed, warned[1]))
      }
      if (!is.numeric(res$work) || length(res$work) != 1L || is.na(res$work) || res$work < 0) {
        structure_ok <- FALSE
        problems <- c(problems, sprintf("seed %d: work is not one non-negative number", seed))
      }
      row_a[seed] <- one_way_a(as.integer(trt), row_block, t, r)
    })[["elapsed"]]
    report(structure_ok, sprintf("stage1 structure %s (%d seeds)%s", label, n_seeds,
                                 if (length(problems)) paste0(": ", problems[1]) else ""))
    cat(sprintf("  time %s: %.2f s for %d seeds; Row A min %.7f max %.7f\n",
                label, seconds, n_seeds, min(row_a, na.rm = TRUE), max(row_a, na.rm = TRUE)))
    for (source in c("baseline", "spec")) {
      value <- if (source == "baseline") floors[key] else spec_floors[key]
      if (is.null(value) || is.na(value)) next
      below <- sum(is.na(row_a) | row_a < value - TOL)
      checked_floor <- c(checked_floor, paste(source, key))
      report(below == 0L, sprintf("stage1 Row A floor (%s %.7f) %s: %d of %d seeds below, min %.7f",
                                  source, value, label, below, n_seeds, min(row_a, na.rm = TRUE)))
    }
  }
}
# Every constant shape inside the run (t <= tmax) must have been checked.
# The names are selected before pasting: paste("spec", character(0)) is "spec ".
floor_t <- function(x) as.numeric(sub("\\|.*$", "", names(x)))
for (source in c("baseline", "spec")) {
  values <- if (source == "baseline") floors else spec_floors
  if (source == "baseline" && is.null(baseline_path)) next
  due <- names(values)[floor_t(values) <= tmax]
  missing <- setdiff(due, sub(paste0("^", source, " "), "", checked_floor[startsWith(checked_floor, paste0(source, " "))]))
  report(length(missing) == 0L,
         sprintf("every %s constant shape with t <= %g was checked (%d of %d)%s",
                 if (source == "spec") "spec 6.1" else "baseline", tmax,
                 length(due) - length(missing), length(due),
                 if (length(missing)) paste0(": missing ", paste(missing, collapse = ", ")) else ""))
}
cat(failures, "FAIL(s)\n")
quit(status = if (failures > 0L) 1 else 0)
