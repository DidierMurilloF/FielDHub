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
# crossed, repair: Tasks 6 and 7.

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

if (check != "stage1") {
  report(FALSE, paste(check, "is not implemented yet (phase 2 Tasks 6-7)"))
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
