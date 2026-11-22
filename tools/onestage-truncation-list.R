# The onestage builds that the work cap cuts short (phase 2 spec 6.1 W1-W8
# and 7, "work cap"; Task 8 Step 6). It replaces the admission list: no shape
# is refused for its size.
#
#   Rscript tools/onestage-truncation-list.R <out.csv> [library] [shard=i/k]
#           [grid=universe|os] [tmax=Inf] [backstop=yes|no]
#
# Universe (the "measured universe" of the spec): every (t, nrows, r) with t
# in {20, 30, 50, 100, 200, 300, 500, 750, 1000, 1500, 2000},
# thin(divisors(t, 2)) nrows (as nested_golden_grid()) and r in 2:4, plus
# every shape of nested_golden_grid("os"), for both codings (latinize FALSE /
# TRUE); only the shapes that pass onestage_feasible(). grid=os keeps the os
# shapes only and tmax= the shapes with t <= tmax (for a short run).
#
# Runs: FielDHub:::build_row_column_onestage() at searches = 200 and at
# searches = 20, with seeds 1:8 for os shapes and seed 1 otherwise; seeds 1:4
# for every other shape whose seed 1 has repair != "none" (a singular stage 1
# design). It reads the attribute "onestage_search" and catches and records
# every warning and error.
#
# Rows: one per shape, coding and `iterations` whose build is not complete on
# seed 1: complete < requested, or a stage of search 1 "cut" or "not_started",
# or repair == "cut"; and one for every shape with a failure (below) on any
# seed. Columns:
#   t,nrows,r,latinize,iterations,in_os_grid,requested,started,complete,
#   row_step,column_step,joint_stage,repair,winner,winner_cut,blocks,work,
#   joint_swaps_1,joint_gain_1,joint_swaps_winner,joint_gain_winner,
#   seconds,outcome,warning,
#   row_D,row_A,col_D,col_A,joint_D,joint_A,z_row_D,...,z_joint_A
# The search counts, the stage statuses, `winner`, `winner_cut`, `work`, the
# joint_* columns and `seconds` are those of seed 1. joint_swaps_1 and
# joint_gain_1 are the swaps the joint stage of search 1 applied (escape
# jumps included) and the joint D of its result less that of its stage 1
# design (0 for a stage 1 design that is not full rank), by the engine's
# unrounded values; joint_*_winner are those of the search returned. They
# tell a joint stage that the cap cut after a few swaps from one that
# searched. `outcome` is "onestage", "fallback" (the
# build raised onestage_infeasible) or "error", the worst over the seeds, and
# `warning` the first warning caught on any seed. For os shapes with a
# baseline at that `iterations`, row_D ... joint_A are the means over the
# seeds of the reported efficiencies and z_* the mean less the baseline mean
# of tools/nested-goldens-os-baseline.csv.gz (seeds where onestage ran) in
# baseline SDs (NA when the baseline SD is 0).
#
# Exit status 1 when, on any shape and any seed: the outcome is "fallback" or
# "error"; a stage is "not_started"; repair == "cut"; a warning is caught (it
# can only be the time-limit warning: rule W7 fails, or the machine was too
# busy; repeat on a quieter machine before deciding); or when an os shape has
# row_step or column_step equal to "cut".
#
# Backstop rows (outside the exit rules; backstop=no skips them; only shard 1
# runs them), both with outcome "backstop":
# - through row_column(), the first of the nested shapes 2500/2/3, 3000/2/3,
#   4000/2/3 and 6000/2/3 whose joint stage is not startable. Its row has the
#   warning text in `warning`, `row_step` holds infoDesign$optimization, and
#   the seconds are those of the call;
# - 2500/2/3 nested through build_row_column_onestage() (seed 1, `iterations`
#   20): beyond the universe a step of stage 1 can be the one that cannot
#   start while the joint stage can (spec 6.1 W3). The row has the stage
#   statuses of the build, so it reports the step that was not started, and
#   any warning caught (none is expected: the warning of rule W8 is for a
#   joint stage that cannot start).
#
# [library] defaults to ~/fieldhub-libs/dev; install with
# `R CMD INSTALL . --preclean` first. It stops if FielDHub is loaded from a
# source tree. shard=i/k runs every k-th shape from the i-th; each shard
# writes its own <out.csv>.
arguments <- commandArgs(trailingOnly = TRUE)
option <- function(name, default) {
  hit <- grep(paste0("^", name, "="), arguments, value = TRUE)
  if (length(hit) == 0L) default else sub(paste0("^", name, "="), "", hit[1])
}
positional <- arguments[!grepl("=", arguments, fixed = TRUE)]
if (length(positional) < 1L || length(positional) > 2L) {
  stop("Usage: Rscript tools/onestage-truncation-list.R <out.csv> [library] [shard=i/k] ",
       "[grid=universe|os] [tmax=Inf] [backstop=yes|no]")
}
path <- positional[1]
library_path <- if (length(positional) == 2L) positional[2] else "~/fieldhub-libs/dev"
.libPaths(c(normalizePath(library_path, mustWork = TRUE), .libPaths()))
shard <- as.integer(strsplit(option("shard", "1/1"), "/", fixed = TRUE)[[1]])
if (length(shard) != 2L || anyNA(shard) || shard[1] < 1L || shard[1] > shard[2]) stop("shard= must be i/k.")
grid_name <- option("grid", "universe")
if (!grid_name %in% c("universe", "os")) stop("grid= must be universe or os.")
tmax <- as.numeric(option("tmax", "Inf"))
backstop <- option("backstop", "yes")
if (!backstop %in% c("yes", "no")) stop("backstop= must be yes or no.")
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
suppressMessages(library(FielDHub))
if (dir.exists(file.path(find.package("FielDHub"), "src"))) {
  stop("FielDHub is loaded from a source tree; run an installed build instead.")
}
source(file.path(script_dir, "..", "tests", "testthat", "helper-nested-goldens.R"))
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
ns <- asNamespace("FielDHub")

thin <- function(x) if (length(x) <= 4L) x else x[unique(round(seq(1, length(x), length.out = 4)))]
divisors <- function(t, from) {
  d <- seq.int(from, t %/% 2L)
  d[t %% d == 0L]
}
os <- nested_golden_grid("os")
os <- os[!duplicated(os[c("t", "k", "r")]), c("t", "k", "r")]
names(os) <- c("t", "nrows", "r")
universe <- os
if (grid_name == "universe") {
  universe <- rbind(do.call(rbind, lapply(c(20, 30, 50, 100, 200, 300, 500, 750, 1000, 1500, 2000),
                                          function(t) expand.grid(t = t, nrows = thin(divisors(t, 2L)), r = 2:4))),
                    os)
}
universe[] <- lapply(universe, as.integer)
universe <- universe[!duplicated(universe) & universe$t <= tmax, , drop = FALSE]
universe <- universe[order(universe$t * universe$r, universe$t, universe$nrows, universe$r), , drop = FALSE]
os_key <- paste(os$t, os$nrows, os$r)
universe <- universe[(seq_len(nrow(universe)) - 1L) %% shard[2] + 1L == shard[1], , drop = FALSE]

statistics <- c("row_D", "row_A", "col_D", "col_A", "joint_D", "joint_A")
baseline <- utils::read.csv(file.path(script_dir, "nested-goldens-os-baseline.csv.gz"))
baseline <- baseline[baseline$status == "ok", , drop = FALSE]
baseline_of <- function(t, nrows, r, latinize, iterations) {
  caller <- if (latinize) "onestage_latinized" else "onestage_nested"
  x <- baseline[baseline$caller == caller & baseline$t == t & baseline$k == nrows & baseline$r == r &
                  baseline$iterations == iterations, statistics, drop = FALSE]
  if (nrow(x) == 0L) return(NULL)
  list(mean = colMeans(x), sd = vapply(x, stats::sd, numeric(1)))
}

# One build: its search description, outcome, first warning, seconds and, when
# `efficiencies`, the reported efficiencies of its design.
one_build <- function(t, nrows, r, latinize, iterations, seed, efficiencies) {
  warned <- character()
  seconds <- system.time(fit <- tryCatch(
    withCallingHandlers(
      ns$build_row_column_onestage(t, nrows, t %/% nrows, r, latinize, iterations, seed),
      warning = function(w) {
        warned <<- c(warned, conditionMessage(w))
        invokeRestart("muffleWarning")
      }),
    error = function(e) e))[["elapsed"]]
  out <- list(seconds = seconds, warning = if (length(warned)) gsub("[\r\n,]+", " ", warned[1]) else "",
              search = NULL, efficiency = NULL)
  if (inherits(fit, "error")) {
    out$outcome <- if (inherits(fit, "onestage_infeasible")) "fallback" else "error"
    out$search <- fit$search
    if (out$outcome == "error" && !nzchar(out$warning)) out$warning <- gsub("[\r\n,]+", " ", conditionMessage(fit))
    return(out)
  }
  out$outcome <- "onestage"
  out$search <- attr(fit, "onestage_search")
  if (efficiencies) {
    joint <- attr(fit, "joint_efficiency")
    attr(fit, "joint_efficiency") <- NULL
    attr(fit, "onestage_search") <- NULL
    model <- ns$report_efficiency(fit, nested = !latinize, joint = joint)
    value <- function(level, column) model[[column]][model$Level == level]
    out$efficiency <- c(row_D = value("Row", "D-Efficiency"), row_A = value("Row", "A-Efficiency"),
                        col_D = value("Column", "D-Efficiency"), col_A = value("Column", "A-Efficiency"),
                        joint_D = value("Row-by-Column", "D-Efficiency"),
                        joint_A = value("Row-by-Column", "A-Efficiency"))
  }
  out
}
field <- function(search, name, default) if (is.null(search) || is.null(search[[name]])) default else search[[name]]
# Element `i` of a per-search vector of the search description (NA without one).
of_search <- function(search, name, i) {
  x <- field(search, name, numeric())
  if (is.na(i) || i > length(x)) NA_real_ else x[[i]]
}
empty <- stats::setNames(rep(NA_real_, 2L * length(statistics)), c(statistics, paste0("z_", statistics)))

rows <- list()
failures <- character()
for (i in seq_len(nrow(universe))) {
  u <- universe[i, ]
  in_os <- paste(u$t, u$nrows, u$r) %in% os_key
  for (latinize in c(FALSE, TRUE)) {
    if (!ns$onestage_feasible(u$t, u$r, u$nrows, u$t %/% u$nrows, latinize)) next
    for (iterations in c(200L, 20L)) {
      label <- sprintf("%d/%d/%d %s iterations %d", u$t, u$nrows, u$r,
                       if (latinize) "latinized" else "nested", iterations)
      first <- one_build(u$t, u$nrows, u$r, latinize, iterations, 1L, in_os)
      seeds <- if (in_os) 2:8 else if (!identical(field(first$search, "repair", "none"), "none")) 2:4 else integer()
      builds <- c(list(first), lapply(seeds, function(seed) {
        one_build(u$t, u$nrows, u$r, latinize, iterations, seed, in_os)
      }))
      problems <- character()
      for (b in seq_along(builds)) {
        x <- builds[[b]]
        s <- x$search
        why <- c(if (x$outcome != "onestage") x$outcome,
                 if ("not_started" %in% c(field(s, "row_step", ""), field(s, "column_step", ""),
                                          field(s, "joint_stage", ""))) "a stage not started",
                 if (identical(field(s, "repair", ""), "cut")) "repair cut",
                 if (nzchar(x$warning)) "warning",
                 if (in_os && "cut" %in% c(field(s, "row_step", ""), field(s, "column_step", ""))) "stage 1 cut on an os shape")
        if (length(why)) problems <- c(problems, sprintf("seed %d: %s", c(1L, seeds)[b], paste(why, collapse = "; ")))
      }
      if (length(problems)) failures <- c(failures, paste0(label, ": ", problems))
      s <- first$search
      incomplete <- is.null(s) || s$complete < s$requested ||
        any(c(s$row_step, s$column_step, s$joint_stage) != "complete") || identical(s$repair, "cut")
      if (!incomplete && length(problems) == 0L) next
      outcomes <- vapply(builds, function(x) x$outcome, character(1))
      warnings_caught <- Filter(nzchar, vapply(builds, function(x) x$warning, character(1)))
      values <- empty
      base <- if (in_os) baseline_of(u$t, u$nrows, u$r, latinize, iterations)
      measured <- do.call(rbind, lapply(builds, function(x) x$efficiency))
      if (!is.null(measured)) {
        values[statistics] <- colMeans(measured)
        if (!is.null(base)) {
          z <- (values[statistics] - base$mean[statistics]) / base$sd[statistics]
          z[!is.finite(z)] <- NA_real_
          values[paste0("z_", statistics)] <- z
        }
      }
      rows[[length(rows) + 1L]] <- cbind(
        data.frame(t = u$t, nrows = u$nrows, r = u$r, latinize = latinize, iterations = iterations,
                   in_os_grid = in_os, requested = field(s, "requested", NA_integer_),
                   started = field(s, "started", NA_integer_), complete = field(s, "complete", NA_integer_),
                   row_step = field(s, "row_step", ""), column_step = field(s, "column_step", ""),
                   joint_stage = field(s, "joint_stage", ""), repair = field(s, "repair", ""),
                   winner = field(s, "winner", NA_integer_), winner_cut = field(s, "winner_cut", NA),
                   blocks = field(s, "blocks", NA_real_), work = field(s, "work", NA_real_),
                   joint_swaps_1 = of_search(s, "joint_swaps", 1L), joint_gain_1 = of_search(s, "joint_gain", 1L),
                   joint_swaps_winner = of_search(s, "joint_swaps", field(s, "winner", NA_integer_)),
                   joint_gain_winner = of_search(s, "joint_gain", field(s, "winner", NA_integer_)),
                   seconds = first$seconds,
                   outcome = if ("error" %in% outcomes) "error" else if ("fallback" %in% outcomes) "fallback" else "onestage",
                   warning = if (length(warnings_caught)) warnings_caught[1] else "",
                   stringsAsFactors = FALSE),
        as.data.frame(as.list(values)))
    }
  }
}

# The backstop: the first shape beyond the universe whose joint stage is not
# startable, through the public function.
if (backstop == "yes" && shard[1] == 1L) {
  for (t in c(2500L, 3000L, 4000L, 6000L)) {
    nrows <- 2L; r <- 3L
    B_row <- r * nrows
    B <- B_row + r * (t %/% nrows)
    if (ns$onestage_startable(3L, t, r, B, B_row = B_row)) next
    warned <- character()
    seconds <- system.time(design <- tryCatch(
      withCallingHandlers(row_column(t = t, nrows = nrows, reps = r, seed = 1, iterations = 20),
                          warning = function(w) {
                            warned <<- c(warned, conditionMessage(w))
                            invokeRestart("muffleWarning")
                          }),
      error = function(e) e))[["elapsed"]]
    rows[[length(rows) + 1L]] <- cbind(
      data.frame(t = t, nrows = nrows, r = r, latinize = FALSE, iterations = 20L, in_os_grid = FALSE,
                 requested = if (inherits(design, "error")) NA_integer_ else design$infoDesign$joint_searches$requested,
                 started = NA_integer_,
                 complete = if (inherits(design, "error")) NA_integer_ else design$infoDesign$joint_searches$completed,
                 row_step = if (inherits(design, "error")) "error" else design$infoDesign$optimization,
                 column_step = "", joint_stage = "", repair = "", winner = NA_integer_, winner_cut = NA,
                 blocks = B, work = NA_real_, joint_swaps_1 = NA_real_, joint_gain_1 = NA_real_,
                 joint_swaps_winner = NA_real_, joint_gain_winner = NA_real_,
                 seconds = seconds, outcome = "backstop",
                 warning = gsub("[\r\n,]+", " ", if (inherits(design, "error")) conditionMessage(design)
                                                  else paste(warned, collapse = " | ")),
                 stringsAsFactors = FALSE),
      as.data.frame(as.list(empty)))
    break
  }
  # A stage 1 step that cannot start where the joint stage can.
  x <- one_build(2500L, 2L, 3L, FALSE, 20L, 1L, FALSE)
  s <- x$search
  rows[[length(rows) + 1L]] <- cbind(
    data.frame(t = 2500L, nrows = 2L, r = 3L, latinize = FALSE, iterations = 20L, in_os_grid = FALSE,
               requested = field(s, "requested", NA_integer_), started = field(s, "started", NA_integer_),
               complete = field(s, "complete", NA_integer_),
               row_step = field(s, "row_step", x$outcome), column_step = field(s, "column_step", ""),
               joint_stage = field(s, "joint_stage", ""), repair = field(s, "repair", ""),
               winner = field(s, "winner", NA_integer_), winner_cut = field(s, "winner_cut", NA),
               blocks = field(s, "blocks", NA_real_), work = field(s, "work", NA_real_),
               joint_swaps_1 = of_search(s, "joint_swaps", 1L), joint_gain_1 = of_search(s, "joint_gain", 1L),
               joint_swaps_winner = of_search(s, "joint_swaps", field(s, "winner", NA_integer_)),
               joint_gain_winner = of_search(s, "joint_gain", field(s, "winner", NA_integer_)),
               seconds = x$seconds, outcome = "backstop", warning = x$warning, stringsAsFactors = FALSE),
    as.data.frame(as.list(empty)))
}

columns <- c("t", "nrows", "r", "latinize", "iterations", "in_os_grid", "requested", "started", "complete",
             "row_step", "column_step", "joint_stage", "repair", "winner", "winner_cut", "blocks", "work",
             "joint_swaps_1", "joint_gain_1", "joint_swaps_winner", "joint_gain_winner",
             "seconds", "outcome", "warning", statistics, paste0("z_", statistics))
out <- if (length(rows)) do.call(rbind, rows)[columns] else
  stats::setNames(data.frame(matrix(nrow = 0L, ncol = length(columns))), columns)
utils::write.csv(out, path, row.names = FALSE)
limits <- ns$onestage_limits()
cat(sprintf("work cap %g, start-up limit %g, memory limit %g, shares %g and %g; %d shapes; %d rows -> %s\n",
            limits$work, limits$startup, limits$memory, limits$row_share, limits$column_share,
            nrow(universe), nrow(out), path))
if (length(failures)) {
  cat("FAIL:\n", paste0("  ", failures, collapse = "\n"), "\n", sep = "")
  quit(status = 1L)
}
