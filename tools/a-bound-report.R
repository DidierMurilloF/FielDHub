# The A-bound report (phase 2 spec 6.4 and 7): per caller and shape of the
# golden files, the recorded (old) bound, the convexity bound of spec 5.2 with
# the R18 rank, FielDHub's own reported bound of spec 5.2b, which component
# gives it, and whether the best recorded design attains each bound.
#   Rscript tools/a-bound-report.R <out.csv> [golden.csv.gz ...]
# Default goldens: tools/nested-goldens-baseline.csv.gz and
# tools/nested-goldens-rc-baseline.csv.gz. Run it against the installed build
# (R CMD INSTALL . --preclean); the bound is the installed package's
# fieldhub_a_bound(), read from asNamespace("FielDHub").
#
# A shape is caller, t, k and r (its iteration counts are pooled). The factor
# is the blocks, and the columns for the row-column callers (`row_column`,
# `onestage_nested`), all in the resolvable class (R = r). A
# `onestage_latinized` shape reports its shared columns in the general class
# (5.2b (e)), from its seeds that ran onestage. "Attained" means the best
# recorded A is within 1.5e-7 of the bound. Columns:
#   caller, t, k, r, seeds, old_bound (recorded), convexity, new_bound,
#   component (the binding component: convexity, second_moment_general,
#   second_moment_resolvable or pair; ties joined by "+"), new_minus_old,
#   best_A, old_attained, convexity_attained, new_attained.
# It prints the per-caller summary of the spec 6.4 table.

arguments <- commandArgs(trailingOnly = TRUE)
if (length(arguments) < 1L) stop("Usage: Rscript tools/a-bound-report.R <out.csv> [golden.csv.gz ...]")
script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))
goldens <- if (length(arguments) > 1L) arguments[-1] else
  file.path(script_dir, c("nested-goldens-baseline.csv.gz", "nested-goldens-rc-baseline.csv.gz"))
fieldhub_a_bound <- get("fieldhub_a_bound", envir = asNamespace("FielDHub"))
tol <- 1.5e-7

records <- do.call(rbind, lapply(goldens, function(path) {
  g <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!"status" %in% names(g)) g$status <- "ok"
  rowcol <- g$caller %in% c("row_column", "onestage_nested", "onestage_latinized")
  data.frame(caller = g$caller, t = g$t, k = g$k, r = g$r, status = g$status,
             A = ifelse(rowcol, g$col_A, g$A),
             old = g$bound,  # the recorded A-Bound of the blocks, or of the columns
             stringsAsFactors = FALSE)
}))
records <- records[records$status == "ok" & !is.na(records$A), ]
key <- paste(records$caller, records$t, records$k, records$r, sep = "|")

rows <- lapply(split(records, factor(key, levels = unique(key))), function(x) {
  caller <- x$caller[1]; t <- x$t[1]; k <- x$k[1]; r <- x$r[1]
  shared <- caller == "onestage_latinized"
  old <- unique(round(x$old, 7))
  if (length(old) != 1L) stop(sprintf("%s: recorded bound varies over seeds", key[1]))
  if (shared) {
    # shared columns: ncols blocks of r * nrows plots
    b <- fieldhub_a_bound(t, r, r * k, t / k, "general")
    parts <- c(convexity = b[["convexity"]], second_moment_general = b[["second_moment"]])
  } else {
    b <- fieldhub_a_bound(t, r, k, r * t / k, "resolvable", R = r)
    general <- fieldhub_a_bound(t, r, k, r * t / k, "general")[["second_moment"]]
    parts <- c(convexity = b[["convexity"]], second_moment_general = general,
               second_moment_resolvable = b[["second_moment"]], pair = b[["pair"]])
  }
  parts <- parts[!is.na(parts)]
  binding <- names(parts)[abs(parts - b[["bound"]]) <= 1e-12]
  # the resolvable second moment is min(L_gen, L_res): name L_res only where
  # it is the smaller
  if (all(c("second_moment_general", "second_moment_resolvable") %in% binding)) {
    binding <- setdiff(binding, "second_moment_resolvable")
  }
  best <- max(x$A)
  data.frame(caller = caller, t = t, k = k, r = r, seeds = nrow(x),
             old_bound = old, convexity = round(b[["convexity"]], 7),
             new_bound = round(b[["bound"]], 7), component = paste(binding, collapse = "+"),
             new_minus_old = round(b[["bound"]] - old, 7), best_A = best,
             old_attained = best >= old - tol,
             convexity_attained = best >= b[["convexity"]] - tol,
             new_attained = best >= b[["bound"]] - tol,
             stringsAsFactors = FALSE)
})
report <- do.call(rbind, rows)
rownames(report) <- NULL
utils::write.csv(report, arguments[1], row.names = FALSE)

callers <- unique(report$caller)
summary_row <- function(x, name) {
  data.frame(caller = name, shapes = nrow(x), old_attained = sum(x$old_attained),
             convexity_attained = sum(x$convexity_attained), new_attained = sum(x$new_attained),
             old_not_new = sum(x$old_attained & !x$new_attained),
             new_not_old = sum(x$new_attained & !x$old_attained),
             gap_old = round(mean(x$old_bound - x$best_A), 5),
             gap_convexity = round(mean(x$convexity - x$best_A), 5),
             gap_new = round(mean(x$new_bound - x$best_A), 5),
             stringsAsFactors = FALSE)
}
summary <- rbind(do.call(rbind, lapply(callers, function(cl) summary_row(report[report$caller == cl, ], cl))),
                 summary_row(report, "All"))
options(width = 200)
cat("A-bound report:", nrow(report), "caller-shapes from", paste(basename(goldens), collapse = ", "), "\n")
cat("Mean gaps are bound - best recorded A.\n\n")
print(summary, row.names = FALSE)
cat(sprintf("\nNew vs old bound: lower on %d, equal on %d, higher on %d (largest excess %.4f)\n",
            sum(report$new_minus_old < -5e-8), sum(abs(report$new_minus_old) <= 5e-8),
            sum(report$new_minus_old > 5e-8), max(report$new_minus_old)))
cat(sprintf("New bound strictly below convexity on %d of %d\n",
            sum(report$new_bound < report$convexity - 5e-8), nrow(report)))
cat(sprintf("New bound below the best recorded A (beyond 1.5e-7) on %d\n",
            sum(report$best_A > report$new_bound + tol)))
cat("\nOld attained, new not:\n")
print(report[report$old_attained & !report$new_attained, c("caller", "t", "k", "r", "old_bound", "new_bound", "best_A", "component")],
      row.names = FALSE)
cat("\nWritten:", arguments[1], "\n")
